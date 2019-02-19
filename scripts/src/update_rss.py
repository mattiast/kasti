import io
import json
import logging
import sys
from datetime import datetime
from sys import argv

import boto3
import psycopg2
import psycopg2.extras
import PyRSS2Gen
import radiorock
from psycopg2.extensions import connection


log = logging.getLogger(__name__)

programs = {
    "thf": {
        "name": "Total Hockey Forever",
        "query": "Total Hockey Forever",
        "feedname": "thf.xml",
    },
    "korp": {
        "name": "Radio Rockin Korporaatio",
        "query": "Korporaatio",
        "feedname": "korp.xml",
    },
}


def dict_factory(cursor, row):
    d = {}
    for idx, col in enumerate(cursor.description):
        d[col[0]] = row[idx]
    return d


def get_epi(conn: connection, name: str):
    c = conn.cursor(cursor_factory=psycopg2.extras.DictCursor)
    c.execute(
        """select date, title, description, audio_url
        from rrepisodes
        where show ~* %s""",
        (name,),
    )
    stuff = [dict(r) for r in c]
    return stuff


def create_rss(info, stuff):
    rss = PyRSS2Gen.RSS2(
        title=info["name"],
        link="http://fake-link.com",
        description="podcast description",
        lastBuildDate=datetime.now(),
        items=[
            PyRSS2Gen.RSSItem(
                title=s["title"],
                description=s["description"],
                guid=PyRSS2Gen.Guid(s["audio_url"]),
                enclosure=PyRSS2Gen.Enclosure(s["audio_url"], 0, "audio/mpeg"),
                pubDate=s["date"],
            )
            for s in stuff
        ],
    )
    return rss


def push_to_s3(conf, key: str, data: str):
    session = boto3.session.Session()
    client = session.client(
        "s3",
        region_name=conf["s3_region_name"],
        aws_access_key_id=conf["s3_access_key"],
        aws_secret_access_key=conf["s3_secret"],
    )

    data_obj = io.BytesIO(bytes(data, encoding="utf-8"))
    client.upload_fileobj(
        data_obj,
        conf["s3_bucket"],
        key,
        ExtraArgs={"ACL": "public-read", "ContentType": "application/xml"},
    )


if __name__ == "__main__":
    logging.basicConfig(stream=sys.stdout, level=logging.INFO)

    info = programs[argv[1]]
    confpath = argv[2]
    log.info("Starting update process for %s", info["name"])

    with open(confpath) as f:
        conf = json.load(f)

    n = radiorock.fetch_and_write(
        info["name"], conf["postgres_string"], query=info["query"]
    )
    log.info("%d new episodes", n)

    if n > 0:
        conn = psycopg2.connect(conf["postgres_string"])
        stuff = get_epi(conn, info["name"])
        conn.close()

        rss = create_rss(info, stuff)
        data = rss.to_xml(encoding="utf-8")
        push_to_s3(conf, info["feedname"], data)
