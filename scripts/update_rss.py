from datetime import datetime
import PyRSS2Gen
from sys import argv
import psycopg2
import psycopg2.extras

from botocore.client import Config
import boto3
import io
import json

import radiorock

programs = {
    'thf' : {
        'name' : 'Total Hockey Forever',
        'feedname' : 'thf.xml',
        }
  , 'korp' : {
        'name' : 'Radio Rockin Korporaatio',
        'feedname' : 'korp.xml',
        }
    }

def dict_factory(cursor, row):
    d = {}
    for idx, col in enumerate(cursor.description):
        d[col[0]] = row[idx]
    return d

def get_epi(conn, name):
    c = conn.cursor(cursor_factory=psycopg2.extras.DictCursor)
    c.execute("select date, title, description, audio_url from rrepisodes where show ~* %s", (name,))
    stuff = [ dict(r) for r in c ]
    return stuff

def create_rss(info, stuff):
    rss = PyRSS2Gen.RSS2(
            title = info['name'],
            link = "http://fake-link.com",
            description = "podcast description",
            lastBuildDate = datetime.now(),
            items = [
                PyRSS2Gen.RSSItem(
                    title = s['title'],
                    description = s['description'],
                    guid = PyRSS2Gen.Guid(s['audio_url']),
                    enclosure=PyRSS2Gen.Enclosure(s['audio_url'], 0, "audio/mpeg"),
                    pubDate = s['date'])
                for s in stuff ])
    return rss

def push_to_s3(conf, key, data):
    session = boto3.session.Session()
    client = session.client('s3',
            # TODO Region is hardcoded (I use Digital Ocean spaces, not amazon S3)
            region_name='nyc3',
            endpoint_url=conf['s3_endpoint'],
            aws_access_key_id=conf['s3_access_key'],
            aws_secret_access_key=conf['s3_secret'],
            config=Config(s3={'addressing_style': 'virtual'}))

    data_obj = io.BytesIO(bytes(data, encoding='utf-8'))
    client.upload_fileobj(
            data_obj,
            conf['s3_bucket'],
            key,
            ExtraArgs={ 'ACL': 'public-read', 'ContentType': 'application/xml'})

if __name__ == '__main__':
    info = programs[argv[1]]
    confpath = argv[2]

    with open(confpath) as f:
        conf = json.load(f)

    n = radiorock.fetch_and_write(info['name'], conf['postgres_string'])
    print("%d new episodes" % n)

    if n > 0:
        conn = psycopg2.connect(conf['postgres_string'])
        stuff = get_epi(conn, info['name'])
        conn.close()

        rss = create_rss(info, stuff)
        data = rss.to_xml()
        push_to_s3(conf, info['feedname'], data)
