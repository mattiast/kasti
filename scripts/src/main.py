import logging
import os
import sys

import psycopg2
from fastapi import FastAPI

import update_rss

logging.basicConfig(stream=sys.stdout, level=logging.INFO)
log = logging.getLogger(__name__)

app = FastAPI()


@app.get("/update/{show}")
def update(show: str):
    info = update_rss.programs[show]
    log.info("Starting update process for %s", info.name)

    conf = {
        "postgres_string": os.environ["POSTGRES_STRING"],
        "s3_access_key": os.environ["S3_ACCESS_KEY"],
        "s3_secret": os.environ["S3_SECRET"],
        "s3_bucket": os.environ["S3_BUCKET"],
        "s3_region_name": os.environ["S3_REGION_NAME"],
    }

    n = update_rss.fetch_and_write(info.name, conf["postgres_string"], info.supla_id)
    log.info("%d new episodes", n)

    if n > 0:
        conn = psycopg2.connect(conf["postgres_string"])
        stuff = update_rss.get_epi(conn, info.name)
        conn.close()

        rss = update_rss.create_rss(info, stuff)
        data = rss.to_xml(encoding="utf-8")
        update_rss.push_to_s3(conf, info.feedname, data)
