import datetime as dt
import logging
from typing import Iterator, Optional
from urllib.parse import quote

import lxml.etree
import psycopg2
import requests
from episodedb import EpisodeData, write_db


log = logging.getLogger(__name__)


def fetch_episodes(
    tag_name: str, tag_cat: str = "ohjelma", page: int = 0
) -> Iterator[EpisodeData]:
    url = "https://www.radiorock.fi/api/content?tagCategory={}&tagName={}&page={}".format(
        tag_cat, quote(tag_name), page
    )

    r = requests.get(url)

    stuff = r.json()

    for post in stuff["posts"]:
        if "media" not in post:
            continue
        title = post["media"]["title"]
        description = post["media"]["description"]
        ts = dt.datetime.utcfromtimestamp(post["created_at_epoch"] / 1000)
        log.info("Found episode: %s", title)
        media_id = post["media"]["id"]

        r = requests.get(
            "https://gatling.nelonenmedia.fi/media-xml-cache?v=2&id={}".format(media_id)
        )
        e = lxml.etree.fromstring(r.content)
        audios = e.xpath("//AudioMediaFile")
        if audios:
            audio_url = audios[0].text
            yield EpisodeData(
                title=title, description=description, date=ts, audio_url=audio_url
            )


def fetch_and_write(showname: str, dbpath: str, query: Optional[str] = None) -> int:
    if query is None:
        query = showname

    stuff = list(fetch_episodes(query))
    conn = psycopg2.connect(dbpath)
    n = write_db(conn, stuff, showname)
    conn.close()
    return n
