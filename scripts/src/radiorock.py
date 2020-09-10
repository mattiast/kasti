import datetime as dt
import logging
from typing import Iterator

import lxml.etree
import psycopg2
import requests

from episodedb import EpisodeData, write_db

log = logging.getLogger(__name__)


def fetch_episodes(supla_id: int) -> Iterator[EpisodeData]:
    url = "https://prod-component-api.nm-services.nelonenmedia.fi/api/component/260035"
    params = {
        "current_primary_content": "series",
        "current_series_content_order_direction": "desc",
        "current_series_id": str(supla_id),
        "app": "supla",
        "client": "web",
    }
    r = requests.get(url, params=params)
    r.raise_for_status()

    stuff = r.json()

    for post in stuff["items"]:
        title = post["title"]
        description = "description"
        ts = post["subtitle_array"][2]["timestamp"]
        ts = dt.datetime.utcfromtimestamp(ts)
        log.info("Found episode: %s", title)
        media_id = post["id"]

        r = requests.get(
            f"https://gatling.nelonenmedia.fi/media-xml-cache?v=2&id={media_id}"
        )
        e = lxml.etree.fromstring(r.content)
        audios = e.xpath("//AudioMediaFile")
        if audios:
            audio_url = audios[0].text
            yield EpisodeData(
                title=title, description=description, date=ts, audio_url=audio_url
            )


def fetch_and_write(showname: str, dbpath: str, show_id: int) -> int:
    stuff = list(fetch_episodes(show_id))
    conn = psycopg2.connect(dbpath)
    n = write_db(conn, stuff, showname)
    conn.close()
    return n
