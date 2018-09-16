import datetime
import logging
import time
from typing import NamedTuple, Iterator, Optional

import dryscrape
import psycopg2
from episodedb import write_db, EpisodeData


log = logging.getLogger(__name__)


class EpisodeInfo(NamedTuple):
    title: str
    description: str
    date: datetime.datetime
    link_url: str


def get_episodes(
    sess: dryscrape.Session, query: str, pages: int = 1
) -> Iterator[EpisodeInfo]:
    log.info("Opening the website")
    sess.visit("/#!/ohjelma/%s" % query)
    log.info("Let's increase the window size and wait a bit...")
    sess.driver.set_viewport_size(2000, 2000)
    time.sleep(3)
    log.info("OK we'll scrape %d pages", pages)
    for i in range(pages - 1):
        log.info("Page %d...", i)
        sess.driver.exec_script("window.scrollBy(0, 2000)")
        time.sleep(1)

    arts = sess.xpath(
        "//article[contains(@class, 'item') and contains(@class ,'post')]"
    )
    log.info("Found %d articles that are potential episodes", len(arts))

    for a in arts:
        [c] = a.xpath("div[@class='item-content']")
        (title,) = c.xpath("h2/a")
        (ep_time,) = c.xpath("div/time")
        ep_time = datetime.datetime.fromtimestamp(int(ep_time["datetime"]) / 1000)

        description = c.xpath("div/p")
        if description:
            description = description[0].text()
        else:
            description = ""

        ep_info = EpisodeInfo(
            title=title.text(),
            date=ep_time,
            description=description,
            link_url=title["href"],
        )
        log.info("Found an episode on %s", ep_info.date)
        yield ep_info


def get_audio_url(sess: dryscrape.Session, link_url: str) -> Optional[str]:
    log.info("Opening the page")
    sess.visit(link_url)

    sess.driver.wait_for(lambda: len(sess.xpath("//audio")) > 1)
    audios = sess.xpath("//audio")
    for a in audios:
        a.exec_script("node.pause()")

    log.info("Found %d audio tags", len(audios))
    audios = [a for a in audios if a["data-isad"] == "false"]
    log.info("Found %d non-ad audio tags", len(audios))

    return audios[0]["src"] if audios else None


def ep_data(ep_info: EpisodeInfo, audio_url: str) -> EpisodeData:
    return EpisodeData(
        title=ep_info.title,
        description=ep_info.description,
        date=ep_info.date,
        audio_url=audio_url,
    )


def fetch_and_write(showname, dbpath, query=None):
    if query is None:
        query = showname
    dryscrape.start_xvfb()
    sess = dryscrape.Session(base_url="https://www.radiorock.fi")
    sess.set_attribute("auto_load_images", False)
    es = get_episodes(sess, query)
    es = list(es)
    sess.reset()
    conn = psycopg2.connect(dbpath)
    c = conn.cursor()
    c.execute("select distinct date from rrepisodes where show = %s", (showname,))
    dates = set(date.replace(tzinfo=None) for (date,) in c)
    stuff = []
    for e in es:
        if e.date in dates:
            log.info("Date = %s already exists, skipping...", e.date)
            continue
        try:
            audio_url = get_audio_url(sess, e.link_url)
        except:
            log.exception("Error while getting audio url for %s", e)
            continue
        sess.reset()
        if audio_url:
            data = ep_data(e, audio_url)
            stuff.append(data)
    n = write_db(conn, stuff, showname)
    conn.close()
    return n
