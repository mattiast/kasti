import argparse
import json
import logging
import sys
from datetime import datetime
from io import BytesIO
from typing import Iterable, Iterator, NamedTuple

import PyRSS2Gen
import requests
from lxml import etree, html
from update_rss import push_to_s3


log = logging.getLogger(__name__)


class Stuff(NamedTuple):
    title: str
    date: datetime
    link_url: str
    video_url: str


def get_episodes2() -> Iterator[Stuff]:
    r = requests.get("https://www.is.fi/aihe/videotuomarit/")
    doc = html.parse(BytesIO(r.content))
    alinks = doc.xpath("//a[@name]")

    for a in alinks:
        hrefs = a.xpath("@href")
        titles = a.xpath("div[@class='teaser-heading']/h2/text()")
        if not hrefs or not titles:
            continue
        title = titles[0].strip()
        link = f"https://www.is.fi{hrefs[0]}"

        r = requests.get(link)
        doc = html.parse(BytesIO(r.content))
        dates = doc.xpath("//time[@itemprop='datePublished']/@content")
        if not dates:
            continue
        date = datetime.fromisoformat(dates[0])
        media_urls = doc.xpath(
            "//div[@class='video-container']/div/@data-config-url"
        )
        if len(media_urls) != 1:
            continue
        media_url, = media_urls

        r = requests.get(f"http:{media_url}")
        d = etree.parse(BytesIO(r.content))

        files = d.xpath("//HTTPMediaFile/text()")
        if not files:
            continue
        video_url = str(files[0])
        stuff = Stuff(title, date, link, video_url)
        yield stuff


def generate_rss(stuff: Iterable[Stuff]) -> PyRSS2Gen.RSS2:
    rss = PyRSS2Gen.RSS2(
        title="Videotuomarit",
        link="http://www.is.fi/videotuomarit",
        description="Videotuomarit",
        lastBuildDate=datetime.now(),
        items=[
            PyRSS2Gen.RSSItem(
                title=s.title,
                guid=PyRSS2Gen.Guid(s.link_url),
                link=s.link_url,
                pubDate=s.date,
                enclosure=PyRSS2Gen.Enclosure(s.video_url, 0, "video/mp4"),
            )
            for s in stuff
        ],
    )
    return rss


if __name__ == "__main__":
    logging.basicConfig(stream=sys.stdout, level=logging.INFO)

    parser = argparse.ArgumentParser(description="Videotuomarit RSS feed")
    parser.add_argument("conffile", type=str)
    args = parser.parse_args()

    with open(args.conffile) as f:
        conf = json.load(f)

    stuff = list(get_episodes2())

    log.info("Generating RSS")
    rss = generate_rss(stuff)
    data = rss.to_xml(encoding="utf-8")
    log.info("Writing to The Cloud")
    push_to_s3(conf, "videotuomarit.xml", data)
    log.info("Done")
