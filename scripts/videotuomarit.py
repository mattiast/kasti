import argparse
import json
import logging
import sys
import time
from datetime import datetime

import dateparser
import dryscrape
import PyRSS2Gen
from update_rss import push_to_s3


log = logging.getLogger(__name__)


def get_episodes():
    dryscrape.start_xvfb()
    sess = dryscrape.Session(base_url='http://www.is.fi')
    sess.set_attribute('auto_load_images', False)
    sess.visit('/videotuomarit')
    time.sleep(3)

    arts = sess.xpath("//article[contains(@class,'teaser') and not(contains(@class,'is-advertorial'))]")
    log.info("Found %d articles", len(arts))

    heading = lambda a: a.xpath("a/div[contains(@class,'teaser-heading')]/h2")[0].text()
    lnk_url = lambda a: a.xpath('a')[0].get_attr('href')
    datef = lambda a: a.xpath("a/div[@class='details']/span[@class='updated']/time")[0].get_attr('data-formatted-date')
    s2d = lambda s: dateparser.parse(s, languages=['fi'])

    stuff = [{
        'title': heading(a),
        'url': "http://www.is.fi" + lnk_url(a),
        'date': s2d(datef(a))
        } for a in arts]
    return stuff


def generate_rss(stuff):
    rss = PyRSS2Gen.RSS2(
            title="Videotuomarit",
            link="http://www.is.fi/videotuomarit",
            description="Videotuomarit",
            lastBuildDate=datetime.now(),
            items=[
                PyRSS2Gen.RSSItem(
                    title=s['title'],
                    guid=PyRSS2Gen.Guid(s['url']),
                    link=s['url'],
                    pubDate=s['date'])
                for s in stuff])
    return rss


if __name__ == '__main__':
    logging.basicConfig(stream=sys.stdout, level=logging.INFO)

    parser = argparse.ArgumentParser(description='Videotuomarit RSS feed')
    parser.add_argument('conffile', type=str)
    args = parser.parse_args()

    with open(args.conffile) as f:
        conf = json.load(f)

    stuff = get_episodes()

    log.info("Generating RSS")
    rss = generate_rss(stuff)
    data = rss.to_xml(encoding='utf-8')
    log.info("Writing to The Cloud")
    push_to_s3(conf, 'videotuomarit.xml', data)
    log.info("Done")
