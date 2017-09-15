import dryscrape
import time
from datetime import datetime
import PyRSS2Gen
from sys import argv


def get_episodes():
    dryscrape.start_xvfb()
    sess = dryscrape.Session(base_url='http://www.is.fi')
    sess.set_attribute('auto_load_images', False)
    sess.visit('/videotuomarit')
    time.sleep(3)

    arts = sess.xpath("//article[contains(@class,'teaser')]")

    heading = lambda a: a.xpath("a/div[contains(@class,'teaser-heading')]/h2")[0].text()
    lnk_url = lambda a: a.xpath('a')[0].get_attr('href')
    datef = lambda a: a.xpath("a/div[@class='details']/span[@class='updated']/time")[0].get_attr('datetime')
    s2d = lambda s: datetime.strptime(s, "%Y-%m-%d")

    stuff = [{
        'title': heading(a),
        'url': "http://www.is.fi" + lnk_url(a),
        'date': s2d(datef(a))
        } for a in arts]
    return stuff


stuff = get_episodes()

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

with open(argv[1], "w") as f:
    rss.write_xml(f)
