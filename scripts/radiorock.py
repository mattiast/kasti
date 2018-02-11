import dryscrape
import time
import datetime
from episodedb import write_db
import psycopg2
import logging

log = logging.getLogger(__name__)


def get_episodes(query, pages=1):
    dryscrape.start_xvfb()
    log.info("Opening the website")
    sess = dryscrape.Session(base_url ='http://www.radiorock.fi')
    sess.set_attribute('auto_load_images', False)
    sess.visit('/#!/ohjelma/%s' % query)
    log.info("Let's increase the window size and wait a bit...")
    sess.driver.set_viewport_size(2000, 2000)
    time.sleep(3)
    log.info("OK we'll scrape %d pages", pages)
    for i in range(pages - 1):
        log.info("Page %d...", i)
        sess.driver.exec_script('window.scrollBy(0, 2000)')
        time.sleep(1)

    arts = sess.xpath("//article[contains(@class, 'item') and contains(@class ,'post')]")
    log.info("Found %d articles that are potential episodes", len(arts))

    for a in arts:
        [c] = a.xpath("div[@class='item-content']")
        url = c.xpath("div/a[@class='podcast-link']")
        if url:
            url = url[0]['href']
        else:
            continue
        tt = c.xpath("div/time")[0]['datetime']
        date = datetime.datetime.fromtimestamp(int(tt) / 1000.0)
        title = c.xpath("h2/a")[0].text()
        description = c.xpath("div/p")
        if description:
            description = description[0].text()
        else:
            description = ''
        log.info("Found an episode on %s", date)

        yield { 'date': date, 'title': title, 'description': description, 'audio_url': url }

def fetch_and_write(showname, dbpath, query=None):
    if query is None:
        query = showname
    es = get_episodes(query)
    conn = psycopg2.connect(dbpath)
    n = write_db(conn, es, showname)
    conn.close()
    return n
