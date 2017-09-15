import dryscrape
import time
import datetime
from episodedb import write_db
import psycopg2

def get_episodes(showname):
    dryscrape.start_xvfb()
    sess = dryscrape.Session(base_url ='http://www.radiorock.fi')
    sess.set_attribute('auto_load_images', False)
    sess.visit('/#!/ohjelma/%s' % showname)
    sess.driver.set_viewport_size(2000, 10000)
    time.sleep(3)
    arts = sess.xpath("//article[contains(@class, 'item') and contains(@class ,'post')]")

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

        yield { 'date': date, 'title': title, 'description': description, 'audio_url': url, 'show': showname }

def fetch_and_write(showname, dbpath):
    es = get_episodes(showname)
    conn = psycopg2.connect(dbpath)
    n = write_db(conn, es)
    conn.close()
    return n
