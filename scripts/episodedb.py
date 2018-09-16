import logging


log = logging.getLogger(__name__)


def write_db(conn, data, showname):
    c = conn.cursor()
    tuples = [
        (s['audio_url'], s['title'], s['description'], showname, s['date'])
        for s in data
    ]
    log.info(
        "Got %d episodes, let's see how many are already in the database",
        len(tuples)
    )
    numchange = 0
    for t in tuples:
        c.execute(
            u'select count(*) from rrepisodes where audio_url = %s',
            (t[0],)
        )
        num = c.fetchone()
        if num[0] == 1:
            continue
        log.info("New episode on %s", t[4])
        c.execute(
            """insert into rrepisodes (audio_url,title,description,show,date)
            values (%s,%s,%s,%s,%s) """,
            t
        )
        numchange = numchange + 1
    conn.commit()
    return numchange
