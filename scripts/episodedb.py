def write_db(conn, data):
    c = conn.cursor()
    tuples = [ (s['audio_url'], s['title'], s['description'], s['show'], s['date']) for s in data ]
    numchange = 0
    for t in tuples:
        c.execute( u'select count(*) from rrepisodes where audio_url = %s', (t[0],) )
        num = c.fetchone()
        if num[0] == 1:
            continue
        c.execute( u'insert into rrepisodes (audio_url,title,description,show,date) values (%s,%s,%s,%s,%s)  ', t )
        numchange = numchange + 1
    conn.commit()
    return numchange