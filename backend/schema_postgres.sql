CREATE TABLE IF NOT EXISTS feeds (
    id SERIAL PRIMARY KEY
  , name text NOT NULL
  , url text NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS episodes (
    id SERIAL PRIMARY KEY
  , feed_id INTEGER NOT NULL
  , url text NOT NULL UNIQUE
  , title text NOT NULL
  , date timestamptz NOT NULL
  , FOREIGN KEY (feed_id) REFERENCES feeds(id)
);

CREATE TABLE IF NOT EXISTS progress (
    id SERIAL PRIMARY KEY
  , episode_id INTEGER NOT NULL UNIQUE
  , position DOUBLE PRECISION NOT NULL
  , duration DOUBLE PRECISION NOT NULL
  , FOREIGN KEY (episode_id) REFERENCES episodes(id)
);

CREATE TABLE IF NOT EXISTS rrepisodes (
     id SERIAL PRIMARY KEY,
     show text NOT NULL,
     title text NOT NULL,
     description text NOT NULL,
     audio_url text UNIQUE NOT NULL,
     date timestamptz NOT NULL
    );

