CREATE TABLE IF NOT EXISTS feeds (
    id INTEGER PRIMARY KEY
  , name text NOT NULL
  , url text NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS episodes (
    id INTEGER PRIMARY KEY
  , feed_id INTEGER NOT NULL
  , url text NOT NULL UNIQUE
  , title text NOT NULL
  , date DATE NOT NULL
  , FOREIGN KEY (feed_id) REFERENCES feeds(id)
);

CREATE TABLE IF NOT EXISTS progress (
    id INTEGER PRIMARY KEY
  , episode_id INTEGER NOT NULL UNIQUE
  , position DOUBLE PRECISION NOT NULL
  , duration DOUBLE PRECISION NOT NULL
  , FOREIGN KEY (episode_id) REFERENCES episodes(id)
);
