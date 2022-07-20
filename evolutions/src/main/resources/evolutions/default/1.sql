-- Users schema

-- !Ups

CREATE TABLE jp_businessdate (
  date Date NOT NULL,
  PRIMARY KEY (date)
);

CREATE TABLE en_businessdate (
  date Date NOT NULL,
  PRIMARY KEY (date)
);

-- !Downs

DROP TABLE en_businessdate;

DROP TABLE jp_businessdate;