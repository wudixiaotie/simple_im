CREATE TABLE public.groups
(
    id          SERIAL PRIMARY KEY,
    name        VARCHAR(20) NOT NUlL,
    creator_id  INTEGER NOT NUlL,
    key         CHAR(16) NOT NULL,
    updated_at  TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    created_at  TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    FOREIGN KEY (creator_id) REFERENCES users(id) ON UPDATE NO ACTION ON DELETE CASCADE
)
WITH (
  OIDS = FALSE
);
CREATE INDEX groups_name_index ON groups(name);