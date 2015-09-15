CREATE TABLE public.groups
(
    id          SERIAL PRIMARY KEY,
    name        VARCHAR(20) NOT NUlL,
    creater_id  INTEGER NOT NUlL,
    updated_at  TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    created_at  TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    FOREIGN KEY (creater_id) REFERENCES users(id) ON UPDATE NO ACTION ON DELETE CASCADE
)
WITH (
  OIDS = FALSE
);
CREATE INDEX groups_name_index ON groups(name);