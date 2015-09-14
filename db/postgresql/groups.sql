CREATE TABLE public.groups
(
    id          SERIAL PRIMARY KEY,
    name        VARCHAR(20) NOT NUlL,
    updated_at  TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    created_at  TIMESTAMP WITHOUT TIME ZONE NOT NUlL
)
WITH (
  OIDS = FALSE
);
CREATE INDEX groups_name_index ON groups(name);