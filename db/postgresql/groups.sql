CREATE TABLE public.groups
(
    id integer,
    name        varchar(20) NOT NUlL,
    updated     timestamp without time zone NOT NUlL,
    created     timestamp without time zone NOT NUlL,
    PRIMARY KEY (id)
)
WITH (
  OIDS = FALSE
);
CREATE INDEX groups_name_index
ON groups(name);