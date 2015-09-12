CREATE TABLE public.users
(
    id          integer,
    name        varchar(20) NOT NUlL,
    phone       char(11) NOT NUlL,
    password    varchar(200) NOT NUlL,
    updated     timestamp without time zone NOT NUlL,
    created     timestamp without time zone NOT NUlL,
    PRIMARY KEY (id)
)
WITH (
  OIDS = FALSE
);
CREATE INDEX users_name_index
ON users(name);
CREATE UNIQUE INDEX users_phone_index
ON users(phone);