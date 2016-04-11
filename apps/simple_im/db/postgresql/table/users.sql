CREATE TABLE public.users
(
    id              SERIAL PRIMARY KEY,
    name            VARCHAR(20) NOT NUlL,
    phone           VARCHAR(15) NOT NUlL,
    password        CHAR(32) NOT NUlL,
    salt            CHAR(16) NOT NULL,
    contact_version INTEGER NOT NULL,
    avatar          VARCHAR(300) NOT NULL,
    updated_at      TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    created_at      TIMESTAMP WITHOUT TIME ZONE NOT NUlL
)
WITH (
  OIDS = FALSE
);
CREATE INDEX users_name_index ON users(name);
CREATE UNIQUE INDEX users_phone_index ON users(phone);