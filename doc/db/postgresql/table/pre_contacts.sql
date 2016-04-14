CREATE TABLE public.pre_contacts
(
    a_id        INTEGER NOT NULL,
    b_id        INTEGER NOT NULL,
    message     VARCHAR(100),
    updated_at  TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    created_at  TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    FOREIGN KEY (a_id) REFERENCES users(id) ON UPDATE NO ACTION ON DELETE CASCADE,
    FOREIGN KEY (b_id) REFERENCES users(id) ON UPDATE NO ACTION ON DELETE CASCADE
)
WITH (
  OIDS = FALSE
);
CREATE INDEX pre_contacts_id_index ON pre_contacts(a_id, b_id);