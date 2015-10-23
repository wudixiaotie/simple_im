CREATE TABLE public.contacts
(
    user_id         INTEGER NOT NULL,
    contact_id      INTEGER NOT NULL,
    contact_version INTEGER NOT NULL,
    updated_at      TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    created_at      TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    FOREIGN KEY (user_id) REFERENCES users(id) ON UPDATE NO ACTION ON DELETE CASCADE,
    FOREIGN KEY (contact_id) REFERENCES users(id) ON UPDATE NO ACTION ON DELETE CASCADE
)
WITH (
  OIDS = FALSE
);
CREATE INDEX contacts_user_id_index ON contacts(user_id);
CREATE INDEX contacts_user_contact_id_index ON contacts(user_id, contact_id);
CREATE INDEX contacts_version_index ON contacts(user_id, contact_version);