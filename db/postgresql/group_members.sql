CREATE TABLE public.group_members
(
    group_id    INTEGER,
    user_id     INTEGER,
    updated_at  TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    created_at  TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    FOREIGN KEY (group_id) REFERENCES groups(id) ON UPDATE NO ACTION ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users(id) ON UPDATE NO ACTION ON DELETE CASCADE
)
WITH (
  OIDS = FALSE
);
CREATE INDEX group_members_group_id_index ON group_members(group_id);
CREATE INDEX group_members_user_id_index ON group_members(user_id);