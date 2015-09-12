CREATE TABLE public.group_members
(
    group_id    integer,
    user_id     integer,
    updated     timestamp without time zone NOT NUlL,
    created     timestamp without time zone NOT NUlL,
    FOREIGN KEY (group_id) REFERENCES groups(id) ON UPDATE NO ACTION ON DELETE NO ACTION,
    FOREIGN KEY (user_id) REFERENCES users(id) ON UPDATE NO ACTION ON DELETE NO ACTION
)
WITH (
  OIDS = FALSE
);
CREATE INDEX group_members_group_id_index
ON group_members(group_id);
CREATE INDEX group_members_user_id_index
ON group_members(user_id);