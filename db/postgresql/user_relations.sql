CREATE TABLE public.user_relations
(
    user_id     INTEGER,
    friend_user_id   INTEGER,
    updated_at  TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    created_at  TIMESTAMP WITHOUT TIME ZONE NOT NUlL,
    FOREIGN KEY (user_id) REFERENCES users(id) ON UPDATE NO ACTION ON DELETE CASCADE,
    FOREIGN KEY (friend_user_id) REFERENCES users(id) ON UPDATE NO ACTION ON DELETE CASCADE
)
WITH (
  OIDS = FALSE
);
CREATE INDEX user_relations_user_id_index ON user_relations(user_id);
CREATE INDEX user_relations_friend_user_id_index ON user_relations(friend_user_id);