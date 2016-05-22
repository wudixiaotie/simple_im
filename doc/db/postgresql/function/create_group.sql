CREATE OR REPLACE FUNCTION create_group(name        groups.name%TYPE,
                                        creator_id  INTEGER,
                                        key         groups.key%TYPE,
                                        created_at  INTEGER,
                                        members     INTEGER[])
RETURNS INTEGER AS
$$
DECLARE
    now         groups.created_at%TYPE;
    group_id    INTEGER;
    member_id   INTEGER;
BEGIN
    now = to_timestamp(created_at);

    group_id = nextval('groups_id_seq');

    INSERT INTO groups(id,
                       name,
                       creator_id,
                       key,
                       updated_at,
                       created_at)
    VALUES(group_id, name, creator_id, key, now, now);

    INSERT INTO group_members VALUES(group_id, creator_id, now, now);

    FOREACH member_id IN ARRAY members
    LOOP
        INSERT INTO group_members VALUES(group_id, member_id, now, now);
    END LOOP;

    RETURN group_id;
END;
$$
LANGUAGE plpgsql;