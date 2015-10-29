CREATE OR REPLACE FUNCTION create_group(name        groups.name%TYPE,
                                        creator_id  INTEGER,
                                        members     INTEGER[])
RETURNS INTEGER AS
$$
DECLARE
    now         groups.created_at%TYPE;
    group_id    INTEGER;
BEGIN
    now = now();

    group_id = nextval('groups_id_seq');

    INSERT INTO groups VALUES(group_id, name, creator_id, now, now);

    PERFORM create_group_members(group_id, members);

    RETURN;
END;
$$
LANGUAGE plpgsql;