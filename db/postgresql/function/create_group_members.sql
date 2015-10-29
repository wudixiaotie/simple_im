CREATE OR REPLACE FUNCTION create_group_members(group_id    INTEGER,
                                                members     INTEGER[])
RETURNS VOID AS
$$
DECLARE
    now         groups.created_at%TYPE;
    member_id   INTEGER;
BEGIN
    now = now();

    FOREACH member_id IN ARRAY members
    LOOP
        INSERT INTO group_members VALUES(group_id, member_id, now, now);
    END LOOP;

    RETURN;
END;
$$
LANGUAGE plpgsql;