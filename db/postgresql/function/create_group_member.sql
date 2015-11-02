CREATE OR REPLACE FUNCTION create_group_member(group_id     INTEGER,
                                               key          groups.key%TYPE,
                                               member_id    INTEGER)
RETURNS INTEGER AS
$$
DECLARE
    now         groups.created_at%TYPE;
    real_key    groups.key%TYPE;
BEGIN
    now = now();

    SELECT g.key INTO real_key
    FROM groups g
    WHERE g.id = group_id;

    IF real_key <> key THEN
        RETURN 1;
    ELSE
        INSERT INTO group_members VALUES(group_id, member_id, now, now);
        RETURN 0;
    END IF;
END;
$$
LANGUAGE plpgsql;