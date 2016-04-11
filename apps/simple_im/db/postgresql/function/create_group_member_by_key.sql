CREATE OR REPLACE FUNCTION create_group_member_by_key(group_id  INTEGER,
                                                      key       groups.key%TYPE,
                                                      member_id INTEGER)
RETURNS INTEGER AS
$$
DECLARE
    now         groups.created_at%TYPE;
    real_key    groups.key%TYPE;
    count       INTEGER;
BEGIN
    now = now();

    SELECT g.key INTO real_key
    FROM groups g
    WHERE g.id = create_group_member_by_key.group_id;

    SELECT count(gm.group_id) INTO count
    FROM group_members gm
    WHERE gm.group_id = create_group_member_by_key.group_id
    AND gm.user_id = create_group_member_by_key.member_id;

    IF real_key IS NULL THEN
        RETURN 1;
    ELSIF real_key <> key THEN
        RETURN 2;
    ELSIF count <> 0 THEN
        -- already been a memeber of group
        RETURN 3;
    ELSE
        INSERT INTO group_members VALUES(group_id, member_id, now, now);
        RETURN 0;
    END IF;
END;
$$
LANGUAGE plpgsql;