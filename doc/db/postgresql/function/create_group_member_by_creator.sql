CREATE OR REPLACE FUNCTION create_group_member_by_creator(group_id      INTEGER,
                                                          creator_id    INTEGER,
                                                          member_id     INTEGER,
                                                          created_at    INTEGER)
RETURNS INTEGER AS
$$
DECLARE
    now             groups.created_at%TYPE;
    real_creator_id INTEGER;
    count           INTEGER;
BEGIN
    now = to_timestamp(created_at);

    SELECT g.creator_id INTO real_creator_id
    FROM groups g
    WHERE g.id = create_group_member_by_creator.group_id;

    SELECT count(gm.group_id) INTO count
    FROM group_members gm
    WHERE gm.group_id = create_group_member_by_creator.group_id
    AND gm.user_id = create_group_member_by_creator.member_id;

    IF real_creator_id IS NULL THEN
        RETURN 1;
    ELSIF real_creator_id <> creator_id THEN
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