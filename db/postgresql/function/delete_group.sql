CREATE OR REPLACE FUNCTION delete_group(group_id    INTEGER,
                                        creator_id  INTEGER)
RETURNS INTEGER AS
$$
DECLARE
    user_id INTEGER;
BEGIN
    SELECT g.creator_id INTO user_id
    FROM groups g
    WHERE g.id = group_id;

    IF user_id <> creator_id THEN
        -- only creator can delete group
        RETURN 1;
    ELSE
        DELETE FROM groups WHERE id = group_id;
        RETURN 0;
    END IF;
END;
$$
LANGUAGE plpgsql;