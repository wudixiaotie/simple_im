CREATE OR REPLACE FUNCTION delete_contact(a_id INTEGER,
                                          b_id INTEGER)
RETURNS SETOF INTEGER AS
$$
DECLARE
    now         contacts.created_at%TYPE;
    new_version INTEGER;
BEGIN
    now = now();

    DELETE FROM contacts c
    WHERE   (c.user_id = a_id AND c.contact_id = b_id)
    OR      (c.user_id = b_id AND c.contact_id = a_id);

    UPDATE  users u
    SET     contact_version = u.contact_version + 1,
            updated_at = now
    WHERE   u.id = a_id
    OR      u.id = b_id;

    SELECT  u.contact_version INTO new_version
    FROM    users u
    WHERE   u.id = a_id;

    RETURN NEXT new_version;

    SELECT  u.contact_version INTO new_version
    FROM    users u
    WHERE   u.id = b_id;

    RETURN NEXT new_version;

    RETURN;
END;
$$
LANGUAGE plpgsql;