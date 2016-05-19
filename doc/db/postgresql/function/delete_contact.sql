CREATE OR REPLACE FUNCTION delete_contact(a_id INTEGER,
                                          b_id INTEGER)
RETURNS INTEGER AS
$$
DECLARE
    count INTEGER;
    now   contacts.created_at%TYPE;
BEGIN
    SELECT count(c.user_id)
    INTO count
    FROM contacts c
    WHERE c.user_id = delete_contact.a_id
    AND c.contact_id = delete_contact.b_id;

    IF count <> 0 THEN
        now = now();

        DELETE FROM contacts c
        WHERE   (c.user_id = a_id AND c.contact_id = b_id)
        OR      (c.user_id = b_id AND c.contact_id = a_id);

        UPDATE  users u
        SET     contact_version = u.contact_version + 1,
                updated_at = now
        WHERE   u.id = a_id
        OR      u.id = b_id;

        RETURN 0;
    ELSE
        RETURN 1;
    END IF;
END;
$$
LANGUAGE plpgsql;