-- CREATE TYPE create_contact_return(f1 varchar(10), f2 varchar(10), ... );
-- hack return type & transcation
CREATE OR REPLACE FUNCTION create_contact(a_id INTEGER,
                                          b_id INTEGER)
RETURNS INTEGER AS
$$
DECLARE
    count   INTEGER;
    now     users.created_at%TYPE;
    version INTEGER;
BEGIN
    SELECT count(pc.a_id)
    INTO count
    FROM pre_contacts pc
    WHERE pc.a_id = create_contact.a_id
    AND pc.b_id = create_contact.b_id;

    IF count <> 0 THEN
        now = now();

        DELETE FROM contacts c
        WHERE   (c.user_id = a_id AND c.contact_id = b_id)
        OR      (c.user_id = b_id AND c.contact_id = a_id);

        DELETE FROM pre_contacts pc
        WHERE pc.a_id = create_contact.a_id
        AND pc.b_id = create_contact.b_id;

        SELECT u.contact_version + 1
        INTO version
        FROM users u
        WHERE u.id = a_id;

        INSERT INTO contacts(user_id,
                             contact_id,
                             contact_version,
                             updated_at,
                             created_at)
        VALUES (a_id, b_id, version, now, now);

        UPDATE users SET contact_version = version WHERE id = a_id;



        SELECT u.contact_version + 1
        INTO version
        FROM users u
        WHERE u.id = b_id;

        INSERT INTO contacts(user_id,
                             contact_id,
                             contact_version,
                             updated_at,
                             created_at)
        VALUES (b_id, a_id, version, now, now);

        UPDATE users SET contact_version = version WHERE id = b_id;

        RETURN 0;
    ELSE
        RETURN 1;
    END IF;
END;
$$
LANGUAGE plpgsql;