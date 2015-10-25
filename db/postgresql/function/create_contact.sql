CREATE OR REPLACE FUNCTION create_contact(a_id users.id%TYPE,
                                          b_id users.id%TYPE)
RETURNS SETOF INTEGER AS
$$
DECLARE
    now         users.created_at%TYPE;
    old_version users.contact_version%TYPE;
    new_version users.contact_version%TYPE;
BEGIN
    now = now();

    PERFORM delete_contact(a_id, b_id);

    SELECT  u.contact_version INTO old_version
    FROM    users u
    WHERE   u.id = a_id;

    new_version = old_version + 1;

    INSERT INTO contacts(user_id,
                         contact_id,
                         contact_version,
                         updated_at,
                         created_at)
    VALUES (a_id, b_id, new_version, now, now);

    UPDATE users SET contact_version = new_version WHERE id = a_id;

    RETURN NEXT new_version;



    SELECT  u.contact_version INTO old_version
    FROM    users u
    WHERE   u.id = b_id;

    new_version = old_version + 1;

    INSERT INTO contacts(user_id,
                         contact_id,
                         contact_version,
                         updated_at,
                         created_at)
    VALUES (b_id, a_id, new_version, now, now);

    UPDATE users SET contact_version = new_version WHERE id = b_id;

    RETURN NEXT new_version;

    RETURN;
END;
$$
LANGUAGE plpgsql;