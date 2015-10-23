CREATE OR REPLACE FUNCTION add_contact(a_id INTEGER, b_id INTEGER)
RETURNS void AS
$$
    DECLARE
    now         TIMESTAMP WITHOUT TIME ZONE;
    old_version INTEGER;
    new_version INTEGER;
 BEGIN
    now := now();

    PERFORM delete_contact(a_id, b_id);

    SELECT contact_version INTO old_version FROM users WHERE id = a_id;

    new_version := old_version + 1;

    INSERT INTO contacts(user_id,
                         contact_id,
                         contact_version,
                         updated_at,
                         created_at)
    VALUES (a_id, b_id, new_version, now, now);

    UPDATE users SET contact_version = new_version WHERE id = a_id;



    SELECT contact_version INTO old_version FROM users WHERE id = b_id;

    new_version := old_version + 1;

    INSERT INTO contacts(user_id,
                         contact_id,
                         contact_version,
                         updated_at,
                         created_at)
    VALUES (b_id, a_id, new_version, now, now);

    UPDATE users SET contact_version = new_version WHERE id = b_id;
 END;
 $$
 LANGUAGE plpgsql;