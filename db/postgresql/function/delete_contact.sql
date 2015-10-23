CREATE OR REPLACE FUNCTION delete_contact(a_id INTEGER, b_id INTEGER)
RETURNS void AS
$$
    DECLARE
 BEGIN
    DELETE FROM contacts
    WHERE   (user_id = a_id and contact_id = b_id)
    OR      (user_id = b_id and contact_id = a_id);
 END;
 $$
 LANGUAGE plpgsql;