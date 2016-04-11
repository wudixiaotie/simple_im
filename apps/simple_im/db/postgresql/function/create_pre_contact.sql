CREATE OR REPLACE FUNCTION create_pre_contact(a_id      INTEGER,
                                              b_id      INTEGER,
                                              message   pre_contacts.message%TYPE)
RETURNS INTEGER AS
$$
DECLARE
    now     pre_contacts.created_at%TYPE;
    count1  INTEGER;
    count2  INTEGER;
BEGIN
    now = now();

    SELECT  count(c.user_id)
    INTO    count1
    FROM    contacts c
    WHERE   (c.user_id = a_id AND c.contact_id = b_id)
    OR      (c.user_id = b_id AND c.contact_id = a_id);

    SELECT  count(pc.a_id)
    INTO    count2
    FROM    pre_contacts pc
    WHERE   (pc.a_id = create_pre_contact.a_id AND
             pc.b_id = create_pre_contact.b_id)
    OR      (pc.a_id = create_pre_contact.b_id AND
             pc.b_id = create_pre_contact.a_id);

    IF count1 <> 0 THEN
        -- b has already been a's contact
        RETURN 1;
    ELSIF count2 <> 0 THEN
        -- b has already been a's pre_contact
        RETURN 2;
    ELSE
        INSERT INTO pre_contacts(a_id,
                                 b_id,
                                 message,
                                 updated_at,
                                 created_at)
        VALUES (a_id, b_id, message, now, now);

        -- success
        RETURN 0;
    END IF;
END;
$$
LANGUAGE plpgsql;