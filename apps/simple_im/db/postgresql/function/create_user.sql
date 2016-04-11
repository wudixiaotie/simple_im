CREATE OR REPLACE FUNCTION create_user(name     users.name%TYPE,
                                       phone    users.phone%TYPE,
                                       password users.password%TYPE,
                                       salt     users.salt%TYPE)
RETURNS INTEGER AS
$$
DECLARE
    now     users.created_at%TYPE;
    count   INTEGER;
BEGIN
    now = now();

    SELECT  count(u.id) INTO count
    FROM    users u
    WHERE   u.phone = create_user.phone;

    IF count <> 0 THEN
        -- user has already existed
        RETURN 1;
    ELSE
        INSERT INTO users(name,
                          phone,
                          password,
                          salt,
                          contact_version,
                          avatar,
                          updated_at,
                          created_at)
        VALUES(create_user.name,
               create_user.phone,
               create_user.password,
               create_user.salt,
               0,
               '',
               now,
               now);
        RETURN 0;
    END IF;
END;
$$
LANGUAGE plpgsql;