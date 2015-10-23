CREATE OR REPLACE FUNCTION HelloWorld() RETURNS void AS
$$
    DECLARE
    testvalue1  VARCHAR(20);
    testvalue2  VARCHAR(20);
 BEGIN
   testvalue1 := 'First Test! ';
   SELECT 'Second Test !' INTO testvalue2;
   INSERT INTO test_helloworld
     SELECT 'Hello World' ;
   INSERT INTO test_helloworld (data)
     VALUES (testvalue1 || testvalue2);
 END;
 $$
 LANGUAGE plpgsql;