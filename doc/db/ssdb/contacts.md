#### contact info
TYPE: Sorted Set  
Name: <<"contacts_", UserIdBin/binary>>  
Key: ContactId  
Score: ContactVersion  
Commands:  
    zset name key score  
    zscan name '' ContactVersion '' -1  
    zremrangebyscore name score score 