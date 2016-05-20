#### contact info
TYPE: Sorted Set  
Name: <<"contacts_", UserIdBin/binary>>  
Key: FriendIdBin  
Score: ContactVersion  
Commands:  
    zset name key score  
    zscan name '' ContactVersion '' -1  
    zremrangebyscore name score score  