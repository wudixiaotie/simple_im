#### group members info
TYPE: Sorted Set  
Name: <<"groups_members_", GroupIdBin/binary>>  
Keys: UserIdBin  
Score: Timestamp  
Commands:  
    zset name key score  
    zscan name '' '' '' -1  
    zdel name key  