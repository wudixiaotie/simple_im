#### contact info
TYPE: Sorted Set  
Name: <<"contacts_", UserIdBin/binary>>  
Key: FriendIdBin/binary,   
Score: FriendVersion  
Commands:  
    zset name key score  
    zrscan name '' '' '' limit  