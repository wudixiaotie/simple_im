#### pre_contact info
TYPE: Sorted Set  
Name: <<"pre_contacts_", UserIdBin/binary>>  
Key: {FriendId, Message}  
Score: Timestamp  
Commands:  
    zset name key score  
    zrscan name '' '' '' limit  

#### pre_contact timestamp
TYPE: Hashmap  
Name: <<"pre_contacts_timestamp_", UserIdBin/binary>>  
Key: FriendId  
Value: Timestamp  
Commands:  
    hset name key value  
    hget name key  
    hdel name key  