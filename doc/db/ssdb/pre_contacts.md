#### pre_contact info
TYPE: Sorted Set  
Name: <<"pre_contacts_", UserIdBin/binary>>  
Key: FriendIdBin:Message  
Score: Timestamp  
Commands:  
    zset name key score  
    zrscan name '' '' '' limit