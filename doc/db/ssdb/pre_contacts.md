#### pre_contact info
TYPE: Sorted Set  
Name: <<"pre_contacts_", UserIdBin/binary>>  
Key: <<FriendIdBin/binary, ":", Message/binary>>  
Score: Timestamp  
Commands:  
    zset name key score  
    zrscan name '' '' '' limit  