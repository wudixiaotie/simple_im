#### group members info
TYPE: List  
Name: <<"groups_members_", GroupIdBin/binary>>  
Keys: [<<"uid1">>,  
       <<"uid2">>,  
       <<"uid3">>,  
       <<"uid4">>,  
       <<"uid5">>]  
Commands:  
    qrange name, 0, -1);
    multi_hset name key1 value1 key2 value2 ...  
    multi_hget name key1 key2 ...  