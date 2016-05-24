#### user info
TYPE: Hashmap  
Name: <<"users_", UserIdBin/binary>>  
Keys: [<<"name">>,  
       <<"phone">>,  
       <<"password">>,  
       <<"salt">>,  
       <<"avatar">>]  
Commands:  
    multi_hset name key1 value1 key2 value2 ...  
    multi_hget name key1 key2 ...  


#### user info
TYPE: Hashmap  
Key: <<"users_phone_", Phone/binary>>  
Keys: [<<"name">>,  
       <<"id">>,  
       <<"password">>,  
       <<"salt">>,  
       <<"avatar">>]  
Commands:  
    multi_hset name key1 value1 key2 value2 ...  
    multi_hget name key1 key2 ...  