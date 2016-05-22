#### group info
TYPE: Hashmap  
Name: <<"groups_", GroupIdBin/binary>>  
Key: [<<"name">>,  
      <<"creator_id">>,  
      <<"key">>,  
      <<"created_at">>]  
Commands:  
    hkeys name '' '' -1  
    hset name key value  
    hclear name  


#### group info
TYPE: Sorted Set  
Name: <<"user_groups_", UserIdBin/binary>>  
Key: GroupIdBin  
Score: CreatedAt  
Commands:  
    zset name key score  
    zrscan name '' '' '' -1  
    zdel name key  