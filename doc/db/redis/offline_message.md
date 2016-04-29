#### Offline message index
TYPE: List  
Key: <<"offline_", UserId/binary>>  
Value: [<<"offline_", UserIdBin/binary, "_", MsgId>>, ..]  


#### Offline message content
Type: STRING  
Key: <<"offline_", UserIdBin/binary, "_", MsgId>>  
Value: MsgBin   