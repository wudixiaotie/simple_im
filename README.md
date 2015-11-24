# simple_im

# Guide
## Start Simple Im
#### 1.Download the porject, and go to the root path of this porject.
#### 2.Create database tables,functions,data:  
Run
```shell
./start.sh task
```
to open the console, run db:init/0 to initialize tables,functions,data
```erlang
db:init()
```
Then quite by type q().
#### 3.Add dns of simple_im.com:    
Edit /etc/hosts file, add line of 'your IP address   simple_im.com', make sure your ip address is the real ip, not localhost or 127.0.0.1  
#### 4.Start Middleman server: 
```shell
./start.sh middleman
```
#### 5.Start IM server:  
For single node run
```shell
./start.sh n1 im
```
to start simple im. If you want to start observer, use
```shell
./start.sh n1 im o
```
For multiple nodes run
```shell
./start.sh n1 im
```
```shell
./start.sh n2 im
```
```shell
./start.sh n3 im
```
#### 6.Start HTTP server:  
```shell
./start.sh http
```
#### 7.Download [simple_im_client](https://github.com/wudixiaotie/simple_im_client), go to its root path:
change the test user info of the client at client_manager.erl then in a new shell type 
```shell
./start.sh a
```
to start the client.  
#### 8.See the server and client log.
```log
Got r id=<<"a_01">>
Got r id=<<"a_01">>
Got msg id=<<"a_02">>
```


### Message transform form one client to other client cost less than 300 microsecond.
### Erlang/OTP Version: 18
#### I use toml as transmission protocol instead of xml or json. I use my own toml helper for toml to erlang terms instead of kalta/etoml,because etoml is not so convenient, because it do not support Array of Tables, like <<"[[products]] sku = 738594937">>
#### Use ets table as session store.
#### Use postgresql as database for user & group information store.
#### The pg connection pool implement by myself instead of use poolboy, why? Because the [pg driver](https://github.com/epgsql/epgsql) allow concurrent pgsql:equery on same connection.
#### Use redis as cache store for :
1. online IM service IP&Port.
2. offline message.
3. client token & service relationship.

## Workflow
### Login:
![Login](https://raw.githubusercontent.com/wudixiaotie/simple_im/master/assets/login.png)  

1.  Login or Reconnect request:
     1. [Http login request](#http_login_request), client authenticate with phone&password and ask for IM server.
     2. [Http reconnect request](#http_reconnect_request), client authenticate with id&token and ask for IM server.
2.  Login or Reconnect response:
     1. [Http login response](#http_login_response), got id, token, IM server ip&port.
     2. [Http reconnect response](#http_reconnect_response), got IM server ip&port.
3. [Http get offline request](#http_get_offline_request), use token to get offline messages.
4. [Http get offline response](#http_get_offline_response), got offline messages.
5. [IM login request](#im_login_request), connect IM server with id and device and token.
6. [IM login response](#im_login_request), got response.
7. IM send msg_cache to client.
8. [Http clean offline request](#http_clean_offline_request), use token to delete offline messages.
9. [Http clean offline response](#http_clean_offline_response), delete offline messages.

### Create Contact:
![CreateContact](https://raw.githubusercontent.com/wudixiaotie/simple_im/master/assets/create_contact.png)  

0. Client A Search Contact:
     1. [Http Search Contact request](#http_find_user_by_phone_request).
     2. [Http Search Contact response](#http_find_user_by_phone_response).
1. Client A add Client B as contact thought http request:
     1. [HTTP add contact request](#http_add_contact_request).
     2. [HTTP add contact response](#http_add_contact_response).
2. Http server send notification to Middleman: [IM add contact notification](#im_add_contact_notification).
3. Middleman send notification to IM: [IM add contact notification](#im_add_contact_notification).
4. IM send notification to client B: [IM add contact notification](#im_add_contact_notification).
5. Client B accept Client A as contact thought http request:
     1. [HTTP accept contact request](#http_accept_contact_request).
     2. [HTTP accept contact response](#http_accept_contact_response).
6. Http server send notification to Middleman: [IM accept contact notification](#im_accept_contact_notification).
7. Middleman send notification to IM: [IM accept contact notification](#im_accept_contact_notification).
8. IM send notification to client B: [IM accept contact notification](#im_accept_contact_notification).


## Protocol
### Request & Response:  
#### <a name="im_login_request">login</a>:  
```toml
[[r]]
id="a_01"
t="login"
[r.user]
id=1
device="ipad"
token="AAklxuC39JJtttUMwKHq3teKwOzWtmJc"
```
#### <a name="im_login_response">login</a>:  
failed(s means status, value 1 is failed):
```toml
[[rr]]
id="a_01"
s=1
r="token error"
```
success(s means status, value 0 is success):
```toml
[[rr]]
id="a_01"
s=0
```

### Message:  
```toml
[[m]]
id="a_02"
c="hello"
to=2
```
### Ack:
```toml
[[a]]
id="a_02"
```
### Group Message:  
```toml
[[gm]]
id="a_02"
c="hello"
group=123
```
### Notification Message:  
```toml
[[n]]
id = "n_1448185739"
t="delete_group_member"
g_id = 2
gm_id = 2
ts = 1448185739
```

## Http request:
### Http server health
curl --cacert priv/ssl/cowboy-ca.crt -i https://localhost:8080/health

### Ask for which node to login/reconnect

#### login:
##### <a name="http_login_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X POST -d "phone=8618266175357" --data-urlencode "password=888888" -i https://localhost:8080/server/login
##### <a name="http_login_response">response</a>:
```toml
[[response]]
status = 0
server = "192.168.3.5"
port = "1987"
[response.user]
id = 1
token = "rylFLDGW4NN0h4leO97O/Gibar8KQS8l"
```

#### reconnect:
##### <a name="http_reconnect_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X POST -d "id=1" --cookie "token=3vPjUabByvMwBFR9tIeP0bDec4INGQ/T" -i https://localhost:8080/server/reconnect
##### <a name="http_reconnect_response">response</a>:
```toml
[[response]]
status = 0
server = "192.168.3.5"
port = "1987"
```

### report IM offline and get new IM service IP&Port
##### <a name="http_report_failed_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X POST --cookie "token=3vPjUabByvMwBFR9tIeP0bDec4INGQ/T" -i https://localhost:8080/server/failed
##### <a name="http_report_failed_response">response</a>:
```toml
[[response]]
status = 3
r = "IM is online"
```
```toml
[[response]]
status = 0
server = "192.168.3.5"
port = "1987"
```

### Offline message
#### Get offline message
##### <a name="http_get_offline_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X GET --cookie "token=7m2jbsYaGNoWT2JztF/Nrh3kQD/YNmDJ" -i https://localhost:8080/offline
##### <a name="http_get_offline_response">response</a>:
```toml
[[response]] status = 0
[[m]] c = "hello" id = "a_02" ts = 1444719450 [m.to] id = 3 [m.from] device = "android" id = 1
[[m]] c = "hello" id = "a_02" ts = 1444719450 [m.to] id = 3 [m.from] device = "android" id = 1
[[m]] c = "hello" id = "a_02" ts = 1444719450 [m.to] id = 3 [m.from] device = "android" id = 1
```
#### Clean offline message
##### <a name="http_clean_offline_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X DELETE --cookie "token=3vPjUabByvMwBFR9tIeP0bDec4INGQ/T" -i https://localhost:8080/offline
##### <a name="http_clean_offline_response">response</a>:
```toml
[[response]] status = 0
```

### Users
#### Find user by phone
##### <a name="http_find_user_by_phone_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X GET --cookie "token=3vPjUabByvMwBFR9tIeP0bDec4INGQ/T" -i https://localhost:8080/user/phone/8618266175357
##### <a name="http_find_user_by_phone_response">response</a>:
```toml
[[response]] status = 0
[[user]] id = 2 name = "xiaotie" phone = "8618266175357" avatar = ""
```
#### Find user by id
##### <a name="http_find_user_by_id_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X GET --cookie "token=3vPjUabByvMwBFR9tIeP0bDec4INGQ/T" -i https://localhost:8080/user/id/2
##### <a name="http_find_user_by_id_response">response</a>:
```toml
[[response]] status = 0
[[user]] id = 2 name = "xiaotie" phone = "8618266175357" avatar = ""
```
#### Create user
##### <a name="http_create_user_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X POST -d "phone=8618266175357" --data-urlencode "name=大傻" --data-urlencode "password=888888" -i https://localhost:8080/user
##### <a name="http_create_user_response">response</a>:
```toml
[[response]] status = 0
```

### Contacts
#### Get contacts
##### <a name="http_find_contacts_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X GET --cookie "token=3vPjUabByvMwBFR9tIeP0bDec4INGQ/T" -i https://localhost:8080/contact/version/0
##### <a name="http_find_contacts_response">response</a>:
```toml
[[response]] status = 0 version = 19
[[user]] id = 2 name = "xiaotie" phone = "8618266175357" avatar = ""
```
#### add contact
##### <a name="http_add_contact_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X POST --data-urlencode "ask=hello" --cookie "token=I0kUMlwgXy6DfFwd+1indo6+rsF+YJfd" -i https://localhost:8080/contact/2
##### <a name="http_add_contact_response">response</a>:
```toml
[[response]] status = 0
```
```toml
[[response]] status = 1 reason = "error reason"
```
##### <a name="im_add_contact_notification">add contact notification</a>:  
```toml
[[n]]
t="add_contact"
from=1
to=2
ask="hello"
ts = 1448185739
```
#### accept contact
##### <a name="http_accept_contact_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X PUT --cookie "token=2nL0Lqm/B77lzNWCfhfG1NWdTyEzlQ3U" -i https://localhost:8080/contact/1
##### <a name="http_accept_contact_response">response</a>:
```toml
[[response]] status = 0
```
```toml
[[response]] status = 1 reason = "error reason"
```
##### <a name="im_accept_contact_notification">accept contact notification</a>:  
```toml
[[n]]
t="accept_contact"
from=2
to=1
ts = 1448185739
```
#### delete contact
##### <a name="http_delete_contact_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X DELETE --cookie "token=I0kUMlwgXy6DfFwd+1indo6+rsF+YJfd" -i https://localhost:8080/contact/2
##### <a name="http_delete_contact_response">response</a>:
```toml
[[response]] status = 0
```
```toml
[[response]] status = 1 reason = "error reason"
```
##### <a name="im_delete_contact_notification">delete contact notification</a>:  
```toml
[[n]]
t="delete_contact"
from=1
to=2
ts = 1448185739
```

### Groups
#### create group
##### <a name="http_create_group_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X POST -d "members[]=2&members[]=3" --data-urlencode "name=group1" --cookie "token=I0kUMlwgXy6DfFwd+1indo6+rsF+YJfd" -i https://localhost:8080/group
##### <a name="http_create_group_response">response</a>:
```toml
[[response]] status = 0
```
```toml
[[response]] status = 1 reason = "error reason"
```
##### <a name="im_create_group_notification">create group notification</a>:  
```toml
[[n]]
t="create_group"
g_id = 9
g_key = "PLwXRfpe7zFl3FWN"
ts = 1448185739
```
#### delete group
##### <a name="http_delete_group_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X DELETE --cookie "token=I0kUMlwgXy6DfFwd+1indo6+rsF+YJfd" -i https://localhost:8080/group/1
##### <a name="http_delete_group_response">response</a>:
```toml
[[response]] status = 0
```
```toml
[[response]] status = 1 reason = "error reason"
```
##### <a name="im_delete_group_notification">delete group notification</a>:  
```toml
[[n]]
t="delete_group"
g_id = 1
ts = 1448185739
```
#### create group member
##### <a name="http_create_group_member_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X POST --data-urlencode "g_key=PLwXRfpe7zFl3FWN" --cookie "token=2nL0Lqm/B77lzNWCfhfG1NWdTyEzlQ3U" -i https://localhost:8080/group/2/member
curl --cacert priv/ssl/cowboy-ca.crt -X POST --cookie "token=I0kUMlwgXy6DfFwd+1indo6+rsF+YJfd" -i https://localhost:8080/group/2/member/2
##### <a name="http_create_group_member_response">response</a>:
```toml
[[response]] status = 0
```
```toml
[[response]] status = 1 reason = "error reason"
```
##### <a name="im_create_group_notification">create group member notification</a>:  
```toml
[[n]]
t="create_group_member"
g_id = 2
gm_id = 2
ts = 1448185739
```
#### delete group member
##### <a name="http_delete_group_member_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X DELETE --cookie "token=eRooKi4EFYz6YYx0PWM86cQQzK0MejNI" -i https://localhost:8080/group/2/member
##### <a name="http_delete_group_member_response">response</a>:
```toml
[[response]] status = 0
```
```toml
[[response]] status = 1 reason = "error reason"
```
##### <a name="im_delete_group_notification">delete group member notification</a>:  
```toml
[[n]]
t="delete_group_member"
g_id = 2
gm_id = 2
ts = 1448185739
```

### Upload
##### <a name="http_upload_audio_request">request</a>:
curl --cacert priv/ssl/cowboy-ca.crt -X POST --cookie "token=MYNvES7FEIbciYGgcmu0YfyZkodY+kzd" -H "Content-Type=audio/AMR" -F "filename=@assets/10010.amr" -i https://localhost:8080/upload/audio


# Redis Data Structure
### Offline message
Type: LIST  
Key: <<"offline_", UserId/binary>>  
Value: [<<"offline_", UserIdBin/binary, "_", MsgId>>, ..]  

Type: STRING  
Key: <<"offline_", UserIdBin/binary, "_", MsgId>>  
Value: MsgBin  
### Relationship between client and node
Type: HASH  
Key: <<"client_", Token/binary>>  
Value: [<<"ip">>, Ip,<<"port">>, Port, <<"user_id">>, UserId]


# Speed of toml and jsx:
### jsx:
#### jsx:decode(<<"{\"r\": {\"id\": \"c_01\", \"t\": \"create_group\", \"name\": \"fuck\", \"members\": [2,3]}}">>).
```shell
=====================  
execute [1000] times of {jsx, decode, [<<"{\"r\": {\"id\": \"c_01\", \"t\": \"create_group\", \"name\": \"fuck\", \"members\": [2,3]}}">>]}:  
Maximum: 65(μs) 6.5e-5(s)  
Minimum: 22(μs) 2.2e-5(s)  
Sum: 27786(μs)  0.027786(s)  
Average: 27.786(μs)     2.7786e-5(s)  
Greater: 432  
Less: 568  
=====================
```
### toml:
#### toml:binary_2_term(<<"[[r]] id=\"c_01\" t=\"create_group\" name=\"fuck\" members=[2,3]">>).
```shell
=====================  
execute [1000] times of {toml, binary_2_term, [<<"[[r]] id=\"c_01\" t=\"create_group\" name=\"fuck\" members=[2,3]">>]}:  
Maximum: 65(μs) 6.5e-5(s)  
Minimum: 6(μs)  6.0e-6(s)  
Sum: 7381(μs)   0.007381(s)  
Average: 7.381(μs)      7.381e-6(s)  
Greater: 289  
Less: 711  
=====================
```