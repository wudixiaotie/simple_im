# simple_im
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
![Login](https://raw.githubusercontent.com/wudixiaotie/simple_im/master/assets/login_reconnect.png)  

1. [Http login request](#http_login_request), client authenticate with phone&password and ask for IM server.
2. [Http login response](#http_login_response), got id, token, IM server ip&port.
3. [IM login request](#im_login_request), connect IM server with id and device and token.
4. [IM login response](#im_login_request), got response.

### Reconnect:
![Reconnect](https://raw.githubusercontent.com/wudixiaotie/simple_im/master/assets/login_reconnect.png)  

1. [Http reconnect request](#http_reconnect_request), client authenticate with id&token and ask for IM server.
2. [Http reconnect response](#http_reconnect_response), got IM server ip&port.
3. [IM reconnect request](#im_reconnect_request), connect IM server with id and device and token.
4. [IM reconnect response](#im_reconnect_response), got response.

## Protocol
### Request:  
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
#### <a name="im_reconnect_request">reconnect</a>:  
```toml
[[r]]
id="b_01"
t="reconnect"
[r.user]
id=1
device="ipad"
token="AAklxuC39JJtttUMwKHq3teKwOzWtmJc"
```
### Request Response:  
#### <a name="im_login_response">login</a>:  
failed(s means status, value 1 is failed):
```toml
[[rr]]
id="a_01"
s=1
t="login"
r="token error"
```
success(s means status, value 0 is success):
```toml
[[rr]]
id="a_01"
t="login"
s=0
```
#### <a name="im_reconnect_response">reconnect</a>:  
failed(s means status, value 1 is failed):
```toml
[[rr]]
id="b_01"
t="reconnect"
r="token error"
s=1
```
success(s means status, value 0 is success):
```toml
[[rr]]
id="b_01"
t="reconnect"
s=0
```
### Message:  
```toml
[[m]]
id="a_02"
c="hello"
[m.from]
id=1
device="android"
[m.to]
id=2
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
[gm.user]
id=1
device="android"
[gm.group]
id=123
```

## Http request:
### Ask for which node to login/reconnect

#### login:
##### <a name="http_login_request">request</a>:
curl -d "phone=18501260698" --data-urlencode "password=888888" http://localhost:8080/server/login
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
curl -d "id=1" --data-urlencode "token=rylFLDGW4NN0h4leO97O/Gibar8KQS8l" http://localhost:8080/server/reconnect
##### <a name="http_reconnect_response">response</a>:
```toml
[[response]]
status = 0
server = "192.168.3.5"
port = "1987"
```

### report IM offline and get new IM service IP&Port
##### request:
curl --data-urlencode "token=CVT1Y6M00u6OO25TJNYCt3VNff8Khlm3" "http://localhost:8080/server/failed"
##### response:
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

### Get offline message
##### request:
curl -d "user_id=1" "http://localhost:8080/offline"
##### response:
```toml
[[m]] id="a_02" c="hello" [m.from] id=2 device="android" [m.to] id=1
[[m]] id="b_02" c="hello" [m.from] id=3 device="android" [m.to] id=1
```


# Guide
## Start Simple Im
#### 1.Download the porject, and go to the root path of this porject.
#### 2.Create database tables, execute .sql files in db/postgresql.  
#### 3.Create test data.  
Run
```shell
./start.sh n1
```
to open the console, run users:create/3, groups:create/3 to create some users and groups for test.
Then quite by type q().
#### 3.Add dns of simple_im.com:    
Edit /etc/hosts file, add line of 'your IP address   simple_im.com', make sure your ip address is the real ip, not localhost or 127.0.0.1  
#### 4.Start IM server:  
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
#### 5.Start HTTP server:  
```shell
./start.sh http
```
#### 6.Download [simple_im_client](https://github.com/wudixiaotie/simple_im_client), go to its root path:
change the test user info of the client at client_manager.erl then in a new shell type 
```shell
./start.sh a
```
to start the client.  
#### 7.See the server and client log.
```log
Got r id=<<"a_01">>
Got r id=<<"a_01">>
Got msg id=<<"a_02">>
```


# Redis Data Structure
### Offline message
Type: LIST  
Key: <<"offline_", UserId/binary>>  
Value: MsgIdList  

Type: STRING  
Key: MsgId  
Value: MsgBin  
### Relationship between client and node
Type: HASH  
Key: <<"client_", Token/binary>>  
Value: [<<"ip">>, Ip,<<"port">>, Port, <<"user_id">>, UserId]

# TODO List:
1. Add friend
2. Add group