# simple_im
#### I use toml as transmission protocol instead of xml or json.
#### Use ets table as session store.
#### Use postgresql as database for user & group information store.
#### The pg connection pool implement by myself instead of use poolboy, why? Because the [pg driver](https://github.com/epgsql/epgsql) allow concurrent pgsql:equery on same connection.
## Protocol
### Request:  
#### login:  
```toml
[r]
id="a_01"
t="login"
[r.user]
id=1
device="ipad"
token="AAklxuC39JJtttUMwKHq3teKwOzWtmJc"
```
#### reconnect:  
```toml
[r]
id="b_01"
t="reconnect"
[r.user]
id=1
device="ipad"
token="AAklxuC39JJtttUMwKHq3teKwOzWtmJc"
```
### Request Response:  
#### login:  
failed(s means status, value 1 is failed):
```toml
[rr]
id="a_01"
s=1
t="login"
r="token error"
```
success(s means status, value 0 is success):
```toml
[rr]
id="a_01"
t="login"
s=0
```
#### reconnect:  
failed(s means status, value 1 is failed):
```toml
[rr]
id="b_01"
t="reconnect"
r="token error"
s=1
```
success(s means status, value 0 is success):
```toml
[rr]
id="b_01"
t="reconnect"
s=0
```
### Message:  
```toml
[m]
id="a_02"
c="hello"
[m.from]
id=1
device="android"
[m.to]
id=2
device="ipad"
```
### Ack:
```toml
[a]
id="a_02"
```
### Group Message:  
```toml
[gm]
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
curl --data-urlencode "phone=18501260698&password=888888" http://simple_im.com/server/login
#### reconnect:
curl --data-urlencode "id=1&token=AAklxuC39JJtttUMwKHq3teKwOzWtmJc" http://localhost:8080/server/reconnect


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
#### 4.Start project:  
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
#### 5.Download [simple_im_client](https://github.com/wudixiaotie/simple_im_client), go to its root path:
change the test user info of the client at client_manager.erl then in a new shell type 
```shell
./start.sh a
```
to start the client.  
#### 6.See the server and client log.
```log
Got r id=<<"a_01">>
Got r id=<<"a_01">>
Got msg id=<<"a_02">>
```


# Redis Data Structure
### Offline message
Type: LIST
Key: <<"offline_", UserId/binary>>
Value: Msg
### Relationship between client and node
Type: HASH
Key: <<"client_", Token/binary>>
Value: [<<"ip">>, Ip,<<"port">>, Port, <<"id">>, UserId]

# TODO List:
1. build a http server for most of the request.Like ask for server ip&port, login, reconnect.
2. add time to message