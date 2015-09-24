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
phone="13812341234"
password="888888"
device="ipad"
```
#### reconnect:  
```toml
[r]
id="b_01"
t="reconnect"
[r.user]
id=1
device="android"
token="w5Y2B+Y5eXNxS1t4"
```
### Request Response:  
#### login:  
failed(s means status, value 1 is failed):
```toml
[rr]
id="a_01"
s=1
t="login"
r="password not match"
```
success(s means status, value 0 is success):
```toml
[rr]
id="a_01"
t="login"
s=0
[rr.user]
id=1
token="w5Y2B+Y5eXNxS1t4"
```
#### reconnect:  
failed(s means status, value 1 is failed):
```toml
[rr]
id="b_01"
t="reconnect"
r="offline"
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


# TODO List:
1. build a http server for most of the request.Like ask for server ip&port, login, reconnect.
2. add time to message