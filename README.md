# simple_im
## I use toml as transmission protocol instead of xml or json.
## Use postgresql as database for user & group information store.
## The pg connection pool implement by myself instead of use poolboy, why? Because the [pg driver](https://github.com/epgsql/epgsql) allow concurrent pgsql:equery on same connection.
### Protocol
Request:  
```toml
[r]
id="a_01"
c="login"
[r.user]
phone="13812341234"
password="888888"
device="ipad"
```
Request Response:  
failed(s means status, value 1 is failed):
```toml
[rr]
id="a_01"
s=1
c="password not match"
```
success(s means status, value 0 is success):
```toml
[rr]
id="a_01"
s=0
user_id=1
```
Message:  
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
Ack:
```toml
[a]
id="a_02"
```
Group Message:  
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

## Use ets table as session store

# Guide
## Start Simple Im
1.Create database tables, execute .sql files in db/postgresql.  
2.Run
```shell
./start.sh n1
```
to open the console, run users:create/3, groups:create/3 to create some users and groups for test.
Then quite by type q().
3.Go to the root path of this project.   
4.Make sure your ip address is the ip variable of the file start.sh, then run
```shell
./start.sh n1 a
```
to start simple im. If you want to start observer, use
```shell
./start.sh n1 ao
```
5.Download [simple_im_client](https://github.com/wudixiaotie/simple_im_client), go to its root path:
change the test user info of the client at client_manager.erl then in a new shell type 
```shell
./start.sh a
```
to start the client.
6.See the server and client log.
```log
Got r id=<<"a_01">>
Got r id=<<"a_01">>
Got msg id=<<"a_02">>
```


# TODO List:
1. finish Users DB Module
2. finish Groups DB Module
3. add time to message