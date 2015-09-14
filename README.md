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
id="xiaotie"
device="ipad"
```
Request Response:  
```toml
[rr]
id="a_01"
c="login fail: Unknow Request"
```
Message:  
```toml
[m]
id="a_02"
c="hello"
[m.from]
id="1"
device="android"
[m.to]
id="2"
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
id="1"
device="android"
[gm.group]
id="123"
```

## Use ets table as session store

# Guide
## Start Simple Im
1.Create database tables, execute .sql files in db/postgresql.  
2.Go to the root path of this project.   
3.Run
```shell
./start.sh n1 a
```
to start simple im. If you want to start observer, use
```shell
./start.sh n1 ao
```
4.Start [simple_im_client](https://github.com/wudixiaotie/simple_im_client), go to its root path: 
```shell
./start.sh a
```
5.See the server and client log.
```log
Got r id=<<"a_01">>
Got r id=<<"a_01">>
Got msg id=<<"a_02">>
```


# TODO List:
1. Users DB Module
2. Groups DB Module
3. User password encryption