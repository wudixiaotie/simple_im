# simple_im
## I use toml as transmission protocol instead of xml or json.
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
1.Go to the root path of this project.   
2.Run
```shell
./start.sh n1 a
```
to start simple im. If you want to start observer, use
```shell
./start.sh n1 ao
```.
3.Start client