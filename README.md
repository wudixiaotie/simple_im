# simple_im
## I use toml as transmission protocol instead of xml or json.
### Protocol
Request:  
```toml
[r]
id="a_01"
c="login"
userid="xiaotie"
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
from="1@android"
to="2@ipad"
```
Ack:
```toml
[a]
id="a_02"
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