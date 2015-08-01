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

## Use global_name instead of mnesia session table