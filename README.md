# simple_im
## I use toml as transmission protocol instead of xml or json.
### Protocol
Message:  
```toml
[msg]
id="a_01"
mc="hello"
from="1@android"
to="2@ipad"
```
Ack:
```toml
[ack]
id="a_01"
```