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
为什么不用一个进程和客户端进行交互呢，因为我要定义心跳超时，当心跳超时的时候receiver就挂掉了，但是如果receiver和client变成一个进程的时候这个进程就会频繁的收到非客户端的消息，例如其他人给他发的消息，这样timeout就会被频繁的重置，变得没有意义。