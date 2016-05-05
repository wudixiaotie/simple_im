# simple_im
## Erlang/OTP Version: 18
## Rebar Version: [rebar3](https://www.rebar3.org/)

0. Custom behaviour gen_msg more faster than gen_server.
1. Message transform form one client to other client cost less than 300 microsecond.
2. I use toml as transmission protocol instead of xml or json. It is much faster than json or xml(I use my own toml helper for toml to erlang terms instead of kalta/etoml, because it do not support Array of Tables, like <<"[[products]] sku = 738594937">>, and faster than etoml), contain more meanings in smaller size of binary.
3. Use dets to store session, it is much faster than redis or ssdb, much faster than mnesia when multiple nodes are connected.
4. All slow request will handle by http service, IM service only do one thing: transmission message.
5. Less processes to carry more user, because there is only be one process for one user's multiple devices.
6. Use SSL/TLS for both HTTP and TCP connection by default.
7. Multiple listener for one IM node, make it easy to accept more clients in less time.
8. Reconnect will not create new process, but reuse the old one.


### Get started: [get started](https://github.com/wudixiaotie/simple_im/wiki/8.Getting-started).
### Wiki: [simple_im wiki](https://github.com/wudixiaotie/simple_im/wiki).
### Production Release: [Release it](https://github.com/wudixiaotie/simple_im/wiki/9.Release-it%EF%BC%81).
### My Blog: [Big If](https://wudixiaotie.github.io)