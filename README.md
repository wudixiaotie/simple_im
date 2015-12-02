# simple_im
## Erlang/OTP Version: 18

1. Message transform form one client to other client cost less than 300 microsecond.
2. I use toml as transmission protocol instead of xml or json. It is much faster than json.(I use my own toml helper for toml to erlang terms instead of kalta/etoml,because etoml is not so convenient, because it do not support Array of Tables, like <<"[[products]] sku = 738594937">>)
3. Use ets to store session, it is much faster than redis or ssdb or mnesia.
4. All slow request will handle by http service, IM service only do one thing: transmission message.
5. Less processes to carry more user, because for one user's multiple devices there will only be one process.
6. Use SSL/TLS for both HTTP and TCP connection by default.
7. Multiple listener for one IM node, make it easy to accept more clients for less time.
8. Reconnect will not create new process, but reuse the old one.


### Get started: [get started](https://github.com/wudixiaotie/simple_im/wiki/8.Getting-started).
### Wiki: [simple_im wiki](https://github.com/wudixiaotie/simple_im/wiki).