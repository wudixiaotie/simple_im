-record (device, {name :: binary(),
                  ssl_socket,
                  token :: binary(),
                  msg_cache = []}).