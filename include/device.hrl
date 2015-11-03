-record (device, {name :: binary(),
                  socket :: port(),
                  token :: binary(),
                  msg_cache = []}).