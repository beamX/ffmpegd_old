-module(data_handler).

-export([write_to_file/1]).



write_to_file(M3U8Part) ->
    #{part := Part, name := Name,
      m3u8 := _MetaData} = M3U8Part,
    file:write_file(<<"/tmp/", Name/bitstring>>, Part).


