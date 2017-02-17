FFMPEG tcp stream pipeline
======================

Erlang implementation for handling a tcp stream of a transcoded file using ffmpeg.

## Usage

start a worker process for listening to the transcoded stream
```erl
1> {ok, Port} = start_m3u8_worker:start_m3u8_worker(my_module, upload_to_s3, [{key1, val1}]).
 {ok, 50000}
```

 `start_m3u8_worker` takes 3 arguments: `module name`, `function name` and `function args`. The moment the worker process starts receiving parts it will call the module function and pass it the arguments along with a map :
 ```erl
 #{part => PartData,
   name => PartName,
   m3u8 => M3U8}
 ```

Now use ffmpeg to tanscode the file and redirect the output to the tcp port returned by executing the above function
```bash
ffmpeg -i file.mkv -f hls tcp://localhost:10009/feed1.ffm
```


## example usage
Let us assume we wish to write the parts to a file, so the following can be a function:
```erl
write_to_file(M3U8Part) ->
    #{part := Part, name := Name,
      m3u8 := _MetaData} = M3U8Part,
    file:write_file(<<"/tmp/", Name/bitstring>>, Part).
```

The function can now be passed to the worker which will later on call it to store the data
```erl
1> {ok, Port} = start_m3u8_worker:start_m3u8_worker(my_module, write_to_file, []).
 {ok, 50000}
```