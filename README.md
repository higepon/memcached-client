### What is this?
A memcached client library for Erlang.
All memcached commannds (including cas) are supported.

### Building and Installing 

  1. Install a recent version of Erlang.
  2. Download [memcached-client-0.0.1.tar.gz](http://cloud.github.com/downloads/higepon/memcached-client/memcached-client-0.0.1.tar.gz).
  3. make
  4. make check (memcached required)
  5. make install

### Functions
- connect/2, disconnect/1
- functions: store/restore Erlang term
  - set/3, set/5
  - get/2, gets/2
  - get_multi/2, gets_multi/2
  - replace/3, replace/5
  - add/3, add/5
  - cas/6
- functions: store/replace binary data
  - setb/3, setb/5
  - getb/2, getsb/2
  - get_multib/2, gets_multib/2
  - replaceb/3, replaceb/5
  - addb/3, addb/5
  - append/3, prepend/3
  - casb/6
- incr/3, decr/3
- delete/2
- stats/1
- flush_all/1, flush_all/2
- version/1
- quit/1

### How to use

    1> {ok, Conn} = memcached:connect("127.0.0.1", 11211).
    {ok,<0.40.0>}
    2> ok = memcached:set(Conn, "Hello", "World").
    ok
    3> ok = memcached:set(Conn, "Say", "Goodbye").
    ok
    4> {ok, [{"Hello", "World"}, {"Say", "Goodbye"}]} = memcached:get_multi(Conn, ["Hello", "Say"]).
    {ok,[{"Hello","World"},{"Say","Goodbye"}]}
    5> ok = memcached:delete(Conn, "Say").
    ok
    6> {error, not_found} = memcached:get(Conn, "Say").
    {error,not_found}
    7> ok = memcached:setb(Conn, "mydata", <<10:64/little>>).
    ok
    8> memcached:getb(Conn, "mydata").
    {ok,<<10,0,0,0,0,0,0,0>>}
    9> ok = memcached:disconnect(Conn).
    ok  




See more examples on [memcached_SUITE.erl](http://github.com/higepon/memcached-client/blob/master/test/memcached_SUITE.erl).

### ToDo
- Mulitple servers with user defined Fun.
- documentation

### How To Contribute
If you would like to contribute, first check out the source.
Then modify the markdown files with your improvements, commit them and either send me a patch, or push the changes into a fork of the repo and send me a pull request.

### Contributors
- [volutas](http://twitter.com/voluntas)
