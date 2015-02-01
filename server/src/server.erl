-module(server).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-export([start/0]).

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),

    Port = 8888,
    Routes = cowboy_router:compile(routes()),
    cowboy:start_http(http, 100,
                      [{port, Port}],
                      [{env, [{dispatch, Routes}]}]),
    io:format("cowboy started at port ~p~n", [Port]),
    ok.

routes()->
    Root = "../",
    [{'_',
      [
       {"/", cowboy_static, {file, Root ++ "index.html", [{mimetypes, cow_mimetypes, all}]}},
       {"/[...]", cowboy_static, {dir, Root, [{mimetypes, cow_mimetypes, all}]}}
      ]}].
