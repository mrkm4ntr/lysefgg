-module(kitty_server2).
-compile(export_all).

-record(cat, {name, color=green, description}).

%%% Client API
start_link() -> spawn_link(?MODULE, init, []).

order_cat(Pid, Name, Color, Description) ->
  my_server:call(Pid, {order, Name, Color, Description}).

return_cat(Pid, Cat = #cat{}) ->
  Pid ! {return, Cat},
  ok.

close_shop(Pid) ->
  my_server:call(Pid, terminate).

%%% Server functions
init() -> loop([]).

loop(Cats) ->
  receive
    {Pid, Ref, {order, Name, Color, Description}} ->
      if Cats =:= [] ->
          Pid ! {Ref, make_cat(Name, Color, Description)},
          loop(Cats);
         Cats =/= [] ->
           Pid ! {Ref, hd(Cats)},
           loop(tl(Cats))
      end;
    {return, Cat = #cat{}} ->
      loop([Cat | Cats]);
    {Pid, Ref, terminate} ->
      Pid ! {Ref, ok},
      terminate(Cats);
    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(Cats)
  end.

make_cat(Name, Color, Description) -> #cat{name=Name, color=Color, description=Description}.

terminate(Cats) ->
  [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
  ok.
