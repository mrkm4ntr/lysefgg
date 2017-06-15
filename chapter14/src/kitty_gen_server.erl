-module(kitty_gen_server).
-behaviour(gen_server).

-export([start_link/0, order_cat/3, return_cat/1, close_shop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(cat, {name, color=green, description}).

%%% Client functions
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

order_cat(Name, Color, Description) ->
  gen_server:call(?MODULE, {order, Name, Color, Description}).

return_cat(Cat) ->
  gen_server:cast(?MODULE, {return, Cat}).

close_shop() ->
  gen_server:call(?MODULE, terminate).

%%% Callback functions
init([]) -> {ok, []}.

handle_call({order, Name, Color, Description}, _From, Cats) ->
  if Cats =:= [] ->
       {reply, make_cat(Name, Color, Description), Cats};
     Cats =/= [] ->
       {reply, hd(Cats), tl(Cats)}
  end;

handle_call(shutdown, _From, Cats) ->
  {reply, ok, Cats}.

handle_cast({return, Cat}, Cats) ->
  {noreply, [Cat | Cats]}.

terminate(shutdown, Cats) ->
  [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
  ok;

terminate(normal, _Cats) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_info(_Msg, Cats) ->
  {noreply, Cats}.

make_cat(Name, Color, Description) ->
  #cat{name=Name, color=Color, description=Description}.
