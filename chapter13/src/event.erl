-module(event).
-compile(export_all).
-record(state, {server, name="", to_go=0}).

normalize(N) ->
  Limit = 49*24*60*60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

loop(S = #state{server=Server, to_go=[T|Next]}) ->
  receive {Server, Ref, cancel} ->
    Server ! {Ref, ok}
  after T * 1000 ->
    if Next =:= [] -> Server ! {done, S#state.name};
       Next =/= [] -> loop(S#state{to_go=Next})
    end
  end.

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, EventName, DateTime) ->
  loop(#state{server=Server, name=EventName, to_go=time_to_go(DateTime)}).

cancel(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.

time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_secondes(TimeOut) - calendar:datetime_to_gregorian_secondes(Now),
  Secs = if ToGo > 0 -> ToGo;
            ToGo =< 0 -> 0
         end,
  normalize(Secs).
