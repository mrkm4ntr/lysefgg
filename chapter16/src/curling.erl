-module(curling).
-export([start_link/2, set_teams/2, add_points/2, next_round/0,
  join_feed/1, leave_feed/1, game_info/0]).

start_link(TeamA, TeamB) ->
  {ok, Pid} = gen_event:start_link({local, ?MODULE}),
  gen_event:add_handler(Pid, curling_scoreboard, []),
  gen_event:add_handler(Pid, curling_accumulator, []),
  set_teams(TeamA, TeamB),
  ok.

set_teams(TeamA, TeamB) ->
  gen_event:notify(?MODULE, {set_teams, TeamA, TeamB}).

add_points(Team, N) ->
  gen_event:notify(?MODULE, {add_points, Team, N}).

next_round() ->
  gen_event:notify(?MODULE, next_round).

join_feed(Pid) ->
  HandlerId = {curling_feed, make_ref()},
  gen_event:add_handler(?MODULE, HandlerId, [Pid]),
  HandlerId.

leave_feed(HandlerId) ->
  gen_event:delete_handler(?MODULE, HandlerId, leave_feed).

game_info() ->
  gen_event:call(?MODULE, curling_accumulator, game_data).
