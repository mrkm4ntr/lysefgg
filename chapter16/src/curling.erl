-module(curling).
-export([start_link/2, set_teams/2, add_points/2, next_round/0]).

start_link(TeamA, TeamB) ->
  {ok, Pid} = gen_event:start_link({local, ?MODULE}),
  gen_event:add_handler(Pid, curling_scoreboard, []),
  set_teams(TeamA, TeamB),
  ok.

set_teams(TeamA, TeamB) ->
  gen_event:notify(?MODULE, {set_teams, TeamA, TeamB}).

add_points(Team, N) ->
  gen_event:notify(?MODULE, {add_points, Team, N}).

next_round() ->
  gen_even:notify(next_round).
