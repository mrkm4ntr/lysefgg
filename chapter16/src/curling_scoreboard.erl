-module(curling_scoreboard).
-behavior(gen_event).

-compile(export_all).

init([]) ->
  {ok, []}.

handle_event({set_teams, TeamA, TeamB}, State) ->
  curling_scoreboard_hw:set_teams(TeamA, TeamB),
  {ok, State};
handle_event({add_points, Team, N}, State) ->
  [curling_scoreboard_hw:add_point(Team) || _ <- lists:seq(1, N)],
  {ok, State};
handle_event(next_round, State) ->
  curling_scoreboard_hw:next_round(),
  {ok, State}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.
