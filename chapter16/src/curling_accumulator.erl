-module(curling_accumulator).
-behavior(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {teams=orddict:new(), round=0}).

init([]) -> {ok, #state{}}.

handle_event({set_teams, TeamA, TeamB}, S=#state{teams=T}) ->
  Teams = orddict:store(TeamA, 0, orddict:store(TeamB, 0, T)),
  {ok, S#state{teams=Teams}};
handle_event({add_points, Team, N}, S=#state{teams=T}) ->
  Teams = orddict:update_counter(Team, N, T),
  {ok, S#state{teams=Teams}};
handle_event(next_round, S=#state{round=R}) ->
  {ok, S#state{round=R+1}};
handle_event(_Event, S) -> {ok, S}.

handle_call(game_data, S=#state{teams=T, round=R}) ->
  {ok, {orddict:to_list(T), {round, R}}, S};
handle_call(_, S) ->
  {ok, ok, S}.

handle_info(_, S) -> {ok, S}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
