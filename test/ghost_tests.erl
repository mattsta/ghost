-module(ghost_tests).

-include_lib("eunit/include/eunit.hrl").

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
ghost_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
     {"Create Objects",
       fun create_objects/0},
     {"Vote",
       fun vote/0},
     {"Highest Ranked Child",
       fun highest_ranked_child/0},
     {"All Children",
       fun all_children/0},
     {"Complete Tree",
       fun complete_tree/0}
    ]
  }.

%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------
create_objects() ->
  AtoZ = lists:seq($A, $Z),
  % Generate keyA - keyZ
  Keys = [<<"key", KeyId>> || KeyId <- AtoZ],
  Vals = [<<"val", KeyId>> || KeyId <- AtoZ],
  Combined = lists:zip(Keys, Vals),
  KeysB = [<<"keyB", KeyId>> || KeyId <- AtoZ],
  ValsB = [<<"valB", KeyId>> || KeyId <- AtoZ],
  ghost:object_create(tester, root, <<"nil">>),
  [ghost:object_create(tester, Key, Val) || {Key, Val} <- Combined],
  [ghost:object_create(tester, KeyB, ValB) || KeyB <- KeysB,
                                              ValB <- ValsB],
  [ghost:object_parent(tester, root, Key) || Key <- Keys],
  [ghost:object_parent(tester, Key, KeyB) || Key <- Keys, KeyB <- KeysB],

  ?assertMatch({error, object_id_already_exists, keyZ},
   ghost:object_create(tester, keyZ, nil)).

vote() ->
  ghost:vote(tester, up, keyB, keyBQ, mateo),
  ?assertEqual(<<"2">>, ghost:vote_total(tester, keyB, keyBQ)).

highest_ranked_child() ->
  % In vote/0, we upvote keyBQ with parent of keyB.  So, the largest
  % child of keyB should now be keyBQ
  Got = ghost:object_top_n_children(tester, keyB, 1),
  ?assertEqual([{<<"keyBQ">>, <<"2">>}], Got).

all_children() ->
  ok.

complete_tree() ->
  ok.

%%%----------------------------------------------------------------------
%%% Setup / Cleanup
%%%----------------------------------------------------------------------
setup() ->
  application:start(er),
  er_pool:start_link(tester, "127.0.0.1", 9961),
  er:flushall(tester).

teardown(_) ->
  ok.
