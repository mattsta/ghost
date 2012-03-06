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
     {"Create Discussion Tree",
       fun create_discussion_tree/0},
     {"Vote",
       fun vote/0},
     {"Highest Ranked Child",
       fun highest_ranked_child/0},
     {"All Children",
       fun all_children/0},
     {"Complete Tree DAG",
       fun complete_tree_dag/0},
     {"Complete Tree Cyclic",
       fun complete_tree_cyclic/0}
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

create_discussion_tree() ->
  ghost:object_create(tester, a1, <<"a1">>),
  ghost:object_create(tester, a2, <<"a2">>),
  ghost:object_create(tester, a3, <<"a3">>),
  ghost:object_create(tester, a4, <<"a4">>),
  ghost:object_create(tester, b1, <<"a1">>),
  ghost:object_create(tester, b2, <<"b2">>),
  ghost:object_create(tester, b3, <<"b3">>),
  ghost:object_create(tester, c1, <<"c1">>),
  ghost:object_create(tester, c2, <<"c2">>),
  ghost:object_create(tester, c3, <<"c3">>),
  % Arrange: a2 -> {b3, b1, b2}
  %                         b2 -> {c2, c1, c3}
  ghost:object_parent(tester, a2, b1),
  ghost:object_parent(tester, a2, b2),
  ghost:object_parent(tester, a2, b3),
  ghost:object_parent(tester, b2, c1),
  ghost:object_parent(tester, b2, c2),
  ghost:object_parent(tester, b2, c3),
  ghost:object_parent(tester, b2, cNotAnObject),
  ghost:vote(tester, up, a2, b3, masterA),
  ghost:vote(tester, up, a2, b3, masterA),
  ghost:vote(tester, up, a2, b2, masterA),
  ghost:vote(tester, up, b2, c2, masterB),
  ghost:vote(tester, up, b2, c2, masterB),
  ghost:vote(tester, up, b2, c1, masterB).

vote() ->
  ghost:vote(tester, up, keyB, keyBQ, mateo),
  ?assertEqual(<<"2">>, ghost:vote_total(tester, keyB, keyBQ)).

highest_ranked_child() ->
  % In vote/0, we upvote keyBQ with parent of keyB.  So, the largest
  % child of keyB should now be keyBQ
  Got = ghost:object_top_n_children(tester, keyB, 1),
  ?assertEqual([{<<"keyBQ">>, <<"2">>}], Got).

all_children() ->
  AChilds = ghost:object_children(tester, a2),
  ?assertEqual([{<<"b3">>, <<"3">>},
                {<<"b2">>, <<"2">>},
                {<<"b1">>, <<"1">>}], AChilds),
  BChilds = ghost:object_children(tester, b2),
  ?assertEqual([{<<"c2">>, <<"3">>},
                {<<"c1">>, <<"2">>},
                {<<"cNotAnObject">>, <<"1">>},
                {<<"c3">>, <<"1">>}], BChilds).

complete_tree_dag() ->
  Got = ghost:object_resolve_to_height(tester, a2, 15),
  ?assertEqual([{<<"b3">>,<<"3">>,[]},
                {<<"b2">>,<<"2">>,
                 [{<<"c2">>,<<"3">>,[]},
                  {<<"c1">>,<<"2">>,[]},
                  {<<"cNotAnObject">>, <<"1">>, []},
                  {<<"c3">>,<<"1">>,[]}]},
                {<<"b1">>,<<"1">>,[]}], Got).

complete_tree_cyclic() ->
  % Add cycle from c3 -> b2:
  ghost:object_parent(tester, c3, b2),
  Got = ghost:object_resolve_to_height(tester, a2, 15),
  ?assertEqual([{<<"b3">>,<<"3">>,[]},
                {<<"b2">>,<<"2">>,
                 [{<<"c2">>,<<"3">>,[]},
                  {<<"c1">>,<<"2">>,[]},
                  {<<"cNotAnObject">>, <<"1">>, []},
                  {<<"c3">>,<<"1">>,
                   % this b2 is one because counts/votes/weights are
                   % PER parent-child PAIR -- not per object entirely.
                   [{<<"b2">>, <<"1">>, cycle}]}]},
                {<<"b1">>,<<"1">>,[]}], Got).

%%%----------------------------------------------------------------------
%%% Setup / Cleanup
%%%----------------------------------------------------------------------
setup() ->
  application:start(er),
  er_pool:start_link(tester, "127.0.0.1", 9961),
  er:flushall(tester).

teardown(_) ->
  ok.
