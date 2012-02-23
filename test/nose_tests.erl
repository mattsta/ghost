-module(nose_tests).

-include_lib("eunit/include/eunit.hrl").

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
nose_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
     {"Object Create",
       fun create_objects/0},
     {"Object Update",
       fun update_objects/0},
     {"Tags",
       fun tags/0},
     {"Name Updating",
       fun update_names/0},
     {"Owners",
       fun find_owners/0}
    ]
  }.

%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------
create_objects() ->
  CreatedHash = rnose:'object-create'(course, testingName, mattUid, 
                                      [k1, v1, k2, v2, k3, v3]),
  ?assertEqual(1, CreatedHash),
  KVals = rnose:'object-all'(course, 1),
  ?assertEqual([{k1, <<"v1">>},
                {k2, <<"v2">>},
                {k3, <<"v3">>},
                {name, <<"testingName">>}], KVals),
  NameTo = rnose:'name-target'(testingName),
  ?assertEqual(<<"course:1">>, NameTo),
  CreatedName = rnose:'object-field'(course, 1, name),
  ?assertEqual(<<"testingName">>, CreatedName),
  ObjectTracked = rnose:'type-object-count'(course),
  ?assertEqual(1, ObjectTracked),  % number of objects in type list
  OneFromTrackingList = rnose:'type-objects'(course),
  ?assertEqual([<<"1">>], OneFromTrackingList). % id of object in type list

update_objects() ->
  rnose:'object-update'(course, 1, k2, newK2),
  NewK2 = rnose:'object-field'(course, 1, k2),
  ?assertEqual(<<"newK2">>, NewK2),
  rnose:'object-update'(course, 1, [k4, v4]),
  NewK4 = rnose:'object-field'(course, 1, k4),
  ?assertEqual(<<"v4">>, NewK4).

tags() ->
  rnose:'tag-add'(course, 1, poopin),
  rnose:'tag-add'(course, 1, doopin),
  M1 = rnose:'tag-members'(course, poopin),
  M2 = rnose:'tag-members'(course, doopin),
  T1 = rnose:'object-tags'(course, 1),
  ?assertEqual([<<"1">>], M1),
  ?assertEqual([<<"1">>], M2),
  ?assertEqual([<<"doopin">>, <<"poopin">>], lists:sort(T1)),

  rnose:'tag-del'(course, 1, poopin),
  M3 = rnose:'tag-members'(course, poopin),
  M4 = rnose:'tag-members'(course, doopin),
  T2 = rnose:'object-tags'(course, 1),
  ?assertEqual([], M3),
  ?assertEqual([<<"1">>], M4),
  ?assertEqual([<<"doopin">>], T2),

  rnose:'tag-del'(course, 1, doopin),
  M5 = rnose:'tag-members'(course, poopin),
  M6 = rnose:'tag-members'(course, doopin),
  T3 = rnose:'object-tags'(course, 1),
  ?assertEqual([], M5),
  ?assertEqual([], M6),
  ?assertEqual([], T3).

update_names() ->
  rnose:'name-update'(testingName, bob222),
  NewN = rnose:'name-target'(testingName),
  ?assertEqual(<<"bob222">>, NewN),
  rnose:'name-expire'(testingName),
  NewestN = rnose:'name-target'(testingName),
  ?assertEqual(<<"::removed::">>, NewestN).

find_owners() ->
  Objs = rnose:'owns-objects'(course, mattUid),
  ?assertEqual([<<"1">>], Objs),

  Owners = rnose:'object-owners'(course, 1),
  ?assertEqual([<<"mattUid">>], Owners),

  NoDelete = rnose:'owner-del'(course, 1, mattUid),
  ?assertEqual(not_removed_you_are_last_owner, NoDelete),

  rnose:'owner-add'(course, 1, bobUid),
  rnose:'owner-del'(course, 1, mattUid),
  BobOwns = rnose:'object-owners'(course, 1),
  ?assertEqual([<<"bobUid">>], BobOwns).

%%%----------------------------------------------------------------------
%%% Setup / Cleanup
%%%----------------------------------------------------------------------
setup() ->
  application:start(er),
  er_pool:start_link(redis_nose, "127.0.0.1", 9961),
  er:flushall(redis_nose).

teardown(_) ->
  ok.
