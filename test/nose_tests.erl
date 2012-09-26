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

%%%%%%%%%%
% -- update / add / remove functions in rnose
% -- update params here for matched functionality
% -- test
%

%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------
create_objects() ->
  CreatedHash = rnose:'object-create'(course, mattUid, 
                                      [k1, v1, k2, v2, k3, v3]),
  ?assertEqual(<<"1">>, CreatedHash),
  rnose:'name-new'(course, CreatedHash, testingName),
  KVals = rnose:'object-all'(course, 1),
  ?assertEqual([{k1, <<"v1">>},
                {k2, <<"v2">>},
                {k3, <<"v3">>},
                {name, <<"testingName">>}], KVals),
  ?assertEqual(true, rnose:'uid-owns-object'(course, mattUid, 1)),
  ?assertEqual(true, rnose:'object-owned-by-uid'(course, 1, mattUid)),
  NameTo = rnose:'name-target'(testingName),
  ?assertEqual(<<"course:1">>, NameTo),
  CreatedName = rnose:'object-field'(course, 1, name),
  ?assertEqual(<<"testingName">>, CreatedName).

update_objects() ->
  rnose:'object-update'(course, 1, k2, newK2),
  NewK2 = rnose:'object-field'(course, 1, k2),
  ?assertEqual(<<"newK2">>, NewK2),
  rnose:'object-update'(course, 1, [k4, v4]),
  NewK4 = rnose:'object-field'(course, 1, k4),
  ?assertEqual(<<"v4">>, NewK4).

tags() ->
  rnose:'object-tag-add'(course, 1, cat, "poopin   HALP"),
  rnose:'object-tag-add'(course, 1, cat, doopin),
  M1 = rnose:'category-tag-objects'(course, cat, "poopin halp"),
  M2 = rnose:'category-tag-objects'(course, cat, doopin),
  T1 = rnose:'object-category-tags'(course, 1, cat),
  ?assertEqual([<<"1">>], M1),
  ?assertEqual([<<"1">>], M2),
  ?assertEqual([<<"doopin">>, <<"poopin halp">>], lists:sort(T1)),

  rnose:'object-tag-del'(course, 1, "cAT", "pOOPin         halp"),
  M3 = rnose:'category-tag-objects'(course, cat, "poopin halp"),
  M4 = rnose:'category-tag-objects'(course, cat, doopin),
  T2 = rnose:'object-category-tags'(course, 1, "CAT"),
  ?assertEqual([], M3),
  ?assertEqual([<<"1">>], M4),
  ?assertEqual([<<"doopin">>], T2),

  rnose:'object-tag-del'(course, 1, cat, doopin),
  M5 = rnose:'category-tag-objects'(course, cat, "poopin halp"),
  M6 = rnose:'category-tag-objects'(course, cat, doopin),
  T3 = rnose:'object-category-tags'(course, 1, "Cat"),
  ?assertEqual([], M5),
  ?assertEqual([], M6),
  ?assertEqual([], T3).

update_names() ->
  rnose:'name-modify'(bob222, testingName),
  NewN = rnose:'name-target'(testingName),
  ?assertEqual(<<"bob222">>, NewN),
  rnose:'name-delete'(testingName),
  NewestN = rnose:'name-target'(testingName),
  ?assertEqual(nil, NewestN).

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
