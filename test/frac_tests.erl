-module(frac_tests).

-include_lib("eunit/include/eunit.hrl").

one_plus_one_test() ->
    ?assertEqual({2,0,1}, frac:add({1,0,1},{1,0,1})).

mixed_add_test() ->
    ?assertEqual({1,1,4}, frac:add({1,0,1},{0,1,4})).

improper_add_test() ->
    ?assertEqual({12,1,2},frac:add({0,24,2},{0,2,4})).

adding_a_negative_test() ->
    ?assertEqual({1,0,1}, frac:add({2,0,1},{-1,0,1})).

one_minus_one_test() ->
    ?assertEqual({0,0,1}, frac:sub({1,0,1},{1,0,1})).

mixed_sub_test() ->
    ?assertEqual({0,1,4}, frac:sub({1,1,4},{1,0,1})).

improper_sub_test() ->
    ?assertEqual({1,1,2}, frac:sub({0,27,2},{0,48,4})).


