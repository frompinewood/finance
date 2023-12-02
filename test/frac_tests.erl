-module(frac_tests).

-include_lib("eunit/include/eunit.hrl").

add_test() ->
    ?assertEqual({2,0,1}, frac:add({1,0,1},{1,0,1})),
    ?assertEqual({1,1,4}, frac:add({1,0,1},{0,1,4})),
    ?assertEqual({12,1,2},frac:add({0,24,2},{0,2,4})),
    ?assertEqual({1,0,1}, frac:add({2,0,1},{-1,0,1})).

sub_test() ->
    ?assertEqual({0,0,1}, frac:sub({1,0,1},{1,0,1})),
    ?assertEqual({0,1,4}, frac:sub({1,1,4},{1,0,1})),
    ?assertEqual({1,1,2}, frac:sub({0,27,2},{0,48,4})),
    ?assertEqual({-3,1,4}, frac:sub({-1,0,1},{2,1,4})),
    ?assertEqual({0,1,4}, frac:sub({-1,1,2},{-1,1,4})).

mul_test() ->
    ?assertEqual({17,0,1}, frac:mul({4,1,4},{4,0,1})),
    ?assertEqual({-1,0,1}, frac:mul({1,0,1},{-1,0,1})).

divd_test() ->
    ?assertEqual({4,1,4}, frac:divd({17,0,1},{4,0,1})),
    ?assertEqual({-5,0,1}, frac:divd({25,0,1},{-5,0,1})).

pow_test() ->
    ?assertEqual({181,26,27}, frac:pow({5,2,3},3)).

lcm_test() ->
    ?assertEqual(6, frac:lcm(2,3)),
    ?assertEqual(63, frac:lcm(7,9)).

gcd_test() ->
    ?assertEqual(1, frac:gcd(2,3)),
    ?assertEqual(4, frac:gcd(8,12)).

conv_test() ->
    ?assertEqual({{0,3,6},{0,2,6}}, frac:conv({0,1,2},{0,1,3})).

simple_test() ->
    ?assertEqual({0,1,2}, frac:simple({0,2,4})),
    ?assertEqual({1,1,4}, frac:simple({0,5,4})),
    ?assertEqual({13,1,3}, frac:simple({1,148,12})).

comparision_test() ->
    ?assertEqual(true, frac:lt({0,0,1},{0,4,2})),
    ?assertEqual(true, frac:lt({-1,0,1},{0,0,1})),
    ?assertEqual(true, frac:gt({0,4,2},{1,0,1})),
    ?assertEqual(true, frac:gt({0,4,2},{-1,0,1})),
    ?assertEqual(true, frac:lte({2,0,1},{0,6,3})),
    ?assertEqual(true, frac:gte({2,0,1},{0,6,3})).

conversion_test() ->
    ?assertEqual(1, frac:frac_to_integer({1,0,1})),
    ?assertEqual({1,0,1}, frac:integer_to_frac(1)),
    ?assertEqual(0.25, frac:frac_to_float({0,1,4})),
    ?assertEqual({0,1,4}, frac:float_to_frac(0.25)).
