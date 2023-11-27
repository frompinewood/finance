-module(frac).
-export([add/2, sub/2, mul/2, divd/2, lcm/2, gcd/2, conv/2, simple/1]).

-type frac() :: {integer(), integer(), integer()}.

-spec add(frac(), frac()) -> frac().
add({Wa, Na, D}, {Wb, Nb, D}) ->
    Rem = (Na + Nb) rem D,
    Div = (Na + Nb) div D,
    Wc = Wa + Wb + Div,
    simple({Wc, Rem, D});
add({Wa, Na, Da}, {Wb, Nb, Db}) -> 
    {A, B} = conv({Wa, Na, Da},{Wb, Nb, Db}),
    add(A, B).

-spec sub(frac(), frac()) -> frac().
sub(A, B) -> throw(noimpl).

-spec mul(frac(), frac()) -> frac().
mul(A, B) -> throw(noimpl).

-spec divd(frac(), frac()) -> frac().
divd(A, B) -> throw(noimpl).

-spec lcm(integer(), integer()) -> integer().
lcm(A, A) -> A;
lcm(A, B) ->
    lcm(A, A, B, B).

-spec lcm(integer(), integer(), integer(), integer()) -> integer().
lcm(_, A1, _, A1) -> A1;
lcm(A, A1, B, B1) when A1 > B1 ->
    lcm(B, B1, A, A1);
lcm(A, A1, B, B1) ->
    lcm(A, A1+A, B, B1).

-spec gcd(integer(), integer()) -> integer().
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

-spec conv(frac(), frac()) -> {frac(), frac()}.
% convert fractions to use a common multiple
conv({_, _, D} = A, {_, _, D} = B) -> {A, B}; 
conv({Wa, Na, Da}, {Wb, Nb, Db}) ->
    LCM = lcm(Da, Db),
    Ra = LCM div Da,
    Rb = LCM div Db,
    {{Wa, Na * Ra, LCM}, {Wb, Nb * Rb, LCM}}.

-spec simple(frac()) -> frac().
%% TODO simple should also reduce fraction if possible
simple({W, N, D}) ->
    GCD = gcd(N, D),
    {W, N div GCD, D div GCD}.
    
