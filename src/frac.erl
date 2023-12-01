-module(frac).

-export([add/2, sub/2, mul/2, divd/2, pow/2]).
-export([lcm/2, gcd/2, conv/2, simple/1, improper/1, recp/1, round/2, is_whole/1, lt/2,
         lte/2, gt/2, gte/2]).
-export([frac_to_integer/1, frac_to_float/1, integer_to_frac/1, float_to_frac/1]).

%% fraction represented as {Whole, Numerator, Denominator}
-type frac() :: {integer(), integer(), integer()}.

-spec add(frac(), frac()) -> frac().
%% add two fractions
add({0, Na, D}, {0, Nb, D}) ->
    simple({0, Na + Nb, D});
add(A, B) ->
    {A1, B1} = conv(A, B),
    add(improper(A1), improper(B1)).

-spec sub(frac(), frac()) -> frac().
%% subtract two fractions
sub({0, Na, D}, {0, Nb, D}) ->
    simple({0, Na - Nb, D});
sub(A, B) ->
    {A1, B1} = conv(A, B),
    sub(improper(A1), improper(B1)).

-spec mul(frac(), frac()) -> frac().
%% multiple two fractions
mul({Wa, Na, Da}, {Wb, Nb, Db}) ->
    Nc = Wa * Da + Na,
    Nd = Wb * Db + Nb,
    simple({0, Nc * Nd, Da * Db}).

-spec divd(frac(), frac()) -> frac().
%% divides two fractions
divd(A, B) ->
    mul(A, recp(B)).

-spec pow(frac(), integer()) -> frac().
%% raises a fraction to an integer power
%% @todo implement fractional exponents
pow(Base, Exp) when is_integer(Exp) ->
    {0, N, D} = improper(Base),
    simple({0, round(math:pow(N, Exp)), round(math:pow(D, Exp))});
pow(Base, {Exp, 0, 1}) ->
    pow(Base, Exp);
pow(_, _) ->
    throw(noimpl).

-spec is_whole(frac()) -> boolean().
%% returns true if the fraction can be simplified into a whole number
is_whole(Val) ->
    case simple(Val) of
        {_, 0, 1} ->
            true;
        _ ->
            false
    end.

-spec round(frac(), integer()) -> frac().
%% @todo rename to something better since this doesn't actually round
round({W, N, D}, Precision) ->
    {W, N div (D div Precision), Precision}.

-spec gcd(integer(), integer()) -> integer().
%% find the greatest common denominator between two integers
gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).

-spec lcm(integer(), integer()) -> integer().
%% find the least common multiple of two integers
lcm(A, B) ->
    A * B div gcd(A, B).

% comparisons
-spec lt(frac(), frac()) -> boolean().
%% less than function for fractions
lt(A, B) ->
    {{0, Na, D}, {0, Nb, D}} = conv(improper(A), improper(B)),
    Na < Nb.

-spec lte(frac(), frac()) -> boolean().
%% less than or equal to function for fractions
lte(A, B) ->
    lt(A, B) or not gt(A, B).

-spec gt(frac(), frac()) -> boolean().
%% greater than function for fractions
gt(A, B) ->
    {{0, Na, D}, {0, Nb, D}} = conv(improper(A), improper(B)),
    Na > Nb.

-spec gte(frac(), frac()) -> boolean().
%% greater than or equal to function for fractions
gte(A, B) ->
    gt(A, B) or not lt(A, B).

% convert fractions to use a common multiple
-spec conv(frac(), frac()) -> {frac(), frac()}.
%% converts both fractions to have a common multiple
conv({_, _, D} = A, {_, _, D} = B) ->
    {A, B};
conv({Wa, Na, Da}, {Wb, Nb, Db}) ->
    LCM = lcm(Da, Db),
    Ra = LCM div Da,
    Rb = LCM div Db,
    {{Wa, Na * Ra, LCM}, {Wb, Nb * Rb, LCM}}.

-spec simple(frac()) -> frac().
%% reduces a fraction to its simpliest form
simple({W, N, D}) ->
    GCD = gcd(N, D),
    Na = N div GCD,
    Da = D div GCD,
    Wa = W + Na div Da,
    Nb = Na rem Da,
    {Wa, Nb, Da}.

-spec improper(frac()) -> frac().
%% converts fraction to an improper fraction
improper({W, N, D}) ->
    {0, N + W * D, D}.

-spec recp(frac()) -> frac().
%% returns the reciprocle of a fraction
recp({W, N, D}) ->
    {0, D, W * D + N}.

-spec frac_to_integer(frac()) -> integer().
%% converts a fraction to an integer. throws an error if supplied a mixed fraction
frac_to_integer(Frac) ->
    case simple(Frac) of
        {W, 0, 1} ->
            W;
        _ ->
            throw({badarg,
                   lists:flatten(
                       io_lib:format("~p cannot be an integer", [Frac]))})
    end.

-spec frac_to_float(frac()) -> float().
%% converts fraction to a float
frac_to_float(Frac) ->
    {W, N, D} = simple(Frac),
    W + N / D.

-spec integer_to_frac(integer()) -> frac().
%% converts an integer to a fraction
integer_to_frac(I) ->
    {I, 0, 1}.

-spec float_to_frac(float()) -> frac().
%% @todo implement
float_to_frac(_) ->
    throw(noimpl).
