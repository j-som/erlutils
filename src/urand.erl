%%% File    : urand.erl
%%% Author  : J <j-som@foxmail.com>
%%% Descrip.: 随机
%%%
%%% Created :  09 Dec 2020 Wed by J <j-som@foxmail.com>
%%%
%%% Copyright (c) 2020 J <j-som@foxmail.com>
-module(urand).
-export([
    list_rand/1,
    rand_by_weight/2
]).

%% 随机获取列表元素
list_rand([H]) -> H;
list_rand(L) ->
    I = rand:uniform(length(L)),
    lists:nth(I, L).

%% 根据权值随机取出某个元素 元素元组中第Index个位置的值为权重
-spec rand_by_weight([Item :: tuple()], Index :: non_neg_integer()) -> Item :: tuple().
rand_by_weight([], _Index) ->
    error(badargs);
rand_by_weight(Tuples, Index) ->
    Sum = lists:foldl(fun(T, A) -> element(Index, T)+A end, 0, Tuples),
    P = rand:uniform() * Sum,
    rand_by_weight(Tuples, Index, P).

rand_by_weight([H], _, _) ->
    H;
rand_by_weight([H|T], Index, P) ->
    case element(Index, H) of
        Weight when P =< Weight ->
            H;
        Weight ->
            rand_by_weight(T, Index, P-Weight)
    end.