%%% File    : urand.erl
%%% Author  : J <j-som@foxmail.com>
%%% Descrip.: 随机
%%%
%%% Created :  09 Dec 2020 Wed by J <j-som@foxmail.com>
%%%
%%% Copyright (c) 2020 J <j-som@foxmail.com>
-module(urand).
-export([
    list_rand/1
]).

%% 随机获取列表元素
list_rand([H]) -> H;
list_rand(L) ->
    I = rand:uniform(length(L)),
    lists:nth(I, L).