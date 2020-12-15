%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author J
%%% @copyright (C) 2017, <COMPANY> j-som@foxmail.com
%%% @doc
%%% 列表相关工具
%%% @end
%%% Created : 2020年07月14日 18:14:48 Tuesday
%%%-------------------------------------------------------------------
-module(ulists).
-compile(inline).
-compile({inline_size,64}).

-export([
    append_if_not_exist/2,
    sorted_insert/3,
    index_of/2,
    replace_or_append/3
]).
%%
%% append_if_not_exist 在表头追加一个元素
%% 如果不存在则追加在列表头，否则保持不变
%%
-spec append_if_not_exist(any(), [any()]) -> [any()].
append_if_not_exist(I, List) ->
    case lists:member(I, List) of 
        true ->
            List;
        false ->
            [I|List]
    end.
%%
%% sorted_insert 按顺序插入一个元素
%% 如果Compare(I, H) == true, 则I插入在H前面
%% 
-spec sorted_insert(any(), fun((any(), any()) -> boolean()), [any()]) -> [any()].
sorted_insert(I, Compare, [H|SortedList]) ->
    case Compare(I, H) of 
        true ->
            [I,H|SortedList];
        false ->
            [H|sorted_insert(I, Compare, SortedList)]
    end;

sorted_insert(I, _, []) ->
    [I].
%%
%% index_of 计算元素在列表中的位置 
%% 从1开始，0表示没找到，因为lists:nth中列表中的位置从1算起
%% 
-spec index_of(any(), [any()]) -> non_neg_integer().
index_of(I, List) ->
    index_of(I, List, 1).

index_of(I, [I|_], N) -> N;
index_of(I, [_|T], N) -> index_of(I, T, N + 1);
index_of(_, [], _) -> 0.

%% 
%% 用I替换掉列表中第一个F(H)返回true的元素，如果没有，则加在最末
%%
-spec replace_or_append(L :: [any()], fun((any()) -> boolean()), any()) -> [any()].
replace_or_append([H|L], F, I) ->
    case F(H) of 
        true ->
            [I|L];
        false ->
            [H|replace_or_append(L, F, I)]
    end;
replace_or_append([], _F, I) ->
    [I].