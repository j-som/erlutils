%%% File    : ufunc.erl
%%% Author  : J <j-som@foxmail.com>
%%% Descrip.: 函数相关工具
%%%
%%% Created :  08 Dec 2020 Tue by J <j-som@foxmail.com>
%%%
%%% Copyright (c) 2020 J <j-som@foxmail.com>
%%% 
-module(ufunc).

-export([
    function_exported/3
]).

function_exported(Module, Fun, Arity) when is_atom(Module)->
    case erlang:module_loaded(Module) of
        true ->
            erlang:function_exported(Module, Fun, Arity);
        _ ->
            code:ensure_loaded(Module),
            erlang:function_exported(Module, Fun, Arity)
    end;
function_exported(_Module, _Fun, _Arity) ->
    false.