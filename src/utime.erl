%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author J
%%% @copyright (C) 2017, <COMPANY> j-som@foxmail.com
%%% @doc
%%% 时间相关工具
%%% @end
%%% Created : 2020年07月14日 17:59:14 Tuesday
%%%-------------------------------------------------------------------
-module(utime).
-compile(inline).
-compile({inline_size,64}).

-define(SECONDS_PER_MINUTE, 60).
-define(SECONDS_PER_HOUR, 3600).
-define(SECONDS_PER_DAY, 86400).

-type seconds_from_1970() :: integer().
-type millisecond_from_1970() :: integer().
-type microsecond_from_1970() :: integer().

-type time_from_1970() ::seconds_from_1970() | millisecond_from_1970() | microsecond_from_1970().

%% 涉及到本地时间与UTC的，没有加UTC的都是本地时间
-export([
    now_as_seconds/0,
    now_as_millisecond/0,
    now_as_microsecond/0,
    now/1,
    is_today/1,
    now_to_date/0,
    time_to_date/1,
    time_to_datetime/1,
    diff_day/2,
    diff_day_abs/2,
    is_same_day/2,
    is_same_week/2,
    is_same_month/2,
    is_same_year/2,
    zero_time/1,
    range_of_day/1,
    range_of_month/1
]).



%%
%% 返回当前时间戳（秒）
%%
-spec now_as_seconds() -> seconds_from_1970().
now_as_seconds() -> now(second).

%%
%% 返回当前时间戳（豪秒）
%%
-spec now_as_millisecond() -> millisecond_from_1970().
now_as_millisecond() -> now(millisecond).

%%
%% 返回当前时间戳（微秒）
%%
-spec now_as_microsecond() -> microsecond_from_1970().
now_as_microsecond() -> now(microsecond).


%%
%% 根据单位返回当前时间戳
%% second：秒;
%% millisecond:豪秒
%% microsecond:微秒
%%
-spec now(TimeUnit) -> time_from_1970() when 
    TimeUnit :: second | millisecond | microsecond.
now(second) ->
    {A, B, _} = os:timestamp(),
    A * 1000000 + B;
now(millisecond) ->
    {A, B, C} = os:timestamp(),
    A * 1000000000 + B * 1000 + C div 1000;
now(microsecond) ->
    {A, B, C} = os:timestamp(),
    A * 1000000000000 + B * 1000000 + C.


%%
%% now_to_date 返回当前时间的{年,月,日}
%%
-spec now_to_date() -> calendar:date().
now_to_date() ->
    {Date, _} = calendar:local_time(),
    Date.

%%
%% time_to_date 返回从1970年算起的某个时间戳的{年,月,日}
%%
-spec time_to_date(seconds_from_1970()) -> calendar:date().
time_to_date(NowTime) ->
    {Date, _} = calendar:now_to_local_time({NowTime div 1000000, NowTime rem 1000000, 0}), 
    Date.

%%
%% time_to_datetime 返回从1970年算起的某个时间戳的{{年,月,日},{时,分,秒}}
%%
-spec time_to_datetime(seconds_from_1970()) ->calendar:datetime().
time_to_datetime(NowTime) ->
    calendar:now_to_local_time({NowTime div 1000000, NowTime rem 1000000, 0}).

%%
%% is_today 比较1970年算起的某个时间戳是否为今天
%%
-spec is_today(seconds_from_1970()) -> boolean().
is_today(NowTime) ->
    {{Y1, M1, D1}, _} = time_to_datetime(NowTime),
    {{Y2, M2, D2}, _} = calendar:local_time(),
    D1 == D2 andalso M1 == M2 andalso Y1 == Y2.

%%
%% 计算两个日期相差天数，Time1 - Time2， Time1和Time2的格式要保持一致
%%
-spec diff_day(Time1, Time2) -> integer() when 
    Time1 :: calendar:datetime1970() | seconds_from_1970(),
    Time2 :: calendar:datetime1970() | seconds_from_1970().
diff_day({Time1, _}, {Time2, _}) ->
    D1 = calendar:date_to_gregorian_days(Time1),
    D2 = calendar:date_to_gregorian_days(Time2),
    D1 - D2;
diff_day(Time1, Time2) when is_integer(Time1) andalso is_integer(Time2) ->
    D1 = calendar:date_to_gregorian_days(time_to_date(Time1)),
    D2 = calendar:date_to_gregorian_days(time_to_date(Time2)),
    D1 - D2.

-spec diff_day_abs(Time1, Time2) -> non_neg_integer() when 
    Time1 :: calendar:datetime1970() | seconds_from_1970(),
    Time2 :: calendar:datetime1970() | seconds_from_1970().
diff_day_abs(Time1, Time2) ->
    erlang:abs(diff_day(Time1, Time2)).


%%
%% is_same_day 判断两个本地时间是否同一天
%%
-spec is_same_day(Time1, Time2) -> boolean()  when 
    Time1 :: calendar:datetime1970() | seconds_from_1970(),
    Time2 :: calendar:datetime1970() | seconds_from_1970().
is_same_day(Time1, Time2) ->
    diff_day(Time1, Time2) == 0.

%%
%% is_same_week 判断两个本地时间是否在同一个星期
%%
-spec is_same_week(Time1, Time2) -> boolean()  when 
    Time1 :: calendar:datetime1970() | seconds_from_1970(),
    Time2 :: calendar:datetime1970() | seconds_from_1970().
is_same_week(Time1, Time2) when is_integer(Time1) andalso is_integer(Time2) ->
    calendar:iso_week_number(time_to_date(Time1)) == calendar:iso_week_number(time_to_date(Time2));
is_same_week({Date1, _}, {Date2, _}) ->
    calendar:iso_week_number(Date1) == calendar:iso_week_number(Date2).

%%
%% is_same_month 判断两个本地时间是否在同一个月
%%
-spec is_same_month(Time1, Time2) -> boolean()   when 
    Time1 :: calendar:datetime1970() | seconds_from_1970(),
    Time2 :: calendar:datetime1970() | seconds_from_1970().
is_same_month(Time1, Time2) when is_integer(Time1) andalso is_integer(Time2) ->
    {{Y1, M1, _}, _} = time_to_datetime(Time1),
    {{Y2, M2, _}, _} = time_to_datetime(Time2), 
    M1 == M2 andalso Y1 == Y2;
is_same_month({{Y1, M1, _}, _}, {{Y2, M2, _}, _}) ->
    M1 == M2 andalso Y1 == Y2.

%%
%% is_same_year 判断两个本地时间是否在同一个月
%%
-spec is_same_year(Time1, Time2) -> boolean()   when 
    Time1 :: calendar:datetime1970() | seconds_from_1970(),
    Time2 :: calendar:datetime1970() | seconds_from_1970().
is_same_year(Time1, Time2) when is_integer(Time1) andalso is_integer(Time2) ->
    {{Y1, _, _}, _} = time_to_datetime(Time1),
    {{Y2, _, _}, _} = time_to_datetime(Time2),
    Y1 == Y2;
is_same_year({{Y1, _, _}, _}, {{Y2, _, _}, _}) ->
    Y1 == Y2.

%%
%% zero_time 返回本地当天0点时间
%%
-spec zero_time(seconds_from_1970()) -> seconds_from_1970().
zero_time(NowTime) ->
    {_, Time} = time_to_datetime(NowTime),
    NowTime - calendar:time_to_seconds(Time).

%%
%% range_of_day 返回某个本地时间戳当天的时间范围 [开始时间,结束时间)
%% @return {开始时间,结束时间(不包含)}
%%
-spec range_of_day(seconds_from_1970()) -> {seconds_from_1970(), seconds_from_1970()}.
range_of_day(NowTime) ->
    ZeroTime = zero_time(NowTime),
    {ZeroTime, ZeroTime + ?SECONDS_PER_DAY}.
%%
%% range_of_month 返回某个本地时间戳当月的时间范围 [开始时间,结束时间)
%% @return {开始时间,结束时间(不包含)}
%%
-spec range_of_month(seconds_from_1970()) -> {seconds_from_1970(), seconds_from_1970()}.
range_of_month(NowTime) ->
    {{Y, M, _}, Time} = time_to_datetime(NowTime),
    DaysOfMonth = calendar:last_day_of_the_month(Y, M),
    Seconds = calendar:time_to_seconds(Time),
    ZeroTime = NowTime - Seconds,
    {ZeroTime, ZeroTime + DaysOfMonth*?SECONDS_PER_DAY}.



