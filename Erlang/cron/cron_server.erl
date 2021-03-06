-module(cron_server).
-behaviour(gen_server).

-record(cron, {
          name,
          function,
          schedule
         }).

%% API
-export([start_link/0,
         stop/0,
         add_job/3,
         view_job/1,
         list_jobs/0,
         modify_job/2,
         delete_job/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(WAIT, 30000).
-define(TIME_ZONES, ["SGT", "EDT", "IST"]).



%%% ---------------------------------- API ---------------------------------

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).

add_job(Name, Fun, Schedule) -> handle_add_job(Name, Fun, Schedule).

list_jobs() -> handle_list_jobs().

view_job(Name) -> handle_view_job(Name).

modify_job(Name, Schedule) -> handle_modify_job(Name, Schedule).

delete_job(Name) -> handle_delete_job(Name).


%%% ------------------------ gen_server callbacks ------------------------

init([]) ->
    process_flag(trap_exit, true),
    init_tables(),
    {ok, #{}, ?WAIT}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(timeout, ThenDateTime) ->
    case sys_date_time() of
        %if same minute, then wait for ?WAIT microseconds.
        ThenDateTime ->
            {noreply, ThenDateTime, ?WAIT};
        %if new minute, process cron jobs.
        NowDateTime ->
            %io:format("~n~n-------------Checking Cron Jobs~p~n", [NowDateTime]),
            handle_jobs(NowDateTime),
            {noreply, NowDateTime, ?WAIT}
    end;

handle_info(Info, SysDateTime) ->
    io:format("Received Info ~p", [Info]),
    {noreply, SysDateTime, ?WAIT}.

terminate(_Reason, _SysDateTime) -> ok.

code_change(_OldVsn, SysDateTime, _Extra) -> {ok, SysDateTime}.


%%% ------------------------- Internal Functions -----------------------

init_tables() ->
    mnesia:create_table(cron, [{attributes, record_info(fields, cron)}]).

%make seconds as 0 in System Time.
sys_date_time() ->
    {Date, {H, M, _S}} = erlang:localtime(),
    {Date, {H, M, 0}}.

handle_add_job(_Name, _Function, _Schedule) ->
    CronJob = #cron{name=_Name, function=_Function, schedule=_Schedule},
    F = fun() -> mnesia:write(CronJob) end,
    mnesia:transaction(F).

handle_view_job(_Name) -> mnesia:dirty_read(cron, _Name).

handle_list_jobs() ->
    lists:foreach(
      fun(CronJob) -> io:format("~n~p", [CronJob]) end,
      lists:flatten([mnesia:dirty_read(cron, X) || X <- mnesia:dirty_all_keys(cron)])
     ).


handle_modify_job(_Name, _Schedule) ->
    case mnesia:dirty_read(cron, _Name) of
        [] -> {error, entry_not_found};
        [CronJob] -> mnesia:dirty_write(CronJob#cron{schedule=_Schedule})
    end.


handle_delete_job(_Name) ->
    CronJob = {cron, _Name},
    F = fun() -> mnesia:delete(CronJob) end,
    mnesia:transaction(F).


handle_jobs(SysDateTime) ->
    lists:foreach(
      fun(CronJob) -> handle_job(SysDateTime, CronJob) end,
      lists:flatten([ mnesia:dirty_read(cron, CronJob) || CronJob <- mnesia:dirty_all_keys(cron) ])).


handle_job(SysDateTime, #cron{name = _Name, function = {M, F, A}, schedule = Schedule}) ->
    % convert SysDateTime to its corresponding UTC time.
    SysDateTimeUTC = erlang:localtime_to_universaltime(SysDateTime),

    % Fetch the time fields from Schedule.
    {JMinute, JHour, JDayOfMonth, JMonth, JDayOfWeek, JTimeZones} = Schedule,
    % Ignore Timezone and form the Schedule.
    JSchedule = {JMinute, JHour, JDayOfMonth, JMonth, JDayOfWeek},

    % If TimeZone in Schedule is '_' then make ?TIME_ZONES as the TimeZones List else, leave as is.
    TimeZonesList = case JTimeZones of
                        ['_'] -> ?TIME_ZONES;
                        _ -> JTimeZones
                    end,
    io:format("~n~n TimeZoneList: ~p~n~n", [TimeZonesList]),

    % For each TimeZone in TimeZonesList, check and execute the current cron job.
    lists:foreach( fun(TimeZone) ->
                           % convert local time to Destination TimeZone and remove seconds from it.
                           DestTime = localtime:local_to_local(SysDateTimeUTC, "UTC", TimeZone),
                           DestTimeNow = seconds(remove, DestTime),

                           % if the above converted DestTime matches that of the time in schedule, execute the job.
                           case match_times(DestTimeNow, JSchedule) of
                               false -> ok;
                               true ->
                                   io:format("~n~n~p:~p~nMatched!~n", [TimeZone, DestTimeNow]),
                                   spawn(M, F, [A])
                           end
                   end,
                   TimeZonesList).

% match the time T1, T2. IF matched, execute cron jobs
match_times(T1, T2) ->
    {{T1_Year, T1_Month, T1_DayOfMonth}, {T1_Hour, T1_Minute}}  = T1,
    {T2_Minute, T2_Hour, T2_DayOfMonth, T2_Month, T2_DayOfWeek} = T2,
    MatchList = [{T1_Minute, T2_Minute}, {T1_Hour, T2_Hour},
                 {T1_DayOfMonth, T2_DayOfMonth}, {T1_Month, T2_Month},
                 {calendar:day_of_the_week({T1_Year, T1_Month, T1_DayOfMonth}), T2_DayOfWeek}
                ],
    % check if the Term pair in all tuples of MatchList are matching.
    lists:all(fun match_values/1, MatchList).


% check if both Terms in the tuple are matching.
match_values({_Value, '_'}) -> true;
match_values({Value, Value}) -> true;
match_values({Value, [$/|Interval]}) ->
    case catch (Value rem list_to_integer(Interval)) of
        {'EXIT', _Reason} -> false;
        0 -> true;
        _ -> false
    end;
match_values({Value, ValuesList}) when is_list(ValuesList) -> lists:member(Value, ValuesList);
match_values(_Y) -> false.


% add or remove seconds from Datetime.
seconds(Action, DateTime) ->
    case Action of
        add ->
            {Date, {H, M}} = DateTime,
            {Date, {H, M, 0}};
        remove ->
            {Date, {H, M, _S}} = DateTime,
            {Date, {H, M}};
        _Other -> DateTime
    end.
