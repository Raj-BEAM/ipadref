-module(timezones).

handle_job(Now, #cron{name = _Name,
                      function = {M, F, A},
                      schedule = Schedule}) ->
  case match_times(Now, Schedule) of
    false -> ok;
    true -> spawn(M, F, [A])
  end.


match_times(Now, JSchedule) ->
  {{_SYYYY, SMM, SDD}, {SHour, SMinute, _SSecond}}  = Now,
  {JMinute, JHour, JDayOfMonth, JMonth, JDayOfWeek, JTimeZone} = JSchedule,

  STimeZone

  MatchList =   [{SMinute, JMinute}, {SHour, JHour},
                 {SDD, JDayOfMonth}, {SMM, JMonth},
                 {calendar:day_of_the_week({_SYYYY, SMM, SDD}), JDayOfWeek},
                 {StimeZone, JTimeZone}
                ],
  %localtime:tz_shift({{2013, 01, 22}, {18, 17, 00}}, "America/New York", "Europe/Moscow").
  lists:all(fun match_values/1, MatchList).

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
