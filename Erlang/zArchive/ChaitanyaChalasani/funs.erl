queue_data_sort(SortOptionsList, QueueMapsList) ->
  SFun = queue_data_sort_fun(SortOptionsList),
  lists:sort(SFun, QueueMapsList).

sort_options_maps_to_list(SortOptions) ->
  lists:keysort(1, [ {Seq, Field, Order} ||
                     #{seq := Seq, field := Field, order := Order}
                     <- SortOptions]).

queue_data_sort_fun([{1, <<"interval">>, <<"asc">>},
                     {2, <<"queue">>,    <<"asc">>}]) ->
  fun(#{interval := IntervalA, extension := QueueA},
      #{interval := IntervalB, extension := QueueB}) ->
      {IntervalA, QueueA} < {IntervalB, QueueB}
  end;
queue_data_sort_fun([{1, <<"interval">>, <<"asc">>},
                     {2, <<"queue">>,    <<"desc">>}]) ->
  fun(#{interval := IntervalA, extension := QueueA},
      #{interval := IntervalB, extension := QueueB}) ->
      if
        IntervalA == IntervalB -> QueueA > QueueB;
        true -> IntervalA < IntervalB
      end
  end;
queue_data_sort_fun([{1, <<"interval">>, <<"desc">>},
                     {2, <<"queue">>,    <<"desc">>}]) ->
  fun(#{interval := IntervalA, extension := QueueA},
      #{interval := IntervalB, extension := QueueB}) ->
      {IntervalA, QueueA} > {IntervalB, QueueB}
  end;
queue_data_sort_fun([{1, <<"interval">>, <<"desc">>},
                     {2, <<"queue">>,    <<"asc">>}]) ->
  fun(#{interval := IntervalA, extension := QueueA},
      #{interval := IntervalB, extension := QueueB}) ->
      if
        IntervalA == IntervalB -> QueueA < QueueB;
        true -> IntervalA > IntervalB
      end
  end;
queue_data_sort_fun([{1, <<"queue">>,   <<"asc">>},
                     {2, <<"interval">>,<<"asc">>}]) ->
  fun(#{interval := IntervalA, extension := QueueA},
      #{interval := IntervalB, extension := QueueB}) ->
      {QueueA, IntervalA} < {QueueB, IntervalB}
  end;
queue_data_sort_fun([{1, <<"queue">>,   <<"asc">>},
                     {2, <<"interval">>,<<"desc">>}]) ->
  fun(#{interval := IntervalA, extension := QueueA},
      #{interval := IntervalB, extension := QueueB}) ->
      if
        QueueA == QueueB -> IntervalA > IntervalB;
        true -> QueueA < QueueB
      end
  end;
queue_data_sort_fun([{1, <<"queue">>,   <<"desc">>},
                     {2, <<"interval">>,<<"desc">>}]) ->
  fun(#{interval := IntervalA, extension := QueueA},
      #{interval := IntervalB, extension := QueueB}) ->
      {QueueA, IntervalA} > {QueueB, IntervalB}
  end;
queue_data_sort_fun([{1, <<"queue">>,   <<"desc">>},
                     {2, <<"interval">>,<<"asc">>}]) ->
  fun(#{interval := IntervalA, extension := QueueA},
      #{interval := IntervalB, extension := QueueB}) ->
      if
        QueueA == QueueB -> IntervalA < IntervalB;
        true -> QueueA > QueueB
      end
  end;
queue_data_sort_fun(Other) ->
  lager:error("Sorting ~p not supported", [Other]),
  fun(A, B) -> A < B end.
