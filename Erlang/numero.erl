-module(numero).
-export([calc/1]).

calc(N) ->
  io:format("\n\nVedic: ~p\nHebrew:~p\nChaldean: ~p\n\n", 
                              [vedic(N), hebrew(N), chaldean(N)]).

hebrew(Name) ->
  digit_sum( hebrew(string:to_upper(Name),0), 0 ).

hebrew([], Sum) -> Sum;
hebrew("CH"++Name, Sum) -> hebrew(Name, Sum + 8);
hebrew("PH"++Name, Sum) -> hebrew(Name, Sum + 80);
hebrew("SH"++Name, Sum) -> hebrew(Name, Sum + 300);
hebrew("TH"++Name, Sum) -> hebrew(Name, Sum + 400);
hebrew("TZ"++Name, Sum) -> hebrew(Name, Sum + 90);
hebrew("A"++Name, Sum) -> hebrew(Name, Sum + 1);
hebrew("B"++Name, Sum) -> hebrew(Name, Sum + 2);
hebrew("C"++Name, Sum) -> hebrew(Name, Sum + 20);
hebrew("D"++Name, Sum) -> hebrew(Name, Sum + 4);
hebrew("E"++Name, Sum) -> hebrew(Name, Sum + 5);
hebrew("F"++Name, Sum) -> hebrew(Name, Sum + 80);
hebrew("G"++Name, Sum) -> hebrew(Name, Sum + 3);
hebrew("H"++Name, Sum) -> hebrew(Name, Sum + 5);
hebrew("I"++Name, Sum) -> hebrew(Name, Sum + 10);
hebrew("J"++Name, Sum) -> hebrew(Name, Sum + 10);
hebrew("K"++Name, Sum) -> hebrew(Name, Sum + 20);
hebrew("L"++Name, Sum) -> hebrew(Name, Sum + 30);
hebrew("M"++Name, Sum) -> hebrew(Name, Sum + 40);
hebrew("N"++Name, Sum) -> hebrew(Name, Sum + 50);
hebrew("O"++Name, Sum) -> hebrew(Name, Sum + 70);
hebrew("P"++Name, Sum) -> hebrew(Name, Sum + 80);
hebrew("Q"++Name, Sum) -> hebrew(Name, Sum + 100);
hebrew("R"++Name, Sum) -> hebrew(Name, Sum + 200);
hebrew("S"++Name, Sum) -> hebrew(Name, Sum + 60);
hebrew("T"++Name, Sum) -> hebrew(Name, Sum + 9);
hebrew("U"++Name, Sum) -> hebrew(Name, Sum + 6);
hebrew("V"++Name, Sum) -> hebrew(Name, Sum + 6);
hebrew("W"++Name, Sum) -> hebrew(Name, Sum + 6);
hebrew("X"++Name, Sum) -> hebrew(Name, Sum + 60);
hebrew("Y"++Name, Sum) -> hebrew(Name, Sum + 10);
hebrew("Z"++Name, Sum) -> hebrew(Name, Sum + 7);
hebrew("1"++Name, Sum) -> hebrew(Name, Sum + 1);
hebrew("2"++Name, Sum) -> hebrew(Name, Sum + 2);
hebrew("3"++Name, Sum) -> hebrew(Name, Sum + 3);
hebrew("4"++Name, Sum) -> hebrew(Name, Sum + 4);
hebrew("5"++Name, Sum) -> hebrew(Name, Sum + 5);
hebrew("6"++Name, Sum) -> hebrew(Name, Sum + 6);
hebrew("7"++Name, Sum) -> hebrew(Name, Sum + 7);
hebrew("8"++Name, Sum) -> hebrew(Name, Sum + 8);
hebrew("9"++Name, Sum) -> hebrew(Name, Sum + 9);
hebrew([_|Name], Sum) -> hebrew(Name, Sum + 0).


vedic(Name) ->
  digit_sum( vedic(string:to_upper(Name),0), 0 ).

vedic([], Sum) -> Sum;
vedic("A"++Name, Sum) -> vedic(Name, Sum + 1);
vedic("B"++Name, Sum) -> vedic(Name, Sum + 2);
vedic("C"++Name, Sum) -> vedic(Name, Sum + 2);
vedic("D"++Name, Sum) -> vedic(Name, Sum + 4);
vedic("E"++Name, Sum) -> vedic(Name, Sum + 5);
vedic("F"++Name, Sum) -> vedic(Name, Sum + 8);
vedic("G"++Name, Sum) -> vedic(Name, Sum + 3);
vedic("H"++Name, Sum) -> vedic(Name, Sum + 8);
vedic("I"++Name, Sum) -> vedic(Name, Sum + 1);
vedic("J"++Name, Sum) -> vedic(Name, Sum + 1);
vedic("K"++Name, Sum) -> vedic(Name, Sum + 2);
vedic("L"++Name, Sum) -> vedic(Name, Sum + 3);
vedic("M"++Name, Sum) -> vedic(Name, Sum + 4);
vedic("N"++Name, Sum) -> vedic(Name, Sum + 5);
vedic("O"++Name, Sum) -> vedic(Name, Sum + 7);
vedic("P"++Name, Sum) -> vedic(Name, Sum + 8);
vedic("Q"++Name, Sum) -> vedic(Name, Sum + 1);
vedic("R"++Name, Sum) -> vedic(Name, Sum + 2);
vedic("S"++Name, Sum) -> vedic(Name, Sum + 3);
vedic("T"++Name, Sum) -> vedic(Name, Sum + 4);
vedic("U"++Name, Sum) -> vedic(Name, Sum + 6);
vedic("V"++Name, Sum) -> vedic(Name, Sum + 6);
vedic("W"++Name, Sum) -> vedic(Name, Sum + 6);
vedic("X"++Name, Sum) -> vedic(Name, Sum + 6);
vedic("Y"++Name, Sum) -> vedic(Name, Sum + 1);
vedic("Z"++Name, Sum) -> vedic(Name, Sum + 7);
vedic("1"++Name, Sum) -> vedic(Name, Sum + 1);
vedic("2"++Name, Sum) -> vedic(Name, Sum + 2);
vedic("3"++Name, Sum) -> vedic(Name, Sum + 3);
vedic("4"++Name, Sum) -> vedic(Name, Sum + 4);
vedic("5"++Name, Sum) -> vedic(Name, Sum + 5);
vedic("6"++Name, Sum) -> vedic(Name, Sum + 6);
vedic("7"++Name, Sum) -> vedic(Name, Sum + 7);
vedic("8"++Name, Sum) -> vedic(Name, Sum + 8);
vedic("9"++Name, Sum) -> vedic(Name, Sum + 9);
vedic([_|Name], Sum) -> vedic(Name, Sum + 0).


chaldean(Name) ->
  digit_sum( chaldean(string:to_upper(Name),0), 0 ).

chaldean([], Sum) -> Sum;
chaldean("A"++Name, Sum) -> chaldean(Name, Sum + 1);
chaldean("B"++Name, Sum) -> chaldean(Name, Sum + 2);
chaldean("C"++Name, Sum) -> chaldean(Name, Sum + 3);
chaldean("D"++Name, Sum) -> chaldean(Name, Sum + 4);
chaldean("E"++Name, Sum) -> chaldean(Name, Sum + 5);
chaldean("F"++Name, Sum) -> chaldean(Name, Sum + 8);
chaldean("G"++Name, Sum) -> chaldean(Name, Sum + 3);
chaldean("H"++Name, Sum) -> chaldean(Name, Sum + 5);
chaldean("I"++Name, Sum) -> chaldean(Name, Sum + 1);
chaldean("J"++Name, Sum) -> chaldean(Name, Sum + 1);
chaldean("K"++Name, Sum) -> chaldean(Name, Sum + 2);
chaldean("L"++Name, Sum) -> chaldean(Name, Sum + 3);
chaldean("M"++Name, Sum) -> chaldean(Name, Sum + 4);
chaldean("N"++Name, Sum) -> chaldean(Name, Sum + 5);
chaldean("O"++Name, Sum) -> chaldean(Name, Sum + 7);
chaldean("P"++Name, Sum) -> chaldean(Name, Sum + 8);
chaldean("Q"++Name, Sum) -> chaldean(Name, Sum + 1);
chaldean("R"++Name, Sum) -> chaldean(Name, Sum + 2);
chaldean("S"++Name, Sum) -> chaldean(Name, Sum + 3);
chaldean("T"++Name, Sum) -> chaldean(Name, Sum + 4);
chaldean("U"++Name, Sum) -> chaldean(Name, Sum + 6);
chaldean("V"++Name, Sum) -> chaldean(Name, Sum + 6);
chaldean("W"++Name, Sum) -> chaldean(Name, Sum + 6);
chaldean("X"++Name, Sum) -> chaldean(Name, Sum + 5);
chaldean("Y"++Name, Sum) -> chaldean(Name, Sum + 1);
chaldean("Z"++Name, Sum) -> chaldean(Name, Sum + 7);
chaldean("1"++Name, Sum) -> chaldean(Name, Sum + 1);
chaldean("2"++Name, Sum) -> chaldean(Name, Sum + 2);
chaldean("3"++Name, Sum) -> chaldean(Name, Sum + 3);
chaldean("4"++Name, Sum) -> chaldean(Name, Sum + 4);
chaldean("5"++Name, Sum) -> chaldean(Name, Sum + 5);
chaldean("6"++Name, Sum) -> chaldean(Name, Sum + 6);
chaldean("7"++Name, Sum) -> chaldean(Name, Sum + 7);
chaldean("8"++Name, Sum) -> chaldean(Name, Sum + 8);
chaldean("9"++Name, Sum) -> chaldean(Name, Sum + 9);
chaldean([_|Name], Sum) -> chaldean(Name, Sum + 0).

digit_sum(0, S) when S < 10 -> S;
digit_sum(0, S) -> digit_sum(S, 0);
digit_sum(N, S) -> digit_sum(N div 10, S + (N rem 10)).
