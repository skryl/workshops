-module(examples).
-export([double/1, fac/1, mul/2, convert/2, convert_length/1, list_length/1,
         print/0]).

-export([format_temps_simple/1, format_temps/1, convert_to_celsius/1]).

-export([list_max/1, reverse/1]).

%% functions

double(X) ->
  2 * X.

fac(1) ->
  1;
fac(N) ->
  N * fac(N - 1).

mul(X, Y) -> 
  X * Y.

%% atoms

convert(M, inch) ->
  M / 2.54;

convert(N, centimeter) ->
  N * 2.54.

%% tuples

convert_length({centimeter, X}) ->
  {inch, X / 2.54};

convert_length({inch, X}) ->
  {centimeter, X * 2.54}.

%% lists

list_length([]) ->
  0;
list_length([_First | Rest]) ->
  1 + list_length(Rest).

%% printing

print() ->
  io:format("hello world ~n", []).

%% example

format_temps_simple([]) ->
  ok;
format_temps_simple([City | Rest]) ->
  print_temp(convert_to_celsius(City)),
  format_temps(Rest).

convert_to_celsius({Name, {c, Temp}}) -> 
  {Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) -> 
  {Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
  io:format("~-15w ~w c~n", [Name, Temp]).

%% matching, guards

list_max([Head|Rest]) ->
  list_max(Rest, Head).

list_max([], Res) ->
  Res;
list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
  list_max(Rest, Head);
list_max([_Head|Rest], Result_so_far) ->
  list_max(Rest, Result_so_far).

%% list reversal

reverse(List) ->
  reverse(List, []).

reverse([Head | Tail], Reversed) ->
  reverse(Tail, [Head | Reversed]);

reverse([], Reversed) ->
  Reversed.

%% match operator

format_temps(List_of_cities) ->
  Converted_List = convert_list_to_c(List_of_cities),
  print_temp(Converted_List),
  {Max_city, Min_city} = find_max_and_min(Converted_List),
  print_max_and_min(Max_city, Min_city).

convert_list_to_c([{Name, {f, F}} | Rest]) ->
  Converted_City = {Name, {c, (F - 32) * 5 / 9}},
  [Converted_City | convert_list_to_c(Rest)];

convert_list_to_c([City | Rest]) ->
  [City | convert_list_to_c(Rest)];

convert_list_to_c([]) ->
  [].

find_max_and_min([City | Rest]) ->
  find_max_and_min(Rest, City, City).

find_max_and_min([{Name, {c, Temp}} | Rest], {Max_Name, {c, Max_Temp}},
                 {Min_Name, {c, Min_Temp}}) ->
  if
    Temp > Max_Temp ->
      Max_City = {Name, {c, Temp}};
    true ->
      Max_City = {Max_Name, {c, Max_Temp}}
  end,
  if 
    Temp < Min_Temp ->
      Min_City = {Name, {c, Temp}};
    true ->
      Min_City = {Min_Name, {c, Min_Temp}}
  end,
  find_max_and_min(Rest, Max_City, Min_City);

find_max_and_min([], Max_City, Min_City) ->
  {Max_City, Min_City}.

print_max_and_min({Max_name, {c, Max_temp}}, {Min_name, {c, Min_temp}}) -> 
  io:format("Max temperature was ~w c in ~w~n", [Max_temp, Max_name]), 
  io:format("Min temperature was ~w c in ~w~n", [Min_temp, Min_name]).

%% testing the if expression (no match blows up!)

test_if(A, B) -> 
  if
    A == 5 ->
      io:format("A = 5~n", []), a_equals_5;
    B == 6 ->
      io:format("B = 6~n", []), b_equals_6;
    A == 2, B == 3 ->
      io:format("A == 2, B == 3~n", []), a_equals_2_b_equals_3;
    A == 1 ; B == 7 ->
      io:format("A == 1 ; B == 7~n", []), a_equals_1_or_b_equals_7
  end.

%% case statement

convert_length_case(Length) -> 
  case Length of
    {centimeter, X} -> 
      {inch, X / 2.54};
    {inch, Y} ->
      {centimeter, Y * 2.54}
  end.

%% built in functions (can be used in guards)

%% trunc(5.6)        => 5
%% round(5.6)        => 6
%% length([a,b,c,d]) => 4
%% float(5)          => 5.00000
%% is_atom(hello)    => true
%% is_list("hello")  => true
%% is_tuple({a,b})   => true

%% other builtins (can't be used in guards)

%% atom_to_list(hello)   => "hello"
%% list_to_atom("good")  => good
%% integer_to_list(22)   => "22"

%% higher order functions

%% Xf = fun(X) -> X * 2 end.
%% Xf(5).

foreach(Fun, [First|Rest]) ->
  Fun(First),
  foreach(Fun, Rest);
foreach(Fun, []) ->
  ok.

map(Fun, [First|Rest]) ->
  [Fun(First)|map(Fun,Rest)];
map(Fun, []) ->
  [].

