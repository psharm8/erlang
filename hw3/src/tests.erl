-module(tests).
-export([start/0]).

start() ->
    lists:map(fun interp:runStr/1,examples()).

examples() ->
    [ ex9()].

ex1() ->
    "let x=1 in let x=3 in +(x,7)".

ex2() ->
    "+(2,3)".

ex3() ->
    "proc (x) +(x,3)".

ex4() ->
    "let y=3 in proc (x) +(x,y)".

ex5() ->
    "let y=3 in +(2,y)".

ex6() ->
    "let y=proc(x) +(x,1) in y(5)".

ex7() ->
    "let x=1 in let y=proc(z) +(z,x) in y(6)".

ex8() ->
    "zero?(7)".

ex9() ->
    "3(4)".

ex10() ->
    "let y = proc(x) if zero?(x) then 0 else x in y(5)".

