-module(onl_vote).

-export([start/0]).


start_adder()->
    spawn(?MODULE, adder, []).

adder()->
    recieve
        {Pid, N1, N2}->
            Pid ! N1+N2
    end,
    adder().

add(Pid, N1, N2)->
    Pid ! {self(), N1, N2},
    recieve
        Response->
            Response
    end.