-module(io).
-export([start/0]).

start()->
    VotePid = onl_vote:start(),
    io:format("Welcome"),
    io:format("Enter your question: "),
    Question = read_line(),
    VoteProcess = onl_vote:init_vote(Question),
    handle_voting(VoteProcess).

handle_voting(VotePid)->
    io:format("Enter your vote (or 'done' to finish voting): "),
    Option = read_line(),
    case Option of
        "done"->
            VotePid ! tally,
            io:format("Voting is now closed"),;
        ->
            onl_vote:cast_vote(VotePid, Option),
            handle_voting(VotePid)
    end.