-module(onl_vote).
-export([start/0, init_vote/1, cast_vote/2, tally_votes/0, vote/2, tally/1]).

% Start the voting system
start() ->
    spawn(?MODULE, vote, []).

% Initiate a new vote with a given question
init_vote(Question) ->
    VotePid = spawn(fun() -> vote(Question, []) end),
    VotePid.

% Cast a vote for a given option in a vote process
cast_vote(VotePid, Option) ->
    VotePid ! {self(), Option}.

% Tally the votes collected in a vote process
tally_votes() ->
    tally([]).

% Internal function to manage the voting process
vote(Question, Votes) ->
    receive
        {Voter, Option} ->
            NewVotes = [{Voter, Option} | Votes],
            vote(Question, NewVotes);
        tally ->
            io:format("Tally for vote: ~p~n", [Votes]),
            tally(Votes)
    end.

% Tally the votes
tally(Votes) ->
    % Here you can perform the tallying logic
    % For demonstration purposes, we simply print out the votes
    io:format("Final Tally: ~p~n", [Votes]).