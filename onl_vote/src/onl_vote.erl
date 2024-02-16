-module(onl_vote).
-export([start/0]).

-record(voter, {id, name}).
-record(candidate, {id, name}).

% Initiate voting system Pid's
start()->
    Registered_voters_Pid = spawn(?MODULE, registered_voters, [[]]),
    Candidate_id_Pid = spawn(?MODULE, candidates, [[]]),
    Votes_Pid = spawn(?MODULE, votes, [[]]),

% Generate unique voter ID
register_voter(Registered_voters_Pid, Voter_name)->
    Registered_voters_Pid ! {self(), {register_voter, Voter_name}},
    receive
        {Registered_voters_Pid, Voter_id}->
            {ok, Voter_id}
    end.

% Add vote to list of votes
cast_vote(Voter_id, Candidate_id, Votes_Pid)->
    Votes_Pid ! {self(), tally_votes},
    receive
        {Votes_Pid, Result}->
            Result
    end.

% Count votes and return the results
tally_votes()->
    io:format("Tallying votes...~n"),
    ok.