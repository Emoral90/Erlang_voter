-module(onl_vote).
-export([start/0, register_voter/2, cast_vote/3, registered_voters/1, candidates/1, votes/1, tally_votes/0]).

% -record(voter, {id, name}).
% -record(candidate, {id, name}).

% Initiate voting system Pid's
start()->
    Registered_voters_Pid = spawn(?MODULE, registered_voters, [[]]),
    Candidate_id_Pid = spawn(?MODULE, candidates, [[]]),
    Votes_Pid = spawn(?MODULE, votes, [[]]),
    ok.

% Generate unique voter ID
register_voter(Registered_voters_Pid, Voter_name)->
    Registered_voters_Pid ! {self(), {register_voter, Voter_name}},
    receive
        {Registered_voters_Pid, Voter_id}->
            {ok, Voter_id}
    end.

% Add vote to list of votes
cast_vote(Voter_id, Candidate_id, Votes_Pid)->
    Votes_Pid ! {self(), {cast_vote, Voter_id, Candidate_id}},
    ok.

% Client functions

registered_voters(Registered_voters)->
    receive
        {From, {register_voter, Voter_name}}->
            Voter_id = make_ref(),
            New_reg_voters = [{Voter_id, Voter_name} | Registered_voters],
            From ! {self(), Voter_id},
            registered_voters(New_reg_voters);
        _->
            registered_voters(Registered_voters)
    end.

candidates(Candidates)->
    receive
        {From, {add_candidate, Candidate_name}}->
            Candidate_id = make_ref(),
            New_cands = [{Candidate_id, Candidate_name} | Candidates],
            From ! {self(), Candidate_id},
            candidates(New_cands);
        {From, get_candidates}->
            From ! {self(), Candidates},
            candidates(Candidates)
    end.

votes(Votes)->
    receive
        {From, {cast_vote, Voter_id, Candidate_id}}->
            New_votes = [{Voter_id, Candidate_id} | Votes],
            From ! ok,
            votes(New_votes);
        {From, tally_votes}->
            Total = tally_votes(),
            From ! {self(), Total},
            votes(Votes)
    end.

% Count votes and return the results
tally_votes()->
    
    io:format("Tallying votes...~n"),
    ok.