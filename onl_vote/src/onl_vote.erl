-module(onl_vote).
-export([start/0, register_voter/2, register_candidate/2, cast_vote/3, registered_voters/1, registered_candidates/1, candidates/1, votes/1, tally_votes/1]).


% Initiate voting system Pid's
start()->
    Voter_pid = spawn(?MODULE, registered_voters, [[]]),
    Candidate_pid = spawn(?MODULE, candidates, [[]]),
    Votes_pid = spawn(?MODULE, votes, [[]]),
    {Voter_pid, Candidate_pid, Votes_pid}.


% Client Functions

% Generate unique voter ID
register_voter(Voter_pid, Voter_name)->
    Voter_pid ! {self(), {register_voter, Voter_name}},
    receive
        {Voter_pid, Voter_id}->
            {ok, Voter_id}
    end.

% Generate unique candidate ID
register_candidate(Candidate_pid, Candidate_name)->
    Candidate_pid ! {self(), {register_voter, Candidate_name}},
    receive
        {Candidate_pid, Candidate_id}->
            {ok, Candidate_id}
    end.

% Add vote to list of votes
cast_vote(Voter_id, Candidate_id, Votes_pid)->
    Votes_pid ! {self(), {cast_vote, Voter_id, Candidate_id}},
    ok.


% Server functions

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

registered_candidates(Registered_candidates)->
    receive
        {From, {register_candidate, Candidate_name}}->
            Candidate_id = make_ref(),
            New_reg_cands = [{Candidate_id, Candidate_name} | Registered_candidates],
            From ! {self(), Candidate_id},
            Registered_candidates(New_reg_cands);
        _->
            Registered_candidates(Registered_candidates)
    end.

candidates(Candidates)->
    receive
        {From, {register_candidate, Candidate_name}}->
            Candidate_id = make_ref(),
            New_cands = [{Candidate_id, Candidate_name} | Candidates],
            From ! {self(), Candidate_id},
            candidates(New_cands);
        {From, get_candidates}->
            From ! {self(), Candidates},
            candidates(Candidates);
        {timeout, _}->
            io:format("Received unexpected message or timeout~n"),
            candidates(Candidates) 
    end.

votes(Votes)->
    receive
        {From, {cast_vote, Voter_id, Candidate_id}}->
            New_votes = [{Voter_id, Candidate_id} | Votes],
            From ! ok,
            votes(New_votes);
        {From, tally_votes}->
            Total = tally_votes(Votes),
            From ! {self(), Total},
            votes(Votes);
        {timeout, _}->
            io:format("Received unexpected message or timeout~n"),
            votes(Votes)
    end.

% Count votes and return the results
tally_votes(Votes)->
    Vote_count = lists:foldl(
        fun({_, CandidateId}, Acc) ->
            lists:keyreplace(CandidateId, 1, Acc, {CandidateId, 1})
        end,
        dict:new(),
        Votes),
    io:format("Vote count: ~p~n", [dict:to_list(Vote_count)]).


% {Voters_pid, Candidate_pid, Votes_pid} = onl_vote:start().
% 
% onl_vote:register_voter(Voters_pid, "Abby").
% onl_vote:register_voter(Voters_pid, "Bryan").
% onl_vote:register_voter(Voters_pid, "Erick").
% 
% onl_vote:register_candidate(Candidate_pid, "Karl Benz").
% onl_vote:register_candidate(Candidate_pid, "Henry Ford").
% 
% onl_vote:cast_vote(Voter_id_Abby, Candidate_id_Henry_Ford, Votes_pid).
% onl_vote:cast_vote(Voter_id_Erick, Candidate_id_Karl_Benz, Votes_pid).
% onl_vote:cast_vote(Voter_id_Bryan, Candidate_id_Henry_Ford, Votes_pid).
% 
% 