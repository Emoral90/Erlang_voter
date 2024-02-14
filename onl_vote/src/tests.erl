-module(tests).

-ifdef(EUNIT).

-include_lib("eunit/include/include/eunit.hrl").
init_vote_test()->
    VotePid = onl_vote:init_vote("Question?"),
    ?_assertNotEqual(undefined, VotePid).

cast_vote_test()->
    VotePid = onl_vote:init_vote("Question?"),
    onl_vote:cast_vote(VotePid, "Option1"),
    onl_vote:cast_vote(VotePid, "Option2"),
    ?_assertNotEqual(undefined, VotePid).

tally_votes_test()->
    VotePid = onl_vote:init_vote("Question?"),
    onl_vote:cast_vote(VotePid, "Option1"),
    onl_vote:cast_vote(VotePid, "Option2"),
    onl_vote:cast_vote(VotePid, "Option2"),
    VotePid ! tally,
    ?_assert(true).

run_tests()->
    ?_test(init_vote_test),
    ?_test(cast_vote_test),
    ?_test(tally_votes_test).

-endif.