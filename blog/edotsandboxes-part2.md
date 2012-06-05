
## The Game Master

The `Game Master` is the agent that watches over every interaction
that goes on between two Players and keeps them honest.  The `Game
Master` also keep tracks of what the current state of the game is and
alters it when things happen.

Before we can create our `Game Master` agent, we'll have to describe the 
game state that the `Game Master` will manage.

### Game Board

Let's start with a module to manipulate the game board.  This code
will be purely functional and referentially transparent and therefore
easy to test. So let's start with the tests.

Edit `apps/edotsandboxes_server/src/edotsandboxes_game_board.erl`:


    -module(edotsandboxes_game_board).
    
    -ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -endif.
    
    -ifdef(TEST).
    -endif.

This is the basic structure of a module that has eunit tests.  The
first `-ifdef(TEST).` includes the eunit header file only if the
module is under test. The eunit header file contains macros that are
used in testing.

So, what is the first thing we for this game board module.  Well, first we
need to create a new board.

    -module(edotsandboxes_game_board).
    
    -export([create_board/2]).
    
    -ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -endif.
    
    create_board(X, Y) ->
        not_implemented.
    
    
    -ifdef(TEST).
    
    create_board_test_() ->
        [
         ].
    
    -endif.
    
Next we'll start with implementing what are invalid board sizes:


    create_board_test_() ->
        [
         ?_assertEqual({error, invalid_board_size},
                       create_board(2,-1)),
         ?_assertEqual({error, invalid_board_size},
                       create_board(-1,2)),
         ?_assertEqual({error, invalid_board_size},
                       create_board(0,0)),
         ?_assertEqual({error, invalid_board_size},
                       create_board(1,1))
        ].

We can run this test by running:

    rebar eunit skip_deps=true

*Aside:* `skip_deps=true` is not really necessary in right now but get in the
habit of adding it because it's required for the expected result for
things like the "create" command.  If you do "rebar create" without
`skip_deps=true`, rebar will generate the template `./deps/*/src` and
`./src`


