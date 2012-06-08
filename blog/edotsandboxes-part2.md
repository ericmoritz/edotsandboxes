## The Game Master

The `Game Master` is the agent that watches over every interaction
that goes on between two Players and keeps them honest.  The `Game
Master` also keep track of what the current state of the game is and
alters it when things happen.

Before we can create our `Game Master` agent, we'll have to describe the 
game state that the `Game Master` will manage.

### Game Board

First we will have to think about how to model the game board.  Let us
think about what elements the game board consists of.  We have dots,
lines and boxes.

Let's create a new module called `src/edotsandboxes_board.erl`:

    -module(edotsandboxes_board.erl).

The code in the `edotsandboxes_board` will be fully functional and
therefore easy to test.  So let's start with the tests:

    -module(edotsandboxes_board).

    -ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -endif.

    -ifdef(TEST).
    -endif.

We can run our tests with rebar:

    $ rebar eunit
    ==> edotsandboxes_server (eunit)
    Compiled src/edotsandboxes_server_app.erl
    Compiled src/edotsandboxes_board.erl
    Compiled src/edotsandboxes_server_sup.erl
      There were no tests to run.

Alright, our tests passed.  Let's ship it.

I kid. It is now time for the real work.  Let us think about what we
elements are part of the game board.  We need a grid of a certain
size:


    -ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
    -endif.
    
    grid(Size) ->
        not_implemented.
    
    -ifdef(TEST).
    
    grid_test_() ->
        [
         ?_assertEqual('???',
                       grid(3))
        ].
    
    -endif.


the `grid_test_/0` function is a test function called a test
generator.  It returns a list of assertions or a tuple that describes
how the test should be ran.  That tuple is outside our current
discussion.  We will meet it soon enough in our future tests where we'll
need setup and tear down functions.

We don't know what our grid should look like just yet, so we assert it
is something that grid/1 doesn't return so that our test fails:

    $ rebar eunit
    ==> edotsandboxes_server (eunit)
    edotsandboxes_board:20: grid_test_ (module
    'edotsandboxes_board')...*failed*
    ::{assertEqual_failed,[{module,edotsandboxes_board},
                         {line,21},
                         {expression,"grid ( 3 )"},
                         {expected,'???'},
                         {value,not_implemented}]}
    
    
    =======================================================
      Failed: 1.  Skipped: 0.  Passed: 0.
    ERROR: One or more eunit tests failed.

So what should the grid be made of?  It's made up of dots, lines and
cells.

Do we need to model the dots? No, they are actually just the position
of the top left of the each box.  Do we need to model the lines?  No
they are the borders of the box. So all we really need is a box and a
grid:

    -ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
     -endif.

    -record(box, {
              position :: point(),
              top = false :: boolean(),
              right = false :: boolean(),
              bottom = false :: boolean(),
              left = false :: boolean(),
              winner :: userid()
             }).
    
    -type point() :: {integer(), integer()}.
    -type userid() :: term().
    -type box() :: #box{}.

Ok, so that's our record.  We added types to each of the fields so
that we can run the Dialyzer static analyzer later.  Dialyzer helps
us ensure that our function calls and returns all match up.  A
mismatch can crop up at run-time in the form of a crash and we are
using Erlang because we want robust systems right?  More on Dialyzer
later.

So our box has a position and four borders.  The borders are marked
true when a player places a line on that border.  Finally we have a
winner field for recording who closed the box. 

So now we have enough to fill in our grid. We could create a list of
lists for rows of columns but we will need to random access to the
cells.  A list of lists would mean that we'd have to do an O(N) look
up every time we need to record a move.  To enable quick lookups we'll
represent our boxes in a dict() that is keyed on its (x,y) position.

To create this dict() we will use lists:seq(0, Size*Size-1) to feed
lists:map/2 but first we need a function to map each item in the
sequence to a {X,Y} position.  We need an `index_to_point/2` function:

    grid(Size) ->
        not_implemented.

    %% --------------------
    %% Internal Functions
    %% --------------------
    -spec index_to_point(integer(), integer()) -> point().
    index_to_point(Index, Size) ->
         not_implemented.

    -ifdef(TEST).
    index_to_point_test_() ->
        [
         ?_assertEqual({0,0},
                       index_to_point(0, 3)),
         ?_assertEqual({0,1},
                       index_to_point(1, 3)),
         ?_assertEqual({0,2},
                       index_to_point(2, 3)),
         ?_assertEqual({1,0},
                       index_to_point(3, 3)),
         ?_assertEqual({1,1},
                       index_to_point(4, 3)),
         ?_assertEqual({1,2},
                       index_to_point(5, 3)),
         ?_assertEqual({2,0},
                       index_to_point(6, 3)),
         ?_assertEqual({2,1},
                       index_to_point(7, 3)),
         ?_assertEqual({2,2},
                       index_to_point(8, 3))
         ].
    
Running `rebar eunit` spits out a ton of errors, lets do some math and
fix that:

    -spec index_to_point(integer(), integer()) -> point().
    index_to_point(Index, Size) ->
        Y = Index rem Size,
        X = (Index - Y) div Size,
        {X,Y}.

Running `rebar eunit` shows that index_to_point/2 is implemented
correctly but our grid function is still not implemented.  So let's
implement that.

First the test:

    grid_test_() ->
        Boxes = dict:from_list([
                 {{0,0}, #box{position={0,0}}},
                 {{0,1}, #box{position={0,1}}},
                 {{0,2}, #box{position={0,2}}},
                 {{1,0}, #box{position={1,0}}},
                 {{1,1}, #box{position={1,1}}},
                 {{1,2}, #box{position={1,2}}},
                 {{2,0}, #box{position={2,0}}},
                 {{2,1}, #box{position={2,1}}},
                 {{2,2}, #box{position={2,2}}}]),
        Grid = #grid{boxes=Boxes},
        [
         ?_assertEqual(Grid,
                       grid(3))
         ].

Let's add the record we need:

    -record(grid, {
                   boxes :: [box()]
                  }).

And now the implementation:

    grid(Size) ->
        Boxes = dict:from_list(lists:map(fun(I) ->
                                  Position = index_to_point(I, Size),
                                  {Position, #box{position=Position}}
                          end, lists:seq(0, Size * Size - 1))),
        #grid{boxes=Boxes}.

To explain the rest of the module here would make this post enormous.
As much as I want to show off how clever I am, I'll let the code do
the talking.  

Before you go off and read my awesome code, I want to explain the
concept of "let it crash".  To greatly simplify our code, if we
validate the inputs of our public functions, we can guarantee what
values and types are passed to our internal functions.

The another public function for this module is the `make_move/3` function:

    %% @doc places a line onto the board for using UserId
    -spec make_move(userid(), line(), grid()) -> {ok, {Points :: integer(), grid()}} | {error, line_error()}.
    make_move(UserId, {Point1, Point2}, Grid) when Point1 > Point2 ->
        make_move(UserId, {Point2, Point1}, Grid);
    make_move(UserId, Line, Grid) ->    
        case validate_move(Grid, Line) of
            true ->
                {ok, mark_boxes(UserId, Line, Grid)};
            {error, Reason} ->
                {error, Reason}
        end.

The first head of this function makes certain that the points in a
line segment is in ascending order.  All our internal functions make
this assumption so we have to enforce that constraint in our public
functions.

In the second head, we validate the move before we pass it onto our
internal code.

I'll let the code of `validate_move/2` speak for itself but it basically
returns an error if the line is out of bounds, longer than 1 box or
diagonal. It also ensures that the move has not already been played.

Now all our internal code will only crash if our spec is wrong or we
programmed it wrong.  In either case, we as a programmer want to know
about it.

Later we'll introduce the concept of the error kernel which this code
is not part of. The error kernel keeps the game state safe in case the
process crashes due to our fallible nature. The supervisor will
restart the process, the state will be restored and no one notices but
the error log.

Since I mentioned programming things wrong, let's talk about Dialyzer.

Dialyzer analyzes your code to make sure that it meets
expectations. Before we do that, we will need to build the plt file
from the Erlang binaries.  This will take a while so run it and take a
five minute break:

    dialyzer --build_plt --apps erts kernel stdlib

That base plt file is stored at ~/.dialyzer_plt and serves as a base
to build on.  Keeping this cache around keeps us from having to run
that time consuming process again.

Now for the moment of truth:

    $ dialyzer --src src/
      Checking whether the PLT /Users/eric/.dialyzer_plt is
      up-to-date... yes
      Proceeding with analysis...
    edotsandboxes_board.erl:25: Function grid/1 will never be called
    edotsandboxes_board.erl:26: The created fun has no local return
    edotsandboxes_board.erl:36: Function index_to_point/2 will never be
      called
     done in 0m0.69s
    done (warnings were emitted)

Ok, the first problem is easily fixed:

    -module(edotsandboxes_board).
    -export([grid/1]).

Rerunning Dialyzer shows we did indeed fix the first problem but more
showed up:

    $ dialyzer --src src/
      Checking whether the PLT /Users/eric/.dialyzer_plt is
      up-to-date... yes
      Proceeding with analysis...
    edotsandboxes_board.erl:25: Function grid/1 has no local return
    edotsandboxes_board.erl:30: Record construction #grid{boxes::dict()}
      violates the declared type of field boxes::'undefined' |
      [#box{position::'undefined' |
      {integer(),integer()},top::boolean(),right::boolean(),bottom::boolean(),left::boolean()}]
     done in 0m0.69s
    done (warnings were emitted)

The "no local return" error is a bit confusing. When I look at the
code I notice that I did not have a -spec to grid/1.

    -type grid() :: #grid{}.

    -spec grid(Size :: integer()) -> grid().
    grid(Size) ->
       ...

Dialyzer continues to complain.  Let's fix the next error. Looks like
our #grid.boxes type is wrong. Whoops, I swear I didn't do that on
purpose. Let's fix it like so:

    -record(grid, {
               boxes :: dict()
             }).

Looks like that fixed both the problems:

    $ dialyzer --src src/
      Checking whether the PLT /Users/eric/.dialyzer_plt is
      up-to-date... yes
      Proceeding with analysis... done in 0m0.73s
    done (passed successfully)

To demonstrate the power of Dialyzer, let's break something:

First let's create a working function that uses the grid record:

    -export([grid/1, to_list/1]).

    ...

    -spec to_list(Grid :: grid()) -> list().
    to_list(Grid) ->
        lists:sort(dict:to_list(Grid#grid.boxes)).

Running Dialyzer shows that everything is ay ok:

    $ dialyzer --src src/
      Checking whether the PLT /Users/eric/.dialyzer_plt is
      up-to-date... yes
      Proceeding with analysis... done in 0m0.72s
    done (passed successfully)

Let's change our implementation of #grid{}:

    -spec grid(Size :: integer()) -> grid().
    grid(Size) ->
    Boxes = orddict:from_list(lists:map(fun(I) ->
                                             Position = index_to_point(I, Size),
                                             {Position, #box{position=Position}}
                                     end, lists:seq(0, Size * Size - 1))),
    #grid{boxes=Boxes}.

We are now using an orddict for #grid.boxes:

    $ dialyzer --src src/
      Checking whether the PLT /Users/eric/.dialyzer_plt is
      up-to-date... yes
      Proceeding with analysis...
    edotsandboxes_board.erl:26: Function grid/1 has no local return
    edotsandboxes_board.erl:31: Record construction #grid{boxes::[{_,_}]}
      violates the declared type of field boxes::'undefined' | dict()
     done in 0m0.69s
    done (warnings were emitted)

Let's fix the type of #grid.boxes:

    -record(grid, {
              boxes :: [{point(), box()}]
             }).

    -type box() :: #box{}.    

And Dialyzer says:
    
    $ dialyzer --src src/
      Checking whether the PLT /Users/eric/.dialyzer_plt is
      up-to-date... yes
      Proceeding with analysis...
    edotsandboxes_board.erl:35: Function to_list/1 has no local return
    edotsandboxes_board.erl:36: The call dict:to_list('undefined' |
      [{{integer(),integer()},#box{position::'undefined' |
      {integer(),integer()},top::boolean(),right::boolean(),bottom::boolean(),left::boolean()}}])
      does not have an opaque term of type dict() as 1st argument
     done in 0m0.67s
    done (warnings were emitted)

Oh look it caught that we forgot to change `to_list/1` to use a
orddict:

    -spec to_list(Grid :: grid()) -> list().
    to_list(Grid) ->
        orddict:to_list(Grid#grid.boxes).

Let us see if that fixed it:

    $ dialyzer --src src/
      Checking whether the PLT /Users/eric/.dialyzer_plt is
      up-to-date... yes
      Proceeding with analysis... done in 0m0.74s
    done (passed successfully)

Sweet, time for a beer.

## Conclusion

This concludes part two of this tutorial.  We learned how to write a
purely functional module and how test driven development can help
us. We also explored how validating public API inputs keeps error
handling code out of our internal functions to keep them
simple. Finally, we explored how to protect ourselves from ourselves
with Dialyzer.

In part 3 I swear we will write the Game Master `gen_server`.
