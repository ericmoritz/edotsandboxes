%%%--------------------------------------------------------------------- 
%%% Description edotsandboxes_board
%%%--------------------------------------------------------------------- 
%%% Creates and Manipulates a dots and boxes game board
%%%--------------------------------------------------------------------- 

-module(edotsandboxes_board).
-export([grid/1, make_move/3]).

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

-record(grid, {
          size :: integer(),
          marked :: sets(line()),
          boxes :: dict(point(), box())}).

-type sets(_Key) :: term().
-type dict(_Key, _Value) :: term().
-type point() :: {integer(), integer()}.
-type line() :: {point(), point()}.
-type box() :: #box{}.
-type grid() :: #grid{}.
-type line_error() :: too_long | diagonal | out_of_bounds | already_marked.
-type userid() :: term().

%% -----------
%% Public API
%% ------------
%% @doc Creates a square grid of boxes with the position in the top left
-spec grid(Size :: integer()) -> grid().
grid(Size) ->
    Boxes = dict:from_list(lists:map(fun(I) ->
                                             P = index_to_point(I, Size),
                                             {P, #box{position=P}}
                                     end, lists:seq(0, (Size*Size-1)))),
    #grid{size=Size,
          marked=sets:new(),
          boxes=Boxes}.


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

%% --------------------
%% Internal Functions
%% --------------------
-spec index_to_point(integer(), integer()) -> point().
index_to_point(Index, Size) ->
    Y = Index rem Size,
    X = (Index - Y) div Size,
    {X,Y}.
    
-spec validate_move(grid(), line()) -> true | {error, line_error()}.
validate_move(#grid{size=Size}, {{C1, _}, {C2, _}}) when C1 < 0; C2 < 0; C1 > Size - 1; C2 > Size - 1 ->
    {error, out_of_bound};    
validate_move(#grid{size=Size}, {{_, C1}, {_, C2}}) when C1 < 0; C2 < 0; C1 > Size - 1; C2 > Size - 1 ->
    {error, out_of_bound};    
validate_move(_, {{X1, Y1}, {X2, Y2}}) when X2 - X1 > 1;  Y2 - Y1 > 1 ->
    {error, too_long};
validate_move(_, {{X1, Y1}, {X2, Y2}}) when X1 < X2, Y1 < Y2 ->
    {error, diagonal};
validate_move(#grid{marked=Marked}, Line) ->
    case sets:is_element(Line, Marked) of
        true ->
            {error, already_marked};
        false ->
            true
    end.

-spec mark_boxes(userid(), line(), grid()) -> {Points :: integer(), grid()}.
mark_boxes(UserId, Line, #grid{size=Size} = Grid) ->
    Positions = boxes_for_line(Size, Line),
    {Points, Grid2} = lists:foldl(fun(Position, {Points, #grid{boxes=Boxes} = G}) ->
                                Box = mark_border(Line,
                                                   dict:fetch(Position, Boxes)),
                                {Point, Box2} = case {is_closed(Box), Box#box.winner} of
                                            {true, undefined} ->
                                                         {1, Box#box{winner=UserId}};
                                                     _ ->
                                                         {0, Box}
                                        end,
                                Boxes2 = dict:store(Position, Box2, Boxes),
                                {Points + Point, G#grid{boxes=Boxes2}}
                        end, {0, Grid}, Positions),
    {Points, Grid2#grid{marked=sets:add_element(Line, Grid2#grid.marked)}}.


-spec boxes_for_line(GridSize :: integer(), Line :: line()) -> [point()].
%% This function calculates the positions of boxes that a line bisects.
%% This allows us to calculate 
%% only bisects one box.
boxes_for_line(GridSize, {Point1, Point2}) when Point1 > Point2 ->
    % The points are reversed
    boxes_for_line(GridSize, {Point2, Point1});
boxes_for_line(GridSize, {{X,Y}, {X,Y2}}) when Y2 =:= Y+1 -> 
    % This line divides two boxes vertically
    LeftPosition  = {X-1, Y},
    RightPosition = {X, Y},
    UpperBounds = GridSize - 1,

    if 
        % The line is not a left or right border
        X > 0 andalso X < UpperBounds ->
            [LeftPosition, RightPosition];
        % The line is on the left border, return the box to the right of it
        X == 0 ->
            [RightPosition];
        % The line is on the right border, return the box to the left of it
        X == UpperBounds ->
            [LeftPosition]
    end;
boxes_for_line(GridSize, {{X,Y}, {X2,Y}}) when X2 =:= X+1 ->
    % This line divides two boxes horizontally
    TopPosition = {X, Y-1},
    BottomPosition = {X, Y},
    UpperBounds = GridSize - 1,

    % The line is not a top or bottom border
    if 
        Y > 0 andalso Y < UpperBounds ->
            [TopPosition, BottomPosition];
        % The line is the top border, return the box below it
        Y == 0 ->
            [BottomPosition];
        % The line is a bottom border, return the box above it
        Y == UpperBounds ->
            [TopPosition]
    end.

-spec which_border(Position :: point(), Line :: line()) -> top | right | bottom | left.
%% Given a boxes position, return which border a line is part of
which_border(Position, {Point1, Point2}) when Point1 > Point2 ->
    which_border(Position, {Point2, Point1});
which_border({X,Y}, {{X,Y}, {X1,Y}}) when X + 1 =:= X1 ->
    top;
which_border({X,Y}, {{X1,Y}, {X1,Y1}}) when X1 =:= X + 1, Y1 =:= Y + 1 ->
    right;
which_border({X,Y}, {{X, Y1},{X1, Y1}}) when X1 =:= X + 1, Y1 =:= Y + 1 ->
    bottom;
which_border({X,Y}, {{X,Y}, {X, Y1}}) when Y1 =:= Y + 1 ->
    left.

-spec border_value(atom(), boolean(), box()) -> box().
border_value(top, Val, Box) ->
    Box#box{top=Val};
border_value(right, Val, Box) ->
    Box#box{right=Val};
border_value(bottom, Val, Box) ->
    Box#box{bottom=Val};
border_value(left, Val, Box) ->
    Box#box{left=Val}.

-spec mark_border(line(), box()) -> box().
mark_border(Line, #box{position=Position} = Box) ->
    Border =  which_border(Position, Line),
    border_value(Border, true, Box).

-spec is_closed(box()) -> boolean().
is_closed(#box{top=true, right=true, bottom=true, left=true}) ->
    true;
is_closed(_) ->
    false.

-ifdef(TEST).
boxes_for_line_test_() ->
    [
     % top border
     ?_assertEqual([{0,0}],
                   boxes_for_line(3, {{0,0}, {1,0}})),
     % inside and horizontal
     ?_assertEqual([{0,0}, {0,1}],
                   boxes_for_line(3, {{0,1}, {1,1}})),
     % bottom border
     ?_assertEqual([{0,1}],
                   boxes_for_line(3, {{0,2}, {1,2}})),
     % left border
     ?_assertEqual([{0,0}],
                   boxes_for_line(3, {{0,0}, {0,1}})),
     % inside and vertical
     ?_assertEqual([{0,0}, {1,0}],
                   boxes_for_line(3, {{1,0}, {1,1}})),
     % right border
     ?_assertEqual([{1,0}],
                   boxes_for_line(3, {{2,0}, {2,1}}))
     ].

which_border_test_() ->
    [
     ?_assertEqual(top,
                   which_border({0,0}, {{0,0}, {1,0}})),
     ?_assertEqual(right,
                   which_border({0,0}, {{1,0}, {1,1}})),
     ?_assertEqual(bottom,
                   which_border({0,0}, {{0,1}, {1,1}})),
     ?_assertEqual(left,
                   which_border({0,0}, {{0,0}, {0,1}}))
     ].

mark_border_test_() ->
    Box = #box{position={0,0}},
    [
     ?_assertEqual(Box#box{top=true},
                   mark_border({{0,0}, {1,0}}, Box)),
     ?_assertEqual(Box#box{right=true},
                   mark_border({{1,0}, {1,1}}, Box)),
     ?_assertEqual(Box#box{bottom=true},
                   mark_border({{0,1}, {1,1}}, Box)),
     ?_assertEqual(Box#box{left=true},
                   mark_border({{0,0}, {0,1}}, Box))
     ].

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

is_closed_test_() ->
    [
     % 0 0 0 0
     ?_assertEqual(false, is_closed(#box{top=false, right=false,  bottom=false, left=false})),
     % 1 0 0 0
     ?_assertEqual(false, is_closed(#box{top=true, right=false,  bottom=false, left=false})),
     % 1 1 0 0
     ?_assertEqual(false, is_closed(#box{top=true, right=true,  bottom=false, left=false})),
     % 1 1 1 0
     ?_assertEqual(false, is_closed(#box{top=true, right=true,  bottom=true, left=false})),
     % 1 1 1 1
     ?_assertEqual(true, is_closed(#box{top=true, right=true,  bottom=true, left=true})),
     % 0 1 0 0
     ?_assertEqual(false, is_closed(#box{top=false, right=true,  bottom=false, left=false})),
     % 0 1 1 0
     ?_assertEqual(false, is_closed(#box{top=false, right=true,  bottom=true, left=false})),
     % 0 1 1 1
     ?_assertEqual(false, is_closed(#box{top=false, right=true,  bottom=true, left=true})),
     % 0 0 1 0
     ?_assertEqual(false, is_closed(#box{top=false, right=false,  bottom=true, left=false})),
     % 0 0 1 1
     ?_assertEqual(false, is_closed(#box{top=false, right=false,  bottom=true, left=true})),
     % 0 0 0 1
     ?_assertEqual(false, is_closed(#box{top=false, right=false,  bottom=false, left=true}))
     ].

validate_move_test_() ->
    Grid = grid(3),
    Line = {{0,0}, {0,1}},
    MarkedGrid = Grid#grid{marked=sets:add_element(Line, Grid#grid.marked)},

    [
     ?_assertEqual(true, validate_move(Grid, Line)),
     ?_assertEqual({error, already_marked},
                   validate_move(MarkedGrid, Line)),
     ?_assertEqual({error, too_long},
                   validate_move(Grid, {{0,0}, {0,2}})),
     ?_assertEqual({error, too_long},
                   validate_move(Grid, {{0,0}, {2,2}})),
     ?_assertEqual({error, too_long},
                   validate_move(Grid, {{0,0}, {2,0}})),
     ?_assertEqual({error, diagonal},
                   validate_move(Grid, {{0,0}, {1,1}})),
     ?_assertEqual({error, out_of_bound},
                   validate_move(Grid, {{0,-1}, {0,0}})),
     ?_assertEqual({error, out_of_bound},
                   validate_move(Grid, {{-1,0}, {0,0}})),
     ?_assertEqual({error, out_of_bound},
                   validate_move(Grid, {{2,0}, {3,0}})),
     ?_assertEqual({error, out_of_bound},
                   validate_move(Grid, {{0,3}, {0,3}}))
     ].

mark_boxes_test_() ->
    Grid1 = grid(3),

    % Top of the {0,0} box
    {0, Grid2} = mark_boxes(eric, {{0,0}, {1,0}}, Grid1),
    Box2 = dict:fetch({0,0}, Grid2#grid.boxes),

    % Right of the {0,0} box
    {0, Grid3} = mark_boxes(eric, {{1,0}, {1,1}}, Grid2),
    Box3 = dict:fetch({0,0}, Grid3#grid.boxes),
    Box4 = dict:fetch({1,0}, Grid3#grid.boxes),

    % Bottom of the {0,0} box
    {0, Grid4} = mark_boxes(eric, {{0,1}, {1,1}}, Grid3),
    Box5 = dict:fetch({0,0}, Grid4#grid.boxes),
    Box6 = dict:fetch({0,1}, Grid4#grid.boxes),

    % Left of the {0,0} box
    {1, Grid5} = mark_boxes(eric, {{0,0}, {0,1}}, Grid4),
    Box7 = dict:fetch({0,0}, Grid5#grid.boxes),

    [
     ?_assertEqual(#box{position={0,0},
                         top=true}, Box2),
     ?_assertEqual(#box{position={0,0},
                         top=true,
                         right=true}, Box3),
     ?_assertEqual(#box{position={1,0},
                         left=true}, Box4),
     ?_assertEqual(#box{position={0,0},
                         top=true,
                         right=true,
                         bottom=true}, Box5),
     ?_assertEqual(#box{position={0,1},
                         top=true}, Box6),
     ?_assertEqual(#box{position={0,0},
                         top=true,
                         right=true,
                         bottom=true,
                         winner=eric,
                         left=true}, Box7)
    ].
-endif.

