%%%--------------------------------------------------------------------- 
%%% Description edotsandboxes_board
%%%--------------------------------------------------------------------- 
%%% Creates and Manipulates a dots and boxes game board
%%%--------------------------------------------------------------------- 

-module(edotsandboxes_board).
-export([new/1, make_move/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(cell, {
          origin :: point(),
          top = false :: boolean(),
          right = false :: boolean(),
          bottom = false :: boolean(),
          left = false :: boolean(),
          winner :: userid()
         }).

-record(grid, {
          size :: integer(),
          marked :: sets(line()),
          cells :: dict(point(), cell())}).

-type sets(_Key) :: term().
-type dict(_Key, _Value) :: term().
-type point() :: {integer(), integer()}.
-type line() :: {point(), point()}.
-type index() :: integer().
-type cell() :: #cell{}.
-type grid() :: #grid{}.
-type line_error() :: too_long | diagonal | out_of_bounds | already_marked.
-type userid() :: term().

%% -----------
%% Public API
%% ------------
%% @doc Creates a square grid of cells with the origin in the top left
-spec new(Size :: integer()) -> grid().
new(Size) ->
    Cells = dict:from_list(lists:map(fun(I) ->
                                             P = index_to_point(I, Size),
                                             {P, #cell{origin=P}}
                                     end, lists:seq(0, (Size*Size-1)))),
    #grid{size=Size,
          marked=sets:new(),
          cells=Cells}.


%% @doc places a line onto the board for using UserId
-spec make_move(userid(), line(), grid()) -> {ok, {Points :: integer(), grid()}} | {error, line_error()}.
make_move(UserId, {Point1, Point2}, Grid) when Point1 > Point2 ->
    make_move(UserId, {Point2, Point1}, Grid);
make_move(UserId, Line, Grid) ->    
    case validate_move(Grid, Line) of
        true ->
            {ok, mark_cells(UserId, Line, Grid)};
        {error, Reason} ->
            {error, Reason}
    end.

%% --------------------
%% Internal Functions
%% --------------------
-spec point_to_index(point(), integer()) -> index().
point_to_index({X,Y}, Size) ->
    Size * Y + X.

-spec index_to_point(integer(), integer()) -> point().
index_to_point(Index, Size) ->
    X = Index rem Size,
    % Size * Y + X = Index,
    % Size * Y = Index - X,
    Y = (Index - X) div Size,
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

-spec mark_cells(userid(), line(), grid()) -> {Points :: integer(), grid()}.
mark_cells(UserId, Line, #grid{size=Size} = Grid) ->
    Origins = cells_for_line(Size, Line),
    {Points, Grid2} = lists:foldl(fun(Origin, {Points, #grid{cells=Cells} = G}) ->
                                Cell = mark_border(Line,
                                                   dict:fetch(Origin, Cells)),
                                {Point, Cell2} = case {is_closed(Cell), Cell#cell.winner} of
                                            {true, undefined} ->
                                                         {1, Cell#cell{winner=UserId}};
                                                     _ ->
                                                         {0, Cell}
                                        end,
                                Cells2 = dict:store(Origin, Cell2, Cells),
                                {Points + Point, G#grid{cells=Cells2}}
                        end, {0, Grid}, Origins),
    {Points, Grid2#grid{marked=sets:add_element(Line, Grid2#grid.marked)}}.


-spec cells_for_line(GridSize :: integer(), Line :: line()) -> [point()].
%% This function calculates the origins of cells that a line bisects.
%% This allows us to calculate 
%% only bisects one cell.
cells_for_line(GridSize, {Point1, Point2}) when Point1 > Point2 ->
    % The points are reversed
    cells_for_line(GridSize, {Point2, Point1});
cells_for_line(GridSize, {{X,Y}, {X,Y2}}) when Y2 =:= Y+1 -> 
    % This line divides two cells vertically
    LeftOrigin  = {X-1, Y},
    RightOrigin = {X, Y},
    UpperBounds = GridSize - 1,

    if 
        % The line is not a left or right border
        X > 0 andalso X < UpperBounds ->
            [LeftOrigin, RightOrigin];
        % The line is on the left border, return the box to the right of it
        X == 0 ->
            [RightOrigin];
        % The line is on the right border, return the box to the left of it
        X == UpperBounds ->
            [LeftOrigin]
    end;
cells_for_line(GridSize, {{X,Y}, {X2,Y}}) when X2 =:= X+1 ->
    % This line divides two cells horizontally
    TopOrigin = {X, Y-1},
    BottomOrigin = {X, Y},
    UpperBounds = GridSize - 1,

    % The line is not a top or bottom border
    if 
        Y > 0 andalso Y < UpperBounds ->
            [TopOrigin, BottomOrigin];
        % The line is the top border, return the box below it
        Y == 0 ->
            [BottomOrigin];
        % The line is a bottom border, return the box above it
        Y == UpperBounds ->
            [TopOrigin]
    end.

-spec which_border(Origin :: point(), Line :: line()) -> {ok, top | right | bottom | left}.
%% Given a cells origin, return which border a line is part of
which_border(Origin, {Point1, Point2}) when Point1 > Point2 ->
    which_border(Origin, {Point2, Point1});
which_border({X,Y}, {{X,Y}, {X1,Y}}) when X + 1 =:= X1 ->
    top;
which_border({X,Y}, {{X1,Y}, {X1,Y1}}) when X1 =:= X + 1, Y1 =:= Y + 1 ->
    right;
which_border({X,Y}, {{X, Y1},{X1, Y1}}) when X1 =:= X + 1, Y1 =:= Y + 1 ->
    bottom;
which_border({X,Y}, {{X,Y}, {X, Y1}}) when Y1 =:= Y + 1 ->
    left.

border_value(top, Val, Cell) ->
    Cell#cell{top=Val};
border_value(right, Val, Cell) ->
    Cell#cell{right=Val};
border_value(bottom, Val, Cell) ->
    Cell#cell{bottom=Val};
border_value(left, Val, Cell) ->
    Cell#cell{left=Val}.

-spec mark_border(cell(), line()) -> {ok, cell()}.
mark_border(Line, #cell{origin=Origin} = Cell) ->
    Border =  which_border(Origin, Line),
    border_value(Border, true, Cell).

-spec is_closed(cell()) -> boolean().
is_closed(#cell{top=true, right=true, bottom=true, left=true}) ->
    true;
is_closed(_) ->
    false.

-ifdef(TEST).
cells_for_line_test_() ->
    [
     % top border
     ?_assertEqual([{0,0}],
                   cells_for_line(3, {{0,0}, {1,0}})),
     % inside and horizontal
     ?_assertEqual([{0,0}, {0,1}],
                   cells_for_line(3, {{0,1}, {1,1}})),
     % bottom border
     ?_assertEqual([{0,1}],
                   cells_for_line(3, {{0,2}, {1,2}})),
     % left border
     ?_assertEqual([{0,0}],
                   cells_for_line(3, {{0,0}, {0,1}})),
     % inside and vertical
     ?_assertEqual([{0,0}, {1,0}],
                   cells_for_line(3, {{1,0}, {1,1}})),
     % right border
     ?_assertEqual([{1,0}],
                   cells_for_line(3, {{2,0}, {2,1}}))
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
    Cell = #cell{origin={0,0}},
    [
     ?_assertEqual(Cell#cell{top=true},
                   mark_border({{0,0}, {1,0}}, Cell)),
     ?_assertEqual(Cell#cell{right=true},
                   mark_border({{1,0}, {1,1}}, Cell)),
     ?_assertEqual(Cell#cell{bottom=true},
                   mark_border({{0,1}, {1,1}}, Cell)),
     ?_assertEqual(Cell#cell{left=true},
                   mark_border({{0,0}, {0,1}}, Cell))
     ].

index_to_point_test_() ->
    [
     ?_assertEqual({0,0},
                   index_to_point(0, 2)),
     ?_assertEqual({1,0},
                   index_to_point(1, 2)),
     ?_assertEqual({0,1},
                   index_to_point(2, 2)),
     ?_assertEqual({1,1},
                   index_to_point(3, 2))
     ].

point_to_index_test_() ->
    [
     ?_assertEqual(0,
                   point_to_index({0,0}, 2)),
     ?_assertEqual(1,
                   point_to_index({1,0}, 2)),
     ?_assertEqual(2,
                   point_to_index({0,1}, 2)),
     ?_assertEqual(3,
                   point_to_index({1,1}, 2))
     ].

is_closed_test_() ->
    [
     % 0 0 0 0
     ?_assertEqual(false, is_closed(#cell{top=false, right=false,  bottom=false, left=false})),
     % 1 0 0 0
     ?_assertEqual(false, is_closed(#cell{top=true, right=false,  bottom=false, left=false})),
     % 1 1 0 0
     ?_assertEqual(false, is_closed(#cell{top=true, right=true,  bottom=false, left=false})),
     % 1 1 1 0
     ?_assertEqual(false, is_closed(#cell{top=true, right=true,  bottom=true, left=false})),
     % 1 1 1 1
     ?_assertEqual(true, is_closed(#cell{top=true, right=true,  bottom=true, left=true})),
     % 0 1 0 0
     ?_assertEqual(false, is_closed(#cell{top=false, right=true,  bottom=false, left=false})),
     % 0 1 1 0
     ?_assertEqual(false, is_closed(#cell{top=false, right=true,  bottom=true, left=false})),
     % 0 1 1 1
     ?_assertEqual(false, is_closed(#cell{top=false, right=true,  bottom=true, left=true})),
     % 0 0 1 0
     ?_assertEqual(false, is_closed(#cell{top=false, right=false,  bottom=true, left=false})),
     % 0 0 1 1
     ?_assertEqual(false, is_closed(#cell{top=false, right=false,  bottom=true, left=true})),
     % 0 0 0 1
     ?_assertEqual(false, is_closed(#cell{top=false, right=false,  bottom=false, left=true}))
     ].

validate_move_test_() ->
    Grid = new(3),
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

mark_cells_test_() ->
    Grid1 = new(3),

    % Top of the {0,0} cell
    {0, Grid2} = mark_cells(eric, {{0,0}, {1,0}}, Grid1),
    Cell2 = dict:fetch({0,0}, Grid2#grid.cells),

    % Right of the {0,0} cell
    {0, Grid3} = mark_cells(eric, {{1,0}, {1,1}}, Grid2),
    Cell3 = dict:fetch({0,0}, Grid3#grid.cells),
    Cell4 = dict:fetch({1,0}, Grid3#grid.cells),

    % Bottom of the {0,0} cell
    {0, Grid4} = mark_cells(eric, {{0,1}, {1,1}}, Grid3),
    Cell5 = dict:fetch({0,0}, Grid4#grid.cells),
    Cell6 = dict:fetch({0,1}, Grid4#grid.cells),

    % Left of the {0,0} cell
    {1, Grid5} = mark_cells(eric, {{0,0}, {0,1}}, Grid4),
    Cell7 = dict:fetch({0,0}, Grid5#grid.cells),

    [
     ?_assertEqual(#cell{origin={0,0},
                         top=true}, Cell2),
     ?_assertEqual(#cell{origin={0,0},
                         top=true,
                         right=true}, Cell3),
     ?_assertEqual(#cell{origin={1,0},
                         left=true}, Cell4),
     ?_assertEqual(#cell{origin={0,0},
                         top=true,
                         right=true,
                         bottom=true}, Cell5),
     ?_assertEqual(#cell{origin={0,1},
                         top=true}, Cell6),
     ?_assertEqual(#cell{origin={0,0},
                         top=true,
                         right=true,
                         bottom=true,
                         winner=eric,
                         left=true}, Cell7)
    ].
-endif.

