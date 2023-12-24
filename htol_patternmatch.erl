-module(htol_patternmatch).
-export([main/0]).
-compile({no_auto_import,[min/2]}).

main() ->
    %% A1, B1, X1, A2, B2, X2, ...
    RawMap = [50, 10, 30, 5, 90, 20, 40, 2, 25, 10, 8, 0],
    Map = parse_map(RawMap),
    lists:reverse(calc_path(Map), []).

parse_map([]) -> [];
parse_map([A,B,X|Rest]) ->
    [{{a,A},{b,B},{x,X}}] ++ parse_map(Rest).

min(Path1, Path2, Path3, Path4) ->
    Cost1 = lists:foldl(fun({_, Val}, Acc) -> Acc + Val end, 0, Path1),
    Cost2 = lists:foldl(fun({_, Val}, Acc) -> Acc + Val end, 0, Path2),
    Cost3 = lists:foldl(fun({_, Val}, Acc) -> Acc + Val end, 0, Path3),
    Cost4 = lists:foldl(fun({_, Val}, Acc) -> Acc + Val end, 0, Path4),
    MinCost = lists:min([Cost1,Cost2,Cost3,Cost4]),
    case MinCost of
        Cost1 -> Path1;
        Cost2 -> Path2;
        Cost3 -> Path3;
        Cost4 -> Path4
    end.

min(Path1, Path2) ->
    Cost1 = lists:foldl(fun({_, Val}, Acc) -> Acc + Val end, 0, Path1),
    Cost2 = lists:foldl(fun({_, Val}, Acc) -> Acc + Val end, 0, Path2),
    MinCost = lists:min([Cost1,Cost2]),
    case MinCost of
        Cost1 -> Path1;
        Cost2 -> Path2
    end.

calc_path([{{a,A},{b,B},{x,X}}|Rest]) ->
    Path1 = calc_path(Rest, [{a,A}]),
    Path2 = calc_path(Rest, [{x,X},{a,A}]),
    Path3 = calc_path(Rest, [{b,B}]),
    Path4 = calc_path(Rest, [{x,X},{b,B}]),
    min(Path1,Path2,Path3,Path4).

calc_path([{{a,_},{b,B},{x,0}}], [{x,X1},{a,A1}|RestPath]) ->
    [{b,B},{x,X1},{a,A1}|RestPath];
calc_path([{{a,A},{b,_},{x,0}}], [{x,X1},{b,B1}|RestPath]) ->
    [{a,A},{x,X1},{b,B1}|RestPath];
calc_path([{{a,A},{b,_},{x,0}}], [{a,A1}|RestPath]) ->
    [{a,A},{a,A1}|RestPath];
calc_path([{{a,_},{b,B},{x,0}}], [{b,B1}|RestPath]) ->
    [{b,B},{b,B1}|RestPath];
calc_path([{{a,_},{b,B},{x,X}}|Rest], [{x,X1},{a,A1}|RestPath]) ->
    Path1 = calc_path(Rest, [{b,B},{x,X1},{a,A1}|RestPath]),
    Path2 = calc_path(Rest, [{x,X},{b,B},{x,X1},{a,A1}|RestPath]),
    min(Path1, Path2);
calc_path([{{a,A},{b,_},{x,X}}|Rest], [{x,X1},{b,B1}|RestPath]) ->
    Path1 = calc_path(Rest, [{a,A},{x,X1},{b,B1}|RestPath]),
    Path2 = calc_path(Rest, [{x,X},{a,A},{x,X1},{b,B1}|RestPath]),
    min(Path1, Path2);
calc_path([{{a,A},{b,_},{x,X}}|Rest], [{a,A1}|RestPath]) ->
    Path1 = calc_path(Rest, [{a,A},{a,A1}|RestPath]),
    Path2 = calc_path(Rest, [{x,X},{a,A},{a,A1}|RestPath]),
    min(Path1, Path2);
calc_path([{{a,_},{b,B},{x,X}}|Rest], [{b,B1}|RestPath]) ->
    Path1 = calc_path(Rest, [{b,B},{b,B1}|RestPath]),
    Path2 = calc_path(Rest, [{x,X},{b,B},{b,B1}|RestPath]),
    min(Path1, Path2).
