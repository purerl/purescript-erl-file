-module(erl_standardResult@foreign).
-export([standardResultToEither_/3]).

standardResultToEither_(Left, Right, Result) ->
  case Result of
    error -> Left(unit);
    {error, X} -> Left(X);
    Z when is_tuple(Z), element(1, Z) =:= error -> Left(erlang:delete_element(1, Z));
    ok -> Right(unit);
    {ok, X} -> Right(X);
    Z when is_tuple(Z), element(1, Z) =:= ok -> Right(erlang:delete_element(1, Z))
  end.