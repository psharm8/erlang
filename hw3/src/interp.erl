-module(interp).
-export([scanAndParse/1, runFile/1, runStr/1]).
-include("types.hrl").

loop(InFile, Acc) ->
  case io:request(InFile, {get_until, prompt, lexer, token, [1]}) of
    {ok, Token, _EndLine} ->
      loop(InFile, Acc ++ [Token]);
    {error, token} ->
      exit(scanning_error);
    {eof, _} ->
      Acc
  end.

scanAndParse(FileName) ->
  {ok, InFile} = file:open(FileName, [read]),
  Acc = loop(InFile, []),
  file:close(InFile),
  {Result, AST} = parser:parse(Acc),
  case Result of
    ok -> AST;
    _ -> io:format("Parse error~n")
  end.


-spec runFile(string()) -> valType().
runFile(FileName) ->
  valueOf(scanAndParse(FileName), env:new()).

scanAndParseString(String) ->
  {_ResultL, TKs, _L} = lexer:string(String),
  parser:parse(TKs).

-spec runStr(string()) -> valType().
runStr(String) ->
  {Result, AST} = scanAndParseString(String),
  case Result of
    ok -> valueOf(AST, env:new());
    _ -> io:format("Parse error~n")
  end.


-spec numVal2Num(numValType()) -> integer().
numVal2Num({num, N}) ->
  N.

-spec boolVal2Bool(boolValType()) -> boolean().
boolVal2Bool({bool, B}) ->
  B.

-spec valueOf(expType(), envType()) -> valType().
valueOf(Exp, Env) ->
  case Exp of
    {numExp, {num, _, N}} -> {num, N};
    {isZeroExp, T} ->
      {bool, numVal2Num(valueOf(T, Env)) == 0};
    {plusExp, L, R} -> {num, numVal2Num(valueOf(L, Env)) + numVal2Num(valueOf(R, Env))};
    {diffExp, L, R} -> {num, numVal2Num(valueOf(L, Env)) - numVal2Num(valueOf(R, Env))};
    {idExp, {id, _, Id}} -> env:lookup(Env, Id);
    {letExp, {id, _, Id}, E, In} ->
      Env2 = env:add(Env, Id, valueOf(E, Env)),
      valueOf(In, Env2);
    {procExp, {id, _, Id}, E} -> {proc, Id, E, Env};
    {appExp, I, E} ->
      EVal = valueOf(E, Env),
      R = erlang:element(1, EVal),
      IVal = valueOf(I, Env),
      L = erlang:element(1, IVal),
      if
        R == num ->
          case IVal of
            {proc, Id, PExp, PEnv} ->
              Env2 = env:add(PEnv, Id, EVal),
              valueOf(PExp, Env2);
            {num, N} -> N * numVal2Num(EVal)
          end;
        true -> io:format("Expected proc(num) or num(num) but found ~w(~w).~n", [L, R])
      end;
    {ifThenElseExp, Cond, TrueExp, FalseExp} ->
      case boolVal2Bool(valueOf(Cond, Env)) of
        true -> valueOf(TrueExp, Env);
        false -> valueOf(FalseExp, Env)
      end;
        _ -> io:format("Fail~n")
end .
