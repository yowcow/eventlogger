-module(eventlogger_util).

-export([hexstr/1, tmpdir/1]).

-spec hexstr(binary()) -> string().
hexstr(Bin) ->
    hexstr(Bin, []).

hexstr(<<>>, Acc) ->
    Acc;
hexstr(<<A:4, B:4, T/binary>>, Acc) ->
    hexstr(T, [hexchar(A), hexchar(B) | Acc]).

hexchar(0) ->
    "0";
hexchar(1) ->
    "1";
hexchar(2) ->
    "2";
hexchar(3) ->
    "3";
hexchar(4) ->
    "4";
hexchar(5) ->
    "5";
hexchar(6) ->
    "6";
hexchar(7) ->
    "7";
hexchar(8) ->
    "8";
hexchar(9) ->
    "9";
hexchar(10) ->
    "a";
hexchar(11) ->
    "b";
hexchar(12) ->
    "c";
hexchar(13) ->
    "d";
hexchar(14) ->
    "e";
hexchar(15) ->
    "f".

-spec tmpdir(BasePath :: string()) -> {ok, string()} | {error, term()}.
tmpdir(BasePath) ->
    Dir = hexstr(crypto:hash(sha, float_to_binary(rand:uniform_real()))),
    Path = [BasePath, "-", Dir],
    case file:make_dir(Path) of
        ok ->
            {ok, Path};
        Err ->
            Err
    end.
