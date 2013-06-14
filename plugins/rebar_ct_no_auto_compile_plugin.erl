-module(rebar_ct_no_auto_compile_plugin).
-export([pre_compile/2, pre_ct/2]).

pre_compile(Config, _AppFile) ->
    case rebar_config:get_local(Config, ct_no_auto_compile, false) of
        false ->
            ok;
        true ->
            move_test_beams_to_ebin()
    end.

pre_ct(Config, _AppFile) ->
    case rebar_config:get_local(Config, ct_no_auto_compile, false) of
        false ->
            ok;
        true ->
            move_test_beams_to_test()
    end.

%% Internal API

move_test_beams_to_ebin() ->
    Cwd = rebar_utils:get_cwd(),
    EbinDir = filename:absname(filename:join(Cwd, "ebin")),
    TestDir = filename:absname(filename:join(Cwd, "test")),
    case {filelib:is_dir(EbinDir), filelib:is_dir(TestDir)} of
        {true, true} ->
            [ mv(Bin, EbinDir) || Bin <- find_files(TestDir, "^.*\\.beam\$")],
            ok;
        _ ->
            %% ebin and/or test non-existent: nothing to do
            ok
    end.

move_test_beams_to_test() ->
    Cwd = rebar_utils:get_cwd(),
    EbinDir = filename:absname(filename:join(Cwd, "ebin")),
    TestDir = filename:absname(filename:join(Cwd, "test")),
    case {filelib:is_dir(EbinDir), filelib:is_dir(TestDir)} of
        {true, true} ->
            [ mv(Bin, TestDir) || Bin <- find_bins(EbinDir, TestDir, "^.*\\.erl\$")],
            ok;
        _ ->
            %% ebin and/or test non-existent: nothing to do
            ok
    end.

%% Helpers

binfile(Src, EbinDir) ->
    filename:join(EbinDir, filename:basename(Src, filename:extension(Src)) ++ ".beam").

find_bins(EbinDir, SrcDir, SrcPattern) ->
    Bins = [binfile(Src, EbinDir) || Src <- rebar_utils:find_files(SrcDir, SrcPattern)],
    [Bin || Bin <- Bins, filelib:is_file(Bin)].

find_files(Dir, Pattern) ->
    [File || File <- rebar_utils:find_files(Dir, Pattern), filelib:is_file(File)].

relative_path(Path, Parent) ->
    Rel = Path -- Parent,
    case Rel of
        Path ->
            Path;
        _ ->
            [_ | Rel2] = Rel,
            Rel2
    end.

mv(Src, SrcDir) ->
    Cwd = rebar_utils:get_cwd(),
    io:format("Moved ~s to ~s~n", [relative_path(Src, Cwd), relative_path(SrcDir, Cwd)]),
    case rebar_file_utils:mv(Src, filename:join(Src, SrcDir)) of
        ok -> ok;
        {error, Reason} -> rebar_utils:abort(Reason, [])
    end.
