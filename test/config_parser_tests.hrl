-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% Unit Tests
%% ------------------------------------------------------------------

get_config_test() ->
	Path = "pathtothefile",
	{test, {test, test}} = get_config(Path).
