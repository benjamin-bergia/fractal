-include_lib("eunit/include/eunit.hrl").

load_file_test() ->
	TestFile = <<>>,
	[_|_] = load_file(TestFile).

parse_test() ->
	
