%% This example config file can be used to create a a simple view tree
% |< View Name >|  |<---------- List of characteristics of this View ------------>|
%{"Web Server 01", ["Linux", "Debian", "Debian 7.3", "Web", "Apache", "Standalone"]}
%
% Characteritics are a list of items, ideas, concepts, properties that can be attached to this machine.
% Add has many characteristics has possible
% Characteristics names are case sensitive
% On possibility could be to list all the installed packages installed and add them in the Characteristics of te view.
% This way all the machines using the same version of packages will be linked in the view tree.
% id generation will propably not be exactly like in this example but the idea is there 

%% Example:
%% The following lines:
{"Host A", ["Linux", "Debian", "Web"]},
{"Host B", ["Linux", "Debian", "Database"]}
%% Will be translated into:
{view,
	{id, 1},
	{name, "Linux"},
	{all, 
		{engine, weighted_engine},
		{threshold, 1},
		{lowers,
			[{lower,
				{id, 5},
			 	{weight, 1}
			},
			{lower,
				{id, 6},
				{weight, 1}
			}]
		}
	}
},
{view,
	{id, 2},
	{name, "Debian"},
	{all, 
		{engine, weighted_engine},
		{threshold, 1},
		{lowers,
			[{lower,
				{id, 5},
			 	{weight, 1}
			},
			{lower,
				{id, 6},
				{weight, 1}
			}]
		}
	}
},
{view,
	{id, 3},
	{name, "Web"},
	{all, 
		{engine, weighted_engine},
		{threshold, 1},
		{lowers,
			[{lower,
				{id, 5},
			 	{weight, 1}
			}]
		}
	}
},
{view,
	{id, 4},
	{name, "Database"},
	{all, 
		{engine, weighted_engine},
		{threshold, 1},
		{lowers,
			[{lower,
				{id, 6},
			 	{weight, 1}
			}]
		}
	}
},
{view,
	{id, 5},
	{name, "Host A"},
	{all, 
		{engine, weighted_engine},
		{threshold, 1},
		{lowers, []}
	}
},
{view,
	{id, 6},
	{name, "Host B"},
	{all, 
		{engine, weighted_engine},
		{threshold, 1},
		{lowers, []}
	}
},
