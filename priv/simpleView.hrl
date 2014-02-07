%% Example of simple view with two lower views
{view,
	{id, 1},
	{name, "Host A"},
	{all, 
		{engine, weighted_engine},
		{threshold, 3},
		{lowers, [{lower, {id, 2}, {weight, 1}},
			  {lower, {id, 3}, {weight, 2}}]
		}
	}
}.
