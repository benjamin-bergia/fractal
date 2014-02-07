%% Example of complex view where engine threshold and lowers are different for each status
{view,
	{id, 1},
	{name, "Host A"},
	{dead, 
		{engine, weighted_engine},
		{threshold, 3},
		{lowers,
			[{lower,
				{id, 2},
			 	{weight, 1}
			},
			{lower,
				{id, 3},
				{weight, 2}
			}]
		}
	},
	{alive, 
		{engine, weighted_engine},
		{threshold, 1},
		{lowers,
			[{lower,
				{id, 2},
			 	{weight, 1}
			}]
		}
	},
	{suspicious, 
		{engine, weighted_engine},
		{threshold, 3},
		{lowers,
			[{lower,
				{id, 2},
			 	{weight, 1}
			},
			{lower,
				{id, 5},
				{weight, 1}
			}]
		}
	}
}.
