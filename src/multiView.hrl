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
}.
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
}.
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
}.
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
}.
{view,
	{id, 5},
	{name, "Host A"},
	{all, 
		{engine, weighted_engine},
		{threshold, 1},
		{lowers, []}
	}
}.
{view,
	{id, 6},
	{name, "Host B"},
	{all, 
		{engine, weighted_engine},
		{threshold, 1},
		{lowers, []}
	}
}.
