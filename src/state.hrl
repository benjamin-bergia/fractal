%% ------------------------------------------------------------------
%% State record
%% ------------------------------------------------------------------
-record(state, {view_name, engine=one_for_all, threshold=1, state_name=dead, upper_views=[], lower_views=[]}).
%% Name / Engine / Weight / UpperViews / LowerViews 
-define(LOWERS(L), {L, dead, 1}).
-define(VIEW(S), {S#state.view_name, {view, start_link, [S]}, permanent, 5000, worker, [view]}).
