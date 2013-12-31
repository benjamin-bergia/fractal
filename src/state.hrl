%% ------------------------------------------------------------------
%% State record
%% ------------------------------------------------------------------
-record(state, {view_name, engine=one_for_all, threshold=1, state_name=dead, upper_views=[], lower_views=[]}).
