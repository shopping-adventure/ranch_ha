-ifndef(ranch_ha_tests_hrl).
-define(ranch_ha_tests_hrl, true).

-define(log(F, A), 
	ct:pal(info, ?STD_IMPORTANCE, "<~s> " ++ F, [node()] ++ A)).
-define(error(F, A), 
	ct:pal(error, ?HI_IMPORTANCE, "<~s> " ++ F, [node()] ++ A)).

-endif.
