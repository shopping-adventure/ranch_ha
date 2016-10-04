-ifndef(debug).
-define(debug(Msg), error_logger:info_msg(Msg)).
-define(debug(Msg, Data), error_logger:info_msg(Msg, Data)).
-endif.

-ifndef(info).
-define(info(Msg), error_logger:info_msg(Msg)).
-define(info(Msg, Data), error_logger:info_msg(Msg, Data)).
-endif.

-ifndef(warn).
-define(warn(Msg),error_logger:warning_msg(Msg)).
-define(warn(Msg, Data), error_logger:warning_msg(Msg, Data)).
-endif.

-ifndef(error).
-define(error(Msg), error_logger:error_msg(Msg)).
-define(error(Msg, Data), error_logger:error_msg(Msg, Data)).
-endif.

-ifndef(ranch_ha_hrl).
-define(ranch_ha_hrl, true).

-define(monitor_id(Cluster), list_to_atom("monitor_" ++ atom_to_list(Cluster))).
-define(policy_id(Cluster), list_to_atom("policy_" ++ atom_to_list(Cluster))).

-record(cluster, {
	  id        :: atom(),
	  monitor   :: atom(),
	  policy    :: atom()
	 }).

-endif.
