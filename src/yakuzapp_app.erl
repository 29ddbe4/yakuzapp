%%%-------------------------------------------------------------------
%% @doc yakuzapp public API
%% @end
%%%-------------------------------------------------------------------

-module(yakuzapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

-export([add_enemy/2
	,add_friend/4
	,add_service/4
	,debt/1
	,delete_enemy/1
	,enemy_by_name/1
	,friend_by_expertise/1
	,friend_by_name/1
	,install/1]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(friend, {name, contact=[], info=[], expertise}).
-record(service, {from, to, date, description}).
-record(enemy, {name, info=[]}).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    mnesia:wait_for_tables([friend, service, enemy], 5000),
    yakuzapp_sup:start_link().

add_enemy(Name, Info) ->
    E = #enemy{name=Name, info=Info},
    Trans = fun() ->
		    mnesia:write(E)
	    end,
    mnesia:activity(transaction, Trans).
	    
add_friend(Name, Contact, Info, Expertise) ->
    F = #friend{name=Name, contact=Contact, info=Info, expertise=Expertise},
    Trans = fun() ->
		    mnesia:write(F)
	    end,
    mnesia:activity(transaction, Trans).

add_service(From, To, Date, Description) ->
    S = #service{from=From, to=To, date=Date, description=Description},
    Trans = fun() ->
		    case
			mnesia:read({friend, From}) =:= [] orelse
			mnesia:read({friend, To}) =:= []
		    of
			true  -> {error, unknown_friend};
			false -> mnesia:write(S)
		    end
	    end,
    mnesia:activity(transaction, Trans).

debt(Name) ->
    Trans = fun() ->
		    QH = qlc:q([if Name =:= From -> {To, -1};
				   Name =:= To   -> {From, 1}
				end || #service{from=From, to=To} <- mnesia:table(service), Name =:= From orelse Name =:= To]),
		    qlc:fold(fun({Person, N}, Dict) ->
				     dict:update(Person, fun(X) -> X + N end, N, Dict)
			     end, dict:new(), QH)
	    end,
    lists:sort([{Debt, Key} || {Key, Debt} <- dict:to_list(mnesia:activity(transaction, Trans))]).

delete_enemy(Name) ->
    Trans = fun() ->
    		    mnesia:delete({enemy, Name})
    	    end,
    mnesia:activity(transaction, Trans).

enemy_by_name(Name) ->
    Trans = fun() ->
		    mnesia:read({enemy, Name})
	    end,
    case mnesia:activity(transaction, Trans) of
	[#enemy{name=Name1, info=Info}] -> {Name1, Info};
	[]                              -> undefined
    end.
	    

friend_by_expertise(Expertise) ->
    Trans = fun() ->
		    QH = qlc:q([{Name, Contact, Info, E, service_by_name(Name)} ||
				   #friend{name=Name, contact=Contact, info=Info, expertise=E} <- mnesia:table(friend), E =:= Expertise]),
		    qlc:eval(QH)
	    end,
    mnesia:activity(transaction, Trans).

friend_by_name(Name) ->
    Trans = fun() ->
    		    case mnesia:read({friend, Name}) of
    			[#friend{contact=Contact, info=Info, expertise=Expertise}] ->
    			    {Name, Contact, Info, Expertise, service_by_name(Name)};
    			[] -> undefined
    		    end
    	    end,
    mnesia:activity(transaction, Trans).

install(Nodes) ->
    mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, mnesia, start, []),
    mnesia:create_table(friend,  [{attributes, record_info(fields, friend)}]),
    mnesia:create_table(service, [{attributes, record_info(fields, service)},
    				  {type, bag}]),
    mnesia:create_table(enemy,   [{attributes, record_info(fields, enemy)},
				  {local_content, true}]),
    rpc:multicall(Nodes, mnesia, stop, []).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

service_by_name(Name) ->
    Match = ets:fun2ms(
	      fun({service, From, To, Date, Description}) when From =:= Name ->
		      {to, To, Date, Description};
		 ({service, From, To, Date, Description}) when To   =:= Name ->
		      {from, From, Date, Description}
	      end),
    mnesia:select(service, Match).
