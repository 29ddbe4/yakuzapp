-module(yakuzapp_SUITE).

-export([all/0
	,init_per_suite/1
	,end_per_suite/1
	,init_per_testcase/2
	,end_per_testcase/2]).

-export([accounts/1,
	 add_service/1
	,enemy/1
	,friend_by_expertise/1
	,friend_by_name/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [accounts
    ,add_service
    ,enemy
    ,friend_by_expertise
    ,friend_by_name].

init_per_suite(Config) ->
    install([node()]),
    application:ensure_all_started(yakuzapp),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

accounts(_Config) ->
    yakuzapp_app:add_friend("Consigliere", [], [you], consigliere),
    yakuzapp_app:add_friend("Gill Bates", [{email, "ceo@macrohard.com"}], [clever, rich], computers),
    yakuzapp_app:add_friend("Pierre Gauthier", [{other, "city arena"}], [{job, "sports team GM"}], sports),
    yakuzapp_app:add_friend("Wayne Gretzky", [{other, "Canada"}], [{born, {1961,1,26}}, "hockey legend"], hockey),
    yakuzapp_app:add_service("Consigliere", "Wayne Gretzky", {1964,1,26}, "Gave first pair of ice skates"),
    yakuzapp_app:add_service("Consigliere", "Gill Bates", {1985,11,20}, "Bought 15 copies of software"),
    yakuzapp_app:add_service("Gill Bates", "Consigliere", {1986,8,17}, "Made computer faster"),
    yakuzapp_app:add_service("Pierre Gauthier", "Consigliere", {2009,6,30}, "Took on a huge, bad contract"),
    [{-1, "Wayne Gretzky"}, {0, "Gill Bates"}, {1, "Pierre Gauthier"}] = yakuzapp_app:debt("Consigliere"),
    [{1, "Consigliere"}] = yakuzapp_app:debt("Wayne Gretzky").

add_service(_Config) ->
    {error, unknown_friend} = yakuzapp_app:add_service("Alan Parsons", "Don Corleone", {1973,3,1}, "Helped release a Pink Floyd album"),
    yakuzapp_app:add_friend("Alan Parsons", [{twitter, "@ArtScienceSound"}], [{born, {1948,12,20}}, musician, 'audio engineer', producer, "has projects"], mixing),
    yakuzapp_app:add_friend("Don Corleone", [], [boss], boss),
    ok = yakuzapp_app:add_service("Alan Parsons", "Don Corleone", {1973,3,1}, "Helped release a Pink Floyd album").

enemy(_Config) ->
    undefined = yakuzapp_app:enemy_by_name("Edward"),
    yakuzapp_app:add_enemy("Edward", [{bio, "Vampire"}, {comment, "He sucks (blood)"}]),
    {"Edward", [{bio, "Vampire"}, {comment, "He sucks (blood)"}]} = yakuzapp_app:enemy_by_name("Edward"),
    yakuzapp_app:delete_enemy("Edward"),
    undefined = yakuzapp_app:enemy_by_name("Edward").

friend_by_expertise(_Config) ->
    yakuzapp_app:add_friend("A Red Panda", [{location, "in a zoo"}], [animal,cute], climbing),
    [{"A Red Panda", _Contact, _Info, climbing, _Services}] = yakuzapp_app:friend_by_expertise(climbing),
    [] = yakuzapp_app:friend_by_expertise(make_ref()).

friend_by_name(_Config) ->
    yakuzapp_app:add_friend("Pete Cityshend", [{phone, "418-542-3000"}, {email, "quadrophonia@example.org"}, {other, "yell real loud"}], [{born, {1945,5,19}}, musician, popular], music),
    {"Pete Cityshend", _Contact, _Info, music, _Services} = yakuzapp_app:friend_by_name("Pete Cityshend"),
    undefined = yakuzapp_app:friend_by_name(make_ref()).

%%====================================================================
%% Internal functions
%%====================================================================

install(Nodes) -> yakuzapp_app:install(Nodes).
