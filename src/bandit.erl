-module(bandit).

-export([
    pull_handle/0,
    pull_handle/2,
    create/1]).

pull_handle() ->
    random:uniform().
    
pull_handle(Lower, Upper) ->
    Rand = random:uniform(),
    Lower + (Upper - Lower) * Rand.

create(ArmList) ->
    fun(Arm) ->
        {Lower, Upper} = lists:nth(Arm, ArmList),
        pull_handle(Lower, Upper)
    end.
    
