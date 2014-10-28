-module(greedy_bandit).

-export([
    init/3, 
    run/2, 
    select/2, 
    max_index/4,
    find_best_action/1,
    select_random_action/1,
    update_list/3,
    create_filled_list/2]).

-record(bandit_state,{bandit_fun, n, expected, greedy_prob, trials}).

init(ArmList, GreedyProb, ExpectedList) ->
    BanditFun = bandit:create(ArmList),
    Arms = length(ArmList),
    #bandit_state{
        bandit_fun  = BanditFun,
        n           = Arms,
        expected    = ExpectedList,
        greedy_prob = GreedyProb,
        trials      = create_filled_list(Arms, 0)}.

max_index([], MaxIndex, _, _) ->
    MaxIndex;
max_index([First|Rest], MaxIndex, Max, CurrIndex) ->
    case (First > Max) or (MaxIndex == 0) of
        true    ->
            NewMaxIndex = CurrIndex,
            NewMax = First,
            max_index(Rest, NewMaxIndex, NewMax, CurrIndex + 1);
        false   -> 
            max_index(Rest, MaxIndex, Max, CurrIndex + 1)
    end.

max_index(SearchList) ->
    max_index(SearchList, 0, 0, 1).

find_best_action(ExpectedList) ->
    max_index(ExpectedList).

select_random_action(ActionCount) ->
    random:uniform(ActionCount).

select(ExpectedList, GreedyProb) ->
    Rand = random:uniform(),
    case Rand > GreedyProb of
        true    -> 
            select_random_action(length(ExpectedList));
        false   ->
            find_best_action(ExpectedList)
    end.

update_list(L, Index, NewElement) ->
    lists:sublist(L, Index - 1) ++ [NewElement] ++ lists:nthtail(Index, L).

create_filled_list(NumEl, Val) ->
    fill_helper([], NumEl, Val).

fill_helper(Partial, 0, _) ->
    lists:reverse(Partial);
fill_helper(Partial, NumEl, Val) ->
    fill_helper([Val|Partial], NumEl - 1, Val).

average_reward(NewReward, OldAverage, Trials) ->
    ((NewReward - OldAverage) / Trials) + OldAverage. 

run(State, 0) ->
    State;
run(State, TrialsRemaining) ->
    %io:format("State ~p, TrialsRemaining ~p~n", [State, TrialsRemaining]),
    % Match the state elements
    #bandit_state{
        bandit_fun  = BanditFun,
        n           = Arms,
        expected    = ExpectedList,
        greedy_prob = GreedyProb,
        trials      = TrialList} = State,
    % Choose an action
    Action = select(ExpectedList, GreedyProb),
    % Carry out the action
    Reward = BanditFun(Action),
    io:format("Action ~p, Reward ~p~n", [Action, Reward]),
    OldTrials = lists:nth(Action, TrialList),
    NewTrials = OldTrials + 1,
    NewTrialList = update_list(TrialList, Action, NewTrials),
    io:format("NewTrialList ~p~n", [NewTrialList]),
    % Compute the new expected value
    NewValue = average_reward(Reward, lists:nth(Action, ExpectedList), NewTrials),
    % Update the expected value for that action
    NewExpected = update_list(ExpectedList, Action, NewValue),
    io:format("Expected ~p~n", [ExpectedList]),
    % Update the state record.
    NewState = #bandit_state{
        bandit_fun  = BanditFun,
        n           = Arms,
        expected    = NewExpected,
        greedy_prob = GreedyProb,
        trials      = NewTrialList},
    run(NewState, TrialsRemaining - 1).
    
