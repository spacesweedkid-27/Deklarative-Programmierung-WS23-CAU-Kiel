rev([]    , []) .   % reverse [] is []
rev([X|Xs], Zs) :- app(Ys, [X], Zs),    % Search for Ys as the list that when appended the head of the input, results in the supposed reverse
                                        % for the list [1,2,3] X would be 1 and Ys would be [3,2] since [3,2] ++ [1] == [3,2,1]
                   rev(Xs, Ys),         % now check if the sublist Ys is really the reverse of Xs,
                                        % which would be in our example [3,2] being reversed to [2,3]
                   !.                   % In this case, this means that if there is another rule for app(Ys, [X], Zs) (here the second rule),
                                        % or rev, then don't try to proove the statement.

app([]    , Ys, Ys    ) .   % append something to empty list -> something
app([X|Xs], Ys, [X|Zs]) :- app(Xs, Ys, Zs). % Zs is Xs appended to Ys

% 2.
% The conclusion seen in the comments about the cut operator suggests that it will try to proove app(Ys, [X], Zs) with the second rule,
% when it can apply the first rule (aka when the lists are short enough). As one may easily see, if the lists are short enough,
% applying the second rule will result in another query that there is no rule to apply to. The same happens with rev, because if the lists
% are short enough, then both rules may be used, but only the first will result in a proof,
% so cutting off the other branch would be the right idea, which we don't do now that we removed the cut...
% This results in prolog waisting resources on branches that will not be able to be prooven.
% This is an example of a green cut.
% Now lets look at the other change:

rev2([]    , []) .   % reverse [] is []
rev2([X|Xs], Zs) :- rev2(Xs, Ys),       % Search for Ys so that Xs and Ys are reverse to each other
                                        % This will result in cutting off X until Xs is empty, resulting in Ys = []
                    app(Ys, [X], Zs).   % Now we append the cut off X's at Ys until we have the full list reversed.

% now because we first search for reverse sublists and then append them, we don't have a problem of failing braches anymore,
% since if rev2(Xs, Ys) fails, there is no reason to search for alternate proofs and app(Ys, [X], Zs) always will pick the correct rule,
% we don't need a cut, EXCEPT when the sublists are that small so that there are two possible rules for app to use,
% then it again may pick the wrong rule (the second one), resulting in some wasted resources again.
% if one would add another cut at the end of the rule, then we would cut off these branches so fix it.
% But also there is another problem with this solution. Because of Prolog always firstly resolving the left branches,
% The query stored would become longer and longer and longer, because rev2 would split itself into two queries until the lists passed are empty.