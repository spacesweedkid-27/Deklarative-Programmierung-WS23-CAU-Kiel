rev([]    , []) .   % reverse [] is []
rev([X|Xs], Zs) :- app(Ys, [X], Zs),    % Search for Ys as the list that when appended the head of the input, results in the supposed reverse
                                        % for the list [1,2,3] X would be 1 and Ys would be [3,2] since [3,2] ++ [1] == [3,2,1]
                   rev(Xs, Ys),         % now check if the sublist Ys is really the reverse of Xs,
                                        % which would be in our example [3,2] being reversed to [2,3]
                   !.                   % if the two conditions are not met, rev is not proovalble, since there is no next rule.
                                        % if the two conditions are met, find out if ' ' is provable, which is true.

app([]    , Ys, Ys    ) .   % append something to empty list -> something
app([X|Xs], Ys, [X|Zs]) :- app(Xs, Ys, Zs). % Zs is Xs appended to Ys

% 2.
% The conclusion seen in the comments about the cut operator suggest that removing it will not make a difference.
% for reference, this is how the cut operator is discribed in the script:
% In Prolog kann ! anstelle von Literalen im Regelrumpf stehen:
% p :- q, !, r.
% Operational bedeutet dies: Wird diese Regel zum Beweis von p benutzt, dann gilt:
% 1. Falls q nicht beweisbar ist: wähle nächste Regel für p.
% 2. Falls q beweisbar ist: p ist nur beweisbar, falls r beweisbar ist. Mit anderen Worten,
% es wird kein Alternativbeweis für q und keine andere Regel für p ausprobiert.
%
% Now lets look at the other change:

rev2([]    , []) .   % reverse [] is []
rev2([X|Xs], Zs) :- rev2(Xs, Ys),       % Search for Ys so that Xs and Ys are reverse to each other
                                        % This will result in cutting off X until Xs is empty, resulting in Ys = []
                    app(Ys, [X], Zs).   % Now we append the cut off X's at Ys until we have the full list reversed.