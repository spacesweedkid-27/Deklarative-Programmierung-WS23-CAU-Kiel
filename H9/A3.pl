% 0, I added some nice comments so that I can understand the ideas.

append([],L,L).                             % list to empty list is the list.
append([E|R],L,[E|RL]) :- append(R,L,RL).   % list to list with front and back is front + back and then the list.

last(L,E) :- append(_,[E],L).               % the last element of a list L is the element that if being appended to a list results in L.

member(E,L) :- append(_,[E|_],L).           % element E is in the list L, if there are lists _ and _ so that [_ | [E | _]] = L

delete(E,L,R) :- append(L1,[E|L2],L),       % "extract" L1 and L2 which hold the property that together with E in the middle they are L
                 append(L1,L2,R).           % then just append L1 and L2.

sublist(T,L) :- append(_,L2,L),             % "extract" L2 so that there is a list which if L2 is appended we have L
                append(T,_,L2).             % then find out if T appended to some list is L2.
                                            % Both steps are important, because we now have a starting index and ending index for T,
                                            % so that neither of them may be at the beginning or end of L.

% 1)
lookup(K, KVs, V) :- member((K, V), KVs).   % pretty straightforward

% 2)
member2(E, L) :- append(Left, Right, L),    % split list in two
                 member(E, Left),           % search for elem in left
                 member(E, Right).          % then in right.

% 3)
% helper function pops last element off, popped element is ignored
pop([_], []).                       % inductive start
pop([A | R], R1) :- pop(R, R2),     % pop the rest
                    R1 = [A | R2].  % and check if the result we want is A concated to pop of the rest


reverse([], []).    % Induction start (case reverse([A], [A]) is already checked by algorithm and there are no lists whose reverse is longer than the other.)
reverse([A | R1], L2) :- last(L2, A),           % check if the last two elements are the same
                         pop(L2, L2Popped),     % pop the last element of R2 off since we don't have to search there anymore
                         reverse(R1, L2Popped). % we don't have to search A anymore, since we already know the result of that.
