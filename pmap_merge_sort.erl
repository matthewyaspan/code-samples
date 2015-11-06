% Matthew Yaspan
% Concurrent versions of  map function and a merge_sort function
% October 22 2015

-module(pmap_merge_sort).
-export([parallel_map/2, merge/2, merge_sort/1]).  % merge_sort/1]).


% This function spawns a process for the transformation of each member of 
% the list. The process sends a message with the result of the transformation
% performed along with the Pid of the spawned process to ensure that
% elements are stored in the new list in order.

parallel_map(_, []) -> [];
parallel_map(Fun, List) ->
  Pids = spawn_lists(Fun, List, self()),
  receive_loop(Pids).


% Helper function to parallel map, performs the spawning and returns a list
% of Pids in the order in which the processes were spawned
spawn_lists(_, [], _) -> [];
spawn_lists(Fun, [H | T], Pid) ->
  lists:map(fun(Elem) ->
               spawn(fun() -> Pid ! {self(), Fun(Elem)} end) end, [H|T]).

% Receives messages in the order of the Pids specified in the list given
% and returns a list of return values in that same order.
receive_loop([]) -> [];
receive_loop([H|T]) ->
   receive
       {H, Val} -> [Val | receive_loop(T)]
   end.

% Calls what I believe is a tail-recursive function if I understand 
% what that is correctly. Merges two sorted list into one large sorted list.
merge(ListA, ListB) -> tl_merge(ListA, ListB, []).

% Tail recursive helper function to return the merged lists
tl_merge([], [], List) -> List;
tl_merge([H|T], [], List) -> tl_merge(T, [], lists:append(List, [H]));
tl_merge([], [H|T], List) -> tl_merge(T, [], lists:append(List, [H]));

tl_merge ([HA|TA], [HB|TB], List) ->
  case HA < HB of
      true -> tl_merge(TA, [HB|TB], lists:append(List, [HA]));
      false -> tl_merge([HA|TA], TB, lists:append(List,  [HB]))
  end.


% Splits a given list in half. Called by merge_sort/1 and merge_sort/2
% Calls split_list/3
split_list([]) -> [];
split_list([Elem]) -> [Elem];
split_list([H|T]) -> split_list([H|T], [], 1).

% splits a list of length more than one in half.
split_list([H|T], FirstHalf,  Num) ->
  case Num < length([H|T]) / 2 of
      true -> split_list(T, lists:append(FirstHalf, [H]), Num + 1);
      false -> {lists:append(FirstHalf, [H]), T}
  end.
    
    


%single arg merge_sort splits the list into two and calls merge_sort/2
merge_sort([]) -> [];
merge_sort([Elem]) -> Elem;
merge_sort(List) ->
  case split_list(List) of 
      {ListA, ListB} ->  merge_sort(ListA, ListB)
  end.


% splits two lists and pattern matches to spawn the right set of functions
% to perform on the now-split lists. spawns that process and waits on the return
% value. returns that.
merge_sort([], List) -> List;
merge_sort(List, []) -> List;
merge_sort(ListA, ListB) ->
 Me = self(),
 ListTuple = {split_list(ListA), split_list(ListB)},
 RPid = case ListTuple of 
   {{A, C}, {B, D}} ->
        spawn(fun() ->  Me ! {self() , merge_sort_aux(A, C, B, D)} end);

   {[A], [B]} ->
        spawn(fun() -> Me ! {self(), merge([A],[B])} end);

   {{A, C}, B} ->
        spawn(fun() -> Me ! {self(), merge(merge(A,C), B)} end);

   {A, {B, C}} ->
        spawn(fun() -> Me ! {self(), merge(merge(B,C), A)} end)

  end,

  receive
       {RPid, List} -> List
  end.

% Auxiliary function called by merge_sort/2 to make it easier to read
merge_sort_aux(A1, A2, B1, B2) ->
  merge(merge_sort(A1, A2), merge_sort(B1, B2)).  


