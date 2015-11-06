% Matthew Yaspan
% Basic Generic Server
% October 22 2016

-module(server_code).
-export([start/1, loop/2, call/2, cast/2, swap_code/2]).

% spawns a server loop, returns the Pid of the loop
start(Module) ->
   spawn(fun() -> loop(Module, Module:init()) end).

% asks the server to call a function
call(Pid, Request) ->
  Pid ! {call, self(), Request},
  receive
      {reply, Reply, _} -> Reply
  end.
 
% asks the server to cast something
cast(Pid, Request) ->
  Pid ! {cast, self(), Request},
  ok.

% Asks the server to call a new provided module
swap_code(Pid, NewCallBackModule) ->
  Pid ! {newmod, self(), NewCallBackModule},
  receive
    {ok, _} -> ok
  end.

% server loop waits obediently for instructions
loop(Module, CurrentState) ->
  receive
     {call, Pid, Request} ->
          Result = Module:handle_call(Request, Pid, CurrentState),
          Pid ! Result,
         {_, _, NewState} = Result;
     {cast, Pid, Request} ->
           Result = Module:handle_cast(Request, CurrentState),
           Pid ! Result,
           {_, NewState} = Result;
     {newmod, Pid, NewCallBackModule} ->
           Result = Module:handle_swap_code(CurrentState),
           Pid ! Result,
           {_, NewState} = Result,
           loop(NewCallBackModule, NewState)
  end,
  loop(Module, NewState).
    
     



