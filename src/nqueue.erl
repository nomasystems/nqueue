%%% Copyright 2022 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(nqueue).

%%% START/STOP EXPORTS
-export([start_link/3, start_link/4, stop/1]).

%%% API EXPORTS
-export([in/2, out/1, info/1, is_empty/1, len/1, to_list/1, total_in/1, total_out/1, rps/1, rps/2]).

%%% NHOOKS EXPORTS
-export([hooks/0]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link(Name, ConsumerCount, ConsumerFun) ->
    start_link(Name, ConsumerCount, ConsumerFun, infinity).

start_link(Name, ConsumerCount, ConsumerFun, Rps) when
    is_atom(Name),
    ConsumerCount > 0,
    is_function(ConsumerFun, 1),
    (is_integer(Rps) or is_float(Rps) or (Rps == infinity)),
    (Rps >= 0)
->
    nqueue_manager:start_link(Name, ConsumerCount, ConsumerFun, Rps).

stop(SrvRef) ->
    nqueue_manager:stop(SrvRef).

%%%-----------------------------------------------------------------------------
%%% API EXPORTS
%%%-----------------------------------------------------------------------------
-spec in(Name, Item) -> Result when
    Name :: atom(),
    Item :: term(),
    Result :: ok | {error, term()}.
in(Name, Item) ->
    nqueue_tab:in(Name, Item).

-spec out(Name) -> Result when
    Name :: atom(),
    Result :: term().
out(Name) ->
    nqueue_tab:out(Name).

-spec info(Name) -> Result when
    Name :: atom(),
    Result :: proplists:proplist().
info(Name) ->
    nqueue_tab:info(Name).

-spec is_empty(Name) -> Result when
    Name :: atom(),
    Result :: boolean().
is_empty(Name) ->
    nqueue_tab:is_empty(Name).

-spec len(Name) -> Result when
    Name :: atom(),
    Result :: integer().
len(Name) ->
    nqueue_tab:len(Name).

-spec to_list(Name) -> Result when
    Name :: atom(),
    Result :: [Object],
    Object :: tuple().
to_list(Name) ->
    nqueue_tab:to_list(Name).

-spec total_in(Name) -> Result when
    Name :: atom(),
    Result :: integer().
total_in(Name) ->
    nqueue_tab:total_in(Name).

-spec total_out(Name) -> Result when
    Name :: atom(),
    Result :: integer().
total_out(Name) ->
    nqueue_tab:total_out(Name).

-spec rps(Name) -> Result when
    Name :: atom(),
    Result :: nthrottle:rps().
rps(Name) ->
    nthrottle:rps(Name).

-spec rps(Name, Rps) -> Result when
    Name :: atom(),
    Rps :: nthrottle:rps(),
    Result :: nthrottle:rps().
rps(Name, Rps) when
    is_atom(Name),
    (is_integer(Rps) or is_float(Rps) or (Rps == infinity)),
    (Rps >= 0)
->
    nthrottle:rps(Name, Rps).

%%%-----------------------------------------------------------------------------
%%% NHOOKS EXPORTS
%%%-----------------------------------------------------------------------------
hooks() ->
    [
        'init_queue',
        'terminate_queue'
    ].
