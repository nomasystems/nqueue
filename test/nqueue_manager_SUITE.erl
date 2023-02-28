%%% Copyright 2023 Nomasystems, S.L. http://www.nomasystems.com
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
-module(nqueue_manager_SUITE).

-include("nqueue_manager.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [handle_debug, replace_state].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    nct_util:setup_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
handle_debug() ->
    [{userdata, [{doc, "Tests handling a debug message"}]}].

handle_debug(_Conf) ->
    Filename = "handle_debug_nqueue_manager.txt",
    {ok, Device} = file:open(Filename, [write]),
    nqueue_manager:write_debug(Device, test_event, [nqueue_manager]),
    ok = file:close(Device),
    {ok, Binary} = file:read_file(Filename),
    true = size(Binary) > 0,
    ok = file:delete(Filename).

replace_state() ->
    [{userdata, [{doc, "Tests replacing the process state"}]}].

replace_state(_Conf) ->
    {ok, Pid} = nqueue_manager:start_link(manager_name, 1, fun(_) -> ok end, 1),
    NewFunction = fun(_) -> other end,
    sys:replace_state(Pid, fun(State) -> State#st{function = NewFunction} end),
    #st{function = NewFunction} = sys:get_state(Pid),
    ok = nqueue_manager:stop(Pid).
