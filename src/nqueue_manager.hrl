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
-ifndef(nqueue_manager).
-define(nqueue_manager, true).

%%% MACROS
-define(MANAGER_NAME(Name), erlang:list_to_atom(erlang:atom_to_list(Name) ++ "_queue_manager")).
-define(PID_NAME(Pid), proplists:get_value(registered_name, erlang:process_info(Pid))).

%%% RECORDS
-record(st, {
    name :: atom(),
    function :: fun(),
    consumers :: [{reference(), pid()}]
}).

% -ifndef(nqueue_manager)
-endif.
