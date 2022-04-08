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
-module(nqueue_tab).

%%% CREATE/DESTROY EXPORTS
-export([create/1, destroy/1]).

%%% IN/OUT EXPORTS
-export([in/2, out/1]).

%%% UTIL EXPORTS
-export([info/1, is_empty/1, len/1, to_list/1, total_in/1, total_out/1]).

%%%-----------------------------------------------------------------------------
%%% CREATE/DESTROY EXPORTS
%%%-----------------------------------------------------------------------------
create(Name) ->
    Name = ets:new(Name, [
        public,
        set,
        named_table,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    ets:insert(Name, {in, 0}),
    ets:insert(Name, {out, 0}),
    ok.

destroy(Name) ->
    ets:delete(Name),
    ok.

%%%-----------------------------------------------------------------------------
%%% IN/OUT EXPORTS
%%%-----------------------------------------------------------------------------
in(Name, Item) ->
    try
        NextIn = ets:update_counter(Name, in, 1),
        in(Name, Item, NextIn)
    catch
        _:_ ->
            {error, {not_found, Name}}
    end.

in(Name, Item, NextIn) ->
    try
        Pid = ets:lookup_element(Name, {wait, NextIn}, 2),
        Pid ! {out_item, Item},
        ok
    catch
        _:_ ->
            ets:insert(Name, {NextIn, Item}),
            ok
    end.

out(Name) ->
    NextOut = ets:update_counter(Name, out, 1),
    out(Name, NextOut).

out(Name, NextOut) ->
    try
        Item = ets:lookup_element(Name, NextOut, 2),
        ets:delete(Name, NextOut),
        Item
    catch
        _:_ ->
            receive_item_until(Name, NextOut, 1000)
    end.

receive_item_until(Name, NextOut, WaitTime) ->
    ets:insert(Name, {{wait, NextOut}, self()}),
    receive
        {out_item, OutItem} ->
            ets:delete(Name, {wait, NextOut}),
            OutItem
    after WaitTime ->
        ets:delete(Name, {wait, NextOut}),
        out(Name, NextOut)
    end.

%%%-----------------------------------------------------------------------------
%%% UTIL EXPORTS
%%%-----------------------------------------------------------------------------
info(Name) ->
    ets:info(Name).

is_empty(Name) ->
    len(Name) =:= 0.

len(Name) ->
    case {ets:lookup_element(Name, in, 2), ets:lookup_element(Name, out, 2)} of
        {In, Out} when In > Out ->
            In - Out;
        _ ->
            0
    end.

to_list(Name) ->
    List = ets:tab2list(Name),
    remove_indexes(List).

remove_indexes(List) ->
    remove_indexes(List, []).

remove_indexes([], Acc) ->
    lists:reverse(Acc);
remove_indexes([{in, _} | T], Acc) ->
    remove_indexes(T, Acc);
remove_indexes([{out, _} | T], Acc) ->
    remove_indexes(T, Acc);
remove_indexes([{{wait, _}, _} | T], Acc) ->
    remove_indexes(T, Acc);
remove_indexes([H | T], Acc) ->
    remove_indexes(T, [H | Acc]).

total_in(Name) ->
    ets:lookup_element(Name, in, 2).

total_out(Name) ->
    case {ets:lookup_element(Name, in, 2), ets:lookup_element(Name, out, 2)} of
        {In, Out} when Out > In ->
            In;
        {_In, Out} ->
            Out
    end.
