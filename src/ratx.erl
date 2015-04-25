-module(ratx).
-export([start_link/2, start/2]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
-export([run/2, run_unsave/2, ask/1, done/1]).
-export([info/1]).

-define(DEFAULT_LIMIT, 100).
-define(DEFAULT_QUEUE, 1000).
-define(QUEUE_TIMEOUT, 1000).
-define(counter, <<>>).
-compile([export_all]).

run_unsave(Name, Fun) ->
    case ask(Name) of
        drop ->
            drop;
        Ref when is_reference(Ref) ->
            Res = Fun(),
            done(Name),
            Res
    end.

run(Name, Fun) ->
    case ask(Name) of
        drop ->
            drop;
        Ref when is_reference(Ref) ->
            Res = (catch Fun()),
            done(Name),
            Res
    end.

ask(Name) ->
    Ref = make_ref(),
    [{opts, Queue, Limit, QueueSize}] = ets:lookup(Name, opts),
    case ets:update_counter(Name, ?counter, [{2, 0}, {2, 1, Limit, Limit}]) of
        [Counter, _] when Counter < Limit ->
            Ref;
        [Limit, Limit] ->
            try_queue(Ref, Name, Queue, QueueSize)
    end.

try_queue(Ref, Name, Queue, QueueSize) ->
    case ets:update_counter(Queue, ?counter, [{2, 0}, {2, 1, QueueSize, QueueSize}]) of
        [ActualQueue, _] when ActualQueue < QueueSize ->
            Mref = monitor(process, whereis(Name)),
            ets:insert(Queue, {{os:timestamp(), Ref, self()}}),
            receive
                {Ref, Reply} ->
                    erlang:demonitor(Mref, [flush]),
                    Reply;
                {'DOWN', _Mref, _, _, _} ->
                    broken
            end;
        [QueueSize, QueueSize] ->
            drop
    end.

info(Name) ->
    Queue = list_to_atom(atom_to_list(Name) ++ "_queue"),
    {ets:tab2list(Name), ets:tab2list(Queue)}.

done(Name) ->
    gen_server:cast(Name, dequeue).

start_link(Name, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, Opts}, []).

start(Name, Opts) ->
    gen_server:start({local, Name}, ?MODULE, {Name, Opts}, []).

-record(state, {name, limit, queue, queue_size, queue_timeout}).

init({Name, Opts}) ->
    Limit = proplists:get_value(limit, Opts, ?DEFAULT_LIMIT),
    QueueSize = proplists:get_value(queue, Opts, ?DEFAULT_QUEUE),
    Timeout = proplists:get_value(queue_timeout, Opts, ?QUEUE_TIMEOUT),
    Queue = list_to_atom(atom_to_list(Name) ++ "_queue"),

    ets:new(Name, [named_table, public, set, {keypos, 1}]),
    ets:insert(Name, [{<<>>, 0}, {opts, Queue, Limit, QueueSize}]),

    ets:new(Queue, [named_table, public, ordered_set, {keypos, 1}]),
    ets:insert(Queue, {<<>>, 0}),

    send_after(Timeout, self(), queue_check),

    {ok, #state{name = Name,
                queue = Queue,
                limit = Limit,
                queue_size = QueueSize,
                queue_timeout = Timeout}}.

handle_call(_Req, _From, S) ->
    {reply, badarg, S}.
handle_cast(dequeue, #state{name = Name, queue = Queue, queue_timeout = Timeout} = S) ->
    DropTimeout = subsctruct_ms_from_now(os:timestamp(), Timeout),
    dequeue(Name, Queue, DropTimeout),
    {noreply, S};
handle_cast(_Req, S) ->
    {noreply, S}.
handle_info(queue_check, #state{queue = Queue, queue_timeout = Timeout} = S) ->
    DropTimeout = subsctruct_ms_from_now(os:timestamp(), Timeout),
    NextTimeout = drop(Queue, DropTimeout, Timeout),
    erlang:send_after(NextTimeout, self(), queue_check),
    {noreply, S};
handle_info(_Info, S) ->
    {noreply, S}.
terminate(_,_) ->
    ok.
code_change(_FromVsn, State, _Extra) ->
    {ok, State}.

dequeue(Name, Queue, DropTimeout) ->
    case find_next(Queue, DropTimeout) of
        false ->
            ets:update_counter(Name, ?counter, [{2, -1, 0, 0}]);
        {_TS, Ref, Pid} = Key ->
            dequeue_key(Queue, Key),
            Pid ! {Ref, Ref}
    end.

dequeue_key(Queue, Key) ->
    ets:delete(Queue, Key),
    ets:update_counter(Queue, ?counter, [{2, -1, 0, 0}]).

drop(Queue, DropTimeout, Timeout) ->
    case find_next(Queue, DropTimeout) of
        false ->
            Timeout;
        {TS, _Ref, _Pid} ->
            (timer:now_diff(TS, DropTimeout) div 1000)
    end.

find_next(Queue, DropTimeout) ->
    case ets:first(Queue) of
        ?counter ->
            false;
        {TS, Ref, Pid} = Key ->
            case TS < DropTimeout of
                true ->
                    dequeue_key(Queue, Key),
                    Pid ! {Ref, drop},
                    find_next(Queue, DropTimeout);
                false ->
                    Key
            end
    end.

subsctruct_ms_from_now({MegaSecs, Secs, MicroSecs}, Ms) ->
    MinusMegaSecs = Ms div 1000000000,
    MinusSecs = (Ms - MinusMegaSecs * 1000000000) div 1000,
    MinusMicroSecs = (Ms rem 1000) * 1000,
    {MegaSecs - MinusMegaSecs, Secs - MinusSecs, MicroSecs - MinusMicroSecs}.

send_after(infinity, _, _Action) -> undefined;
send_after(Timeout, _, Action)  -> erlang:send_after(Timeout, self(), Action).
