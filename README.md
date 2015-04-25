# Ratx

The Ratx Erlang application provides overload protection for
Erlang systems. It provides queueing facilities for tasks to be
executed so their concurrency can be limited on a running
system.

# Inspiration

Ratx owes its inspiration to Ulf Wigers `jobs` framework and
Jesper Louis Andersen `safetyvalve`, but it is a different
implementation and a different approach as well.

The main difference and goal is to try to eliminate the one gen_server
process to handle all requests. And, the same as `safetyvalve` to
have one process per queue, which can be supervised(and restarted,
if something go wrong).

`ets:update_counter/3` seems the best options and natural to try, and
heavily used in production for such use cases with good effort.

Due to this change, the queue process should only handle all `done`-s,
and dropping of entries in a queue after timeout. If queue is full, no
messages or calls will be sent to a queue process, that is a protection
for a process on big birsts.

# Using

Example of using

```erlang
ratx:start_link(test_queue, [{limit, 10}, {queue, 100}, {queue_timeout, 1000}]),
Action = fun(I, Time) -> io:format("~p..", [I]), timer:sleep(Time) end,
[spawn(fun() -> ratx:run(test_queue, fun() -> Action(I, 40) end) end) || I <- lists:seq(1, 30)], ok.
```
