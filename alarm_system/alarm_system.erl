-module(alarm_system).
-export([start/0, subscribe/1, unsubscribe/1, cancel_event/2, add_event/2, alarm_server/2, alarm_event/2, shutdown/1]).

start() ->
    spawn(?MODULE, alarm_server, [[],[]]).

subscribe(Pid) ->
    Pid ! {self(), subscribe}.

unsubscribe(Pid) ->
    Pid ! {self(), unsubscribe}.

add_event(Pid, Event={_Name, _Desc, {{_Year, _Month, _Day}, {_Hour, _Min}}}) ->
    Pid ! {self(), add, Event}.

cancel_event(Pid, EventName) ->
    Pid ! {self(), cancel, EventName}.

shutdown(Pid) ->
    Pid ! {self(), shutdown}.

alarm_server(Subscribers, Events) ->
    receive
        {From, subscribe} ->
            From ! subscribed,
            alarm_server([From|Subscribers], Events);
        {From, unsubscribe} ->
            From ! unsubscribed,
            alarm_server(lists:delete(From, Subscribers), Events);
        {From, add, Event} ->
            EventPid = spawn(?MODULE, alarm_event, [self(), Event]),
            From ! event_added,
            alarm_server(Subscribers, [{EventPid, Event}|Events]);
        {From, cancel, EventName} ->
            NewEvents = lists:filter(
                fun({CurrEventPid, {CurrName, _, _}}) ->
                    if CurrName == EventName ->
                        CurrEventPid ! {self(), cancel},
                        false;
                    true -> true
                    end
                end,
                Events
            ),
            From ! {event_canceled, EventName},
            alarm_server(Subscribers, NewEvents);
        {_, done, {EventName, EventDesc}} ->
            lists:map(
                fun(Sub) ->
                    Sub ! {done, {EventName, EventDesc}}
                end,
                Subscribers
            ),
            NewEvents = lists:filter(
                fun({_, {CurrName, _, _}}) ->
                    if CurrName == EventName ->
                        false;
                    true -> true
                    end
                end,
                Events
            ),
            alarm_server(Subscribers, NewEvents);
        {From, shutdown} ->
            From ! shutdown,
            exit(normal)
    end.

alarm_event(Server, Event={Name, Desc, Date}) ->
    {{CurrYear, CurrMonth, CurrDay}, {CurrHour, CurrMin, _}} = erlang:localtime(),
    if {{CurrYear, CurrMonth, CurrDay}, {CurrHour, CurrMin}} == Date ->
        Server ! {self(), done, {Name, Desc}};
    true -> 
        receive
            {_, cancel} ->
                exit(normal)
        after 1000 ->
            alarm_event(Server, Event)
        end
    end.