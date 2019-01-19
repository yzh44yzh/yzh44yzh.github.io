# Этапы

+ выступление на митапе FuncBy 12-го
+ выступление на внутреннем митапе
- пост в блог
- мастер-класс. Тут надо проект посложнее. И в нем должны быть ошибки, которые можно найти трассировкой.


# Интроспекция и трассировка эрланг-сервиса на примере реального проекта

Те, кто что-то слышал про Эрланг, наверняка слышали и про то, что Эрланг позволяет подключиться к работающей ноде на проде, и в реальном времени получать много информации о происходящем. Можно следить за тем, с каким аргументами вызываются функции и что они возвращают. Можно следить за созданием и завершением процессов. Можно видеть сообщения, которые передаются между процессами, и их состояния. Про такие мелочи, как нагрузка на CPU, расход памяти и срабатывания сборщика мусора можно даже и не упоминать.

Мы запустим реальный проект, подадим на него пользовательские запросы, подключимся к ноде, и посмотрим в реальности, что интересного можно узнать и сделать внутри эрланговского сервиса.


## Теория

Традиционно у разработчика есть 2 средства для получения обратной связи от системы на проде -- это логи и метрики. Логи либо ротируются в файлах, либо собираются куда-нибудь, например, в **ElasticSearch**. Метрики отображаются где-нибудь, например, в **Graphana**. Все это мощные инструменты, и с ними многое можно сделать.

Но у эрланга есть больше возможностей. Когда мы подключаемся консолью к ноде на удаленной машине, то можем:
- изучать текущее состояние системы (интроспекция);
- наблюдать изменение системы в динамике (трассировка);
- активно модифицировать систему (на проде такое делать опасно, но если очень надо, то можно сделать).

Мы всем этим займемся в практической части. Но сперва немного теории.

Трассировка -- это отслеживание событий в системе. Главная проблема трассировки, как в огромном потоке событий найти именно то, что нужно?

В эрланг для этого нужно указать два фильтра. Первый фильтр определяет активность каких потоков нас интересуют. Второй фильтр определяет вызовы каких функций нас интересуют.

Потоки можно выбрать по pid, или только существующие (игнорировать те, что запускаются после начала трассировки), или только новые (наоборот, выбрать только те, что запускаются после начала трассировки), или просто выбрать все потоки.

Функции можно выбрать, указав **module-function-arity**, либо с помощью [match specification](http://erlang.org/doc/apps/erts/match_spec.html). Второй вариант представляет собой специальный шаблон, позволяющий выбрать функцию не только по названию, но и по значениям аргументов.

В тот момент, когда выбраные потоки вызывают выбраные функции, возникают события трассировки.

Встроеные в виртуальную машину эрланга механизмы трассировки сами по себе на производительность влияют мало. Но при этом может генерироваться очень большое количество событий. Они могут переполнить память, либо забить непрерывным выводом консоль и сделать невозможным ввод команды с консоли.

Для трассировки есть средства из коробки: модуль [dbg](http://www.erlang.org/doc/man/dbg.html), функции [trace](http://erlang.org/doc/man/erlang.html#trace-3) и [trace_pattern](http://erlang.org/doc/man/erlang.html#trace_pattern-3). Пользоваться этими средствами напрямую не очень удобно -- слишком много действий приходится совершать.

И есть инструменты от сторонних разработчиков: [redbug](https://github.com/massemanet/redbug) и [recon](http://ferd.github.io/recon/). Они легко подключаются к проекту и гораздо удобнее в использовании.

Мы рассмотрим все варианты :)

Какую информацию мы можем извлечь из событий трассировки? Это информация о процессах, и информация о функциях.

О процессах можно узнать следущее:
- отправка/получение сообщений;
- события жизненного цикла процесса: spawn, exit, register, unregister;
- события установки связей между процессами: link, unlink, getting_linked, getting_unlinked;
- работа планировщика процессов;
- работа сборщика мусора.

О функциях можно узнать следущее:
- момент вызова с точностью до микросекунд;
- значения аргументов;
- возвращаемое значение;
- вызывающая функция (caller), а если мы используем redbug -- то полный стейктрейс вызова;
- пид процесса, в котором выполнялась функция;
- дамп процесса (некоторое текстовое представление памяти процесса).

TODO stopped here

События трассировки можно направить в кастомную функцию, и потом как-то обрабатывать
(сохранять в БД, передавать в elastic search, whatever)
можно захадркодить в проекте включение/выключение трассировки с определенными настройками
И потом в рантайме при каких-то условиях включать это (вручную или автоматически).

Match specifications are compiled to a format close to the one used by the emulator, making them more efficient than functions.
dbg:fun2ms/1 call. It converts specifications that are described using fun syntax into match specifications.
literal fun
-include_lib("stdlib/include/ms_transform.hrl").
The specifications are tuples with three elements of the format [Head, Conditions, Body]
Head -- аргументы функции
Conditions -- гарды
Body -- побочные эффекты (генерация событий, изменение флагов трассировки)

Сложная штука, много нюансов.
Хорошо описана тут:
- Erlang Programming. Francesco Cesarini and Simon Thompson.
  Chapter 17. Trace BIFs, the dbg Tracer, and Match Specifications.

redbug и recon отказались от использования match specifications напрямую и придумали свои варианты.

Оригинальный match specification вам понадобится, если вы будете пользоваться dbg
или захотите написать свой redbug :)


### проект cat_traveler

бизнес требования


### system.marfa

api, устройство


### load.agent



## Практика

### Observer

http://erlang.org/doc/apps/observer/observer_ug.html

и для system.marfa (там стейт интереснее), и для load.agent (там процессов больше)

Запустить, пройти по всем вкладкам, объяснить, что там.
Пройтись по всем приложениям, показать дерево супервизоров. И как посмотреть стейт процесса.

У него есть UI для трассировки. Неплохой. Но я так и не смог увидеть никаких событий. Почему-то не показываются.
При том, что такая же трассировка из консоли работает.

### Redbug

https://github.com/massemanet/redbug

```
redbug:start("cat_traveler:enter/2 -> return", [{time, 15000}, {msgs, 10}]).
```
Приходит 2 сообщения на каждый вызов функции:

```
% 00:44:52 <0.1297.0>({cowboy_stream_h,request_process,3})
% cat_traveler:enter({cat,<<"Murka">>}, {town,<<"Minsk">>})

% 00:44:52 <0.1297.0>(dead)
% cat_traveler:enter/2 -> ok
```
Первое сообщение генерируется в момент вызова функции.
Оно содержит timestamp, инфу о процессе, в котором выполняется функция (pid и стартовая функция),
название и аргументы функции.

Второе сообщение генерируется при завершении функции.
Оно содержит timestamp, инфу о процессе,
название функции и возвращаемое значение.

В нашем случае процесс уже завершился.
После чего его pid по-прежнему известен, а стартовая функция уже не известна.

Можно получить более точный timestamp
```
redbug:start("cat_traveler:enter/2 -> return", [{time, 15000}, {msgs, 10}, {print_msec, true}]).
```
Теперь timestamp в миллисекундах. Что все равно недостаточно, чтобы оценить время выполнения функции, например.


Отфильтруем только те вызовы, которые касаются кота Тихона:
```
redbug:start("cat_traveler:enter({cat,<<\"Tihon\">>}, _) -> return", [{time, 15000}, {msgs, 10}]).
```

Кроме возвращаемого значения можно посмотреть стек вызовов.
```
redbug:start("cat_traveler:enter/2 -> stack,return", [{time, 15000}, {msgs, 10}]).
```

Приходят 2 события. В первом агрументы и стэк. Во втором возвращаемый результат.
```
% 23:41:44 <0.585.0>({cowboy_stream_h,request_process,3})
% cat_traveler:enter({cat,<<"Marfa">>}, {town,<<"Amsterdam">>})
  proc_lib:init_p_do_apply/3
  cowboy_stream_h:request_process/3
  cowboy_stream_h:execute/3
  cowboy_handler:execute/2
  ct_handler_api:init/2

% 23:41:44 <0.585.0>(dead)
% cat_traveler:enter/2 -> ok
```

Note that not all functions in the call chain are on the stack,
only functions we will return to (this is a consequence of tail call optimization.)


Ну и посмотрим все вызовы функций в модуле cat_traveler:
```
redbug:start("cat_traveler -> return", [{time, 15000}, {msgs, 10}]).
```


### recon_trace

http://ferd.github.io/recon/recon_trace.html

```
recon_trace:calls({cat_traveler, enter, '_'}, 5).

23:20:54.652981 <0.346.0> cat_traveler:enter({cat,<<"Murka">>}, {town,<<"Praha">>})
23:20:56.772668 <0.349.0> cat_traveler:enter({cat,<<"Vasjka">>}, {town,<<"Amsterdam">>})
23:20:58.766497 <0.351.0> cat_traveler:enter({cat,<<"Marfa">>}, {town,<<"Minsk">>})
23:20:59.538641 <0.352.0> cat_traveler:enter({cat,<<"Tihon">>}, {town,<<"Barcelona">>})
23:21:03.601575 <0.355.0> cat_traveler:enter({cat,<<"Murka">>}, {town,<<"Praha">>})
Recon tracer rate limit tripped.
```

Видим пид процесса и аргументы функции.

```
recon_trace:calls({cat_traveler, enter, fun(_) -> return_trace() end}, 4).
23:23:08.437399 <0.469.0> cat_traveler:enter({cat,<<"Tihon">>}, {town,<<"Barcelona">>})
23:23:08.437941 <0.469.0> cat_traveler:enter/2 --> ok
23:23:08.931518 <0.470.0> cat_traveler:enter({cat,<<"Marfa">>}, {town,<<"Minsk">>})
23:23:08.931922 <0.470.0> cat_traveler:enter/2 --> ok
Recon tracer rate limit tripped.
```
Теперь мы видим возвращаемое значение (отдельным событием).
В отличие от redbug timestamp приходит с точностью до микросекунд. Так что мы можем посмотреть, сколько времени работала функция.

Фильтрация по аргументам:
```
recon_trace:calls({cat_traveler, enter, fun([{cat, <<"Tihon">>}, _]) -> return_trace() end}, 4).
```
Обратите внимание, что аргументы функции нужно оборачивать в список.

Стектрейс посмотреть нельзя, но можно увидеть caller -- вызывающую функцию.
Match Specifiation http://erlang.org/doc/apps/erts/match_spec.html
позволяет добавлять в событие дополнительную информацию с помощью команды message.
Из интересного, там может быть caller или process_dump.
```
recon_trace:calls({cat_traveler, enter, fun(_) -> message(caller()) end}, 4).
21:36:24.193053 <0.4492.0> unknown trace type call -- [{cat_traveler,enter,
                                                        [{cat,<<"Marfa">>},
                                                         {town,<<"Minsk">>}]},
                                                       {ct_handler_api,
                                                        handle,3}]
```
Правда, recon_trace не понимает сообщения такого вида, но все равно показывает их.

Что касается стектрейса, то erlang:trace_pattern и match_spec не позволяют его получить.
Но redbug получает. Если заглянуть в его исходники, то становится понятно, как.
redbug для каждого события вызова функции бросает исключение, перехватывает его, берет стектрейс, и продолжает работу.

Сравнение с redbug:
- лимит задается либо на число событий, либо на число событий в единицу времени. Нет лимита просто по времени.
  (во втором случае нужно вызывать recon_trace:clear(), чтобы остановить трейсинг)
- Показывает, хотя и криво, message(caller) и message(process_dump)
- нет трассировки событий процессов, только вызовы функций.
- timestamp с точностью до микросекунд, тогда как у redbug до миллисекунд.
- ситаксис более громоздкий и документация сложнее

### dbg

http://www.erlang.org/doc/man/dbg.html

Запустим трассировку в самом простом виде:
```
dbg:tracer().
MS1 = dbg:fun2ms(fun(_) -> return_trace() end).
dbg:tp(cat_traveler, enter, MS1).
dbg:p(all, [c]).
(<0.2127.0>) call cat_traveler:enter({cat,<<"Marfa">>},{town,<<"Minsk">>})
(<0.2127.0>) returned from cat_traveler:enter/2 -> ok
(<0.2129.0>) call cat_traveler:enter({cat,<<"Vasjka">>},{town,<<"Barcelona">>})
(<0.2129.0>) returned from cat_traveler:enter/2 -> ok
(<0.2130.0>) call cat_traveler:enter({cat,<<"Murka">>},{town,<<"Minsk">>})
(<0.2130.0>) returned from cat_traveler:enter/2 -> ok
(<0.2133.0>) call cat_traveler:enter({cat,<<"Tihon">>},{town,<<"Barcelona">>})
(<0.2133.0>) returned from cat_traveler:enter/2 -> ok
(<0.2135.0>) call cat_traveler:enter({cat,<<"Murka">>},{town,<<"Stockholm">>})
(<0.2135.0>) returned from cat_traveler:enter/2 -> ok
dbg:stop_clear().
```
Видим по 2 события на каждый вызов функции.
В первом событии pid и аргументы функции.
Во втором событии pid и возвращаемое значение.

Добавим timestamp
```
dbg:tracer().
MS2 = dbg:fun2ms(fun(_) -> enable_trace(timestamp), return_trace() end).
dbg:tp(cat_traveler, enter, MS2).
dbg:p(all, [c]).
(<0.2589.0>) call cat_traveler:enter({cat,<<"Murka">>},{town,<<"Praha">>}) (Timestamp: {1541,
                                                                                        613751,
                                                                                        772026})
(<0.2589.0>) returned from cat_traveler:enter/2 -> ok (Timestamp: {1541,
                                                                   613751,
                                                                   772126})
dbg:stop_clear().
```

Хех, это не удобный способ представления времени. Нужна еще кастомная функция форматирования, чтобы сделать удобнее.

```
DbgFun = fun(Event, _Acc) ->
  case Event of
    {trace_ts, Pid, call, FunAndArgs, TS} ->
      DT = calendar:now_to_datetime(TS),
      io:format("~p, ~p, Function call ~p~n", [DT, Pid, FunAndArgs]);
    {trace_ts, Pid, return_from, MFA, Result, TS} ->
      DT = calendar:now_to_datetime(TS),
      io:format("~p, ~p, Function ~p returns ~p~n", [DT, Pid, MFA, Result])
  end
end.
dbg:tracer(process, {DbgFun, null}).
dbg:tp(cat_traveler, enter, MS2).
dbg:p(all, [c]).
{ {2018,11,7},{18,32,21} }, <0.4267.0>, Function call {cat_traveler,enter,
                                                     [{cat,<<"Marfa">>},
                                                      {town,<<"Munchen">>}]}
{ {2018,11,7},{18,32,21} }, <0.4267.0>, Function {cat_traveler,enter,2} returns ok
dbg:stop_clear().
```
Такую функцию в консоли не наберешь. Ее нужно готовить и отлаживать -- отдельная морока.



Добавим фильтрацию по аргументам:
```
MS3 = dbg:fun2ms(fun([{cat, <<"Tihon">>}, _]) -> return_trace() end).
dbg:tracer().
dbg:tp(cat_traveler, enter, MS3).
dbg:p(all, [c]).
(<0.5295.0>) call cat_traveler:enter({cat,<<"Tihon">>},{town,<<"Minsk">>})
(<0.5295.0>) returned from cat_traveler:enter/2 -> ok
(<0.5301.0>) call cat_traveler:enter({cat,<<"Tihon">>},{town,<<"Barcelona">>})
(<0.5301.0>) returned from cat_traveler:enter/2 -> ok
dbg:stop_clear().
```

Давайте посмотрим caller()
```
MS4 = dbg:fun2ms(fun(_) -> message(caller()), return_trace() end).
dbg:tracer().
dbg:tp(cat_traveler, enter, MS4).
dbg:p(all, [c]).
(<0.5391.0>) call cat_traveler:enter({cat,<<"Marfa">>},{town,<<"Munchen">>}) ({ct_handler_api,
                                                                               handle,
                                                                               3})
dbg:stop_clear().
```

И давайте посмотрим process_dump
```
MS5 = dbg:fun2ms(fun(_) -> message(process_dump()), return_trace() end).
dbg:tracer().
dbg:tp(cat_traveler, enter, MS5).
dbg:p(all, [c]).
(<0.5444.0>) call cat_traveler:enter({cat,<<"Tihon">>},{town,<<"Minsk">>}) (<<"=proc:<0.5444.0>
...
dbg:stop_clear().
```

Дамп большой, и не очень читабельный. Тут нужна форматирующая функция.

```
DbgFun2 = fun(Event, _Acc) ->
  case Event of
    {trace, Pid, call, FunAndArgs, Info} ->
      io:format("~p, Function call ~p~n", [Pid, FunAndArgs]),
      Lines = binary:split(Info, <<"\n">>, [global]),
      lists:foreach(fun(Line) -> io:format("~s~n", [Line]) end, Lines);
    {trace, Pid, return_from, MFA, Result} ->
      io:format("~p, Function ~p returns ~p~n", [Pid, MFA, Result])
  end
end.
dbg:tracer(process, {DbgFun2, null}).
dbg:tp(cat_traveler, enter, MS5).
dbg:p(all, [c]).
<0.6769.0>, Function call {cat_traveler,enter,
                              [{cat,<<"Marfa">>},{town,<<"Minsk">>}]}
=proc:<0.6769.0>
State: Running
Spawned as: proc_lib:init_p/5
...
dbg:stop_clear().
```
Вызывающий процесс -- это http хендлер. И дамп его стейта соответствующий.

Ну и раз у нас полноценный dbg, а не ограниченый redbug, то можно смотреть не только вызовы функций,
а и события, связаные с процессами. Например, все входящие и исходящие сообщения.

```
dbg:tracer().
dbg:p(cat_traveler_srv, [send, 'receive']).
(<0.214.0>) << {'$gen_call',{<0.6856.0>,#Ref<0.293362199.3499622401.187526>},
                            {leave,{cat,<<"Tihon">>},{town,<<"Barcelona">>}}}
(<0.214.0>) <0.6856.0> ! {#Ref<0.293362199.3499622401.187526>,ok}
(<0.214.0>) << {'$gen_call',{<0.6857.0>,#Ref<0.293362199.3499622401.187535>},
                            {leave,{cat,<<"Marfa">>},{town,<<"Minsk">>}}}
(<0.214.0>) <0.6857.0> ! {#Ref<0.293362199.3499622401.187535>,ok}
dbg:stop_clear().
```

Сравнение с redbug:
- доступны все возможности трассировки
- лимит на число сообщений и на время создать нельзя, нужно явно останавливать трассировку
- защиты от большого количества событий нет
- нужно изучить Match Specification и dbg
- бывает нужна форматирующая функция

### erlang:trace

http://erlang.org/doc/man/erlang.html#trace-3
http://erlang.org/doc/man/erlang.html#trace_pattern-3

Вызовы функций cat_traveler с фильтрацией по аргументам:
```
MS3 = dbg:fun2ms(fun([{cat, <<"Tihon">>}, _]) -> return_trace() end).
erlang:trace(all, true, [call]).
erlang:trace_pattern({cat_traveler, '_', '_'}, MS3, []).
% wait some time
flush().
Shell got {trace,<0.480.0>,call,
                 {cat_traveler,leave,[{cat,<<"Tihon">>},{town,<<"Minsk">>}]}}
Shell got {trace,<0.480.0>,return_from,{cat_traveler,leave,2},ok}
Shell got {trace,<0.484.0>,call,
                 {cat_traveler,enter,
                               [{cat,<<"Tihon">>},{town,<<"Barcelona">>}]}}
Shell got {trace,<0.484.0>,return_from,{cat_traveler,enter,2},ok}

erlang:trace(all, false, [call]).
```

Вызовы функций cat_traveler с добавлением caller():
```
MS4 = dbg:fun2ms(fun(_) -> message(caller()), return_trace() end).
erlang:trace(all, true, [call]).
erlang:trace_pattern({cat_traveler, '_', '_'}, MS4, []).

flush().
Shell got {trace,<0.649.0>,call,
                 {cat_traveler,enter,[{cat,<<"Marfa">>},{town,<<"Minsk">>}]},
                 {ct_handler_api,handle,3}}
Shell got {trace,<0.649.0>,return_from,{cat_traveler,enter,2},ok}

erlang:trace(all, false, [call]).
```

Все входящие и исходящие сообщения cat_traveler_srv
```
Pid = whereis(cat_traveler_srv).
erlang:trace(Pid, true, [send, 'receive']).

flush().
Shell got {trace,<0.214.0>,'receive',
                 {'$gen_call',{<0.855.0>,#Ref<0.2245986846.305135617.225212>},
                              {enter,{cat,<<"Vasjka">>},
                                     {town,<<"Barcelona">>}}}}
Shell got {trace,<0.214.0>,send,
                 {#Ref<0.2245986846.305135617.225212>,ok},
                 <0.855.0>}

erlang:trace(Pid, false, [send, 'receive']).
```

### recon

http://ferd.github.io/recon/recon.html

recon:proc_count(memory, 5).
recon:proc_window(memory, 5, 1000).

Что такое proc_window?
Fetches a given attribute from all processes (except the caller) and returns the biggest entries, over a sliding time window.
This function is particularly useful when processes on the node are mostly short-lived.

recon:proc_count(reductions, 5).
recon:proc_window(reductions, 5, 1000).

recon:proc_count(message_queue_len, 5).
recon:proc_window(message_queue_len, 5, 1000).

recon:info({0, 1872, 0}).
wrapper for erlang:process_info/1

recon:get_state({0, 1872, 0}).
wrapper around sys:get_state/1

recon:info(cat_traveler_srv).
recon:get_state(cat_traveler_srv).

Рассказать, как был найден hot spot в роутере
riak core, vnodes
300 ключей на vnode. Одна из них была горячая по CPU (по редукциям).
я взял ее стейт, пересмотрел все 300 ключей, и нашел ключ, к которому обращались очень часто.

### sys

http://www.erlang.org/doc/man/sys.html

- события внутри ген-сервера sys:trace
  Можно просматривать все сообщения, которые проходят через gen_server

sys:trace(cat_traveler_srv, true).
sys:trace(cat_traveler_srv, false).

Сообщения:
- входящие call и cast
- исходящие (ответы клиенту) и измененный стейт

sys:log(cat_traveler_srv, true).
sys:log(cat_traveler_srv, print).
sys:log(cat_traveler_srv, false).
Безопаснее, т.к. позволяет ограничить к-во сообщений.

sys:log(cat_traveler_srv, {true, 5}).
sys:log(cat_traveler_srv, get).
sys:log(cat_traveler_srv, false).

print -- выводит на консоль
get -- возвращает списком
maximum of N events are kept in the debug structure (default is 10).


Можно посмотреть статистику работы процесса:
время старта, к-во сообщений полученных и отправленных, инфа от планировщика.

sys:statistics(cat_traveler_srv, true).
sys:statistics(cat_traveler_srv, get).
sys:statistics(cat_traveler_srv, false).


стейт потока, хранящего инфу о котах: cat_traveler_srv

sys:get_status(cat_traveler_srv).
{status, Pid, {module, Mod}, [ProcessDictionary, SysState, Parend, Dbg, Misc]}.
SysState: running | suspended

sys:get_state(cat_traveler_srv).

модифицировать стейт, подсунуть кота в город, проследить, как возникают ошибки в АПИ
можно и закрашить процесс, если модифицировать стейт неправильно.

```
F = fun({state, Cats, Towns}) ->
Cats2 = Cats#{ {cat, <<"Tihon">>} => {town, <<"Minsk">>} },
{state, Cats2, Towns}
end.
sys:replace_state(cat_traveler_srv, F).
sys:get_state(cat_traveler_srv).
```

```
ERROR: invalid request "/leave" <<"{\"cat\": \"Tihon\", \"town\": \"Barcelona\"}">> 400 <<"{\"error\":\"not_in_town\"}">>
```


## Выводы

recon -- это больше про интроспекцию
redbug -- это про трассировку
dbg использовать довольно сложно

Тема большая и сложная. Тут важно правильно начать -- с простых тулов.
