# Год с Erlang, продолжение

Прошло 2 месяца после [первой статьи](http://erlang-russian.org/post/146), где я рассказывал про
опыт использования Erlang в игровом проекте. И за эти 2 месяца много воды утекло, и кое-что изменилось.
И поэтому нужно продолжение :)


## Еще раз про тестирование

Прошлый раз я ныл^W рассказывал, как трудно тестировать сервер автоматически, и почему его приходится
тестировать вручную. Так вот, это, во-первых, не так уж и трудно, если захотеть;
во-вторых, это необходимо )

В какой-то момент мы поняли, что багов у нас больше, чем хотелось бы, и нужно принимать решительные меры.
Время, которого не хватало раньше на написание годных тестов, вдруг нашлось. И около 2 недель я занимался
стабилизацией сервера.

Нет, я не стал выдумывать хитрые моки, чтобы покрыть все юнит-тестами. И не стал сочинять
сложный тестовый стенд, прогоняющий внутри себя игры и эмулирующий реальных пользователей.
Сделал проще: Консольное Mac OS приложение запускает N десятков (сотен) тестовых клиентов,
каждый из которых дергает все возможные методы серверного АПИ. Клиенты сгруппированы по функциям.
Часть из них дергает регистрацию и авторизацию, часть АПИ игры, часть АПИ покупок и т.д.
Ответы сервера собираются, но не анализируются автоматически. Еще ведется учет дисконнектов
(они, обычно, вызваны ошибками на сервере).

Далее этих клиентов можно запускать в двух режимах. Можно запустить небольшое количество и не на долго,
на 3-5 минут. Потом вручную просмотреть логи на клиентах и на сервере, нет ли в них ошибок
или необычных данных. А можно запустить несколько сотен клиентов на 10-15 минут. Получится режим
стресс-теста с нагрузкой на сервер порядка 5-8 тысяч запросов в секунду. Тут уж все логи вручную
просмотреть не получится, но ошибки на сервере, если они будут, незамеченными не останутся.

С помощью этих тестов были найдены узкие места, код был оптимизирован по производительности
и отрефакторен. Кое-где переделана архитектура. После рефакторинга появились ошибки типа race condition,
когда из разных потоков одновременно модифицировалось состояние игры. Опять переделана архитектура,
чтобы устранить такие ошибки. И, в итоге, получился стабильный сервер без багов. С тех пор я больше
не вижу ошибок в логах :)


## Обновление сервера по-взрослому

С легкомысленными обновлениями сервера, когда каждая новая фича тут же выкатавалась в продакшен
с помощью горячего обновления, тоже решено было покончить. Подняли тестовый сервер с внешним IP
(чтобы можно было коннектится из дома). Я разработал и задокументировал процедуру обновления,
типа, сперва прогоняются тесты, потом обновляется тестовый сервер, запускаются тесты на нем,
играем с разных девайсов, и т.д.

Частые обновления и нестабильность принял на себя тестовый сервер. А продакшн сервер стал обновляться
после каждого спринта -- раз в 2 недели. Однако горячие обновления стали невозможны, требовалась перегрузка.
Но в любой момент времени на сервере идут игры. Прежний легкомысленный подход -- оборвать все игры
и вернуть ставки, больше не подходил. Пришлось решать проблему восстановления состояний игр. 

Впрочем, решилось это относительно просто. Каждая игра -- это gen_server процесс, и его состояние
укладывается в некий довольно сложный record. Каким бы сложным ни был этот рекорд, он легко
сериализуется в бинарник с помощью **term_to_binary** и десериализуется обратной функцией.
Бинарник же можно положить в БД перед остановкой сервера. Так что процесс получился такой:

- запускаем из удаленной консоли команду **adm:stop_server()**;
- сервер перестает принимать запросы клиентов и останавливает таймеры -- состояния замораживаются;
- сервер обходит существующие комнаты, сериализует состояния, складывает в БД;
- закрывает соединения и останавливается;
- клиенты, потеряв соединение, делают попытки реконнекта каждые 5 секунд;
- архивируем старую версию кода и выкладываем на ее место новую;
- модифицируем структуру БД, если нужно;
- запускаем новый сервер;
- он достает из БД состояния игр, десериализует их, и запускает процессы игр;
- начинает принимать соединения от клиентов;
- клиенты реконнектятся, подключаются к своим играм, и продолжают играть.

Со стороны клиента это выглядит так, что игра замирает на 5-10-15 секунд, потом продолжается дальше.


## Еще раз про горячее обновление кода

Имея эту процедуру, оказалось проще обновлять и тестовый сервер так же. Но некоторая польза от горячего
обновления все-таки осталась. Есть две ситуации, где оно используется, даже на продакшн сервере.

Очевидная ситуация -- выкатывание мелких фиксов и фич, из-за которых перегружать сервер не хочется.
Но и откладывать их до конца спринта не хочется тоже. На тестовый сервер их можно выкатывать вообще спокойно.
А на продакшн можно, если понятно, что изменения безопасны.

Другая ситуация не очевидна и хорошо демонстрирует удобство Erlang. Допустим, мы видим какое-нибудь
странное поведение или баг на сервере и не можем это объяснить. Надо бы собрать побольше инфы,
данных из состояний разных процессов. Но тут вдруг, бац -- в модуле нет функций, которые могли бы
достать нужную инфу из состояния процесса и показать ее. Не беда -- дописываем нужную функцию,
компилируем модуль, обновляем по-горячему -- и, вуаля, можем дернуть свежепоявившуюся функцию из консоли
и поглядеть, что она достала из состояния процесса. После того, как проблема решена,
эту функцию можно убрать.

В итоге, нет надобности заранее продумывать средства диагностики. Их легко добавлять прямо
на работающем сервере, и это очень помогает. Вместо того, чтобы копатся в логах, всегда можно взять
любые данные из памяти процессов и показать их в любом виде.





