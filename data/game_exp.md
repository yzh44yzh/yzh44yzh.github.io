# Опыт использования Erlang в разработке многопользовательской игры

Выступил с докладом на  "IT_Share. GameDev Web". Видео будет позже, [презентация тут](http://www.slideshare.net/YuriZhloba/erlang-25985745). Полный текст ниже. 
## Кратенько про Erlang

Наверняка большая часть аудитории мало знает об Erlang. Это не
беда. Несмотря на это, я полагаю, что доклад будет всем понятен. Но
небольшое введение о том, что такое Erlang, нужно.

Разработан в компании Ericsson в середине 80х годов. Создавался он не
из научного интереса, а для решения конкретных практических задач. А
именно, для коммутаторов широкополосных телефонных линий. То есть,
еще до появления этих ваших интернетов Erlang уже был предназначен для
высоких нагрузок и обслуживания большого числа пользователей :)

В любой книге про Erlang вы прочитаете, что упор в дизайне языка
сделан на 4 фичи:

 - Модель многопоточности, основанная на легких процессах, обменивающихся сообщениями;
 - Распределенность, сетевая прозрачность;
 - Устойчивость к ошибкам;
 - Горячее обновление кода.

Это все было необходимо в той предметной области, для которой Erlang
изначально создавался. Но не все так уж необходимо в других
областях. Посмотрим, что пишут об этих фичах в теории, и как они
полезны на практике.


## Кратенько про проект

Мы, компания Diesel Puppet, использовали Erlang в 3х проектах, один из
которых дожил до релиза, и уже 5 месяцев находится в
production. Проект этот с одной стороны не очень сложный. Это игра
"Русское лото", с мобильным (iPhone/iPad) клиентом и серверной частью на
Erlang. (На подходе flash-клиент для вконтакте). С другой стороны,
многопользовательские real time игры, даже такие простые, таят
в себе свои сложности.

Мы не будем сосредотачиваться именно на этом проекте, а поговорим о
том, как хорош (или не хорош) Erlang в предметной области "разработка
многопользовательских игр".


## Фичи Erlang в теории и на практике

### Многопоточность. Теория.

Разработчики, использующие мейнстримовые языки, такие как Java или
.NET, знают, что многопоточность -- это очень сложно. Нужно выучить
назубок **Java Concurrency in Practice**.  Мьютексы и семафоры должны
сниться по ночам. И все равно неуловимые и невоспроизводимые баги
будут блуждать по коду. Лучше вообще с этим не связываться. Но в
Erlang все не так.

Erlang имеет свою виртуальную машину с эффективной моделью
многопоточности. Потоки в Erlang представляют собой легковестные
сущности, не требующие много ресурсов, которые быстро создаются,
работают, и умирают, освобождая ресурсы. Виртуальная машина запускает
свои потоки в процессах операционной системы, по много потоков в одном
процессе. И она имеет собственный планировщик, управляющий этими
потоками. И даже не один планировщик, а несколько, по числу доступных
ядер. Они даже умеют передавать потоки друг другу, балансируя нагрузку
на ядра.

Каждый поток имеет свою собственную область памяти и не делит ее с
другими. Нет общей области памяти, разделяемой между процессами, и нет
типичных проблем многопоточных систем: race condition, dead lock и
т.д. (На самом деле кое где есть и разделяемая память, и оные
проблемы, но эти нюансы пока опустим. На начальном уровне можно
считать, что их нет :)

Процессы обмениваются друг с другом сообщениями. Чтобы передать
какие-нибудь данные из одного процесса в другой, то первый процесс
создает копию данных и посылает специальное сообщение. Оно попадает в
специальную область памяти (mailbox) второго процесса. Второй процесс,
когда считает нужным, проверяет свой mailbox, и реагирует на сообщения
в нем так, как считает нужным. Это очень похоже на то, как люди
обмениваются письмами по почте.

Каждый процесс имеет собственный сборщик мусора. Эти сборщики работают
независимо друг от друга, в разное время, а не все одновременно. И
сборка обычно срабатывает быстро, ибо область памяти, которую нужно
почистить, не велика. Поэтому сборка мусора в Erlang не так
сильно влияет на общую производительность системы, как, например, в
Java.


### Многопоточность. Практика.

На практике процессы являются основными кирпичиками, из которых
строится архитектура проекта.

Мы заводим отдельный процесс для каждого клиентского соединения и
храним в его состоянии все, что связано с пользователем и
сессией. Пользователей мы собираем в комнаты по 2-6 человек, где они
играют между собой. Разумеется, каждая комната, это отдельный процесс,
хранящий состояние одной конкретной игры.

Пользователей не всегда бывает много, поэтому, чтобы им не было
скучно, нужно периодически запускать ботов. Разумеется, каждый бот,
это отдельный процесс. В комнатах есть таймеры, которые генерируют
разные игровые события. Каждый таймер -- отдельный процесс.

Есть пул из 50 соединений с базой данных. Каждое соединение --
отдельный процесс (там даже больше, вроде бы по 3 процесса на одно
соединение).

Есть служебные сервисы. Например, один из них периодически проходит по
базе данных и суммирует разную статистику. Другой рассылает
пуш-уведомления пользователям. Третий занимается валидацией
платежей. Каждый сервис -- ну вы поняли :) Есть еще админка с
веб-интерфейсом, и прочие штуки.

Всего работает несколько тысяч процессов. Я создаю их не считая,
направо и налево, как Java разработчик не считая создает экземпляры
классов.

Вообще есть сходство с ООП. Процессы являются аналогами объектов. Они
инкапсулируют данные в своем состоянии, предлагают публичное АПИ для
работы с ними, имеют приватное АПИ для собственных нужд. У них даже
конструкторы и деструкторы есть :) И взаимодействие через отправку
сообщений, а не через вызов методов, это вполне в духе ООП. Хотя
Erlang относится к функциональной парадигме, но есть нюансы. Жаль,
нет времени развить эту тему :)

Когда я в первый раз получил в своем коде dead lock, то даже удивился,
что такое бывает. Бывает, бывает, если не знать некоторых нюансов
работы gen_server. К счастью, это легко диагностируется и легко
исправляется. Позже и race condition получал, но и с ними то же не
трудно. То есть, такие проблемы бывают и в Erlang, но их фиксить не
сложнее, чем "обычные" баги.

Кто делал похожие (многопользовательские) сервера на python, или
node.js, или еще на чем-то, тот знает, что нужно запустить несколько
нод на одной машине, чтобы утилизировать все ядра процессора. По одной
ноде на каждое ядро. Нужно наладить коммуникацию между нодами. А для
этого подключить что-нибудь типа Rabbit MQ. С Erlang ничего этого не
нужно. Одна нода эффективно использует все имеющиеся в наличии ядра. И
все процессы в этой ноде общаются без лишних посредников. Все это
проще и в разработке, и в развертывании, и в диагностике, и в
поддержке.

Многопроцессорная модель Erlang -- это самое ценное, что в нем
есть. Это самая суть языка, из которой вытекает все остальное. Не зря
она заимствуется при разработке других языков, например Scala и Go.


### Распределенность и сетевая прозрачность

Тут только теория. Увы, я пока не использовал это на практике. Мы
изначально планировали, что нам хватит одного узла. Так оно и
есть. Делать систему из нескольких узлов в текущем проекте вряд ли
понадобится. А в других проектах, как знать, поглядим :)

Итак, вам не нужно возиться с сокетами и сериализацией данных, чтобы
наладить общение нескольких нод друг с другом. Нодам нужно только
знать имена друг друга (node-name@server-name), чтобы собраться в
кластер. И дальше работает сетевая прозрачность -- любой процесс может
оправить сообщение другому процессу в другой ноде точно так же, как и
процессу в своей ноде.

Вопрос безопасности решается с помощью специальных кук. Все ноды
должны иметь одинаковую куку, которую они читают на старте. Ноды с
разными куками не смогут соединится. Куки формируют общую зону, внутри
которой все процессы доверяют друг другу.

Но если такая модель безопасности не устраивает, то всегда можно
вернуться к уровню ниже, к сокетам, и стоить на них свои сервисы со
своим АПИ.


### Устойчивость к ошибкам. Теория.

Коммутатор широкополосных телефонных линий должен обслуживать
клиентов что бы ни случилось. При разработке Erlang ставилось
требование сохранять работоспособность в любой ситуации. И есть три
уровня защиты.

Во-первых, потоки (и данные, и выполнение) изолированы друг от
друга. И если случается баг, то прерывается работа только одного
потока. Вся остальная система продолжает работать.

Во-вторых, потоки работают под присмотром специальных процессов --
супервайзеров. Если поток прерывается, то супервайзер запускает его
заново. Это как если бы админ перегрузил сервер, только в масштаб
мелкий. Конечно, может быть и так, что баг повторяется снова и снова,
и поток все время падает, перегрузки не помогают. После нескольких
попыток супервайзер сдается и падает сам. И уже его перегружает его
родительский супервайзер. То есть, перегрузка происходит на все более
высоком уровне, пока проблема не решится, или не упадет корневой
супервайзер. Это значит, что упала вся нода.

Третий уровень защиты -- распределенность. Erlang-узлы объединяются в
кластер, и это позволяет сохранять работоспособность при падении
одной ноды.


### Устойчивость к ошибкам. Практика.

На практике, конечно, все это не спасает от проблем. Волшебной
таблетки нет :) Если у вас баг в логике, то сервер не может нормально
обслуживать клиентов, пока не найдешь и не исправишь баг.

Бывали у меня косяки, и серьезные косяки. Нет, нода целиком никогда не
падала, но были отказы в обслуживании. А для клиента нет разницы,
работает ли сервер или не работает совсем, если он не обслуживает
запросы.

Так что по части обработки ошибок большой разницы в сравнении с той же
Java нет. Разве что не нужно везде и всюду пихать try/catch. В Erlang
реакция по умолчанию на ошибку -- записать в лог и перезапустить
процесс. В большинстве случаев это подходит, и явную обработку ошибок
писать не нужно. Подход Let it crash. Ну а где не подходит, там можно
и try/catch поставить.


### Горячее обновление кода. Теория.

Коммутатор широкополосных телефонных линий нельзя останавливать. А
обновлять на нем софт нужно. Поэтому Erlang поддерживает горячее
обновление кода.

Тут стоит объяснить, как вообще живет Erlang-процесс. Процесс
запускается, чтобы выполнить какую-то функцию. Однако функция может
быть бесконечно-рекурсивной, всегда вызывать саму себя. В этом случае
рекурсия, конечно, должна быть хвостовой. Тогда процесс выполняется
бесконечно, но при этом не происходит накопление данных на стеке, и
стек не переполняется.

Упрощенно можно представить себе, что процесс входит в некую
loop-функцию, проверяет сообщения в mailbox, обрабатывает их каким-то
образом, и опять входит в эту же loop-функцию. Причем аргументы
loop-функции и являются состоянием процесса, а каждый новый вызов может
идти с новыми аргументами. Таким образом происходит маленькое чудо --
аргументы функции и все переменные в ней являются неизменяемыми, а
состояние процесса меняется :) По сути состояние хранится на стеке.

Так вот, что происходит при горячем обновлении кода? Меняется код
функций, в т.ч. loop-функции. В какой-то момент завершается выполнение
старой версии функции, делается следующий вход в нее, но это вход уже
в новую версию функции.

В простых случаях это хорошо работает. Но бывают случаи сложные.

Может измениться структура данных, и новая версия функции не сможет
работать с текущим состоянием процесса. На этот случай предусмотрены
средства, позволяющие преобразовать состояние процесса в новую форму.

Может измениться дерево супервайзеров. Тут универсального решения нет,
в каждом случае разработчик должен сам решать, как запустить или
перезапустить процессы, чтобы из старого дерева получить новое.

Наконец, разные потоки переходят на выполнение нового кода в разное
время. Код обновили для всех одновременно, но какой-то поток вошел в
новый код раньше, а какой-то позже. Например, он был блокирован ожиданием
долгой синхронной операции. Соответственно, взаимодействие потоков
между собой, когда они выполняют разные версии кода, тоже может быть
не простым.

Для решения всех этих проблем в Ericsson существуют инженеры по
миграции, которые занимаются именно задачами перехода на новую версию
кода. Ну а многие разработчики пользуются горячим обновлением в
простых случаях, а в сложных просто перегружают сервер :)


### Горячее обновление кода. Практика.

В разработке на своей локально машине горячее обновление бывает
удобно. Можно менять код на лету, не теряя состояния сервера. Есть
специальные средства, которые автоматически загружают новый код, как
только он меняется. Или можно вручную загружать отдельные модули.

На ранних этапах я использовал горячее обновление и в
production. Сделал очередную фичу -- тут же выкатил. Бывало, и по 5
раз за день выкатывал. Рестартовать сервер не хотелось. В любой момент
времени идут игры и при рестарте теряется их состояние.

Однако такое выкатывание фич -- дело опасное. Production сервер
получается нестабильный, а пользователи выполняют роль
бета-тестеров. И они не всегда довольны этим :) В какой-то момент
стало понятно, что так жить нельзя.

Мы подняли отдельный тестовый сервер, и выкатывали изменения сперва на
него. Production сервер обновляли теперь уже редко, раз 1-2 недели
(после каждого спринта). Изменений накапливалось много, обновлять их
по-горячему уже было проблематично из-за вышеописанных сложных
случаев. Так что перешли на обновление с рестартом сервера.

Пришлось решать проблему сохранения и восстановления состояний
игр. Это решилось относительно просто. Каким бы сложным ни было
состояние процесса игры, оно легко сериализуется в бинарник и
десериализуется из него стандартными средствами Erlang. Бинарник же
можно положить в базу данных перед остановкой сервера, и достать из
базы после запуска сервера.

В итоге роль горячего обновления уменьшилась, но я все равно его
использую. Есть две ситуации, где оно полезно даже на production
сервере.

Очевидная ситуация -- выкатывание быстрых фиксов и мелких фич, из-за
которых перегружать сервер не хочется. Но и откладывать их до конца
спринта не хочется тоже. На тестовый сервер их можно выкатывать вообще
спокойно. И на production можно, если понятно, что изменения безопасны.

Другая ситуация не очевидна и хорошо демонстрирует удобство
Erlang. Допустим, мы видим какое-нибудь странное поведение или баг на
сервере. Надо бы собрать побольше инфы, данных из состояний разных
процессов. Но тут вдруг, бац -- в модуле нет функций, которые могли бы
достать нужную инфу из состояния процесса и показать ее. Не беда --
дописываем нужную функцию, компилируем модуль, обновляем по-горячему
-- и, вуаля, можем дернуть свежепоявившуюся функцию из консоли и
поглядеть, что она достала из состояния процесса. После того, как
проблема решена, эту функцию можно убрать.

В итоге, нет надобности заранее продумывать средства диагностики.  Их
можно добавлять на лету прямо в работающую систему.  И это очень
ценная возможность.


## Борьба за качество проекта

На мой взгляд автоматизированное тестирование -- довольно странная и
противоречивая вещь. С одной стороны всякому разработчику очевидно,
что гораздо лучше, если работа выполняется машиной автоматически, а не
разработчиком вручную. С другой стороны, покрыть тестами
сколько-нибудь существенную часть проекта настолько трудоемко, что
начинается казаться, что тестить вручную таки проще :) 

И автоматическое, и ручное тестирование обходятся дорого. И по
затраченным усилиям, и по времени. А качество обеспечивать
нужно. Большие компании находят выход в том, что выделяют отдельных
людей для этого. А маленькой компании, такой как наша, приходится
как-то выкручиваться. И вот я расскажу, как выкручиваемся мы :)

Не зря в функциональном программировании (и не только в нем), так
ценятся функции без побочных эффектов. Их любо-дорого
тестировать. Написал юнит-тест, с разными аргументами, с правильными,
с неправильными, с граничными случаями. Запустил -- и уверен на 100%,
что функция правильная.

Но проект не может состоять только из чистых функций. В проекте есть
побочные эффекты. Их полным полно. Повсюду. Кишмя кишат :) Например:

 - послали данные в сокет -- побочный эффект;
 - записали чего-то в файл -- побочный эффект;
 - вывели сообщение на консоль -- побочный эффект;
 - сохранили данные в базу -- побочный эффект;
 - запустили новый процесс -- побочный эффект;
 - послали сообщение другому процессу -- побочный эффект.

Я вам открою страшную тайну, о которой не говорят теоретики
программирования :) Побочный эффект -- это смысл работы любой
программы, это и есть то полезное, что она делает. Код без побочных
эффектов бесполезный сам по себе. Чтобы извлечь из него пользу, нужен
еще один код, который что-то куда-то таки сохранит, пошлет или
покажет.

Вот есть такой язык -- Haskell. Якобы язык без побочных
эффектов. Чтобы заставить его сделать что-то годное, нужны монады. Без
монад он ну вообще бесполезен. А монады, это замаскированный способ
таки добавить побочные эффекты, просто на другом уровне абстракции :)

Но это все философия, а нам нужно обеспечить качество кода, и чем
меньшими усилиями, тем лучше. Ну, положим, какую-то часть кода мы
покрыли юнит-тестами. Небольшую. Дальше сложнее.

Дальше берем, например, работу с базой данных. Да, можно сделать
моки. Но я мало вижу проку в тестировании моков, я хочу тестировать
работу с базой данных :) Это реально -- создаем отдельную, тестовую
БД. При каждом запуске тестов создаем в ней пустые таблицы (и дропаем
предыдущие таблицы), наполняем их тестовыми данными, и гоняем
тесты. Формально это будут уже не юнит тесты, хотя их можно создавать
с помощью инфраструктуры для юнит тестов.

Дальше берем, например, взаимодействие нескольких процессов,
обменивающихся синхронными и асинхронными сообщениями. Это тоже можно
тестировать, но для тестов уже нужно подымать иерархию
супервайзеров. Ну и асинхронные запросы получают ответ не сразу, тесту
нужно повисеть, подождать. Обязательно вылезут какие-нибудь
непредвиденные эффекты, зависимость от очередности вызовов или от фазы
луны. Опять все не просто.

Есть еще замечательный вариант: пишется тестовый клиент, который
запускается отдельно от сервера, и посылает реальные запросы на
сервер. Они проходят всю цепочку: передача данных, десериализация,
вызов АПИ, бизнес-логика, работа с БД. Клиент получает ответ и
сравнивает с эталоном, то ли пришло от сервера, что нужно.

Вот этот последний вариант для меня оказался самым важным. И он
довольно прост, если ограничить автоматический анализ ответов сервера,
и дополнить его ручным анализом :) Я сделал консольное приложение,
которое запускает несколько десятков или сотен тестовых клиентов,
каждый из которых дергает все возможные методы серверного АПИ. Клиенты
сгруппированы по функциям.  Часть из них дергает регистрацию и
авторизацию, часть АПИ игры, часть АПИ покупок и т.д.  Ответы сервера
собираются и выводятся в консоль. Еще ведется учет дисконнектов. Они,
обычно, вызваны ошибками на сервере.

Далее этих клиентов можно запускать в двух режимах. Можно запустить
небольшое количество и не на долго, на 3-5 минут. Потом вручную
просмотреть сообщения в консоли на клиентах и на сервере, нет ли в них
ошибок или необычных данных. А можно запустить несколько сотен
клиентов на 10-15 минут. Получится режим стресс-теста с нагрузкой на
сервер порядка 5-8 тысяч запросов в секунду. Тут уж все логи вручную
просмотреть не получится, но ошибки, если они будут, незамеченными не
останутся.

С помощью именно этих тестов сервер был изрядно оптимизирован по
архитектуре и по производительности и достиг таки хорошего стабильного
состояния. После этого, когда я утром прихожу на работу и смотрю логи
сервера за последние сутки, я очень редко вижу там что-то необычное :)


## Недостатки Erlang

Было бы неправильным умолчать об этом :)

Пожалуй, главный недостаток любого не мейнстримового языка -- малое
количество разработчиков. Для компании, которая решится использовать
Erlang в своем бизнесе, это серьезный риск. Поэтому и компаний таких
мало. Впрочем, некоторые компании таки научились с этим справляться.

Erlang не является языком общего назначения. Он не создавался для
решения любых задач в любых областях, а хорош только в своих
нишах. Это, может, не стоит относить к недостаткам, но подчеркнуть
стоит.

Динамическая типизация -- для меня это недостаток. Я бы предпочел
статическую, но без фанатизма :) В дело обеспечения качества продукта
статическая типизация вносит свой весомый вклад.

Есть некоторый хаос с библиотеками третьих сторон, не входящих в
стандартную поставку Erlang. Устоявшихся библиотек, которым можно
доверять, не так много. Большинство не стабильны, слабо
документированы, часто не имеют нормальной версионности, зато имеют
много форков :) Мне самому пришлось форкать драйвер для работы с
PostgreSQL, чтобы пофиксить в нем какую-то мелочь.


## Выводы

Самый главный -- будем ли мы применять Erlang в следующих проектах?
Однозначно да :)
