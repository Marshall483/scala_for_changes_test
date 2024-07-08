
## Домашнее задание 1

Основная тема - алгоритмы сортировки в функциональном стиле.

### Мотивация

Частая претензия к функциональным подходам - их "непрактичность". 
В том смысле, что классические алгоритмы часто требуют разного рода
изменений состояния, а алгоритмы работы с коллекциями - быстрого доступа
к элементу по его индексу.

Иногда эти претензии можно признать справедливыми, но не всегда. И в ходе
реализации этого домашнего задания мы постараемся убедиться, что иногда
достаточно посмотреть на известный алгоритм немного с другой стороны и увидеть возможность
реализовать его, оставаясь в рамках парадигм ФП и неизменяемых структур данных.


### Структуры данных

Мы будем опираться на функциональный список в его обобщенной реализации.

Базовое определение дано как часть постановки задачи в исходном коде. Ожидается, что вам
скорее всего потребуются дополнительные методы. Что-то типа map, filter, foldLeft,
foldRight. Реализуйте их с учетом ограничений.

Что-то уже там реализовано. Можно и нужно брать это за образец.

Только с equals особо пример не берите. Я его определил, чтобы не полагаться
на стандартный - он может быть не подготовлен к глубокой рекурсии. 

А реализовать его через более красивые match-и нельзя - это приводит к сравнениям
и неуместной  бесконечной рекурсии.

(hash я не делал - он по идее не должен здесь нигде вызываться).

Сортируемыми данными будут пары, состоящие из числа и строки. Ключом сортировки
будет числовой элемент, а строка будет нужна для гарантий сохранения порядка.

То есть ваши реализации должны будут гарантировать, что если в исходных данных
пара (a, b) идет раньше пары (a, c), то в полученном результате их относительный
порядок сохранится.

Также в коде будет определен трейт с интерфейсом сортировки. И ваши решения должны
быть его реализациями.


### Приоритеты и общие критерии

Во всех заданиях мы получим асимптотику не хуже, чем в классических алгоритмах.
Сначала я подскажу, как ее достигнуть, а вашей задачей будет это реализовать,
не скатившись при этом на качественно худший уровень.

Местами можно будет побороться за константы. Иногда самая простая реализация может
потребовать на несколько проходов больше, чем в классическом алгоритме, но при более хитрой реализации
можно будет сократить число проходов.

Общая схема приоритетов в заданиях выглядит так:

- первостепенная задача - корректность алгоритма

- следующее по значимости - хвостовая рекурсия и асимптотический порядок сложности

- наименее приоритетно, но может быть ценным - константны в формуле сложности

То есть, важнее всего, чтобы алгоритм возвращал корректный результат. В корректность входит
и сохранение порядка элементов с одним ключом.

Никакая скорость и оптимальность не стоит корректности результата.

Невыполнение этого условия скорее всего повлечет возврат задания на доделку. И пока оно не заработает,
ревью проводиться не будет.

Я рекомендую написать преобразование вашего списка в какую-нибудь стандарную коллекцию и обратно,
нагенерировать кучу случайных данных, сортировать их встроенными функциями и сравнивать с вашими результатами.

(Можете даже добавить ваши тесты в отправляемое задание. Там может быть более или менее любой код.
Проверяться на стиль он точно не будет. Но будет подтверждением того, что вы пытались что-то
тестировать)

Граничные случаи типа пустого или одноэлементного списка всегда должны работать корректно. 

Если с корректностью все хорошо, смотрится асимптотика и хвостовая рекурсия.

Это два разных аспекта, но их сложно однозначно упорядочить. Многое зависит от масштаба проблем с асимптотикой.

Если вы где-то недосмотрели и посадили квадрат вместо nlog(n) - будет значимое, но не уничтожающее снижение балла
с возможностью исправить с некоторым штрафом.

Если с асимптотикой все хорошо или проблемы ограничиваются квадратом там, где ожидалось nlog - тогда станет важной
хвостовая рекурсия. 

Она станет важной, потому что нет большого толка в хорошей асимптотике, если получим переполнение стека на списке из
 50000 элементов.

Но она будет не так важна при совсем вопиющих проблемах с асимптотикой. Потому что при вопиющих проблемах с асимптотикой
мы можем просто не доползти до проблем со стеком.

Если код будет работать бесконечно долго на списке из 10000 элементов -
возможен возврат на доделку с задержкой ревью до исправления.

С константами еще мягче. Если это заведомо лишний проход, без которого можно обойтись без ущерба для читабельности -
возможно значимое, но не уничтожающее снижение балла с возможностью исправить.

Если не сделано то, что можно было придумать, но оно не лежит на поверхности - то скорее будет совсем маленькое снижение.

Если вы увидели какую-то возможность, но не стали реализовывать, потому что это сильно усложняет код и описали это в
комментарии - снижения не будет. Если вы меня убедили. А если не убедили, я напишу, в чем я не согласен.

Если я увижу, что мало кто стал бороться за константы, а вы стали - не исключаю, что дам бонусный балл.

Могу дать бонусный балл, если придумаете что-то особо красивое. Или хватит терпения на что-то особенно заморочное,
но при этом это что-то все-таки хоть как-то должно быть полезным.

### Ограничения

Никаких мутабельностей - ни в прямых, ни в косвенных формах (атомики, мутирующие коллекции и т.п.).

Во всех заданиях мы возвращаем новый список. Если список уже отсортирован, то не возбраняется вернуть
тот же список (не создавая новый), но это не обязательно.

Элементы (пары)  должны остаться теми же. Их мы не копируем, не создаем новые.

Если вы решили побороться за константы - напишите коммментарий. Даже если считаете, что baseline-алгоритм
и есть оптимальный - напишите, почему вы так думаете. 

Без комментариев мне сложно понять, вы целенаправленно добились оптимального решения или оно само как-то.
Поэтому я по умолчанию буду считать, что само как-то.

Про корректность, асимптотику и стек никаких комментариев не нужно.

Скала-структуры (которые в стандартной библиотеке из коробки есть) не используем.
Даже immutable.
Имеется ввиду - в реализации сортировок. В тестах - можно.

И если будут проблемы с печатью больших списков и вам захочется свой  toString или какой-нибудь
prettyPrint свой - можете в нем использовать StringBuilder. Но остальные требования даже к этим
методам остаются.


### Задание 1. Сортировка выбором

20 баллов

https://en.wikipedia.org/wiki/Selection_sort

Реализуем класс SelectSorter.

####  Базовый алгоритм

Базовый алгоритм состоит в том, чтобы формировать результат элемент за элементом. Сначала
найти минимальный (нулевой по счету в результате), потом следующий и так далее.

В реализации на массивах с прямым доступом мы внешним циклом идем по позициям от начала до конца, а
в каждом внутреннем цикле проходим от текущего внешнего цикла и находим минимальный на этом отрезке.

Если текущий внешнего цикла оказался минимальным, больше ничего на этой внешней итерации не делаем.

Иначе меняем местами текущий внешнего цикла и минимальный.

В таком виде базовый алгоритм НЕ гарантирует сохранения порядка элементов с одинаковым ключом.

И такой алгоритм всегда работает за квадрат

####  Модификация в духе ФП

Не будем делать in-place со swap-ами. Вместо этого сформируем новый список. Зато сохраним порядок элементов
с одинаковым ключом. 

Будем строить список с конца. Сначала найдем максимальный элемент. Положим его в конец результирующего списка.
И вытащим его из оригинального. Потом рекурсивно повторим схему.

Получаем чистый квадрат, ничуть не хуже, чем базовый алгоритм.

Получается, что внутри у нас как минимум два, а то и три прохода - найти максимальный и извлечь его из списка.
Три - потому что извлекать можно так, что сначала идти до известного элемента, а потом - формировать новый список.

Я рекомендую сначала сделать baseline, который корректно работает за квадрат и не переполняет стек. А потом уже
побороться за константы.

Один из вариантов борьбы за константы - отложенное удаление. Сначала находим нужный элемент. А потом на каждой итерации
совмещаем поиск нового максимума с удалением предыдущего. 

Подумайте над использованием переворачивания списка. Оно может быть полезным, потому что несложно переворачивать список
без переполнения стека. И процесс переворачивания можно совместить с поиском максимума/минимума и/или удалением какого-то
элемента.

Рассмотрите вариант вообще ничего не удалять. Это в 2 раза повысит число переборов. Но не исключено, что это может оказаться
выгодным.


### Задание 2. Сортировка вставкой

20 баллов


Реализуем класс InsertSorter.

https://en.wikipedia.org/wiki/Insertion_sort

####  Базовый алгоритм

Базовый алгоритм состоит, как и в случае сортировкм выбором, в том, чтобы формировать результат элемент за элементом,
но по-другому. Перебирая элемент за элементом, добавляем его в результат так, чтобы на каждом шаге все добавленные
в результат элементы были отсортированы.

В реализации на массивах с прямым доступом мы пропускаем начальную внешнюю итерациию, полагая что начальный элемент
уже составляет одноэлементый результирующий массив.

Далее мы внешним циклом идем по позициям от следующей за началом до конечной, а
в каждом внутреннем цикле пытаемся "протолкнуть" злемент с текущей позиции внешнего цикла как можно "левее".

Сначала сравниваем его с соседом слева и, если ключ соседа больше, то меняем элементы местами и сравниваем уже с новым
соседом слева. И так продолжаем, пока есть основания меняться местами с соседом слева и не дошли до крайней левой позиции.

Такой алгоритм сохраняет порядок элементов с одинаковым ключом.

В целом асимптотика - квадрат.

Но есть хорошие частные случаи. Например, для уже сортированного массива алгоритм отработает за линию.
Более обобщенно - количество операций сопоставимо с количеством инверсий в исходных данных.


####  Модификация в духе ФП

Если список не пуст, возьмем его голову и создадим одноэлементный вариант результирующего списка.

Дальше будем брать элемент за элементом из исходного списка и искать им место в результирующем.

В отличие от базовой версии на массивах, где мы шли "от конца" частичного результата, здесь мы будем идти "от головы".

Нам надо для каждого элемента найти место в частично сформированном результирующем списке.

Здесь надо подумать, чтобы аккуратно учесть эту инверсии и не нарушить общие требования к результату.

Будет здорово если у вас получится сохранить свойство базовой сортировки вставкой - линейное время на предсортированнои списке.
Еще лучше - чтобы количество операций было сопоставимо с количеством инверсий.

Если линия будет на списке, сортированном по убыванию - тоже неплохо.

Но в целом этот линейный частный случай по значиомсти - на уровне борьбы за константы.

Корректность результата и способность работать на миллионе элементов без переполнения стека - гораздо важнее.


### Задание 3. Сортировка слиянием

30 баллов

https://en.wikipedia.org/wiki/Merge_sort

Сортировка выбором - худшая из разумных и интересна только как материал для тренировки.

Сортировка вставкой обладает не очень вдохновляющей асимптотикой, но ее линейный частный случай делает ее иногда привлекательной
на практике, но в особых частных случаях.

Займемся же более практически значимым алгоритмами.

В парадигме ФП прекрасно реализуема сортировка HeapSort. Надо только вместо класической кучи использовать биномиальную.
Но мы в этом практиковаться не будем, чтобы не фрустрировать тех, кто прошел Kotlin-часть курса, переписыванием на Scala того,
что уже сделали в прошлом семестре на Kotlin.

Кому интересно - могут почитать здесь: https://en.wikipedia.org/wiki/Binomial_heap 

Реализуем сортировку слиянием через класс MergeSorter. На первый взгляд как-то не очень подходящую под функциональные списки.

####  Базовый алгоритм

Делим данные на две половинки. Если половинки - размером 1, то их сортировать не надо. 
А если большего размера - то рекурсивно применим алгоритм к каждой половинке.

В любом случае - получив отсортированные половинки, "сольем" их вместе, двигаясь по каждой половинке и добавляя в результат
меньший из двух текущих элементов.

В реализации на массивах, разбивая данные пополам, существенно используем индексы и прямой доступ к элементам по индексу.
Что больше всего и смущает при мыслях о реализации на функциональных списках.

Отметим также, что не существует варианта реализации сортировки слиянием на массивах без создания нового массива.

В этом смысле "копирующая" сортировка точно не должна смущать.

Сложность сортировки слиянием - nlog(n) и она сохраняет порядок элементов с равными ключами

####  Модификация в духе ФП

Чтобы красиво реализовать сортировку слиянием на функциональных списках, достаточно радикально пересмотреть первую, "рекурсивную"
фазу.

Не будем ничего делить пополам. Просто преобразуем наш список в список одноэлементных списков.

Отдельно реализуем слияние двух отсортированных списков.

А дальше надо научиться проходить по списку списков, извлекая пары списков, сливая их и добавляя в новый список.

Можно это сделать непосредственно, а можно - через map/filter/fold.

Важно только аккуратно обработать случай нечетного числа элементов в списке "верхнего" уровня.

И конечно, не забыть про переполнение стека и порядок совпадающих элементов.

Кстати, во всех ли подзадачах нужна хвостовая рекурсия ?

И совсем здорово, если удастся подумать про оптимизацию констант.


### Задание 4. Быстрая сортировка

30 баллов

https://en.wikipedia.org/wiki/Quicksort

Реализуем QuickSorter.

####  Базовый алгоритм

Общая идея: берем случайный элемент в качестве "опорного". Элементы, меньшие опорного помещаем "левее", а большие - "правее".

И повторяем схему рекурсивно.

Полулярная реализация на массивах использует возможности массивов для того, чтобы обойтись без вспомогательного массива.
Но она не сохраняет порядок элементов с одним ключом.
Хотя сама общая идея алгоритма позволяет это сделать.

В целом асимптотика nlog(n), но есть неприятные частные случаи, когда опорный элемент каждый раз неудачен и все элементы оказываются
по одну сторону от опорного.

Что самое обидное - это если на входе уже отсортированный массив, а в качестве опорного берется начальный.

Часто в реализация на массивах использует перемешивание, после которого в качестве опорного элемента берут начальный.


####  Модификация в духе ФП

baseline-вариантом будем считать следующий.

Пренебрегая возможностью вырожденного случая с квадратичной сложностью, берем всегда в качестве опорного элемента начальный.

Реализуем функцию разбиения списка по результатам сравнения с опорным элементом. Функция возвращает пару списков.

Рекурсивно применяем к каждому списку такую же схему, пока не получим вариант из 1-2 элементов. 

Если вам удастся сделать эту схему, она будет работать корректно и все будет хорошо со стеком и в асимптотике будет nlog(n)
с оговоркой про вырожденный случай - это будет заявкой на балл, близкий к полному.

(В вырожденном случае сложность может быть квадратом, но переполнения стека не должно быть по-любоиу)

В рамках полировки можно попробовать реализовать квазифункциональное квазиперемешивание.

Например, так. На каждом шаге рекурсии брать в качестве опорного элемента не нулевой, а какой-то со случайным номером.
Совсем по простому - зная длину списка, породить случайный индекс нужного диапазона и отдельным проходом узнать его значение.
После чего уже разбивать.

А можно попытаться обойтись без этого прохода. Например, порождать номер опорного для следующего прохода и разбиение сопровождать
определением опорного. Только надо обработать случай, при котором индекс окажется больше, чем длина соответствующего списка.
Можно немного покреативить на тему того, как с этим быть, сохранив что-то, похожее на случайность. Например, брать в качестве запасного опорного
тот, чей индекс сравним по модулю с каким-то небольшим числом, пока не дошли до настоящего опорного.

Но если на такое хватит терпения - это уже точно бонусный балл будет.

(Эта схема квазифункциональна, потому что сам вызов чего-то типа Math.random() сомнителен с этой точки зрения и мы еще вернемся на семинарах
к этой теме)

