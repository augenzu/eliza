; "Доктор". Осень 2021

; В учебных целях используется базовая версия Scheme
#lang scheme/base

(require racket/vector)
(require racket/list)

; функция, запускающая однопользовательского "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?)) ; Доктор выводит приветствие
  (doctor-driver-loop-v2 name #()) ; и переходит к циклу диалога с пациентом
  )

; функция для ввода имени пациента
; (именем пациента считается первый элемент списка, введённого пользователем)
(define (ask-patient-name)
  (begin
    (println '(next!))
    (println '(who are you?))
    (print '**)
    (car (read))
    )
  )

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
  (newline)
  (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
  (let ((user-response (read)))
    (if (equal? user-response '(goodbye))
        (begin (printf "Goodbye, ~a!\n" name) ; реплика '(goodbye) служит для выхода из цикла
               (print '(see you next week)))
        (begin (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
               (doctor-driver-loop name))
        )
    )
  )

; основная функция, запускающая многопользовательского "Доктора"
; параметр stop-word -- стоп-слово, после использования которого в качестве имени очередного пациента Доктор завершает работу
; параметр max-patients-served -- максимальное количество пациентов, которое может принять Доктор
(define (visit-doctor-v2 stop-word max-patients-served)
  (let visit-doctor-iter ; функция-итерация (играет роль однопользовательского Доктора)
    ((patients-served 0) (name (ask-patient-name))) ; patients-served - счетчик принятых пациентов, name - имя очередного пациента
    (if (equal? name stop-word) ; если вместо имени пациента введено стоп-слово,
        (print '(time to go home)) ; Доктор завершает работу
        (begin (printf "Hello, ~a!\n" name) ; Иначе выводит приветствие
               (print '(what seems to be the trouble?))
               (doctor-driver-loop-v2 name #()) ; и запускает цикл диалога с пациентом
               (if (= (add1 patients-served) max-patients-served) ; если на данный момент принято максимальное количество пациентов,
                   (print '(time to go home)) ; то Доктор завершает работу
                   (visit-doctor-iter (add1 patients-served) (ask-patient-name))) ; Иначе переходит к новой итерации, запрашивая имя следующего пациента
               )
        )
    )
  )

; цикл диалога Доктора с пациентом - версия с сохранением вектора всех реплик пользователя
; параметр name -- имя пациента
; параметр response-history -- вектор всех предыдущих ответов пользователя
(define (doctor-driver-loop-v2 name response-history)
  (newline)
  (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
  (let ((user-response (read)))
    (if (equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
        (begin (printf "Goodbye, ~a!\n" name)
               (print '(see you next week))
               (newline))
        (begin (print (reply-v2 strategies user-response response-history)) ; иначе Доктор генерирует ответ, печатает его
               (doctor-driver-loop-v2 name (vector-append (vector user-response) response-history))) ; и продолжает цикл
        )
    )
  )

; генерация ответной реплики по user-response -- реплике от пользователя
; параметр response-history -- вектор всех предыдущих ответов пользователя,
; нужен для стратегии history-answer
(define (reply user-response response-history)
  (if (vector-empty? response-history) ; история пуста - пациент ввел первую реплику
      (if (contains-keyword? user-response)
          (case (random 3)
            ((0) (qualifier-answer user-response)) ; 1й способ
            ((1) (hedge)) ; 2й способ
            ((2) (keyword-answer user-response)) ; 4й способ
            )
          (case (random 2)
            ((0) (qualifier-answer user-response)) ; 1й способ
            ((1) (hedge)) ; 2й способ
            )
          )
      (if (contains-keyword? user-response)
          (case (random 4)
            ((0) (qualifier-answer user-response)) ; 1й способ
            ((1) (hedge)) ; 2й способ
            ((2) (history-answer response-history)) ; 3-й способ
            ((3) (keyword-answer user-response)) ; 4й способ
            )
          (case (random 3)
            ((0) (qualifier-answer user-response)) ; 1й способ
            ((1) (hedge)) ; 2й способ
            ((2) (history-answer response-history)) ; 3-й способ
            )
          )
      )
  )

; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
  (append (pick-random-vector '#((can you explain why)
                                 (did you come to me because)
                                 (does it trouble you that)
                                 (how did you come to think that)
                                 (is it important to you that)
                                 (what makes you suppose that)
                                 (what makes you think that)
                                 (why are you sure that)
                                 (why do you say that)
                                 (why do you suppose that)
                                 (why do you think that)
                                 (you feel that)
                                 (you say that)
                                 (you seem to think that))
                              )
          (change-person user-response)
          )
  )

; случайный выбор одного из элементов вектора vctr
(define (pick-random-vector vctr)
  (vector-ref vctr (random (vector-length vctr)))
  )

; случайный выбор одного из элементов списка lst
(define (pick-random-list lst)
  (list-ref lst (random (length lst)))
  )

; случайный выбор одного из элементов списка lst с учетом весов weights
; weights - список натуральных чисел (включая 0)
(define (pick-random-list-with-weight lst weights)
  (let select-item-by-weighted-index ; выбираем элемент со случайным индексом idx из списка, в котором по порядку идут
    ((idx (random (foldl + 0 weights))) (lst lst) (weights weights)) ; w_i элементов e_i для всех i от 0 до len(lst) - 1; e_i - элементы lst
    (if (< idx (car weights)) ; если idx < w_0 + .. + w_i (все элементы, равные e_i, имеют индексы из [w_0 + .. + w_i-1, w_0 + .. + w_i)),
        (car lst) ; то результатом является элемент e_i
        (select-item-by-weighted-index idx ; иначе запускаем аналогичкую проверку для следующего элемента e_i+1
                                       (cdr lst) ; и индексов [w_0 + .. + w_i, w_0 + .. + w_i+1) соответственно
                                       (cons (+ (car weights) (cadr weights)) (cddr weights))
                                       )
        )
    )
  )

; замена лица во фразе
(define (change-person phrase)
  (many-replace-v3 '((am are)
                     (are am)
                     (i you)
                     (me you)
                     (mine yours)
                     (my your)
                     (myself yourself)
                     (you i)
                     (your my)
                     (yours mine)
                     (yourself myself)
                     (we you)
                     (us you)
                     (our your)
                     (ours yours)
                     (ourselves yourselves)
                     (yourselves ourselves)
                     (shall will))
                   phrase)
  )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
  (if (null? lst)
      lst
      (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
        (cons (if pat-rep
                  (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                  (car lst) ; иначе в начале ответа помещается прежнее начало списка без изменений
                  )
              (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
              )
        )
      )
  )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs - итеративная версия
(define (many-replace-v2 replacement-pairs lst)
  (reverse ; получаем список с заменами в перевернутом виде (чтобы не использовать append), затем разворачиваем
   (let many-replace-iter ; вспомогательная функция - порождает итеративный процесс за счет хвостовой рекурсии
     ((replaced '()) (unreplaced lst)) ; replaced - начальная часть исходного списка, в которой уже произведены замены
     (if (null? unreplaced)            ; unreplaced - хвост исходного списка, над которым еще не проведены замены
         replaced ; Если исходный список кончился (хвост пустой), то возвращаем измененную часть
         (many-replace-iter (let ((pat-rep (assoc (car unreplaced) replacement-pairs)))
                              (cons (if pat-rep ; Иначе берем первый элемент хвоста исходного списка (unreplaced),
                                        (cadr pat-rep) ; производим над ним замену
                                        (car unreplaced) ; и присоединяем результат к replaced;
                                        )
                                    replaced
                                    )
                              )
                            (cdr unreplaced) ; после этого переходим к новой итерации
                            )
         )
     )
   )
  )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs - версия с map
(define (many-replace-v3 replacement-pairs lst)
  (map (lambda (pat) ; Анонимная функция для отображения исходного списка lst в список-результат
         (let ((pat-rep (assoc pat replacement-pairs)))
           (if pat-rep
               (cadr pat-rep) ; Либо находит замену для элемента pat,
               pat ; либо возвращает элемент неизменным, если такой замены не существует
               )
           )
         )
       lst)
  )

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
  (pick-random-vector '#((can you elaborate on this)
                         (can you think of a specific example)
                         (does talking about this bother you)
                         (i am not sure i understand you fully)
                         (i see)
                         (many people have the same sorts of feelings)
                         (many of my patients have told me the same thing)
                         (many had to find themselves in such a situation)
                         (many have to go through it)
                         (many of my patients have had to deal with it)
                         (please be more specific)
                         (please continue)
                         (please don't stop)
                         (please go on)
                         (tell me more about that)
                         (this is very common problem)
                         (what does that suggest to you)
                         (what is the connection, do you suppose))
                      )
  )

; 3й способ генерации ответной реплики -- реплика начинается с "earlier you said that"
; и продолжается одной из предыдущих фраз пациента, в которой произведена замена лица
(define (history-answer response-history)
  (append '(earlier you said that)
          (change-person (pick-random-vector response-history)))
  )

; список пар "список ключевых слов - список шаблонов ответа";
; структура данных для 4го способа генерации ответной реплики - keyword-answer
(define keywords-structure
  '(
    ((depressed suicide exams university)
     ((when you feel depressed, go out for ice cream)
      (depression is a disease that can be treated)))
    ((mother father parents brother sister uncle aunt grandma grandpa)
     ((tell me more about your * i want to know all about your *)
      (why do you feel that way about your *)))
    ((university scheme lections)
     ((your education is important)
      (how much time do you spend to learning)))
    )
  )

; множество всех ключевых слов для 4го способа генерации ответной реплики - keyword-answer
(define keywords-set
  (remove-duplicates ; собираем все уникальные ключевые слова из keywords-structure
   (foldl (lambda (kws-patterns-pair kws-set) (append kws-set (car kws-patterns-pair))) ; kws-patterns-pair - элемент keywords-structure,
          '()                                                                           ; пара из списка ключевых слов и списка шаблонов
          keywords-structure
          )
   )
  )

; функция-предикат, проверяющая, есть ли в реплике пользователя хотя бы одно ключевое слово
(define (contains-keyword? user-response)
  (if (empty? user-response)
      #f
      (if (member (car user-response) keywords-set) ; проверяем, является ли первое слово ответа ключевым
          #t
          (contains-keyword? (cdr user-response)) ; если нет, рекурсивно проверяем хвост ответа
          )
      )
  )

; 4й способ генерации ответной реплики - по ключевым словам
(define (keyword-answer user-response)
  (let*
      ((response-keywords (remove-duplicates (filter (lambda (word) (member word keywords-set))
                                                     user-response))) ; список всех уникальных ключевых слов, содержащихся в ответе пользователя
       (weights (foldr (lambda (keyword weights) (cons (length (indexes-of user-response keyword)) weights))
                       '()
                       response-keywords)) ; веса, с которыми эти ключевые слова встречаются в ответе пользователя
       (keyword (pick-random-list-with-weight response-keywords weights)) ; выбираем случайное ключевое слово для построения реплики
       (response-patterns (foldl (lambda (kws-patterns-pair response-patterns) ; kws-patterns-pair - элемент keywords-structure, пара из списка ключевых слов и списка шаблонов
                                   (if (member keyword (car kws-patterns-pair))
                                       (append response-patterns (cadr kws-patterns-pair))
                                       response-patterns))
                                 '()
                                 keywords-structure)) ; список всех шаблонов реплик, которые могут соответствовать выбранному ключевому слову
       (response-pattern (pick-random-list response-patterns))) ; случайным образом выбираем один из шаблонов
    (many-replace-v3 (list (list '* keyword)) response-pattern) ; производим подстановку ключевого слова в шаблон и получаем ответную реплику
    )
  )

; конструктор и селекторы для стратегии
(define (make-strategy appliable? weight body)
  (list appliable? weight body)
  )
(define (extract-appliable? strategy)
  (car strategy)
  )
(define (extract-weight strategy)
  (cadr strategy)
  )
(define (extract-body strategy)
  (caddr strategy)
  )

; выясняет, применяема ли стратегия strategy
; params - параметры предиката appliable? стратегии
(define (appliable? strategy . params)
  (apply (extract-appliable? strategy) params)
  )

; применяет стратегию strategy к ее параметрам params
(define (apply-strategy strategy . params)
  (apply (extract-body strategy) params)
  )

; список всех используемых стратегий
; тела стратегий оборачиваются в лямбды с двумя обязательными парметрами user-response и response-history
; для того, чтобы можно было потом применять apply-strategy в унифицированном виде (см. reply-v2)
(define strategies
  (list (make-strategy (lambda params #t)
                       4
                       (lambda (user-response response-history . other-params)
                         (qualifier-answer user-response)))
        (make-strategy (lambda params #t)
                       1
                       (lambda (user-response response-history . other-params)
                         (hedge)))
        (make-strategy (lambda (user-response response-history . other-params) (not (vector-empty? response-history)))
                       2
                       (lambda (user-response response-history . other-params)
                         (history-answer response-history)))
        (make-strategy (lambda (user-response . other-params) (contains-keyword? user-response))
                       8
                       (lambda (user-response response-history . other-params)
                         (keyword-answer user-response)))
        )
  )

(define (reply-v2 strategies user-response response-history)
  (let*
      ((appliable-strategies (filter (lambda (strategy) (appliable? strategy user-response response-history))
                                     strategies)) ; список стратегий, применимых в текущей ситуации
       (weights (map extract-weight appliable-strategies)) ; веса применимых стратегий
       (chosen-strategy (pick-random-list-with-weight appliable-strategies weights))) ; случайным образом выбираем одну из стратегий
    (apply-strategy chosen-strategy user-response response-history) ; применяем выбранную стратегию и возвращаем ее результат в качестве ответной реплики
    )
  )
