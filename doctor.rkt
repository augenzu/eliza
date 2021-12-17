; "Доктор". Осень 2021

; В учебных целях используется базовая версия Scheme
#lang scheme/base

(require racket/vector)
(require racket/list)

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

; основная функция, запускающая многопользовательского "Доктора"
; параметр stop-word -- стоп-слово, после использования которого в качестве имени очередного пациента Доктор завершает работу
; параметр max-patients-served -- максимальное количество пациентов, которое может принять Доктор
(define (visit-doctor stop-word max-patients-served)
  (let visit-doctor-iter ; функция-итерация (играет роль однопользовательского Доктора)
    ((patients-served 0) (name (ask-patient-name))) ; patients-served - счетчик принятых пациентов, name - имя очередного пациента
    (if (equal? name stop-word) ; если вместо имени пациента введено стоп-слово,
        (print '(time to go home)) ; Доктор завершает работу
        (begin (printf "Hello, ~a!\n" name) ; Иначе выводит приветствие
               (print '(what seems to be the trouble?))
               (doctor-driver-loop name #()) ; и запускает цикл диалога с пациентом
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
(define (doctor-driver-loop name response-history)
  (newline)
  (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
  (let ((user-response (read)))
    (if (equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
        (begin (printf "Goodbye, ~a!\n" name)
               (print '(see you next week))
               (newline))
        (begin (print (reply strategies user-response response-history)) ; иначе Доктор генерирует ответ, печатает его
               (doctor-driver-loop name (vector-append (vector user-response) response-history))) ; и продолжает цикл
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

; замена лица во фразе
(define (change-person phrase)
  (many-replace '((am are)
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

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs - версия с map
(define (many-replace replacement-pairs lst)
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
      (depression is a disease that can be treated)
      (i know you are trying hard)
      (when you are exhausted, you should stop studying for sometimes)
      (tell me what you really want right now)))
    ((mother father parents brother sister uncle aunt grandma grandpa)
     ((tell me more about your * i want to know all about your *)
      (why do you feel that way about your *)
      (have you discussed this wih your *)
      (how does your * treat you)
      (even our relatives can sometimes be toxic)))
    ((university scheme lections)
     ((your education is important)
      (how much time do you spend to learning)
      (do you hate *)
      (* is important but so are you)))
    ((apathy depression depressed suicide sadness stress distress melancholy worry nervous relapse relapsing)
     ((that must be very challenging for you)
      (you seem like you are under a lot of stress right now)
      (this seems like it is really confusing for you)
      (i know this shit is really hard to live with)))
    ((health weakness tremor fatigue lethargy apathy fever temperature sweat health sleep sick sickness nausea)
     ((your health is important, do not forget about it)
      (do not forget to take care of yourself)
      (* can be a syndrom of some diseases, you should tell your therapist about it)
      (* can affect your mental state)))
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
  (ormap (lambda (word) (member word keywords-set))
         user-response)
  )

; 4й способ генерации ответной реплики - по ключевым словам
(define (keyword-answer user-response)
  (let*
      ((response-keywords (filter (lambda (word) (member word keywords-set))
                                  user-response)) ; список всех уникальных ключевых слов, содержащихся в ответе пользователя
       (weights (foldr (lambda (keyword weights) (cons (length (indexes-of user-response keyword)) weights))
                       '()
                       response-keywords)) ; веса, с которыми эти ключевые слова встречаются в ответе пользователя
       (keyword (pick-random-list response-keywords)) ; выбираем случайное ключевое слово для построения реплики
       (response-patterns (foldl (lambda (kws-patterns-pair response-patterns) ; kws-patterns-pair - элемент keywords-structure, пара из списка ключевых слов и списка шаблонов
                                   (if (member keyword (car kws-patterns-pair))
                                       (append response-patterns (cadr kws-patterns-pair))
                                       response-patterns))
                                 '()
                                 keywords-structure)) ; список всех шаблонов реплик, которые могут соответствовать выбранному ключевому слову
       (response-pattern (pick-random-list response-patterns))) ; случайным образом выбираем один из шаблонов
    (many-replace (list (list '* keyword)) response-pattern) ; производим подстановку ключевого слова в шаблон и получаем ответную реплику
    )
  )

; конструктор и селекторы для стратегии
(define (make-strategy appliable? weight body)
  (list appliable? weight body)
  )
(define (extract-appliable strategy)
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
  (apply (extract-appliable strategy) params)
  )

; применяет стратегию strategy к ее параметрам params
(define (apply-strategy strategy . params)
  (apply (extract-body strategy) params)
  )

; список всех используемых стратегий
; тела стратегий оборачиваются в лямбды, в которых праметры если появляются,
; то обязательно в таком порядке: user-response, response-history, other-params
; для того, чтобы можно было потом применять apply-strategy в унифицированном виде (см. reply)
(define strategies
  (list (make-strategy (lambda params #t)
                       4
                       (lambda (user-response . other-params)
                         (qualifier-answer user-response)))
        (make-strategy (lambda params #t)
                       1
                       (lambda other-params
                         (hedge)))
        (make-strategy (lambda (user-response response-history . other-params) (not (vector-empty? response-history)))
                       2
                       (lambda (user-response response-history . other-params)
                         (history-answer response-history)))
        (make-strategy (lambda (user-response . other-params) (contains-keyword? user-response))
                       8
                       (lambda (user-response . other-params)
                         (keyword-answer user-response)))
        )
  )

; случайный выбор одного из элементов списка стратегий strategies с учетом весов
; для каждой стратегии ее вес находится во 2м по счету поле структуры, а тело - в 3м
; (см. конструктор и селекторы структуры strategy)
(define (pick-random-strategy-with-weight strategies)
  (let select-strategy-by-weighted-index ; выбираем стратегию со случайным индексом idx из списка, в котором по порядку идут
    ((idx (random (foldl (lambda (strategy weights-sum) ; w_i стратегий s_i для всех i от 0 до len(strategies) - 1; s_i - стратегии (элементы strategies)
                           (+ weights-sum (extract-weight strategy)))
                         0
                         strategies)))
     (strategies strategies))
    (if (< idx (extract-weight (car strategies))) ; если idx < w_0 + .. + w_i (все стратегии, равные s_i, имеют индексы из [w_0 + .. + w_i-1, w_0 + .. + w_i)),
        (car strategies) ; то результатом является стратегия s_i; (idx < w_0 + .. + w_i <=> idx - (w_0 + .. + w_i-1) < w_i - поэтому на каждой новой итерации мы уменьшаем idx на текущий вес)
        (select-strategy-by-weighted-index (- idx (extract-weight (car strategies))) ; иначе запускаем аналогичкую проверку для следующей стратегии s_i+1
                                           (cdr strategies) ; и индексов [w_0 + .. + w_i, w_0 + .. + w_i+1) соответственно
                                           )
        )
    )
  )

(define (reply strategies user-response response-history)
  (let*
      ((appliable-strategies (filter (lambda (strategy) (appliable? strategy user-response response-history))
                                     strategies)) ; список стратегий, применимых в текущей ситуации
       (chosen-strategy (pick-random-strategy-with-weight appliable-strategies))) ; случайным образом выбираем одну из стратегий
    (apply-strategy chosen-strategy user-response response-history) ; применяем выбранную стратегию и возвращаем ее результат в качестве ответной реплики
    )
  )
