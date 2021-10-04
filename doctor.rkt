; "Доктор". Осень 2021

; В учебных целях используется базовая версия Scheme
#lang scheme/base

; подключаем функции для работы с векторами
(require racket/vector)

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
    (cond
      	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
                  )
            )
    )
  )

; основная функция, запускающая многопользовательского "Доктора"
; параметр stop-word -- стоп-слово, после использования которого в качестве имени очередного пациента Доктор завершает работу
; параметр max-patients-served -- максимальное количество пациентов, которое может принять Доктор
(define (visit-doctor-v2 stop-word max-patients-served)
  (let visit-doctor-iter ; функция-итерация (играет роль однопользовательского Доктора)
    ((patients-served 0) (name (ask-patient-name))) ; patients-served - счетчик принятых пациентов, name - имя очередного пациента
    (cond ((equal? name stop-word) ; если вместо имени пациента введено стоп-слово,
           (print '(time to go home))) ; Доктор завершает работу
          (else
           (printf "Hello, ~a!\n" name) ; Иначе выводит приветствие
           (print '(what seems to be the trouble?))
           (doctor-driver-loop-v2 name #()) ; и запускает цикл диалога с пациентом
           (cond ((= (add1 patients-served) max-patients-served) ; если на данный момент принято максимальное количество пациентов,
                  (newline)
                  (print '(time to go home))) ; то Доктор завершает работу
                 (else
                  (visit-doctor-iter (add1 patients-served) (ask-patient-name))) ; Иначе переходит к новой итерации, запрашивая имя следующего пациента
                 )
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
    (cond ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
           (printf "Goodbye, ~a!\n" name)
           (print '(see you next week))
           (newline))
          (else
           (print (reply user-response response-history)) ; иначе Доктор генерирует ответ, печатает его
           (doctor-driver-loop-v2 name (vector-append (vector user-response) response-history)) ; и продолжает цикл
           )
          )
    )
  )

; генерация ответной реплики по user-response -- реплике от пользователя
; параметр response-history -- вектор всех предыдущих ответов пользователя,
; нужен для стратегии history-answer
(define (reply user-response response-history)
  (if (vector-empty? response-history) ; история пуста - пациент ввел первую реплику
      (case (random 2) ; => применимы только первые две стратегии
        ((0) (qualifier-answer user-response)) ; 1й способ
        ((1) (hedge))  ; 2й способ
        )
      (case (random 3) ; с равной вероятностью выбирается один из трех способов построения ответа
        ((0) (qualifier-answer user-response)) ; 1й способ
        ((1) (hedge))  ; 2й способ
        ((2) (history-answer response-history)) ; 3-й способ
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
  (cond ((null? lst) lst)
        (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                (cons (if pat-rep
                          (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                          (car lst) ; иначе в начале ответа помещается прежнее начало списка без изменений
                          )
                      (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                      )
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
                              (cons (if pat-rep  ; Иначе берем первый элемент хвоста исходного списка (unreplaced),
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
  (pick-random-vector '#((can you elaborate on this?)
                         (can you think of a specific example?)
                         (does talking about this bother you?)
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
                         (what does that suggest to you?)
                         (what is the connection, do you suppose?))
                      )
  )

; 3й способ генерации ответной реплики -- реплика начинается с "earlier you said that"
; и продолжается одной из предыдущих фраз пациента, в которой произведена замена лица
(define (history-answer response-history)
  (append '(earlier you said that)
          (change-person (pick-random-vector response-history)))
  )
