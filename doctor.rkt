; "Доктор". Осень 2021
#lang scheme/base
; В учебных целях используется базовая версия Scheme

(require racket/vector)
; подключаем функции для работы с векторами

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name)
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

; генерация ответной реплики по user-response -- реплике от пользователя
(define (reply user-response)
  (case (random 2) ; с равной вероятностью выбирается один из двух способов построения ответа
    ((0) (qualifier-answer user-response)) ; 1й способ
    ((1) (hedge))  ; 2й способ
    )
  )

; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
  (append (pick-random-vector '#((you seem to think that)
                                 (you feel that)
                                 (why do you believe that)
                                 (why do you say that)
                                 (why do you suppose that)
                                 (why are you sure that)
                                 (what makes you believe that)
                                 (what makes you feel that)
                                 (what makes you suppose that)
                                 (what makes you think that)
                                 (can you be sure that)
                                 (how did you come to think that)
                                 (so you notice that)
                                 (you are under the impression that))
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
         replaced ; Если исходный список кончился (хвост пустой), то возвращаем измененную часть.
         (many-replace-iter (let ((pat-rep (assoc (car unreplaced) replacement-pairs)))
                              (cons (if pat-rep  ; Иначе берем первый элемент хвоста исходного списка (unreplaced),
                                        (cadr pat-rep) ; производим над ним замену
                                        (car unreplaced) ; и присоединяем результат к replaced;
                                        )
                                    replaced
                                    )
                              )
                            (cdr unreplaced) ; после этого переходим к новой итерации.
                            )
         )
     )
   )
  )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs - версия с map
(define (many-replace-v3 replacement-pairs lst)
  (map (lambda (pat) ; Анонимная функция для отображения исходного списка lst в список-результат.
         (let ((pat-rep (assoc pat replacement-pairs)))
           (if pat-rep
               (cadr pat-rep) ; Либо находит замену для элемента pat,
               pat ; либо возвращает элемент неизменным, если такой замены не существует.
               )
           )
         )
       lst)
  )

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
  (pick-random-vector '#((please go on)
                         (many people have the same sorts of feelings)
                         (many of my patients have told me the same thing)
                         (many had to find themselves in such a situation)
                         (many have to go through it)
                         (many of my patients have had to deal with it)
                         (please continue)
                         (please don't stop)
                         (please tell me more)
                         (this is very common problem))
                      )
  )