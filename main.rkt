; заготовка "Доктора". Сентябрь 2026
; В учебных целях используется базовая версия Scheme
#lang scheme/base

; Подключаем Racket-библиотеки для векторов и списков, на всякий случай
(require racket/vector)
(require racket/list)

; Упражнение 4: "многопользовательский" Доктор
; основная функция, запускающая "Доктора"
; параметр stop-word -- имя, при вводе которого доктор заканчивает работу (по умолчанию suppertime)
; параметр max-patients -- сколько пациентов доктор примет, прежде чем закончить (по умолчанию 3)
; можно вызывать как (visit-doctor), так и, например, (visit-doctor 'suppertime 5)
(define (visit-doctor [stop-word 'suppertime] [max-patients 3])
  (let loop ((patients-left max-patients))
    (if (<= patients-left 0)
        (print '(time to go home)) ; приняли всех, кого собирались
        (let ((name (ask-patient-name)))
          (cond ((equal? name stop-word)
                 (print '(time to go home))) ; введено стоп-слово
                (else
                 (see-patient name)               ; приём одного пациента
                 (loop (sub1 patients-left))))))) ; переход к следующему
)

; запрос имени очередного пациента (из PDF)
; пациент вводит имя списком, например (Hal Abelson), берётся первое слово
(define (ask-patient-name)
  (newline)
  (print '(next!))
  (newline)
  (print '(who are you?))
  (newline)
  (car (read))
)

; приём одного пациента -- то, что раньше делала visit-doctor
; параметр name -- имя пациента
(define (see-patient name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name '()) ; Упражнение 3: в начале приёма история реплик пуста
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
; Упражнение 3: параметр history -- список всех прошлых реплик пациента
; (новые реплики добавляются в начало списка). set! не нужен:
; при каждом рекурсивном вызове цикла передаётся новый, расширенный список.
(define (doctor-driver-loop name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name (cons user-response history)) ; запоминаем текущую реплику
             )
       )
      )
)

; генерация ответной реплики по user-response -- реплике от пользователя
; Упражнение 3: history -- список прошлых реплик пациента
; Пока история пуста, третий способ невозможен, поэтому выбор идёт из двух способов;
; когда история есть -- из трёх, с равной вероятностью
(define (reply user-response history)
      (case (random 0 (if (null? history) 2 3))
          ((0) (hedge-answer))  ; 1й способ
          ((1) (qualifier-answer user-response)) ; 2й способ
          ((2) (history-answer history)) ; 3й способ (Упражнение 3)
      )
)

; Упражнение 3
; 3й способ генерации ответной реплики -- случайная прошлая реплика пациента
; с заменой лица и приписанным началом (earlier you said that)
(define (history-answer history)
  (append '(earlier you said that)
          (change-person (pick-random-list history))))

; случайный выбор одного из элементов непустого списка
(define (pick-random-list lst)
  (list-ref lst (random 0 (length lst))))

; 1й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
; Упражнение 1: репертуар расширен с 4 до 10 фраз
(define (hedge-answer)
       (pick-random-vector #((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              (i see)
                              (that is very interesting)
                              (tell me more about it)
                              (how does that make you feel)
                              (go on i am listening)
                              (such feelings are quite common))
         )
)

; случайный выбор одного из элементов непустого вектора
(define (pick-random-vector vctr)
  (vector-ref vctr (random 0 (vector-length vctr)))
)

; 2й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату случайно выбранного нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector #((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       (it seems to you that)
                                       (so you are saying that)
                                       (what makes you think that)
                                       (are you sure that)
                                       (how long have you felt that)
                                       (does it bother you that))
                )
                (change-person user-response)
        )
 )

; список пар для замены лица (в обе стороны)
(define person-pairs
  '((am are)
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
    (shall will)))

; замена лица во фразе
(define (change-person phrase)
  (many-replace person-pairs phrase))

; замена одного слова: если слово есть в списке пар -- берём замену, иначе оставляем
(define (replace-word word replacement-pairs)
  (let ((pat-rep (assoc word replacement-pairs)))
    (if pat-rep (cadr pat-rep) word)))

(define (many-replace replacement-pairs lst)
  (map (lambda (word) (replace-word word replacement-pairs)) lst))

; в Racket нет vector-foldl, реализуем для случая с одним вектором (vect-foldl f init vctr)
; у f три параметра i -- индекс текущего элемента, result -- текущий результат свёртки, elem -- текущий элемент вектора
(define (vector-foldl f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i 0) (result init))
   (if (= i length) result
    (loop (add1 i) (f i result (vector-ref vctr i)))))))
	
; аналогично от конца вектора к началу
(define (vector-foldr f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i (sub1 length)) (result init))
   (if (= i -1) result
    (loop (sub1 i) (f i result (vector-ref vctr i)))))))