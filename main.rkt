; заготовка "Доктора". Сентябрь 2026
; В учебных целях используется базовая версия Scheme
#lang scheme/base
(provide (all-defined-out))

; Подключаем Racket-библиотеки для векторов и списков, на всякий случай
(require racket/vector)
(require racket/list)

; task 4
; (visit-doctor 'suppertime 3)
(define (visit-doctor stop-word max-patients)
  (let loop ((served 0))
    (if (>= served max-patients)
        (print '(time to go home))
        (let ((name (ask-patient-name)))
          (if (equal? name stop-word)
              (print '(time to go home))
              (begin (doctor-session name)
                     (newline)
                     (loop (add1 served))))))))

; task 4
(define (ask-patient-name)
  (print '(next!))
  (newline)
  (print '(who are you?))
  (newline)
  (let ((answer (read)))
    (if (pair? answer)
        (car answer)
        (ask-patient-name))))

(define (doctor-session name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name '()) ; task 3
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
; task 3
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
; task 3, task 5
(define (reply user-response history)
  (let ((strategies (append '(hedge qualifier)
                            (if (null? history) '() '(history))
                            (if (has-keywords? user-response) '(keyword) '()))))
    (case (pick-random-list strategies)
      ((hedge) (hedge-answer))                          ; 1й способ
      ((qualifier) (qualifier-answer user-response))    ; 2й способ
      ((history) (history-answer history))              ; 3й способ (task 3)
      ((keyword) (keyword-answer user-response)))))     ; 4й способ (task 5)

; task 5
(define keywords-structure
  '(
    ( (depressed suicide)
      ((when you feel depressed go out for ice cream)
       (depression is a disease that can be treated)
       (have you talked to someone you trust about how you feel ?)) )
    ( (mother father parents brother sister uncle aunt grandma grandpa)
      ((tell me more about your *)
       (why do you feel that way about your * ?)
       (does your * know how you feel)
       (what was your relationship with your * like when you were a child)) )
    ( (university exams lectures studies)
      ((your education is important)
       (how much time do you spend on your studies ?)
       (what worries you most about your *)) )
    ( (friend friends girlfriend boyfriend)
      ((tell me more about your *)
       (do you trust your * ?)
       (how did you meet your * ?)) )
    ( (work job boss colleagues)
      ((how do you feel about your * ?)
       (does your * make you anxious ?)
       (many people find their * stressful)) )
  )
)

(define all-keywords
  (remove-duplicates (apply append (map car keywords-structure))))

(define (keyword? word)
  (if (member word all-keywords) #t #f))

(define (has-keywords? user-response)
  (ormap keyword? user-response))

(define (keywords-in user-response)
  (filter keyword? user-response))

(define (templates-for keyword)
  (apply append
         (map cadr
              (filter (lambda (group) (member keyword (car group)))
                      keywords-structure))))

; task 5
(define (keyword-answer user-response)
  (let* ((keyword (pick-random-list (keywords-in user-response)))
         (template (pick-random-list (templates-for keyword))))
    (many-replace (list (list '* keyword)) template)))

; task 3
(define (history-answer history)
  (append '(earlier you said that)
          (change-person (pick-random-list history))))

(define (pick-random-list lst)
  (list-ref lst (random 0 (length lst))))

; 1й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
       (pick-random-vector #((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              ; новые фразы
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
                                       ; task 1
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

; task 2
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