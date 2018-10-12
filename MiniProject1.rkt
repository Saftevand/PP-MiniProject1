#lang racket

;Author:  Mathias Lundhede Hansen
;         Mlha15@student.aau.dk
;         20154238

(require "music-base.rkt")

(define velocity 80) ;keeping the velocity constant

;Predicates
(define (pitch? pitchvalue)
  (and (real? pitchvalue) (>= pitchvalue 0) (<= pitchvalue 127)))

(define (duration? x)
  (and (real? x) (>= x 0)))

(define (starttime? x)
  (and (real? x) (>= x 0)))

(define (instrument? type)
  (if(real? type) (and(>= type 0)(<= type 8)) #f))

(define (note? x)
  (and (procedure? x) (eqv? 'Note (send 'get-type x))))

(define (pause? x)
  (and (procedure? x) (eqv? 'Pause (send 'get-type x))))

(define (sequentialMusicElement? x)
  (and (procedure? x) (eqv? 'SequentialMusicElement (send 'get-type x))))

(define (parallelMusicElement? x)
  (and (procedure? x)(eqv? 'ParallelMusicElement (send 'get-type x))))

(define (musicElement? x)
  (if(procedure? x)
    (cond((note? x) #t)
         ((pause? x) #t)
         ((sequentialMusicElement? x) #t)
         ((parallelMusicElement? x) #t)
         (else #f))
    #f))



;Functions
(define (send message obj . par)
  (let ((method (obj message)))
    (apply method par)))

(define (can-create-sequential-or-parallel-MusicElement? x)
  (if(null? x)
     #f
     (can-create-sequential-or-parallel-MusicElement-helper x)))

(define (can-create-sequential-or-parallel-MusicElement-helper x)
  (if(null? x)
     #t
     (if(musicElement? (car x))
        (can-create-sequential-or-parallel-MusicElement-helper (cdr x))
        #f)))




(define (convert-to-note-abs-time-with-duration seq-mus-elem)
  (remove-empty(list-builder seq-mus-elem '())))

(define (convert-to-note-abs-time-with-duration-note x)
  (note-abs-time-with-duration (send 'get-start-time x) (send 'get-instrument x) (send 'get-pitch x) velocity (send 'get-duration x)))

(define (convert-to-note-abs-time-with-duration-pause x)
  (note-abs-time-with-duration (send 'get-start-time x) 1 1 0 (send 'get-duration x)))

(define (convert-to-note-abs-time-with-duration-recursive x)
  (if(null? x)
     '()
     (if (musicElement? x)
         (cond ((eqv? 'Note (send 'get-type x))
              (convert-to-note-abs-time-with-duration-note x))
           ((eqv? 'Pause (send 'get-type  x))
              (convert-to-note-abs-time-with-duration-pause x))
           ((eqv? 'SequentialMusicElement (send 'get-type  x))
              (convert-to-note-abs-time-with-duration-recursive (send 'get-elements x)))
           ((eqv? 'ParallelMusicElement (send 'get-type x))
              (convert-to-note-abs-time-with-duration-recursive (send 'get-elements x)))
           (else error "Error: Unkown type"))
         
         (if(musicElement? (car x))
         (cond ((eqv? 'Note (send 'get-type (car x)))
              (cons (convert-to-note-abs-time-with-duration-note (car x))
                    (convert-to-note-abs-time-with-duration-recursive (cdr x))))
           ((eqv? 'Pause (send 'get-type (car x)))
              (cons (convert-to-note-abs-time-with-duration-pause (car x))
                    (convert-to-note-abs-time-with-duration-recursive (cdr x))))
           ((eqv? 'SequentialMusicElement (send 'get-type (car x)))
              (cons (convert-to-note-abs-time-with-duration-recursive (car x))
                    (convert-to-note-abs-time-with-duration-recursive (cdr x))))
           ((eqv? 'ParallelMusicElement (send 'get-type (car x)))
              (cons (convert-to-note-abs-time-with-duration-recursive (car x))
                    (convert-to-note-abs-time-with-duration-recursive (cdr x))))
           (else error "Error: Unkown type"))
         (append (convert-to-note-abs-time-with-duration-recursive (car x)) (convert-to-note-abs-time-with-duration-recursive (cdr x)))))))


(define (list-builder x)
  (if(null? x)
     '()
     (if (pair? x)
     (cond ((and (procedure? (car x)) (eqv? 'Note (send 'get-type (car x))))
              (cons (car x) (list-builder (cdr x))))           
           ((and (procedure? (car x)) (eqv? 'Pause (send 'get-type (car x))))
              (cons (car x) (list-builder (cdr x))))           
           ((and (procedure? (car x)) (eqv? 'SequentialMusicElement (send 'get-type (car x))))
              (cons (list-builder (send 'get-elements (car x))) (list-builder (cdr x))))           
           ((and (procedure? (car x)) (eqv? 'ParallelMusicElement (send 'get-type (car x))))
              (cons (list-builder (send 'get-elements (car x))) (list-builder (cdr x))))           
           (else list-builder (car x)))
     
     (cond ((and (procedure? x) (eqv? 'Note (send 'get-type x))) x)           
           ((and (procedure? x) (eqv? 'Pause (send 'get-type x))) x)           
           ((and (procedure? x) (eqv? 'SequentialMusicElement (send 'get-type x))) (list-builder (send 'get-elements x)))           
           ((and (procedure? x) (eqv? 'ParallelMusicElement (send 'get-type x))) (list-builder (send 'get-elements x)))           
           (else error "Invalid musicElement")))))

(define (remove-empty lst)
  (remove-empty-helper lst '()))

(define (remove-empty-helper lst result-lst)
  (if (pair? lst)
      (if(null? (car lst))
        (remove-empty-helper (cdr lst) result-lst)
        (cons (car lst)(remove-empty-helper (cdr lst) result-lst)))
      lst))

(define (calc-duration-seq x)
  (calc-duration-seq-helper x 0))

(define (calc-duration-seq-helper x res)
  (if(null? x)
     res
     (calc-duration-seq-helper (cdr x) (+ (send 'get-duration (car x)) res))))

(define (calc-duration-par x)
  (calc-duration-par-helper x 0))

(define (calc-duration-par-helper x longest-duration)
  (if(null? x)
     longest-duration
     (if(and (procedure? x))
        (if (> (send 'get-duration x) longest-duration)
            (send 'get-duration x)
            longest-duration)
        (if(> (send 'get-duration (car x)) longest-duration)
           (calc-duration-par-helper (cdr x) (send 'get-duration (car x)))
           (calc-duration-par-helper (cdr x) longest-duration)))))

(define (set-start-time-par x time)
  (if(null? x)
     '()
     (if(musicElement? x)
        (cond((eqv? 'Note (send 'get-type x)) (note (send 'get-pitch x) (send 'get-duration x) (send 'get-instrument x) time))
             ((eqv? 'Pause (send 'get-type x)) (pause (send 'get-duration x) time))
             ((eqv? 'SequentialMusicElement (send 'get-type x)) (set-start-time-seq x time))
             ((eqv? 'ParallelMusicElement (send 'get-type x)) (set-start-time-par (send 'get-elements x) time))
             (else error "Error: Unkown type"))
        
        (cond((eqv? 'Note (send 'get-type (car x)))
                (cons (note (send 'get-pitch (car x)) (send 'get-duration (car x)) (send 'get-instrument (car x)) time) (set-start-time-par (cdr x) time)))
             ((eqv? 'Pause (send 'get-type (car x)))
                (cons (pause (send 'get-duration (car x)) time) (set-start-time-par (cdr x) time)))
             ((eqv? 'SequentialMusicElement (send 'get-type (car x)))
                (cons (set-start-time-seq (car x) time) (set-start-time-par (cdr x) time)))
             ((eqv? 'ParallelMusicElement (send 'get-type (car x)))
                (cons (set-start-time-par (car x) time) (set-start-time-par (cdr x) time)))
             (else error "Error: Unkown type")))))

(define (set-start-time-seq x time)
  (if(null? x)
     '()
     (if(musicElement? x)
        (cond((eqv? 'Note (send 'get-type x)) (note (send 'get-pitch x) (send 'get-duration x) (send 'get-instrument x) time))
             ((eqv? 'Pause (send 'get-type x)) (pause (send 'get-duration x) time))
             ((eqv? 'SequentialMusicElement (send 'get-type x)) (set-start-time-seq (send 'get-elements x) time))
             ((eqv? 'ParallelMusicElement (send 'get-type x)) (set-start-time-par (send 'get-elements x) time))
             (else error "Error: Unkown type"))
        
        (if (musicElement? (car x))
            (cond((eqv? 'Note (send 'get-type (car x)))
                  (cons (note (send 'get-pitch (car x)) (send 'get-duration (car x)) (send 'get-instrument (car x)) time) (set-start-time-seq (cdr x) (+ (send 'get-duration (car x)) time))))
                 ((eqv? 'Pause (send 'get-type (car x)))
                  (cons (pause (send 'get-duration (car x)) time) (set-start-time-seq (cdr x) (+ (send 'get-duration (car x))time))))
                 ((eqv? 'SequentialMusicElement (send 'get-type (car x)))
                  (cons (set-start-time-seq (car x) time) (set-start-time-seq (cdr x) (+ (send 'get-duration (car x))time))))
                 ((eqv? 'ParallelMusicElement (send 'get-type (car x)))
                  (cons (set-start-time-par (car x) time) (set-start-time-par (cdr x) (+ (send 'get-duration (car x))time))))
                 (else error "Error: Unkown type"))
            (set-start-time-seq (car x) time)))))
                        

(define (transpose-element x amount) ;Virker ikke helt endnu makkeres
  (if(null? x)
     '()
     (if(musicElement? x)
        (cond((eqv? 'Note (send 'get-type x)) (send 'transpose x amount))
             ((eqv? 'Pause (send 'get-type x)) x)
             ((eqv? 'SequentialMusicElement (send 'get-type x)) (send 'transpose x amount)) ;(SequentialMusicElement (car (transpose-element (send 'get-elements x) amount))))
             ((eqv? 'ParallelMusicElement (send 'get-type x)) (send 'transpose x amount));(ParallelMusicElement(car (transpose-element (send 'get-elements x) amount ))))
             (else error "Error: Unkown type"))
        (if(musicElement? (car x))
           (cond((eqv? 'Note (send 'get-type (car x))) (cons (send 'transpose (car x) amount) (transpose-element (cdr x) amount)))
             ((eqv? 'Pause (send 'get-type (car x))) (transpose-element (cdr x) amount))
             ((eqv? 'SequentialMusicElement (send 'get-type (car x))) (append (transpose-element (car x) amount) (transpose-element (cdr x) amount)))
             ((eqv? 'ParallelMusicElement (send 'get-type (car x))) (append (transpose-element (car x) amount) (transpose-element (cdr x) amount)))
             (else error "Error: Unkown type"))
           (transpose-element (car x) amount)))))

(define (convert-to-pitch x) ; Ved ikke om den her skal bruges?
  (cond((eqv? x ) "Piano")
       ((eqv? x 2) "Organ")
       ((eqv? x 3) "Guitar")
       ((eqv? x 4) "Violin")
       ((eqv? x 5) "Flute")
       ((eqv? x 6) "Trumpet")
       ((eqv? x 7) "Helicopter")
       ((eqv? x 8) "Telephone")
       (else (error "Invalid instrument"))))

(define (convert-from-instrument x) ; "" -> '
  (cond((eqv? x 'Piano) 1)
       ((eqv? x 'Organ) 2)
       ((eqv? x 'Guitar) 3)
       ((eqv? x 'Violin) 4)
       ((eqv? x 'Flute) 5)
       ((eqv? x 'Trumpet) 6)
       ((eqv? x 'Helicopter) 7)
       ((eqv? x 'Telephone)  8)
       (else (error "Invalid instrument"))))
        


;Constructor functions
(define (note pitch duration instrument start)
  (if(and (pitch? pitch) (duration? duration) (instrument? instrument))
     (let ((pitch-value pitch)
        (duration-value duration)
        (instrument-type instrument)
        (start-time start))    

       (define (get-pitch) pitch-value)
       (define (get-duration) duration-value)
       (define (get-instrument) instrument-type)
       (define (get-start-time) start-time)
       (define (scale factor) (note pitch-value (* factor duration-value) instrument-type start-time))
       (define (transpose amount) (note (+ amount pitch-value) duration-value instrument-type start-time))
       (define (re-instrument new-instrument) (note pitch-value duration-value new-instrument start-time))
       (define (get-type) 'Note)
       (define (self message)
         (cond ((eqv? message 'get-pitch) get-pitch)
               ((eqv? message 'get-duration) get-duration)
               ((eqv? message 'get-instrument) get-instrument)
               ((eqv? message 'get-start-time) get-start-time)
               ((eqv? message 'scale) scale)
               ((eqv? message 'transpose) transpose)
               ((eqv? message 're-instrument) re-instrument)
               ((eqv? message 'get-type) get-type)))
       self)
     (error "Invalid note!")))
    
(define (pause duration start)
  (if(duration? duration)
     (let ((duration-value duration)
           (start-time start))

       (define (get-duration) duration-value)
       (define (get-type) 'Pause)
       (define (get-start-time) start-time)
       (define (self message)
         (cond ((eqv? message 'get-duration) get-duration)
               ((eqv? message 'get-start-time) get-start-time)
               ((eqv? message 'get-type) get-type)))
       self)
     (error "Invalid pause!")))

(define (SequentialMusicElement . elements)
  (if(can-create-sequential-or-parallel-MusicElement? elements)
     (let ((elements-list elements))

       (define (get-elements) elements-list)
       (define (get-type) 'SequentialMusicElement)
       (define (transpose amount) (SequentialMusicElement (transpose-element elements-list amount)))
       (define (get-duration) (calc-duration-seq elements-list))
       (define (self message)
         (cond ((eqv? message 'get-elements) get-elements)
               ((eqv? message 'transpose) transpose)
               ((eqv? message 'get-duration) get-duration)
               ((eqv? message 'get-type) get-type)))
       self)
     (error "Invalid SequentialMusicElement")))

(define (ParallelMusicElement . elements)
  (if(can-create-sequential-or-parallel-MusicElement? elements)
     (let ((elements-list elements))

       (define (get-elements) elements-list)
       (define (get-type) 'ParallelMusicElement)
       (define (transpose amount) (ParallelMusicElement (transpose-element elements-list amount)))
       (define (get-duration) (calc-duration-par elements-list))
       (define (self message)
         (cond ((eqv? message 'get-elements) get-elements)
               ((eqv? message 'transpose) transpose)
               ((eqv? message 'get-duration) get-duration)
               ((eqv? message 'get-type) get-type)))
       self)
     (error "Invalid ParallelMusicElement")))




;The song
(define (note-duration x)
  (* x 420))
;960 midi = 1 sec
;BPM 180

;(note 5 (note-duration 1) (convert-from-instrument ') 0)
;(pause (note-duration 5) 0)


(define TrebleClef1 (SequentialMusicElement (pause (note-duration 1) 0)
                                           (note 59 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                           (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0) ;1
                                           (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                           
                                           (note 59 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                           (note 62 (note-duration 4) (convert-from-instrument 'Piano) 0)
                                           (note 59 (note-duration 1) (convert-from-instrument 'Piano) 0) ;2
                                           (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                           (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                           
                                           (note 59 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                           (note 57 (note-duration 4) (convert-from-instrument 'Piano) 0)
                                           (note 59 (note-duration 1) (convert-from-instrument 'Piano) 0) ;3
                                           (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                           (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                           
                                           (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                           (note 57 (note-duration 2) (convert-from-instrument 'Piano) 0)
                                           (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                           (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0) ;4
                                           (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                           (note 59 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                           (note 55 (note-duration 4) (convert-from-instrument 'Piano) 0)))

(define BassClef-B(SequentialMusicElement;d
  (note 50 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 50 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 50 (note-duration 2) (convert-from-instrument 'Piano) 0)))
(define BassClef-A(SequentialMusicElement;c
  (note 48 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 48 (note-duration 3) (convert-from-instrument 'Piano) 0) 
  (note 48 (note-duration 2) (convert-from-instrument 'Piano) 0)))
(define BassClef-G(SequentialMusicElement;b
  (note 47 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 47 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 47 (note-duration 2) (convert-from-instrument 'Piano) 0)))
(define BassClef-C(SequentialMusicElement;e
  (note 52 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 52 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 52 (note-duration 2) (convert-from-instrument 'Piano) 0)))

(define BassClef1 (SequentialMusicElement (pause (note-duration 4) 0) ;1
                                          BassClef-A                  ;2
                                          BassClef-B                  ;3
                                          BassClef-G))                ;4

(define TrebleClef2 (SequentialMusicElement TrebleClef1
                                            (note 59 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0) ;5
                                            (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                           
                                            (note 59 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 62 (note-duration 4) (convert-from-instrument 'Piano) 0)
                                            (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0) ;6
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0)

                                            (note 59 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 57 (note-duration 4) (convert-from-instrument 'Piano) 0)
                                            (note 59 (note-duration 1) (convert-from-instrument 'Piano) 0) ;7
                                            (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)

                                            (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 57 (note-duration 2) (convert-from-instrument 'Piano) 0)
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0) ;8
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 55 (note-duration 2) (convert-from-instrument 'Piano) 0)

                                            (note 55 (note-duration 3) (convert-from-instrument 'Piano) 0)
                                            (note 59 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0) ;9
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)

                                            (note 64 (note-duration 2) (convert-from-instrument 'Piano) 0)
                                            (note 62 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 59 (note-duration 3) (convert-from-instrument 'Piano) 0) ; 10
                                            (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)

                                            (note 59 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 57 (note-duration 2) (convert-from-instrument 'Piano) 0)
                                            (pause (note-duration 1) 0)
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0) ;11
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)

                                            (note 64 (note-duration 2) (convert-from-instrument 'Piano) 0)
                                            (note 62 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 59 (note-duration 1) (convert-from-instrument 'Piano) 0) ;12
                                            (note 59 (note-duration 2) (convert-from-instrument 'Piano) 0)
                                            (note 57 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 55 (note-duration 1) (convert-from-instrument 'Piano) 0)
                                            (note 55 (note-duration 3) (convert-from-instrument 'Piano) 0)
                                            ))

(define BassClef2 (SequentialMusicElement BassClef1
                                          BassClef-C   ;5
                                          BassClef-A   ;6
                                          BassClef-B   ;7
                                          BassClef-G   ;8
                                          BassClef-C   ;9
                                          BassClef-A   ;10
                                          BassClef-B   ;11
                                          BassClef-G)) ;12


(define lovescenario (SequentialMusicElement (ParallelMusicElement TrebleClef1 BassClef1) (ParallelMusicElement TrebleClef2 BassClef2)))



;For testing
;(define tnote (note 90 15 (convert-from-instrument 'Flute) 0))
;(define tnote2 (note 60 (note-duration 1) (convert-from-instrument 'Piano) 0))
;(define tpause (pause 500 0))
;(define tSME (SequentialMusicElement tnote2 tpause))
;(define tPME (ParallelMusicElement tnote2 tnote2 tSME))


;(define longtest1 (SequentialMusicElement tnote2 tpause tnote2 tpause))
;(define longtest (SequentialMusicElement longtest1 longtest1))

;(define li (SequentialMusicElement longtest longtest longtest));(transpose-element longtest 10) (transpose-element longtest 20)))
(define lt (set-start-time-seq lovescenario 1000))

(define song (convert-to-note-abs-time-with-duration-recursive lt))
(transform-to-midi-file-and-write-to-file! song "test.midi")