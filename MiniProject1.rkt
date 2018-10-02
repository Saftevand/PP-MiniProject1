#lang racket

;Author:  Mathias Lundhede Hansen
;         Mlha15@student.aau.dk
;         20154238

(require "music-base.rkt")


;Predicates
(define (pitch? pitchvalue)
  (and (>= pitchvalue 0) (<= pitchvalue 127)))

(define (duration? x)
  (and (real? x) (>= x 0)))

(define (instrument? type)
  (if(real? type) (and(>= type 0)(<= type 8)) #f))

(define (note? x)
  (eqv? 'Note (send 'get-type x)))

(define (pause? x)
  (eqv? 'Pause (send 'get-type x)))

(define (sequentialMusicElement? x)
  (eqv? 'SequentialMusicElement (send 'get-type x)))

(define (parallelMusicElement? x)
  (eqv? 'ParallelMusicElement (send 'get-type x)))

(define (musicElement? x)
  (if(procedure? x)
    (cond((note? x) #t)
         ((pause? x) #t)
         ((sequentialMusicElement? x) #t)
         ((parallelMusicElement? x) #t)
         (else #f))
    #f))



;Help functions
(define (send message obj . par)
  (let ((method (obj message)))
    (apply method par)))

(define (can-create-sequential-or-parallel-MusicElement? x)
  (if(null? x)
     #f
     (can-create-sequential-or-parallel-MusicElement-helper x)))

(define (can-create-sequential-or-parallel-MusicElement-helper . x)
  (if(null? x)
     (if(musicElement? (car x))
        (can-create-sequential-or-parallel-MusicElement-helper (cdr x))
        #f)
     #t))

(define (convert-to-note-abs-time-with-duration seq-mus-elem)
  (convert-helper seq-mus-elem '()))           ;TODO test at list-builder virker + Lav en metode som omdanner listen fra list-builder til note-abs-time-with-duration formen + Ændre scale transform til music element

(define (list-builder lst result-lst)
  (if(null? lst)
     '()
     (cond((eqv? 'note (send 'get-type (car lst))) (cons result-lst (cons (car lst) (list-builder (cdr lst)))))
          ((eqv? 'pause (send 'get-type (car lst))) (cons result-lst (cons (car lst) (list-builder (cdr lst)))))
          ((eqv? 'SequentialMusicElement (send 'get-type (car lst))) (cons result-lst (cons (list-builder (car lst)) (list-builder (cdr lst)))))
          ((eqv? 'ParallelMusicElement (send 'get-type (car lst))) (cons result-lst (cons (list-builder (car lst)) (list-builder (cdr lst)))))
          (else error "Invalid type!"))))

;Constructor functions
(define (note pitch duration instrument)
  (if(and (pitch? pitch) (duration? duration) (instrument? instrument))
     (let ((pitch-value pitch)
        (duration-value duration)
        (instrument-type instrument))    

       (define (get-pitch) pitch-value)
       (define (get-duration) duration-value)
       (define (get-instrument) instrument-type)
       (define (scale factor) (note pitch-value (* factor duration-value) instrument-type))
       (define (transpose amount) (note (+ amount pitch-value) duration-value instrument-type))
       (define (re-instrument new-instrument) (note pitch-value duration-value new-instrument))
       (define (get-type) 'Note)
       (define (self message)
         (cond ((eqv? message 'get-pitch) get-pitch)
               ((eqv? message 'get-duration) get-duration)
               ((eqv? message 'get-instrument) get-instrument)
               ((eqv? message 'scale) scale)
               ((eqv? message 'transpose) transpose)
               ((eqv? message 're-instrument) re-instrument)
               ((eqv? message 'get-type) get-type)))
       self)
     (error "Invalid note!")))
    
(define (pause duration)
  (if(duration? duration)
  (let ((duration-value duration))

  (define (get-duration) duration-value)
  (define (get-type) 'Pause)
  (define (self message)
    (cond ((eqv? message 'get-duration) get-duration)
          ((eqv? message 'get-type) get-type)))
  self)
  (error "Invalid pause!")))

(define (SequentialMusicElement . elements)
  (if(can-create-sequential-or-parallel-MusicElement? elements)
     (let ((elements-list elements))

       (define (get-elements) elements-list)
       (define (get-type) 'SequentialMusicElement)
       (define (self message)
         (cond ((eqv? message 'get-elements) get-elements)
               ((eqv? message 'get-type) get-type)))
       self)
     (error "Invalid SequentialMusicElement")))

(define (ParallelMusicElement . elements)
  (if(can-create-sequential-or-parallel-MusicElement? elements)
     (let ((elements-list elements))

       (define (get-elements) elements-list)
       (define (get-type) 'ParallelMusicElement)
       (define (self message)
         (cond ((eqv? message 'get-elements) get-elements)
               ((eqv? message 'get-type) get-type)))
       self)
     (error "Invalid ParallelMusicElement")))




;For testing
(define tnote (note 1 2 3))
(define tpause (pause 22))
(define tSME (SequentialMusicElement '(tnote tpause)))
(define tPME (ParallelMusicElement '(tnote tpause tSME)))
