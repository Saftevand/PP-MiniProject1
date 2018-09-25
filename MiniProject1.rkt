#lang racket

;Author Mathias Lundhede Hansen Mlha15@student.aau.dk


(define (pitch pitchvalue)
  (if(and (>= pitchvalue 0) (<= pitchvalue 127)) pitchvalue (error "Invalid pitchvalue!")))

;(define (duration timeunits) (timeunits))

(define (instrument type)
  (cond((eq? type "Piano")      "Piano")
       ((eq? type "Organ")      "Organ")
       ((eq? type "Guitar")     "Guitar")
       ((eq? type "Violin")     "Violin")
       ((eq? type "Flute")      "Flute")
       ((eq? type "Trumpet")    "Trumpet")
       ((eq? type "Helicopter") "Helicopter")
       ((eq? type "Telephone")  "Telephone")
       (else (error "Invalid instrument"))))

(define (note pitch duration instrument) (1))

(define (pause duration) (duration))

(define (MusicElement) (1))

(define (SequentialMusicElement) (1))

(define (ParallelMusicElement) (1))



