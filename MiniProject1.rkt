#lang racket

;Author:  Mathias Lundhede Hansen
;         Mlha15@student.aau.dk
;         20154238

(require "music-base.rkt")

;---------------------------------------------;
;                                             ;
;      ---BEFORE RUNNING THE PROGRAM---       ;
;  The program will automatically generate    ;
;  a midi file of the example song.           ;
;  If this is not wanted, then simply         ;
;  commen out the last line of this document  ;
;                                             ;
;---------------------------------------------;


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

;Given a music element, it will create note-abs-time-with-duration, by converting
;the music element(and its sup-elements) into the corect format of note-abs-time-with-duration
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

;Converts a note to note-abs-time-with-duration 
(define (convert-to-note-abs-time-with-duration-note x)
  (note-abs-time-with-duration (send 'get-start-time x) (send 'get-instrument x) (send 'get-pitch x) velocity (send 'get-duration x)))

;Converts a pause to a note-abs-time-with-duration
(define (convert-to-note-abs-time-with-duration-pause x)
  (note-abs-time-with-duration (send 'get-start-time x) 1 1 0 (send 'get-duration x)))

;Given a sequential music element, it will find the sum of its elements
;duration, which will be the duration of the sequential music element
(define (calc-duration-seq x)
  (calc-duration-seq-helper x 0))

;Helper function for calc-duration-seq, which finds the total sum of all the
;durations of the elements in a sequential music element
(define (calc-duration-seq-helper x res)
  (if(null? x)
     res
     (calc-duration-seq-helper (cdr x) (+ (send 'get-duration (car x)) res))))

;Given a parallel music element, it will return the duration of the element
;with the longest duration, which is the same as the parallel music elements duration
(define (calc-duration-par x)
  (calc-duration-par-helper x 0))

;Helper function for calc-duration-par, which finds the element with the longest duration
;in a parallel music element, throught the use of longest-duration
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

;Given an parallel music element, it will set the start time for the element
;as well as the elements it contains, so that all its elements will start at the same time
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

;Given an sequential music element, it will set the start time for the element
;as well as the elements it contains, so that the elements will start one after the other
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


;Given the name of an instrument, it will return the instruments id
(define (convert-from-instrument x) ; "" -> '
  (cond((eqv? x 'Piano) 1)
       ((eqv? x 'Organ) 2)
       ((eqv? x 'Guitar) 3)
       ((eqv? x 'Violin) 4)
       ((eqv? x 'Flute) 5)
       ((eqv? x 'Trumpet) 6)
       ((eqv? x 'Helicopter) 7)
       ((eqv? x 'Telephone)  8)
       (else (error "Unknown instrument"))))
        


;The music elements "created" as ojects
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

;Used to the duriation of a note, so that it is easier to create notes from a piano sheet
;However it should only be used for songs with the same BPM as the one created below
(define (note-duration x)
  (* x 450))

;keeping the velocity constant
(define velocity 80) 


;The song
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

(define BassClef-D(SequentialMusicElement
  (note 50 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 50 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 50 (note-duration 2) (convert-from-instrument 'Piano) 0)))
(define BassClef-C(SequentialMusicElement
  (note 48 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 48 (note-duration 3) (convert-from-instrument 'Piano) 0) 
  (note 48 (note-duration 2) (convert-from-instrument 'Piano) 0)))
(define BassClef-B(SequentialMusicElement
  (note 47 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 47 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 47 (note-duration 2) (convert-from-instrument 'Piano) 0)))
(define BassClef-E(SequentialMusicElement
  (note 52 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 52 (note-duration 3) (convert-from-instrument 'Piano) 0)
  (note 52 (note-duration 2) (convert-from-instrument 'Piano) 0)))

(define BassClef1 (SequentialMusicElement (pause (note-duration 4) 0) ;1
                                          BassClef-C                  ;2
                                          BassClef-D                  ;3
                                          BassClef-B))                ;4

(define BassClef2 (SequentialMusicElement BassClef1
                                          BassClef-E                  ;5
                                          BassClef-C                  ;6
                                          BassClef-D                  ;7
                                          BassClef-B                  ;8
                                          BassClef-E                  ;9
                                          BassClef-C                  ;10
                                          BassClef-D                  ;11
                                          BassClef-B))                ;12

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

(define lovescenario (SequentialMusicElement (ParallelMusicElement TrebleClef1 BassClef1) (ParallelMusicElement TrebleClef2 BassClef2)))
;I shifted the start of the song with ~a second to give the audio player a change to load
;as i experienced windows media player to sometimes skip the first note or two as it
;was loading the song
(define lovescenario-with-correct-start (set-start-time-seq lovescenario 1000))
(define song (convert-to-note-abs-time-with-duration-recursive lovescenario-with-correct-start))
;If it is not wanted to have the program output a media file, then comment out the following line
(transform-to-midi-file-and-write-to-file! song "Mlha15TestSong.midi")