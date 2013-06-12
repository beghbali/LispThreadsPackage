;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Program: The producer-consumer problem(also known as the bounded-buffer
;;;          problem. This implementation uses the the accompanying thread
;;;          package modules. 
;;;
;;; Copyright (C) 2002  Bashir Eghbali
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define queue ())
(define queue-size 4)
(define capacity 0)
(define identity -1)
(define empty (box (sem-init 2)))
(define full (box (sem-init 0)))

(define (queue-get)
  (define item 0)
  (if(null? queue) 
	(begin (write-string "ERROR: empty queue")(newline)'())
	(begin (set! item (car queue))
	(set! queue (cdr queue))
	(set! capacity (- capacity 1))
	(write-string "Consumer#")(write identity)(write-string " got ")(write item)
	(newline)
	item)))

(define (queue-put num)
  (if(equal? capacity queue-size)
	(begin (write-string "ERROR: full queue")(newline))
	(begin (set! queue (append queue (list num)))
	(set! capacity (+ capacity 1))
	(write-string "Producer#")(write identity)(write-string " put ")
	(write num)(newline))))

(define (producer ident numin)
	(let loop ((num numin))
		(if(not(<= num 0)) 
		(begin
			(down-sem empty)
			(set! identity ident)
			(queue-put num)
			(up-sem full)
			(loop (- num 1)))))
		(exit))

(define (consumer ident numin)
  (let loop ((num numin))
	(if(not(<= num 0))
	(begin
		(down-sem full)
		(set! identity ident)
		(queue-get)
		(up-sem empty)
		(loop (- num 1)))))
	(exit))

;;; name driver
;;; params: none
;;; description: entry point to the producer-consumer problem. creats
;;;              and runs two threads, the producer, and the consumer
;;;              in parallel. Their interaction is printed to the 
;;;              command line by latter functions. 
	
(define (driver)
  (define thread1 (thread-create (lambda () (producer 1 10)))) 
  (define thread2 (thread-create (lambda () (consumer 1 10))))
  (enter-thread thread1)
  (enter-thread thread2)
  (yield))
