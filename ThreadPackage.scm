;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Program: Simple threads package for multitasking in MIT scheme
;;;          Simple FIFO scheduling is provided as well as means of explicit
;;;          scheduling. General semaphores are provided for synchronization. 
;;;          They can be adopted to implement more specific synchronization
;;;          Mechanisms such as Mutexes and Binary Semaphores. 
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

(define DEBUGGING 0)

(define MAX_NUM_THREADS 512)
(define next_thread_id -1)
(define current_thread '())
(define-structure (thread
		   (constructor thread-init (id entry_point #!optional arg)))
  (id 0 read-only #t)
  (entry_point)
  (arg '()))
(define-structure (semaphore
		   (constructor sem-init (#!optional value)))
  (value 1)
  (waitlist ()))

#|-------------------------------utilities-----------------------------------|#
	
(define box (lambda (x) (list x)))
(define set-box! (lambda (b v) (set-car! b v)))
(define unbox (lambda (x) (car x)))
	
#|--------------------------debugging modules-------------------------------|#
;;; name: dump-thread
;;; params: thread, the thread to dump its state out
;;; description: if the thread is in the ready queue then prints out its id
;;;              otherwise an error message is printed

(define (dump-thread thread)
  (if (pair? (unbox readyQ))
      (if (memq thread (unbox readyQ)) 
	  (write (thread-id thread)) 
	(begin 
	 (write-string "unknown thread: ") 
	 (write (thread-id thread)) (newline)))
    (begin (write-string "empty queue" ) (newline))))

(define (dump-id thread)
  (write (thread-id thread)))
	
(define (dump-readyQ)
  (map dump-id (unbox readyQ)))
	

#|--------------------------mutual exclusion modules-------------------------|#
(define readyQ (box ()))
(define blockedQ (box ()))

;;; name: save-context
;;; params: curr_context
;;; description: saves the current context(continuation) and returns

(define (save-context curr_context)
  (if(not(null? current_thread))
     (set-thread-entry_point! current_thread curr_context)))

;;; name: enqueue
;;; params: list_, the list to enqueue thread on
;;;         thread, the object to enqueue
;;; description: enqueues thread onto the end of list_

(define (enqueue list_ thread)
  (set-box! list_ (append (unbox list_) (list thread))))

;;; name: dequeue
;;; params: list_, the list to dequeue
;;; description: dequeues the front element of list_ and returns it

(define (dequeue list_)
  (if (null? (unbox list_))
      (begin (write-string "ERROR:dequeing empty queue!") '())
    (begin (let ((head (car (unbox list_))))
	     (set-box! list_ (cdr (unbox list_)))
	     head))))
	
;;; name: set-blocked
;;; params: thread, the thread to move to blocked queue
;;; description: moves thread from readyQ to blockedQ

(define (set-blocked thread)
  (delete! thread (unbox readyQ))
  (enqueue blockedQ thread))

;;; name: set-unblocked
;;; params: thread, the thread to move to ready queue
;;; description: moves thread from blockedQ to readyQ

(define (set-unblocked thread)
  (delete! thread (unbox blockedQ))
  (if (not(member thread (unbox readyQ))) 
    (enqueue readyQ thread)))
	
;;; name: sleep-thread
;;; params: none
;;; description: puts the current running thread to sleep, saves its context 
;;;              and calls scheduler to schedule another thread to run

(define (sleep-thread thread)
  (set-blocked thread) 
  (schedule-thread))

;;; name: wakeup-thread
;;; params: thread, the thread to wakeup
;;; description: puts thread onto the readyQ to be considered for execution by
;;;              the scheduler

(define (wakeup-thread thread)	
  (set-unblocked thread))

;;; name: down-sem
;;; params: sem, the semaphore to do an down-sem operation on
;;; description: decrements the value of sem and if it is less than 0 it puts 
;;; the current running thread to sleep

(define (down-sem sem)
(set-semaphore-value! (unbox sem) (- (semaphore-value (unbox sem)) 1))
	(let ((semval (semaphore-value (unbox sem))))
		(if (< semval 0)
			(begin 
			(enqueue (box(semaphore-waitlist (unbox sem))) (get-current-thread))
			(sleep-thread (get-current-thread))))))

;;; name: up-sem
;;; params: sem, the semaphore to do an up-sem operation on
;;; description: decrements the value of sem and if it is less than 0 it puts 
;;; the current running thread to sleep

(define (up-sem sem)
 (let ((semval (semaphore-value (unbox sem))))
	(set-semaphore-value! (unbox sem) (+ semval 1))
  (if (not (null? (semaphore-waitlist (unbox sem))))
	(wakeup-thread (dequeue (box (semaphore-waitlist (unbox sem))))))))
						
#|------------------------thread management modules--------------------------|#
		
;;; name: thread-create
;;; params: entry_point, the entry point to the thread(a procedure pointer)
;;;         arg, the argument to the entry point procedure(only one allowed)
;;; description: if the maximum number of threads have not been created, it
;;;              creates a new thread, initializes it and returns it. 
;;; returns: a new thread if MAX_NUM_THREADS is not reached, null otherwise
               
(define (thread-create entry_point #!optional arg) 
  (let ((new_thread 0))
    (if (> next_thread_id MAX_NUM_THREADS)
	'()
      (begin (set! next_thread_id (+ next_thread_id 1))
	     (if (default-object? arg)
		 (set! new_thread (thread-init next_thread_id entry_point '()))
	       (set! new_thread (thread-init next_thread_id entry_point arg)))
	     (if (> DEBUGGING 0) 
		 (begin (write-string "Created Thread#")(dump-id new_thread)(newline)))
	     new_thread))))
	
;;; name: get-current-thread
;;; params: none
;;; description: returns the thread structure for the current thread(current 
;;;              context)

(define (get-current-thread) current_thread)

;;; name: threads-package-init
;;; params: none
;;; description: initializes the threads package, setting the current running 
;;;              thread and returns immediately, now more threads can be forked
;;;              from this parent thread. 

(define (threads-package-init)
  (set! next_thread_id -1)
  (set! current_thread (thread-create 0 0))
  (enter-thread current_thread))
		 	
;;; name: exec-thread
;;; params: thread, the thread to execute
;;; description: runs/executes the specified thread. Namely resumes its saved 
;;;              continuation

(define (exec-thread thread)
  (set! current_thread thread)
  (let ((entry_point (thread-entry_point thread)))
    (if (null? (thread-arg thread))
	(if (continuation? entry_point)
	    (entry_point #t)
	    (entry_point))
	(entry_point (thread-arg thread)))))

;;; name: display-deadlock
;;; params: none
;;; description: called by the scheduler when a deadlock happens to report the 
;;;              deadlock to the console and the threads that are still in the
;;;              system

(define (display-deadlock)
  (write-string "Deadlock!, following threads are still in the system: ") 
  (map (lambda (thread) (write (thread-id thread)) (write-string " ")) (unbox blockedQ))
  (newline))

;;; name: thread-yield
;;; params: target_thread, the thread to yield the current running thread to
;;; description: saves the continution of the the current thread and calls the
;;;              scheduler to schedule another thread

(define (thread-yield target_thread)
  (if (> DEBUGGING 0)
      (begin (write-string "yield to #")(write (thread-id target_thread))(newline)))
  (call-with-current-continuation 
   (lambda (rest-of-computation) 
     (save-context rest-of-computation)
     (exec-thread target_thread))))

;;; name: reschedule-thread
;;; params: none
;;; description: puts thread in front of the readyQ on the back of the readyQ

(define (reschedule-thread)
  (enqueue readyQ (dequeue readyQ)))

;;; name: schedule-thread
;;; params: none
;;; description: schedules the next thread to be run and runs it. The 
;;;              scheduling is FIFO. 

(define (schedule-thread)
  (if (> DEBUGGING 0)
      (begin (write-string "Scheduler's Queue: ")(dump-readyQ)(newline)))
  (if (null? (unbox readyQ))
       (begin 
	 (if (not (null? (unbox blockedQ))) 
	     (display-deadlock))
	 '())
       (begin (reschedule-thread)
	      (thread-yield (car (unbox readyQ))))))


;;; name: enter-thread
;;; params: new_thread, the thread to enter into the system
;;; description: puts new_thread on the readyQ, to be eligable for scheduling. 
;;;              Does not run new_thread however
	
(define (enter-thread new_thread)
  (if (member new_thread (unbox readyQ))
      '()
    (enqueue readyQ new_thread)))

;;; name: exit-thread
;;; params: thread, the thread to exit
;;; description: removes thread from the system, if thread is the current 
;;;              running thread it calls the scheduler to schedule another 
;;;              thread to run

(define (exit-thread thread)
  (set-box! readyQ (delete thread (unbox readyQ)))
  (set-box! blockedQ (delete thread (unbox blockedQ)))
  (if (equal? (thread-id thread) (thread-id (get-current-thread)))
      (schedule-thread)))

(define (yield) (schedule-thread))
(define (exit) (exit-thread current_thread))

