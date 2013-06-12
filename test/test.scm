(define DEBUGGING 1)

(define readyQ ())
(define blockedQ ())
(define MAX_NUM_THREADS 512)
(define next_thread_id 0)
(define current_thread 0)
(define-structure (thread
	(constructor thread-init (id entry_point #!optional arg)))
	(id id)
	(entry_point entry_point)
	(arg 0))
(define-structure (semaphore
	(constructor sem-init ()))
	(value 0)
	(waitlist ()))
	
#|--------------------------start of debugging modules-----------------------|#
;;; name: dump-thread
;;; params: thread, the thread to dump its state out
;;; description: if the thread is in the ready queue then prints out its id
;;;              otherwise an error message is printed

(define (dump-thread thread)
	(if (pair? readyQ)
		(if (memq thread readyQ) 
			(write (thread-id thread)) 
			(begin (write-string "unknown thread: ") (write (thread-id thread)) (newline)))
	(begin (write-string "empty queue" ) (newline) (- 0 1))))

(define (dump-id thread)
	(write (thread-id thread)))
	
(define (dump-readyQ)
	(map dump-id readyQ))
	
#|--------------------------end of debugging modules--------------------------|#
	
#|--------------------------mutual exclusion modules--------------------------|#
;;; name: enqueue
;;; params: list, the list to enqueue thread on
;;;         thread, the object to enqueue
;;; description: enqueues thread onto the end of list

(define (enqueue list thread)
	(set! list (reverse (cons thread (reverse list)))))

;;; name: dequeue
;;; params: list, the list to dequeue
;;; description: dequeues the front element of list and returns it

(define (dequeue list)
	(define head (cdr list))
	(set! list (rest list))
	(return head))
	
;;; name: set-blocked
;;; params: thread, the thread to move to blocked queue
;;; description: moves thread from readyQ to blockedQ

(define (set-blocked thread)
	(delete! thread readyQ)
	(enqueue blockedQ thread))

;;; name: set-unblocked
;;; params: thread, the thread to move to ready queue
;;; description: moves thread from blockedQ to readyQ

(define (set-unblocked thread)
	(delete! thread blockedQ)
	(enqueue readyQ thread))
	
;;; name: sleep-thread
;;; params: none
;;; description: puts the current running thread to sleep, saves its context and calls scheduler to 
;;;              schedule another thread to run

(define (sleep-thread thread)
	(set-blocked thread) 
	(call-with-current-continuation (lambda (rest-of-computation) (schedule-thread rest-of-computation))))

;;; name: wakeup-thread
;;; params: thread, the thread to wakeup
;;; description: puts thread onto the readyQ to be considered for execution by the scheduler

(define (wakeup-thread thread)	
	(set-unblocked thread))

;;; name: down-sem
;;; params: sem, the semaphore to do an down-sem operation on
;;; description: decrements the value of sem and if it is less than 0 it puts the current running thread to sleep

(define (down-sem sem)
	(- (semaphore-value sem) 1)
	(if (< (semaphore-value sem) 0)
		(begin (enqueue (semaphore-waitlist sem) get-current-thread) (sleep-thread get-current-thread))))

;;; name: up-sem
;;; params: sem, the semaphore to do an up-sem operation on
;;; description: decrements the value of sem and if it is less than 0 it puts the current running thread to sleep

(define (up-sem sem)
	(+ (semaphore-value sem) 1)
	(wakeup-thread (dequeue (semaphore-waitlist sem))))
						
#|------------------------thread management modules-----------------------------|#
	
;;; name: thread-create
;;; params: entry_point, the entry point to the thread(a procedure pointer)
;;;         arg, the argument to the entry point procedure(only one allowed)
;;; description: if the maximum number of threads have not been created, it
;;;              creates a new thread, initializes it and returns it. 
;;; returns: a new thread if MAX_NUM_THREADS is not reached, null otherwise
               
(define (thread-create entry_point arg)
	(define new_thread 0)
	(if (> next_thread_id MAX_NUM_THREADS)
		(null)
		(begin (set! next_thread_id (+ next_thread_id 1))
		(set! new_thread (thread-init next_thread_id entry_point arg))
		(if (> DEBUGGING 0) (dump-thread new_thread))
		new_thread)))
		
;;; name: get-current-thread
;;; params: none
;;; description: returns the thread structure for the current thread(current context)

(define (get-current-thread) current_thread)

;;; name: enter-thread
;;; params: new_thread, the thread to enter into the system
;;; description: puts new_thread on the readyQ, to be eligable for scheduling. Does not run
;;;              new_thread however
	
(define (enter-thread new_thread)
	(enqueue readyQ new_thread))

;;; name: exit-thread
;;; params: thread, the thread to exit
;;; description: removes thread from the system, if thread is the current running thread it 
;;;              calls the scheduler to schedule another thread to run

(define (exit-thread thread)
	(delete! thread readyQ)
	(delete! thread blockedQ)
	(if (equal? thread-id get-current-thread-id)
		(schedule-thread null)))
		 	
;;; name: exec-thread
;;; params: thread, the thread to execute
;;; description: runs/executes the specified thread. Namely resumes its saved continuation

(define (exec-thread thread)
	(set! current_thread thread)
	(thread-entry_point arg))

;;; name: schedule-thread
;;; params: entry_point, the entry point into the thread, it may be the continuation of the
;;;         execution of the thread or just the start procedure
;;;         target_thread, if this is an explicit yield then the thread to yield to is specified
;;;         otherwise the scheduler will pick the next ready thread
;;; description: schedules the next thread to be run and runs it. The scheduling is FIFO. 
	
(define (schedule-thread entry_point target_thread)
	(define cthread (get-current-thread))
	(set! cthread-entry_point entry_point)
	(if (null? target_thread)
		(exec-thread (set-blocked (car readyQ)))
		(exec-thread target_thread)))
		

;;; name: thread-yield
;;; params: target_thread, the thread to yield the current running thread to
;;; description: saves the continution of the the current thread and calls the scheduler to schedule 
;;;              another thread

(define (thread-yield target_thread) 
	(call-with-current-continuation (lambda (rest-of-computation) (schedule-thread rest-of-computation target_thread))))

#|---------------------------------testing modules-----------------------------------|#
;;; name: myinc
;;; params: x, the number to increment
;;; description: increments x by 1-used as part of testing
(define x 0)
(define lock 0)
(set! lock (sem-init))

(define (myinc) 
	(up-sem lock)
	(set! x (+ x 1)))
	(down-sem lock)
	(write-string "thread[")
	(write get-current-thread-id)
	(write-string "]=")
	(write x)
	(newline))