;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Frank Tamborello
;;; Copyright   : (c) 2013-5 Frank Tamborello
;;; Availability: Public Domain
;;; Address     : United States Naval Research Laboratory
;;;		: 4555 Overlook Ave SW
;;;		: Code 5515
;;;		: Washington, DC 20375
;;;		: franklin.tamborello.ctr@nrl.navy.mil
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename     : UNRAVEL.lisp
;;; Revision     : 56
;;; 
;;; Description : A process model of systematic procedural error in a continuous 
;;; task, Altmann & Trafton's UNRAVEL task. Based on Tamborello & Trafton's
;;; (2013a & b) model of postcompletion, anticipation, and perseveration error
;;; in a routine procedural non-continuous task. Developed with ACT-R6 (r1227),
;;; boost-chunks module 10 (may be made available upon request), spacing-effect
;;; module (ACT-R extras), &amp; new-threads 2.0a (ACT-R extras).
;;; 
;;; Bugs: none known
;;;
;;; To do: 
;;;
;;; ----- History -----
;;; See prior revisions for earlier history.
;;;
;;; 2014.07.30 fpt 45
;;; Run-model calls print-model-run whether one or all conditions are run.
;;;
;;; 2014.07.31 fpt 46
;;; Moved the trial's time delay from the procedure step retrieval request
;;; production to the two response productions, where it really makes more
;;; sense for them to be, particularly because I'm preparing to use a retrieval
;;; fail try again loop at that point to explain anctipations' longer resumption
;;;
;;; 2014.07.31 fpt 47
;;; Model longer resumption lag of anticipation errors: sometimes (how
;;; often?) the retrieved episodic context gets loaded into the goal buffer chunk
;;; rather than the imaginal buffer chunk. Does this by scheduling an event after
;;; the imaginal module to clear the imaginal buffer chunk step slot and instead
;;; load the context from the retrieved episodic chunk into the step slot of the
;;; goal buffer chunk.
;;;
;;; 2014.08.14 fpt 48
;;; Model longer resumption lag of anticipation errors: 
;;; A quick & dirty version saps activation away by 
;;; having a couple of dummy chunks occupying slots of the imaginal buffer chunk.
;;;
;;; 2014.08.15 fpt 49
;;; 1. Ditch the random rehearsal failure mechanism. Instead use a high :RT
;;;
;;; 2014.08.15 fpt 50
;;; 1. Ditch dummy chunks
;;;
;;; 2. Use a high :rt, like 3, to make the model occassionally miss rehearsals, 
;;; rather than the model-busy-param.
;;;
;;; 2015.04.12 fpt 51
;;; Can the model work without the episodic module?
;;;
;;; 2015.04.17 fpt 52
;;; 1. Forked from r50 as the answer to the question posed by r51 seems to be no:
;;; The model's too unlikely to successfully rehearse the correct ps chunk.
;;; The model can't seem to build up enough difference in BL activation between the
;;; current, recent, and old chunks.
;;; 2. Turn on production compilation
;;;
;;; 2015.04.23 fpt 53
;;; 1. Trying to fix resumption behavior
;;; 2. Run-model now takes a keyword parameter, worker, telling it whether to 
;;; return *resp-session-data*.
;;; 3. Workers break when more than one of them tries to write to the same file 
;;; at the same time. If run-model's worker keyword paremeter it set to t then 
;;; it doesn't write to a file.
;;;
;;; 2015.04.28 fpt 54
;;; Squashed bug: Somewhere along the way, rehearsal broke such that resumption  
;;; no longer matches the data.
;;; Resumption perseverations seem to be sort of okay now.
;;;
;;; 2015.05.14 fpt 55
;;; Make concurrent-friendly: Run-model is simplified such that it'll run only
;;; one condition each time it's called, returning a list of the model run data.
;;; It assumes it's being run as a worker and it doesn't collect the timing data.
;;;
;;; 2015.05.22 fpt 56
;;; Replaced a circular list I used for the exp-wind's steps slot since it seemed
;;; problematic for sending over tcp socket streams. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load ACT-R
(unless
    (member ':act-r-6.0 *features*)
  (load "/Users/frank/Documents/NRL/Error/actr6/load-act-r-6.lisp"))

;; Load the threaded cognition module
(unless
;; Is there something specific to new-threads I can use to differeniate it from 
;; ACT-R's standard goal module? For assume the thread module is loaded if spacing-
;; effect is also loaded.
    (get-module spacing-effect)
  (load "/Users/frank/Documents/NRL/Error/actr6/extras/threads/new-threads.lisp"))

;; Load the spacing effect module
(unless
    (get-module spacing-effect)
  (load "/Users/frank/Documents/NRL/Error/actr6/extras/spacing-effect/spacing-effect.lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Task Environment: The Unravel Task
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :virtual-experiment-window)
(require :seq-math)


(defvar *resp-session-data* nil)
(defvar *latency-data* nil)

(setf *print-circle* t)

(defclass exp-window (virtual-experiment-window)
;; Later: two kinds, no-iti and 50% chance 1s iti
  ((iti :accessor iti :initarg :iti :initform 0)
   (m :accessor m :initarg :m :initform nil)
   (steps :accessor steps :initarg :steps :initform (list 'u 'n 'r 'a 'v 'e 'l))
   (widgets :accessor widgets :initarg :widgets :initform nil)
;; stimulus attributes of the previous trial, 
;; a list of symbols in order as they appear in this class definition
   (previous :accessor previous :initarg :previous :initform nil)
   (last-unravel :accessor last-unravel :initarg :last-unravel :initform nil)
   (letter :accessor letter :initarg :letter :initform '(a b u v))
   (number :accessor number :initarg :number :initform '(1 2 8 9))
   (font :accessor font :initarg :font :initform '(lu li ru ri))
   (color :accessor color :initarg :color :initform '(lr ly rr ry))
   (height :accessor height :initarg :height :initform '(la lb ra rb))
   (print-to-tl :accessor print-to-tl :initarg :print-to-tl :initform t)
   (worker :accessor worker :initarg :worker :initform nil)
   (offset-table 
    :accessor offset-table :initarg :offset-table 
    :initform (make-hash-table :size 7))
   (data-int :accessor data-int :initarg :data-int :initform (list 0 0 0 0 0 0 0))
   (data-base :accessor data-base :initarg :data-base :initform (list 0 0 0 0 0 0 0))
   (r-data :accessor r-data :initarg :r-data :initform nil)
   (l-data :accessor l-data :initarg :l-data :initform (make-instance 'latency-data))
   (retrieval-tries :accessor retrieval-tries :initarg :retrieval-tries :initform 0))
  (:default-initargs
      :nblocks 4))


(defclass utrial (trial)
  ((xcond :accessor xcond :initarg :xcond :initform nil)
   (trl-num :accessor trl-num :initarg :trl-num :initform 0)
   (response :accessor response :initarg :response :initform nil)
   (latency :accessor latency :initarg :latency :initform nil)
   (int-nmbr :accessor int-nmbr :initarg :int-nmbr :initform nil)))


(defclass unravel-trial (utrial)
  ((interrupted :accessor interrupted :initarg :interrupted :initform nil)
   (lr-order :accessor lr-order :initarg :lr-order :initform 
             (if (flip) 'letter-number 'number-letter))
   (letter :accessor letter :initarg :letter :initform nil)
   (number :accessor number :initarg :number :initform nil)
   (font :accessor font :initarg :font :initform nil)
   (color :accessor color :initarg :color :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (offset :accessor offset :initarg :offset :initform nil))
  (:default-initargs
      :trial-kind 'u))

(defclass interruption-trial (utrial)
  ((stimulus 
    :accessor stimulus :initarg :stimulus 
    :initform 
    (randomize-list
     '(#\u #\n #\r #\a #\v #\e #\l #\i #\f #\y #\b #\c #\o #\m)))
   (busy-param :accessor busy-param :initarg :busy-param
               :initform (act-r-random 100)))
  (:default-initargs
      :trial-kind 'interruption))

(defclass latency-data ()
  ((bsln-correct :accessor bsln-correct :initarg :bsln-correct :initform nil)
   (bsln-err :accessor bsln-err :initarg :bsln-correct :initform nil)
   (rsmp-correct :accessor rsmp-correct :initarg :rsmp-correct :initform nil)
   (rsmp-err :accessor rsmp-err :initarg :rsmp-err :initform nil)
   (int :accessor int :initarg :int :initform nil)))



(defun build-basic-trials (wind)
;; loop
;; loop for 10 interruption times
;; make a random number of u-trials: 
;;  "The number of trials for a given run was computed by adding three 
;;  to a sample from an exponential distribution with mean three."
;; make cond number of i-trials
    (loop
    with stim-1 = (loop
                for i from 1 to (+ 3 (act-r-random 7))
                collect (make-instance 'unravel-trial) into slst
                finally (return slst))

    for i from 1 to 10
    collect (loop
              for i from 1 to (xcond wind)
              collect 
              (make-instance 'interruption-trial :int-nmbr i)
              into slst
              finally (return slst))
    into stim-2
    collect (loop
              for i from 1 to (+ 3 (act-r-random 7))
              collect (make-instance 'unravel-trial) into slst
              finally (return slst))
    into stim-2
    finally (return (append stim-1 (flatten stim-2)))))


(defun do-stimgen (wind)
  (loop
    for i from 1 to (nblocks wind)
    collect
     (let ((stim-lst (build-basic-trials wind)))
       (make-instance 'trial-block
         :size (length stim-lst)
         :trial-lst stim-lst))
    into blk-lst
    finally (return blk-lst)))

(defmethod initialize-instance :after ((wind exp-window) &key)
  (setf (base-path wind) "/Users/frank/Documents/NRL/Error/UNRAVEL/UNRAVEL-model-data/"
        (write-type wind) :SS)
  (setf 
   (block-lst wind) (do-stimgen wind))
  (loop
    for i from 1 to 7
    do (setf (gethash (nth (1- i) '(u n r a v e l)) (offset-table wind)) i)))



(defmethod make-data-path ((wind exp-window) type)
  (when (base-path wind)
    (make-pathname :directory (base-path wind)
                   :name
                   (format nil "unravel-data-~a" (m wind))
                   :type type
                   :version :newest)))

(defun infer-response-type (rsp)
  (case rsp
    (#\u 'u)
    (#\i 'u)
    (#\n 'n)
    (#\f 'n)
    (#\r 'r)
    (#\y 'r)
    (#\a 'a)
    (#\b 'a)
    (#\v 'v)
    (#\c 'v)
    (#\e 'e)
    (#\o 'e)
    (#\l 'l)
    (#\m 'l)
    (t (format t "setup-trial: response processing error"))))

(defun find-next-pseudocircular (itm lst &optional (tst #'eql))
  "Takes an item, a list, and an optional test function which defaults to eql. 
  Returns the next item from the list of steps of the exp-wind. Known limitation:
  does not terminate if the item is not in the list."
  (if (apply tst itm `(,(car lst)))
      (aif (car (cdr lst))
           it
           (car (steps (current-device))))
      (find-next-pseudocircular itm (cdr lst) tst)))


(defmethod setup-trial :after ((wind exp-window) (trl unravel-trial))
;; set some blank slots
  (setf (letter trl) (random-item (letter wind))
        (number trl) (random-item (number wind))
        (font trl) (random-item (font wind))
        (color trl) (random-item (color wind))
        (height trl) (random-item (height wind))
        (xcond trl) (xcond wind)
        (start-time trl) (current-time (timer wind)))

  (let ((prv (previous wind))
        (lu (last-unravel wind)))

;; when the current trial is an unravel-trial and the previous trial was
;; an interruption, set this trial's interrupted slot to t
    (when (and (eql (class-name (class-of trl))
                    'unravel-trial)
               (eql (class-name (class-of prv))
                    'interruption-trial))
      (setf (interrupted trl) t))
    
;; when there's a previous unravel-trial,
;; set this unravel-trial's type to be the appropriate follower to the last 
;; unravel-trial's
    (when lu
      (setf 
       (trial-kind trl)
       (find-next-pseudocircular 
        (infer-response-type (response lu))
        (steps wind)))

;; ...and ensure stim attr do not repeat from last trial
      (dolist (attr '(letter number font color height))
        (when (eq (slot-value trl attr) 
                  (slot-value lu attr))
          (setf (slot-value trl attr) 
                (random-not-item 
                 (slot-value trl attr) 
                 (slot-value wind attr))))))

;; draw items for act-r
   (let*
        ((vis-locs
          (define-chunks-fct
              `((isa visual-location
                     screen-x 50
                     screen-y 50
                     kind rectangle
                     color gray
                     height 50
                     width 100)
                (isa visual-location
                      screen-x 50
                      screen-y ,(case (height trl)
                                  (la 0)
                                  (lb 100)
                                  (t 50))
                      kind text
                      color ,(case (color trl)
                                  (lr 'red)
                                  (ly 'yellow)
                                  (t 'white))
                      height 20
                      width 10)
                 (isa visual-location
                      screen-x 150
                      screen-y ,(case (height trl)
                                  (ra 0)
                                  (rb 100)
                                  (t 50))
                      kind text
                      color ,(case (color trl)
                                  (rr 'red)
                                  (ry 'yellow)
                                  (t 'white))
                      height 20
                      width 10))))
         (vis-objs
           (define-chunks-fct
               `((isa visual-object 
                      value rectangle 
                      screen-pos ,(nth 0 vis-locs))
                 (isa text 
                      screen-pos ,(nth 1 vis-locs)
                      value ,(if (eq (lr-order trl) 
                                     'letter-number)
                               (letter trl)
                               (number trl))
                      color ,(chunk-slot-value-fct (nth 1 vis-locs) 'color)
                      status ,(case (font trl)
                                (li 'italic)
                                (lu 'underline)                                
                                (t 'regular)))
                 (isa text 
                      screen-pos ,(nth 2 vis-locs)
                      value ,(if (eq (lr-order trl) 
                                     'letter-number)
                               (number trl)
                               (letter trl))
                      color ,(chunk-slot-value-fct (nth 2 vis-locs) 'color)
                      status ,(case (font trl)
                                (ri 'italic)
                                (ru 'underline)                                
                                (t 'regular)))))))
      (let ((wgts nil))
        (dotimes (i (length vis-locs) (setf (widgets wind) (nreverse wgts)))
          (push
           (make-instance 'widget 
             :vwindow wind 
             :nick-name (nth i '(box left right))
             :vis-loc (nth i vis-locs) 
             :vis-obj (nth i vis-objs))
           wgts))))

;; get act-r started on this trial
  (proc-display :clear t)
  (start-timing (timer wind))
  (model-output "~%Trial kind: ~A~%" (trial-kind trl))))


(defmethod setup-trial :after ((wind exp-window) (trl interruption-trial))
;; set some blank slots
  (setf (xcond trl) (xcond wind)
        (start-time trl) (current-time (timer wind)))
     
;; draw items for act-r
   (let*
        ((vis-locs
          (define-chunks-fct
              `((isa visual-location
                     screen-x 100
                     screen-y 100
                     kind text
                     color black))))
         (vis-objs
           (define-chunks-fct
               `((isa text 
                      value ,(stimulus trl) 
                      screen-pos ,(nth 0 vis-locs)
                      color ,(chunk-slot-value-fct (nth 0 vis-locs) 'color))))))
      (let ((wgts nil))
        (dotimes (i (length vis-locs) (setf (widgets wind) (nreverse wgts)))
          (push
           (make-instance 'widget 
             :vwindow wind 
             :nick-name (nth i '(int-stim))
             :vis-loc (nth i vis-locs) 
             :vis-obj (nth i vis-objs))
           wgts))))

;; get act-r started on this trial
  (proc-display :clear t)
  (start-timing (timer wind))
  (model-output "~%Trial kind: ~A~%" (trial-kind trl))
  (model-output "Interruption Number: ~A~%" (int-nmbr trl))
  (model-output "Time: ~A~%" (mp-time)))


;; Since the experiment has to wait for the model's execution to produce a trial response,
;; finish trials like they're event-based
(defmethod finish-trial ((wind exp-window))
  (let ((blk (nth (cblock wind) (block-lst wind)))
        (trl (current-trial wind)))
    (incf (completed-trials wind))
    (setf (trl-num trl) (completed-trials wind))
    (model-output "Completed trials: ~A~%" (completed-trials wind))
    (incf (current-idx blk))
    (setf (previous wind) trl)
    (when (eql (class-name (class-of trl))
               'unravel-trial)
      (setf (last-unravel wind) trl))
    (if (eql (size blk) (current-idx blk))
;; if it's the final trial of the block, call finish-block
      (finish-block wind blk)
;; else there's more trials in this block, so continue
      (progn
        (setf (current-trial wind) 
              (nth (current-idx blk) (trial-lst blk)))
        (setup-trial wind (current-trial wind))))))



(defmethod write-trial ((trl trial) stream)
  ;; Don't print to stream (a file) if this is running on a worker node
  (unless (worker (current-device))
    ;; both trial kinds
    (format stream "~A	" (snum (current-device)))
    (format stream "~A	" (xcond trl))
    (format stream "~A	" (cblock (current-device)))
    (format stream "~A	" (trl-num trl))
    (format stream "~A	" (trial-kind trl))
    (format stream "~7,1F	" (latency trl))
    (format stream "~7,1F	" (start-time trl))
    
    ;; interruptions
    (if (eql (trial-kind trl) 'interruption)
        (progn
          (terpri stream)
          (push (latency trl) (int (l-data (current-device)))))
        
        ;; unravel trials
        (progn
          (if (not (interrupted trl))
              (if (= 0 (offset trl))
                  (push (latency trl) (bsln-correct (l-data (current-device))))
                  (push (latency trl) (bsln-err (l-data (current-device)))))
              (if (= 0 (offset trl))
                  (push (latency trl) (rsmp-correct (l-data (current-device))))
                  (push (latency trl) (rsmp-err (l-data (current-device))))))
          (format stream "~A	" (response trl))
          (format stream "~A	" (offset trl))
          (format stream "~A	" (interrupted trl))
          (format stream "~A	" (lr-order trl))
          (format stream "~A	" (letter trl))
          (format stream "~A	" (number trl))
          (format stream "~A	" (font trl))
          (format stream "~A	" (color trl))
          (format stream "~A	" (height trl))
          (terpri stream)))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;   ACT-R Device Handler Methods
;;;; ---------------------------------------------------------------------- ;;;;

(defmethod build-vis-locs-for ((device exp-window) vismod)
  (declare (ignore vismod))
  (labels ((wdgt-lst (lst)
             (if (null lst)
               nil
               (cons (vis-loc (car lst)) (wdgt-lst (cdr lst))))))
    (wdgt-lst (widgets device))))

(defmethod vis-loc-to-obj ((device exp-window) vl)
  "Returns the vis-obj of the widget containing the vis-loc."
  (labels ((get-vis-obj (lst)
             (cond 
               ((null lst) nil)
               ((eq (vis-loc (car lst)) vl) (vis-obj (car lst)))
               (t (get-vis-obj (cdr lst))))))
  (get-vis-obj (widgets device))))
               

(defmethod device-move-cursor-to ((device exp-window) loc) 
  (setf (mouse-pos device) loc))

(defmethod get-mouse-coordinates ((device exp-window))
  (mouse-pos device))

(defmethod device-handle-click ((device exp-window))
  (awhen (current-widget device (get-mouse-coordinates device)) 
         (progn
           (model-output "~%Model clicked ~A.~%" (nick-name it)) 
           (state-check device (nick-name it)))))

(defmethod device-handle-keypress ((device exp-window) key)
  (let ((trl (current-trial device)))
;; record the data into the trial so that write-trial can write it to the date file later on
    (if (eql (trial-kind trl) 'interruption)
;; how do I terminate when key is return?
      (progn
          (setf (latency trl) (start-stop-timer (timer device)))
          (finish-trial device))
      (progn
        (setf (response trl) key
              (latency trl) (start-stop-timer (timer device))
              (offset trl)
              (let ((exp-nmbr
                     (gethash
                      (trial-kind trl)
                      (offset-table device)))
                    (rsp-nmbr
                     (gethash
                      (infer-response-type (response trl))
                      (offset-table device))))
                (cond
                 ((< (abs (- rsp-nmbr exp-nmbr)) 4) (- rsp-nmbr exp-nmbr))
                 ((> exp-nmbr rsp-nmbr) (- (+ rsp-nmbr 7) exp-nmbr))
                 ((> rsp-nmbr exp-nmbr) (- rsp-nmbr (+ exp-nmbr 7))))))
;; incf the window's data lists
        (if (interrupted trl)
          (incf 
           (nth
            (+ 3 
               (offset trl))
            (data-int device)))
          (incf 
           (nth
            (+ 3 
               (offset trl))
            (data-base device))))
        (when (interrupted trl) (model-output "~%Response Offset: ~A~%" (offset trl)))
        (finish-trial device)))))





;;;; ---------------------------------------------------------------------- ;;;;
;;;;   Testing & Running 
;;;; ---------------------------------------------------------------------- ;;;;
(defmethod run-experiment ((wind exp-window))
  (unless (worker wind)
    (with-open-file 
        (strm 
         (data-file wind)
         :direction :output 
         :if-exists :supersede
         :if-does-not-exist :create)
      ;; run#, trial#, trial-type/task-id, the correct procedure step, the model's 
      ;; action, got step# - expected step#, the step was/was not interrupted, the
      ;; model's action was/was not correct, Inter-Trial Interval, Step Completion
      ;; Latency
      (let ((col-hdings (list "Run" "Cond" "Block" "Trial" "Kind" "RT" "StTime" "Response" "Offset" "Intrpt" "LR_order" "letter" "number" "font" "color" "height")))
        (format strm "~{~a~^	~}~%" col-hdings))))
  (setup-experiment wind)
  (progn
    (goal-focus start-unravel)
    (schedule-set-buffer-chunk 
     'imaginal
     (car (define-chunks-fct '((isa task-state step start))))
     0 :module 'imaginal))
  (run-block wind (car (block-lst wind))))

(defun summarize-cond (data)
  (let ((s-data (list (list 0 0 0 0 0 0 0)
                      (list 0 0 0 0 0 0 0)))
        (nulls 0)
        (n 0))
    (loop for i in data ; iterate through runs
      do (if (null i)
           (incf nulls)
           (loop for j from 0 to 1 ; iterate through trial-types
             do (loop for k from 0 to 6 ; iterate through offsets
                  do (incf (nth k (nth j s-data)) (nth k (nth j i)))))))
    (terpri)
    (when (> 0 nulls)
      (format t "Null runs: ~A~%" nulls))
    (setf n (- (length data) nulls))
    (if (eq n 0)
      (format t "No run finished!~%")
      (loop for i from 0 to 1
        collect (loop for j from 0 to 6
                  collect (/ (nth j (nth i s-data)) n) into type-summary
                  finally (return type-summary)) into summary
        finally (return summary)))))
         
    

(defun print-model-run (data latencies &optional cond)
  (terpri)
  (let ((param (list ':mas ':ans ':boosti ':boostq)))
    (format t "~{~a: ~a	~}~%" (flatten (pairlis param (no-output (sgp-fct param))))))
  (format t "	-3	-2	-1	+1	+2	+3~%")
  (loop for h from 1 to (length data) ; for however many conditions are run
    do (format t "Cond: ~A~%" (aif cond it h))
    do (loop for i from 0 to 1
         do (format t "~A	" (nth i '("Int" "Bsln")))
         do (loop for item in (nth i (nth (1- h) data))
              with count = 0
              do (unless (eq count 3)
                   (format t "~2,1F	" item))
              do (incf count))
         (terpri))
    (format t "Latencies:		mean	sd~%")
    (format t "Baseline, Correct:	~{~2,1F~^	~}~%" 
            (multiple-value-list 
             (seq-mean-stdev 
              (flatten
               (mapcar #'bsln-correct 
                       (nth (1- h) latencies))))))
    (format t "Baseline, Error:	~{~2,1F~^	~}~%" 
            (multiple-value-list 
             (seq-mean-stdev 
              (flatten
               (mapcar #'bsln-err 
                       (nth (1- h) latencies))))))
    (format t "Resump'n, Correct:	~{~2,1F~^	~}~%" 
            (multiple-value-list 
             (seq-mean-stdev 
              (flatten
               (mapcar #'rsmp-correct 
                       (nth (1- h) latencies))))))
    (format t "Resump'n, Error:	~{~2,1F~^	~}~%" 
            (multiple-value-list 
             (seq-mean-stdev 
              (flatten
               (mapcar #'rsmp-err 
                       (nth (1- h) latencies))))))
    (format t "Int Duration:	~{~2,1F~^	~}~%" 
            (multiple-value-list 
             (seq-mean-stdev 
              (flatten
               (let (sums 
                     (lats (flatten (mapcar #'int (nth (1- h) latencies)))))
                 (do ((i 0 (incf i (aif cond it h))))
                     ((eq i (length lats))
                      sums)
                   (push (apply #'+ (subseq lats i (+ i (aif cond it h)))) sums)))))))
    (terpri))
  (terpri))


(defun print-r-friendly ()
  (let ((data *resp-session-data*))
    (loop for h from 1 to (length data) ; for however many conditions are run
      do (loop for i from 0 to 1
           do (format t "mm~a~a <- c(" h (nth i '(".int" ".bsln")))
           do (loop for item in (nth i (nth (1- h) data))
                with count = 0
                do (unless (eq count 3)
                     (if (eq count 6)
                       (format t "~2,1F)~%" item)
                       (format t "~2,1F, " item)))
                do (incf count))))))



(defun run-model (&key (n 1) (c 1) (m 0) (p t))
  (let (data)
    (format t "C# ~a, Run:" c)
    (dotimes (i n (progn
                    (format t " complete.~%")
                    data))
      (progn
        (reset)
        (install-device 
         (make-instance 
             'exp-window 
           :snum i 
           :m m 
           :xcond c
           :print-to-tl p
           :worker t))
        (format t " ~A" i)
        (run-experiment (current-device))
        (run-until-condition (lambda () nil))
        (push (r-data (current-device)) data)))))





;; full-featured
(defmethod finish-experiment ((wind virtual-experiment-window))
  (progn
    (schedule-break-after-all :details "…the end.")
    (let ((dlsts `(,(data-int wind) ,(data-base wind)))
          (sums `(,(apply #'+ (data-int wind)) ,(apply #'+ (data-base wind)))))
      (loop
        for i from 0 to 1
        collect (loop for j in (nth i dlsts)
                  collect (* 100 (/ j (nth i sums))) into rates
                  finally (return rates)) into rates
        finally (return (setf (r-data wind) rates))))))




;;;;
;;;; Specialty Functions for the Model 
;;;;
(defun unravel-vl (step)
  (let ((rqst
         (case step
           (r (schedule-module-request 
               'visual-location
               (define-chunk-spec-fct
                   '(isa visual-location
                         kind text
                         - color white))
               0
               :module 'visual))
           (a (schedule-module-request 
               'visual-location
               (define-chunk-spec-fct
                   '(isa visual-location
                         kind text
                         - screen-y 50))
               0
               :module 'visual))
           (t (schedule-module-request 
               'visual-location
               (define-chunk-spec-fct
                   '(isa visual-location
                         kind text
                         < screen-x 100))
               0
               :module 'visual)))))
;    (model-output "~A~%" rqst)
    rqst))

(defun process-vo (value status step)
  (cond
   ((and (eql step 'u) (not (eql status 'regular)))
    (if (eql status 'italic) 'i 'u))
   ((and (eql step 'n) (or (eql value 'a) (eql value 'b))) 'n)
   ((and (eql step 'n) (or (eql value 'u) (eql value 'v))) 'f)
   ((and (eql step 'v) (or (eql value 'a) (eql value 'u))) 'v)
   ((and (eql step 'v) (or (eql value 'b) (eql value 'v))) 'c)
   ((numberp value)
    (cond
     ((and (eql step 'e) (evenp value))'e)
     ((and (eql step 'e) (oddp value)) 'o)
     ((and (eql step 'l) (< value 5)) 'l)
     ((and (eql step 'l) (> value 5)) 'm)))))


(defun print-some-cats (tm start-ep &optional other-ep full-back)
  (loop for i from start-ep downto (- start-ep (if full-back 6 3))
    do (funcall
        #'print-chunk-activation-trace-fct 
        (intern (format nil "~a~a~a" "EPISODIC" i "-0"))
        tm)
    finally (aif other-ep
              (funcall
               #'print-chunk-activation-trace-fct 
               (intern (format nil "~a~a~a" "EPISODIC" it "-0"))
               tm))))





;;;; ---------------------------------------------------------------------- ;;;;
;;;;   The Model
;;;; ---------------------------------------------------------------------- ;;;;

(progn
(clear-all)

(define-model 
    unravel
    (let ((mas 7))
      (sgp-fct 
       (list
;; model debugging & running
        :v nil
        :trace-detail 'low

;; declarative
        :imaginal-activation 1.1
        :ans .3 ;.325
        :rt -5 ; 3.03
        :bll .5
        :mas mas
        :ol nil ; spacing effect module needs this to be nil
;        :sact 'medium

;; spacing-effect module
        :eblse t
        :se-intercept 0.177 
        :se-scale .217

;; episodic
        :boostq 4 ; 3
        :boosti .5 ; .85
;        :boost-power nil

#| ;; productions
        :ul t
        :egs 1
        :epl t ; production compilation
        :pct t
|#
;; central parameters
        :er t :esc t))
      
      (mapcar 
       'chunk-type-fct 
       '((task operator step)
         ((unravel (:include task)))
         ((resume (:include task)))
         ((interruption (:include task)))
         (procedure-step place)
         (episodic context unique-name)
         (task-state step))) ; dummy0 dummy1 dummy2 dummy3)))
      
      (let
          ((cks 
            `((start isa chunk)
              (start-unravel
               isa unravel
               step start
               operator retrieve-ps)))
           (ps-cks)
           (sjis))
;; iterate over the list of UNRAVEL steps
        (loop
          for i from 0 to 6
          collecting 
          `(,(nth i '(u n r a v e l))
            isa procedure-step
            place ,(nth i '(u n r a v e l)))
          into ps
          finally (return (setf ps-cks ps)))

;; add chunks & ps-chunks into declarative memory
;; saving the chunk names to feed to add-sji
        (setf ps-cks (add-dm-fct (append cks ps-cks)))

;; but exclude start & start-unravel
        (let (temp)
          (loop
            for i in ps-cks
            do (unless 
                   (member i '(start start-unravel)) 
                 (push i temp))
            finally (return (setf ps-cks (nreverse temp)))))
  
;; set strengths of associations for the procedure-step chunks
;; reciprocal function of distance to succeeding step,
;; modulo number of steps, multiplied by :mas
        (push `(start ,(car ps-cks) ,mas) sjis)
        (push '(start start 0) sjis)
        (loop for j from 1 to (length ps-cks)
          do (loop for i from 1 to (length ps-cks)
            do (push 
                `(,(nth (1- j) ps-cks)
                  ,(nth (1- i) ps-cks) 
                  ,(if (eq j i)
;; for linear boosting
                     mas
;; for exponential boosting
;                     (- mas)
                     (/ 
                      mas 
                      (1+
                       (if (> j i)
                         (- (+ i 7) j)
                         (- i j))))))
                sjis)))
        (add-sji-fct sjis)))

  


;;;
;; Productions
;;;
(p retrieve-next-procedure-step
   =goal>
	isa unravel
        operator retrieve-ps
   =imaginal>
	isa task-state
        step =s
   ?episodic>
   	buffer empty
        state free
   ?retrieval>
        buffer empty
	state free
#|   !eval! (not
           (eq 
            'interruption 
            (trial-kind (current-trial (current-device))))) |#
==>
   =goal>
        operator retrieving-ps
   =imaginal>
   +retrieval>
	isa procedure-step
        - place =s
; !eval! (buffer-chunk imaginal)
; !eval! (buffer-chunk goal)
; !eval! (mp-show-queue)
; !eval! (get-m-buffer-chunks 'goal)
)


(p retrieve-next-procedure-step-failed-try-again
   =goal>
      isa unravel
      operator retrieving-ps
   ?retrieval>
	buffer empty
        state error
   =imaginal>
	isa task-state
        step =s
==>
   =imaginal>
   +retrieval>
	isa procedure-step
        - place =s
   =goal>)



;; Find
(p retrieved-ps
   =goal>
	isa unravel
	operator retrieving-ps
   =retrieval>
	isa procedure-step
        place =p
   =imaginal>  
	isa task-state
==>
   =goal>
	operator retrieved
        step =retrieval
   =imaginal>
	step =retrieval)

(p find-stim
   =goal>
   	isa unravel
        operator retrieved
        step =s
==>
   =goal>
	operator finding
;; if place is r or a, look for a colored or hi/lo item
;; else look for the left item
   !eval! (unravel-vl =s)
; !eval! (buffer-chunk retrieval)
; !eval! (buffer-chunk goal)
)

;; Move
(p found-r-or-a-respond
   =goal>
	isa unravel
   	operator finding
   =visual-location>
   	isa visual-location
        kind text
        color =c
        screen-y =y
   =imaginal>
	isa task-state
   !bind! =k (case (chunk-slot-value-fct 
                    (no-output 
                     (car 
                      (buffer-chunk-fct '(imaginal)))) 
                    'step)
               (r (case =c
                    (red 'r)
                    (yellow 'y)))
               (a (case =y
                    (0 'a)
                    (100 'b))))
   ?manual>
	state free
   ?visual>
	buffer empty
	state free
==>
   =imaginal>
   +visual>
	isa clear
   =goal>
	operator pause
!eval! (schedule-event-relative
        2 ; added to match subjects' mean step completion latency
        (lambda ()
          (mod-chunk-fct 
           (no-output (car (buffer-chunk-fct '(goal))))
           '(operator responding)))
        :module 'scheduling
        :details "Pause for task step latency")
!eval! (schedule-module-request
        'manual
        (define-chunk-spec-fct '(isa press-key key =k))
        2))

(p not-left-vl-continue-right
   =goal>
	isa unravel
   	operator finding
   =visual-location>
   	isa visual-location
        kind text
        color =c
        screen-y =y
   =imaginal>
	isa task-state
   !eval! (case (chunk-slot-value-fct 
                    (no-output 
                     (car 
                      (buffer-chunk-fct '(imaginal)))) 
                    'step)
               (r (eql =c 'white))
               (a (eql =y 50)))
==>
   =goal>
	operator finding
   =imaginal>
   +visual-location>
	isa visual-location
        kind text
        > screen-x 100
)

(p move-visual-attention-to-vl
   =goal>
	isa unravel
   	operator finding
   =visual-location>
   	isa visual-location
        kind text
   =imaginal>
	isa task-state
   !eval! (member 
           (chunk-slot-value-fct 
            (no-output 
             (car 
              (buffer-chunk-fct '(imaginal)))) 
            'step)
           '(u n v e l)) ; place is u, n, v, e, or l
   ?visual>
	buffer empty
   	state free
==>
   =goal>
   	operator attending
   =imaginal>
   +visual>
   	isa move-attention
   	screen-pos =visual-location
; !eval! (buffer-chunk retrieval)
; !eval! (buffer-chunk goal)
)

(p harvest-vo-respond
;; place is u & vo has a status (font is underlined or italicized)
;; place is n or v & vo is a letter
;; place is e or l & vo is a number
   =goal>
   	isa unravel
        operator attending
   =imaginal>
	isa task-state
        step =step
   =visual>
	isa text
        status =status
        value =value
   !bind! =k (process-vo =value =status =step)
   ?manual>
	state free
==>
   =imaginal>
   +visual>
	isa clear
   =goal>
	operator pause
!eval! (schedule-event-relative
        2 ; added to match subjects' mean step completion latency
        (lambda ()
          (mod-chunk-fct 
           (no-output (car (buffer-chunk-fct '(goal))))
           '(operator responding)))
        :module 'scheduling
        :details "Pause for task step latency")
!eval! (schedule-module-request
        'manual
        (define-chunk-spec-fct '(isa press-key key =k))
        2))

(p not-left-vo-continue-right
;; place is u & vo has a status (font is underlined or italicized)
;; place is n or v & vo is a letter
;; place is e or l & vo is a number
   =goal>
   	isa unravel
        operator attending
   =imaginal>
	isa task-state
        step =step
   =visual>
	isa text
        status =status
        value =value
   !eval! (not (process-vo =value =status =step))
==>
   =goal>
	operator finding
   =imaginal>
   +visual-location>
	isa visual-location
        kind text
        > screen-x 100
)


        


(p encode-episodic
   =goal>
	isa unravel
	operator responding
   ?manual>
	preparation free
   ?episodic>
	buffer empty
        state free
   =imaginal>
   	isa task-state
        step =s
==>
   !bind! =uname (new-symbol "episodic")
   +episodic>
	isa episodic
        context =s
        unique-name =uname
   =goal>
	operator encoding-episode
   =imaginal>
)

(p episodic-encoded
   =goal>
   	isa unravel
	operator encoding-episode
   =episodic> 
	isa episodic
   ?retrieval>
	buffer empty
	state free
   ?imaginal>
	state free
   =imaginal>
   	isa task-state
==>
   -episodic>
; !eval! (schedule-clear-buffer 'episodic 1)
   =goal>
	operator check-for-trial-type
   =imaginal>
)

(p check-for-trial-type
   =goal>
	isa unravel
        operator check-for-trial-type
==>
   =goal>
   !eval! (schedule-mod-buffer-chunk 
           'goal
           `(operator ,(if 
                        (eq 
                         'interruption 
                         (trial-kind (current-trial (current-device))))
                        'start-int
                        'retrieve-ps))
           0))
	


  (p start-interruption
     !eval! (eq 
             'interruption 
             (trial-kind (current-trial (current-device))))
     ?manual>
	state free
     ?episodic>
     	buffer empty
     ?retrieval>
     	buffer empty
     	state free
     =goal>
	isa unravel
     	operator start-int
     =imaginal>
     	isa task-state
     !bind! =im (awhen (int-nmbr (current-trial (current-device))) it)
     ==>
     -goal>
     +goal> 
     isa interruption
     operator rehearse
     !eval!
     (let ((intvl (case =im (1 13) (2 9) (3 10)))
           (evt-lst)
           (rhrsl-intvl .636))
       (push
        (schedule-module-request
         'retrieval
         (define-chunk-spec-fct '(isa episodic))
         rhrsl-intvl) ;.62 ;.518)
        evt-lst)
       ;; If it's the last interruption of this set,
       ;; set the resumption goal after the appropriate latency,
       (if (eq =im (xcond (current-device)))
         (push 
          (schedule-event-relative
           intvl
           (lambda ()
             (clear-buffer 'retrieval) ; because resume-retrieve-episode expects it to be empty
             (clear-buffer 'goal)
             (goal-focus-fct (car (define-chunks-fct '((isa resume operator start))))))
           :module 'goal
           :details "Set goal buffer chunk to a resume goal.")
          evt-lst)
         (push 
          (schedule-event-relative
           intvl
           (lambda ()
             (let ((ck (no-output (car (buffer-chunk-fct '(goal))))))
               (if (and (eq 'interruption (chunk-chunk-type-fct ck))
                        (eq 'rehearse (chunk-slot-value-fct ck 'operator)))
                 (mod-chunk-fct ck '(operator rehearse-again)))))
           :module 'goal
           :details "Set goal buffer chunk to a resume goal.")
          evt-lst))
       
       ;; I'm not going to bother having the model actually do the interruption task,
       ;; so just finish the trial
       (push 
        (schedule-event-relative
         intvl
         (lambda ()
           (device-handle-keypress (current-device) #\return))
         :module 'scheduling
         :details "Finish an interruption trial")
        evt-lst)
#|   (when (eq =im (xcond (current-device)))
       (push
       (schedule-break-relative 
       (1+ intvl) 
       :details 
       "Stop for debugging")
       evt-lst)) |#
       (nreverse evt-lst)))



(p continue-interruption
   "Start another rehearsal cycle and schedule the trial's end."
   =goal>
	isa interruption
        operator rehearse-again
;   =retrieval>
;   	isa episodic
;   ?retrieval>
;   	buffer empty
   !bind! =im (awhen (int-nmbr (current-trial (current-device))) it)
==>
   =goal> 
	operator rehearse
   +retrieval> 
   	isa episodic
!eval!
 (let ((intvl (case =im (1 13) (2 9) (3 10)))
       (evt-lst))
;; If it's the last interruption of this set,
;; set the resumption goal after the appropriate latency,
   (if (eq =im (xcond (current-device)))
     (push 
      (schedule-event-relative
       intvl
       (lambda ()
         (clear-buffer 'retrieval) ; because resume-retrieve-episode expects it to be empty
         (clear-buffer 'goal)
         (goal-focus-fct (car (define-chunks-fct '((isa resume operator start))))))
       :module 'goal
       :details "Set goal buffer chunk to a resume goal.")
      evt-lst)
     (push 
      (schedule-event-relative
       intvl
       (lambda ()
         (let ((ck (no-output (car (buffer-chunk-fct '(goal))))))
           (if (and (eq 'interruption (chunk-chunk-type-fct ck))
                    (eq 'rehearse (chunk-slot-value-fct ck 'operator)))
             (mod-chunk-fct ck '(operator rehearse-again)))))
       :module 'goal
       :details "Another interruption trial, so rehearse some more.")
      evt-lst))

;; I'm not going to bother having the model actually do the interruption task,
;; so just finish the trial
   (push 
    (schedule-event-relative
     intvl
     (lambda ()
       (device-handle-keypress (current-device) #\return))
     :module 'scheduling
     :details "Finish an interruption trial")
    evt-lst)
#|   (when (eq =im (xcond (current-device)))
     (push
      (schedule-break-relative 
       (1+ intvl) 
       :details 
       "Stop for debugging")
      evt-lst)) |#
   (nreverse evt-lst)))


  (p interruption-rehearse
     !eval! (eq 
             'interruption 
             (trial-kind (current-trial (current-device))))
     =goal> 
     isa interruption
     operator rehearse
     =retrieval>
     isa episodic
     !bind! =im (awhen (int-nmbr (current-trial (current-device))) it)
     ==>
     =goal>
     !eval!
     ;; This retrievel must be contingent on the trial type still being interruption
     (when (eq 
            'interruption 
            (trial-kind (current-trial (current-device))))
;       (unless ; (and
;           (> (busy-param (current-trial (current-device))) 80)
                #| (> (stop-timing (timer (current-device))) 
                   (* 1000 (- (case =im (1 13) (2 9) (3 10)) 1)))) |#
         (schedule-event-relative
          .62 ; .518
          (lambda ()
            (when (and
                   (eq 'interruption (trial-kind (current-trial (current-device))))
                   (eq 'rehearse (chunk-slot-value-fct 
                                  (no-output (car (buffer-chunk-fct '(goal)))) 'operator)))
              (module-request
               'retrieval
               (define-chunk-spec-fct '(isa episodic)))
              (mod-chunk-fct (no-output (car (buffer-chunk-fct '(goal))))
                             '(operator rehearse)))))))
;     !eval! (mp-show-queue))

(p interruption-rehearse-failed-try-again
   !eval! (eq 
           'interruption 
           (trial-kind (current-trial (current-device))))
   =goal> 
	isa interruption
        operator rehearse
   ?retrieval>
   	buffer empty
        state error
==>
   =goal>
   +retrieval> 
      isa episodic)





;; Goal Resumption

(p resume-retrieve-episode-retrieval-not-empty
   =goal>
	isa resume
        operator start
   ?retrieval>
	buffer full
	state free
==>
   =goal>
   -retrieval>)

(p resume-retrieve-episode
   =goal>
	isa resume
        operator start
   ?retrieval>
	buffer empty
	state free
==>
   =goal>
	operator retrieving-episode
   +retrieval>
	isa episodic)

(p resume-retrieve-episode-failed-try-again
   =goal>
	isa resume
        operator retrieving-episode
   ?retrieval>
	buffer empty
	state error
==>
   =goal>
   +retrieval>
	isa episodic)

(p harvest-retrieved-episodic
   =goal>
   	isa resume
	operator retrieving-episode
   =retrieval>
	isa episodic
        context =c
   ?imaginal>
	buffer empty
	state free
==>
   +imaginal> 
   	isa task-state
        step =c
   -goal>
   +goal>
   	isa unravel
	operator retrieve-ps

;;; How many dummy slots should I fill?
#| !eval!
(schedule-event-after-module
 'imaginal
 (lambda ()
   (let ((cks (define-chunks-fct '((isa chunk) (isa chunk) (isa chunk) (isa chunk)))))
     (mod-chunk-fct (no-output (car (buffer-chunk-fct '(imaginal))))
                  `(dummy0 ,(nth 0 cks)))
#|     (mod-chunk-fct (no-output (car (buffer-chunk-fct '(imaginal))))
                  `(dummy1 ,(nth 1 cks)))
     (mod-chunk-fct (no-output (car (buffer-chunk-fct '(imaginal))))
                  `(dummy2 ,(nth 2 cks)))
     (mod-chunk-fct (no-output (car (buffer-chunk-fct '(imaginal))))
                  `(dummy3 ,(nth 3 cks))) |#
     ))) |#
     

)))


; (run-model :n 2)