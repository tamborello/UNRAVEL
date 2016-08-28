;;; 2013.02.25	fpt	1
;;; Added an ACT-R parameter, boostq. It's an integer specifying the number of
;;; retrieval references the episodic chunk is to receive at encoding.
;;; 2013.04.29	fpt	2
;;; Added an ACT-R parameter, boosti. It's a number specifying the duration of the
;;; interval between each boost instance entered into the epsisodic chunk's retrieval
;;; reference list.


;; following needed?
(require-compiled "GOAL-STYLE-MODULE" "ACT-R6:support;goal-style-module")

;; A structure to use for the simple module
;; only needs a slot to hold the chunk that
;; will get boosted.

(defstruct boost chunk-to-boost param-boostq param-boosti)

;; All that needs to happen at reset is to
;; make sure that the function which gets
;; notified by declarative that a chunk has 
;; been added is put in place.

(defun reset-boost (boost)
  (declare (ignore boost))
  (sgp :chunk-add-hook check-newly-added-dm-chunk))

;; A dummy query because the module has to
;; have a query function since it has a buffer.

(defun boost-query (module buffer query val)
  (declare (ignore module buffer query val))
  t)

;; This function will be called everytime a
;; chunk gets cleared from a buffer.
;; We want to record the chunk name when
;; it's coming out of the episodic buffer.

(defun check-for-boosting (boost buffer chunk)
  (if (eq buffer 'episodic)
      (setf (boost-chunk-to-boost boost) chunk)
    (setf (boost-chunk-to-boost boost) nil)))

;; This function will be called by the declarative
;; module everytime it adds a new chunk to DM
;; but not when a chunk gets merged with another.
;; If that chunk is the one that came from the
;; episodic buffer then we want to give it 4
;; references (making sure to check how :ol is
;; set to add the references the right way.)

(defun check-newly-added-dm-chunk (chunk-name)
  (let ((boost (get-module boost-chunks))
        (time (mp-time))
        (ref-ct (boost-param-boostq (get-module boost-chunks)))
        (ref-int (boost-param-boosti (get-module boost-chunks)))
        (lst nil)
        (ol-status (car (no-output (sgp :ol)))))
    
    (when (eq chunk-name (boost-chunk-to-boost boost))
      (if (and
           ol-status
           (not (numberp ol-status)))
          (sdp-fct (list chunk-name :reference-count ref-ct))
        (sdp-fct 
         (list 
          chunk-name 
          :reference-list 
          (dotimes (i ref-ct (nreverse lst))
            (push (+ time (* i ref-int)) lst))))))))


(defun boost-module-params-fct (instance param)
  (if (consp param)
     (case (car param)
       (:boostq
        (setf (boost-param-boostq instance) (cdr param)))
       (:boosti
        (setf (boost-param-boosti instance) (cdr param)))
       (t  (model-warning "Parameter ~A with value ~A does not belong to the boost module." 
                          (car param) (cdr param))))
     (case param
       (:boostq (boost-param-boostq instance))
       (:boosti (boost-param-boosti instance)))))


;; Create the module which will be doing the
;; boosting with nothing special going on.

(define-module-fct 'boost-chunks '(episodic) 
  (list 
   (define-parameter :boostq
                     :documentation "Boostq is an integer specifying the number of retrieval references the episodic chunk is to receive at encoding."
     :default-value 4
     :valid-test (lambda (x) (and (integerp x) (>= x 0)))
     :warning "The value supplied to parameter boostq was not a valid positive integer."
     :owner t)
   (define-parameter :boosti
                     :documentation "Boosti It's a number specifying the duration of the interval between each boost instance entered into the epsisodic chunk's retrieval reference list."
     :default-value 1
     :valid-test (lambda (x) (numberp x))
     :warning "The value supplied to parameter boosti was not a valid number."
     :owner t))
  :version "0.5a r2"
  :documentation "Module which gives extra references to chunks which leave its buffer"
  
  ;; Basic creation and setting of the query
  :creation (lambda (name) (declare (ignore name)) (make-boost))
  :query 'boost-query
  
  :request #'goal-style-request

  ;; When setting the reset function it has to use the
  ;; secondary reset because it's changing another
  ;; module's parameter.  Otherwise the default value
  ;; would overwrite the change.
  
  :reset (list nil 'reset-boost)
  
  ;; The notify-on-clear option lets the module
  ;; check on every chunk leaving a buffer - this's
  ;; the same mechanism which DM uses to get the chunks 
  ;; it adds.
  
  :notify-on-clear 'check-for-boosting

;; Tell ACT-R what function will handle the boost module's parameters
  :params #'boost-module-params-fct)

