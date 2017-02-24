;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
;;;;;;	REINFORCEMENT LEARNING ACT-R model in order to show the developmental transitions in reasoning about false beliefs of others.
;;;;;;  These stand from a child's reasoning from his/her own point of view (zero-order) to taking into consideration
;;;;;;  an other agent’s beliefs (first-order) and later to taking into consideration an other agent’s beliefs about
;;;;;;  other agents’ beliefs (second-order).
;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
;;;;;;	Instructions on using this model:
;;;;;;		1. Make sure you have ACT-R 6.0
;;;;;;		2. Call (fbt) if you run the model for 1 child doing the experiment 1 time
;;;;;;    3. Call (do-fbt) if you run the model for 1 child doing the experiment 100 times
;;;;;;    4. Call (do-fbt-n N) if you run the model for N children doing the experiment 100 times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar *response* nil) ;; global variable that stores model output
(defvar *model* nil)
(defvar *hold-responses* nil)  ;; global variable that stores model output



;; do experiment 1 time
(defun fbt (&optional who)

(goal-focus goal)
  (if (eq who 'human)
      (setf *model* nil)
         (setf *model* t)
   )
        (run 30 :real-time t)
)


;; do experiment 100 times
(defun do-fbt ()
	(dotimes (i 100)
            (setf *response* nil)
			(fbt)
               (push *response* *hold-responses*)
         )
)

;; do experiment nx100 times
(defun do-fbt-n (n)
	  (dotimes (i n)
              (setf *hold-responses* nil)
                        (reset)
			(do-fbt)
                        (write-to-file (concatenate 'string "dat-" (write-to-string i)) (nreverse *hold-responses*))
;(setf *hold-response* nil)
         )
)

;; write results to a file
(defun write-to-file (name lst)
	(with-open-file
		(out
			(ensure-directories-exist
				(merge-pathnames
					(make-pathname :name name :directory '(:relative "ACTR_model_output") :type "txt")
					"~/")
                        )
			:direction :output :if-does-not-exist :create :if-exists :supersede
                 )
			(dolist (line lst)
				(format out "~{~a~^~t~}~%" line))
         )
)



(clear-all) ;; clear model settings

(define-model FBT_2   ;name of the model

;;Model Parameters:

;;Below you see the related parameters for the model. You’re expected to assign valuesto them or keep them default or turn them on (t) or off (nil)
;; you can change the parameters once you have a complete model at the end of the course.
;; when you write your scientific report, you are expected to explain your decisions about the parameters.

(sgp

	:esc  t ;  sub-symbolic level
	:ol  t ;  optimised learning
	:rt  -5 ;  retrieval threshold
	:ans  .1 ;  instantaneous noise
  :egs 0 ;  utility noise
  :ul  t ;  utility learning
  :pas  nil ; permanent noise

;; The parameters below for tracking the model’s trace. You can change them if you need to turn off the details etc.
;; See reference manual of ACT-R for further details, which is in the “docs” folder of ACT-R’s main folder

        :ult nil ;turn on the trace for the utilities
        :v  t   ; trace detail
        :act low  ; activation trace parameter
        :sact low ; save activation trace

)

;Chunk-types of the Model:

;You will use the following chunk-type for the story facts. Story facts are the sentences from the story, i.e., see lines 141-146.
;The slot "subject" is for the subjects (i.e., "maxi", "sally", "mother").
;The slot "negation" is for negating the verbs (i.e., either nil or "not").
;The slot "verb" is for the verbs (i.e., "put", "see").
;The slot "object" is for the object (i.e., "chips").
;The slot "location" is for the locations (i.e., "cupboard", "oven", "trashbin").
;The slot "time" is for when the story is happened (i.e., 1,2,3, which represent time t0, t1, t2)(see below the temporal order chunk explanation, lines 141-147).
;The slot "type" is holding the type of a verb (i.e., action, perception).
;The slot "self-ref" refers to the chunks name itself. Each story fact chunk will refer to itself.
;The slot "ref" refers to other story facts which occur at the same time. (see below the temporal order chunk explanation, i.e., lines 141-146).
;The slot "level" is to hold the previously retrieved level of reasoning chunk in order to use it when it is necessary. (You might need this at some point in first-order and second-order reasoning)

(chunk-type story subject negation verb object location time type self-ref ref level)


;For the time sequences of the events. (You might need these but it is not necessary if you find other ways to model).

(chunk-type tijd t0 t1 t2 t3 t4)

; The following chunk type is for the goals and their states.
; The slot "type" is for the where question (action)
; The slot "output"  is for holding the output of the model (i.e., cupboard, oven, trashbin)
(chunk-type goal state type output)


(add-dm

;The story fact chunks
(not isa chunk) (action isa chunk) (perception isa chunk) (subject isa chunk)
(location isa chunk) (verb isa chunk) (object isa chunk) (negation isa chunk)
(put isa chunk) (see isa chunk) (type isa chunk) (maxi isa chunk) (sally isa chunk) (chips isa chunk) (mother isa chunk)

;goal state chunks. You’re expected to write the goal state chunks below
(cupboard isa chunk) (oven isa chunk) (trashbin isa chunk)


(start isa chunk) (findlocation isa chunk) (answer isa chunk) (transit isa chunk) (answer-zero isa chunk)
(answer-first isa chunk)
;temporal order chunk. There are three seperate time points in the story:
; at t0 Maxi put the chips into the cupboard.
; at t1 Sally put the chips into the oven.
; at t1 Maxi saw Sally.
; at t1 Sally did not see Maxi.
; at t2 The mother put the chips into the trashbin.
 (t0 ISA tijd t0 1 t1 2 t2 3)


;Here, you are expected to write the model's knowledge representations about the story facts (i.e., lines 154-158) based on the defined story chunk-type above.
(fact1 ISA story subject maxi negation nil verb put object chips location cupboard time 1 type action self-ref fact1 ref nil level nil)
(fact2 ISA story subject sally negation nil verb put object chips location oven time 2 type action self-ref fact2 ref fact3 level nil)
(fact3 ISA story subject maxi negation nil verb see object sally location nil time 2 type perception self-ref fact3 ref fact4 level nil)
(fact4 ISA story subject sally negation not verb see object maxi location nil time 2 type perception self-ref fact4 ref fact2 level nil)
(fact5 ISA story subject mother negation nil verb put object chips location trashbin time 3 type action self-ref fact5 ref nil level nil)


;Below chunk assigns the goal at the beginning of the trial
 (goal isa goal state start type action output nil)
)

; For the Assignment 2, you are expected to write production rules to apply zero-order reasoning and gives the answer "trashbin" (as if the model reasons about the question "Where is the chips?").

; The production rule that gives the answer should also have the following functions for the output of the model:
; To put 0 (zero) as a strategy level representing the zero-order strategy to the variable response:
;!safe-eval! (push (spp ("you should write the name of the production rule that gives the zero-order answer"
;"you should write the name of the first production rule of the first-order strategy") :name :utility :u :at :reward) *response*)

(P start-zero-order
  =goal>
    isa       goal
    state     start
==>
  +retrieval>
    isa       story ; retrieve the story facts at the last time-chunk
    time      3
  =goal>
    state     transit
)

(P transit-zero-order
  =goal>
    ISA       goal
    state     transit
  =retrieval>
    isa       story
    time      3
    type 	    =type ; make sure the type is action and not perception
    location  =location ; get the location from the last story fact
    subject   =subject
    ;negation  =negation 
    verb      =verb 
    object    =object
    time      =time 
  ?imaginal>
    state     free
==>
  +imaginal>
    isa       story
    time      3
    type      =type ; make sure the type is action and not perception
    location  =location ; get the location from the last story fact
    subject   =subject
    ;negation  =negation 
    verb     =verb 
    object    =object
    time      =time 
  =goal>
    state     answer-zero
)

(P answer-zero-order
  =goal>
    isa       goal
    state     answer-zero
  =imaginal>
    isa       story
    location  =location
    time      3 
==>
  -goal>
  !output!    (=location)
  !safe-eval! (push 0 *response*) ; use zero-order reasoning
  !safe-eval! (push (spp (answer-zero-order start-first-order) :name :utility :u :at :reward) *response*) ; magic
)

(P start-first-order
  =goal>
    isa       goal
    state     answer-zero
==>
  +retrieval>
    isa       story ; retrieve the story facts at the time-chunk where maxi is the subject
    time      2
    subject   maxi
  =goal>
    state     findlocation
)

(P find-location-first-order
  =goal>
    isa       goal
    state     findlocation
  =retrieval>
    isa       story
    type      perception
==>
  +retrieval>
    isa       story
    time      2
    type      action
  =goal>
    state     answer-first
  )

(P answer-first-order
  =goal>
    ISA       goal
    state     answer-first
  =retrieval>
    isa       story
    type      action
    location  =location ; get the location from the second story fact
==>
  -goal>
  !output!    (=location)
  !safe-eval! (push 1 *response*) ; use first-order reasoning
  !safe-eval! (push (spp (answer-zero-order start-first-order) :name :utility :u :at :reward) *response*) ; magic
)

;; The assignment will be graded in terms of the following criteria:
;; 1) Output
;; 2) Cognitive Plausibility of the production rules
;; 3) The quality of the code document in terms of clear explanations.




; For the Assignment 2, you're expected to write an initial utility value for the zero-order strategy below.
; In the following assignments, you will also add intial utility values for the first-order and second-order strategies.

(spp answer-zero-order :u 20)
(spp start-first-order :u 10)

; For the Assignment 2, you're expected to write a reward value for the zero-order stategy below.
; In the following assignments, you will also add reward values for the first-order and second-order strategies.

(spp answer-zero-order :reward 0)
(spp start-first-order :reward 0)



)
