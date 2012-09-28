;Copywright: Robert J. Schalkoff, Artificial Intelligence: An Engineering Approach (McGraw-Hill, 1990)
;Enhanced by Bryan Alcorn
;fwd_r.l
;this program implements the forward chaining algorithm
;Global Vars: *facts* *rules*
;*facts* = (fact fact...)
;*rules* = (rule rule...)
;rule = (rule rule-name (IF fact fact ...)
			;(THEN fact fact ..))

(defvar *facts* '())
(defvar *rules* '((rule isinvertebrate (if no-bones)                      (then invertebrate))
                  (rule isvertebrate   (if bones)                         (then vertebrate))
                  (rule isarachnid     (if invertebrate eight-legs)       (then arachnid))
                  (rule isinsect       (if invertebrate six-legs)         (then insect))
                  (rule ismammal       (if vertebrate fur)                (then mammal))
                  (rule isfish         (if vertebrate gills scales)       (then fish))
                  (rule isfish2        (if vertebrate gills)              (then fish))
                  (rule isbird         (if vertebrate wings lays-eggs)    (then bird))
                  (rule isbird2        (if vertebrate wings)              (then bird))
                  (rule isreptile      (if vertebrate scales lays-eggs)   (then reptile))
                  (rule isamphibian    (if vertebrate skin water-or-land) (then amphibian))
                  (rule isamphibian2   (if vertebrate water-or-land)      (then amphibian))
                  (rule isamphibian3   (if vertebrate skin)               (then amphibian))))

;'remember-new-fact' adds a new fact to the fact list
;input: fact
;output: nil if the fact is on the fact list, otherwise the argumented fact list;side effect: the modification of fact list
(defun remember-new-fact (new-fact)
	(if (member new-fact *facts*)
		nil
		(setq *facts* (cons new-fact *facts*))))

;'recall-fact' checks if a fact is on the fact list
;input: fact
;output: nil if the fact is not on the fact list, otherwise T

(defun recall-fact (fact)
	(if (member fact *facts*) fact nil))

;'get-antecedents' gets the antecedents of given rule
;input: rule
;output: antecedents

(defun get-antecedents (a-rule)
	(rest (nth 2 a-rule)))

;'get-consequences' gets the consequents of given rule
;input: rule
;output: concequents

(defun get-consequences (a-rule)
	(rest (nth 3 a-rule)))

;'test-antecedents' checks if all antecedents are on the fact list
;input: antecedents
;output: T if all antecedents are on the fact list, otherwise nil

(defun test-antecedents (antecedents)
	(if (null antecedents) T
		(if (recall-fact (first antecedents))
		(test-antecedents (rest antecedents)))))

;'check-consequences' checks if any consequence is not on the fact list
;input: consequences
;output: T if any consequence is not on the fact list, otherwise nil

(defun check-consequences (consequences)
	(if (null consequences) nil
		(if (recall-fact (first consequences))
		(check-consequences (rest consequences))
		T)))

;'update-facts' adds the consequences to the fact list
;input: consequences
;output: T

(defun update-facts (consequences)
	(cond
		((null consequences) T)
		(T (cond
			((remember-new-fact (first consequences))
			(format t "~a~%" (first consequences))))
		(update-facts (rest consequences)))))

;'fire-a-rule' fires an applicable rule
;input: rule
;output: T if this rule is fired, otherwise nil

(defun fire-a-rule (a-rule)
	(cond
		((and
			(test-antecedents (get-antecedents a-rule))
			(check-consequences (get-consequences a-rule)))
		(format t "~{~a ~}" (list "Firing:" (nth 0 a-rule) (nth 1 a-rule)))
		(format t "~%Deduce: ")
		(update-facts (get-consequences a-rule)))))

;'chain-forward' fires all applicable rules
;input: the rule list
;output: nil

(defun chain-forward (rulelist)
	(cond
		((null rulelist)
                 (format t "No Further Forward Inference~%")
                 (format t "Updated Fact List: ~a" *facts*))
	((fire-a-rule (car rulelist))
	(chain-forward rulelist))
	(T (chain-forward (rest rulelist)))))

;Resets the fact list
(defun set-facts (fact-list)
  (setq *facts* fact-list))

;Starts the forward chaining algorithm using a set of facts instead of rules
(defun start-chain (facts)
  (set-facts facts)
  (chain-forward *rules*))

;takes a list fact-lists and chains them all together and updates the input
;list of used traits: bones,no-bones,six-legs,eight-legs,fur,gills,scales,wings,lays-eggs,skin,water-or-land
(defun main (main-list)
  (dotimes (i (length main-list))
    (start-chain (nth i main-list))
    (format t "~%Finished with list: ~d~%~%" i)
    (setf (nth i main-list) *facts*)))

;These two functions search through a list of attribute lists and find any lists with 'arg' in them
(defvar index -1)
(defvar list-of-indices '())
(defun find-attribute (list arg)
  (cond
    ((null (first list)) (format t "Reached the end of list~%"))
    ((listp (first list)) (incf index) (find-attribute (first list) arg) (find-attribute (rest list) arg))
    ((eq nil (eq arg (first list))) (find-attribute (rest list) arg))
    ((eq arg (first list)) (format t "Attribute found at: ~d~%" index) (push index list-of-indices))))

(defun check-attributes (list arg)
  (find-attribute list arg)
  (dolist (i list-of-indices)
    (format t "List: ~{~a ~}~%" (nth (pop list-of-indices) list)))
  (setf index -1))
