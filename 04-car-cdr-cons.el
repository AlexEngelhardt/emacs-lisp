;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 04 - car, cdr, cons: Fundamental Functions

;; cons: construct lists
;;  "Construct"
;; car, cdr: Take them apart
;;  "Contents of the Address part of the Register"
;;  "Contents of the Decrement part of the Register" :3

(car '(rose violet daisy buttercup))  ; car: First element
(cdr '(rose violet daisy buttercup))  ; cdr: All other elements
;; Wasn't there a weird language where head is the first element, and tail
;;  is all others? SPSS? Matlab?

;; Constructing lists
(cons 'buttercup ())
(cons 'rose '(violet daisy buttercup))

;; Length of lists
(length ())
(length
 (cons 'rose '(violet daisy buttercup))
	)


;; Iterated cdr

(cdr (cdr '(pine fir oak maple)))
(nthcdr 2 '(pine fir oak maple))

(nth 2 '(pine fir oak maple))  ; oak tho
(nth 0 '(pine fir oak maple))  ; pine
(nth 4 '(pine fir oak maple))  ; nil


;; cons, car, cdr are non-destructive. They don't modify lists.
;; These guys, however, do:

(setq animals '(antelope giraffe lion tiger))
animals

(setcar animals 'hippopotamus)
animals

(cons 'animals animals)  ; huh.
(cons animals animals)  ; heh.
(cons 'animals 'animals)  ; wat.
