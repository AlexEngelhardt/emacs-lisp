
(defun multiply-by-seven (number)
  "Multiply the argument 'number' by seven."
  (* 7 number))

;; Evaluating the definition "installs" the function
;; It returns the name of the function.

(multiply-by-seven 11)

;; Do C-h f "multiply-by-seven" to see the docstring

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

;; They are often called for their side effects (e.g. moving the cursor)
;; instead of their return value. They therefore don't print the return
;; value in the echo area.

(defun multiply-by-seven (number)       ; Interactive version.
  "Multiply NUMBER by seven."
  (interactive "p")
  ;; The "p" in the interactive call tells Emacs to pass a prefix argument
  ;; as a parameter to the function 
  (message "The result is %d" (* 7 number)))

(multiply-by-seven 123)

;; C-u 3 mu-s [RET]

(global-set-key "f" 'multiply-by-seven)

;; Now you can't type the letter f anymore :D
;; But `C-u 3 f` returns "The result is 21" in the echo area

(global-set-key "f" 'self-insert-command)  ; this lets you use 'f' again :)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive shenanigans

;; Consider the function zap-to-char. Its interactive expression is
;;  (interactive "p\ncZap to char: ")
;; The \n separates the two arguments. 'p' and 'c' are important here.
;; See here for the doc:
;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Interactive-Codes

;; When a function does not take arguments, interactive does not require any. 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; let - Local definitions


(setq capital "Berlin")
(setq name "Germany")
(message "%s is the capital of %s" capital name)

;; let introduces a local namespace, much like `with` in R
(let ((capital "Munich")
      (name "Bavaria"))
  (message "%s is the capital of %s" capital name)
  )

(message "%s is the capital of %s" capital name)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if-else

;; nil is false -- and the empty list: ()
;; anything else is true

t
nil

(> 5 4)
(< 5 4)

(if (> 5 4)
    (message "5 is greater than 4!")
  )

(if (> 5 4)
    "TRUE DAT"
  )
(if (> 5 4)
    'TRUE-DAT
  )

(defun type-of-animal (characteristic)
  "Print message in echo area depending on CHARACTERISTIC.
     If the CHARACTERISTIC is the string \"fierce\",
     then warn of a tiger."
  (if (equal characteristic "fierce")
      (message "It is a tiger!")))  ; else nil

(type-of-animal "fierce")
(type-of-animal "striped")

(defun type-of-animal (characteristic)
  "Print message in echo area depending on CHARACTERISTIC.
     If the CHARACTERISTIC is the string \"fierce\",
     then warn of a tiger."
  (if (equal characteristic "fierce")
      (message "It is a tiger!") 
    (message "All is well")
   ))

(type-of-animal "striped")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save-excursion

;; Similar to `progn`. But this one saves the cursor position ("point")

(defun doot ()
  (insert "DOOT")
  (message "I dooted!")
  )

(doot)DOOT  ; watch what happens to the cursor

(defun doot2 ()
  (save-excursion
    (insert "DOOT")
    (message "I dooted with save-excursion!")
    )
  )

(doot2)  ; the cursor stays where it is!
