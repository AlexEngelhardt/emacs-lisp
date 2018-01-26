;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6 - Narrowing and Widening

;; Narrowing makes part of the buffer invisible so you can focus on a
;;  specific section of the document.
;; Widening undoes it.

;; C-h f:
(describe-function 'narrow-to-region)
(describe-function 'widen)

;; The what-line function, for example, removes the narrowing from a buffer, if it has any narrowing and when it has finished its job, restores the narrowing to what it was.

(defun what-line ()
  "Print the current buffer line number and narrowed line number of point."
  (interactive)
  (let ((start (point-min))
	(n (line-number-at-pos)))
    (if (= start 1)  ; if buffer has not been narrowed
	(message "Line %d" n)
      (save-excursion  ; else
	(save-restriction  ; this special form keeps track of the current narrowing
	  (widen)
	  (message "line %d (narrowed line %d)"
		   (+ n (line-number-at-pos start) -1) n))))))


;;;;
;; 6.3 Exercise with Narrowing

(defun first-60 ()
  "Write a function that will display the first 60 characters of
  the current buffer, even if you have narrowed the buffer to its
  latter half so that the first line is inaccessible. Restore
  point, mark, and narrowing."
  (interactive)

  (save-excursion
    (save-restriction
      (widen)
      (setq first-60-chars (buffer-substring 1 60))
      ))
  (message "first 60 chars: %s" first-60-chars)
  )

;; Execute with M-x first-60 to see a bunch of semicolons.
