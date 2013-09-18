;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[ World State ];;

;;
; *allowed-commands*
;
; The list of allowed user interface commands that `game-eval` will allow.
;
(defparameter *allowed-commands* '(look inventory pickup walk))

;;
; *edges*
;
; An alist associated by the location name. Values provided are
; the edge nodes that connect the location to another location
; with descriptors for the other location name,
; the direction (used with `walk`), and a descriptor for the
; connector (e.g. door).
;
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

;;
; *location*
;
; Sets the default location to a value.
;
(defparameter *location* 'living-room)

;;
; *nodes*
;
; An alist describing generally the location without objects or
; edge connectivity.
;
(defparameter *nodes* '((living-room (you are in the living room.
                                          a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                                     there is a well in front of you.))
                        (attic (you are in the attic.
                                    there is a giant welding torch in the corner.))))

;;
; *objects*
;
; The list of known objects, regardless of location.
;
(defparameter *objects* '(whiskey bucket frog chain))

;;
; *object-locations*
;
; An alist of object to location associations.
;
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[ World Description ];;

;;
; (describe-location location nodes)
;
; Prints pre-described location info.
; Function has no knowledge of game state and is merely a lookup
; function. Must be given the location and the nodes alist.
;
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;;
; (describe-objects location objects object-locations)
;
; Prints a describing sentence for all objects at the given location.
; Function has no knowledge of game state and is merely a lookup
; function. Must be given the location and the edges alist.
;
(defun describe-objects (location objects object-locations)
  (labels ((describe-obj (object)
                         `(you see a ,object on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at location objects object-locations)))))

;;
; (describe-path edge)
;
; Prints a single given path in user-readable terms.
;
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;;
; (describe-paths)
;
; Prints all connecting paths from the given location.
; Function has no knowledge of game state and is merely a lookup
; function. Must be given the location and the edges alist.
;
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;
; (objects-at location objects object-locations)
;
; Prints the objects that are located at the given location.
; Function has no knowledge of game state and is merely a lookup
; function. Must be given the location, object, and object location state.
;
(defun objects-at (location objects object-locations)
  (labels ((at-loc-p (object)
                     (eq (cadr (assoc object object-locations)) location)))
    (remove-if-not #'at-loc-p objects)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[ User Interface ];;

;;
; (inventory)
;
; Prints all objects currently in the user's inventory.
;
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;;
; (look)
;
; Describes the current location, the paths from this location, and the
; objects located here. Function knows about external state.
;
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))


;;
; (pickup object)
;
; Add an object to the player's inventory, if available in the current
; location. Otherwise prints an error.
;
(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

;;
; (walk direction)
;
; Update *location* to the given valid direction or prints an error
; if the direction is not valid.
;
(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
      (progn (setf *location* (car next))
             (look))
      '(you cannot go that way.))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[ REPL Functions ];;


;;
; (game-eval)
;
; Eval the given s-expression if it starts with an allowed-command.
; Otherwise an error is printed.
;
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
    (eval sexp)
    '(i do not know that command)))


;;
; (game-print lst)
;
; Given a list, game-print will convert that list into a string that
; has been manipulated by `tweak-text`.
;
; Example
;
;   (game-print '(this is a string. what do you think of that?))
;   "This is a string. What do you think of that?"
;
(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

;;
; (game-read)
;
; Reads line from stdin and converts into an eval-able command.
;
; Example
;
;   walk foo bar baz
;   (walk 'foo 'bar 'baz)
;
(defun game-read ()
  (let ((cmd (read-from-string
               (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
                     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;;
; (game-repl)
;
; A custom REPL that allows a simpler interface for the user
; to interact with.
;
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

;;
; (tweak-text lst caps lit)
;
; Turns a list of characters into sentences that obey normal
; grammar rules like capitalization.
;
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eq item #\") (tweak-text rest caps (not lit)))
             (lit (cons item (tweak-text rest nil lit)))
            ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(princ "Starting your LISP adventure")
(princ '#\newline)
(game-repl)
