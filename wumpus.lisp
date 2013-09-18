(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

;;
; (connect-all-islands nodes edge-list)
;
; Recursively find all island nodes and connect them with edges.
;
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands node edge-list)) edge-list)

;;
; (connect-with-bridges islands)
;
; Connect given island nodes with edges.
;
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

;;
; (direct-edges node edge-list)
;
; Find all edges that start from a given node.
;
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))
;;
; (edge-pair a b)
;
; Returns a list of two edges between two nodes for both directions.
;
(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

;;
; (find-islands nodes edge-list)
;
; No idea yet.
;
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
                          (let* ((connected (get-connected (car nodes) edge-list))
                                 (unconnected (set-difference nodes connected)))
                            (push connected islands)
                            (when unconnected
                              (find-island unconnected)))))
      (find-island nodes))
    islands))

;;
; (get-connected node edge-list)
;
; I have no idea.
;
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
                       (unless (member node visited)
                         (push node visited)
                         (mapc (lambda (edge)
                                 (traverse (cdr edge)))
                               (direct-edges node edge-list)))))
      (traverse node))
    visited))

;;
; (make-edge-list)
;
; Generate a random node/edge game board.
;
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))

;;
; (random-node)
;
; Return a random number from 1 >= *node-num*.
;
(defun random-node ()
  (1+ (random *node-num*)))


