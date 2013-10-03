(load "graph-util")

(setf *random-state* (make-random-state t)) ; make random actually random
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

;;
; (add-cops edge-alist edges-with-cops)
;
; Add cops randomly into the edge alist.
;
(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                  (list node2 'cops)
                                  edge)))
                            node1-edges))))
            edge-alist))

;;
; (charge pos)
;
; Charge the given position. If the wumpus is in pos, you win the game.
; If not, you lose.
;
(defun charge (pos)
  (handle-direction pos t))

;;
; (connect-all-islands nodes edge-list)
;
; Recursively find all island nodes and connect them with edges.
;
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

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
; (edges-to-alist edge-list)
;
; Turns the undirected edge-list into an alist keyed by node:
;
; '((1 . 2) (2 . 1) (2 . 3) (3 . 2)) becomes...
; '((1 (2)) (2 (1) (3)) (3 (2)))
;
;
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

;;
; (draw-city)
;
; Draw the city graph.
;
(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

;;
; (draw-known-city)
;
; Draw the known city graph.
;
(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

;;
; (find-empty-node)
;
; Given an already populated board, randomly find a node that isn't empty.
; Recurses if the random node is not empty.
;
(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
      (find-empty-node)
      x)))

;;
; (find-islands nodes edge-list)
;
; TODO
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
; TODO
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
; (handle-direction pos charging)
;
; Move the player to pos assuming the move is legal.
; Optional charging flag may be provided.
;
(defun handle-direction (pos charging)
  (let ((edge (assoc pos
                     (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
      (handle-new-place edge pos charging)
      (princ "That location does not exist!"))))

;;
; (handle-new-place edge pos charging)
;
; Move the player position to the given node along the given edge.
; Draw the known city map after moving positions.
; Check if the cops are on the edge you just moved along, if so game over.
; Check if the wumpus is in your new pos, if it is and you were charging you win.
; If you're not charging, you run into him.
; If you charge and the wumpus is not there, you lose.
; If the new node has a glow worm gang, you are randomly transported to a different
; part of the city.
;
(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
          ((member 'wumpus node) (if charging
                                   (princ "You found the Wumpus!")
                                   (princ "You ran into the Wumpus")))
          (charging (princ "You wasted your last bullet. Game Over."))
          (has-worm (let ((new-pos (random-node)))
                      (princ "You ran into a Glow Worm Gang! You're now at ")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))

;;
; (known-city-edges)
;
; Returns the city edges we actually know about based on where
; the player has been.
;
(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                 (if (member (car x) *visited-nodes*)
                                   x
                                   (list (car x))))
                               (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))

;;
; (known-city-nodes)
;
; Returns the city nodes we actually know about based on where
; the player has been.
;
(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
              (let ((n (assoc node *congestion-city-nodes*)))
                (if (eql node *player-pos*)
                  (append n '(*))
                  n))
              (list node '?)))
          (remove-duplicates
            (append *visited-nodes*
                    (mapcan (lambda (node)
                              (neighbors node *congestion-city-edges*))
                            *visited-nodes*)))))

;;
; (make-city-edges)
;
; Builds the connected city map, sans islands, with cops.
;
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
                      collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

;;
; (make-city-nodes edge-alist)
;
; Build the final city map.
;
(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                          collect (random-node))))
    (loop for n from 1 to *node-num*
          collect (append (list n)
                          (cond ((eql n wumpus) '(wumpus))
                                ((within-two n wumpus edge-alist) '(blood!)))
                          (cond ((member n glow-worms)
                                 '(glow-worm))
                                ((some (lambda (worm)
                                         (within-one n worm edge-alist))
                                       glow-worms)
                                 '(lights!)))
                          (when (some #'cdr (cdr (assoc n edge-alist)))
                            '(sirens!))))))

;;
; (make-edge-list)
;
; Generate a random node/edge game board.
;
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))

;;
; (neighbors node edge-alist)
;
; Return an array of neighboring nodes to the given node.
;
(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

;;
; (new-game)
;
; Start a new game of Grand Theft Wumpus by populating the edges,
; randomly filling the nodes, setting the player's current position
; (to an empty node), and setting an initial list of the player's
; visited nodes.
;
(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

;;
; (random-node)
;
; Return a random number from 1 >= *node-num*.
;
(defun random-node ()
  (1+ (random *node-num*)))

;;
; (walk pos)
;
; Walk to the given position.
;
(defun walk (pos)
  (handle-direction pos nil))

;;
; (within-one a b edge-list)
;
; Check to see if the given nodes have a single node in-between.
;
(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

;;
; (within-two a b edge-list)
;
; Check to see if the given nodes have at most two nodes in-between.
;
(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
              (within-one x b edge-alist))
            (neighbors a edge-alist))))

