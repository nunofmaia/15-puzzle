(compile-file "procura.lisp")
(load "procura")

(defvar *estado-final* (make-array '(4 4)
                                      :initial-contents '((1 2 3 4)
                                                          (5 6 7 8)
                                                          (9 10 11 12)
                                                          (13 14 15 nil))))

(defvar *mapa-posicoes* (make-hash-table))
(setf (gethash 1 *mapa-posicoes*) '(0 0))
(setf (gethash 2 *mapa-posicoes*) '(0 1))
(setf (gethash 3 *mapa-posicoes*) '(0 2))
(setf (gethash 4 *mapa-posicoes*) '(0 3))
(setf (gethash 5 *mapa-posicoes*) '(1 0))
(setf (gethash 6 *mapa-posicoes*) '(1 1))
(setf (gethash 7 *mapa-posicoes*) '(1 2))
(setf (gethash 8 *mapa-posicoes*) '(1 3))
(setf (gethash 9 *mapa-posicoes*) '(2 0))
(setf (gethash 10 *mapa-posicoes*) '(2 1))
(setf (gethash 11 *mapa-posicoes*) '(2 2))
(setf (gethash 12 *mapa-posicoes*) '(2 3))
(setf (gethash 13 *mapa-posicoes*) '(3 0))
(setf (gethash 14 *mapa-posicoes*) '(3 1))
(setf (gethash 15 *mapa-posicoes*) '(3 2))
(setf (gethash nil *mapa-posicoes*) '(3 3))

(defun linha-posicao (posicao)
    (car posicao))

(defun coluna-posicao (posicao)
    (cadr posicao))

(defun encontra-posicao (estado)
    (let ((value nil))
        (dotimes (i (array-dimension estado 0) value)
            (dotimes (j (array-dimension estado 1))
                (if (null (aref estado i j))
                    (setf value (list i j)))))
        value))

;;; Generic movement function

(defun mover (estado sentido predicado tipo-movimento)
    (let* ((posicao-atual (car estado))
           (estado-atual (cdr estado))
           (valor nil)
           (movimento nil)
           (nova-posicao (copy-list posicao-atual))
           (novo-estado (copy-array estado-atual)))
        (when (funcall predicado posicao-atual)
            (cond ((string-equal tipo-movimento "vertical")
                    (setf movimento (funcall sentido (linha-posicao posicao-atual)))
                    (setf valor (aref estado-atual movimento (coluna-posicao posicao-atual)))
                    (setf (aref novo-estado movimento (coluna-posicao posicao-atual)) nil)
                    (setf (car nova-posicao) movimento))
                  ((string-equal tipo-movimento "horizontal")
                    (setf movimento (funcall sentido (coluna-posicao posicao-atual)))
                    (setf valor (aref estado-atual (linha-posicao posicao-atual) movimento))
                    (setf (aref novo-estado (linha-posicao posicao-atual) movimento) nil)
                    (setf (cadr nova-posicao) movimento)))
            (setf (aref novo-estado (linha-posicao posicao-atual) (coluna-posicao posicao-atual)) valor)
            (list (cons nova-posicao novo-estado)))))

;;; Move up

(defun mover-cima-p (posicao)
    (if (>= (1- (linha-posicao posicao)) 0)
        t
        nil))

(defun mover-cima (estado)
    (mover estado #'1- #'mover-cima-p "vertical"))

;;; Move down

(defun mover-baixo-p (posicao)
    (if (< (1+ (linha-posicao posicao)) 4)
        t
        nil))

(defun mover-baixo (estado)
    (mover estado #'1+ #'mover-baixo-p "vertical"))

;;; Move esquerda

(defun mover-esquerda-p (posicao)
    (if (>= (1- (coluna-posicao posicao)) 0)
        t
        nil))

(defun mover-esquerda (estado)
    (mover estado #'1- #'mover-esquerda-p "horizontal"))

;;; Move direita

(defun mover-direita-p (posicao)
    (if (< (1+ (coluna-posicao posicao)) 4)
        t
        nil))

(defun mover-direita (estado)
    (mover estado #'1+ #'mover-direita-p "horizontal"))

;;; Heuristic functions

(defun posicoes-fora-do-sitio (estado)
    (let ((value 0)
          (estado-atual (cdr estado)))
        (dotimes (i (array-dimension estado-atual 0) value)
            (dotimes (j (array-dimension estado-atual 1))
                (if (not (equalp (aref estado-atual i j) (aref *estado-final* i j)))
                    (incf value))))
        value))


(defun distancia-de-manhattan (estado)
    (let ((value 0)
          (estado-atual (cdr estado))
          (valor-atual nil)
          (posicao-suposta nil))
        (dotimes (i (array-dimension estado-atual 0) value)
            (dotimes (j (array-dimension estado-atual 1))
                (setf valor-atual (aref estado-atual i j))
                (setf posicao-suposta (gethash valor-atual *mapa-posicoes*))
                (setf value (+ value (+ (abs (- (linha-posicao posicao-suposta) i))
                                        (abs (- (coluna-posicao posicao-suposta) j)))))))
        value))

(defun posicao-para-valor (i j)
  (+ 1 j (* 4 i)))

(defun distancia-a-posicao-correta (estado)
    (let ((value 0)
          (estado-atual (cdr estado))
          (valor-atual nil)
          (valor-suposto nil))
        (dotimes (i (array-dimension estado-atual 0) value)
            (dotimes (j (array-dimension estado-atual 1))
                (setf valor-atual (aref estado-atual i j))
                (setf valor-suposto (posicao-para-valor i j))
                (when (not (null valor-atual))
                  (setf value (+ value (abs (- valor-suposto valor-atual)))))))
        (+ value (posicoes-fora-do-sitio estado))))


(defun testa-estado (estado-a estado-b)
    (equalp (cdr estado-a) (cdr estado-b)))

;;; Problem solver

(defun resolve-problema (estado-inicial &optional (tipo-procura "profundidade"))
    (let* ((posicao-inicial (encontra-posicao estado-inicial))
          (par-inicial (cons posicao-inicial estado-inicial))
          (problema (cria-problema
                      par-inicial
                      (list #'mover-cima #'mover-esquerda #'mover-direita #'mover-baixo)
                      :estado-final (cons '(3 3) *estado-final*)
                      :heuristica #'distancia-a-posicao-correta
                      :estado= #'testa-estado)))
        (procura problema tipo-procura)))


(defvar estado-1)
(defvar estado-2)
(defvar estado-3)
(defvar estado-4)
(defvar estado-5)

(setf estado-1 (make-array '(4 4)
                                :initial-contents '((1 2 3 4)
                                                    (5 6 7 8)
                                                    (13 9 10 11)
                                                    (14 nil 15 12))))

(setf estado-2 (make-array '(4 4)
                                :initial-contents '((2 6 3 4)
                                                    (1 13 7 8)
                                                    (5 9 11 12)
                                                    (10 14 nil 15))))

(setf estado-3 (make-array '(4 4)
                                :initial-contents '((nil 6 3 4)
                                                    (2 13 7 8)
                                                    (1 5 9 12)
                                                    (10 14 11 15))))

(setf estado-4 (make-array '(4 4)
                                :initial-contents '((6 13 3 4)
                                                    (2 7 8 12)
                                                    (nil 1 5 15)
                                                    (10 14 9 11))))

(setf estado-5 (make-array '(4 4)
                                :initial-contents '((13 10 11 6)
                                                    (5 7 4 8)
                                                    (1 12 14 9)
                                                    (3 15 2 nil))))
