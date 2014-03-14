(compile-file "procura.lisp")
(load "procura")

(defvar *estado-final* (make-array '(4 4)
                                      :initial-contents '((1 2 3 4)
                                                          (5 6 7 8)
                                                          (9 10 11 12)
                                                          (13 14 15 nil))))

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

;;; Generic vertical movement

(defun mover-vertical (estado sentido predicado)
    (let* ((posicao (encontra-posicao estado))
          (valor nil)
          (linha-futura (funcall sentido (linha-posicao posicao)))
          (novo-estado (copy-array estado)))
        (when (funcall predicado posicao)
               (setf valor (aref estado linha-futura (coluna-posicao posicao)))
               (setf (aref novo-estado linha-futura (coluna-posicao posicao)) nil)
               (setf (aref novo-estado (linha-posicao posicao) (coluna-posicao posicao)) valor)
               (setf (car posicao) linha-futura))
        (list novo-estado)))

;;; Generic horizontal movement

(defun mover-horizontal (estado sentido predicado)
    (let* ((posicao (encontra-posicao estado))
          (valor nil)
          (coluna-futura (funcall sentido (coluna-posicao posicao)))
          (novo-estado (copy-array estado)))
        (when (funcall predicado posicao)
               (setf valor (aref estado (linha-posicao posicao) coluna-futura))
               (setf (aref novo-estado (linha-posicao posicao) coluna-futura) nil)
               (setf (aref novo-estado (linha-posicao posicao) (coluna-posicao posicao)) valor)
               (setf (cadr posicao) coluna-futura))
        (list novo-estado)))

;;; Move up

(defun mover-cima-p (posicao)
    (if (>= (1- (linha-posicao posicao)) 0)
        t
        nil))

(defun mover-cima (estado)
    (mover-vertical estado #'1- #'mover-cima-p))

;;; Move down

(defun mover-baixo-p (posicao)
    (if (< (1+ (linha-posicao posicao)) 4)
        t
        nil))

(defun mover-baixo (estado)
    (mover-vertical estado #'1+ #'mover-baixo-p))

;;; Move esquerda

(defun mover-esquerda-p (posicao)
    (if (>= (1- (coluna-posicao posicao)) 0)
        t
        nil))

(defun mover-esquerda (estado)
    (mover-horizontal estado #'1- #'mover-esquerda-p))

;;; Move direita

(defun mover-direita-p (posicao)
    (if (< (1+ (coluna-posicao posicao)) 4)
        t
        nil))

(defun mover-direita (estado)
    (mover-horizontal estado #'1+ #'mover-direita-p))

;;; Heuristic functions

(defun posicoes-fora-do-sitio (estado)
    (let ((value 0))
        (dotimes (i (array-dimension estado 0) value)
            (dotimes (j (array-dimension estado 1))
                (if (not (equalp (aref estado i j) (aref *estado-final* i j)))
                    (incf value))))
        value))


(defun distancia-a-posicao-correta (estado)
    (let ((posicao (encontra-posicao estado)))
        (+ (- 3 (linha-posicao posicao)) (- 3 (coluna-posicao posicao)))))


;;; Problem solver

(defun resolve-problema (estado-inicial &optional (tipo-procura "profundidade"))
    (let ((problema (cria-problema
                      estado-inicial
                      (list #'mover-cima #'mover-esquerda #'mover-direita #'mover-baixo)
                      :estado-final *estado-final*
                      :heuristica #'posicoes-fora-do-sitio
                      :estado= #'equalp)))
        (procura problema tipo-procura)))

(setf dummy-estado (make-array '(4 4)
                                :initial-contents '((1 2 3 4)
                                                    (5 6 7 8)
                                                    (13 9 10 11)
                                                    (14 nil 15 12))))
(setf hard-estado (make-array '(4 4)
                                :initial-contents '((nil 15 14 13)
                                                    (12 11 10 9)
                                                    (8 7 6 5)
                                                    (4 3 2 1))))
