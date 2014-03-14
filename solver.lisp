; (compile-file "procura.lisp")
; (load "procura")

;;; Structure to hold the current state

(defstruct fp-estado
    estado
    posicao)

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

;;; Move up

(defun mover-cima-p (posicao)
    (if (>= (1- (linha-posicao posicao)) 0)
        t
        nil))

(defun mover-cima (estado)
    (let* ((posicao (encontra-posicao estado))
          (valor nil)
          (linha-futura (1- (linha-posicao posicao))))
        (when (mover-cima-p posicao)
               (setf valor (aref estado linha-futura (coluna-posicao posicao)))
               (setf (aref estado linha-futura (coluna-posicao posicao)) nil)
               (setf (aref estado (linha-posicao posicao) (coluna-posicao posicao)) valor)
               (setf (car posicao) linha-futura))
        (print (list estado))
        (list estado)))

;;; Move down

(defun mover-baixo-p (posicao)
    (if (< (1+ (linha-posicao posicao)) 4)
        t
        nil))

(defun mover-baixo (estado)
    (let* ((posicao (encontra-posicao estado))
          (valor nil)
          (linha-futura (1+ (linha-posicao posicao))))
        (when (mover-baixo-p posicao)
               (setf valor (aref estado linha-futura (coluna-posicao posicao)))
               (setf (aref estado linha-futura (coluna-posicao posicao)) nil)
               (setf (aref estado (linha-posicao posicao) (coluna-posicao posicao)) valor)
               (setf (car posicao) linha-futura))
        (print (list estado))
        (list estado)))

;;; Move esquerda

(defun mover-esquerda-p (posicao)
    (if (>= (1- (coluna-posicao posicao)) 0)
        t
        nil))

(defun mover-esquerda (estado)
    (let* ((posicao (encontra-posicao estado))
          (valor nil)
          (coluna-futura (1- (coluna-posicao posicao))))
        (when (mover-esquerda-p posicao)
               (setf valor (aref estado (linha-posicao posicao) coluna-futura))
               (setf (aref estado (linha-posicao posicao) coluna-futura) nil)
               (setf (aref estado (linha-posicao posicao) (coluna-posicao posicao)) valor)
               (setf (cadr posicao) coluna-futura))
        (print (list estado))
        (list estado)))

;;; Move direita

(defun mover-direita-p (posicao)
    (if (< (1+ (coluna-posicao posicao)) 4)
        t
        nil))

(defun mover-direita (estado)
    (let* ((posicao (encontra-posicao estado))
          (valor nil)
          (coluna-futura (1+ (coluna-posicao posicao))))
        (when (mover-direita-p posicao)
               (setf valor (aref estado (linha-posicao posicao) coluna-futura))
               (setf (aref estado (linha-posicao posicao) coluna-futura) nil)
               (setf (aref estado (linha-posicao posicao) (coluna-posicao posicao)) valor)
               (setf (cadr posicao) coluna-futura))
        (print (list estado))
        (list estado)))

(defun resolve-problema (estado-inicial &optional (tipo-procura "profundidade"))
    (let ((problema (cria-problema
                      estado-inicial
                      (list #'mover-cima #'mover-esquerda #'mover-direita #'mover-baixo)
                      :estado-final (make-array '(4 4)
                                      :initial-contents '((1 2 3 4)
                                                          (5 6 7 8)
                                                          (9 10 11 12)
                                                          (13 14 15 nil)))
                      :estado= #'equalp)))
        (procura problema tipo-procura)))

(setf dummy-estado (make-array '(4 4)
                                :initial-contents '((1 2 3 4)
                                                    (5 6 7 8)
                                                    (13 9 10 11)
                                                    (14 nil 15 12))))