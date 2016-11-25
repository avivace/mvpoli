;; Ispetioning monomials

(defun monomial-degree (x)
  (third x))

(defun monomial-coefficient (x)
  (second x))

(defun varpowers (x)
    (fourth x))

(defun var-of (x)
  (let ((a (varpowers x)))
    (var-of-helper a)))

(defun var-of-helper (x)
  (if (eql 'nil (cdr x))
      (third (car x))
      (list (third (car x)) (var-of-helper (cdr x)))))

;;; Ispetioning polynomials

(defun monomials (x)
  (second x))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun variables-helper (x)
  (if (not (eql '() x))
      (cons (var-of (car x)) (variables-helper (cdr x)))))


(defun variables (x)
  (remove-duplicates (flatten (variables-helper (second x)))))

(defun coefficients (x)
  (flatten (coefficients-helper (second x))))

(defun coefficients-helper (x)
  (if (not (eql '() x))
      (cons (monomial-coefficient (car x)) (coefficients-helper (cdr x)))))

(defun maxdegree (x)
  (apply 'max (poly-degrees (second x))))

(defun mindegree (x)
  (apply 'min (poly-degrees (second x))))

(defun poly-degrees (x)
  (if (not (eql '() x))
      (cons (monomial-degree (car x)) (poly-degrees (cdr x)))))

;;; Parsing and normalization

;; da fixare : monomio non normalizzato in caso di variabili uguali
(defun as-monomial (x) 
  (cond ((integerp x)
         (list 'm x 0 'nil))
        ((and (atom x) (symbolp x)) 
              (list 'm 1 1 (list 'v 1 x)))
        ((eql (car x) '*) 
         (if (integerp (second x))
             (let ((a (as-monomial-helper (cdr (cdr x)) 0)))         
               (list 'm (second x) (first (last a))
                     (sort (butlast a) #' string-lessp :key #' third)))
             (let ((a (as-monomial-helper (cdr x) 0)))         
               (list 'm 1 (first (last a))
                     (sort (butlast a) #' string-lessp :key #' third)))))))


(defun as-monomial-helper (x acc)
  (if (eql 'nil x) 
        (list acc)
      (cond ((and (atom (car x)) (symbolp (car x)))
            (cons (list 'v 1 (car x)) 
                  (as-monomial-helper (cdr x) (+ acc 1)))) 
            (T (cons (list 'v (third (car x)) (second (car x)))
              (as-monomial-helper (cdr x) (+ acc (third (car x)))))))))

;; da fixare : i monomi devono essere ordinati lessicalmente a parit� di grado       
(defun as-polynomial (x)
  (if (eql (car x) '+)
      (let ((a (as-polynomial-helper (cdr x))))
        (list 'p (sort a #'> :key #' second)))))
(defun as-polynomial-helper (x) 
  (if (eql 'nil (cdr x))
      (list (as-monomial (car x)))
      (cons (as-monomial (car x)) (as-polynomial-helper (cdr x)))))
      

;;; Checking

(defun is-monomial (m)
  (and (listp m)
       (eq 'm (first m))
       (let ((mtd (monomial-degree m))
             (vps (varpowers m))
             )
         (and (integerp mtd)
              (>= mtd 0)
              (listp vps)
              (every #'is-varpower vps)))))

(defun is-varpower (vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (second vp))
             (v (third vp))
             )
         (and (integerp p)
              (>= p 0)
              (symbolp v)))))

(defun is-polynomial (p)
  (and (listp p)
       (eq 'p (first p))
       (let ((ms (monomials p)))
         (and (every #'is-monomial ms)))))


;; to do

;; pprint-polynomial
;; polyval
;; polyminus
;; polytimes
;; polyplus

