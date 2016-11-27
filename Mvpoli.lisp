;; Ispetioning monomials

(defun monomial-degree (x)
  (third x))

(defun monomial-coefficient (x)
  (second x))

(defun varpowers (x)
    (fourth x))

(defun var-of (x)
  (let ((a (varpowers x)))
    (flatten (var-of-helper a))))

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
  (sort (remove-duplicates (flatten (variables-helper (second x)))) #'string-lessp ))

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

(defun as-monomial (x) 
  (cond ((integerp x)
         (list 'm x 0 'nil))
        ((and (atom x) (symbolp x)) 
              (list 'm 1 1 (list 'v 1 x)))
        ((eql (car x) '*) 
         (if (integerp (second x))
             (let ((a (as-monomial-helper (cdr (cdr x)) 0)))         
               (list 'm (second x) (first (last a))
                    (m-normalizer 
                     (sort (butlast a)
                           #' string-lessp :key #' third))))
             (let ((a (as-monomial-helper (cdr x) 0)))         
               (list 'm 1 (first (last a))
                     (m-normalizer 
                      (sort (butlast a) 
                            #' string-lessp :key #' third))))))))


(defun as-monomial-helper (x acc)
  (if (eql 'nil x) 
        (list acc)
      (cond ((and (atom (car x)) (symbolp (car x)))
            (cons (list 'v 1 (car x)) 
                  (as-monomial-helper (cdr x) (+ acc 1)))) 
            (T (cons (list 'v (third (car x)) (second (car x)))
              (as-monomial-helper (cdr x) (+ acc (third (car x)))))))))


(defun m-normalizer (x)
  (cond ((eql (car x) 'nil) 'nil)
        ((eql (cdr x) 'nil) x)
        ((eql (third (first x)) (third (second x)))
         (let ((a (+ (second (first x)) (second (second x)))))
           (if (eql a 0)
               (m-normalizer (cdr (cdr x)))
             (m-normalizer (cons (list 'm a (third x) (varpowers x))
                                 (cdr (cdr x)))))))
        (T (cons (car x) (m-normalizer (cdr x))))))

      
(defun as-polynomial (x)
  (if (eql (car x) '+)
      (let ((a (as-polynomial-helper (cdr x))))
        (list 'p (p-norm (sort a #'> :key #'third ))))))

(defun as-polynomial-helper (x) 
  (if (eql 'nil (cdr x))
      (list (as-monomial (car x)))
      (cons (as-monomial (car x)) (as-polynomial-helper (cdr x)))))

(defun p-norm (x)
  (cond ((eql (car x) 'nil) 'nil)
        ((eql (cdr x) 'nil) x)
        ((and (eql (third (first x)) (third (second x)))
              (equal (varpowers (first x)) (varpowers (second x))))
         (let ((a (first x)) (b (second x)))
           (if (eql (+ (second a) (second b)) 0)
               (p-norm (cdr (cdr x)))
             (p-norm (cons (list 'm (+ (second a) (second b))
                                 (third a) (fourth a)) 
                           (cdr (cdr x)))))))
        (T (cons (car x) (p-norm (cdr x))))))
           


;; polynomials' operations

(defun polyval (x y)
  (cond ((eql (first x) 'p)
         (polyval-helper1 x y))
        ((eql (first x) '+)
         (let ((a (as-polynomial x)))
           (polyval-helper1 a y)))
        ((eql (first x) '*)
         (let ((a (as-polynomial (list '+ x))))
           (polyval-helper1 a y)))))

(defun polyval-helper1 (x y)
  (polyval-helper2 (second x) (var-values (variables x) y) 0))

(defun polyval-helper2 (x y acc)
  (if (not (eql (car x) 'nil))
      (let ((a (+ acc (monomialval (car x) y))))
        (polyval-helper2 (cdr x) y a))
      acc))
 

(defun find-val (x y)
  (if (eql (car (car y)) x)
      (cdr (car y))
      (find-val x (cdr y))))
      
(defun monomialval (x y)
  (if (not (eql (second x) 1))
      (* (second x) (monomialval-helper (fourth x) y 1))
      (monomialval-helper (fourth x) y 1)))

(defun monomialval-helper (x y acc)
  (if (not (eql (car x) 'nil))
      (let ((a (car x))
            (b (find-val (third (car x)) y)))
        (if (> (second a) 1)
            (let ((c (* acc (expt b (second a)))))
              (monomialval-helper (cdr x) y c))
            (let ((d (* acc b)))
              (monomialval-helper (cdr x) y d))))
      acc))
          
(defun var-values (x y)
  (if (not (eql (car x) 'nil))
      (cons (cons (car x) (car y)) 
            (var-values (cdr x) (cdr y)))))
  
(defun polyplus (x y)
  (if (and (eql 'p (car x)) (eql 'p (car x)))
      (let ((a (append (car (cdr x)) (car (cdr y)))))
        (list 'p (p-norm a)))))

(defun polyminus (x y)
  (if (and (eql 'p (car x)) (eql 'p (car x)))
      (let ((a (append (car (cdr x)) 
                       (p-coefficientchange (car (cdr y)) (- 1)))))
        (list 'p (p-norm a)))))

(defun p-coefficientchange (x y)
  (if (not (eql (car x) 'nil))
      (cons (list 'm (* (second (car x)) y) 
                  (third (car x)) (fourth (car x)))
            (p-coefficientchange (cdr x) y))))

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

;;printing 

(defun pprint-polynomial (x)
  (pprint-polynomial-helper (second x)))

(defun pprint-polynomial-helper (x)
  (cond ((eql (cdr x) 'nil)
         (pprint-monomial (car x)))
        (T (pprint-monomial (car x))
            (prin1 '+)
            (pprint-polynomial-helper (cdr x)))))

(defun pprint-monomial (x) 
  (if (not (eql (second x) 1)) 
        (prin1 (second x)))
  (if (> (third x) 0)
      (pprint-monomial-helper (fourth x))))

(defun pprint-monomial-helper (x)
  (if (not (eql (car x) 'nil))
      (let ((a (car x)))
        (prin1 (third a))
        (cond ((> (second a) 1)
               (prin1 '^) 
               (prin1 (second a))))
        (pprint-monomial-helper (cdr x)))))
  
;; to do

;; polyminus
;; polytimes
;; polyplus

