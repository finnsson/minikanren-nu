; trying to convert https://github.com/miniKanren/miniKanren/blob/master/mk.scm


; missing helper methods in Nu

; dot notation
(macro d (f g) `(let (__x ,f) (,g __x) ) )

; return the element from list "l" at index "i"
(function element-at (l i)
  (case i
    (0 (head l))
    (else (element-at (tail l) (- i 1)))
  )
)

(function cadr (v) (car (tail v)))

(function caddr (v) (car (tail (tail v))))

; check if "v" is a do-block. Return 1 if do-block, else 0
; TODO: check for NuClassOperator (e.g. send)?
; returns the "baked" do-block, else nil
(function dos? (v) (or (send v isKindOfClass:(NuBlock class)) (and (send v isKindOfClass:(NuSymbol class) (send (v evalWithContext:(context)) isKindOfClass:(NuBlock class )) ))))

(function do?? (v)
  (cond
    ; return v if v is a NuBlock
    ( (do? v) v )
    ; return v's NuBlock if v is a NuSymbol that represents a NuBlock
    ( (and (send v isKindOfClass:(NuSymbol class) (send (v evalWithContext:(context)) isKindOfClass:(NuBlock class )) )) (v evalWithContext:(context))  )
    ; else return nil
    (else nil)
  )
)

(function do? (v) (send v isKindOfClass:(NuBlock class)))

; returns a DoBlock if v or (v) is a do-block or a symbol representing a DoBlock
(function do1? (v)
  (cond
    ( (do?? v) (do?? v) )
    ( (and (not (atom v)) (eq 1 (v length)) ) (do?? (car v) ) )
    (else nil)
  )
)


; here starts the convertion

(set empty-c '(() () () ()))

; lambdag@
; lambdag x = do x

; this might be wrong...
(macro lambdag@ (c S D A T e)
  `(do (v)
    (set ,S (element-at v 0) )
    (set ,D (element-at v 1) )
    (set ,A (element-at v 2) )
    (set ,T (element-at v 3) )
    ,e
  )
)

; mzero takes no arguments, returns nil/false
(function mzero () () )

; unit takes 1 argument, returns same argument
(function unit (c) c )

; choice is cons ?
(function choice (c f) (cons c f))

; lambdaf@
; (lambdaf@ nil x) == (inc x)

; inc is baking "e" into a lambda - increasing the number of calls needed
(macro inc (e) `( do () ,e ) )

; empty-f is a lambda that returns nil
(set empty-f (inc nil))


(macro case-inf (v a b c d)
  `(
    (let ((__c-inf ,v) )
    (cond
      ((not __c-inf) ,(cadr a))
      ((do? __c-inf) (let (,(car b) __c-inf) ,(cadr b) ))
      ((not (and (pair? __c-inf)
        (do1? (cadr __c-inf))))
        (let (,(car c) __c-inf) ,(cadr c)))
      (else ( let (( ,(car (car d)) (car __c-inf)) (,(cadr (car d)) (cadr __c-inf))) ,(cadr d))) 
    ))
  )
)

; TODO: move to test file
;(case-inf (list 4 do?)
;  (() (puts "nil")); if nil
;  (f (puts "in f")); if do-block
;  (c (puts "in c")); if value?
;  ((c f) (puts (+ "f(c): " (f c) )))
;)

; run - wait for fresh
(macro run (n x g0 *g)
  (take n
    (do ()
      ((fresh (x) g0 ,@g
        (do (final-c)
          (let ((z ((reify x) final-c)))
            (choice z empty-f)
          )
        )
      ) empty-c )
    )
  )
)


; run* wait for run
(macro run* (x *gs)
  (run nil x ,@*gs)
)

; take
(function take (n f)
  (cond
    ((zero? n) nil)
    (else
      (case-inf (f)
        (() nil)
        (f (take n f))
        (c (cons c nil))
        ((c f) (cons c (take (- n 1) f)))
      )
    )
  )
)

; fresh
(macro fresh (bindings g0 *gs)
  `(do c
    (inc
      (let
        ( ,(bindings map: (do (x) (list x array `',x ) ) ) ) 
        (bind* (,g0 c) ,@*gs)
      )
    )
  )
)

; bind*
; calling "bind" in recursion on every argument
; 1 arg => return arg
; 2 arg => return bind arg
; 3 arg => return (bind (bind arg1 arg2) arg3)
; 4 arg => return (bind (bind (bind arg1 arg2) arg3) arg4)
(macro bind* (*body)
  (set arg-length (*body length) )
  (case arg-length
    (1 `(,@*body) )
    (else `(bind* (bind ,(car *body) ,(cadr *body) ) ,@(tail (tail *body))  ) )
  )
)

; bind
(function bind (c-inf g)
  (case-inf c-inf
    (() (mzero) )
    (f (inc (bind (f) g)) )
    (c (g c) )
    ( (c f) (mplus (g c) (inc (bind (f) g) ) ) )
  )
)

; mplus
(function mplus (c-inf f)
  (case-inf c-inf
    (() (f))
    (f* (inc (mplus (f) f*)))
    (c (cons c f))
    ((c f*) (cons c (inc (mplus (f) f*))))

  )
)

; line 103
; conde
(macro conde ((g0 *gs) (g1 *ghs) *as)
  `(do (x)
    (inc (mplus* (bind* (,g0 ,c) ,@*gs)
                 (bind* (,g1 ,c) ,@*ghs) ,@*as )
    )
  )
)

; line 109
; mplus*
(macro mplus* (*es)
  ; if (es length == 1)
  (case (*es length)
    (1 `,@*es)
    ; else
    (else `(mplus ,(car es) (do () (mplus* ,@( cdr *es ))) ) )
  )
)


; line 114
; case-value
(macro case-value (u ((t1) e0) ((at dt) e1) ((t2) e2))
  `(let ((t ,u))
    (cond
      ((var? t) (let ((,t1 t)) ,e0))
      ((pair? t) (let ((,at (car t)) (,dt (cadr t))) ,e1))
      (else (let ((,t2 t)) ,e2))
    )
  )
)

; make-tag-A - TODO: wait for make-tag-A+

; make-tag-A+ - TODO: wait for subsume and subsume-A

; subsume-A TODO: wait for subsume-A+
(function subsume-A (S D A T)
  (let ((x* (rem-dups (map lhs A))))
    (subsume-A+ x* S D A T))
)

; subsume-A+
(function subsume-A+ (x* S D A T)
  (cond
    ((null? x*) `(,S ,D ,A ,T))
    (else (let ((x (car ,x*)))
            (let ((D-T (update-D-T s S D A T)))
              (let ((D (car D-T)) (T (cdr D-T)))
                `(,S ,D ,A ,T)
              )
            )
          )
    )
  )
)

; line 163
; ext-A
(function ext-A (x tag pred S A)
  (cond
    ((null? A) `((,x . (,tag . ,pred))))
    (else
      (let ((a (car A)) (A (cdr A)))
        (let ((a-tag (pr->tag a)))
          (cond
            ((eq? (walk (lhs a) S) x)
             (cond
               ((tag=? a-tag tag) '())
               (else nil)
             ))
            (else (ext-A x tag pred S A) )
          )
        )
      )
    )
  )
)

; unify
(function unify (u v S)
  (let ((u (walk u S)) (v (walk v S)))
    (cond
      ((and (pair? u) (pair? v))
        (let ((S (unify (car u) (car v) S)))
          (and S (unify (cdr u) (cdr v) S ))))
      (else (unify-nonpair u v S))
    )
  )
)

; unify-nonpair
(function unify-nonpair (u v S)
  (cond
    ((eq? u v) S)
    ((var? u) (ext-S u v S))
    ((var? v) (ext-S u v S))
    (else nil)
  )
)

; ext-S
(function ext-S (x v S)
  (case-value v
    ((y) (cons `(,x . ,y) S))
    ((au du) (cond
      ((occurs-check x v S) nil)
      (else (cons `(,x . ,v) S))))
    ((v) (cons `(,x . ,v) S )))
)

; occurs-check
(function occurs-check (x v S)
  (case-value (walk v S)
    ; y is var
    ((y) (eq? y x))
    ; walk v S is a pair
    ((av dv) (or (occurs-check x av S)
                 (occurs-check x dv S)))
    ; else
    ((v) nil))
)

; walk* - wait for case-value


; reify - wait for walk*, reify-S, 

; line 187
; pr->tag
(function pr->tag (pr) (car (rhs pr)))

; line 189
; pr->pred
(function pr->pred (pr) (cdr (rhs pr)))

; line 191
; =/= - wait for post-unify-=/=

; post-unify-=/= - wait for prefix-S, unit

; line 209
; prefix-S
(function prefix-S (S+ S)
  (cond
    ((eq? S+ S) nil)
    (else (cons (car S+) (prefix-S (cdr S+) S)))
  )
)

; line 215
; subsume
(function subsume (ApT D)
  (remp (do (d) (exists (subsumed-pr? ApT) d)) D)
) 

; line 220
; subsumed-pr?
(function subsumed-pr? (ApT)
  (do (pr-d)
    (let ((u (rhs pr-d)))
      (cond
        ((var? u) nil)
        (else
          (let ((pr (assq (lhs pr-d) ApT)))
            (and pr
              (let ((tag (pr->tag pr)))
                (cond
                  ((and (tag? tag) (tag? u) (tag=? u tag)))
                  (((pr->pred pr) u) nil)
                  (else t)
                )
              )
            )
          )
        )
      )
    )
  )
)


; line 237
; == TODO: post-unify-==


; line 245
; post-unify-== TODO: post-verify-D
(function post-unify-== (c S D A T)
  (do (S+)
    (cond
      ((eq S+ S) (unit c))
      (else
        (let (verified ((verify-D D S+)))
          (cond
            (verified
              (let ((post-verified (post-verify-D S+ D A T)))
                (cond
                  (post-verified (unit post-verified))
                  (else (mzero))
                )
              )
            )
            (else (mzero))
          )
        )
      )
    )
  )
)

; line 257
; verify-D
(function verify-D (D S)
  (cond
    ((null? D) nil)
    (else
      (let ((D+ (verify-D (cdr D) S)))
        (cond
          (D+ (verify-D+ (car D) D+ S))
          (else nil)
        )
      )
    )
  )
)

; line 266
; verify-D+
(function verify-D+ (d D S)
  (let ((S+ (unify* d S) ))
    (cond
      (S+ (cond
        ((eq? S+ S) nil)
        (else (cons (prefix-S S+ S) D) )
      ))
    )
  )
)

; line 276
; post-verify-D
(function post-verify-D (S D A T)
  (let (verified (verify-A A S) )
    (cond
      (verified ((post-verify-A S D T) verified))
      (else nil)
    )
  )
)


; verify-A
(function verify-A (A S)
  (cond
    ((null? A) nil)
    (else
      (set A0 (verify-A (cdr A) S))
      (cond
        ((A0)
          (let ((u (walk (lhs (car A)) S))
                (tag (pr->tag (car A)))
                (pred (pr->pred (car A))))
            (cond
              ((var? u)
               (set A+ (ext-A u tag pred S A0))
               (cond
                 ((A+) append A+ A0)
                 (else nil)
               )
              )
              (else (and (pred u) A0 ) )
            )
          )
        )
        (else nil)
      )
    )
  )
)

; line 302
; post-verify-A
(function post-verify-A (S D T)
  (do (A)
    (let ((D (subsume A D)) (verified (verify-T T S))  )
      (cond
        (verified ( (post-verify-T S D A) verified ))
        (else nil) 
      )
    )
  )
)


; line 310
; verify-T
(set verify-T 
  (do (T S)
    (cond
      ((null? T) nil)
      ((verify-T (cdr T) S) => (verify-T+ (lhs (car T)) T S))
      (else nil))))

; line 317
; verify-T+
(function verify-T+ (x T S)
  (do (T0)
    (let ((tag (pr->tag (car T)))
            (pred (pr->pred (car T))))
	(case-value (walk x S)
	  ((x) (cond
                 ((ext-T+ x tag pred S T0) =>
                  (do (T+) (append T+ T0)))
                 (else nil)))
          ((au du) (cond
                     (((verify-T+ au T S) T0) =>
                      (verify-T+ du T S))
                     (else nil)))
          ((u) (and (pred u) T0)))
    )
  )
)

; line 333
; post-verify-T
(function post-verify-T (S D A)
  (subsume-T T S (subsume T D) A nil)
)

; line 338
(function subsume-T (T+ S D A T)
  (let ((x* (rem-dups (map lhs A))))
    (subsume-T+ x* t+ S D A T)
  )
)

; line 343
(function subsume-T+ (x* T+ S D A T)
  (cond
    ((null? x*)
      (let ((T (append T+ T)))
        `(,S ,D ,A ,T))
    )
    (else
      (let ((x (car x*)) (x* (cdr x*)))
        (let ((DpT (update-DpT x D S A T+)))
          (let ((D (car DpT)) (+ (cdr DpT)))
            (subsume-T+ x* T+ S D A T)
          )
        )
      )
    )
  )
)

; line 457
; ext-T+
(function ext-T+ (x tag pred S T)
  (cond
    ((null? T) `((, x . (,tag . ,pred))))
    (else
      (let ((t (car t)))
        (let ((t-tag (pr->tag t)))
          (cond
            ((eq? (walk (lhs t) S) x)
              (cond
                ((tag=? t-tag tag) '())
                (else
                  (ext-T+ x tag pred S (cdr T))
                )
              )
            )
            (else
              (ext-T+ x tag pred S (cdr T))
            )
          )
        )
      )
    )
  )
)

; line 475
; make-pred-T
(function make-pred-T (tag)
  (do (x)
    (not (and (tag? x) (tag=? x tag)))
  )
)

; line 480
; tag?
(function tag? (tag)
  (symbol? tag)
)

; line 484
; tag=?
; same as eq?
(function tag=? (tag1 tag2)
  (eq? tag1 tag2)
)

; line 488
(function var (dummy) (array dummy))

; line 490
(function var? (v) (send v isKindOfClass:(NSArray class)))

; line 501
; lhs (left-hand-side) is the same as car
(function lhs (pr) (car pr))

; line 503
; rhs (right-hand-side) is the same as cdr
(function rhs (pr) (cdr pr))

; line 509
; walk
(function walk (u S)
  (cond
    ((and (var? u) (assq u S)) =>
      ( do (pr) (walk (rhs pr) S ))
    )
  )
)


; line 704
; unify*
(function unify* (S+ S)
  (unify (S+ map:lhs) (S+ map:rhs) S)
)

; move to scheme.nu

; see http://web.mit.edu/scheme_v9.0.1/doc/mit-scheme-ref/Association-Lists.html
(function assq (key hash)
  (do (key hash) ((hash find: (do (e) (eq (car e) key)))))
)

(function remp (proc l)
  (l select:(do (i) (not (proc i))  ))
)

(function exists (proc l)
  (l find:proc)
)

(set any exists)
