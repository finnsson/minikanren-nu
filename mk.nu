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

(function var (dummy) (array dummy))

(function var? (v) (send v isKindOfClass:(NSArray class)))

; lhs (left-hand-side) is the same as car
(function lhs (pr) (car pr))

; rhs (right-hand-side) is the same as cdr
(function rhs (pr) (cdr pr))

; walk
(function walk (u S)
  (cond
    ((and (var? u) (assq u S)) =>
      ( do (pr) (walk (rhs pr) S ))
    )
  )
)

; unify

; unify-nonpair

; ext-S

; occurs-check

; walk* - wait for case-value


; reify - wait for walk*, reify-S, 




; move to scheme.nu

; see http://web.mit.edu/scheme_v9.0.1/doc/mit-scheme-ref/Association-Lists.html
(function assq (key hash)
  (do (key hash) ((hash find: (do (e) (eq (car e) key)))))
)


