(load "mk")

(class TestHelpers is NuTestCase

  ; test d macro
  (- (id) test-d is
    (function double (x) (* 2 x))
    (assert_equal 14 (d (+ 3 4) double))
  )

)

(class TestTake is NuTestCase
  ; test nil (take 0 (do () 4))
  (- (id) test-nil is
    (assert_equal nil (take 0 (do () 4)))
  )

  ; test value
  (- (id) test-value is
    (assert_equal (list 4) (take 1 (do () 4)))
  )

  ; test trying to get 4 values
  (- (id) test-more-values is
    (assert_equal (list 3) (take 3 (do () 3)))
  )

  ; test do in do 42
  (- (id) test-do is
    (assert_equal (list 42) (take 8 (do () (do () 42))))
  )


  ; test (value do)
  (- (id) test-value-do is
    (assert_equal (list 1 2) (take 2 (do () (list 1 (do () 2)))))
  )

  ; test (value do) in 5 levels - taking 3
  (- (id) test-value-do-in-5-levels-taking-3 is
    (assert_equal (list 1 2 3) (take 3 (do () (list 1 (do () (list 2 (do () (list 3 (do () (list 4 (do () list 5)))))))))))
  )

  ; test (value do) in 3 levels taking 5
  (- (id) test-value-do-in-4-levels-taking-5 is
    (assert_equal (list 1 2 3 4) (take 5 (do () (list 1 (do () (list 2 (do () (list 3 (do () 4)))))))))
  )
)

; mplus can take
; mplus nil x => (x)
; mplus x (list y) => x ..y
; mplus (do () x) 
(class TestMplus is NuTestCase
  ; test (mplus nil (do () 4))
  (- (id) test-nil is
    (assert_equal 4 (mplus nil (do () 4)))
  )

  ; (mplus 3 (list 4))
  (- (id) test-mplus-3-list-4 is
    (assert_equal (list 3 4) (mplus 3 (list 4)))
  )

  ; (mplus (do) (do) => 
  (- (id) test-do-pair is
    (assert_equal 5 (car ((mplus (do () (list 4)) (do () 5)))))
  )

  ; TODO: add more tests once I understand what mplus is supposed to do

)

(class TestBind is NuTestCase
  ; test nil
  (- (id) test-nil is
    (assert_equal nil (bind nil 4))
  )

  ; test func
  (- (id) test-func is
    (assert_equal 10 ((bind (do () 5) (do (x) (* 2 x)) )) )
  )

  ; test const
  (- (id) test-const is
    (assert_equal 10 (bind 5 (do (x) (* 2 x)) ) )
  )

  ; test (const func)

)

(class TestBind* is NuTestCase
  ; test nil
  (- (id) test-nil is
    (assert_equal nil (bind* nil 4))
  )

  ; test func
  (- (id) test-func is
    (assert_equal 10 ((bind* (do () 5) (do (x) (* 2 x)) )) )
  )

  ; test const
  (- (id) test-const is
    (assert_equal 10 (bind* 5 (do (x) (* 2 x)) ) )
  )

  ; test func
  (- (id) test-func-to-const is
    (assert_equal 20 ((bind* (do () 5) (do (x) (* 2 x)) (do (x) (* 2 x ) )) ) )
  )

)

(class TestCaseValue is NuTestCase
  ; test array
  (- (id) test-array is
    (assert_equal 4 (case-value (array 2) ((a) (* 2 (a objectAtIndex:0 ))) ((p1 p2) nil ) ((e) nil) ) )
  )

  ; test pair
   ; test array
  (- (id) test-pair is
    (assert_equal 24 (case-value (list 4 6) ((a) (* 2 (a objectAtIndex:0 ))) ((p1 p2) (* p1 p2) ) ((e) nil) ) )
  )

  ; test else
   ; test array
  (- (id) test-else is
    (assert_equal 121 (case-value 11 ((a) (* 2 (a objectAtIndex:0 ))) ((p1 p2) nil ) ((e) (* e e)) ) )
  )
)

(class TestCaseInf is NuTestCase
  ; test false (nil?)
  (- (id) test-false is
    (assert_equal "false" (case-inf nil (() "false") ((f) ("f")) ((c) ("c")) ((a b) ("pair"))))
  )

  ; test (do() )
  (- (id) test-do is
    (assert_equal "f" (case-inf (do () 4) (() "false") ((f) ("f")) ((c) ("c")) ((a b) ("pair"))))
  )


  ; test value
  (- (id) test-value is
    (assert_equal "c" (case-inf 4 (() "false") ((f) ("f")) ((c) ("c")) ((a b) ("pair"))))
  )

  ; test (value do())
  (- (id) test-pair is
    (assert_equal 16 (case-inf (list 4 (do (x) (* x x))) (() "false") ((f) ("f")) ((c) ("c")) ((c f) (f c))))
  )
)
