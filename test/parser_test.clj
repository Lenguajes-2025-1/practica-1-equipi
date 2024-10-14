(ns parser-test

  (:require [grammars :as gr]
            [parser :as sut]
            [clojure.test :as t]))

(t/deftest parserAE-test
    (t/testing "Num"
        (t/is (.equals (gr/numG 2) (sut/parser-AE '2))))
  (t/testing "Suma"
        (t/is (.equals (gr/addG (gr/numG 2) (gr/numG 4)) (sut/parser-AE '(+ 2 4)))))
    (t/testing "Resta Anidada"
        (t/is (.equals (gr/subG (gr/subG (gr/numG 7) (gr/numG 8)) (gr/numG 4)) (sut/parser-AE '(- (- 7 8) 4)))))
    (t/testing "Resta y Suma Anidada"
        (t/is (.equals (gr/subG (gr/subG (gr/numG 7) (gr/numG 8)) (gr/addG (gr/numG 11) (gr/numG 122))) (sut/parser-AE '(- (- 7 8) (+ 11 122))))))
)

(t/deftest parserWAE-test
  (t/testing "ID"
    (t/is (.equals (gr/idG 'x) (sut/parser-WAE 'x))))
  (t/testing "With"
    (t/is (.equals (gr/withG (gr/bindings 'x (gr/numG 2)) (gr/addG (gr/idG 'x) (gr/numG 2))) 
          (sut/parser-WAE '(with [x 2] (+ x 2))))))
  (t/testing "With Anidado"
    (t/is (.equals (gr/withG (gr/bindings 'y (gr/numG 2)) 
                        (gr/withG (gr/bindings 'x (gr/numG 3))
                            (gr/addG (gr/idG 'x) (gr/numG 2)))) 
          (sut/parser-WAE '(with [y 2] (with [x 3] (+ x 2))))))))


