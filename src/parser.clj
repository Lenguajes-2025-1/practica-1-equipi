(ns parser
  (:require [grammars :as wae]))

(defn parser-AE
  "Parser AE"
  [exp]
  (cond
    (number? exp)
    (wae/numG exp)
    (list? exp)
    (let [x (first exp)]
      (cond
        (= x '+) (wae/addG (parser-AE (second exp)) (parser-AE (nth exp 2)))
        (= x '-) (wae/subG (parser-AE (second exp)) (parser-AE (nth exp 2)))
        :else (throw (IllegalArgumentException. (str "Operador no válido: " x)))))
    :else
    (throw (IllegalArgumentException. "Expresión no válida en AE"))))



(defn parser-WAE
    "Parser WAE"
    [exp]
  (cond
    (number? exp)
    (wae/numG exp)
    (symbol? exp)
    (wae/idG exp)
    (list? exp)
    (let [x (first exp)]
      (cond
        (= x '+) (wae/addG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))
        (= x '-) (wae/subG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))
        (= x 'with) (let [[id val] (second exp)](wae/withG (wae/bindings id (parser-WAE val)) (parser-WAE (nth exp 2)))) 
        :else (throw (IllegalArgumentException. (str "Operador no válido: " x)))))
    :else
    (throw (IllegalArgumentException. "Expresión no válida en AE")))
  )
