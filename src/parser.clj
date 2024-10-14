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
      (case x '+ (wae/addG (parser-AE (second exp)) (parser-AE (nth exp 2)))
                x '- (wae/subG (parser-AE (second exp)) (parser-AE (nth exp 2))) 
                )
      )
    )  
  )

(defn parser-WAE
    "Por implemntar"
    [exp]
    ())
