(ns interp
  (:require [grammars :as wae]
            [parser :as ps]
            ))

(defn interp-AE
  "Interprete de AE"
  [exp]
()
  )

(defn interp-WAE
  "Interprete de WAE"
  [exp]
  (let [par (ps/parser-WAE exp)]
    (cond
      (instance? wae/NumG par)
      (:n par)
      (instance? wae/IdG par)
      (:i par)
      (instance? wae/AddG par)
        (+ (interp-WAE (:izq par)) (interp-WAE (:der par))))
        
        (instance? wae/SubG par)
        (- (interp-WAE (:izq par)) (interp-WAE (:der par)))
        
        (instance? wae/WithG par)
        (let [binding (:assign exp)]
          (str "(with (" (:id binding) " " (print_WAE (:value binding)) ") " (print_WAE (:body exp)) ")"))
      )
    )
)

(defn subst
[exp sub_id val]
(cond
  (instance? wae/IdG exp)
  (if (= (:i exp) sub_id)
    (val)
    )
  (instance? wae/NumG exp)
  (:n exp)
  (instance? wae/AddG exp)
  (wae/addG (subst (:izq exp) ) (interp-WAE (:der exp))))
        
        (instance? wae/SubG exp)
        (- (interp-WAE (:izq exp)) (interp-WAE (:der exp)))
  )
)
