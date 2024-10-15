(ns interp
  (:require [grammars :as wae]
            [parser :as ps]
            ))

(defn interp-AE
  "Interprete de AE"
  [exp]
  (let [par (ps/parser-AE exp)]
    (cond
      (instance? ae/NumG par)
      (:n par)

      (instance? ae/AddG par)
        (+ (interp-AE (:izq par)) (interp-AE (:der par)))
        
        (instance? ae/SubG par)
        (- (interp-AE (:izq par)) (interp-AE (:der par)))
        )
      )
    )


(defn subst
[exp sub_id val]
(cond
  (instance? wae/IdG exp)
  (if (= (:i exp) sub_id)
    val exp
  )
  (instance? wae/NumG exp)
  exp
  (instance? wae/AddG exp)
  (wae/addG (subst (:izq exp) sub_id val) (subst (:der exp) sub_id val))
  (instance? wae/SubG exp)
  (wae/subG (subst (:izq exp) sub_id val) (subst (:der exp) sub_id val))
  (instance? wae/WithG exp)
  (let [binding (:assign exp)]
    (if (= (:id binding) sub_id)
      (wae/withG (wae/bindings (:id binding) (subst (:value binding) sub_id val))(:body exp))
      (wae/withG (wae/bindings (:id binding) (subst (:value binding) sub_id val))(subst (:body exp) sub_id val))
      )
    )
  )
  )


(defn interp-WAE
  "Interprete de WAE"
  [exp]
  (let [par (ps/parser-WAE exp)]
    (cond
      (instance? wae/NumG par)
      (:n par)
      (instance? wae/IdG par)
      (throw (IllegalArgumentException. "Oh OH, identificador libre"))
      (instance? wae/AddG par)
        (+ (interp-WAE (:izq par)) (interp-WAE (:der par)))
        
        (instance? wae/SubG par)
        (- (interp-WAE (:izq par)) (interp-WAE (:der par)))
        
        (instance? wae/WithG par)
        (let [binding (:assign par)]
          (interp-WAE (subst (:body par) (:id binding) (interp-WAE (:value binding))))
          )

        )
      )
    )



