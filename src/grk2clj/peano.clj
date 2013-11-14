(ns grk2clj.peano
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [clojure.core.match :only [match]])
  (:use [grk2clj.supp])
  (:use [clojure.test]))

(defn nato [n]
  (conde
   [(== n 'z)]
   [(fresh (n1)
           (== n `(~'s ~n1))
           (nato n1))]))

(is (= (run* [q] (nato 'z))
       '(_0)))
(is (= (run* [q] (nato '(s z)))
       '(_0)))
(is (= (run* [q] (nato 1))
       '()))
(is (= (run 10 [q] (nato q))
'(z
 (s z)
 (s (s z))
 (s (s (s z)))
 (s (s (s (s z))))
 (s (s (s (s (s z)))))
 (s (s (s (s (s (s z))))))
 (s (s (s (s (s (s (s z)))))))
 (s (s (s (s (s (s (s (s z))))))))
 (s (s (s (s (s (s (s (s (s z))))))))))))

(defn plus [n1 n2]
  (match [n1]
         ['z] n2
         [(['s n1-1] :seq)]
         `(~'s ~(plus n1-1 n2))))

(is (= (plus '(s z) '(s (s z)))
       '(s (s (s z)))))

(defn pluso [n1 n2 n3]
  (conde
   [(== n1 'z) (== n2 n3)]
   [(fresh (n1-1 n3-1)
           (== n1 `(~'s ~n1-1))
           (== n3 `(~'s ~n3-1))
           (pluso n1-1 n2 n3-1))]))

(is (= (run* [q] (pluso '(s z) '(s (s z)) q))
       '((s (s (s z))))))

(is (= (run* [q] (pluso q '(s z) '(s (s z))))
       '((s z))))

