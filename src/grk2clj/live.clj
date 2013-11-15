(ns grk2clj.live
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [clojure.core.match :only [match]])
  (:use [grk2clj.supp])
  (:use [clojure.test]))

(defn nato [n]
  (conde
   [(== n 'z)]
   [(fresh [n-1]
           (== n `(~'s ~n-1))
           (nato n-1))]))

(run* [q] (nato 'z))
(run* [q] (nato '(s z)))
(run* [q] (nato 1))

(run 10 [q] (nato q))

(defn plus [n1 n2]
  (match [n1]
         ['z] n2
         [(['s n1-1] :seq)]
         `(~'s ~(plus n1-1 n2))))

(plus 'z '(s z))
(plus '(s z) '(s z))
(plus '(s (s z)) '(s z))

(defn pluso [n1 n2 n3]
  (conde
   [(== n1 'z) (== n2 n3)]
   [(fresh [n1-1 n3-1]
           (== n1 `(~'s ~n1-1))
           (== n3 `(~'s ~n3-1))
           (pluso n1-1 n2 n3-1))]))

(run* [q] (pluso 'z '(s z) q))
(run* [q] (pluso q '(s z) '(s (s z))))
(run 10 [q] (fresh [a b c]
                   (== q [a b c])
                   (pluso a b c)))

(defn env-lookupo [env x tx]
  (conde
   [(fresh [envr]
           (conso [x tx] envr env))]
   [(fresh [envr y ty]
           (conso [y ty] envr env)
           (!= x y)
           (env-lookupo envr x tx))]))

(run* [q] (env-lookupo '([x 1] [y 1]) 'x q))
(run* [q] (env-lookupo '([x 1] [x 2]) 'x q))

(defn tpo [env e t]
  (conde
   [(symo e) (env-lookupo env e t)]
   [(fresh [x eb t1 t2]
           (== e `(~'fn [~x] ~eb))
           (== t `(~t1 ~'=> ~t2))
           (tpo (lcons [x t1] env) eb t2))]
   [(fresh [e1 e2 t1]
           (== e `(~e1 ~e2))
           (tpo env e1 `(~t1 ~'=> ~t))
           (tpo env e2 t1))]))

(run* [q] (tpo '() '(fn [x] x) q))
(run 10 [q] (fresh [a b]
                   (== q [a b])
                   (tpo '() a b)))
(run 10 [q] (fresh [a b]
                   (== q [a b])
                   (tpo '() a '(A => A))))
