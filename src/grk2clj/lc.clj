(ns grk2clj.lc
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [clojure.core.match :only [match]])
  (:use [grk2clj.supp])
  (:use [clojure.test]))

(comment  
  x, y, z := ;; variable
       <sym>
  c, n := ;; constant
       <num>
  e :=    ;; term
       x         ;; variable
       c         ;; constant
       (fn [x] e) ;; abstraction
       (e1 e2)   ;; application
)

;;; Syntax
(defn lc-varo [x]
  (symo x))
(defn lc-csto [c]
  (numo c))
(defn lc-termo [e]
  (conde
   [(lc-varo e)]
   [(lc-csto e)]
   [(fresh (x eb)
           (== e `(~'fn [~x] ~eb))
           (lc-varo x)
           (lc-termo eb))]
   [(fresh (ea eb)
           (== e `(~ea ~eb))
           (lc-termo ea)
           (lc-termo eb))]))

(is (= (run* [q] (lc-termo 1))
       '(_0)))

(is (= (run* [q] (lc-termo 'x))
       '(_0)))

(is (= (run* [q] (lc-termo '(fn [x] x)))
       '(_0)))

(is (= (run 10 [q] (lc-termo q))
       '((_0 :- (sym _0))
         (_0 :- (num _0))
         ((fn [_0] _1) :- (sym _0) (sym _1))
         ((fn [_0] _1) :- (sym _0) (num _1))
         ((_0 _1) :- (sym _0) (sym _1))
         ((_0 _1) :- (sym _0) (num _1))
         ((fn [_0] (fn [_1] _2)) :- (sym _0) (sym _2) (sym _1))
         ((_0 _1) :- (sym _1) (num _0))
         ((fn [_0] (fn [_1] _2)) :- (sym _0) (sym _1) (num _2))
         ((fn [_0] (_1 _2)) :- (sym _0) (sym _2) (sym _1)))))

;;; Evaluation

(comment
  v := ;; values
     <num>
     (closure env (fn [x] e))
  env := ;; environment
     ([x v] ...)
)

(defn env-lookupo [env x v]
  (conde
   [(fresh [envr]
           (conso [x v] envr env))]
   [(fresh [envr y vy]
           (conso [y vy] envr env)
           (!= x y)
           (env-lookupo envr x v))]))

(is (= (run* [q] (env-lookupo '() 'x q))
       '()))

(is (= (run* [q] (env-lookupo '([x 1] [y 2]) 'x q))
       '(1)))

(is (= (run* [q] (env-lookupo '([x 1] [x 2]) 'x q))
       '(1)))


(defn evo [env e v]
  (conde
   [(lc-varo e) (env-lookupo env e v)]
   [(lc-csto e) (== e v)]
   [(fresh (x eb)
           (== e `(~'fn [~x] ~eb))
           (lc-varo x)
           (== v `(~'closure ~env ~x ~eb)))]
   [(fresh (ea eb va vb envc xc ec)
           (== e `(~ea ~eb))
           (== va `(~'closure ~envc ~xc ~ec))
           (evo env ea va)
           (evo env eb vb)
           (evo (lcons [xc vb] envc) ec v))]))

(is (= (run* [q] (evo '() 1 q))
       '(1)))

(is (= (run* [q] (evo '() '((fn [x] x) 1) q))
       '(1)))

(is (= (run* [q] (evo '() '(((fn [x] (fn [y] x)) 1) 2) q))
       '(1)))

;;; Typing
(comment
  t :=          ;; types
     Num        ;; Number
     (t1 => t2) ;; Arrow
)

(defn tpo [env e t]
  (conde
   [(lc-varo e) (env-lookupo env e t)]
   [(lc-csto e) (== 'Num t)]
   [(fresh (x eb tx tb)
           (== e `(~'fn [~x] ~eb))
           (== t `(~tx ~'=> ~tb))
           (tpo (lcons [x tx] env) eb tb))]
   [(fresh (ea eb ta tb)
           (== e `(~ea ~eb))
           (== ta `(~tb ~'=> ~t))
           (tpo env ea ta)
           (tpo env eb tb))]))

(is (= (run* [q] (tpo '() '(fn [x] x) q))
       '((_0 => _0))))

(is (= (run* [q] (tpo '() '((fn [x] x) 1) q))
       '(Num)))


    
