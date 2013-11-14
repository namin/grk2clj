(ns grk2clj.supp
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [clojure.core.match :only [match]])
  (:use [clojure.test]))

;;; prettier reification for single-variable constraints
(defn reifier-for [tag x]
  (fn [c v r a]
    (let [x (walk* r (walk* a x))]
      (when (symbol? x)
        `(~tag ~x)))))

(defn symo [x] (predc x symbol? (reifier-for 'sym x)))
(defn numo [x] (predc x number? (reifier-for 'num x)))
