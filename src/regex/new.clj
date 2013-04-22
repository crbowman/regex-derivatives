;;
;; Author: Curtis Bowman
;;
;;
(ns: regex.derivatives)


;; Predicates
(defn concatenation? [re]
  (= (:op re) :cat))

(defn alternation? [re]
  (= (:op re) :alt))

(defn repetition? [re]
  (= (:op re) :rep))

(defn null-string? [re]
  (= (:op re) :null-string))

(defn null-set? [re]
  (nil? re))

(defn atomic? [re]
  (or (char? re) (symbol? re)))

;; Constructors

(defn null-string []
  {:op :null-string})

(def null-set
  nil)

;; catenation
(defn cat [e1 e2]
  (cond (or (null-set? e1) (null-set? e2)) null-set
        (null-string? e1) e2
        (null-string? e2) e1
        :else {:op :cat, :e1 e1, :e2 e2}))

;; alternation
(defn alt [e1 e2]
  (cond
   (null-set? e1) e2
   (null-set? e2) e1
   :else {:op :alt, :e1 e1, :e2 e2}))

;; repetition
(defn rep [e]
  (cond
   (null-set? e)   null-string
   (null-string e) null-string
   :else {:op :rep, :e1 e}))

;; nullable: regex -> regex
;;  - return the null-string if the language defined by re contains the empty string,
;;    otherwise return the null-string.
(defn nullable? [re]
  (cond (null-string? re)   null-string
        (null-set? re)      null-set
        (atomic? re)        null-set
        (repetition? re)    null-string
        (concatenation? re) (and (nullable? (:e1 re))
                                 (nullable? (:e2 re)))
        (alternation? re)   (or (nullable? (:e1 re))
                                (nullable? (:e2 re)))
        :else null-set))



;; regex-derivative: regex * regex-atom -> regex
;;  - Returns the derivative of re with repect to c.
(defn regex-derivative [re c]
  (cond (null-string? c)    re
        (null-string? re)   null-string
        (null-set? re)      null-set
        (atomic? re)        (if (= c re) null-string
                                         null-set)
        (concatenation? re) (alt (cat (regex-derivative (:e1 re) c) (:e2 re))
                                 (cat (nullable? (:e1 re)) (regex-derivative (:e2 re) c)))
        (alternation? re)   (alt (regex-derivative (:e1 re) c)
                                 (regex-derivative (:e2 re) c))
        (repetition? re)    (cat (regex-derivative (:e1 re) c)
                                 (rep (:e1 re)))
        :else null-set))







;; Tests

(def a (alt (cat \a \b) (cat \a \c)))
(def b \a)

(nullable? a)
(nil? (nullable? a))
(regex-derivative a \a)lo
