;;
;; Author: Curtis Bowman
;; Site: https://partiallyapplied.io/
;;

; The code in this file implements a
; regular-expression matcher based on the derivative of
; regular expressions.

; The derivative of a regular expression 're' with respect
; to a character 'c' is a new regular expression which matches
; what the expression 're' would match if it first matched 'c'.

; For example, using POSIX notation, the derivative of
; (foo|frak)* with respect to 'f' is (oo|rak)(foo|frak)*

; To account for the possibility that a regular expression
; may not match any string, including the empty string,
; regular expressions include an unmatchable pattern:

; <regex> ::= null-set               ; Unmatchable pattern
;          |  null-string            ; Empty/blank pattern
;          |  <symbol>              ; Symbol
;          |  <char>               ; Character
;          |  (alt <regex> <regex>)  ; Alternation
;          |  (seq <regex> <regex>)  ; Sequence
;          |  (rep <regex>)          ; Repetition

; Further reading:

; [1] Janusz Brzozowski. "Derivatives of Regular Expressions." 1964.
; [2] Scott Owens, John Reppy, Aaron Turon. "Regular expression derivatives re-examined." 2009.
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
;;  - Returns the derivative of re with respect to c.
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

(def re (alt (cat \a \b) (cat \a \c)))
(def str \a)

(nullable? str)
(nil? (nullable? str))
(regex-derivative re str)
