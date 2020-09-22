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
;          |  <char>                 ; Character
;          |  (alt <regex> <regex>)  ; Alternation
;          |  (cat <regex> <regex>)  ; Concatenation
;          |  (rep <regex>)          ; Repetition

; Further reading:

; [1] Janusz Brzozowski. "Derivatives of Regular Expressions." 1964.
; [2] Scott Owens, John Reppy, Aaron Turon. "Regular expression derivatives re-examined." 2009.
(ns regex.derivatives)

(def null-string 
  {:op :null-string})

(def null-set
  nil)

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
  (char? re))

;; Constructors
(defn cat [e1 e2]
  (cond (or (null-set? e1) (null-set? e2)) null-set
        (null-string? e1) e2
        (null-string? e2) e1
        :else {:op :cat, :e1 e1, :e2 e2}))

(defn alt [e1 e2]
  (cond
   (null-set? e1) e2
   (null-set? e2) e1
   :else {:op :alt, :e1 e1, :e2 e2}))

(defn rep [e]
  (cond
   (null-set? e)   null-string
   (null-string e) null-string
   :else {:op :rep, :e1 e}))


;; return true if the language defined by re contains the empty string,
;; otherwise return false.
(defn nullable? [re]
  (cond (null-string? re) true
        (null-set? re) false
        (atomic? re) false
        (repetition? re) true
        (concatenation? re) (and (nullable? (:e1 re))
                                 (nullable? (:e2 re)))
        (alternation? re)   (or (nullable? (:e1 re))
                                (nullable? (:e2 re)))
        :else false))


;; Returns the derivative of re with respect to c.
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

(defn rest-of-string [s]
  (if (= (count s) 1)
    null-string
    (rest s)))

(defn regex-match [re s]
  (if (null-string? s)
    (nullable? re)
    (regex-match (regex-derivative re (first s)) (rest-of-string s))))
