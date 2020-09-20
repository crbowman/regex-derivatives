;; A Simple Regular Expression Grammar in EBNF:
;;
;;
;;
;; <empty string> = ''
;;
;; <digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
;;
;; <letter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G"
;;            | "H" | "I" | "J" | "K" | "L" | "M" | "N"
;;            | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
;;            | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
;;            | "c" | "d" | "e" | "f" | "g" | "h" | "i"
;;            | "j" | "k" | "l" | "m" | "n" | "o" | "p"
;;            | "q" | "r" | "s" | "t" | "u" | "v" | "w"
;;            | "x" | "y" | "z"
;;
;; <char> ::= <digit>
;;          | <letter>
;;
;; <repetition> ::= <char> '*'
;;
;; <concatenation> ::= <char> <char>
;;                   | <repetition> <char>
;;                   | <char> <repetition>
;;                   | <repetition> <repetition>
;;
;; <alternation> ::= <char> '|' <char>
;;                 | <char> '|' <repetition>
;;                 | <char> '|' <concatenation>
;;                 | <repetition> '|' <char>
;;                 | <repetition> '|' <repetition>
;;                 | <repetition> '|' <concatenation>
;;                 | <concatenation> '|' <expr>
;;                 | <concatenation> '|' <repetition>
;;                 | <concatenation> '|' <concatenation>
;;
;; <regex> ::= <repetition>
;;           | <concatenation>
;;           | <alternation>
;;           | <char>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; <regex> ::= <term> '|' <regex>
;;           | <term>
;;
;; <term> ::= { <factor> }
;;
;; <factor> ::= <base> { '*' }
;;
;; <base> ::= <char>
;;          |  '\' <char>
;;          |  '(' <regex> ')'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; <alt> ::= <cat> '|' <alt>
;;           | <cat>
;;
;; <cat> ::= { <rep> }
;;
;; <rep> ::= <term> [ '*' ]
;;
;; <term> ::= <char>
;;          |  '\' <char>
;;          |  '(' <regex> ')'
;;
(ns
 ^{:author "Curtis Bowman"
   :doc "Recursive descent regular expression parser"}
 regex.parser
  (:require [clojure.string :refer [includes? split index-of]]
            [clojure.core.async :refer [>! alts!! timeout chan go]]))


(defn alphanumeric? [c]
  (Character/isLetterOrDigit c))

(defn lparen? [c]
  (= c \())

(defn rparen? [c]
  (= c \)))

(defn kleene? [c]
  (= c \*))

(defn vbar? [c]
  (= c \|))

(defn escape? [c]
  (= c \\))

(def symbols (atom []))

(defn eat! [c]
  (if (= (first @symbols) c)
    (swap! symbols rest)
    (throw (ex-info "Failed eat!" {:char c :symbols @symbols}))))

(defn more-chars? []
  (if (seq @symbols)
    true
    false))

(defn more-factors? []
  (not= nil (first @symbols)))

(defn first-char-is? [c]
                (= (first @symbols) c))

(declare parse-regex!)
(declare parse-term!)
(declare parse-factor!)
(declare parse-base!)

(defn wait [ms f & args]
  (let [c (chan)]
    (go (>! c (apply f args)))
    (first (alts!! [c (timeout ms)]))))

(defn parse-regex [xs]
  (reset! symbols (seq xs))
  (parse-regex!))

;; (defn parse-regex! []
;;   #break
;;   (let [term (parse-term!)]
;;     (if (seq @symbols)
;;       (if (= (first @symbols) \|)
;;        (do (eat! \|)
;;            (println "parse-regex!: more")
;;            [:alt
;;             term
;;             (parse-regex!)])
;;        [term
;;         [:error "Expected | after term!"]])
;;       [:term term])))

(defn parse-term! []
  #break
  (let [fact (parse-factor!)
        #break
        factors []]
    (while (and (more-chars?)
                (more-factors?))
      (if (first-char-is? nil)
          nil
          (conj factors (parse-factor!))))
    (do (eat! nil)
        (let [factors (vec (remove nil? (flatten (cons fact factors))))]
          (if (= (factors 0) :cat)
            factors
            (if (> (count factors) 1)
              (vec (cons :cat factors))
              factors))))))

(defn parse-factor! []
  #break
  (let [base (parse-base!)]
    #break
    (if (more-chars?)
      (cond
        (first-char-is? \*) (do (println "Repetition")
                                    (swap! symbols rest)
                                    [:rep base])
        (first-char-is? nil) nil
        (nil? base) nil
        :else (flatten (conj [base] (parse-factor!))))
      base)))

(defn parse-regex! []
  #break
  (parse-term!))

(defn parse-base! []
  #break
  (let [c (first @symbols)]
    #break
    (cond
      (nil? c) [:null-string]
      (lparen? c) (do (eat! \()
                      (let [re (parse-regex!)]
                        #break
                        (eat! \))
                        re))
      (escape? c) (do (eat! \\)
                      (let [b (first @symbols)]
                        #break
                        (eat! b)
                        b))
      (rparen? c) (do (swap! symbols #(cons nil %))
                      nil)
      (alphanumeric? c) (do (eat! c)
                            c)
      :else (swap! symbols #(cons nil %)))))


(def binding (let [a 1]
               (let [f #(println a)]
                 (let [a 2]
                   (f)))))
