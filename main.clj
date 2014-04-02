(defmacro s-expr->semantic-whitespace 
  "Convert a list of expressions in Clojure to semantic-whitespace form." 
  ([l] (apply str (interpose " " (macroexpand-1 `(s-expr->semantic-whitespace ~l 1))))) 
  ([l n] (map (fn [x] (if (list? x) (apply str "\n" (apply str (repeat n "\t")) (interpose " " (macroexpand-1 `(s-expr->semantic-whitespace ~x ~(+ n 1)))) ) x)) l)))


(def pre-example '(s-expr->semantic-whitespace (defn tabs->parens [c] (map add-parens (rest (split-on-newlines c)) (list-deltas (code->numtabs c))))))
(def example (macroexpand-1 pre-example))
;(println pre-example)
(println example)
;=>
;defntabs->parens[c]
;	map add-parens 
;		rest 
;			split-on-newlines c 
;		list-deltas 
;			code->numtabs c
;
;
;


(defn split-on-newlines "Splits a string into a list of strings by any newline characters." [s] (clojure.string/split s #"\n"))

(defn indented? "Returns true if the string has at least one tab character, otherwise false." [s] (not (nil? (re-find #"\t" s))))

(defn remove-first-tab "Removes the first tab character from a string." [x] (clojure.string/replace-first x #"\t" ""))

(defn whitespace->s-exprs "Converts code from semantic-whitespace form to S-expression form. Accepts either a list of lines or a string." [l] (if (string? l) (whitespace->s-exprs (split-on-newlines l)) (map (fn [x] (concat (vec (first x)) (vec (whitespace->s-exprs (map remove-first-tab (second x)))))) (partition-all 2 (partition-by indented? l)))))

(prn (whitespace->s-exprs example))
