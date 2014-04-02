(defmacro s-expr->semantic-whitespace 
  "Convert a list of expressions in Clojure to semantic-whitespace form." 
  ([l] (apply str (interpose " " (macroexpand-1 `(s-expr->semantic-whitespace ~l 1))))) 
  ([l n] (map (fn [x] (if (list? x) (apply str "\n" (apply str (repeat n "\t")) (interpose " " (macroexpand-1 `(s-expr->semantic-whitespace ~x ~(+ n 1)))) ) x)) l)))

(defn split-on-newlines "Splits a string into a list of strings by any newline characters." [s] (clojure.string/split s #"\n"))

(defn indented? "Returns true if the string has at least one tab character, otherwise false." [s] (not (nil? (re-find #"\t" s))))

(defn remove-first-tab "Removes the first tab character from a string." [x] (clojure.string/replace-first x #"\t" ""))

(defn whitespace->parentheses "Converts a string of code from whitespace notation to parenthetical notation. Output is explicitly wrapped in a do-block for clarity." [l] (if (string? l) (print-str (conj (whitespace->parentheses (split-on-newlines l)) 'do)) (map (fn [x] (concat (vec (first x)) (vec (whitespace->parentheses (map remove-first-tab (second x)))))) (partition-all 2 (partition-by indented? l)))))

(defn whitespace->s-exprs "Converts from a string of whitespace-syntax code to an evaluable S-expression." [s] (read-string (whitespace->parentheses s)))


(comment "And below we have some examples.")
(def s-expression-example '(s-expr->semantic-whitespace (defn tabs->parens [c] (map add-parens (rest (split-on-newlines c)) (list-deltas (code->numtabs c))))))
(def example (macroexpand-1 s-expression-example))
(def back-to-s-expr-example (whitespace->s-exprs example))


(prn back-to-s-expr-example)
