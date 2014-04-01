(defmacro s-expr->semantic-whitespace "Convert a list of expressions in Clojure to semantic-whitespace form." ([l] (apply str (interpose " "(macroexpand-1 `(s-expr->semantic-whitespace ~l 1))))) ([l n] (map (fn [x] (if (list? x) (apply str "\n" (apply str (repeat n "\t")) (interpose " "(macroexpand-1 `(s-expr->semantic-whitespace ~x ~(+ n 1)))) ) x)) l)))

(def example (macroexpand-1 '(s-expr->semantic-whitespace (defn 
                                                            tabs->parens [c] (map add-parens (rest

                                                                                               (split-on-newlines 
                                                                                                 c)) (list-deltas (code->numtabs c)))))))

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


(defn split-on-newlines [s] (clojure.string/split s #"\n"))

(defn indented? [s] (not (nil? (re-find #"\t" s))))

(defn tabs->lists [l] (reduce (fn [a b] (if (indented? b) 
                                          (conj (pop a) (conj (peek a) (clojure.string/replace-first b #"\t" "")))
                                          (conj a [b]))) [] l))
(defn disp [x] (if (list? x) (map tabs->lists x) x))

(defn whitespace->s-exprs [s] (-> s split-on-newlines tabs->lists))

(println example)

;(prn (whitespace->s-exprs example))

(prn (disp (split-on-newlines example)))
