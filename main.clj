(use '[clojure.string :as s :only [replace split]] )
(use 'clojure.java.io)

(defn remove-whitespace [c] (s/replace c #"\s+" " "))
(defn remove-right-parens [c] (s/replace c #"\)" "\n"))
(defn split-on-parens [c] (s/split c #"\("))
(defn num-right-parens [c] (count (re-seq #"\)" c)))
(defn num-tabs [l] (let [a (map (fn [y] (num-right-parens y)) l)] (map (fn [n] (- (dec n) (apply + (take n a)))) (range 1 (inc (count a))))))  
(defn newlines-and-tabs [l] (map (fn [i] (apply str "\n" (repeat i "    "))) (num-tabs l)))
(defn parens->tabreturns [c] (let [b (split-on-parens c)] (interleave (map remove-right-parens b) (newlines-and-tabs b))))

(def pythonize (comp (partial apply str) parens->tabreturns remove-whitespace))

(defn newlines->leftparens [c] (s/replace c #"\n" "("))
(defn tabs->rightparens [c] (s/replace c #"\s\s\s\s" ")"))
(def lispify (comp newlines->leftparens tabs->rightparens))

(def example "(defn line->ints [line] (map (fn [l] (Integer/parseInt l)) (split line #\"\\s+\"))); Convert a string/line to a list of integers.

             (defn solutions->outputs [solutions] (map (fn [n s] (str \"Case #\" (inc n) \": \" s \"\n\")) (range (count solutions)) solutions)); Convert a sequence of solutions to a sequence of formatted Case# strings.

             (let [
             filename \"A-large-practice\"
  outfile (str filename \".out\")
             infile (str filename \".in\")
             write (fn [outs] (with-open [wrt (writer outfile)] (doseq [o outs] (.write wrt o))))
             inputs (split-lines (slurp infile))
             [firstline & lines] inputs
             [L D N] (line->ints firstline)

             words (take D lines)
             cases (map casestring->pattern (drop D lines))
             wordmatches (fn [pattern] (map #(re-matches pattern %) words))
             nummatches (fn [pattern] (count (remove nil? (wordmatches pattern))))
             solutions (map nummatches cases)
             outputs (solutions->outputs solutions)
             ]
             (write outputs)
             )")

(def pythonized-example (pythonize example))
(def relispified-example (lispify pythonized-example))
(println pythonized-example)
(newline)
(println relispified-example)

