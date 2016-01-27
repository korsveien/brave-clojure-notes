(ns brave-clojure.functional-programming)

(defn sum
  ([vals] (sum vals 0))
  ([vals accumulating-total]
   (if (empty? vals)
     accumulating-total
     (sum (rest vals) (+ (first vals) accumulating-total)))))

;;; use recur to acoid StackOverFlow
(defn sum
  ([vals] sum vals 0)
  ([valse accumulating-total]
   (if (empty? vals)
     accumulating-total
     (recur (rest vals) (+ (first vals) accumulating-total)))))


;;; ---------Function composition----------
((comp inc *) 2 7)
;;; => 3

(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})
(def c-int (comp :intelligence :attributes))
(def c-str (comp :strength :attributes))
(def c-dex (comp :dexterity :attributes))

(c-int character)
(c-str character)
(c-dex character)

(defn spell-slots
  [char]
  (int (inc (/ (c-int char) 2))))

(spell-slots character)

(def spell-slots-comp (comp int inc #(/ % 2) c-int))

(spell-slots-comp character)

(defn two-comp
  [f g]
  (fn [& args]
    (f (apply g args))))

((two-comp inc inc) 1)

((multi-comp-1 inc inc inc) 1)

(defn multi-comp-2
  [& fns]
  (fn [& args]
    (reduce (fn [result-so-far next-fn] (next-fn result-so-far))
            (apply (last fns) args) (rest (reverse fns)))))

(defn multi-comp-with-let
  [& fns]
  (fn [& args]
    (let [ordered-fns (reverse fns)
          first-result (apply (first ordered-fns) args)
          remaining-fns (rest ordered-fns)]
      (reduce
       (fn [result-so-far next-fn] (next-fn result-so-far))
       first-result
       remaining-fns))))

((multi-comp-with-loop inc inc inc) 1)

(defn multi-comp-with-loop
  [& fns]
  (fn
    [& args]
    (let [ordered-fns (reverse fns)
          first-result (apply (first ordered-fns) args)]
      (loop [result-so-far first-result
             remaining-fns (rest ordered-fns)]
        (if (empty? remaining-fns)
          result-so-far
          (let [next-fn (first remaining-fns)]
            (recur (next-fn result-so-far) (rest remaining-fns))))))))

((multi-comp-with-loop inc inc inc) 1)

;;; -------Memoization------------
(defn sleepy-identity
  [x]
  (Thread/sleep 1000)
  x)

(sleepy-identity "Mr. Fantastico")
(sleepy-identity "Mr. Fantastico")

(def memo-sleep-identity (memoize sleepy-identity))

(memo-sleep-identity "Mr. Fantastico")
; => "Mr. Fantastico" after 1 second

(memo-sleep-identity "Mr. Fantastico")
; => "Mr. Fantastico" immediately

