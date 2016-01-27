(ns chapter-notes.core-functions-in-depth)

(def human-consumption   [8.1 7.3 6.6 5.0])
(def critter-consumption [0.0 0.2 0.3 1.1])

(defn unify-diet-data
  [human critter]
  {:human human
   :critter critter})

(map unify-diet-data human-consumption critter-consumption)


;;; passing map a collection of functions
(def sum #(reduce + %))
(def avg #(/ (sum %) (count %)))

(defn stats
  [numbers]
  (map #(% numbers) [sum count avg]))

(stats [3 4 10])

(def identities
  [{:alias "Batman" :real "Bruce Wayne"}
   {:alias "Spider-Man" :real "Peter Parker"}
   {:alias "Santa" :real "Your mom"}
   {:alias "Easter Bunny" :real "Your dad"}])

(map :real identities)

;;; transform a maps' values using reduce
(reduce (fn [new-map [key val]]
          (assoc new-map key (inc val)))
        {}
        {:max 30 :min 10})

;;; filter out keys based on a value
(reduce (fn [new-map [key val]]
          (if (> val 4)
            (assoc new-map key val)
            new-map))
        {}
        {:human 4.1
         :critter 3.9})

(def food-journal
  [{:month 1 :day 1 :human 5.3 :critter 2.3}
   {:month 1 :day 2 :human 5.1 :critter 2.0}
   {:month 2 :day 1 :human 4.9 :critter 2.1}
   {:month 2 :day 2 :human 5.0 :critter 2.5}
   {:month 3 :day 1 :human 4.2 :critter 3.3}
   {:month 3 :day 2 :human 4.0 :critter 3.8}
   {:month 4 :day 1 :human 3.7 :critter 3.9}
   {:month 4 :day 2 :human 3.7 :critter 3.6}])

(take-while #(< (:month %) 3) food-journal)
(drop-while #(< (:month %) 3) food-journal)




(def vampire-database
  {0 {:makes-blood-puns? false, :has-pulse? true  :name "McFishwich"}
   1 {:makes-blood-puns? false, :has-pulse? true  :name "McMackson"}
   2 {:makes-blood-puns? true,  :has-pulse? false :name "Damon Salvatore"}
   3 {:makes-blood-puns? true,  :has-pulse? true  :name "Mickey Mouse"}})

(defn vampire-related-details
  [social-security-number]
  (Thread/sleep 1000)
  (get vampire-database social-security-number))

(defn vampire?
  [record]
  (and (:makes-blood-puns? record)
       (not (:has-pulse? record))
       record))

(defn identify-vampire
  [social-security-numbers]
  (first (filter vampire?
                 (map vampire-related-details social-security-numbers))))



(time (vampire-related-details 0))

(time (def mapped-details (map vampire-related-details (range 0 1000000))))

;; (time (first mapped-details))

;;; infinite sequences
(concat (take 8 (repeat "na")) ["Batman!"])
(take 3 (repeatedly (fn [] (rand-int 10))))

(defn even-numbers
  ([] (even-numbers 0))
  ([n] (cons n (lazy-seq (even-numbers (+ n 2))))))

(take 10 (even-numbers))

;;; will return a seq
(map identity {:sunlight-reaction "Glitter!"})
;;; into will "cast" it back to a map
(into {} (map identity {:sunlight-reaction "Glitter!"}))

;;; conj works differently than into
(conj [0] [1]) ;; => [0 [1]]
(into [0] [1]) ;; => [0 1]

(conj [0] 1 2 3 4)
(conj {:time "midnight"} [:place "ye olde cemetarium"])


;;; (max [1 2 3]) does not work, max expects x & xs
(max 1  2  3)
;;; apply explodes the elements of a collection
(apply max [1 2 3])

(def add10 (partial + 10))
(add10 3) ;; => 13
(add10 5) ;; => 15

(defn my-partial
  [partialized-fn & args]
  (fn [& more-args]
    (apply partialized-fn (into args more-args))))

(def add20 (my-partial + 20))
(add20 3) ;; => 23

(defn lousy-logger
  [log-level message]
  (condp = log-level
    :warn (clojure.string/lower-case message)
    :emergency (clojure.string/upper-case message)))

(def warn (partial lousy-logger :warn))

(warn "Red light ahead")

(defn identify-humans
  [social-security-numbers]
  (filter #(not (vampire? %)) ;; here we could just use complement
          (map vampire-related-details social-security-numbers)))

(def not-vampire? (complement vampire?))

(defn my-complement
  [f]
  (fn [& args]
    (not (apply f args))))

(def my-pos? (complement neg?))
(my-pos? 1)
(my-pos? 2)
