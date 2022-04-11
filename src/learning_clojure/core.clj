(ns learning-clojure.core
  (:import (clojure.java.api Clojure)))

; Flatten
;(defn problem-28
;  [list]
;  (loop [ret-list []
;         [head & tail] list]
;    (if (nil? head)
;      ret-list
;      (if (coll? head)
;        (recur (apply conj ret-list (problem-28 head)) tail)
;        (recur (conj ret-list head) tail)
;      ))
;    ))
;
;(problem-28 [1 2 3 4])
;(problem-28 [1 [2 3] 4])
;(problem-28 [1 [2 [3]] 4])
;(problem-28 '((((:a)))))
;
;(conj [1 2] [3 4])
;
;(apply conj [1 2] [3 4])
;
;((fn problem-28 [list]
;  (loop [ret-list []
;         [head & tail] list]
;    (if (nil? head)
;      ret-list
;      (if (coll? head)
;        (recur (apply conj ret-list (problem-28 head)) tail)
;        (recur (conj ret-list head) tail)
;        ))
;    )) [1 2 [3 [4]]])
;
;(defn problem-28-from-online
;  ([a]
;   (if (coll? a) (apply problem-28-from-online a) [a]))
;  ([a & more]
;   (concat (problem-28-from-online a) (apply problem-28-from-online more))))
;
;(problem-28-from-online [1 [2 [3]] 4])
;
;((complement true?) true)
;
;(defn problem-29
;  [word]
;  (apply str (filter #(re-find #"[A-Z]" (str %)) word)))
;
;(problem-29 "HelLLO")
;
;(defn problem-29-from-online
;  [st]
;  (apply str (filter #(<= (int \A) (int %) (int \Z)) st)))
;
;(problem-29-from-online "HelLOOOOa")
;
;(defn problem-30
;  [list]
;  (reverse (loop [[a & more] list
;         ret-list nil]
;    (if (empty? more)
;      (conj ret-list a)
;      (if (= a (first more))
;        (recur more ret-list)
;        (recur more (conj ret-list a)))
;    ))))
;
;(problem-30 "aabbcc")
;(problem-30 [1 1 2 3 3 2 2 3])
;(conj nil [1])
;(conj nil "aabc")
;
;(defn problem-30-from-online
;  [list]
;  (lazy-seq
;    (if (empty? list)
;      ()
;      (let [f (first list)
;            more (drop-while #(= f %) list)]
;        (cons f (problem-30-from-online more))))))
;
;(problem-30-from-online [1 1 1 2 2 2 3 2 1 1 1])
;
;(defn probem-30-partition-by
;  [ls]
;  (map first (partition-by identity ls)))
;
;(probem-30-partition-by [1 1 2 2 3 3 4 4])
;(probem-30-partition-by [[1 2] [2 3] [1 2] [1 2] [2 3] [4113 23 "aaa"]])
;
;(defn problem-31
;  [list]
;  (partition-by identity list))
;
;(problem-31 '(:a :a :a :b :b :a :a :b :a))
;
;(defn problem-31-from-online
;  [list]
;  (lazy-seq
;    (if (empty? list) ()
;    (let [f (first list)
;          fs (take-while #(= f %) list)
;          rs (drop-while #(= f %) list)]
;      (cons fs (problem-31-from-online rs))))))
;
;(problem-31-from-online '(:a :a :a :b :b :a :a :b :a))
;
;(defn problem-32
;  [list]
;  (reverse (loop [[a & more] list
;         ret-list nil]
;    (if (nil? a)
;      ret-list
;      (recur more (conj ret-list a a)))
;    )))
;
;(problem-32 [1 2 3])
;
;(defn problem-33-reduce
;  [list n]
;  (reduce (fn [acc term] (concat acc (repeat n term))) [] list))
;
;(problem-32-reduce [1 2 3] 3)
;(problem-32-reduce [[1 2] [3 4] 2] 3)
;
;(problem-32-reduce '([1 2] [1 2] [3]) 3)
;
;
;(repeat 3 (conj (repeat 3 (conj [] 2 )) 3))
;
;(#(mapcat (partial repeat %2) %) [1 2] 3)
;
;(defn problem-34-didnt-read-correctly
;  [low
;   high]
;  (fn [list] (filter #(<= low %) (filter #(>= high %) list))))
;
;((problem-34 3 6) [1 2 3 4 5 6 7])
;
;
;(defn problem-34
;  [low high]
;  (apply list (rest (let [count (- high low)]
;    (reverse
;      (reduce (fn [acc _]
;                (cons (+ (first acc) 1) acc))
;              [(- low 1)] (repeat count low)))))))
;
;(problem-34 5 8)
;
;(defn problem-34-from-online
;  [low high]
;  (take-while (complement #(= % high)) (iterate inc low)))
;
;(problem-34-from-online -2 2)
;
;(defn problem-38
;  [& more]
;  (reduce (fn [acc val] (if (nil? acc)
;              val
;              (if (< acc val)
;                val
;                acc))) nil more)
;  )
;
;(problem-38 1 8 3 4)
;
;((fn [& more] (reduce #(if (< %1 %2) %2 %1) more))  1 2 5 3 -15)
;
;; problem-39
;((fn [& more] ((fn prob-39 [a b ret-val] (when (and a b)
;                             (prob-39 (rest a) (rest b) (conj ret-val a b)))
;   ) (first more) (second more) (list))) [1 2 3] [:a :b :c])
;
;((fn [a b] (map conj (list a) (list b))) [1 2 3] [:a :b :c])
;
;((fn prob-39 [a b]
;   (loop [l a
;          r b
;          rem []]
;     (if (or (empty? l) (empty? r))
;       rem
;       (recur (rest l)
;              (rest r)
;              (conj rem (first l) (first r)))))) [1 2 3] [:a :b :c])
;
;(and [3] [1])
;
;(mapcat list [1 2 3] [:a :b :c])
;(apply map list [[1 2 3] [:a :b :c]])
;(map list [[1 2 3] [:a :b :c]])
;(apply list [[1 2 3] [:a :b :c]])
;(apply map + [[1 2 3] [1 2 3]])
;(map + [[1 2 3] [1 2 3]])
;(map + [1 2 3] [1 2 3])
;
;; interpose
;((fn prob-40
;   [s terms]
;   (let [f #(list s %)]
;     (flatten (concat [(first terms)] (map f (rest terms)))))) 0 [1 2 3])
;
;(concat [1] [2] [3] [4])
;
;((fn from-online-40 [x ls] (rest (interleave (repeat x) ls))) 0 [1 2 3])
;
;((fn prob-41
;   [ls n]
;   (if (empty? ls)
;     ls
;     (concat (take (- n 1) ls) (prob-41 (drop n ls) n)))) [:a :b :c :d :e :f] 2)
;
;((fn prob-41-online
;   [s k]
;   (for [[x i] (map vector s (cycle (range k))) :when (not= i (dec k))] x)) [:a :b :c :d :e :f] 2)
;
;((fn prob-42
;   [x]
;   (apply * (rest (range (inc x))))) 3)
;
;; Slightly optimized after learning that range can take a start point argument.
;(#(apply * (range 2 (inc %))) 8)
;(range 3)
;(range 2 5)
;
;((fn prob-43
;  [ls n]
;  (let [k (cycle (range n))]
;    (map #(map first %) (vals
;      (group-by #(second %) (map vector ls k))
;      )))) (range 9) 3)
;
;(map #(map first %) [[[0 0] [3 0]] [[1 1] [4 1]]])
;
;((fn prob-43-online
;   [ls n]
;   (partition (/ (count ls) n) (apply interleave (partition n ls)))) (range 9) 3)
;
;
;((fn prob-43-from-online-ish-from-memory
;   [ls n]
;   (apply map list (partition n ls))) (range 9) 3)
;
;((fn prob-44
;   [n ls]
;   (let [c (count ls)] (take c (drop (+ c (mod n c)) (cycle ls))))) -4 '(:a :b :c))
;
;; Semi-copied, semi written myself.
;((fn prob-44-loop-recur
;   [n ls]
;   (loop [i 0
;          ret ls]
;       (if (= i n)
;         ret
;         (cond
;           (> n i) (recur (inc i) (concat (rest ret) [(first ret)]))
;           (< n i) (recur (dec i) (concat [(last ret)] (butlast ret)))
;           true ret
;        ))
;                   )) -4 '(:a :b :c))
;
;((fn prob-45
;   [x]
;   (take x (iterate #(+ 3 %) 1))) 5)
;
;(defn prob-46
;   [f]
;  (fn [& args]
;      (apply f (reverse args))))
;
;((prob-46 take) [1 2 3 4 5] 3)
;
;(quot 8 2)
;
;(reverse ())
;
;(defn a
;  [f]
;  (fn [& x] (apply f x)))
;
;((a println) 1 2 3)
;
;(contains? {4 :a 2 :b} 2)
;(contains? {1 2 3 4} 4)
;(contains? #{1 2 3 4 4} 5)
;(contains? [1 2 3 4] 4)
;
;(some #{2 7 6} [5 6 7 8])
;
;((fn prob-49
;   [n ls]
;   (loop [ret-list []
;          i 0
;          rem ls]
;     (if (= n i)
;       (conj []  ret-list rem)
;       (recur (conj ret-list (first rem)) (inc i) (rest rem))
;       ))) 3 [1 2 3 4 5 6])
;
;(conj (conj [] 1) 2)
;
;((fn prob-49-drop
;   [n ls]
;   (conj [] (take n ls) (drop n ls))) 3 [1 2 3 4 5 6])
;
;;49 from online
;; juxt = very nice way of writing apply set of functions to same inputs
;((juxt take drop) 3 [1 2 3 4 5 6])
;
;((fn prob-50
;   [ls]
;   (loop [ret-map {}
;          rem ls]
;     (if (empty? rem)
;       (map reverse (vals ret-map))
;       (recur (update ret-map (type (first rem)) #(conj % %2) (first rem))
;              (rest rem))
;       ))) [1 2 :a :b 3 [1 2] [3 4] "aaa"])
;
;(vals {:a 1 :b 2 3 :c})
;
;; Came up with the most concise on my own!
;((fn prob-50
;   [ls]
;   vals (group-by type ls)) [1 2 :a :b 3 [1 2] [3 4] "aaa" "b " 4])
;
;(range)
;
;((fn prob-53
;   [ls]
;   (let [f #(if (< (count %1) (count %2))
;               %2
;               %1)
;         g #(if (< 1 (count %)) % [])]
;     (loop [longest-seq []
;          recent-seq []
;          recent-seen (dec (apply min ls))
;          [a & rem] ls]
;     (if (empty? rem)
;       (g (f longest-seq (if (< recent-seen a) (conj recent-seq a) recent-seq)))
;       (if (< recent-seen a)
;         (recur (f longest-seq (conj recent-seq a)) (conj recent-seq a) a rem)
;         (recur (f longest-seq recent-seq) (vector a) a rem))))))
;  [7 6 5 4]
; )
;
;
;(< -1 2)
;(< [-1] [2])
;(#(if (< (count %1) (count %2))
;    %2
;    %1) [1 2 3] [1 2])
;(#(if (< (count %1) (count %2))
;    %2
;    %1) [1 2] [1 2 3] )
;
;(vector 1)
;
;(fn prob-51-inefficient
;  [ls]
;  (let [f (fn fJoiner [l]  (loop [[x & res] l] (if (> x (first res)) [] (conj x (fJoiner res)))))
;        g #(if (< (count %1) (count %2)) %2 %1)]
;    (reduce g (apply f ls))) [[1 2] [3 4 5] [6 7]])
;
;(reduce #(if (< (count %1) (count %2)) %2 %1) [[1 2] [3 4 5] [6 7]])
;
;(take-while (< % (fnext %2)))
;
;(< 1 (fnext 1))
;(defn fJoiner [l]  (loop [[x & res] l] (if (> x (first res)) [] (conj x (fJoiner res)))))
;
;(fJoiner [3 4 5 1 2])
;
;(seq [1 2 3])
;(seq "abcdef")
;(seq [])
;
;(partition 2 1 [5 6 1 2 3 4])
;(partition-by #(- (second %) (first %)) ((5 6) (6 1)))
;
;; Uses partition with offset, and the fact that we're always incrementing by 1
;; so partition-by ends up creating groups where every increment is 1.
;(fn [coll]
;  (->> (partition 2 1 coll)
;       (partition-by #(- (second %) (first %)))
;       (filter #(= 1 (- (second (first %)) (ffirst %))))
;       (reduce #(if (< (count %1) (count %2)) %2 %1) [])
;       flatten
;       distinct
;       )) [5 6 1 2 3 4]
;(->> (partition 2 1 [5 6 1 2 3 4])
;     (partition-by #(- (second %) (first %))))
;
;(partition-by #(- (second %) (first %)) [[5 6] [6 1] [1 2] [2 3] [3 4]])
;(partition-by #(- (second %) (first %)) [[5 6] [6 1] [1 2] [2 3] [3 4]])
;
;(iterate next [5 6 1 2 3 4])
;(take-while identity (iterate next '(5 6 1 2 3 4)))
;
;(take-while #(> (count %) 1) (iterate butlast '(5 6 1 2 3 4)))
;
;; More prob 53. This does the slow way that seemed fun! Ty person who created this
;(fn [x] (->> x
;             (iterate next)                                 ; creates an iterator
;             (take-while identity)
;             (mapcat (fn [s] (->> s
;                                  (iterate butlast)
;                                  (take-while #(> (count %) 1))
;                                  (filter (partial apply <)))))
;             (cons [])
;             (reduce #(if (> (count %2) (count %1)) %2 %1))
;             ))
;
;((fn prob-54
;   [n ls]
;   (filter #(= n (count %))
;               (loop [ret-list []
;                      rem-list ls]
;                 (if (empty? rem-list)
;                   ret-list
;                   (recur (conj ret-list (take n rem-list)) (drop n rem-list)))))) 3 (range 9))
;
;
;(filter #(= 3 (count %) [[1 2 3] [4 5 6] [7 8]]))
;(filter #(= 3 (count %)) [[1 2 3] [4 5 6] [7 8]])
;
;(loop [ret-list []
;       rem-list [1 2 3 4 5 6 7 8]]
;  (if (empty? rem-list)
;    ret-list
;    (recur (conj ret-list (take 3 rem-list)) (drop 3 ret-list))))
;
;((fn prob-55
;   [ls]
;   (map count (group-by identity ls)))
; [1 1 2 3 2 1 1])
;
;((fn prob-55
;   [ls]
;   (loop [rem (seq (group-by identity ls))
;          m {}]
;     (if (empty? rem)
;       m
;       (recur (rest rem) (assoc m (ffirst rem) (count (second (first rem)))))))
;   )
; [1 1 2 3 2 1 1])
;
;((fn prob-55-zipmap
;   [ls]
;   (let [a (group-by identity ls)]
;     (zipmap (keys a) (map count (vals a)))))
; [1 1 2 3 2 1 1])
;
;((fn prob-56
;   [ls]
;   (loop [ret-ls []
;          [h & rem] ls
;          seen (set nil)]
;      (if (nil? h)
;        ret-ls
;        (if (contains? seen h)
;          (recur ret-ls rem seen)
;          (recur (conj ret-ls h) rem (conj seen h))))
;     ))
;  '([2 4] [1 2] [1 3] [1 3])
; )
;
;((fn prob-56-filter
;   [a & ls]
;   (let [rem (filter #(not= a %) ls)]
;     (if (empty? rem)
;       a
;       (conj a (prob-56-filter rem))))
;   )
; '([2 4] [1 2] [1 3] [1 3])
; )
;
;(filter #(not= [1 3] %) '([2 4] [1 2] [1 3] [1 3]))

((fn prob-56-filter
   [[a & ls]]
   (let [rem (filter (fn [x] (not= a x)) ls)]
     (if (empty? rem)
       [a]
       (concat []  [a] (prob-56-filter rem))))
   )
  [:a :a :b :b :c :c]
 )


