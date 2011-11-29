(ns geom.seg
  (:require [geom.pt :as pt])
  (:use [geom.infinity]))


(defn new-seg [e1 e2 & neighbor]
  (let [slope (possibly-infinite (/ (- (e1 :y) (e2 :y)) (- (e1 :x) (e2 :x))))
        y-intercept (possibly-infinite (- (e1 :y) (possibly-infinite (* slope (e1 :x)))))]
    {:e1 e1 :e2 e2 :neighbor (first neighbor) :slope slope :y-intercept y-intercept}))

(defn all-same? [pred coll]
  (or (every? pred coll)
      (not-any? pred coll)))

(def all-different? (complement all-same?))

(defn pt-on-line? [pt seg]
  (if (infinite? (seg :slope))
    (= (pt :x) (get-in seg [:e1 :x]))
    (= (pt :y) (+ (* (seg :slope) (pt :x)) (seg :y-intercept)))))

(defn pt-within-seg-range? [pt seg]
  (let [pt-x (pt :x)
        pt-y (pt :y)
        x-diffs (map #(- pt-x (get-in seg [% :x])) '(:e1 :e2))
        y-diffs (map #(- pt-y (get-in seg [% :y])) '(:e1 :e2))]
    (boolean
     (or
      (= pt (seg :e1))
      (= pt (seg :e2))
      (and
       (all-different? #(> 0 %) x-diffs)
       (all-different? #(> 0 %) y-diffs))
      (and (some #(= 0 %) x-diffs)
           (all-different? #(> 0 %) y-diffs))
      (and (some #(= 0 %) y-diffs)
           (all-different? #(> 0 %) x-diffs))))))

(defn pt-on-seg? [pt seg]
  (and (pt-on-line? pt seg) (pt-within-seg-range? pt seg)))

(defn pt-inside-region?
  "Tests whether or not the given point is inside the counterclockwise-oriented
   convex polygon defined by region."
  [pt region]
  (every? #(or (pt/left-turn? (% :e1) (% :e2) pt)
               (pt-on-seg? pt %)) region))

(defn intersection [s1 s2]
  (if (= (s1 :slope) (s2 :slope))
    nil
    (if (infinite? (s1 :slope))
      (let [x (get-in s1 [:e1 :x])]
        (pt/new-pt x (+ (* (s2 :slope) x) (s2 :y-intercept))))
      (if (infinite? (s2 :slope))
        (let [x (get-in s2 [:e1 :x])]
          (pt/new-pt x (+ (* (s1 :slope) x) (s1 :y-intercept))))
        (let [x (possibly-infinite (/ (- (s2 :y-intercept) (s1 :y-intercept))
                                      (- (s1 :slope) (s2 :slope))))]
          (pt/new-pt x (+ (* (s1 :slope) x) (s1 :y-intercept))))))))

(defn intersection-on-seg? [s1 s2]
  (let [i (intersection s1 s2)]
    (if (nil? i)
      false
      (or (pt-on-seg? i s1)
          (pt-on-seg? i s2)))))

(defn pts-list-to-region [pts]
  (map new-seg (cons (last pts) pts) pts))