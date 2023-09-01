(ns day14 (:require [clojure.string :refer [split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn parse-speeds [line]
  (let [parts (split line #" ")]
    {:reindeer (nth parts 0)
     :speed (read-string (nth parts 3))
     :flight (read-string (nth parts 6))
     :rest (read-string (nth parts 13))}))

(defn calculate-distance-old [reindeer duration]
  (let [;; flight time
        t_F (:flight reindeer)
        ;; rest time
        t_R (:rest reindeer)
        ;; total activity time
        t_T (+ t_F t_R)
        s (:speed reindeer)
        ;; whole number of activities
        n (quot duration t_T)
        ;; whole seconds of flight
        t_Flight (* n t_F)
        t_Rest (- duration (* n t_T))
        t_Total (if (<= t_F t_Rest) (+ t_Flight t_Rest) t_Flight)
        d (* t_Total s)]
    (println (:reindeer reindeer) "t_F:" t_F "t_R:" t_R "t_T:" t_T "s:" s "n:" n [t_Flight (* s t_Flight) t_Rest t_Total] d)
    d))

(defn calculate-distance [reindeer total_travel_time]
  (let [fly_time (:flight reindeer)
        rest_time (:rest reindeer)
        speed (:speed reindeer)
        total_activity_time (+ fly_time rest_time)
        activities (quot total_travel_time total_activity_time)
        total_whole_travel (* activities speed fly_time)
        extra (- total_travel_time (* activities total_activity_time))
        additional (if (>= extra fly_time) (* speed fly_time) 0)
        distance (+ total_whole_travel additional)
        ]
    (println reindeer {:t_a_t total_activity_time :act activities :t_w_t total_whole_travel :e extra :a additional :d distance})
    distance))

(defn solve-1 [input-file duration]
  (let [input (input->vec input-file)
        speeds (map parse-speeds input)
        distances (for [s speeds] (merge s {:distance (calculate-distance s duration)}))]
    (:distance (last (sort-by :distance distances)))))

(println (solve-1 "test-input" 1000))
(println (solve-1 "input" 2503))
