(ns fractal-quil.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate complex-number ([x y] means "x + y * i") ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mult
  ([] [1 1])
  ([x] x)
  ([x y]
   (let [[x1 y1] x
         [x2 y2] y]
     [(- (* x1 x2) (* y1 y2))
      (+ (* x1 y2) (* x2 y1))]))
  ([x y & more]
   (reduce mult (mult x y) more)))

(defn plus
  ([] [0 0])
  ([x] x)
  ([x y]
   (let [[x1 y1] x
         [x2 y2] y]
     [(+ x1 x2)
      (+ y1 y2)]))
  ([x y & more]
   (reduce plus (plus x y) more)))

(defn minus
  ([] [0 0])
  ([x] x)
  ([x y]
   (let [[x2 y2] y]
     (plus x [(* -1 x2) (* -1 y2)])))
  ([x y & more]
   (reduce minus (minus x y) more)))

(defn conjugate [x]
  (let [[x1 y1] x]
    [x1 (* -1 y1)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate levy-fractal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn levy-f0 [base a b]
  (plus (mult a base)
        (mult b (conjugate base))))

(defn levy-f1 [base c d]
  (plus (mult c (minus base [1 0]))
        (mult d (minus (conjugate base) [1 0]))
        [1 0]))

(defn levy-fractal [base ^Integer depth a b c d]
  (let [f0 (levy-f0 base a b)
        f1 (levy-f1 base c d)]
    (if (> depth 0)
      (concat [f0 f1]
              (levy-fractal f0 (dec depth) a b c d)
              (levy-fractal f1 (dec depth) a b c d))
      [f0 f1])))

(defn levy-fractals [{:keys [depth init a b c d]}]
  (levy-fractal init depth a b c d))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def depth 15)
(def parameter-map
  (atom {:depth depth :init [0 1] :a [0.5 0.5] :b [0 0] :c [0.5 -0.5] :d [0 0]}))
(def parameter-map-a
  {:depth depth :init [0 1] :a [0.5 0.5] :b [0 0] :c [0.5 -0.5] :d [0 0]})
(def parameter-map-b
  {:depth depth :init [0 1] :a [0 0] :b [0 0.2887] :c [0 0] :d [0.6667 0]})
(def parameter-map-c
  {:depth depth :init [0 1] :a [0 0.707] :b [0 0] :c [0 0.5] :d [0 0]})
(def parameter-map-d
  {:depth depth :init [0 1] :a [0.4614 0.4614] :b [0 0] :c [0.622 -0.192] :d [0 0]})

(defn change-parameter [^String choice]
  (condp = choice
    "a" (reset! parameter-map parameter-map-a)
    "b" (reset! parameter-map parameter-map-b)
    "c" (reset! parameter-map parameter-map-c)
    "d" (reset! parameter-map parameter-map-d)
    (swap! parameter-map parameter-map-a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; draw levy-fractal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def width 800)
(defn zoom-pos [levy-pos zoom]
  (map (fn [v] (let [[x1 y1] v] [(* x1 zoom) (* y1 zoom)])) levy-pos))

(defn draw []
  (let [levy-pos (levy-fractals @parameter-map)
        zoom (/ width 2)
        zoomed-levy-pos (zoom-pos levy-pos zoom)]
    (doall (map (fn [v]
                  (let [[x1 y1] v]
                    (q/set-pixel (+ 200 x1) (+ 200 y1)
                                 (q/color 1 1 1))))
           zoomed-levy-pos))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  {:color 0
   :angle 0})

(defn -main []
  (q/defsketch fractal-quil
    :title "Fractal-example"
    :size [width width]
    :setup setup
    :draw draw))
