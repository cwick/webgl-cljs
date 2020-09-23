(ns gltf.ui (:require [reagent.core :as r]
                      goog.string.format))

(def model-repository "https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Models/master/2.0/")

(defn- fetch-json [url]
  (-> (js/fetch url)
      (.then #(.json %))
      (.then #(js->clj % :keywordize-keys true))))

(defn- load-model [model callback]
  (let [base-url (str model-repository (:name model) "/glTF/")
        url (str base-url (get-in model [:variants :glTF]))]
    (-> (fetch-json url)
        (.then #(callback % base-url)))))

(defn SelectModel [{:keys [on-select]}]
  (let [models (r/atom nil)
        on-change #(load-model
                    (nth @models (js/parseInt (-> % .-target .-value)))
                    on-select)]
    (-> (fetch-json (str model-repository "model-index.json"))
        (.then #(reset! models %)))

    (fn []
      [:select {:on-change on-change}
       (map-indexed (fn [idx model]
                      [:option {:value idx :key (:name model)} (:name model)])
                    @models)])))

(defonce ui-state (atom nil))
(defonce hud-ctx (atom nil))
(def LINE-HEIGHT 20)
(def INITIAL-STATE {:line -1})

(defn resize-canvas [width height]
  (when-let [ctx (:context @ui-state)]
    (let [canvas (.-canvas ctx)
          scale js/devicePixelRatio]
      ; https://developer.mozilla.org/en-US/docs/Web/API/Window/devicePixelRatio
      (set! (.-width canvas) (js/Math.floor (* scale width)))
      (set! (.-height canvas) (js/Math.floor (* scale height)))
      (.setTransform ctx 1 0 0 1 0 0)
      (.scale ctx scale scale))))

(defn init [canvas]
  (let [ctx (.getContext canvas "2d")]
    (reset! ui-state (merge {:context ctx} INITIAL-STATE))
    (resize-canvas (-> ctx .-canvas .-clientWidth)
                   (-> ctx .-canvas .-clientHeight))))

(defn clear []
  (when-let [ctx (:context @ui-state)]
    (set! (.-fillStyle ctx) "white")
    (set! (.-font ctx) "18px consolas")
    (set! (.-textBaseline ctx) "top")
    (.clearRect ctx 0 0
                (-> ctx .-canvas .-width)
                (-> ctx .-canvas .-height))
    (swap! ui-state merge INITIAL-STATE)))

(defn debug [text]
  (when-let [ctx (:context @ui-state)]
    (let [line (inc (:line @ui-state))]
      (.fillText ctx text 0 (* LINE-HEIGHT line))
      (swap! ui-state assoc :line line))))

(defn average [x [values sum]]
  (let [target-samples 120
        values (or values #queue[])
        sum (or sum 0)

        [new-sum new-values]
        (if (< (count values) target-samples)
          [(+ sum x) (conj values x)]
          [(- (+ sum x) (peek values)) (conj (pop values) x)])

        avg
        (/ new-sum (count new-values))]
    [avg [new-values new-sum]]))

(defn average! [x state]
  (let [[result new-state] (average x @state)]
    (reset! state new-state)
    result))

(defn benchmark [f & args]
  (let [start-time (js/performance.now)]
    (apply f args)
    (- (js/performance.now) start-time)))

(defonce benchmark-state (atom {}))

(defn draw-benchmark [name f & args]
  (let [state (@benchmark-state name (atom nil))
        time (benchmark f args)]
    (debug (str name ": " (.toFixed (average! time state) 2) "ms"))
    (swap! benchmark-state assoc name state)))

(defn draw-hud []
  (when-let [ctx (:context @ui-state)]
    (.beginPath ctx)
    (.arc ctx
          (/ (-> ctx .-canvas .-width) (* 2 js/devicePixelRatio))
          (/ (-> ctx .-canvas .-height) (* 2 js/devicePixelRatio))
          3 0 (* js/Math.PI 2))
    (.fill ctx)))