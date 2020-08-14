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

(defn init [canvas]
  (let [ctx (.getContext canvas "2d")]
    ; TODO don't hardcode this.
    (.scale ctx 1.25 1.25)
    (set! (.-fillStyle ctx) "white")
    (set! (.-font ctx) "18px consolas")
    (set! (.-textBaseline ctx) "top")
    (reset! ui-state (merge {:context ctx} INITIAL-STATE))))

(defn debug [text]
  (when-let [ctx (:context @ui-state)]
    (let [line (inc (:line @ui-state))]
      (.fillText ctx text 0 (* LINE-HEIGHT line))
      (swap! ui-state assoc :line line))))

(defn clear []
  (when-let [ctx (:context @ui-state)]
    (.clearRect ctx 0 0
                (-> ctx .-canvas .-width)
                (-> ctx .-canvas .-height))
    (swap! ui-state merge INITIAL-STATE)))