(ns gltf.ui (:require [reagent.core :as r]
                      [goog.string :as gstring]
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

(defonce hud-ctx (atom nil))

(defn init-hud [canvas]
  (let [ctx (.getContext canvas "2d")]
    (.scale ctx 1.25 1.25)
    (set! (.-fillStyle ctx) "white")
    (set! (.-font ctx) "20px consolas")
    (set! (.-textBaseline ctx) "top")
    (reset! hud-ctx ctx)))

(defn draw-text [text position]
  (when-let [ctx @hud-ctx]
    (.fillText ctx text 0 position)))

(defn draw-hud [game-state]
  (when-let [ctx @hud-ctx]
    (let [camera (:camera game-state)]
      (.clearRect ctx 0 0
                  (-> ctx .-canvas .-width)
                  (-> ctx .-canvas .-height))
      (draw-text (str (:buttons game-state)) 0)
      (draw-text (apply gstring/format "Y:%-+7.2f° P:%-+6.2f° T:[%.2f %.2f %.2f]"
                        (:yaw camera)
                        (:pitch camera)
                        (:position camera)) 20)
      (draw-text (apply gstring/format "dT:[%.2f %.2f %.2f]"
                        (:velocity camera)) 40)
      (draw-text (apply gstring/format "L:[%.3f %.3f %.3f]"
                        (:look camera)) 60))))