(ns gltf.main (:require [reagent.core :as r]
                        [reagent.dom :as rdom]
                        [datafrisk.core :as datafrisk]
                        [gltf.loader]
                        [gltf.input :as input]
                        [gltf.webgl.core :as gl]
                        [gltf.ui :as ui]
                        ["gl-matrix/mat4" :as mat4]
                        ["gl-matrix/quat" :as quat]))

(defonce app-state (r/atom {}))

(defonce game-state
  (atom {:camera {:yaw 0
                  :pitch 0
                  :roll 0
                  :position [0 0 3.5]
                  :velocity [0 0 0]
                  :angular-speed [0 0 0]}
         :buttons #{}
         :last-frame-time nil}))

(defn- load-model [gltf base-url]
  (-> (gltf.loader/load-gltf gltf base-url)
      (.then #(swap! app-state assoc :model % :gltf gltf))))

(defn App [state]
  [:<>
   [ui/SelectModel {:on-select load-model}]
   [datafrisk/DataFriskShell
    (:gltf @state)
    (:model @state)]])

(defn- set-view-matrix! []
  (let [camera (:camera @game-state)
        q (quat/fromEuler (quat/create) (:pitch camera) (- (:yaw camera)) 0)
        matrix (mat4/fromRotationTranslation (mat4/create) q (clj->js (-> @game-state :camera :position)))]
    (gl/set-view-matrix! (mat4/invert (mat4/create) matrix))))

(defn- draw-model []
  (when-let [model (:model @app-state)]
    (set-view-matrix!)
    (gl/draw model)))

(defn- update-position [pos velocity time]
  (let [[x y z] pos
        [dx dy dz] velocity]
    [(+ x (* dx time))
     (+ y (* dy time))
     (+ z (* dz time))]))

(defn- update-camera [camera time]
  (as-> camera camera
    (update camera :position #(update-position % (:velocity camera) time))
    (let [[dy dp dr] (:angular-speed camera)]
      (-> camera
          (update :yaw #(mod (+ % dy) 360))
          (update :pitch #(+ % dp))
          (update :roll #(+ % dr))))))

(defn- handle-mouse-input [dx dy]
  (let [sensitivity 0.1
        camera (:camera @game-state)]
    (->>
     (-> camera
         (update :yaw #(mod (+ % (* dx sensitivity)) 360))
         (update :pitch #(+ % (* dy sensitivity))))
     (swap! game-state assoc :camera))))

(defn- handle-keyboard-input [pressed-buttons]
  (let [camera-speed 1
        velocity (cond-> [0 0 0]
                   (contains? pressed-buttons "KeyW")
                   (update 2 #(- % camera-speed))
                   (contains? pressed-buttons "KeyS")
                   (update 2 #(+ % camera-speed)))]
    (swap! game-state assoc-in [:camera :velocity] velocity)))

(defn ^:dev/after-load start []
  (gl/recompile-shaders)
  (rdom/force-update-all))

(defn ^:dev/before-load stop [])

(defn main-loop [time]
  (let [time-seconds (/ time 1000)
        last-frame-time (or (:last-frame-time @game-state) time-seconds)
        time-delta (- time-seconds last-frame-time)]
    (when (> time-delta 0)
      (swap! game-state update :camera #(update-camera % time-delta))
      (draw-model)
      (ui/draw-hud @game-state))
    (swap! game-state assoc :last-frame-time time-seconds)
    (js/requestAnimationFrame main-loop)))

(defn init []
  (start)
  (rdom/render
   [App app-state]
   (js/document.getElementById "app"))

  (let [canvas (js/document.getElementById "canvas")
        hud-canvas (js/document.getElementById "hud-canvas")]
    (ui/init-hud hud-canvas)
    (input/init hud-canvas)
    (input/on-mouse-move hud-canvas #'handle-mouse-input)
    (input/on-key-down hud-canvas #'handle-keyboard-input)
    (input/on-key-up hud-canvas #'handle-keyboard-input)
    (gl/init-webgl canvas))

  (add-watch app-state :model (fn [_ _ old new]
                                (when-not (identical? (:model old) (:model new))
                                  (draw-model))))

  (js/requestAnimationFrame main-loop))