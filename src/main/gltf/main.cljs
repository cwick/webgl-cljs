(ns gltf.main (:require [reagent.core :as r]
                        [reagent.dom :as rdom]
                        [datafrisk.core :as datafrisk]
                        [gltf.loader]
                        [gltf.input :as input]
                        [gltf.webgl.core :as gl]
                        [gltf.ui :as ui]
                        [gltf.camera :as camera]
                        [goog.vec.vec2f :as vec2]))

(defonce app-state (r/atom {}))

(defonce game-state
  (atom {:camera {:yaw 0
                  :pitch 0
                  :position [0 0 3.5]
                  :velocity [0 0 0]
                  :impulse [0 0]}
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

(defn- draw-model []
  (when-let [model (:model @app-state)]
    (gl/set-view-matrix! (get-in @game-state [:camera :view-matrix]))
    (gl/draw model)))

(defn- handle-mouse-input [dx dy]
  (let [sensitivity 0.1
        camera (:camera @game-state)
        clamp #(max -90 (min 90 %))]
    (->>
     (-> camera
         (update :yaw #(mod (+ % (* dx sensitivity)) 360))
         (update :pitch #(clamp (+ % (* dy sensitivity)))))
     (swap! game-state assoc :camera))))

(defn- handle-keyboard-input [pressed-buttons]
  (let [impulse
        (cond-> [0 0]
          (contains? pressed-buttons "KeyW")
          (update 0 inc)
          (contains? pressed-buttons "KeyS")
          (update 0 dec)
          (contains? pressed-buttons "KeyD")
          (update 1 inc)
          (contains? pressed-buttons "KeyA")
          (update 1 dec))
        camera-speed 20
        normalized-impulse (if (= impulse [0 0])
                             #js[0 0]
                             (vec2/normalize (clj->js impulse) (vec2/create)))
        scaled-impulse (vec2/scale normalized-impulse camera-speed (vec2/create))]
    (swap! game-state
           #(-> %
                (assoc-in [:camera :impulse] [(aget scaled-impulse 0) (aget scaled-impulse 1)])
                (assoc :buttons pressed-buttons)))))

(defn ^:dev/after-load start []
  (gl/recompile-shaders)
  (rdom/force-update-all))

(defn ^:dev/before-load stop [])

(defn main-loop [time]
  (let [time-seconds (/ time 1000)
        last-frame-time (or (:last-frame-time @game-state) time-seconds)
        time-delta (- time-seconds last-frame-time)]
    (when (> time-delta 0)
      (swap! game-state update :camera #(camera/update-camera % time-delta))
      (ui/draw-hud @game-state)
      (draw-model))
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

  (js/requestAnimationFrame main-loop))
