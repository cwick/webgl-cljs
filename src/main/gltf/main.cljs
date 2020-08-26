(ns gltf.main (:require [reagent.core :as r]
                        [reagent.dom :as rdom]
                        [datafrisk.core :as datafrisk]
                        [gltf.loader]
                        [gltf.controllers :as controllers]
                        [gltf.input :as input]
                        [gltf.webgl.core :as gl]
                        [gltf.ui :as ui]
                        [gltf.camera :as camera]
                        [gltf.math.vec3 :as vec3]
                        [gltf.math.mat4 :as mat4]))

(defonce app-state (r/atom {}))

(defonce game-state
  (atom {:camera {:yaw 0
                  :pitch 0
                  :position (vec3/create 0 1 3.5)
                  :velocity (vec3/zero)
                  :impulse (vec3/zero)}
         :buttons #{}
         :last-frame-time nil}))

(defn- create-default-scene []
  (let [texture-scale 50
        vertex-data
        (js/Float32Array.from
         #js[; Positions
             ; Triangle A
             1.0 0 1.0
             -1.0 0 -1.0
             -1.0 0 1.0
             ; Triangle B
             1.0 0 1.0
             1.0 0 -1.0
             -1.0 0 -1.0

             ; UVs
             ; Triangle A
             texture-scale texture-scale
             0 0
             0 texture-scale
             ; Triangle B
             texture-scale texture-scale
             texture-scale 0
             0 0])
        vertex-buffer
        {:data (.-buffer vertex-data)}
        vertex-buffer-view
        {:byteLength (+ (* 6 3 4) (* 6 2 4))
         :buffer vertex-buffer
         :byteOffset 0
         :byteStride 0
         :target :ARRAY_BUFFER}
        position-attribute
        {:byteOffset 0
         :componentType :FLOAT
         :type :VEC3
         :count 6
         :bufferView vertex-buffer-view}
        texcoord-attribute
        {:byteOffset (* 6 3 4)
         :componentType :FLOAT
         :type :VEC2
         :count 6
         :bufferView vertex-buffer-view}
        texture
        {:sampler {:minFilter 9728 :magFilter 9728}
         :source {:width 2
                  :height 2
                  :data (js/Uint8Array.from
                         #js[255 255 255 255
                             0 0 0 255
                             0 0 0 255
                             255 255 255 255])}}
        material
        {:pbrMetallicRoughness {:baseColorTexture texture :baseColorFactor [0.75 0.7 0.7 1.0]}}]
    {:nodes
     [{:name "Floor"
       :matrix (-> (mat4/create-identity)
                   (mat4/translate! 0 0 0)
                   (mat4/scale! texture-scale texture-scale texture-scale))
       :mesh {:name "Floor"
              :primitives
              [{:attributes
                {:POSITION position-attribute
                 :TEXCOORD_0 texcoord-attribute}
                :mode :TRIANGLES
                :material material}]}}]}))

(def default-scene (create-default-scene))

(defn- load-model [gltf base-url]
  (-> (gltf.loader/load-gltf gltf base-url)
      (.then (fn [scene]
               (swap! app-state assoc
                      :gltf gltf
                      :scene (update default-scene :nodes conj {:children (:nodes scene)}))))))

(defn App [state]
  [:<>
   [ui/SelectModel {:on-select load-model}]
   [datafrisk/DataFriskShell
    (:gltf @state)
    (:scene @state)]])

(defn- draw-model []
  (when-let [model (:scene @app-state)]
    (gl/set-view-matrix! (get-in @game-state [:camera :view-matrix]))
    (gl/draw model)))

(defn- handle-mouse-input [dx dy]
  (let [camera-motions (controllers/handle-mouse-input dx dy)]
    (swap! game-state update :camera
           #(-> %
                (update :yaw-delta + (:yaw-delta camera-motions))
                (update :pitch-delta + (:pitch-delta camera-motions))))))

(defn- handle-keyboard-input [pressed-buttons]
  (let [impulse (controllers/handle-keyboard-input pressed-buttons)]
    (swap! game-state assoc-in [:camera :impulse] impulse)))

(defn ^:dev/after-load start []
  (gl/recompile-shaders)
  (rdom/force-update-all))

(defn ^:dev/before-load stop [])

(defn- main-loop [time]
  (let [time-seconds (/ time 1000)
        last-frame-time (or (:last-frame-time @game-state) time-seconds)
        time-delta (- time-seconds last-frame-time)]
    (when (> time-delta 0)
      (ui/clear)
      (ui/draw-benchmark
       "Update time"
       (fn []
         (swap! game-state update :camera #(camera/update-camera % time-delta))))
      (draw-model))
    (swap! game-state assoc :last-frame-time time-seconds)
    (js/requestAnimationFrame main-loop)))

(defn- observe-canvas [canvas]
  (let [observer
        (js/ResizeObserver.
         #(let [rect (.-contentRect (aget % 0))
                width (.-width rect)
                height (.-height rect)]
            (gl/set-projection-matrix!
             (mat4/create-perspective
              (* 50 (/ js/Math.PI 180)) ; fov-y
              (/ width height) ; aspect
              0.1 ; near
              10000)) ; far
            (ui/resize-canvas width height)))]
    (.observe observer canvas)))

(defn init []
  (start)
  (rdom/render
   [App app-state]
   (js/document.getElementById "app"))

  (swap! app-state assoc :scene default-scene)
  (let [canvas (js/document.getElementById "canvas")
        ui-canvas (js/document.getElementById "ui-canvas")]
    (ui/init ui-canvas)
    (input/init ui-canvas)
    (input/on-mouse-move ui-canvas #'handle-mouse-input)
    (input/on-key-down ui-canvas #'handle-keyboard-input)
    (input/on-key-up ui-canvas #'handle-keyboard-input)
    (gl/init-webgl canvas)
    (observe-canvas canvas))

  (js/requestAnimationFrame main-loop))
