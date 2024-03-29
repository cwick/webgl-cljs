(ns gltf.main (:require [reagent.core :as r]
                        [reagent.dom :as rdom]
                        [datafrisk.core :as datafrisk]
                        [gltf.loader]
                        [gltf.input-devices :as input-devices]
                        [gltf.webgl.core :as gl]
                        [gltf.webgl.debug :as gl-debug]
                        [gltf.ui :as ui]
                        [gltf.camera :as camera]
                        [gltf.input :as input]
                        [gltf.math.vec3 :as vec3]
                        [gltf.math.mat4 :as mat4]
                        [gltf.math.quat :as quat]
                        [gltf.scene :as scene]))

(defonce app-state (r/atom {}))

(defonce game-state
  (atom {:grab-tool {:grabbing? false
                     :controller (input/create-controller
                                  [(input/map-axis "grabbing?" input/button-axis
                                                   [:keyboard :buttons "KeyG"])])}
         :last-frame-time nil}))

; TODO put this somewhere else
(defn- create-floor-node []
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
    (scene/create-node
     {:name "Floor"
      :position (vec3/zero)
      :rotation (quat/create-identity)
      :scale (vec3/create texture-scale texture-scale texture-scale)
      :mesh {:name "Floor"
             :primitives
             [{:attributes
               {:POSITION position-attribute
                :TEXCOORD_0 texcoord-attribute}
               :mode :TRIANGLES
               :material material}]}})))

(def default-scene (-> (scene/create)
                       (scene/add-child (create-floor-node))
                       (assoc :camera (camera/create))))

(defn- load-model [gltf base-url]
  (swap! app-state assoc :gltf gltf)
  (-> (gltf.loader/load-gltf gltf base-url)
      (.then (fn [scene]
               (let [new-game-state
                     (swap! game-state update
                            :scene #(-> (scene/merge-scene default-scene scene)
                                        (assoc :camera (:camera %))))]
                 (swap! app-state assoc :scene (:scene new-game-state)))))))

(defn ^:dev/after-load start []
  (gl/recompile-shaders (:gl @game-state))
  (rdom/force-update-all))

(defn ^:dev/before-load stop [])

(defn App [state]
  [:<>
   [ui/SelectModel {:on-select load-model}]
   [datafrisk/DataFriskShell
    (:gltf @state)
    (:scene @state)]])

; TODO need to convert from local to global and back again when grabbing
(defn- update-grab-tool [old-grab scene camera time-delta]
  (let [node (second (scene/children scene (scene/root scene)))
        grab (-> old-grab
                 (update :controller input/update-controller @input-devices/input-state time-delta))
        grab-triggered? (-> grab :controller :grabbing? :pressed?)]
    (if (and grab-triggered? node)
      (if (not (:grabbing? old-grab))
        (assoc grab
               :grabbing? true
               :grab-point (mat4/mult-vec3 (:view-matrix camera) (:position node))
               :grab-node-id (:id node))
        (assoc grab :grabbing? false))
      grab)))

(defn- update-scene [old-scene game-state]
  (let [grab-tool (:grab-tool game-state)
        camera (-> game-state :scene :camera)]
    (if-let [node-id (and (:grabbing? grab-tool)
                          (:grab-node-id grab-tool))]
      (scene/set-position old-scene
                          node-id
                          (mat4/mult-vec3 (:world-matrix camera) (:grab-point grab-tool)))
      old-scene)))

(defn- update-game-state [old-state time-delta]
  (as-> old-state state
    (assoc state :input-state @input-devices/input-state)
    (update-in state [:scene :camera]
               camera/update-camera
               state
               time-delta)
    (update state :grab-tool update-grab-tool (:scene state) (-> state :scene :camera) time-delta)
    (update state :scene update-scene state)
    (update state :scene scene/update-transforms)))

(defn- main-loop [time]
  (let [time-seconds (/ time 1000)
        last-frame-time (or (:last-frame-time @game-state) time-seconds)
        time-delta (- time-seconds last-frame-time)]
    (when (> time-delta 0)
      (ui/clear)
      (ui/draw-benchmark
       "Update time"
       #(swap! game-state update-game-state time-delta))
      (let [gl (:gl @game-state)]
        (gl/draw gl (:scene @game-state))
        (ui/draw-hud)
        (gl-debug/set-camera gl (-> @game-state :scene :camera))
        (gl-debug/point gl 0 0 0 [1.0 0 0])))
    (swap! game-state assoc :last-frame-time time-seconds)
    (input-devices/end-frame)
    (js/requestAnimationFrame main-loop)))

(defn- observe-canvas [canvas]
  (let [observer
        (js/ResizeObserver.
         #(let [rect (.-contentRect (aget % 0))
                width (.-width rect)
                height (.-height rect)]
            (swap! game-state update-in [:scene :camera] camera/set-aspect-ratio (/ width height))
            (ui/resize-canvas width height)))]
    (.observe observer canvas)))

(defn init []
  (rdom/render
   [App app-state]
   (js/document.getElementById "app"))

  (let [canvas (js/document.getElementById "canvas")
        ui-canvas (js/document.getElementById "ui-canvas")]
    (ui/init ui-canvas)
    (input-devices/init ui-canvas)
    (let [gl (gl/init-webgl canvas)]
      (swap! game-state assoc
             :scene default-scene
             :gl gl))
    (observe-canvas canvas))

  (start)
  (js/requestAnimationFrame main-loop))
