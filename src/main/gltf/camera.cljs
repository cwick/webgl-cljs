(ns gltf.camera
  (:require
   [gltf.math.mat4 :as mat4]
   [gltf.math.vec3 :as vec3]
   [gltf.input :as input]
   [gltf.math.utils :as math]
   [gltf.webgl.core :as gl]
   [gltf.ui :as ui]))

(defn- orbit? [camera]
  (and
   (== 1 (-> camera :controller :orbit? :value))
   (:orbit-point camera)))
(defn- pitch-delta [camera] (-> camera :controller :pitch :value))
(defn- yaw-delta [camera] (-> camera :controller :yaw :value))

; Extract forward and right vectors from the rotation matrix
; https://community.khronos.org/t/get-direction-from-transformation-matrix-or-quat/65502/2
(defn- get-right-vector [orientation]
  (mat4/get-column orientation 0))

(defn- get-forward-vector [orientation]
  (-> (mat4/get-column orientation 2)
      (vec3/negate!)))

(defn- get-forward-movement-direction [camera]
  (if (orbit? camera)
    (-> (vec3/normalize (vec3/subtract (:position camera) (:orbit-point camera)))
        (vec3/scale! -1))
    (:forward camera)))

(defn- get-speed [camera]
  (if (orbit? camera)
    (let [distance (vec3/distance (:position camera) (:orbit-point camera))]
      (math/clamp distance 6 200))
    12))

(defn- update-velocity [camera]
  (let [speed (get-speed camera)
        forward-velocity (-> (get-forward-movement-direction camera)
                             (vec3/scale (-> camera :controller :forward :value)))
        right-velocity (-> (:right camera)
                           (vec3/scale (if-not (orbit? camera)
                                         (-> camera :controller :right :value)
                                         0)))
        up-velocity (-> (vec3/world-up)
                        (vec3/scale! (if-not (orbit? camera)
                                       (-> camera :controller :up :value)
                                       0)))
        velocity (-> (vec3/add forward-velocity right-velocity up-velocity)
                     (vec3/scale! speed)
                     (vec3/clamp! speed))]
    (assoc camera
           :velocity velocity)))

(defn- move-camera [camera time]
  (let [camera (update-velocity camera)
        velocity (:velocity camera)
        position (:position camera)]
    (assoc camera
           :position (vec3/scale-and-add position velocity time))))

(defn- constrain-pitch [pitch]
  (math/clamp pitch (- (/ js/Math.PI 2)) (/ js/Math.PI 2)))

(defn- update-pitch-yaw
  "Adjust yaw/pitch based on desired change"
  [camera]
  (let [invert-yaw (if (orbit? camera) -1 1)]
    (-> camera
        (update :yaw #(mod
                       (+ % (* (yaw-delta camera) invert-yaw))
                       (* 2 js/Math.PI)))
        (update :pitch #(constrain-pitch (+ % (pitch-delta camera)))))))

(defn- update-orientation [camera]
  (-> camera
      (update-pitch-yaw)
      (as-> camera
            (let [orientation (-> (mat4/create-identity)
                                  (mat4/rotate-y! (- (:yaw camera)))
                                  (mat4/rotate-x! (:pitch camera)))]
              (assoc camera
                     :orientation orientation
                     :forward (get-forward-vector orientation)
                     :right (get-right-vector orientation))))))

(defn- update-fly-movement [camera time]
  (-> camera
      (update-orientation)
      (move-camera time)))

(defn- unproject [camera win-x win-y depth]
  (let [inverse (mat4/invert! (mat4/mult-mat (:projection-matrix camera) (:view-matrix camera)))
        win-z (- (* 2 depth) 1)]
    (mat4/mult-vec3-projective inverse (vec3/create win-x win-y win-z))))

(defn- pick-orbit-point [camera gl]
  (if (-> camera :controller :orbit? :pressed?)
    (let [depth (gl/pick gl 0 0)
          orbit-point
          (if (== depth 1)
            nil
            (unproject camera 0 0 depth))]
      (assoc camera :orbit-point orbit-point))
    camera))

(defn- update-orbit-movement [camera time]
  (let [pitch-delta (-
                     (constrain-pitch (+ (:pitch camera)
                                         (pitch-delta camera)))
                     (:pitch camera))]
    (-> camera
        (update :position
                #(as-> (mat4/create-identity) m
                   (apply mat4/translate! m (:orbit-point camera))
                   (mat4/rotate-y! m (yaw-delta camera))
                   (apply mat4/rotate! m pitch-delta (:right camera))
                   (apply mat4/translate! m (vec3/negate (:orbit-point camera)))
                   (mat4/mult-vec3 m %)))
        (update-fly-movement time))))

(defn update-camera [camera game-state time]
  (let [camera (as-> camera camera
                 (update camera :controller input/update-controller (:input-state game-state) time)
                 (pick-orbit-point camera (:gl game-state))
                 (if (orbit? camera)
                   (update-orbit-movement camera time)
                   (update-fly-movement camera time)))
        [x y z] (:position camera)
        ; Inverse of pure rotation matrix is its transpose
        view-matrix (-> (mat4/transpose (:orientation camera))
                        (mat4/translate! (- x) (- y) (- z)))
        world-matrix (mat4/mult-mat!
                      (mat4/make-translate x y z)
                      (:orientation camera))]
    (assoc camera
           :view-matrix view-matrix
           :world-matrix world-matrix)))

(defn- update-projection-matrix [camera]
  (assoc camera
         :projection-matrix
         (mat4/create-perspective
          (:fov-y camera)
          (:aspect camera)
          (:near camera)
          (:far camera))))

(defn set-aspect-ratio [camera aspect]
  (update-projection-matrix (assoc camera :aspect aspect)))

(defn create []
  (let [force 2
        counter-force 6
        absolute-axis (input/digital-to-absolute-axis force counter-force)
        mouse-sensitivity (* 0.1 (/ js/Math.PI 180))
        identity-matrix (mat4/create-identity)]
    {:yaw 0
     :pitch 0
     :position (vec3/create 0 1 3.5)
     :velocity (vec3/zero)
     :orientation identity-matrix
     :right (get-right-vector identity-matrix)
     :forward (get-forward-vector identity-matrix)
     :fov-y (* 50 (/ js/Math.PI 180))
     :near 0.5
     :far 500
     :aspect 1
     :view-matrix (mat4/create-identity)
     :controller (input/create-controller
                  [(input/map-axis "right" absolute-axis
                                   [:keyboard :buttons "KeyA"] [:keyboard :buttons "KeyD"])

                   (input/map-axis "forward" absolute-axis
                                   [:keyboard :buttons "KeyS"] [:keyboard :buttons "KeyW"])

                   (input/map-axis "up" absolute-axis
                                   [:keyboard :buttons "KeyC"] [:keyboard :buttons "Space"])

                   (input/map-axis "yaw" (input/sensitivity mouse-sensitivity)
                                   [:mouse :axes :relative-x])

                   (input/map-axis "pitch" (input/sensitivity mouse-sensitivity)
                                   [:mouse :axes :relative-y])

                   (input/map-axis "orbit?" input/button-axis
                                   [:mouse :buttons 2])])}))