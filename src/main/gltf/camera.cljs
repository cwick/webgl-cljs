(ns gltf.camera
  (:require
   [gltf.math.mat4 :as mat4]
   [gltf.math.vec3 :as vec3]
   [gltf.input :as input]
   [gltf.math.utils :as math]))

(defn- orbit? [camera] (-> camera :controller :orbit? :value))
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
    (-> (vec3/normalize (:position camera))
        (vec3/scale! -1))
    (:forward camera)))

(defn- update-velocity [camera]
  (let [max-speed 12
        forward-velocity (-> (get-forward-movement-direction camera)
                             (vec3/scale (-> camera :controller :forward :value)))
        right-velocity (-> (:right camera)
                           (vec3/scale (-> camera :controller :right :value)))
        up-velocity (-> (vec3/world-up)
                        (vec3/scale! (-> camera :controller :up :value)))
        velocity (-> (vec3/add forward-velocity right-velocity up-velocity)
                     (vec3/scale! max-speed)
                     (vec3/clamp! max-speed))]
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

(defn- update-orbit-movement [camera time]
  (let [pitch-delta (-
                     (constrain-pitch (+ (:pitch camera)
                                         (pitch-delta camera)))
                     (:pitch camera))]
    (-> camera
        (update :position
                #(-> (mat4/create-identity)
                     (as-> m (apply mat4/rotate! m pitch-delta (:right camera)))
                     (mat4/rotate-y! (yaw-delta camera))
                     (mat4/mult-vec3 %)))
        ; OK for now, but need to refine camera motion while in orbit mode
        (update-fly-movement time))))

(defn update-camera [camera input-state time]
  (let [camera (as-> camera camera
                 (update camera :controller input/update-controller input-state time)
                 (if (orbit? camera)
                   (update-orbit-movement camera time)
                   (update-fly-movement camera time)))
        [x y z] (:position camera)
        ; Inverse of pure rotation matrix is its transpose
        view-matrix (-> (mat4/transpose (:orientation camera))
                        (mat4/translate! (- x) (- y) (- z)))]
    (assoc camera :view-matrix view-matrix)))

(defn create []
  (let [force 2
        counter-force 6
        axis (input/digital-to-absolute-axis force counter-force)
        mouse-sensitivity (* 0.1 (/ js/Math.PI 180))
        identity-matrix (mat4/create-identity)]
    {:yaw 0
     :pitch 0
     :position (vec3/create 0 1 3.5)
     :velocity (vec3/zero)
     :orientation identity-matrix
     :right (get-right-vector identity-matrix)
     :forward (get-forward-vector identity-matrix)
     :controller (input/create-controller
                  [(input/map-axis "right" axis
                                   [:keyboard :buttons "KeyA"] [:keyboard :buttons "KeyD"])

                   (input/map-axis "forward" axis
                                   [:keyboard :buttons "KeyS"] [:keyboard :buttons "KeyW"])

                   (input/map-axis "up" axis
                                   [:keyboard :buttons "KeyC"] [:keyboard :buttons "Space"])

                   (input/map-axis "yaw" (input/sensitivity mouse-sensitivity)
                                   [:mouse :axes :relative-x])

                   (input/map-axis "pitch" (input/sensitivity mouse-sensitivity)
                                   [:mouse :axes :relative-y])

                   (input/map-axis "orbit?" #(== 1 (apply input/button-axis %&))
                                   [:mouse :buttons 2])])}))