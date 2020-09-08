(ns gltf.math.mat4 (:require
                    [goog.vec.mat4f :as gmat4]
                    [goog.vec.mat3f :as gmat3]
                    [goog.vec.Quaternion :as gquat]
                    [goog.vec.vec3f :as gvec3]
                    [gltf.math.vec3 :as vec3]
                    [gltf.math.quat :as quat]))

(deftype Mat4 [data]
  IPrintWithWriter
  (-pr-writer [o writer _]
    (-write writer (str "["
                        (aget data 0) " "
                        (aget data 1) " "
                        (aget data 2) " "
                        (aget data 3) " "
                        (aget data 4) " "
                        (aget data 5) " "
                        (aget data 6) " "
                        (aget data 7) " "
                        (aget data 8) " "
                        (aget data 9) " "
                        (aget data 10) " "
                        (aget data 11) " "
                        (aget data 12) " "
                        (aget data 13) " "
                        (aget data 14) " "
                        (aget data 15) " "
                        (aget data 16) "]")))

  ICloneable
  (-clone [_] (Mat4. (gmat4/setFromMat4f (gmat4/create) data))))

(defn create
  ([v00 v10 v20 v30
    v01 v11 v21 v31
    v02 v12 v22 v32
    v03 v13 v23 v33]
   (Mat4.
    (-> (gmat4/create)
        (gmat4/setFromValues
         v00 v10 v20 v30
         v01 v11 v21 v31
         v02 v12 v22 v32
         v03 v13 v23 v33)))))

(defn create-identity []
  (Mat4. (gmat4/createIdentity)))

(defn create-rotation-translation-scale [r t s]
  (Mat4.
   (gmat4/makeRotationTranslationScale
    (gmat4/create) (.-data r) (.-data t) (.-data s))))

(defn create-perspective [fov-y aspect near far]
  (Mat4.
   (gmat4/makePerspective
    (gmat4/create) fov-y aspect near far)))

(defn get-column [m i]
  (->> (gvec3/create)
       (gmat4/getColumn (.-data m) i)
       (vec3/Vec3.)))

; For get-translation, get-scale, and get-rotation:
; https://math.stackexchange.com/questions/237369/given-this-transformation-matrix-how-do-i-decompose-it-into-translation-rotati/417813
(defn get-translation [m]
  (let [result (gvec3/create)]
    (gmat4/getTranslation (.-data m) result)
    (vec3/Vec3. result)))

(defn get-scale [m]
  (let [sx (vec3/magnitude (get-column m 0))
        sy (vec3/magnitude (get-column m 1))
        sz (vec3/magnitude (get-column m 2))]
    (vec3/create sx sy sz)))

(defn get-rotation [m]
  (let [[sx sy sz] (get-scale m)
        v0 (vec3/scale (get-column m 0) (/ 1 sx))
        v1 (vec3/scale (get-column m 1) (/ 1 sy))
        v2 (vec3/scale (get-column m 2) (/ 1 sz))]
    (quat/Quat.
     (as-> (gmat3/create) m
       (apply gmat3/setColumnValues m 0 v0)
       (apply gmat3/setColumnValues m 1 v1)
       (apply gmat3/setColumnValues m 2 v2)
       (gquat/fromRotationMatrix3 m (gquat/createFloat32))))))

(defn transpose [m]
  (Mat4. (gmat4/transpose (.-data m) (gmat4/create))))

(defn mult-mat! [m1 m2]
  (Mat4. (gmat4/multMat (.-data m1) (.-data m2) (.-data m1))))

(defn mult-mat [m1 m2]
  (mult-mat! (clone m1) m2))

(defn mult-vec3 [m v]
  (vec3/Vec3. (gmat4/multVec3 (.-data m) (.-data v) (gvec3/create))))

(defn rotate! [m angle x y z]
  (gmat4/rotate (.-data m) angle x y z)
  m)

(defn rotate-x! [m angle]
  (gmat4/rotateX (.-data m) angle)
  m)

(defn rotate-y! [m angle]
  (gmat4/rotateY (.-data m) angle)
  m)

(defn translate! [m x y z]
  (gmat4/translate (.-data m) x y z)
  m)

(defn make-translate [x y z]
  (Mat4. (gmat4/makeTranslate (gmat4/create) x y z)))

(defn scale! [m x y z]
  (gmat4/scale (.-data m) x y z)
  m)

(defn invert! [m]
  (if (gmat4/invert (.-data m) (.-data m))
    m
    nil))

(defn invert [m]
  (invert! (clone m)))

(defn data
  "Returns a typed array representing the underlying matrix data"
  [m]
  (.-data m))