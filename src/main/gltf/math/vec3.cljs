(ns gltf.math.vec3
  (:refer-clojure :exclude [zero?])
  (:require
   [goog.vec.vec3f :as gvec3]))

(deftype Vec3 [data]
  Object
  (toString [_] (str "["
                     (aget data 0) " "
                     (aget data 1) " "
                     (aget data 2) "]"))

  IPrintWithWriter
  (-pr-writer [this writer _]
    (-write writer (.toString this)))

  IIndexed
  (-nth [_ n] (aget data n))
  (-nth [_ n not-found] (or (aget data n) not-found))

  ISeqable
  (-seq [_] (IndexedSeq. data 0 nil))

  ICloneable
  (-clone [_] (Vec3. (gvec3/clone data))))

(defn create
  ([] (Vec3. (gvec3/create)))
  ([x y z] (Vec3. (gvec3/createFromValues x y z))))

(defn add! [v1 v2]
  (gvec3/add (.-data v1) (.-data v2) (.-data v1))
  v1)

(defn add
  ([v1 v2]
   (add! (clone v1) v2))
  ([v1 v2 v3]
   (-> (clone v1)
       (add! v2)
       (add! v3))))

(defn negate! [v]
  (gvec3/negate (.-data v) (.-data v))
  v)

(defn negate [v]
  (negate! (clone v)))

(defn scale! [v scalar]
  (gvec3/scale (.-data v) scalar (.-data v))
  v)

(defn scale [v scalar]
  (scale! (clone v) scalar))

(defn scale-and-add [v1 v2 scalar]
  (Vec3. (as-> (gvec3/create) temp
           (gvec3/scale (.-data v2) scalar temp)
           (gvec3/add (.-data v1) temp temp))))

(declare zero?)
(declare zero)

(defn normalize! [v]
  (if (zero? v)
    v
    (do (gvec3/normalize (.-data v) (.-data v))
        v)))

(defn normalize [v]
  (normalize! (clone v)))

(defn magnitude-squared [v]
  (gvec3/magnitudeSquared (.-data v)))

(defn magnitude [v]
  (gvec3/magnitude (.-data v)))

(defn zero? [v]
  (let [data (.-data v)]
    (== (aget data 0)
        (aget data 1)
        (aget data 2)
        0)))

(defn zero [] (create))
(defn world-up [] (create 0 1 0))

(defn clamp! [v max-length]
  (if (zero? v)
    v
    (if (> (magnitude-squared v) (* max-length max-length))
      (-> (normalize! v)
          (scale! max-length))
      v)))

(defn clamp [v max-length]
  (clamp! (clone v) max-length))
