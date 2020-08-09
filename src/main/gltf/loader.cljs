(ns gltf.loader (:require [goog.uri.utils :as uri]
                          [goog.vec.mat4f :as mat4]
                          [goog.vec.Quaternion :as quat]))

(defn- load-buffer [buffer base-url]
  (-> (js/fetch (uri/appendPath base-url (:uri buffer)))
      (.then #(.arrayBuffer %))
      (.then #(assoc buffer :data %))))

(defn- load-assets [data base-url]
  (-> (js/Promise.all (map #(load-buffer % base-url) (:buffers data)))
      (.then #(assoc data :buffers (into [] %)))))

(defn- resolve-buffer [data buffer-id]
  (-> (get-in data [:buffers buffer-id])
      (assoc :id buffer-id)))

(defn- resolve-buffer-view [data buffer-view-id]
  (let [buffer-view (get-in data [:bufferViews buffer-view-id])]
    (-> (assoc buffer-view
               :buffer (resolve-buffer data (:buffer buffer-view))
               :id buffer-view-id
               :byteStride (or (:byteStride buffer-view) 0)
               :byteOffset (or (:byteOffset buffer-view) 0)
               :target (condp = (:target buffer-view)
                         34962 :ARRAY_BUFFER
                         34963 :ELEMENT_ARRAY_BUFFER
                         nil))
        (as-> b (if (nil? (:target b))
                  (dissoc b :target)
                  b)))))

(defn- resolve-accessor [data accessor-id]
  (let [accessor (get-in data [:accessors accessor-id])]
    (assoc accessor
           :bufferView (resolve-buffer-view data (:bufferView accessor))
           :id accessor-id
           :byteOffset (or (:byteOffset accessor) 0)
           :type (keyword (:type accessor))
           :normalized (or (:normalized accessor) false)
           :componentType (condp = (:componentType accessor)
                            5120 :BYTE
                            5121 :UNSIGNED_BYTE
                            5122 :SHORT
                            5123 :UNSIGNED_SHORT
                            5125 :UNSIGNED_INT
                            5126 :FLOAT))))

(defn- resolve-attributes [data attributes]
  (into {} (map
            (fn [[name index]]
              [name (resolve-accessor data index)])
            attributes)))

(defn- resolve-primitive [data primitive]
  (cond-> primitive
    (contains? primitive :indices)
    (assoc :indices (resolve-accessor data (:indices primitive)))

    :then
    (assoc
     :attributes (resolve-attributes data (:attributes primitive))
     :mode (condp = (:mode primitive)
             0 :POINTS
             1 :LINES
             2 :LINE_LOOP
             3 :LINE_STRIP
             4 :TRIANGLES
             5 :TRIANGLE_STRIP
             6 :TRIANGLE_FAN
             :TRIANGLES))))

(defn- resolve-primitives [data primitives]
  (map #(resolve-primitive data %) primitives))

(defn- resolve-mesh [data mesh-id]
  (let [mesh (get-in data [:meshes mesh-id])]
    (-> (update mesh :primitives #(resolve-primitives data %))
        (assoc :id mesh-id))))

(defn- resolve-camera [data camera-id]
  (let [camera (get-in data [:cameras camera-id])]
    (assoc camera :id camera-id)))

(declare resolve-node)
(defn- resolve-children [data children]
  (map #(resolve-node data %) children))

(defn- get-transformation-matrix [node]
  ; TRS properties are converted to matrices and postmultiplied in 
  ; the T * R * S order to compose the transformation matrix; 
  ; first the scale is applied to the vertices, then the rotation, and then the translation.
  (mat4/makeRotationTranslationScale
   (mat4/create)
   (or (clj->js (:rotation node)) (quat/createIdentityFloat32))
   (or (clj->js (:translation node)) #js[0 0 0])
   (or (clj->js (:scale node)) #js[1 1 1])))

(defn- resolve-node [data node-id]
  (let [node (transient (get-in data [:nodes node-id]))]
    (->
     (cond-> node
       (contains? node :mesh)
       (assoc! :mesh (resolve-mesh data (:mesh node)))

       (contains? node :children)
       (assoc! :children (resolve-children data (:children node)))

       (contains? node :camera)
       (assoc! :camera (resolve-camera data (:camera node)))

       (some #(% node) [:translation :rotation :scale])
       (assoc! :matrix (get-transformation-matrix node))

       (:matrix node)
       (assoc! :matrix (apply mat4/setFromValues (mat4/create) (:matrix node))))
     (assoc! :id node-id)
     (dissoc! :translation :rotation :scale)
     (persistent!))))

(defn- resolve-nodes [data node-ids]
  (map
   #(resolve-node data %)
   node-ids))

(defn resolve-scene [data scene-id]
  (let [scene (get-in data [:scenes scene-id])]
    (update scene :nodes #(resolve-nodes data %))))

(defn load-gltf [data base-url]
  (-> (load-assets data base-url)
      (.then #(resolve-scene % (:scene data)))))


