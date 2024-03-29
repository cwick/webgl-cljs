(ns gltf.loader (:require [goog.uri.utils :as uri]
                          [gltf.math.mat4 :as mat4]
                          [gltf.math.vec3 :as vec3]
                          [gltf.math.quat :as quat]
                          [gltf.scene :as scene]))

(defn- load-buffer [buffer base-url]
  (-> (js/fetch (uri/appendPath base-url (:uri buffer)))
      (.then #(.arrayBuffer %))
      (.then #(assoc buffer :data %))))

(defn- load-image [image base-url]
  (let [image-url (uri/appendPath base-url (:uri image))
        js-image (js/Image.)]
    (set! (.-src js-image) image-url)
    (when (not= js/window.location.origin (.-origin (js/URL. image-url)))
      (set! (.-crossOrigin js-image) "anonymous"))
    (js/Promise. (fn [resolve]
                   (.addEventListener
                    js-image
                    "load"
                    #(resolve (assoc image
                                     :data js-image
                                     :width (.-width js-image)
                                     :height (.-height js-image))))))))

(defn- load-assets [data base-url]
  (let [buffer-promises (js/Promise.all (map #(load-buffer % base-url) (:buffers data)))
        image-promises (js/Promise.all (map #(load-image % base-url) (:images data)))]
    (-> (js/Promise.all [buffer-promises image-promises])
        (.then (fn [[buffers images]]
                 (assoc data
                        :buffers (into [] buffers)
                        :images (into [] images)))))))

(defn- resolve-buffer [data buffer-id]
  (get-in data [:buffers buffer-id]))

(defn- resolve-buffer-view [data buffer-view-id]
  (let [buffer-view (get-in data [:bufferViews buffer-view-id])]
    (-> (assoc buffer-view
               :buffer (resolve-buffer data (:buffer buffer-view))
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
  (-> primitive
      (cond->
       (contains? primitive :indices)
        (assoc :indices (resolve-accessor data (:indices primitive)))

        (contains? primitive :material)
        (assoc :material (get-in data [:materials (:material primitive)])))

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
    (update mesh :primitives #(resolve-primitives data %))))

(defn- resolve-texture [data texture-id]
  (let [texture (get-in data [:textures texture-id])]
    (-> texture
        (update :source #(get-in data [:images %]))
        (update :sampler #(get-in data [:samplers %])))))

(defn- resolve-pbr [data pbr]
  (cond-> pbr
    (contains? pbr :baseColorTexture)
    (update :baseColorTexture #(resolve-texture data (:index %)))

    (contains? pbr :metallicRoughnessTexture)
    (update :metallicRoughnessTexture #(resolve-texture data (:index %)))))

(defn- resolve-material [data material-id]
  (let [material (get-in data [:materials material-id])]
    (update material :pbrMetallicRoughness #(resolve-pbr data %))))

(defn- resolve-all-materials [data]
  (assoc data :materials
         (into [] (map #(resolve-material data %) (range (count (:materials data)))))))

(defn- resolve-all-meshes [data]
  (assoc data :meshes
         (into [] (map #(resolve-mesh data %) (range (count (:meshes data)))))))

(defn- preprocess [data]
  (-> data
      ; Order of resolution matters here
      (resolve-all-materials)
      (resolve-all-meshes)))

(defn- resolve-camera [data camera-id]
  (get-in data [:cameras camera-id]))

(defn- resolve-node [data node-id]
  (let [node (transient (get-in data [:nodes node-id]))]
    (->
     (cond-> node
       (contains? node :mesh)
       (assoc! :mesh (get-in data [:meshes (:mesh node)]))

       (contains? node :camera)
       (assoc! :camera (resolve-camera data (:camera node)))

       (contains? node :translation)
       (->
        (assoc! :position (apply vec3/create (:translation node)))
        (dissoc! :translation))

       (contains? node :rotation)
       (assoc! :rotation (apply quat/create (:rotation node)))

       (contains? node :scale)
       (assoc! :scale (apply vec3/create (:scale node))))
     (assoc! :matrix (if-let [m (:matrix node)] (apply mat4/create m) (mat4/create-identity)))
     (as-> node (let [matrix (:matrix node)]
                  (cond-> node
                    (not (:position node))
                    (assoc! :position (mat4/get-translation matrix))

                    (not (:rotation node))
                    (assoc! :rotation (mat4/get-rotation matrix))

                    (not (:scale node))
                    (assoc! :scale (mat4/get-scale matrix)))))
     (dissoc! :matrix)
     (persistent!))))


(defn- add-scene-node [scene gltf-data gltf-node parent-id]
  (let [new-node (if parent-id (scene/create-node gltf-node) (scene/root scene))]
    (reduce (fn [new-scene gltf-node-id]
              (add-scene-node new-scene
                              gltf-data
                              (resolve-node gltf-data gltf-node-id)
                              (:id new-node)))
            (if parent-id
              (scene/add-child scene new-node parent-id)
              scene)
            (:children gltf-node))))

(defn resolve-scene [data scene-id]
  (let [gltf-scene (get-in data [:scenes scene-id])
        scene (scene/create)]
    (add-scene-node scene data {:children (:nodes gltf-scene)} nil)))

(defn load-gltf [data base-url]
  (-> (load-assets data base-url)
      (.then #(preprocess %))
      (.then #(resolve-scene % (:scene %)))))
