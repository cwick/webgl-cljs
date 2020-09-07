(ns gltf.scene (:require [gltf.math.vec3 :as vec3]
                         [gltf.math.mat4 :as mat4]
                         [gltf.math.quat :as quat]))

(defonce auto-id (atom 0))

(defn- assoc-node [scene node]
  (assoc-in scene [:nodes (:id node)] node))

(defn create-node
  ([] (create-node nil))

  ([data]
   (merge data
          {:id (swap! auto-id inc)
           :children []})))

(defn create []
  (let [node (create-node)]
    {:nodes {(:id node) node} :root (:id node)}))

(defn get-node [scene node-id]
  (if-let [node (get-in scene [:nodes node-id])]
    node
    (throw (js/Error. (str "Invalid node " node-id)))))

(defn root [scene] (get-node scene (:root scene)))

(defn update-node [scene node-id f & args]
  (assoc-node scene (apply f (get-node scene node-id) args)))

(defn add-child
  ([scene child] (add-child scene child (:id (root scene))))
  ([scene child parent-id]
   (-> scene
       (assoc-node child)
       (update-node parent-id
                    update :children #(conj % (:id child))))))

(defn children [scene node]
  (map #(get-node scene %) (:children node)))

(defn merge-scene [scene other]
  (let [root-node? (fn [[node-id _]] (contains? #{(:root scene) (:root other)} node-id))]
    (-> (create)
        (update :nodes #(merge % (:nodes scene) (:nodes other)))
        (update :nodes #(into {} (remove root-node? %)))
        (as-> new-scene
              (update-in
               new-scene
               [:nodes (:root new-scene) :children]
               #(vec (concat %
                             (:children (root scene))
                             (:children (root other)))))))))

(def no-translation (vec3/create))
(def no-rotation (quat/create-identity))
(def no-scale (vec3/create 1 1 1))

(defn- get-local-transform [node]
  (mat4/create-rotation-translation-scale
   (or (:rotation node) no-rotation)
   (or (:position node) no-translation)
   (or (:scale node) no-scale)))

(defn- update-node-transforms [scene nodes parent-transform node]
  (let [global-transform
        (mat4/mult-mat parent-transform (get-local-transform node))

        updated-nodes (assoc! nodes (:id node) (assoc node :global-transform global-transform))]
    (reduce
     #(update-node-transforms scene %1 global-transform %2)
     updated-nodes
     (children scene node))))

(defn update-transforms [scene]
  (let [nodes
        (update-node-transforms scene (transient (:nodes scene)) (mat4/create-identity) (root scene))]
    (assoc scene :nodes (persistent! nodes))))

