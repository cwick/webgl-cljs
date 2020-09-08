(ns gltf.scene (:require [gltf.math.vec3 :as vec3]
                         [gltf.math.mat4 :as mat4]
                         [gltf.math.quat :as quat]))

(defonce auto-id (atom 0))

(defn- assoc-node [scene node]
  (assoc-in scene [:nodes (:id node)] node))

(defn create-node
  ([] (create-node nil))

  ([data]
   (merge
    {:position (vec3/create)
     :rotation (quat/create-identity)
     :scale (vec3/create 1 1 1)}
    data
    {:id (swap! auto-id inc)
     :children []
     :dirty? true})))

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
  (let [new-scene (create)
        new-root-id (:id (root new-scene))]
    (-> new-scene
        (update :nodes #(merge % (:nodes scene) (:nodes other)))
        (assoc :camera (:camera scene))
        (add-child (root scene) new-root-id)
        (add-child (root other) new-root-id))))

; Note: Calling delay directly in the node update function
; causes massive memory leaks, somehow related to
; creating anonymous functions in JS and stuff being retained in
; the closure.
(defn- lazy-translation [t] (delay (mat4/get-translation t)))
(defn- lazy-scale [t] (delay (mat4/get-scale t)))
(defn- lazy-rotation [t] (delay (mat4/get-rotation t)))

(declare update-node-transforms)
(defn- continue-node-updates [scene scene-nodes parent-transform children]
  (reduce
   #(update-node-transforms
     scene %1 parent-transform %2)
   scene-nodes
   children))

(defn- force-update-node-transforms [scene nodes parent-transform node]
  (let [global-transform
        (mat4/mult-mat parent-transform (mat4/create-rotation-translation-scale
                                         (:rotation node)
                                         (:position node)
                                         (:scale node)))

        global-position
        (lazy-translation global-transform)

        global-scale
        (lazy-scale global-transform)

        global-rotation
        (lazy-rotation global-transform)

        updated-nodes
        (assoc! nodes (:id node)
                (-> (transient node)
                    (assoc! :global-transform global-transform
                            :global-position global-position
                            :global-scale global-scale
                            :global-rotation global-rotation
                            :dirty? false)
                    (persistent!)))]
    (reduce
     #(force-update-node-transforms
       scene %1 global-transform %2)
     updated-nodes
     (children scene node))))

(defn- update-node-transforms [scene nodes parent-transform node]
  (if (:dirty? node)
    (force-update-node-transforms scene nodes parent-transform node)
    (continue-node-updates scene nodes (:global-transform node) (children scene node))))

(defn update-transforms [scene]
  (let [nodes
        (update-node-transforms
         scene
         (transient (:nodes scene))
         (mat4/create-identity)
         (root scene))]
    (assoc scene :nodes (persistent! nodes))))

