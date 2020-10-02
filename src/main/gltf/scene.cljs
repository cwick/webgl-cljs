(ns gltf.scene (:require [gltf.math.vec3 :as vec3]
                         [gltf.math.mat4 :as mat4]
                         [gltf.math.quat :as quat]))

(defonce auto-id (atom 0))

(defn- assoc-node [scene node]
  (assoc-in scene [:nodes (:id node)] node))

(defn- assoc-node! [scene node]
  (assoc! scene :nodes
          (assoc! (:nodes scene)
                  (:id node)
                  node)))

(defn create-node
  ([] (create-node nil))

  ([data]
   (merge
    {:position (vec3/create)
     :rotation (quat/create-identity)
     :scale (vec3/create 1 1 1)}
    data
    {:id (swap! auto-id inc)
     :children []})))

(declare dirty-node)
(defn create []
  (let [root-node (create-node {:depth 0})
        scene {:nodes {(:id root-node) root-node}
               :root (:id root-node)}]
    (-> scene
        (dirty-node root-node))))

(defn get-node [scene node-id]
  (if-let [node (get-in scene [:nodes node-id])]
    node
    (throw (js/Error. (str "Invalid node " node-id)))))

(defn root [scene] (get-node scene (:root scene)))

(defn- update-node [scene node-id f & args]
  (assoc-node scene (apply f (get-node scene node-id) args)))

(defn- dirty-node? [scene node]
  (get-in scene [:dirty-nodes (:depth node) (:id node)]))

(defn- dirty-node [scene node]
  (update-in scene [:dirty-nodes (:depth node)] #(conj (or %1 #{}) (:id node))))

(defn- clean-node! [scene node]
  (assoc! scene :dirty-nodes
          (update (:dirty-nodes scene) (:depth node) #(disj (or %1 #{}) (:id node)))))

(defn set-position [scene node-id pos]
  (let [new-scene (update-node scene node-id assoc
                               :position pos)
        node (get-node scene node-id)]
    (dirty-node new-scene node)))

(defn add-child
  ([scene child] (add-child scene child (:id (root scene))))
  ([scene child parent-id]
   (let [parent (get-node scene parent-id)]
     (-> scene
         (assoc-node (assoc child :depth (inc (:depth parent))))
         (update-node parent-id
                      update :children #(conj % (:id child)))))))

(defn children [scene node]
  (map #(get-node scene %) (:children node)))

(defn- update-depth-values
  ([scene]
   (update-depth-values scene (root scene) -1))
  ([scene node parent-depth]
   (let [depth (inc parent-depth)]
     (reduce #(update-depth-values %1 %2 depth)
             (update-in scene [:nodes (:id node)]
                        assoc :depth depth)
             (children scene node)))))

(defn merge-scene [scene other]
  (let [new-scene (create)
        new-root-id (:id (root new-scene))]
    (-> new-scene
        (update :nodes #(merge % (:nodes scene) (:nodes other)))
        (assoc :camera (:camera scene))
        (add-child (root scene) new-root-id)
        (add-child (root other) new-root-id)
        (update-depth-values))))

(defn- update-node-transforms
  ([scene node] (update-node-transforms scene node (mat4/create-identity)))
  ([scene node parent-transform]
   (let [global-transform
         (mat4/mult-mat parent-transform (mat4/create-rotation-translation-scale
                                          (:rotation node)
                                          (:position node)
                                          (:scale node)))
         updated-scene
         (-> scene
             (assoc-node!
              (assoc node :global-transform global-transform))
             (clean-node! node))]
     (reduce
      #(update-node-transforms %1 %2 global-transform)
      updated-scene
      (children scene node)))))

(defn- update-nodes [scene node-ids]
  (reduce (fn [scene node-id]
            (let [node (get-node scene node-id)]
              (if (dirty-node? scene node)
                (update-node-transforms scene node)
                scene)))
          scene
          node-ids))

(defn update-transforms [scene]
  (let [transient-scene
        (assoc! (transient scene) :nodes (transient (:nodes scene)))

        new-scene
        (reduce-kv (fn [scene _ node-ids] (update-nodes scene node-ids))
                   transient-scene
                   (:dirty-nodes scene))]
    (-> new-scene
        (assoc! :nodes (persistent! (:nodes new-scene)))
        (persistent!))))