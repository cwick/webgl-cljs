(ns gltf.scene)

(defonce auto-id (atom 0))

(defn assoc-node [scene node]
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
