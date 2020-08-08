(ns gltf.input)

(defonce input-state (atom {:initialized false
                            :buttons #{}}))

(defn- on-mouse-move-impl [e]
  (when-let [callback (get-in @input-state [:callbacks (.-target e) :on-mouse-move])]
    (callback (.-movementX e) (.-movementY e))))

(defn- on-key-down-impl [e]
  (let [buttons (:buttons @input-state)
        code (.-code e)
        new-buttons (conj buttons code)]
    (swap! input-state assoc :buttons new-buttons)
    (when-let [callback (and (not (contains? buttons code))
                             (get-in @input-state [:callbacks (.-target e) :on-key-down]))]
      (callback new-buttons code))))

(defn- on-key-up-impl [e]
  (let [buttons (:buttons @input-state)
        code (.-code e)
        new-buttons (disj buttons code)]
    (swap! input-state assoc :buttons new-buttons)
    (when-let [callback (get-in @input-state [:callbacks (.-target e) :on-key-up])]
      (callback new-buttons code))))

(defn- on-pointer-lock-change [canvas]
  (fn []
    (if (== js/document.pointerLockElement canvas)
      (.addEventListener canvas "mousemove" on-mouse-move-impl)
      (.removeEventListener canvas "mousemove" on-mouse-move-impl))))

(defn- on-canvas-click [e] (.requestPointerLock (.-target e)))

(defn init [canvas]
  (when-not (:initialized @input-state)
    (.addEventListener js/document "pointerlockchange" (on-pointer-lock-change canvas))
    (.addEventListener canvas "click" on-canvas-click)
    (.addEventListener canvas "keydown" on-key-down-impl)
    (.addEventListener canvas "keyup" on-key-up-impl)
    (swap! input-state assoc :initialized true)))

(defn on-mouse-move [canvas callback]
  (swap! input-state assoc-in [:callbacks canvas :on-mouse-move] callback))

(defn on-key-down [canvas callback]
  (swap! input-state assoc-in [:callbacks canvas :on-key-down] callback))

(defn on-key-up [canvas callback]
  (swap! input-state assoc-in [:callbacks canvas :on-key-up] callback))