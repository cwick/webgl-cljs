(ns gltf.input-devices)

(defonce input-state (atom {:initialized false
                            :keyboard {:buttons #{}}
                            :mouse {:buttons #{}
                                    :axes {:relative-x 0
                                           :relative-y 0}}}))

(defn- get-callback-for-event [e kind]
  (get-in @input-state [:callbacks (.-target e) kind]))

(defn- on-mouse-move-impl [e]
  (let [dx (.-movementX e)
        dy (.-movementY e)]
    ; Accumulate dx and dy as multiple mouse move events can happen in between frames
    (swap! input-state #(-> %
                            (update-in [:mouse :axes :relative-x] + dx)
                            (update-in [:mouse :axes :relative-y] + dy)))

    (when-let [callback (get-callback-for-event e :on-mouse-move)]
      (callback dx dy))))

(defn- on-mouse-down-impl [e]
  (let [buttons (-> @input-state :mouse :buttons)
        button (.-button e)
        new-buttons (conj buttons button)]
    (swap! input-state assoc-in [:mouse :buttons] new-buttons)
    (when-let [callback (get-callback-for-event e :on-mouse-down)]
      (callback new-buttons button))))

(defn- on-mouse-up-impl [e]
  (let [buttons (-> @input-state :mouse :buttons)
        button (.-button e)
        new-buttons (disj buttons button)]
    (swap! input-state assoc-in [:mouse :buttons] new-buttons)
    (when-let [callback (get-callback-for-event e :on-mouse-up)]
      (callback new-buttons button))))

(defn- on-key-down-impl [e]
  (let [buttons (-> @input-state :keyboard :buttons)
        code (.-code e)
        new-buttons (conj buttons code)]
    (.preventDefault e)
    (swap! input-state assoc-in [:keyboard :buttons] new-buttons)
    ; Key down will get fired repeatedly by the browser for the same key (auto key repeat)
    ; Detect when that's happening and don't call the user back.
    (when-let [callback (and (not (contains? buttons code))
                             (get-callback-for-event e :on-key-down))]
      (callback new-buttons code))))

(defn- on-key-up-impl [e]
  (let [buttons (-> @input-state :keyboard :buttons)
        code (.-code e)
        new-buttons (disj buttons code)]
    (.preventDefault e)
    (swap! input-state assoc-in [:keyboard :buttons] new-buttons)
    (when-let [callback (get-callback-for-event e :on-key-up)]
      (callback new-buttons code))))

(defn- on-pointer-lock-change [canvas]
  (fn []
    (if (== js/document.pointerLockElement canvas)
      (do
        (.addEventListener canvas "mousemove" on-mouse-move-impl)
        (.addEventListener canvas "mousedown" on-mouse-down-impl)
        (.addEventListener canvas "mouseup" on-mouse-up-impl))
      (do
        (.removeEventListener canvas "mousemove" on-mouse-move-impl)
        (.removeEventListener canvas "mousedown" on-mouse-down-impl)
        (.removeEventListener canvas "mouseup" on-mouse-up-impl)))))

(defn- on-canvas-click [e] (.requestPointerLock (.-target e)))

(defn init [canvas]
  (when-not (:initialized @input-state)
    (.addEventListener js/document "pointerlockchange" (on-pointer-lock-change canvas))
    (.addEventListener canvas "click" on-canvas-click)
    (.addEventListener canvas "keydown" on-key-down-impl)
    (.addEventListener canvas "keyup" on-key-up-impl)
    (.addEventListener canvas "contextmenu" #(.preventDefault %))
    (swap! input-state assoc :initialized true)))

;; Need to put more thought into how callbacks are triggered (would likely need to fire at beginning of frame)
;; (defn on-mouse-move [canvas callback]
;;   (swap! input-state assoc-in [:callbacks canvas :on-mouse-move] callback))

;; (defn on-mouse-down [canvas callback]
;;   (swap! input-state assoc-in [:callbacks canvas :on-mouse-down] callback))

;; (defn on-mouse-up [canvas callback]
;;   (swap! input-state assoc-in [:callbacks canvas :on-mouse-up] callback))

;; (defn on-key-down [canvas callback]
;;   (swap! input-state assoc-in [:callbacks canvas :on-key-down] callback))

;; (defn on-key-up [canvas callback]
;;   (swap! input-state assoc-in [:callbacks canvas :on-key-up] callback))

(defn end-frame []
  (swap! input-state #(-> %
                          (assoc-in [:mouse :axes :relative-x] 0)
                          (assoc-in [:mouse :axes :relative-y] 0))))