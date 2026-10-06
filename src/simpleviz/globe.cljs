(ns simpleviz.globe
  (:require [simpleviz.world-model :as m]))

;; A world-model node's globe, drawn inside the node by the diagram painter:
;; the earth, the satellites on their orbits and the ground stations,
;; through wm/project (orthographic), plus the control bar. Only
;; calls the SVG recorder has (no gradients, no clipping), so exports
;; carry it too: whatever falls outside the globe's rect is left out.
;; Holds the view state — one camera per world-model node, one clock
;; for all — outside the app state, so turning and the clock repaint
;; without re-rendering.

(def CLASS-COLORS {"leo" "#16a34a" "meo" "#d97706" "geo" "#9333ea"})
(def ^:private STATION "#dc2626")
(def ^:private OCEAN "#2c64a6")
(def ^:private OCEAN-LIT "#5d9ae0")

(def DEFAULT-CAMERA {:yaw 0.6 :pitch 0.35 :zoom 1})

;; scene id ("n:eo") -> {:yaw :pitch :zoom}
(def cameras (atom {}))
(def clock (atom {:t 0 :playing true :speed 600 :last nil}))

(defn camera [id] (get @cameras id DEFAULT-CAMERA))

(defn update-camera! [id f & args]
  (swap! cameras (fn [cs] (assoc cs id (apply f (get cs id DEFAULT-CAMERA) args)))))

(defn turn
  "Camera c turned by a drag of dx, dy screen pixels: grab the globe
  and move it."
  [c dx dy]
  (let [a 0.008]
    (assoc c
           :yaw (+ (:yaw c) (* a dx))
           :pitch (js/Math.max (/ js/Math.PI -2)
                               (js/Math.min (/ js/Math.PI 2) (+ (:pitch c) (* a dy)))))))

(defn press!
  "Do what control-bar button `action` of world-model node `id` does."
  [id action]
  (case action
    "play" (swap! clock (fn [c] (assoc c :playing (not (:playing c)) :last nil)))
    "speed" (swap! clock (fn [c]
                           (let [i (.indexOf m/SPEEDS (:speed c))]
                             (assoc c :speed (nth m/SPEEDS (mod (inc i) (count m/SPEEDS)))))))
    "zoom-in" (update-camera! id update :zoom (fn [z] (js/Math.min 40 (* z 1.4))))
    "zoom-out" (update-camera! id update :zoom (fn [z] (js/Math.max 0.4 (/ z 1.4))))
    "reset" (do (swap! cameras dissoc id) (swap! clock assoc :t 0))
    nil))

(defn advance!
  "Run the clock to animation time `now` (ms): by the real time since
  the last call (at most 0.1 s, so a hidden tab doesn't jump) times its
  speed. True when it moved."
  [now]
  (let [{:keys [playing last t speed]} @clock]
    (if playing
      (let [dt (if (some? last) (js/Math.min 0.1 (/ (- now last) 1000)) 0)]
        (swap! clock assoc :t (+ t (* dt speed)) :last now)
        (pos? dt))
      (do (when (some? last) (swap! clock assoc :last nil)) false))))

(defn- inside? [r p]
  (and (>= (:x p) (:x r)) (<= (:x p) (+ (:x r) (:w r)))
       (>= (:y p) (:y r)) (<= (:y p) (+ (:y r) (:h r)))))

(defn- polyline!
  "Stroke the screen points pts: segments touching a :hidden point (behind
  the earth) at `hidden-alpha` dashed (0: not at all), the rest solid;
  segments leaving rect r are left out."
  [ctx r pts color width alpha hidden-alpha]
  (set! (.-strokeStyle ctx) color)
  (set! (.-lineWidth ctx) width)
  (doseq [hidden? [true false]]
    (when (or (not hidden?) (pos? hidden-alpha))
      (set! (.-globalAlpha ctx) (* alpha (if hidden? hidden-alpha 1)))
      (when hidden? (.setLineDash ctx [3 4]))
      (.beginPath ctx)
      (loop [i 1 open? false]
        (when (< i (count pts))
          (let [a (nth pts (dec i)) b (nth pts i)
                on? (and (= hidden? (or (:hidden a) (:hidden b)))
                         (inside? r a) (inside? r b))]
            (when on?
              (when-not open? (.moveTo ctx (:x a) (:y a)))
              (.lineTo ctx (:x b) (:y b)))
            (recur (inc i) on?))))
      (.stroke ctx)
      (.setLineDash ctx [])))
  (set! (.-globalAlpha ctx) alpha))

(defn- disc!
  "Fill the disc at cx, cy of radius r as far as it lies in rect g: a
  polygon around it with its points pulled into g (no clipping in the
  SVG recorder) — the whole rect once the earth fills it."
  [ctx g cx cy r color]
  (let [cl (fn [v lo hi] (js/Math.max lo (js/Math.min hi v)))]
    (.beginPath ctx)
    (doseq [k (range 96)]
      (let [a (* 2 js/Math.PI (/ k 96))
            x (cl (+ cx (* r (js/Math.cos a))) (:x g) (+ (:x g) (:w g)))
            y (cl (+ cy (* r (js/Math.sin a))) (:y g) (+ (:y g) (:h g)))]
        (if (zero? k) (.moveTo ctx x y) (.lineTo ctx x y))))
    (.closePath ctx)
    (set! (.-fillStyle ctx) color)
    (.fill ctx)))

(defn- label! [ctx text x y color halo]
  (set! (.-font ctx) "10px system-ui, sans-serif")
  (set! (.-textAlign ctx) "left")
  (set! (.-lineWidth ctx) 3)
  (set! (.-strokeStyle ctx) halo)
  (.strokeText ctx text x y)
  (set! (.-fillStyle ctx) color)
  (.fillText ctx text x y))

(defn- draw-bar!
  "The control bar: the buttons left, the clock and contact count right."
  [ctx layout palette contacts text?]
  (when text?
    (let [{:keys [playing speed t]} @clock
          bar (:bar layout)
          ty (+ (:y bar) (/ (:h bar) 2) 3.5)]
      (doseq [b (:buttons layout)]
        (.beginPath ctx)
        (.roundRect ctx (:x b) (:y b) (:w b) (:h b) 3)
        (set! (.-fillStyle ctx) (:btn-fill palette))
        (.fill ctx)
        (set! (.-strokeStyle ctx) (:node-stroke palette))
        (set! (.-lineWidth ctx) 1)
        (.stroke ctx)
        (set! (.-textAlign ctx) "center")
        (set! (.-font ctx) "10px system-ui, sans-serif")
        (set! (.-fillStyle ctx) (:text palette))
        (.fillText ctx (m/button-label (:action b) playing speed)
                   (+ (:x b) (/ (:w b) 2)) (+ (:y b) (/ (:h b) 2) 3.5)))
      (set! (.-textAlign ctx) "right")
      (set! (.-font ctx) "10px ui-monospace, monospace")
      (set! (.-fillStyle ctx) (:sub palette))
      (.fillText ctx (str (m/fmt-time t) " · " contacts (if (= contacts 1) " contact" " contacts"))
                 (- (+ (:x bar) (:w bar)) 8) ty))))

(defn draw!
  "World-model node `item` (a scene node with :world-model) in graph units on
  ctx, in the painter's `palette`, at opacity `alpha`; text? draws the
  labels and the control bar."
  [ctx item palette alpha text?]
  (let [world-model (:world-model item)
        layout (m/node-layout item)
        g (:globe layout)
        c (camera (:id item))
        t (:t @clock)
        ;; zoom 1 fits the outermost orbit
        s (/ (* (:zoom c) 0.48 (js/Math.min (:w g) (:h g))) (m/extent world-model))
        ox (+ (:x g) (/ (:w g) 2))
        oy (+ (:y g) (/ (:h g) 2))
        screen (fn [p]
                 (let [q (m/project c p)]
                   {:x (+ ox (* s (nth q 0))) :y (- oy (* s (nth q 1)))
                    :depth (nth q 2) :hidden (m/hidden? q)}))
        rs (* s m/RE)
        halo (:node-fill palette)
        contacts (m/contacts world-model t)]
    (set! (.-globalAlpha ctx) alpha)
    ;; the earth, lit from the upper left
    (disc! ctx g ox oy rs OCEAN)
    (set! (.-globalAlpha ctx) (* alpha 0.45))
    (disc! ctx g (- ox (* 0.25 rs)) (- oy (* 0.25 rs)) (* 0.6 rs) OCEAN-LIT)
    (set! (.-globalAlpha ctx) alpha)
    ;; graticule every 30°, turning with the earth: only the near side
    (let [ring (fn [f n] (mapv (fn [k] (let [p (screen (f (* 360 (/ k n))))]
                                         (assoc p :hidden (neg? (:depth p)))))
                               (range (inc n))))]
      (doseq [lat [-60 -30 0 30 60]]
        (polyline! ctx g (ring (fn [a] (m/surface-point lat a m/RE t)) 72)
                   (if (zero? lat) "rgba(255,255,255,0.6)" "rgba(255,255,255,0.25)")
                   (if (zero? lat) 1 0.6) alpha 0))
      (doseq [lon (range 0 360 30)]
        (polyline! ctx g (ring (fn [a] (m/surface-point (- (/ a 2) 90) lon m/RE t)) 36)
                   (if (zero? lon) "rgba(255,255,255,0.5)" "rgba(255,255,255,0.25)")
                   (if (zero? lon) 1 0.6) alpha 0)))
    ;; the axis, poles a little outside the surface
    (let [n (screen [0 0 (* 1.25 m/RE)])]
      (polyline! ctx g [n (screen [0 0 m/RE])] (:sub palette) 1 alpha 0)
      (polyline! ctx g [(screen [0 0 (- m/RE)]) (screen [0 0 (* -1.25 m/RE)])] (:sub palette) 1 alpha 0)
      (when (and text? (inside? g n)) (label! ctx "N" (+ (:x n) 3) (:y n) (:sub palette) halo)))
    ;; orbits: the part behind the earth faint and dashed
    (doseq [sat (:satellites world-model)]
      (polyline! ctx g (mapv screen (m/orbit-path sat 120))
                 (get CLASS-COLORS (:orbit sat)) 1.2 alpha 0.3))
    ;; contacts: a dashed line from each station to each satellite it sees
    (let [sats (into {} (map (fn [x] [(:id x) x])) (:satellites world-model))
          gss (into {} (map (fn [x] [(:id x) x])) (:stations world-model))]
      (set! (.-strokeStyle ctx) (:accent palette))
      (set! (.-lineWidth ctx) 1)
      (.setLineDash ctx [4 3])
      (.beginPath ctx)
      (doseq [cn contacts]
        (let [a (screen (m/station-position (get gss (:station cn)) t))
              b (screen (m/sat-position (get sats (:sat cn)) t))]
          (when (and (inside? g a) (inside? g b))
            (.moveTo ctx (:x a) (:y a))
            (.lineTo ctx (:x b) (:y b)))))
      (.stroke ctx)
      (.setLineDash ctx []))
    ;; ground stations: triangles on the near side
    (doseq [gs (:stations world-model)]
      (let [p (screen (m/station-position gs t))]
        (when (and (not (neg? (:depth p))) (inside? g p))
          (.beginPath ctx)
          (.moveTo ctx (:x p) (- (:y p) 5))
          (.lineTo ctx (+ (:x p) 4) (+ (:y p) 2.5))
          (.lineTo ctx (- (:x p) 4) (+ (:y p) 2.5))
          (.closePath ctx)
          (set! (.-fillStyle ctx) STATION)
          (.fill ctx)
          (set! (.-strokeStyle ctx) "#fff")
          (set! (.-lineWidth ctx) 0.8)
          (.stroke ctx)
          (when text? (label! ctx (:name gs) (+ (:x p) 6) (+ (:y p) 3) "#fff" "rgba(0,0,0,0.5)")))))
    ;; satellites, back to front
    (doseq [[sat p] (sort-by (fn [[_ p]] (:depth p))
                             (mapv (fn [sat] [sat (screen (m/sat-position sat t))])
                                   (:satellites world-model)))]
      (when (inside? g p)
        (set! (.-globalAlpha ctx) (* alpha (if (:hidden p) 0.3 1)))
        (.beginPath ctx)
        (.arc ctx (:x p) (:y p) 3.5 0 (* 2 js/Math.PI))
        (set! (.-fillStyle ctx) (get CLASS-COLORS (:orbit sat)))
        (.fill ctx)
        (set! (.-strokeStyle ctx) halo)
        (set! (.-lineWidth ctx) 1.2)
        (.stroke ctx)
        (when (and text? (not (:hidden p)))
          (label! ctx (:name sat) (+ (:x p) 6) (- (:y p) 4) (:text palette) halo))
        (set! (.-globalAlpha ctx) alpha)))
    (draw-bar! ctx layout palette (count contacts) text?)
    (set! (.-globalAlpha ctx) 1)))
