(ns simpleviz.world-model
  (:require [simpleviz.editor :as editor]))

;; A world-model node: a node whose :world-model attribute is a map
;; describing the world a mission plays in — satellites and ground
;; stations around the earth.
;; Pure data and math (no DOM), so node tests cover it. The node shows
;; the setting in 3D inside the diagram: transform.cljs sizes it,
;; globe.cljs paints it, app.cljs edits it from the inspector.
;;
;;   :world-model {:satellites      {:sat-1 {:orbit :leo :inclination 98 :raan 30}}
;;             :ground-stations {:svalbard {:lat 78.2 :lon 15.4}}}
;;
;; Orbits are circular and ideal (no perturbations), positions in km in an
;; earth-centred inertial frame whose x axis points at longitude 0 at
;; time 0 — an illustration of the setting, not an ephemeris.

(def RE "Earth's equatorial radius, km." 6378.137)
(def MU "Earth's gravitational parameter, km³/s²." 398600.4418)
(def OMEGA-E "Earth's rotation rate, rad/s (one sidereal day)." 7.2921159e-5)

(def DEG (/ js/Math.PI 180))

;; the orbit classes, with the defaults a satellite of that class gets
(def ORBITS
  {"leo" {:label "LEO" :altitude 550 :inclination 53}
   "meo" {:label "MEO" :altitude 20200 :inclination 55}
   "geo" {:label "GEO" :altitude 35786 :inclination 0}})

(def ORBIT-ORDER ["leo" "meo" "geo"])

(def MIN-ELEVATION "Default elevation mask of a ground station, degrees." 10)

(defn world-model-of
  "The :world-model map of an element's attrs, nil when it has none (or it
  isn't a map)."
  [attrs]
  (let [m (get attrs "world-model")]
    (when (map? m) m)))

(defn- orbit-for-altitude [alt]
  (cond (< alt 2000) "leo"
        (< alt 35000) "meo"
        :else "geo"))

(defn- section
  "The entries of section k (\"satellites\" …) as [id value] pairs sorted
  by id; a non-map section is a problem."
  [m k problems]
  (let [s (get m k)]
    (cond (nil? s) []
          (map? s) (sort-by first (js/Object.entries s))
          :else (do (.push problems (str ":" k " must be a map of id to settings")) []))))

(defn- num-field
  "Field k of settings v as a number within [lo hi], dflt when absent;
  nil (with a problem) when it is something else."
  [v k dflt lo hi what problems]
  (let [x (get v k)]
    (cond (nil? x) dflt
          (and (number? x) (<= lo x hi)) x
          :else (do (.push problems (str what ": :" k " must be a number from " lo " to " hi))
                    nil))))

(defn- parse-satellite [[id v] problems]
  (let [what (str "satellite " id)]
    (if-not (or (nil? v) (map? v))
      (do (.push problems (str what ": settings must be a map")) nil)
      (let [v (or v {})
            n (count problems)
            alt0 (num-field v "altitude" nil 100 100000 what problems)
            orbit (let [o (get v "orbit")]
                    (cond (some? o) (.toLowerCase (str o))
                          (some? alt0) (orbit-for-altitude alt0)
                          :else "leo"))
            dflt (get ORBITS orbit)]
        (if (nil? dflt)
          (do (.push problems (str what ": unknown :orbit " (pr-str (get v "orbit"))
                                   " (leo, meo or geo)"))
              nil)
          (let [geo? (= orbit "geo")
                alt (or alt0 (:altitude dflt))
                incl (num-field v "inclination" (:inclination dflt) 0 180 what problems)
                lon (when geo? (num-field v "lon" nil -180 360 what problems))
                raan (num-field v "raan" 0 -360 360 what problems)
                phase (num-field v "phase" 0 -360 360 what problems)
                r (+ RE alt)]
            (when (= n (count problems))
              {:id id
               :name (str (or (get v "name") id))
               :orbit orbit
               :altitude alt :inclination incl
               ;; a GEO satellite placed by longitude: at time 0 the
               ;; inertial and earth-fixed frames coincide
               :raan (if (some? lon) 0 raan)
               :phase (if (some? lon) lon phase)
               :r r
               :period (* 2 js/Math.PI (js/Math.sqrt (/ (* r r r) MU)))})))))))

(defn- parse-station [[id v] problems]
  (let [what (str "ground station " id)]
    (if-not (map? v)
      (do (.push problems (str what ": needs a map with :lat and :lon")) nil)
      (let [n (count problems)
            lat (num-field v "lat" nil -90 90 what problems)
            lon (num-field v "lon" nil -180 360 what problems)
            mask (num-field v "min-elevation" MIN-ELEVATION 0 90 what problems)]
        (cond
          (not= n (count problems)) nil
          (or (nil? lat) (nil? lon)) (do (.push problems (str what ": needs :lat and :lon")) nil)
          :else {:id id :name (str (or (get v "name") id))
                 :lat lat :lon lon :min-elevation mask})))))

(defn parse
  "The world-model map m as {:satellites [..] :stations [..] :problems [..]}:
  every satellite with its orbit filled in from its class's defaults
  (and :r, the orbit radius, and :period in seconds), every ground
  station with its elevation mask. An entry with a bad value is left
  out and explained in :problems."
  [m]
  (let [problems (js/Array.)
        sats (vec (keep (fn [e] (parse-satellite e problems)) (section m "satellites" problems)))
        stations (vec (keep (fn [e] (parse-station e problems))
                            (section m "ground-stations" problems)))]
    {:satellites sats :stations stations :problems (vec problems)}))

;; ---- positions ----

(defn sat-position
  "Satellite sat's position [x y z] (km, inertial) at time t (s)."
  [sat t]
  (let [r (:r sat)
        i (* (:inclination sat) DEG)
        o (* (:raan sat) DEG)
        u (+ (* (:phase sat) DEG) (* t (/ (* 2 js/Math.PI) (:period sat))))
        cu (js/Math.cos u) su (js/Math.sin u)
        co (js/Math.cos o) so (js/Math.sin o)]
    [(* r (- (* co cu) (* so su (js/Math.cos i))))
     (* r (+ (* so cu) (* co su (js/Math.cos i))))
     (* r su (js/Math.sin i))]))

(defn orbit-path
  "n+1 points around sat's orbit (inertial, so the same at every time),
  the last closing the ring."
  [sat n]
  (let [sat (assoc sat :phase 0)]
    (mapv (fn [k] (sat-position sat (* (/ k n) (:period sat)))) (range (inc n)))))

(defn surface-point
  "The point at lat/lon (degrees) on a sphere of radius r, inertial, at
  time t: the earth has turned OMEGA-E·t since time 0."
  [lat lon r t]
  (let [la (* lat DEG)
        lo (+ (* lon DEG) (* OMEGA-E t))]
    [(* r (js/Math.cos la) (js/Math.cos lo))
     (* r (js/Math.cos la) (js/Math.sin lo))
     (* r (js/Math.sin la))]))

(defn station-position [gs t] (surface-point (:lat gs) (:lon gs) RE t))

(defn elevation
  "Elevation (degrees) of point p seen from ground point g: the angle
  between the line to p and g's horizon plane."
  [g p]
  (let [[gx gy gz] g
        dx (- (nth p 0) gx) dy (- (nth p 1) gy) dz (- (nth p 2) gz)
        gl (js/Math.hypot gx gy gz)
        dl (js/Math.hypot dx dy dz)]
    (/ (js/Math.asin (/ (+ (* dx gx) (* dy gy) (* dz gz)) (* gl dl))) DEG)))

(defn contacts
  "Every station–satellite pair in line of sight at time t: the
  satellite above the station's elevation mask. [{:station id :sat id
  :elevation deg}]."
  [{:keys [satellites stations]} t]
  (vec (for [gs stations
             :let [g (station-position gs t)]
             sat satellites
             :let [el (elevation g (sat-position sat t))]
             :when (>= el (:min-elevation gs))]
         {:station (:id gs) :sat (:id sat) :elevation el})))

;; ---- the camera ----

(defn project
  "Point p seen by an orthographic camera turned by `yaw` about the
  earth's axis and raised by `pitch` above the equator (radians):
  [x up depth], depth growing toward the viewer."
  [{:keys [yaw pitch]} p]
  (let [[x y z] p
        cy (js/Math.cos yaw) sy (js/Math.sin yaw)
        x1 (- (* x cy) (* y sy))
        d (- (+ (* x sy) (* y cy)))
        cp (js/Math.cos pitch) sp (js/Math.sin pitch)]
    [x1 (- (* z cp) (* d sp)) (+ (* d cp) (* z sp))]))

(defn hidden?
  "Is projected point q behind the earth?"
  [q]
  (let [[x u d] q]
    (and (neg? d) (< (+ (* x x) (* u u)) (* RE RE)))))

(defn extent
  "The radius (km) the view must show: the outermost orbit, at least a
  margin around the earth."
  [{:keys [satellites]}]
  (reduce (fn [m s] (js/Math.max m (:r s))) (* 1.3 RE) satellites))

;; ---- the world-model node in the diagram ----

;; a world-model node is laid out this big: its name on top as usual, the
;; globe under it and a control bar at the bottom
(def NODE-MIN-W 300)
(def GLOBE-H 250)
(def BAR-H 26)

(def SPEEDS "Clock speeds the speed button cycles through, simulated s per s." [60 600 3600 21600])

(defn speed-label [s]
  (case s 60 "1 min/s" 600 "10 min/s" 3600 "1 h/s" 21600 "6 h/s" (str s "×")))

;; the control bar's buttons, left to right: [action width]
(def ^:private BUTTONS [["play" 22] ["speed" 54] ["zoom-out" 22] ["zoom-in" 22] ["reset" 22]])

(defn node-layout
  "Where a world-model node item's parts are, in graph units: :globe and
  :bar rects ({:x :y :w :h}) and the bar's :buttons ({:action :x :y :w
  :h}). The header above the globe keeps the node's usual height."
  [{:keys [x y w h]}]
  (let [bar-y (- (+ y h) BAR-H)
        gy (- bar-y GLOBE-H)]
    {:globe {:x (+ x 6) :y gy :w (- w 12) :h GLOBE-H}
     :bar {:x x :y bar-y :w w :h BAR-H}
     :buttons (loop [bx (+ x 6) bs BUTTONS acc []]
                (if (empty? bs)
                  acc
                  (let [[action bw] (first bs)]
                    (recur (+ bx bw 4) (rest bs)
                           (conj acc {:action action :x bx :y (+ bar-y 3) :w bw :h (- BAR-H 8)})))))}))

(defn button-label [action playing speed]
  (case action
    "play" (if playing "❚❚" "▶")
    "speed" (speed-label speed)
    "zoom-out" "−"
    "zoom-in" "+"
    "reset" "⟲"
    ""))

(defn button-title [action]
  (case action
    "play" "run / pause the clock"
    "speed" "clock speed (click for the next)"
    "zoom-out" "zoom out"
    "zoom-in" "zoom in"
    "reset" "reset the view and the clock"
    action))

;; ---- edits ----

(defn- edn-number [x]
  (str (/ (js/Math.round (* x 1000)) 1000)))

(defn new-id
  "The id for a new entry named `nm` among `taken` ids: derived from the
  name like node ids, else `prefix-N` with the first free N. {:id ..},
  or {:error ..} when the name's id is taken."
  [nm taken prefix]
  (let [id (editor/name->id nm)
        taken (set taken)]
    (cond
      (= id "") {:id (some (fn [n] (let [c (str prefix "-" n)] (when-not (contains? taken c) c)))
                           (range 1 10000))}
      (contains? taken id) {:error (str "\"" id "\" is already in the world model")}
      :else {:id id})))

(defn- parse-number
  "Text from an input as a number within [lo hi]; nil for blank,
  :bad for anything else."
  [text lo hi]
  (let [s (.trim (str (or text "")))]
    (if (= s "")
      nil
      (let [x (js/Number s)]
        (if (and (js/Number.isFinite x) (<= lo x hi)) x :bad)))))

(defn satellite-settings
  "The EDN text for a new satellite of class `orbit` from the add form's
  texts {:name :inclination :raan :lon}, or {:error ..}. Left blank, the
  inclination is the class default; the right ascension (or a GEO
  satellite's longitude) spreads the class's satellites 45° apart, so a
  new one doesn't sit on top of the last; `n` is how many it has."
  [orbit {:keys [name inclination raan lon]} n]
  (let [dflt (get ORBITS orbit)
        incl (parse-number inclination 0 180)
        raan (parse-number raan -360 360)
        lon (parse-number lon -180 360)]
    (cond
      (nil? dflt) {:error (str "unknown orbit " orbit)}
      (= incl :bad) {:error "inclination must be a number from 0 to 180"}
      (= raan :bad) {:error "RAAN must be a number from -360 to 360"}
      (= lon :bad) {:error "longitude must be a number from -180 to 360"}
      :else
      (let [nm (.trim (str (or name "")))
            spread (mod (* 45 n) 360)
            fields (if (= orbit "geo")
                     [[":lon" (edn-number (if (some? lon) lon spread))]]
                     [[":inclination" (edn-number (if (some? incl) incl (:inclination dflt)))]
                      [":raan" (edn-number (if (some? raan) raan spread))]
                      [":phase" (edn-number (mod (* 30 n) 360))]])]
        {:edn (str "{"
                   (.join (mapv (fn [[k v]] (str k " " v))
                                (concat (when (not= nm "") [[":name" (pr-str nm)]])
                                        [[":orbit" (str ":" orbit)]
                                         [":altitude" (edn-number (:altitude dflt))]]
                                        fields))
                          " ")
                   "}")}))))

(defn station-settings
  "The EDN text for a new ground station from the add form's texts
  {:name :lat :lon}, or {:error ..}."
  [{:keys [name lat lon]}]
  (let [la (parse-number lat -90 90)
        lo (parse-number lon -180 360)
        nm (.trim (str (or name "")))]
    (cond
      (or (nil? la) (= la :bad)) {:error "latitude must be a number from -90 to 90"}
      (or (nil? lo) (= lo :bad)) {:error "longitude must be a number from -180 to 360"}
      :else {:edn (str "{"
                       (if (= nm "") "" (str ":name " (pr-str nm) " "))
                       ":lat " (edn-number la) " :lon " (edn-number lo) "}")})))

(defn set-entry-op
  "The edit op storing settings `edn` as entry `id` of section k
  (\"satellites\" or \"ground-stations\") of node `node-id`'s :world-model."
  [node-id k id edn]
  {:op "set-attr" :section "nodes" :id node-id :attr "world-model"
   :path [k id] :value edn :fallback false})

(defn remove-entry-op
  "The edit op deleting entry `id` of section k of node `node-id`'s :world-model."
  [node-id k id]
  {:op "del-attr" :section "nodes" :id node-id :attr "world-model" :path [k id]})

(defn fmt-period
  "A period in seconds as \"95 min\" or \"12.0 h\"."
  [s]
  (if (< s 7200)
    (str (js/Math.round (/ s 60)) " min")
    (str (.toFixed (/ s 3600) 1) " h")))

(defn fmt-time
  "Time t (s) as \"T+2d 03:04\"."
  [t]
  (let [s (js/Math.floor t)
        d (js/Math.floor (/ s 86400))
        h (js/Math.floor (/ (mod s 86400) 3600))
        m (js/Math.floor (/ (mod s 3600) 60))
        pad (fn [x] (.padStart (str x) 2 "0"))]
    (str "T+" (if (pos? d) (str d "d ") "") (pad h) ":" (pad m))))
