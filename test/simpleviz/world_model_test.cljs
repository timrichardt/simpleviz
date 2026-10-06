(ns simpleviz.world-model-test
  (:require ["node:test" :refer [test]]
            ["node:assert/strict$default" :as assert]
            [simpleviz.world-model :as m]))

(defn- close? [a b eps] (< (js/Math.abs (- a b)) eps))

(test "world-model-of: only a map is a world-model"
  (fn []
    (assert/deepEqual (m/world-model-of {"world-model" {"satellites" {}}}) {"satellites" {}})
    (assert/ok (nil? (m/world-model-of {"world-model" true})))
    (assert/ok (nil? (m/world-model-of {})))))

(test "parse fills a satellite in from its class's defaults"
  (fn []
    (let [{:keys [satellites problems]}
          (m/parse {"satellites" {"b" {"orbit" "meo"}
                                  "a" {"orbit" "leo" "inclination" 98 "raan" 30 "name" "Sentinel"}}})]
      (assert/deepEqual problems [])
      (assert/deepEqual (mapv :id satellites) ["a" "b"] "sorted by id")
      (let [[a b] satellites]
        (assert/equal (:name a) "Sentinel")
        (assert/equal (:altitude a) 550)
        (assert/equal (:inclination a) 98)
        (assert/equal (:raan a) 30)
        (assert/equal (:name b) "b")
        (assert/equal (:altitude b) 20200)
        (assert/equal (:inclination b) 55)
        ;; a LEO at 550 km goes round in about 95 minutes
        (assert/ok (close? (/ (:period a) 60) 95.6 0.5) (str (:period a)))))))

(test "the orbit class follows the altitude when :orbit is missing"
  (fn []
    (let [{:keys [satellites]} (m/parse {"satellites" {"x" {"altitude" 800}
                                                       "y" {"altitude" 23222}
                                                       "z" {}}})]
      (assert/deepEqual (mapv :orbit satellites) ["leo" "meo" "leo"]))))

(test "a GEO satellite takes one sidereal day and can be placed by longitude"
  (fn []
    (let [{:keys [satellites]} (m/parse {"satellites" {"astra" {"orbit" "geo" "lon" 19.2}}})
          sat (first satellites)]
      (assert/ok (close? (:period sat) 86164 2) (str (:period sat)))
      (assert/equal (:phase sat) 19.2)
      (assert/equal (:raan sat) 0)
      ;; it stays over its longitude while the earth turns
      (doseq [t [0 3600 40000]]
        (let [p (m/sat-position sat t)
              g (m/surface-point 0 19.2 m/RE t)]
          (assert/ok (close? (m/elevation g p) 90 0.1) (str t)))))))

(test "bad entries are left out and explained"
  (fn []
    (let [{:keys [satellites stations problems]}
          (m/parse {"satellites" {"a" {"orbit" "heo"} "b" {"inclination" "steep"} "c" {}}
                    "ground-stations" {"x" {"lat" 100 "lon" 0} "y" {"lat" 1} "z" {"lat" 1 "lon" 2}}})]
      (assert/deepEqual (mapv :id satellites) ["c"])
      (assert/deepEqual (mapv :id stations) ["z"])
      (assert/equal (:min-elevation (first stations)) m/MIN-ELEVATION)
      (assert/equal (count problems) 4)
      (assert/ok (.includes (first problems) "unknown :orbit") (first problems)))
    (assert/deepEqual (:problems (m/parse {"satellites" [1 2]}))
                      [":satellites must be a map of id to settings"])))

(test "positions: a satellite stays on its orbit radius and inclination"
  (fn []
    (let [sat (first (:satellites (m/parse {"satellites" {"s" {"orbit" "leo" "inclination" 60}}})))]
      (doseq [t [0 500 1234 5000]]
        (let [[x y z] (m/sat-position sat t)]
          (assert/ok (close? (js/Math.hypot x y z) (:r sat) 1e-6))
          ;; never further from the equator than the inclination
          (assert/ok (<= (js/Math.abs z) (+ (* (:r sat) (js/Math.sin (* 60 m/DEG))) 1e-6)))))
      (let [path (m/orbit-path sat 8)]
        (assert/equal (count path) 9)
        (assert/ok (close? (nth (first path) 0) (nth (last path) 0) 1e-6) "the ring closes")))))

(test "elevation: straight up is 90, the horizon 0"
  (fn []
    (let [g [m/RE 0 0]]
      (assert/ok (close? (m/elevation g [(* 2 m/RE) 0 0]) 90 1e-9))
      (assert/ok (close? (m/elevation g [m/RE 1000 0]) 0 1e-9))
      (assert/ok (neg? (m/elevation g [(- m/RE) 0 0]))))))

(test "contacts: a satellite overhead is in contact, one on the far side not"
  (fn []
    (let [world-model (m/parse {"satellites" {"near" {"orbit" "geo" "lon" 0}
                                          "far" {"orbit" "geo" "lon" 180}}
                            "ground-stations" {"gs" {"lat" 0 "lon" 0}}})
          cs (m/contacts world-model 0)]
      (assert/deepEqual (mapv (fn [c] [(:station c) (:sat c)]) cs) [["gs" "near"]]))))

(test "project: the camera turns about the axis and tilts"
  (fn []
    (let [front {:yaw 0 :pitch 0}]
      ;; at yaw 0 the viewer looks along +y: -y is toward the viewer
      (assert/deepEqual (mapv js/Math.round (m/project front [0 -1 0])) [0 0 1])
      (assert/deepEqual (mapv js/Math.round (m/project front [0 0 1])) [0 1 0])
      ;; raised 90°, the north pole faces the viewer
      (assert/deepEqual (mapv js/Math.round (m/project {:yaw 0 :pitch (/ js/Math.PI 2)} [0 0 1]))
                        [0 0 1]))
    (assert/ok (m/hidden? [0 0 (- m/RE)]))
    (assert/ok (not (m/hidden? [0 0 m/RE])))
    (assert/ok (not (m/hidden? [(* 2 m/RE) 0 (- m/RE)])) "beside the earth is visible")))

(test "new-id derives from the name, else numbers the prefix"
  (fn []
    (assert/deepEqual (m/new-id "Sentinel 2A" [] "sat") {:id "sentinel-2a"})
    (assert/deepEqual (m/new-id "" ["sat-1" "sat-3"] "sat") {:id "sat-2"})
    (assert/deepEqual (m/new-id "  " [] "gs") {:id "gs-1"})
    (assert/ok (:error (m/new-id "a" ["a"] "sat")))))

(test "satellite-settings: class defaults, spread and errors"
  (fn []
    (assert/deepEqual (m/satellite-settings "leo" {} 0)
                      {:edn "{:orbit :leo :altitude 550 :inclination 53 :raan 0 :phase 0}"})
    (assert/deepEqual (m/satellite-settings "meo" {:name "Gal 1" :inclination "56" :raan ""} 2)
                      {:edn "{:name \"Gal 1\" :orbit :meo :altitude 20200 :inclination 56 :raan 90 :phase 60}"})
    (assert/deepEqual (m/satellite-settings "geo" {:lon "-30.5"} 1)
                      {:edn "{:orbit :geo :altitude 35786 :lon -30.5}"})
    (assert/ok (:error (m/satellite-settings "leo" {:inclination "200"} 0)))
    (assert/ok (:error (m/satellite-settings "leo" {:raan "east"} 0)))))

(test "station-settings needs a latitude and a longitude"
  (fn []
    (assert/deepEqual (m/station-settings {:name "Kiruna" :lat "67.86" :lon "20.96"})
                      {:edn "{:name \"Kiruna\" :lat 67.86 :lon 20.96}"})
    (assert/ok (:error (m/station-settings {:lat "" :lon "1"})))
    (assert/ok (:error (m/station-settings {:lat "1" :lon "x"})))))

(test "the edit ops address one entry inside :world-model"
  (fn []
    (assert/deepEqual (m/set-entry-op "ops" "satellites" "s1" "{:orbit :leo}")
                      {:op "set-attr" :section "nodes" :id "ops" :attr "world-model"
                       :path ["satellites" "s1"] :value "{:orbit :leo}" :fallback false})
    (assert/deepEqual (m/remove-entry-op "ops" "ground-stations" "gs")
                      {:op "del-attr" :section "nodes" :id "ops" :attr "world-model"
                       :path ["ground-stations" "gs"]})))

(test "time and period formats"
  (fn []
    (assert/equal (m/fmt-time 0) "T+00:00")
    (assert/equal (m/fmt-time (+ 86400 3600 120)) "T+1d 01:02")
    (assert/equal (m/fmt-period 5700) "95 min")
    (assert/equal (m/fmt-period 43082) "12.0 h")))

(test "node-layout: globe under the header, buttons in the bottom bar"
  (fn []
    (let [{:keys [globe bar buttons]} (m/node-layout {:x 10 :y 20 :w 300 :h 320})]
      (assert/deepEqual bar {:x 10 :y (- 340 m/BAR-H) :w 300 :h m/BAR-H})
      (assert/equal (+ (:y globe) (:h globe)) (:y bar))
      (assert/deepEqual (mapv :action buttons) ["play" "speed" "zoom-out" "zoom-in" "reset"])
      (doseq [b buttons]
        (assert/ok (and (>= (:y b) (:y bar)) (<= (+ (:y b) (:h b)) (+ (:y bar) (:h bar)))))
        (assert/ok (<= (+ (:x b) (:w b)) (+ 10 300)))))))
