(ns mass-spring.graphics
  (:use [penumbra opengl]
        [cantor])
  (:require [penumbra.app :as app]
            [penumbra.text :as text]
            [penumbra.time :as time]
            [penumbra.data :as data]))

(defn rand-phob []
  {:force (vec3 0 0 0)
   :vel   (apply vec3 (take 3 (repeatedly #(- (rand 1) 0.5))))
   :pos   (apply vec3 (take 3 (repeatedly #(- (rand 2) 1))))})

(defn init [state]
  (app/title! "Mass-Spring Physics")
  (app/vsync! true)
  (app/key-repeat! false)
  (assoc state :frame 0
               :phobs (take 4 (repeatedly #(rand-phob)))))

(defn draw-sphere [vertices]
  (doseq [arcs (partition 2 1 vertices)]
    (draw-quad-strip
     (doseq [[a b] (map list (first arcs) (second arcs))]
       (vertex a) (vertex b)))))

(defn sphere-vertices [lod]
  (for [theta (range 0 361 (/ 360 lod))]
    (for [phi (range -90 91 (/ 180 (/ lod 2)))]
      (cartesian (polar3 theta phi)))))

(defn draw-phob [phob]
  (push-matrix
   (apply translate (:pos phob))
   (scale 0.1 0.1 0.1)
   (draw-sphere (sphere-vertices 24))))

(defn display [[dt time] state]
  ;(text/write-to-screen (str (:frame state) " frame") 0 40)
  (text/write-to-screen (str (int (/ (:frame state) time)) " fps") 0 40)
  (with-render-mode :wireframe
    (doseq [phob (:phobs state)]
      (draw-phob phob)))
  (app/repaint!))

(defn update-phob [phob dt]
  (let [vel* (add (:vel phob) (mul (:force phob) dt))
        pos* (add (:pos phob) (mul vel* dt))]
    (assoc phob :vel vel* :pos pos*)))
  
(defn update-physics [phobs dt]
  (map #(update-phob % dt) phobs))

(defn update [[dt time] state]
  (assoc state :frame (+ 1 (:frame state))
               :phobs (update-physics (:phobs state) dt)))

(defn start []
  (app/start {:init init
              :display display
              :update update}
             {}))

(start)