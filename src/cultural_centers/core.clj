(ns cultural-centers.core
  (:require [clojure.data.json :as json]
            [clojure.math.numeric-tower :as math]
            [cheshire.core :as ch]))

(defn get-coord-place
  [place]
  (let [x (->> place
               :properties
               :COORD_X)
        y (->> place
               :properties
               :COORD_Y)]
    [x y]))

(defn filtered-place
  [place]
  (let [id (:id place)
        properties (:properties place)]
    {:id    id
     :name  (:NOM_LIEU properties)
     :city  (:COMMUNE properties)
     :geocoord (get-coord-place place)
     :postcode (:CODE_POSTAL properties)
     :dept (:DEP properties)
     }))

(defn parse-in
  [path]
  (let [data (:features (json/read-str (slurp path) :key-fn keyword))]
    (map #(assoc %1 :id %2) data (range)))
  )

(def cultural-places
  (parse-in "LIEU_CULTUREL_EXISTANT.geojson"))


(defn get-distance
  [[x1 y1] [x2 y2]]
  (let [x-diff (- x2 x1)
        y-diff (- y2 y1)]
    (math/sqrt(+
                (math/expt x-diff 2)
                (math/expt y-diff 2)))))

(defn close-place?
  [place-ref place-to-compare threshold]
  (let [coord-p1 (get-coord-place place-ref)
        coord-p2 (get-coord-place place-to-compare)
        distance (get-distance coord-p1 coord-p2) ]
    (when (< distance threshold)
      (filtered-place place-to-compare))))

(defn get-neighboors
  [place places threshold]
  (reduce #(if-let [candidate (close-place? place %2 threshold)] (update %1 :neighboors conj candidate) %1) (filtered-place place)  places))


(defn get-neighboors-of-places
  [threshold]
  (map #(get-neighboors %1 cultural-places threshold) cultural-places))

(defn write-json
  [threshold]
  (let [data ( get-neighboors-of-places threshold)
        file-name (str "cultural-places" "-Seuil-" threshold "m.json")]
    (spit file-name (ch/generate-string data {:pretty true}))))