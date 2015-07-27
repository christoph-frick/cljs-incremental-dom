(ns ^:figwheel-always incdom.core
    (:require [clojure.string :as s]))

(enable-console-print!)

(defn- bench
  [fn]
  (let [start (.now js/performance) 
        result (fn)
        end (.now js/performance)]
    (.info js/console (str "Took " (- end start) "ms"))
    result))

(defn- de-keyword 
  [elem]
  (if (keyword? elem)
    (name elem)
    (str elem)))

(defn- extract-classes
  [elem]
  (let [[tn & cls] (s/split (de-keyword elem) ".")]
    [tn (if (empty? cls) {} {:class (s/join " " cls)})]))

(defn- flatten-attr
  [a v]
  (condp (fn [[a-name v-check] [a v]] (and (v-check v) (or (nil? a-name) (= a-name a)))) [a v]
    [:style map?] (s/join ";" (map (fn [[k v]] (str (de-keyword k) ":" v)) v))
    [nil fn?] v
    (str v)))

(defn- apply-incdom 
  [incdom-fn elem attrs]
  (let [[tag-name class-map] (extract-classes elem)
        attrs (merge-with #(str %1 " " %2) attrs class-map) 
        call-args (reduce 
                    (fn [a [k v]]
                      (.push a (name k))
                      (.push a (flatten-attr k v))
                      a)
                    (array tag-name (:key attrs) nil)
                    attrs)]
    (.apply incdom-fn js/IncrementalDOM call-args)))

(defn- element-void
  [elem attrs]
  (apply-incdom (.-elementVoid js/IncrementalDOM) elem attrs))

(defn- element-open 
  [elem attrs]
  (apply-incdom (.-elementOpen js/IncrementalDOM) elem attrs))

(defn- element-close
  [elem]
  (.elementClose js/IncrementalDOM elem))

(defn- text 
  [txt]
  (.text js/IncrementalDOM (str txt)))

(defn patch [root fn]
  (bench #(.patch js/IncrementalDOM root fn)))

(defn- frest [s] [(first s) (rest s)])

; this might not even be close to the capabilities of hiccup
(defn hiccup->incremental-dom
  [root]
  (let [[elem remainder] (frest root)
        [attr remainder] (if (map? (first remainder))
                           (frest remainder)
                           [{} remainder])]
    (if (empty? remainder)
      (element-void elem attr)
      (do
        (element-open elem attr)
        (doseq [r remainder]
          (condp #(%1 %2) r
            vector? (hiccup->incremental-dom r)
            sequential? (doseq [rr r] (hiccup->incremental-dom rr))
            (text r)))
        (element-close elem)))))

(defonce state (atom ()))

(defn render! []
  (patch (.getElementById js/document "app")
         (fn []
           (hiccup->incremental-dom 
             [:div.main
              [:div
               "Some plain text"
               " "
               [:strong "Some strong text"]]
              [:div.row
               [:div.bolder {:rel-data "test" :class "bold"}
                [:label "A Label"]
                [:input {:static/type "text" :value (count @state)}]]
               (into
                 [:div {:class "state"}]
                 (for [d @state] [:div {:style {:font-weight "bold"}} (str d)]))]]))))

(add-watch state :render render!)

(swap! state conj (js/Date.))

(defn on-js-reload [])

