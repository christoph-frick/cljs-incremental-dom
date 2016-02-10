(ns ^:figwheel-always incdom.core
  (:require [clojure.string :refer [split join]]))

(enable-console-print!)

(def ^:private all-dot-re (js/RegExp. "\\." "g"))

(defn- bench
  [fn]
  (let [start (.now js/performance) 
        result (fn)
        end (.now js/performance)]
    (.info js/console (str "Took " (- end start) "ms"))
    result))

(defn- extract-classes
  "Extract tag and optional classes out of a keyword in the form :tag.cls1.cls2"
  [elem]
  (let [[tn & cls] (split (name elem) ".")]
    [tn (conj {} (when (seq? cls) [:class (join " " cls)]))]))

(defn- convert-attr
  "Convert an attr into a representation incdom allows ot use"
  [attr]
  (cond
    (map? attr) (clj->js attr)
    (fn? attr) attr
    :else (str attr)))

(defn- build-apply-attrs
  "Builds an JS array to be consumed via .apply for incdom void/open 
  <tag> <key?> <static> <k,v>*"
  [elem attrs]
  (let [[tag-name class-map] (extract-classes elem)
        attrs (merge-with #(str %1 " " %2) attrs class-map)]
    (reduce 
      (fn [a [k v]]
        (doto a
          (.push (name k))
          (.push (convert-attr v))))
      (array tag-name (:key attrs) nil)
      attrs)))

(defn- element-void
  [elem attrs]
  (.apply (.-elementVoid js/IncrementalDOM) js/IncrementalDOM (build-apply-attrs elem attrs)))

(defn- element-open 
  [elem attrs]
  (.apply (.-elementOpen js/IncrementalDOM) js/IncrementalDOM (build-apply-attrs elem attrs)))

(defn- element-close
  [elem]
  (.elementClose js/IncrementalDOM elem))

(defn- text 
  [txt]
  (.text js/IncrementalDOM (str txt)))

(defn patch [root fn]
  (bench #(.patch js/IncrementalDOM root fn)))

; this might not even be close to the capabilities of hiccup
(defn hiccup->incremental-dom
  [root]
  (let [[elem & remainder] root
        [attr & remainder] (if (map? (first remainder))
                             remainder
                             (conj remainder {}))]
    (if (empty? remainder)
      (element-void elem attr)
      (do
        (element-open elem attr)
        (doseq [r remainder]
          (cond 
            (vector? r) (hiccup->incremental-dom r)
            (sequential? r) (doseq [rr r] (hiccup->incremental-dom rr))
            :else (text r)))
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
                " "
                [:input {:type "text" :value (count @state)}]]
               (into
                 [:div {:class "state"}]
                 (for [d @state] [:div {:style {:font-weight "bold"}} (str d)]))]]))))

(add-watch state :render render!)

(swap! state conj (js/Date.))

(defn on-js-reload [])

