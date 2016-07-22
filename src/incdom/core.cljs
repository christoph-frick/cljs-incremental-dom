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

(defn- convert-attr-name
  "Coerce an attribute name into a string"
  [attr-name]
  (cond
    (keyword? attr-name) (name attr-name)
    :else (str attr-name)))

(defn- convert-attr-value
  "Coerce an attribute value into a representation incdom allows to use"
  [attr-value]
  (cond
    (map? attr-value) (clj->js attr-value)
    (fn? attr-value) attr-value
    :else (str attr-value)))

(defn- attr
  "Render an attribute via incdom"
  [attr-name attr-value]
  (.attr js/IncrementalDOM 
         (convert-attr-name attr-name) 
         (convert-attr-value attr-value)))

(defn- element-open 
  "Render an hiccup style opening tag via incdom"
  [elem attrs]
  (let [[tag-name class-map] (extract-classes elem)
        attrs (merge-with #(str %1 " " %2) attrs class-map)]
    (.elementOpenStart js/IncrementalDOM tag-name (:key attrs) nil)
    (run! (partial apply attr) attrs)
    (.elementOpenEnd js/IncrementalDOM tag-name)))

(defn- element-close
  "Render a closing tag via incdom"
  [elem]
  (.elementClose js/IncrementalDOM elem))

(defn- element-void
  "Render an empty tag via incdom"
  [elem attrs]
  (do
    (element-open elem attrs)
    (element-close elem)))

(defn- text 
  "Render an text node via incdom"
  [txt]
  (.text js/IncrementalDOM (str txt)))

(defn patch 
  "Apply a function, that calls incdom dom manipulations, to a root dom node"
  [root fn]
  (.patch js/IncrementalDOM root fn))

; this might not even be close to the capabilities of hiccup
(defn hiccup->incremental-dom
  "Run incdom manipulation for a hiccup vector"
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

;;; for live testing

(defonce state (atom ()))

(defn render! []
  (bench 
    #(patch (.getElementById js/document "app")
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
                    (for [d @state] [:div {:style {:font-weight "bold"}} (str d)]))]])))))

(add-watch state :render render!)

(swap! state conj (js/Date.))

(defn on-js-reload [])

