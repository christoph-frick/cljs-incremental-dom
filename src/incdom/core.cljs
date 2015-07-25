(ns ^:figwheel-always incdom.core
    (:require))

(enable-console-print!)

(defn- elem->tag-name 
  [elem]
  (if (keyword? elem)
    (name elem)
    (str elem)))

; TODO: what's the sane default?
(defn- decide-static 
  [kw]
  (if (= (namespace kw) "static") 
    :static 
    :dynamic))

(defn- attrs->dynamic-static-attrs 
  [attrs]
  (->> attrs
       (group-by #(decide-static (first %)))
       (map 
         (fn [[k vs]] 
           [k (mapcat (fn [[k v]] 
                        [(name k) (str v)]) 
                      vs)]))  
       (into {})))

(defn- apply-incdom 
  [incdom-fn elem attrs]
  (let [{:keys [static dynamic]} (attrs->dynamic-static-attrs attrs)]
    (.apply incdom-fn js/IncrementalDOM
            (into-array (concat 
                          [(elem->tag-name elem) 
                           (:key attrs) 
                           (clj->js static)]
                          dynamic)))))

(defn- element-void
  ([elem]
   (element-void elem {}))
  ([elem attrs]
   (apply-incdom (.-elementVoid js/IncrementalDOM) elem attrs)))

(defn- element-open 
  ([elem]
   (element-open elem {}))
  ([elem attrs]
   (apply-incdom (.-elementOpen js/IncrementalDOM) elem attrs)))

(defn- element-close
  ([elem]
   (.elementClose js/IncrementalDOM elem)))

(defn- text [txt]
  (.text js/IncrementalDOM (str txt)))

(defn- patch [root fn]
  (.patch js/IncrementalDOM root fn))

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
          (if (keyword? (first r))
            (hiccup->incremental-dom r)
            (text r)))
        (element-close elem)))))

(defonce state (atom ()))

(defn render! []
  (patch (.getElementById js/document "app")
         (fn []
           (hiccup->incremental-dom 
             [:div {:static/class "main"}
              [:div ; noattr
               "Some plain text"
               " "
               [:strong "Some strong text"]]
              [:div {:class "row"}
               [:div {:static/rel-data "test" :class "bold"}
                [:label "A Label"]
                [:input {:static/type "text" :value (count @state)}]]
               (concat
                 [:div {:class "state"}]
                 (for [d @state] [:div (str d)]))]]))))

(add-watch state :render render!)

(swap! state conj (js/Date.))

(defn on-js-reload [])

