(ns ^:figwheel-hooks malli.web
  (:require
    [cljsjs.codemirror]
    [cljsjs.codemirror.addon.edit.matchbrackets]
    [cljsjs.codemirror.addon.lint.lint]
    [cljsjs.codemirror.mode.clojure]
    [cljsjs.parinfer]
    [cljsjs.parinfer-codemirror]
    [clojure.string :as str]
    [fipp.clojure :as fipp]
    [goog.functions :as functions]
    [reagent.core :as r]
    [edamame.core :as e]
    [malli.core :as m]
    [malli.error :as me]
    [malli.provider :as mp]
    [malli.generator :as mg]
    [malli.json-schema :as mj]
    [malli.swagger :as ms])
  (:import [goog Uri]))

(defn read [x]
  (try (e/parse-string x) (catch js/Error _)))

(defn read-schema [x]
  (try (m/deserialize x) (catch js/Error _)))

(defn read-value [x]
  (try
    (let [v (e/parse-string x)]
      (if-not (keyword-identical? v :edamame.impl.parser/eof) v))
    (catch js/Error _)))

(defn pretty [x]
  (try (str/trim (with-out-str (fipp/pprint x))) (catch js/Error _ "")))

(defn inferred [value]
  (if (and value (not (str/blank? value)))
    (try (pretty (mp/provide [(read value)])) (catch js/Error _))
    ""))

(defn query [k f]
  (if-let [p (.getParameterValue (.parse Uri js/location) k)]
    (if-not (str/blank? p)
      (try
        (pretty (f (read (str/trim p))))
        (catch js/Error _ ""))
      "")))

(def models
  {:empty {}
   :any? {:schema any?}
   :address {:schema [:map
                      {:title "Address"}
                      [:id string?]
                      [:tags [:set keyword?]]
                      [:address
                       [:map
                        [:street string?]
                        [:city string?]
                        [:zip int?]
                        [:lonlat [:tuple double? double?]]]]]
             :value {:id "Lillan"
                     :tags #{:artesan :coffee :hotel}
                     :address {:street "Ahlmanintie 29"
                               :city "Tampere"
                               :zip 33100
                               :lonlat [61.4858322, 23.7854658]}}}
   :user {:schema [:map
                   {:title "User"}
                   [:name string?]
                   [:age [:and int? [:> 18]]]
                   [:gender {:optional true} [:enum :male :female :other]]
                   [:email [:re {:description "https://github.com/gfredericks/test.chuck/issues/46"
                                 :gen/fmap '(constantly "random@example.com")
                                 :error/message "should be email"}
                            #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$"]]]
          :value {:name "Tiina"
                  :age 81
                  :email "tiina@example.com"}}
   :xy {:schema [:and
                 [:map
                  [:x int?]
                  [:y int?]]
                 [:fn {:error/message "x should be greater than y"}
                  '(fn [{:keys [x y]}] (> x y))]]
        :value {:x 1, :y 2}}})

(defonce state* (r/atom {:schema (or (query "schema" m/form) (pretty (m/form (-> models :address :schema))))
                         :value (or (query "value" identity) (pretty (-> models :address :value)))}))
(defonce delayed-state* (r/atom nil))
(defonce mirrors* (r/atom {}))

(defonce delayed (functions/debounce
                   (fn [state]
                     (let [schema (read-schema (:schema state))
                           value (read-value (:value state))
                           inferred (try (mp/provide [value]) (catch js/Error _))]
                       (.replaceState js/window.history nil "" (str (.setParameterValue (.parse Uri js/location) "value" (:value state))))
                       (.replaceState js/window.history nil "" (str (.setParameterValue (.parse Uri js/location) "schema" (:schema state))))
                       (.setValue (@mirrors* "samples") (try (str/join "\n\n" (map pretty (mg/sample schema))) (catch js/Error _ "")))
                       (.setValue (@mirrors* "json-schema") (try (pretty (mj/transform schema)) (catch js/Error _ "")))
                       (.setValue (@mirrors* "swagger-schema") (try (pretty (ms/transform schema)) (catch js/Error _ "")))
                       (.setValue (@mirrors* "inferred") (pretty inferred))
                       (.setValue (@mirrors* "samples-inferred") (try (str/join "\n\n" (map pretty (mg/sample inferred))) (catch js/Error _ "")))
                       (reset! delayed-state* state))) 500))

(defn sync-delayed-state! []
  (when (not= @delayed-state* @state*)
    (delayed @state*)))

(r/track! sync-delayed-state!)

(defn reset-value! [value]
  (swap! state* assoc :value value)
  (.setValue (@mirrors* "value") (or value "")))

(defn reset-schema! [value]
  (swap! state* assoc :schema value)
  (.setValue (@mirrors* "schema") (or value "")))

(defn editor [id state* {:keys [value on-change]}]
  (r/create-class
    {:render (fn [] [:textarea
                     {:type "text"
                      :id id
                      :default-value (or value @state*)
                      :auto-complete "off"}])
     :component-did-mount (fn [this]
                            (let [opts (if value
                                         #js {:mode "clojure"
                                              :matchBrackets true
                                              :readOnly true
                                              :lineNumbers true}
                                         #js {:mode "clojure"
                                              :matchBrackets true
                                              :lineNumbers true})
                                  cm (.fromTextArea js/CodeMirror (r/dom-node this) opts)]
                              (js/parinferCodeMirror.init cm)
                              (.removeKeyMap cm)
                              (when-not value
                                (.on cm "change" #(let [value (.getValue %)]
                                                    (reset! state* value)
                                                    (when on-change (on-change value))))
                                (.setOption cm "extraKeys" #js {:Shift-Tab false
                                                                :Tab false}))
                              (swap! mirrors* assoc id cm)))
     :component-will-unmount (fn [] (.toTextArea (@mirrors* id)))}))

(defn examples []
  (let [reset! (fn [schema]
                 (fn []
                   (reset-schema! (some-> models schema :schema m/form pretty))
                   (reset-value! (some-> models schema :value pretty))
                   nil))]
    [:div.buttons
     [:button.btn.btn-sm.btn-outline-warning
      {:on-click (reset! :empty)}
      "empty"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :any?)}
      "any?"]
     [:button.btn.btn-sm.btn-outline-danger
      {:on-click (reset! :xy)}
      "xy-map"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :user)}
      "User"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :address)}
      "Address"]]))


(defn error [error]
  [:div.alert.alert-danger
   (update error :schema m/form)])

(defn valid [schema value]
  (try
    (let [valid? (m/validate schema value)]
      [:pre {:class (if valid? "alert alert-success" "alert alert-danger")}
       (str valid?)])
    (catch js/Error _ [:div.alert.alert-warning "???"])))

(defn errors [schema value]
  (try
    [:div
     (or (seq (for [[i error] (map-indexed vector (:errors (m/explain schema value)))
                    :let [error' (me/with-error-message error)]]
                [:div.alert.alert-danger
                 {:key i}
                 [:b (:message error') ": "]
                 [:span (-> error' (update :schema m/form) (dissoc :message) (cond-> (not (:type error')) (dissoc :type)) pr-str)]]))
         [:div.alert.alert-success "nil"])]
    (catch js/Error _ [:div.alert.alert-warning "???"])))

(defn code [id]
  (try [editor id nil {:value ""}]
       (catch js/Error _)))

(defn swagger-schema [schema]
  (let [schema (try (m/deserialize schema) (catch js/Error _))]
    [:<>
     [:h3 "Swagger2 Schema"]
     [code "swagger2-schema" (try (pretty (ms/transform schema)) (catch js/Error _ ""))]]))

(defn app []
  (let [schema (read-schema (-> state* deref :schema))
        value (read-value (-> state* deref :value))]

    [:div#malli.container
     [:div.row
      [:p.col-12.lead
       [:span [:a {:href "https://github.com/metosin/malli"
                   :target "_blank"}
               "malli"]
        " playground"]]]
     [:div
      [examples]
      [:h3 "Schema"]
      [editor "schema" (r/cursor state* [:schema])]
      [:h3 "Value"]
      [editor "value" (r/cursor state* [:value])]
      [:h3 "Valid"]
      [valid schema value]
      [:h3 "Errors"]
      [errors schema value]
      [:div
       {:class (if (not= @state* @delayed-state*) "overlay")}
       [:h3 "Sample values"]
       [code "samples"]
       [:h3 "JSON Schema"]
       [code "json-schema"]
       [:h3 "Swagger Schema"]
       [code "swagger-schema"]
       [:h3 "Inferred Schema"]
       [code "inferred"]
       [:h3 "Sample values from Inferred Schema"]
       [code "samples-inferred"]]]]))

(defn mount-app-element []
  (when-let [el (js/document.getElementById "app")]
    (r/render-component [app] el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
