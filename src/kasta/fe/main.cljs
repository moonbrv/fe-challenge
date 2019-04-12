(ns kasta.fe.main
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [rum.core :as rum]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [cljs-time.core :refer [within? now]]
            [cljs-time.coerce :refer [from-string]]))

;; ----- UTILS -----
(defn campaign-active? [campaign]
  "campaign is active if current time between start and finish time of campaign"
  (let [start-time (from-string (:starts_at campaign))
        end-time (from-string (:finishes_at campaign))]
    (within? start-time end-time (now))))

(defn generate-tag-map [available-filters]
  "from menu list of response from serve we generate hash map,
  where the key is a TAG like 'A' or 'F' and value is whole menu item object
  {'A' {:tag ['A'] ... } 'F' {:tag ['F'] ...}"
  (reduce (fn [xs x]
            (reduce (fn [acc tag]
                      (assoc acc tag x)) xs (:tag x))) {} available-filters))

(defn get-tags-from-campaigns [campaigns]
  "generate set of tags from :tags string from campaign
  from the list of all active campaigns
  'FMA' -> #{'F' 'M' 'A'}"
  (disj (reduce (fn [xs x]
                  (into xs (clojure.string/split (:tags x) #""))) #{} campaigns) ""))

(defn get-active-campaigns-filters [active-campaigns available-filters]
  "generate list of filters(available menu items) from list of active campaigns
  to avoid menu items that are not present in active campaigns"
  (let [available-tags (get-tags-from-campaigns active-campaigns)
        tag-map (generate-tag-map available-filters)]
    (map #(get tag-map %) available-tags)))

(defn get-matched-campaigns [campaigns-list filter-set]
  "if set filters(set of tags) is empty return all campaigns
  if filters is not empty return only those campaigns that have at least one matched
  tag letter with filter"
  (if (empty? filter-set)
    campaigns-list
    (filter (fn [camp]
              (let [tags-set (set (clojure.string/split (:tags camp) #""))]
                (not (empty? (clojure.set/intersection filter-set tags-set))))) campaigns-list)))

(defn filters-has-intersection? [tags filters]
  "check if vector of tags has at least one tag in filter set"
  (let [tag-set (set tags)]
    (not (empty? (clojure.set/intersection tag-set filters)))))

;; ----- MODEL -----
(defonce state (atom {:campaigns          []
                      :active-tags        #{}
                      :available-filters  []
                      :loading            false
                      :error              false}))

(def campaigns (rum/cursor-in state [:campaigns]))
(def active-filters (rum/cursor-in state [:active-tags]))
(def campaigns-filters (rum/cursor-in state [:available-filters]))

(defn merge-state [state-map]
  (swap! state merge state-map))

(defn fetch-campaigns []
  (merge-state {:loading true})
  (go (let [response (<! (http/get "/api/campaigns"))
            data (:body response)
            active-campaigns (filter campaign-active? (:items data))
            available-filters (get-active-campaigns-filters active-campaigns (:menu data))]
        (merge-state {:loading false})
        (if (= :no-error (:error-code response))
          (merge-state {:campaigns         active-campaigns
                        :available-filters available-filters})
          (merge-state {:error true})))))

;; ----- MIXINS -----
(def fetch-campaigns-mixin
  {:will-mount (fn [state]
                 (fetch-campaigns) state)})

;; ----- VIEWS -----
(rum/defc campaigns-card [campaign]
  [:div.box
   [:article.media
    [:div.media-content
     [:div.content
      [:p [:strong (:name campaign)] [:br] (:description campaign)]]]]])

(rum/defc campaigns-grid [campaigns-list]
  [:div.columns.is-multiline
   (for [campaign campaigns-list]
     [:div.column.is-half {:key (:id campaign)} (campaigns-card campaign)])])

(rum/defc buttons-list < rum/reactive []
  [:div.columns
   [(for [campaign-filter (rum/react campaigns-filters)]
      (let [all-active-filters (rum/react active-filters)
            tag (:tag campaign-filter)
            is-active-filter (filters-has-intersection? tag all-active-filters)]
        [:div.column {:key (:name campaign-filter)}
         [:button.button {:on-click (fn []
                                      (if is-active-filter
                                        (swap! active-filters #(reduce disj % tag))
                                        (swap! active-filters #(reduce conj % tag))))
                          :class    (if is-active-filter "is-primary" nil)}
          (:name campaign-filter)]]))]])

(rum/defc Root
  < fetch-campaigns-mixin
    rum/reactive
  []
  (cond
    (:loading (rum/react state)) [:section.hero.is-primary
                                  [:div.hero-body
                                   [:div.container
                                    [:h1.title "Loading..."]]]]
    (:error (rum/react state)) [:section.hero.is-danger
                                [:div.hero-body
                                 [:div.container
                                  [:h1.title "An error ocured during fetching data"]]]]
    :else [:section.section [:div.container
                             [:div.has-text-centered (buttons-list)]
                             (campaigns-grid (get-matched-campaigns (rum/react campaigns) (rum/react active-filters)))]]))


(defn ^:export trigger-render []
  (rum/mount (Root) (js/document.getElementById "content")))
