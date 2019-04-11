(ns kasta.fe.main
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [rum.core :as rum]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [cljs-time.core :refer [within? now]]
            [cljs-time.coerce :refer [from-string]]))

;; ----- UTILS -----
(defn campaign-active? [campaign]
  (let [start-time (from-string (:starts_at campaign))
        end-time (from-string (:finishes_at campaign))]
    (within? start-time end-time (now))))

(defn generate-tag-map [available-filters]
  (reduce (fn [xs x]
            (reduce (fn [acc tag]
                      (assoc acc tag x)) xs (:tag x))) {} available-filters))

(defn get-tags-from-campaigns [campaigns]
  (disj (reduce (fn [xs x]
                  (into xs (clojure.string/split (:tags x) #""))) #{} campaigns) ""))

(defn get-active-campaigns-filters [active-campaigns available-filters]
  (let [available-tags (get-tags-from-campaigns active-campaigns)
        tag-map (generate-tag-map available-filters)]
    (map #(get tag-map %) available-tags)))

;; ----- MODEL -----
(defonce state (atom {:campaigns []
                      :active-filters #{}
                      :available-filters []}))

(def campaigns (rum/cursor-in state [:campaigns]))
(def active-filters (rum/cursor-in state [:active-filters]))

(defn fetch-campaigns []
  (go (let [response (<! (http/get "/api/campaigns"))
            data (:body response)
            active-campaigns (filter campaign-active? (:items data))
            available-filters (get-active-campaigns-filters active-campaigns (:menu data))]
        (swap! state merge {:campaigns active-campaigns
                            :available-filters available-filters}))))

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

(rum/defc campaigns-grid [campaigns-list active-filters]
  [:div.columns.is-multiline
   (for [campaign campaigns-list]
     [:div.column.is-half (rum/with-key (campaigns-card campaign) (:id campaign))])])

(rum/defc Root
  < fetch-campaigns-mixin
    rum/reactive
  []
  [:div.container
   [:pre (str (:available-filters (rum/react state)))]
   (campaigns-grid (rum/react campaigns) (rum/react active-filters))])


(defn ^:export trigger-render []
  (rum/mount (Root) (js/document.getElementById "content")))
