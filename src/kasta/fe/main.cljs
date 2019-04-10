(ns kasta.fe.main
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [rum.core :as rum]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]))

;; ----- MODEL -----
(defonce state (atom {:campaigns []
                      :active-filters #{}
                      :available-filters []}))

(def campaigns (rum/cursor-in state [:campaigns]))
(def active-filters (rum/cursor-in state [:active-filters]))

(defn fetch-campaigns []
  (go (let [response (<! (http/get "/api/campaigns"))
            data (:body response)]
        (swap! state merge {:campaigns (:items data)
                            :available-filters (:menu data)}))))

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
    [:div.container (campaigns-grid (rum/react campaigns) (rum/react active-filters))])


(defn ^:export trigger-render []
  (rum/mount (Root) (js/document.getElementById "content")))
