(ns status-im.models.network
  (:require [clojure.string :as string]
            [status-im.utils.ethereum.core :as ethereum]
            [status-im.utils.handlers-macro :as handlers-macro]
            [status-im.ui.screens.accounts.utils :as accounts.utils]))

(def url-regex
  #"https?://(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}")

(defn valid-rpc-url? [url]
  (boolean (re-matches url-regex (str url))))

(defn validate-string [{:keys [value]}]
  {:value value
   :error (string/blank? value)})

(defn validate-url [{:keys [value]}]
  {:value value
   :error (not (valid-rpc-url? value))})

(defn validate-manage [manage]
  (-> manage
      (update :url validate-url)
      (update :name validate-string)
      (update :chain validate-string)))

(defn valid-manage? [manage]
  (->> (validate-manage manage)
       vals
       (map :error)
       (not-any? identity)))

(defn- new-network [{:keys [random-id] :as cofx} network-name upstream-url type]
  (let [data-dir (str "/ethereum/" (name type) "_rpc")
        config   {:NetworkId      (ethereum/chain-keyword->chain-id type)
                  :DataDir        data-dir
                  :UpstreamConfig {:Enabled true
                                   :URL     upstream-url}}]
    {:id         (string/replace random-id "-" "")
     :name       network-name
     :config     config}))

(defn set-input [input-key value {:keys [db]}]
  {:db (-> db
           (update-in [:networks/manage input-key] assoc :value value)
           (update-in [:networks/manage] validate-manage))})

(defn save [{{:networks/keys [manage] :account/keys [account] :as db} :db :as cofx}]
  (when (valid-manage? manage)
    (let [{:keys [name url chain]} manage
          network                  (new-network cofx (:value name) (:value url) (:value chain))
          new-networks             (merge {(:id network) network} (:networks account))]
      (handlers-macro/merge-fx cofx
                               {:db       (dissoc db :networks/manage)
                                :dispatch [:navigate-back]}
                               (accounts.utils/account-update {:networks new-networks})))))

(defn edit [{db :db}]
  {:db       (update-in db [:networks/manage] assoc
                        :name  {:error true}
                        :url   {:error true}
                        :chain {:value :mainnet})
   :dispatch [:navigate-to :edit-network]})
