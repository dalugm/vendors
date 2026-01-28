#!/usr/bin/env bb

(ns bt-tracker
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(def sources
  {:NGOSANG_ALL
   "https://ngosang.github.io/trackerslist/trackers_all.txt"
   :NGOSANG_ALL_IP
   "https://ngosang.github.io/trackerslist/trackers_all_ip.txt"
   :NGOSANG_BEST
   "https://ngosang.github.io/trackerslist/trackers_best.txt"
   :NGOSANG_BEST_IP
   "https://ngosang.github.io/trackerslist/trackers_best_ip.txt"
   :XIU2_BEST
   "https://trackerslist.com/best.txt"
   :XIU2_ALL
   "https://trackerslist.com/all.txt"
   :XIU2_HTTP
   "https://trackerslist.com/http.txt"})

(defn fetch-trackers [url]
  (try
    (->> (slurp url)
         str/split-lines
         (map str/trim)
         (remove str/blank?)
         vec)
    (catch Exception e
      (println "Failed to get url:" url)
      (println (ex-message e))
      [])))

(defn update-aria2-conf! [trackers config-path]
  (let [path (if (str/blank? config-path)
               (fs/path (System/getProperty "user.home") ".aria2" "aria2.conf")
               (fs/path config-path))
        file (fs/file path)]
    (if-not (fs/exists? file)
      (do (println "Failed to read config file.")
          (println (ex-message (ex-info "File not found" {:path (str path)}))))
      (let [content (slurp file)
            pattern #"(?m)^bt-tracker=.*$"]
        (if-not (re-find pattern content)
          (println "No bt-tracker configuration found in config.")
          (do (spit file
                    (str/replace content
                                 pattern
                                 (str "bt-tracker=" (str/join "," trackers))))
              (println "Trackers Updated.")))))))

(def cli-spec
  {:print {:alias :p :coerce :boolean
           :default false
           :desc "print trackers to console"}
   :update-aria2-path {:coerce :string :default ""
                       :desc "where to update aria2 trackers (default \"$HOME/.aria2/aria2.conf\")"}
   :source {:alias :s :coerce :string :default "XIU2_BEST"
            :desc "NGOSANG_{BEST,ALL}[_IP] XIU2_{BEST,ALL,HTTP} (default \"XIU2_BEST\")"}})

(defn -main [& args]
  (let [opts (cli/parse-opts args {:spec cli-spec})
        {:keys [print update-aria2-path source]} opts]

    (when (or (:help opts) (:h opts))
      (println (cli/format-opts {:spec cli-spec}))
      (System/exit 0))

    (println "Getting trackers ...")

    (let [trackers (->> (str/split source #",")
                        (map keyword)
                        (mapcat (fn [k]
                                  (if-let [url (k sources)]
                                    (fetch-trackers url)
                                    (do (println "Unknown source:" k)
                                        [])))))]
      (if print
        (do (doseq [t trackers] (println t))
            (println (count trackers) "tracker(s) total"))
        (update-aria2-conf! trackers update-aria2-path)))))

(apply -main *command-line-args*)

(comment
  (map keyword ["1" "2" "3"])
  (let [source-list (str/split "XIU2_BEST,XIU2_ALL" #",")]
    (->> source-list
         (map keyword)
         (map (fn [k] (k sources)))))

  (let [path (fs/path (System/getProperty "user.home") ".aria2" "aria2.conf")
        file (fs/file path)]
    (fs/exists? file))
  :done)
