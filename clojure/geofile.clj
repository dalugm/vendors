#!/usr/bin/env bb

(ns geo-updater
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.http-client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private geo-file-sources
  {"github" ["https://github.com/Loyalsoldier/v2ray-rules-dat/releases/latest/download/geoip.dat"
             "https://github.com/Loyalsoldier/v2ray-rules-dat/releases/latest/download/geosite.dat"]
   "cdn" ["https://cdn.jsdelivr.net/gh/Loyalsoldier/v2ray-rules-dat@release/geoip.dat"
          "https://cdn.jsdelivr.net/gh/Loyalsoldier/v2ray-rules-dat@release/geosite.dat"]
   "fastly" ["https://fastly.jsdelivr.net/gh/Loyalsoldier/v2ray-rules-dat@release/geoip.dat"
             "https://fastly.jsdelivr.net/gh/Loyalsoldier/v2ray-rules-dat@release/geosite.dat"]})

(defn- extract-filename [url]
  (let [path (java.net.URI. url)
        filename (last (str/split (.getPath path) #"/"))]
    (if (or (str/blank? filename) (= filename "/"))
      "downloaded_file"
      (first (str/split filename #"\?")))))

(defn- download! [url save-path]
  (let [filename (extract-filename url)
        full-path (if (str/blank? save-path)
                    filename
                    (str (fs/path save-path filename)))
        tmp-path (str full-path ".tmp")]

    (println (format "Downloading %s..." filename))

    (try
      (when-not (str/blank? save-path)
        (fs/create-dirs save-path))

      (let [response (http/get url
                               {:headers {"User-Agent" "clojure-geo-updater/1.0"}
                                :as :stream})]

        (when (>= (:status response) 400)
          (throw (ex-info (format "HTTP %d" (:status response))
                          {:status (:status response)})))

        (with-open [in (:body response)
                    out (io/output-stream tmp-path)]
          (io/copy in out))

        (let [size (fs/size tmp-path)]
          (fs/move tmp-path full-path {:replace-existing true})
          (println (format "Saved %s (%d bytes)" full-path size))))

      (catch Exception e
        (when (fs/exists? tmp-path)
          (fs/delete tmp-path))
        (ex-info (format "Failed to download %s: %s" url (ex-message e))
                 {:url url :error e})))))

(defn- download-concurrently! [urls save-path]
  (let [futures (doall (map #(future (download! % save-path)) urls))
        results (map deref futures)
        errors (filter #(instance? clojure.lang.ExceptionInfo %) results)]
    errors))

(def ^:private cli-spec
  {:source {:alias :s :default "github" :desc "Source: github, cdn, fastly"}
   :path {:alias :p :default "" :desc "Path to save downloaded files"}})

(defn -main [& args]
  (let [opts (cli/parse-opts args {:spec cli-spec})
        {:keys [source path]} opts
        urls (get geo-file-sources source)]

    (when (or (:help opts) (:h opts))
      (println (cli/format-opts {:spec cli-spec}))
      (System/exit 0))

    (when-not urls
      (binding [*out* *err*]
        (println (format "Error: Unknown source '%s'" source)))
      (System/exit 1))

    (println "Downloading geo files...")

    (let [errors (download-concurrently! urls path)]
      (if (seq errors)
        (do (binding [*out* *err*]
              (println (format "Completed with %d errors:" (count errors)))
              (doseq [e errors]
                (println (format "  x %s" (ex-message e))))))
        (println "All files downloaded successfully")))))

(apply -main *command-line-args*)
