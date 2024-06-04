(ns extract
  (:require
   ["@mozilla/readability" :as read]
   ["fs" :as fs]
   ["jsdom" :refer [JSDOM]]
   ["process" :refer [argv]]))

(defn make-readable [input output]
  (let [dom (JSDOM. (fs/readFileSync input "utf-8"))
        reader (read/Readability. dom.window.document {:keepClasses true})
        article (.parse reader)
        content (.-content article)]
    (fs/writeFileSync output content "utf-8")))

(defn -main []
  (let [input (nth argv 3)
        output (nth argv 4)]
    (if (and input output)
      (make-readable input output)
      (println "Usage: nbb extract.cljs <input-file> <output-file>"))))

(-main)
