
(ns menu
 (:require [clojure.java.io :as io]
  [ compress :refer [ print-menu , display-list-files, display-files-contents ,Compress-a-file,Uncompress-a-file, get-user-input]]))
 
(defn process-choice [choice]
  (cond
    (= choice "1") (display-list-files)
    (= choice "2") (display-files-contents)
    (= choice "3") (Compress-a-file)
    (= choice "4") (Uncompress-a-file)
    (= choice "5") ()
    :else (println "Invalid choice: Please select choice from the menu")))

(defn program_loop[]
  (print-menu)
(let [choice (get-user-input)]
  (process-choice choice)
  (when (not= choice "5") 
    (program_loop)))
  )

(defn -main []
  (program_loop))
(-main)