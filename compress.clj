(ns compress 
  (:require [clojure.string :as str]))
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn read-file-as-string [file-path]
  (with-open [reader (io/reader file-path)]
    (str/join \space (line-seq reader))))

(defn extract-words [text]
  (str/split text #"\s+"))

(defn clean-word [word]
  (-> word
      clojure.java.io/reader
      slurp
      str/lower-case))

(defn file-exists? [file-path]
  (.exists (io/file file-path)))

(defn is-number? [word]
  (try
    (Double/parseDouble word)
    true
    (catch NumberFormatException e
      false)))

(defn remove-space-around-special-chars [string]
   (let [without_space (clojure.string/replace string #"\s*([\W&&[^\s]])" "$1")
         with_space (clojure.string/replace without_space #"(?<!\s)([@$!\[\{\(])" " $1")
         without_extra_spaces (clojure.string/replace with_space #"(?<=[@$\[\{\(])\s" "")
         with_capitalized (clojure.string/replace without_extra_spaces #"(?<=\.\s)\w" #(clojure.string/upper-case %))
         removed_space_hyphen (clojure.string/replace with_capitalized #"- " " - ")
         first_letter_capital(clojure.string/replace removed_space_hyphen #"^\p{L}" #(clojure.string/upper-case %))]
     first_letter_capital))


(defn find-first-occurrences [word-list target-file]
  (let [word-file-exists? (file-exists? word-list)] 
    (if (not word-file-exists?)
      (do
        (println "Oops: specified file does not exist") 
        ) 
      
      (let [target-words (-> target-file
                             clojure.java.io/reader
                             slurp
                             (clojure.string/split #"\s+")
                             (distinct)
                             vec)
            i-words1 (re-seq #"[^\s\w]|\b\w+\b|@\d+@" (slurp word-list))
            output (atom "")] 
        (doseq [word i-words1]
          (let [index (first (keep-indexed #(when (= (str/lower-case word) %2) %1) target-words))]
            (if (not (is-number? word))
              (if index
                (swap! output #(str % index " "))
                (swap! output #(str % word " ")))
              (swap! output #(str % "@" word "@" " ")))))
        (let [newFilename (str word-list ".ct")]
          (spit newFilename @output))))))

(defn decompress-file [compressed-file target-file]
  (let [word-file-exists? (file-exists? compressed-file)] 
    (if (not word-file-exists?)
      (do
        (println "Oops: specified file does not exist") 
        ) 
  (let [compressed-output (slurp compressed-file)
        target-words (-> target-file
                         clojure.java.io/reader
                         slurp
                         (clojure.string/split #"\s+")
                         (distinct)
                         vec)

        decompressed-output (clojure.string/replace compressed-output #"\d+|@[^@]+@" (fn [m]
                                                                                       (if (re-matches #"\d+" m)
                                                                                         (let [index (Integer/parseInt m)]
                                                                                           (if (and (>= index 0) (< index (count target-words)))
                                                                                             (get target-words index)
                                                                                             m))
                                                                                         (subs m 1 (dec (count m))))))]
    (let [result (remove-space-around-special-chars decompressed-output)]
      (println)
      (println result))))))

(defn get-file-names []

  (let [current-directory (System/getProperty "user.dir")]
    (->> (io/file current-directory)   ; Create a File object for the current directory
         (file-seq)                 ; Retrieve a lazy sequence of all files in the directory
         (filter #(.isFile %))         ; Filter out directories
         (map #(.getName %)))))



(defn display-list-files []
  (println)
  (println "File Lists:")
  (println)
  (doseq [file-name (get-file-names)]
    (println "*./" file-name)))

(defn display-contents-files []
  (print "Enter the file name you want to read the content of=>")
  (flush)
  (read-line))

(defn display-file-content [file-path]
  (try
    (let [content (slurp file-path)]
      (println)
      (println content))
    (catch java.io.FileNotFoundException e
      (println "Oops: specified file does not exist"))))

(defn display-files-contents []
  (let [file-name (display-contents-files)]
    (display-file-content file-name)))

(defn ask_for_file []
  (print "Enter the file name you want to decompress the content of=>")
  (flush)
  (read-line))

(defn get-user-input []
  (print "Enter your choice=>")
  (flush)
  (read-line))

(defn Compress-a-file []
  (let [source-words-file (display-contents-files)] 
    (find-first-occurrences source-words-file "frequency.txt")))


(defn Uncompress-a-file []
  (let [file-name (ask_for_file)]
    (decompress-file file-name "frequency.txt")))


 (defn print-menu []
   (println)
   (println "****Compression Menu****")
   (println)
   (println "1. Display list of files")
   (println "2. Display file contents")
   (println "3. Compress a file")
   (println "4. Uncompress a file")
   (println "5. Exit"))









