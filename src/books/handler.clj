(ns books.handler
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [hiccup.page :refer [html5 include-css]]
            [hiccup.core :refer [html]]
            [clj-time.format :refer [formatter parse unparse]]
            [clj-time.core :refer [month date-time]]
            [clojure.pprint :refer [cl-format]]
            [me.raynes.laser :as laser]
            [clj-http.client :as client])
  (:gen-class))

(def file-format (formatter "dd/MM/yyyy"))

(def screen-format (formatter "dd MMM"))

(def month-name (formatter "MMM"))

(defn load-book [lines]
  {:title (first lines)
   :author (second lines)
   :date (parse file-format (nth lines 2))
   :review (apply str (interpose " " (drop 3 lines)))
   }
  )

(defn blank-string? [s]
  (or (empty? s) (nil? (re-find #"\S+" s))))

(defn index-maps [ms]
  (map (fn [[k v]] (assoc v :index k)) (zipmap (range 1 (inc (count ms))) ms)))

(defn index-maps [ms]
    (map #(assoc %1 :index %2) ms (range 1 (inc (count ms)))))

(defn load-books [file]
  (index-maps
    (sort-by :date
      (map load-book
       (filter
        #(not (blank-string? (first %)))
        (partition-by blank-string? (line-seq (clojure.java.io/reader file))))))))

(defn two-char-hex [i]
  (cl-format nil "~16,2,'0r" i))

(defn random-colour []
  (str "#" (apply str (map two-char-hex [(rand-int 200) (rand-int 200) (rand-int 200)]))))

(defn month-groups [books]
  (group-by #(month (:date %)) books))

(defn files []
   (sort-by :year (map
    (fn [f] {:year (subs (.getName f) 0 4) :file f})
    (rest (file-seq (clojure.java.io/file "resources/public/books"))))))

(defn book-page [file-map file-maps]
    (let [books (load-books (:file file-map))]
  (html
    [:style (slurp "resources/public/css/style.css")]
    [:ul.year-list
      (for [f file-maps]
       [:li [:a {:href (str (:year f) ".html")} (:year f)]])
     ]
    [:h1 (str "Books " (:year file-map))]
    [:div.squares
     (for [[m month-books] (month-groups books)]
       [:div.bookmonth
         [:a.booksquare.month {:href "#"}
          (first (unparse month-name (date-time 2013 m)))]
       (for [book month-books]
         (let [c (random-colour)]
       [:a.booksquare {:style (str "background-color:" c "; border-color: " c ";")
                       :href (str "#book-" (:index book))
                       :title (:title book)}
        (:index book)
        ]
         ))
        ]
       )
     ]
    [:div.books
    (for [book books]
      [:div.book {:id (str "book-" (:index book))}
        [:h2 (str (:title book) " - " (:author book))]
        [:h3 (unparse screen-format (:date book))]
        [:p (:review book)]
      ]
      )
     ]
   )
  ))

(defn wrap-page [template-loc file-map file-maps]
(laser/document
 (laser/parse (:body (client/get template-loc)))
 (laser/element= :title)
 (laser/content (str "Books ") (:year file-map))
 (laser/class= "content")
 (laser/content (laser/unescaped (book-page file-map file-maps)))
 (laser/class= "books-link")
 (laser/classes "selected")
 ))

(defroutes app-routes
  (GET "/" [] (wrap-page
               "http://design.johncowie.co.uk"
               "Books 2013"
               (book-page "resources/public/markdown/2013.book")))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))

(defn -main [& args]
  (let [file-maps (files)]
    (doseq [f file-maps]
      (spit (str (first args) "/" (:year f) ".html") (wrap-page (second args) f file-maps)))
    (spit (str (first args) "/index.html") (wrap-page (second args) (last file-maps) file-maps))))
