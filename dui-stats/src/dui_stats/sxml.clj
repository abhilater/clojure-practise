(ns dui-stats.sxml)


(defn format-attrs-map [attrs-map]
  (->> (seq attrs-map)
       (map (fn [[key val]] (str (name key) ":" "'" val "'")))
       (clojure.string/join " ")))


(defn generate-html [tag-name attrs body]
  (clojure.string/join ["<" tag-name (if (empty? attrs) ">" (str \space (format-attrs-map attrs) ">")) body "</" tag-name ">"]   ))


(defn tag-expand-rec [[name attrs & body]]
  (if (seq? (first body))
    (generate-html name attrs (apply str (mapcat tag-expand-rec body)))
    [(generate-html name attrs (first body))]))


(defmacro html [[doctype] & body]
  (tag-expand-rec `("html" {:doctype ~doctype} ~@body)))


(comment
  (html ["DOCTYPE"]
        (head {}
              (style {:type "text/css" :href "/css/test-1.css"})
              (style {:type "text/css" :href "/css/test-2.css"})
              (style {} ".body {font-size: 10}")
              (script {} "alert(\"hello\");")
              (style {:type "1"} (style {} "hello")))
        (body {}
              (h1 {} "This is a heading")
              (p {:stlye "text-align:center"} "This is a paragraph")
              (div {:stlye "text-align:center"} (p {} "This is an inner paragraph"))
              (div {} (p {:stlye "text-color:red"} "This is a red inner paragraph")))))


