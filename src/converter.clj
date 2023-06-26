(ns converter
  (:require
    [clojure.string :as str]
    [hickory.core :as hc]
    [zprint.core :as zp])
  (:import
    (java.awt.datatransfer StringSelection)                 ;
    (java.awt Toolkit)                                      ;
    (java.awt.datatransfer DataFlavor)))

(defn fragment->hiccup [content]
  (->> content
    (hc/parse-fragment)
    (map hc/as-hiccup)))

(defn is-empty? [elem]
  (and
    (string? elem)
    (= "" elem)))

(defn is-html-comment? [elem]
  (and
    (string? elem)
    (str/starts-with? elem "<!--")
    (str/ends-with? elem "-->")))

(defn is-newline? [elem]
  (and
    (string? elem)
    (re-matches #"^[ \n]*$" elem)))

(defn ->text [text]
  ;;todo deal with html entities?
  (concat (list) (keep identity [(symbol (str "dom/text")) text])))

(defn svg? [text]
  ;; need to add to these
  (contains? #{"path" "svg"} text))

(defn ->props [props]
  (concat (list) (keep identity [(symbol (str "dom/props")) props])))

(defn ->convert [elem]
  (cond
    (is-empty? (first elem)) nil
    (is-html-comment? elem) nil
    (is-newline? elem) nil
    (string? elem) (->text elem)
    (vector? elem) (let [tag               (name (first elem))
                         props             (->> elem (second) (into (sorted-map)) (->props))
                         children          (keep ->convert (drop 2 elem))
                         expanded-children (reduce
                                             (fn [acc c]
                                               (if (vector? c)
                                                 (into [] (concat acc c))
                                                 (conj acc c)))
                                             []
                                             children)]
                     (concat (list) (keep identity [(if (svg? tag)
                                                      (symbol (str "svg/" tag))
                                                      (symbol (str "dom/" tag))) props]) expanded-children))))

(def clipboard (.getSystemClipboard (Toolkit/getDefaultToolkit)))

(defn to-clipboard! [s]
  (let [string-selection (StringSelection. s)]
    (.setContents clipboard string-selection nil)
    :ok!))

(defn from-clipboard []
  (.getData clipboard DataFlavor/stringFlavor))

(defn convert-from-clipboard! [_]
  (zp/set-options! {:map {:comma? false}})

  (->> (from-clipboard)
    (fragment->hiccup)
    (keep ->convert)
    (first)
    (zp/zprint-str)
    (to-clipboard!)))

(def now convert-from-clipboard!)
