;*CLJSBUILD-MACRO-FILE*;

(ns cbsbot.macros
  (:require [cbsbot.core :refer [create-object register-behaviour]]))

(defmacro defobject
  "Add an object template to the repository"
  [name & keyvals]
  (let [tpl (apply create-object name keyvals)]
    `(def ~name ~tpl)))

; TODO identical to defobject
(defmacro defgui [name & keyvals]
  (let [tpl (apply create-object name keyvals)]
    `(def ~name ~tpl)))

(defmacro defbehaviour [name & keyvals]
  (let [tpl (apply create-object name keyvals)]
    `(do
       (def ~name ~tpl)
       (register-behaviour ~tpl))))

