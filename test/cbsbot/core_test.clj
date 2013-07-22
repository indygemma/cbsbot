(ns cbsbot.core-test
  (:use clojure.test)
  (:require [cbsbot.core :as object]
            [cbsbot.macros :refer [defgui defobject defbehaviour]]))

(defn wrap-setup [f]
  (object/destroy-objects)
  (f))

(use-fixtures :once wrap-setup)

(defgui simple-gui
  :init (fn []
          "sample gui"))

(defobject sample-object-singleton
  :tags #{}
  :singleton true
  :events #{:sos.hello}
  :behaviours [:set-read-only :cleanup]
  :gui simple-gui
  )

(defbehaviour set-read-only
  :listens #{:init}
  :do (fn [this]
        (object/set-attr this :initialized true)))

(defbehaviour cleanup
  :listens #{:destroy}
  :do (fn [this]
        (object/set-attr this :initialized false)
        (let [other (object/select-object :another-object-singleton)]
          (when other
            (object/set-attr other :on-cleanup true)))
        ))

(defobject another-object-singleton
  :behaviours []
  :singleton true
  :events #{}
  :tags #{}
  :gui simple-gui
  :on-cleanup false
  )

(defbehaviour respond-to-sos-hello
  :listens #{:sos.hello}
  :do (fn [this message]
        (object/set-attr this :hello-called message)))

(deftest test-object
  (testing "has attributes after object creation"
    (let [obj (object/create sample-object-singleton)
          id  (object/get-id obj)]
      (is (= "sample gui" (object/select-gui obj)))
      (is (not (= nil (object/get-id obj))))
      (is (= 1 (count (object/select-objects :sample-object-singleton))))
      (is (= obj (first (object/select-objects :sample-object-singleton))))
      (is (= obj (object/select-object :sample-object-singleton)))
      (is (= id  (object/get-id (object/select-object :sample-object-singleton))))
      (is (= obj (object/lookup-object id)))
      (object/destroy obj)
      (is (= 0 (count (object/select-objects :sample-object-singleton))))))

  (testing "getting and setting attributes to object instances"
    (let [obj (object/create sample-object-singleton)]
      (is (= (object/get-attr obj :value) nil))
      (let [obj' (object/set-attr obj :value true)]
        (is (= true (object/get-attr obj :value))))
      ; get-attr/set-attr can take id or obj instances as first parameter
      (is (= (object/get-attr (object/get-id obj) :value) true))
      (object/set-attr (object/get-id obj) :test 1)
      (is (= (object/get-attr obj :test) 1))
      ))

  (testing "behaviour and event handling/dispatch"
    (let [obj (object/create sample-object-singleton)]
      (is (= true (object/get-attr obj :initialized)) ":init event is triggered on object creation")
      (object/assign-behaviour obj :respond-to-sos-hello)
      (object/emit-event :sos.hello obj "hello world")
      (is (= "hello world" (object/get-attr obj :hello-called)))
      (object/create another-object-singleton)
      (is (= 1 (count (object/select-objects :another-object-singleton))))
      (is (= false (object/get-attr (object/select-object :another-object-singleton) :on-cleanup)))
      (object/destroy obj)
      (is (= true (object/get-attr (object/select-object :another-object-singleton) :on-cleanup)))
    )))

(run-tests)
