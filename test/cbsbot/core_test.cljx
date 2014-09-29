(ns cbsbot.core-test
  (:use clojure.test)
  (:require [cbsbot.core :as object])
  (#+clj :require #+cljs :require-macros
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
  :behaviours [:set-read-only :cleanup :on-multi1 :on-multi2]
  :gui simple-gui
  :multi1-val false
  :multi2-val false
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

(defbehaviour on-multi1
  :listens #{:multi1}
  :do (fn [this]
        (object/set-attr this :multi1-val true)))

(defobject another-object-singleton
  :behaviours [:on-multi2]
  :singleton true
  :events #{}
  :tags #{}
  :gui simple-gui
  :on-cleanup false
  :multi2-val false
  )

(defbehaviour respond-to-sos-hello
  :listens #{:sos.hello}
  :do (fn [this message]
        (object/set-attr this :hello-called message)))

(defbehaviour on-multi2
  :listens #{:multi2}
  :do (fn [this & optional-val]
        (if (empty? optional-val)
          (object/set-attr this :multi2-val true)
          (object/set-attr this :multi2-val (first optional-val)))))

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
    ))

  (testing "multiple object instance event dispatch"
    (let [obj1 (object/create sample-object-singleton)
          obj2 (object/create another-object-singleton)]
      ; initial values are false
      (is (= false (object/get-attr obj1 :multi1-val)))
      (is (= false (object/get-attr obj1 :multi2-val)))
      (is (= false (object/get-attr obj2 :multi2-val)))
      (object/emit-event :multi1)
      ; only obj1 is affected
      (is (= true  (object/get-attr obj1 :multi1-val)))
      (is (= false (object/get-attr obj1 :multi2-val)))
      (is (= false (object/get-attr obj2 :multi2-val)))
      (object/emit-event :multi2)
      ; now both obj1 + obj2 are affected
      (is (= true (object/get-attr obj1 :multi1-val)))
      (is (= true (object/get-attr obj1 :multi2-val)))
      (is (= true (object/get-attr obj2 :multi2-val)))
      ; now with arguments
      (object/emit-event :multi2 :override-value)
      (is (= :override-value (object/get-attr obj1 :multi2-val)))
      (is (= :override-value (object/get-attr obj2 :multi2-val)))
      ))

  (testing "helper functions"
    (let [obj1 (object/create sample-object-singleton)
          obj2 (object/create another-object-singleton)
          behaviours1 (map #(:name %) (object/get-behaviours obj1))
          behaviours2 (map #(:name %) (object/get-behaviours obj2))
          events1 (object/get-registered-events obj1)
          events2 (object/get-registered-events obj2)
          ]
      (is (= (:behaviours obj1) behaviours1))
      (is (= (:behaviours obj2) behaviours2))
      (is (= #{:init :destroy :multi1 :multi2} events1))
      (is (= #{:multi2} events2))
      ))

  (testing "get-id only works on object instances"
    (let [obj (object/create sample-object-singleton)]
      (is (not= nil (object/get-id obj)))
      (is (= nil (object/get-id "some-string")))))

  (testing "bulk attribute setting"
    (let [obj (object/create sample-object-singleton)
          obj' (object/set-attrs obj {:multi1-val false :multi2-val false})]
      ; the values are initialized as false
      (is (= false (object/get-attr obj :multi1-val)))
      (is (= false (object/get-attr obj :multi2-val)))
      ; we are setting them to false via set-attrs
      (is (= false (:multi1-val obj')))
      (is (= false (:multi2-val obj')))
    ))

  )

(run-tests)
