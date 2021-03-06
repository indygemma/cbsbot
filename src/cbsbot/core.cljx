(ns cbsbot.core
  (:require [cbsbot.random :refer [gen-id]]))

(def object-instances (atom {})) ; :object-name -> [obj1 id, obj2 id]
(def object-ids (atom {}))  ; object-id -> obj
(def behaviour-index (atom {})) ; :behaviour-name -> behaviour
(def event-index (atom {})) ; event-name -> [obj1 id, obj2 id ...]

#+clj
(def error (fn [x] (Exception. x)))
#+cljs
(def error (fn [x] (js/Error. x)))

(defn get-id [obj-instance]
  (try
    (get obj-instance :_id)
    #+clj
    (catch Exception e nil)
    #+cljs
    (catch js/Error e nil)
    ))

(defn lookup-object
  "lookup an object by its id"
  [obj-id]
  (@object-ids obj-id))

(defn valid-obj-or-id? [obj-or-id]
  (let [obj-id (get-id obj-or-id)]
    (if (nil? obj-id)
      (let [obj (lookup-object obj-or-id)]
        (if (nil? obj) false obj))
      (lookup-object obj-id))))

(defn- get-obj-or-id [obj-or-id]
  (let [obj (valid-obj-or-id? obj-or-id)]
    (if (= false obj)
      (throw (error (str "Need either object instance or object id as first parameter. Got " obj-or-id " instead.")))
      obj)))

(defn register-behaviour [tpl]
  (let [events-listened (:listens tpl)]
    (swap! behaviour-index (fn [xs] (assoc xs (:name tpl) tpl)))
      ))

(defn get-behaviour [name]
  (@behaviour-index name))

(defn get-behaviours [obj-or-id]
  (let [obj (get-obj-or-id obj-or-id)
        behaviour-names (:behaviours obj)]
    (map #(get-behaviour %) behaviour-names)))

(defn get-obj-event-behaviours [obj event]
  (let [behaviours  (get-behaviours obj)
        ; TODO: this filtering has linear complexity, improve?
        behaviours' (filter #(contains? (:listens %) event) behaviours)]
  behaviours'))

(defn get-registered-events [obj-or-id]
  (let [obj (get-obj-or-id obj-or-id)
        behaviours (get-behaviours obj-or-id)
        events (reduce (fn [s behaviour]
                         (reduce conj s (:listens behaviour)))
                       #{} behaviours)]
    events))

(defn- emit-event-single
  [event obj-or-id args]
  (let [obj (get-obj-or-id obj-or-id)
        behaviours (get-obj-event-behaviours obj event)]
    (dorun
      (map #(apply (:do %) obj args) behaviours))))

(defn- emit-event-all
  [event args]
  (let [obj-ids (@event-index event)]
    (dorun
      (map (fn [obj-id]
             (emit-event-single event obj-id args))
           obj-ids))))

(defn emit-event
  [event & args]
  (cond (false? (valid-obj-or-id? (first args))) (emit-event-all event args)
        :else (emit-event-single event (first args) (rest args))))

(defn assign-behaviour [obj-or-id behaviour-name]
  (let [obj                (get-obj-or-id obj-or-id)
        behaviour-instance (@behaviour-index behaviour-name)
        events             (:listens behaviour-instance)
        ; make sure this is unique, replace old one (TODO improve linear search here)
        behaviours' (if (nil? (some #{behaviour-name} (:behaviours obj)))
                     (conj (:behaviours obj) behaviour-name)
                     (:behaviours obj))
        obj'               (assoc obj :behaviours behaviours')
        ; build mapping: {:event1 [obj-id], :event2 [obj-id], ...}
        event-obj          (reduce (fn [h event]
                                     (assoc h event [(get-id obj)]))
                                     {} events)]
    (swap! object-ids (fn [oi]
                        (assoc oi (get-id obj') obj')))
    ; update event-index so we can execute this behaviour without specifying
    ; the concrete object instance.
    (swap! event-index (fn [ei]
                         (merge-with concat ei event-obj)))
    ))

(defn assign-behaviours [obj-or-id]
  (let [obj (get-obj-or-id obj-or-id)
        behaviour-names (:behaviours obj)]
    (doseq [behaviour-name behaviour-names]
      (assign-behaviour obj behaviour-name))))

(defn create-object [name & keyvals]
  (let [as-map (apply hash-map keyvals)]
    (conj as-map {:name (keyword name)})))

(defn create-gui [gui] ((:init gui)))

(defn create-id-until-unique [ids-var]
  (loop []
    (let [new-id (gen-id)]
      (if (contains? @ids-var new-id)
        (recur)
        new-id))))

(defn create' [objtpl ids-var instance-var]
  (let [objname            (:name objtpl)
        obj-instances      (@instance-var objname)
        is-singleton?      (= true (:singleton objtpl))
        instance-exists?    (not (empty? obj-instances))]
    (if (and instance-exists? is-singleton?)
      (lookup-object (first obj-instances))
      (do
        (let [new-id  (create-id-until-unique ids-var)
              objtpl' (-> objtpl
                          (#(assoc % :_gui-instance (create-gui (:gui %))))
                          (#(assoc % :_id new-id))
                          )]
          (swap! instance-var (fn [xs]
                                    (let [xs' (assoc xs objname (conj obj-instances (get-id objtpl')))]
                                      xs')))
          (swap! ids-var (fn [xs]
                           (assoc xs new-id objtpl')))
          (assign-behaviours objtpl')
          (emit-event :init objtpl')
          ; return the object by id (might have been changed during :init event)
          (lookup-object new-id))))))

(defn create [objtpl] (create' objtpl object-ids object-instances))

(defn destroy' [obj-or-id ids-var instance-var]
  (let [obj (get-obj-or-id obj-or-id)]
    (when obj
      (let [objname (:name obj)
            objid   (get-id obj)
            obj-instances (@instance-var objname)
            registered-events (get-registered-events obj)]
      (emit-event :destroy obj)
      (swap! instance-var (fn [xs]
                            (let [xs' (assoc xs (:name obj) (remove #(= % objid) obj-instances))]
                              xs')))
      (swap! ids-var (fn [xs]
                       (dissoc xs objid)))
      ; cleanup event-index
      (swap! event-index (fn [ei]
                           (reduce (fn [ei' event]
                                     (let [obj-ids (get ei' event)
                                           obj-id-removed (remove #(= % objid) obj-ids)]
                                       (assoc ei' event obj-id-removed)))
                                   ei registered-events)))
      ))))

(defn destroy [obj-or-id] (destroy' obj-or-id object-ids object-instances))

(defn destroy-objects []
  (doseq [obj-id (keys @object-ids)]
    (destroy obj-id)))

(defn list-objects [] (for [[k v] @object-ids] v))

(defn select-objects
  [obj-name]
  (for [obj-id (@object-instances obj-name)]
    (lookup-object obj-id)))

(defn select-object
  "Singular return of object by its name"
  [obj-name]
  (first (select-objects obj-name)))

(defn get-attr
  [obj-or-id attr]
  (let [obj (get-obj-or-id obj-or-id)]
    (when (not (nil? obj))
      (obj attr))))

(defn set-attr
  "Set the object's attr to value that is returned by select-object"
  [obj-or-id attr value]
  (let [obj (get-obj-or-id obj-or-id)
        obj' (assoc obj attr value)]
    (swap! object-ids (fn [xs]
                        (assoc xs (get-id obj) obj')))
    obj'))

(defn set-attrs
  [obj-or-id m]
  (let [obj (get-obj-or-id obj-or-id)
        obj' (merge obj m)]
    (swap! object-ids (fn [x]
                        (assoc x (get-id obj) obj')))
    obj'))

(defn tag-behaviours [tag behaviours])

(defn select-gui [obj-instance]
  (:_gui-instance obj-instance))

(let [o {:fill 2 :what "true"}]
  (->> (map vector (keys o) (vals o))
       ((fn [xs] (map #(apply hash-map %) xs)))))
