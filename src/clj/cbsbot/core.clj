(ns cbsbot.core
  (:require [cbsbot.random :refer [gen-id]]))

(def object-instances (atom {})) ; :object-name -> [obj1 id, obj2 id]
(def object-ids (atom {}))  ; object-id -> obj
(def behaviour-index (atom {})) ; :behaviour-name -> behaviour
(def event-index (atom {})) ; event-name -> [obj1 id, obj2 id ...]

(defn get-id [obj-instance]
  (:_id obj-instance))

(defn lookup-object
  "lookup an object by its id"
  [obj-id]
  (@object-ids obj-id))

(defn- get-obj-or-id [obj-or-id]
  (let [obj-id (get-id obj-or-id)]
    (if (nil? (get-id obj-or-id))
      (let [obj (lookup-object obj-or-id)]
        (if (nil? obj)
          (throw (str "Need either object instance or object id as first parameter. Got " obj-or-id " instead."))
          obj))
      (lookup-object obj-id))))

(defn get-obj-event-behaviours [obj event]
  (get-in obj [:_behaviour_handlers event]))

(defn emit-event
  ; TODO: what about args?
  ([event]
     (let [obj-ids (@event-index event)]
       (dorun
         (map (fn [obj-id]
                  (emit-event event obj-id))
              obj-ids))))
  ([event obj-or-id & args]
    (let [obj (get-obj-or-id obj-or-id)
          behaviours (get-obj-event-behaviours obj event)]
      (dorun
        (map #(apply % obj args) behaviours)))))

(defn assign-behaviour [obj-or-id behaviour-name]
  (let [obj                (get-obj-or-id obj-or-id)
        behaviour-handlers (or (:_behaviour_handlers obj) {})
        behaviour-instance (@behaviour-index behaviour-name)
        events             (:listens behaviour-instance)
        ; build mapping: {:event1 [obj-id], :event2 [obj-id], ...}
        event-obj          (reduce (fn [h event]
                                     (assoc h event [(get-id obj)]))
                                     {} events)
        handler            (:do behaviour-instance)
        behaviour-handlers' (reduce (fn [hm event]
                                      (let [v (or [] (hm event))
                                            v' (conj v handler)]
                                        (assoc hm event v')))
                                    behaviour-handlers events)
        obj' (assoc obj :_behaviour_handlers behaviour-handlers')]
    (swap! object-ids (fn [xs]
                        (assoc xs (get-id obj) obj')))
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

(defn menu-item [& keyvals] nil)

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

(defn get-registered-events [obj-or-id]
  (let [obj (get-obj-or-id obj-or-id)
        behaviours (get-behaviours obj-or-id)
        events (reduce (fn [s behaviour]
                         (reduce conj s (:listens behaviour)))
                       #{} behaviours)]
    events))

(defn list-objects [] (apply concat (for [[k v] @object-instances] (vec v))))

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

(defn tag-behaviours [tag behaviours])

(defn select-gui [obj-instance]
  (:_gui-instance obj-instance))

