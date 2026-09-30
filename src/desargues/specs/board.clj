(ns desargues.specs.board
  "Core layer of boards: the value objects, as specs.

   BoardSpec   what an author writes: a :kind, the function(s) over :var and
               the slider :params, the :window.
   Param       one slider: an id, a range, a step, an initial value.
   Window      the plotted rectangle and the sample count n.
   KernelRef   where the page finds the compiled kernel: a URL and an export.
   Layer       one drawn element, as data: a :layer kind plus what it reads.
   Board       the compiled value plato renders (see desargues.board)."
  (:require [clojure.spec.alpha :as s]))

(s/def ::id (s/or :keyword keyword? :symbol symbol?))
(s/def ::min number?)
(s/def ::max number?)
(s/def ::init number?)
(s/def ::step (s/and number? pos?))
(s/def ::label string?)
(s/def ::param (s/and (s/keys :req-un [::id ::min ::max ::init] :opt-un [::step ::label])
                      #(<= (:min %) (:init %) (:max %))))
(s/def ::params (s/coll-of ::param :kind vector?))

(s/def ::interval (s/and (s/tuple number? number?) (fn [[a b]] (< a b))))
(s/def ::x ::interval)
(s/def ::y ::interval)
(s/def ::n (s/and int? #(> % 1)))
(s/def ::window (s/keys :req-un [::x ::y ::n]))

(s/def ::kind keyword?)
(s/def ::var symbol?)
(s/def ::f any?)
(s/def ::board-spec (s/keys :req-un [::id ::kind ::params] :opt-un [::f ::var ::window ::label]))

;; ---- the compiled Board -------------------------------------------------

(s/def ::wasm string?)
(s/def ::export string?)
(s/def ::kernel-ref (s/keys :req-un [::wasm ::export]))

(s/def ::layer-kind keyword?)
(s/def ::layer (s/and map? #(keyword? (:layer %))))

(s/def :board/id keyword?)
(s/def :board/kind keyword?)
(s/def :board/kernel ::kernel-ref)
(s/def :board/window ::window)
;; The keyword check runs FIRST, on the raw value: s/and hands later
;; predicates the conformed value, where ::id has become [:keyword :a].
(s/def :board/params (s/coll-of (s/and #(keyword? (:id %)) ::param) :kind vector?))
(s/def :board/outputs (s/coll-of keyword? :kind vector? :min-count 1))
(s/def :board/layers (s/coll-of ::layer :kind vector?))
(s/def :board/probes map?)
(s/def :board/frame (s/map-of keyword? (s/coll-of number? :kind vector?)))
(s/def :board/label string?)
;; The board's mathematics as TeX, one display line each, in reading order.
;; Kind-neutral on purpose: a renderer typesets the lines without knowing
;; which kind of board wrote them.
(s/def :board/math (s/coll-of string? :kind vector?))

(s/def ::board
  (s/and (s/keys :req [:board/id :board/kind :board/kernel :board/window
                       :board/params :board/outputs :board/layers :board/probes
                       :board/frame :board/label]
                 :opt [:board/math])
         (fn [{:board/keys [outputs frame window]}]
           (every? #(= (:n window) (count (get frame %))) outputs))))
