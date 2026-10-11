;; Minimal repro (cljw 1.14.11): inside a defrecord method, a parameter that
;; destructures to a name equal to a record FIELD must shadow the field.
;; JVM prints [:param :param :param]; cljw printed [:field :param :param] for the first.
(defprotocol P (f [this m]) (g [this x]) (h [this m]))

(defrecord R [a]
  P
  (f [_ {:keys [a]}] a)          ; destructured param named like the field
  (g [_ a] a)                    ; plain param named like the field
  (h [_ m] (let [{:keys [a]} m] a))) ; destructured in a let

(let [r (->R :field)]
  (prn [(f r {:a :param}) (g r :param) (h r {:a :param})]))

;; Second repro: a fully-qualified clojure.core/defmacro head.
(prn (try (eval '(clojure.core/defmacro qm [x] x)) :defined
          (catch Throwable e (str "FAIL " (ex-message e)))))
