(require '[clojure.main])

(clojure.main/repl
 :init #(do
          (require '[corasaurus-hex.fs :as fs])
          (in-ns 'corasaurus-hex.fs)))
