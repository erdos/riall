(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'dev.erdos/riall)
(def version (format "1.2.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))
(def ns-main 'riall.core)

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis basis
                  :ns-compile [ns-main]
                  :class-dir class-dir
                  :compile-opts {:elide-meta [:doc :file :line] :direct-linking true}
                  })
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main ns-main}))

(defn native [_]
  (uber nil)
  ;;; TODO: call `native-image`

)