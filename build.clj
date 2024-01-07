(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'dev.erdos/riall)
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))

;; compare version value to previous commit, set RELEASE_VERSION only when changed.
(def version
  (let [stable   (-> basis :version :stable)
        stable-1 (-> (b/git-process {:git-args ["show" "HEAD~:deps.edn"]}) read-string :version :stable)]
    (if (= stable stable-1)
      (-> basis :version :snapshot)
      (let [envfile (System/getenv "GITHUB_ENV")]
        (println :version-bump stable-1 '-> stable)
        (spit envfile (format "RELEASE_VERSION=%s\n" stable) :append true)
        stable))))

(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))
(def ns-main 'riall.core)

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src"] :target-dir class-dir})
  (spit "target/classes/riall/core.cljc"
        (format "(def version \"%s\")" version)
        :append true)
  (b/compile-clj {:basis basis
                  :ns-compile [ns-main]
                  :class-dir class-dir
                  :compile-opts {:elide-meta [:doc :file :line] :direct-linking true}})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main ns-main
           ; :exclude [".*\\.cljc$" ".*\\.clj$" ".*pprint.*"]
           }))

(def native-image-cmd "native-image")

(defn native [_]
  (uber nil)
  (b/process {:out :inherit
              :err :inherit
              :command-args [native-image-cmd
                             "--no-fallback" "--report-unsupported-elements-at-runtime" "--initialize-at-build-time"
                             "-jar" uber-file
                             "-o" "sankey"]}))
