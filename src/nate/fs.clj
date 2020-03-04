(ns nate.fs
  {:clj-kondo/config '{:lint-as {nate.fs/with-temp-directory clojure.core/fn}}}
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (java.nio.file CopyOption
                          Files
                          FileSystems
                          LinkOption
                          NoSuchFileException
                          Path
                          StandardCopyOption)
           (java.nio.file.attribute FileAttribute
                                    PosixFilePermissions
                                    UserPrincipalNotFoundException)))

;; TODO: glob https://github.com/Raynes/fs/blob/master/src/me/raynes/fs.clj#L390
;;       some kind of posix-ish sub-ns that has more posix-ish commands like rm, mv, cp, chmod, chown, chgrp, etc. maybe based on ruby FileUtils

(defmacro ^:private debug [message & args]
  `(prn ~message (into {} (zipmap ~(vec (map keyword args))
                                  ~(vec args)))))

(def default-file-system (FileSystems/getDefault))
(def file-separator (.getSeparator default-file-system))
(def current-path (.getCanonicalPath (io/file ".")))
(def tmpdir
  ^{:doc "The system temp directory."}
  (-> "java.io.tmpdir"
      System/getProperty
      io/file
      .getCanonicalPath))

(def home
  ^{:doc "The current user's home directory."}
  (str (System/getProperty "user.home")))

(defn ^:private coerce-path-to-string
  [path]
  (if (instance? Path path)
    (str path)
    path))

(defn as-file
  [path & paths]
  (apply io/file (coerce-path-to-string path) (map coerce-path-to-string paths)))

(defn as-path
  [path & paths]
  (.toPath (apply as-file path paths)))

(defn ^:private first-path-segment
  [path]
  (first (map str (as-path path))))

(defn expand-home
  "Takes a path and expands leading reference to `~` to be the current user's home directory."
  [path]
  (if (and (.startsWith path "~")
           (= "~" (first-path-segment path)))
    (str home (subs path 1))
    path))

(def ^:private number-to-permissions
  {7 "rwx"
   6 "rw-"
   5 "r-x"
   4 "r--"
   3 "-wx"
   2 "-w-"
   1 "--x"
   0 "---"})

(def ^:private permission-to-numbers
  {"READ" 4
   "WRITE" 2
   "EXECUTE" 1})

(defn ^:private octal->string-permissions
  [permissions]
  (->> (str permissions)
       (map (comp number-to-permissions
                  #(Character/digit % 10)))
       (apply str)))

(defn ^:private permissions->octal
  [permissions]
  (->> permissions
       (into #{}
             (map (comp #(string/split % #"_")
                        str)))
       (map (fn [[who perm]]
              [who (permission-to-numbers perm)]))
       (reduce (fn [m [who perm]]
                 (update m who #(+ perm %)))
               {"OWNER" 0 "GROUP" 0 "OTHERS" 0})
       (#(+ (* 100 (% "OWNER"))
            (* 10 (% "GROUP"))
            (% "OTHERS")))))

(defn ^:private permissions->string
  [permissions]
  (-> permissions
      permissions->octal
      octal->string-permissions))

(defn ->posix-file-permissions
  [permissions]
  (cond
    (string? permissions)
    (PosixFilePermissions/fromString permissions)

    (integer? permissions)
    (->posix-file-permissions (octal->string-permissions permissions))

    :else
    (throw (ex-info "Invalid permissions!" {:permissions permissions}))))

(defn ->link-options
  "Converts a hash-map of options into an array of LinkOption objects.

  | key              | description |
  | -----------------|-------------|
  | `:nofollow-links`| Adds LinkOption/NOFOLLOW_LINKS to the array. Default: `false`
  "
  ([]
   (make-array LinkOption 0))
  ([{:keys [nofollow-links]}]
   (into-array
    LinkOption
    (if nofollow-links
      [LinkOption/NOFOLLOW_LINKS]
      []))))

(defn ->copy-options
  "Converts a hash-map of options into an array of CopyOption objects.

  | key                | description |
  | -------------------|-------------|
  | `:replace-existing`| Adds StandardCopyOption/REPLACE_EXISTING to the array. Default: `false`
  | `:atomic-move`     | Adds StandardCopyOption/ATOMIC_MOVE to the array. Default: `false`
  | `:copy-attributes` | Adds StandardCopyOption/COPY_ATTRIBUTES to the array. Default: `false`
  | `:nofollow-links`  | Adds LinkOption/NOFOLLOW_LINKS to the array. Default: `false`
  "
  ([]
   (make-array CopyOption 0))
  ([{:keys [replace-existing
            atomic-move
            copy-attributes
            nofollow-links]}]
   (into-array
    CopyOption
    (cond-> []
      replace-existing (conj StandardCopyOption/REPLACE_EXISTING)
      atomic-move (conj StandardCopyOption/ATOMIC_MOVE)
      copy-attributes (conj StandardCopyOption/COPY_ATTRIBUTES)
      nofollow-links (conj LinkOption/NOFOLLOW_LINKS)))))

(defn ->file-attributes
  "Converts a seq of file-attributes into an array of FileAttribute objects."
  ([]
   (make-array FileAttribute 0))
  ([{:keys [posix-file-permissions]}]
   (into-array
    FileAttribute
    (if posix-file-permissions
      [(PosixFilePermissions/asFileAttribute (->posix-file-permissions posix-file-permissions))]
      []))))

(defn join-paths
  "Joins one or more path segments into a single String path object."
  [path & paths]
  (.getPath (apply as-file path paths)))

(defn split-path
  [path]
  (let [paths (map str (as-path path))]
    (or (seq paths)
        '(""))))

(defn last-path-segment
  [path]
  (.getName (as-file path)))

(defn filename
  [path]
  (when-not (.endsWith path file-separator)
    (last-path-segment path)))

(defn parent-path
  [path]
  (.getParent (as-file path)))

(defn parent-paths
  [path]
  (when-let [parent (parent-path path)]
    (cons parent (lazy-seq (parent-paths parent)))))

(defn child-of?
  [parent-path child-path]
  (boolean (some #{parent-path} (parent-paths child-path))))

(defn extension
  [path]
  (when-let [filename (filename path)]
    (let [dot-index (string/last-index-of filename ".")]
      (when (and filename
                 (pos-int? dot-index)
                 (not= dot-index (-> filename count dec)))
        (subs filename (inc dot-index))))))

(defn without-extension
  [path]
  (if-let [filename (filename path)]
    (let [dot-index (string/last-index-of filename ".")]
      (if (and (pos-int? dot-index)
               (not= dot-index (-> filename count dec)))
        (let [chars-to-remove (- (count filename) dot-index)
              full-path-dot-index (- (count path) chars-to-remove)]
          (subs path 0 full-path-dot-index))
        path))
    path))

(defn normalize-path
  [path]
  (-> (as-file path)
      .toPath
      .normalize
      .toString))

(defn absolute-path?
  [path]
  (.isAbsolute (as-file path)))

(defn relative-path?
  [path]
  (not (absolute-path? path)))

(defn absolute-path
  [path]
  (.getAbsolutePath (as-file path)))

(defn canonical-path
  [path]
  (.getCanonicalPath (as-file path)))

(defn children
  ([]
   (children "."))
  ([path]
   (vec (.list (as-file path)))))

(defn size
  [path]
  (try
    (Files/size (as-path path))
    (catch NoSuchFileException _ nil)))

(defn exists?
  ([path]
   (exists? path {}))
  ([path {:keys [nofollow-links]}]
   (Files/exists (as-path path) (->link-options {:nofollow-links nofollow-links}))))

(defn directory?
  ([path]
   (directory? path {}))
  ([path {:keys [nofollow-links]}]
   (Files/isDirectory (as-path path) (->link-options {:nofollow-links nofollow-links}))))

(defn executable?
  [path]
  (Files/isExecutable (as-path path)))

(defn hidden?
  [path]
  (Files/isHidden (as-path path)))

(defn readable?
  [path]
  (Files/isReadable (as-path path)))

(defn writable?
  [path]
  (Files/isWritable (as-path path)))

(defn regular-file?
  ([path]
   (regular-file? path {}))
  ([path {:keys [nofollow-links]}]
   (Files/isRegularFile (as-path path) (->link-options {:nofollow-links nofollow-links}))))

(defn same-file?
  [path1 path2]
  (Files/isSameFile (as-path path1) (as-path path2)))

(defn symlink?
  [path]
  (Files/isSymbolicLink (as-path path)))

(defn copy
  ([from to]
   (copy from to {}))
  ([from to {:keys [replace-existing
                    copy-attributes
                    nofollow-links]}]
   (Files/copy (as-path from) (as-path to) (->copy-options
                                            {:replace-existing replace-existing
                                             :copy-attributes copy-attributes
                                             :nofollow-links nofollow-links}))))

(defn ^:private copy-all
  [copy-options copies]
  (doseq [[from to] copies]
    (Files/copy (as-path from) (as-path to) copy-options)))

(defn ^:private copy-from-to
  [from-root to-root from]
  (let [subpath (subs from (count from-root))]
    [from (str to-root subpath)]))

(defn ^:private recursive-files-and-directories
  ([path]
   (recursive-files-and-directories path {}))
  ([path {:keys [nofollow-links]}]
   (let [paths (->> (as-file path)
                    file-seq
                    (group-by #(directory? % {:nofollow-links nofollow-links})))
         directories (sort (map str (or (paths true) [])))
         files (sort (map str (or (paths false) [])))]
     {:directories directories
      :files files})))


(defn copy-recursively
  ([from to]
   (copy-recursively from to {}))
  ([from to {:keys [replace-existing
                    copy-attributes
                    nofollow-links]}]
   (let [copy-options (->copy-options
                       {:replace-existing replace-existing
                        :copy-attributes copy-attributes
                        :nofollow-links nofollow-links})
         {:keys [files
                 directories]} (recursive-files-and-directories from
                                                                {:nofollow-links nofollow-links})
         to (if (and (directory? to)
                     (not (.endsWith to "/")))
              (join-paths to (last-path-segment from))
              to)]
     (copy-all copy-options
               (map (partial copy-from-to from to)
                    directories))
     (copy-all copy-options
               (map (partial copy-from-to from to)
                    files)))))

(defn move
  ([from to]
   (move from to {}))
  ([from to {:keys [replace-existing
                    atomic-move]}]
   (Files/move (as-path from) (as-path to) (->copy-options
                                            {:replace-existing replace-existing,
                                             :atomic-move atomic-move}))))

(defn rename
  ([current-path new-name]
   (rename current-path new-name {}))
  ([current-path new-name {:keys [replace-existing
                                  atomic-move]}]
   (let [from-path (as-path current-path)
         to-path (.resolveSibling from-path new-name)]
     (move from-path
           to-path
           {:replace-existing replace-existing
            :atomic-move atomic-move}))))

(defn create-directory
  "Warning: Setting posix-file-permissions on create will not always
  result in the permissions you specify. This is a limitation of the
  implementation. To guarantee those permissions you should set the
  permissions as another step after creating the file."
  ([path]
   (create-directory path {}))
  ([path {:keys [posix-file-permissions]}]
   (Files/createDirectory (as-path path)
                          (->file-attributes {:posix-file-permissions posix-file-permissions}))))

(defn create-directories
  "Warning: Setting posix-file-permissions on create will not always
  result in the permissions you specify. This is a limitation of the
  implementation. To guarantee those permissions you should set the
  permissions as another step after creating the file."
  ([path]
   (create-directories path {}))
  ([path {:keys [posix-file-permissions]}]
   (Files/createDirectories (as-path path)
                            (->file-attributes {:posix-file-permissions posix-file-permissions}))))

(defn create-temp-directory
  "Warning: Setting posix-file-permissions on create will not always
  result in the permissions you specify. This is a limitation of the
  implementation. To guarantee those permissions you should set the
  permissions as another step after creating the file."
  ([prefix]
   (create-temp-directory prefix {}))
  ([prefix {:keys [posix-file-permissions]}]
   (Files/createTempDirectory prefix
                              (->file-attributes {:posix-file-permissions posix-file-permissions}))))

(defn append-to-file
  [path content]
  (spit (as-file path) content :append true))

(defn create-file
  "Warning: Setting posix-file-permissions on create will not always
  result in the permissions you specify. This is a limitation of the
  implementation. To guarantee those permissions you should set the
  permissions as another step after creating the file."
  ([path]
   (create-file path {}))
  ([path {:keys [posix-file-permissions content]}]
   (let [file (Files/createFile (as-path path)
                                (->file-attributes {:posix-file-permissions posix-file-permissions}))]
     (when content
       (append-to-file path content))
     file)))

(defn create-temp-file
  "Warning: Setting posix-file-permissions on create will not always
  result in the permissions you specify. This is a limitation of the
  implementation. To guarantee those permissions you should set the
  permissions as another step after creating the file."
  ([prefix suffix]
   (create-temp-file prefix suffix {}))
  ([prefix suffix {:keys [posix-file-permissions content]}]
   (let [path (Files/createTempFile prefix
                                    suffix
                                    (->file-attributes {:posix-file-permissions posix-file-permissions}))]
     (when content
       (append-to-file path content))
     path)))

(defn create-link
  [link-path target-path]
  (Files/createLink (as-path link-path) (as-path target-path)))

(defn create-symlink
  "Not allowing posix-file-permissions for file attributes since it is not supported on MacOS
  or Linux for this operation."
  [link-path target-path]
  (Files/createSymbolicLink (as-path link-path) (as-path target-path) (->file-attributes)))

(defn delete
  [path]
  (Files/delete (as-path path)))

(defn delete-if-exists
  [path]
  (Files/deleteIfExists (as-path path)))

(defn delete-recursively
  ([path]
   (delete-recursively path {}))
  ([path {:keys [nofollow-links]
          :or {nofollow-links true}}]
   (let [{:keys [files directories]} (recursive-files-and-directories path
                                                                      {:nofollow-links nofollow-links})]
     (doseq [path (concat files (reverse directories))]
       (.delete (as-file path))))))

(defn file-system-for
  [path]
  (.getFileSystem (as-path path)))

(defn supported-file-attribute-views
  [path]
  (.supportedFileAttributeViews (file-system-for path)))

(defn get-attribute
  ([path attribute]
   (get-attribute path attribute {}))
  ([path attribute {:keys [nofollow-links]}]
   (Files/getAttribute (as-path path)
                       attribute
                       (->link-options {:nofollow-links nofollow-links}))))

(defn set-attribute
  ([path attribute value]
   (set-attribute path attribute value {}))
  ([path attribute value {:keys [nofollow-links]}]
   (Files/setAttribute (as-path path)
                       attribute
                       value
                       (->link-options {:nofollow-links nofollow-links}))))

(defn read-attributes
  ([path attributes]
   (read-attributes path attributes {}))
  ([path attributes {:keys [nofollow-links]}]
   (Files/readAttributes (as-path path)
                         attributes
                         (->link-options {:nofollow-links nofollow-links}))))

(defn read-all-attributes
  ([path]
   (read-all-attributes path {}))
  ([path {:keys [nofollow-links]}]
   (let [path (as-path path)
         file-system (.getFileSystem path)
         views (.supportedFileAttributeViews file-system)
         link-options (->link-options {:nofollow-links nofollow-links})]
     (into {}
           (map #(do [% (Files/readAttributes path (str % ":*") link-options)]))
           views))))

(defn lookup-user!
  ([user-name]
   (lookup-user! user-name default-file-system))
  ([user-name file-system]
   (-> file-system
       .getUserPrincipalLookupService
       (.lookupPrincipalByName user-name))))

(defn lookup-user
  ([user-name]
   (lookup-user user-name default-file-system))
  ([user-name file-system]
   (try
     (lookup-user! user-name file-system)
     (catch UserPrincipalNotFoundException _ nil))))

(defn lookup-group!
  ([group-name]
   (lookup-group! group-name default-file-system))
  ([group-name file-system]
   (-> file-system
       .getUserPrincipalLookupService
       (.lookupPrincipalByGroupName group-name))))

(defn lookup-group
  ([group-name]
   (lookup-group group-name default-file-system))
  ([group-name file-system]
   (try
     (lookup-group! group-name file-system)
     (catch UserPrincipalNotFoundException _ nil))))

(defn set-owner
  ([path user-name]
   (set-owner path user-name {}))
  ([path user-name {:keys [nofollow-links]}]
   (let [file-system (file-system-for path)
         user (lookup-user! user-name file-system)]
     (set-attribute path
                    "owner:owner"
                    user
                    {:nofollow-links nofollow-links}))))

(defn set-group
  ([path group-name]
   (set-group path group-name {}))
  ([path group-name {:keys [nofollow-links]}]
   (let [path (as-path path)
         file-system (.getFileSystem path)
         group (lookup-group! group-name file-system)
         link-options (->link-options {:nofollow-links nofollow-links})]
     (Files/setAttribute path "posix:group" group link-options))))

(defn set-posix-file-permissions
  ([path permissions]
   (set-posix-file-permissions path permissions {}))
  ([path permissions {:keys [nofollow-links]}]
   (set-attribute path
                  "posix:permissions"
                  (->posix-file-permissions permissions)
                  {:nofollow-links nofollow-links})))

(defn get-posix-file-permissions
  ([path]
   (get-posix-file-permissions path {}))
  ([path {:keys [nofollow-links format]}]
   (let [permissions (get-attribute path "posix:permissions" {:nofollow-links nofollow-links})]
     (case format
       :octal (permissions->octal permissions)
       :string (permissions->string permissions)
       permissions))))

(defn last-modified-time
  ([path]
   (last-modified-time path {}))
  ([path {:keys [nofollow-links]}]
   (get-attribute path "basic:lastModifiedTime" {:nofollow-links nofollow-links})))

(defn set-last-modified-time
  ([path time]
   (set-last-modified-time path time {}))
  ([path time {:keys [nofollow-links]}]
   (set-attribute path "basic:lastModifiedTime" time {:nofollow-links nofollow-links})))

(defn last-access-time
  ([path]
   (last-access-time path {}))
  ([path {:keys [nofollow-links]}]
   (get-attribute path "basic:lastAccessTime" {:nofollow-links nofollow-links})))

(defn set-last-access-time
  ([path time]
   (set-last-access-time path time {}))
  ([path time {:keys [nofollow-links]}]
   (set-attribute path "basic:lastAccessTime" time {:nofollow-links nofollow-links})))

(defn creation-time
  ([path]
   (creation-time path {}))
  ([path {:keys [nofollow-links]}]
   (get-attribute path "basic:creationTime" {:nofollow-links nofollow-links})))

(defn set-creation-time
  ([path time]
   (set-creation-time path time {}))
  ([path time {:keys [nofollow-links]}]
   (set-attribute path "basic:creationTime" time {:nofollow-links nofollow-links})))

(defn with-temp-directory*
  [f]
  (let [dir (canonical-path (Files/createTempDirectory "fs" (->file-attributes)))]
    (try
      (f dir)
      (finally
        (delete-recursively dir)))))

(defmacro with-temp-directory [[path-sym] & body]
  `(with-temp-directory* (fn [~path-sym] ~@body)))

(defn with-temp-file*
  [f]
  (with-temp-directory [dir-path]
    (let [file-path (canonical-path (Files/createTempFile (as-path dir-path) "tmp" "tmp" (->file-attributes)))]
      (f dir-path file-path))))

(defmacro with-temp-file [[dir-sym path-sym] & body]
  `(with-temp-file* (fn [~dir-sym ~path-sym] ~@body)))
