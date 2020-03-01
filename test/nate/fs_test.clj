(ns nate.fs-test
  {:clj-kondo/config '{:linters {:unresolved-symbol {:exclude []}}
                       :lint-as {nate.fs/with-temp-directory cljs.test/async
                                 nate.fs/with-temp-file cljs.test/async}}}
  (:require [clojure.test :as test :refer [deftest is testing]]
            [nate.fs :as fs]
            [clojure.java.io :as io])
  (:import (java.nio.file FileAlreadyExistsException
                          NoSuchFileException
                          LinkOption
                          StandardCopyOption
                          Path)
           (java.nio.file.attribute PosixFilePermission)))

(deftest test-as-path
  (testing "creates a path object"
    (is (instance? Path (fs/as-path "foo")))
    (is (= "/foo/bar" (.toString (fs/as-path "/foo/bar"))))))

(deftest test-expand-home
  (testing "changes ~ into the user home path"
    (is (= fs/home (fs/expand-home "~")))
    (testing "changes a leading ~/ into the user home path"
      (is (= (str fs/home "/foo") (fs/expand-home "~/foo"))))
    (testing "does not change other paths containing ~"
      (is (= "/foo/~/bar" (fs/expand-home "/foo/~/bar")))
      (is (= "~foo/bar" (fs/expand-home "~foo/bar"))))))

(deftest test-->posix-file-permissions
  (testing "creates from a string"
    (is (= [PosixFilePermission/OWNER_READ,
            PosixFilePermission/OWNER_WRITE,
            PosixFilePermission/OWNER_EXECUTE,
            PosixFilePermission/GROUP_READ,
            PosixFilePermission/GROUP_EXECUTE,
            PosixFilePermission/OTHERS_READ]
           (vec (fs/->posix-file-permissions "rwxr-xr--"))))
    (is (thrown? java.lang.IllegalArgumentException
                 (fs/->posix-file-permissions "asdf"))))
  (testing "creates from an integer"
    (is (= [PosixFilePermission/OWNER_READ,
            PosixFilePermission/OWNER_WRITE,
            PosixFilePermission/OWNER_EXECUTE,
            PosixFilePermission/GROUP_READ,
            PosixFilePermission/GROUP_EXECUTE,
            PosixFilePermission/OTHERS_READ]
           (vec (fs/->posix-file-permissions 754))))
    (is (thrown? java.lang.IllegalArgumentException (fs/->posix-file-permissions 999))))
  (testing "errors on odd types"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Invalid permissions!"
                          (fs/->posix-file-permissions {})))))

(deftest test-->link-options
  (testing "creates an empty array"
    (is (empty? (fs/->link-options))))
  (testing "creates an array with nofollow-links set"
    (is (= [LinkOption/NOFOLLOW_LINKS] (vec (fs/->link-options {:nofollow-links true}))))))

(deftest test-->copy-options
  (testing "creates an empty array"
    (is (empty? (fs/->copy-options))))
  (let [options {:replace-existing StandardCopyOption/REPLACE_EXISTING
                 :atomic-move StandardCopyOption/ATOMIC_MOVE
                 :copy-attributes StandardCopyOption/COPY_ATTRIBUTES
                 :nofollow-links LinkOption/NOFOLLOW_LINKS}]
    (doseq [[option value] options]
      (let [result (fs/->copy-options {option true})]
        (testing (str "creates an array with " option " set")
          (is (= [value] (vec result))))))
    (let [args (flatten (vec (zipmap (keys options) (repeat true))))
          result (fs/->copy-options args)]
      (testing "creates an array with all options set"
        (is (= (frequencies result) (frequencies (vals options))))))))

(deftest test-->file-attributes
  (testing "creates an empty array"
    (is (empty? (fs/->file-attributes))))
  (testing "creates an array with posix file permissions set"
    (is (= (frequencies [PosixFilePermission/OWNER_READ,
                         PosixFilePermission/OWNER_WRITE,
                         PosixFilePermission/OWNER_EXECUTE,
                         PosixFilePermission/GROUP_READ,
                         PosixFilePermission/GROUP_EXECUTE,
                         PosixFilePermission/OTHERS_READ])
           (-> (fs/->file-attributes {:posix-file-permissions 754})
               first
               .value
               vec
               frequencies)))))

(deftest test-join-paths
  (testing "joins a single path segment"
    (is (= "foo" (fs/join-paths "foo"))))
  (testing "joins multiple path segments"
    (is (= "foo/bar/baz" (fs/join-paths "foo" "bar" "baz"))))
  (testing "preserves absoluteness of path"
    (is (= "/foo" (fs/join-paths "/foo")))
    (is (= "/foo/bar/baz" (fs/join-paths "/foo" "bar" "baz"))))
  (testing "does not preserve trailing file separators"
    (is (= "foo" (fs/join-paths "foo/")))
    (is (= "foo/bar/baz" (fs/join-paths "foo" "bar" "baz/"))))
  (testing "does not double up on file separators"
    (is (= "foo/bar/baz" (fs/join-paths "foo/" "bar//" "baz")))))

(deftest test-split-path
  (testing "splits a single path segment"
    (is (= '("foo") (fs/split-path "foo"))))
  (testing "splits into multiple path segments"
    (is (= '("foo" "bar" "baz") (fs/split-path "foo/bar/baz"))))
  (testing "does not split on leading or trailing path segments"
    (is (= '("foo" "bar" "baz") (fs/split-path "/foo/bar/baz/"))))
  (testing "does not split on duplicated file separators"
    (is (= '("foo" "bar" "baz") (fs/split-path "foo//bar///baz")))))

(deftest test-last-path-segment
  (testing "returns the last path segment"
    (is (= "foo.ext" (fs/last-path-segment "foo.ext")))
    (is (= "foo.ext" (fs/last-path-segment "bar/foo.ext")))
    (is (= "foo.ext" (fs/last-path-segment "/bar/foo.ext")))
    (is (= "foo.ext" (fs/last-path-segment "baz/bar/foo.ext")))
    (is (= "foo.ext" (fs/last-path-segment "/baz/bar/foo.ext")))
    (is (= "foo.ext" (fs/last-path-segment "/baz/bar/foo.ext/")))))

(deftest test-filename
  (testing "returns the last path segment with no trailing file separator"
    (is (= "foo.ext" (fs/filename "foo.ext")))
    (is (= "foo.ext" (fs/filename "bar/foo.ext")))
    (is (= "foo.ext" (fs/filename "/bar/foo.ext")))
    (is (= "foo.ext" (fs/filename "baz/bar/foo.ext")))
    (is (= "foo.ext" (fs/filename "/baz/bar/foo.ext")))
    (is (nil? (fs/filename "/baz/bar/foo.ext/")))
    (is (nil? (fs/filename "/")))))

(deftest test-parent-path
  (testing "returns nil if there is no parent path"
    (is (nil? (fs/parent-path "foo")))
    (is (nil? (fs/parent-path "/"))))
  (testing "returns the parent of a relative path"
    (is (= "foo" (fs/parent-path "foo/bar")))
    (is (= "foo" (fs/parent-path "foo/bar/"))))
  (testing "returns the parent of an absolute path"
    (is (= "/foo" (fs/parent-path "/foo/bar")))
    (is (= "/foo" (fs/parent-path "/foo/bar/"))))
  (testing "returns the root path"
    (is (= "/" (fs/parent-path "/foo")))))

(deftest test-parent-paths
  (testing "returns a sequence of all parent paths"
    (is (= ["/foo/bar" "/foo" "/"] (vec (fs/parent-paths "/foo/bar/baz"))))
    (is (= ["foo/bar" "foo"] (vec (fs/parent-paths "foo/bar/baz"))))))

(deftest test-child-of?
  (testing "returns true for paths where the child path is beneath the parent path"
    (is (fs/child-of? "/foo/bar" "/foo/bar/baz"))
    (is (fs/child-of? "foo/bar" "foo/bar/baz")))
  (testing "returns false for paths where the child path is not beneat the parent path"
    (is (not (fs/child-of? "/foo/bar/baz" "/foo/bar")))
    (is (not (fs/child-of? "/foo/bar" "foo/bar/baz")))
    (is (not (fs/child-of? "foo/bar" "/foo/bar/baz")))
    (is (not (fs/child-of? "/foo/bar/baz" "/foo/bar/qux")))))

(deftest test-extension
  (testing "returns a file extension when present"
    (is (= "ext" (fs/extension "/foo/bar/baz.ext")))
    (is (= "e" (fs/extension "foo/bar/baz.e"))))
  (testing "returns nil when no file extension"
    (is (nil? (fs/extension "/foo/bar/baz")))
    (is (nil? (fs/extension "/foo/bar/baz.")))
    (is (nil? (fs/extension "/foo/b.ar/baz")))
    (is (nil? (fs/extension "/foo/bar/.baz")))
    (is (nil? (fs/extension "/foo/bar/b.az/")))))

(deftest test-without-extension
  (testing "returns a path without the file extension"
    (is (= "/foo/bar/baz" (fs/without-extension "/foo/bar/baz.ext")))
    (is (= "foo/bar/baz" (fs/without-extension "foo/bar/baz.e"))))
  (testing "returns paths that have no extension to begin with"
    (is (= "/foo/bar/baz" (fs/without-extension "/foo/bar/baz")))
    (is (= "/foo/bar/baz." (fs/without-extension "/foo/bar/baz.")))
    (is (= "/foo/b.ar/baz" (fs/without-extension "/foo/b.ar/baz")))
    (is (= "/foo/bar/.baz" (fs/without-extension "/foo/bar/.baz")))
    (is (= "/foo/bar/b.az/" (fs/without-extension "/foo/bar/b.az/")))))

(deftest test-normalize-path
  (testing "normalizes the path"
    (is (= "foo/baz" (fs/normalize-path "foo/bar/.././baz")))
    (is (= "/foo/baz" (fs/normalize-path "/foo/bar/.././baz")))))

(deftest test-absolute-path?
  (testing "returns true for absolute paths"
    (doseq [path ["/" "/foo" "/foo/bar"]]
      (is (true? (fs/absolute-path? path)))))
  (testing "returns false for relative paths"
    (doseq [path ["" "foo" "foo/bar"]]
      (is (false? (fs/absolute-path? path))))))

(deftest test-relative-path?
  (testing "returns false for relative paths"
    (doseq [path ["/" "/foo" "/foo/bar"]]
      (is (false? (fs/relative-path? path)))))
  (testing "returns true for relative paths"
    (doseq [path ["" "foo" "foo/bar"]]
      (is (true? (fs/relative-path? path))))))

(deftest test-with-temp-directory
  (testing "creates a temp directory and passes it into the function and deletes afterwards"
    (let [dir (volatile! nil)]
      (fs/with-temp-directory path
        (vreset! dir path)
        (is (let [file (fs/as-file path)]
              (and (.exists file)
                   (.isDirectory file)))))
      (is (.startsWith @dir fs/tmpdir))
      (is (not (.exists (fs/as-file @dir)))))))
(testing "recursively deletes everything within that directory"
  (let [dir (volatile! nil)]
    (fs/with-temp-directory path
      (vreset! dir path)
      (let [file (fs/as-file path "foo" "bar" "baz")]
        (io/make-parents file)
        (fs/create-file file)
        (is (.exists file))
        (is (.startsWith (.getPath file) fs/tmpdir))))
    (is (.startsWith @dir fs/tmpdir))
    (is (not (.exists (fs/as-file @dir))))))

(deftest test-with-temp-file
  (testing "calls the function with a path to an existing file"
    (let [path (volatile! nil)]
      (fs/with-temp-file p
        (vreset! path p)
        (is (let [file (fs/as-file p)]
              (and (.exists file)
                   (.isFile file)))))
      (is (.startsWith @path fs/tmpdir))
      (let [file (fs/as-file @path)]
        (is (not (.exists file)))
        (is (not (.exists (.getParentFile file))))))))

(deftest test-absolute-path
  (testing "returns the absolute path"
    (is (= (str fs/current-path "/foo")
           (fs/absolute-path "foo")))
    (is (= "/foo/bar"
           (fs/absolute-path "/foo/bar")))))

(deftest test-canonical-path
  (testing "returns the canonical path"
    (is (= "/bar/baz" (fs/canonical-path "/foo/../bar/./baz")))
    (fs/with-temp-directory path
      (fs/create-file (fs/join-paths path "foo"))
      (fs/create-symlink (fs/join-paths path "bar") (fs/join-paths path "foo"))
      (is (= (fs/join-paths path "foo")
             (fs/canonical-path (fs/join-paths path "bar")))))))

(deftest test-children
  (testing "returns the child paths of the current directory"
    (is (= (vec (.list (fs/as-file ".")))
           (fs/children))))
  (testing "returns the child paths of a specific directory"
    (fs/with-temp-directory path
      (fs/create-file (fs/join-paths path "bar"))
      (fs/create-file (fs/join-paths path "baz"))
      (fs/create-file (fs/join-paths path "foo"))
      (is (= ["bar" "baz" "foo"]
             (sort (fs/children path)))))))

(deftest test-size
  (testing "returns nil when a file does not exist"
    (is (nil? (fs/size "asdfasdfasdfasdfasdf"))))
  (testing "returns the file size in bytes of a file that exists"
    (fs/with-temp-file path
      (fs/append-to-file path "abcd1234")
      (is (= 8 (fs/size path))))))

(deftest test-exists?
  (testing "returns true when something exists at that path"
    (fs/with-temp-directory path
      (is (fs/exists? path))))
  (testing "returns false when something does not exist at that path"
    (fs/with-temp-directory path
      (is (not (fs/exists? (fs/join-paths path "asdf"))))))
  (testing "optionally follows symlinks"
    (fs/with-temp-directory path
      (fs/create-symlink (fs/as-path path "foo") (fs/as-path path "bar"))
      (is (not (fs/exists? (fs/join-paths path "foo"))))
      (is (fs/exists? (fs/join-paths path "foo") {:nofollow-links true})))))

(deftest test-directory?
  (testing "returns true for directories"
    (fs/with-temp-directory path
      (is (fs/directory? path))))
  (testing "returns false for anything else"
    (fs/with-temp-file path
      (is (not (fs/directory? path)))))
  (testing "optionally follows symlinks"
    (fs/with-temp-directory path
      (let [link-path (fs/join-paths path "my-link")]
        (fs/create-symlink link-path path)
        (is (fs/directory? link-path))
        (is (not (fs/directory? link-path {:nofollow-links true})))))))

(deftest test-executable?
  (testing "returns true for executable files"
    (fs/with-temp-file path
      (fs/set-posix-file-permissions path 700)
      (is (fs/executable? path))))
  (testing "returns false for other files"
    (fs/with-temp-file path
      (fs/set-posix-file-permissions path 600)
      (is (not (fs/executable? path))))))

(deftest test-hidden?
  (testing "returns true for hidden files"
    (fs/with-temp-directory path
      (let [file-path (fs/join-paths path ".file")]
        (fs/create-file file-path)
        (is (fs/hidden? file-path)))))
  (testing "returns false for visible files"
    (fs/with-temp-file path
      (is (not (fs/hidden? path))))))

(deftest test-writable?
  (testing "returns true for files that are writable"
    (fs/with-temp-file path
      (fs/set-posix-file-permissions path 700)
      (is (fs/writable? path))))
  (testing "returns false for files that are not writable"
    (fs/with-temp-file path
      (fs/set-posix-file-permissions path 400)
      (is (not (fs/writable? path))))))

(deftest test-readable?
  (testing "returns true for files that are readable"
    (fs/with-temp-file path
      (fs/set-posix-file-permissions path 700)
      (is (fs/readable? path))))
  (testing "returns false for files that are not readable"
    (fs/with-temp-file path
      (fs/set-posix-file-permissions path 300)
      (is (not (fs/readable? path))))))

(deftest test-regular-file?
  (testing "returns true for regular files"
    (fs/with-temp-file path
      (is (fs/regular-file? path))))
  (testing "returns false for non-regular files"
    (fs/with-temp-directory path
      (is (not (fs/regular-file? path)))))
  (testing "optionally follows symlinks"
    (fs/with-temp-file path
      (let [link-path (fs/join-paths (fs/parent-path path) "my-link")]
        (fs/create-symlink link-path path)
        (is (fs/regular-file? link-path))
        (is (not (fs/regular-file? link-path {:nofollow-links true})))))))

(deftest test-same-file?
  (testing "returns true when the files are the same"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")
            file3 (fs/join-paths path "file3")]
        (fs/create-file file1 {:content "asdf"})
        (fs/create-link file2 file1)
        (fs/create-symlink file3 file1)
        (is (fs/same-file? file1 file1))
        (is (fs/same-file? file1 file2))
        (is (fs/same-file? file1 file3)))))
  (testing "returns false when the files are different"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1 {:content "asdf"})
        (fs/create-file file2 {:content "asdf"})
        (is (not (fs/same-file? file1 file2)))))))

(deftest test-symlink?
  (testing "returns true when the file is a symlink"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1)
        (fs/create-symlink file2 file1)
        (is (fs/symlink? file2)))))
  (testing "returns false when the file is not a symlink"
    (fs/with-temp-directory path
      (is (not (fs/symlink? path))))))

(deftest test-copy
  (testing "copies a file from one location to another"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1)
        (is (not (fs/exists? file2)))
        (fs/copy file1 file2)
        (is (fs/exists? file2)))))
  (testing "copies a directory from one location to another but not the contents"
    (fs/with-temp-directory path
      (let [dir1 (fs/join-paths path "dir1")
            dir2 (fs/join-paths path "dir2")]
        (fs/create-directory dir1)
        (fs/create-file (fs/join-paths dir1 "file1"))
        (is (not (fs/exists? dir2)))
        (fs/copy dir1 dir2)
        (is (fs/exists? dir2))
        (is (empty? (fs/children dir2))))))
  (testing "copies with the option to replace the existing file"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1 {:content "asdf"})
        (fs/create-file file2 {:content "fdsa"})
        (is (thrown? FileAlreadyExistsException
                     (fs/copy file1 file2)))
        (fs/copy file1 file2 {:replace-existing true})
        (is (= (slurp file1) (slurp file2))))))
  (testing "copies with the option to copy attributes"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")
            file3 (fs/join-paths path "file3")]
        (fs/create-file file1)
        (fs/set-posix-file-permissions file1 421)
        (fs/copy file1 file2)
        (fs/copy file1 file3 {:copy-attributes true})
        (is (not= (->> file1 fs/get-posix-file-permissions (map str) sort vec)
                  (->> file2 fs/get-posix-file-permissions (map str) sort vec)))
        (is (->> file1 fs/get-posix-file-permissions (map str) sort vec)
            (->> file3 fs/get-posix-file-permissions (map str) sort vec)))))
  (testing "copies with the option to not follow symlinks"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")
            file3 (fs/join-paths path "file3")
            file4 (fs/join-paths path "file4")]
        (fs/create-file file1)
        (fs/create-symlink file2 file1)
        (fs/copy file2 file3)
        (fs/copy file2 file4 {:nofollow-links true})
        (is (not (fs/symlink? file3)))
        (is (fs/symlink? file4))))))

(deftest test-copy-recursively
  (testing "copies files"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1)
        (fs/copy-recursively file1 file2)
        (is (fs/exists? file2)))))
  (testing "copies directories"
    (fs/with-temp-directory path
      (let [dir1 (fs/join-paths path "dir1")
            dir2 (fs/join-paths path "dir2")]
        (fs/create-directory dir1)
        (fs/copy-recursively dir1 dir2)
        (is (fs/exists? dir2)))))
  (testing "copies directories and their contents recursively"
    (fs/with-temp-directory path
      (let [dir1 {:path (fs/join-paths path "dir1")
                  :file1 (fs/join-paths path "dir1" "file1")
                  :file2 (fs/join-paths path "dir1" "file2")}
            dir2 {:path (fs/join-paths path "dir2")
                  :file1 (fs/join-paths path "dir2" "file1")
                  :file2 (fs/join-paths path "dir2" "file2")}]
        (fs/create-directory (:path dir1))
        (fs/create-file (:file1 dir1))
        (fs/create-file (:file2 dir1))
        (fs/copy-recursively (:path dir1) (:path dir2))
        (is (fs/exists? (:path dir2)))
        (is (fs/exists? (:file1 dir2))
            (is (fs/exists? (:file2 dir2)))))))
  (testing "copies within the destination if it is a directory"
    (fs/with-temp-directory path
      (let [dir1 {:path (fs/join-paths path "dir1")
                  :file1 (fs/join-paths path "dir1" "file1")}
            dir2 {:path (fs/join-paths path "dir2")
                  :file1 (fs/join-paths path "dir2" "file1")}]
        (fs/create-directory (:path dir1))
        (fs/create-file (:file1 dir1) {:content "asdf11"})
        (fs/create-directory (:path dir2))
        (fs/create-file (:file1 dir2) {:content "asdf21"})
        (fs/copy-recursively (:path dir1) (:path dir2) {:replace-existing true})
        (is (= "asdf21" (slurp (:file1 dir2))))
        (is (fs/exists? (fs/join-paths path "dir2" "dir1" "file1"))))))
  (testing "copies with the replace-existing option"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1 {:content "asdf"})
        (fs/create-file file2 {:content "fdsa"})
        (fs/copy-recursively file1 file2 {:replace-existing true})
        (is (= "asdf" (slurp file2))))))
  (testing "copies with the copy-attributes option"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1)
        (fs/set-posix-file-permissions file1 777)
        (fs/copy-recursively file1 file2 {:copy-attributes true})
        (is (= 777 (fs/get-posix-file-permissions file2 {:format :octal}))))))
  (testing "copies with the nofollow-links option"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")
            file3 (fs/join-paths path "file3")
            file4 (fs/join-paths path "file4")]
        (fs/create-file file1)
        (fs/create-symlink file2 file1)
        (fs/copy-recursively file2 file3)
        (fs/copy-recursively file2 file4 {:nofollow-links true})
        (is (not (fs/symlink? file3)))
        (is (fs/symlink? file4))))))

(deftest test-move
  (testing "moves files"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1)
        (fs/move file1 file2)
        (is (not (fs/exists? file1)))
        (is (fs/exists? file2)))))
  (testing "moves directories"
    (fs/with-temp-directory path
      (let [dir1 (fs/join-paths path "dir1")
            dir2 (fs/join-paths path "dir2")]
        (fs/create-directory dir1)
        (fs/move dir1 dir2)
        (is (not (fs/exists? dir1)))
        (is (fs/exists? dir2)))))
  (testing "moves with the replace-existing option"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1 {:content "asdf"})
        (fs/create-file file2 {:content "fdsa"})
        (is (thrown? FileAlreadyExistsException
                     (fs/move file1 file2)))
        (fs/move file1 file2 {:replace-existing true})
        (is (not (fs/exists? file1)))
        (is (fs/exists? file2))
        (is (= "asdf" (slurp file2))))))
  (testing "moves with atomic-move option"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1)
        (fs/move file1 file2 {:atomic-move true})
        (is (not (fs/exists? file1)))
        (is (fs/exists? file2))))))

(deftest test-rename
  (testing "renames a file"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1)
        (fs/rename file1 "file2")
        (is (not (fs/exists? file1)))
        (is (fs/exists? file2)))))
  (testing "renames a directory"
    (fs/with-temp-directory path
      (let [dir1 (fs/join-paths path "dir1")
            dir2 (fs/join-paths path "dir2")]
        (fs/create-directory dir1)
        (fs/rename dir1 "dir2")
        (is (not (fs/exists? dir1)))
        (is (fs/exists? dir2)))))
  (testing "renames with the replace-existing option"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1 {:content "asdf"})
        (fs/create-file file2 {:content "fdsa"})
        (is (thrown? FileAlreadyExistsException
                     (fs/rename file1 "file2")))
        (fs/rename file1 "file2" {:replace-existing true})
        (is (not (fs/exists? file1)))
        (is (fs/exists? file2))
        (is (= "asdf" (slurp file2))))))
  (testing "renames with atomic-move option"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1)
        (fs/rename file1 "file2" {:atomic-move true})
        (is (not (fs/exists? file1)))
        (is (fs/exists? file2))))))

(deftest test-create-directory
  (testing "creates a directory"
    (fs/with-temp-directory path
      (let [dir (fs/join-paths path "dir")]
        (fs/create-directory dir)
        (is (fs/directory? dir)))))
  (testing "creates a directory with posix file permissions"
    (fs/with-temp-directory path
      (let [dir (fs/join-paths path "dir")]
        (fs/create-directory dir {:posix-file-permissions 200})
        (is (fs/directory? dir))
        (is (= 200 (fs/get-posix-file-permissions dir {:format :octal})))))))

(deftest test-create-directories
  (testing "creates a directory and all parent directories"
    (fs/with-temp-directory path
      (let [dir (fs/join-paths path "dir1" "dir2" "dir3")]
        (fs/create-directories dir)
        (is (fs/directory? dir)))))
  (testing "creates directories with the :posix-file-permissions option"
    (fs/with-temp-directory path
      (let [dir (fs/join-paths path "dir1" "dir2" "dir3")]
        (fs/create-directories dir {:posix-file-permissions 700})
        (is (fs/directory? dir))
        (is (= 700 (fs/get-posix-file-permissions dir {:format :octal})))
        (is (= 700 (fs/get-posix-file-permissions (fs/parent-path dir) {:format :octal})))))))

(deftest test-create-temp-directory
  (testing "creates a temp directory with the given prefix"
    (let [path (fs/canonical-path (fs/create-temp-directory "test-fs"))]
      (try
        (is (fs/directory? path))
        (is (.startsWith path (str fs/tmpdir "/")))
        (is (.startsWith (fs/last-path-segment path) "test-fs"))
        (finally (fs/delete-recursively path)))))
  (testing "creates a directory with posix file permissions"
    (let [path (fs/create-temp-directory "test-fs" {:posix-file-permissions 200})]
      (try
        (is (= 200 (fs/get-posix-file-permissions path {:format :octal})))
        (finally (fs/delete-recursively path))))))

(deftest test-append-to-file
  (testing "appends content to a file"
    (fs/with-temp-file path
      (fs/append-to-file path "asdf")
      (fs/append-to-file path "fdsa")
      (is (= "asdffdsa" (slurp path))))))

(deftest test-create-file
  (testing "creates a file"
    (fs/with-temp-directory path
      (let [file (fs/join-paths path "file")]
        (fs/create-file file)
        (is (fs/regular-file? file)))))
  (testing "creates a file with posix file permissions"
    (fs/with-temp-directory path
      (let [file (fs/join-paths path "file")]
        (fs/create-file file {:posix-file-permissions 200})
        (is (fs/regular-file? file))
        (is (= 200 (fs/get-posix-file-permissions file {:format :octal})))))))

(deftest test-create-link
  (testing "creates a hard link to a file"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1)
        (fs/create-link file2 file1)
        (is (fs/regular-file? file1))
        (is (fs/regular-file? file2))
        (is (fs/same-file? file1 file2))))))

(deftest test-create-symlink
  (testing "creates a symbolic link to a file"
    (fs/with-temp-directory path
      (let [file1 (fs/join-paths path "file1")
            file2 (fs/join-paths path "file2")]
        (fs/create-file file1)
        (fs/create-symlink file2 file1)
        (is (fs/regular-file? file1))
        (is (fs/symlink? file2))
        (is (fs/same-file? file1 file2))))))

(deftest test-delete
  (testing "deletes a file"
    (fs/with-temp-directory path
      (let [file (fs/join-paths path "file")]
        (fs/create-file file)
        (is (fs/exists? file))
        (fs/delete file)
        (is (not (fs/exists? file)))))))

(deftest test-delete-if-exists
  (testing "does not throw an error when the file does not exist"
    (fs/with-temp-directory path
      (let [file (fs/join-paths path "file")]
        (is (thrown? NoSuchFileException (fs/delete file)))
        (fs/delete-if-exists file)))))

(deftest test-delete-recursively
  (testing "deletes recursively"
    (fs/with-temp-directory path
      (let [dir (fs/join-paths path "dir")
            file1 (fs/join-paths path "dir" "file1")
            file2 (fs/join-paths path "dir" "file2")]
        (fs/create-directory dir)
        (fs/create-file file1)
        (fs/create-file file2)
        (is (fs/exists? dir))
        (fs/delete-recursively dir)
        (is (not (fs/exists? dir))))))
  (testing "deletes recursively with the nofollow-links option"
    (fs/with-temp-directory path
      (let [dir1 (fs/join-paths path "dir1")
            dir2 (fs/join-paths path "dir2")
            file1 (fs/join-paths path "dir1" "file1")
            file2 (fs/join-paths path "dir1" "file2")]
        (fs/create-directory dir1)
        (fs/create-symlink dir2 dir1)
        (fs/create-file file1)
        (fs/create-file file2)
        (is (fs/exists? dir1))
        (is (fs/exists? dir2))
        (fs/delete-recursively dir2 {:nofollow-links true})
        (is (not (fs/exists? dir2)))
        (is (fs/exists? dir1))))))

(deftest test-file-system-for
  (testing "returns the filesystem for a path"
    (is (= fs/default-file-system
           (fs/file-system-for "/")))))

(deftest test-supported-file-attribute-views
  (testing "returns the list of supported file attribute views"
    (is (= #{"owner" "basic" "posix" "unix"}
           (fs/supported-file-attribute-views "/")))))
