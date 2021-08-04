;;; wad-tests.el --- Tests for wad.el -*- lexical-binding: t;-*-

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'wad)

;;;; User

(ert-deftest wad! () (ert-skip "TODO"))

(ert-deftest wad-package! () (ert-skip "TODO"))  

;;;; Modules

(ert-deftest wad--modules-get ()
  (cl-letf
      ((wad--modules (make-hash-table :test 'equal)))
    (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
    (should (equal
	     (wad--modules-get :foo 'bar)
	     '(:recipe "recipe"))))
  (cl-letf
      ((wad--modules (make-hash-table :test 'equal)))
    (should (equal (wad--modules-get :foo 'bar) nil)))
  (cl-letf
      ((wad--modules (make-hash-table :test 'equal)))
    (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
    (should (equal (wad--modules-get :foo 'bar :recipe) "recipe"))))

(ert-deftest wad--modules-set ()
  (cl-letf ((wad--modules (make-hash-table :test 'equal)))
    (should (equal (wad--modules-set :foo 'bar) nil))))

(ert-deftest wad--modules-map ()
  (let ((x 0))
    (cl-letf ((wad--modules (make-hash-table :test 'equal)))
      (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
      (wad--modules-map
       (lambda (&rest args)
	 (if (= x 0) (should (equal args '(:foo bar (:recipe "recipe")))))
	 (setq x (+ x 1))))
      (should (equal x 1)))))

(ert-deftest wad--modules-init ()
  (let ((hook-called-times 0) (init-called-times 0))
    (cl-letf (((symbol-function 'wad--module-init)
	       (lambda (&rest args)
		 (setq init-called-times (+ 1 init-called-times))))
	      (wad--modules (make-hash-table :test 'equal)))
      (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
      (puthash (cons :bar 'foo) '() wad--modules)
      (wad--modules-init)
      (should (equal init-called-times 2))
      (should (equal hook-called-times 0))))
  (let ((hook-called-times 0)
	(init-called-times 0))
    (cl-letf (((symbol-function 'wad--module-init)
	       (lambda (&rest args)
		 (setq init-called-times (+ 1 init-called-times))))
	      (wad--modules (make-hash-table :test 'equal))
	      (wad--modules-init (list (cons :foo 'bar))))
      (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
      (puthash (cons :bar 'foo) '() wad--modules)
      (wad--modules-init)
      (should (equal init-called-times 1))
      (should (equal hook-called-times 0))))
  (let ((hook-called-times 0)
	(init-called-times 0))
    (cl-letf (((symbol-function 'wad--module-init)
	       (lambda (&rest args)
		 (setq init-called-times (+ 1 init-called-times))))
	      (wad--modules (make-hash-table :test 'equal))
	      (wad--hook-modules-before-init '())
	      (wad--hook-modules-after-init '()))
      (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
      (add-hook 'wad--hook-modules-before-init
		(lambda ()
		  (setq hook-called-times (+ 1 hook-called-times))))
      (add-hook 'wad--hook-modules-before-init
		(lambda ()
		  (setq hook-called-times (+ hook-called-times 1))))
      (add-hook 'wad--hook-modules-after-init
		(lambda ()
		  (setq hook-called-times (+ 1 hook-called-times))))
      (wad--modules-init)
      (should (equal init-called-times 1))
      (should (equal hook-called-times 3)))))

(ert-deftest wad--modules-init-packages ()
  (let ((hook-called-times 0) (init-packages-called-times 0))
    (cl-letf (((symbol-function 'wad--module-init-packages)
	       (lambda (&rest args)
		 (setq init-packages-called-times (+ 1 init-packages-called-times))))
	      (wad--modules (make-hash-table :test 'equal)))
      (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
      (puthash (cons :bar 'foo) '() wad--modules)
      (wad--modules-init-packages)
      (should (equal init-packages-called-times 2))
      (should (equal hook-called-times 0))))
  (let ((hook-called-times 0)
	(init-packages-called-times 0))
    (cl-letf (((symbol-function 'wad--module-init-packages)
	       (lambda (&rest args)
		 (setq init-packages-called-times (+ 1 init-packages-called-times))))
	      (wad--modules (make-hash-table :test 'equal))
	      (wad--modules-init-packages (list (cons :foo 'bar))))
      (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
      (puthash (cons :bar 'foo) '() wad--modules)
      (wad--modules-init-packages)
      (should (equal init-packages-called-times 1))
      (should (equal hook-called-times 0))))
  (let ((hook-called-times 0)
	(init-packages-called-times 0))
    (cl-letf (((symbol-function 'wad--module-init-packages)
	       (lambda (&rest args)
		 (setq init-packages-called-times (+ 1 init-packages-called-times))))
	      (wad--modules (make-hash-table :test 'equal))
	      (wad--hook-modules-before-init-packages '())
	      (wad--hook-modules-after-init-packages '()))
      (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
      (add-hook 'wad--hook-modules-before-init-packages
		(lambda ()
		  (setq hook-called-times (+ 1 hook-called-times))))
      (add-hook 'wad--hook-modules-before-init-packages
		(lambda ()
		  (setq hook-called-times (+ hook-called-times 1))))
      (add-hook 'wad--hook-modules-after-init-packages
		(lambda ()
		  (setq hook-called-times (+ 1 hook-called-times))))
      (wad--modules-init-packages)
      (should (equal init-packages-called-times 1))
      (should (equal hook-called-times 3)))))

(ert-deftest wad--modules-ensure-packages ()
  (let ((hook-called-times 0) (ensure-packages-called-times 0))
    (cl-letf (((symbol-function 'wad--module-ensure-packages)
	       (lambda (&rest args)
		 (setq ensure-packages-called-times (+ 1 ensure-packages-called-times))))
	      (wad--modules (make-hash-table :test 'equal)))
      (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
      (puthash (cons :bar 'foo) '() wad--modules)
      (wad--modules-ensure-packages)
      (should (equal ensure-packages-called-times 2))
      (should (equal hook-called-times 0))))
  (let ((hook-called-times 0)
	(ensure-packages-called-times 0))
    (cl-letf (((symbol-function 'wad--module-ensure-packages)
	       (lambda (&rest args)
		 (setq ensure-packages-called-times (+ 1 ensure-packages-called-times))))
	      (wad--modules (make-hash-table :test 'equal))
	      (wad--modules-ensure-packages (list (cons :foo 'bar))))
      (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
      (puthash (cons :bar 'foo) '() wad--modules)
      (wad--modules-ensure-packages)
      (should (equal ensure-packages-called-times 1))
      (should (equal hook-called-times 0))))
  (let ((hook-called-times 0)
	(ensure-packages-called-times 0))
    (cl-letf (((symbol-function 'wad--module-ensure-packages)
	       (lambda (&rest args)
		 (setq ensure-packages-called-times (+ 1 ensure-packages-called-times))))
	      (wad--modules (make-hash-table :test 'equal))
	      (wad--hook-modules-before-ensure-packages '())
	      (wad--hook-modules-after-ensure-packages '()))
      (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
      (add-hook 'wad--hook-modules-before-ensure-packages
		(lambda ()
		  (setq hook-called-times (+ 1 hook-called-times))))
      (add-hook 'wad--hook-modules-before-ensure-packages
		(lambda ()
		  (setq hook-called-times (+ hook-called-times 1))))
      (add-hook 'wad--hook-modules-after-ensure-packages
		(lambda ()
		  (setq hook-called-times (+ 1 hook-called-times))))
      (wad--modules-ensure-packages)
      (should (equal ensure-packages-called-times 1))
      (should (equal hook-called-times 3)))))

(ert-deftest wad--modules-config ()
  (let ((hook-called-times 0) (config-called-times 0))
    (cl-letf (((symbol-function 'wad--module-config)
	       (lambda (&rest args)
		 (setq config-called-times (+ 1 config-called-times))))
	      (wad--modules (make-hash-table :test 'equal)))
      (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
      (puthash (cons :bar 'foo) '() wad--modules)
      (wad--modules-config)
      (should (equal config-called-times 2))
      (should (equal hook-called-times 0))))
  (let ((hook-called-times 0) (config-called-times 0))
    (cl-letf (((symbol-function 'wad--module-config)
	       (lambda (&rest args)
		 (setq config-called-times (+ 1 config-called-times))))
	      (wad--modules (make-hash-table :test 'equal))
	      (wad--modules-config (list (cons :foo 'bar))))
      (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
      (puthash (cons :bar 'foo) '() wad--modules)
      (wad--modules-config)
      (should (equal config-called-times 1))
      (should (equal hook-called-times 0))))
  (let ((hook-called-times 0) (config-called-times 0))
    (cl-letf (((symbol-function 'wad--module-config)
	       (lambda (&rest args)
		 (setq config-called-times (+ 1 config-called-times))))
	      (wad--modules (make-hash-table :test 'equal))
	      (wad--hook-modules-before-config '())
	      (wad--hook-modules-after-config '()))
      (puthash (cons :foo 'bar) '(:recipe "recipe") wad--modules)
      (add-hook 'wad--hook-modules-before-config
		(lambda ()
		  (setq hook-called-times (+ 1 hook-called-times))))
      (add-hook 'wad--hook-modules-before-config
		(lambda ()
		  (setq hook-called-times (+ hook-called-times 1))))
      (add-hook 'wad--hook-modules-after-config
		(lambda ()
		  (setq hook-called-times (+ 1 hook-called-times))))
      (wad--modules-config)
      (should (equal config-called-times 1))
      (should (equal hook-called-times 3)))))
 
(ert-deftest wad--module-enabled-p ()
  (cl-letf ((wad--modules (make-hash-table :test 'equal)))
    (puthash (cons :foo 'bar) '(:hello "world") wad--modules)
    (should (wad--module-enabled-p :foo 'bar))
    (should (not (wad--module-enabled-p :bar 'foo)))))

(ert-deftest wad--module-init ()
  (let (file category module)
    (cl-letf ((wad--modules-init)
	      ((symbol-function 'wad--module-file-loader)
	     (lambda (x)
	       (setq file x)
	       (lambda (y z _)
		 (setq category y)
		 (setq module z)))))
    (wad--module-init :foo 'bar)
    (should (equal file wad--module-init-file))
    (should (equal category :foo))
    (should (equal module 'bar))
    (should (wad--module-init-p :foo 'bar)))))

(ert-deftest wad--module-init-p ()
  (cl-letf ((wad--modules-init '((:foo . bar))))
    (should (wad--module-init-p :foo 'bar))
    (should (not (wad--module-init-p :bar 'foo)))))

(ert-deftest wad--module-init-packages ()
  (let (file category module)
    (cl-letf ((wad--modules-init-packages)
	      ((symbol-function 'wad--module-file-loader)
	       (lambda (x)
		 (setq file x)
		 (lambda (y z _)
		   (setq category y)
		   (setq module z)))))
      (wad--module-init-packages :foo 'bar)
      (should (equal file wad--module-packages-file))
      (should (equal category :foo))
      (should (equal module 'bar))
      (should (wad--module-init-packages-p :foo 'bar)))))

(ert-deftest wad--module-init-packages-p ()
  (cl-letf ((wad--modules-init-packages '((:foo . bar))))
    (should (wad--module-init-packages-p :foo 'bar))
    (should (not (wad--module-init-packages-p :bar 'foo)))))

(ert-deftest wad--module-ensure-packages ()
  (let (was-called args)
    (cl-letf ((wad-packages '((pkg . (:modules ((:foo . bar))))))
	      ((symbol-function 'wad--packages-ensure)
	       (lambda (&rest x)
		 (setq was-called t)
		 (setq args x))))
      (wad--module-ensure-packages :foo 'bar)
      (should was-called)
      (should (equal args '(pkg (:modules ((:foo . bar))))))))
  (let (was-called)
    (cl-letf ((wad-packages '())
	      ((symbol-function 'wad--packages-ensure)
	       (lambda (&rest x) (setq was-called t))))
      (wad--module-ensure-packages :foo 'bar)
      (should (not was-called)))))

(ert-deftest wad--module-ensure-packages-p ()
  (cl-letf ((wad--modules-ensure-packages '((:foo . bar))))
    (should (wad--module-ensure-packages-p :foo 'bar))
    (should (not (wad--module-ensure-packages-p :bar 'foo)))))

(ert-deftest wad--module-config ()
  (let (file category module)
    (cl-letf ((wad--modules-config)
	      ((symbol-function 'wad--module-file-loader)
	       (lambda (x)
		 (setq file x)
		 (lambda (y z _)
		   (setq category y)
		   (setq module z)))))
      (wad--module-config :foo 'bar)
      (should (equal file wad--module-config-file))
      (should (equal category :foo))
      (should (equal module 'bar))
      (should (wad--module-config-p :foo 'bar)))))

(ert-deftest wad--module-config-p ()
  (cl-letf ((wad--modules-config '((:foo . bar))))
    (should (wad--module-config-p :foo 'bar))
    (should (not (wad--module-config-p :bar 'foo)))))

(ert-deftest wad--module-key ()
  (should (equal (wad--module-key :foo 'bar) (cons :foo 'bar)))
  (should (equal (wad--module-key 'foo 'bar) nil))
  (should (equal (wad--module-key :foo "bar") nil)))

(ert-deftest wad--module-path-from-key ()
  (cl-letf ((wad--modules-directories '("/modules/"))
	    ((symbol-function 'file-exists-p) (lambda (_) t)))
    (should (equal
	     (wad--module-path-from-key :foobar)
	     "/modules/foobar/")))
  (cl-letf ((wad--modules-directories '("/modules/"))
	    ((symbol-function 'file-exists-p) (lambda (_) t)))
    (should (equal
	     (wad--module-path-from-key :foo 'bar)
	     "/modules/foo/bar/")))
  (cl-letf ((wad--modules-directories '("/modules/"))
	    ((symbol-function 'file-exists-p) (lambda (_) t)))
    (should (equal
	     (wad--module-path-from-key :foo 'bar "init.el")
	     "/modules/foo/bar/init.el"))))

(ert-deftest wad--module-key-from-path ()
  (cl-letf (((symbol-function 'wad--file-name)
	     (lambda () "/modules/foo/bar")))
    (should (equal (wad--module-key-from-path)
		   (cons :foo 'bar))))
  (cl-letf (((symbol-function 'wad--file-name)
	     (lambda () "/modules/foo/bar"))
	    (wad--modules-current '((:foo . bar))))
    (should (equal (wad--module-key-from-path)
		   (cons :foo 'bar))))
  (should (equal (wad--module-key-from-path "/modules/foo/bar")
		 (cons :foo 'bar))))

(ert-deftest wad--module-file-loader ()
  (let (args)
    (cl-letf (((symbol-function 'wad--load) (lambda (&rest x) (setq args x))))
      (funcall (wad--module-file-loader "foobar") :foo 'bar '(:path "/foo/bar"))
      (should (equal args '("foobar" "/foo/bar" t))))))

(ert-deftest wad--module-packages-map ()
  (let ((was-called-times 0))
    (cl-flet ((fn (_ _) (setq was-called-times (+ was-called-times 1))))
      (cl-letf ((wad-packages '((foo . (:modules ((:foo . bar))))
				(bar . (:modules ((:foo . bar))))
				(quz . (:modules ((:bar . foo)))))))
	(wad--module-packages-map #'fn (wad--module-key :foo 'bar))
	(should (equal was-called-times 2))))))

;;;; Packages

(ert-deftest wad--packages-add ()
  (let (wad-packages)
    (wad--packages-add 'foo '(:bar "qux"))
    (should (equal
	     (alist-get 'foo wad-packages)
	     '(:bar "qux")))))

(ert-deftest wad--packages-get ()
  (let ((wad-packages '((foo . (:bar "qux")))))
    (should (equal (wad--packages-get 'foo) '(:bar "qux")))
    (should (equal (wad--packages-get 'foo :bar) "qux"))))

(ert-deftest wad--packages-set ()
  (let ((wad-packages '((foo . (:bar "qux")))))
    (wad--packages-set 'foo :bar "quz")
    (should (equal
	     (alist-get 'foo wad-packages)
	     '(:bar "quz")))))

(ert-deftest wad--packages-pinned ()
  (let ((wad-packages '((foo . (:ignore))
			(bar . (:disable))
			(qux . (:pin "123456"))
			(quz . (:unpin "123456")))))
    (should (equal (wad--packages-pinned)
		   '(("qux" . "123456"))))))

(ert-deftest wad--packages-ensure ()
  (let (args)
    (cl-letf ((wad-packages
	       '((foobar . (:recipe "foo"))))
	      ((symbol-function 'wad--packages-ensure-handler)
	       (lambda (&rest x) (setq args x))))
      (wad--packages-ensure 'foobar)
      (should (equal args '(foobar (:recipe "foo") "foo")))))
  (let (args)
    (cl-letf ((wad-packages
	       '((foobar . ())))
	      ((symbol-function 'wad--packages-ensure-handler)
	       (lambda (&rest x) (setq args x))))
      (wad--packages-ensure 'foobar '(:recipe "bar"))
      (should (equal args '(foobar (:recipe "bar") "bar")))))
  (let (args)
    (cl-letf ((wad-packages
	       '((foobar . (:recipe "qux"))))
	      (wad--packages-ensure-handlers
	       (list (cons 'foobar #'(lambda (&rest x) (setq args x))))))
      (wad--packages-ensure 'foobar)
      (should (equal args '(foobar (:recipe "qux") "qux"))))))

(ert-deftest wad--packages-load ()
  (let (args)
    (cl-letf ((wad-packages
	       '((foobar . (:recipe "foo"))))
	      ((symbol-function 'wad--packages-load-handler)
	       (lambda (&rest x) (setq args x))))
      (wad--packages-load 'foobar)
      (should (equal args '(foobar (:recipe "foo") "foo")))))
  (let (args)
    (cl-letf ((wad-packages
	       '((foobar . ())))
	      ((symbol-function 'wad--packages-load-handler)
	       (lambda (&rest x) (setq args x))))
      (wad--packages-load 'foobar '(:recipe "bar"))
      (should (equal args '(foobar (:recipe "bar") "bar")))))
  (let (args)
    (cl-letf ((wad-packages
	       '((foobar . (:recipe "qux"))))
	      (wad--packages-load-handlers
	       (list (cons 'foobar #'(lambda (&rest x) (setq args x))))))
      (wad--packages-load 'foobar)
      (should (equal args '(foobar (:recipe "qux") "qux"))))))

(ert-deftest wad--packages-reset ()
  (let ((wad-packages '((foo . (:bar "qux")))))
    (wad--packages-reset)
    (should (equal wad-packages '()))))

;;;; Hooks

(ert-deftest wad--hook-try-run ()
  (let ((hook-called-times 0))
    (cl-letf ((my-hook '()))
      (add-hook 'my-hook
		(lambda ()
		  (setq hook-called-times (+ hook-called-times 1))))
      (run-hook-wrapped 'my-hook #'wad--hook-try-run)
      (should (equal hook-called-times 1)))))

;;;; Library Helpers

(ert-deftest wad--directory-name ()
  (let (byte-compile-current-file
	(load-file-name "/tmp/abc.def"))
    (should (equal
	     (wad--directory-name)
	     "/tmp"))))

(ert-deftest wad--file-name-load-file ()
  (let (byte-compile-current-file
	(load-file-name "foobar"))
    (should (equal
	     (wad--file-name)
	     "foobar"))))

(ert-deftest wad--file-name-buffer ()
  (let ((byte-compiled-current-file nil)
	(buffer-file-name "foobar"))
    (should (equal
	     (wad--file-name)
	     "foobar"))))

(ert-deftest wad--file-name-error ()
  (let ((byte-compiled-current-file nil)
	(current-load-list nil))
    (should-error (wad--file-name))))

(ert-deftest wad--path-with-base ()
  (should (equal (wad--path "foo" "/bar")
		 "/bar/foo")))

(ert-deftest wad--path-without-base ()
  (cl-letf (((symbol-function 'wad--directory-name)
	     (lambda () "/tmp/")))
    (should (equal (wad--path "foo") "/tmp/foo"))))

(ert-deftest wad--load-filename ()
  (let (load-args)
    (cl-flet ((load (&rest args) (setq load-args args)))
      (wad--load "foobar")
      (should (string-suffix-p "/foobar" (car load-args)))
      (should (equal (cdr load-args) '(nil nomessage))))))

(ert-deftest wad--load-filename-and-path ()
  (let (load-args)
    (cl-flet ((load (lambda (&rest args) (setq load-args args))))
      (wad--load "foobar" "/tmp")
      (should (equal load-args '("/tmp/foobar" nil nomessage))))))

(ert-deftest wad--process-with-debug-p ()
  (let ((wad--debug-p t))
  (cl-letf (((symbol-function 'wad--process-sync)
	     (lambda (&rest _) (setq process-sync-called t))))
    (wad--process "echo" "hello world")
    (should process-sync-called))))

(ert-deftest wad--process-without-debug-p ()
  (let ((wad--debug-p nil))
    (cl-letf (((symbol-function 'wad--process-async)
	       (lambda (&rest _) (setq process-async-called t))))
      (wad--process "echo" "hello world")
      (should process-async-called))))

(ert-deftest wad--process-sync ()
  (should (equal (wad--process-sync "bash" "-c" "echo foobar")
		 '(0 . "foobar")))
  (should (equal (wad--process-sync "bash" "-c" "exit 2")
		 '(2 . ""))))

(ert-deftest wad--process-async ()
  (should (equal (wad--process-async "bash" "-c" "exit 0")
		 '(0 . "")))
  (should (equal (wad--process-async "bash" "-c" "exit 2")
		 '(2 . ""))))

(ert-deftest wad--log-with-debug-p ()
  (let ((wad--debug-p t)	message-args)
    (cl-flet ((message (&rest args) (setq message-args args)))
      (wad--log "foo" 'bar)
      (should (equal message-args '("foo" bar))))))

(ert-deftest wad--log-without-debug-p ()
  (let ((wad--debug-p nil) message-called)
    (cl-flet ((message (&rest _) (setq message-called t)))
      (wad--log "foo" 'bar)
      (should (equal message-called nil)))))

(ert-deftest wad--string-to-keyword-with-string ()
  (should (equal (wad--string-to-keyword "foo") :foo)))

(ert-deftest wad--string-to-keyword-with-keyword ()
  (condition-case err
      (wad--string-to-keyword :foo)
    (wrong-type-argument t)))

(ert-deftest wad--keyword-to-string-with-keyword ()
  (should (equal (wad--keyword-to-string :bar) "bar")))

(ert-deftest wad--keyword-to-string-with-string ()
  (condition-case err
      (wad--keyword-to-string "bar")
    (wrong-type-argument t)))

(ert-deftest wad--ensure-list-with-string ()
  (should (equal (wad--ensure-list "foo")
		 '("foo"))))

(ert-deftest wad--ensure-list-with-list ()
  (should (equal (wad--ensure-list '("foo" "bar"))
		 '("foo" "bar"))))

(ert-deftest wad--plist-put! ()
  (let ((plist '(:foo "bar")))
    (wad--plist-put! plist :bar "bar" :foo "foo")
    (should (equal plist '(:foo "foo" :bar "bar")))))

(ert-deftest wad--plist-delete ()
  (let ((plist '(:foo "bar")))
    (should
     (equal
      (wad--plist-delete plist :bar "bar" :foo "foo")
      '()))
    (should (equal plist '(:foo "bar")))))

(ert-deftest wad--plist-delete! ()
  (let ((plist '(:foo "bar")))
    (wad--plist-delete! plist :foo)
    (should (equal plist '()))))

(ert-deftest wad---loop-plist! ()
  (let ((plist '(:foo "bar" :bar "bar"))
	(props '())
	(values (list)))
    (should (equal
	     (wad--loop-plist!
		 ((p v) plist :foobar)
	       (setq props (cons p (or props '())))
	       (setq values (cons v (or values '()))))
	     :foobar))
    (should (equal props '(:bar :foo)))
    (should (equal values '("bar" "bar")))))

(ert-deftest wad--rpartial ()
  (should (equal
	   (funcall (wad--rpartial #'+ 10) 3)
	   13)))

(ert-deftest wad--http ()
  (should (equal
	   (wad--http "foo" "bar")
	   "https://foo/bar")))

(ert-deftest wad--github-url ()
  (cl-letf ((gnutls-verify-error t))
    (should (equal (wad--github-url "foo/bar")
		   "https://github.com/foo/bar"))))

;;; wad-tests.el ends here
