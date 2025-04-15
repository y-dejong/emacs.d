;; Add melpa as a source for packages, and define ysd-require which
;; installs a package if it is missing

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-native-compile t)

(defun ysd-require (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE with MIN-VERSION.
  If NO-REFRESH is nil, `package-refresh-contents' is called."
  (unless (package-installed-p package min-version)
	(message "Missing package: %s" package)
	(unless (or (assoc package package-archive-contents) no-refresh)
	  (package-refresh-contents))
	(package-install package)
	(push package package-selected-packages))
  (require package))

;; Add site-lisp for manually installed packages
(let ((default-directory (locate-user-emacs-file "site-lisp")))
  (normal-top-level-add-subdirs-to-load-path))

(provide 'init-packages)
