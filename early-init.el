;; Increase garbage collection threshold significantly, then reduce afterwards
(let ((normal-gc-cons-threshold (* 100 1024 1024)) ;; Max # of bytes to use in memory
      (normal-gc-cons-percentage .3) ;; Max percentage of memory to use
      (init-gc-cons-threshold most-positive-fixnum) ;; Max # of bytes to use in memory during startup
      (init-gc-cons-percentage .6)) ;; Max percentage of memory to use during startup

    (setq gc-cons-threshold init-gc-cons-threshold
          gc-cons-percentage init-gc-cons-percentage)
    (add-hook 'emacs-startup-hook
              `(lambda ()
                (setq gc-cons-threshold ,normal-gc-cons-threshold
                      gc-cons-percentage ,normal-gc-cons-percentage))))
