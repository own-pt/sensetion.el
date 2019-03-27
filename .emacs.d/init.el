(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

;; ---------------------------------------------------------------
;; copy below this part if you already have MELPA configured

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package exec-path-from-shell :ensure t)

;; If on Mac this might be necessary
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package hydra     :defer t :ensure t)
(use-package flycheck  :defer t :ensure t)
(use-package s         :defer t :ensure t)
(use-package f         :defer t :ensure t)
(use-package trie      :defer t :ensure t)
(use-package async     :defer t :ensure t)

(use-package sensetion
  :commands (sensetion sensetion-annotate sensetion-make-index)
  ;; change path to repository here
  :load-path "~/sensetion.el/"
  :bind (:map sensetion-mode-map
              ;; here you can customize the standard command names; we
              ;; don't like to use '/' to invoke sensetion-edit, so we
              ;; bind it to 'e'
              ("e" . sensetion-edit-sense))
  :hook (sensetion-edit-mode
         ;;  here we can specify modes to be turned on for the raw
         ;;  sentence editing
         . lispy-mode)
  ;; change path to annotation files directory here
  :custom (sensetion-annotation-dir "~/sensetion-data/"))
