(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

(add-to-list 'auto-mode-alist '("\\.text\\" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\" . markdown-mode))

(setq load-path (append '("/home/mikelin2/.emacs.d/polymode/" "/home/mikelin2/.emacs.d/polymode/modes") load-path))

(require 'poly-R) 
(require 'poly-markdown)

(eval-after-load 'ess-site 
  '(progn 
     (add-to-list 'auto-mode-alist '("\\.[rR]md" . poly-markdown+r-mode))
     (add-to-list 'auto-mode-alist '("\\.[rR]nw" . poly-noweb+r-mode))
     (add-to-list 'auto-mode-alist '("\\.R\\'" . r-mode))))
;; consider adding other R modes to this list, if they don't work right?

(ess-toggle-underscore nil)
