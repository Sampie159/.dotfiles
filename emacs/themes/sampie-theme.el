;;; sampie-theme.el --- Sampie's theme follows the idea of "minimal" highlighting

;; Author: Sampie159 <Sampie159@users.noreply.github.com>
;; Version: 0.1
;; Filename: sampie-theme.el
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/Sampie159/sampie-theme
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Sampie's theme is based on the idea of Jon Blow's theme, that is
;; with "minimal" hightlighting, this means that hightlighting only happens
;; in certain situations, such as type definitions, builtin keywords, variable
;; declaration, etc...

;; This file structured is based on Nick Aversano's naysayer theme
;; https://github.com/nickav/naysayer-theme.el

;;; Code:

(unless (>= emacs-major-version 24)
  (error "Sampie theme requires Emacs 24 or later!"))

(deftheme sampie "Sampie color theme.")

(let ((background     "#c0c0c0")
      (gutters        "#c0c0c0")
      (gutter-fg      "#c0c0c0")
      (gutters-active "#c0c0c0")
      (builtin        "#be29ec")
      (selection      "#0000ff")
      (text           "#333333")
      (comments       "#004c4c")
      (punctuation    "#03396c")
      (keywords       "#008080")
      (variables      "#420420")
      (functions      "#ffffff")
      (methods        "#420420")
      (strings        "#ac1ee7")
      (constants      "#ff084a")
      (macros         "#03396c")
      (numbers        "#ff084a")
      (white          "#ffffff")
      (error          "#cc0000")
      (warning        "#b3ecec")
      (highlight-line "#b3ecec")
      (line-fg        "#126367"))
  
  (custom-theme-set-faces
   'sampie

   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,text :background ,background, :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background ,text                        ))))
   `(fringe                           ((t (:background ,background   :foreground ,white))))
   `(linum                            ((t (:background ,background :foreground ,gutter-fg))))
   `(highlight                        ((t (:foreground nil :background ,selection))))

   ;; Font lock faces
   ;; *****************************************************************************

   `(font-lock-keyword-face           ((t (:foreground ,keywords))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments))))
   `(font-lock-doc-face               ((t (:foreground ,comments))))
   `(font-lock-function-name-face     ((t (:foreground ,functions))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings))))
   `(font-lock-preprocessor-face      ((t (:foreground ,macros))))
   `(font-lock-warning-face           ((t (:foreground ,warning))))

   ;; Plugins
   ;; *****************************************************************************
   `(trailing-whitespace ((t (:foreground nil :background ,warning))))
   `(whitespace-trailing ((t (:background nil :foreground ,warning :inverse-video t))))

   `(linum ((t (:foreground ,line-fg :background ,background))))
   `(linum-relative-current-face ((t (:foreground ,white :background ,background))))
   `(line-number ((t (:foreground ,line-fg :background ,background))))
   `(line-number-current-line ((t (:foreground ,white :background ,background))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,highlight-line))))
   `(hl-line-face ((t (:background ,highlight-line))))

   ;; which-func
   `(which-func ((t (:inverse-video unspecified
                                    :underline unspecified
                                    :foreground ,background
                                    :weight bold
                                    :box nil))))

   ;; mode-line and powerline
   `(mode-line-buffer-id ((t (:foreground ,background :distant-foreground ,text :text ,text :weight bold))))
   `(mode-line ((t (:inverse-video unspecified
                                   :underline unspecified
                                   :foreground ,background
                                   :background ,text
                                   :box nil))))
   `(powerline-active1 ((t (:background ,text :foreground ,background))))
   `(powerline-active2 ((t (:background ,text :foreground ,background))))

   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
                                            :foreground ,text
                                            :background ,background
                                            :box nil))))
   `(powerline-inactive1 ((t (:background ,background :foreground ,text))))
   `(powerline-inactive2 ((t (:background ,background :foreground ,text)))))

  (custom-theme-set-variables
    'sampie
    '(linum-format " %5i ")))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************

(provide-theme 'sampie)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'sampie-theme)

;;; sampie-theme.el ends here
