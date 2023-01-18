;; * Problems with the org-latex-preview API
;;
;; 1. The overlays have to be placed through a separate function call to
;; `org-latex-preview--ensure-overlay'.
;;
;; 2. `org-latex-preview--colors-at' has to be called separately.
;;
;; 3. `org-latex-preview--hash' uses the default header instead of the one calc
;; provides.
;;
;; 4. Calling `org-latex-preview-create-image-async' means we can't reuse
;; existing images.
;;
;; 5. Supplying the header requires let-binding `org-latex-preview-process-alist'.

(require 'org-latex-preview)

(define-key calc-mode-map (kbd "C-c C-x C-l") #'latex-preview-buffer)
(define-key calc-mode-map (kbd "C-c C-x DEL") #'latex-preview-clear-cache)
(define-key julia-mode-map (kbd "C-c C-x C-l") #'latex-preview-buffer)
(define-key julia-mode-map (kbd "C-c C-x DEL") #'latex-preview-clear-cache)

;; --------------------------------------------
;; * Generic code (all major-modes)

;; This code is a pile of glue, most of which we can eliminate by refactoring
;; org-latex-preview a bit.  The major-mode specific code further below will be
;; up to package authors to provide.

(setq org-latex-preview-auto-generate nil)

(defvar latex-preview-header
  (concat
   (org-latex-make-preamble
    (org-combine-plists
     (org-export-get-environment (org-export-get-backend 'latex))
     '(:time-stamp-file nil))
    org-latex-preview-header 'snippet)
   "\\usepackage[fleqn]{amsmath}\n\\usepackage{amssymb}\n"))

(defvar latex-preview-fragment-finder-alist
  '((calc-mode . calc-preview--get-fragment-info)
    (julia-mode . prog-preview--get-fragment-info)))

(defun latex-preview-buffer (fragment-finder-func &optional arg)
  "Preview latex fragments in buffer.

FRAGMENT-FINDER-FUNC is the major-mode-specific parser that
gathers up fragment data."
  (interactive (list (or (alist-get major-mode latex-preview-fragment-finder-alist)
                         (user-error "No latex preview method for %S" major-mode))
                     current-prefix-arg))
  (if arg
      (mapc #'delete-overlay (overlays-in (point-min) (point-max)))
    (let* ((org-latex-preview-process-alist
            `((dvisvgm
               :programs ("latex" "dvisvgm")
               :description "dvi > svg"
               :message "you need to install the programs: latex and dvisvgm."
               :image-input-type "dvi"
               :image-output-type "svg"
               :image-size-adjust (1.4 . 1.2)
               :latex-compiler ("%l -interaction nonstopmode -output-directory %o %f")
               :latex-precompiler ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
               :image-converter ("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview --scale=%S -o %B-%%9p.svg %f")
               :latex-header ,latex-preview-header)))
           (fragments-info (latex-preview--prepare-fragments fragment-finder-func))
           (new-fragments-info
            (cl-loop for fragment in fragments-info
                     for hash = (plist-get fragment :key)
                     for ov = (plist-get fragment :overlay)
                     for path-info = (org-latex-preview--get-cached hash)
                     if path-info
                     do (org-latex-preview--update-overlay ov path-info)
                     else
                     collect fragment)))
      ;; (message "%S" org-latex-preview-process-alist)
      (when new-fragments-info
        (org-latex-preview-create-image-async 'dvisvgm new-fragments-info)))))

(defun latex-preview--prepare-fragments (fragment-finder-func)
  "Create a list of plists of fragments info as required by org-latex-preview.

FRAGMENT-FINDER-FUNC is the major-mode-specific parser function
that gathers up fragment data."
  (pcase-let* ((`(,fragment-positions ,fragment-strings) (funcall fragment-finder-func))
               (preview-overlays (mapcar (lambda (pos-beg-and-end)
                                           (apply #'org-latex-preview--ensure-overlay pos-beg-and-end))
                                         fragment-positions)))
    (cl-loop for string in fragment-strings
             for ov     in preview-overlays
             for positions in fragment-positions
             for (fg bg) = (org-latex-preview--colors-at (car positions))
             for string-hash = (org-latex-preview--hash 'dvisvgm string "svg" fg bg)
             collect (list :string (concat  string)
                           :overlay ov
                           :key string-hash))))

(defun latex-preview-clear-cache (fragment-finder-func)
  "TODO"
  (interactive (list (or (alist-get major-mode latex-preview-fragment-finder-alist)
                         (user-error "No latex preview method for %S" major-mode))))
  (pcase-let* ((`(,fragment-positions ,fragment-strings) (funcall fragment-finder-func)))
    (cl-loop for string in fragment-strings
             for positions in fragment-positions
             for (fg bg) = (org-latex-preview--colors-at (car positions))
             for string-hash = (org-latex-preview--hash 'dvisvgm string "svg" fg bg)
             do (org-latex-preview--remove-cached string-hash)))
  (mapc #'delete-overlay (overlays-in (point-min) (point-max))))

;; --------------------------------------------
;; * Mode-specific code

;; Each major-mode must supply a function that gathers up latex fragment
;; positions and contents for that mode and feeds it to latex-preview. The
;; prog-mode version is simple when limited to single-line math fragments, the
;; calc-mode version is... not. (Unless you enforce the latex-display mode in
;; calc.)

;; ** PROG-MODE PREVIEWS (BUT ONLY TESTED IN JULIA-MODE)

;; \begin{...}...\end{...} not supported yet
(defvar prog-preview--latex-delimiters
  '(("\\[" . "\\]")
    ("\\(" . "\\\\)")))

(defun prog-preview--get-fragment-info ()
  "Parse a prog-mode buffer for latex fragments."
  (save-excursion
    (goto-char (point-min))
    (let (positions fragments-strings)
      (while (re-search-forward org-latex-preview--tentative-math-re nil t)
        ;; Ensure we're in a comment
        (when-let* (((nth 4 (syntax-ppss)))
                    (start (match-beginning 0))
                    (end-delimiter (alist-get (match-string 0)
                                              prog-preview--latex-delimiters
                                              nil nil #'equal))
                    (end (save-excursion
                           (when (and (re-search-forward end-delimiter nil t)
                                      (nth 4 (syntax-ppss)))
                             (point)))))
          (push (list start end) positions)
          (push (buffer-substring-no-properties start end)
                fragments-strings)))
      (list (nreverse positions) (nreverse fragments-strings)))))

;; ** CALC-MODE

(defun calc-preview--at-prompt-p ()
  (string= "." (string-trim (buffer-substring
                             (line-beginning-position)
                             (line-end-position)))))

;; Kludge city here, be warned
(defun calc-preview--get-fragment-info ()
  "Parse calc buffer for latex fragments.

Return a list of (list of positions) and (list of fragment
strings)."
  (when (and (string= "*Calculator*" (buffer-name))
             calc-line-numbering)
    (let ((p (point))
          positions fragment-strings)
      (goto-char (point-min))

      ;; Run 1, capture positions
      (while (not (eobp))
        (unless (calc-preview--at-prompt-p)
          (let* ((expr-start (if calc-line-numbering
                                 (+ (line-beginning-position) 2)
                               (line-beginning-position)))
                 (expr-end))
            (when (looking-at "^[0-9]+:")
              (forward-line 1)
              (while (and (not (calc-preview--at-prompt-p))
                          (not (looking-at "^[0-9]+:")))
                (forward-line 1))
              (forward-line -1))
            (setq expr-end (line-end-position))
            (push (list expr-start expr-end) positions)))
        (forward-line 1))

      ;; Run 2, capture strings
      (let ((disp-lang calc-language))
        (calc-set-language 'latex nil)
        (goto-char (point-min))
        (while (not (eobp))
          (unless (calc-preview--at-prompt-p)
            (let* ((expr-start (if calc-line-numbering
                                   (+ (line-beginning-position) 2)
                                 (line-beginning-position)))
                   (expr-end (line-end-position))
                   (string (buffer-substring expr-start expr-end)))
              (push (concat "\\begin{equation*}\n" string "\n\\end{equation*}")
                    fragment-strings)))
          (forward-line 1))
        (calc-set-language disp-lang))

      (goto-char p)
      (list (nreverse positions) (nreverse fragment-strings)))))

(provide 'latex-preview)
