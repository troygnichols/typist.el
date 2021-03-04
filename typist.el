;;; typist.el --- This is a typing tutor based on speed-type.el and inspired by typelit.io

;;; Commentary:

;; - smallest steps possible...
;; 1) read text into a new buffer
;;
;; don't worry about concept of books or WPM stats for now
;;
;; Features to consider eventually
;; read a book and keep place
;; count WPM (maybe multiple passes through same page, keep history, compare progress)
;; convert PDF/ebook formats to text buffer
;; browse project gutenberg, or static list of avail books
;; loading screen, pick from list of books, like ibooks

;;; Code:


(defvar-local tgn-typist--orig-text nil)
(defvar-local tgn-typist--entries 0)
(defvar-local tgn-typist--errors 0)
(defvar-local tgn-typist--remaining 0)
(defvar-local tgn-typist--mod-str nil)

(defface tgn-typist-correct
  '((t :foreground "green"))
  "Face for correctly typed characters."
  :group 'tgn-typist)

(defface tgn-typist-mistake
  '((t :foreground "red" :underline "red"))
  "Face for incorrectly typed characters"
  :group 'tgn-typist)

(defface tgn-typist-main
  '((t :family "Georgia"))
  "Face for setting the font"
  :group 'tgn-typist)

(defun tgn-typist--trim (str)
  "Trim leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any "\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun tgn-typist-start (text)
  "Start a new typist buffer with contents of given TEXT."

  (with-temp-buffer
    (insert text)
    (delete-trailing-whitespace)
    (setq text (tgn-typist--trim (buffer-string))))
  (let ((buf (generate-new-buffer "typist"))
        (len (length text)))
    (set-buffer buf)
    (buffer-face-set 'tgn-typist-main)
    (face-remap-add-relative 'default :family "PT Mono" :height 160)
    (setq tgn-typist--orig-text text)
    (setq tgn-typist--mod-str (make-string len 0))
    (setq tgn-typist--remaining len)
    (insert text)
    (set-buffer-modified-p nil)
    (switch-to-buffer buf)
    (with-no-warnings (goto-line 0))
    (add-hook 'after-change-functions 'tgn-typist--change nil t)
    (add-hook 'first-change-hook 'tgn-typist--first-change nil t)
    )
  )

(defun tgn-typist--first-change ()
  "Start the timer. TODO: implement timer."
  )

(defun tgn-typist--handle-del (start end)
  "Keep track of the statistics when a deletion occurs between START and END. TODO: implement stats."
  (delete-region start end)
  )

(defun tgn-typist--change (start end length)
  "Handle buffer change.

START beginning index of the region that just changed.
END ending index of the region that just changed
LENGTH lenght of the region that just changed

Make  sure the contents don't actually change, but rather the contents are color
coded to show what has been typed already, which characters are mistakes and
which are correct."
  (let ((len (length tgn-typist--orig-text)))
    (when (<= start len)
      (let* ((end (if (> end (1+ len)) len end))
             (length (if (> (+ start length) len) (1+ (- len start)) length))
             (start0 (1- start))
             (end0 (1- end))
             (new-text (buffer-substring start end))
             (old-text (substring tgn-typist--orig-text
                                  start0 (+ start0 length)))
             (orig (substring tgn-typist--orig-text start0 end0)))
        (tgn-typist--handle-del start end)
        (insert old-text) ;; <-- inserts old text when deleting
        (tgn-typist--diff orig new-text start end)
        (goto-char end)
        (when (= tgn-typist--remaining 0)
          (tgn-typist--handle-complete))))))

(defun tgn-typist--handle-complete ()
  "Remove typing hooks from the buffer and (TODO: implement stats) print statistics."
  (remove-hook 'after-change-functions 'tgn-typist--change)
  (remove-hook 'first-change-hook 'tgn-typist--first-change)
  )

(defun tgn-typist--diff (orig new start end)
  "Update stats and buffer contents with result of change in text.

ORIG section of the original text (probably 1 char) under edit, before the edit
NEW section of the original text (probably 1 char) ) under edit, after the edit
START where in the buffer the change starts
END where in the buffer the change ends (usually START + 1)"
  (let ((start0 (1- start))
        (end0 (1- end))
        (correct nil))
    (dotimes (i (- end start) nil)
      (let ((pos0 (+ start0 i))
            (pos (+ start i)))
        (if (tgn-typist--check-same i orig new)
            (progn (setq correct t)
                   (store-substring tgn-typist--mod-str pos0 1))
          (progn (cl-incf tgn-typist--errors)
                 (store-substring tgn-typist--mod-str pos0 2)
                 (delete-char 1)
                 (insert (substring new i (1+ i)))))
        (cl-incf tgn-typist--entries)
        (cl-incf tgn-typist--remaining)
        (add-text-properties pos (1+ pos)
                             `(face ,(if correct
                                         'tgn-typist-correct
                                       'tgn-typist-mistake)))))))

(defun tgn-typist--check-same (pos a b)
  "Return non-nil if both A[POS] B[POS] are whitespace or if they are the same."
  (let ((q (aref a pos))
        (p (aref b pos)))
    (or (and (= (char-syntax p) ?\s)
             (= (char-syntax q) ?\s))
        (= p q))))



(tgn-typist-start "It was the best of times. It was the worst of times.")



;;; typist.el ends here
