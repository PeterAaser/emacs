;;; package --- Anus.
;;; Commentary:

;;; -*- lexical-binding: t; -*-

(defun edit/arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))


;; Where the fuck else would it have ended??
;;; edit.el ends here
