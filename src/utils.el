(defun load-files-in-directory(dir)
  "It loads all the files in a given directory. Useful for tests loading"
  (interactive "D")
  ;; remove / from the dir name
  (let ((dir (string-remove-suffix "/" dir)))
    (message (format "%s"
		     (mapcar
		      (lambda(fn) nil nil (if (and (not (equal fn "."))
						   (not (equal fn ".."))
						   (not (string-suffix-p "~" fn)))
					      ;; load file
					      (load (format "%s/%s" dir fn))
					    ;; else noop
					    nil))
		      ;; files in directory
		      (directory-files dir))))) )
