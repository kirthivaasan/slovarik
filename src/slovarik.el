;; Copyright (c) 2020 kirthip

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'ansi-color)
(require 'subr-x)

;; load lists
(load "nouns")
(load "verbs")
(load "inflections")
(load "adverbs")
(load "adjectives")
(load "pronouns")
(load "prepositions")
(load "prefixes")
(load "conjunctions")

(load "stemmer")

;; user controls
(define-minor-mode slovarik-mode
  "A dictionary for translating russian."
  nil
  " Slovarik"
  ;; The minor mode keymap
  `(
    (,(kbd "C-c C-v") . slovarik-auto-lookup)
   )
   :global 0
)

(defun normalize-e (word)
  (subst-char-in-string ?ё ?е word))

(defun slovarik-auto-lookup ()
  (interactive)
  (message (concat "Searching for " (thing-at-point 'word)))
  (user-lookup (thing-at-point 'word)))

(setq slovarik--buffer "*slovarik*")

(defun print-elements-of-list (l title)
  (setq n 1)
    (with-current-buffer slovarik--buffer (insert (concat "\n" (propertize (upcase title) 'font-lock-face '(:background "white" :foreground "black")) "\n" (make-string (length title) ?=))))
    (while l
      (setq elem (car l))
      (with-current-buffer slovarik--buffer (insert (concat "\n" (number-to-string n) ") " (propertize (elt elem 0) 'font-lock-face 'bold))))
      (with-current-buffer slovarik--buffer (insert (concat "\n   " (propertize (elt elem 1) 'font-lock-face 'italic) "\n")))
      (setq n (1+ n))
      (setq l (cdr l))))

(defun print-list (l title)
  (if l (print-elements-of-list l title)))

(defun stem-to-word-length-ratio (stemword word) t)

(defun pristavki-analysis (word)
  (setq prefix (aref prefixes (get-prefix word)))
  (setq rest-of-word (remove-n-frombeg word (length prefix)))
  (setq i 0)
  (setq results '())
  (while (< i (- (length slovarik-prefixes) 1))
    (setq i (1+ i))
    (setq w (aref slovarik-prefixes i))
    (if (string= word w)
	(setq results (cons (list w i) results))))
  results)


(defun slovarik-find-words(word wordlist)
  (setq word (normalize-e word))
  (setq i 0)
  (setq stemmed (if (>= (length (stem word)) 3) (stem word)))
  (setq results '())
  (while (< i (- (length wordlist) 1))
    (setq i (1+ i))
    (setq w (aref wordlist i))
    ;; don't forget to normalize the lookup word (e: -> e)? or (e -> e:)
    ;; 100 is arbitrary. we probably don't want more than a hundred hits for a target
    (if (and (< (length results) 100)
	     (or (string= word w)
		 (string-match-p word w)
		 (if stemmed (string-match-p stemmed w))))
	(setq results (cons i results))))
  results)

(defun slovarik-lookup (word wordlist deflist)
  (let* ((search-res (slovarik-find-words word wordlist))
	 (words-res (mapcar (lambda (i) (list (aref wordlist i) i)) search-res))
	 (topresults (seq-take (sort words-res (lambda (w i) (length w))) 3)))
    (mapcar (lambda (w) (list (car w) (aref deflist (elt w 1)))) topresults)))

;; search with the wordlist, but use the otherlist for resulting keys
(defun slovarik-lookup-with-otherlist (word wordlist deflist otherlist)
  (let* ((search-res (slovarik-find-words word wordlist))
	 (words-res (mapcar (lambda (i) (list (aref otherlist i) i)) search-res))
	 (topresults (seq-take (sort words-res (lambda (w i) (length w))) 5)))
    (mapcar (lambda (w) (list (car w) (aref deflist (elt w 1)))) topresults)))

(defun user-lookup (word)
  (if (is-cyrillic-word word)
  (let ((noun-hits (slovarik-lookup word slovarik-nouns slovarik-nouns-defs))
	(verb-hits (slovarik-lookup word slovarik-verbs slovarik-verbs-defs))
	(infl-hits (slovarik-lookup-with-otherlist word slovarik-inflections slovarik-verbs-defs slovarik-verbs))
	(adj-hits (slovarik-lookup word slovarik-adjectives slovarik-adjectives-defs))
	(adverb-hits (slovarik-lookup word slovarik-adverbs slovarik-adverbs-defs))
	(pronoun-hits (slovarik-lookup word slovarik-pronouns slovarik-pronouns-defs))
	(conjunction-hits (slovarik-lookup word slovarik-conjunctions slovarik-conjunctions-defs))
	(prep-hits (slovarik-lookup word slovarik-prepositions slovarik-prepositions-defs)))
    (with-output-to-temp-buffer slovarik--buffer
      (print-list noun-hits "[noun]")
      (print-list verb-hits "[verb]")
      (print-list infl-hits "[inflection]")
      (print-list adverb-hits "[adverb]")
      (print-list pronoun-hits "[pronoun]")
      (print-list conjunction-hits "[conjunction]")
      (print-list adj-hits "[adjective]")
      (print-list prep-hits "[preposition]")
      ))
  (message "Not russian word")
  ))
