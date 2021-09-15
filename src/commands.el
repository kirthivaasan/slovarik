
;; ;; Copyright (c) 2020 kirthip

;; ;; This program is free software: you can redistribute it and/or modify
;; ;; it under the terms of the GNU General Public License as published by
;; ;; the Free Software Foundation, either version 3 of the License, or
;; ;; (at your option) any later version.

;; ;; This program is distributed in the hope that it will be useful,
;; ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; ;; GNU General Public License for more details.

;; ;; You should have received a copy of the GNU General Public License
;; ;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; ;; Packages
;; (require 'ansi-color)
;; (require 'subr-x)

;; ;; user configurations >
;; ;; user configurations #
;; ;; HACK: don't delete the previous line!

;; ;; load lists
;; ;; src/default/* are the wordlists scraped from WD
;; (load "default/nouns")
;; (load "default/verbs")
;; (load "default/adjectives")

;; (load "adverbs")
;; (load "inflections")
;; (load "pronouns")
;; (load "prepositions")
;; (load "prefixes")
;; (load "conjunctions")

;; (load "stemmer")

;; ;; user controls
;; (define-minor-mode slovarik-mode
;;   "A dictionary for translating russian."
;;   nil
;;   " Slovarik"
;;   ;; The minor mode keymap
;;   `(
;;     (,(kbd "C-c C-v") . slovarik-auto-lookup)
;;     (,(kbd "C-c C-i") . slovarik-insert-word)
;;     (,(kbd "C-c C-r C-u") . slovarik-reset-user-env)
;;    )
;;    :global 0
;; )

(defun normalize (word)
  (remove-stress-symbol (subst-char-in-string ?ё ?е word)))

(defun slovarik-auto-lookup ()
  (interactive)
  (message (concat "Searching for " (thing-at-point 'word)))
  (user-lookup (thing-at-point 'word)))

(defun slovarik-user-lookup ()
  (interactive)
  (setq user-query (read-string "Enter word: "))
  (message (concat "Searching for " user-query))
  (user-lookup user-query))

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
  (setq word (normalize word))
  (setq i 0)
  (setq stemmed (if (>= (length (stem word)) 3) (stem word)))
  (setq results '())
  (while (< i (length wordlist) )
    (setq w (aref wordlist i))
    ;; 100 is arbitrary. we probably don't want more than a hundred hits for a target
    (if (and (< (length results) 100)
	     (or (string= word w)
		 (string-match-p word w)
		 (if stemmed (string-match-p stemmed w))))
	(setq results (cons i results)))
    (setq i (1+ i)))
  results)

(defun slovarik-lookup-list (word wordlist deflist)
  (let* ((search-res (slovarik-find-words word wordlist))
	 (words-res (mapcar (lambda (i) (list (aref wordlist i) i)) search-res))
	 (topresults (seq-take (sort words-res (lambda (w i) (length w))) 3)))
    (mapcar (lambda (w) (list (car w) (aref deflist (elt w 1)))) topresults)))

;; search with the wordlist, but use the otherlist for resulting keys
(defun slovarik-lookup-list-with-otherlist (word wordlist deflist otherlist)
  (let* ((search-res (slovarik-find-words word wordlist))
	 (words-res (mapcar (lambda (i) (list (aref otherlist i) i)) search-res))
	 (topresults (seq-take (sort words-res (lambda (w i) (length w))) 5)))
    (mapcar (lambda (w) (list (car w) (aref deflist (elt w 1)))) topresults)))

;; TODO: fix bug on slovarik-user-nouns by commenting and uncommenting those lines each time the user
;;       env is reset and set, respectively
(defun user-lookup (word)
  (if (is-cyrillic-word (normalize word))
      (let* ((word (normalize word))
	     (noun-hits (slovarik-lookup-list word slovarik-nouns slovarik-nouns-defs))
	     (user-noun-hits (slovarik-lookup-list word slovarik-user-nouns slovarik-user-nouns-defs))	     
	     (verb-hits (slovarik-lookup-list word slovarik-verbs slovarik-verbs-defs))
	     (user-verb-hits (slovarik-lookup-list word slovarik-user-verbs slovarik-user-verbs-defs))	     
	     (infl-hits (slovarik-lookup-list-with-otherlist word slovarik-inflections slovarik-verbs-defs slovarik-verbs))
	     (adj-hits (slovarik-lookup-list word slovarik-adjectives slovarik-adjectives-defs))
	     (user-adj-hits (slovarik-lookup-list word slovarik-user-adjectives slovarik-user-adjectives-defs))
	     (adverb-hits (slovarik-lookup-list word slovarik-adverbs slovarik-adverbs-defs))
	     (pronoun-hits (slovarik-lookup-list word slovarik-pronouns slovarik-pronouns-defs))
	     (conjunction-hits (slovarik-lookup-list word slovarik-conjunctions slovarik-conjunctions-defs))
	     (prep-hits (slovarik-lookup-list word slovarik-prepositions slovarik-prepositions-defs)))
	(with-output-to-temp-buffer slovarik--buffer
	  (print-list noun-hits "[NOUN]")
	  (print-list user-noun-hits "[NOUN]")
	  (print-list verb-hits "[verb]")
	  (print-list user-verb-hits "[verb]")	  
	  (print-list infl-hits "[verbal inflection]")
	  (print-list adverb-hits "[adverb]")
	  (print-list pronoun-hits "[pronoun]")
	  (print-list conjunction-hits "[conjunction]")
	  (print-list adj-hits "[adjective]")
	  (print-list user-adj-hits "[adjective]")
	  (print-list prep-hits "[preposition]")
	  ))
    (message "Not russian word"))
    )


;; punctuation
(setq punctuation ["." "," "!" "'" "\"" ":" "?" "«" "»" "—" "*" "/" "+" "#" "@"])

(defun add-spaces-to-punctuation (text)
  (seq-reduce (lambda (clean_text c) (replace-in-string c (concat " " c " ") clean_text))  punctuation text))

(defun remove-spaces-from-punctuation (text)
  (seq-reduce (lambda (clean_text c) (replace-in-string (concat " " c) c clean_text))  punctuation text))


; tokenizes highlighted region
(defun tokenize-text-by-spaces (text)
  (split-string (add-spaces-to-punctuation text)))

(defun slovarik-tokenize-region (x y)
  (interactive "r")
  (print (tokenize-text-by-spaces (buffer-substring-no-properties x y))))

; applies function to text region, token by token
(defun apply-func-text (func text)
  (remove-spaces-from-punctuation (mapconcat 'identity (seq-map func (tokenize-text-by-spaces text)) " ")))


;; russian romanization
;; https://en.wikipedia.org/wiki/Romanization_of_Russian (passport 2013 ICAO convention)
(setq roman-table ["a" "b" "v" "g" "d" "e" "zh" "z" "i" "i" "k" "l" "m" "n" "o" "p" "r" "s" "t" "u" "f" "kh" "ts" "ch" "sh" "shch" "ie" "y" "'" "e" "iu" "ia" "e" "e" "-"])

;; this logic has to be fixed. a dash is not a cyr-char although it appears in russian words
(defun cyr-to-roman (c)
  (if (is-cyr-char c)
    (if (= c 45) "-"
      (aref roman-table (- c 1072)))
    c))

(defun romanize-word (word)
  (if (is-cyrillic-word (remove-stress-symbol word))
      (let ((norm-word (remove-stress-symbol (downcase word))))
	(seq-reduce
	 (lambda (acc c) (concat acc (cyr-to-roman c))) norm-word ""))
    word))

(defun slovarik-romanize ()
  (interactive)
  (message (romanize-word (thing-at-point 'word))))

(defun slovarik-romanize-text (text)
  (apply-func-text 'romanize-word text))

(defun slovarik-romanize-region (x y)
  (interactive "r")
  (with-output-to-temp-buffer slovarik--buffer
    (print (slovarik-romanize-text (buffer-substring-no-properties x y)))))


;; todo
;; define allowable chars for tts and filter out all others that don't fit/substitute them
(defun remove-newlines (text)
  (remove-punctuation (replace-in-string "\n" " " text)))

(defun remove-punctuation (text)
  (replace-in-string "]" " " (replace-in-string "[" " " (replace-in-string ";" "," text))))

; Aleksandr/Anna/Artemiy/Elena/Irina
(setq rhvoice-voice-name "Aleksandr")

(defun slovarik-rhvoice-tts (x y)
  (interactive "r")
  (with-output-to-temp-buffer "slovarik-rhvoice-temp-buffer"
    (let ((cmd (concat "echo «" (remove-newlines (buffer-substring-no-properties x y)) "»|RHVoice-test -p " rhvoice-voice-name)))
      (shell-command cmd))))

(setq slovarik-opus-server-path "~/.emacs.d/eval_these/slovarik/opus-server/query.py") ; path to opus server query.py

(defun query-word-sentences (word lang)
  (with-output-to-temp-buffer slovarik--buffer
    (print (shell-command-to-string (concat "python3 " slovarik-opus-server-path " ru " "\"" word "\"")))))

(defun slovarik-get-sentences ()
  (interactive)
  (query-word-sentences (remove-stress-symbol (thing-at-point 'word)) "ru"))

(defun slovarik-get-en-sentences (x y)
  (interactive "r")
  (query-word-sentences (concat "\"" (remove-punctuation (remove-stress-symbol (buffer-substring-no-properties x y))) "\"") "en"))

;; helpers of slovarik-insert-word >
(defun slovarik-set-home()
  "Sets slovarik home."
    (if (not (boundp 'slovarik-home))
       (setq slovarik-home
	     (string-remove-suffix "/" (read-file-name "Insert the path to slovarik: "))))
  (message (concat "slovarik-home=" slovarik-home)) )

(defun slovarik-insert-string-at-word (dest-file string offset-word count)
  "Opens DEST-FILE and inserts STRING to the line next to the COUNT-th match of OFFSET-WORD.

DEST-FILE - file to insert the string
STRING - string to be inserted
OFFSET-WORD - searched string specifying together with COUNT the insertion offset
COUNT - match count"
  (find-file dest-file)
  (beginning-of-buffer)
  (search-forward offset-word nil nil count)
  (move-beginning-of-line nil)
  (open-line 1)
  (insert string)
  (save-buffer)
  (kill-buffer)
  )

(defun slovarik-init-user-config-file(home-path)
  "Writes slovarik-home variable in /src/user/conf.el.

HOME-PATH - slovarik home"
  (let* ((conf-fn "conf")
	 (conf-buf (generate-new-buffer conf-fn))
	 (full-path-to-user-conf (concat home-path "/src/user/" conf-fn ".el")))
      ;; insert slovarik-home to src/user/conf.el
      (set-buffer conf-buf)
      (insert (format "(setq slovarik-home \"%s\")\n" home-path))
      (write-file full-path-to-user-conf)
      (kill-buffer conf-buf)
      ;; insert (load src/user/conf.el) to slovarik.el
      (slovarik-insert-string-at-word
       (concat home-path "/slovarik.el")
       (format "(load \"%s\")" full-path-to-user-conf)
       ";; user configurations #" 1)
      )
  )

(defun slovarik-init-user-wordlist(home-path wordlist-id)
  "Initialise the user's wordlist as specified by WORDLIST-ID.

Preconditions: 
                * HOME-PATH/src/user/conf.el exists
HOME-PATH - slovarik home
WORDLIST-ID - wordlist identifier to be initialised. Identifiers resolve to either of the following:
            * n - HOME-PATH/src/user/nouns.el
            * a - HOME-PATH/src/user/adjectives.el
            * v - HOME-PATH/src/user/verbs.el"
  (let (( full-wordlist-id
	  (cond ((equal wordlist-id "n") "nouns")
		((equal wordlist-id "v") "verbs")
		((equal wordlist-id "a") "adjectives")
		(t nil)) ))
    (if (equal full-wordlist-id nil)
	(user-error "Choose a valid wordlist identifier: n/a/v")
      (progn
	(message (format "Wordlist \"%s\" selected" full-wordlist-id))
	(let* ((empty-list-format "[\n\t\t\t]")
	       (full-path-to-wordlist (concat home-path "/src/user/" full-wordlist-id ".el"))
	       (wordlist-buf (generate-new-buffer full-wordlist-id))
	       (conf-fn "conf")
	       (conf-buf (generate-new-buffer conf-fn)))

	      ;; add empty lists to wordlist file
	      (set-buffer wordlist-buf) 
	      (insert (format "(setq slovarik-user-%s %s)\n\n" full-wordlist-id empty-list-format))
	      (insert (format "(setq slovarik-user-%s-defs %s)" full-wordlist-id empty-list-format))
	      (write-file full-path-to-wordlist)
	      (kill-buffer wordlist-buf)
	      ;; add load smt to src/user/conf.el
	      (set-buffer conf-buf)
	      (insert (format "\n(load \"%s\")" full-path-to-wordlist))
	      (append-to-file nil nil (concat home-path "/src/user/" conf-fn ".el"))
	      (kill-buffer conf-buf)
	      (message (format "User wordlist %s initialised" full-path-to-wordlist)) ) ) ) ) )

(defun slovarik-init-user-env()
  "Initialise the user environment in HOME-PATH/src/user.

HOME-PATH - slovarik home"
  (slovarik-set-home)
  (if (not (file-exists-p (concat slovarik-home "/src/user")))
      (progn
	(make-directory (concat slovarik-home "/src/user") t)
	(slovarik-init-user-config-file slovarik-home)
	(slovarik-init-user-wordlist slovarik-home "n")
	(slovarik-init-user-wordlist slovarik-home "v")
	(slovarik-init-user-wordlist slovarik-home "a")
	(load (format "%s/src/user/conf.el" slovarik-home))
	(message (format "User environment correctly setup in %s/src/user" slovarik-home)))) )
;; helpers of slovarik-insert-word #

(defun slovarik-insert-word ()
  "Insert <word> into <slovarik-home>/src/user/<wordlist>.el, where <word> and <wordlist> are prompted."
  (interactive)
  (slovarik-init-user-env)
  (setq word (read-string "Insert word: "))
  (setq def (read-string "Insert definition: "))
  
  (setq word-type (read-string "Insert word type (Aa/Nn/Vv): "))
  (cond
   ((or (equal word-type "A") (equal word-type "a"))
    (setq wordlist "adjectives"))
   ((or (equal word-type "N") (equal word-type "n"))
    (setq wordlist "nouns"))
   ((or (equal word-type "V") (equal word-type "v"))
    (setq wordlist "verbs"))
   (t
    (user-error "%s is not a valid word type, please insert Aa/Nn/Vv" word-type))
   )
  (slovarik-set-home)
  (setq wordlist (concat slovarik-home "/src/user/" wordlist ".el"))
  (slovarik-insert-string-at-word wordlist (concat "\"" word "\"") "])" 1)
  (slovarik-insert-string-at-word wordlist (concat "\"" def "\"") "])" 2)
  (load wordlist)
  (message "The word %s has been successfully stored in %s" word wordlist)
  )

(defun slovarik-reset-user-env()
  "Deletes the user environment."
  (interactive)
  (if (y-or-n-p "Deleting the user environment, continue?")
      (if (boundp 'slovarik-home)
	  (progn
	    (let ((path-to-user (format "%s/src/user" slovarik-home)))
	      (if (file-exists-p path-to-user)
		  (progn
		    ;; delete user's directory
		    (delete-directory  path-to-user t)
		    (message (format "%s deleted" path-to-user)))
		(message (format "%s doesn't exist" path-to-user)))
	      
	      (condition-case nil
		  (progn
		    ;; delete stm load user's configuration
		    (find-file (format "%s/slovarik.el" slovarik-home))
		    (beginning-of-buffer)
		    (search-forward (format "(load \"%s/conf.el\")" path-to-user))
		    (beginning-of-line)
		    (kill-line)
		    (kill-line)    
		    (save-buffer)
		    (kill-buffer)
		    (message (format "Stm (load \"%s/conf.el\") removed from slovarik.el" path-to-user)))
		(error
		 (progn
		   (message (format "Stm (load \"%s/conf.el\") not found in slovarik.el" path-to-user))
		   (kill-buffer))
		 )))
    	    (makunbound 'slovarik-home)
	    (message "slovarik-home unset")
	    (message "User env successfully reset")
	    ;; TODO: add here function to erase content of user's wordlists
	    )
	(message "Impossible to delete user environment: slovarik-home not set"))
    (message "User environment reset cancelled")))
