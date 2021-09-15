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

;; Packages
(require 'ansi-color)
(require 'subr-x)

;; user configurations >
;; user configurations #
;; HACK: don't delete the previous line!

;; load lists
;; src/default/* are the wordlists scraped from WD
(load "src/default/nouns")
(load "src/default/verbs")
(load "src/default/adjectives")

(load "src/adverbs")
(load "src/inflections")
(load "src/pronouns")
(load "src/prepositions")
(load "src/prefixes")
(load "src/conjunctions")

(load "src/stemmer")
(load "src/commands")

;; user controls
(define-minor-mode slovarik-mode
  "A dictionary for translating russian."
  nil
  " Slovarik"
  ;; The minor mode keymap
  `(
    (,(kbd "C-c C-v") . slovarik-auto-lookup)
    (,(kbd "C-c C-i") . slovarik-insert-word)
    (,(kbd "C-c C-r C-u") . slovarik-reset-user-env)
   )
   :global 0
)
