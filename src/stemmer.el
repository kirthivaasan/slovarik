;; snowball stemmer for russian
;; http://snowball.tartarus.org/algorithms/russian/stemmer.html

(require 'cl-extra)
(require 'cl)
;;45 are dashes, ?ё=1105
;; todo add other encodings (e.g. windows 1251)
(defun is-cyr-char (c) (or (and (>= c 1072) (<= c 1105)) (= c 45) (= c 32)))
(defun is-cyrillic-word (word)
  (seq-reduce (lambda (acc c) (and acc (is-cyr-char c))) (downcase word) t))

;; maybe use a char-table instead?
(setq alphabet "абвгдежзийклмнопрстуфхцчшщъыьэюяѐё")
(setq vowels   "1000010010000010000100000001011111")
;(setq vowels (bool-vector t nil ...)

(defun is-vowel (c)
  (if (and (is-cyr-char c) (not (= c 45)) (not (= c 32)))
      (eq (aref "1" 0) (aref vowels (- c 1072)))))

(setq perfective-gerund-g1 ["вшись" "вши" "в"])
(setq perfective-gerund-g2 ["ывшись" "ившись" "ывши" "ивши" "ив" "ыв"])


;; (setq adjective ["ими" "ыми" "ему" "ому" "его" "ого" "ее" "ие" "ое" "ые" "ей" "ий" "ый" "ой" "ем" "им" "ым" "ом" "их" "ых" "ую" "юю" "ая" "яя" "ою" "ею"])

(setq participle-g1 ["ем" "нн" "вш" "ющ" "щ"])
(setq participle-g2 ["ивш" "ывш" "ующ"])

;; manually compiled list of all  possible adjective endings in wiktionary
(setq adjective ["аемое" "аемый" "яемое" "яемый" "аннее" "анное" "анные" "анней" "анний" "анный" "анной" "анном" "анных" "анная" "анною" "янный" "янной" "янная" "авший" "явший" "ающее" "ающие" "ающий" "ающая" "яющее" "яющий" "яющая" "ившее" "ивший" "ывший" "ующее" "ующие" "ующей" "ующий" "ующим" "ующая" "ующею" "емого" "нному" "ннего" "нного" "ящему" "ящего" "вшего" "емое" "емей" "емый" "емой" "ннее" "нное" "нные" "нней" "нний" "нный" "нной" "нным" "нном" "нних" "нных" "нную" "нная" "нняя" "нною" "вшее" "вший" "вшая" "ющее" "ющие" "ющей" "ющий" "ющим" "ющая" "ющею" "ащее" "ащий" "ащем" "ящее" "ящей" "ящий" "ящем" "ящая" "ящею" "щему" "щего" "щее" "щие" "щей" "щий" "щем" "щим" "щая" "щею" "ими" "ыми" "ему" "ому" "его" "ого" "ее" "ие" "ое" "ые" "ей" "ий" "ый" "ой" "ем" "им" "ым" "ом" "их" "ых" "ую" "юю" "ая" "яя" "ою" "ею"])

;; any g1 must follow either а or я
(setq reflexive ["ся" "сь"])

(setq verb-g1 ["ете" "йте" "ешь" "нно" "ла" "на" "ли" "й" "л" "ем" "н" "ло" "но" "ет" "ют" "ны" "ть"])
(setq verb-g2 ["уйте" "ейте" "ила" "ыла" "ена" "ите" "или" "ыли" "ило" "ыло" "ено" "ует" "уют" "ены" "ить" "ыть" "ишь" "ит" "ыт" "ят" "ей" "уй" "ил" "ыл" "им" "ым" "ен" "ую" "ю"])

(setq noun ["иями" "ком" "ями" "ами" "ией" "иям" "иях" "ием" "еи" "ии" "ей" "ой" "ий" "ям" "ам" "ье" "ем" "ях" "ах" "ев" "ие" "ию" "ью" "ия" "ья" "ов" "ом" "й" "и" "е" "у" "о" "ю" "ь" "ы" "а" "я"])

(setq superlative ["ейше" "ейш"])

(setq derivational ["ость" "ост"])

;; an adjectival ending is an adjectival ending optionally preceded by a participle ending
(defun rv-search (l)
(dotimes (i (length l))
  (when (is-vowel (aref l i))
    (return i))))

(defun rv (l)
  (let ((idx (rv-search l)))
    (if idx idx (length l))))

(defun r1-search (l)
  (let ((si (rv l)))
    (dotimes (i (length l))
      (when (and (not (is-vowel (aref l i))) (> i si))
	(return i)))))

(defun r1 (l)
  (let ((idx (r1-search l)))
    (if idx idx (length l))))

(defun r2-search (l)
  (let ((si (r1 l)))
    (dotimes (i (length l))
      (when (and (is-vowel (aref l i)) (> i si))
	(return i)))))

(defun r2 (l)
  (let ((idx (r2-search l)))
    (if idx idx (length l))))

(defun has-ending (word suffixes)
  (cl-some (lambda (suffix) (string-suffix-p suffix word)) suffixes))

;; checks if word has one of the endings in the list l and returns index if found or nil
(defun which-ending (word l)
  (dotimes (i (length l))
    (when (string-suffix-p (aref l i) word)
      (return i))))

(defun which-ending-g1 (word l)
  (dotimes (i (length l))
    (when (or (string-suffix-p (concat "а" (aref l i)) word)
	      (string-suffix-p (concat "я" (aref l i)) word))
      (return i))))

(defun has-perfective-gerund-ending (word)
   (or (which-ending-g1 word perfective-gerund-g1)
       (which-ending word perfecive-gerund-g2)))

(defun has-verb-ending (word)
   (or (which-ending-g1 word verb-g1)
       (which-ending word verb-g2)))

;; truncates a string by n
(defun remove-n-fromend (word n)
  (substring word 0 (- (length word) n)))

(defun remove-ending (word get-ending)
  (remove-n-fromend word (length (funcall get-ending word))))

;; remove pge = perfective gerund ending
(defun get-pge-end (word)
  (let ((i1 (which-ending-g1 word perfective-gerund-g1))
	(i2 (which-ending word perfective-gerund-g2)))
    (cond (i1 (aref perfective-gerund-g1 i1))
	  (i2 (aref perfective-gerund-g2 i2)))))
(defun remove-pge (word)
  (remove-ending word 'get-pge-end))

;; remove reflexive ending
(defun get-reflex-end (word)
  (let ((i (which-ending word reflexive)))
    (cond (i (aref reflexive i)))))

(defun remove-reflex (word)
  (remove-ending word 'get-reflex-end))

;; remove adjective ending
(defun get-adj-end (word)
  (let ((i (which-ending word adjective)))
    (cond (i (aref adjective i)))))

(defun remove-adj (word)
  (remove-ending word 'get-adj-end))

;; remove verb ending
(defun get-verb-end (word)
  (let ((i1 (which-ending-g1 word verb-g1))
	(i2 (which-ending word verb-g2)))
    (cond (i1 (aref verb-g1 i1))
	  (i2 (aref verb-g2 i2)))))

(defun remove-verb (word)
  (remove-ending word 'get-verb-end))

;; remove noun ending
(defun get-noun-end (word)
  (let ((i (which-ending word noun)))
    (cond (i (aref noun i)))))

(defun remove-noun (word)
  (remove-ending word 'get-noun-end))

(defun remove-i (word)
  (if (string-suffix-p "и" word) (remove-n-fromend word 1) word))

(defun get-derivational (word)
  (let ((i (which-ending word derivational)))
    (cond (i (aref derivational i)))))

(defun remove-derivational (word)
    (if (r2-search word) (remove-ending word 'get-derivational) word))

(defun undouble-n (word)
  (if (string-suffix-p "нн" word) (remove-n-fromend word 1) word))

(defun get-super-end (word)
  (let ((i (which-ending word superlative)))
    (cond (i (aref superlative i)))))

(defun remove-super (word)
  (remove-ending word 'get-super-end))

(defun remove-myakki-znak (word)
  (if (string-suffix-p "ь" word) (remove-n-fromend word 1) word))

;; 1) Search for a PERFECTIVE GERUND ending.
;; If one is found remove it, and that is then the end of step 1.
;; Otherwise try and remove a REFLEXIVE ending,
;; and then search in turn for
;; (1) an ADJECTIVAL, (2) a VERB or (3) a NOUN ending.
;; As soon as one of the endings (1) to (3) is found remove it, and terminate step 1.

;; Step 2: If the word ends with и (i), remove it.

;; Step 3: Search for a DERIVATIONAL ending in R2 (i.e. the entire ending must lie in R2),
;; and if one is found, remove it.

;; Step 4: (1) Undouble н (n), or, (2) if the word ends with a SUPERLATIVE ending,
;; remove it and undouble н (n), or (3) if the word ends ь (') (soft sign) remove it.

(defun part1 (word)
  (let ((word (remove-reflex word)) (l (length word)))
    (let ((noun-stem-length (length (remove-noun word)))
	  (verb-stem-length (length (remove-verb word)))
	  (adj-stem-length (length (remove-adj word))))
      (if (< noun-stem-length verb-stem-length)
	  (if (< noun-stem-length adj-stem-length) (remove-noun word) (remove-adj word))
	(if (< verb-stem-length adj-stem-length) (remove-verb word) (remove-adj word))))))

(defun step1 (word)
  (if (< (length (remove-pge word)) (length word)) (remove-pge word)
    (let ((p (part1 word)))
      (if p p word))))

(defun step2 (word)
  (remove-i word))

(defun step3 (word)
  (remove-derivational word))

(defun step4 (word)
  (if (string-suffix-p "ь" word) (remove-n-fromend word 1)
      (undouble-n (remove-super word))))

(defun stem (word)
  (let ((word (downcase (string-trim word)))) (step4 (step3 (step2 (step1 word))))))


;; DEPREFIXER
(defun remove-n-frombeg (word n)
  (substring word n (length word)))

(setq prefixes ["четверо" "четырёх" "электро" "германо" "противо" "псевдо" "русско" "внутри" "экстра" "ультра" "мульти" "взаимо" "франко" "анархо" "гипер" "гекто" "черес" "через" "благо" "йокто" "милли" "зепто" "между" "интер" "турбо" "Санкт" "после" "техно" "видео" "радио" "супер" "инфра" "сверх" "микро" "гидро" "прото" "макро" "много" "едино" "аудио" "англо" "контр" "кибер" "квази" "тетра" "кино" "чрез" "мото" "пона" "низо" "недо" "деци" "аэро" "гига" "мега" "кило" "пост" "двух" "фото" "трёх" "теле" "мини" "вице" "одно" "себе" "само" "авто" "архи" "евро" "афро" "нано" "полу" "анти" "авиа" "дека" "разо" "подо" "надо" "пред" "пере" "роз" "экс" "еже" "наи" "все" "зоо" "гео" "меж" "кое" "нео" "три" "тре" "дву" "суб" "сов" "рос" "уни" "вне" "пол" "пра" "цис" "рас" "ото" "обо" "нис" "изо" "взо" "вос" "бес" "раз" "про" "при" "пре" "под" "низ" "без" "воз" "над" "су" "па" "ди" "не" "ко" "мг" "мк" "со" "об" "ис" "вс" "вз" "во" "по" "от" "вы" "из" "за" "до" "на" "не" "да" "у" "о" "в" "с" "д" "г" "с" "м"])

(defun get-prefix (word)
  (dotimes (i (length prefixes))
    (when (string-prefix-p (aref prefixes i) word)
      (return i))))

(defun remove-prefix (word)
  (remove-n-frombeg word (length (aref prefixes (get-prefix word)))))
