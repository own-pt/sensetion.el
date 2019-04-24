;;; sensetion.el --- -*- lexical-binding: t; -*-

(require 'emacsql)
(require 'emacsql-sqlite)

(defvar db (emacsql-sqlite "~/.sensetion.db"))

(defun sensetion--sql-setup-wn (db)
  (emacsql db
	   [:create-table synsets
	    ([(id object :primary-key) gloss])])
  (emacsql db [:create-table lemmas
	       ([(lemma object :primary-key) pos synset-ids sense-keys sent-ids tk-ids])]))


(defun sensetion--sql-index-wn (db synsets)
  ;; TODO: FIXME
  (emacsql db [:insert
	       :into synsets
	       :values (["14696793-n" '("stone" "rock") '("stone1" "rock1") "material consisting of the aggregate of minerals like those making up the Earth's crust"]
			["09416076-n" '("stone" "rock") '("stone2" "rock2") "a lump or mass of hard consolidated mineral matter"])])
  (emacsql db [:insert
	       :into lemmas
	       :values (["stone" "n" '("14696793-n" "09416076-n") '("stone1" "stone2") '(1) '(3)]
			["rock" "n" '("14696793-n" "09416076-n") '("rock1" "rock2") '(2) '(4)])]))


(defun sensetion-sql-setup-corpus (db)
  (emacsql db))


(defun sensetion--sql-index-sents (db sents)
  (dolist (s sents)
    (emacsql db [:insert
		 :into corpus
		 :values (["1" '("oi" "tchau" "bla" "rock")]
			  ["2" '("tchau" "oi" "blabla" "patati" "stone")])])))

