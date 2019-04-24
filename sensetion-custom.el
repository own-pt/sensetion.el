;;; sensetion.el --- -*- lexical-binding: t; -*-

(defgroup sensetion nil
  "Support for annotating word senses."
  :group 'data)


(defcustom sensetion-output-buffer-name "sensetion"
  "Buffer name where sensetion results are displayed."
  :group 'sensetion
  :type 'string)


(defcustom sensetion-annotation-dir
  (expand-file-name "~/sensetion-data/")
  "Path to annotation directory"
  :group 'sensetion
  :type 'directory)


(defcustom sensetion-db-file-path
  (f-join sensetion-annotation-dir ".sensetion.db")
  "Path to index file."
  :group 'sensetion
  :type 'file)


(defcustom sensetion-annotation-file-type
  "plist"
  "File type (extension) of annotation files."
  :group 'sensetion
  :type 'string)


(defcustom sensetion-number-completions
  15
  "Number of lemma completions to show in `sensetion-annotate'."
  :group 'sensetion
  :type  'integer)


(defcustom sensetion-sense-menu-show-synset-id
  nil
  "Show synset id in sense menu during annotation."
  :group 'sensetion
  :type  'boolean)


(defcustom sensetion-unnanotated-colour
  "salmon"
  "Colour to display the selected tokens in."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-previously-annotated-colour
  "dark green"
  "Colour to display the tokens which have been previously
annotated."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-previously-annotated-unsure-colour
  "light green"
  "Colour to display the tokens which have been previously
annotated with low confidence."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-currently-annotated-colour
  "dark blue"
  "Colour to use in displaying tokens annotated in this batch."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-currently-annotated-unsure-colour
  "light blue"
  "Colour to use in displaying tokens annotated in this batch,
with low confidence."
  :group 'sensetion
  :type 'color)
