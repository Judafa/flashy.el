;;; flashy.el --- Flash cards stored as Org headings  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Julien Dallot

;; Author: Julien Dallot <judafa@protonmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.3"))
;; Keywords: outlines, hypermedia, learning
;; URL: https://github.com/judafa/flashy.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Flashy turns Org headings into flash cards.
;;
;; A card is an Org heading carrying a `FLASHY_CREATED' property.  The
;; heading text is the question; the body of the entry (everything
;; after the property drawer, up to the next heading) is the answer.
;; Review metadata lives in the property drawer:
;;
;;   * What is the capital of Australia?
;;     :PROPERTIES:
;;     :ID:                 6f1c...
;;     :FLASHY_CREATED:     [2026-11-12 Thu 10:00]
;;     :FLASHY_REVIEWS:     3
;;     :FLASHY_SUCCESSES:   2
;;     :FLASHY_SUCCESS_RATE: 0.67
;;     :FLASHY_LAST_REVIEW: [2026-11-12 Thu 18:30]
;;     :FLASHY_LAST_RESULT: success
;;     :END:
;;     Canberra.
;;
;; Usage:
;;
;;   M-x flashy-create-card   turn the heading at point into a card
;;   M-x flashy-ask           review cards from a file (or a card ID)
;;   M-x flashy-ask-id        review the card with a given ID
;;
;; Reviewing happens in the *flashy* buffer, in `flashy-mode':
;;
;;   SPC / RET  reveal the answer (then: next card, in a file session)
;;   y          "I remembered it": record a success
;;   n          "I forgot it": record a failure
;;   e          jump to the card in its Org file
;;   q          quit

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'subr-x)

;;;; Customization

(defgroup flashy nil
  "Flash cards stored as Org headings."
  :group 'org
  :prefix "flashy-")

(defcustom flashy-buffer-name "*flashy*"
  "Name of the buffer used to ask questions."
  :type 'string)

(defcustom flashy-save-after-review t
  "When non-nil, save the card's file after each review."
  :type 'boolean)

(defcustom flashy-select-function #'flashy-select-weighted
  "Function picking the next card from a list of card markers.
It is called with one argument, a non-empty list of markers, and must
return one of them."
  :type '(choice (function-item flashy-select-weighted)
                 (function-item flashy-select-random)
                 (function :tag "Custom function")))

(defface flashy-question-face
  '((t :inherit org-level-1 :weight bold :height 1.3))
  "Face for the question.")

(defface flashy-dim-face
  '((t :inherit shadow))
  "Face for secondary information (path, statistics, hints).")

(defface flashy-key-face
  '((t :inherit help-key-binding))
  "Face for key hints.")

(defface flashy-success-face
  '((t :inherit success :weight bold))
  "Face for a successful recall.")

(defface flashy-failure-face
  '((t :inherit error :weight bold))
  "Face for a failed recall.")

;;;; Card properties

(defconst flashy--prop-created "FLASHY_CREATED")
(defconst flashy--prop-reviews "FLASHY_REVIEWS")
(defconst flashy--prop-successes "FLASHY_SUCCESSES")
(defconst flashy--prop-rate "FLASHY_SUCCESS_RATE")
(defconst flashy--prop-last-review "FLASHY_LAST_REVIEW")
(defconst flashy--prop-last-result "FLASHY_LAST_RESULT")

(defmacro flashy--at-card (marker &rest body)
  "Evaluate BODY with point on the heading of the card at MARKER."
  (declare (indent 1) (debug t))
  `(with-current-buffer (marker-buffer ,marker)
     (org-with-wide-buffer
      (goto-char ,marker)
      (org-back-to-heading t)
      ,@body)))

(defun flashy--timestamp ()
  "Return an inactive Org timestamp for the current time."
  (format-time-string (org-time-stamp-format t t)))

(defun flashy--card-p ()
  "Return non-nil if the heading at point is a flash card."
  (org-entry-get nil flashy--prop-created))

(defun flashy--get-number (prop)
  "Return the numeric value of property PROP at point, or 0."
  (let ((v (org-entry-get nil prop)))
    (if v (string-to-number v) 0)))

(defun flashy--card-question (marker)
  "Return the question (heading text) of the card at MARKER."
  (flashy--at-card marker
    (org-get-heading t t t t)))

(defun flashy--card-path (marker)
  "Return the outline path of the card at MARKER, as a string."
  (flashy--at-card marker
    (let ((path (org-get-outline-path)))
      (concat (file-name-nondirectory
               (or (buffer-file-name) (buffer-name)))
              (when path
                (concat " › " (mapconcat #'identity path " › ")))))))

(defun flashy--card-answer (marker)
  "Return the answer (entry body) of the card at MARKER."
  (flashy--at-card marker
    (let ((end (save-excursion (outline-next-heading) (point)))
          (start (save-excursion (org-end-of-meta-data t) (point))))
      (string-trim (buffer-substring-no-properties (min start end) end)))))

(defun flashy--card-stats (marker)
  "Return a plist of review statistics for the card at MARKER."
  (flashy--at-card marker
    (list :reviews (flashy--get-number flashy--prop-reviews)
          :successes (flashy--get-number flashy--prop-successes)
          :created (org-entry-get nil flashy--prop-created)
          :last-review (org-entry-get nil flashy--prop-last-review)
          :last-result (org-entry-get nil flashy--prop-last-result))))

(defun flashy--record (marker success)
  "Record a review of the card at MARKER.
SUCCESS non-nil means the user remembered the answer."
  (flashy--at-card marker
    (let* ((reviews (1+ (flashy--get-number flashy--prop-reviews)))
           (successes (+ (flashy--get-number flashy--prop-successes)
                         (if success 1 0))))
      (org-entry-put nil flashy--prop-reviews (number-to-string reviews))
      (org-entry-put nil flashy--prop-successes (number-to-string successes))
      (org-entry-put nil flashy--prop-rate
                     (format "%.2f" (/ (float successes) reviews)))
      (org-entry-put nil flashy--prop-last-review (flashy--timestamp))
      (org-entry-put nil flashy--prop-last-result
                     (if success "success" "failure"))))
  (when (and flashy-save-after-review (buffer-file-name (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (save-buffer))))

;;;; Finding cards

(defun flashy--org-buffer (file)
  "Return a buffer visiting FILE, in `org-mode'."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (user-error "Not an Org file: %s" file)))
    buf))

(defun flashy--file-cards (file)
  "Return a list of markers to every card in FILE."
  (with-current-buffer (flashy--org-buffer file)
    (org-with-wide-buffer
     (delq nil (org-map-entries
                (lambda () (when (flashy--card-p) (point-marker))))))))

(defun flashy--id-card (id)
  "Return a marker to the card whose ID property is ID."
  (let ((marker (org-id-find id 'marker)))
    (unless marker
      (user-error "No heading with ID %s" id))
    (unless (flashy--at-card marker (flashy--card-p))
      (user-error "Heading %s is not a flashy card (use `flashy-create-card')"
                  id))
    marker))

;;;; Selection strategies

(defun flashy-select-random (cards)
  "Return a uniformly random element of CARDS."
  (nth (random (length cards)) cards))

(defun flashy--weight (marker)
  "Return the selection weight of the card at MARKER.
New cards weigh 1.0; reviewed cards weigh more the more they were
failed, with a floor so that well-known cards still come back."
  (let* ((stats (flashy--card-stats marker))
         (reviews (plist-get stats :reviews))
         (successes (plist-get stats :successes)))
    (if (zerop reviews)
        1.0
      (+ 0.1 (- 1.0 (/ (float successes) reviews))))))

(defun flashy-select-weighted (cards)
  "Return an element of CARDS, favoring cards that are often failed."
  (let* ((weights (mapcar #'flashy--weight cards))
         (r (* (cl-reduce #'+ weights) (/ (random 1000000) 1000000.0))))
    (catch 'found
      (cl-loop for card in cards
               for w in weights
               do (setq r (- r w))
               when (<= r 0) do (throw 'found card))
      (car (last cards)))))

(defun flashy--pick (cards &optional avoid)
  "Pick a card from CARDS using `flashy-select-function'.
If AVOID is a marker and CARDS has other cards, never return AVOID's
card.  Markers that are not returned are released."
  (let* ((candidates (if (and avoid (cdr cards))
                         (cl-remove-if (lambda (m) (equal m avoid)) cards)
                       cards))
         (choice (funcall flashy-select-function candidates)))
    (dolist (m cards)
      (unless (eq m choice) (set-marker m nil)))
    choice))

;;;; The flashy buffer

(defvar-local flashy--card nil
  "Marker to the card currently asked.")

(defvar-local flashy--source nil
  "File the current session draws cards from, or nil for a single card.")

(defvar-local flashy--state 'question
  "Current state: `question', `answer', or `rated'.")

(defvar-local flashy--last-success nil
  "Non-nil if the last rating in this buffer was a success.")

(defun flashy--fontify-org (text)
  "Return TEXT fontified as in `org-mode'.
Faces are copied to `font-lock-face' so they survive in `flashy-mode'."
  (with-temp-buffer
    (let ((org-inhibit-startup t))
      (delay-mode-hooks (org-mode)))
    (insert text)
    (font-lock-ensure)
    (let ((pos (point-min)))
      (while (< pos (point-max))
        (let ((next (next-single-property-change pos 'face nil (point-max)))
              (face (get-text-property pos 'face)))
          (when face
            (put-text-property pos next 'font-lock-face face))
          (setq pos next))))
    (buffer-string)))

(defun flashy--face (text face)
  "Return TEXT with FACE, robust to font-lock."
  (propertize text 'font-lock-face face 'face face))

(defun flashy--hint (&rest pairs)
  "Insert a line of key hints from PAIRS (KEY DESCRIPTION ...)."
  (insert "\n")
  (while pairs
    (insert (flashy--face (concat "[" (pop pairs) "]") 'flashy-key-face)
            " "
            (flashy--face (pop pairs) 'flashy-dim-face)
            "   "))
  (insert "\n"))

(defun flashy--stats-line (marker)
  "Return a one-line summary of the statistics of the card at MARKER."
  (let* ((s (flashy--card-stats marker))
         (reviews (plist-get s :reviews))
         (successes (plist-get s :successes)))
    (if (zerop reviews)
        (format "New card · created %s" (or (plist-get s :created) "?"))
      (format "Reviews: %d · Success: %d%% · Last: %s (%s)"
              reviews
              (round (* 100.0 (/ (float successes) reviews)))
              (or (plist-get s :last-review) "?")
              (or (plist-get s :last-result) "?")))))

(defun flashy--render ()
  "Redraw the flashy buffer according to its state."
  (let ((inhibit-read-only t)
        (card flashy--card))
    (erase-buffer)
    (insert (flashy--face (flashy--card-path card) 'flashy-dim-face) "\n\n")
    (insert (flashy--face (flashy--card-question card) 'flashy-question-face)
            "\n\n")
    (insert (flashy--face (flashy--stats-line card) 'flashy-dim-face) "\n")
    (insert (flashy--face (make-string 60 ?─) 'flashy-dim-face) "\n")
    (if (eq flashy--state 'question)
        (progn
          (insert "\n" (flashy--face "Try to recall the answer…" 'flashy-dim-face)
                  "\n")
          (flashy--hint "SPC" "show answer" "e" "edit" "q" "quit"))
      (let ((answer (flashy--card-answer card)))
        (insert "\n"
                (if (string-empty-p answer)
                    (flashy--face "(no answer)" 'flashy-dim-face)
                  (flashy--fontify-org answer))
                "\n\n")
        (insert (flashy--face (make-string 60 ?─) 'flashy-dim-face) "\n")
        (if (eq flashy--state 'answer)
            (progn
              (insert "\nDid you remember it?\n")
              (flashy--hint "y" "yes" "n" "no" "e" "edit" "q" "quit"))
          (insert "\n"
                  (if flashy--last-success
                      (flashy--face "✓ Remembered" 'flashy-success-face)
                    (flashy--face "✗ Forgotten" 'flashy-failure-face))
                  "\n")
          (if flashy--source
              (flashy--hint "SPC" "next card" "e" "edit" "q" "quit")
            (flashy--hint "e" "edit" "q" "quit")))))
    (goto-char (point-min))))

(defun flashy--revert (&rest _)
  "Redraw the flashy buffer (used as `revert-buffer-function')."
  (flashy--render))

(defvar flashy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'flashy-continue)
    (define-key map (kbd "RET") #'flashy-continue)
    (define-key map (kbd "y") #'flashy-success)
    (define-key map (kbd "n") #'flashy-failure)
    (define-key map (kbd "e") #'flashy-goto-card)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `flashy-mode'.")

(define-derived-mode flashy-mode special-mode "Flashy"
  "Major mode for reviewing flash cards.

\\{flashy-mode-map}"
  (setq-local revert-buffer-function #'flashy--revert)
  (setq-local truncate-lines nil)
  (visual-line-mode 1))

(defun flashy--ensure-mode ()
  "Signal an error unless in a flashy buffer with a card."
  (unless (and (derived-mode-p 'flashy-mode) flashy--card)
    (user-error "Not in a flashy review buffer")))

(defun flashy--show (card source)
  "Display CARD in the flashy buffer; SOURCE is the session's file or nil."
  (let ((buf (get-buffer-create flashy-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'flashy-mode)
        (flashy-mode))
      (when (and flashy--card (not (eq flashy--card card)))
        (set-marker flashy--card nil))
      (setq flashy--card card
            flashy--source source
            flashy--state 'question
            flashy--last-success nil)
      (flashy--render))
    (pop-to-buffer-same-window buf)))

;;;; Commands

(defun flashy-continue ()
  "Reveal the answer, or go to the next card once the card is rated."
  (interactive)
  (flashy--ensure-mode)
  (pcase flashy--state
    ('question (setq flashy--state 'answer) (flashy--render))
    ('answer (message "Did you remember it? Press y or n"))
    ('rated
     (if flashy--source
         (flashy--show (flashy--pick (flashy--file-cards flashy--source)
                                     flashy--card)
                       flashy--source)
       (quit-window)))))

(defun flashy--rate (success)
  "Rate the current card; SUCCESS non-nil means it was remembered."
  (flashy--ensure-mode)
  (pcase flashy--state
    ('question (user-error "Reveal the answer first (SPC)"))
    ('rated (user-error "This card has already been rated"))
    ('answer
     (flashy--record flashy--card success)
     (setq flashy--state 'rated
           flashy--last-success success)
     (flashy--render))))

(defun flashy-success ()
  "Record that you remembered the current card."
  (interactive)
  (flashy--rate t))

(defun flashy-failure ()
  "Record that you did not remember the current card."
  (interactive)
  (flashy--rate nil))

(defun flashy-goto-card ()
  "Jump to the current card in its Org file."
  (interactive)
  (flashy--ensure-mode)
  (let ((m flashy--card))
    (pop-to-buffer (marker-buffer m))
    (when (or (< m (point-min)) (> m (point-max)))
      (widen))
    (goto-char m)
    (org-back-to-heading t)
    (org-reveal)
    (with-no-warnings
      (if (fboundp 'org-fold-show-entry)
          (org-fold-show-entry)
        (org-show-entry)))))

;;;###autoload
(defun flashy-create-card ()
  "Turn the Org heading at point into a flash card.
The heading is the question, its body is the answer.  An ID is
created if needed so the card can be asked with `flashy-ask-id'."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (save-excursion
    (org-back-to-heading t)
    (when (flashy--card-p)
      (user-error "This heading is already a card"))
    (org-id-get-create)
    (org-entry-put nil flashy--prop-created (flashy--timestamp))
    (org-entry-put nil flashy--prop-reviews "0")
    (org-entry-put nil flashy--prop-successes "0"))
  (message "Flashy card created"))

;;;###autoload
(defun flashy-ask-id (id)
  "Ask the question of the card whose ID property is ID."
  (interactive
   (list (read-string "Card ID: "
                      (and (derived-mode-p 'org-mode)
                           (org-entry-get nil "ID")))))
  (flashy--show (flashy--id-card id) nil))

;;;###autoload
(defun flashy-ask (target)
  "Ask a flash card question from TARGET.
TARGET is either an Org file, in which case a card is drawn from it
with `flashy-select-function' and the session continues with further
cards from that file, or the ID of a card heading, in which case only
that card is asked.

Interactively, read a file, defaulting to the current one."
  (interactive
   (list (read-file-name "Flashy file: " nil
                         (buffer-file-name) t
                         (and (buffer-file-name)
                              (file-name-nondirectory (buffer-file-name))))))
  (if (file-regular-p target)
      (let* ((file (expand-file-name target))
             (cards (flashy--file-cards file)))
        (unless cards
          (user-error "No flashy card in %s (use `flashy-create-card')" file))
        (flashy--show (flashy--pick cards) file))
    (flashy-ask-id target)))

(provide 'flashy)
;;; flashy.el ends here
