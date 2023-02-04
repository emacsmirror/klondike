;;; klondike.el --- Klondike                   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Jean Libète

;; Author: Jean Libète <tomenzgg@mail.mayfirst.org>
;; URL: https://codeberg.org/tomenzgg/Emacs-Klondike
;; Package-Requires: ((emacs "28.1"))
;; Version: 1.0
;; Keywords: game, rpg

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Sterf

;;; Code:
;; cl-case
;; cl-evenp
(require 'cl-lib)
;; pcase
(require 'pcase)
;; if-let*
(require 'subr-x)
;; run-at-time
(require 'timer)

(defconst klondike----buffer-name "Klondike"
  "The name of the buffer the Klondike solitaire game always runs in.")

(defvar klondike----mode-line-status ""
  "")

(defcustom klondike----window-padding 2
  ""
  :type  'natnum
  :group 'klondike)

(defcustom klondike----card-width  11
  ""
  :type  'natnum
  :group 'klondike)
(defcustom klondike----card-height 11
  ""
  :type  'natnum
  :group 'klondike)

(defcustom klondike----top-&-bottom-row-spacing 4
  ""
  :type  'natnum
  :group 'klondike)

(defvar klondike----facedown-stack `(() . ())
  "")
(defvar klondike----faceup-stack   `(() . ())
  "")

(defvar klondike----empty-0-stack  `(() . ())
  "")
(defvar klondike----empty-1-stack  `(() . ())
  "")
(defvar klondike----empty-2-stack  `(() . ())
  "")
(defvar klondike----empty-3-stack  `(() . ())
  "")

(defvar klondike----pile-0-stack  `(() . ())
  "")
(defvar klondike----pile-1-stack  `(() . ())
  "")
(defvar klondike----pile-2-stack  `(() . ())
  "")
(defvar klondike----pile-3-stack  `(() . ())
  "")
(defvar klondike----pile-4-stack  `(() . ())
  "")
(defvar klondike----pile-5-stack  `(() . ())
  "")
(defvar klondike----pile-6-stack  `(() . ())
  "")

(defun klondike--stack-get (stack-type stack-num)
  ""

  (cl-case stack-type
    ('faceup klondike----faceup-stack)
    ('pile   (cl-case stack-num
               (0 klondike----pile-0-stack)
               (1 klondike----pile-1-stack)
               (2 klondike----pile-2-stack)
               (3 klondike----pile-3-stack)
               (4 klondike----pile-4-stack)
               (5 klondike----pile-5-stack)
               (6 klondike----pile-6-stack)))
    ('empty  (cl-case stack-num
               (0 klondike----empty-0-stack)
               (1 klondike----empty-1-stack)
               (2 klondike----empty-2-stack)
               (3 klondike----empty-3-stack)))))
(defmacro klondike--stack-set (stack cards visible-num x y)
  ""

  `(setq ,stack (cons (cons ,cards ,visible-num) (cons ,x ,y))))
(defun klondike--stack-get-cards (stack)
  ""

  (caar stack))
(defun klondike--stack-set-cards (stack cards)
  ""

  (setcar (car stack) cards))
(defun klondike--stack-get-visible (stack)
  ""

  (cdar stack))
(defun klondike--stack-set-visible (stack visible-num)
  ""

  (let ((l (length (klondike--stack-get-cards stack))))
    (setcdr (car stack) (if (> visible-num l) l visible-num))))
(defun klondike--stack-get-x (stack)
  ""

  (cadr stack))
(defun klondike--stack-get-y (stack)
  ""

  (cddr stack))

(defvar klondike----history '(() . -1)
  "")
(defun klondike--history-get-timeline ()
  ""

  (car klondike----history))
(defun klondike--history-get-timeline-current ()
  ""

  (nth (klondike--history-get-index) (klondike--history-get-timeline)))
(defun klondike--history-get-index ()
  ""

  (cdr klondike----history))
(defun klondike--history-set-index (index)
  ""

  (setcdr klondike----history index))
(defun klondike--history-save ()
  ""

  (let ((timeline (klondike--history-get-timeline))
        (index    (klondike--history-get-index)))
    (setq klondike----history (cons (append (butlast timeline (- (1- (length timeline)) index))
                                            `(((:facedown . ,(copy-tree klondike----facedown-stack))
                                               (:faceup   . ,(copy-tree klondike----faceup-stack))
                                               (:empty0   . ,(copy-tree klondike----empty-0-stack))
                                               (:empty1   . ,(copy-tree klondike----empty-1-stack))
                                               (:empty2   . ,(copy-tree klondike----empty-2-stack))
                                               (:empty3   . ,(copy-tree klondike----empty-3-stack))
                                               (:pile0    . ,(copy-tree klondike----pile-0-stack))
                                               (:pile1    . ,(copy-tree klondike----pile-1-stack))
                                               (:pile2    . ,(copy-tree klondike----pile-2-stack))
                                               (:pile3    . ,(copy-tree klondike----pile-3-stack))
                                               (:pile4    . ,(copy-tree klondike----pile-4-stack))
                                               (:pile5    . ,(copy-tree klondike----pile-5-stack))
                                               (:pile6    . ,(copy-tree klondike----pile-6-stack)))))
                                    (1+ index)))))
(defun klondike--history-alter (index)
  ""

  (klondike--history-set-index index)

  (let ((current (klondike--history-get-timeline-current)))
    (setq klondike----facedown-stack (copy-tree (alist-get :facedown current)))
    (setq klondike----faceup-stack   (copy-tree (alist-get :faceup   current)))
    (setq klondike----empty-0-stack  (copy-tree (alist-get :empty0   current)))
    (setq klondike----empty-1-stack  (copy-tree (alist-get :empty1   current)))
    (setq klondike----empty-2-stack  (copy-tree (alist-get :empty2   current)))
    (setq klondike----empty-3-stack  (copy-tree (alist-get :empty3   current)))
    (setq klondike----pile-0-stack   (copy-tree (alist-get :pile0    current)))
    (setq klondike----pile-1-stack   (copy-tree (alist-get :pile1    current)))
    (setq klondike----pile-2-stack   (copy-tree (alist-get :pile2    current)))
    (setq klondike----pile-3-stack   (copy-tree (alist-get :pile3    current)))
    (setq klondike----pile-4-stack   (copy-tree (alist-get :pile4    current)))
    (setq klondike----pile-5-stack   (copy-tree (alist-get :pile5    current)))
    (setq klondike----pile-6-stack   (copy-tree (alist-get :pile6    current))))

  (klondike--card-insert-all))
(defun klondike-history-prev ()
  ""
  (interactive)

  (if (zerop (klondike--history-get-index))
      (message "No more undo!")
    (klondike--history-alter (1- (klondike--history-get-index)))))
(defun klondike-history-next ()
  ""
  (interactive)

  (if (= (klondike--history-get-index)
         (1- (length (klondike--history-get-timeline))))
      (message "No more redo!")
    (klondike--history-alter (1+ (klondike--history-get-index)))))

(defconst klondike----suits-icon-spade   "♠"
  "")
(defconst klondike----suits-icon-heart   "♥"
  "")
(defconst klondike----suits-icon-diamond "♦"
  "")
(defconst klondike----suits-icon-club    "♣"
  "")

(defconst klondike----card-values '("A" "2" "3"  "4" "5" "6"
                                    "7" "8" "9" "10" "J" "Q" "K")
  "")
(defun klondike--card-create (suit-symbol value)
  ""

  `((:suit . ,(pcase suit-symbol
                ((or 'spade 'club)    (cl-case suit-symbol
                                        ('spade klondike----suits-icon-spade)
                                        ('club  klondike----suits-icon-club)))
                ((or 'heart 'diamond) (propertize (cl-case suit-symbol
                                                    ('heart   klondike----suits-icon-heart)
                                                    ('diamond klondike----suits-icon-diamond))
                                                  'face
                                                  '(:foreground "red")))))
    (:value . ,(let ((v (if (numberp value) (number-to-string value) value)))
                 (pcase suit-symbol
                   ((or 'spade 'club)    v)
                   ((or 'heart 'diamond) (propertize v 'face '(:foreground "red"))))))))
(defun klondike--card-get-suit (card)
  ""

  (alist-get :suit card))
(defun klondike--card-get-value (card)
  ""

  (alist-get :value card))
(defun klondike--card-next-p (card1 card2 to-empty-p)
  ""

  (let ((next (lambda (c ascending-p)
                (let ((mem (member (klondike--card-get-value c)
                                   klondike----card-values)))
                  (if ascending-p
                      (if-let ((c (cadr mem))) c (car klondike----card-values))
                    (let* ((len (length klondike----card-values))
                           (n   (- len (length mem))))
                      (nth (1- (if (zerop n) len n)) klondike----card-values)))))))
    (or (and to-empty-p       (not card2) (string= (klondike--card-get-value card1) "A"))
        (and (not to-empty-p) (not card2) (string= (klondike--card-get-value card1) "K"))
        (and (string= (klondike--card-get-value card2) (funcall next card1 (not to-empty-p)))
             (if to-empty-p
                 (string= (klondike--card-get-suit  card1) (klondike--card-get-suit card2))
               (or (and (or (string= (klondike--card-get-suit card1) klondike----suits-icon-club)
                            (string= (klondike--card-get-suit card1) klondike----suits-icon-spade))
                        (or (string= (klondike--card-get-suit card2) klondike----suits-icon-heart)
                            (string= (klondike--card-get-suit card2) klondike----suits-icon-diamond)))
                   (and (or (string= (klondike--card-get-suit card2) klondike----suits-icon-club)
                            (string= (klondike--card-get-suit card2) klondike----suits-icon-spade))
                        (or (string= (klondike--card-get-suit card1) klondike----suits-icon-heart)
                            (string= (klondike--card-get-suit card1) klondike----suits-icon-diamond)))))))))
(defun klondike--card-to-unicode (card)
  ""

  (pcase (klondike--card-get-suit card)
    ('nil                                            "🂠")
    ((pred (string= klondike----suits-icon-club))    (cl-case (intern (klondike--card-get-value card))
                                                       (  'A "🃑")
                                                       ( '\2 "🃒")
                                                       ( '\3 "🃓")
                                                       ( '\4 "🃔")
                                                       ( '\5 "🃕")
                                                       ( '\6 "🃖")
                                                       ( '\7 "🃗")
                                                       ( '\8 "🃘")
                                                       ( '\9 "🃙")
                                                       ('\10 "🃚")
                                                       (  'J "🃛")
                                                       (  'Q "🃝")
                                                       (  'K "🃞")))
    ((pred (string= klondike----suits-icon-heart))   (propertize (cl-case (intern (klondike--card-get-value card))
                                                                   (  'A "🂱")
                                                                   ( '\2 "🂲")
                                                                   ( '\3 "🂳")
                                                                   ( '\4 "🂴")
                                                                   ( '\5 "🂵")
                                                                   ( '\6 "🂶")
                                                                   ( '\7 "🂷")
                                                                   ( '\8 "🂸")
                                                                   ( '\9 "🂹")
                                                                   ('\10 "🂺")
                                                                   (  'J "🂻")
                                                                   (  'Q "🂽")
                                                                   (  'K "🂾"))
                                                                 'face
                                                                 '(:foreground "red")))
    ((pred (string= klondike----suits-icon-spade))   (cl-case (intern (klondike--card-get-value card))
                                                       (  'A "🂡")
                                                       ( '\2 "🂢")
                                                       ( '\3 "🂣")
                                                       ( '\4 "🂤")
                                                       ( '\5 "🂥")
                                                       ( '\6 "🂦")
                                                       ( '\7 "🂧")
                                                       ( '\8 "🂨")
                                                       ( '\9 "🂩")
                                                       ('\10 "🂪")
                                                       (  'J "🂫")
                                                       (  'Q "🂭")
                                                       (  'K "🂮")))
    ((pred (string= klondike----suits-icon-diamond)) (propertize (cl-case (intern (klondike--card-get-value card))
                                                                   (  'A "🃁")
                                                                   ( '\2 "🃂")
                                                                   ( '\3 "🃃")
                                                                   ( '\4 "🃄")
                                                                   ( '\5 "🃅")
                                                                   ( '\6 "🃆")
                                                                   ( '\7 "🃇")
                                                                   ( '\8 "🃈")
                                                                   ( '\9 "🃉")
                                                                   ('\10 "🃊")
                                                                   (  'J "🃋")
                                                                   (  'Q "🃍")
                                                                   (  'K "🃎"))
                                                                 'face
                                                                 '(:foreground "red")))))



(defun klondike--card-insert (x y empty-p &optional facedown-p   total-num
                                                    faceup-cards show-stack-p)
  ""

  (read-only-mode 0)

  (let* ((delete-reg (lambda ()
                       (delete-region (point) (+ (point)
                                                 klondike----card-width))))
         (move-to    (lambda (w z)
                       (goto-line      z)
                       (move-to-column w)

                       (funcall delete-reg))))
    (funcall move-to x (1+ y))
    (insert " "
            (mapconcat (lambda (num)
                         (if (and (cl-evenp num) empty-p) " " "_"))
                       (number-sequence 1 (- klondike----card-width 2)))
            " ")



    (let* ((cardHeightW/oTopBot (- klondike----card-height 2))
           (totalN              (if total-num total-num 0))
           (faceups             (if (and (not show-stack-p) (> (length faceup-cards) 0))
                                    (list (car faceup-cards))
                                  faceup-cards))
           (numOfFacedownCards  (if show-stack-p (- totalN
                                                    (length faceups)) 0)))
      (dotimes (i numOfFacedownCards)
        (funcall move-to x (+ y 1 (1+ i)))
        (insert "|"
                (mapconcat (lambda (num)
                             (if (cl-evenp num) " " "\\"))
                           (number-sequence 1 (- klondike----card-width 2)))
                "|"))



      (if faceups
          (let ((faceupRev (reverse faceups)))
            (dotimes (faceupIndex (length faceups))
              (funcall move-to x (+ y 1 numOfFacedownCards (1+ faceupIndex)))
              (insert (string-replace " "
                                      (if (and (zerop numOfFacedownCards) (= faceupIndex 0)) " " "‾")
                                      (format (concat "|%-2s%"
                                                      (number-to-string
                                                        (- klondike----card-width 4))
                                                      "s|")
                                              (klondike--card-get-value
                                                (nth faceupIndex faceupRev))
                                              "")))))
        (funcall move-to x (+ y 1 (1+ numOfFacedownCards)))
        (insert "|" (make-string (- klondike----card-width 2)
                                 (if (zerop totalN) ?  ?‾))    "|"))



      (let ((rows (- cardHeightW/oTopBot 2)))
        (dotimes (offset rows)
          (funcall move-to x (+ y
                                1
                                numOfFacedownCards
                                (if faceups (length faceups) 1)
                                (1+ offset)))
          (let ((top (= offset (1- (/ rows 2))))
                (mid (= offset     (/ rows 2)))
                (bot (= offset (1+ (/ rows 2)))))
            (if (and (not empty-p) facedown-p (or top mid bot))
                (insert (cond
                         (top "| _       |")
                         (mid "|/ `/|// /|")
                         (bot "|_;/ |/_/ |")))
              (let* ((widthMinus (- klondike----card-width 2))
                     (widthHalf  (/ widthMinus 2))
                     (widthRest  (- widthMinus widthHalf 1)))
                (insert (if (and empty-p (cl-evenp offset)) " " "|")
                        (make-string widthRest ? )
                        (if (and faceups (= offset (/ rows 2)))
                            (klondike--card-get-suit (car faceups))
                          " ")
                        (make-string widthHalf ? )
                        (if (and empty-p (cl-evenp offset)) " " "|"))))))



        (funcall move-to x (+ y
                              1
                              numOfFacedownCards
                              (if faceups (length faceups) 1)
                              (1+ rows)))
        (insert (format (concat "|%"
                                (number-to-string (- klondike----card-width 4))
                                "s%2s|")
                        ""
                        (if faceups
                            (klondike--card-get-value (car faceups))
                          "")))



        (funcall move-to x (+ y
                              1
                              numOfFacedownCards
                              (if faceups (length faceups) 1)
                              (1+ rows)
                              1))
        (insert (if empty-p " ‾ ‾ ‾ ‾ ‾ " " ‾‾‾‾‾‾‾‾‾ "))



        (when show-stack-p
          (dotimes (offset 10)
            (funcall move-to x (+ y
                                  1
                                  numOfFacedownCards
                                  (if faceups (length faceups) 1)
                                  (1+ rows)
                                  1
                                  (1+ offset)))
            (insert "           "))))))

  (read-only-mode t)
  (goto-line      0)
  (move-to-column 1)

  (setq klondike----mode-line-status (concat " "
                                             (klondike--card-to-unicode
                                               (car (klondike--stack-get-cards
                                                      klondike----empty-0-stack)))
                                             " "
                                             (klondike--card-to-unicode
                                               (car (klondike--stack-get-cards
                                                      klondike----empty-1-stack)))
                                             " "
                                             (klondike--card-to-unicode
                                               (car (klondike--stack-get-cards
                                                      klondike----empty-2-stack)))
                                             " "
                                             (klondike--card-to-unicode
                                               (car (klondike--stack-get-cards
                                                      klondike----empty-3-stack)))
                                             "  ")))
(defun klondike--card-insert-all (&optional stacks-to-print)
  ""

  (let ((current (klondike--history-get-timeline-current)))
    (mapc (lambda (toPrintSymbol)
            (let ((stack (alist-get toPrintSymbol current)))
              (cl-case toPrintSymbol
                (:facedown (klondike--card-insert (klondike--stack-get-x stack)
                                                  (klondike--stack-get-y stack)
                                                  (zerop (length (klondike--stack-get-cards stack)))
                                                  t))
                (:faceup   (klondike--card-insert (klondike--stack-get-x stack)
                                                  (klondike--stack-get-y stack)
                                                  (zerop (length (klondike--stack-get-cards stack)))
                                                  nil
                                                  (length (klondike--stack-get-cards stack))
                                                  (klondike--stack-get-cards stack)))
                (t         (klondike--card-insert (klondike--stack-get-x stack)
                                                  (klondike--stack-get-y stack)
                                                  (zerop (length (klondike--stack-get-cards stack)))
                                                  nil
                                                  (length (klondike--stack-get-cards stack))
                                                  (butlast (klondike--stack-get-cards stack)
                                                           (- (length (klondike--stack-get-cards stack))
                                                              (klondike--stack-get-visible stack)))
                                                  (let ((str (symbol-name toPrintSymbol)))
                                                    (string= "pile"
                                                             (substring str 1 (1- (length str))))))))))
          (if stacks-to-print stacks-to-print (mapcar 'car current)))))
(defun klondike--stack-number (stack)
  ""

  (read-only-mode 0)

  (let ((totalNum (length (klondike--stack-get-cards stack))))
    (dotimes (stackIndex (klondike--stack-get-visible stack))
      (goto-line      (+ 1 (klondike--stack-get-y stack) (- totalNum stackIndex)))
      (move-to-column (+ (klondike--stack-get-x stack) (- klondike----card-width 3)))

      (delete-region (point) (+ (point) 2))

      (let ((result (number-to-string (1+ stackIndex))))
        (insert (concat (if (= (length result) 1)
                            (if (= (1+ stackIndex) totalNum) " " "‾")
                          "")
                        (propertize result 'face '(:slant      italic
                                                   :foreground "yellow")))))))

  (read-only-mode t)
  (goto-line      0)
  (move-to-column 1))
(defun klondike--stack-number-select (stack selected-num &optional hide-stack-p)
  ""

  (read-only-mode 0)

  (let ((  totalNum (if hide-stack-p 1 (length (klondike--stack-get-cards stack))))
        (visibleNum (klondike--stack-get-visible stack)))
    (goto-line      (+ 1
                       (klondike--stack-get-y stack)
                       (- totalNum        visibleNum)
                       (- (1+ visibleNum) selected-num)))
    (move-to-column (+ (klondike--stack-get-x stack) (- klondike----card-width 3)))

    (delete-region (point) (+ (point) 2))

    (let ((stringNum (number-to-string selected-num)))
      (insert (concat (if (= (length stringNum) 1)
                          (if (= selected-num visibleNum totalNum) " " "‾")
                        "")
                      (propertize stringNum
                                  'face '(:slant      italic
                                          :weight     bold
                                          :foreground "purple"))))))

  (read-only-mode t)
  (goto-line      0)
  (move-to-column 1))
(defun klondike--stack-clear-selects (stack &optional hide-stack-p)
  ""

  (read-only-mode 0)

  (let ((totalNum (if hide-stack-p 1 (length (klondike--stack-get-cards stack)))))
    (dotimes (stackIndex (klondike--stack-get-visible stack))
      (goto-line      (+ 1 (klondike--stack-get-y stack) (- totalNum stackIndex)))
      (move-to-column (+ (klondike--stack-get-x stack) (- klondike----card-width 3)))

      (delete-region (point) (+ (point) 2))

      (let ((result (number-to-string (1+ stackIndex))))
        (insert (if (= (1+ stackIndex) totalNum) "  " "‾‾")))))

  (read-only-mode t)
  (goto-line      0)
  (move-to-column 1))



(defun klondike--initialize-cards ()
  ""

  (let* ((1card+padding (+ klondike----card-width     klondike----window-padding))
         (topBotPadding (/ klondike----window-padding 2))
         (cardPack      (let ((suits  '(club heart spade diamond))
                              (result '()))
                          (dotimes (index 4)
                            (setq result (append result
                                                 (mapcar (lambda (value)
                                                           (klondike--card-create (nth index
                                                                                       suits)
                                                                                  value))
                                                         klondike----card-values))))

                          result))
         (fill-stack    (lambda (num)
                          (let ((result '()))
                            (dotimes (n num)
                              (let* ((r    (random (length cardPack)))
                                     (tail (nthcdr r cardPack)))
                                (setq cardPack (append (butlast cardPack
                                                                (length tail))
                                                       (cdr tail))
                                      result   (cons (car tail) result))))

                            result))))
    (klondike--stack-set klondike----empty-0-stack
                         '()
                         0
                         (+ klondike----window-padding (* (+ 0 3) 1card+padding))
                         topBotPadding)
    (klondike--stack-set klondike----empty-1-stack
                         '()
                         0
                         (+ klondike----window-padding (* (+ 1 3) 1card+padding))
                         topBotPadding)
    (klondike--stack-set klondike----empty-2-stack
                         '()
                         0
                         (+ klondike----window-padding (* (+ 2 3) 1card+padding))
                         topBotPadding)
    (klondike--stack-set klondike----empty-3-stack
                         '()
                         0
                         (+ klondike----window-padding (* (+ 3 3) 1card+padding))
                         topBotPadding)

    (let ((y (+ topBotPadding
                klondike----card-height
                klondike----top-&-bottom-row-spacing))
          (x (lambda (cardIndex)
               (+ klondike----window-padding (* cardIndex 1card+padding)))))
      (klondike--stack-set klondike----pile-0-stack (funcall fill-stack 1) 1
                                                    (funcall x 0)          y)
      (klondike--stack-set klondike----pile-1-stack (funcall fill-stack 2) 1
                                                    (funcall x 1)          y)
      (klondike--stack-set klondike----pile-2-stack (funcall fill-stack 3) 1
                                                    (funcall x 2)          y)
      (klondike--stack-set klondike----pile-3-stack (funcall fill-stack 4) 1
                                                    (funcall x 3)          y)
      (klondike--stack-set klondike----pile-4-stack (funcall fill-stack 5) 1
                                                    (funcall x 4)          y)
      (klondike--stack-set klondike----pile-5-stack (funcall fill-stack 6) 1
                                                    (funcall x 5)          y)
      (klondike--stack-set klondike----pile-6-stack (funcall fill-stack 7) 1
                                                    (funcall x 6)          y))

    (klondike--stack-set klondike----facedown-stack
                         (funcall fill-stack 24)
                         0
                         klondike----window-padding
                         topBotPadding)
    (klondike--stack-set klondike----faceup-stack
                         '()
                         0
                         (+ klondike----window-padding (1- 1card+padding))
                         topBotPadding))

  (klondike--history-save))



(defun klondike--card-find-available-empty (stack-symbol &optional stack-num)
  ""

  (let* ((stack (cl-case stack-symbol
                  ('faceup klondike----faceup-stack)
                  ('pile   (cl-case stack-num
                             (0 klondike----pile-0-stack)
                             (1 klondike----pile-1-stack)
                             (2 klondike----pile-2-stack)
                             (3 klondike----pile-3-stack)
                             (4 klondike----pile-4-stack)
                             (5 klondike----pile-5-stack)
                             (6 klondike----pile-6-stack)))))
         (card  (car (klondike--stack-get-cards stack)))
         (mNum  (cond
                 ((klondike--card-next-p card (car (klondike--stack-get-cards klondike----empty-0-stack)) t) 0)
                 ((klondike--card-next-p card (car (klondike--stack-get-cards klondike----empty-1-stack)) t) 1)
                 ((klondike--card-next-p card (car (klondike--stack-get-cards klondike----empty-2-stack)) t) 2)
                 ((klondike--card-next-p card (car (klondike--stack-get-cards klondike----empty-3-stack)) t) 3)
                 (t                                                                                          nil))))
    (if mNum
        (klondike--card-move stack-symbol stack-num 1 'empty mNum)
      (run-at-time 0.1 nil (lambda () (message "Ain't any available spot…"))))))

(defun klondike--card-move (type1 index1 stack-depth type2 index2)
  ""

  (let* ((stack1     (klondike--stack-get type1 index1))
         (stack2     (klondike--stack-get type2 index2))
         (movingCard (nth (1- stack-depth) (klondike--stack-get-cards stack1)))
         ( underCard (car (klondike--stack-get-cards stack2))))
    (if (or (> stack-depth (klondike--stack-get-visible stack1))
            (not (klondike--card-next-p movingCard underCard (eq type2 'empty))))
        (run-at-time 0.1 nil (lambda () (message "Can't do that, Jack!")))
      (klondike--stack-set-cards stack2 (append (butlast (klondike--stack-get-cards stack1)
                                                         (- (length (klondike--stack-get-cards stack1))
                                                            stack-depth))
                                                (klondike--stack-get-cards stack2)))
      (klondike--stack-set-cards stack1 (cdr (member movingCard
                                                     (klondike--stack-get-cards stack1))))

      (klondike--stack-set-visible stack1 (if (eq type1 'empty)
                                              (if (klondike--stack-get-cards stack1) 1 0)
                                            (let ((v (- (klondike--stack-get-visible stack1)
                                                        stack-depth)))
                                              (if (> v 0) v 1))))
      (klondike--stack-set-visible stack2 (if (eq type2 'empty)
                                              (if (klondike--stack-get-cards stack2) 1 0)
                                            (+ (klondike--stack-get-visible stack2) stack-depth)))

      (klondike--history-save)
      (klondike--card-insert-all `(,(cl-case type1
                                      ('faceup :faceup)
                                      ('pile   (cl-case index1
                                                 (0 :pile0) (1 :pile1)
                                                 (2 :pile2) (3 :pile3)
                                                 (4 :pile4) (5 :pile5)
                                                 (6 :pile6)))
                                      ('empty  (cl-case index1
                                                 (0 :empty0) (1 :empty1)
                                                 (2 :empty2) (3 :empty3))))
                                   ,(cl-case type2
                                      ('faceup :faceup)
                                      ('pile   (cl-case index2
                                                 (0 :pile0) (1 :pile1)
                                                 (2 :pile2) (3 :pile3)
                                                 (4 :pile4) (5 :pile5)
                                                 (6 :pile6)))
                                      ('empty  (cl-case index2
                                                 (0 :empty0) (1 :empty1)
                                                 (2 :empty2) (3 :empty3)))))))))

(defvar klondike----stack-pile-pick-stack (cons nil -1)
  "")
(defvar klondike----stack-pile-pick-num -1
  "")
(defun klondike--stack-pick-or-select (stack-type &optional stack-num)
  ""

  (setq klondike----stack-pile-pick-stack `(,stack-type . ,stack-num))

  (let ((stack (klondike--stack-get stack-type stack-num)))
    (if (not (klondike--stack-get-cards stack))
        (message "That spot there's empty, pardner…")
      (if (= (klondike--stack-get-visible stack) 1)
          (progn
            (setq klondike----stack-pile-pick-num 1)

            (klondike-select-mode))
        (klondike-picker-mode)))))
(defun klondike--stack-pick-or-select-quit ()
  ""
  (interactive)

  (let* ((stack-symbol      (car klondike----stack-pile-pick-stack))
         (stack-num         (cdr klondike----stack-pile-pick-stack))
         (stack        (klondike--stack-get stack-symbol stack-num)))
    (if (or (eq major-mode 'klondike-picker-mode)
            (and (eq major-mode 'klondike-select-mode)
                 (and (= klondike----stack-pile-pick-num     1)
                      (= (klondike--stack-get-visible stack) 1))))
        (klondike-mode)
      (klondike-picker-mode))))
(defun klondike--stack-find-available-empty ()
  ""
  (interactive)

  (let* ((stack-symbol      (car klondike----stack-pile-pick-stack))
         (stack-num         (cdr klondike----stack-pile-pick-stack))
         (stack        (klondike--stack-get stack-symbol stack-num)))
    (if (and (eq major-mode 'klondike-select-mode)
             (not (and (= klondike----stack-pile-pick-num     1)
                       (= (klondike--stack-get-visible stack) 1))))
        (message "Not allowed…")
      (klondike--card-find-available-empty stack-symbol stack-num)

      (klondike-mode))))
(defun klondike--stack-pick (card-num)
  ""

  (let ((stack (klondike--stack-get (car klondike----stack-pile-pick-stack)
                                    (cdr klondike----stack-pile-pick-stack))))
    (if (> card-num (klondike--stack-get-visible stack))
        (message "Mmmm…that's not an option; move which card in the stack?")
      (setq klondike----stack-pile-pick-num card-num)

      (klondike-select-mode))))
(defun klondike--stack-select (stack-type stack-num)
  ""

  (klondike--card-move (car klondike----stack-pile-pick-stack)
                       (cdr klondike----stack-pile-pick-stack)
                       klondike----stack-pile-pick-num
                       stack-type
                       stack-num)

  (klondike-mode))

(defun klondike-card-deck-next ()
  ""
  (interactive)

  (if (zerop (length (klondike--stack-get-cards klondike----facedown-stack)))
      (progn
        (klondike--stack-set-cards klondike----facedown-stack
                                   (reverse (klondike--stack-get-cards klondike----faceup-stack)))
        (klondike--stack-set-cards klondike----faceup-stack   '())

        (klondike--stack-set-visible klondike----faceup-stack 0))
    (klondike--stack-set-cards klondike----faceup-stack
                               (cons (car (klondike--stack-get-cards klondike----facedown-stack))
                                     (klondike--stack-get-cards klondike----faceup-stack)))
    (klondike--stack-set-cards klondike----facedown-stack
                               (cdr (klondike--stack-get-cards klondike----facedown-stack)))

    (klondike--stack-set-visible klondike----faceup-stack 1))

  (klondike--history-save)
  (klondike--card-insert-all '(:facedown :faceup)))

(defvar klondike-mode-map (let ((mode-map (make-sparse-keymap)))
                            (define-key mode-map (kbd "SPC") #'klondike-card-deck-next)

                            (define-key mode-map (kbd "0") (lambda ()
                                                             (interactive)

                                                             (klondike--stack-pick-or-select 'faceup)))

                            (define-key mode-map (kbd "!") (lambda ()
                                                             (interactive)

                                                             (klondike--stack-pick-or-select 'empty
                                                                                             0)))
                            (define-key mode-map (kbd "@") (lambda ()
                                                             (interactive)

                                                             (klondike--stack-pick-or-select 'empty
                                                                                             1)))
                            (define-key mode-map (kbd "#") (lambda ()
                                                             (interactive)

                                                             (klondike--stack-pick-or-select 'empty
                                                                                             2)))
                            (define-key mode-map (kbd "$") (lambda ()
                                                             (interactive)

                                                             (klondike--stack-pick-or-select 'empty
                                                                                             3)))

                            (mapc (lambda (num)
                                    (define-key mode-map
                                                (kbd (number-to-string (1+ num)))
                                                (lambda ()
                                                  (interactive)

                                                  (klondike--stack-pick-or-select 'pile
                                                                                  num))))
                                  (number-sequence 0 6))

                            (define-key mode-map (kbd "C-/")    #'klondike-history-prev)
                            (define-key mode-map (kbd "C-_")    #'klondike-history-prev)
                            (define-key mode-map (kbd "<undo>") #'klondike-history-prev)
                            (define-key mode-map (kbd "C-x u")  #'klondike-history-prev)

                            mode-map)
  "Keymap for `klondike-mode'.")
(define-derived-mode klondike-mode fundamental-mode "Klondike"
  "Major mode for the Klondike solitaire game for Emacs."

  (when-let ((stack-type (car klondike----stack-pile-pick-stack)))
    (klondike--stack-clear-selects
      (klondike--stack-get stack-type (cdr klondike----stack-pile-pick-stack))
      (not (eq stack-type 'pile))))

  (setq klondike----stack-pile-pick-stack (cons nil -1))
  (setq klondike----stack-pile-pick-num   -1))

(defvar klondike-picker-mode-map (let ((mode-map (make-sparse-keymap)))
                                   (define-key mode-map (kbd "TAB") #'klondike--stack-find-available-empty)

                                   (mapc (lambda (num)
                                           (define-key mode-map
                                                       (kbd (concat "<f"
                                                                    (number-to-string num)
                                                                    ">"))
                                                       (lambda ()
                                                         (interactive)

                                                         (klondike--stack-pick num))))
                                         (number-sequence 1 12))

                                   (mapc (lambda (num)
                                           (define-key mode-map
                                                       (kbd (number-to-string (1+ num)))
                                                       (lambda ()
                                                         (interactive)

                                                         (let ((sType (car klondike----stack-pile-pick-stack))
                                                               (sNum  (cdr klondike----stack-pile-pick-stack)))
                                                           (setq klondike----stack-pile-pick-num
                                                                 (if (eq sType 'empty)
                                                                     1
                                                                   (klondike--stack-get-visible
                                                                     (klondike--stack-get sType sNum)))))

                                                         (klondike--stack-select 'pile
                                                                                 num))))
                                         (number-sequence 0 6))

                                   (define-key mode-map (kbd "C-g") #'klondike--stack-pick-or-select-quit)

                                   mode-map)
  "Keymap for `klondike-picker-mode'.")
(define-derived-mode klondike-picker-mode fundamental-mode "Klondike Picker"
  "Major mode for picking an upward-facing card from a stack in the Klondike
solitaire game for Emacs."

  (klondike--stack-number
    (klondike--stack-get (car klondike----stack-pile-pick-stack)
                         (cdr klondike----stack-pile-pick-stack)))

  (message "Move which card in the stack?"))

(defvar klondike-select-mode-map (let ((mode-map (make-sparse-keymap)))
                                   (define-key mode-map (kbd "TAB") #'klondike--stack-find-available-empty)

                                   (define-key mode-map (kbd "!") (lambda ()
                                                                    (interactive)

                                                                    (klondike--stack-select 'empty 0)))
                                   (define-key mode-map (kbd "@") (lambda ()
                                                                    (interactive)

                                                                    (klondike--stack-select 'empty 1)))
                                   (define-key mode-map (kbd "#") (lambda ()
                                                                    (interactive)

                                                                    (klondike--stack-select 'empty 2)))
                                   (define-key mode-map (kbd "$") (lambda ()
                                                                    (interactive)

                                                                    (klondike--stack-select 'empty 3)))

                                   (mapc (lambda (num)
                                           (define-key mode-map
                                                       (kbd (number-to-string (1+ num)))
                                                       (lambda ()
                                                         (interactive)

                                                         (klondike--stack-select 'pile num))))
                                         (number-sequence 0 6))

                                   (define-key mode-map (kbd "C-g") #'klondike--stack-pick-or-select-quit)

                                   mode-map)
  "Keymap for `klondike-select-mode'.")
(define-derived-mode klondike-select-mode fundamental-mode "Klondike Select"
  "Major mode for selecting a stack to move an upward-facing card to in the
Klondike solitaire game for Emacs."

  (klondike--stack-number-select
    (klondike--stack-get (car klondike----stack-pile-pick-stack)
                         (cdr klondike----stack-pile-pick-stack))
    klondike----stack-pile-pick-num
    (not (eq (car klondike----stack-pile-pick-stack) 'pile)))

  (message (concat "Move the cards to which stack "
                   "(use Shift to move to one of "
                   "the 4 stacks on the top-right)?")))

(defun klondike ()
  ""
  (interactive)

  (if-let ((existing (get-buffer klondike----buffer-name)))
      (switch-to-buffer existing)
    (switch-to-buffer klondike----buffer-name)

    (toggle-truncate-lines t)
    (read-only-mode        0)
    (erase-buffer)

    (klondike--initialize-cards)

    (let ((1card+padding (+ klondike----card-width
                            klondike----window-padding))
          (topBotPadding (1- klondike----window-padding)))
      (dotimes (lineNum (+ topBotPadding
                           klondike----card-height
                           klondike----top-&-bottom-row-spacing
                           klondike----card-height
                           6
                           topBotPadding
                           20))
        (goto-line (1+ lineNum))

        (insert (make-string (+ klondike----window-padding
                                (* 7 1card+padding))        ? ) "\n"))

      (klondike--card-insert-all))

    (read-only-mode        t))

  (klondike-mode))


(setq global-mode-string
      (cond
       ((consp global-mode-string)   (add-to-list 'global-mode-string
                                                  'klondike----mode-line-status
                                                  'APPEND))
       ((not global-mode-string)     (list "" 'klondike----mode-line-status))
       ((stringp global-mode-string) (list global-mode-string
                                           'klondike----mode-line-status))))



(provide 'klondike)

;;; klondike.el ends here
