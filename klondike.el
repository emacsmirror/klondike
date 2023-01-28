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
(defconst klondike----buffer-name "Klondike"
  "The name of the buffer the Klondike solitaire game always runs in.")

(defconst klondike----window-padding 2
  "")

(defconst klondike----card-width  11
  "")
(defconst klondike----card-height 11
  "")

(defconst klondike----top-&-bottom-row-spacing 4
  "")

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
                ((or 'spade 'club)    (pcase suit-symbol
                                        ('spade klondike----suits-icon-spade)
                                        ('club  klondike----suits-icon-club)))
                ((or 'heart 'diamond) (propertize (pcase suit-symbol
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
    (insert (if empty-p " _ _ _ _ _ " " _________ "))



    (let* ((cardHeightW/oTopBot (- klondike----card-height 2))
           (totalN              (if total-num total-num 0))
           (faceups             (if (and (not show-stack-p) (> (length faceup-cards) 0))
                                    (list (car faceup-cards))
                                  faceup-cards))
           (numOfFacedownCards  (if show-stack-p (- totalN
                                                    (length faceups)) 0)))
      (dotimes (i numOfFacedownCards)
        (funcall move-to x (+ y 1 (1+ i)))
        (insert "|\\ \\ \\ \\ \\|"))



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
                (insert (if (and empty-p (evenp offset)) " " "|")
                        (make-string widthRest ? )
                        (if (and faceups (= offset (/ rows 2)))
                            (klondike--card-get-suit (car faceups))
                          " ")
                        (make-string widthHalf ? )
                        (if (and empty-p (evenp offset)) " " "|"))))))



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
  (move-to-column 1))
