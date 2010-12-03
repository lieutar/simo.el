;;; simo.el --- simple image manuplation object.

;; Copyright (C) 2010  

;; Author:  <lieutar@TREEFROG>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(require 'eieio)

(defconst simo-xpm-symbol-table
  (let ((n 33)
        T)
    (while (< n 127)
      (add-to-list 'T n)
      (setq n (1+ n)))
    (replace-regexp-in-string "[\\\\|\"]" "" (apply 'string (reverse T)))))

(defconst simo-xpm-max-colors (length simo-xpm-symbol-table))
(defconst simo-xpm-char-width 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; simo-palette - xpm palette object for simo
;;;


(defclass simo-palette ()
  (
   (sym
    :reader  simo-palette-sym
    :writer  simo-palette-set-sym
    :type    vector
    :initarg :sym
    :initform []
    :documentation "symbol palette.")

   (full
    :reader  simo-palette-full
    :writer  simo-palette-set-full
    :type    vector
    :initarg :full
    :initform []
    :documentation "24bit color palette.")

   (mono
    :reader  simo-palette-mono
    :writer  simo-palette-set-mono
    :type    vector
    :initarg :mono
    :initform []
    :docmentation "binary palette.")

   (g4
    :reader  simo-palette-g4
    :writer  simo-palette-set-g4
    :type    vector
    :initarg :g4
    :initform []
    :documentation "grayscale (4 stages) palette.")

   (gray
    :reader  simo-palette-gray
    :writer  simo-palette-set-gray
    :type    vector
    :initarg :gray
    :initform []
    :documentation "grayscale palette.")
   )
  :documentation "indexed color palette for `simo'."
  )


(defun simo-palette::new (&rest args)
  (apply 'make-instance 'simo-palette args))

(defsubst simo-palette::-number-to-xpm-color-info (self attr sym idx default)
  (let ((ar (eieio-oref self attr)))
    (if ar
        (condition-case nil
            (format " %s %s" sym (aref ar idx))
          (args-out-of-range default))
      default)))

(defmethod simo-palette-xpm-sym-info ((self simo-palette) idx)
  (simo-palette::-number-to-xpm-color-info self 'sym "s" idx
                                           (format "color-%s" idx)))

(defmethod simo-palette-xpm-full-info ((self simo-palette) idx)
  (simo-palette::-number-to-xpm-color-info self 'full "c" idx ""))

(defmethod simo-palette-xpm-mono-info ((self simo-palette) idx)
  (simo-palette::-number-to-xpm-color-info self 'mono "m" idx ""))

(defmethod simo-palette-xpm-gray-info ((self simo-palette) idx)
  (simo-palette::-number-to-xpm-color-info self 'gray "g" idx ""))

(defmethod simo-palette-xpm-g4-info ((self simo-palette) idx)
  (simo-palette::-number-to-xpm-color-info self 'g4  "g4"idx ""))

(defmethod simo-palette-xpm-line ((self simo-palette) sym num)
  (labels ((palref (ar sym idx default)
                   (if ar
                       (condition-case nil
                           (let ((color (aref ar idx)))
                             (if color
                                 (format " %s %s" sym color)
                               default))
                         (args-out-of-range default))
                     default)))
    (format "\"%s\t%s%s%s%s%s\","
            sym
            (palref (oref self sym)  "s"  num (format "s color-%d" num))
            (palref (oref self full) "c"  num "")
            (palref (oref self mono) "m"  num "")
            (palref (oref self g4)   "g4" num "")
            (palref (oref self gray) "g"  num ""))))


(defmethod simo-palette-set-n-colors ((self simo-palette)
                                      n-colors &rest default)
  (dolist (attr '(sym full mono g4 gray))
    (eieio-oset self attr (make-vector
                           n-colors
                           (or (plist-get default
                                          (intern (format ":%s" attr)))
                               nil)))))

(defmethod simo-palette-add-xpm-info ((self simo-palette) index data)
  (dolist (slot '(("s\\s +\\([^\t ]+\\)"  . sym)
                  ("c\\s +\\([^\t ]+\\)"  . full)
                  ("m\\s +\\([^\t ]+\\)"  . mono)
                  ("g4\\s +\\([^\t ]+\\)" . g4)
                  ("g\\s +\\([^\t ]+\\)"  . gray)))
    (let ((regex (car slot))
          (attr  (cdr slot)))
      (when (string-match regex data)
        (aset (eieio-oref self attr) index (match-string 1 data))))))






;;;
;;; preset palettes
;;;

(defconst simo-list-palette-16 '( "#000000"
                                  "#FF0000"
                                  "#00FF00"
                                  "#FFFF00"
                                  "#0000FF"
                                  "#FF00FF"
                                  "#00FFFF"
                                  "#FFFFFF"
                                  "#666666"
                                  "#990000"
                                  "#009900"
                                  "#999900"
                                  "#000099"
                                  "#990099"
                                  "#009999"
                                  "#999999" ))
(defconst simo-vector-palette-16 (apply 'vector simo-list-palette-16))

(defun simo-palette::list-to-alist (src)
  (let ((n 0))
    (mapcar (lambda (color)
              (let ((R (cons n color)))
                (setq n (1+ n))
                R))
            src)))

(defconst simo-alist-palette-16
  (simo-palette::list-to-alist simo-list-palette-16))

(defconst simo-palette-16
  (simo-palette::new :full simo-vector-palette-16))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; simo -  simple image manupulation object
;;;

(defclass simo ()
  ((width
    :reader  simo-width
    :type    integer
    :initarg :width)
   (height
    :reader  simo-height
    :type    integer
    :initarg :height)
   (pixels
    :reader  simo-pixels
    :type    vector
    )))

(defun simo::new (&rest args)
  (let* ((self (apply 'make-instance 'simo args))
         (w    (simo-width  self))
         (h    (simo-height self)))
    (oset  self pixels (make-vector (* w h) -1))
    self))


(defmethod simo-resize ((self simo) width height x y)
  (let ((tmp (simo::new :width width :height height))
        (w   (simo-width  self))
        (h   (simo-height self)))

    (let ((Y 0))
      (while (< Y h)
        (let ((X 0))
          (while (< X w)
            (simo-put-pixel (+ X x)
                                    (+ Y y)
                                    (simo-get-pixel self X Y))
            (setq X (1+ X))))
        (setq Y (1+ Y))))
    ))

(defmethod simo-get-pixel ((self simo) x y)
  (let ((width  (simo-width   self))
        (height (simo-height  self))
        (pixels (oref self pixels)))
    (when (and (<  x width)
               (<  y height)
               (>= x 0)
               (>= y 0))
      (aref pixels (+ x (* width y))))))

(defmethod simo-put-pixel ((self simo) x y c)
  (let ((width  (simo-width   self))
        (height (simo-height  self))
        (pixels (oref self pixels)))
    (when (and (<  x width)
               (<  y height)
               (>= x 0)
               (>= y 0))
      (aset pixels
            (+ x (* width y)) c))))


;;;
;;;;; SEE ALSO
;;;
;;; (browse-url "http://rctools.sourceforge.jp/pukiwiki/index.php?soccerwindow2%2FXPM%A5%D5%A5%A9%A1%BC%A5%DE%A5%C3%A5%C8")
;;;
(defmethod simo-to-xpm ((self simo) &rest more-spec)
  (let ((width   (simo-width  self))
        (height  (simo-height self))
        (y       0)
        (rows    nil)
        (symbols (make-hash-table :test 'eq))
        (ncolors 0)
        (pixels  nil)
        (colors  nil)
        (imgname  (or (plist-get more-spec :name) "untitled"))
        (palette  (or (plist-get more-spec :palette) (simo-palette::new)))
        (pal-sym  (plist-get more-spec     :sym))
        (pal-mono (plist-get more-spec     :mono))
        (pal-full (plist-get more-spec     :full))
        (pal-g4   (plist-get more-spec     :g4))
        (pal-gray (plist-get more-spec     :gray)))

    (when pal-full (simo-palette-set-full palette pal-full))
    (when pal-g4   (simo-palette-set-g4   palette pal-g4))
    (when pal-mono (simo-palette-set-mono palette pal-mono))
    (when pal-gray (simo-palette-set-gray palette pal-gray))

    (while (< y height)
      (let ((x   0)
            (row nil))
        (while (< x width)
          (let* ((c   (simo-get-pixel self x y))
                 (sym 32))

            (when (> c -1)
              (setq sym (aref simo-xpm-symbol-table c))
              (puthash c (string sym) symbols))

            (setq row (cons sym row)))
          (setq x (1+ x)))
        (setq rows (cons (apply 'string (reverse row)) rows)))
      (setq y (1+ y)))
    
    (setq pixels
          (mapconcat (lambda (row) (format "%S" row)) (reverse rows) ",\n"))

    (setq colors (mapconcat (lambda (slot) 
                              (simo-palette-xpm-line palette
                                                     (cdr slot)
                                                     (car slot)))
                            (let ((R))
                              (maphash (lambda (key val)
                                         (setq ncolors (1+ ncolors))
                                         (setq R (cons (cons key val) R)))
                                       symbols)
                              R)
                            "\n"))

    (format "/* XPM */
static char * %s[] = {
/* Values */
\"%d %d %d %d\",
/* Colors */
\" \t s None m None g None g4 None c None\"%s
/* pixels */
%s
};
"
            imgname
            width height (1+ ncolors) simo-xpm-char-width
            (if (> ncolors 0) (concat ",\n" colors) "")
            pixels)))



(defun simo::add-image-properties (data &rest props)
  (let ((str     (plist-get props :string))
        (begin   (plist-get props :begin))
        (end     (plist-get props :end))
        (buffer  (plist-get props :buffer))
        (type    (plist-get props :type))
        (ascent  (plist-get props :acent))
        (palette (plist-get props :colors))
        (copy    (plist-get props :copy))
        (obj     nil))

    (setq obj
          (if (or begin
                  end
                  buffer)
              (progn
                (setq buffer (or buffer (current-buffer)))
                (save-excursion 
                  (set-buffer buffer)
                  (setq begin (or begin (point-min)))
                  (setq end   (or end   (min (1+ begin) (point-max))))))
            (progn
              (setq str (or str " "))
              (when copy (setq str (copy-sequence str)))
              (setq begin (or begin 0))
              (setq end   (or end  (length str)))
              str)))
    
    (add-text-properties
     begin end
     `(display (image :type   ,(or type 'xpm)
                      :data   ,data
                      :ascent ,(or ascent 'center)
                      ,@(and palette
                             `(:color-symbols
                               ,(mapcar
                                 (lambda (slot)
                                   (let ((sym (car slot))
                                         (val (cdr slot)))
                                     (when (numberp sym)
                                       (setq sym (format "color-%d" sym)))
                                     (cons sym val)))
                                 palette
                                 )))))
     obj)
    obj))



(defmethod simo-string ((self simo) &rest opt)
  (let ((R   (or (plist-get opt :string) " "))
        (img (apply 'simo-to-xpm self opt)))
    (apply 'simo::add-image-properties img :string R :copy t opt)))

(defmethod simo-insert ((self simo) &rest opt)
  (insert (apply 'simo-string self opt)))

;;

(defmethod simo-rect ((self simo) x0 y0 x1 y1 c)
  (let ((x (min x1 x0))
        (y (min y1 y0))
        (X (max x1 x0))
        (Y (max y1 y0)))
    (let ((x x))
      (while (<= x X)
        (simo-put-pixel self x y      c)
        (simo-put-pixel self x (1- Y) c)
        (setq x (1+ x))))
    (let ((y y))
      (while (<= y Y)
        (simo-put-pixel self x y c)
        (simo-put-pixel self (1- X) y c)
        (setq y (1+ y))))))

(defmethod simo-fill-rect ((self simo) x0 y0 x1 y1 c)
  (let ((x (min x1 x0))
        (y (min y1 y0))
        (X (max x1 x0))
        (Y (max y1 y0)))
    (while (<= y Y)
      (let ((x x))
        (while (<= x X)
          (simo-put-pixel self x y c)
          (setq x (1+ x))))
      (setq y (1+ y)))))

(defmethod simo-draw-line ((self simo) x0 y0 x1 y1 c)
  (let* ((w  (- x1 x0))
         (h  (- y1 y0))
         (W  (abs w))
         (H  (abs h))
         (L  (max W H))
         (S  (min W H))
         (b  0)
         (x  x0)
         (y  y0)
         (w1 (/ w W))
         (h1 (/ h H))
         ls
         ss
         l1
         s1)

    (if (> W H)
        (progn (setq ls 'x)
               (setq ss 'y)
               (setq l1 w1)
               (setq s1 h1))
      (setq ls 'y)
      (setq ss 'x)
      (setq l1 h1)
      (setq s1 h1))

    (let ((s 0))
      (while (< s S)
        (while (< b L)
          (simo-put-pixel self x y c)
          (set ls (+ (symbol-value ls) l1))
          (setq b (+ b S)))
        (set ss (+ (symbol-value ss) s1))
        (setq s (1+ s))
        (setq b (- b L))))))

(defalias 'simo::-read-c-string 'read)

(defun simo::from-xpm-buffer (&optional buf)
  (save-excursion
    (set-buffer (or buf (current-buffer)))
    (goto-char (point-min))
    (let ((pixdic (make-hash-table :test 'equal))
          (regexp-c-string "\\(\"\\([^\\\"]\\|\\\\.\\)+\"\\)")
          width height p-width ncolors palette img)
      ;;
      ;; get values
      ;;
      (re-search-forward 
       "\"\\s *\\([0-9]+\\)\\s +\\([0-9]+\\)\\s +\\([0-9]+\\)\\s +\\([0-9]+\\)\\s *\"")
      (setq width   (string-to-number (match-string 1)))
      (setq height  (string-to-number (match-string 2)))
      (setq ncolors (string-to-number (match-string 3)))
      (setq p-width (string-to-number (match-string 4)))
      (setq img    (simo::new :width width :height height))
      (setq palette (simo-palette::new))
      (simo-palette-set-n-colors palette ncolors)

      ;;
      ;; get colors
      ;;
      (let ((i 0))
        (while (< i ncolors)
          (re-search-forward regexp-c-string)
          (let* ((data       (simo::-read-c-string (match-string 1)))
                 (sym        (substring data 0 p-width))
                 (color-info (substring data p-width)))
            (simo-palette-add-xpm-info palette i color-info)
            (puthash sym i pixdic))
          (setq i (1+ i))))

      ;;
      ;; get pixels
      ;;
      (let ((y 0))
        (while (< y height)
          (re-search-forward regexp-c-string)
          (let* ((data (simo::-read-c-string (match-string 1)))
                 (len  (length data))
                 (x 0))
            (while (< x width)
              (simo-put-pixel img x y
                              (gethash (substring data
                                                  (* x p-width)
                                                  (* (setq x (1+ x)) p-width))
                                       pixdic))))
          (setq y (1+ y))))
      (list palette img))))

(defun simo::from-xpm-file (file)
  (let* ((buf (find-file-no-select file))
         (simo (simo::from-xpm-buffer buf)))
    (kill-buffer buf)
    simo))

(defun simo::from-xpm (str)
  (with-temp-buffer
    (insert str)
    (simo::from-xpm-buffer (current-buffer))))


(provide 'simo)
;;; simo.el ends here
