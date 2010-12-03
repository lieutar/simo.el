(error "
============================================================
               You must not load this file.
============================================================

  If you try to execute this file, open this file by
  'M-x find-file' and evaluate by 'C-x C-e'.


")

(let ((load-path (cons default-directory load-path))
      (n 32))
  (require 'simo)
  (let* ((w  n)
         (h  n)
         (hw (/ w 2))
         (hh (/ h 2))
         (img (simo::new :width w :height h)))
    (simo-fill-rect img 0 0  w  h 0)

    (simo-draw-line img 0 0  w hh 1)
    (simo-draw-line img 0 0  w  h 1)
    (simo-draw-line img 0 0 hw  h 1)

    (simo-draw-line img w 0  0 hh 2)
    (simo-draw-line img w 0  0  h 2)
    (simo-draw-line img w 0 hw  h 2)

    (simo-draw-line img w h  0 hh 3)
    (simo-draw-line img w h  0  0 3)
    (simo-draw-line img w h hw  0 3)

    (simo-draw-line img 0 h  w hh 4)
    (simo-draw-line img 0 h  w  0 4)
    (simo-draw-line img 0 h hw  0 4)

    (simo-rect      img 0 0  w  h 7)

    (let ((xpm (simo-to-xpm img)))
      (insert xpm "\n")
      (insert (simo::add-image-properties
               xpm
               :palette simo-alist-palette-16))
      (insert (simo::add-image-properties
               xpm
               :palette (simo-palette::list-to-alist
                         (reverse simo-list-palette-16))))
      (insert "\n")
      )))
