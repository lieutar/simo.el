;;
;; This file is sample for usage of simo.
;;

(error "


============================================================
               Must not load this file.
============================================================

  If you try to execute this file, open this file by
  'M-x find-file' and evaluate by 'C-x C-e'.




")

(let ((load-path (cons default-directory load-path))
      (n 64)
      (buf (get-buffer-create "*simo-test*"))
      (win (selected-window)))

  (load "simo")

  (set-buffer buf)
  (delete-region (point-min) (point-max))
  (let* ((w  n)
         (h  n)
         (hw (/ w 2))
         (hh (/ h 2))
         (3w (/ w 3))
         (3h (/ h 3))
         (6w (- w 3w))
         (6h (- h 3w))
         (img (simo::new :width w :height h)))
    (simo-fill-rect img 0 0  w  h 0)

    (simo-draw-line img 3w 0 3w h 5)
    (simo-draw-line img 6w h 6w 0 6)
    (simo-draw-line img 0 3h w 3h 7)
    (simo-draw-line img w 6h 0 6h 8)

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


    (simo-rect      img 0 0  w  h 9)

    (let ((xpm (simo-to-xpm img :cpp4 t)))
      (insert xpm "\n")
      (insert (simo::add-image-properties
               xpm
               :colors simo-alist-palette-16))
      (insert (simo::add-image-properties
               xpm
               :colors (simo-palette::list-to-alist
                        (reverse simo-list-palette-16))))
      (insert "\n\n")
      (let* ((result  (simo::from-xpm (simo-to-xpm img
                                                   :palette simo-palette-16)))
             (palette (car result))
             (img2    (cadr result)))
        (insert (simo-to-xpm img2 :palette palette)"\n")
        (simo-insert img2 :palette palette)
        )))
  (pop-to-buffer buf)
  (select-window win))
