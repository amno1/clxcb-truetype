;;;; glyphs-vs-masks.lisp — glyph-set text against the old whole-string alpha masks
;;;: ink per case, and cmp-NN.pgm images (old above new)
;;;; Run with tests/run.sh (see there)

(asdf:load-asd (merge-pathnames "../clxcb-truetype.asd" *load-truename*))
(ql:quickload :clxcb-truetype :silent t)

(defpackage :glyph-test (:use :cl))

(in-package :glyph-test)

(defparameter *w* 640)
(defparameter *h* 40)

(defun grab (conn pm)
  (let ((r (xcb:get-image-reply conn (xcb:get-image conn 2 pm 0 0 *w* *h* #xffffffff))))
    (coerce (xcb:data r) '(vector (unsigned-byte 8)))))

(defun render (conn screen font string line-p glyphs-p)
  (let* ((pm (xcb:generate-id conn)))
    (xcb:create-pixmap conn (xcb:root-depth screen) pm (xcb:root screen) *w* *h*)
    (let ((d (make-instance 'xcb-truetype:pixmap :owned t :connection conn :id pm :screen screen))
          (xcb-truetype:*draw-with-glyph-sets* glyphs-p))
      (xcb-render:composite conn 1 (xcb-truetype:solid-fill-picture conn 0) 0 (xcb-truetype:drawable-picture-for d) 0 0 0 0 0 0 *w* *h*)
      (if line-p
          (xcb-truetype:draw-text-line d font string 4 28 :colour #xFFFFFF)
          (xcb-truetype:draw-text d font string 4 28 :colour #xFFFFFF))
      (prog1
          (grab conn pm)
        (xcb-truetype:destroy-drawable d)))))

(defun ppm (path a b)
  (with-open-file (o path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (let ((hdr (format nil "P5 ~d ~d 255~%" *w* (* 2 *h*))))
      (write-sequence (map 'vector #'char-code hdr) o))
    (dolist (img (list a b))
      (dotimes (i (* *w* *h*))
        (write-byte (aref img (* 4 i)) o)))))

(xcb:with-x-connection (conn)
  (let ((screen (first (xcb:conn-roots conn))) (n 0) (worst 0))
    (xcb-truetype:cache-fonts)
    (dolist (spec '(("DejaVu Sans" nil 14) ("DejaVu Sans" nil 24) ("DejaVu Serif" nil 18)
                    ("Liberation Sans" "Bold" 16) ("Liberation Mono" nil 13)))
      (dolist (line-p '(nil t))
        (dolist (opts '(() (:underline t) (:strikethrough t :overline t) (:antialias nil)))
          (let* ((font (apply #'make-instance 'xcb-truetype:font
                              :family (first spec)
                              :subfamily (second spec)
                              :size (third spec) opts))
                 (s "Hello, World: AVATAR Ty fj Wa 0123 ~qg|")
                 (a (render conn screen font s line-p nil))
                 (b (render conn screen font s line-p t))
                 (diff 0) (maxd 0) (inka 0) (inkb 0))
            (dotimes (i (* *w* *h*))
              (let ((x (aref a (* 4 i))) (y (aref b (* 4 i))))
                (incf inka x)
                (incf inkb y)
                (when (> (abs (- x y)) 64)
                  (incf diff))
                (setf maxd (max maxd (abs (- x y))))))
            (setf worst (max worst (/ (abs (- inka inkb)) (max 1 inka))))
            (format t "~&~a ~a ~a line=~a ~s: ink old ~d new ~d (~,1,2f%) pixels >64 apart ~d~%"
                    (first spec) (or (second spec) "") (third spec) line-p opts inka inkb
                    (/ (- inkb inka) (max 1 inka)) diff)
            (when (< n 40)
              (ppm (format nil "cmp-~2,'0d.pgm" (incf n)) a b))))))

    (format t "~&WORST ink difference ~,1,2f%~%" worst)))
