;;;; tests/bench.lisp — draw-text time per string, glyph sets against alpha masks.
;;;; Run with tests/run.sh (see there).

(asdf:load-asd (merge-pathnames "../clxcb-truetype.asd" *load-truename*))
(ql:quickload :clxcb-truetype :silent t)
(defpackage :glyph-bench (:use :cl)) (in-package :glyph-bench)

(xcb:with-x-connection (conn)
  (let* ((screen (first (xcb:conn-roots conn)))
         (pm (xcb:generate-id conn)))
    (xcb-truetype:cache-fonts)
    (xcb:create-pixmap conn (xcb:root-depth screen) pm (xcb:root screen) 800 600)
    (let ((d (make-instance 'xcb-truetype:pixmap :owned t :connection conn :id pm :screen screen))
          (font (make-instance 'xcb-truetype:font :family "DejaVu Sans" :size 14))
          (labels (loop for i below 40 collect (format nil "Label number ~d: some text" i))))
      (flet ((run (glyphs-p strings n)
               (let ((xcb-truetype:*draw-with-glyph-sets* glyphs-p))
                 (dolist (s strings) (xcb-truetype:draw-text d font s 10 20)) ; warm caches
                 (xcb:get-input-focus-reply conn (xcb:get-input-focus conn))
                 (let ((t0 (get-internal-real-time)))
                   (dotimes (k n) (dolist (s strings) (xcb-truetype:draw-text d font s 10 (+ 20 (mod k 500)))))
                   (xcb:get-input-focus-reply conn (xcb:get-input-focus conn))
                   (/ (- (get-internal-real-time) t0) (/ internal-time-units-per-second 1000000) (* n (length strings)))))))
        (format t "~&repeated labels: old ~,1f us/string, glyph sets ~,1f us/string~%" (run nil labels 50) (run t labels 50))
        (let ((fresh (lambda () (loop for i below 40 collect (format nil "Counter ~d ~d" (random 1000000) i)))))
          (flet ((once (g) (let ((xcb-truetype:*draw-with-glyph-sets* g) (ss (funcall fresh)))
                             (xcb:get-input-focus-reply conn (xcb:get-input-focus conn))
                             (let ((t0 (get-internal-real-time)))
                               (dolist (s ss) (xcb-truetype:draw-text d font s 10 20))
                               (xcb:get-input-focus-reply conn (xcb:get-input-focus conn))
                               (/ (- (get-internal-real-time) t0) (/ internal-time-units-per-second 1000000) 40)))))
            (format t "~&new strings each time: old ~,1f us/string, glyph sets ~,1f us/string~%"
                    (/ (loop repeat 20 sum (once nil)) 20) (/ (loop repeat 20 sum (once t)) 20))))))))
