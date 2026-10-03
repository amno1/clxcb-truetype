;;;; screens-and-threads.lisp — two screens (depths 24 and 16) and four threads drawing on one connection; needs two screens.
;;;; Run with tests/run.sh (see there).

(asdf:load-asd (merge-pathnames "../clxcb-truetype.asd" *load-truename*))
(ql:quickload :clxcb-truetype :silent t)

(defpackage :glyph-mt (:use :cl))

(in-package :glyph-mt)

(defmacro check (name form expect)
  `(let ((v ,form)) (format t "~&~:[FAIL~;ok  ~] ~a: ~s~%" (equal v ,expect) ,name v)))

(defparameter *w* 400)
(defparameter *h* 30)

(defun sync (conn)
  (xcb:get-input-focus-reply conn (xcb:get-input-focus conn)))

(defun x-errors (conn)
  (sync conn)
  (let ((n 0))
    (loop (handler-case (unless (xcb:poll-for-event conn) (return))
            (error (e) (incf n) (format t "~&  X error: ~a~%" e))))
    n))

(defun make-target (conn screen)
  (let ((pm (xcb:generate-id conn)))
    (xcb:create-pixmap conn (xcb:root-depth screen) pm (xcb:root screen) *w* *h*)
    (make-instance 'xcb-truetype:pixmap :owned t :connection conn :id pm :screen screen)))

(defun clear (d)
  (let ((conn (xcb-truetype:drawable-connection d)))
    (xcb-render:composite conn 1 (xcb-truetype:solid-fill-picture conn 0) 0
                          (xcb-truetype:drawable-picture-for d) 0 0 0 0 0 0 *w* *h*)))
(defun grab (d)
  (let ((conn (xcb-truetype:drawable-connection d)))
    (coerce (xcb:data (xcb:get-image-reply conn (xcb:get-image conn 2 (xcb-truetype:drawable-id d) 0 0 *w* *h* #xffffffff)))
            '(vector (unsigned-byte 8)))))

(defun ink (img &optional (depth 24))
  ;; The green channel of every pixel: 8 bits at depth 24 (BGRX),
  ;; 6 bits at depth 16 (RGB565, little-endian), scaled to 0..255.
  (if (= depth 16)
      (loop for i from 0 below (length img) by 2
            sum (round (* 255 (ldb (byte 6 5) (logior (aref img i) (ash (aref img (1+ i)) 8)))) 63))
      (loop for i from 1 below (length img) by 4 sum (aref img i))))

(defun draw (d font s glyphs-p)
  (let ((xcb-truetype:*draw-with-glyph-sets* glyphs-p))
    (clear d) (xcb-truetype:draw-text d font s 4 22 :colour #xFFFFFF) (grab d)))

(xcb:with-x-connection (conn)
  (xcb-truetype:cache-fonts)
  (let* ((screens (xcb:conn-roots conn))
         (font (make-instance 'xcb-truetype:font :family "DejaVu Sans" :size 16))
         (s "Two screens, one glyph set: Wq0"))
    ;;; Two screens
    (check "the X server has two screens" (length screens) 2)
    (check "  of depths 24 and 16" (mapcar #'xcb:root-depth screens) '(24 16))

    (dolist (screen screens)
      (let* ((d (make-target conn screen))
             (old (ink (draw d font s nil) (xcb:root-depth screen)))
             (new (ink (draw d font s t) (xcb:root-depth screen))))
        (check (format nil "screen of depth ~d: text drawn" (xcb:root-depth screen)) (plusp new) t)
        (check (format nil "  same ink as the old path, within 1% (~d vs ~d)" old new)
               (< (abs (- old new)) (* 0.01 old)) t)
        (xcb-truetype:destroy-drawable d)))

    (check "one glyph set serves both screens"
           (hash-table-count (gethash conn xcb-truetype::*glyph-sets*)) 1)
    (check "no X errors on two screens" (x-errors conn) 0)

    ;;; Threads drawing on one connection at once
    (let* ((screen (first screens))
           (fonts (list font
                        (make-instance 'xcb-truetype:font :family "DejaVu Serif" :size 13)
                        (make-instance 'xcb-truetype:font :family "Liberation Mono" :size 15)
                        (make-instance 'xcb-truetype:font :family "DejaVu Sans" :size 22 :underline t)))
           (final (lambda (i) (format nil "Thread ~d done: ÅÄÖ ~d" i (* i 7919))))
           (targets (loop for i below 4 collect (make-target conn screen)))
           (failures 0)
           (threads
             (loop for i below 4
                   for d in targets
                   collect (let ((i i) (d d))
                             (bt:make-thread
                              (lambda ()
                                (handler-case
                                    (let ((r (make-random-state t)))
                                      (dotimes (k 1500)
                                        ;; New strings, so new glyphs keep being uploaded
                                        ;; while the other threads draw.
                                        (xcb-truetype:draw-text
                                         d (nth (random 4 r) fonts)
                                         (format nil "~d ~a ~c~c"
                                                 (random 100000 r)
                                                 (code-char (+ 192 (random 200 r)))
                                                 (code-char (+ 65 (random 58
                                                                          r)))
                                                 (code-char (+ 913 (random 40 r))))
                                         4 22))
                                      (clear d)
                                      (xcb-truetype:draw-text d (nth i fonts) (funcall final i) 4 22 :colour #xFFFFFF)
                                      (xcb:flush conn))
                                  (error (e) (incf failures) (format t "~&  thread ~d: ~a~%" i e))))
                              :name (format nil "drawer ~d" i))))))
      (mapc #'bt:join-thread threads)
      (check "four threads finished without a Lisp error" failures 0)
      (check "no X errors from four threads" (x-errors conn) 0)
      (loop for i below 4 for d in targets
            do (let ((threaded (grab d))
                     (alone (draw d (nth i fonts) (funcall final i) t)))
                 (check (format nil "thread ~d's last string is what one thread draws" i)
                        (equalp threaded alone) t))))))
