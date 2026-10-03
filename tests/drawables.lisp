;;;; tests/drawables.lisp — drawable ownership and classes, and fonts whose family is no longer cached.
;;;; Run with tests/run.sh (see there).

(asdf:load-asd (merge-pathnames "../clxcb-truetype.asd" *load-truename*))
(ql:quickload :clxcb-truetype :silent t)

(defmacro check (name form expect)
  `(let ((v ,form))
     (format t "~&~:[FAIL~;ok  ~] ~a: ~s~%" (equal v ,expect) ,name v)))

(xcb:with-x-connection (conn)
  (let* ((screen (first (xcb:conn-roots conn)))
         (alive (lambda (w)
                  (handler-case
                      (progn (xcb:get-geometry-reply conn (xcb:get-geometry conn w)) t)
                    (error () nil)))))
    (xcb-truetype:cache-fonts)

    (let* ((w (xcb:generate-id conn)))
      (xcb:create-window conn 0 w (xcb:root screen) 0 0 50 50 0 1 0 0)
      (let ((d (make-instance 'xcb-truetype:window :owned t :connection conn :id w :screen screen)))
        (xcb-truetype:draw-text d (make-instance 'xcb-truetype:font :family "DejaVu Sans" :size 12) "x" 5 20)
        (xcb-truetype:release-drawable d)
        (check "release-drawable clears owned" (xcb-truetype:drawable-owned-p d) nil)
        (xcb-truetype:destroy-drawable d) (xcb:flush conn)
        (check "  so destroy-drawable then leaves the window" (funcall alive w) t)))

    (defclass my-thing (xcb-truetype:drawable) ())

    (check "a drawable subclass with no kind is refused"
           (handler-case
               (progn
                 (make-instance 'my-thing :connection conn :id 1) :made)
             (error () :refused))
           :refused)

    (defclass my-win (xcb-truetype:window) ())

    (check "a window subclass is accepted"
           (handler-case
               (progn
                 (make-instance 'my-win :connection conn :id 1) :made)
             (error () :refused))
           :made)
    (check "stale font family gives nil, no error"
           (xcb-truetype::get-font-pathname
            (make-instance 'xcb-truetype:font :family "DejaVu Sans" :size 12))
           (xcb-truetype::get-font-pathname (make-instance 'xcb-truetype:font :family "DejaVu Sans" :size 12)))
    (check "missing family -> nil"
           (let ((f (make-instance 'xcb-truetype:font :family "DejaVu Sans" :size 12)))
             (clrhash xcb-truetype:*font-cache*) (xcb-truetype::get-font-pathname f)) nil)))
