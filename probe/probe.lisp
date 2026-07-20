;;; cl-pdf Japanese probe -- pure ASCII source on purpose.
(require :asdf)

(defparameter *base* #P"C:/Users/snmst/AppData/Local/Temp/claude/C--msys64-home-snmst-work-nanka/09cbee8f-7469-442f-84d4-ce36440c0801/scratchpad/")

(push (merge-pathnames "cl-pdf/" *base*) asdf:*central-registry*)

(handler-bind ((warning #'muffle-warning))
  (funcall (read-from-string "ql:quickload") :cl-pdf :silent t))

(format t "~&==== cl-pdf loaded ====~%")

(defparameter *ttf* #P"C:/Windows/Fonts/yumin.ttf")

(format t "~&== loading font: ~a~%" *ttf*)
(defparameter *t0* (get-internal-real-time))
(defparameter *fm* (pdf:load-ttf-font *ttf*))
(format t "   load time : ~,1f sec~%"
        (/ (- (get-internal-real-time) *t0*) internal-time-units-per-second))
(format t "   font-name : ~s~%" (pdf::font-name *fm*))
(format t "   length1   : ~:d bytes (embedded verbatim)~%" (pdf::length1 *fm*))
(format t "   glyphs    : ~:d char-metrics~%" (hash-table-count (pdf::characters *fm*)))
(format t "   code range: #x~x .. #x~x~%" (pdf::min-code *fm*) (pdf::max-code *fm*))

;;; Decisive check: does CIDToGIDMap resolve these code points to a real GID?
;;; GID 0 == .notdef == glyph absent.
(defun gid-of (code)
  (let ((c2g (pdf::c2g *fm*)))
    (+ (ash (char-code (aref c2g (* 2 code))) 8)
       (char-code (aref c2g (1+ (* 2 code)))))))

(format t "~&== CIDToGIDMap probe (0 = .notdef = MISSING)~%")
(dolist (entry '((#x65E5 "KANJI hi")   (#x672C "KANJI hon") (#x8A9E "KANJI go")
                 (#x306E "HIRA no")    (#x30C6 "KATA te")   (#x3002 "IDEO period")
                 (#x300C "L cornerbkt")(#x3000 "IDEO space") (#x5433 "KANJI go2")
                 (#x0041 "LATIN A")))
  (destructuring-bind (code label) entry
    (format t "   U+~4,'0X ~14a -> GID ~5d ~a~%"
            code label (gid-of code) (if (zerop (gid-of code)) "  <== MISSING" ""))))

(defun jstr (&rest codes) (map 'string #'code-char codes))

(defparameter *s1* (jstr #x65E5 #x672C #x8A9E #x306E #x30C6 #x30B9 #x30C8))
(defparameter *s2* (jstr #x5433 #x8F29 #x306F #x732B #x3067 #x3042 #x308B #x3002
                         #x540D #x524D #x306F #x307E #x3060 #x7121 #x3044 #x3002))
(defparameter *s3* (concatenate 'string "Common Lisp "
                                (jstr #x3068 #x65E5 #x672C #x8A9E)))

(defparameter *out* (merge-pathnames "ja-probe.pdf" *base*))

(format t "~&== emitting PDF~%")
(pdf:with-document ()
  (pdf:with-page ()
    (let ((f (pdf:get-font (pdf::font-name *fm*))))
      (format t "   font class    : ~a~%" (type-of f))
      (format t "   encoding used : ~a~%" (type-of (pdf:encoding f)))
      (pdf:in-text-mode
        (pdf:set-font f 24.0)
        (pdf:move-text 60 760)
        (pdf:draw-text *s1*))
      (pdf:in-text-mode
        (pdf:set-font f 14.0)
        (pdf:move-text 60 710)
        (pdf:draw-text *s2*))
      (pdf:in-text-mode
        (pdf:set-font f 14.0)
        (pdf:move-text 60 680)
        (pdf:draw-text *s3*))))
  (pdf:write-document *out*))

(let ((size (with-open-file (in *out* :element-type '(unsigned-byte 8)) (file-length in))))
  (format t "~&==== RESULT ====~%")
  (format t "   output    : ~a~%" *out*)
  (format t "   PDF size  : ~:d bytes (~,2f MB)~%" size (/ size 1048576.0))
  (format t "   font was  : ~:d bytes (~,2f MB)~%"
          (pdf::length1 *fm*) (/ (pdf::length1 *fm*) 1048576.0))
  (format t "   overhead  : font is ~,1f%% of the PDF~%"
          (* 100.0 (/ (pdf::length1 *fm*) size))))
