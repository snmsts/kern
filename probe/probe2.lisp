;;; cl-pdf Japanese probe, round 2 -- pure ASCII source on purpose.
(require :asdf)

(defparameter *base* #P"C:/Users/snmst/AppData/Local/Temp/claude/C--msys64-home-snmst-work-nanka/09cbee8f-7469-442f-84d4-ce36440c0801/scratchpad/")
(push (merge-pathnames "cl-pdf/" *base*) asdf:*central-registry*)

(handler-bind ((warning #'muffle-warning))
  (funcall (read-from-string "ql:quickload") '(:cl-pdf :zpb-ttf) :silent t))

;;; --- WORKAROUND for upstream bug: pdf.lisp:577 calls PDF::EXTENDED-ASCII-P,
;;; --- which is only defined in pdf-parser.lisp, a component of the SEPARATE
;;; --- :cl-pdf-parser system. Loading :cl-pdf alone => write-document dies.
(unless (fboundp (find-symbol "EXTENDED-ASCII-P" :pdf))
  (eval (read-from-string
         "(defun pdf::extended-ascii-p (char) (<= 128 (char-code char) 253))"))
  (format t "~&[patched] defined missing PDF::EXTENDED-ASCII-P~%"))

(defparameter *ttf* #P"C:/Windows/Fonts/yumin.ttf")
(defparameter *fm* (pdf:load-ttf-font *ttf*))
(format t "~&font: ~s  glyphs=~:d  size=~:d bytes~%"
        (pdf::font-name *fm*) (hash-table-count (pdf::characters *fm*)) (pdf::length1 *fm*))

(defun gid-of (code)
  (let ((c2g (pdf::c2g *fm*)))
    (+ (ash (char-code (aref c2g (* 2 code))) 8)
       (char-code (aref c2g (1+ (* 2 code)))))))

;;; ---- Is the reverse glyph->codepoint walk losing code points? ----
(format t "~&==== cl-pdf c2g  vs  zpb-ttf cmap (ground truth) ====~%")
(zpb-ttf:with-font-loader (fl *ttf*)
  (dolist (entry '((#x65E5 "hi/nichi")(#x672C "hon")(#x3000 "IDEOGRAPHIC SPACE")
                   (#x3002 "ideo period")(#x4E00 "ichi")(#x570B "kuni kyuji")))
    (destructuring-bind (code label) entry
      (let* ((ch (code-char code))
             (exists (zpb-ttf:glyph-exists-p ch fl))
             (g (and exists (zpb-ttf:find-glyph ch fl)))
             (true-gid (and g (zpb-ttf:font-index g)))
             (reported-cp (and g (zpb-ttf:code-point g))))
        (format t "  U+~4,'0X ~18a cmap-gid=~5a  c2g-gid=~5d  glyph's code-point=~a~a~%"
                code label (or true-gid "-") (gid-of code)
                (if reported-cp (format nil "U+~4,'0X" reported-cp) "-")
                (cond ((not exists) "   [not in font]")
                      ((zerop (gid-of code)) "   <== LOST BY cl-pdf")
                      (t ""))))))

  ;; Quantify the loss across the ranges that matter for Japanese.
  (format t "~&==== how many code points does cl-pdf lose? ====~%")
  (dolist (range '((#x3040 #x309F "Hiragana")
                   (#x30A0 #x30FF "Katakana")
                   (#x3000 #x303F "CJK punctuation")
                   (#xFF00 #xFFEF "Halfwidth/Fullwidth forms")
                   (#x4E00 #x9FFF "CJK Unified Ideographs")))
    (destructuring-bind (lo hi label) range
      (let ((in-font 0) (lost 0))
        (loop for code from lo to hi
              for ch = (code-char code)
              when (zpb-ttf:glyph-exists-p ch fl)
                do (incf in-font)
                   (when (zerop (gid-of code)) (incf lost)))
        (format t "  ~28a in font: ~5d   lost by cl-pdf: ~5d  (~,1f%)~%"
                label in-font lost (if (plusp in-font) (* 100.0 (/ lost in-font)) 0))))))

;;; ---- Now actually emit the PDF ----
(defun jstr (&rest codes) (map 'string #'code-char codes))
(defparameter *s1* (jstr #x65E5 #x672C #x8A9E #x306E #x30C6 #x30B9 #x30C8))
(defparameter *s2* (jstr #x5433 #x8F29 #x306F #x732B #x3067 #x3042 #x308B #x3002
                         #x540D #x524D #x306F #x307E #x3060 #x7121 #x3044 #x3002))
(defparameter *s3* (concatenate 'string "Common Lisp "
                                (jstr #x3068 #x65E5 #x672C #x8A9E)))
(defparameter *out* (merge-pathnames "ja-probe.pdf" *base*))

(pdf:with-document ()
  (pdf:with-page ()
    (let ((f (pdf:get-font (pdf::font-name *fm*))))
      (pdf:in-text-mode (pdf:set-font f 24.0) (pdf:move-text 60 760) (pdf:draw-text *s1*))
      (pdf:in-text-mode (pdf:set-font f 14.0) (pdf:move-text 60 710) (pdf:draw-text *s2*))
      (pdf:in-text-mode (pdf:set-font f 14.0) (pdf:move-text 60 680) (pdf:draw-text *s3*))))
  (pdf:write-document *out*))

(let ((size (with-open-file (in *out* :element-type '(unsigned-byte 8)) (file-length in))))
  (format t "~&==== RESULT ====~%")
  (format t "  PDF written : ~a~%" *out*)
  (format t "  PDF size    : ~:d bytes (~,2f MB)~%" size (/ size 1048576.0))
  (format t "  font bytes  : ~:d (~,1f%% of the PDF)~%"
          (pdf::length1 *fm*) (* 100.0 (/ (pdf::length1 *fm*) size))))
