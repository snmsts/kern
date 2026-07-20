;;; パッチ検証。vendor/cl-pdf (修正済み) を読む。
;;; ★ extended-ascii-p の回避を意図的に入れていない。write-document が通れば patch 1 が効いている。
(require :asdf)

(defparameter *base* #P"C:/msys64/home/snmst/work/nanka/")
(push (merge-pathnames "vendor/cl-pdf/" *base*) asdf:*central-registry*)

(handler-bind ((warning #'muffle-warning))
  (funcall (read-from-string "ql:quickload") '(:cl-pdf :zpb-ttf) :silent t))

(when (fboundp (find-symbol "EXTENDED-ASCII-P" :pdf))
  (format t "~&[patch 1] PDF::EXTENDED-ASCII-P is defined by :cl-pdf alone -- OK~%"))

(defparameter *ttf* #P"C:/Windows/Fonts/yumin.ttf")
(defparameter *t0* (get-internal-real-time))
(defparameter *fm* (pdf:load-ttf-font *ttf*))
(format t "~&font ~s  load ~,1f sec~%" (pdf::font-name *fm*)
        (/ (- (get-internal-real-time) *t0*) internal-time-units-per-second))

(defun gid-of (code)
  (let ((c2g (pdf::c2g *fm*)))
    (+ (ash (char-code (aref c2g (* 2 code))) 8)
       (char-code (aref c2g (1+ (* 2 code)))))))

(defparameter *common*
  '(#x65E5 #x4E00 #x5927 #x5E74 #x4E2D #x4EBA #x672C #x4E0A #x51FA #x8005
    #x5730 #x696D #x5206 #x751F #x884C #x65B9 #x540C #x4E8B #x81EA #x6642
    #x9AD8 #x524D #x529B #x5185 #x4E8C #x4E09 #x5341 #x56FD #x624B #x5186
    #x6728 #x6C34 #x706B #x5C71 #x5DDD #x7530 #x76EE #x53E3 #x5FC3 #x5973
    #x5B50 #x6708 #x91D1 #x571F #x8ECA #x898B #x8A00 #x8DB3 #x624D #x77F3
    #x7ACB #x7530 #x7537 #x5B57 #x738B #x767D #x96E8 #x7A7A #x82B1 #x5C0F))

(format t "~&==== [patch 2a] c2g ====~%")
(let ((lost (count-if (lambda (c) (zerop (gid-of c))) *common*)))
  (format t "  頻出漢字 60 字中 GID 0: ~d  (修正前 36)~%" lost))
(dolist (c '(#x65E5 #x4E00 #x3000 #x672C))
  (format t "  U+~4,'0X -> GID ~d~%" c (gid-of c)))

;;; ★ここが今回の本題: 幅も復活しているか
(format t "~&==== [patch 2b] メトリクス (get-char-width) ====~%")
(let ((f (pdf:get-font (pdf::font-name *fm*))))
  (dolist (entry '((#x65E5 "hi   (旧: 部首に負けて幅 0)")
                   (#x4E00 "ichi (同上)")
                   (#x672C "hon  (元から無事)")
                   (#x3000 "ideographic space")))
    (destructuring-bind (code label) entry
      (format t "  U+~4,'0X ~28a width=~a~%"
              code label (pdf:get-char-width (code-char code) f))))
  (let ((w-hi (pdf:get-char-width (code-char #x65E5) f))
        (w-hon (pdf:get-char-width (code-char #x672C) f)))
    (format t "  → 日 と 本 の幅が一致: ~a~%" (if (= w-hi w-hon) "YES" "NO <== まだ壊れている"))))

;;; /W 配列に載っているか
(format t "~&==== [patch 2c] /W (cid-widths) ====~%")
(let ((cw (pdf::cid-widths *fm*)))
  (format t "  エントリ数: ~:d~%" (length cw))
  (dolist (code '(#x65E5 #x4E00 #x3000))
    (let ((pos (position code cw)))
      (format t "  U+~4,'0X -> ~a~%" code
              (if pos (format nil "/W に有り (~a)" (aref cw (1+ pos))) "無し <== まだ壊れている")))))

;;; 実際に PDF を出す
(defun jstr (&rest codes) (map 'string #'code-char codes))
(defparameter *out* (merge-pathnames "ja-patched.pdf" *base*))
(pdf:with-document ()
  (pdf:with-page ()
    (let ((f (pdf:get-font (pdf::font-name *fm*))))
      (pdf:in-text-mode (pdf:set-font f 24.0) (pdf:move-text 60 760)
        (pdf:draw-text (jstr #x65E5 #x672C #x8A9E #x306E #x30C6 #x30B9 #x30C8)))
      (pdf:in-text-mode (pdf:set-font f 14.0) (pdf:move-text 60 710)
        (pdf:draw-text (jstr #x4E00 #x4E8C #x4E09 #x5927 #x5C0F #x4EBA #x53E3 #x5C71 #x5DDD #x65E5)))
      (pdf:in-text-mode (pdf:set-font f 14.0) (pdf:move-text 60 680)
        (pdf:draw-text (concatenate 'string "Common Lisp " (jstr #x3068 #x65E5 #x672C #x8A9E))))))
  (pdf:write-document *out*))

(format t "~&==== RESULT ====~%")
(format t "  PDF: ~a~%" *out*)
(format t "  size: ~:d bytes~%"
        (with-open-file (in *out* :element-type '(unsigned-byte 8)) (file-length in)))
