;;; Round 3: how bad is the Kangxi-radical collision in practice, and does the fix work?
(require :asdf)
(defparameter *base* #P"C:/Users/snmst/AppData/Local/Temp/claude/C--msys64-home-snmst-work-nanka/09cbee8f-7469-442f-84d4-ce36440c0801/scratchpad/")
(push (merge-pathnames "cl-pdf/" *base*) asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning))
  (funcall (read-from-string "ql:quickload") '(:cl-pdf :zpb-ttf) :silent t))

(defparameter *ttf* #P"C:/Windows/Fonts/yumin.ttf")
(defparameter *fm* (pdf:load-ttf-font *ttf*))

(defun gid-of (code)
  (let ((c2g (pdf::c2g *fm*)))
    (+ (ash (char-code (aref c2g (* 2 code))) 8)
       (char-code (aref c2g (1+ (* 2 code)))))))

;; The 60 most frequent kanji in ordinary Japanese text.
(defparameter *common*
  '(#x65E5 #x4E00 #x5927 #x5E74 #x4E2D #x4EBA #x672C #x4E0A #x51FA #x8005
    #x5730 #x696D #x5206 #x751F #x884C #x65B9 #x540C #x4E8B #x81EA #x6642
    #x9AD8 #x524D #x529B #x5185 #x4E8C #x4E09 #x5341 #x56FD #x624B #x5186
    #x6728 #x6C34 #x706B #x5C71 #x5DDD #x7530 #x76EE #x53E3 #x5FC3 #x5973
    #x5B50 #x6708 #x91D1 #x571F #x8ECA #x898B #x8A00 #x8DB3 #x624D #x77F3
    #x7ACB #x7530 #x7537 #x5B57 #x738B #x767D #x96E8 #x7A7A #x82B1 #x5C0F))

(format t "~&==== impact on the 60 most common kanji ====~%")
(let ((lost '()))
  (dolist (c *common*)
    (when (zerop (gid-of c)) (push c lost)))
  (setf lost (nreverse lost))
  (format t "  broken: ~d of ~d  (~,1f%)~%"
          (length lost) (length *common*)
          (* 100.0 (/ (length lost) (length *common*))))
  (format t "  the broken ones: ~{U+~4,'0X ~}~%" lost))

;;; ---- Does the forward cmap walk fix it? ----
;;; cl-pdf builds c2g by iterating GLYPHS and asking each its code-point
;;; (zpb-ttf-load.lisp:36-39). Shared glyphs => only one code point wins.
;;; Fix: iterate CODE POINTS and ask the cmap for the glyph.
(format t "~&==== rebuilding c2g by forward cmap walk ====~%")
(defparameter *added* 0)
(zpb-ttf:with-font-loader (fl *ttf*)
  (let ((c2g (pdf::c2g *fm*)))
    (loop for code from 0 to #xFFFE
          for ch = (code-char code)
          when (and (zerop (gid-of code)) (zpb-ttf:glyph-exists-p ch fl))
            do (let ((gid (zpb-ttf:font-index (zpb-ttf:find-glyph ch fl))))
                 (when (plusp gid)
                   (setf (aref c2g (* 2 code))      (code-char (ash gid -8))
                         (aref c2g (1+ (* 2 code))) (code-char (logand gid #xFF)))
                   (incf *added*))))))
(format t "  code points recovered: ~:d~%" *added*)

(format t "~&==== recheck after fix ====~%")
(let ((still-lost 0))
  (dolist (c *common*) (when (zerop (gid-of c)) (incf still-lost)))
  (format t "  broken among the 60 common kanji: ~d~%" still-lost))
(dolist (c '(#x65E5 #x4E00 #x3000))
  (format t "  U+~4,'0X -> GID ~d~%" c (gid-of c)))
