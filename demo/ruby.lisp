;;;; ruby.lisp -- ルビを実際に PDF に描くデモ (段1: モノルビ)
;;;;
;;;; 最小のルビ入力パス: units の並びを item 列にする。
;;;;   要素が整数        → 通常の1字
;;;;   要素が (親 . ルビ列) → モノルビ (親コード + ルビのコードポイント列)
;;;; 日本語リテラルはソースに置かず、コードポイントで書く (ja-pdf と同じ方針)。
;;;; これが将来の S 式文書層 (B) の最小の芽。

(in-package #:kern)

(defun ruby-demo-items (font size units)
  "UNITS を item 列にする。要素:
     整数                     → 通常字
     (親 . ルビコード列)       → モノルビ
     (:group (親列) (ルビ列)) → グループルビ
   隣接和文の間に JFM のクラス対 glue を入れる。"
  (let ((rs (default-ruleset)) (items '()) (prev nil))
    (dolist (u units)
      (multiple-value-bind (box code)
          (cond
            ((integerp u) (values (emit-char-box rs font size u 0 0) u))
            ((eq (first u) :group)
             (values (group-ruby-box font size
                                     (map 'string #'code-char (second u))
                                     (map 'string #'code-char (third u)))
                     (first (second u))))
            (t (values (mono-ruby-box font size (first u)
                                      (map 'string #'code-char (rest u)))
                       (first u))))
        (let ((class (char-class-of rs code)))
          (when prev
            (let ((g (inter-glue rs prev class size)))
              (when g (push g items))))
          (push box items)
          (setf prev class))))
    (nreverse items)))

(defun ruby-demo-codes (units)
  "サブセット化に要る全コードポイント (親 + ルビ)。"
  (let ((codes '()))
    (dolist (u units)
      (cond
        ((integerp u) (push u codes))
        ((eq (first u) :group)
         (dolist (c (second u)) (push c codes))
         (dolist (c (third u))  (push c codes)))
        (t (push (first u) codes)
           (dolist (r (rest u)) (push r codes)))))
    (coerce (nreverse codes) 'vector)))

;;; 「漢字にルビを振る テスト」相当。親コード + ルビのコードポイント列。
;;;   漢(かん) 字(じ) に 振(ふ) る 　テ ス ト
(defparameter *ruby-sample*
  (list (list #x6F22 #x304B #x3093)   ; 漢 + かん
        (list #x5B57 #x3058)          ; 字 + じ
        #x306B                        ; に
        (list #x632F #x3075)          ; 振 + ふ
        #x308B                        ; る
        #x3000                        ; 全角スペース
        (list :group (list #x5927 #x4EBA) (list #x304A #x3068 #x306A)) ; 大人(おとな) グループ
        #x3000                        ; 全角スペース
        #x30C6 #x30B9 #x30C8))        ; テスト

(defun run-ruby-pdf (&key (size 24) (units *ruby-sample*))
  "ルビ付きの1行を組んで PDF に描く。"
  (let* ((fm    (pdf:load-ttf-font *ttf*))
         (font  (pdf:get-font (pdf::font-name fm)))
         (codes (ruby-demo-codes units))
         (width (* size 12))
         (items (coerce (finish-paragraph (ruby-demo-items font size units)) 'vector))
         (lines (layout-items items width size)))
    (format t "~&=== ルビ PDF デモ ===~%")
    (format t "  フォント : ~a~%" (pdf::font-name fm))
    (format t "  級数     : ~,1fpt / 版面 ~,1fpt~%" (float size) (float width))
    (format t "  行数     : ~d~%" (length lines))
    (format t "  ルビ箱   : ~d~%"
            (count-if (lambda (it) (typep it 'ruby-box)) items))
    ;; 各行のグリフを y でグループ表示 (親=y0, ルビ=y>0)
    (dolist (l lines)
      (let ((base (count-if (lambda (g) (zerop (placed-y g))) (line-glyphs l)))
            (ruby (count-if (lambda (g) (plusp (placed-y g))) (line-glyphs l))))
        (format t "    line: 親グリフ ~d / ルビグリフ ~d  status=~a~%"
                base ruby (line-status l))))
    (install-subset fm *ttf* codes)
    (let ((pdf-path (rel "demo/ruby.pdf")))
      (pdf:with-document ()
        (pdf:with-page ()
          (draw-lines lines font size :x 60 :y 700)
          (install-tounicode font codes))
        (pdf:write-document pdf-path))
      (format t "  PDF      : ~a (~:d bytes)~%" pdf-path
              (with-open-file (in pdf-path :element-type '(unsigned-byte 8))
                (file-length in))))))
