;;;; ruby.lisp -- ルビを実際に PDF に描くデモ (段1: モノルビ)
;;;;
;;;; 最小のルビ入力パス: units の並びを item 列にする。
;;;;   要素が整数        → 通常の1字
;;;;   要素が (親 . ルビ列) → モノルビ (親コード + ルビのコードポイント列)
;;;; 日本語リテラルはソースに置かず、コードポイントで書く (ja-pdf と同じ方針)。
;;;; これが将来の S 式文書層 (B) の最小の芽。

(in-package #:kern)

(defun unit-lead-code (u)
  "UNIT の先頭の親コードポイント (overhang の except-kanji 判定に使う)。"
  (cond ((integerp u) u)
        ((and (consp u) (member (first u) '(:group :jukugo))) (first (second u)))
        (t (first u))))

(defun ruby-demo-items (font size units)
  "UNITS を item 列にする。要素:
     整数                     → 通常字
     (親 . ルビコード列)       → モノルビ (ルビ>親なら隣が仮名の側へ overhang)
     (:group (親列) (ルビ列)) → グループルビ
   隣接和文の間に JFM のクラス対 glue を入れる。
   ★モノルビの overhang 可否は隣接ユニットの先頭が漢字か否かで決める (except-kanji)。
     隣が無い/漢字なら食い込まない、仮名等なら食い込む。行頭行末の抑制は未実装。"
  (let* ((rs (default-ruleset))
         (vec (coerce units 'vector))
         (n (length vec))
         (items '()) (prev nil))
    (dotimes (i n)
      (let ((u (aref vec i)))
        (multiple-value-bind (box code)
            (cond
              ((integerp u) (values (emit-char-box rs font size u 0 0) u))
              ((eq (first u) :group)
               (values (group-ruby-box font size
                                       (map 'string #'code-char (second u))
                                       (map 'string #'code-char (third u)))
                       (first (second u))))
              ((eq (first u) :jukugo)
               ;; (:jukugo (親コード列) (親字ごとのルビコード列 の list))
               (values (jukugo-ruby-box font size
                                        (map 'string #'code-char (second u))
                                        (mapcar (lambda (cs) (map 'string #'code-char cs))
                                                (third u)))
                       (first (second u))))
              (t (let* ((lc (when (> i 0)        (unit-lead-code (aref vec (1- i)))))
                        (rc (when (< (1+ i) n)   (unit-lead-code (aref vec (1+ i))))))
                   (values (mono-ruby-box font size (first u)
                                          (map 'string #'code-char (rest u))
                                          :overhang-left-p  (and lc (not (kanji-code-p lc)))
                                          :overhang-right-p (and rc (not (kanji-code-p rc))))
                           (first u)))))
          (let ((class (char-class-of rs code)))
            (when prev
              (let ((g (inter-glue rs prev class size)))
                (when g (push g items))))
            (push box items)
            (setf prev class)))))
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
        ((eq (first u) :jukugo)
         (dolist (c (second u)) (push c codes))
         (dolist (cs (third u)) (dolist (c cs) (push c codes))))
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
        #x30C6 #x30B9 #x30C8          ; テスト
        #x3000                        ; 全角スペース
        ;; overhang 対比: ルビ>親の 都(みやこ)。隣が仮名(の/に)=食い込む
        #x306E (list #x90FD #x307F #x3084 #x3053) #x306B  ; の 都(みやこ) に
        #x3000                        ; 全角スペース
        ;; 隣が漢字(京/府)=食い込まない (except-kanji) → 箱がルビ幅に広がる
        #x4EAC (list #x90FD #x307F #x3084 #x3053) #x5E9C  ; 京 都(みやこ) 府
        #x3000                        ; 全角スペース
        ;; 熟語B 二(に)十(じゅう): じゅう>十 だが熟語内で融通・平坦化して釣り合う
        (list :jukugo (list #x4E8C #x5341)
              (list (list #x306B) (list #x3058 #x3085 #x3046)))
        #x3000                        ; 全角スペース
        ;; 熟語A 名(な)前(まえ): どちらも親に収まる → 各ルビを親字上に個別中央
        (list :jukugo (list #x540D #x524D)
              (list (list #x306A) (list #x307E #x3048)))))

(defun run-ruby-pdf (&key (size 24) (units *ruby-sample*))
  "ルビ付きの1行を組んで PDF に描く。"
  (let* ((fm    (pdf:load-ttf-font *ttf*))
         (font  (pdf:get-font (pdf::font-name fm)))
         (codes (ruby-demo-codes units))
         (width (* size 15))
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
