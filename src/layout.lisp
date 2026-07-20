;;;; layout.lisp -- テキスト → item 列 → 行 → 位置の決まったグリフ
;;;;
;;;; ★この層だけが言語を知っている。行分割器とグルー解決器は何も知らない。
;;;;
;;;; ★ここに置いている表は【暫定の手書き】。本物は jfm-jlreq.lua (BSD-2) の
;;;;   約25クラス × クラス対の glue/kern 行列。差し替える前提で、
;;;;   同じ形 (クラス → クラス対 → glue) にしてある。
;;;;   数値は jfm-jlreq.lua に合わせてある:
;;;;     kanjiskip  = {0,    0.25, 0    }
;;;;     xkanjiskip = {0.25, 0.25, 0.125}
;;;;
;;;; ★この層は cl-pdf を参照しない。フォントには総称関数で問い合わせる。

(in-package #:typeset)

;;; ---------------------------------------------------------------------------
;;; メトリクス協定 -- バックエンドが実装する
;;; ---------------------------------------------------------------------------

(defgeneric glyph-advance (font char size)
  (:documentation "1文字の送り幅。和文の全角なら SIZE と等しくなるはず。"))

(defgeneric font-ascent* (font size)
  (:documentation "ベースラインから上。行送りの算出に使う。"))

;;; ---------------------------------------------------------------------------
;;; 文字クラス (暫定)
;;; ---------------------------------------------------------------------------

(defparameter *kinsoku-head*
  '(#x3002 #x3001 #x300D #x300F #xFF09 #xFF01 #xFF1F #x30FC #x3005
    #x3041 #x3043 #x3045 #x3047 #x3049 #x3063 #x3083 #x3085 #x3087
    #x30A1 #x30A3 #x30A5 #x30A7 #x30A9 #x30C3 #x30E3 #x30E5 #x30E7)
  "行頭禁則。")

(defparameter *kinsoku-tail*
  '(#x300C #x300E #xFF08)
  "行末禁則。")

(defun char-class (code)
  (cond ((member code '(#x3002 #x3001))          :kuten)   ; 句点・読点
        ((member code '(#x300D #x300F #xFF09))   :close)   ; 閉じ括弧
        ((member code '(#x300C #x300E #xFF08))   :open)    ; 開き括弧
        ((= code #x3000)                         :ideographic-space)
        ((= code #x20)                           :space)
        ((< code #x2E80)                         :latin)   ; 乱暴だが暫定
        (t                                       :ideographic)))

(defun japanese-p (class)
  (member class '(:kuten :close :open :ideographic :ideographic-space)))

;;; 約物は全角の枠に字面が半角ぶん寄り、残りがアキになる。
;;; そのアキが詰めの原資 (追い込み)。段階は jfm-jlreq の priority に対応させる。
(defun space-after (class)  (if (member class '(:kuten :close)) 1/2 0))
(defun space-before (class) (if (eq class :open) 1/2 0))

(defparameter *kanjiskip*  '(0 1/4 0)     "自然幅・伸び・縮み (em)。")
(defparameter *xkanjiskip* '(1/4 1/4 1/8) "和欧間。四分アキ。")

(defun gap-between (class-a class-b size)
  "A と B のあいだに入れる glue を返す。無ければ NIL。
   ★jfm-jlreq のクラス対表を、クラスが少ない版で真似たもの。"
  (let ((after (space-after class-a))
        (before (space-before class-b)))
    (cond
      ;; 約物のアキ。詰められる。段階を分ける (句点後が先、括弧が後)
      ((plusp (+ after before))
       (let ((amount (* size (+ after before))))
         (make-glue amount :shrink amount
                           :shrink-priority (if (plusp after) 2 1))))
      ;; 和欧間
      ((or (and (japanese-p class-a) (eq class-b :latin))
           (and (eq class-a :latin) (japanese-p class-b)))
       (destructuring-bind (nat str shr) *xkanjiskip*
         (make-glue (* size nat) :stretch (* size str) :shrink (* size shr)
                                 :shrink-priority 0 :stretch-priority 0)))
      ;; 和文字間。自然幅 0、伸びのみ。均等割りの担い手
      ((and (japanese-p class-a) (japanese-p class-b))
       (destructuring-bind (nat str shr) *kanjiskip*
         (make-glue (* size nat) :stretch (* size str) :shrink (* size shr)
                                 :shrink-priority -1 :stretch-priority 0)))
      (t nil))))

;;; ---------------------------------------------------------------------------
;;; item 生成
;;; ---------------------------------------------------------------------------

(defun text-items (codes font size &key (kinsoku t))
  "コードポイント列を item 列にする。source-start/end も埋める (逆写像)。

   ★禁則は『この位置で切ってはいけない』という一つの判定に畳める:
       行頭禁則 (B を行頭に置かない) = A と B のあいだで切らない
       行末禁則 (A を行末に置かない) = A と B のあいだで切らない
     penalty を glue の【前】に置くと、glue も penalty 自身も分割点でなくなる。"
  (let ((n (length codes))
        (items '()))
    (dotimes (i n)
      (let* ((c (aref codes i))
             (class (char-class c)))
        (if (eq class :space)
            ;; 欧文の単語間。フォントの空白幅を自然幅にする
            (let ((w (glyph-advance font (code-char c) size)))
              (push (make-glue w :stretch (/ w 2) :shrink (/ w 3)
                                 :source-start i :source-end (1+ i))
                    items))
            (push (make-glyph-box
                   ;; 約物は字面ぶんだけ。残りのアキは前後の glue が持つ
                   (- (glyph-advance font (code-char c) size)
                      (* size (+ (space-after class) (space-before class))))
                   (string (code-char c))
                   :source-start i :source-end (1+ i))
                  items))
        (when (< (1+ i) n)
          (let* ((next (aref codes (1+ i)))
                 (next-class (char-class next))
                 (forbid (and kinsoku
                              (or (member next *kinsoku-head*)
                                  (member c *kinsoku-tail*))))
                 (glue (unless (or (eq class :space) (eq next-class :space))
                         (gap-between class next-class size))))
            (when (and forbid glue)
              (push (make-penalty +inf-penalty+) items))
            (when glue (push glue items))))))
    (nreverse items)))

;;; ---------------------------------------------------------------------------
;;; 行への割り付け
;;; ---------------------------------------------------------------------------

(defstruct (laid-line (:conc-name line-))
  (glyphs '())     ; ((x . 文字列) ...) x は行頭からの相対位置
  ;; 実寸が正になった glue。((x . 幅) ...)
  ;; 描画には要らない (グリフの x に織り込み済み) が、
  ;; 検証と診断には要る。これが無いと「和欧間のアキが本当に入ったか」を
  ;; 出力から確かめられない。
  (gaps '())
  (advance 0)      ; 行の実寸合計。均等割りできているかの確認用
  (ratio 0)
  (status :exact))

(defun layout-paragraph (codes font size line-width &key (kinsoku t)
                                                         (params (make-break-params)))
  "テキストを行に割り付け、各グリフの位置を確定させる。
   返り値は LAID-LINE の並び。"
  (let* ((raw (text-items codes font size :kinsoku kinsoku))
         (items (coerce (finish-paragraph raw) 'vector))
         (breaks (break-paragraph items line-width :params params :finish nil))
         (start 0)
         (lines '()))
    (dolist (br breaks)
      (let ((b (getf br :position)))
        (multiple-value-bind (sizes status) (set-glue items line-width :start start :end b)
          (let ((x 0) (glyphs '()) (gaps '()))
            (loop for i from start below b
                  for k from 0
                  for item = (aref items i)
                  do (cond ((typep item 'glyph-box)
                            (push (cons x (box-glyphs item)) glyphs))
                           ((and (typep item 'glue) (plusp (aref sizes k)))
                            (push (cons x (aref sizes k)) gaps)))
                     (incf x (aref sizes k)))
            (push (make-laid-line :glyphs (nreverse glyphs)
                                  :gaps (nreverse gaps)
                                  :advance x
                                  :ratio (getf br :ratio)
                                  :status status)
                  lines)))
        (setf start (skip-discardables items (1+ b)))))
    (nreverse lines)))
