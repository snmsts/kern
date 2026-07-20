;;;; layout.lisp -- テキスト → item 列 → 行 → 位置の決まったグリフ
;;;;
;;;; ★この層だけが言語を知っている。行分割器とグルー解決器は何も知らない。
;;;;
;;;; ★文字クラスと空き量は jfm-jlreq.lua (BSD-2) から読んだ ruleset を使う。
;;;;   手書きの暫定表は撤去した。約25クラス × クラス対の glue 行列がそのまま効く。
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
;;; 既定の ruleset
;;; ---------------------------------------------------------------------------

(defparameter *default-jfm*
  (asdf:system-relative-pathname "typeset" "vendor/jlreq/jfm-jlreq.lua")
  "既定の JFM。abenori/jlreq (BSD-2)。")

(defvar *ruleset* nil "現在の ruleset。default-ruleset が遅延構築する。")

(defun default-ruleset ()
  (or *ruleset* (setf *ruleset* (build-ruleset *default-jfm*))))

;;; jfm-jlreq の値は zw (=1em) 単位。SIZE を掛けて実寸にする。
(defun jfm-glue->glue (jg size &rest initargs)
  (apply #'make-glue (* size (jg-natural jg))
         :stretch (* size (jg-stretch jg))
         :shrink  (* size (jg-shrink jg))
         :stretch-priority (jg-stretch-priority jg)
         :shrink-priority  (jg-shrink-priority jg)
         :ratio (jg-ratio jg)
         initargs))

(defun latin-class-p (rs class) (declare (ignore rs)) (= class 27))
(defun ideographic-class-p (class)
  "JFM のクラスのうち、字間 (kanjiskip) 調整の対象になる和文クラス。
   jfm-jlreq のコメント: 4,9,10,11,15,16,19(=0) との間は (x)kanjiskip。"
  (member class '(0 4 9 10 11 15 16)))

(defun inter-glue (rs class-a class-b size)
  "A と B のあいだに入れる glue。まず JFM のクラス対表を引き、
   無ければ kanjiskip / xkanjiskip にフォールバックする。"
  (let ((jg (class-glue rs class-a class-b)))
    (cond
      (jg (jfm-glue->glue jg size))
      ;; 和欧間
      ((or (and (ideographic-class-p class-a) (latin-class-p rs class-b))
           (and (latin-class-p rs class-a) (ideographic-class-p class-b)))
       (jfm-glue->glue (rs-xkanjiskip rs) size))
      ;; 和文字間
      ((and (ideographic-class-p class-a) (ideographic-class-p class-b))
       (jfm-glue->glue (rs-kanjiskip rs) size))
      (t nil))))

;;; ---------------------------------------------------------------------------
;;; item 生成
;;; ---------------------------------------------------------------------------

(defun forbid-break-p (rs a b)
  "A と B のあいだで切ってはいけないか。
   行頭禁則 (B を行頭に置かない) と 行末禁則 (A を行末に置かない) を畳む。"
  (or (gethash b (rs-line-start-forbidden rs))
      (gethash a (rs-line-end-forbidden rs))))

(defun text-items (codes font size &key (kinsoku t) (ruleset (default-ruleset)))
  "コードポイント列を item 列にする。source-start/end も埋める (逆写像)。

   ★禁則は『この位置で切ってはいけない』という一つの判定に畳める。
     penalty を glue の【前】に置くと、glue も penalty 自身も分割点でなくなる。"
  (let ((rs ruleset)
        (n (length codes))
        (items '()))
    (dotimes (i n)
      (let* ((c (aref codes i))
             (class (char-class-of rs c)))
        (if (= c #x20)
            ;; 欧文の単語間。フォントの空白幅を自然幅にする
            (let ((w (glyph-advance font (code-char c) size)))
              (push (make-glue w :stretch (/ w 2) :shrink (/ w 3)
                                 :source-start i :source-end (1+ i))
                    items))
            (push (make-glyph-box (glyph-advance font (code-char c) size)
                                  (string (code-char c))
                                  :source-start i :source-end (1+ i))
                  items))
        (when (< (1+ i) n)
          (let* ((next (aref codes (1+ i)))
                 (next-class (char-class-of rs next))
                 (forbid (and kinsoku (forbid-break-p rs c next)))
                 (glue (unless (or (= c #x20) (= next #x20))
                         (inter-glue rs class next-class size))))
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
