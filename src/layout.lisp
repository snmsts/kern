;;;; layout.lisp -- テキスト → item 列 → 行 → 位置の決まったグリフ
;;;;
;;;; ★この層だけが言語を知っている。行分割器とグルー解決器は何も知らない。
;;;;
;;;; ★文字クラスと空き量は jfm-jlreq.lua (BSD-2) から読んだ ruleset を使う。
;;;;   手書きの暫定表は撤去した。約25クラス × クラス対の glue 行列がそのまま効く。
;;;;
;;;; ★この層は cl-pdf を参照しない。フォントには総称関数で問い合わせる。

(in-package #:kern)

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
  (asdf:system-relative-pathname "kern" "vendor/jlreq/jfm-jlreq.lua")
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

(defun class-glyph-width (rs class size)
  "そのクラスの字面幅 (JFM の width)。約物は 0.5em 等。
   フォントの実送り幅ではなく JFM 上の枠幅を返す。全角なら 1。"
  (let ((c (gethash class (rs-classes rs))))
    (if c (* size (jc-width c)) size)))

(defun class-align (rs class)
  "字面が全角枠のどちら寄りか。'left / 'right / 'middle / NIL。
   JFM の align。描画時に字面をどこに置くかに使う。"
  (let ((c (gethash class (rs-classes rs))))
    (when c
      (let ((a (jc-align c)))
        (cond ((equal a "left") :left)
              ((equal a "right") :right)
              ((equal a "middle") :middle))))))

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

(defun break-prohibited-p (rs a b kinsoku)
  "A と B のあいだで【切ってはいけない】か。2つの規則を合流させる:
     - 禁則 (JFM 由来、JLReq の tailoring): 和文の行頭・行末禁則
     - UAX #14 (サブセット): 欧文・数字・記号の分割可否
   どちらかが禁じれば切れない。両者は和文では一致し、欧文では kinsoku が沈黙する。"
  (or (and kinsoku
           (or (gethash b (rs-line-start-forbidden rs))
               (gethash a (rs-line-end-forbidden rs))))
      (uax14-prohibited-p (break-class a) (break-class b))))

(defun emit-char-box (rs font size c start end)
  "1文字を glyph-box にする。約物は JFM の字面幅、align に応じたオフセット。
   欧文は比例幅なのでフォントの実送り幅そのまま。"
  (let ((class (char-class-of rs c)))
    (if (latin-class-p rs class)
        (make-glyph-box (glyph-advance font (code-char c) size) (string (code-char c))
                        :source-start start :source-end end)
        (let* ((full (glyph-advance font (code-char c) size))
               (face (class-glyph-width rs class size))
               (slack (- full face))
               (offset (case (class-align rs class)
                         (:right (- slack))
                         (:middle (- (/ slack 2)))
                         (t 0))))
          (make-glyph-box face (string (code-char c))
                          :glyph-offset offset
                          :source-start start :source-end end)))))

(defun text-items (codes font size &key (kinsoku t) (ruleset (default-ruleset)))
  "コードポイント列を item 列にする。source-start/end も埋める (逆写像)。

   ★全文字を1つずつ box にする。ラテンを run にまとめる特別扱いは無い。
     『欧単語が単語内で切れない』は UAX #14 の LB28 (英字どうし不可分) が引き受ける。
   ★各境界で2つを別々に決める:
     - 間隔  = JFM のクラス対 glue (inter-glue)
     - 可否  = 禁則 + UAX #14 (break-prohibited-p)
     間隔と可否は直交する。glue があってもそこで切れないことも、
     glue が無くても切れることもある。"
  (let ((rs ruleset)
        (n (length codes))
        (items '()))
    (dotimes (i n)
      (let ((c (aref codes i)))
        ;; その位置の item (空白は glue、他は box)
        (if (= c #x20)
            (let ((w (glyph-advance font (code-char c) size)))
              (push (make-glue w :stretch (/ w 2) :shrink (/ w 3)
                                 :source-start i :source-end (1+ i))
                    items))
            (push (emit-char-box rs font size c i (1+ i)) items))
        ;; 直後の文字との境界。両側とも非空白のときだけ。
        (when (and (< (1+ i) n)
                   (/= c #x20)
                   (/= (aref codes (1+ i)) #x20))
          (let* ((next (aref codes (1+ i)))
                 (glue (inter-glue rs (char-class-of rs c) (char-class-of rs next) size))
                 (prohibited (break-prohibited-p rs c next kinsoku)))
            (cond
              (glue
               ;; 間隔がある。切れないなら glue の前に禁止 penalty を置く。
               (when prohibited (push (make-penalty +inf-penalty+) items))
               (push glue items))
              ((not prohibited)
               ;; 間隔は無いが切ってよい (例: ハイフンの後)。幅0の分割点を置く。
               (push (make-penalty 0) items)))))))
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
                            ;; 描画位置は box 左端 + 字面オフセット
                            (push (cons (+ x (glyph-offset item)) (box-glyphs item)) glyphs))
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
