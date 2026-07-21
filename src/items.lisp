;;;; items.lisp -- 組版要素の模型
;;;;
;;;; 行分割器が見るのはこの3種類だけ。言語のことは何も知らない。
;;;; 言語規則は「どの item をどんな値で並べるか」に翻訳されて入ってくる。
;;;;
;;;; discretionary (分割すると中身が変わる箇所 = 欧文のハイフン挿入) は未実装。
;;;; 日本語 v1 には要らない。禁則は全て penalty で表現できる。欧文対応時に足す。

(defpackage #:kern
  (:use #:cl)
  (:export #:len
           #:item #:source-start #:source-end #:advance #:discardable-p
           #:box #:ascent #:descent #:protrusion
           #:glyph-box #:box-font #:box-glyphs #:glyph-offset
           #:glue #:stretch #:shrink #:stretch-order #:shrink-order
           #:stretch-priority #:shrink-priority #:glue-ratio
           #:penalty #:penalty-value #:flagged-p
           #:+inf-penalty+ #:+forced-break+
           #:make-box #:make-glue #:make-penalty #:make-glyph-box))

(in-package #:kern)

;;; 内部は抽象単位。バックエンドが DPI 換算する。
;;; 有理数にするのは丸め誤差の累積を避けるため。均等割りは除算を大量にやるので、
;;; 単精度で持つと行末が数百分の1ポイントずれて詰め処理の判定が揺れる。
;;; 速度が問題になったら「1pt = 65536」の固定小数点整数に落とす (TeX の sp と同じ手)。
(deftype len () 'rational)

(defclass item ()
  (;; 逆写像。印刷だけなら無駄だが後から入らない。
   ;; GUI のヒットテストとキャレット位置、および PDF の /ToUnicode がこれに乗る。
   (source-start :initarg :source-start :initform nil :accessor source-start)
   (source-end   :initarg :source-end   :initform nil :accessor source-end)))

;;; 既定を持たせて、行分割器が種類を気にせず合計を取れるようにする。
;;; box と glue は :accessor で上書きするので、そちらが優先される。
(defgeneric advance (item)
  (:method ((item item)) 0))
(defgeneric stretch (item)
  (:method ((item item)) 0))
(defgeneric shrink (item)
  (:method ((item item)) 0))
(defgeneric stretch-order (item)
  (:method ((item item)) 0))
(defgeneric shrink-order (item)
  (:method ((item item)) 0))

(defgeneric discardable-p (item)
  (:documentation
   "行頭で捨てられる要素か。分割の直後に続く glue / penalty は捨てられる。
    box は捨てられない。")
  (:method ((item item)) nil))

;;; ---------------------------------------------------------------------------
;;; box -- 固定寸法の中身。分割不可。
;;; ---------------------------------------------------------------------------
;;; 寸法の名前は縦組みを見越して advance / ascent / descent にしてある。
;;; 横組みなら advance = 幅、縦組みなら advance = 高さ。

(defclass box (item)
  ((advance :initarg :advance :initform 0 :accessor advance :type len)
   (ascent  :initarg :ascent  :initform 0 :accessor ascent  :type len)
   (descent :initarg :descent :initform 0 :accessor descent :type len)
   ;; 行端で版面外へはみ出してよい量。ぶら下げと欧文の突き出しが同じ機構に乗る。
   ;; ★行分割器から見える必要がある。ぶら下げると「入らなかった行が入る」ので
   ;;   分割の判断そのものが変わる。描画時の後処理では駄目。
   (protrusion :initarg :protrusion :initform 0 :accessor protrusion :type len)))

(defclass glyph-box (box)
  ((font   :initarg :font   :initform nil :accessor box-font)
   ;; ★入口は「文字」ではなく「整形済みグリフ列」。
   ;;   シェーピングは実装しないので当面ここには文字がそのまま入るが、
   ;;   型をグリフ列にしておくと将来 HarfBuzz を噛ませる合流点が確保される。
   (glyphs :initarg :glyphs :initform nil :accessor box-glyphs)
   ;; ★字面のオフセット。box の advance は JFM の字面幅 (約物なら 0.5em) だが、
   ;;   フォントの実グリフは全角の枠に入っている。align に応じて、枠内での
   ;;   描画位置を box の左端からずらす。
   ;;   例: 句点 (align=left) は字面が枠の左寄り = オフセット 0。
   ;;       始め括弧 (align=right) は字面が右寄り = 実グリフを左へ引く負のオフセット。
   (glyph-offset :initarg :glyph-offset :initform 0 :accessor glyph-offset :type len)))

;;; ---------------------------------------------------------------------------
;;; glue -- 伸縮する空き。均等割りの担い手。
;;; ---------------------------------------------------------------------------

(defclass glue (item)
  ((advance :initarg :advance :initform 0 :accessor advance :type len)
   (stretch :initarg :stretch :initform 0 :accessor stretch :type len)
   (shrink  :initarg :shrink  :initform 0 :accessor shrink  :type len)
   ;; TeX の無限位数。0=有限, 1=fil, 2=fill, 3=filll。
   ;; 中央揃え・右揃えを「無限に伸びる glue を端に置く」で表現するために要る。
   ;; cl-typesetting は +huge-number+ という有限の大きい数で代用しているが、
   ;; それだと「無限グルーがある行では有限グルーは伸びない」性質が出ない。
   (stretch-order :initarg :stretch-order :initform 0 :accessor stretch-order)
   (shrink-order  :initarg :shrink-order  :initform 0 :accessor shrink-order)
   ;; ★JLReq の詰めは段階的。jfm-jlreq.lua より:
   ;;     優先順位は，第n段階を 3-n に対応させる．
   ;;       段階   1, 2, 3, 4, 5, 6
   ;;     priority 2, 1, 0,-1,-2,-3
   ;;   段階の高いものから使い切り、足りなければ次の段階へ。
   ;;   ★stretch-order (無限位数) とは別物。
   ;;     無限位数は「無限が有限を支配する」= 有限側は一切伸びない。
   ;;     priority は「第1段階を使い切ってから第2段階」= 有限量の順序付き消費。
   ;;     両方要る。行分割器は総量しか見ないので影響を受けないが、
   ;;     グルー解決器 (set-glue) はこれを見る。
   (stretch-priority :initarg :stretch-priority :initform 0 :accessor stretch-priority)
   (shrink-priority  :initarg :shrink-priority  :initform 0 :accessor shrink-priority)
   ;; その空きが左右どちらの文字に属するかの比率 (jfm-jlreq では 0, 1/3, 1/2, 1)。
   ;; 行頭・行末で版面の端が揃うかに効く。
   (ratio :initarg :ratio :initform 1/2 :accessor glue-ratio)))

(defmethod discardable-p ((item glue)) t)

;;; ---------------------------------------------------------------------------
;;; penalty -- その位置で分割することの好ましくなさ。禁則はここに乗る。
;;; ---------------------------------------------------------------------------

(defconstant +inf-penalty+  10000 "これ以上は分割禁止。")
(defconstant +forced-break+ -10000 "これ以下は強制分割。")

(defclass penalty (item)
  ((value :initarg :value :initform 0 :accessor penalty-value)
   ;; TeX の flagged。連続する flagged break に追加ペナルティを課す。
   ;; 「ハイフンで終わる行が3行続く」を抑制する。
   (flagged-p :initarg :flagged-p :initform nil :accessor flagged-p)))

(defmethod discardable-p ((item penalty)) t)

;;; ---------------------------------------------------------------------------
;;; 構築子
;;; ---------------------------------------------------------------------------

(defun make-box (advance &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'box :advance advance initargs))

(defun make-glyph-box (advance glyphs &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'glyph-box :advance advance :glyphs glyphs initargs))

(defun make-glue (advance &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'glue :advance advance initargs))

(defun make-penalty (value &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'penalty :value value initargs))
