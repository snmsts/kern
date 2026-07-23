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
           #:ruby-box #:ruby-placements #:ruby-mono #:mono-ruby-box
           #:ruby-group #:group-ruby-box #:jukugo-ruby-box #:distribute-even #:kanji-code-p
           #:ruby-suppress-overhang #:ruby-oh-left #:ruby-oh-right
           #:make-placed #:placed-x #:placed-y #:placed-size #:placed-string
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
;;; 均等配置 -- JLReq グループルビ (§3.3.6)。両端の空き = 字間の半分。
;;; ---------------------------------------------------------------------------
;;; WIDTHS の各要素を TOTAL-WIDTH に均等に散らし、各要素の左端 x の list を返す。
;;;   余り extra を 2N 単位に割り、字間=2単位・両端=1単位 (= 字間の半分)。
;;;   extra<=0 (詰まってる/ぴったり) なら中央寄せで連続配置。
;;; luatexja の fil 配分 1:2:…:2:1 と一致 (compare/ruby-group で実測)。

(defun distribute-even (widths total-width)
  (let* ((n (length widths))
         (glyphs-w (reduce #'+ widths :initial-value 0))
         (extra (- total-width glyphs-w)))
    (if (or (<= n 1) (<= extra 0))
        (let ((x (/ (max 0 extra) 2)) (xs '()))
          (dolist (w widths (nreverse xs)) (push x xs) (incf x w)))
        (let ((end (/ extra (* 2 n)))        ; 両端 = extra/(2N)
              (internal (/ extra n)))         ; 字間 = extra/N = 2*end
          (let ((x end) (xs '()))
            (dolist (w widths (nreverse xs))
              (push x xs)
              (incf x (+ w internal))))))))

;;; ---------------------------------------------------------------------------
;;; 配置済みグリフ -- (x y size . 文字列)
;;; ---------------------------------------------------------------------------
;;; laid-line の glyph 列と ruby-box の placements が共有する形。
;;;   x    : 行頭 (または box 左端) からの相対位置
;;;   y    : 親ベースラインから上が正 (通常グリフは 0、ルビは rise ぶん上)
;;;   size : そのグリフの実サイズ (通常は行のサイズ、ルビは半分)
;;; バックエンドはこの並びを歩き、各 x に size で文字列を置くだけでよい。

(defun make-placed (x y size string) (list x y size string))
(defun placed-x (g) (first g))
(defun placed-y (g) (second g))
(defun placed-size (g) (third g))
(defun placed-string (g) (fourth g))

;;; ---------------------------------------------------------------------------
;;; ruby-box -- 親文字の上にルビが乗った単位。ストリーム内では atomic な box。
;;; ---------------------------------------------------------------------------
;;; ★単一ベースラインの前提を破る唯一の箱。親グリフ (通常サイズ) と
;;;   ルビグリフ (半分サイズ) を別々の y に置く。行分割器は advance しか見ないので
;;;   これも普通の不可分 box として扱える。上への張り出しは ascent に出す。
;;; ★placements = ((x y size . 文字列) ...)。box 左端・親ベースライン基準。
;;;   y は上が正 (親グリフ y=0、ルビは rise ぶん上)。size はそのグリフの実サイズ。
;;;   描画バックエンドはこの並びを歩いて、各 x に size で文字列を置くだけ。

(defclass ruby-box (box)
  ((placements :initarg :placements :initform nil :accessor ruby-placements)
   ;; 行境界での overhang 抑制に要る情報。oh-left/right = 各側の食い込み量、
   ;; base-adv/ruby-adv = 素の親幅・ルビ幅 (作り直しに使う)。ルビ<=親なら oh は 0。
   (oh-left  :initarg :oh-left  :initform 0 :accessor ruby-oh-left)
   (oh-right :initarg :oh-right :initform 0 :accessor ruby-oh-right)
   (base-adv :initarg :base-adv :initform 0 :accessor ruby-base-adv)
   (ruby-adv :initarg :ruby-adv :initform 0 :accessor ruby-ruby-adv)))

(defun %ruby-place (base-adv ruby-adv oh-left oh-right)
  "ルビの水平配置。(values 箱advance 親x ルビx) を返す。
   ルビ<=親: 箱=親幅・親x=0・ルビ中央。
   ルビ>親: 箱=ルビ幅−食込左−食込右 (親幅が下限)・親中央・ルビは −食込左 から。"
  (if (> ruby-adv base-adv)
      (let ((adv (max base-adv (- ruby-adv oh-left oh-right))))
        (values adv (/ (- adv base-adv) 2) (- oh-left)))
      (values base-adv 0 (/ (- base-adv ruby-adv) 2))))

(defun ruby-mono (base-advance base-ascent base-descent base-string base-size
                  ruby-advance ruby-string ruby-size ruby-ascent
                  &key (gap 0) (overhang-left 0) (overhang-right 0))
  "モノルビ (親1字 + ルビ1組) の ruby-box。

   ★ルビの baseline は 親ascent + ルビdescent + gap の高さ。ルビ descent 分だけ浮かせ、
     ルビ字面の下端が親 ascent に接する (luatexja と一致、compare/ で実測)。
   ★ルビ<=親: 箱=親幅、親 x=0・ルビ中央。
   ★ルビ>親: OVERHANG-LEFT/RIGHT は各側で隣へ食い込ませてよい量 (呼び手が上限つきで決める)。
     箱 advance = ルビ幅 − 食い込み左 − 食い込み右 (親幅を下回らない)。ルビは箱左端から
     −食い込み左 の位置に置き、両隣の領域へはみ出す。両側フル食い込みなら箱=親幅
     (luatexja の『箱=親幅 + ルビ shifted』と一致)。食い込み0なら箱=ルビ幅 (単独/漢字隣)。"
  (let* ((ruby-descent (- ruby-size ruby-ascent))
         (rise         (+ base-ascent ruby-descent gap))
         (over         (> ruby-advance base-advance))
         (ol           (if over overhang-left 0))
         (or*          (if over overhang-right 0)))
    (multiple-value-bind (adv base-x ruby-x)
        (%ruby-place base-advance ruby-advance ol or*)
      (make-instance 'ruby-box
                     :advance adv :ascent (+ rise ruby-ascent) :descent base-descent
                     :oh-left ol :oh-right or* :base-adv base-advance :ruby-adv ruby-advance
                     :placements (list (make-placed base-x 0    base-size base-string)
                                       (make-placed ruby-x rise ruby-size ruby-string))))))

(defun ruby-suppress-overhang (rb &key left right)
  "行境界に落ちた ruby-box の、境界側の overhang を消した新しい ruby-box を返す。
   LEFT が真なら左の食い込みを 0 に、RIGHT が真なら右を 0 に。箱がその分広がり、
   親・ルビの水平位置を計算し直す (y/サイズ/文字列は不変)。luatexja の Case C 相当。"
  (let* ((ol (if left  0 (ruby-oh-left rb)))
         (or* (if right 0 (ruby-oh-right rb)))
         (bp (first  (ruby-placements rb)))
         (rp (second (ruby-placements rb))))
    (multiple-value-bind (adv base-x ruby-x)
        (%ruby-place (ruby-base-adv rb) (ruby-ruby-adv rb) ol or*)
      (make-instance 'ruby-box
                     :advance adv :ascent (ascent rb) :descent (descent rb)
                     :oh-left ol :oh-right or*
                     :base-adv (ruby-base-adv rb) :ruby-adv (ruby-ruby-adv rb)
                     :placements (list (make-placed base-x (placed-y bp) (placed-size bp) (placed-string bp))
                                       (make-placed ruby-x (placed-y rp) (placed-size rp) (placed-string rp)))))))

(defun ruby-group (base-strings base-widths base-size base-ascent base-descent
                   ruby-strings ruby-widths ruby-size ruby-ascent &key (gap 0))
  "グループルビ (親複数字に1ルビ)。BASE-STRINGS/RUBY-STRINGS は1字文字列の list、
   *-WIDTHS はそれぞれの送り幅の list。JLReq §3.3.6: 広い方の幅に箱を合わせ、
   狭い方を均等配置 (両端=字間の半分)。オーバーハング無し。

   ★distribute-even を両列に適用するだけでよい: 広い列は extra=0 で連続、狭い列は
     均等に散る。ルビ<親なら親が連続・ルビが散り、ルビ>親なら逆になる。"
  (let* ((base-w (reduce #'+ base-widths :initial-value 0))
         (ruby-w (reduce #'+ ruby-widths :initial-value 0))
         (box-w  (max base-w ruby-w))
         (ruby-descent (- ruby-size ruby-ascent))
         (rise   (+ base-ascent ruby-descent gap))
         (placements '()))
    (loop for s in base-strings for x in (distribute-even base-widths box-w)
          do (push (make-placed x 0 base-size s) placements))
    (loop for s in ruby-strings for x in (distribute-even ruby-widths box-w)
          do (push (make-placed x rise ruby-size s) placements))
    (make-instance 'ruby-box
                   :advance box-w
                   :ascent  (+ rise ruby-ascent)
                   :descent base-descent
                   :placements (nreverse placements))))

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
