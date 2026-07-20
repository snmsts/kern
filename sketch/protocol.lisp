;;;; プロトコルのスケッチ — 議論用の草稿であって実装ではない
;;;;
;;;; 目的は「どこに何を差し込むか」を固定すること。中身は後で埋まる。
;;;; 判断が要る箇所には ??? を付けてある。

(defpackage #:typeset-sketch
  (:use #:cl))
(in-package #:typeset-sketch)

;;; ---------------------------------------------------------------------------
;;; 単位
;;; ---------------------------------------------------------------------------
;;; 内部は抽象単位。バックエンドが DPI 換算する。
;;; 有理数にするのは丸め誤差の累積を避けるため — 均等割りは除算を大量にやるので、
;;; 単精度で持つと行末が数百分の1ポイントずれて詰め処理の判定が揺れる。
;;; 速度が問題になったら「1pt = 65536」の固定小数点整数に落とす (TeX の sp と同じ手)。

(deftype len () 'rational)

;;; ---------------------------------------------------------------------------
;;; item モデル — TeX の box / glue / penalty / discretionary
;;; ---------------------------------------------------------------------------
;;; 行分割器が見るのはこの4種類だけ。言語のことは何も知らない。
;;; 言語規則は「どの item をどんな値で並べるか」に翻訳されて入ってくる。

(defclass item ()
  (;; ★不可逆な決定: 逆写像。印刷だけなら完全に無駄だが後から入らない。
   ;; GUI のヒットテストとキャレット位置がこれに乗る。
   (source-start :initarg :source-start :initform nil :accessor source-start)
   (source-end   :initarg :source-end   :initform nil :accessor source-end))
  (:documentation "行分割器に流れる要素の基底。"))

;;; 寸法の呼び方について:
;;;   縦書きを後で入れる前提なので height/depth という横書き前提の名前を避け、
;;;   advance (行方向) / ascent・descent (行に直交する方向) にしてある。
;;;   横組みなら advance=幅、縦組みなら advance=高さ。
;;;   ??? ascent/descent も縦組みでは左右になるので、いずれ cross-start/cross-end
;;;       のような名前にすべきかもしれない。今は読みやすさを優先。

(defclass box (item)
  ((advance :initarg :advance :accessor advance :type len)
   (ascent  :initarg :ascent  :initform 0 :accessor ascent  :type len)
   (descent :initarg :descent :initform 0 :accessor descent :type len)
   ;; 行端に来たときに版面外へはみ出してよい量。ぶら下げ (行末の句読点を
   ;; 版面外に出す) と、欧文の突き出し (microtype の protrusion) が同じ機構に乗る。
   ;; ★行分割器から見える必要がある。ぶら下げると「入らなかった行が入る」ので、
   ;;   分割の判断そのものが変わる。描画時の後処理では駄目。
   ;; ★当初 discretionary で表現しようとしたが筋が悪い。discretionary は
   ;;   「分割点で中身が入れ替わる」機構で、ぶら下げは「分割点の手前にある文字の
   ;;   実効幅が縮む」話なので捻れる。スロットで持つ方が素直で、負の寸法も要らない。
   (protrusion :initarg :protrusion :initform 0 :accessor protrusion :type len))
  (:documentation "固定寸法の中身。分割不可。"))

(defclass glyph-box (box)
  ((font   :initarg :font   :accessor box-font)
   ;; ★入口は「文字」ではなく「整形済みグリフ列」。
   ;; シェーピングは実装しないので、当面ここには文字がそのまま入る。
   ;; だが型をグリフ列にしておくと将来 HarfBuzz を噛ませる合流点が確保される。
   (glyphs :initarg :glyphs :accessor box-glyphs))
  (:documentation "整形済みランの描画単位。"))

(defclass glue (item)
  ((advance :initarg :advance :initform 0 :accessor advance :type len)
   (stretch :initarg :stretch :initform 0 :accessor stretch :type len)
   (shrink  :initarg :shrink  :initform 0 :accessor shrink  :type len)
   ;; TeX の無限位数。0=有限, 1=fil, 2=fill, 3=filll。
   ;; 中央揃え・右揃えを「無限に伸びる glue を端に置く」で表現するために要る。
   ;; cl-typesetting は +huge-number+ という有限の大きい数で代用していたが、
   ;; それだと「無限グルーがある行では有限グルーは伸びない」という性質が出ない。
   (stretch-order :initarg :stretch-order :initform 0 :accessor stretch-order)
   (shrink-order  :initarg :shrink-order  :initform 0 :accessor shrink-order)
   ;; ★JLReq の詰めは【段階的】。jfm-jlreq.lua より:
   ;;     優先順位は，第n段階を 3-n に対応させる．
   ;;       段階   1, 2, 3, 4, 5, 6
   ;;     priority 2, 1, 0,-1,-2,-3
   ;;   実際の値は {-1,-2} {0,-2} {0,-1} のように【伸び用と縮み用の対】。
   ;;   段階の高いものから使い切り、足りなければ次の段階へ、という順序で処理する。
   ;;   ★これは stretch-order (TeX の無限位数) とは別物。
   ;;     無限位数は「無限が有限を支配する」= 有限側は一切伸びない。
   ;;     priority は「第1段階を使い切ってから第2段階」= 有限量の順序付き消費。
   ;;     両方要る。
   (stretch-priority :initarg :stretch-priority :initform 0 :accessor stretch-priority)
   (shrink-priority  :initarg :shrink-priority  :initform 0 :accessor shrink-priority)
   ;; その空きが左右どちらの文字に属するかの比率 (jfm-jlreq では 0, 1/3, 0.5, 1)。
   ;; 行頭・行末で版面の端が揃うかに効く。
   (ratio :initarg :ratio :initform 1/2 :accessor glue-ratio))
  (:documentation "伸縮する空き。均等割りの担い手。"))

(defconstant +inf-penalty+  10000 "これ以上は分割禁止。")
(defconstant +forced-break+ -10000 "これ以下は強制分割。")

(defclass penalty (item)
  ((value :initarg :value :initform 0 :accessor penalty-value)
   ;; TeX の flagged。連続する flagged break に追加ペナルティを課すためのもの。
   ;; 「ハイフンで終わる行が3行続く」を抑制する。
   ;; 和文では連続する約物ぶら下げの抑制などに転用できる ???
   (flagged-p :initarg :flagged-p :initform nil :accessor flagged-p))
  (:documentation "その位置で分割することの好ましくなさ。禁則はここに乗る。"))

(defclass discretionary (item)
  ((pre-break  :initarg :pre-break  :initform nil :accessor pre-break)
   (post-break :initarg :post-break :initform nil :accessor post-break)
   (no-break   :initarg :no-break   :initform nil :accessor no-break)
   (penalty    :initarg :penalty    :initform 50  :accessor disc-penalty))
  (:documentation
   "分割すると中身が変わる箇所。欧文のハイフン挿入がこれ。
    ぶら下げは box の protrusion スロットに移した (上記参照)。"))

;;; ---------------------------------------------------------------------------
;;; プロトコル (1) — メトリクス
;;; ---------------------------------------------------------------------------
;;; エンジンがフォントに聞くこと。実装は cl-pdf 用と GUI ツールキット用。
;;; cl-typesetting の実測では結合点が 6 関数だったので、規模はこの程度で足りるはず。

(defgeneric font-ascent (font)
  (:documentation "ベースラインから上の標準的な伸び。"))

(defgeneric font-descent (font)
  (:documentation "ベースラインから下。正の値で返す。"))

(defgeneric font-line-gap (font)
  (:documentation "推奨行間。和文では使わず 行送り を別に決めることが多い ???"))

(defgeneric glyph-advance (font glyph size)
  (:documentation "1グリフの送り幅。和文は原則 size と等しい (全角)。"))

(defgeneric kerning (font glyph-1 glyph-2 size)
  (:documentation "対のカーニング。無ければ 0。")
  (:method (font glyph-1 glyph-2 size)
    (declare (ignore font glyph-1 glyph-2 size))
    0))

(defgeneric shape-run (font text &key script direction)
  (:documentation
   "テキストを整形済みグリフ列にする。
    ★今回は実装しない。既定は恒等 (文字をそのままグリフとする)。
    和文・欧文はこれで足りる。将来 HarfBuzz を噛ませるならここに刺す。")
  (:method (font text &key script direction)
    (declare (ignore font script direction))
    text))

;;; ★フォントメトリクスの出所について
;;; 画面と印刷でメトリクスが違う (量子化・ヒンティング) ため、同じ文書が
;;; 画面と PDF で違う行分割になりうる。
;;; 方針: 常に印刷メトリクスで組み、画面表示は拡大縮小のみ (InDesign 系の割り切り)。
;;; つまり GUI バックエンドでも組版時は cl-pdf 側のメトリクスを使う。
;;; ??? これでいいか。エディタ用途では画面メトリクスで組みたくなる場面があるかも。

;;; ---------------------------------------------------------------------------
;;; プロトコル (2) — item 生成 = 言語拡張点
;;; ---------------------------------------------------------------------------
;;; ここが「新言語対応 = 表を1枚足す」を成立させる層。
;;; コードを足すのではなく ruleset のインスタンスを作る。

(defclass ruleset ()
  ((tables :initarg :tables :initform nil :accessor ruleset-tables)
   ;; 合成済みの平坦な参照構造。CONSTRUCT-RULESET が作る。
   (compiled :initform nil :accessor ruleset-compiled))
  (:documentation
   "言語・組版方針の束。日本語なら JLReq 由来の表が入る。

    ★合成の方針 (「JLReq + この文書だけの禁則追加」をどう表現するか):
      1. ruleset は表を保持し、合成は【表のマージ】= データ操作。
         「行頭禁則に3文字足す」のにメソッドを書かせるのは重い。
      2. アクセサは【総称関数のまま】。変な条件付き規則はメソッドで殴れる逃げ道。
      3. ★合成は【構築時に1回】行って平坦な参照構造にコンパイルする。
         LINE-BREAK-CLASS と BREAK-PENALTY-VALUE は1文字ごと・1文字対ごとに
         呼ばれる内側のループなので、問い合わせ時にチェーンを辿る設計にすると
         GUI の再組版で効いてくる。合成コストは構築時に払い切る。"))

(defgeneric construct-ruleset (base &key overrides)
  (:documentation
   "ベース ruleset に差分をマージし、平坦化した ruleset を返す。
    ここでコンパイルを済ませ、以降の問い合わせを配列/ハッシュ一発にする。"))

;;; --- 合法性: UAX #14 (全言語共通のデータ) ---

;;; ★行の境界そのものを文字クラスとして扱うこと。
;;;   jfm-jlreq.lua は class 90 = parbdd / boxbdd (段落頭・ボックス境界) を持ち、
;;;   行頭・行末を特別扱いせずクラス対表に載せている。
;;;   「行頭の開き括弧は半角になる」といった規則が、表からそのまま落ちてくる。
;;;   → 下の総称関数群は :line-start / :line-end を文字クラスとして受け取れること。
;;;     特別扱いの分岐をコードに書かない。表に寄せる。
;;;
;;; ★和文の文字クラスは JLReq で約25個:
;;;   開き括弧 / 閉じ括弧 / ハイフン / ダッシュ / 波ダッシュ / 感嘆疑問 / 中点類 /
;;;   句点 / 読点 / 繋ぎ符号 / 繰返し記号 / 長音 / 小書き仮名 / 前置記号 / 後置記号 /
;;;   和字間隔 / ひらがな / カタカナ / 半角カタカナ / 数字(半角・三分・四分) /
;;;   漢字 / 欧文 / 境界
;;;   これは UAX #14 のクラスとは【別の軸】。UAX #14 は分割の合法性、
;;;   JLReq のクラスは空き量。両方引く。

(defgeneric line-break-class (ruleset char)
  (:documentation
   "UAX #14 の行分割クラスを返す (:ID :CL :OP :NS :AL :SP ...)。
    Unicode の LineBreak.txt から生成した表を引くだけ。言語非依存。"))

(defgeneric break-opportunity (ruleset class-before class-after)
  (:documentation
   "クラス対に対して :prohibited / :allowed / :mandatory を返す。
    これも UAX #14 の対表そのもの。"))

;;; --- 重み: JLReq (言語ごとのポリシー) ---

(defgeneric break-penalty-value (ruleset class-before class-after)
  (:documentation
   "分割可能な箇所の「嫌さ」。UAX #14 は allowed/prohibited しか言わないので、
    『切れるが避けたい』の階調はここから来る。
    禁則はこれが +inf-penalty+ になったもの。
    ★行頭禁則も行末禁則も、結局この1つの関数に畳める。
      行頭禁則 (。を行頭に置かない) = 。の直前で切ることの禁止
      行末禁則 (「を行末に置かない) = 「の直後で切ることの禁止
    どちらも『クラス対に対する分割の可否』なので、同じ表で表現できる。"))

;;; --- 字間: 約物の詰めと和欧間アキ ---

(defgeneric inter-char-glue (ruleset class-before class-after font size)
  (:documentation
   "文字間に入れる glue。JLReq の文字間空き量の表そのもの。
      約物の詰め      → shrink を持つ glue
      和欧間          → natural width + stretch
      通常の和文字間   → 幅0 + わずかな stretch/shrink (均等割りの担い手)

    ★具体的な数値は JLReq には【書かれていない】。JLReq は要件文書で、
      数値は JIS X 4051 (有償) に委ねられている。実装から取るしかない。

      pTeX の既定値:
        \\xkanjiskip (和欧間)   = .25zw plus1pt minus1pt   ← 四分アキ
        \\kanjiskip  (和文字間) = 0pt plus.4pt minus.4pt   ← ベタ組み + 微小伸縮

      CSS text-autospace: ideograph-alpha は同じ和欧間に 1/8 em を推奨。
      **pTeX の 1/4 em と 2 倍食い違う。**

      → 唯一の正解が無いので、値はコードに焼かず ruleset の表に置く。
        既定は pTeX 互換、CSS 互換の表も選べるようにする。これが表駆動の根拠。

    ★\\kanjiskip の自然幅が 0 なのが要点。和文の均等割りは『字間を空ける』のではなく
      『ベタ組みを基準に必要な分だけ伸縮させる』。
      cl-typesetting の make-inter-char-glue (typo.lisp:213-217) と発想が一致する。"))

;;; --- 均等割り戦略 ---

(defgeneric justification-strategy (ruleset script)
  (:documentation
   "そのスクリプトで行をどう伸縮させるか。
      欧文       → スペースを伸縮
      和文       → 字間を伸縮 + 詰め
      アラビア語 → カシーダ優先、足りなければスペース
    ★重要: グルー解決器は戦略を知らない。
      戦略が『どんな glue を吐くか』を決め、解決器は幅しか見ない。
      これで解決器が言語非依存のまま保たれる。"))

;;; --- 入口 ---

(defclass run ()
  ((text      :initarg :text      :accessor run-text)
   (script    :initarg :script    :accessor run-script)
   (direction :initarg :direction :initform :ltr :accessor run-direction)
   (font      :initarg :font      :accessor run-font)
   (size      :initarg :size      :accessor run-size)
   ;; 原文でのこのランの開始位置。逆写像はここから伝播する。
   (offset    :initarg :offset    :initform 0 :accessor run-offset))
  (:documentation
   "スクリプト・書字方向・フォント・スタイルが均一な区間。
    ★item 生成の入口を『文字列』ではなく『ラン』にするのが要点。
      文字列を直接受けると、将来シェーピングを挟む場所が無くなる。"))

(defgeneric items-for-run (ruleset run)
  (:documentation
   "ラン1つを item 列にする。上の総称関数群を使う既定実装を用意し、
    言語ごとの逸脱があればメソッドを足す。
    ここで source-start / source-end を run-offset から埋めること。★"))

(defgeneric itemize (ruleset text)
  (:documentation
   "テキストをランに分割する。スクリプト境界・書字方向・スタイル変更で切る。
    BiDi (UAX #9) はここ。★v1 では実装しない (和欧混在に BiDi は不要)。
    が、入口だけ作っておく。"))

;;; ---------------------------------------------------------------------------
;;; 行分割 — 言語非依存。拡張点を持たない。
;;; ---------------------------------------------------------------------------
;;; ★ここが総称関数でないことが設計上重要。
;;;   層2 が拡張可能だったら、言語規則が層1に畳めていないというサイン。

(defstruct line-break-params
  (tolerance 100)          ; badness の許容上限。超える候補は捨てる
  (line-penalty 10)        ; 行数を増やすことのコスト
  (hyphen-penalty 50)
  (adjacent-demerits 10000)) ; flagged break が連続することの追加コスト

(defun break-into-lines (items line-widths params)
  "Knuth & Plass 1981。active node のリストを持つ動的計画法。
   ★性能注意: 和文は全文字間が breakpoint なので 1000字の段落 = 1000 breakpoint。
     tolerance を超えた active node を積極的に捨てないと膨らむ。
     刈り込みは最初から入れる。GUI でリアルタイム再組版するなら特に。
   ★line-widths を列にしてあるのは、行ごとに幅が変わる場合 (回り込み) のため。"
  (declare (ignore items line-widths params))
  (error "未実装"))

;;; ---------------------------------------------------------------------------
;;; グルー解決 — 言語非依存。次元非依存。
;;; ---------------------------------------------------------------------------

(defun set-glue (items target-advance)
  "決まった行幅に対して glue の実寸を決める。★段階付き。

   処理の順序:
     1. 無限位数 (stretch-order/shrink-order) の最大を求める。
        位数の高い glue が1つでもあれば、低い位数は一切伸縮しない (TeX と同じ)。
     2. 同位数のなかで priority の高い段階から順に消費する。
        第n段階の伸縮量を使い切ってから第n+1段階へ。JLReq の詰め順序。
     3. 同一段階内では伸縮量に比例して配分し、飽和したものはロックして再配分。

   ★cl-typesetting の spread-boxes (layout.lisp:193-232) は 3 だけを実装している。
     次元非依存 (size-fn を渡す) なのは良い設計なので借りるが、
     1 と 2 は無い。特に 2 が無いと JLReq の詰めが表現できない。
   ★次元非依存を保つこと。横組みの均等割りと縦方向の行送りに同じコードが効く。"
  (declare (ignore items target-advance))
  (error "未実装"))

;;; ---------------------------------------------------------------------------
;;; 垂直方向 — ページ分割
;;; ---------------------------------------------------------------------------
;;; ★同じ item モデルを垂直リストに再利用する。TeX が水平リストと垂直リストの
;;;   両方に penalty を持つのと同型。行が box、行間が glue、
;;;   泣き別れ抑制・keep-with-next・見出し直後の禁止が penalty。
;;;
;;; ★ここは【差し替え可能にしてよい】。境界の判定基準は:
;;;
;;;     「その2つの変種が、同じ行・同じページに同時に必要になることがあるか?」
;;;
;;;   言語規則は【共存する】(日本語と英語が同じ行に混ざる) ので、
;;;   1つのアルゴリズムがデータとして食えないと破綻する → データ一択。
;;;   ページ分割の戦略は【共存しない】(文書ごとに1つ選ぶだけ) → コードでよい。
;;;
;;;   だから水平 (行分割) は固定、垂直 (ページ分割) は差し替え可能、で非対称。
;;;   重要度の差ではなく、合成範囲の差。
;;;
;;;   ただし【入力側は相変わらずデータ】。垂直 penalty を読むのは全戦略共通。

(defgeneric break-into-pages (strategy vertical-items page-heights params)
  (:documentation
   "垂直リストをページに割る。STRATEGY で差し替える。
      :greedy    TeX 流。行分割と違い大域最適化しない。v1 はこれで足りる
      :optimal   章全体で demerits 最小化
      :grid      行取りグリッドに吸着させる (和文の版面設計で要求されることがある)"))

;;; ---------------------------------------------------------------------------
;;; プロトコル (3) — 描画バックエンド
;;; ---------------------------------------------------------------------------
;;; レイアウト結果は「位置付き item の木」。バックエンドはそれを歩くだけ。

(defclass placed ()
  ((item :initarg :item :accessor placed-item)
   (x    :initarg :x    :accessor placed-x :type len)
   (y    :initarg :y    :accessor placed-y :type len))
  (:documentation "位置が確定した item。逆写像は item 側の source-start/end が持つ。"))

(defgeneric draw-glyphs (backend glyph-box x y size)
  (:documentation "整形済みグリフ列を描く。PDF なら Tj、GUI ならツールキットの描画。"))

(defgeneric draw-rule (backend x y advance thickness)
  (:documentation "罫線・下線・圏点の下地など。"))

(defgeneric backend-font (backend font-designator)
  (:documentation
   "バックエンド固有のフォントオブジェクトを得る。
    ★メトリクスプロトコル (1) の実装はこれが返すオブジェクトに対して定義する。"))

;;; ---------------------------------------------------------------------------
;;; 決めきれていないこと
;;; ---------------------------------------------------------------------------
;;;
;;; 1. 解決 → 行送りは【外枠基準】。JLReq の用語で:
;;;      行間 (line gap)   = 文字の外枠と外枠の【あいだ】
;;;      行送り (line pitch) = 外枠の高さ + 行間
;;;    欧文の baseline-to-baseline とはモデルが違う。両方サポートし、
;;;    どちらを使うかは ruleset が決める。
;;;    ★既定値は JLReq に無い (JIS X 4051 送り)。だが行送りは文書設計の
;;;      パラメータであって定数ではないので、エンジンが既定を知る必要は無い。
;;;      「3行取り = 3×外枠 + 2×行間」という合成規則だけ実装できればよい。
;;;
;;; 2. 解決 (要検証) → 横組みでは【全部アルファベティックベースラインに乗せる】。
;;;    和文フォントの CJK グリフは欧文と同じベースライン基準で設計されているので、
;;;    原理的にはシフト不要。x-height の食い違いによる見た目の調整は
;;;    美的問題であって正しさの問題ではない。
;;;    そこでランごとに任意の baseline-shift を持てるようにしておく:
;;;      - ルビ・上付き下付きでどのみち必要
;;;      - PDF では Ts 演算子にそのまま落ちる
;;;      - GUI バックエンドでも同じ抽象で済む
;;;    OpenType の BASE テーブル (表意ベースライン) が要るのは縦組み (v2)。
;;;    ★実際の出力で目視確認すること。フォントによっては破綻するかもしれない。
;;;
;;; 3. 解決済 → box の PROTRUSION スロット。discretionary は筋が悪かった。
;;; 4. 解決済 → BREAK-INTO-PAGES を差し替え可能に。入力は垂直 penalty というデータ。
;;;             差し替えてよい理由は「共存しないから」(上記の境界判定を参照)。
;;; 5. 解決済 → 表マージ + CLOS 逃げ道、CONSTRUCT-RULESET で構築時に平坦化。
;;;
;;; 残る 1 と 2 は JLReq を読まないと決まらない。次はそこ。
