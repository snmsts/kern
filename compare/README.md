# kern vs LuaLaTeX+jlreq — ground-truth 比較

kern の出力が「回帰しない」だけでなく「組版として妥当」かを、参照実装と突き合わせて確かめる。

## なぜ jlreq (LuaLaTeX) か

`jlreq` クラスは LuaTeX モードで **`jfm-jlreq.lua` を使う** — kern が読むのと**同じ JFM データ**。
だから両者が食い違えば、差は JFM(データ)でなく**エンジン論理**（kern の `set-glue` vs luatexja の
行調整）に局在する。§4.19 の突き合わせとして pTeX(和文専用エンジン)より鋭い。ルビも
`luatexja-ruby` で比較できる。

## 落とし穴: フォントを混ぜるな

kern=游明朝、jlreq=既定(原ノ味明朝等)だと字送りが違い、**絶対位置の差が font 差か engine 差か
判別できない**。対策:

- **§4.19**: 絶対位置でなく **zw 単位のグルー分配**で比較する。`jfm-jlreq.lua` のグルーは zw 定義で
  フォント非依存。「行を X zw 詰める/空ける時、各クラス対のアキがどれだけ動くか」を比べれば
  font 差が消え、エンジン論理だけが残る。luatexja の `\showbox` が吐く glue ノードから抽出。
- **ルビ**: 位置比較するなら**両者を同じ TTF で**組む (kern 側 yumin.ttf、jlreq 側も
  `\usepackage{luatexja-fontspec}` 等で yumin 指定) か、比率(親幅に対するルビ位置)で比べる。

## 手順

1. `docker pull texlive/texlive:latest` (arm64 ネイティブ)。
2. `docker run --rm -v "$PWD":/work -w /work texlive/texlive lualatex -interaction=nonstopmode compare/jlreq-probe.tex`
3. `jlreq-probe.log` の `\showbox` 出力から、行の glue/kern ノードと set 比を読む。
4. kern 側で同じ和文・同じ zw グルーを `set-glue` に通し、分配を1対1で diff。

## 状態

- [x] `jlreq-probe.tex` (段落モードの §4.19 行ダンプ + モノルビ box ダンプ)
- [x] toolchain 確認: `texlive/texlive` **arm64 native**、lualatex+jlreq+luatexja-ruby 動作
- [x] `\showbox` の形を確認 → 抽出設計の材料が揃った
- [ ] §4.19 priority の比較: **詰め (追い込み) が要る行**を作って diff (下記)
- [ ] ルビ位置比較は済 (下記 findings)

## Findings (2026-07-22, 初回)

**ルビ幾何 = luatexja と一致（妥当性検証 OK）**。`\ruby{漢|字}{かん|じ}` を fontsize=10pt で:
- ルビフォント 5pt (親10の半分) — kern の `ruby-size = size/2` と一致。
- 外箱 ascent = **13.80002** = 親ascent 8.8 + ルビ 5.0 — kern の `ruby-mono` ascent **13.8** と一致
  (丸め差 2e-5)。
- ルビ<親 (字+じ) は両側 `\glue plus 1.0fil` = 中央配置 — kern の (親-ルビ)/2 中央と等価。
→ kern のルビは「意図どおり」だけでなく「参照実装と同じ位置」。

**§4.19 グルー data = 一致**。luatexja のノードで:
- 和文字間 `\glue 0.0 plus 2.5` = nat0・stretch 四分・**shrink 無し** = kern の kanjiskip。
- 句点後 `\glue 5.0 plus 2.5` (minus 無し) = **句点後は詰めない** = kern の発見。

## Findings (2026-07-22, 詰めシナリオ — §4.19 priority)

`shrink-probe.tex`: `\hbox to 7\zw{AA BB「あ」DD}` を自然幅より狭く組んで、欧文間(空白)・
約物(括弧アキ)・和欧間がどう縮むかをノードから読んだ。

**kern と luatexja default が分岐した。kern の方が §4.19 に忠実。**

- luatexja: `glue set - 0.6993` = **単一比を全 shrink 成分に比例適用**。欧文間 空白
  `3.33 minus 1.1111`・括弧アキ `5.0 minus 5.0` ともに 0.6993 倍だけ**一律に縮む**。
  → TeX 古典の比例分配。§4.19 の段階順ではない。
- kern: §4.19 priority 段階。probe で「1pt だけ詰める」と **欧文間だけ −1.0 (先に全潰し)、
  約物は無傷**。luatexja なら同条件で欧文間 −0.1・約物 −0.45 と全部が縮む。

**評価**: §4.19 は詰めを「欧文間→中点→括弧→和欧間」の**優先順**で明記している。よって
kern は JIS X 4051 の字面に忠実。luatexja default は比例(簡潔・広く使われ・見た目も許容)。
どちらが良い見た目かは美的判断で、レンダラが無いのでここでは保留。

**要注意**: log に `luatexja.adjust` モジュールが出るので luatexja に priority 調整モードが
ある可能性あり。ここで観測したのは jlreq default の挙動。「luatexja は priority できない」
とは断定しない — default が比例、が正確。追検証: `\ltjsetparameter` で調整モードを試す。

**未了**: 詰めシナリオの位置比較を「同一フォント」でやれば絶対位置でも突き合わせられる
(kern=yumin, jlreq 側も yumin 指定)。今回は zw 単位のグルー分配で差を確定させた。

## Findings (2026-07-22, 視覚比較)

コンテナの `gs` で PDF→PNG (300dpi) して初めて出力を目視した。

- `docker run --rm -v "$(pwd -W):/work" -w /work texlive/texlive
   gs -dNOPAUSE -dBATCH -sDEVICE=png16m -r300 -dFirstPage=1 -dLastPage=1
   -sOutputFile=OUT.png IN.pdf`

**kern のモノルビ (demo/ruby.pdf) は jlreq と視覚的に一致**。半分サイズ・親の直上・中央。
数値 (ascent 13.8) に続き絵でも参照実装と同じ。差らしい差なし。

polish/未了として観測:
- kern は `ruby-mono` の `gap=0` でルビが親の直上ぴったり。jlreq はごく僅かな空きあり。
  → `gap` を小さく足すと参照に近づく (polish)。
- kern はルビ文字列を1つの連続文字列として中央描画。「かん=漢幅」の even ケースは正しく
  見えるが、ルビが親より狭い多字ルビの**均等配置は未実装 (phase 2 グループルビ)**。
- テスト行が緩い (STRETCHED) のは test 幅 (9字を12zw) 由来でルビ/組版のバグではない。

## Findings (2026-07-22, グループルビ)

`ruby-group.tex`: `\ruby{大人}{おとな}` (親20, ルビ15<20)。luatexja のルビ hbox は
fil グルー **1:2:2:1** で配分 = 両端が字間の半分 (JLReq §3.3.6 の均等配置)。
余り 5pt を 6 単位で割り 端5/6・字間5/3、位置 お=5/6 と=15/2 な=85/6。

**kern の `ruby-group` はこれと厳密一致** (probe & 視覚)。`distribute-even` を親列・ルビ列の
両方に「広い方の幅」で適用するだけ: 広い列は extra=0 で連続、狭い列が均等に散る。
ルビ<親なら親連続・ルビ散り、ルビ>親なら逆、が自動で出る。数値・視覚とも参照実装と一致。

## Findings (2026-07-22, オーバーハング / phase 3)

`ruby-overhang.tex`: 都(10) に みやこ(15) = ルビ 5pt 長い。隣接違いで luatexja の box:

| ケース | box 幅 | luatexja の構造 |
|---|---|---|
| A 隣=仮名 (の都に) | **10 (親幅)** | ルビ hbox が `shifted -2.5` = 両隣へ 2.5 食い込む。**箱は親幅** |
| B 隣=漢字 (京都府) | **15 (ルビ幅)** | 食い込まず (except-kanji)。`kern2.5/-2.5` マーカー、箱ルビ幅 |
| C 単独 (都) | **15 (ルビ幅)** | 隣なし=食い込めず、箱ルビ幅 |

→ **ルビ>親のとき box advance が隣に依存する** (仮名隣=親幅・漢字/無し=ルビ幅)。これが
「box は固定幅」を破る本丸。tractable な解: **食い込み可否を itemization (隣を知る層) が決め、
box advance を確定** すれば行分割は固定幅を見られる。

kern の実装 (probe & 視覚とも luatexja 一致):
- `ruby-mono` に `:overhang-left/right` (量)。box advance = ルビ幅−食込左−食込右 (親幅が下限)、
  ルビは −食込左 から置く。両側フル=箱親幅・ルビ-2.5、食込0=箱ルビ幅・親中央。
- `mono-ruby-box` は可否ブールを受け、量 = min((ルビ−親)/2, ルビサイズ) で上限 (AH 既定)。
- itemization (`ruby-demo-items`) が **隣の先頭コードが漢字か** (`kanji-code-p`, Unicode 範囲) で
  可否を決める。仮名隣=食込、漢字/無し=食込まず。

**行境界の overhang 抑制 (解決, 2026-07-23)**: ruby-box が行頭/行末に落ちると隣は別行/版面外
なので overhang できない (luatexja の Case C = 箱ルビ幅)。行分割の後、各行の先頭が oh-left>0 の
ruby-box なら左を、末尾が oh-right>0 なら右を消す。`ruby-suppress-overhang` (箱を作り直す) と
`adjust-line-boundaries` (layout-items が各行で適用)。%ruby-place に配置計算を集約し両者で共有。
- probe: 都(みやこ) を単独行に落とすと箱が 15 (ルビ幅) に広がり、みやこ が x=0 (版面外へ出ない)。
- mid-line (同一行の の都に) では 都 は行の先頭/末尾でないので抑制されず、食い込みは保たれる。
- 行分割との相互作用: 行分割は mid-line の advance (親幅) で切り、その後に境界だけ作り直す
  = 破壊的な再分割は不要。widen で行が僅かに変わるが set-glue が吸収する。

## Findings (2026-07-23, 熟語ルビ / phase 4)

`ruby-jukugo.tex`: 二|十 / に|じゅう。に=5<二10, じゅう=15>十10, 合計 20=20。
- 独立モノ2つ (box1) = 二(に)箱10 + 十(じゅう)箱15 = 計25、じゅう がはみ出す。
- 熟語 (box0) = box 20 (=親合計)、にじゅう を**連続配置** (に0 じ5 ゅ10 う15)、二0 十10。
  → **熟語は片方の余り (二の slack) を融通し、じゅう がはみ出さず全体で釣り合う**。
  に|じゅう 群境界の gap (stretch 2.5) が群内 (1.25) より大きい = 群境界を意識。

**決定的な気づき: 釣り合った熟語 = 連結した親・ルビへの group ruby**。kern の `ruby-group`
がそのまま box0 と厳密一致 (probe: 親 0,10 / ルビ 0,5,10,15)。`jukugo-ruby-box` は各字の
ルビを連結して group ruby に渡すだけ。視覚も一致 (二十/にじゅう が釣り合って収まる)。

**熟語の2モード (2026-07-23 追加, `ruby-jukugo`)**: `ruby-jukugo2.tex` で余りのある熟語
名|前 / な|まえ (な5≤名10, まえ10≤前10) を実測したら、luatexja は**平坦化しなかった**:
各ルビを自分の親字上に個別配置 (な 中央2.5 over 名、まえ over 前 10,15)、群間に伸縮 glue。
→ 熟語は2モード:
- **全ルビが自分の親字に収まる** → 各ルビを親字上に中央 (モノ的、名前)。`ruby-jukugo` モードA。
- **はみ出す親字がある** → 全体平坦化・均等配置で融通 (二十)。モードB (= ruby-group)。
kern は両モードとも luatexja と一致 (probe & 視覚: 名前=個別中央, 二十=平坦)。

**未実装 (残る真の難所, 正直に)**:
- **熟語内の行分割**: 二|十 で切り各字がルビを連れて別行へ (研究曰く「InDesign でも自動困難」
  の核心)。現状は atomic。行分割との相互作用そのもの。
- モードBの群境界アキ重み付けの精密化 (余りが大きい平坦熟語での | 境界強調) は近似のまま。
