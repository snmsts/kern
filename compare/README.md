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
