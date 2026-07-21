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

- [x] `jlreq-probe.tex` (toolchain 検証 + §4.19 hbox ダンプ + ルビ)
- [ ] toolchain が通ることの確認 (image pull 後)
- [ ] `\showbox` 出力の形を見て抽出スクリプトを設計 (blind に作らない)
- [ ] kern 側の対応 probe と diff
