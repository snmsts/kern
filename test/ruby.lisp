;;;; test/ruby.lisp -- ルビ (JLReq §3.3) の回帰テスト
;;;;
;;;; 段1 (モノルビ・オーバーハング無し) の幾何を固定する。フォント不要。
;;;; la-check= 等は line-adjustment.lisp で定義済み (同一パッケージ・先にロード)。
;;;; 単位は rational なので期待値は厳密一致。

(in-package #:kern)

;;; 親 ascent=44/5(=8.8) descent=2 を共通に使う。ルビ size=5 (親10の半分)。

;; ルビ size=5 のとき ruby-ascent=5*88/100=22/5(4.4), ruby-descent=3/5(0.6)。
(defun test-ruby-mono-shorter ()
  ;; ルビ(5) < 親(10): 箱は親幅、ルビは中央。
  (let ((rb (ruby-mono 10 44/5 2 "国" 10 5 "て" 5 22/5)))
    (la-check= (advance rb) 10   "モノ ルビ<親: 箱幅=親10")
    (la-check= (ascent rb)  69/5 "モノ: ascent=親8.8+ルビ5=13.8")
    (la-check= (descent rb) 2    "モノ: descent=親")
    (destructuring-bind (bx by bs bstr) (first (ruby-placements rb))
      (la-check= bx 0  "親 x=0")
      (la-check= by 0  "親 y=0 (親ベースライン)")
      (la-check= bs 10 "親 size=10")
      (la-check (string= bstr "国") "親 str"))
    (destructuring-bind (rx ry rs rstr) (second (ruby-placements rb))
      (la-check= rx 5/2  "ルビ中央 x=(10-5)/2")
      (la-check= ry 47/5 "ルビ rise=親ascent8.8+ルビdescent0.6=9.4 (luatexja と一致)")
      (la-check= rs 5    "ルビ size=半分")
      (la-check (string= rstr "て") "ルビ str"))))

(defun test-ruby-mono-longer ()
  ;; ルビ(15) > 親(10): 箱をルビ幅へ広げ、親を中央へ。オーバーハング無し。
  (let ((rb (ruby-mono 10 44/5 2 "駅" 10 15 "みかん" 5 22/5)))
    (la-check= (advance rb) 15 "ルビ>親: 箱幅=ルビ15")
    (la-check= (first (first  (ruby-placements rb))) 5/2 "親中央 x=(15-10)/2")
    (la-check= (first (second (ruby-placements rb))) 0   "ルビ x=0")))

(defun test-ruby-gap ()
  ;; gap=1: ルビと親の空きを1入れると rise と ascent が1増える。
  (let ((rb (ruby-mono 10 44/5 2 "水" 10 10 "みず" 5 22/5 :gap 1)))
    (la-check= (ascent rb) 74/5 "gap=1: ascent=親8.8+ルビ5+gap1=14.8")
    (la-check= (second (second (ruby-placements rb))) 52/5
               "gap=1: ルビ rise=親8.8+ルビdescent0.6+gap1=10.4")))

(defun test-ruby-emission-in-line ()
  ;; ルビ箱が実 layout パス (break→set-glue→layout-items) で、box の絶対 x ぶん
  ;; ずれた配置済みグリフとして行に出るか。フォント不要 (box に幅を直接持たせる)。
  ;; 本(20) 漢[かん](箱20) 文(20) を幅60=自然幅で組む。
  (let* ((sz 20)
         ;; ルビ size=10 → ruby-ascent=10*88/100=44/5(8.8), ruby-descent=6/5(1.2)。
         ;; rise = 親ascent(88/5) + ルビdescent(6/5) = 94/5 = 18.8。
         (rb (ruby-mono 20 (* sz 88/100) (* sz 12/100) "漢" 20 20 "かん" 10 (* 10 88/100)))
         (items (coerce (finish-paragraph
                         (list (make-glyph-box 20 "本") rb (make-glyph-box 20 "文")))
                        'vector))
         (lines (layout-items items 60 sz))
         (gs (and lines (line-glyphs (first lines)))))
    (la-check= (length lines) 1 "1行")
    (la-check= (line-status (first lines)) :exact "幅60=自然幅でぴったり")
    (la-check= (length gs) 4 "グリフ4つ (本 漢 かん 文)")
    (when (= (length gs) 4)
      (destructuring-bind (g1 g2 g3 g4) gs
        (la-check (equal g1 (list 0    0    20 "本"))   "本 (0,0,20)")
        (la-check (equal g2 (list 20   0    20 "漢"))   "ルビ親 漢 (箱x20+中央0)")
        (la-check (equal g3 (list 20   94/5 10 "かん")) "ルビ かん (箱x20, rise18.8, 半分10)")
        (la-check (equal g4 (list 40   0    20 "文"))   "文 (40,0,20)")))))

(defun test-distribute-even ()
  ;; 均等配置: 両端=字間の半分 (JLReq グループルビ)。
  (la-check (equal (distribute-even (list 5 5 5) 20) (list 5/6 15/2 85/6))
            "均等 3@5→20: 端5/6 字間5/3")
  (la-check (equal (distribute-even (list 5 5 5 5) 20) (list 0 5 10 15))
            "ぴったり(extra0)は連続")
  (la-check (equal (distribute-even (list 10) 20) (list 5))
            "1個は中央 (extra/2)"))

(defun test-ruby-group ()
  ;; グループルビ 大人(20)/おとな(15<20)。親=自然位置、ルビ=均等配置。
  ;; luatexja 実測 (compare/ruby-group): 親 0,10 / ルビ 5/6, 15/2, 85/6。
  (let ((rb (ruby-group (list "大" "人") (list 10 10) 10 44/5 6/5
                        (list "お" "と" "な") (list 5 5 5) 5 22/5)))
    (la-check= (advance rb) 20   "グループ: 箱幅=親20 (ルビ<親)")
    (la-check= (ascent rb)  69/5 "グループ: ascent 13.8")
    (destructuring-bind (b1 b2 r1 r2 r3) (ruby-placements rb)
      (la-check= (placed-x b1) 0    "親 大 x=0 (自然)")
      (la-check= (placed-x b2) 10   "親 人 x=10 (自然)")
      (la-check= (placed-x r1) 5/6  "ルビ お x=5/6 (両端=字間半分)")
      (la-check= (placed-x r2) 15/2 "ルビ と x=15/2")
      (la-check= (placed-x r3) 85/6 "ルビ な x=85/6 (luatexja と一致)")
      (la-check= (placed-y r1) 47/5 "ルビ rise=9.4"))))

(defun run-ruby-tests ()
  "ルビ (モノ + グループ, オーバーハング無し) の回帰テスト。全通過なら T。"
  (setf *la-checks* 0 *la-fails* 0)
  (test-ruby-mono-shorter)
  (test-ruby-mono-longer)
  (test-ruby-gap)
  (test-ruby-emission-in-line)
  (test-distribute-even)
  (test-ruby-group)
  (format t "~&ruby (mono, no-overhang): ~a/~a checks passed~a~%"
          (- *la-checks* *la-fails*) *la-checks*
          (if (zerop *la-fails*) "  OK" "  *** FAIL ***"))
  (zerop *la-fails*))
