;;;; test/ruby.lisp -- ルビ (JLReq §3.3) の回帰テスト
;;;;
;;;; 段1 (モノルビ・オーバーハング無し) の幾何を固定する。フォント不要。
;;;; la-check= 等は line-adjustment.lisp で定義済み (同一パッケージ・先にロード)。
;;;; 単位は rational なので期待値は厳密一致。

(in-package #:kern)

;;; 親 ascent=44/5(=8.8) descent=2 を共通に使う。ルビ size=5 (親10の半分)。

(defun test-ruby-mono-shorter ()
  ;; ルビ(5) < 親(10): 箱は親幅、ルビは中央。
  (let ((rb (ruby-mono 10 44/5 2 "国" 10 5 "て" 5)))
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
      (la-check= ry 44/5 "ルビ rise=親ascent")
      (la-check= rs 5    "ルビ size=半分")
      (la-check (string= rstr "て") "ルビ str"))))

(defun test-ruby-mono-longer ()
  ;; ルビ(15) > 親(10): 箱をルビ幅へ広げ、親を中央へ。オーバーハング無し。
  (let ((rb (ruby-mono 10 44/5 2 "駅" 10 15 "みかん" 5)))
    (la-check= (advance rb) 15 "ルビ>親: 箱幅=ルビ15")
    (la-check= (first (first  (ruby-placements rb))) 5/2 "親中央 x=(15-10)/2")
    (la-check= (first (second (ruby-placements rb))) 0   "ルビ x=0")))

(defun test-ruby-gap ()
  ;; gap=1: ルビと親の空きを1入れると rise と ascent が1増える。
  (let ((rb (ruby-mono 10 44/5 2 "水" 10 10 "みず" 5 :gap 1)))
    (la-check= (ascent rb) 74/5 "gap=1: ascent=親8.8+gap1+ルビ5=14.8")
    (la-check= (second (second (ruby-placements rb))) 49/5
               "gap=1: ルビ rise=親ascent+gap")))

(defun run-ruby-tests ()
  "ルビ段1 (モノ・オーバーハング無し) の回帰テスト。全通過なら T。"
  (setf *la-checks* 0 *la-fails* 0)
  (test-ruby-mono-shorter)
  (test-ruby-mono-longer)
  (test-ruby-gap)
  (format t "~&ruby (mono, no-overhang): ~a/~a checks passed~a~%"
          (- *la-checks* *la-fails*) *la-checks*
          (if (zerop *la-fails*) "  OK" "  *** FAIL ***"))
  (zerop *la-fails*))
