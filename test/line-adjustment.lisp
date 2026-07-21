;;;; test/line-adjustment.lisp -- JIS X 4051 §4.19 行長調整の回帰テスト
;;;;
;;;; フォントもバックエンドも要らない。ruleset のグルーと set-glue だけで、
;;;; §4.19 の段階順・上限・句点例外を固定する。
;;;; 単位は rational (deftype len = rational) なので期待値は厳密一致で書ける。
;;;;
;;;; 実行: (asdf:test-system "kern") または (kern::run-line-adjustment-tests)
;;;;       → t なら全通過
;;;;
;;;; 行の作り: 各段階を単一グルーにしてあるので、比例配分は「その段が全部取る」
;;;;   = 厳密整数。size=24 を使うと 四分=6 三分=8 二分=12 八分=3 がすべて整数。

(in-package #:kern)

(defvar *la-checks* 0)
(defvar *la-fails* 0)

(defun la-check= (got expected label)
  (incf *la-checks*)
  (unless (eql got expected)
    (incf *la-fails*)
    (format t "  FAIL ~a: expected ~a, got ~a~%" label expected got)))

(defun la-check (ok label)
  (incf *la-checks*)
  (unless ok
    (incf *la-fails*)
    (format t "  FAIL ~a~%" label)))

(defun la-sizes (items target &rest indices)
  "ITEMS を TARGET に組んだときの、指定 index の実寸を list で返す。"
  (let ((sizes (set-glue items target)))
    (mapcar (lambda (i) (aref sizes i)) indices)))

;;; --- latin-space-glue が §4.19 のパラメータを持つ -------------------------
(defun test-latin-space-params ()
  ;; size=24, 空白の実送り幅 w=8 (=三分)。詰めは 四分(6)まで=shrink 2、
  ;; 空けは 二分(12)まで=stretch 4。priority は詰め空けとも段1。
  (let ((g (latin-space-glue 8 24 0)))
    (la-check= (advance g) 8 "latin-space advance")
    (la-check= (shrink g)  2 "latin-space shrink→四分 (8-6)")
    (la-check= (stretch g) 4 "latin-space stretch→二分 (12-8)")
    (la-check= (stretch-priority g) 2 "latin-space stretch-priority (段1)")
    (la-check= (shrink-priority g)  1 "latin-space shrink-priority (段1)"))
  ;; 空白が四分より狭ければ詰め0、二分より広ければ空け0 (上限で頭打ち)。
  (let ((narrow (latin-space-glue 5 24 0))    ; w=5 < 四分(6)
        (wide   (latin-space-glue 13 24 0)))  ; w=13 > 二分(12)
    (la-check= (shrink narrow) 0 "四分より狭い空白は詰め0")
    (la-check= (stretch wide)  0 "二分より広い空白は空け0")))

;;; --- ruleset の約物 priority が §4.19 段順に一致する ----------------------
(defun test-yakumono-priorities ()
  (let ((rs (default-ruleset)))
    (let ((chuten   (jg-shrink-priority (class-glue rs 0 5)))   ; 中点前 = 段2
          (kakko-o  (jg-shrink-priority (class-glue rs 0 1)))   ; 始め括弧前 = 段3
          (kakko-c  (jg-shrink-priority (class-glue rs 2 0)))   ; 終わり括弧後 = 段3
          (waou     (jg-shrink-priority (rs-xkanjiskip rs))))   ; 和欧間 = 段4
      (la-check= chuten  -1 "中点 shrink-priority (§4.19 段2)")
      (la-check= kakko-o -2 "始め括弧 shrink-priority (§4.19 段3)")
      (la-check= kakko-c -2 "終わり括弧 shrink-priority (§4.19 段3)")
      (la-check= waou    -3 "和欧間 shrink-priority (§4.19 段4)")
      ;; 詰め順: 中点(段2) > 括弧(段3) > 和欧間(段4)
      (la-check (> chuten kakko-o) "中点 > 括弧 の詰め順")
      (la-check (> kakko-o waou)   "括弧 > 和欧間 の詰め順"))
    ;; 句点(6)の後ろは詰め対象外 = shrink 0
    (la-check= (jg-shrink (class-glue rs 6 0)) 0 "句点後は詰めない (shrink 0)")))

;;; --- 詰め (追い込み) の段階順: 欧文間 > 括弧 > 和欧間 ----------------------
(defun compression-line ()
  "box eng box 括弧 box 和欧間 box。自然幅 122。eng=1 括弧=3 和欧間=5。"
  (let* ((rs (default-ruleset)) (sz 24))
    (coerce (list (make-box 24)
                  (latin-space-glue 8 sz 0)              ; eng: shrink2 pri1
                  (make-box 24)
                  (jfm-glue->glue (class-glue rs 0 1) sz) ; 括弧: shrink12 pri-2
                  (make-box 24)
                  (jfm-glue->glue (rs-xkanjiskip rs) sz)  ; 和欧間: shrink3 pri-3
                  (make-box 24))
            'vector)))

(defun test-compression-order ()
  (let ((items (compression-line)))          ; 自然幅 122
    ;; 1詰め: 欧文間だけ動く
    (destructuring-bind (e k w) (la-sizes items 121 1 3 5)
      (la-check= e 7  "詰め1: 欧文間 8→7")
      (la-check= k 12 "詰め1: 括弧 不動")
      (la-check= w 6  "詰め1: 和欧間 不動"))
    ;; 5詰め: 欧文間を使い切り(2)、残り3を括弧へ。和欧間は不動
    (destructuring-bind (e k w) (la-sizes items 117 1 3 5)
      (la-check= e 6 "詰め5: 欧文間 四分で停止 (6)")
      (la-check= k 9 "詰め5: 括弧 12→9 (残り3)")
      (la-check= w 6 "詰め5: 和欧間 不動"))
    ;; 16詰め: 欧文間2+括弧12を使い切り、残り2を最後に和欧間へ
    (destructuring-bind (e k w) (la-sizes items 106 1 3 5)
      (la-check= e 6 "詰め16: 欧文間 停止 (6)")
      (la-check= k 0 "詰め16: 括弧 ベタ (0)")
      (la-check= w 4 "詰め16: 和欧間 6→4 (最後)"))))

;;; --- 空け (追い出し) の段階順 + 段4上限 -----------------------------------
(defun expansion-line ()
  "box eng box 和欧間 box kanjiskip box。自然幅 110。eng=1 和欧間=3 字間=5。"
  (let* ((rs (default-ruleset)) (sz 24))
    (coerce (list (make-box 24)
                  (latin-space-glue 8 sz 0)              ; eng: stretch4 pri2
                  (make-box 24)
                  (jfm-glue->glue (rs-xkanjiskip rs) sz)  ; 和欧間: stretch6 pri1
                  (make-box 24)
                  (jfm-glue->glue (rs-kanjiskip rs) sz)   ; 字間: stretch6 pri0
                  (make-box 24))
            'vector)))

(defun test-expansion-order ()
  (let ((items (expansion-line)))            ; 自然幅 110
    ;; 3空け: 欧文間だけ伸びる
    (destructuring-bind (e w j) (la-sizes items 113 1 3 5)
      (la-check= e 11 "空け3: 欧文間 8→11")
      (la-check= w 6  "空け3: 和欧間 不動")
      (la-check= j 0  "空け3: 字間 不動"))
    ;; 12空け: 欧文間4+和欧間6を使い切り、残り2を字間へ
    (destructuring-bind (e w j) (la-sizes items 122 1 3 5)
      (la-check= e 12 "空け12: 欧文間 二分で停止 (12)")
      (la-check= w 12 "空け12: 和欧間 二分で停止 (12)")
      (la-check= j 2  "空け12: 字間 0→2"))
    ;; 20空け (段4): 全段を超えても欧文間/和欧間は二分で頭打ち、
    ;;   余りは最下位段=字間(分割可能文字間)だけが無限に空く
    (destructuring-bind (e w j) (la-sizes items 130 1 3 5)
      (la-check= e 12 "段4: 欧文間 二分で頭打ち (超過しない)")
      (la-check= w 12 "段4: 和欧間 二分で頭打ち (超過しない)")
      (la-check= j 10 "段4: 字間が残余を吸収 (6→10)"))))

(defun run-line-adjustment-tests ()
  "§4.19 行長調整の回帰テストを走らせる。全通過なら T。"
  (setf *la-checks* 0 *la-fails* 0)
  (test-latin-space-params)
  (test-yakumono-priorities)
  (test-compression-order)
  (test-expansion-order)
  (format t "~&§4.19 line-adjustment: ~a/~a checks passed~a~%"
          (- *la-checks* *la-fails*) *la-checks*
          (if (zerop *la-fails*) "  OK" "  *** FAIL ***"))
  (zerop *la-fails*))
