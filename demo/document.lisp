;;;; document.lisp -- B 層 (S 式文書) のデモ
;;;;
;;;; ここは B 層の価値を見せる場なので、日本語を S 式に直接書く。
;;;; read がこれをパースし、layout-document が既存のエンジン (ルビ・§4.19・行分割) に繋ぐ。

(in-package #:kern)

(defparameter *sample-doc*
  '(:document (:size 14 :line-width 294)
    (:h1 (:group "組版" "くみはん") "エンジン" (:group "試験" "しけん"))
    (:p "吾輩は" (:ruby "猫" "ねこ") "である。"
        (:ruby "名" "な") (:ruby "前" "まえ") "はまだ" (:ruby "無" "な") "い。")
    (:p "どこで" (:ruby "生" "う") "れたか" (:ruby "見" "み") "当がつかぬ。"
        "「どこで" (:ruby "生" "う") "れたか」" "とも" (:ruby "思" "おも") "う。")
    (:h2 "ルビの" (:ruby "種" "しゅ") "類")
    (:p "これは" (:group "組版" "くみはん") "エンジンの" (:group "試験" "しけん") "である。"
        (:jukugo "二十" ("に" "じゅう")) "の" (:jukugo "名前" ("な" "まえ")) "も" (:ruby "組" "く") "める。"))
  "B 層のサンプル文書。見出し・段落字下げ・モノ/グループ/熟語ルビ・約物・括弧を含む。")

(defparameter *vertical-doc*
  '(:document (:size 16 :line-width 360 :direction :vertical :indent nil)
    (:h1 (:ruby "縦" "たて") (:ruby "書" "が") "き")
    (:p "吾輩は" (:ruby "猫" "ねこ") "である。"
        (:ruby "名" "な") (:ruby "前" "まえ") "はまだ" (:ruby "無" "な") "い。")
    (:p "これは" (:group "縦組" "たてぐみ") "の" (:group "試験" "しけん") "である。"
        (:ruby "組" "く") "める。"))
  "縦書き文書のサンプル。同じ S 式に :direction :vertical を足すだけ。
   縦ルビ (親の右) が座標写像でそのまま出るかの試験も兼ねる。")

(defun run-document-pdf (&key (doc *sample-doc*) (out (rel "demo/document.pdf")))
  "S 式文書を組んで PDF に描く。B 層の端から端まで。:direction は文書 opts から。"
  (let* ((fm     (pdf:load-ttf-font *ttf*))
         (font   (pdf:get-font (pdf::font-name fm)))
         (dir    (getf (document-options doc) :direction :horizontal))
         (blocks (layout-document doc font))
         (codes  (document-codes doc)))
    (format t "~&=== B 層 (S 式文書) デモ [~a] ===~%" dir)
    (format t "  ブロック : ~d  各行数 ~{~d~^ ~}~%"
            (length blocks) (mapcar (lambda (b) (length (lb-lines b))) blocks))
    (install-subset fm *ttf* codes)
    (pdf:with-document ()
      (pdf:with-page ()
        ;; 縦組みは右上から列を左へ。横組みは左上から行を下へ。
        (draw-document blocks font :x (if (eq dir :vertical) 540 60) :y 800 :direction dir)
        (install-tounicode font codes))
      (pdf:write-document out))
    (format t "  PDF      : ~a (~:d bytes)~%" out
            (with-open-file (in out :element-type '(unsigned-byte 8)) (file-length in)))))

(defun run-vertical-document-pdf ()
  "縦書き文書デモ。*sample-doc* と同じ B 層で :direction :vertical だけ違う。"
  (run-document-pdf :doc *vertical-doc* :out (rel "demo/document-v.pdf")))
