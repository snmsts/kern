;;;; document.lisp -- B 層 (S 式文書) のデモ
;;;;
;;;; ここは B 層の価値を見せる場なので、日本語を S 式に直接書く。
;;;; read がこれをパースし、layout-document が既存のエンジン (ルビ・§4.19・行分割) に繋ぐ。

(in-package #:kern)

(defparameter *sample-doc*
  '(:document (:size 14 :line-width 294)
    (:p "吾輩は" (:ruby "猫" "ねこ") "である。"
        (:ruby "名" "な") (:ruby "前" "まえ") "はまだ" (:ruby "無" "な") "い。")
    (:p "どこで" (:ruby "生" "う") "れたか" (:ruby "見" "み") "当がつかぬ。"
        "「どこで" (:ruby "生" "う") "れたか」" "とも" (:ruby "思" "おも") "う。")
    (:p "これは" (:group "組版" "くみはん") "エンジンの" (:group "試験" "しけん") "である。"
        (:jukugo "二十" ("に" "じゅう")) "の" (:jukugo "名前" ("な" "まえ")) "も" (:ruby "組" "く") "める。"))
  "B 層のサンプル文書。段落・モノルビ・グループルビ・熟語ルビ・約物・括弧を含む。")

(defun run-document-pdf (&key (doc *sample-doc*))
  "S 式文書を組んで PDF に描く。B 層の端から端まで。"
  (let* ((fm    (pdf:load-ttf-font *ttf*))
         (font  (pdf:get-font (pdf::font-name fm)))
         (size  (or (getf (document-options doc) :size) 14))
         (paras (layout-document doc font))
         (codes (document-codes doc)))
    (format t "~&=== B 層 (S 式文書) デモ ===~%")
    (format t "  段落数   : ~d~%" (length paras))
    (format t "  各段落行 : ~{~d~^ ~}~%" (mapcar #'length paras))
    (install-subset fm *ttf* codes)
    (let ((pdf-path (rel "demo/document.pdf")))
      (pdf:with-document ()
        (pdf:with-page ()
          (draw-document paras font size :x 60 :y 780
                                         :line-pitch (* size 9/5) :para-gap (* size 1))
          (install-tounicode font codes))
        (pdf:write-document pdf-path))
      (format t "  PDF      : ~a (~:d bytes)~%" pdf-path
              (with-open-file (in pdf-path :element-type '(unsigned-byte 8))
                (file-length in))))))
