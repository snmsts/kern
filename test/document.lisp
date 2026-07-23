;;;; test/document.lisp -- B 層 (S 式文書) のパース部の回帰テスト (フォント不要)
;;;;
;;;; インライン内容→item やレイアウトはフォント計測が要る (デモで視覚検証)。
;;;; ここは木を歩く純粋な部分 (平坦化・コード収集・opts) を固める。

(in-package #:kern)

(defun codes-of (s) (map 'list #'char-code s))

(defun test-document-parse ()
  ;; %inline-atoms: 文字列は各字コードへ、ruby 形はそのまま atom に。
  (la-check (equal (%inline-atoms (list "ab" (list :ruby "猫" "ねこ") "c"))
                   (append (codes-of "ab") (list (list :ruby "猫" "ねこ")) (codes-of "c")))
            "inline 平坦化")
  ;; %atom-code: ruby 形の先頭は base の先頭字。
  (la-check= (%atom-code (list :ruby "猫" "ねこ")) (char-code #\猫) "ruby atom の code=base先頭")
  (la-check= (%atom-code 65) 65 "整数 atom はそのまま")
  ;; document-codes: 親+ルビの全コードを順に。
  (let ((doc '(:document (:size 12)
               (:p "あ" (:ruby "猫" "ねこ"))
               (:p (:group "大人" "おとな") (:jukugo "二十" ("に" "じゅう"))))))
    (la-check (equal (coerce (document-codes doc) 'list)
                     (codes-of "あ猫ねこ大人おとな二十にじゅう"))
              "document-codes: 全コード収集"))
  ;; opts / blocks
  (la-check (equal (document-options '(:document (:size 12) (:p "x"))) '(:size 12)) "opts あり")
  (la-check (null  (document-options '(:document (:p "x")))) "opts 省略")
  (la-check= (length (document-blocks '(:document (:size 12) (:p "a") (:p "b")))) 2 "blocks 数 (opts 有)")
  (la-check= (length (document-blocks '(:document (:p "a") (:p "b")))) 2 "blocks 数 (opts 無)"))

(defun run-document-tests ()
  "B 層パース部の回帰テスト。全通過なら T。"
  (setf *la-checks* 0 *la-fails* 0)
  (test-document-parse)
  (format t "~&document (B層 parse): ~a/~a checks passed~a~%"
          (- *la-checks* *la-fails*) *la-checks*
          (if (zerop *la-fails*) "  OK" "  *** FAIL ***"))
  (zerop *la-fails*))
