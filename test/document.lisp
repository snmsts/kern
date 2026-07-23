;;;; test/document.lisp -- B 層 (S 式文書) のパース部の回帰テスト (フォント不要)
;;;;
;;;; インライン内容→item やレイアウトはフォント計測が要る (デモで視覚検証)。
;;;; ここは木を歩く純粋な部分 (平坦化・コード収集・opts) を固める。

(in-package #:kern)

(defun codes-of (s) (map 'list #'char-code s))

(defun test-document-parse ()
  ;; %inline-atoms: (atom . font) 対へ平坦化。文字列は各字、ruby 形はそのまま。font=nil。
  (la-check (equal (%inline-atoms (list "ab" (list :ruby "猫" "ねこ") "c") nil nil)
                   (append (mapcar (lambda (c) (cons c nil)) (codes-of "ab"))
                           (list (cons (list :ruby "猫" "ねこ") nil))
                           (mapcar (lambda (c) (cons c nil)) (codes-of "c"))))
            "inline 平坦化 (atom . font)")
  ;; (:font key ...) で font が切り替わる。
  (la-check (equal (%inline-atoms (list "a" (list :font :g "b") "c") :def (list :g :gothic))
                   (list (cons (char-code #\a) :def)
                         (cons (char-code #\b) :gothic)
                         (cons (char-code #\c) :def)))
            "(:font key ...) で最小単位フォント切替")
  ;; %atom-code: ruby 形の先頭は base の先頭字。
  (la-check= (%atom-code (list :ruby "猫" "ねこ")) (char-code #\猫) "ruby atom の code=base先頭")
  (la-check= (%atom-code 65) 65 "整数 atom はそのまま")
  ;; :em → 各字を圏点 atom (:kenten code) へ展開。
  (la-check (equal (%inline-atoms (list (list :em "強調")) nil nil)
                   (list (cons (list :kenten (char-code #\強)) nil)
                         (cons (list :kenten (char-code #\調)) nil)))
            ":em → 各字 :kenten")
  (la-check= (%atom-code (list :kenten (char-code #\強))) (char-code #\強) ":kenten atom の code")
  (let ((doc (list :document (list :p (list :em "強")))))
    (la-check (equal (coerce (document-codes doc) 'list)
                     (list (char-code #\強) (char-code (char *kenten-mark* 0))))
              ":em codes = 本文+圏点マーク"))
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
  (la-check= (length (document-blocks '(:document (:p "a") (:p "b")))) 2 "blocks 数 (opts 無)")
  ;; 見出しブロック (:h1/:h2) も block-heads・codes 収集に含まれる。
  (let ((doc '(:document (:h1 "見出し") (:p "本文"))))
    (la-check= (length (document-blocks doc)) 2 "見出し+段落=2ブロック")
    (la-check (equal (coerce (document-codes doc) 'list) (codes-of "見出し本文"))
              "見出しのコードも収集"))
  ;; block-style: 見出しは本文より大きく前後アキがある。
  (multiple-value-bind (ps pp pb pa) (block-style :p 12)
    (declare (ignore pp))
    (la-check= ps 12 ":p 級数=本文") (la-check= pb 0 ":p 前アキ0") (la-check= pa 0 ":p 後アキ0"))
  (multiple-value-bind (h1s) (block-style :h1 10)
    (la-check (> h1s 10) ":h1 は本文より大きい")))

(defun run-document-tests ()
  "B 層パース部の回帰テスト。全通過なら T。"
  (setf *la-checks* 0 *la-fails* 0)
  (test-document-parse)
  (format t "~&document (B層 parse): ~a/~a checks passed~a~%"
          (- *la-checks* *la-fails*) *la-checks*
          (if (zerop *la-fails*) "  OK" "  *** FAIL ***"))
  (zerop *la-fails*))
