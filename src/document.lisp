;;;; document.lisp -- B 層: S 式文書を item 列 → 行に落とす
;;;;
;;;; read がパーサ。ここは木を歩いて既存の text-items / ルビ / layout に繋ぐだけ。
;;;; keyword 頭にするのはパッケージ非依存にするため (ユーザがどの package で書いても同じ)。
;;;;
;;;;   (:document (:size 12 :line-width 240)
;;;;     (:p "吾輩は" (:ruby "猫" "ねこ") "である。")
;;;;     (:p "名前はまだ無い。"))
;;;;
;;;; インライン内容 = 文字列と ruby 形の並び:
;;;;   "文字列"                    → 各字 (emit-char-box)
;;;;   (:ruby "猫" "ねこ")          → モノルビ (親1字)
;;;;   (:group "大人" "おとな")     → グループルビ
;;;;   (:jukugo "二十" ("に" "じゅう")) → 熟語ルビ (ルビは親字ごとの list)

(in-package #:kern)

(defun %inline-atoms (content)
  "インライン内容を atom の list へ平坦化。atom = 文字コード (通常字) or ruby 形 (そのまま)。"
  (let ((atoms '()))
    (dolist (node content)
      (if (stringp node)
          (loop for ch across node do (push (char-code ch) atoms))
          (push node atoms)))
    (nreverse atoms)))

(defun %atom-code (atom)
  "atom の先頭親コード (inter-glue のクラス・overhang の隣判定に使う)。"
  (if (integerp atom) atom (char-code (char (second atom) 0))))

(defun %atom->box (atom font size rs left-code right-code)
  "atom を box にする。ruby 形は隣コードから overhang 可否を決める (except-kanji)。"
  (if (integerp atom)
      (emit-char-box rs font size atom 0 0)
      (ecase (first atom)
        (:ruby   (mono-ruby-box font size (char-code (char (second atom) 0)) (third atom)
                                :overhang-left-p  (and left-code  (not (kanji-code-p left-code)))
                                :overhang-right-p (and right-code (not (kanji-code-p right-code)))))
        (:group  (group-ruby-box  font size (second atom) (third atom)))
        (:jukugo (jukugo-ruby-box font size (second atom) (third atom))))))

(defun inline->items (content font size &key (ruleset (default-ruleset)))
  "インライン内容 (文字列と ruby 形の list) を item 列にする。
   隣接境界に JFM のクラス対 glue (inter-glue) を入れる。text-items のルビ対応・構造化版。"
  (let* ((rs ruleset)
         (atoms (coerce (%inline-atoms content) 'vector))
         (n (length atoms))
         (items '()) (prev nil))
    (dotimes (i n)
      (let* ((atom (aref atoms i))
             (lc   (when (> i 0)      (%atom-code (aref atoms (1- i)))))
             (rc   (when (< (1+ i) n) (%atom-code (aref atoms (1+ i)))))
             (box  (%atom->box atom font size rs lc rc))
             (class (char-class-of rs (%atom-code atom))))
        (when prev
          (let ((g (inter-glue rs prev class size)))
            (when g (push g items))))
        (push box items)
        (setf prev class)))
    (nreverse items)))

;;; ---------------------------------------------------------------------------
;;; 文書
;;; ---------------------------------------------------------------------------

(defparameter *block-heads* '(:p)
  "ブロック要素の頭。opts-plist と区別するのに使う (:p 等は opts でなくブロック)。")

(defun document-options (doc)
  "(:document [opts-plist] block...) の opts を返す。無ければ NIL。
   第2要素が keyword 頭の cons でも、それがブロック頭 (:p 等) なら opts ではない。"
  (let ((x (second doc)))
    (when (and (consp x) (keywordp (car x)) (not (member (car x) *block-heads*)))
      x)))

(defun document-blocks (doc)
  "(:document [opts] block...) の block 群。"
  (if (document-options doc) (cddr doc) (cdr doc)))

(defun layout-document (doc font &key size line-width (direction :horizontal))
  "S 式文書 DOC を段落ごとに組む。返り値は段落ごとの LAID-LINE list の list。
   SIZE / LINE-WIDTH は引数優先、無ければ文書 opts の :size / :line-width。"
  (let* ((opts (document-options doc))
         (sz   (or size (getf opts :size) 12))
         (lw   (or line-width (getf opts :line-width) (* sz 24)))
         (dir  (or direction (getf opts :direction) :horizontal)))
    (declare (ignore dir))              ; 方向は当面 backend が持つ (advance は中立)
    (loop for form in (document-blocks doc)
          when (and (consp form) (eq (first form) :p))
            collect (layout-items
                     (coerce (finish-paragraph (inline->items (rest form) font sz)) 'vector)
                     lw sz))))

(defun document-codes (doc)
  "文書中の全コードポイント (フォントのサブセット化用)。"
  (let ((codes '()))
    (labels ((str (s) (loop for ch across s do (push (char-code ch) codes)))
             (node (a)
               (if (stringp a)
                   (str a)
                   (ecase (first a)
                     (:ruby   (str (second a)) (str (third a)))
                     (:group  (str (second a)) (str (third a)))
                     (:jukugo (str (second a)) (dolist (p (third a)) (str p)))))))
      (dolist (form (document-blocks doc))
        (when (and (consp form) (eq (first form) :p))
          (dolist (n (rest form)) (node n)))))
    (coerce (nreverse codes) 'vector)))
