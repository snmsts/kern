;;;; uax14.lisp -- UAX #14 行分割の可否 (日本語向けサブセット)
;;;;
;;;; UAX #14 の本質は「各文字境界で、両側の文字クラスの対から
;;;; 切ってよい/だめ/必ず切る を引く」こと。禁則 (forbid-break-p) と同じ形。
;;;;
;;;; ★これは【正しい形のサブセット】。フル化は再設計ではなく:
;;;;   - break-class を LineBreak.txt を読む版に差し替える (jfm と同じやり方)
;;;;   - uax14-prohibited-p に LB 規則の残りを足す (追加のみ)
;;;;   呼び出し側 (layout.lisp) は変えない。
;;;;
;;;; ★役割分担:
;;;;   和文の細かい分割規則 (禁則) は JFM 由来の kinsoku が持つ (JLReq の tailoring)。
;;;;   ここは欧文・数字・記号の世界を受け持つ。両者は break-prohibited-p で合流する。
;;;;   そのため和文 (漢字・仮名・和字約物) は一括して :id に落とし、
;;;;   細かい面倒は kinsoku に任せる。
;;;;
;;;; クラス (UAX #14 の略号、サブセット):
;;;;   ID 表意  AL 英字  NU 数字  OP 開き  CL 閉じ  CP 閉じ丸  QU 引用符
;;;;   HY ハイフン  BA 後分割  BB 前分割  NS 非開始  EX 感嘆  IS 中置separator
;;;;   IN 不可分  SY スラッシュ  PO 後置  PR 前置  GL 不可分空白  WJ 語結合
;;;;   ZW ゼロ幅空白  SP 空白 (境界では扱わない)

(in-package #:kern)

(defun break-class (code)
  "コードポイントの UAX #14 行分割クラス (サブセット)。
   ★和文・CJK は一括 :id。細かい tailoring は kinsoku に任せる。
   ★フル化: この関数を LineBreak.txt を読む版に差し替える。"
  (cond
    ((= code #x20) :sp)
    ;; --- ASCII を正確に ---
    ((<= #x30 code #x39) :nu)                 ; 0-9
    ((or (<= #x41 code #x5A) (<= #x61 code #x7A)) :al)  ; A-Z a-z
    ((member code '(#x28 #x5B #x7B)) :op)     ; ( [ {
    ((member code '(#x29 #x5D #x7D)) :cp)     ; ) ] }
    ((= code #x2D) :hy)                        ; -
    ((= code #x2F) :sy)                        ; /
    ((member code '(#x21 #x3F)) :ex)          ; ! ?
    ((member code '(#x22 #x27)) :qu)          ; " '
    ((member code '(#x2E #x2C #x3A #x3B)) :is) ; . , : ;
    ((= code #x25) :po)                        ; %
    ((member code '(#x24 #x23)) :pr)          ; $ #
    ((<= #x21 code #x7E) :al)                  ; 残りの ASCII 図形文字
    ;; --- 和文・CJK は一括 :id (kinsoku が細部を持つ) ---
    ((<= #x2E80 code #x9FFF) :id)              ; CJK 部首・漢字
    ((<= #x3000 code #x30FF) :id)              ; CJK 記号・かな
    ((<= #x31F0 code #x4DBF) :id)              ; かな拡張・漢字拡張A
    ((<= #xF900 code #xFAFF) :id)              ; CJK 互換漢字
    ((<= #xFF00 code #xFFEF) :id)              ; 半角・全角形
    ((<= #x20000 code #x3FFFF) :id)            ; CJK 拡張B以降
    ;; --- その他 ---
    ((= code #x00A0) :gl)                      ; NBSP
    ((= code #x200B) :zw)                      ; ゼロ幅空白
    (t :al)))                                  ; 未知は英字扱い (安全側)

;;; ---------------------------------------------------------------------------
;;; 対規則 (LB のサブセット、優先順)
;;; ---------------------------------------------------------------------------
;;; SP は境界に現れない (空白は glue として別に扱う) ので表から外す。

(defun uax14-prohibited-p (a b)
  "クラス A の後、クラス B の前で【切ってはいけない】か。
   UAX #14 の LB 規則のうち、空白と改行を除いたサブセット。
   ここに該当しなければ LB31 (既定) で切ってよい。"
  (or
   ;; LB11: 語結合子の前後で切らない
   (eq a :wj) (eq b :wj)
   ;; LB12: 不可分空白の後で切らない
   (eq a :gl)
   ;; LB13: 閉じ括弧・句点類・中置separator・スラッシュの前で切らない
   (member b '(:cl :cp :ex :is :sy))
   ;; LB14: 開き括弧の後で切らない
   (eq a :op)
   ;; LB15: 引用符 → 開き括弧 で切らない
   (and (eq a :qu) (eq b :op))
   ;; LB16: 閉じ括弧 → 非開始文字 で切らない
   (and (member a '(:cl :cp)) (eq b :ns))
   ;; LB19: 引用符の前後で切らない
   (eq a :qu) (eq b :qu)
   ;; LB21: 後分割・ハイフン・非開始の前、前分割の後で切らない
   (member b '(:ba :hy :ns)) (eq a :bb)
   ;; LB22: 不可分 (…) の前で切らない
   (eq b :in)
   ;; LB23: 英字と数字は続ける
   (and (eq a :al) (eq b :nu)) (and (eq a :nu) (eq b :al))
   ;; LB24: 前置・後置記号と英字は続ける
   (and (member a '(:pr :po)) (eq b :al)) (and (eq a :al) (member b '(:pr :po)))
   ;; LB25: 数の内部は続ける (数字・中置・スラッシュ・前後置)
   (and (eq a :nu) (member b '(:nu :is :sy :po :pr)))
   (and (member a '(:is :sy :pr)) (eq b :nu))
   ;; LB28: 英字どうしは続ける ← 欧単語をまとめる要
   (and (eq a :al) (eq b :al))
   ;; LB29: 中置separator → 英字 は続ける
   (and (eq a :is) (eq b :al))
   ;; LB30: 英数字 → 開き括弧、閉じ丸括弧 → 英数字 は続ける
   (and (member a '(:al :nu)) (eq b :op))
   (and (eq a :cp) (member b '(:al :nu)))))
