;;;; japanese-stress.lisp -- 和文形の負荷試験
;;;;
;;;; 検証したいこと:
;;;;   (a) 全文字間が breakpoint でも active node が膨らまないか (刈り込みは効くか)
;;;;   (b) 禁則が penalty だけで正しく実現できるか
;;;;   (c) 行頭禁則と行末禁則が【同じ機構】に畳めるか
;;;;
;;;; 表もフォントも使わない。合成データのみ。
;;;; ソースに日本語リテラルを置かない (SBCL on Windows の外部形式を変数から外すため)。

(in-package #:quad)

;;; ---------------------------------------------------------------------------
;;; 文字クラス (最小限)
;;; ---------------------------------------------------------------------------

(defparameter *line-start-forbidden*
  '(#x3002 #x3001 #x300D #x300F #xFF09 #xFF01 #xFF1F #x30FC #x3005
    #x3041 #x3043 #x3045 #x3047 #x3049 #x3063 #x3083 #x3085 #x3087)
  "行頭禁則。句点 読点 閉じ括弧 感嘆符 疑問符 長音 々 小書き仮名。")

(defparameter *line-end-forbidden*
  '(#x300C #x300E #xFF08)
  "行末禁則。開き括弧。")

;;; 約物の空き。JLReq/JFM の大幅な単純化:
;;;   句読点・閉じ括弧は全角枠に字面が寄り、【後ろ】に半角アキ
;;;   開き括弧は全角枠に字面が寄り、【前】に半角アキ
;;; そのアキは詰められる (= 追い込みの原資)。本物は約25クラスの対表。
(defun stress-space-after (code)
  (if (member code '(#x3002 #x3001 #x300D #x300F #xFF09)) 1/2 0))
(defun stress-space-before (code)
  (if (member code '(#x300C #x300E #xFF08)) 1/2 0))

(defun stress-char-advance (code)
  "字面の幅。前後のアキを引いた残り。全角なら 1、約物なら 1/2。"
  (- 1 (stress-space-after code) (stress-space-before code)))

(defparameter *kanjiskip-stretch* 1/4
  "jfm-jlreq.lua: kanjiskip = {0, 0.25, 0}。自然幅 0、伸びのみ、縮みなし。")

;;; ---------------------------------------------------------------------------
;;; item 生成
;;; ---------------------------------------------------------------------------

(defun japanese-items (codes &key (kinsoku t))
  "コードポイントの列を item 列にする。

   ★禁則の実装がこの関数の要点。行頭禁則も行末禁則も
     『この位置で切ってはいけない』という一つの判定に畳める:
       行頭禁則 (B を行頭に置かない) = A と B のあいだで切らない
       行末禁則 (A を行末に置かない) = A と B のあいだで切らない
     どちらも分割点の左右から見ているだけなので、同じ penalty になる。

   ★penalty を glue の【前】に置くのが肝。legal-breakpoint-p は
     『直前が捨てられない要素のときだけ glue で切れる』ので、
     penalty を挟むと glue も penalty 自身も分割点にならない。"
  (let ((n (length codes))
        (items '()))
    (dotimes (i n)
      (let ((c (aref codes i)))
        (push (make-glyph-box (stress-char-advance c) (string (code-char c))
                              :source-start i :source-end (1+ i))
              items)
        (when (< (1+ i) n)
          (let* ((next (aref codes (1+ i)))
                 (gap (+ (stress-space-after c) (stress-space-before next)))
                 (forbid (and kinsoku
                              (or (member next *line-start-forbidden*)
                                  (member c *line-end-forbidden*)))))
            (when forbid
              (push (make-penalty +inf-penalty+) items))
            (push (make-glue gap
                             :stretch (if (zerop gap) *kanjiskip-stretch* 0)
                             :shrink gap)
                  items)))))
    (nreverse items)))

;;; ---------------------------------------------------------------------------
;;; 合成テキスト (決定的な擬似乱数)
;;; ---------------------------------------------------------------------------

(defvar *seed* 20260720)
(defun nextrand (n)
  (setf *seed* (mod (+ (* *seed* 1103515245) 12345) 2147483648))
  (mod (floor *seed* 65536) n))

(defparameter *pool*
  (coerce (append
           ;; ひらがな
           (loop for c from #x3042 to #x3093 collect c)
           ;; 漢字 (適当に散らす)
           '(#x65E5 #x672C #x8A9E #x6587 #x5B57 #x7D44 #x7248 #x884C #x5206 #x5272
             #x898F #x5247 #x51E6 #x7406 #x6A5F #x69CB #x8A2D #x8A08 #x5B9F #x88C5
             #x6642 #x9593 #x4EBA #x624B #x76EE #x8005 #x5834 #x5408 #x4E0A #x4E0B))
          'vector))

(defun synth-japanese (n-chars)
  "N-CHARS 前後の合成和文。約物を現実的な頻度で混ぜる。"
  (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
    (loop while (< (fill-pointer out) n-chars)
          do (let ((sentence-len (+ 8 (nextrand 22))))
               ;; たまに鉤括弧で囲む
               (when (zerop (nextrand 5))
                 (vector-push-extend #x300C out)
                 (dotimes (i (+ 3 (nextrand 6)))
                   (vector-push-extend (aref *pool* (nextrand (length *pool*))) out))
                 (vector-push-extend #x300D out))
               (dotimes (i sentence-len)
                 (vector-push-extend (aref *pool* (nextrand (length *pool*))) out)
                 ;; 読点をたまに
                 (when (and (> i 3) (zerop (nextrand 12)))
                   (vector-push-extend #x3001 out)))
               (vector-push-extend #x3002 out)))
    out))

;;; ---------------------------------------------------------------------------
;;; 検証
;;; ---------------------------------------------------------------------------

(defun line-boundaries (items breaks)
  "各行の (先頭の文字, 末尾の文字) を返す。禁則が守られたかの確認用。"
  (let* ((v (coerce (finish-paragraph items) 'vector))
         (start 0)
         (result '()))
    (dolist (br breaks)
      (let ((b (getf br :position))
            (first-ch nil) (last-ch nil))
        (loop for i from start below b
              for item = (aref v i)
              when (typep item 'glyph-box)
                do (unless first-ch (setf first-ch (box-glyphs item)))
                   (setf last-ch (box-glyphs item)))
        (when first-ch (push (cons first-ch last-ch) result))
        (setf start (skip-discardables v (1+ b)))))
    (nreverse result)))

(defun check-kinsoku (bounds)
  "禁則違反を数える。"
  (let ((head-violations '()) (tail-violations '()))
    (dolist (b bounds)
      (let ((head (char-code (char (car b) 0)))
            (tail (char-code (char (cdr b) 0))))
        (when (member head *line-start-forbidden*) (push head head-violations))
        (when (member tail *line-end-forbidden*)   (push tail tail-violations))))
    (values head-violations tail-violations)))

;;; ---------------------------------------------------------------------------

(defun stress (&key (n-chars 1000) (width 40))
  (let* ((*seed* 20260720)
         (codes (synth-japanese n-chars)))
    (format t "~&=== 和文負荷試験: ~:d 文字 / 行幅 ~d 文字 ===~%" (length codes) width)
    (dolist (kinsoku '(nil t))
      (let* ((items (japanese-items codes :kinsoku kinsoku))
             (t0 (get-internal-real-time)))
        (multiple-value-bind (breaks stats) (break-paragraph items width)
          (let ((elapsed (/ (- (get-internal-real-time) t0)
                            internal-time-units-per-second)))
            (multiple-value-bind (head tail)
                (check-kinsoku (line-boundaries items breaks))
              (format t "~%-- 禁則 ~a --~%" (if kinsoku "あり" "なし"))
              (format t "  item 数      : ~:d~%" (getf stats :items))
              (format t "  分割候補     : ~:d~%" (getf stats :breakpoints))
              (format t "  active 最大  : ~d~%" (getf stats :max-active))
              (format t "  探索した辺   : ~:d~%" (getf stats :edges))
              (format t "  行数         : ~d~%" (length breaks))
              (format t "  総 demerits  : ~:d~%" (getf stats :demerits))
              (format t "  時間         : ~,3f 秒~%" (float elapsed))
              (format t "  禁則違反     : 行頭 ~d 件 / 行末 ~d 件~a~%"
                      (length head) (length tail)
                      (if (and kinsoku (or head tail)) "   <== 失敗" "")))))))))

(defun scaling (&key (width 40))
  "文字数に対する計算量の伸び。O(n) に近ければ刈り込みが効いている。"
  (format t "~&文字数    item数   候補数  active最大    辺数     時間~%")
  (dolist (n '(500 1000 2000 4000 8000))
    (let* ((*seed* 20260720)
           (codes (synth-japanese n))
           (items (japanese-items codes :kinsoku t))
           (t0 (get-internal-real-time)))
      (multiple-value-bind (breaks stats) (break-paragraph items width)
        (declare (ignore breaks))
        (format t "~7:d ~8:d ~8:d ~11d ~8:d ~8,3f~%"
                (length codes) (getf stats :items) (getf stats :breakpoints)
                (getf stats :max-active) (getf stats :edges)
                (float (/ (- (get-internal-real-time) t0)
                          internal-time-units-per-second)))))))
