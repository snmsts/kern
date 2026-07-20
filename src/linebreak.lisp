;;;; linebreak.lisp -- Knuth & Plass の最適行分割
;;;;
;;;; Knuth, D. E. and Plass, M. F. "Breaking Paragraphs into Lines",
;;;; Software: Practice and Experience 11 (1981), 1119-1184.
;;;;
;;;; ★この層は言語非依存であること。総称関数にしない。
;;;;   層2 に拡張点があったら、言語規則を層1 (item 生成) に畳めていないというサイン。
;;;;   禁則も約物も和欧間も、全て penalty と glue の値として入ってくる。
;;;;
;;;; ★priority (JLReq の段階的な詰め) はここでは見ない。
;;;;   行分割の判断は伸縮の【総量】から出るので、配分の【順序】には影響されない。
;;;;   priority を見るのはグルー解決器 (set-glue、未実装)。

(in-package #:typeset)

(defstruct (break-params (:conc-name param-))
  ;; badness がこれを超える分割候補は捨てる。TeX の \tolerance。
  ;; ★これは品質の閾値であって、計算量の主因ではない (実測)。
  ;;   active node を支配していたのは候補の鍵に行番号を含めるかどうかだった。
  ;;   uniform-width の項を参照。
  (tolerance 200)
  ;; 行数を増やすことのコスト。TeX の \linepenalty。
  (line-penalty 10)
  ;; flagged な分割が連続することの追加コスト。TeX の \doublehyphendemerits。
  (adjacent-demerits 10000)
  ;; 隣接する行の fitness class が2段以上離れることのコスト。TeX の \adjdemerits。
  (fitness-demerits 3000))

(defstruct (node (:conc-name node-))
  (position 0)        ; 分割位置 (items の index)
  (line 0)            ; この分割で終わる行の番号
  (fitness 2)         ; 0=very loose 1=loose 2=decent 3=tight
  (after 0)           ; 次の行が始まる index (捨てる要素を飛ばした後)
  (demerits 0)        ; ここまでの累積 demerits
  (ratio 0)           ; この行の調整比 (報告用)
  (flagged nil)       ; この分割位置が flagged だったか
  (previous nil))

;;; ---------------------------------------------------------------------------
;;; 補助
;;; ---------------------------------------------------------------------------

(defun skip-discardables (items i)
  "I から始めて、捨てられる要素 (glue / penalty) を飛ばした位置を返す。
   分割の直後に来る空きを行頭に残さない、という規則の実装。"
  (let ((n (length items)))
    (loop while (and (< i n) (discardable-p (aref items i)))
          do (incf i))
    i))

(defun legal-breakpoint-p (items i)
  "I で分割してよいか。TeX と同じ規則:
     - glue は、直前が捨てられない要素 (= box) のときだけ分割可能。
       これで行頭の空きや連続する空きの途中では切れなくなる。
     - penalty は、値が禁止でなければ分割可能。★禁則はここで効く。"
  (let ((item (aref items i)))
    (typecase item
      (glue (and (plusp i) (not (discardable-p (aref items (1- i))))))
      (penalty (< (penalty-value item) +inf-penalty+))
      (t nil))))

(defun forced-break-p (items i)
  (let ((item (aref items i)))
    (and (typep item 'penalty)
         (<= (penalty-value item) +forced-break+))))

(defun adjustment-ratio (natural stretch shrink inf-stretch target)
  "行の調整比。正なら伸ばす、負なら縮める。

   ★NIL を返さない。『足りない』と『溢れた』を区別するのが要点:
     - 足りないが伸ばせない → 大きな正の値。badness は最大になるが r >= -1 なので
       node は生存する。後続の材料を足せば救えるので殺してはいけない。
     - 溢れて縮められない   → -2。r < -1 なので node は落とされる。もう救えない。"
  (cond
    ;; 無限に伸びる glue があれば常にぴったり収まる (最終行の \hfil がこれ)
    ((and (< natural target) (plusp inf-stretch)) 0)
    ((< natural target) (if (plusp stretch) (/ (- target natural) stretch) 10000))
    ((> natural target) (if (plusp shrink)  (/ (- target natural) shrink)  -2))
    (t 0)))

(defun badness (r)
  "TeX の badness = 100 |r|^3、上限 10000。"
  (if (> (abs r) 5)
      10000                            ; 100*5^3 = 12500 > 10000 なので早期打ち切り
      (min 10000 (round (* 100 (expt (abs r) 3))))))

(defun fitness-class (r)
  (cond ((< r -1/2) 3)                 ; tight
        ((<= r 1/2) 2)                 ; decent
        ((< r 1) 1)                    ; loose
        (t 0)))                        ; very loose

(defun compute-demerits (bad pen flagged prev-flagged fitness prev-fitness params)
  (let* ((base (expt (+ (param-line-penalty params) bad) 2))
         (d (cond ((>= pen 0)             (+ base (* pen pen)))
                  ((> pen +forced-break+) (- base (* pen pen)))
                  (t                      base))))
    (when (and flagged prev-flagged)
      (incf d (param-adjacent-demerits params)))
    (when (> (abs (- fitness prev-fitness)) 1)
      (incf d (param-fitness-demerits params)))
    d))

;;; ---------------------------------------------------------------------------
;;; 段落の締め
;;; ---------------------------------------------------------------------------

(defun finish-paragraph (items)
  "TeX の \\par と同じ末尾を付ける: 禁止ペナルティ、無限に伸びる空き、強制分割。
   最終行が均等割りされずに残るのはこの無限グルーのため。
   また、最終分割は必ず r=0 になるので、分割が必ず成立することも保証される。"
  (append (coerce items 'list)
          (list (make-penalty +inf-penalty+)
                (make-glue 0 :stretch 1 :stretch-order 1)
                (make-penalty +forced-break+))))

;;; ---------------------------------------------------------------------------
;;; 本体
;;; ---------------------------------------------------------------------------

(defun break-paragraph (items line-width &key (params (make-break-params)))
  "ITEMS を行に分割する。LINE-WIDTH は数か、行番号を取る関数。
   返り値は分割の並び: 各要素が (:position P :ratio R :line N)。

   動的計画法。active node が『まだ続きうる分割の候補』で、
   各 breakpoint で全 active node から接続を試し、
   実行可能なものを (行番号, fitness) ごとに最良1つだけ残す。"
  (let* ((items (coerce (finish-paragraph items) 'vector))
         (n (length items))
         (width-fn (if (functionp line-width) line-width (constantly line-width)))
         ;; ★行幅が一定なら、候補を行番号で分ける必要がない。
         ;;   行番号を鍵に含めるのは (a) 行ごとに幅が変わる場合 (回り込み) と
         ;;   (b) TeX の \looseness のように行数そのものを操作したい場合だけ。
         ;;   最小 demerits だけが目的なら、行数の差は line-penalty が既に勘定している。
         ;;   ここを分けると到達可能な行数の広がりぶん active が膨らみ、
         ;;   長い段落で計算量が二乗になる。
         (uniform-width (not (functionp line-width)))
         (max-active 1)        ; active node 数の最大 (刈り込みが効いているかの指標)
         (n-breakpoints 0)     ; 検討した分割候補の数
         (n-edges 0)           ; (node, breakpoint) の組み合わせを見た回数
         ;; 累積和。w[i] = items[0..i-1] の advance の合計。
         ;; 行の材料は [node-after(a), b) なので、差で取れる。
         (w  (make-array (1+ n) :initial-element 0))
         (y  (make-array (1+ n) :initial-element 0))   ; 有限の伸び
         (z  (make-array (1+ n) :initial-element 0))   ; 縮み
         (yi (make-array (1+ n) :initial-element 0)))  ; 無限位数の伸び
    (loop for i from 0 below n
          for item = (aref items i)
          do (setf (aref w  (1+ i)) (+ (aref w i) (advance item))
                   (aref y  (1+ i)) (+ (aref y i)
                                       (if (zerop (stretch-order item)) (stretch item) 0))
                   (aref z  (1+ i)) (+ (aref z i) (shrink item))
                   (aref yi (1+ i)) (+ (aref yi i)
                                       (if (plusp (stretch-order item)) (stretch item) 0))))
    (let ((active (list (make-node :position 0 :line 0 :fitness 2 :after 0))))
      (dotimes (b n)
        (when (legal-breakpoint-p items b)
          (incf n-breakpoints)
          (let* ((item (aref items b))
                 (forced (forced-break-p items b))
                 (pen (if (typep item 'penalty) (penalty-value item) 0))
                 (flagged (and (typep item 'penalty) (flagged-p item)))
                 (candidates (make-hash-table :test #'equal))
                 (survivors '())
                 (deactivated '()))
            (dolist (a active)
              (incf n-edges)
              (let* ((from (node-after a))
                     (natural (- (aref w b) (aref w from)))
                     (st  (- (aref y b)  (aref y from)))
                     (sh  (- (aref z b)  (aref z from)))
                     (inf (- (aref yi b) (aref yi from)))
                     (line (1+ (node-line a)))
                     (r (adjustment-ratio natural st sh inf (funcall width-fn line)))
                     (bad (badness r)))
                ;; 溢れている、または強制分割なら、この node はここで打ち切り。
                ;; 『足りない』だけなら生存させる (後続の材料で救える)。
                (if (or forced (< r -1))
                    (push a deactivated)
                    (push a survivors))
                ;; 実行可能なら候補に入れる
                (when (and (<= -1 r) (<= bad (param-tolerance params)))
                  (let* ((fit (fitness-class r))
                         (key (if uniform-width fit (list line fit)))
                         (d (+ (node-demerits a)
                               (compute-demerits bad pen flagged (node-flagged a)
                                                 fit (node-fitness a) params)))
                         (cur (gethash key candidates)))
                    (when (or (null cur) (< d (first cur)))
                      (setf (gethash key candidates) (list d a r fit line)))))))
            ;; 生き残りに、この breakpoint で作った新しい node を足す
            (let ((new '())
                  (after (skip-discardables items (1+ b))))
              (maphash (lambda (key val)
                         (declare (ignore key))
                         (destructuring-bind (d a r fit line) val
                           (push (make-node :position b :line line :fitness fit
                                            :after after :demerits d :ratio r
                                            :flagged flagged :previous a)
                                 new)))
                       candidates)
              (setf active (nconc new survivors))
              (setf max-active (max max-active (length active)))
              ;; 保険: 全滅したら、落とした node のうち最も筋の良いものから強制的に切る。
              ;; TeX は tolerance を上げて組み直す多段構成だが、ここでは1行だけ諦める。
              ;; (\hfil + 強制分割で終わるので、最終分割では起きないはず)
              (when (and (null active) deactivated)
                (let ((a (first (sort deactivated #'< :key #'node-demerits))))
                  (setf active
                        (list (make-node :position b :line (1+ (node-line a)) :fitness 3
                                         :after after
                                         :demerits (+ (node-demerits a) 100000)
                                         :ratio -1 :flagged flagged :previous a)))))))))
      ;; 最終の強制分割で作られた node のうち demerits 最小のものを採る
      (let ((best (first (sort (copy-list active) #'< :key #'node-demerits))))
        (unless best
          (error "行分割に失敗した (active node が空)"))
        (values
         (nreverse
          (loop for node = best then (node-previous node)
                while (and node (node-previous node))
                collect (list :position (node-position node)
                              :ratio (node-ratio node)
                              :line (node-line node))))
         ;; 第2値: 刈り込みが効いているかを見るための統計
         (list :items n
               :breakpoints n-breakpoints
               :max-active max-active
               :edges n-edges
               :demerits (node-demerits best)))))))
