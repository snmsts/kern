;;;; setglue.lisp -- 段階付きグルー解決器
;;;;
;;;; 行分割器が「どこで切るか」を決めた後、この層が「各空きを実際に何にするか」を決める。
;;;;
;;;; ★TeX の素朴なグルー模型との違い。
;;;;   TeX は r = 必要量 / 伸縮の総量 を求め、各 glue に r × 自身の伸縮量 を配る。
;;;;   つまり【比例配分】。無限位数 (fil/fill/filll) だけが例外で、
;;;;   位数の高い glue があれば低い位数は一切動かない。
;;;;
;;;;   JLReq の詰めはこれで表現できない。jfm-jlreq.lua より:
;;;;       優先順位は，第n段階を 3-n に対応させる．
;;;;         段階   1, 2, 3, 4, 5, 6
;;;;       priority 2, 1, 0,-1,-2,-3
;;;;   これは【順序付きの有限量の消費】= 第1段階を使い切ってから第2段階、である。
;;;;   無限位数 (支配) とも比例配分とも別の、第三の機構。
;;;;
;;;;   したがって3層になる:
;;;;     1. 位数  -- 高い位数があれば低い位数は動かない        (TeX と同じ)
;;;;     2. 段階  -- 同位数のなかで priority の高い順に使い切る (JLReq)
;;;;     3. 比例  -- 同段階のなかで伸縮量に比例して配る        (TeX と同じ)
;;;;   cl-typesetting の spread-boxes (layout.lisp:193-232) は 3 だけを実装している。
;;;;
;;;; ★item は書き換えない。実寸の配列を新しく返す。
;;;;   同じ item 列を別の幅で組み直せるようにするため (GUI の再レイアウト)。

(in-package #:kern)

(defun %participants (items start end amount-fn order-fn)
  "伸縮に参加する glue を集め、最高位数のものだけに絞って (位置 . item) の並びを返す。"
  (let ((max-order 0)
        (all '()))
    (loop for i from start below end
          for item = (aref items i)
          when (and (typep item 'glue) (plusp (funcall amount-fn item)))
            do (setf max-order (max max-order (funcall order-fn item)))
               (push (cons i item) all))
    (remove-if-not (lambda (p) (= (funcall order-fn (cdr p)) max-order))
                   (nreverse all))))

(defun %distribute (sizes start need participants amount-fn priority-fn sign overuse-p)
  "NEED (常に正) を PARTICIPANTS に配る。SIGN は +1 (伸ばす) か -1 (縮める)。

   priority の高い段階から順に使い切り、足りなければ次の段階へ。
   同じ段階のなかでは伸縮量に比例して配る。

   OVERUSE-P が真なら、全段階を使い切ってもまだ残る場合に【最下位段階だけ】へ
   比例で【超過して】配る (伸ばしすぎ。TeX の r>1 に相当)。
   全参加者でなく最下位段階に限るのは JIS X 4051 §4.19 空け段4 に沿うため:
   上位段階 (欧文間=二分, 和欧間=二分) はそれぞれの上限で止め、余りは最下位段階
   (=分割可能文字間の kanjiskip 字間) だけを無限に空ける。純欧文行では欧文間が
   唯一の段=最下位なので、従来どおり欧文間が上限を越えて伸びる (欧文の均等割り)。
   偽なら残りをそのまま返す。縮みは自然幅 - 縮み量を下回れないのでこちら。

   返り値: 配りきれずに残った量。"
  (flet ((amount (p) (funcall amount-fn (cdr p)))
         (priority (p) (funcall priority-fn (cdr p)))
         (give (p x) (incf (aref sizes (- (car p) start)) (* sign x))))
    (let ((stages (sort (remove-duplicates (mapcar #'priority participants)) #'>))
          (remaining need))
      (dolist (pri stages)
        (when (plusp remaining)
          (let* ((group (remove-if-not (lambda (p) (= (priority p) pri)) participants))
                 (avail (reduce #'+ group :key #'amount :initial-value 0)))
            (cond
              ((zerop avail))
              ((<= remaining avail)
               ;; この段階で足りる。段階内は伸縮量に比例して配って終わり
               (dolist (p group)
                 (give p (* remaining (/ (amount p) avail))))
               (setf remaining 0))
              (t
               ;; 段階を使い切って次へ
               (dolist (p group) (give p (amount p)))
               (decf remaining avail))))))
      (when (and (plusp remaining) overuse-p stages)
        ;; §4.19 空け段4: 最下位段階 (分割可能文字間) だけを無限に空ける。
        ;; 上位段階は自身の上限で止まったまま = 欧文間/和欧間は二分を越えない。
        (let* ((lowest (car (last stages)))
               (group (remove-if-not (lambda (p) (= (priority p) lowest)) participants))
               (total (reduce #'+ group :key #'amount :initial-value 0)))
          (when (plusp total)
            (dolist (p group)
              (give p (* remaining (/ (amount p) total))))
            (setf remaining 0))))
      remaining)))

(defun set-glue (items target &key (start 0) end)
  "ITEMS[START..END) を幅 TARGET に収めたときの各要素の実寸の配列を返す。

   返り値: (values 実寸の配列 状態 残量)
     状態  :exact / :stretched / :shrunk / :underfull / :overfull
     :underfull は伸ばす材料が無かった場合、:overfull は縮みきれなかった場合。

   ★縮みは自然幅 - 縮み量を下回れない (TeX と同じ)。伸びには上限がない。"
  (let* ((items (if (vectorp items) items (coerce items 'vector)))
         (end (or end (length items)))
         (sizes (make-array (- end start) :initial-element 0))
         (natural 0))
    (loop for i from start below end
          for k from 0
          for a = (advance (aref items i))
          do (setf (aref sizes k) a)
             (incf natural a))
    (let ((delta (- target natural)))
      (cond
        ((zerop delta)
         (values sizes :exact 0))
        ((plusp delta)
         (let ((participants (%participants items start end #'stretch #'stretch-order)))
           (if (null participants)
               (values sizes :underfull delta)
               (let ((left (%distribute sizes start delta participants
                                        #'stretch #'stretch-priority +1 t)))
                 (values sizes (if (plusp left) :underfull :stretched) left)))))
        (t
         (let* ((need (- delta))
                (participants (%participants items start end #'shrink #'shrink-order)))
           (if (null participants)
               (values sizes :overfull need)
               (let ((left (%distribute sizes start need participants
                                        #'shrink #'shrink-priority -1 nil)))
                 (values sizes (if (plusp left) :overfull :shrunk) left)))))))))
