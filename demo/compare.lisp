;;;; compare.lisp -- 貪欲法 vs Knuth-Plass
;;;;
;;;; 1文字 = 1単位として ASCII で組み、空きを実際に伸縮させて描く。
;;;; 行末が揃うのは両方同じ。違いは【空きの散らばり方】に出る。

(in-package #:kern)

(defparameter *space-natural* 1)
(defparameter *space-stretch* 1/2)
(defparameter *space-shrink*  1/3)

(defun split-words (text)
  (loop with n = (length text)
        with i = 0
        while (< i n)
        for start = (position #\Space text :start i :test-not #'char=)
        while start
        for end = (or (position #\Space text :start start) n)
        collect (subseq text start end)
        do (setf i end)))

(defun text->items (text)
  "単語を box、単語間を glue にする。source-start/end も埋める。"
  (let ((items '())
        (pos 0)
        (first t))
    (dolist (word (split-words text))
      (unless first
        (push (make-glue *space-natural*
                         :stretch *space-stretch* :shrink *space-shrink*)
              items)
        (incf pos))
      (setf first nil)
      (push (make-glyph-box (length word) word
                            :source-start pos :source-end (+ pos (length word)))
            items)
      (incf pos (length word)))
    (nreverse items)))

;;; ---------------------------------------------------------------------------
;;; 比較対象: 貪欲法 (cl-typesetting の fit-lines と同じ考え方)
;;; ---------------------------------------------------------------------------

(defun greedy-break (items width)
  "前方一直線。直近の分割可能点を1つだけ覚え、溢れたらそこまで巻き戻す。
   先読みも後戻りもしない。返り値は break-paragraph と同じ形。"
  (let* ((v (coerce (finish-paragraph items) 'vector))
         (n (length v))
         (start 0) (line 0)
         (last-legal nil)
         (result '()))
    (let ((i 0))
      (loop while (< i n)
            do (cond
                 ((forced-break-p v i)
                  (push (list :position i
                              :ratio (greedy-line-ratio v start i width)
                              :line (incf line))
                        result)
                  (setf i n))
                 ((legal-breakpoint-p v i)
                  (if (<= (greedy-line-natural v start i) width)
                      (progn (setf last-legal i) (incf i))   ; まだ入る。覚えておく
                      (let ((b (or last-legal i)))           ; 溢れた。直近の点まで巻き戻す
                        (push (list :position b
                                    :ratio (greedy-line-ratio v start b width)
                                    :line (incf line))
                              result)
                        (setf start (skip-discardables v (1+ b))
                              last-legal nil
                              i start))))
                 (t (incf i)))))
    (nreverse result)))

(defun greedy-line-natural (v from to)
  (loop for i from from below to sum (advance (aref v i))))

(defun greedy-line-ratio (v from to width)
  (let ((natural 0) (st 0) (sh 0) (inf 0))
    (loop for i from from below to
          for item = (aref v i)
          do (incf natural (advance item))
             (if (plusp (stretch-order item))
                 (incf inf (stretch item))
                 (incf st (stretch item)))
             (incf sh (shrink item)))
    (adjustment-ratio natural st sh inf width)))

;;; ---------------------------------------------------------------------------
;;; 描画
;;; ---------------------------------------------------------------------------

(defun render (items breaks width label)
  "空きを実際に r で伸縮させて描く。ASCII なので四捨五入するが、
   散らばり方の違いは十分見える。"
  (let* ((v (coerce (finish-paragraph items) 'vector))
         (start 0)
         (ratios '()))
    (format t "~&~%=== ~a ===~%" label)
    (format t "+~a+~%" (make-string width :initial-element #\-))
    (dolist (br breaks)
      (let* ((b (getf br :position))
             (r (getf br :ratio))
             (last-line-p (= b (1- (length v))))
             (out (make-string-output-stream)))
        (loop for i from start below b
              for item = (aref v i)
              do (typecase item
                   (glyph-box (write-string (box-glyphs item) out))
                   (glue
                    ;; 最終行は無限グルーで埋まるので自然幅のまま描く
                    (let* ((extra (if (or last-line-p (zerop r))
                                      0
                                      (if (plusp r)
                                          (* r (stretch item))
                                          (* r (shrink item)))))
                           (wd (max 0 (round (+ (advance item) extra)))))
                      (write-string (make-string wd :initial-element #\Space) out)))))
        (let ((s (get-output-stream-string out)))
          (format t "|~a~a|~@[  r=~,2f~]~%"
                  s
                  (make-string (max 0 (- width (length s))) :initial-element #\Space)
                  (unless last-line-p (float r)))
          (unless last-line-p (push (float r) ratios)))
        (setf start (skip-discardables v (1+ b)))))
    (format t "+~a+~%" (make-string width :initial-element #\-))
    (let ((rs (nreverse ratios)))
      (format t "  行数 ~d / 最終行を除く r: 最大 ~,2f, 平均絶対値 ~,2f, 二乗和 ~,1f~%"
              (length breaks)
              (if rs (reduce #'max rs :key #'abs) 0)
              (if rs (/ (reduce #'+ rs :key #'abs) (length rs)) 0)
              (if rs (reduce #'+ rs :key (lambda (x) (* x x))) 0)))))

;;; ---------------------------------------------------------------------------

(defparameter *text*
  "In olden times when wishing still helped one, there lived a king whose daughters were all beautiful, but the youngest was so beautiful that the sun itself, which has seen so much, was astonished whenever it shone in her face.")

(defun stats (v breaks)
  "最終行を除く r の統計。(行数, 最大|r|, 二乗和) を返す。"
  (let ((rs (loop for br in breaks
                  unless (= (getf br :position) (1- (length v)))
                    collect (getf br :ratio))))
    (values (length breaks)
            (if rs (reduce #'max rs :key #'abs) 0)
            (if rs (reduce #'+ rs :key (lambda (x) (* x x))) 0))))

(defun sweep (&key (from 26) (to 72))
  "行幅を振って、貪欲法と Knuth-Plass が食い違う条件を探す。
   二乗和が小さいほど空きの散らばりが少ない = 組版として良い。"
  (let* ((items (text->items *text*))
         (v (coerce (finish-paragraph items) 'vector))
         (diff 0))
    (format t "~&幅   貪欲(行 最大r 二乗和)      KP(行 最大r 二乗和)   差~%")
    (loop for w from from to to
          do (multiple-value-bind (gn gmax gsq) (stats v (greedy-break items w))
               (multiple-value-bind (kn kmax ksq) (stats v (break-paragraph items w))
                 (let ((differs (or (/= gn kn) (/= gsq ksq))))
                   (when differs (incf diff))
                   (format t "~3d  ~3d ~6,2f ~8,2f   ~3d ~6,2f ~8,2f  ~a~%"
                           w gn (float gmax) (float gsq)
                           kn (float kmax) (float ksq)
                           (cond ((not differs) "")
                                 ((< ksq gsq) "KP が改善")
                                 ((> ksq gsq) "!! KP が悪化 !!")
                                 (t "行数のみ差")))))))
    (format t "~&差が出た幅: ~d / ~d~%" diff (1+ (- to from)))))

(defun run (&key (width 45))
  (let ((items (text->items *text*)))
    (format t "~&行幅 ~d / 単語 ~d~%" width (length (split-words *text*)))
    (render items (greedy-break items width) width "貪欲法 (first-fit)")
    (render items (break-paragraph items width) width "Knuth-Plass (最適)")))
