;;;; glue.lisp -- 段階付きの詰め vs 比例配分
;;;;
;;;; JLReq は詰めの【順序】を規定している。TeX / cl-typesetting の比例配分では
;;;; その順序が出せない。同じ行を同じ幅に収めても、どの空きが詰まるかが変わる。

(in-package #:kern)

;;; 全角を 1 とする。ラベルは ASCII (ソースに日本語リテラルを置かない方針)。
;;;
;;; 行の構成:
;;;   文 。[句点後アキ] 文 [字間] 」[括弧間アキ] 「 文 [字間] 文
;;;
;;; 句点・括弧は全角枠に字面が半角ぶん寄り、残りがアキになる。
;;; そのアキが詰めの原資。字間 (kanjiskip) は jfm-jlreq.lua のとおり
;;; {0, 0.25, 0} = 自然幅0・伸びのみ・縮みなし。
(defparameter *line-spec*
  '((:box  1   nil nil "moji")
    (:box  1/2 nil nil "kuten")
    (:glue 1/2 1/2 2   "kuten-ato-aki")     ; 第1段階 (priority 2)
    (:box  1   nil nil "moji")
    (:glue 0   0   0   "kanjiskip")         ; 縮まない
    (:box  1/2 nil nil "toji-kakko")
    (:glue 1   1   1   "kakko-kan-aki")     ; 第2段階 (priority 1)
    (:box  1/2 nil nil "hiraki-kakko")
    (:box  1   nil nil "moji")
    (:glue 0   0   0   "kanjiskip")
    (:box  1   nil nil "moji")))

(defun build-line ()
  (loop for (kind adv shr pri label) in *line-spec*
        collect (ecase kind
                  (:box (make-glyph-box adv label))
                  (:glue (make-glue adv :stretch 1/4 :shrink shr
                                        :shrink-priority pri)))))

(defun glue-labels ()
  (loop for (kind adv shr pri label) in *line-spec*
        when (eq kind :glue) collect (list label shr pri)))

(defun proportional-set-glue (items target)
  "priority を無視した配分 = TeX / spread-boxes の挙動。比較用。"
  (let* ((v (coerce items 'vector))
         (sizes (make-array (length v)))
         (natural 0))
    (loop for i from 0 below (length v)
          for a = (advance (aref v i))
          do (setf (aref sizes i) a) (incf natural a))
    (let ((delta (- target natural)))
      (if (minusp delta)
          (let ((participants (%participants v 0 (length v) #'shrink #'shrink-order)))
            (%distribute sizes 0 (- delta) participants
                         #'shrink (constantly 0) -1 nil)   ; ← 段階を潰す
            sizes)
          sizes))))

(defun show (target)
  (let* ((items (build-line))
         (v (coerce items 'vector))
         (natural (loop for i across v sum (advance i))))
    (multiple-value-bind (staged status left) (set-glue items target)
      (let ((prop (proportional-set-glue items target)))
        (format t "~&幅 ~4,1f (自然幅 ~,1f / 詰め ~,2f 必要)  状態 ~a~@[ 残 ~,2f~]~%"
                (float target) (float natural) (float (max 0 (- natural target)))
                status (when (plusp left) (float left)))
        (format t "    ~22a ~10a ~10a~%" "glue" "段階付き" "比例配分")
        (let ((gi 0))
          (loop for i from 0 below (length v)
                for item = (aref v i)
                when (typep item 'glue)
                  do (destructuring-bind (label shr pri) (nth gi (glue-labels))
                       (format t "    ~22a ~10,3f ~10,3f   (縮み~,2f 段階~d)~%"
                               label (float (aref staged i)) (float (aref prop i))
                               (float shr) pri)
                       (incf gi))))))))

(defun run-glue ()
  (format t "~&=== 段階付きの詰め vs 比例配分 ===~%")
  (format t "行の構成: 文 。[アキ] 文 [字間] 」[アキ] 「 文 [字間] 文~%")
  (format t "詰められるのは 句点後アキ(0.5, 第1段階) と 括弧間アキ(1.0, 第2段階) の計 1.5~%")
  (dolist (target '(7 13/2 6 11/2 5))
    (terpri)
    (show target)))
