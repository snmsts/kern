;;;; ja-pdf.lisp -- 端から端まで: テキスト → item → 行分割 → 詰め → PDF
;;;;
;;;; ソースに日本語リテラルを置かず、UTF-8 のファイルから読む。
;;;; 出力の目視用に、組まれた行を UTF-8 のテキストにも書き出す
;;;; (Windows のコンソールは和文を化けさせるため)。

(in-package #:typeset)

(defun rel (path)
  "システムからの相対パス。絶対パスを埋め込まないため。"
  (asdf:system-relative-pathname "typeset" path))

(defparameter *ttf* #P"C:/Windows/Fonts/yumin.ttf"
  "游明朝。Windows に素の .ttf で入っている数少ない和文フォント
   (他はほぼ .ttc で zpb-ttf が読めない)。")

(defun read-codes (path)
  "UTF-8 のファイルを読み、改行を除いたコードポイントの配列にする。"
  (with-open-file (in path :external-format :utf-8)
    (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
      (loop for ch = (read-char in nil)
            while ch
            unless (member ch '(#\Newline #\Return))
              do (vector-push-extend (char-code ch) out))
      out)))

(defun line-text (line &key (threshold 0))
  "行のテキスト。★glue も x 順に混ぜて、THRESHOLD 以上のアキを空白1つで表す。
   グリフだけを連結すると『和欧間のアキが入ったか』が出力から確かめられず、
   欧文の単語間まで消えて見えるので、診断として使い物にならない。"
  (let ((events (sort (append
                       (mapcar (lambda (g) (list (car g) :glyph (cdr g))) (line-glyphs line))
                       (mapcar (lambda (g) (list (car g) :gap (cdr g)))   (line-gaps line)))
                      #'< :key #'first)))
    (with-output-to-string (s)
      (dolist (e events)
        (ecase (second e)
          (:glyph (write-string (third e) s))
          (:gap (when (>= (third e) threshold) (write-char #\Space s))))))))

(defun glyphs-only (line)
  (apply #'concatenate 'string (mapcar #'cdr (line-glyphs line))))

(defun check-lines (lines)
  "禁則が守られたか数える。"
  (let ((head 0) (tail 0))
    (dolist (l lines)
      (let ((s (glyphs-only l)))
        (when (plusp (length s))
          (when (member (char-code (char s 0)) *kinsoku-head*) (incf head))
          (when (member (char-code (char s (1- (length s)))) *kinsoku-tail*) (incf tail)))))
    (values head tail)))

(defun diagnose (&key (size 21/2) (chars-per-line 24))
  "均等割りが版面幅にぴったり合わない行を突き止める。"
  (let* ((codes (read-codes (rel "demo/sample-ja.txt")))
         (fm (pdf:load-ttf-font *ttf*))
         (font (pdf:get-font (pdf::font-name fm)))
         (width (* size chars-per-line))
         (raw (text-items codes font size))
         (items (coerce (finish-paragraph raw) 'vector))
         (breaks (break-paragraph items width :finish nil))
         (start 0))
    (format t "~&行  target   natural       合計      差     status      末尾のitem~%")
    (dolist (br breaks)
      (let* ((b (getf br :position))
             (nat (loop for i from start below b sum (advance (aref items i)))))
        (multiple-value-bind (sizes status) (set-glue items width :start start :end b)
          (let ((tot (reduce #'+ sizes)))
            (format t "~3d ~7,2f ~9,3f ~10,3f ~7,3f  ~11a ~a~%"
                    (getf br :line) (float width) (float nat) (float tot)
                    (float (- tot width)) status
                    (type-of (aref items (1- b))))))
        (setf start (skip-discardables items (1+ b)))))))

(defun run-ja-pdf (&key (size 21/2) (chars-per-line 24))
  (let* ((codes (read-codes (rel "demo/sample-ja.txt")))
         (fm (pdf:load-ttf-font *ttf*))
         (font (pdf:get-font (pdf::font-name fm)))
         (width (* size chars-per-line))
         (t0 (get-internal-real-time)))
    (format t "~&=== 端から端まで ===~%")
    (format t "  文字数     : ~d~%" (length codes))
    (format t "  フォント   : ~a~%" (pdf::font-name fm))
    (format t "  級数       : ~,1fpt / 版面 ~,1fpt (全角 ~d 字)~%"
            size (float width) chars-per-line)

    ;; 幅の健全性確認: 全角は size ちょうどのはず (パッチが効いているか)
    (let ((w-kanji (glyph-advance font (code-char #x65E5) size))   ; 日 = 部首と衝突する字
          (w-kana  (glyph-advance font (code-char #x3042) size)))  ; あ
      (format t "  U+65E5 幅 : ~,3f  (~a)~%" (float w-kanji)
              (if (= w-kanji size) "OK 全角" "NG <== cl-pdf 未パッチの疑い"))
      (format t "  U+3042 幅 : ~,3f~%" (float w-kana)))

    (let ((lines (layout-paragraph codes font size width)))
      (format t "  行数       : ~d~%" (length lines))
      (format t "  組版時間   : ~,3f 秒~%"
              (float (/ (- (get-internal-real-time) t0)
                        internal-time-units-per-second)))
      (multiple-value-bind (head tail) (check-lines lines)
        (format t "  禁則違反   : 行頭 ~d / 行末 ~d ~a~%" head tail
                (if (and (zerop head) (zerop tail)) "" " <== 失敗")))
      (format t "  各行の状態 : ~{~a~^ ~}~%"
              (mapcar (lambda (l) (string-downcase (symbol-name (line-status l)))) lines))
      (format t "  各行の r   : ~{~,2f~^ ~}~%"
              (mapcar (lambda (l) (float (line-ratio l))) lines))
      ;; 均等割りの検算: 最終行を除き、実寸の合計は版面幅にぴったり一致するはず
      (let ((bad (loop for l in (butlast lines)
                       unless (= (line-advance l) width) count 1)))
        (format t "  均等割り   : 最終行を除く ~d 行中 ~d 行が版面幅と不一致~a~%"
                (1- (length lines)) bad (if (zerop bad) " (= 全行ぴったり)" " <== 失敗")))

      ;; 目視用のテキスト出力
      (let ((txt (rel "demo/out-lines.txt")))
        (with-open-file (out txt :direction :output :if-exists :supersede
                                 :external-format :utf-8)
          (format out "~a / ~,1fpt / ~d chars per line~%" (pdf::font-name fm)
                  size chars-per-line)
          (format out "~a~%" (make-string (* 2 chars-per-line) :initial-element #\-))
          (dolist (l lines)
            ;; 閾値は kanjiskip の伸び (最大 1/4em、実際は r 倍) より上、
            ;; 和欧間の四分アキより下に置く
            (format out "~a~%" (line-text l :threshold (/ size 5)))))
        (format t "  行のテキスト: ~a~%" txt))

      ;; ★フォントのサブセット化。cl-pdf に手を入れず binary-data を差し替える
      (let ((gids (gids-for-codes fm codes)))
        (multiple-value-bind (after before) (install-subset fm *ttf* codes)
          (format t "  サブセット : ~:d → ~:d bytes (~,2f%)  使用グリフ ~d~%"
                  before after (* 100.0 (/ after before)) (length gids)))
        ;; 作ったフォントを読み直して検算する
        (multiple-value-bind (ok report) (verify-subset (pdf::binary-data fm) gids)
          (format t "  検算       : ~a  (glyf ~:d bytes / 中身のあるグリフ ~d/~d)~%"
                  (if ok "OK" "NG")
                  (getf report :glyf-bytes) (getf report :non-empty) (getf report :wanted))
          (dolist (p (getf report :problems))
            (format t "               ! ~a~%" p))))

      ;; PDF 出力
      (let ((pdf-path (rel "demo/ja-typeset.pdf")))
        (pdf:with-document ()
          (pdf:with-page ()
            (draw-measure-rules lines size :x 60 :y 760 :width width)
            (draw-lines lines font size :x 60 :y 760))
          (pdf:write-document pdf-path))
        (format t "  PDF        : ~a (~:d bytes)~%" pdf-path
                (with-open-file (in pdf-path :element-type '(unsigned-byte 8))
                  (file-length in)))))))
