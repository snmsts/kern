;;;; vertical.lisp -- 縦書きデモ (段1: 正立縦組み)
;;;;
;;;; 芯 (行分割・set-glue・§4.19) は方向非依存で不変。ここは layout-paragraph の
;;;; 結果 (方向中立の LAID-LINE) を backend に :direction :vertical で描かせるだけ。
;;;; 約物の縦位置・字形回転・縦中横は段2以降。

(in-package #:kern)

(defun run-vertical-pdf (&key (size 16) (chars-per-col 24) (max-chars 260))
  "和文を縦組みで PDF に描く。列は右→左、字は下へ。"
  (let* ((fm    (pdf:load-ttf-font *ttf*))
         (font  (pdf:get-font (pdf::font-name fm)))
         (all   (read-codes (rel "demo/sample-ja.txt")))
         (codes (subseq all 0 (min max-chars (length all))))
         (col-h (* size chars-per-col))
         (lines (layout-paragraph codes font size col-h)))
    (format t "~&=== 縦組みデモ ===~%")
    (format t "  級数     : ~,1fpt / 1列 ~d字 (列長 ~,1fpt)~%" (float size) chars-per-col (float col-h))
    (format t "  列数     : ~d  文字数 ~d~%" (length lines) (length codes))
    (install-subset fm *ttf* codes)
    (let ((pdf-path (rel "demo/vertical.pdf")))
      (pdf:with-document ()
        (pdf:with-page ()
          ;; 右上を起点に。列間 = 1.8em、列は左へ進む。
          (draw-lines lines font size :x 540 :y 800
                                      :line-pitch (* size 9/5) :direction :vertical)
          (install-tounicode font codes))
        (pdf:write-document pdf-path))
      (format t "  PDF      : ~a (~:d bytes)~%" pdf-path
              (with-open-file (in pdf-path :element-type '(unsigned-byte 8))
                (file-length in))))))
