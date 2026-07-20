;;;; tounicode.lisp -- /ToUnicode CMap の生成
;;;;
;;;; PDF は「位置の決まったグリフ」を保存する形式で、テキストは保存しない。
;;;; だから検索・コピペ・支援技術のためには、CID からもとの Unicode への
;;;; 逆写像を /ToUnicode CMap として別に書く必要がある。cl-pdf はこれを出さない。
;;;;
;;;; ★我々にとってはほぼタダ。cl-pdf の CID = Unicode コードポイントなので、
;;;;   CMap は恒等写像に近い。使った文字ぶんだけ <src> <dst> を並べればよい。
;;;;
;;;; ★この関数は PDF ライブラリを知らない。CMap の本文 (文字列) を返すだけ。
;;;;   cl-pdf への注入は pdf-backend.lisp が行う。

(in-package #:typeset)

(defun utf16be-hex (code)
  "コードポイントを UTF-16BE の16進文字列にする。/ToUnicode の dst は UTF-16BE。
   BMP 内なら4桁、SMP ならサロゲートペアで8桁。"
  (if (<= code #xFFFF)
      (format nil "~4,'0X" code)
      (let* ((c (- code #x10000))
             (hi (+ #xD800 (ldb (byte 10 10) c)))
             (lo (+ #xDC00 (ldb (byte 10 0) c))))
        (format nil "~4,'0X~4,'0X" hi lo))))

(defun tounicode-cmap (codes)
  "使用したコードポイントの集合から /ToUnicode CMap の本文を作る。
   src は Identity-H の2バイトコード = CID = コードポイント (cl-pdf の設計)。"
  (let ((sorted (sort (remove-duplicates
                       (remove-if-not (lambda (c) (<= 0 c #xFFFE)) (coerce codes 'list)))
                      #'<)))
    (with-output-to-string (s)
      (format s "/CIDInit /ProcSet findresource begin~%")
      (format s "12 dict begin~%begincmap~%")
      (format s "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def~%")
      (format s "/CMapName /Adobe-Identity-UCS def~%/CMapType 2 def~%")
      (format s "1 begincodespacerange~%<0000> <FFFF>~%endcodespacerange~%")
      ;; bfchar は1回あたり最大100件までという制限があるので分割する
      (loop for chunk in (chunks sorted 100)
            do (format s "~d beginbfchar~%" (length chunk))
               (dolist (code chunk)
                 (format s "<~4,'0X> <~a>~%" code (utf16be-hex code)))
               (format s "endbfchar~%"))
      (format s "endcmap~%CMapName currentdict /CMap defineresource pop~%end~%end~%"))))

(defun chunks (list n)
  "LIST を長さ N 以下の部分リストに分ける。"
  (loop for rest = list then (nthcdr n rest)
        while rest
        collect (subseq rest 0 (min n (length rest)))))
