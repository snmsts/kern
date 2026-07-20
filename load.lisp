;;;; load.lisp -- 開発用のロード手順
;;;;
;;;;   ros run -- --load load.lisp
;;;;
;;;; ★cl-pdf は vendor/cl-pdf (local-fixes ブランチ) を使う。
;;;;   quicklisp 配布版と上流 master には2つのバグがあり、
;;;;   (a) write-document が undefined function で落ちる
;;;;   (b) 康熙部首と衝突する漢字が豆腐になり幅 0 になる
;;;;   DESIGN.md「上流 (mbattyani/cl-pdf) の状況」を参照。

(require :asdf)

;;; ★zlib の選択は cl-pdf.asd が読まれる【前】に決まる。
;;;   既定は :use-no-zlib で、その場合ストリームもフォントも無圧縮になる。
;;;   feature を切り替えた直後は fasl が古いままなので、一度だけ
;;;   (asdf:load-system "typeset/demo" :force t) が要る。
(pushnew :use-salza2-zlib *features*)

(let ((here (make-pathname :name nil :type nil :defaults *load-truename*)))
  (pushnew here asdf:*central-registry* :test #'equal)
  (pushnew (merge-pathnames "vendor/cl-pdf/" here) asdf:*central-registry* :test #'equal)
  (unless (probe-file (merge-pathnames "vendor/cl-pdf/cl-pdf.asd" here))
    (warn "vendor/cl-pdf が無い。~%  git clone https://github.com/mbattyani/cl-pdf.git vendor/cl-pdf~%  そのうえで DESIGN.md のパッチ2本を当てること。")))

(asdf:load-system "typeset/demo")

;;; ストリーム圧縮は既定 nil。*compress-fonts* は既定 t なので、
;;; zlib 実装さえ入ればフォントも圧縮される。
(setf pdf:*compress-streams* t)

(format t "~&~%typeset loaded.~%")
(format t "  (typeset::run-ja-pdf)   端から端まで: 和文を組んで PDF を書く~%")
(format t "  (typeset::run)          欧文で貪欲法と Knuth-Plass を比較~%")
(format t "  (typeset::sweep)        行幅を振って両者の差を測る~%")
(format t "  (typeset::run-glue)     段階付きの詰めと比例配分を比較~%")
(format t "  (typeset::stress)       和文の負荷試験 (禁則の検証つき)~%")
(format t "  (typeset::scaling)      文字数に対する計算量~%")
