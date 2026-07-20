;;;; typeset.asd
;;;;
;;;; ★システム名とパッケージ名は暫定。プロジェクト名が決まったら変える。
;;;;
;;;; ★systems を分けてあるのは方針の表明。
;;;;   :typeset      は組版の芯。フォントにも PDF にも依存しない。
;;;;                 メトリクスは総称関数で外に問い合わせるだけ。
;;;;   :typeset/pdf  だけが cl-pdf を知っている。
;;;;                 GUI バックエンドを足すときは :typeset/gui を兄弟として置く。
;;;;   依存が増えて芯が汚れていないかは、この .asd を見れば分かる。
;;;;
;;;; cl-pdf は vendor/cl-pdf (upstream + local-fixes ブランチ) を使うこと。
;;;; 上流および quicklisp 配布版には
;;;;   (a) write-document が undefined function で落ちる
;;;;   (b) 康熙部首と衝突する漢字が豆腐になり幅 0 になる
;;;; の2つのバグがある。DESIGN.md の「上流の状況」を参照。

(defsystem "typeset"
  :description "Japanese-capable typesetting engine (working title)"
  :license "BSD-2-Clause"
  :depends-on ()
  :serial t
  :components ((:module "src"
                :components ((:file "items")
                             (:file "linebreak")
                             (:file "setglue")
                             (:file "layout")
                             (:file "ttf-subset")))))

(defsystem "typeset/pdf"
  :description "cl-pdf backend"
  :license "BSD-2-Clause"
  :depends-on ("typeset" "cl-pdf")
  :serial t
  :components ((:module "src"
                :components ((:file "pdf-backend")))))

(defsystem "typeset/demo"
  :description "Demonstrations and measurements"
  :license "BSD-2-Clause"
  :depends-on ("typeset/pdf")
  :serial t
  :components ((:module "demo"
                :components ((:file "compare")
                             (:file "glue")
                             (:file "japanese-stress")
                             (:file "ja-pdf")))))
