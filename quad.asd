;;;; quad.asd
;;;;
;;;; ★名前 Quad の由来: 込め物 (quadrat) = 活字間の込め物・アキ。
;;;;   このエンジンの芯は box/glue によるアキ量の計算そのものなので、
;;;;   機構を素直に指す内部エンジン名として採った。人向けの上位層
;;;;   (オーサリング言語) は別 system として後から別名で乗せる。
;;;;
;;;; ★systems を分けてあるのは方針の表明。
;;;;   :quad      は組版の芯。フォントにも PDF にも依存しない。
;;;;              メトリクスは総称関数で外に問い合わせるだけ。
;;;;   :quad/pdf  だけが cl-pdf を知っている。
;;;;              GUI バックエンドを足すときは :quad/gui を兄弟として置く。
;;;;   依存が増えて芯が汚れていないかは、この .asd を見れば分かる。
;;;;
;;;; cl-pdf は vendor/cl-pdf (upstream + local-fixes ブランチ) を使うこと。
;;;; 上流および quicklisp 配布版には
;;;;   (a) write-document が undefined function で落ちる
;;;;   (b) 康熙部首と衝突する漢字が豆腐になり幅 0 になる
;;;; の2つのバグがある。DESIGN.md の「上流の状況」を参照。

(defsystem "quad"
  :description "Japanese-capable typesetting engine"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:module "src"
                :components ((:file "items")
                             (:file "linebreak")
                             (:file "setglue")
                             (:file "jfm")
                             (:file "uax14")
                             (:file "layout")
                             (:file "ttf-subset")
                             (:file "tounicode")))))

(defsystem "quad/pdf"
  :description "cl-pdf backend"
  :license "MIT"
  :depends-on ("quad" "cl-pdf")
  :serial t
  :components ((:module "src"
                :components ((:file "pdf-backend")))))

(defsystem "quad/demo"
  :description "Demonstrations and measurements"
  :license "MIT"
  :depends-on ("quad/pdf")
  :serial t
  :components ((:module "demo"
                :components ((:file "compare")
                             (:file "glue")
                             (:file "japanese-stress")
                             (:file "ja-pdf")))))
