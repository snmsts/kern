;;;; kern.asd
;;;;
;;;; ★名前 Kern の由来: カーニング = 活字間のアキの調整。
;;;;   このエンジンの芯は box/glue によるアキ量の計算そのものなので、
;;;;   機構を素直に指す内部エンジン名として採った。人向けの上位層
;;;;   (オーサリング言語) は別 system として後から別名で乗せる。
;;;;   (初出は Quad と名付けたが、同ドメインの先客 = Matthew Butterick の
;;;;    Racket 製 `quad` があり、敬意を払って改名した。)
;;;;
;;;; ★systems を分けてあるのは方針の表明。
;;;;   :kern      は組版の芯。フォントにも PDF にも依存しない。
;;;;              メトリクスは総称関数で外に問い合わせるだけ。
;;;;   :kern/pdf  だけが cl-pdf を知っている。
;;;;              GUI バックエンドを足すときは :kern/gui を兄弟として置く。
;;;;   依存が増えて芯が汚れていないかは、この .asd を見れば分かる。
;;;;
;;;; cl-pdf は vendor/cl-pdf (upstream + local-fixes ブランチ) を使うこと。
;;;; 上流および quicklisp 配布版には
;;;;   (a) write-document が undefined function で落ちる
;;;;   (b) 康熙部首と衝突する漢字が豆腐になり幅 0 になる
;;;; の2つのバグがある。DESIGN.md の「上流の状況」を参照。

(defsystem "kern"
  :description "Japanese-capable typesetting engine"
  :license "MIT"
  :depends-on ()
  :serial t
  ;; (asdf:test-system "kern") → kern/test の test-op へ委譲する。
  :in-order-to ((test-op (test-op "kern/test")))
  :components ((:module "src"
                :components ((:file "items")
                             (:file "linebreak")
                             (:file "setglue")
                             (:file "jfm")
                             (:file "uax14")
                             (:file "layout")
                             (:file "ttf-subset")
                             (:file "tounicode")))))

(defsystem "kern/pdf"
  :description "cl-pdf backend"
  :license "MIT"
  :depends-on ("kern" "cl-pdf")
  :serial t
  :components ((:module "src"
                :components ((:file "pdf-backend")))))

(defsystem "kern/demo"
  :description "Demonstrations and measurements"
  :license "MIT"
  :depends-on ("kern/pdf")
  :serial t
  :components ((:module "demo"
                :components ((:file "compare")
                             (:file "glue")
                             (:file "japanese-stress")
                             (:file "ja-pdf")))))

;;; テストは芯 (kern) だけに依存する。cl-pdf も vendor も要らないので
;;; DLL/フォントを用意せず素の SBCL で回帰確認できる。
(defsystem "kern/test"
  :description "Regression tests (JIS X 4051 §4.19 line adjustment). Font-free."
  :license "MIT"
  :depends-on ("kern")
  :serial t
  :components ((:module "test"
                :components ((:file "line-adjustment")
                             (:file "ruby"))))
  :perform (test-op (o c)
             (let ((ok1 (uiop:symbol-call '#:kern '#:run-line-adjustment-tests))
                   (ok2 (uiop:symbol-call '#:kern '#:run-ruby-tests)))
               (unless (and ok1 ok2)
                 (error "kern regression tests failed")))))
