;;;; pdf-backend.lisp -- cl-pdf への出力
;;;;
;;;; ★このファイルだけが cl-pdf を知っている。
;;;;   layout.lisp / linebreak.lisp / setglue.lisp は PDF を一切参照しない。
;;;;   GUI バックエンドを足すときは、同じ2つの協定を実装した別ファイルを置く。
;;;;
;;;; 前提: パッチ済みの cl-pdf (vendor/cl-pdf, branch local-fixes)。
;;;;   未パッチだと (a) write-document が undefined function で落ち、
;;;;   (b) 康熙部首と衝突する漢字が豆腐になり幅 0 になる。

(in-package #:quad)

;;; --- メトリクス協定の実装 ---

(defmethod glyph-advance ((font pdf::font) char size)
  ;; ★境界で有理数に戻すこと。
  ;;   cl-pdf のメトリクスは単精度浮動小数点 (font-metrics は
  ;;   (* 0.001 units (zpb-ttf:advance-width g)) で作られる)。
  ;;   これをそのまま item の advance に入れると、以降の演算が全て float になり、
  ;;   「有理数で通して丸め誤差を溜めない」という設計が境界で破れる。
  ;;   実測では均等割りした行幅が版面幅と 1e-5 ずれ、行末が揃わなくなる。
  ;;
  ;;   RATIONALIZE を使うのは、フォントのメトリクスが本来
  ;;   整数 / unitsPerEm という有理数だから。単精度に落ちたものから
  ;;   元の分数を復元できる。RATIONAL だと二進の近似値をそのまま
  ;;   分数にしてしまい、1 のはずの全角幅が 1 にならない。
  (* size (rationalize (pdf:get-char-width char font))))

(defmethod font-ascent* ((font pdf::font) size)
  ;; 和文は外枠基準で行送りを決めるので通常これは使わない。
  ;; 欧文専用の行送りが要るときのために置いてある。
  (* size 88/100))

;;; --- 描画協定の実装 ---

(defun draw-lines (lines font size &key (x 60) (y 760) (line-pitch (* size 17/10)))
  "LAID-LINE の並びを現在のページに描く。

   ★グリフごとの位置決めに Td (cl-pdf の move-text) を使う。
     Td は【直前の行頭からの相対】なので、隣り合うグリフの x の差を渡せばよい。
     Tj による送りは行列を動かさないので干渉しない。
     cl-pdf 自身も text.lisp:47 で同じ手を使っている。"
  (loop for line in lines
        for i from 0
        do (pdf:in-text-mode
             (pdf:set-font font size)
             (pdf:move-text x (- y (* i line-pitch)))
             (let ((last-x 0))
               (dolist (g (line-glyphs line))
                 (pdf:move-text (float (- (car g) last-x)) 0)
                 (pdf:draw-text (cdr g))
                 (setf last-x (car g)))))))

;;; --- フォントのサブセット化を cl-pdf に差し込む ---

(defun gids-for-codes (fm codes)
  "使用したコードポイントから、cl-pdf の c2g を引いてグリフ番号を集める。"
  (let ((c2g (pdf::c2g fm))
        (gids '()))
    (map nil (lambda (code)
               (when (<= 0 code #xfffe)
                 (let ((gid (+ (ash (char-code (aref c2g (* 2 code))) 8)
                               (char-code (aref c2g (1+ (* 2 code)))))))
                   (when (plusp gid) (pushnew gid gids)))))
         codes)
    gids))

(defun install-subset (fm path codes)
  "FM の埋め込みデータを、CODES に必要なぶんだけのサブセットに差し替える。
   ★cl-pdf には一切手を入れない。binary-data と length1 を置き換えるだけ。
     グリフ番号を保存しているので CIDToGIDMap も /W もそのまま使える。"
  (let* ((before (pdf::length1 fm))
         (subset (subset-ttf path (gids-for-codes fm codes))))
    (setf (pdf::binary-data fm) subset
          (pdf::length1 fm) (length subset))
    (values (length subset) before)))

;;; --- /ToUnicode を cl-pdf のフォント辞書に注入する ---

(defun install-tounicode (font codes)
  "FONT (pdf:get-font が返すもの) の Type0 辞書に /ToUnicode CMap を足す。
   これで PDF が検索・コピペ・支援技術に対応する。

   ★cl-pdf は無改造。find-font-object がフォントを *document* に登録し、
     その content が make-dictionary の作った Type0 辞書なので、そこへ
     add-dict-value するだけ。
   ★フォントを登録させるため、一度ページ内で使われた後に呼ぶこと
     (draw-lines のあと)。まだ登録されていなければ何もしない。"
  (let ((fo (cdr (assoc font (pdf::fonts pdf::*document*)))))
    (when fo
      (let ((dict (pdf::content fo))
            (cmap (tounicode-cmap codes)))
        (unless (pdf::get-dict-value dict "/ToUnicode")
          (pdf::add-dict-value
           dict "/ToUnicode"
           (make-instance 'pdf::indirect-object
                          :content (make-instance 'pdf::pdf-stream
                                                  :content cmap
                                                  :no-compression (not pdf:*compress-streams*)))))
        cmap))))

(defun draw-measure-rules (lines size &key (x 60) (y 760) (width 0)
                                           (line-pitch (* size 17/10)))
  "版面の左右端に細い罫線を引く。行末が揃っているかの目視確認用。"
  (pdf:set-line-width 0.2)
  (pdf:set-rgb-stroke 0.8 0.2 0.2)
  (let ((top (+ y size))
        (bottom (- y (* (1- (length lines)) line-pitch) (* size 1/4))))
    (dolist (xx (list x (+ x width)))
      (pdf:move-to xx top)
      (pdf:line-to xx bottom))
    (pdf:stroke)))
