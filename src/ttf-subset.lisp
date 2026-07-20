;;;; ttf-subset.lisp -- TrueType のサブセット化
;;;;
;;;; 和文フォントは数 MB〜十数 MB あるので、丸ごと埋め込むと
;;;; 漢字数文字の PDF が 13MB になる。欧文フォント (数百 KB) では
;;;; 誰も困らなかったが、和文では実用上避けられない。
;;;;
;;;; ★方針: グリフ番号を【振り直さない】。使わないグリフを空にするだけ。
;;;;
;;;;   一般的なサブセット化はグリフを詰めて番号を振り直すが、そうすると
;;;;   cl-pdf が書き出す CIDToGIDMap (CID=Unicode → GID) が全て無効になり、
;;;;   cl-pdf 側に手を入れる必要が出る。
;;;;   番号を保てば CIDToGIDMap はそのまま使えて、cl-pdf は無改造で済む。
;;;;
;;;;   代償は loca と hmtx が全グリフ分残ること (numGlyphs × 4 バイト程度)。
;;;;   15,000 グリフのフォントで 120KB ほど。glyf 本体が数 MB→数十 KB になるので
;;;;   全体では十分小さくなる。
;;;;
;;;; ★不要なテーブルは落とす。PDF に CIDFontType2 として埋め込む場合、
;;;;   必要なのは glyf / head / hhea / hmtx / loca / maxp と、
;;;;   ヒンティングを残すなら cvt / fpgm / prep だけ。
;;;;   cmap は CIDToGIDMap があるので要らない。GSUB / GPOS / name / post も不要。

(in-package #:typeset)

;;; ---------------------------------------------------------------------------
;;; ビッグエンディアンの読み書き
;;; ---------------------------------------------------------------------------

(declaim (inline rd-u8 rd-u16 rd-u32 rd-i16))
(defun rd-u8  (v i) (aref v i))
(defun rd-u16 (v i) (+ (ash (aref v i) 8) (aref v (+ i 1))))
(defun rd-u32 (v i) (+ (ash (aref v i) 24) (ash (aref v (+ i 1)) 16)
                       (ash (aref v (+ i 2)) 8) (aref v (+ i 3))))
(defun rd-i16 (v i) (let ((x (rd-u16 v i))) (if (>= x #x8000) (- x #x10000) x)))

(defun wr-u16 (out x)
  (vector-push-extend (ldb (byte 8 8) x) out)
  (vector-push-extend (ldb (byte 8 0) x) out))

(defun wr-u32 (out x)
  (vector-push-extend (ldb (byte 8 24) x) out)
  (vector-push-extend (ldb (byte 8 16) x) out)
  (vector-push-extend (ldb (byte 8 8) x) out)
  (vector-push-extend (ldb (byte 8 0) x) out))

(defun wr-bytes (out v &optional (start 0) (end (length v)))
  (loop for i from start below end do (vector-push-extend (aref v i) out)))

(defun make-buf (&optional (n 0))
  (make-array n :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

;;; ---------------------------------------------------------------------------
;;; テーブル一覧
;;; ---------------------------------------------------------------------------

(defun tag-string (v i)
  (map 'string #'code-char (subseq v i (+ i 4))))

(defun read-table-directory (v)
  "タグ → (offset . length) のハッシュを返す。"
  (let ((n (rd-u16 v 4))
        (tables (make-hash-table :test #'equal)))
    (dotimes (i n)
      (let ((p (+ 12 (* 16 i))))
        (setf (gethash (tag-string v p) tables)
              (cons (rd-u32 v (+ p 8)) (rd-u32 v (+ p 12))))))
    tables))

(defun table-bytes (v tables tag)
  (let ((e (gethash tag tables)))
    (when e (subseq v (car e) (+ (car e) (cdr e))))))

;;; ---------------------------------------------------------------------------
;;; loca / glyf
;;; ---------------------------------------------------------------------------

(defun read-loca (v tables num-glyphs long-format-p)
  "グリフ番号 → glyf 内の (開始 . 終了) の配列。"
  (let* ((e (gethash "loca" tables))
         (base (car e))
         (offsets (make-array (1+ num-glyphs))))
    (dotimes (i (1+ num-glyphs))
      (setf (aref offsets i)
            (if long-format-p
                (rd-u32 v (+ base (* 4 i)))
                (* 2 (rd-u16 v (+ base (* 2 i)))))))
    offsets))

(defparameter +arg-1-and-2-are-words+ #x0001)
(defparameter +we-have-a-scale+       #x0008)
(defparameter +more-components+       #x0020)
(defparameter +x-and-y-scale+         #x0040)
(defparameter +two-by-two+            #x0080)

(defun composite-components (glyf glyf-base start end)
  "複合グリフが参照するグリフ番号を集める。単純グリフなら NIL。
   ★これを忘れると、複合グリフ (濁点付き仮名など) の部品が落ちて欠ける。"
  (let ((p (+ glyf-base start)))
    (when (and (< start end) (minusp (rd-i16 glyf p)))
      (incf p 10)                       ; numberOfContours + bbox
      (loop with refs = '()
            for flags = (rd-u16 glyf p)
            for gid = (rd-u16 glyf (+ p 2))
            do (push gid refs)
               (incf p 4)
               (incf p (if (logtest flags +arg-1-and-2-are-words+) 4 2))
               (cond ((logtest flags +we-have-a-scale+) (incf p 2))
                     ((logtest flags +x-and-y-scale+)   (incf p 4))
                     ((logtest flags +two-by-two+)      (incf p 8)))
            while (logtest flags +more-components+)
            finally (return refs)))))

(defun glyph-closure (v tables loca gids)
  "GIDS から複合グリフの参照を推移的にたどって、必要なグリフ番号の集合を返す。"
  (let* ((e (gethash "glyf" tables))
         (glyf-base (car e))
         (needed (make-hash-table))
         (stack (copy-list gids)))
    (setf (gethash 0 needed) t)         ; .notdef は常に残す
    (dolist (g gids) (setf (gethash g needed) t))
    (loop while stack
          do (let* ((g (pop stack))
                    (start (aref loca g))
                    (end (aref loca (1+ g))))
               (dolist (ref (composite-components v glyf-base start end))
                 (unless (gethash ref needed)
                   (setf (gethash ref needed) t)
                   (push ref stack)))))
    needed))

;;; ---------------------------------------------------------------------------
;;; 組み立て
;;; ---------------------------------------------------------------------------

(defparameter *keep-tables*
  '("head" "hhea" "hmtx" "maxp" "cvt " "fpgm" "prep")
  "そのまま複製するテーブル。glyf と loca は作り直すので含めない。
   cmap は CIDToGIDMap があるので不要。GSUB/GPOS/name/post も落とす。")

(defun pad4 (n) (* 4 (ceiling n 4)))

(defun table-checksum (v start len)
  (let ((sum 0))
    (loop for i from start below (+ start len) by 4
          do (incf sum (+ (ash (if (< i (+ start len)) (aref v i) 0) 24)
                          (ash (if (< (+ i 1) (+ start len)) (aref v (+ i 1)) 0) 16)
                          (ash (if (< (+ i 2) (+ start len)) (aref v (+ i 2)) 0) 8)
                          (if (< (+ i 3) (+ start len)) (aref v (+ i 3)) 0))))
    (ldb (byte 32 0) sum)))

(defun subset-ttf (path gids)
  "PATH の TrueType から、GIDS (と複合グリフの部品) だけを残した
   フォントのバイト列を作る。グリフ番号は保存される。"
  (let* ((v (with-open-file (in path :element-type '(unsigned-byte 8))
              (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
                (read-sequence buf in)
                buf)))
         (tables (read-table-directory v))
         (head (gethash "head" tables))
         (maxp (gethash "maxp" tables))
         (num-glyphs (rd-u16 v (+ (car maxp) 4)))
         (long-loca-p (= 1 (rd-i16 v (+ (car head) 50))))
         (loca (read-loca v tables num-glyphs long-loca-p))
         (needed (glyph-closure v tables loca gids))
         (glyf-base (car (gethash "glyf" tables))))
    ;; 新しい glyf と loca
    (let ((new-glyf (make-buf (* 64 1024)))
          (new-offsets (make-array (1+ num-glyphs))))
      (dotimes (g num-glyphs)
        (setf (aref new-offsets g) (fill-pointer new-glyf))
        (when (gethash g needed)
          (let ((s (aref loca g)) (e (aref loca (1+ g))))
            (wr-bytes new-glyf v (+ glyf-base s) (+ glyf-base e))
            ;; グリフは 4 バイト境界に揃える
            (loop while (plusp (mod (fill-pointer new-glyf) 4))
                  do (vector-push-extend 0 new-glyf)))))
      (setf (aref new-offsets num-glyphs) (fill-pointer new-glyf))
      ;; loca は元の形式のまま作る (head を書き換えなくて済む)
      (let ((new-loca (make-buf)))
        (dotimes (i (1+ num-glyphs))
          (if long-loca-p
              (wr-u32 new-loca (aref new-offsets i))
              (wr-u16 new-loca (floor (aref new-offsets i) 2))))
        (when (and (not long-loca-p) (> (fill-pointer new-glyf) #x1FFFE))
          (error "short loca では収まらない。long 形式への変換が要る"))
        ;; 出力するテーブルを集める
        (let ((out-tables '()))
          (dolist (tag *keep-tables*)
            (let ((b (table-bytes v tables tag)))
              (when b (push (cons tag b) out-tables))))
          (push (cons "glyf" (coerce new-glyf '(vector (unsigned-byte 8)))) out-tables)
          (push (cons "loca" (coerce new-loca '(vector (unsigned-byte 8)))) out-tables)
          (setf out-tables (sort out-tables #'string< :key #'car))
          (build-font out-tables))))))

(defun build-font (out-tables)
  "テーブル一覧からフォントファイルのバイト列を組み立てる。"
  (let* ((n (length out-tables))
         ;; log を使うと 2 の冪で 2.9999997 のような値になり floor がずれる。
         ;; integer-length なら整数演算で済む。
         (entry-selector (1- (integer-length n)))
         (search-range (* 16 (expt 2 entry-selector)))
         (range-shift (- (* 16 n) search-range))
         (out (make-buf (* 256 1024)))
         (dir-size (+ 12 (* 16 n)))
         (offset dir-size)
         (head-pos nil))
    ;; ヘッダ
    (wr-u32 out #x00010000)
    (wr-u16 out n) (wr-u16 out search-range)
    (wr-u16 out entry-selector) (wr-u16 out range-shift)
    ;; テーブルディレクトリ (checksum は後で埋める)
    (dolist (te out-tables)
      (destructuring-bind (tag . bytes) te
        (loop for ch across tag do (vector-push-extend (char-code ch) out))
        (wr-u32 out 0)                  ; checksum、後で
        (wr-u32 out offset)
        (wr-u32 out (length bytes))
        (incf offset (pad4 (length bytes)))))
    ;; 本体
    (dolist (te out-tables)
      (destructuring-bind (tag . bytes) te
        (when (string= tag "head") (setf head-pos (fill-pointer out)))
        (wr-bytes out bytes)
        (loop while (plusp (mod (fill-pointer out) 4))
              do (vector-push-extend 0 out))))
    (let ((v (coerce out '(vector (unsigned-byte 8)))))
      ;; head.checkSumAdjustment を 0 にしてから各表の checksum を計算する
      (when head-pos
        (loop for i from 0 below 4 do (setf (aref v (+ head-pos 8 i)) 0)))
      (let ((off dir-size))
        (loop for te in out-tables
              for i from 0
              do (let ((len (length (cdr te)))
                       (p (+ 12 (* 16 i) 4)))
                   (let ((sum (table-checksum v off len)))
                     (setf (aref v p)       (ldb (byte 8 24) sum)
                           (aref v (+ p 1)) (ldb (byte 8 16) sum)
                           (aref v (+ p 2)) (ldb (byte 8 8) sum)
                           (aref v (+ p 3)) (ldb (byte 8 0) sum)))
                   (incf off (pad4 len)))))
      ;; ファイル全体の checksum から checkSumAdjustment を決める
      (when head-pos
        (let ((adj (ldb (byte 32 0) (- #xB1B0AFBA (table-checksum v 0 (length v))))))
          (setf (aref v (+ head-pos 8))       (ldb (byte 8 24) adj)
                (aref v (+ head-pos 8 1))     (ldb (byte 8 16) adj)
                (aref v (+ head-pos 8 2))     (ldb (byte 8 8) adj)
                (aref v (+ head-pos 8 3))     (ldb (byte 8 0) adj))))
      v)))

;;; ---------------------------------------------------------------------------
;;; 検算
;;; ---------------------------------------------------------------------------
;;; cmap を落としているので zpb-ttf では読み直せない (PDF には不要だが検証には使えない)。
;;; 自前で構造を確かめる。

(defun verify-subset (bytes wanted-gids)
  "サブセットしたバイト列を読み直して検算する。
   (values ok-p 報告の plist)"
  (let* ((tables (read-table-directory bytes))
         (maxp (gethash "maxp" tables))
         (head (gethash "head" tables))
         (problems '()))
    (unless (and maxp head (gethash "glyf" tables) (gethash "loca" tables)
                 (gethash "hmtx" tables) (gethash "hhea" tables))
      (push "必須テーブルが欠けている" problems))
    (let* ((num-glyphs (rd-u16 bytes (+ (car maxp) 4)))
           (long-p (= 1 (rd-i16 bytes (+ (car head) 50))))
           (loca (read-loca bytes tables num-glyphs long-p))
           (glyf-len (cdr (gethash "glyf" tables)))
           (non-empty 0)
           (missing '()))
      ;; loca は単調増加で、最後が glyf の長さに一致するはず
      (loop for i from 1 to num-glyphs
            when (< (aref loca i) (aref loca (1- i)))
              do (push (format nil "loca が単調でない (gid ~d)" i) problems)
                 (return))
      (unless (= (aref loca num-glyphs) glyf-len)
        (push (format nil "loca の末尾 ~d と glyf の長さ ~d が食い違う"
                      (aref loca num-glyphs) glyf-len)
              problems))
      ;; 要求したグリフは中身を持っているはず
      (dolist (g wanted-gids)
        (if (< (aref loca g) (aref loca (1+ g)))
            (incf non-empty)
            (push g missing)))
      (when missing
        (push (format nil "要求したのに空のグリフが ~d 個" (length missing)) problems))
      ;; head の checkSumAdjustment を 0 にして全体を検算すると 0xB1B0AFBA になるはず
      (let* ((copy (copy-seq bytes))
             (hp (car head)))
        (loop for i from 0 below 4 do (setf (aref copy (+ hp 8 i)) 0))
        (let ((sum (ldb (byte 32 0) (+ (table-checksum copy 0 (length copy))
                                       (rd-u32 bytes (+ hp 8))))))
          (unless (= sum #xB1B0AFBA)
            (push (format nil "checkSumAdjustment が合わない (~8,'0x)" sum) problems))))
      (values (null problems)
              (list :num-glyphs num-glyphs :glyf-bytes glyf-len
                    :non-empty non-empty :wanted (length wanted-gids)
                    :problems (nreverse problems))))))

;;; ---------------------------------------------------------------------------
;;; cl-pdf への差し込み
;;; ---------------------------------------------------------------------------

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
