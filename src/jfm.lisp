;;;; jfm.lisp -- jfm-jlreq.lua を読んで文字クラス表を作る
;;;;
;;;; JLReq 本文は数値を JIS X 4051 に委ねているが、その JIS X 4051 を
;;;; データ化したのが abenori/jlreq の jfm-jlreq.lua (BSD-2, Noriyuki Abe)。
;;;; コメントに JLReq の節番号が引かれている。ここを唯一の数値の出所とする。
;;;;
;;;; ★汎用 Lua パーサは書かない。この JFM のテーブルコンストラクタと
;;;;   末尾の copy_jfm だけを読む専用リーダー。add_space などの条件分岐は
;;;;   既定設定 (jlreq == nil) では発火しないので読み飛ばす。
;;;;
;;;; ★変換結果をソースに焼き込まず、原典を実行時に読む。
;;;;   原典が更新されても追随でき、BSD-2 の帰属も原典ファイルで示せる。
;;;;
;;;; クラス番号 (jfm-jlreq.lua より):
;;;;   1 始め括弧  2 終わり括弧  300/301/302 ハイフン/ダッシュ/波ダッシュ
;;;;   4 区切り約物  5 中点類  6 句点  7 読点  8 分離禁止  9 繰返し記号
;;;;   10 長音  11 小書き仮名  12 前置省略記号  13 後置省略記号  14 和字間隔
;;;;   15 平仮名  16 片仮名  161 半角カナ  17 等号  18 演算記号
;;;;   0(=19) 漢字等  191/192/193 半角/三分/四分 数字  26 欧文間隔  90 行頭
;;;;   (20-30 のルビ・割注・縦中横は今は無視する)

(in-package #:quad)

;;; ---------------------------------------------------------------------------
;;; ごく小さな Lua テーブルリーダー
;;; ---------------------------------------------------------------------------
;;; 対応する構文: 数値 (負・小数)、'文字'、"文字列"、{ ... }、key = value、
;;; [n] = value、コメント (-- と --[[ ]])。この JFM が使う範囲だけ。

(defstruct (lua-reader (:conc-name lr-))
  (string "" :type string)
  (pos 0 :type fixnum))

(defun lr-peek (r)
  (when (< (lr-pos r) (length (lr-string r)))
    (char (lr-string r) (lr-pos r))))

(defun lr-next (r) (prog1 (lr-peek r) (incf (lr-pos r))))

(defun lr-skip-ws (r)
  "空白とコメントを飛ばす。"
  (loop
    (let ((c (lr-peek r)))
      (cond
        ((null c) (return))
        ((member c '(#\Space #\Tab #\Newline #\Return)) (lr-next r))
        ((and (eql c #\-) (eql (lr-peek2 r) #\-))
         (incf (lr-pos r) 2)
         (if (and (eql (lr-peek r) #\[) (eql (lr-peek2 r) #\[))
             (progn (incf (lr-pos r) 2)          ; 長コメント --[[ ... ]]
                    (loop until (or (null (lr-peek r))
                                    (and (eql (lr-peek r) #\]) (eql (lr-peek2 r) #\])))
                          do (lr-next r))
                    (incf (lr-pos r) 2))
             (loop until (member (lr-peek r) '(#\Newline nil)) do (lr-next r))))
        (t (return))))))

(defun lr-peek2 (r)
  (when (< (1+ (lr-pos r)) (length (lr-string r)))
    (char (lr-string r) (1+ (lr-pos r)))))

(defun lr-read-string (r quote)
  (lr-next r)                            ; 開きクォート
  (with-output-to-string (s)
    (loop for c = (lr-next r)
          until (eql c quote)
          do (if (eql c #\\)
                 (write-char (lr-next r) s)   ; エスケープは素通し (この JFM には無い)
                 (write-char c s)))))

(defun lr-read-number (r)
  (let ((start (lr-pos r)))
    (loop for c = (lr-peek r)
          while (and c (or (digit-char-p c) (member c '(#\. #\- #\+ #\e #\E))))
          do (lr-next r))
    (let ((str (subseq (lr-string r) start (lr-pos r))))
      ;; 整数はそのまま、小数は有理数にする (エンジンは有理数で通す)
      (if (or (find #\. str) (find #\e str) (find #\E str))
          (rationalize (read-from-string str))
          (parse-integer str)))))

(defun lr-read-name (r)
  (let ((start (lr-pos r)))
    (loop for c = (lr-peek r)
          while (and c (or (and (standard-char-p c) (alphanumericp c)) (eql c #\_)))
          do (lr-next r))
    (when (= (lr-pos r) start)
      ;; 進まなかった = 想定外の文字。無限ループを避けるため1文字消費する。
      ;; jfm-jlreq.lua の想定文法では起きないはずだが、保険。
      (error "lr-read-name: 識別子が読めない at ~d: ~s"
             start (subseq (lr-string r) start (min (length (lr-string r)) (+ start 20)))))
    (subseq (lr-string r) start (lr-pos r))))

;;; Lua テーブルを表す。int-keyed / str-keyed は alist、positional は list。
(defstruct (lua-table (:conc-name lt-))
  (int '()) (str '()) (pos '()))

(defun lt-get (table key &optional default)
  "文字列キー、または整数キーで引く。"
  (let ((cell (if (integerp key)
                  (assoc key (lt-int table))
                  (assoc key (lt-str table) :test #'string=))))
    (if cell (cdr cell) default)))

(defparameter *lua-globals*
  '(("stretch_width" . 1/4))            ; jfm-jlreq.lua: local stretch_width = 0.25
  "値位置に現れる Lua 変数。jfm-jlreq.lua はこの1つだけ使う。")

(defun lr-read-value (r)
  (lr-skip-ws r)
  (let ((c (lr-peek r)))
    (let ((v (cond
               ((null c) (return-from lr-read-value nil))
               ((eql c #\{) (lr-read-table r))
               ((or (eql c #\') (eql c #\")) (lr-read-string r c))
               ((or (digit-char-p c) (member c '(#\- #\+ #\.))) (lr-read-number r))
               (t (let ((name (lr-read-name r)))   ; 識別子 or 変数
                    (let ((g (assoc name *lua-globals* :test #'string=)))
                      (if g (cdr g) name)))))))
      ;; ★数値位置の除算 X/Y に対応する。jfm-jlreq.lua は 1/2, 1/3, 1/4 を使う。
      ;;   Lua に有理数リテラルは無いのでこれは実際の除算式。
      (lr-skip-ws-inline r)
      (if (and (realp v) (eql (lr-peek r) #\/))
          (progn (lr-next r)
                 (lr-skip-ws-inline r)
                 (/ v (lr-read-value r)))
          v))))

(defun lr-skip-ws-inline (r)
  "改行を挟まない空白だけ飛ばす (式の途中用)。コメントは扱わない。"
  (loop while (member (lr-peek r) '(#\Space #\Tab)) do (lr-next r)))

(defun lr-read-table (r)
  "{ ... } を読んで lua-table を返す。"
  (lr-next r)                            ; {
  (let ((int-keyed '()) (str-keyed '()) (positional '()))
    (loop
      (lr-skip-ws r)
      (let ((c (lr-peek r)))
        (cond
          ((or (null c) (eql c #\})) (lr-next r) (return))
          ((eql c #\,) (lr-next r))
          ((eql c #\[)                   ; [n] = value
           (lr-next r)
           (let ((k (lr-read-number r)))
             (lr-skip-ws r) (lr-next r)  ; ]
             (lr-skip-ws r) (lr-next r)  ; =
             (push (cons k (lr-read-value r)) int-keyed)))
          ((alpha-char-p c)              ; name = value  か  裸の識別子
           (let ((save (lr-pos r))
                 (name (lr-read-name r)))
             (lr-skip-ws r)
             (if (eql (lr-peek r) #\=)
                 (progn (lr-next r) (push (cons name (lr-read-value r)) str-keyed))
                 (progn (setf (lr-pos r) save)      ; = でなければ位置引数の識別子
                        (push (lr-read-value r) positional)))))
          (t (push (lr-read-value r) positional)))))
    (make-lua-table :int (nreverse int-keyed)
                    :str (nreverse str-keyed)
                    :pos (nreverse positional))))

;;; ---------------------------------------------------------------------------
;;; JFM の意味を持たせる
;;; ---------------------------------------------------------------------------

(defstruct (jfm-glue (:conc-name jg-))
  (natural 0) (stretch 0) (shrink 0)
  (stretch-priority 0) (shrink-priority 0)
  (ratio 1/2))

(defstruct (jfm-class (:conc-name jc-))
  (number 0)
  (chars '())           ; 文字コードの list と、'parbdd 等のシンボル
  (width 1) (height 0) (depth 0)
  (align nil)           ; "left"/"right"/"middle" — 字面が枠のどちら寄りか
  (glue (make-hash-table)))   ; 相手クラス番号 → jfm-glue

(defparameter *char-tokens*
  '(("parbdd" . :par-start) ("boxbdd" . :box-start)
    ("alchar" . :latin) ("nombre" . :nombre))
  "chars に現れる非文字トークン。")

(defun parse-chars (positional)
  "chars = {'あ','い',...} の位置配列を、文字コードとシンボルの list にする。"
  (loop for x in positional
        collect (cond ((and (stringp x) (= (length x) 1)) (char-code (char x 0)))
                      ((stringp x) (or (cdr (assoc x *char-tokens* :test #'string=))
                                       (intern (string-upcase x) :keyword)))
                      (t x))))

(defun glue<-lua (lt)
  "glue の1エントリ (lua-table) を jfm-glue にする。
   {natural, stretch, shrink, priority={s,k}, ratio=r, ...}"
  (let* ((pos (lt-pos lt))
         (pri (lt-get lt "priority"))
         (ratio (lt-get lt "ratio")))
    (make-jfm-glue
     :natural (or (first pos) 0)
     :stretch (or (second pos) 0)
     :shrink  (or (third pos) 0)
     :ratio (or ratio 1/2)
     ;; priority = {伸び用, 縮み用}
     :stretch-priority (if pri (or (first (lt-pos pri)) 0) 0)
     :shrink-priority  (if pri (or (second (lt-pos pri)) 0) 0))))

(defun class<-lua (number lt)
  "クラス定義 (lua-table) を jfm-class にする。"
  (let ((chars-lt (lt-get lt "chars"))
        (glue-lt (lt-get lt "glue"))
        (jc (make-jfm-class :number number
                            :width  (or (lt-get lt "width") 1)
                            :height (or (lt-get lt "height") 0)
                            :depth  (or (lt-get lt "depth") 0)
                            :align  (lt-get lt "align"))))
    (when chars-lt
      (setf (jc-chars jc) (parse-chars (lt-pos chars-lt))))
    (when glue-lt
      (loop for (target . entry) in (lt-int glue-lt)
            do (setf (gethash target (jc-glue jc)) (glue<-lua entry))))
    jc))

(defun parse-jfm (path)
  "jfm-jlreq.lua を読んで、クラス番号 → jfm-class のハッシュを返す。
   末尾の copy_jfm(from,to) も適用する。"
  (let* ((src (with-open-file (in path :external-format :utf-8)
                (let ((s (make-string (file-length in))))
                  (subseq s 0 (read-sequence s in)))))
         (r (make-lua-reader :string src))
         (classes (make-hash-table)))
    ;; `local jfm = {` まで飛ばす
    (let ((p (search "local jfm = {" src)))
      (unless p (error "jfm テーブルが見つからない"))
      (setf (lr-pos r) (+ p (length "local jfm = "))))
    ;; jfm テーブル本体を読む
    (let ((jfm (lr-read-table r)))
      ;; 整数キー = クラス定義
      (loop for (num . def) in (lt-int jfm)
            when (lua-table-p def)
              do (setf (gethash num classes) (class<-lua num def))))
    ;; 末尾の copy_jfm(from,to) を拾って適用する
    (apply-copy-jfm src classes)
    classes))

(defun apply-copy-jfm (src classes)
  "copy_jfm(from,to) を全部拾って適用する。
   Lua の定義通り: to のクラスに from の glue 行を複製し、
   さらに各クラスの glue に [from] があれば [to] にも複製する。"
  (let ((pos 0))
    (loop
      (let ((p (search "copy_jfm(" src :start2 pos)))
        (unless p (return))
        (let* ((open (+ p (length "copy_jfm(")))
               (close (position #\) src :start open))
               (args (subseq src open close))
               (comma (position #\, args)))
          ;; `local function copy_jfm(from,to)` の定義行は数字でないので飛ばす
          (when (and comma
                     (every #'digit-char-p (string-trim " " (subseq args 0 comma)))
                     (every #'digit-char-p (string-trim " " (subseq args (1+ comma)))))
            (let ((from (parse-integer args :end comma :junk-allowed t))
                  (to (parse-integer args :start (1+ comma) :junk-allowed t)))
              (when (and from to (gethash from classes) (gethash to classes))
                (copy-class-glue from to classes))))
          (setf pos close))))))

(defun copy-class-glue (from to classes)
  (let ((fc (gethash from classes))
        (tc (gethash to classes)))
    ;; from の glue 行を to に複製
    (maphash (lambda (k v) (setf (gethash k (jc-glue tc)) v)) (jc-glue fc))
    ;; 各クラスの glue に [from] があれば [to] にも
    (maphash (lambda (num class)
               (declare (ignore num))
               (let ((g (gethash from (jc-glue class))))
                 (when g (setf (gethash to (jc-glue class)) g))))
             classes)))

;;; ---------------------------------------------------------------------------
;;; 文字 → クラス番号の逆引き
;;; ---------------------------------------------------------------------------

(defun build-char->class (classes)
  "文字コード → クラス番号 のハッシュ。0 (漢字等) は既定なので入れない。"
  (let ((table (make-hash-table)))
    (maphash (lambda (num class)
               (unless (zerop num)
                 (dolist (c (jc-chars class))
                   (when (integerp c)
                     (setf (gethash c table) num)))))
             classes)
    table))

;;; ---------------------------------------------------------------------------
;;; ruleset -- 表を束ねて、問い合わせを平坦な参照にする
;;; ---------------------------------------------------------------------------
;;; ★合成は構築時に済ませ、内側のループ (1文字ごと) は配列/ハッシュ一発にする。
;;;   問い合わせ時にチェーンを辿る設計にしない。

(defstruct (ruleset (:conc-name rs-))
  (classes (make-hash-table))     ; クラス番号 → jfm-class
  (char->class (make-hash-table)) ; 文字コード → クラス番号
  (kanjiskip nil)                 ; jfm-glue: 和文字間
  (xkanjiskip nil)                ; jfm-glue: 和欧間
  ;; 禁則: 行頭に来てはいけない/行末に来てはいけない文字コードの集合。
  ;; JFM のクラス対で「切ってはいけない」場所は penalty で表すが、
  ;; JFM 自体は禁則を直接持たないので、クラスから導く (build-ruleset を参照)。
  (line-start-forbidden (make-hash-table))
  (line-end-forbidden (make-hash-table)))

(defun char-class-of (rs code)
  "文字コードのクラス番号。

   ★JFM の欧文クラス (27) は chars = {'alchar'} という特殊トークンで、
     個別の文字を列挙しない。'alchar' は『全ラテン文字』を表すマクロ。
     なので ASCII 範囲 (制御・空白を除く) を欧文クラス 27 に落とす。
     和字間隔・全角記号などは別途 JFM に登録されているのでそちらが優先。
   ★それ以外の未登録は 0 (漢字等)。"
  (let ((registered (gethash code (rs-char->class rs))))
    (cond (registered registered)
          ((<= #x21 code #x7E) 27)      ; ASCII 図形文字 = alchar
          (t 0))))

(defun class-glue (rs class-a class-b)
  "クラス A の後、クラス B の前に入る jfm-glue。無ければ NIL。"
  (let ((ca (gethash class-a (rs-classes rs))))
    (when ca (gethash class-b (jc-glue ca)))))

;;; JLReq の行頭禁則・行末禁則に相当する文字クラス。
;;; jfm-jlreq.lua はクラスは持つが禁則自体は持たない (禁則は pTeX 側のマクロ)。
;;; JLReq 表4/表5 に基づく標準的な割り当てをクラス番号で与える。
(defparameter *line-start-forbidden-classes*
  '(2 4 5 6 7 9 10 11)
  "行頭禁則: 終わり括弧 区切り約物 中点 句点 読点 繰返し 長音 小書き仮名。")
(defparameter *line-end-forbidden-classes*
  '(1)
  "行末禁則: 始め括弧。")

(defun build-ruleset (jfm-path)
  "jfm-jlreq.lua から ruleset を構築する。"
  (let* ((classes (parse-jfm jfm-path))
         (rs (make-ruleset :classes classes
                           :char->class (build-char->class classes))))
    ;; kanjiskip / xkanjiskip はトップレベルにあるので別に読む
    (multiple-value-bind (kanji xkanji) (read-skip-parameters jfm-path)
      (setf (rs-kanjiskip rs) kanji
            (rs-xkanjiskip rs) xkanji))
    ;; 禁則クラスに属する文字を集める
    (dolist (cls *line-start-forbidden-classes*)
      (let ((c (gethash cls classes)))
        (when c (dolist (ch (jc-chars c))
                  (when (integerp ch)
                    (setf (gethash ch (rs-line-start-forbidden rs)) t))))))
    (dolist (cls *line-end-forbidden-classes*)
      (let ((c (gethash cls classes)))
        (when c (dolist (ch (jc-chars c))
                  (when (integerp ch)
                    (setf (gethash ch (rs-line-end-forbidden rs)) t))))))
    rs))

(defun read-skip-parameters (path)
  "kanjiskip = {n,s,h} と xkanjiskip = {n,s,h} を読む。
   {伸び用priority, 縮み用priority} は別のコメントにあるが、この JFM では
   xkanjiskip の priority は {1,-3}、kanjiskip は {0,0}。ここでは値だけ拾う。"
  (let* ((src (with-open-file (in path :external-format :utf-8)
                (let ((s (make-string (file-length in))))
                  (subseq s 0 (read-sequence s in)))))
         (kanji (read-named-triple src "kanjiskip"))
         (xkanji (read-named-triple src "xkanjiskip")))
    (values
     (make-jfm-glue :natural (first kanji) :stretch (second kanji) :shrink (third kanji)
                    :stretch-priority 0 :shrink-priority 0)
     (make-jfm-glue :natural (first xkanji) :stretch (second xkanji) :shrink (third xkanji)
                    :stretch-priority 1 :shrink-priority -3))))

(defun read-named-triple (src name)
  "`NAME = {a, b, c}` を探して (a b c) を返す。xkanjiskip を先に読まないよう、
   語境界に注意する。"
  (let* ((needle (concatenate 'string name " = {"))
         (p (search needle src)))
    ;; kanjiskip は xkanjiskip の部分文字列なので、前が英字でない出現を選ぶ
    (loop while (and p (plusp p) (alpha-char-p (char src (1- p))))
          do (setf p (search needle src :start2 (1+ p))))
    (unless p (error "~a が見つからない" name))
    (let ((r (make-lua-reader :string src :pos (+ p (length name) 3))))
      (let ((tbl (lr-read-table r)))
        (lt-pos tbl)))))
