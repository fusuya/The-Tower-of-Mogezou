;;文字幅取得
(defun moge-char-width (char)
    (if (<= #x20 (char-code char) #x7e)
        1
	2))
;;string全体の文字幅
(defun string-width (string)
  (apply #'+ (map 'list #'moge-char-width string)))
;;最低n幅もったstring作成
(defun minimum-column (n string)
  (let ((pad (- n (string-width string))))
    (if (> pad 0)
	(concatenate 'string string (make-string pad :initial-element #\ ))
        string)))

;;n内の１以上の乱数
(defun randval (n)
  (1+ (random (max 1 n))))

;;a→ 0 b→ 1 c→ 2 ...
(defun ascii->number (x)
  (if (null (numberp x))
      (- (char-code (char (symbol-name x) 0)) 65)))

;; 1->a 2->b 3->c ...
(defun number->a (x)
  (code-char (+ x 96)))


;; ランキングは (("一位の名前" 秒数) ("二位の名前" 秒数) ...) の形の属
;; 性リストで、秒数でソートされて保存される。
(defconstant +ranking-file-name+ "ranking.lisp") ; ランキングファイルの名前
(defconstant +ranking-max-length+ 10)            ; ランキングに登録するエントリーの最大数

;; 合計の秒数を (時 分 秒) のリストに変換する。
(defun total-seconds-to-hms (ss)
  (let* ((h (floor ss 3600))
         (m (floor (mod ss 3600) 60))
         (s (mod ss 60)))
    (list h m s)))

;; プレーヤー name の記録 total-seconds を ranking に登録し、新しいラ
;; ンキングデータを返す。ranking に既にプレーヤーの項目がある場合は、
;; 秒数が少なければ項目を更新する。項目の数が +ranking-max-length+ を
;; 超えると、超えた分は削除される。
(defun ranking-update (name total-seconds ranking)
  (let ((ranking1
         (stable-sort
          (if (and (assoc name ranking :test #'string-equal)
                   (< total-seconds (cadr (assoc name ranking :test #'string-equal))))
              (mapcar (lambda (entry)
                        (if (string-equal (car entry) name)
                            (list name total-seconds)
                          entry))
                      ranking)
            ;; 同じタイムは後ろに追加する。早い者勝ち。
            (append ranking (list (list name total-seconds))))
          #'< :key #'cadr)))
    ;; 最大で +ranking-max-length+ の項目を返す。
    (loop for i from 1 to +ranking-max-length+
          for entry in ranking1
          collect entry)))

;; ランキングの内容を表示する。name を指定すると該当の項目の左に矢印が
;; 表示される。
(defun ranking-show (ranking &optional name)
  (loop for place from 1 to 10
        for entry in ranking
        do
        (destructuring-bind (entry-name total-seconds) entry
          (destructuring-bind (h m s) (total-seconds-to-hms total-seconds)
            (let ((arrow (if (string-equal entry-name name) "=>" "  ")))
              (scr-format "~a ~a位 ~2,'0d:~2,'0d:~2,'0d ~a~%"
                          arrow place h m s entry-name))))))

;; ランキングを更新する。ランキングファイルからデータを読み込み、1引数
;; の関数 fun にランキングデータを渡す。fun の返り値をランキングファイ
;; ルに保存する。
;;
;; TODO: 別のプロセスがランキングを同時に変更しないようにロックすべき。
(defun ranking-transaction (fun)
  (flet ((read-ranking ()
                       (with-open-file (file +ranking-file-name+
                                             :external-format :utf8
                                             :if-does-not-exist nil)
                                       (if file
                                           (let ((buf (make-string (file-length file))))
                                             (read-sequence buf file)
                                             (read-from-string buf))
                                         ;; ランキングファイルが存在しなかった場合は空のデータを返す。
                                         '())))
         (write-ranking (ranking)
                        (with-open-file (file +ranking-file-name+
                                              :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
                                        (format file "~S" ranking))))
    (let ((ranking (read-ranking)))
        (write-ranking (funcall fun ranking)))))

;; メッセージ message を表示し、ユーザーから 1 あるいは 2 を受け取る。
;; 1 を受け取れば t を、2を受け取れば nil を返す。それ以外はループ
(defun yes-no-dialog (message)
  (scr-format "~a(yes=1 or no=2)~%" message)
  (case (read-command-char)
    (1 t)
    (2 nil)
    (otherwise (yes-no-dialog message))))

;; クリア記録 total-seconds をランキングファイルへ登録時のダイアログ。
(defun ranking-dialog (total-seconds)
  (when (yes-no-dialog "ランキングに登録しますか？")
    (gamen-clear)
    (endo-win)
    (fresh-line)
    (format t "~%名前を教えてください：~%")
    ;;(scr-format "名前を入力してください:~%")
    (let ((name (read-line)))
      (ranking-transaction
       (lambda (ranking)
         (let ((ranking1 (ranking-update name total-seconds ranking)))
           (if (equal ranking1 ranking)
               (progn
                 (scr-format "ランキングに入りませんでした。~%")
                 (ranking-show ranking)
                 ranking)
             (progn
               (scr-format "見事ランクイン！~%")
               (ranking-show ranking1 name)
               ranking1))))))
    (init-charms)))
