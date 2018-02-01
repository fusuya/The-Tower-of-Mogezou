(ql:quickload :cl-charms)
(ql:quickload :exit-hooks)
(ql:quickload :flexi-streams)

(defun scr-fresh-line ()
  (let ((x (cl-charms/low-level:getcurx cl-charms/low-level:*stdscr*)))
    (if (/= 0 x)
        (cl-charms/low-level:addstr (format nil "~%")))))

(defun scr-princ (obj)
  (cl-charms/low-level:addstr (format nil "~A" obj)))

(defun scr-format (&rest args)
  (cl-charms/low-level:addstr (apply #'format (append '(nil) args))))

;; () -> sexp
(defun read-command-char ()
  (let ((c (cl-charms/low-level:getch)))
    (cond
      ((= c cl-charms/low-level:KEY_UP)    (setf c (char-code #\w)))
      ((= c cl-charms/low-level:KEY_LEFT)  (setf c (char-code #\a)))
      ((= c cl-charms/low-level:KEY_DOWN)  (setf c (char-code #\s)))
      ((= c cl-charms/low-level:KEY_RIGHT) (setf c (char-code #\d))))
    (if (and (<= 0 c) (<= c 127)) ; ascii range
        (let ((char (code-char c)))
          (if (or
               (and (char<= #\a char) (char<= char #\z))
               (and (char<= #\A char) (char<= char #\Z))
               (and (char<= #\0 char) (char<= char #\9)))
              (read-from-string (format nil "~C" (code-char c)))
	      nil))
	nil)))

(defun gets ()
  (let ((buf (make-array 0 :adjustable t :fill-pointer 0)))
    (labels
        ((add-char ()
                   (let ((code (cl-charms/low-level:getch)))
                     (cond
		       ((= code 10)
			(cl-charms/low-level:scrl 1)
			(flexi-streams:octets-to-string buf :external-format :utf-8))
		       (t
			 (vector-push-extend code buf)
                         (add-char))))))
      (add-char))))

;; () -> sexp
(defun read-command-line ()
  (read-from-string (gets)))

;; () -> string
(defun read-string ()
  (gets))

(defun init-charms ()
  (cl-charms/low-level:initscr)
  (charms/ll:clearok charms/ll:*stdscr* 1)
  (cl-charms/low-level:scrollok cl-charms/low-level:*stdscr* 1)
  (cl-charms/low-level:keypad cl-charms/low-level:*stdscr* 1)
  (cl-charms/low-level:raw)
  ;;(charms/ll:timeout 2000)
  (charms/ll:noecho)
  (exit-hooks:add-exit-hook #'cl-charms/low-level:endwin))


;;移動先選択
(defun map-move (map pt)
  (unless (or *battle?* (= *end* 2))
    (show-map map pt)
    ;;(scr-format "~s~%" (donjon-enemies map))
    ;;(scr-format "~s~%" (list (player-posx p) (player-posy p)))
    (case (read-command-char)
      ((w k 8) (update-map map pt -1 0))
      ((s j 2)
       (update-map map pt 1 0))
      ((d l 6)
       (update-map map pt 0 1))
      ((a h 4)
       (update-map map pt 0 -1))
      (q (select-heal pt 0 0 0))
       (i
        (show-item pt))
      (r
       (setf *end* 2)
       (return-from map-move))
      (otherwise
       (scr-format "w,a,s,d,q,rの中から選んでください！~%")))
    (encount-enemy pt map) ;;敵との当たり判定
    (enemy-move map)       ;;敵移動
    ;;(show-map map pt)      ;;マップ表示
    (encount-enemy pt map) ;;敵との当たり判定
    (map-move map pt)))

(defun show-map-key ()
  (scr-format "[f]オート回復薬設定 [i]持ち物を見る [g]武器合成~%")
  (scr-format "どちらに移動しますか？[↑ ]上 [↓ ]下 [→ ]右 [← ]左 [q]薬を使う [r]終わる: ~%"))

(defun gamen-clear ()
  (charms/ll:clear))

(defun endo-win ()
  (charms/ll:endwin))

(defun utf-8-char-bytes (byte)
  "最初のバイトから一文字のバイト数を算出する"
  (let ((n (- 8 (integer-length (logxor byte #xff)))))
    (case n
      (0 1)
      (1 nil)
      (otherwise n))))
