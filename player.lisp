(defmethod player-type-name (p)
  (case (player-type p)
    (0 "プレイヤー")
    (1 "オーク")
    (2 "スライム")
    (3 "ヒドラ")
    (4 "ブリガンド")
    (5 "メテルヨテイチ")
    (otherwise
     (error (format nil "unknown player type ~a" (player-type p))))))

(defmethod player-command-list (p)
  (case (player-type p)
    (0 '("突く" "ダブルアタック" "薙ぎ払う" "待機" "回復薬"))
    (1 '("殴る" "ぶん殴る" "待機" "回復薬"))
    (2 '("ビンタ" "ベトベト液" "待機" "回復薬"))
    (3 '("かじる" "暴れる" "待機" "回復薬"))
    (4 '("叩く" "鞭アタック" "待機" "回復薬"))
    (5 '("タイピング" "待機" "回復薬"))
    (otherwise
     (error (format nil "unknown player type ~a" (player-type p))))))



;;---------------------攻撃方法---------------------------------------------------
;;突く
(defun stab-dmg (pt p bc-cursor)
  (let ((m (pick-monster2 pt p (ato-monster-alive? 0 pt) bc-cursor)))
    (if m
	(progn (monster-hit2 pt p m (+ 2 (randval (ash (player-str p) -1))))
	       t)
	nil)))

;;ダブルアタック
(defun d-atk (pt p bc-cursor)
  (let ((m (pick-monster2 pt p (ato-monster-alive? 0 pt) bc-cursor)))
    (if (null m)
        nil
	(let ((x (randval (truncate (/ (player-str p) 6)))))
	  (monster-hit2 pt p m x) ;;選ばれたモンスターにダメージ与える
	  (unless (monsters-dead) ;;生き残ってるモンスターがいるなら２回目の攻撃
	    ;;キャンセルなしピックモンスター
	    (let ((m2 (pick-monster3 pt p (ato-monster-alive? 0 pt) bc-cursor)))
	      (monster-hit2 pt p m2 x)))
	  t))))

;;なぎ払い
(defun swing (pt p bc-cursor)
  (declare (ignore bc-cursor))
  (dotimes (x (1+ (randval (truncate (/ (player-str p) 3)))))
    (unless (monsters-dead)
      (monster-hit2 pt p (random-monster) 1)))
  t)

;;オーク殴る
(defun orc-naguru (pt p bc-cursor)
  (let ((m (pick-monster2 pt p (ato-monster-alive? 0 pt) bc-cursor)))
    (if m
        (progn (monster-hit2 pt p m (randval (+ 2 (ash (player-str p) -1))))
	       t)
	nil)))

;;オークぶん殴る 1/4の確率で大ダメージ
(defun orc-bun-naguru (pt p bc-cursor)
  (let ((m (pick-monster2 pt p (ato-monster-alive? 0 pt) bc-cursor)))
    (if m
        (let ((dmg (randval (player-str p))))
	  (if (> (floor (* (player-str p) 3/4)) dmg)
	      (setf dmg 1))
	  (monster-hit2 pt p m dmg)
	  t)
	nil)))

;;ヒドラ　かじる
(defun hydra-eat (pt p bc-cursor)
  (let ((m (pick-monster2 pt p (ato-monster-alive? 0 pt) bc-cursor)))
    (if m
        (let ((dmg (randval (+ (truncate (/ (player-str p) 4)) (truncate (/ (player-hp p) 4))))))
	  (monster-hit2 pt p m dmg)
	  t)
	nil)))

;;ヒドラ　暴れる　敵複数にダメージ
(defun hydra-aba (pt p bc-cursor)
  (declare (ignore bc-cursor))
  (dotimes (x (randval (truncate (/ (player-hp p) 6))))
    (unless (monsters-dead)
      (monster-hit2 pt p (random-monster) (randval (truncate (/ (player-str p) 6))))))
  t)

;;スライムビンタ
(defun slime-binta (pt p bc-cursor)
  (let ((m (pick-monster2 pt p (ato-monster-alive? 0 pt) bc-cursor)))
    (if m
        (let ((dmg (randval (ash (player-str p) -1))))
	  (monster-hit2 pt p m dmg)
	  t)
	nil)))

;;スライムベトベト液 敵全員のagiにダメージ
(defun slime-betobeto (pt p bc-cursor)
  (declare (ignore pt bc-cursor))
  (loop for m across *monsters* do
    (if (null (monster-dead m))
	(monster-agi-hit m (randval (truncate (/ (player-str p) 5))))))
  t)

;;ブリガンド叩く
(defun brigand-tataku (pt p bc-cursor)
  (let ((m (pick-monster2 pt p (ato-monster-alive? 0 pt) bc-cursor)))
    (if m
        (let ((dmg (1+ (randval (ash (player-str p) -1)))))
	  (monster-hit2 pt p m dmg)
	  t)
	nil)))

;;ブリガンド 鞭　敵の攻撃力(Lv)ダウン
(defun brigand-muchi (pt p bc-cursor)
  (let ((m (pick-monster2 pt p (ato-monster-alive? 0 pt) bc-cursor)))
    (if m
        (let ((dmg (randval (truncate (/ (player-str p) 10)))))
	  (monster-str-hit m dmg)
	  t)
	nil)))

;;メタルヨテイチ タイピング
(defun yote1-typing (pt p bc-cursor)
  (declare (ignore bc-cursor))
  (dotimes (x (randval (truncate (/ (player-agi p) 3))))
    (unless (monsters-dead)
      (monster-hit2 pt p (random-monster) 1)))
  t)

;;-------------------------------------------------------------------------------
;;薬を使う
(defun use-heal (p)
  (scr-format "~%「~aに回復薬を使った。」~%" (player-name p))
  (setf (player-hp p)  (player-maxhp p)
	(player-agi p) (player-maxagi p)
	(player-str p) (player-maxstr p)))

;;回復薬使う相手を選ぶ
(defun select-heal (pt p bc-cursor &optional (cursor 0))
  (gamen-clear)
  (show-player-status pt)
  (scr-fresh-line)
  (if (= (party-heal pt) 0)
      (progn (scr-format "回復薬を持っていません！~%")
	     (scr-format "次へ press any key")
	     (read-command-char))
      (progn
	(scr-format "誰に回復薬を使いますか？(z:決定 x:キャンセル)~%~%")
	(loop for p in (party-players pt)
	      for i from 0 do
		(if (= cursor i)
		    (scr-format " ▶ ")
		    (scr-format "   "))
		(scr-format "~a~%" (player-name p)))
	(case (read-command-char)
	  (z
	   (decf (party-heal pt))
	   (use-heal (nth cursor (party-players pt))))
	  (w ;;↑
	   (cond
	     ((> cursor 0)
	      (select-heal pt p bc-cursor (1- cursor)))
	     ((= cursor 0)
	      (select-heal pt p bc-cursor (1- (length (party-players pt)))))))
	  (s ;;↓
	   (cond
	     ((> (1- (length (party-players pt))) cursor)
	      (select-heal pt p bc-cursor (1+ cursor)))
	     ((= cursor (1- (length (party-players pt))))
	      (select-heal pt p bc-cursor 0))))
	  (x
	   nil)
	  (otherwise
	   (select-heal pt p bc-cursor cursor))))))

;;コマンドと攻撃関数の連想リスト
(defparameter *kougeki*
  (list
   (cons "突く" #'stab-dmg)
   (cons "薙ぎ払う" #'swing)
   (cons "ダブルアタック" #'d-atk) 
   (cons "殴る" #'orc-naguru)
   (cons "ビンタ" #'slime-binta)
   (cons "叩く" #'brigand-tataku)
   (cons "かじる" #'hydra-eat)
   (cons "ぶん殴る" #'orc-bun-naguru)
   (cons "ベトベト液" #'slime-betobeto)
   (cons "暴れる" #'hydra-aba)
   (cons "鞭アタック" #'brigand-muchi)
   (cons "回復薬" #'select-heal)
   (cons "タイピング" #'yote1-typing)))
   
