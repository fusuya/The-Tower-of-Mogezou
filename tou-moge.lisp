(load "item.lisp" :external-format :utf-8)
(load "komono.lisp")
(load "define.lisp" :external-format :utf-8)
(load "player.lisp")
(load "make-monster.lisp" :external-format :utf-8)

(load "maze-test.lisp" :external-format :utf-8)

(defun init-data ()
  (setf *battle?* nil
	*monster-num* 6
	*monster-level* 1
	*boss?* 0
	*end* 0
	*start-time* (get-internal-real-time)
	*ha2ne2* nil
	*copy-buki* (copy-tree *buki-d*)))



;;コンティニューメッセージ
(defun continue-message ()
  (scr-format "もう一度挑戦しますか？(yes=1 or no=2)~%")
  (case (read-command-char)
    (1 (main))
    (2 nil)
    (otherwise (continue-message))))
;;ゲームオーバーメッセージ
(defun game-over-message (pt)
  (scr-format "Game Over.~%")
  (scr-format "あなたは地下~d階で力尽きた。~%" (party-map pt))
  ;;(ranking-dialog 0)
  (continue-message))




;;レベルアップポイント振り分け
(defun point-wake (p point n cursor)
  (if (= n 0)
      ;;振り分け終わったらステータス全回復
      (setf (player-hp p) (player-maxhp p)
	    (player-str p) (player-maxstr p)
	    (player-agi p) (player-maxagi p))
      (progn
	(gamen-clear)
	(scr-format "~%~%")
	(scr-format "~aがレベルアップ！ステータスポイントを~d獲得しました。~%" (player-name p) point)
	(scr-format "ポイントを振り分けてください。残り ~d ポイント~%" n)
	(loop for i from 0 to 2
	      do
		 (if (= cursor i)
		     (scr-format " ▶ ")
		     (scr-format "   "))
		 (case i
		   (0 (scr-format "H P ~d~%" (player-maxhp p)))
		   (1 (scr-format "ATK ~d~%" (player-maxstr p)))
		   (2 (scr-format "AGI ~d~%" (player-maxagi p)))))
	(case (read-command-char)
	  (z
	   (case cursor
	     (0 (incf (player-maxhp p))
	      (point-wake p point (1- n) cursor))
	     (1 (incf (player-maxstr p))
	      (point-wake p point (1- n) cursor))
	     (2 (incf (player-maxagi p))
	      (point-wake p point (1- n) cursor))))
	  (w ;;↑
	   (if (> cursor 0)
	       (point-wake p point n (1- cursor))
	       (point-wake p point n cursor)))
	  (s ;;↓
	   (if (> 2 cursor)
	       (point-wake p point n (1+ cursor))
	       (point-wake p point n cursor)))
	  (otherwise
	   (point-wake p point n cursor))))))

;;戦闘終了後レベルアップ
(defun level-up (p)
  (loop while (>= (player-exp p) (player-level-exp p)) do
    (let ((point (randval 3)))
      (point-wake p point point 0)
      (decf (player-exp p) (player-level-exp p))
      (incf (player-level p))
      (incf (player-level-exp p) 10))))

;;レベルアップした味方がいるか
(defun level-up-pt (pt)
  (dolist (p (party-players pt))
    (level-up p)))



;;武器装備してステータス更新
(defun equip-buki (item p)
  (incf (player-hp p)     (- (third item) (third (player-buki p))))
  (incf (player-maxhp p)  (- (third item) (third (player-buki p))))
  (incf (player-str p)    (- (second item) (second (player-buki p))))
  (incf (player-maxstr p) (- (second item) (second (player-buki p))))
  (incf (player-agi p)    (- (fourth item) (fourth (player-buki p))))
  (incf (player-maxagi p) (- (fourth item) (fourth (player-buki p))))
  (if (>= 0 (player-hp p))
      (setf (player-hp p) 1))
  (if (> 0 (player-str p))
      (setf (player-str p) 0))
  (if (> 0 (player-agi p))
      (setf (player-agi p) 0))
  (setf (player-buki p) item))

(defun equip-select (pt p item)
  (scr-format "「~aは~aを装備した。」~%" (player-name p) (first item))
  (if (not (string= "なし" (first (player-buki p))))
      (push (player-buki p) (party-item pt)))
  (equip-buki item p))

;;見つけた武器を装備するか
(defun equip? (pt item cursor)
  (let ((len (string-width (first item))))
    (gamen-clear)
    (scr-format "~%~aを見つけた。誰に装備しますか?(z:決定 x:キャンセル)~%~%" (first item))
    (loop for p in (party-players pt) do
      (if (> (string-width (first (player-buki p))) len)
	  (setf len (string-width (first (player-buki p))))))
    (scr-format "                ~a  ATK   HP  AGI~%" (format nil (minimum-column len " name")))
    (scr-format "   発見した武器:~a   ~2d   ~2d   ~2d~%~%"
		(format nil (minimum-column len (first item)))
		(second item) (third item) (fourth item))
    (loop for p in (party-players pt)
	  for i from 0 do
	    (if (= cursor i)
		(scr-format " ▶ ")
		(scr-format "   "))
	    (scr-format "~a:~a   ~2d   ~2d   ~2d~%"
			(format nil (minimum-column 12 (player-name p)))
			(format nil (minimum-column len (first (player-buki p))))
			(second (player-buki p))
			(third (player-buki p)) (fourth (player-buki p))))
    (case (read-command-char)
      (z
       (equip-select pt (nth cursor (party-players pt)) item))
      (w ;;↑
       (cond
	 ((> cursor 0)
	  (equip? pt item (1- cursor)))
	 ((= cursor 0)
	  (equip? pt item (1- (length (party-players pt)))))))
      (s ;;↓
       (cond
	 ((> (1- (length (party-players pt))) cursor)
	  (equip? pt item (1+ cursor)))
	 ((= cursor (1- (length (party-players pt))))
	  (equip? pt item 0))))
      (x
       (scr-format "「~aを見なかったことにした。」~%" (first item)))
      (otherwise
       (equip? pt item cursor)))))
  

;;戦闘終了後アイテム入手
(defun item-drop? (pt)
  ;;(gamen-clear)
  (scr-format "~%~%")
  (dolist (item (party-drop pt))
    (let ((buki (assoc item *event-buki* :test #'equal)))
      (cond
	(buki (equip? pt buki 0))
	((string= item "ハンマー")
	 (scr-format "「ハンマーを拾った！」~%")
	 (incf (party-hammer pt)))
	((string= item "回復薬")
	 (scr-format "「回復薬を拾った！」~%")
	 (incf (party-heal pt))))
      (setf (party-drop pt) nil))) ;;ドロップ品を消す
  (scr-format "~%~%次へ = any key")
  (read-command-char))






(defgeneric agi (character))
(defmethod agi ((character monster))
  (monster-agi character))
(defmethod agi ((character player))
  (player-agi character))

;;agiでプレイヤーと敵をソートする
(defun sort-agi (p-m-list)
  (sort
   p-m-list  #'> :key #'agi))


;;プレイヤーの生死判定
(defun player-dead (p)
  (<= (player-hp p) 0))

(defun players-dead (pt)
  (every #'player-dead (party-players pt)))


;;cursorより前に生きてるモンスターおるか？
(defun mae-monster-alive? (cursor)
  (if (> 0 cursor)
      nil
      (if (monster-dead (aref *monsters* cursor))
	  (mae-monster-alive? (1- cursor))
	  cursor))) ;;生きてるモンスターの番号を返す

;;cursorより後ろに生きてるモンスターおるか？
(defun ato-monster-alive? (cursor pt)
  (if (>= cursor (party-monster-num pt))
      nil
      (if (monster-dead (aref *monsters* cursor))
	  (ato-monster-alive? (1+ cursor) pt)
	  cursor)))

;;ランダムでモンスターを選択
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))


;;汎用
(defun show-command-k (pt p bc-cursor atk-list)
  (scr-format "------------------------------------------------------------~%")
  (scr-format "~aの行動~%" (player-name p))
  (loop for atk in atk-list
	for i from 0 do
	  (if (= i bc-cursor)
	       (scr-format "▶ ")
	       (scr-format "  "))
	   (if (string= "回復薬" atk)
	       (scr-format "~a[~d]~%" atk (party-heal pt))
	       (scr-format "~a~%" atk))))

;;プレイヤーズのステータス表示
(defun show-player-status (pt)
  (loop for i from 0 to 4 do
    (loop for p in (party-players pt) do
      (let ((ww (max 13 (+ 2 (string-width (format nil "Lv~2d ~a" (player-level p) (player-name p)))))))
      (case i
	(0 (scr-format "~a" (minimum-column ww (format nil "Lv~2d ~a" (player-level p) (player-name p)))))
	(1 (scr-format "~a" (minimum-column ww (format nil "H P ~3d/~3d" (player-hp p) (player-maxhp p)))))
	(2 (scr-format "~a" (minimum-column ww (format nil "ATK ~3d/~3d" (player-str p) (player-maxstr p)))))
	(3 (scr-format "~a" (minimum-column ww (format nil "AGI ~3d/~3d" (player-agi p) (player-maxagi p)))))
	(4 (scr-format "~a" (minimum-column ww (format nil "EXP ~3d/~3d" (player-exp p) (player-level-exp p))))))))
    (scr-fresh-line)))

;;(カーソル付きで)敵表示
(defun show-pick-monsters (&optional (cursor 0) (pick nil))
  (scr-fresh-line)
  (scr-format "-----------------------敵が現れた！-------------------------~%")
  (scr-format "敵:~%")
  (loop for m across *monsters*
	for x = 0 then x
	do
	   (cond
	     ((monster-dead m)
	      (scr-format "~a" (minimum-column 3 ""))
	      (scr-format "~c."  (number->a (incf x)))
	      (if (> (monster-damage m) 0)
		  (progn
		    (scr-format "~a" (minimum-column 31 "**死亡**"))  
		    (scr-format "~dのダメージを与え倒した。~%" (monster-damage m)))
		  (scr-format "**死亡**~%")))
	     (t
	      (if (and pick (= x cursor)) ;;敵選択するときだけカーソル表示
		  (scr-format "~a" (minimum-column 4 " ▶ "))
		  (scr-format "~a" (minimum-column 3 "")))
	      (scr-format "~c."  (number->a (incf x)))
	      (scr-format "~a"
			  (minimum-column 9 (format nil "(HP=~d) " (monster-health m))))
	      (scr-format "~a" (minimum-column 22 (monster-show m)))
	      (cond
		((> (monster-damage m) 0)
		 (scr-format "~dのダメージを与えた。~%" (monster-damage m)))
		((> (monster-agi-damage m) 0)
		 (scr-format "agiに~dのダメージを与えた。~%" (monster-agi-damage m)))
		((> (monster-str-damage m) 0)
		  (scr-format "strに~dのダメージを与えた。~%" (monster-str-damage m))))
	      (scr-fresh-line)))
	   (setf (monster-damage m) 0
		 (monster-agi-damage m) 0
		 (monster-str-damage m) 0)));;与えたダメージリセット

;;モンスター選択 カーソル選択ver bc-cursor= battle command cursor
(defun pick-monster2 (pt p cursor bc-cursor &optional (cancellable t))
  (let ((atk-list (player-command-list p)))
    (gamen-clear)
    (show-player-status pt)
    (show-pick-monsters cursor t)
    (show-command-k pt p bc-cursor atk-list))
  (case (read-command-char)
    (z (aref *monsters* cursor)) ;;cursor位置のモンスターを返す
    (x ;;キャンセル
     (if cancellable
         nil
         (pick-monster2 pt p cursor bc-cursor nil)))
    (w ;;↑
     (let ((alive-num (mae-monster-alive? (1- cursor))))
       (if alive-num
           (pick-monster2 pt p alive-num bc-cursor)
           (pick-monster2 pt p cursor bc-cursor))))
    (s ;;下
     (let ((alive-num (ato-monster-alive? (1+ cursor) pt)))
       (if alive-num
           (pick-monster2 pt p alive-num bc-cursor)
           (pick-monster2 pt p cursor bc-cursor))))
    (otherwise
     (pick-monster2 pt p cursor bc-cursor))))
;;ダブルスウィング２回目用　キャンセルなしピックモンスター
(defun pick-monster3 (pt p cursor bc-cursor)
  (pick-monster2 pt p cursor bc-cursor nil))

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
(defun swing (pt p)
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
(defun hydra-aba (pt p)
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
(defun slime-betobeto (p)
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
;;-------------------------------------------------------------------------------
  
;;薬を使う
(defun use-heal (p)
  (scr-format "~%「~aに回復薬を使った。」~%" (player-name p))
  (setf (player-hp p)  (player-maxhp p)
	(player-agi p) (player-maxagi p)
	(player-str p) (player-maxstr p)))

;;回復薬使う相手を選ぶ
(defun select-heal (pt cursor)
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
	      (select-heal pt (1- cursor)))
	     ((= cursor 0)
	      (select-heal pt (1- (length (party-players pt)))))))
	  (s ;;↓
	   (cond
	     ((> (1- (length (party-players pt))) cursor)
	      (select-heal pt (1+ cursor)))
	     ((= cursor (1- (length (party-players pt))))
	      (select-heal pt 0))))
	  (x
	   nil)
	  (otherwise
	   (select-heal pt cursor))))))

;;攻撃方法カーソル選択 type 0:主人公 1:オーク 2:スライム 3:ヒドラ 4:ブリガンド 5:メタルヨテイチ
(defun player-attack3 (pt p bc-cursor)
  (let* ((atk-list (player-command-list p))
	 (command-len (1- (length atk-list))))
    (gamen-clear)
    (show-player-status pt)
    (show-pick-monsters)
    (show-command-k pt p bc-cursor atk-list)
    (case (read-command-char)
      (z ;;決定
       (let ((cmd (nth bc-cursor atk-list)))
         (cond
           ((equal cmd "待機") nil)
           ((equal cmd "回復薬") (select-heal pt 0))
           (t
            (if (not (cond ((equal cmd "突く") (stab-dmg pt p bc-cursor))
                           ((equal cmd "殴る") (orc-naguru pt p bc-cursor))
                           ((equal cmd "ビンタ") (slime-binta pt p bc-cursor))
                           ((equal cmd "かじる") (hydra-eat pt p bc-cursor))
                           ((equal cmd "叩く") (brigand-tataku pt p bc-cursor))
                           ((equal cmd "ダブルスウィング") (d-atk pt p bc-cursor))
                           ((equal cmd "ぶん殴る") (orc-bun-naguru pt p bc-cursor))
                           ((equal cmd "ベトベト液") (slime-betobeto p))
                           ((equal cmd "暴れる") (hydra-aba pt p))
                           ((equal cmd "鞭アタック") (brigand-muchi pt p bc-cursor))
                           ((equal cmd "薙ぎ払う") (swing pt p))
                           (t (error (format nil "unknown command ~a" cmd)))))
                (player-attack3 pt p bc-cursor)))))) ;;pickmonsterがキャンセルされた場合
      (w ;;↑
       (cond
	 ((= bc-cursor 0) ;;カーソルが家一番上にあったら一番下へ
	  (player-attack3 pt p command-len))
	 ((> bc-cursor 0) ;;カーソルを一個上へ
	  (player-attack3 pt p (1- bc-cursor)))))
      (s ;;↓
       (cond
	 ((= command-len bc-cursor) ;;カーソルが一番下にあったら一番上に行く
	  (player-attack3 pt p 0))
	 ((> command-len bc-cursor) ;;カーソルを一個下へ
	  (player-attack3 pt p (1+ bc-cursor)))))
      (otherwise
       (player-attack3 pt p bc-cursor)))))



;;ランダムなモンスターグループを作る
(defun init-monsters (pt)
  (setf *monsters*
	(map 'vector
	     (lambda (x)
	       (declare (ignore x))
	       ;;(funcall (nth (random (length *monster-builders*)) *monster-builders*)))
               (let ((y (random 101)))
		 ;;モンスターの出現率
                 (cond
                   ((<= 0 y 25) (make-orc))
                   ((<= 26 y 50) (make-hydra))
                   ((<= 51 y 75) (make-slime-mold))
                   ((<= 76 y 99) (make-brigand))
                   (t (make-yote1 :health 3)))))
	     (make-array (setf (party-monster-num pt)
			       (randval (+ *monster-num*
                                           (floor (player-level (car (party-players pt))) 4))))))))
;;配列の０番目にボス、あとはランダムなモンスター(m=0,もげぞう m=1,ハツネツ)
(defun boss-monsters (pt m)
  (let ((hoge 0))
    (setf *monsters*
	  (map 'vector
	       (lambda (x)
		 (declare (ignore x))
		 (if (= hoge 0)
		     (progn (incf hoge)
			    (cond
			      ((= m 0) (make-boss :health 300))
			      ((= m 1) (make-ha2ne2 :health 220))))
		     (funcall (nth (random (length *monster-builders*))
				   *monster-builders*))))
	       (make-array 10)))
    (setf (party-monster-num pt) 10)))

;;モンスターの行動を表示
(defun show-monster-atk (pt m players)
  (gamen-clear)
  (show-player-status pt)
  (show-pick-monsters)
  (scr-format "------------------------------------------------------------~%")
  (monster-attack m players)
  (scr-fresh-line)
  (scr-format "次へ any key~%")
  (read-command-char))

;;勝利メッセージ
(defun victory-message (pt)
  (gamen-clear)
  (show-player-status pt)
  (show-pick-monsters)
  (scr-format "~%~%")
  (scr-format "「大 勝 利 ！」~%~%")
  (scr-format "次へ = any key")
  (read-command-char))

;;パーティ人数がいっぱいのとき 仲間のlist番号に合わせてcursor位置も1から
(defun change-nakama? (pt p cursor)
  (gamen-clear)
  (let ((name-len (string-width (player-name p))))
    (scr-format "パーティー人数が一杯です。交代する仲間を選んでください。~%")
    (scr-format "(z:決定 x:キャンセル)~%~%")
    (loop for i from 1 to 3 do
      (if (> (string-width (player-name (nth i (party-players pt)))) name-len)
	  (setf name-len (string-width (player-name (nth i (party-players pt)))))))
    (scr-format "        ~a  HP  STR  AGI~%" (format nil (minimum-column name-len "  name")))
    (scr-format "   New! ~a  ~2d   ~2d  ~2d~%~%"
		(format nil (minimum-column name-len (player-name p)))
		(player-hp p) (player-str p) (player-agi p))
    (loop for i from 1 to 3 do
      (let ((n (nth i (party-players pt))))
	(if (= cursor i)
	    (scr-format "      ▶ ")
	    (scr-format "        "))
	(scr-format "~a  ~2d   ~2d  ~2d~%"
		    (format nil (minimum-column name-len (player-name n)))
		    (player-maxhp n) (player-maxstr n) (player-maxagi n))))
    (case (read-command-char)
      (z ;;決定
       (let ((out-nakama (nth cursor (party-players pt))))
	 (setf (party-players pt) (substitute p out-nakama (party-players pt) :test #'equalp :count 1)
	       (party-new-nakama pt) nil))) ;;new仲間を空にする
      (w ;;↑
       (cond
	 ((= cursor 1) ;;カーソルが家一番上にあったら一番下へ
	  (change-nakama? pt p 3))
	 ((> cursor 1) ;;カーソルを一個上へ
	  (change-nakama? pt p (1- cursor)))))
      (s ;;↓
       (cond
	 ((= 3 cursor) ;;カーソルが一番下にあったら一番上に行く
	  (change-nakama? pt p 1))
	 ((> 3 cursor) ;;カーソルを一個下へ
	  (change-nakama? pt p (1+ cursor)))))
      (x ;;キャンセル
       (setf (party-new-nakama pt) nil)) ;;new仲間を空にする
      (otherwise
       (change-nakama? pt p cursor)))))

;;仲間にするか？
(defun nakama-in? (pt p cursor)
  (let ((mon-type (player-type-name p))
	(name-len (string-width (player-name p))))
    (gamen-clear)
    (scr-format "~%~%~aが起き上がり仲間になりたそうにこちらを見ている。~%~%" mon-type)
    (scr-format "~a  HP  STR  AGI~%" (format nil (minimum-column name-len "  name")))
    (scr-format "~a  ~2d   ~2d  ~2d~%~%" (player-name p) (player-hp p) (player-str p) (player-agi p))
    (scr-format "仲間にしますか？~%")
    (loop for mes in '("はい" "いいえ")
	  for i from 0 to 1 do
	    (if (= cursor i)
		(scr-format " ▶ ")
		(scr-format "   "))
	    (scr-format "~a~%" mes))
    (case (read-command-char)
      (z
       (if (= cursor 0)
	   (if (>= (length (party-players pt)) 4)
	       (change-nakama? pt p 1)
	       (setf (party-players pt) (append (party-players pt) (list p))
		     (party-new-nakama pt) nil))
	   (setf (party-new-nakama pt) nil)))
      (w ;;↑
       (cond
	 ((> cursor 0)
	  (nakama-in? pt p (1- cursor)))
	 ((= cursor 0)
	  (nakama-in? pt p 1))))
      (s ;;↓
       (cond
	 ((> 1 cursor)
	  (nakama-in? pt p (1+ cursor)))
	 ((= cursor 1)
	  (nakama-in? pt p 0))))
      (x
       (setf (party-new-nakama pt) nil))
      (otherwise
       (nakama-in? pt p cursor)))))

(defun junban-list (lst)
  (loop for p in lst
	collect (if (equal 'player (type-of p))
		    (player-name p)
		    (type-of p))))
;;バトル時、プレイヤーが死ぬかモンスターが全滅するまでループ
(defun game-loop (pt)
  (unless (or (players-dead pt) (monsters-dead))
    (let ((attack-jun (sort-agi (append (party-players pt)
                                        (coerce *monsters* 'list)))))
      (loop for p in attack-jun
	    until (or (players-dead pt) (monsters-dead))
	    do
	(case (type-of p)
	  (player
	   (player-attack3 pt p 0))
	  (otherwise
	   (if (null (monster-dead p))
	       (show-monster-atk pt p (party-players pt))))))
      (game-loop pt))))

;;バトル開始
(defun orc-battle (pt)
  (cond ;;モンスターズ作成
    ((= *boss?* 1) ;;ラスボス
     (boss-monsters pt 0))
    ((= *boss?* 2) ;;中ボス
     (boss-monsters pt 1))
    ((= *boss?* 0) ;;雑魚
     (init-monsters pt)))
  (game-loop pt) ;;バトルループ
  (gamen-clear)
  (show-player-status pt)
  (show-pick-monsters)
  (scr-format "~%~%")
  (cond
    ((players-dead pt) ;;プレイヤーが死んだとき
     (game-over-message pt)
     (setf *end* 2))
    (t ;;(monsters-dead) 敵を倒したとき
     (level-up-pt pt) ;;レベルアップ処理
     (if (party-drop pt)
	 (item-drop? pt)) ;;アイテム入手処理
     (if (party-new-nakama pt) ;;仲間になる敵がいる
	 (nakama-in? pt (party-new-nakama pt) 0))
     (cond
       ((= *boss?* 1) (setf *end* 1)) ;;ラスボスならエンディングへ
       ((= *boss?* 2) (setf *ha2ne2* t))) ;;中ボス倒したフラグ
     ;;バトルフラグとボスフラグを初期化
     (setf *battle?* nil
	   *boss?* 0)
     (victory-message pt))))

    






;;-----------------------マップ------------------------------------------------------------
;;---------------------------------------------------------------------------------------
;;マップ移動
(defun show-msg (pt)
  (if (party-msg pt)
      (scr-format "~a~%" (party-msg pt)))
  (setf (party-msg pt) nil))


;;現在の装備表示
(defun show-item (pt)
  (let ((name-len 0)
	(buki-len 0))
    (gamen-clear)
    (loop for p in (party-players pt) do
      (if (> (string-width (first (player-buki p))) buki-len)
	  (setf buki-len (string-width (first (player-buki p)))))
      (if (> (string-width (player-name p)) name-len)
	  (setf name-len (string-width (player-name p)))))
    (show-player-status pt)
    (scr-fresh-line)
    (scr-format "~a:~a  ATK  HP  AGI~%"
		(format nil (minimum-column name-len "  name"))
		(format nil (minimum-column buki-len " 武器")))
    (loop for p in (party-players pt)
	  for x from 1 do
	    (scr-format "~a:~a   ~2,'0d  ~2,'0d   ~2,'0d~%"
		        (format nil (minimum-column name-len (player-name p)))
			(format nil (minimum-column buki-len (first (player-buki p))))
			(second (player-buki p))
			(third (player-buki p)) (fourth (player-buki p))))
    (scr-fresh-line)
    (scr-format "戻る press any key~%")
    (read-command-char)))
    

  

(defun map-type (num)
  (case num
    (30 "ロ") ;; 壁
    (40 "ロ")
    (0  "　")
    (1  "＠") ;; プレイヤーの位置
    (4  "薬") ;; 薬
    (5  "ボ") ;;ボス
    (3  "宝") ;; 宝箱
    (2  "上") ;; 下り階段
    (6  "イ") ;; イベント
    (7  "ハ") ;; 中ボス ハツネツエリア
    ))

;;マップ表示
(defun show-map (map pt)
  (gamen-clear)
  (scr-format "~d階~%" (party-map pt))
  ;;(show-players pt)
  (show-player-status pt)
  (scr-format "~%")
  (loop for i from 0 below (donjon-tate map) do
    (loop for j from 0 below (donjon-yoko map) do
      
      (if (find (list j i) (donjon-enemies map) :test #'equal)
	  (scr-format "て")
	  (scr-format (map-type (aref (donjon-map map) i j))))
      (if (= j (- (donjon-yoko map) 1))
	  (case i
            (1 (scr-format " 回復薬    ~d個~%" (party-heal pt)))
            (2 (scr-format " ハンマー  ~d個~%" (party-hammer pt)))
	    (3 (scr-format " 現在の装備品[i]~%"))
	    (6 (scr-format " 薬を使う[q]~%"))
 	    (8 (scr-format " ヘルプ[h]~%"))
	    (9 (scr-format " 終わる[r]~%"))
	    (otherwise (scr-fresh-line))))))
  (show-msg pt)
  )

(defun show-help ()
  (gamen-clear)
  (scr-format "-------マップ記号の意味-------~%")
  (scr-format " 主:プレイヤーの位置~%")
  (scr-format " 宝:宝箱~%")
  (scr-format " 下:下り階段~%")
  (scr-format " 薬:回復薬~%")
  (scr-format " ボ:ボス~%")
  (scr-format " イ:イベント~%")
  (scr-format " ハ:中ボス~%")
  (scr-format "~%")
  (scr-format "-----------------キーバインド------------------~%")
  (scr-format "[z]:決定           [x]:キャンセル [g]武器合成~%")
  (scr-format "[i]:持ち物表示     [q]:薬を使う   [r]:ゲーム終了~%")
  (scr-format "[f]:オート回復設定 [h]:ヘルプを開く~%")
  (read-command-char))
#|
;;マップ表示 視界制限ver
(defun show-fog-map (map p)
  (scr-format "地下~d階~%" (player-map p))
  (scr-format "現在のステータス HP ~d, 素早さ ~d, 力 ~d, exp ~d~%" (player-hp p) (player-agi p)
	  (player-str p) (player-exp p))
  (scr-format "現在の武器:~a~%" (first (player-buki p)))
  (scr-format "持ち物:回復薬 ~d個 ハンマー~d個~%" (player-heal p) (player-hammer p))
  (loop for i from 0 below *tate* do
    (loop for j from 0 below *yoko* do
      (cond
	((or (= i 0) (= i (- *tate* 1)))
	 (scr-princ (map-type (aref map i j))))
	((or (= j 0) (= j (- *yoko* 1)))
	 (scr-princ (map-type (aref map i j))))
	((and (>= (+ (player-posy p) 2) i (- (player-posy p) 2))
	      (>= (+ (player-posx p) 2) j (- (player-posx p) 2)))
	 (scr-princ (map-type (aref map i j))))
	(t
	 (scr-princ "暗")))
      (if (= j (- *yoko* 1))
	  (case i
	    (0 (scr-format " 主:プレイヤーの位置~%"))
	    (2 (scr-format " 宝:宝箱~%"))
	    (1 (scr-format " 下:下り階段~%"))
	    (3 (scr-format " 薬:回復薬~%"))
	    (4 (scr-format " 暗:見えてない場所~%"))
	    (otherwise (scr-fresh-line)))))))
|#
;;エンディング
(defun ending ()
  (let* ((ss (floor (- (get-internal-real-time) *start-time*) 1000))
	 (h (floor ss 3600))
	 (m (floor (mod ss 3600) 60))
	 (s (mod ss 60)))
    (if *ha2ne2*
	(scr-format "~%「あなたは見事もげぞうの迷宮を完全攻略した！」~%")
	(progn (scr-format "~%「もげぞうを倒したが、逃したハツネツエリアが新たな迷宮を作り出した・・・」~%")
	       (scr-format "「が、それはまた別のお話。」~%")))
    (scr-format "クリアタイムは~2,'0d:~2,'0d:~2,'0d でした！~%" h m s)
    (ranking-dialog ss)
    (continue-message)))
;;プレイヤーが死ぬか戦闘に入るか*end*=2になるまでループ
(defun main-game-loop (map pt)
  (unless (or (= *end* 2) (players-dead pt))
    (map-move map pt)
    (if *battle?*
	(orc-battle pt))
    (cond
      ((= *end* 1) ;;ゲームクリア
       (ending))
      ((= *end* 0) ;;ゲームループ
       (main-game-loop map pt)))))
;;ゲーム開始
(defun main ()
  (let ((name "もげ"))
    (setf *random-state* (make-random-state t))
    ;;(format t "プレイヤーの名前を決めてください:~%")
    ;;(setf name (read-line))
    (init-charms)
    (let* ((p (make-player :name name))
           (p2 (make-player :name "にゃんちゅう" :type 4 :agi 33 :maxagi 33))
	   (p3 (make-player :name "カツオ" :type 2))
	   (p4 (make-player :name "あんぱん" :type 3))
           (pt (make-party :players (list p p2 p3 p4)))
           (map (make-donjon)))
      (init-data) ;;データ初期化
      (maze map pt) ;;マップ生成
      (main-game-loop map pt))))

;;壁破壊
(defun kabe-break (map pt y x)
  (scr-format "「ハンマーで壁を壊しますか？」[yes=z or no=anykey]:~%")
  (case (read-command-char)
    (z
      (if (>= (random 10) 3)
	(setf (aref map (+ (party-posy pt) y) (+ (party-posx pt) x)) 0)
	(setf (aref map (+ (party-posy pt) y) (+ (party-posx pt) x)) 3))
     (decf (party-hammer pt)))))
;;(scr-format "「壁を壊しました。」~%"))))




(defun hummer-get (pt)
  (setf (party-msg pt) "「ハンマーを見つけた。」")
  (incf (party-hammer pt)))

(defun kusuri-get (pt)
  (setf (party-msg pt) "「回復薬を見つけた。」")
  (incf (party-heal pt)))



;;重み付け抽選-----------------------------------------------
(defun rnd-pick (i rnd lst len)
  (if (= i len)
      (1- i)
      (if (< rnd (nth i lst))
	  i
	  (rnd-pick (1+ i) (- rnd (nth i lst)) lst len))))
;;lst = *copy-buki*
(defun weightpick (lst)
  (let* ((lst1 (mapcar #'cdr lst))
	 (total-weight (apply #'+ lst1))
	 (len (length lst1))
	 (rnd (random total-weight)))
    (car (nth (rnd-pick 0 rnd lst1 len) lst))))
;;------------------------------------------------------------
;; lst = *copy-buki*
;;*copy-buki*の確率の部分をずらす
(defun omomin-zurashi (lst)
  (let ((buki (mapcar #'car lst))
	(omomi (mapcar #'cdr lst)))
    (setf omomi (butlast omomi))
    (push 0 omomi)
    (mapcar #'cons buki omomi)))
;;テスト用------------------------------------
#|
(defun test-pick ()
  (let ((hoge (make-array 54)))
    (dotimes (i 10000)
      (incf (aref hoge (weightpick *omomin*))))
    hoge))
(defun test-hoge ()
  (let ((x 1))
     (loop for hoge from 0 to 53)
     collect x
     do (incf x 1)))
|#
;;---------------------------------------------
;;武器ゲット２ 全アイテムからランダム
(defun item-get2 (pt)
  (case (random 7)
    ((0 1 2 5) ;;武器ゲット
     (equip? pt (weightpick *copy-buki*) 0))
    ((3 6) (hummer-get pt)) ;;ハンマーゲット
    (4 (kusuri-get pt)))) ;;回復薬ゲット

;;プレイヤーの場所更新
(defun update-player-pos (pt x y map)
  (setf (aref map (+ (party-posy pt) y) (+ (party-posx pt) x)) 1)
  (setf (aref map (party-posy pt) (party-posx pt)) 0)
  (setf (party-posy pt) (+ (party-posy pt) y)
	(party-posx pt) (+ (party-posx pt) x)))


;;100階イベント
(defun moge-event (pt)
  (dolist (p (party-players pt))
    (if (equal (car (player-buki p)) "もげぞーの剣")
	(progn
	  (if (null (party-msg pt))
	      (setf (party-msg pt) "~%「もげぞーの剣が輝き出し、もげぞうの剣に進化した！」~%"))
	  (equip-buki (assoc "もげぞうの剣" *event-buki* :test #'equal) p))
	(setf (party-msg pt) "~%「なにも起こらなかった。」~%"))))

;;敵シンボルと接触判定
(defun encount-enemy (pt map)
  (let ((p-pos (list (party-posx pt) (party-posy pt)))) 
    (if (find p-pos (donjon-enemies map) :test #'equal)
	(progn (setf (donjon-enemies map) (remove p-pos (donjon-enemies map) :test #'equal :count 1))
	       (setf *battle?* t)))))

;;敵シンボルの移動
(defun enemy-move (map)
  (let ((new nil))
    (loop for e in (donjon-enemies map) do
      (let ((ok nil))
	(loop for n in '((0 1) (0 -1) (1 0) (-1 0)) do
	  (let ((n-pos (mapcar #'+ e n)))
	    (if (and (/= 30 (aref (donjon-map map) (cadr n-pos) (car n-pos)))
		     (/= 40 (aref (donjon-map map) (cadr n-pos) (car n-pos))))
		(push (mapcar #'+ e n) ok))))
	(push e ok)
	(push (nth (random (length ok)) ok) new)))
    (setf (donjon-enemies map) new)))
    
;;移動後のマップ更新
(defun update-map (map pt y x)
  (case (aref (donjon-map map) (+ (party-posy pt) y) (+ (party-posx pt) x))
    (30 ;;壁
     (if (and (> (party-hammer pt) 0)
	      (> (- (donjon-tate map) 1) (+ (party-posy pt) y) 0)
	      (> (- (donjon-yoko map) 1) (+ (party-posx pt) x) 0))
	 (kabe-break (donjon-map map) pt y x)))
    ;;(scr-format "「そっちには移動できません！！」~%")))
    (40
     nil)
    (2 ;;くだり階段
     (incf (party-map pt))
     (maze map pt)
     ;;２階降りるごとにハンマーもらえる
     ;; (if (= (mod (party-map p) 2) 0)
     ;; 	 (incf (party-hammer p)))
     ;;５階降りるごとに宝箱の確率変わる
     (if (= (mod (party-map pt) 5) 0)
	 (setf *copy-buki* (omomin-zurashi *copy-buki*)))
     ;;７階降りるごとに敵のレベル上がる
     (if (= (mod (party-map pt) 7) 0)
	 (incf *monster-level*)))
    (3 ;;宝箱
     (item-get2 pt)
     (update-player-pos pt x y (donjon-map map)))
    (5 ;;ボス
     (update-player-pos pt x y (donjon-map map))
     (setf *battle?* t
	   *boss?* 1))
    (6 ;;イベント
     (update-player-pos pt x y (donjon-map map))
     (moge-event pt))
    (7 ;;中ボス
     (update-player-pos pt x y (donjon-map map))
     (setf *battle?* t
           *boss?* 2))
    (otherwise
     (update-player-pos pt x y (donjon-map map)))))


;;うらわざ
(defun urawaza (p)
  (gamen-clear)
  (scr-format "神の力を授かった！~%")
  (setf (player-hp p) 999
	(player-maxhp p) 999
	(player-str p) 999
	(player-maxstr p) 999
	(player-agi p) 999
	(player-maxagi p) 999)
  (setf *urawaza* nil)
  (read-command-char))

