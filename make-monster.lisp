;;-----------------------------------------------------------------------
;;モンスターデータ作成用
(defstruct monster
  (health (randval (+ 10 *monster-level*)))
  (agi    (randval (+ 10 *monster-level*)))
  (damage  0)
  (agi-damage 0)
  (str-damage 0))

;;モンスターの生死判定
(defun monster-dead (m)
  (<= (monster-health m) 0))
;;モンスターグループが全滅したか判定
(defun monsters-dead ()
  (every #'monster-dead *monsters*))

;;-----------------敵からのアイテムドロップ-------------------------
(defun yote1-drop (pt)
  (if (= 1 (random 100))
      (push "メタルヨテイチの剣" (party-drop pt))))
(defun ha2ne2-drop (pt)
  (if (= 0 (random 1)) ;;とりあえず100%
      (push "ハツネツの剣" (party-drop pt))))

(defun orc-drop (pt)
  (if (= 1 (random 10))
      (push "ハンマー" (party-drop pt))))
(defun slime-drop (pt)
  (if (= 1 (random 20))
      (push "回復薬" (party-drop pt))))

;;----------仲間になるか？-----------------------------------------------------
(defun nakama? (pt name-list type)
  (if (and (null (party-new-nakama pt)) (= 0 (random 1)))
      (let ((level 0))
	(dolist (p (party-players pt))
	  (if (> (player-level p) level)
	      (setf level (player-level p))))
	(let ((hp (+ 30 (random level)))
	      (str (+ 30 (random level)))
	      (agi (+ 30 (random level))))
	  (setf (party-new-nakama pt)
		(make-player :hp hp :maxhp hp
			     :str str :maxstr str
			     :agi agi :maxagi agi
			     :level level :level-exp (+ 100 (* 10 (1- level)))
			     :name (nth (random (length name-list)) name-list) :type type))))))
;;-----------------------------------------------------------------
;;モンスターの受けたダメージ処理
(defmethod monster-hit2 (pt p m x)
  (decf (monster-health m) x)
  (incf (monster-damage m) x)
  ;;倒したら経験値取得
  (if (monster-dead m)
      (case (type-of m)
        (ha2ne2
	 (ha2ne2-drop pt)
	 (incf (player-exp p) 99))
	(orc
	 (orc-drop pt)
	 (nakama? pt *orc-name* 1)
	 (incf (player-exp p) 2))
	(slime-mold
	 (nakama? pt *slime-name* 2)
	 (slime-drop pt)
	 (incf (player-exp p) 3))
	(hydra
	 (nakama? pt *hydra-name* 3)
	 (incf (player-exp p) 4))
	(brigand
	 (nakama? pt *brigand-name* 4)
	 (incf (player-exp p) 5)))))

;;モンスターにAGIのダメージ
(defun monster-agi-hit (m x)
  (decf (monster-agi m) x)
  (incf (monster-agi-damage m) x))



(defmethod monster-attack (m players))
;;--------中ボス------------------------------------------------------------------------
(defstruct (ha2ne2 (:include monster)) (h-atk 8))
(defmethod monster-show ((m ha2ne2))
  (format nil "ボス：ハツネツエリア"))
(defmethod monster-attack ((m ha2ne2) players)
  (let ((x (+ 3 (randval (+ (player-level p) (ha2ne2-h-atk m))))))
    (case (random 3)
      (0
       (scr-format "「ハツネツの攻撃。~dのダメージをくらった。」~%" x)
       (decf (player-hp p) x))
      (1
       (let ((dame-str (- (player-str p) x)))
	 (if (= (player-str p) 0)
	     (progn (scr-format "「ネコPパンチ。HPが~d下がった。」~%" x)
		    (decf (player-hp p) x))
	     (if (>= dame-str 0)
		 (progn (scr-format "「ネコPパンチ。力が~d下がった。」~%" x)
			(decf (player-str p) x))
		 (progn (scr-format "「ネコPパンチ。力が~d下がった。」~%" (player-str p))
			(setf (player-str p) 0))))))
      (2
       (scr-format "「ハツネツが料理してご飯を食べている。ハツネツのHPが~d回復した！」~%" x)
       (incf (monster-health m) x)))))

;;--------ボス------------------------------------------------------------------------
(defstruct (boss (:include monster)) (boss-atk 10))
(defmethod monster-show ((m boss))
  (format nil "ボス：もげぞう"))
(defmethod monster-attack ((m boss) players)
  (let ((x (+ 5 (randval (+ (player-level p) (boss-boss-atk m))))))
    (case (random 5)
      ((0 3)
       (scr-format "「もげぞうの攻撃。~dのダメージをくらった。」~%" x)
       (decf (player-hp p) x))
      ((1 4)
       (let ((dame-agi (- (player-agi p) x)))
	 (if (= (player-agi p) 0)
	     (progn (scr-format "「もげぞうの攻撃。~dのダメージをくらった。」~%" x)
		    (decf (player-hp p) x))
	     (if (>= dame-agi 0)
		 (progn (scr-format "「もげぞうの不思議な踊り。素早さが~d下がった。」~%" x)
			(decf (player-agi p) x))
		 (progn (scr-format "「もげぞうの不思議な踊り。素早さが~d下がった。」~%" (player-agi p))
			(setf (player-agi p) 0))))))
      (2
       (let ((dame-agi (- (player-agi p) x))
	     (dame-str (- (player-str p) x)))
	 (scr-format "「もげぞうのなんかすごい攻撃！すべてのステータスが~d下がった！」~%" x)
	 (decf (player-hp p) x)
	 (if (>= dame-agi 0)
	     (decf (player-agi p) x)
	     (setf (player-agi p) 0))
	 (if (>= dame-str 0)
	     (decf (player-str p) x)
	     (setf (player-str p) 0)))))))

;;-------------------メタルヨテイチ--------------------------------------------------
(defstruct (yote1 (:include monster))
  (atk    (randval (+ 10 *monster-level*))))
;;(push #'make-yote1 *monster-builders*)

(defmethod monster-show ((m yote1))
  (format nil "メタルヨテイチ"))

(defmethod monster-attack ((m yote1) players)
  (let* ((atk (randval (yote1-atk m)))
	 (p (nth (random (length players)) players)))
    (case (random 2)
      (0 (scr-format "「メタルヨテイチは何もしていない。」~%"))
      (1 (scr-format "「メタルヨテイチが突然殴り掛かってきた。~aは~dのダメージを受けた。」~%" (player-name p) atk)
       (decf (player-hp p) atk)))))

(defmethod monster-hit2 (pt (p player) (m yote1) x)
  (decf (monster-health m))
  (incf (monster-damage m))
  (if (monster-dead m)
      (progn (incf (player-exp p) 100)
	     (nakama? pt *yote1-name* 5)
	     (yote1-drop pt))))

;;-------------------オーク---------------------------------------------------------
(defstruct (orc (:include monster))
  (club-level (randval (+ 8 *monster-level*)))
  (name "オーク"))

(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (let ((x (orc-club-level m)))
    (cond
      ((>= 3 x 1) (format nil "か弱いオーク"))
      ((>= 6 x 4) (format nil "日焼けしたオーク"))
      ((>= 9 x 7) (format nil "邪悪なオーク"))
      (t (format nil "マッチョオーク")))))

(defmethod monster-attack ((m orc) players)
  (let ((x (randval (orc-club-level m)))
	(p (nth (random (length players)) players)))
    (scr-format (monster-show m))
    (scr-format "が棍棒で殴ってきて ~a は ~d のダメージをくらった。~%" (player-name p) x)
    (decf (player-hp p) x)))



;;-------------------ヒドラ------------------------------
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)


(defmethod monster-show ((m hydra))
  (let ((x (monster-health m)))
    (cond
      ((>= 3 x 1)
       (format nil "意地悪なヒドラ"))
      ((>= 6 x 4)
       (format nil "腹黒いヒドラ"))
      ((>= 9 x 7)
       (format nil "強欲なヒドラ"))
      (t (format nil "グレートヒドラ")))))


(defmethod monster-attack ((m hydra) players)
  (let ((x (randval (ash (monster-health m) -1)))
	(p (nth (random (length players)) players)))
    (scr-format (monster-show m))
    (scr-format "の攻撃 ~a は ~dのダメージを食らった。~%" (player-name p) x)
    (scr-format (monster-show m))
    (scr-format "の首が一本生えてきた！~%")
    (incf (monster-health m))
    (decf (player-hp p) x)))


;;-------------------スライム------------------------------
(defstruct (slime-mold (:include monster)) (sliminess (randval (+ 5 *monster-level*))))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (let ((x (slime-mold-sliminess m)))
    (cond
      ((<= 1 x 3) (format nil "ベタベタなスライム"))
      ((<= 4 x 6) (format nil "ベトベトなスライム"))
      ((<= 7 x 9) (format nil "ベチョベチョなスライム"))
      (t (format nil "ヌルヌルなスライム")))))

(defmethod monster-attack ((m slime-mold) players)
  (let ((x (randval (slime-mold-sliminess m)))
	(p (nth (random (length players)) players)))
    (cond
      ((> (player-agi p) 0)
       (let ((dame-agi (- (player-agi p) x)))
	 (if (>= dame-agi 0)
	     (progn (scr-format (monster-show m))
		    (scr-format "は足に絡みついてきて~aの素早さが ~d 下がった！~%" (player-name p) x)
		    (decf (player-agi p) x))
	     (progn (scr-format (monster-show m))
		    (scr-format "は足に絡みついてきて~aの素早さが ~d 下がった！~%"
				(player-name p) (player-agi p))
		    (setf (player-agi p) 0)))))
      (t (scr-format (monster-show m))
	 (scr-format "が何か液体を吐きかけてきて ~aは ~d ダメージくらった！~%" (player-name p) x)
	 (decf (player-hp p) x)))))

;;-------------------ブリガンド------------------------------
(defstruct (brigand (:include monster)) (atk (+ 2 (random *monster-level*))))
(push #'make-brigand *monster-builders*)

(defmethod monster-show ((m brigand))
  (let ((x (brigand-atk m)))
    (cond
      ((<= 1 x 3) (format nil "毛の薄いブリガンド"))
      ((<= 4 x 6) (format nil "ひげもじゃなブリガンド"))
      ((<= 7 x 9) (format nil "胸毛の濃いブリガンド"))
      (t (format nil "禿げてるブリガンド")))))

(defmethod monster-attack ((m brigand) players)
  (let* ((p (nth (random (length players)) players))
	 (x (max (player-hp p) (player-agi p) (player-str p)))
	 (damage (brigand-atk m))	)
    (scr-format (monster-show m))
    (cond ((= x (player-hp p))
	   (scr-format "のスリングショットの攻撃で~aは ~d ダメージくらった！~%" (player-name p) damage)
	   (decf (player-hp p) damage))
	  ((= x (player-agi p))
	   (scr-format "は鞭で~aの足を攻撃してきた！素早さが ~d 減った！~%" (player-name p) damage)
	   (decf (player-agi p) damage))
	  ((= x (player-str p))
	   (scr-format "は鞭で~aの腕を攻撃してきた！力が ~d 減った！~%" (player-name p) damage)
	   (decf (player-str p) damage)))))


;;モンスターのSTRにダメージ
(defun monster-str-hit (m x)
  (case (type-of m)
    (orc        (decf (orc-club-level m) x))
    (hydra      (decf (monster-health m) x))
    (slime-mold (decf (slime-mold-sliminess m) x))
    (brigand    (decf (brigand-atk m) x))
    (yote1      (decf (yote1-atk m) x))
    (ha2ne2     (decf (ha2ne2-h-atk m) x))
    (boss       (decf (boss-boss-atk m) x)))
  (incf (monster-str-damage m) x))
