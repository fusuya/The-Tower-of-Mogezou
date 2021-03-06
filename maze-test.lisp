(defparameter *map100*
  #2A((30 30 30 30 30 30 30 30 30 30 30)
      (30 30 30 30 30 30 30 30 30 30 30)
      (30 30 30 30  0  5  0 30 30 30 30)
      (30 30 30 30  0  0  0 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30  3  0  0  0  0  0  0  0  6 30)
      (30 30 30 30 30  1 30 30 30 30 30)
      (30 30 30 30 30 30 30 30 30 30 30)))


(defun init-map (map tate yoko) ;;マップを壁で埋める
  (loop for i from 0 below tate do
    (loop for j from 0 below yoko do
      (if (or (= i 0) (= j 0) (= i (1- tate)) (= j (1- yoko)))
	  (setf (aref map i j) 40) ;;壊せない壁
	  (setf (aref map i j) 30))))) ;;壊せる壁
  
(defun rand1234 (lst lst1)
  (if (null lst1)
      lst
      (let* ((n (random (length lst1)))
	    (m (nth n lst1)))
	(push m lst)
	(rand1234 lst (remove m lst1)))))

(defun rem-blocks (x y map dir)
  (case dir
    (1
     (if (= 30 (aref (donjon-map map) (- y 1) x))
	 (setf (aref (donjon-map map) (- y 1) x) 0)))
    (2
     (if (= 30 (aref (donjon-map map) (+ y 1) x))
	 (setf (aref (donjon-map map) (+ y 1) x) 0)))
    (3
     (if (= 30 (aref (donjon-map map) y (+ x 1)))
	 (setf (aref (donjon-map map) y (+ x 1)) 0)))
    (4
     (if (= 30 (aref (donjon-map map) y (1- x)))
	 (setf (aref (donjon-map map) y (1- x)) 0)))))
     

(defun recursion (y x map dir)
  (let ((lst (rand1234 '() '(1 2 3 4)))
	(stop? t))
     (loop for i in lst do
	 (case i
	   (1 ;;上
	    (if (< 0 (- y 2)) ;;2マス先が迷路の外か
		(cond
		  ((= (aref (donjon-map map) (- y 2) x) 30) ;;2マス先が壁
		   (setf (aref (donjon-map map) (- y 2) x) 0)
		   (setf (aref (donjon-map map) (- y 1) x) 0)
		   (push (list x (- y 2)) (donjon-path map))
		   (push (list x (- y 1)) (donjon-path map))
		   (setf stop? nil)
		   (recursion (- y 2) x map 1)))))
	    ;;(return))
	   (2 ;;下
	    (if (> (donjon-tate map) (+ y 2)) ;;2マス先が迷路の外か
		(cond
		  ((= (aref (donjon-map map) (+ y 2) x) 30)
		   (setf (aref (donjon-map map) (+ y 2) x) 0)
		   (setf (aref (donjon-map map) (+ y 1) x) 0)
		   (push (list x (+ y 2)) (donjon-path map))
		   (push (list x (+ y 1)) (donjon-path map))
		   (setf stop? nil)
		   (recursion (+ y 2) x map 2)))))
	    ;;(return))
	   (3 ;;右
	    (if (> (donjon-yoko map) (+ x 2)) ;;2マス先が迷路の外か
		(cond
		  ((= (aref (donjon-map map) y (+ x 2)) 30)
		   (setf (aref (donjon-map map) y (+ x 2)) 0)
		   (setf (aref (donjon-map map) y (+ x 1)) 0)
		   (push (list (+ x 2) y) (donjon-path map))
		   (push (list (+ x 1) y) (donjon-path map))
		   (setf stop? nil)
		   (recursion y (+ x 2) map 3)))))
	    ;;(return))
	   (4 ;;左
	    (if (< 0 (- x 2)) ;;2マス先が迷路の外か
		(cond
		  ((= (aref (donjon-map map) y (- x 2)) 30)
		   (setf (aref (donjon-map map) y (- x 2)) 0)
		   (setf (aref (donjon-map map) y (- x 1)) 0)
		   (push (list (- x 2) y) (donjon-path map))
		   (push (list (- x 1) y) (donjon-path map))
		   (setf stop? nil)
		   (recursion y (- x 2) map 4)))))))
    (if stop? ;;行き止まりだったら
	(progn
	  ;;(scr-format "y=~d x=~d~%" y x);;テスト用
	  (rem-blocks x y map dir)
	  (push (list y x) (donjon-stop-list map)) ;;行き止まりの座標リスト
	  (setf (aref (donjon-map map) y x) 3)))))

;;numとは異なるlen内の乱数を返す((diff-num 0 1)だと無限ループになる)
(defun diff-num (num len)
  (let ((hoge (random len)))
    (if (= hoge num)
	(diff-num num len)
	hoge)))
;;マップにボスと階段をセット
(defun set-boss-kaidan (map boss-num)
  (let* ((len (length (donjon-stop-list map)))
	 (k (random len)) (b (diff-num k len))
	 (kaidan (nth k (donjon-stop-list map)))
	 (boss (nth b (donjon-stop-list map))))
    (if (= boss-num 0)
	(setf (aref (donjon-map map) (car kaidan) (cadr kaidan)) 2)
	(setf (aref (donjon-map map) (car kaidan) (cadr kaidan)) 2
	      (aref (donjon-map map) (car boss) (cadr boss)) boss-num))))

;;敵を配置する
(defun set-enemies (map)
  (let ((len (length (donjon-path map)))
	(enemy-num (+ 3 (random 6))))
    (loop for i from 0 to enemy-num do
      (push (nth (random len) (donjon-path map)) (donjon-enemies map)))))

;;マップ設定
(defun set-map (map pt moto)
  (setf (donjon-tate map) 11
        (donjon-yoko map) 11)
  (loop for i from 0 below (donjon-tate map) do
    (loop for j from 0 below (donjon-yoko map) do
      (if (= (aref moto i j) 1)
	  (setf (party-posx pt) j
		(party-posy pt) i))
      (setf (aref (donjon-map map) i j) (aref moto i j)))))

(defun maze (map pt)
  (let* ((x 0)
	 (startx 0)
	 (y 0) 
	 (starty 0))
    ;; (if (= (player-map p) 50);;50階ボスマップは広くする
    ;; 	   (setf (donjon-yoko map) (+ *yoko* 8)
    ;; 		 (donjon-tate map) (+ *tate* 2))
    ;; 	   (setf (donjon-yoko map) *yoko*
    ;; 		 (donjon-tate map) *tate*))
    (setf (donjon-map map) (make-array (list (donjon-tate map) (donjon-yoko map))));;マップ配列作成
    (init-map (donjon-map map) (donjon-tate map) (donjon-yoko map)) ;;マップ初期化
    (setf (donjon-stop-list map) nil
	  (donjon-enemies map) nil
	  (donjon-path map) nil)
    (cond
      ((= (party-map pt) 100) ;; 100階は固定マップ
       (set-map map pt *map100*))
      (t
       ;;奇数座標を初期位置にする
       (setf x (random (floor (donjon-yoko map) 2))
	     y (random (floor (donjon-tate map) 2))
	     startx (+ (* x 2) 1)
	     starty (+ (* y 2) 1))
       (setf (aref (donjon-map map) starty startx) 0) ;;初期位置を通路にする
       (recursion starty startx map 0) ;;迷路生成
       (loop until (<= 2 (length (donjon-stop-list map)))
             do
             ;; 行き止まりが 1 つしか無かったのでやりなおし
             (init-map (donjon-map map) (donjon-tate map) (donjon-yoko map))
             (setf (donjon-stop-list map) nil)
             (setf (aref (donjon-map map) starty startx) 0)
             (recursion starty startx map 0))
       (setf (aref (donjon-map map) starty startx) 1) ;;主人公の位置
       (setf (party-posy pt) starty
	     (party-posx pt) startx) ;;初期位置
       ;;パーティーの位置に敵を配置しないようにする
       (setf (donjon-path map) (remove (list (party-posx pt) (party-posy pt)) (donjon-path map) :test #'equal))
       (set-enemies map) ;;敵を配置
       (cond
	 ((= (party-map pt) 50)
	  (set-boss-kaidan map 7))
	 (t (set-boss-kaidan map 0)))))))
    ;;(d-map-map mapn)))
    ;;(test-show-map (d-map-map mapn))))
    

