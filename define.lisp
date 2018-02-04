
(defparameter *tate* 11) ;;マップサイズ
(defparameter *yoko* 11)
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)

(defparameter *battle?* nil)
(defparameter *monster-num* 10)
(defparameter *monster-level* 1) ;;階数によるモンスターのレベル
(defparameter *boss?* 0)
(defparameter *end* 0)
(defparameter *lv-exp* 100)
(defparameter *start-time* 0)
(defparameter *ha2ne2* nil)
(defparameter *copy-buki* (copy-tree *buki-d*))
(defparameter *urawaza* nil)


(defparameter *orc-name*
  '("オークピー" "トンヌラ" "ブタピー" "オークタン" "コブタ" "ほげぞう" "オオオーク"
    "最終皇帝" "ブヒブヒ" "たけし" "オークデビル" "イノシシ" "茨城オーク"))

(defparameter *slime-name*
  '("スラリソ" "ヌルリン" "ベチョリン" "ベトリン" "デロデロ" "ヌルイム" "ズラリン"
    "ケロリン" "群馬スライム" "まさし" "スラぞう"))

(defparameter *hydra-name*
  '("ヒードラ" "よしひこ" "ヘビリン" "へびぞう" "スゴイヘビ" "キングギドラ" "ヘビックス"
    "ヤマダノ" "オロチ" "ハイドラ" "ドラタロウ" "にしきのくん" "パイソン"))

(defparameter *brigand-name*
  '("サンゾク" "ムナゲ" "ヒザゲ" "ワキゲ" "アラサー" "ぶりぶり" "ガンドー" "ケツゲ"
    "やわ毛" "バリカタ" "親方" "ヒゲガンド" "うす毛" "カンダタ" "ブリトニー"))

(defparameter *yote1-name*
  '("ヨテイチ" "メタイチ" "うなぎ" "ナニモシナイ"))
(defstruct party
  (posx 0)
  (posy 0)
  (players nil)
  (heal 2)
  (hammer 5)
  (map 1)
  (msg nil)
  (item nil) ;;持ち物リスト
  (drop nil) ;;敵からのドロップ品一時保管場所
  (new-nakama nil)
  (monster-num 0)) ;;戦闘時の敵の総数

(defstruct player
  (hp 30)
  (maxhp 30)
  (agi 30)
  (maxagi 30)
  (str 30)
  (maxstr 30)
  (level 1)
  (level-exp 100)
  (exp 0)
  (name nil)
  (type 0) ;; 0:プレイヤー 1:オーク 2:スライム 3:ヒドラ 4:ブリガンド 5 メテルヨテイチ
  (buki '("なし" 0 0 0)))
  
(defstruct donjon
  (map nil)  ;;マップ
  (tate 13)  ;;縦幅
  (yoko 19)  ;;横幅
  (enemies nil)
  (path nil)
  (stop-list nil)) ;;行き止まりリスト
