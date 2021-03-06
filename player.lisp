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



