(ns adventure.core
  (:gen-class))


; it's a 5*5 square maze.
(def maze {
    1 ["E:" 2 "S:" 6] 2 ["W:" 1 "E:" 3 "S:" 7] 3 ["W:" 2 "E:" 4 "S:" 8]
    4 ["W:" 3 "E:" 5 "S" 9] 5 ["W:" 4 "S:" 10]
    6 ["N:" 1 "E:" 7 "S:" 11] 7 ["N:" 2 "W:" 6 "E:" 8 "S:" 12] 8 ["N:" 3 "W:" 7 "E:" 9 "S:" 13]
    9 ["N:" 4 "W:" 8 "E:" 10 "S:" 14] 10 ["N:" 5 "W:" 9 "S:" 15]
    11 ["N:" 6 "E:" 12 "S:" 16] 12 ["N:" 7 "W:" 11 "E:" 13 "S:" 17]
    13 ["N:" 8 "W:" 12 "E:" 14 "S:" 18] 14 ["N:" 9 "W:" 13 "E:" 15 "S:" 19]
    15 ["N:" 10 "W:" 14 "S:" 20] 16 ["N:" 11 "E:" 17 "S:" 21]
    17 ["N:" 12 "W:" 16 "E:" 18 "S:" 22] 18 ["N:" 13 "W:" 17 "E:" 19 "S:" 23]
    19 ["N:" 14 "W:" 18 "E:" 20 "S:" 24] 20 ["N:" 15 "W:" 19 "S:" 25]
    21 ["N:" 16 "E:" 22] 22 [ "N:" 17 "W:" 21 "E:" 23] 23 ["N:" 18 "W:" 22 "E:" 24]
    24 ["N:" 19 "W:" 23 "E:" 25] 25 ["N:" 20 "W:" 24]}) ;created by myself
(def maze-size (count maze)) ;used from wumpus

(defn rand-unique
    "got from wumpus"
    [max exclude]
    (let [pick (rand-int max)]
        (if (exclude pick) (rand-unique max exclude) pick))) ;used from wumpus



(defn init-adventurer []
  (do
    (println "Please choose your vocation")
    (println "Rouge can avoid the attack of the trap")
    (println "Warrior can avoid the attack of a monster once")
    (println "Which vocation ? ([R]ouge/[W]arrior)")
    (let [choice (read-line)]
      (cond (= choice "R")
        {:inventory #{"torch"}
         :hp 15
         :abnormal 0
         :ability 1
         :vocation 0
         :seen #{}}
         (= choice "W")
        {:inventory #{"torch"}
         :hp 18
         :abnormal 0
         :ability 1
         :vocation 1
         :seen #{}}
        :else (do (println "It is a invalid vocation.")
                  (println "You will become a rouge by default!")
                  {:inventory #{"torch"}
                   :hp 18
                   :abnormal 0
                   :ability 1
                   :vocation 1
                   :seen #{}}
          )
      )
    )
  )
)

(defn new-game []
   (let [slime1 (rand-unique maze-size #{0})
         slime2 (rand-unique maze-size #{slime1 0})
         goblin1 (rand-unique maze-size #{slime1 slime2 0})
         goblin2 (rand-unique maze-size #{goblin1 slime1 slime2 0})
         trap (rand-unique maze-size #{goblin1 slime1 slime2 goblin2 0})
         adv (rand-unique maze-size #{goblin1 slime1 slime2 goblin2 trap 0})
         key (rand-unique maze-size #{goblin1 slime1 slime2 goblin2 trap adv 0})
         treasure (rand-unique maze-size #{goblin1 slime1 slime2 goblin2 trap adv key 0})
         medicine (rand-unique maze-size #{goblin1 slime1 slime2 goblin2 trap adv key treasure 0})
         ]
       {:slime1 slime1 :slime2 slime2
        :goblin1 goblin1 :goblin2 goblin2
        :trap trap :key key
        :medicine medicine
        :adventurer adv :treasure treasure
        :chest :locked
        :status (init-adventurer)
        })
)


;remove from the inventory
(defn removeinven [string state]
  (assoc state :status (assoc (:status state) :inventory (into #{} (remove #{string} (:inventory (:status state))))))
)

;check if has such item
(defn ifhas [item state]
  (if (contains? (:inventory (:status state)) item) true
      false
  )
)




;add to the inventory
(defn addinven [string state]
   (assoc state :status (assoc (:status state) :inventory (conj (:inventory (:status state)) string)))
)

(defn addseen [num state]
  (assoc state :status (assoc (:status state) :seen (conj (:seen (:status state)) num)))
)

;hp--
(defn hpminus [num state]
  (assoc state :status (assoc (:status state) :hp (- (:hp (:status state)) num)))
)


;torch
(defn lighttorch [state]
  (do (println "You have just used the torch, now you can examine a room")
      (println "Which room do you want to examine? Please enter a number")
      (let [loc (read-line)]
        (let [newstate (removeinven "torch" state)]
          (cond (or (= loc (:slime1 state)) (= loc (:slime2 state))) (do (println "There is a slime there!") newstate)
              (or (= loc (:goblin1 state)) (= loc (:goblin2 state))) (do (println "There is a goblin there!") newstate)
              (= loc (:trap state)) (do (println "There is a trap here!") newstate)
              (= loc (:treasure state)) (do (println "There is a chest here!") newstate)
              (= loc (:key state)) (do (println "There is a key here!") newstate)
              (= loc (:medicine state)) (do (println "There is a medicine here!") newstate)
              :else (do (println "This room is not special") newstate)
          )
        )
      )
  )
)



;medicine
(defn takemedicine [state]
  (do (println "You have used the medicine")
      (println "You healed yourself")
      (let [newstate (removeinven "medicine" state)]
        (hpminus -5 newstate)
      )
  )
)

(defn pickkey [state]
  (do (println "Do you want take up the key? ([Y]es/[N]o)")
      (let [ifpick (read-line)]
        (if (= ifpick "Y")
          (do (println "You took up the key!")
            (assoc (addinven "key" state) :key 0)
          )
          (do (println "You choose not to take up the key.")
            state
          )
        )
      )
  )
)

(defn pickmedicine [state]
  (do (println "Do you want take up the medicine? ([Y]es/[N]o)")
      (let [ifpick (read-line)]
        (if (= ifpick "Y")
          (do (println "You took up the medicine!")
            (assoc (addinven "medicine" state) :medicine 0)
          )
          (do (println "You choose not to take up the medicine.")
            state
          )
        )
      )
  )
)


;inven: print inventory, and can use or drop.
(defn inven [state]
  (do (println "Your inventory:")
      (apply println (:inventory (:status state)))
      (loop [state state]
        (do (println "What do you want to do with your inventory? ([U]se/[D]rop/[E]xamine/[N]othing)")
          (let [choice (read-line)]
            (cond (= choice "U")
              (do (println "Which item do you want to use? (Please enter the full name of the item)" )
                (let [chouse (read-line)]
                  (if (ifhas chouse state)
                    (cond (= chouse "torch") (recur (lighttorch state))
                          (= chouse "medicine") (recur (takemedicine state))
                          (= chouse "key")
                            (do (println "The key shines, and a number appears on the key!")
                                (println (str "The number is " (:treasure state)))
                                (recur state)
                            )
                    )
                    (do (println "There is no such item in your inventory!")
                    (recur state))
                  )
                )
              )
                  (= choice "D")
                    (do (println "Which item do you want to drop? (Please enter the full name of the item)")
                        (println "Warning: If you drop the torch, it will disappear!")
                        (println "But if you drop the medicine, it can remove the abnormal status.")
                        (let [chodrop (read-line)]
                          (if (ifhas chodrop state)
                            (cond (= chodrop "torch")
                                   (do (println "You have dropped the torch.")
                                       (recur (removeinven "torch" state)))
                                 (= chodrop "medicine")
                                   (do (println "You have dropped the medicine.")
                                       (println "The evaporated medicine gives a nice smell and detoxifies you. The abnormal status is removed.")
                                       (recur (removeinven "medicine" (assoc state :status (assoc (:status state) :abnormal 0)))))
                                 (= chodrop "key")
                                   (do (println "You can't drop the key!")
                                   (recur state))
                            )
                            (do (println "There is no such item in your inventory!")
                            (recur state))
                          )
                        )
                    )
                (= choice "E")
                  (do (println "Which item do you want to examine? (Please enter the full name of the item)")
                    (let [choexa (read-line)]
                      (if (ifhas choexa state)
                        (cond (= choexa "torch")
                                (do (println "It's a torch.")
                                    (println "You can use it to examine a room in the maze.")
                                    (println "The room you examine do not have to be the room nearby.")
                                    (recur state))
                              (= choexa "medicine")
                                (do (println "Medicine is a good thing!")
                                    (println "If you use it, your hp will rise by 5")
                                    (println "If you drop it, the abnormal status will be moved.")
                                    (recur state))
                              (= choexa "key")
                                (do (println "It's the key for the chest")
                                    (println "If you use the key, you will know the location of the chest")
                                    (recur state))
                        )
                        (do (println "There is no such item in your inventory!")
                        (recur state))
                      )
                    )
                  )

                (= choice "N")
                  state
                :else (do (println "Invalid Input!")
                          (println "Please type 'U', 'D', 'E' or 'N'")
                          (recur state))
            )
          )
        )
      )

    )
)

;losing hp caused by traps
(defn jiesuan-toxic [state]
  (if (> (:abnormal (:status state)) 0)
    (do (println "You lose 1 hp due to the trap!")
      (let [newsta (hpminus 1 state)]
                   (assoc newsta :status (assoc (:status newsta) :abnormal (dec (:abnormal (:status newsta)))))
      )
    )

    state
  )
)

;chest
(defn check-chest [state]
  (do (println "You find the chest. That's your final target!")
      (println "You try to open the chest.")
      (if (ifhas "key" state)
        (do (println "You have the key!")
            (println "You use the key and open the chest.")
            (println "You find the treasure!")
            (assoc (hpminus 100 state) :chest :unlocked)
        )
        (do (println "You do not have the key!")
            (println "You need to find the key first!")
            state
        )
      )
  )
)

(defn kaishi-trap [state]
  (if (and (= (:vocation (:status state)) 0) (> (:ability (:status state)) 0))
    (do (println "There is a trap here. You used your ability to avoid the damage of trap!")
        (assoc state :status (assoc (:status state) :ability (dec (:ability (:status state)))))
    )
    (do (println "Ouch! You have just walked over a trap.")
        (assoc state :status (assoc (:status state) :abnormal 10))
    )
  )
)


;losing hp by slime
(defn jiesuan-attack-slime [state]
  (if (and (= (:vocation (:status state)) 1) (> (:ability (:status state)) 0))
    (do (println "You meet a slime!")
        (println "Since you are warriors, you can avoid the attack of slime only once")
        (println "Do your want to use the ability? ([Y]es/[N]o)")
        (let [choice (read-line)]
          (if (= choice "Y")
            (do (println "You used your ability to avoid the attack of slime!")
                (assoc state :status (assoc (:status state) :ability (dec (:ability (:status state)))))
            )
            (do (println "You meet a slime!")
                (println "After fight, you lose 4 hp!")
                (hpminus 4 state)
            )
          )
        )
    )
    (do (println "You meet a slime!")
        (println "After fight, you lose 4 hp!")
        (hpminus 4 state)
    )
  )
)

;losing hp by goblin
(defn jiesuan-attack-goblin [state]
  (if (and (= (:vocation (:status state)) 1) (> (:ability (:status state)) 0))
    (do (println "You meet a goblin!")
        (println "Since you are warriors, you can avoid the attack of goblin only once")
        (println "Do your want to use the ability? ([Y]es/[N]o)")
        (let [choice (read-line)]
          (if (= choice "Y")
            (do (println "You used your ability to avoid the attack of slime!")
                (assoc state :status (assoc (:status state) :ability (dec (:ability (:status state)))))
            )
            (do (println "You meet a goblin!")
                (println "After fight, you lose 6 hp!")
                (hpminus 6 state)
            )
          )
        )
    )
    (do (println "You meet a goblin!")
        (println "After fight, you lose 6 hp!")
        (hpminus 6 state)
    )
  )
)

(defn what-happens [state]
  (let [loc (state :adventurer)]
    (cond (= (:slime1 state) loc) (jiesuan-attack-slime (assoc state :slime1 0))
          (= (:slime2 state) loc) (jiesuan-attack-slime (assoc state :slime2 0))
          (= (:goblin1 state) loc) (jiesuan-attack-goblin (assoc state :goblin1 0))
          (= (:goblin2 state) loc) (jiesuan-attack-goblin (assoc state :goblin2 0))
          (= (:trap state) loc) (kaishi-trap (assoc state :trap 0))
          (= (:treasure state) loc) (check-chest state)
          (= (:key state) loc) (pickkey state)
          (= (:medicine state) loc) (pickmedicine state)
          :else state
    )
  )
)

(defn vector-has [v elt]
  (some #{elt} v))

(defn repl [state]
    (loop [state state]
        (if (> (:hp (:status state)) 0)
            (do
               (println "You are in room" (state :adventurer))
               (println "The rooms that are connected are" (-> :adventurer state maze))
               (println "What do you want to do? ([M]ove/[Q]uit/[S]tatus) ")
               (let [choice (read-line)]
                  (cond (= choice "M")
                      (do (println "Which room do you want to go? (Please enter the room number)")
                          (let [room (try
                                        (read-string (read-line))
                                        (catch Exception e "Please enter the room number"))]
                              (if (vector-has (-> :adventurer state maze) room)
                                  (recur (jiesuan-toxic (what-happens (addseen room (assoc state :adventurer room)))))
                                  (do (println "You can't go there.")
                                      (recur state))
                              )
                          )
                      )
                        (= choice "Q")
                          (println "Thanks for playing!")
                        (= choice "S")
                          (do (println (str "Your hp is " (:hp (:status state)) ))
                                (if (= (:ability (:status state)) 0)
                                  (println "You have used your ability.")
                                  (println "Your ability is available.")
                                )
                                (println "You have visited those rooms:")
                                (apply println (:seen (:status state)))
                              (recur (inven state)))
                        :else (do (println "Invalid Input")
                                  (println "Please type 'M', 'Q' or 'S'")
                          (recur state))
                  )
               )
            )
            (if (= (:chest state) :unlocked)
              (println "You win!")
              (println "Your hp is 0. You lose!")
            )
        )
    )
)

; can use (declare checkgoeast)
(defn -main
  "trying to do something"
  [& args]
  (repl (new-game)))
