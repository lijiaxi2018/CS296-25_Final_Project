(ns adventure.core
  (:gen-class))


; it's a 5*5 square maze.
(def maze {
    1 [2 6] 2 [1 3 7] 3 [2 4 8] 4 [3 5 9] 5 [4 10]
    6 [1 7 11] 7 [2 6 8 12] 8 [3 7 9 13] 9 [4 8 10 14] 10 [5 9 15]
    11 [6 12 16] 12 [7 11 13 17] 13 [8 12 14 18] 14 [9 13 15 19]
    15 [10 14 20] 16 [11 17 21] 17 [12 16 18 22] 18 [13 17 19 23]
    19 [14 18 20 24] 20 [15 19 25] 21 [16 22] 22 [17 21 23] 23 [18 22 24] 24 [19 23 25] 25 [20 24]}) ;created by myself
(def maze-size (count maze)) ;used from wumpus

(defn rand-unique
    "got from wumpus"
    [max exclude]
    (let [pick (rand-int max)]
        (if (exclude pick) (rand-unique max exclude) pick))) ;used from wumpus



(defn init-adventurer []
  (do
    (println "first choose your vocation")
    (println "rouge can avoid the the trappings once")
    (println "warriors can avoid the damge of monster once")
    (println "which vocation will you choose? ([R]ouge/[W]arrior)")
    (let [choice (read-line)]
      (cond (= choice "R")
        {:inventory #{}
         :hp 18
         :abnormal 0
         :skill 1
         :vocation 0
         :seen #{}}
         (= choice "W")
        {:inventory #{}
         :hp 20
         :abnormal 0
         :skill 1
         :vocation 1
         :seen #{}}
      )
    )
  )
)

(defn new-game []
   (let [slime1 (rand-int maze-size)
         slime2 (rand-unique maze-size #{slime1})
         goblin1 (rand-unique maze-size #{slime1 slime2})
         goblin2 (rand-unique maze-size #{goblin1 slime1 slime2})
         trap (rand-unique maze-size #{goblin1 slime1 slime2 goblin2})
         adv (rand-unique maze-size #{goblin1 slime1 slime2 goblin2 trap})
         key (rand-unique maze-size #{goblin1 slime1 slime2 goblin2 trap adv})
         treasure (rand-unique maze-size #{goblin1 slime1 slime2 goblin2 trap adv key})
         ]
       {:slime1 slime1 :slime2 slime2
        :goblin1 goblin1 :goblin2 goblin2
        :trap trap :key key
        :adventurer adv :treasure treasure
        :status (init-adventurer)
        })
)

(defn vector-has [v elt]
  (some #{elt} v))

(defn repl [state]
    (loop [state state]
        (if (> (:hp (:status state)) 0)
            (do
               (println "You are in room" (state :adventurer))
               (println "The rooms that are connected are" (-> :adventurer state maze))
               (println "What do you want to do? ([M]ove/[Q]uit) ")
               (let [choice (read-line)]
                  (cond (= choice "M")
                      (do (println "Which room?")
                          (let [room (read-string (read-line))]
                              (if (vector-has (-> :adventurer state maze) room)
                                  (recur (assoc state :adventurer room))
                                  (do (println "You can't go there.")
                                      (recur state))
                              )
                          )
                      )
                        (= choice "Q")
                          (println "Thanks for playing!")
                  )
               )
            )
            (println "Game over.")
        )
    )
)

; can use (declare checkgoeast)
(defn -main
  "trying to do something"
  [& args]
  (repl (new-game)))
