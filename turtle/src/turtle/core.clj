(ns assignment3.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.java.io :as io]))


;Defining global and atoms required for the program
(def message (atom "Step Mode"))
(def sub-text (atom ""))
(def blue  [13 18 255])
(def command-history (atom []))
(def file-pointer (atom 0))
(def data-file (io/file(io/resource "turtle.txt" )))

; Defining turtle for the program
(defn new-turtle
  []
  (atom {:x 0
         :y 0
         :angle 0
         :pen true}))

(def turtle (new-turtle))

(def lines (atom []))

(defn alter-turtle
  [turt f]
  (swap! turt f)
  turt)

(defn reset-pointer
  []
  (reset! sub-text "You are at the start of program")
  (reset! file-pointer 0))

;This operation turns the direction of the turtle the given amount in degrees.
(defn turn
  ([ang]
     (turn turtle ang))
  ([turt ang]
     (letfn [(add-angle
               [{:keys [angle] :as t}]
               (let [new-angle (-> angle
                                   (- ang)
                                   (mod 360))]
                 (assoc t :angle new-angle)))]
       (alter-turtle turt add-angle))))

(defn undo-turn
  ([ang]
     (undo-turn turtle ang))
  ([turt ang]
     (letfn [(add-angle
               [{:keys [angle] :as t}]
               (let [new-angle (-> angle
                                   (+ ang)
                                   (mod 360))
                     old-angle (get @turtle :angle)]
                 (assoc t :angle new-angle)))]
       (alter-turtle turt add-angle))))


(defn translate
  [{:keys [x y pen] :as t} dx dy]
  (let [new-x (+ x dx)
        new-y (+ y dy)
        line [[x y] [new-x new-y]]]
    (when (and pen
               (not= [x y] [new-x new-y]))
      (swap! lines conj line))
    (assoc t
      :x new-x
      :y new-y)))

(defn undo-translate
  [{:keys [x y pen] :as t} dx dy]
  (let [new-x (- x dx)
        new-y (- y dy)
        line [[x y] [new-x new-y]]]
    (when (and pen
               (not= [x y] [new-x new-y]))
      (swap! lines pop))
    (assoc t
      :x new-x
      :y new-y)))

;Converts degree to radians
(def deg->radians q/radians)

;converts radians to degree
(def radians->deg q/degrees)

(def atan q/atan)

;This operation moves the turtle given amount in the current direction
(defn move
  ([len]
     (move turtle len))
  ([turt len]
     (let [rads (deg->radians (get @turt :angle))
           dx (* len (Math/cos rads))
           dy (* len (Math/sin rads))
           alter-fn #(translate % dx dy)]
       (alter-turtle turt alter-fn))))


;When the pen is up and the turtle moves nothing is drawn
(defn penup
  ([]
     (penup turtle))
  ([turt]
     (letfn [(alter-fn [t] (assoc t :pen false))]
       (alter-turtle turt alter-fn))))

;When the pen is down and the turtle moves a black line is drawn
(defn pendown
  ([]
     (pendown turtle))
  ([turt]
     (letfn [(alter-fn [t] (assoc t :pen true))]
       (alter-turtle turt alter-fn))))

(defn draw-turtle
  ([]
     (draw-turtle turtle))
  ([turt]
         (let [lines (partition 2 1 [[0 0] [0 0]])]
           (dorun
            (map (fn [line] (apply q/line (flatten line))) lines)))))


(defn get-lines []
  (clojure.string/split-lines (slurp data-file)))

(defn save-command
  [command value]
  (swap! command-history conj [command value]))

;This is to convert the data read from turtle.txt file to function calls
(defn func-call
  [command value]
  (reset! sub-text (clojure.string/join [command " " value]))
  (save-command command value)
  (if (empty? value)
   ((ns-resolve 'assignment3.core(symbol command)))
    ((ns-resolve 'assignment3.core(symbol command)) (Integer/parseInt value))))

;Called when user presses r key i.e Run Mode
(defn run-command
  []
  (reset! message (str "Run Mode"))
  (loop [data (get-lines) index @file-pointer]
   (when (< index (count (get-lines)))
   (func-call (get (vec (.split (get (get-lines) index) "\\s+"))0) (get (vec (.split (get (get-lines) index) "\\s+")) 1))
   (recur (next data) (swap! file-pointer inc))))
   (reset! sub-text "At end of the program"))


;Called when user switches to step mode.
(defn step-command
  []
  (reset! message "Step Mode")
  (if (< @file-pointer (count (get-lines)))
  (let [file-data (vec (.split (get (get-lines) @file-pointer) "\\s+"))
        com (get file-data 0)
        value (get file-data 1)]
  (swap! file-pointer inc)
  (reset! sub-text (clojure.string/join [com " " value]))
  (func-call com value))))

(defn undo-move
  ([len]
    (undo-move turtle len))
  ([turt len]
     (let [rads (deg->radians (get @turt :angle))
           dx (* len (Math/cos rads))
           dy (* len (Math/sin rads))
           alter-fn #(undo-translate % dx dy)]
       (alter-turtle turt alter-fn))))

(defn reset-rendering
  []
  (.clear (q/current-graphics))
  (q/background 200)                 ;; Set the background colour to
                                     ;; a nice shade of grey.
    (q/stroke-weight 1))

;Function to undo the lines drawn
(defn remove-command
  [comm]
  (swap! command-history pop)
  (let [command (get comm 0)
        value (get comm 1)]
  (reset! sub-text (clojure.string/join ["undo " command " " value]))
  (cond (= command "move") (undo-move (Integer/parseInt value))
        (= command "turn")(undo-turn (Integer/parseInt value))
        (= command "pendown")(penup)
        (= command "penup") (pendown))))

(defn undo-command
  []
   (reset! message "Step Mode")
   (let [last-command (last @command-history)]
   (if (not= @file-pointer 0) (swap! file-pointer dec))
   (if (= (count @command-history) 0)
   (reset-pointer)
   (remove-command last-command))))

(defn unknown-command
  []
  (reset! sub-text "program not available")
  (q/redraw))

(defn keyboard-action
  []
  (let [key (q/key-as-keyword)]
    (reset! sub-text (str key "key pressed"))
    (cond (= key :r)(run-command)
          (= key :right)(step-command)
          (= key :left)(undo-command)
          :else (unknown-command))))

(defn setup []
  (q/smooth)                          ;; Turn on anti-aliasing
  (q/frame-rate 1)
  (q/text-font (q/create-font "DejaVu Sans" 16 true))
  (reset-rendering));; Set framerate to 1 FPS

(defn draw []
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (reset-rendering)
    (q/push-matrix)
    (q/apply-matrix 1  0 0
                    0  1 0)
    (apply q/fill blue)
    (q/scale 1)
    (q/text @message -220 -200)
    (q/fill 0)
    (q/text @sub-text -200 -180)
    (q/stroke-weight 1)
    (q/apply-matrix 1  0 0
                    0  -1 0)
    (doseq [l @lines]
      (let [[[x1 y1] [x2 y2]] l]
        (q/line x1 y1 x2 y2)))
    (draw-turtle)
    (q/pop-matrix)))


(defn start-turtle
  []
  (q/defsketch assignment3              ;; Define a new sketch named assignment3
    :title "Turtle"       ;; Set the title of the sketch
    :setup setup                        ;; Specify the setup fn
    :draw  draw
    :key-pressed keyboard-action        ;; Specify the draw fn
    :size [500 500]
    :features [:keep-on-top]))

(start-turtle)



