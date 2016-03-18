(ns assign4.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [schema.core :as s :include-macros true]))

;single ratom to store the values required for drawing
(def app-state (reagent/atom {:start {:x1 "none" :y1 "none"}
                              :end {:x2 "none" :y2 "none"}
                              :mode "none"
                              :user-steps []
                              :clicked 0}))

;reagent cursors to access small part of the ratom
(def end-location (reagent/cursor app-state [:end :x2]))
(def end-location2 (reagent/cursor app-state [:end :y2]))
(def start-location (reagent/cursor app-state [:start :x1]))
(def start-location2 (reagent/cursor app-state [:start :y1]))
(def user-steps (reagent/cursor app-state [:user-steps]))
(def clicked (reagent/cursor app-state [:clicked]))
(def current-mode (reagent/cursor app-state [:mode]))


;; -------------------------
;; Views


(def ratom-schema
  "schema that validates single ratom"
  { :start {:x1 s/Any :y1 s/Any}
    :end {:x2 s/Any :y2 s/Any}
    :mode s/Str
    :user-steps [{(s/optional-key :x1) s/Int (s/optional-key :y1) s/Int
                  (s/optional-key :x2) s/Int (s/optional-key :y2) s/Int
                  (s/required-key :mode) s/Str (s/required-key :action) s/Str}]
    :clicked s/Int
    })

; method to draw multiple lines, circle and rectangles
(defn line
  [x1 y1 x2 y2]
  [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2}])

(defn circle
  [x1 y1 x2 y2]
  (let [r (.sqrt js/Math (+ (* (- x2 x1)(- x2 x1)) (* (- y2 y1)(- y2 y1))))]
  [:circle {:cx x1 :cy y1 :r r :fill "none"}]))

(defn rectangle
  [x1 y1 x2 y2]
  (let [height (- y1 y2)
        width (- x1 x2)
        x (if (< x1 x2) x1 x2)
        y (if (< y1 y2) y1 y2)]
  [:rect {:x x :y y :width (.abs js/Math width) :height (.abs js/Math height)}]))


;to draw lines, rectangles and circle based on current mode and value
(defn draw-lines
  []
  (if (not= 0 @clicked)
   (if (and (not= "none" @start-location) (not= "none" @start-location2))
   [:line {:x1 @start-location :y1 @start-location2 :x2 @end-location :y2 @end-location2}])))

(defn draw-nothing
  [])

(defn draw-circles
  []
  (if (not= 0 @clicked)
   (if (and (not= "none" @start-location) (not= "none" @start-location2))
      (let [x1 @start-location
            y1 @start-location2
            x2 @end-location
            y2 @end-location2
            r (.sqrt js/Math (+ (* (- x2 x1)(- x2 x1)) (* (- y2 y1)(- y2 y1))))]
      [:circle {:cx x1 :cy y1 :r r :fill "none"}]))))


(defn draw-rectangles
  []
  (if (not= 0 @clicked)
   (if (and (not= "none" @start-location) (not= "none" @start-location2))
   (let [height (- @start-location2 @end-location2)
          width  (- @start-location @end-location)
          x (if (< @end-location @start-location) @end-location @start-location)
          y (if (< @end-location2 @start-location2) @end-location2 @start-location2)]
      [:rect {:x x :y y :width (.abs js/Math width) :height (.abs js/Math height)}]))))


(defn undo-drawing
  []
  (if (not (empty? @user-steps))
    (swap! user-steps pop))
  (if (not (empty? @user-steps))
    (do
    (reset! current-mode (:mode (last @user-steps))))
    (reset! current-mode "none")))


;set the start points after two mouse clicks
(defn reset-start-points
  [x y]
  (reset! start-location x)
  (reset! start-location2 y))

(defn handle-click
  [event]
  (swap! clicked inc)
  (if (= 1 @clicked)
    (reset-start-points (.-clientX event)(.-clientY event)))
  (if (= 2 @clicked)
    (do
      (swap! user-steps conj {:x1 @start-location :y1 @start-location2 :x2 @end-location :y2 @end-location2 :mode @current-mode :action "draw"})
      (reset! end-location "none")
      (reset! end-location2 "none")
      (reset! start-location "none")
      (reset! start-location2 "none")
      (reset! clicked 0))))

(defn draw-area
  []
  [:div
   [:div.button
    [:p (str "Mode " @current-mode )]
     [:ul
      [:li
       [:button.but
         {:on-click
          (fn line-click [e]
            (reset! current-mode "line")
            (swap! user-steps conj {:mode "line" :action "select"}))}
        "Line"]]
      [:li
       [:button.but
         {:on-click
          (fn circle-click [e]
            (reset! current-mode "circle")
            (swap! user-steps conj {:mode "circle" :action "select"}))}
       "Circle"]]
      [:li
       [:button.but
         {:on-click
          (fn rect-click [e]
            (reset! current-mode "rectangle")
            (swap! user-steps conj {:mode "rectangle" :action "select"}))}
       "Rectangle"]]
      [:li
        [:button.but
          {:on-click undo-drawing}
        "Undo"]]]]
   [:div
    {:on-mouse-move
       (fn [event]
        (reset! end-location (.-clientX event))
        (reset! end-location2 (.-clientY event)))
     :on-click
       (fn [event]
         (handle-click event))}
     [:svg
       {:width 600 :height 600 :stroke "black"
        :style {:position :fixed :top 0 :left 0 :border "black solid 1px"}}
        (doall (for [m @user-steps]
            (if (= "draw" (:action m))
             (let [{:keys [x1 y1 x2 y2 mode]} m]
                (case mode
                  "line" (do ^{:key (str x1 (rand) x2 mode)} [line x1 y1 x2 y2])
                  "circle" (do ^{:key (str x1 (rand) x2 mode)} [circle x1 y1 x2 y2])
                  "rectangle" (do ^{:key (str x1 (rand) x2 mode)} [rectangle x1 y1 x2 y2])
                  "default")))))
        (let [mode @current-mode]
           (list (case mode
              "line" ^{:key (str (rand) mode)} [draw-lines]
              "circle" ^{:key (str (rand) mode)} [draw-circles]
              "rectangle" ^{:key (str (rand) mode)} [draw-rectangles]
              "none" ^{:key (str (rand) mode)} [draw-nothing]
               )))]]])


(defn home-page []
  {:pre [(s/validate ratom-schema @app-state)]
   :post [(s/validate ratom-schema @app-state)] }
   [draw-area])

(defn about-page []
  [:div [:h2 "About assign4-test"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!)
  (accountant/dispatch-current!)
  (mount-root))
