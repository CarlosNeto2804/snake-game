
(ns snake.core
  (:import (java.awt Color Dimension Font)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener KeyEvent))
  (:gen-class))

;;; constants

(def c-width   "number of horizontal elements" 20)
(def c-height  "number of vertical elements"   20)
(def e-size    "size of an element in pixels"               14)
(def i-quantum "initial duration of repainting period"      100)
(def d-quantum "change of the duration for two succ levels" -5)
(def m-quantum "limit for the duration"                     50)
(def i-length  "initial length for the snake to win"        5)
(def d-length  "change of the length for two succ levels"   3)
(def p-width   "width of the game panel"   (* c-width e-size))
(def p-height  "height of the game panel" (* c-height e-size))
(def dirs      "mapping from even code to direction"
  {KeyEvent/VK_LEFT  [-1  0]
   KeyEvent/VK_RIGHT [ 1  0]
   KeyEvent/VK_UP    [ 0 -1]
   KeyEvent/VK_DOWN  [ 0  1]})

;;; colors

(def color-variation  35)
(def bright-sum       350)
(def bright-diff      250)
(def background-color (Color/WHITE))
(def text-color       (Color/DARK_GRAY))


(defn quantum [level]
  (max (+ i-quantum (* level d-quantum)) m-quantum))

(defn length [level]
  (+ i-length (* level d-length)))

(def length (memoize length))

(defn sum-points 
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(defn move [{:keys [body dir] :as snake} grows]
  (assoc snake :body
         (cons (sum-points (first body) dir)
               (if grows body (butlast body)))))

(defn turn [snake dir]
  (assoc snake :dir dir))

(defn snake-win? [{body :body} level]
  (>= (count body) (length level)))

(defn eats-self? [[head & tail]]
  (contains? (set tail) head))

(defn snake-touch-in-border? [[[x y]]]
  (or (>= x c-width)
      (>= y c-height)
      (< x 0)
      (< y 0)))

(defn snake-lose? [{body :body}]
  "Check if snake lose game"
  (or (eats-self?   body)
      (snake-touch-in-border? body)))

(defn eats-apple? [{[head] :body} {[apple] :body}]
  (= head apple))

(defn screen-rect [[x y]]
  "Converts a pair of coordinates into x, y, width, and height of a
  rectangle on the screen."
  (map (fn [x] (* e-size x))
       [x y 1 1]))

(def screen-rect (memoize screen-rect))


(defn ->color [[r g b]]
  (Color. r g b))

(defn generate-bright-color []
  (->> (repeatedly #(rand-int 256))
       (partition 3 1)
       (some #(when (= (apply + %) bright-sum) %))))

(defn contrast? [x y]
  (letfn [(diff [x y] (Math/abs (- x y)))]
    (if (>= (apply + (map diff x y))
            bright-diff)
      y nil)))

(defn generate-contrast-color [color]
  (->> (repeatedly generate-bright-color)
       (some (partial contrast? color))))

(defn vary-component [x]
  (letfn [(pm [x] [(rand-int x) (rand-int (- x))])]
    (let [x (apply + x (pm color-variation))]
      (cond (> x 255) 255
            (< x 0)   0
            :else     x))))

(defn vary-color [color]
  (->color (map vary-component color)))

(defn create-new-snake []
  {:body  (list [1 1])
   :dir   [1 0]
   :color (generate-bright-color)})

(defn create-new-apple-for [{color :color}]
  {:body  [[(rand-int c-width)
            (rand-int c-height)]]
   :color (generate-contrast-color color)})

;;; non-pure section

(defn restart-game [snake apple pause]
  (dosync
   (ref-set snake (create-new-snake))
   (ref-set apple (create-new-apple-for @snake))
   (ref-set pause true))
  nil)

(defn update-direction-snake [snake dir]
  (when dir
    (dosync (alter snake turn dir))))

(defn update-position-elements [snake apple]
  (dosync
   (if (eats-apple? @snake @apple)
     (do (ref-set apple (create-new-apple-for @snake))
         (alter   snake move true))
     (alter snake move false)))
  nil)


(defn draw-components [g {:keys [body color]}]
  (doseq [[x y w h] (map screen-rect body)]
    (doto g
      (.setColor (vary-color color))
      (.fillRect x y w h))))

(defn show-text [g title subtitle]
  "Shows some text: title and subtitle."
  (doto g
    (.setColor text-color)
    (.setFont (Font. "Tahoma" Font/TRUETYPE_FONT 30))
    (.drawString title 80 120)
    (.setFont (Font. "Tahoma" Font/TRUETYPE_FONT 12))
    (.drawString subtitle 60 150)))

(defn create-game-panel [snake apple level pause timer]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (if @pause
        (show-text g (str "Nível " @level)
                   "Precione qualquer tecla...")
        (do (draw-components g @snake)
            (draw-components g @apple))))
    (actionPerformed [e]
      (when-not @pause
        (update-position-elements snake apple))
      (when (snake-lose? @snake)
        (restart-game snake apple pause))
      (when (snake-win? @snake @level)
        (swap! level inc)
        (restart-game snake apple pause)
        (.setDelay timer (quantum @level)))
      (.repaint this))
    (keyPressed [e]
      (if @pause
        (dosync (ref-set pause false))
        (update-direction-snake snake (dirs (.getKeyCode e)))))
    (windowClosed []
      (System/exit 0))
    (keyReleased [e])
    (keyTyped    [e])))

(defn start-game []
  (let [snake  (ref (create-new-snake))
        apple  (ref (create-new-apple-for @snake))
        level  (atom 0)
        pause  (ref true)
        frame  (JFrame. "Simple Snake Game")
        timer  (Timer. (quantum @level) nil)
        panel  (create-game-panel snake apple level pause timer)]
    (doto panel
      (.setFocusable   true)
      (.addKeyListener panel)
      (.setBackground  background-color)
      (.setPreferredSize (Dimension. p-width p-height)))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true)
      (.setResizable false)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setLocationRelativeTo nil))
    (doto timer
      (.addActionListener panel)
      (.start))
    [snake apple level timer]))

(defn -main [& args]
  (start-game))
