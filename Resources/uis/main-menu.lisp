`(:spec
  (nk:with-color (button-text-color :r 215 :g 217 :b 221 :a 255)
    (nk:with-styles context
        ((:item nk:+style-window-fixed-background+
                (if (game-started-p)
                    (nk:style-item-color (nk:rgba 0 0 0 192))
                    (nk:style-item-image background-image)))
         (:item nk:+style-button-normal+ (nk:style-item-image button-normal-image))
         (:item nk:+style-button-hover+ (nk:style-item-image button-hover-image))
         (:item nk:+style-button-active+ (nk:style-item-image button-active-image))
         (:color nk:+style-button-text-normal+ button-text-color)
         (:color nk:+style-button-text-hover+ button-text-color)
         (:color nk:+style-button-text-active+ button-text-color))
      (with-system-config-options ((display-width display-height))
        (nk:with-window context "Main menu" (nk:recti 0 0 display-width display-height) nil
          (nk:with-layout-space context :+dynamic+ 36 1
            (nk:layout-space-push context (nk:rect 0.34 8f0 0.333 0.84))
            (when (game-started-p)
              (nk:with-button-label context "Continue"
                (make-button-press-sound entity)
                (toggle-ui entity)))
            (nk:with-button-label context "New game"
              (make-button-press-sound entity)
              (toggle-ui entity)
              (new-game))
            (nk:with-button-label context "Credits"
              (make-button-press-sound entity)
              (toggle-ui entity)
              (toggle-ui (credits-screen) t))
            (nk:with-button-label context "Exit"
              (make-button-press-sound entity)
              (toggle-ui entity)
              (exit)))))))
  :background-image
  ,(ensure-loaded #'nk:allegro-create-image "images/background.png")
  :button-normal-image
  ,(ensure-loaded #'nk:allegro-create-image "images/button-normal.png")
  :button-hover-image
  ,(ensure-loaded #'nk:allegro-create-image "images/button-hover.png")
  :button-active-image
  ,(ensure-loaded #'nk:allegro-create-image "images/button-active.png"))
