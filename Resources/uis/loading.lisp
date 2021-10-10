`(:spec
  (nk:with-color (text-color :r 215 :g 217 :b 221 :a 255)
    (nk:with-styles context
        ((:item nk:+style-window-fixed-background+
                (nk:style-item-color (nk:rgba 0 0 0 192)))
         (:color nk:+style-text-color+ text-color))
      (with-system-config-options ((display-width display-height))
        (nk:with-window context "Loading screen" (nk:recti 0 0 display-width display-height) nil
          (nk:with-layout-space context :+dynamic+ 54 1 
            (nk:layout-space-push context (nk:rect 0.28 6f0 0.45 1.0))
            (nk:with-styles context
                ((:item nk:+style-progress-normal+ (nk:style-item-image progress-normal-image))
                 (:item nk:+style-progress-cursor-normal+ (nk:style-item-image progress-cursor-image))
                 (:vec2 nk:+style-progress-padding+ (nk:vec-2i 0 0)))
              (nk:prog- context (truncate (* progress 100)) 100 0))
            (nk:label context (format nil " Loading ~a ..." message)
                      (nk:flags 'nk:text-alignment :+text-left+)))))))
  :progress 0d0
  :message ""
  :progress-normal-image
  ,(ensure-loaded #'nk:allegro-create-image "images/progress-empty.png")
  :progress-cursor-image
  ,(ensure-loaded #'nk:allegro-create-image "images/progress-full.png"))
