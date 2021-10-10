`(:spec
  (nk:with-styles context
      ((:item nk:+style-window-fixed-background+ (nk:style-item-color (nk:rgba 0 0 0 192)))
       (:color nk:+style-text-color+ (nk:rgba 255 255 255 255))
       (:font 0 font))
    (with-system-config-options ((display-width display-height))
      (nk:with-window context "Credits" (nk:recti 0 0 display-width display-height) nil
        (let ((label-flags (nk:flags 'nk:text-alignment :+text-left+)))
          (dolist (line text)
            (nk:layout-row-dynamic context 21.0 1)
            (nk:label context line label-flags)))
        (unless (zerop (nk:input-has-mouse-click context :+button-left+))
          (toggle-ui entity)
          (toggle-ui (main-menu))))))
  :font
  ,(ensure-loaded #'nk:allegro-font-create-from-file "fonts/noto-mono.ttf" 14 0)
  :text
  ,(read-file-into-list "texts/credits.txt"))
