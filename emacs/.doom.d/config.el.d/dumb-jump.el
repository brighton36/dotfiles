;-*- mode: elisp -*-

; TODO: let's move this into an el file...
;; Dumb Jump ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

; TODO: This ... stopped working when we upgraded to emacs 29. And, I'm not sure if we still want it
;(global-set-key
;	(kbd "C-j")
;	(defhydra dumb-jump-hydra (:color blue :columns 3)
;		"Dumb Jump"
;		("j" dumb-jump-go "Go")
;		("o" dumb-jump-go-other-window "Other window")
;		("e" dumb-jump-go-prefer-external "Go external")
;		("x" dumb-jump-go-prefer-external-other-window "Go external other window")
;		("i" dumb-jump-go-prompt "Prompt")
;		("l" dumb-jump-quick-look "Quick look")
;		("b" dumb-jump-back "Back"))
;)

