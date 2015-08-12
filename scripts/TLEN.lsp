;|

TLEN.LSP - Total LENgth of selected objects
The result will be automatically copied to clipboard
(c) 1998 Tee Square Graphics

|;

(defun C:TLEN (/ ss tl n ent itm obj l)
  (setq ss (ssget)
        tl 0
        n (1- (sslength ss)))
  (while (>= n 0)
    (setq ent (entget (setq itm (ssname ss n)))
          obj (cdr (assoc 0 ent))
          l (cond
              ((= obj "LINE")
                (distance (cdr (assoc 10 ent))(cdr (assoc 11 ent))))
              ((= obj "ARC")
                (* (cdr (assoc 40 ent))
                   (if (minusp (setq l (- (cdr (assoc 51 ent))
                                          (cdr (assoc 50 ent)))))
                     (+ pi pi l) l)))
              ((or (= obj "CIRCLE")(= obj "SPLINE")(= obj "POLYLINE")
                   (= obj "LWPOLYLINE")(= obj "ELLIPSE"))
                (command "_.area" "_o" itm)
                (getvar "perimeter"))
              (T 0))
          tl (+ tl l)
          n (1- n)))
  (LM:copytoclipboard (rtos tl))
  (prompt (strcat "Total length of selected objects is " (rtos tl)))
  (princ)
)

;; Copy to Clipboard  -  Lee Mac
;; Using the same method as MP demonstrates here: http://bit.ly/170kacW

(defun LM:copytoclipboard ( str / clp htm par )
    (if (setq htm (vlax-create-object "htmlfile"))
        (progn
            (vl-catch-all-apply
               '(lambda ( )
                    (setq par (vlax-get htm 'parentwindow)
                          clp (vlax-get par 'clipboarddata)
                    )
                    (vlax-invoke clp 'setdata "Text" str)
                )
            )
            (foreach obj (list clp par htm)
                (if (= 'vla-object (type obj))
                    (vlax-release-object obj)
                )
            )
            str
        )
    )
)

(vl-load-com) (princ)