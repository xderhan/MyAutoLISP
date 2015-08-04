;Selection set of line lengths are totaled and displayed.
;--------------------------------------------------
(defun GetArcLength (END_ANG START_ANG ARC_RAD / TOTAL_ANG)
   (setq TOTAL_ANG (- END_ANG START_ANG))
   (while (< TOTAL_ANG 0)
      (setq TOTAL_ANG (+ TOTAL_ANG (* 2 pi)))
   )
   (while (> TOTAL_ANG (* 2 pi))
      (setq TOTAL_ANG (- TOTAL_ANG (* 2 pi)))
   )
   (* (* 2 pi ARC_RAD) (/ TOTAL_ANG (* 2 pi)))
)
;--------------------------------------------------
;get length of lwpolyline or polyline (no bulges)
(defun GET_LWPL ()
  (vl-load-com)
;  (setq OBJ (vlax-ename->vla-object (car (entsel "Select entity: "))))
   (setq OBJ (vlax-ename->vla-object ENT))
  (if (vlax-property-available-p OBJ 'Length)
    (setq LWTLEN (vlax-get obj 'Length))
;    (princ "Entity has no Length property")
     (setq LWTLEN 0.0)
  )
  (setq
        LLEN LWTLEN
        TLEN (+ LLEN TLEN)
  )
) ;end GET_LWPL
;--------------------------
;(defun GET_ENDPTS ()
;  (setq
;        CNUM1       1
;        CAT1_ENTAL (entget ENT)
;        G1G        (nth CNUM1 CAT1_ENTAL)
;        LWP1  nil
;        LWP2  nil
;;        CNUM1 14
;        LWTLEN 0
;  )
;  (if (eq (cdr (assoc 0 CAT1_ENTAL)) "LWPOLYLINE")
;   (progn
;;    (setq
;;          LWP1START (cdr (nth 14 CAT1_ENTAL))
;;    )
;    (while G1G
;       (if (eq (car G1G) 10)
;         (progn
;           (setq
;                 LWP1 (cdr G1G)
;                 LWP1START LWP1
;           )
;           (if LWP2
;             (setq
;                   LWLLEN  (distance LWP1 LWP2)
;                   LWTLEN (+ LWLLEN LWTLEN)
;                   LWP2    LWP1
;                   CNUM1 (+ CNUM1 1)
;                   G1G   (nth CNUM1 CAT1_ENTAL)
;             )
;             (setq
;                   CNUM1 (+ CNUM1 1)
;                   G1G   (nth CNUM1 CAT1_ENTAL)
;                   LWP2  LWP1
;             )
;           )
;         )
;         (setq
;             CNUM1 (+ CNUM1 1)
;             G1G   (nth CNUM1 CAT1_ENTAL)
;         )
;     )
;    );end while loop for LWPOLYLINE
;   )
;   (progn				;else it's a POLYLINE
;      (setq
;            EN2        (entnext ENT)
;            CAT1_ENTAL (entget EN2)
;            LWP1       (cdr (assoc 10 CAT1_ENTAL))	;verify first vertex
;            EN1        EN2
;            EN2        (entnext EN1)
;            CAT1_ENTAL (entget EN2)
;            CNUM1      (+ CNUM1 1)
;            LWP2       (cdr (assoc 10 CAT1_ENTAL))
;      )
;      (while (eq (cdr (assoc 0 cat1_ental)) "VERTEX")   ;then loop and
;        (setq
;              LWP2       (cdr (assoc 10 cat1_ental))	;find last vertex
;              EN1        EN2
;              EN2        (entnext EN1)
;              CAT1_ENTAL (entget EN2)
;              CNUM1       (+ CNUM1 1)
;        )
;      )
;   )
;  ) ; end if
; for closed lwpolylines
;  (if (eq (cdr (assoc 70 CAT1_ENTAL)) 1)
;      (setq LWP1START (cdr (nth 14 CAT1_ENTAL)) LWLLEN (distance LWP2 LWP1START) LWTLEN (+ LWLLEN LWTLEN))
;  )
;
;  (setq
;        LLEN LWTLEN
;        TLEN (+ LLEN TLEN)
;  )
;) ;end GET_ENDPTS
;; --------------------------------------------------------------------------;
;--------------------------------------------------
;--------------------------------------------------
(defun c:ADDLINE () ;( / A B LEN TLEN LLEN )
   (prompt "Select desired LINE and/or ARC entities ->") (terpri)
   (setq SS  (ssget)
         SSZERO (ssadd)
         SSL (sslength SS)
         NUM  0
         TLEN 0
         LLEN 0      
   )
   (repeat SSL
    (setq ENT     (ssname SS NUM)
          ENTAL   (entget ENT)
          ENTTYPE (cdr (assoc 0 ENTAL))
    )
    (cond
	((eq ENTTYPE "LINE")
         (progn
           (GET_LWPL)
           (prompt (strcat "\n" (rtos NUM 2 0) " " ENTTYPE " "
            (rtos LLEN (getvar "lunits") (getvar "luprec"))))
        ))
	((eq ENTTYPE "POLYLINE")
         (progn
           (GET_LWPL)
           (prompt (strcat "\n" (rtos NUM 2 0) " " ENTTYPE " "
            (rtos LLEN (getvar "lunits") (getvar "luprec"))))
        ))
	((eq ENTTYPE "ARC")
         (progn
           ; Get the Arc's dimensional data:
           (setq SA (cdr (assoc 50 ENTAL))		;start angle
                 EA (cdr (assoc 51 ENTAL))		;end angle
                 AR (cdr (assoc 40 ENTAL))		;arc radius
           )
           ; Calculate the ARC's length:
           (setq AL (GetArcLength EA SA AR)		;arc length
                 LLEN AL
                 TLEN (+ LLEN TLEN)
           )
           (if (eq LLEN 0.0) (ssadd ENT SSZERO))
           (prompt (strcat "\n" (rtos NUM 2 0) " " ENTTYPE " "
            (rtos LLEN (getvar "lunits") (getvar "luprec"))))
        ))
        ((eq ENTTYPE "LWPOLYLINE")
          (progn
           (GET_LWPL)
           (prompt (strcat "\n" (rtos NUM 2 0) " " ENTTYPE " "
            (rtos LLEN (getvar "lunits") (getvar "luprec"))))
        ))
        (ENTTYPE
          (progn
           (ssadd ENT SSZERO)
           (prompt (strcat "\n" (rtos NUM 2 0) " " ENTTYPE " " "Length not found."))
          )
        )
    )
    (setq NUM (1+ NUM))
   )
   (setq
          SHOWLEN (strcat "\nTotal length: " (rtos TLEN (getvar "lunits") (getvar "luprec"))
                  " and [" (rtos (sslength SSZERO) 2 0) "] object(s) ignored."
                  )
   )
   (prompt SHOWLEN)
    (princ)
)
;-
