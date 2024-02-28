(require cKanren/miniKanren)
(require cKanren/neq)
;;  caro cdro conso nullo eqo
;;  pairo membero rembero appendo
;;  flatteno anyo nevero alwayso
;;  build-num poso >1o full-addero
;;  addero gen-addero pluso minuso
;;  *o odd-*o bound-*o =lo <lo <=lo
;;  <o <=o /o splito logo exp2
;;  repeated-mul expo prnt zeroo

(define (lengtho l out)
    (conde
     [(nullo l) (== '() out)]
     [(fresh (d res)
             (cdro l d)
             (pluso '(1) res out)
             (lengtho d res))]))

;; a alkane is a list of substituent pairs
;; a substituent is a list of substituent substituent
;; C2H6 -> '((() . ()) (() . ()))
;; CH(CH3)3 ->  '((() . ()) (() . (((() . ())))) (() . ()))
(define (alkaneo l main-length avail-carbon)
  (fresh (calc-length)
         (minuso main-length '(1) calc-length)
         (palkaneo l calc-length main-length avail-carbon)))

(define (palkaneo l main-length avail-length avail-carbon) ; main-length 比实际长度小 1，便于计算
  (conde
   [(nullo l) (nullo avail-length) (nullo avail-carbon)]      ; 到达末端，此时剩余碳为 0
   [(fresh (remain-length pos rel-length remain-carbon sub-carbon-count sub-alkanes d)
           (=/= '() avail-carbon)
           (=/= '() avail-length)
           (minuso avail-length '(1) remain-length) ; 当前剩余碳链(不包括自身)长度
           (== `(,sub-alkanes . ,d) l)
           (minuso main-length remain-length pos) ; 当前碳位置
           (conde                                 ; 当前碳到两端的最小长度
            [(<=o pos remain-length) (== rel-length pos)]
            [(<o remain-length pos) (== rel-length remain-length)])
           (sub2-alkaneo sub-alkanes rel-length sub-carbon-count) ; 支链的碳数+1(当前碳)
           (minuso avail-carbon sub-carbon-count remain-carbon)   ; 剩余碳数
           (palkaneo d main-length remain-length remain-carbon)
           )]))

(define (sub2-alkaneo sub-alkanes rel-length carbon-count)
  (fresh (a1 a2 carbon1 carbon2 carbons)
         (== `(,a1 . ,a2) sub-alkanes)
         (sub-alkaneo a1 rel-length carbon1)
         (sub-alkaneo a2 rel-length carbon2)
         (pluso carbon1 carbon2 carbons)
         (pluso carbons '(1) carbon-count)))

; one layer of recursion mising
(define (sub-alkaneo l rel-length carbon-count)
  (fresh (len)
         (lengtho l len)
         (== carbon-count len)
         (<=o len rel-length)           ; 支链长度小于等于当前碳到端长度
         ))

