{include/i_dbvers.i}
/*******************************************************************************
**
**   INAPI402.i: Prepara chamada da MPAPI002
**               DEFINI€ÇO DA TEMP-TABLE TT-MAR-RAT-MED-INV
**
*********************************************************************************/

DEF TEMP-TABLE tt-mat-rat-med-inv NO-UNDO                                         
    FIELD nr-contrato       AS INT  FORMAT ">>>>>>>>9"
    FIELD num-seq-item      AS INT  FORMAT ">,>>9"
    FIELD numero-ordem      AS INT  FORMAT ">>>>>9,99"
    FIELD num-seq-event     AS INT  FORMAT ">,>>9"
    FIELD num-seq-medicao   AS INT  FORMAT ">,>>9"
    FIELD conta-contabil    AS CHAR FORMAT "x(17)"
    FIELD ct-codigo         AS CHAR FORMAT "x(8)"
    FIELD sc-codigo         AS CHAR FORMAT "x(8)"
&IF "{&mguni_version}" >= "2.071" &THEN
    FIELD ep-codigo         LIKE empresa.ep-codigo
&ELSE
    FIELD ep-codigo         AS INT  FORMAT ">>9"
&ENDIF
    FIELD num-ord-inv       AS INT  FORMAT ">>>>>,>>>"
    FIELD perc-rateio       AS DEC  FORMAT ">>9.99"
    FIELD seq-comp          AS INT  FORMAT ">>9"
    FIELD ind-sit-medicao   AS INT  FORMAT "9"
    FIELD dat-prev-medicao  AS DATE FORMAT "99/99/9999"
    FIELD observacao        AS CHAR FORMAT "x(2000)"
    FIELD narrat-item       AS CHAR FORMAT "x(2000)"
    FIELD cod-est-exec      AS CHAR FORMAT "x(3)"
    FIELD num-projeto       AS INT  FORMAT ">>>>9"
    FIELD num-ordem         AS INT  FORMAT ">>9"
    FIELD num-secao         AS INT  FORMAT "9"
    FIELD cod-especialidade AS INT  FORMAT ">9"
    FIELD cod-sub-espec     AS INT  FORMAT ">9"
    FIELD cod-origem        AS INT  FORMAT "9"
    INDEX ch-mat-rat-med-inv IS PRIMARY
          nr-contrato
          num-seq-item
          numero-ordem
          num-seq-event
          num-seq-medicao
          conta-contabil
          num-ord-inv.
