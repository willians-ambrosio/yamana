/************************************************************************
**
**  PROGRAMA - escd0901
**
**  OBJETIVO - Mostrar a carteira e nosso numero de bancos na previstos
**
**  AUTOR    - Gilberto Rissati Garcia (Datasul)
**
**  DATA     - 18/10/2002
**
*************************************************************************/

DEFINE INPUT  PARAMETER  h_botao                     AS WIDGET-HANDLE NO-UNDO. 
DEFINE INPUT  PARAMETER  p-wgh-frame                 AS WIDGET-HANDLE NO-UNDO.

DEFINE VARIABLE h_frame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE h_campo AS WIDGET-HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE up-fp3000b_wh_des_msg_envel_fp AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE b02py040_rowid                 AS ROWID NO-UNDO.

IF h_botao:NAME = "bt_alt_msg" THEN DO:
   RUN fpp/esfp3000b.w (INPUT b02py040_rowid).
END.
