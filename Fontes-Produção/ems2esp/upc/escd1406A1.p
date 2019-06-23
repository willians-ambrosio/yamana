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

DEFINE NEW GLOBAL SHARED VARIABLE escd1406a1_qt-a-atender      AS DECIMAL FORMAT "->>>,>>>,>>9.99999" NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE up-cd1406a1_it-codigo        LIKE ITEM.it-codigo NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE up-cd1406a1_bt-save          AS WIDGET-HANDLE    NO-UNDO.

DEFINE VARIABLE de-qtidade-atu LIKE saldo-estoq.qtidade-atu NO-UNDO.
DEFINE VARIABLE lg-confirma    AS LOGICAL    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gr-requisicao                AS ROWID NO-UNDO.

DEFINE BUFFER b_requisicao FOR requisicao.

DEFINE BUFFER b_item FOR ITEM.

IF h_botao:NAME = "bt-ok" OR 
   h_botao:NAME = "bt-save" THEN DO:

   FIND FIRST b_requisicao NO-LOCK
        WHERE ROWID(b_requisicao) = gr-requisicao
          AND b_requisicao.tp-requis = 1
          NO-ERROR.
   IF AVAIL b_requisicao THEN DO:
      ASSIGN de-qtidade-atu = 0.
      FOR EACH saldo-estoq NO-LOCK
         WHERE saldo-estoq.it-codigo = up-cd1406a1_it-codigo
           AND saldo-estoq.qtidade-atu > 0:
         ASSIGN de-qtidade-atu = de-qtidade-atu + saldo-estoq.qtidade-atu.
      END.
      /*
      MESSAGE "up-cd1406a1_it-codigo" up-cd1406a1_it-codigo SKIP
              "de-qtidade-atu       " de-qtidade-atu SKIP
              "escd1406a1_qt-a-atender " escd1406a1_qt-a-atender
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
      */
      IF escd1406a1_qt-a-atender > de-qtidade-atu THEN DO:
         MESSAGE "Item nÆo possui saldo suficiente em estoque!" SKIP(1)
                 "Saldo do Item: " STRING(de-qtidade-atu,"->>,>>>,>>>,>>9.9999")
          VIEW-AS ALERT-BOX ERROR TITLE "Aten‡Æo".
      END.
      ELSE
         APPLY "CHOOSE":U TO h_botao.
   END.
   ELSE
      APPLY "CHOOSE":U TO h_botao.
   
   RETURN "OK".
END.

IF h_botao:NAME = "qt-a-atender" THEN DO:
   ASSIGN escd1406a1_qt-a-atender = DEC(h_botao:SCREEN-VALUE).
END.


IF h_botao:NAME = "it-codigo" THEN DO:
   ASSIGN up-cd1406a1_it-codigo = h_botao:SCREEN-VALUE.
END.
