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

DEFINE NEW GLOBAL SHARED VARIABLE up-cd1409_de-saldo-req    AS WIDGET-HANDLE NO-UNDO.

IF h_botao:NAME = "qtidade-req" THEN DO:
   ASSIGN h_botao:SCREEN-VALUE = up-cd1409_de-saldo-req:SCREEN-VALUE.
END.


/*
IF h_botao:NAME = "bt-ok" THEN DO:

   ASSIGN de-qtidade-atu = 0.
   FOR EACH saldo-estoq NO-LOCK
      WHERE saldo-estoq.it-codigo = up-cd1406a1_it-codigo
        AND saldo-estoq.qtidade-atu > 0:
      ASSIGN de-qtidade-atu = de-qtidade-atu + saldo-estoq.qtidade-atu.
   END.
   /*
   MESSAGE "up-cd1406a1_it-codigo   " up-cd1406a1_it-codigo SKIP
           "de-qtidade-atu          " de-qtidade-atu  SKIP
           "escd1406a1_qt-a-atender " escd1406a1_qt-a-atender
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
   */
   IF escd1406a1_qt-a-atender > de-qtidade-atu THEN DO:
      MESSAGE "Item n∆o possui saldo suficiente em estoque!" SKIP(1)
       VIEW-AS ALERT-BOX ERROR TITLE "Atená∆o".
   END.
   ELSE
      APPLY "CHOOSE":U TO h_botao.
   
   RETURN "OK".
END.

IF h_botao:NAME = "botao_bt-save" THEN DO:

   ASSIGN de-qtidade-atu = 0.
   FOR EACH saldo-estoq NO-LOCK
      WHERE saldo-estoq.it-codigo = up-cd1406a1_it-codigo
        AND saldo-estoq.qtidade-atu > 0:
      ASSIGN de-qtidade-atu = de-qtidade-atu + saldo-estoq.qtidade-atu.
   END.
   /*
   MESSAGE "up-cd1406a1_it-codigo   " up-cd1406a1_it-codigo SKIP
           "de-qtidade-atu          " de-qtidade-atu  SKIP
           "escd1406a1_qt-a-atender " escd1406a1_qt-a-atender
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
   FIND FIRST b_item NO-LOCK
        WHERE b_item.it-codigo = up-cd1406a1_it-codigo
        NO-ERROR.
   IF AVAIL b_item AND escd1406a1_qt-a-atender > de-qtidade-atu THEN DO:
      MESSAGE "Item n∆o possui saldo suficiente em estoque!" 
       VIEW-AS ALERT-BOX ERROR TITLE "Atená∆o". 
   END.
   ELSE 
     APPLY "CHOOSE":U TO up-cd1406a1_bt-save. 
END.

IF h_botao:NAME = "qt-a-atender" THEN DO:
   ASSIGN escd1406a1_qt-a-atender = DEC(h_botao:SCREEN-VALUE).
END.

IF h_botao:NAME = "it-codigo" THEN DO:
   ASSIGN up-cd1406a1_it-codigo = h_botao:SCREEN-VALUE.
END.
*/
