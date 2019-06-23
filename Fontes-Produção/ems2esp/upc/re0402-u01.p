/*-----------------------------------------------------------------------------------
    PROGRAMA : re0401-u01.p
    OBJETIVO : Chamanda de UPC do re0204
    AUTOR    : Thiago Coutinho (CSX)
    DATA     : Mar‡o/2012
-----------------------------------------------------------------------------------*/
/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{tools/fc-handle-obj.i}
{tools/fc-falso.i}                                

/*************************************************************************************
                                     PARAMETROS
*************************************************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/*************************************************************************************
                                    VARIAVEIS GLOBAIS
*************************************************************************************/
DEFINE NEW GLOBAL SHARED VARIABLE wh-RE0402-upc01-pg-sel        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-RE0402-upc01-pg-dig        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-RE0402-upc01-br-dig        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-RE0402-upc01-bt-exec       AS WIDGET-HANDLE NO-UNDO.
/*************************************************************************************
                                    VARIAVEIS LOCAIS
*************************************************************************************/
DEFINE VARIABLE c-handle-obj  AS CHARACTER          NO-UNDO.
DEFINE VARIABLE rw-ext-nf-e   AS ROWID              NO-UNDO.
DEFINE VARIABLE wh-pesquisa   AS WIDGET-HANDLE      NO-UNDO.  
DEFINE VARIABLE l-implanta    AS LOGICAL            NO-UNDO.


/* RUN pi-msg. */

/* ---> INSTANCIA OBJETOS <--- */ 
IF p-ind-event               = "INITIALIZE"    AND
   p-ind-object              = "CONTAINER"     AND
   p-wgh-object:PRIVATE-DATA = "rep/re0402.w"  THEN DO:

  c-handle-obj = fc-handle-obj("im-pg-sel",p-wgh-frame).
  wh-RE0402-upc01-pg-sel           = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
  c-handle-obj = fc-handle-obj("im-pg-dig",p-wgh-frame).
  wh-RE0402-upc01-pg-dig           = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
  c-handle-obj = fc-handle-obj("br-digita",p-wgh-frame).
  wh-RE0402-upc01-br-dig           = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
  c-handle-obj = fc-handle-obj("bt-executar",p-wgh-frame).
  wh-RE0402-upc01-bt-exec          = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.

  IF VALID-HANDLE(wh-RE0402-upc01-pg-sel) AND
     VALID-HANDLE(wh-RE0402-upc01-pg-dig) THEN DO:

      ASSIGN wh-RE0402-upc01-pg-sel:SENSITIVE = NO.
      apply "mouse-select-click" to wh-RE0402-upc01-pg-dig.

  END.
  RETURN "OK":U.

END.

/* IF VALID-HANDLE(wh-RE0402-upc01-bt-exec) AND                                                          */
/*    VALID-HANDLE(wh-RE0402-upc01-br-dig) THEN DO:                                                      */
/*     ON 'mouse-select-click':U OF wh-RE0402-upc01-bt-exec                                              */
/*     DO:                                                                                               */
/*         if num-results("wh-RE0402-upc01-br-dig":U) = 0 THEN DO:                                       */
/*             MESSAGE 'nao deve executar'                                                               */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
/*             RETURN NO-APPLY.                                                                          */
/*         END.                                                                                          */
/*         ELSE DO:                                                                                      */
/*             MESSAGE 'nao ira executar'                                                                */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
/*         END.                                                                                          */
/* /*         br-digita:num-selected-rows > 0 then do on error undo, return no-apply:                 */ */
/* /*         IF wh-RE0402-upc01-br-dig:NUM-SELECTED-ROWS > 0 then do on error undo, return no-apply: */ */
/* /*             RETURN.                                                                             */ */
/* /*         ELSE                                                                                    */ */
/* /*             RETURN ERROR.                                                                       */ */
/*     END.                                                                                              */
/* END.                                                                                                  */

PROCEDURE pi-msg:
MESSAGE "p-ind-event..:" p-ind-event                  SKIP
        "p-ind-object.:" p-ind-object                 SKIP
        "p-cod-table..:" STRING(p-cod-table)          SKIP
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP
        "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP
        "p-row-table..:" string(p-row-table)          skip
        VIEW-AS ALERT-BOX INFO BUTTONS OK.                    
END.
