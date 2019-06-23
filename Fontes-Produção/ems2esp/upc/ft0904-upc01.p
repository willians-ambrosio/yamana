/*-----------------------------------------------------------------------------------
    PROGRAMA : ft0904-upc01.p
    OBJETIVO : Chamanda de UPC do ft0904
    AUTOR    : Wellington Aparecido  (DSC)
    DATA     : 04/07/2008
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
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft0904-upc01-cod-estabel       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft0904-upc01-serie             AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft0904-upc01-nr-nota-fis       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft0904-upc01-bt-nfe            AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft0904-upc01-bt-sea            AS WIDGET-HANDLE NO-UNDO.

/*************************************************************************************
                                    VARIAVEIS LOCAIS
*************************************************************************************/

DEFINE VARIABLE c-handle-obj  AS CHARACTER        NO-UNDO.
DEFINE VARIABLE rw-ext-nf-e   AS ROWID            NO-UNDO.
  
 /*RUN pi-msg.  */

PROCEDURE pi-msg:
MESSAGE "p-ind-event..:" p-ind-event                  SKIP
        "p-ind-object.:" p-ind-object                 SKIP
        "p-cod-table..:" STRING(p-cod-table)          SKIP
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP
        "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP
        "p-row-table..:" string(p-row-table)          skip
        VIEW-AS ALERT-BOX INFO BUTTONS OK.                    
END.
           
IF p-ind-event  = "BEFORE-INITIALIZE"    AND
   p-ind-object = 'CONTAINER'               THEN DO:

  c-handle-obj = fc-handle-obj("bt-sea",p-wgh-frame).
  wh-ft0904-upc01-bt-sea         = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.

  CREATE BUTTON wh-ft0904-upc01-bt-nfe
  ASSIGN FRAME      = p-wgh-frame
         LABEL      = "NFE"
         FONT       = 0
         WIDTH      = wh-ft0904-upc01-bt-sea:WIDTH
         ROW        = wh-ft0904-upc01-bt-sea:ROW + 0.125
         COLUMN     = wh-ft0904-upc01-bt-sea:COL + 25
         VISIBLE    = TRUE
         SENSITIVE  = TRUE
         TOOLTIP    = "Consulta NF-e"
         HEIGHT     = wh-ft0904-upc01-bt-sea:HEIGHT.
  ON 'CHOOSE':U OF wh-ft0904-upc01-bt-nfe PERSISTENT RUN upc/ft0904-upc01.p("choose",
                                                                            "NFE",
                                                                            wh-ft0904-upc01-bt-nfe,
                                                                            p-wgh-frame,
                                                                            "",
                                                                            ?) .

  wh-ft0904-upc01-bt-nfe:LOAD-IMAGE-UP("image\im-inf.bmp").

END.


IF p-ind-event  = "BEFORE-INITIALIZE"    AND
   p-ind-object = 'VIEWER'               AND 
   p-wgh-object:FILE-NAME  = "divwr/v02di135.w" THEN DO:
    
  c-handle-obj = fc-handle-obj("cod-estabel",p-wgh-frame).
  wh-ft0904-upc01-cod-estabel    = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
  c-handle-obj = fc-handle-obj("serie",p-wgh-frame).
  wh-ft0904-upc01-serie          = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
  c-handle-obj = fc-handle-obj("nr-nota-fis",p-wgh-frame).
  wh-ft0904-upc01-nr-nota-fis    = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
 
END. 

/* ---> Choose do Bot∆o NF-e <--- */
IF p-ind-event  = 'choose' AND
   p-ind-object = 'NFE' THEN DO:

   
  FIND FIRST ext-nota-fiscal-nfe NO-LOCK 
    WHERE ext-nota-fiscal-nfe.cod-estabel = wh-ft0904-upc01-cod-estabel:SCREEN-VALUE
    AND   ext-nota-fiscal-nfe.serie       = wh-ft0904-upc01-serie:SCREEN-VALUE      
    AND   ext-nota-fiscal-nfe.nr-nota-fis = wh-ft0904-upc01-nr-nota-fis:SCREEN-VALUE NO-ERROR.
  IF AVAIL ext-nota-fiscal-nfe THEN 
    RUN upc/ft0904-upc02.w (INPUT ROWID(ext-nota-fiscal-nfe)).
END.

IF p-ind-event  = "DISPLAY"    AND
   p-ind-object = 'VIEWER'               AND 
   p-wgh-object:FILE-NAME  = "divwr/v02di135.w" THEN DO:
    
  FIND FIRST ext-nota-fiscal-nfe NO-LOCK 
    WHERE ext-nota-fiscal-nfe.cod-estabel = wh-ft0904-upc01-cod-estabel:SCREEN-VALUE
    AND   ext-nota-fiscal-nfe.serie       = wh-ft0904-upc01-serie:SCREEN-VALUE      
    AND   ext-nota-fiscal-nfe.nr-nota-fis = wh-ft0904-upc01-nr-nota-fis:SCREEN-VALUE NO-ERROR.
  IF AVAIL ext-nota-fiscal-nfe THEN 
    ASSIGN wh-ft0904-upc01-bt-nfe:SENSITIVE = TRUE.
  ELSE
    ASSIGN wh-ft0904-upc01-bt-nfe:SENSITIVE = FALSE.

END.


    
