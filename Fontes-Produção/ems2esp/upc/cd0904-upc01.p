/*-----------------------------------------------------------------------------------
    PROGRAMA : cd0904-upc01.p
    OBJETIVO : Chamanda de UPC do cd0904
    AUTOR    : Wellington Aparecido - WSA  (DSC)
    DATA     : 08|08|2008
-----------------------------------------------------------------------------------*/
def buffer unid-feder for ems2cadme.unid-feder.

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i cd0904-upc01.p 2.06.00.000} 
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
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0904-upc01-cod-sinief        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0904-upc01-cod-ibge          AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0904-upc01-cod-ibge-txt      AS WIDGET-HANDLE NO-UNDO.

/*************************************************************************************
                                    VARIAVEIS LOCAIS
*************************************************************************************/
DEFINE VARIABLE c-handle-obj  AS CHARACTER        NO-UNDO.
DEFINE VARIABLE rw-ext-nf-e   AS ROWID            NO-UNDO.
  
 /* RUN pi-msg.         */


/* ---> INSTANCIA OBJETOS <--- */ 
IF p-ind-event               = "BEFORE-INITIALIZE"     AND
   p-ind-object              = "CONTAINER"      THEN DO:

  c-handle-obj = fc-handle-obj("cod-sinief",p-wgh-frame).
  wh-cd0904-upc01-cod-sinief = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
END.  

IF p-ind-event               = "BEFORE-INITIALIZE"     AND
   p-ind-object              = "VIEWER"             AND
   p-wgh-object:PRIVATE-DATA = "unvwr/v02un007.w"      THEN DO:

  CREATE FILL-IN wh-cd0904-upc01-cod-ibge
    ASSIGN 
    NAME      = "c-ibge"
    FRAME     = wh-cd0904-upc01-cod-sinief:FRAME 
    COL       = wh-cd0904-upc01-cod-sinief:COL + 18
    ROW       = wh-cd0904-upc01-cod-sinief:ROW  
    FORMAT    = "99"
    WIDTH     = 4.5
    HEIGHT    = 0.88
    VISIBLE   = YES
    SENSITIVE = NO .  

  CREATE TEXT wh-cd0904-upc01-cod-ibge-txt
    ASSIGN 
    FRAME        = wh-cd0904-upc01-cod-ibge:FRAME
    FORMAT       = "x(10)"
    WIDTH        = 8
    HEIGHT-CHARS = 0.88
    SCREEN-VALUE = "Cod.IBGE:"
    ROW          = wh-cd0904-upc01-cod-ibge:ROW 
    COL          = wh-cd0904-upc01-cod-ibge:COL - 8.0
    VISIBLE      = YES.                               
END.

IF VALID-HANDLE(wh-cd0904-upc01-cod-ibge-txt) THEN
  ASSIGN wh-cd0904-upc01-cod-ibge-txt:SCREEN-VALUE = "Cod.IBGE:".


/* ---> <--- */
/* ---> NAVEGA€ÇO <--- */ 
IF p-ind-event               = "DISPLAY"    AND
   p-ind-object              = "VIEWER"               AND
   p-wgh-object:PRIVATE-DATA = "unvwr/v02un007.w"     THEN DO:

  FIND FIRST unid-feder NO-LOCK 
    WHERE ROWID(unid-feder) = p-row-table NO-ERROR.
  IF AVAIL unid-feder THEN DO:
    
    FIND FIRST mgesp.ext-unid-feder OF unid-feder NO-ERROR.
    IF AVAIL mgesp.ext-unid-feder THEN
      ASSIGN wh-cd0904-upc01-cod-ibge:SCREEN-VALUE = STRING(mgesp.ext-unid-feder.cod-ibge). 
    ELSE
      ASSIGN wh-cd0904-upc01-cod-ibge:SCREEN-VALUE = "". 
  END. 
END.
  
/* ---> ENABLE/DISABLE  <--- */ 
IF p-ind-event  = "AFTER-ENABLE"    AND
   p-ind-object = 'VIEWER'          AND 
   p-wgh-object:PRIVATE-DATA = "unvwr/v02un007.w" THEN 
   ASSIGN wh-cd0904-upc01-cod-ibge:SENSITIVE = YES.

IF p-ind-event  = "AFTER-DISABLE"    AND
   p-ind-object = 'VIEWER'           AND 
   p-wgh-object:PRIVATE-DATA = "unvwr/v02un007.w" THEN 
   ASSIGN wh-cd0904-upc01-cod-ibge:SENSITIVE = NO.


/* ---> GRAVANDO  <--- */ 
IF p-ind-event  = "ASSIGN"    AND
   p-ind-object = 'VIEWER'        AND
   p-wgh-object:PRIVATE-DATA = "unvwr/v02un007.w"  THEN DO:

  FIND FIRST unid-feder NO-LOCK 
    WHERE ROWID(unid-feder) = p-row-table NO-ERROR.
  IF AVAIL unid-feder THEN DO:
    
    FIND FIRST ext-unid-feder OF unid-feder NO-ERROR.
    IF NOT AVAIL ext-unid-feder THEN DO:
      CREATE ext-unid-feder.
      ASSIGN
        ext-unid-feder.pais     = unid-feder.pais
        ext-unid-feder.estado   = unid-feder.estado
        ext-unid-feder.cod-ibge = INT(wh-cd0904-upc01-cod-ibge:SCREEN-VALUE).  
    END.
    ELSE
      ASSIGN
        ext-unid-feder.cod-ibge = INT(wh-cd0904-upc01-cod-ibge:SCREEN-VALUE).
    
  END. 
END.



PROCEDURE pi-msg:
MESSAGE "p-ind-event..:" p-ind-event                  SKIP
        "p-ind-object.:" p-ind-object                 SKIP
        "p-cod-table..:" STRING(p-cod-table)          SKIP
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP 
        "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP
        "p-row-table..:" string(p-row-table)          skip
        VIEW-AS ALERT-BOX INFO BUTTONS OK.                    
END.
