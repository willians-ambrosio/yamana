/*-----------------------------------------------------------------------------------
    PROGRAMA : ft0502-upc01.p
    OBJETIVO : Chamanda de UPC do ft0502  - Modificaá∆o Layout NF-e.
    AUTOR    : Wellington Aparecido - WSA  (DSC)
    DATA     : 22/07/2008
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
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft0502-upc01-bt-sav            AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft0502-upc01-nfe               AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE r-gl-nota-fis                     AS ROWID         NO-UNDO.

/*************************************************************************************
                                    VARIAVEIS LOCAIS
*************************************************************************************/
DEFINE VARIABLE c-handle-obj  AS CHARACTER        NO-UNDO.
DEFINE VARIABLE rw-ext-nf-e   AS ROWID            NO-UNDO.
  
/*RUN pi-msg.       */


/* ---> INSTANCIA OBJETOS <--- */ 
IF p-ind-event               = "BEFORE-INITIALIZE"     AND
   p-ind-object              = "CONTAINER"      THEN DO:

  c-handle-obj = fc-handle-obj("bt-sav",p-wgh-frame).
  wh-ft0502-upc01-bt-sav       = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.

  CREATE BUTTON wh-ft0502-upc01-nfe
  ASSIGN FRAME  = p-wgh-frame
         COL    = wh-ft0502-upc01-bt-sav:COL + 40  
         ROW    = wh-ft0502-upc01-bt-sav:ROW + 0.35  
         NAME   = "bt-nfe"  
         WIDTH  = wh-ft0502-upc01-bt-sav:WIDTH 
         HEIGHT = wh-ft0502-upc01-bt-sav:HEIGHT
         VISIBLE = YES
         SENSITIVE = NO
         LABEL     = "NFE" 
         TOOLTIP    = "Reenvia NF-e".
         ON 'CHOOSE':U OF wh-ft0502-upc01-nfe PERSISTENT RUN upc/ft0502-upc01.p("choose",
                                                                                "NFE",
                                                                                wh-ft0502-upc01-nfe,
                                                                                p-wgh-frame,
                                                                                "",
                                                                                ?) .
END.  

/* ---> <--- */
IF p-ind-event               = "DISPLAY"    AND
   p-ind-object              = "VIEWER"     THEN DO:

  FIND FIRST nota-fiscal NO-LOCK
    WHERE ROWID(nota-fiscal) = p-row-table NO-ERROR.
  IF AVAIL nota-fiscal THEN DO: 
    FIND FIRST mgesp.ext-ser-estab OF nota-fiscal NO-LOCK NO-ERROR.
    IF mgesp.ext-ser-estab.log-gera-nfe THEN
      ASSIGN 
        wh-ft0502-upc01-nfe:VISIBLE = YES.
    ELSE
      ASSIGN 
        wh-ft0502-upc01-nfe:VISIBLE = NO.
  END.                                   
END.

/* ---> <--- */
IF p-ind-event  = "AFTER-VALIDATE"    AND
   p-ind-object = 'VIEWER'            THEN DO:

  FIND FIRST nota-fiscal NO-LOCK
    WHERE ROWID(nota-fiscal) = p-row-table NO-ERROR.
  IF AVAIL nota-fiscal THEN DO:

    FIND FIRST ext-nota-fiscal-nfe OF nota-fiscal NO-LOCK NO-ERROR.
    IF AVAIL ext-nota-fiscal-nfe THEN
    IF  (ext-nota-fiscal-nfe.codigo  = 6 /* REjeitado */ 
      OR ext-nota-fiscal-nfe.codigo  = 8) /* REJEITADO RECEB */ THEN
      ASSIGN
        wh-ft0502-upc01-nfe:SENSITIVE = YES
        r-gl-nota-fis                 = p-row-table.
  END.                                  
END.  

/* ---> Choose do Bot∆o NF-e <--- */
IF p-ind-event  = 'choose' AND
   p-ind-object = 'NFE' THEN DO:

    FIND FIRST nota-fiscal NO-LOCK
      WHERE ROWID(nota-fiscal) = r-gl-nota-fis NO-ERROR.
    IF AVAIL nota-fiscal THEN
      RUN upc\bodi317ef-u01a.p(INPUT ROWID(nota-fiscal)).

  ASSIGN
    wh-ft0502-upc01-nfe:SENSITIVE = NO.
END.





/*
IF VALID-HANDLE(wh-ft0502-upc01-lcp116-txt) THEN
  ASSIGN wh-ft0502-upc01-lcp116-txt:SCREEN-VALUE = "Cod.Serviáo(LCP116):".


/* ---> NAVEGAÄ«O <--- */ 
IF p-ind-event               = "DISPLAY"    AND
   p-ind-object              = "VIEWER"               AND
   p-wgh-object:PRIVATE-DATA = "invwr/v36in172.w"     THEN DO:

  FIND FIRST item NO-LOCK 
    WHERE ROWID(item) = p-row-table NO-ERROR.
  IF AVAIL item THEN DO:
    
    FIND FIRST ext-item-servico OF item NO-ERROR.
    IF AVAIL ext-item-servico THEN
      ASSIGN wh-ft0502-upc01-lcp116:SCREEN-VALUE = ext-item-servico.class-servico. 
    ELSE
      ASSIGN wh-ft0502-upc01-lcp116:SCREEN-VALUE = "".
  END. 
END.
  
/* ---> ENABLE/DISABLE  <--- */ 
IF p-ind-event  = "AFTER-ENABLE"    AND
   p-ind-object = 'VIEWER'          AND 
   p-wgh-object:PRIVATE-DATA = "invwr/v36in172.w" AND 
   wh-ft0502-upc01-tipo-controle:SCREEN-VALUE = "DEBITO DIRETO" THEN 
   ASSIGN wh-ft0502-upc01-lcp116:SENSITIVE = YES.

IF p-ind-event  = "AFTER-DISABLE"    AND
   p-ind-object = 'VIEWER'           AND 
   p-wgh-object:PRIVATE-DATA = "invwr/v36in172.w" THEN 
   ASSIGN wh-ft0502-upc01-lcp116:SENSITIVE = NO.


/* ---> GRAVANDO  <--- */ 
IF p-ind-event  = "ASSIGN"    AND
   p-ind-object = 'VIEWER'        AND
   p-wgh-object:PRIVATE-DATA = "invwr/v36in172.w"  THEN DO:

  FIND FIRST item NO-LOCK 
    WHERE ROWID(item) = p-row-table NO-ERROR.
  IF AVAIL item THEN DO:
    
    FIND FIRST ext-item-servico OF item NO-ERROR.
    IF NOT AVAIL ext-item-servico THEN DO:
      CREATE ext-item-servico.
      ASSIGN
        ext-item-servico.it-codigo        = item.it-codigo
        ext-item-servico.class-servico    = wh-ft0502-upc01-lcp116:SCREEN-VALUE.  .
    END.
    ELSE
      ASSIGN
        ext-item-servico.class-servico = wh-ft0502-upc01-lcp116:SCREEN-VALUE.
    
  END. 
END.

/* --->   DELETANDO REGISTRO VIA 
        TRIGGER DE DELETE NA TABELA <--- */ 

/* --->  VALUE-CHARGED DO TIPO DE CONTROLE <--- */
IF p-ind-event               = "VALUE-CHANGED"    AND
   p-ind-object              = "wh-ft0502-upc01-tipo-controle" THEN DO:

  IF wh-ft0502-upc01-tipo-controle:SCREEN-VALUE = "DEBITO DIRETO" THEN
    ASSIGN wh-ft0502-upc01-lcp116:SENSITIVE = YES.
  ELSE
    ASSIGN 
      wh-ft0502-upc01-lcp116:SENSITIVE = NO
      wh-ft0502-upc01-lcp116:SCREEN-VALUE = "". 
END. */

PROCEDURE pi-msg:
MESSAGE "p-ind-event..:" p-ind-event                  SKIP
        "p-ind-object.:" p-ind-object                 SKIP
        "p-cod-table..:" STRING(p-cod-table)          SKIP
     /* "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP */
        "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP
        "p-row-table..:" string(p-row-table)          skip
        VIEW-AS ALERT-BOX INFO BUTTONS OK.                    
END.
