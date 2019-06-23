/*-----------------------------------------------------------------------------------
    PROGRAMA : ft0114-upc01.p
    OBJETIVO : Definir se a Ser x Est ira ger  NF-e.
    AUTOR    : Wellington Aparecido - WSA  (DSC)
    DATA     : 08/08/2008
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
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft0114-upc01-cod-estabel       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft0114-upc01-serie             AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft0114-upc01-log-gera-nfe      AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft0114-upc01-rs-amb-sefaz      AS WIDGET-HANDLE NO-UNDO.

/*************************************************************************************
                                    VARIAVEIS LOCAIS
*************************************************************************************/
DEFINE VARIABLE c-handle-obj  AS CHARACTER        NO-UNDO.
DEFINE VARIABLE rw-ext-nf-e   AS ROWID            NO-UNDO.
  
 /* RUN pi-msg. */

/* ---> INSTANCIA OBJETOS <--- */ 
IF p-ind-event  = "BEFORE-INITIALIZE"    AND
   p-ind-object = 'VIEWER'               THEN DO:

  c-handle-obj = fc-handle-obj("cod-estabel",p-wgh-frame).
  wh-ft0114-upc01-cod-estabel   = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
  c-handle-obj = fc-handle-obj("serie",p-wgh-frame).
  wh-ft0114-upc01-serie         = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.

  CREATE TOGGLE-BOX wh-ft0114-upc01-log-gera-nfe
    ASSIGN 
      NAME  = "tg-gera-nfe"
      FRAME = wh-ft0114-upc01-serie:FRAME 
      COL   = wh-ft0114-upc01-serie:COL 
      ROW   = wh-ft0114-upc01-serie:ROW + 10.5 
      LABEL = "Gera Nota Fiscal Eletronica"
      WIDTH  = 32
      VISIBLE = YES
      SENSITIVE = NO
      CHECKED = NO.
  ON "VALUE-CHANGED" OF wh-ft0114-upc01-log-gera-nfe PERSISTENT RUN upc\ft0114-upc01.p("VALUE-CHANGED",
                                                                                       "wh-ft0114-upc01-log-gera-nfe",
                                                                                       p-wgh-object,
                                                                                       p-wgh-frame,
                                                                                       "",
                                                                                       ?).

  IF VALID-HANDLE(wh-ft0114-upc01-log-gera-nfe) THEN DO:

    CREATE RADIO-SET wh-ft0114-upc01-rs-amb-sefaz
      ASSIGN
      FRAME         = wh-ft0114-upc01-log-gera-nfe:FRAME
      ROW           = wh-ft0114-upc01-log-gera-nfe:ROW + 1
      COL           = wh-ft0114-upc01-log-gera-nfe:COL
      FONT          = 1
      HORIZONTAL    = YES
      SENSITIVE     = NO
      EXPAND        = YES
      RADIO-BUTTONS = "Homologa‡Æo,1,"
                      + "Produ‡Æo,2"
      VISIBLE       = YES.
    ON "VALUE-CHANGED" OF wh-ft0114-upc01-rs-amb-sefaz PERSISTENT RUN upc\ft0114-upc01.p("VALUE-CHANGED",
                                                                                         "wh-ft0114-upc01-rs-amb-sefaz",
                                                                                         p-wgh-object,
                                                                                         p-wgh-frame,
                                                                                         "",
                                                                                         ?).
  END.
END.

/* ---> NAVEGA€ÇO <--- */ 
IF p-ind-event  = "DISPLAY"    AND
   p-ind-object = 'VIEWER'     THEN DO:

  FIND FIRST ser-estab NO-LOCK 
    WHERE ROWID(ser-estab) = p-row-table NO-ERROR.
  IF AVAIL ser-estab THEN DO:
     
    FIND FIRST mgesp.ext-ser-estab NO-LOCK
         WHERE mgesp.ext-ser-estab.serie       = ser-estab.serie
           AND mgesp.ext-ser-estab.cod-estabel = ser-estab.cod-estabel NO-ERROR.
    IF AVAIL mgesp.ext-ser-estab THEN 
      ASSIGN 
        wh-ft0114-upc01-log-gera-nfe:SCREEN-VALUE = STRING(mgesp.ext-ser-estab.log-gera-nfe)
        wh-ft0114-upc01-rs-amb-sefaz:SCREEN-VALUE = STRING(mgesp.ext-ser-estab.log-amb-sefaz) .  
    ELSE
      ASSIGN 
        wh-ft0114-upc01-log-gera-nfe:SCREEN-VALUE = "NO"
        wh-ft0114-upc01-rs-amb-sefaz:SCREEN-VALUE = "1"   .
  END. 
END.

/* ---> GRAVANDO  <--- */ 
IF p-ind-event  = "ASSIGN"    AND
   p-ind-object = 'VIEWER'               THEN DO:

  FIND FIRST ser-estab NO-LOCK 
    WHERE ROWID(ser-estab) = p-row-table NO-ERROR.
  IF AVAIL ser-estab THEN DO:
      
    FIND FIRST ext-ser-estab OF ser-estab NO-ERROR.
    IF NOT AVAIL ext-ser-estab THEN DO:
      CREATE ext-ser-estab.
      ASSIGN
        ext-ser-estab.serie         = ser-estab.serie       
        ext-ser-estab.cod-estabel   = ser-estab.cod-estabel
        ext-ser-estab.log-gera-nfe  = LOGICAL(wh-ft0114-upc01-log-gera-nfe:SCREEN-VALUE) 
        ext-ser-estab.log-amb-sefaz = INT(wh-ft0114-upc01-rs-amb-sefaz:SCREEN-VALUE) 
          .
    END.
    ELSE
      ASSIGN 
        ext-ser-estab.log-gera-nfe = LOGICAL(wh-ft0114-upc01-log-gera-nfe:SCREEN-VALUE) 
        ext-ser-estab.log-amb-sefaz = INT(wh-ft0114-upc01-rs-amb-sefaz:SCREEN-VALUE) .
    
  END. 
END.

/* ---> ENABLE/DISABLE  <--- */ 
IF p-ind-event  = "habilita-nfe"    AND
   p-ind-object = 'VIEWER'          THEN DO:

  ASSIGN 
    wh-ft0114-upc01-log-gera-nfe:SENSITIVE = YES.

  
  IF wh-ft0114-upc01-rs-amb-sefaz:SCREEN-VALUE = "2" 
    AND wh-ft0114-upc01-cod-estabel:SENSITIVE = NO THEN DO:
    ASSIGN 
      wh-ft0114-upc01-rs-amb-sefaz:SENSITIVE = NO.
      wh-ft0114-upc01-log-gera-nfe:SENSITIVE = NO.
  END.
 
  IF wh-ft0114-upc01-cod-estabel:SENSITIVE  THEN DO:
    ASSIGN 
      wh-ft0114-upc01-log-gera-nfe:SENSITIVE = YES
      wh-ft0114-upc01-log-gera-nfe:SCREEN-VALUE = "no".
  END.
END.
   

IF p-ind-event  = "AFTER-DISABLE"    AND
   p-ind-object = 'VIEWER'          THEN 
   ASSIGN 
     wh-ft0114-upc01-log-gera-nfe:SENSITIVE = NO
     wh-ft0114-upc01-rs-amb-sefaz:SENSITIVE = NO.

IF p-ind-event  = "VALUE-CHANGED"  AND
   p-ind-object = 'wh-ft0114-upc01-log-gera-nfe' THEN DO:
   
  IF wh-ft0114-upc01-log-gera-nfe:SCREEN-VALUE = "YES" THEN 
    ASSIGN wh-ft0114-upc01-rs-amb-sefaz:SENSITIVE = YES.
  ELSE
    ASSIGN wh-ft0114-upc01-rs-amb-sefaz:SENSITIVE = NO.

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
