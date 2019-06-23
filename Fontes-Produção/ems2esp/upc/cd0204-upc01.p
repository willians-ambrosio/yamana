/*-----------------------------------------------------------------------------------
    PROGRAMA : cd0204-upc01.p
    OBJETIVO : Chamanda de UPC do cd0204
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
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc01-it-codigo         AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc01-tipo-controle     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc01-lcp116            AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc01-lcp116-txt        AS WIDGET-HANDLE NO-UNDO.


/*************************************************************************************
                                    VARIAVEIS LOCAIS
*************************************************************************************/
DEFINE VARIABLE c-handle-obj  AS CHARACTER          NO-UNDO.
DEFINE VARIABLE rw-ext-nf-e   AS ROWID              NO-UNDO.
DEFINE VARIABLE wh-pesquisa   AS WIDGET-HANDLE      NO-UNDO.  
DEFINE VARIABLE l-implanta    AS LOGICAL            NO-UNDO.


/* RUN pi-msg.    */

/* ---> INSTANCIA OBJETOS <--- */ 
IF p-ind-event               = "BEFORE-INITIALIZE"    AND
   p-ind-object              = "VIEWER"               AND
   p-wgh-object:PRIVATE-DATA = "invwr/v34in172.w"      THEN DO:

  c-handle-obj = fc-handle-obj("it-codigo",p-wgh-frame).
  wh-cd0204-upc01-it-codigo      = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
END.

IF p-ind-event               = "BEFORE-INITIALIZE"    AND
   p-ind-object              = "VIEWER"               AND
   p-wgh-object:PRIVATE-DATA = "invwr/v36in172.w"     THEN DO:

  c-handle-obj = fc-handle-obj("cb-tipo-controle",p-wgh-frame).
  wh-cd0204-upc01-tipo-controle  = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.

  
  ON "VALUE-CHANGED" OF wh-cd0204-upc01-tipo-controle PERSISTENT RUN upc\cd0204-upc01.p("VALUE-CHANGED",
                                                                                        "wh-cd0204-upc01-tipo-controle",
                                                                                        p-wgh-object,
                                                                                        p-wgh-frame,
                                                                                        "",
                                                                                        ?).    
  CREATE FILL-IN wh-cd0204-upc01-lcp116      
    ASSIGN 
      NAME      = "c-lcp116"
      FRAME     = wh-cd0204-upc01-tipo-controle:FRAME 
      COL       = wh-cd0204-upc01-tipo-controle:COL + 35
      ROW       = wh-cd0204-upc01-tipo-controle:ROW  
      FORMAT    = "x(6)"
      WIDTH     = 8
      HEIGHT    = 0.88
      VISIBLE   = YES
      SENSITIVE = NO
      TRIGGERS:
        ON "F5", "MOUSE-SELECT-DBLCLICK" PERSISTENT RUN upc/cd0204-upc01.p(INPUT "F5",
                                                                           INPUT "lcp116",
                                                                           INPUT p-wgh-object,
                                                                           INPUT p-wgh-frame,
                                                                           INPUT ?,
                                                                           INPUT ?).
      END TRIGGERS.
      wh-cd0204-upc01-lcp116:LOAD-MOUSE-POINTER("image/lupa.cur":U).


  CREATE TEXT wh-cd0204-upc01-lcp116-txt
    ASSIGN 
      FRAME        = wh-cd0204-upc01-lcp116:FRAME
      FORMAT       = "x(20)"
      WIDTH        = 15
      HEIGHT-CHARS = 0.88
      SCREEN-VALUE = "Cod.Servi‡o(LCP116):"
      ROW          = wh-cd0204-upc01-lcp116:ROW 
      COL          = wh-cd0204-upc01-lcp116:COL - 15.5
      VISIBLE      = YES.                  
END.

IF VALID-HANDLE(wh-cd0204-upc01-lcp116-txt) THEN
  ASSIGN wh-cd0204-upc01-lcp116-txt:SCREEN-VALUE = "Cod.Servi‡o(LCP116):".


/* ---> NAVEGA€ÇO <--- */ 
IF p-ind-event               = "DISPLAY"    AND
   p-ind-object              = "VIEWER"               AND
   p-wgh-object:PRIVATE-DATA = "invwr/v36in172.w"     THEN DO:

  FIND FIRST item NO-LOCK 
    WHERE ROWID(item) = p-row-table NO-ERROR.
  IF AVAIL item THEN DO:
    
    FIND FIRST ext-item-servico OF item NO-ERROR.
    IF AVAIL ext-item-servico THEN
      ASSIGN wh-cd0204-upc01-lcp116:SCREEN-VALUE = ext-item-servico.class-servico. 
    ELSE
      ASSIGN wh-cd0204-upc01-lcp116:SCREEN-VALUE = "".
  END. 
END.
  
/* ---> ENABLE/DISABLE  <--- */ 
IF p-ind-event  = "AFTER-ENABLE"    AND
   p-ind-object = 'VIEWER'          AND 
   p-wgh-object:PRIVATE-DATA = "invwr/v36in172.w" AND 
   wh-cd0204-upc01-tipo-controle:SCREEN-VALUE = "DEBITO DIRETO" THEN 
   ASSIGN wh-cd0204-upc01-lcp116:SENSITIVE = YES.

IF p-ind-event  = "AFTER-DISABLE"    AND
   p-ind-object = 'VIEWER'           AND 
   p-wgh-object:PRIVATE-DATA = "invwr/v36in172.w" THEN 
   ASSIGN wh-cd0204-upc01-lcp116:SENSITIVE = NO.


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
        ext-item-servico.class-servico    = wh-cd0204-upc01-lcp116:SCREEN-VALUE.  .
    END.
    ELSE
      ASSIGN
        ext-item-servico.class-servico = wh-cd0204-upc01-lcp116:SCREEN-VALUE.
    
  END. 
END.

/* --->   DELETANDO REGISTRO VIA 
        TRIGGER DE DELETE NA TABELA <--- */ 

/* --->  VALUE-CHARGED DO TIPO DE CONTROLE <--- */
IF p-ind-event               = "VALUE-CHANGED"    AND
   p-ind-object              = "wh-cd0204-upc01-tipo-controle" THEN DO:

  IF wh-cd0204-upc01-tipo-controle:SCREEN-VALUE = "DEBITO DIRETO" THEN
    ASSIGN wh-cd0204-upc01-lcp116:SENSITIVE = YES.
  ELSE
    ASSIGN 
      wh-cd0204-upc01-lcp116:SENSITIVE = NO
      wh-cd0204-upc01-lcp116:SCREEN-VALUE = "". 
END.

/* --->  VALUE-CHARGED DO TIPO DE CONTROLE <--- */
IF p-ind-event               = "F5"    AND
   p-ind-object              = "lcp116" THEN DO:

  {include/zoomvar.i 
   &prog-zoom=esp/esnfe003-z01.W 
   &proghandle   = wh-pesquisa
   &campohandle  = wh-cd0204-upc01-lcp116
   &campozoom    = cod-class }
END.

IF p-ind-event            = "VALIDATE"    AND
   p-ind-object           = "VIEWER"      AND 
   p-wgh-object:PRIVATE-DATA = "invwr/v36in172.w" THEN DO:

  FIND FIRST es-class-servico NO-LOCK 
    WHERE es-class-servico.cod-class = INT(wh-cd0204-upc01-lcp116:SCREEN-VALUE) NO-ERROR.
  IF NOT AVAIL es-class-servico THEN DO:
    RUN utp/ut-msgs.p(INPUT 'show',
                      INPUT 2,
                      INPUT "Classificacao de Servico").
    RETURN 'NOK':U.
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
