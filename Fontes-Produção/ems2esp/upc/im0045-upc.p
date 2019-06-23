/*******************************************************************************
** Programa: mi0045-upc
** Autor...: Joao B. C. Bisneto
** Data....: 10/2015
** OBS.....: UPC utilizada pelo programa mi0045
** Objetivo: 
*******************************************************************************/
DEF INPUT PARAM p-ind-event                                AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object                               AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object                               AS   HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame                                AS   WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table                                AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table                                AS   ROWID         NO-UNDO.
/*---------------------------------------------------------------------*/
DEFINE VARIABLE c-char                                     AS CHARACTER       NO-UNDO.
DEFINE VARIABLE wh-grupo                                   AS WIDGET-HANDLE   NO-UNDO.
DEFINE VARIABLE wh-child                                   AS WIDGET-HANDLE   NO-UNDO.
/*---------------------------------------------------------------------*/     
DEFINE NEW GLOBAL SHARED VAR wgh-embarque-mi0045           AS WIDGET-HANDLE   NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-lbl-fornec-ii-im0045      AS WIDGET-HANDLE   NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-fornec-ii-im0045          AS WIDGET-HANDLE   NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-nome-fornec-ii-im0045     AS WIDGET-HANDLE   NO-UNDO.
/*---------------------------------------------------------------------*/                                                                          
DEFINE NEW GLOBAL SHARED VAR wgh-lbl-estab-ii-im0045       AS WIDGET-HANDLE   NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-estab-ii-im0045           AS WIDGET-HANDLE   NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-nome-estab-ii-im0045      AS WIDGET-HANDLE   NO-UNDO.
def    new global shared var v_cdn_empres_usuar            like mguni.empresa.ep-codigo no-undo.
/*---------------------------------------------------------------------*/
ASSIGN 
  c-char = ENTRY(NUM-ENTRIES(
    p-wgh-object:FILE-NAME,'/'),
    p-wgh-object:FILE-NAME,'/').
/*---------------------------------------------------------------------*/
/* MESSAGE "p-ind-event..:" p-ind-event                  SKIP */
/*         "p-ind-object.:" p-ind-object                 SKIP */
/*         "p-cod-table..:" STRING(p-cod-table)          SKIP */
/*         "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP */
/*         "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP */
/*         "p-wgh-frame..:" p-wgh-frame:NAME             SKIP */
/*         "p-row-table..:" STRING(p-row-table)          SKIP */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */
/*---------------------------------------------------------------------*/
IF
  p-ind-event  = "leave-fornec-ii":U      AND
  VALID-HANDLE(wgh-fornec-ii-im0045)      AND 
  VALID-HANDLE(wgh-nome-fornec-ii-im0045) THEN 
  DO:
    FIND emitente
      NO-LOCK
      WHERE emitente.cod-emitente = INT(wgh-fornec-ii-im0045:SCREEN-VALUE)
      NO-ERROR.
    IF AVAIL emitente THEN
      ASSIGN wgh-nome-fornec-ii-im0045:SCREEN-VALUE = emitente.nome-abrev.
    ELSE
      ASSIGN wgh-nome-fornec-ii-im0045:SCREEN-VALUE = "NÆo Identificado".
  END.
/*---------------------------------------------------------------------*/
IF
  p-ind-event  = "leave-estab-ii":U      AND
  VALID-HANDLE(wgh-estab-ii-im0045)      AND 
  VALID-HANDLE(wgh-nome-estab-ii-im0045) THEN 
  DO:
    FIND estabelec
      NO-LOCK
      WHERE estabelec.cod-estabel = wgh-estab-ii-im0045:SCREEN-VALUE
      NO-ERROR.
    IF AVAIL estabelec THEN
      ASSIGN wgh-nome-estab-ii-im0045:SCREEN-VALUE = estabelec.nome.
    ELSE
      ASSIGN wgh-nome-estab-ii-im0045:SCREEN-VALUE = "NÆo Identificado".
  END.
/*---------------------------------------------------------------------*/
IF
  p-ind-event  = "before-initialize":U AND
  p-ind-object = "CONTAINER"           THEN 
  DO:
    RUN pi-pega-handle.
    IF VALID-HANDLE(wgh-embarque-mi0045) THEN
      DO:
        RUN pi-cria-objetos.
      END.
  END.
/*---------------------------------------------------------------------*/
  IF p-ind-event = "AFTER-ASSIGN"  THEN 
  DO:
    FIND embarque-imp
      NO-LOCK
      WHERE ROWID(embarque-imp) = p-row-table
      NO-ERROR.
    IF AVAIL embarque-imp THEN
      DO:
        FIND FIRST es-emb-imp
          WHERE es-emb-imp.cod-estabel = embarque-imp.cod-estabel
          AND   es-emb-imp.embarque    = embarque-imp.embarque   
          AND   es-emb-imp.ep-codigo   = v_cdn_empres_usuar
          NO-ERROR.
        IF NOT AVAIL es-emb-imp THEN
          DO:
            CREATE es-emb-imp.
            ASSIGN 
              es-emb-imp.cod-estabel = embarque-imp.cod-estabel    
              es-emb-imp.embarque    = embarque-imp.embarque       
              es-emb-imp.ep-codigo   = v_cdn_empres_usuar.
          END.
        ASSIGN
          es-emb-imp.cod-fornec-ii  = INT(wgh-fornec-ii-im0045:SCREEN-VALUE)
          es-emb-imp.cod-estabel-ii = wgh-estab-ii-im0045     :SCREEN-VALUE.
      END.
  END.
/*---------------------------------------------------------------------*/
IF p-ind-event = "AFTER-DISPLAY"  THEN 
  DO: 
    FIND embarque-imp
      NO-LOCK
      WHERE ROWID(embarque-imp) = p-row-table
      NO-ERROR.
    IF AVAIL embarque-imp THEN
      DO:
        /*------------------------------------------------------------------*/  
        ASSIGN
          wgh-fornec-ii-im0045     :SCREEN-VALUE = ""
          wgh-nome-fornec-ii-im0045:SCREEN-VALUE = ""
          wgh-estab-ii-im0045      :SCREEN-VALUE = ""
          wgh-nome-estab-ii-im0045 :SCREEN-VALUE = "".
        FOR FIRST es-emb-imp
          NO-LOCK
          WHERE es-emb-imp.cod-estabel = embarque-imp.cod-estabel
          AND   es-emb-imp.embarque    = embarque-imp.embarque   
          AND   es-emb-imp.ep-codigo   = v_cdn_empres_usuar:
            FIND emitente
              NO-LOCK
              WHERE emitente.cod-emit = es-emb-imp.cod-fornec-ii
              NO-ERROR.
            FIND estabelec
              NO-LOCK
              WHERE estabelec.cod-estabel = es-emb-imp.cod-estabel-ii
              NO-ERROR.
            ASSIGN
              wgh-fornec-ii-im0045     :SCREEN-VALUE = STRING(es-emb-imp.cod-fornec-ii)
              wgh-nome-fornec-ii-im0045:SCREEN-VALUE = IF AVAIL emitente THEN emitente.nome-abrev ELSE "NÆo Identificado"
              wgh-estab-ii-im0045      :SCREEN-VALUE = es-emb-imp.cod-estabel-ii 
              wgh-nome-estab-ii-im0045 :SCREEN-VALUE = IF AVAIL estabelec THEN estabelec.nome ELSE "NÆo Identificado".
          END.
        /*------------------------------------------------------------------*/  
      END.
  END.
/*---------------------------------------------------------------------*/
IF p-ind-event = "AFTER-ENABLE"  THEN 
  DO: 
    ASSIGN
      wgh-fornec-ii-im0045:SENSITIVE = YES
      wgh-estab-ii-im0045:SENSITIVE  = YES.
  END.
/*---------------------------------------------------------------------*/
IF p-ind-event = "AFTER-DISABLE"  THEN 
  DO: 
    ASSIGN
      wgh-fornec-ii-im0045:SENSITIVE = NO
      wgh-estab-ii-im0045:SENSITIVE  = NO.
  END.
/*---------------------------------------------------------------------*/
PROCEDURE pi-pega-handle:
  ASSIGN wh-grupo = p-wgh-frame:FIRST-CHILD.
  DO WHILE VALID-HANDLE(wh-grupo):
    ASSIGN  wh-child = wh-grupo:FIRST-CHILD.
    DO WHILE VALID-HANDLE(wh-child):
       /* depuracao 
       IF wh-child:NAME = "rt-mold-2" THEN
         MESSAGE wh-child:NAME
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       */
       CASE wh-child:TYPE:
         when "fill-in" then do:
             IF wh-child:NAME = "embarque":U THEN
                 ASSIGN wgh-embarque-mi0045 = wh-child.
         END.
       END.
       ASSIGN wh-child = wh-child:NEXT-SIBLING.
    END.
    ASSIGN wh-grupo = wh-grupo:NEXT-SIBLING.
  END.
END PROCEDURE.
PROCEDURE pi-cria-objetos:
  /*-------------------------------------------------------------*/
  CREATE FILL-IN wgh-fornec-ii-im0045
  ASSIGN
    FRAME              = p-wgh-frame
    DATA-TYPE          = "INTEGER"
    FORMAT             = ">>>>>>>>9"
    SCREEN-VALUE       = "0"
    HEIGHT             = wgh-embarque-mi0045:HEIGHT
    WIDTH              = wgh-embarque-mi0045:WIDTH - 4
    ROW                = wgh-embarque-mi0045:ROW + 10.6
    COL                = wgh-embarque-mi0045:COL + 10
    HELP               = "Fornecedor do Imposto de Importa‡Æo"
    TOOLTIP            = "Fornecedor do Imposto de Importa‡Æo"
    VISIBLE            = YES
    SENSITIVE          = NO
    NAME               = "perc-primeira-parcela"
    TRIGGERS:
      ON LEAVE PERSISTENT RUN upc/im0045-upc.p(
        INPUT "leave-fornec-ii" ,
        INPUT p-ind-object,
        INPUT p-wgh-object,
        INPUT p-wgh-frame ,
        INPUT p-cod-table ,
        INPUT p-row-table ).
    END TRIGGERS.

  CREATE TEXT wgh-lbl-fornec-ii-im0045
  ASSIGN 
    FRAME              = p-wgh-frame
    FORMAT             = "x(10)"
    WIDTH              = 7
    SCREEN-VALUE       = "Fornec II:"
    ROW                = wgh-fornec-ii-im0045:ROW + .1
    COL                = wgh-fornec-ii-im0045:COL - 07
    VISIBLE            = YES
    FONT               = 1.
  CREATE FILL-IN wgh-nome-fornec-ii-im0045
  ASSIGN
    FRAME              = p-wgh-frame
    DATA-TYPE          = "character"
    FORMAT             = "X(60)"
    SCREEN-VALUE       = "0"
    HEIGHT             = wgh-fornec-ii-im0045:HEIGHT
    WIDTH              = wgh-fornec-ii-im0045:WIDTH + 25
    ROW                = wgh-fornec-ii-im0045:ROW 
    COL                = wgh-fornec-ii-im0045:COL + 10
    HELP               = "Nome Fornecedor do Imposto de Importa‡Æo"
    TOOLTIP            = "Nome Fornecedor do Imposto de Importa‡Æo"
    VISIBLE            = YES
    SENSITIVE          = NO
    NAME               = "Nome Fornec II".
  /*-------------------------------------------------------------*/
  CREATE FILL-IN wgh-estab-ii-im0045
  ASSIGN
    FRAME              = p-wgh-frame
    DATA-TYPE          = "INTEGER"
    FORMAT             = ">>>>>>>>9"
    SCREEN-VALUE       = "0"
    HEIGHT             = wgh-embarque-mi0045:HEIGHT
    WIDTH              = wgh-embarque-mi0045:WIDTH - 4
    ROW                = wgh-embarque-mi0045:ROW + 11.5
    COL                = wgh-embarque-mi0045:COL + 10
    HELP               = "Fornecedor do Imposto de Importa‡Æo"
    TOOLTIP            = "Fornecedor do Imposto de Importa‡Æo"
    VISIBLE            = YES
    SENSITIVE          = NO
    NAME               = "perc-primeira-parcela"
    TRIGGERS:
      ON LEAVE PERSISTENT RUN upc/im0045-upc.p(
        INPUT "leave-estab-ii" ,
        INPUT p-ind-object,
        INPUT p-wgh-object,
        INPUT p-wgh-frame ,
        INPUT p-cod-table ,
        INPUT p-row-table ).
      END TRIGGERS.
  CREATE TEXT wgh-lbl-estab-ii-im0045
  ASSIGN 
    FRAME              = p-wgh-frame
    FORMAT             = "x(10)"
    WIDTH              = 7
    SCREEN-VALUE       = "Estab II:"
    ROW                = wgh-fornec-ii-im0045:ROW + 1.1
    COL                = wgh-fornec-ii-im0045:COL - 07
    VISIBLE            = YES
    FONT               = 1.
  CREATE FILL-IN wgh-nome-estab-ii-im0045
  ASSIGN
    FRAME              = p-wgh-frame
    DATA-TYPE          = "character"
    FORMAT             = "X(60)"
    SCREEN-VALUE       = "0"
    HEIGHT             = wgh-fornec-ii-im0045:HEIGHT
    WIDTH              = wgh-fornec-ii-im0045:WIDTH + 25
    ROW                = wgh-fornec-ii-im0045:ROW + .9
    COL                = wgh-fornec-ii-im0045:COL + 10
    HELP               = "Nome Fornecedor do Imposto de Importa‡Æo"
    TOOLTIP            = "Nome Fornecedor do Imposto de Importa‡Æo"
    VISIBLE            = YES
    SENSITIVE          = NO
    NAME               = "Nome Estab II".
  /*-------------------------------------------------------------*/
END PROCEDURE.
