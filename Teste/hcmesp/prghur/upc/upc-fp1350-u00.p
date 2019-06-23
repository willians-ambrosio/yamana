
/*-----> Define de Parametros <---------------------------------------*/
DEF INPUT PARAMETER p-ind-event              AS CHAR          NO-UNDO.
DEF INPUT PARAMETER p-ind-object             AS CHAR          NO-UNDO.
DEF INPUT PARAMETER p-wgh-object             AS HANDLE        NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame              AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAMETER p-cod-table              AS CHAR          NO-UNDO.
DEF INPUT PARAMETER p-row-table              AS ROWID         NO-UNDO.

/*-----> Define de Variaveis Locais <---------------------------------*/
DEF VAR c-handle-obj                         AS CHARACTER     NO-UNDO.
DEF VAR c-folder        AS CHARACTER                NO-UNDO.
DEF VAR c-objects       AS CHARACTER                NO-UNDO.

/*-----DEFINICAO DE VARIAVEIS -----*/
DEF VAR wh-programa            AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-cdn-estab                      AS HANDLE        NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cdn-funcionario                AS HANDLE        NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-fp1350-container               AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR adm-broker-hdl                   AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-fp1350-browse                 AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-fp1350-folder                 AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-objeto                        AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR p-row-upc-fp1350      AS ROWID NO-UNDO.

DEFINE VARIABLE char-hdl AS CHARACTER  NO-UNDO.
DEFINE VARIABLE container-hdl AS HANDLE     NO-UNDO.

/* /* ----------> Mensagem para analisar evento da UPC <---------- */  */
/* MESSAGE "p-ind-event..:" p-ind-event            SKIP  */
/*         "p-ind-object.:" p-ind-object           SKIP  */
/*         "p-wgh-object.:" p-wgh-object:FILE-NAME SKIP  */
/*         "p-wgh-frame..:" STRING(p-wgh-frame)    SKIP  */
/*         "p-cod-table..:" STRING(p-cod-table)    SKIP  */
/*         "p-row-table..:" STRING(p-row-table)    SKIP  */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                */
/* /* ----------> Instanciando Folder "Inf.Comp" <---------- */        */


IF p-cod-table   = "funcionario"   THEN DO:
   ASSIGN p-row-upc-fp1350 = IF p-row-table <> ? THEN p-row-table ELSE ?.
END.

IF p-ind-event            = "BEFORE-INITIALIZE"  AND
   p-ind-object           = "CONTAINER"          AND 
   p-wgh-object:FILE-NAME = "prghur/fpp/fp1350.w"   THEN DO:

    /*Muda o tamanho da Window */
    p-wgh-object:CURRENT-WINDOW:WIDTH = 95.
    p-wgh-frame:WIDTH = 95.
/*     p-wgh-object:CURRENT-WINDOW:HEIGHT = 10.  */
/*     p-wgh-frame:HEIGHT = 10.                  */

/*   ASSIGN wh-fp1350-container = p-wgh-object.  */

  RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                         INPUT "PAGE-SOURCE":U,
                                         OUTPUT c-folder).

  ASSIGN wh-fp1350-folder = WIDGET-HANDLE(c-folder) NO-ERROR.

  RUN SET-SIZE IN wh-fp1350-folder (9 , 95) NO-ERROR.

  RUN create-folder-page IN wh-fp1350-folder (INPUT 9,INPUT "Gestor":U).
  RUN create-folder-label IN wh-fp1350-folder (INPUT 9,INPUT "Gestor":U).

  RUN select-page IN p-wgh-object (INPUT 9).
        
  RUN init-object IN p-wgh-object (INPUT "prghur/upc/upc-fp1350-b01.w":U,
                                   INPUT p-wgh-frame,
                                   INPUT "Layout =":U,
                                   OUTPUT wh-fp1350-browse).

  RUN set-position IN wh-fp1350-browse (7.00, 3.00).
  
  RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                         INPUT "CONTAINER-TARGET":U,
                                         OUTPUT c-objects).


  RUN dispatch IN wh-fp1350-browse ("initialize":U).
  RUN select-page IN p-wgh-object (INPUT 1).


  /* Links to BrowserCadastro2 h_b01py118. */
/*   RUN add-link IN adm-broker-hdl ( h_Q01PY085 , 'Record':U , wh-fp1350-browse ).  */

/*   /* Adjust the tab order of the smart objects. */                        */
/*   RUN adjust-tab-order IN adm-broker-hdl (wh-fp1350-browse ,              */
/*                                           wh-fp1350-folder, 'AFTER':U ).  */
  
/*   RUN dispatch IN wh-fp1350-browse ("hide":U).  */

END.

IF p-ind-event            = "DISPLAY"                    AND
   p-ind-object           = "VIEWER"                     AND 
   p-wgh-object:FILE-NAME = "object/sopy/vwr/v01py085.w" AND
   p-cod-table            = "funcionario"                AND 
   p-row-table            <> ? THEN DO:   

   ASSIGN p-row-upc-fp1350 = p-row-table.

   IF VALID-HANDLE(wh-fp1350-browse) THEN DO:
        RUN local-open-query IN wh-fp1350-browse.
   END.

END.
