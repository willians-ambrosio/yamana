/***************************************************************************************************
**    Programa: upc-cd4035-u01.p
**    Objetivo: Criar BotÆo Gerar Por Nota
**       Autor: Willians Moreira Ambrosio - Grupo DKP
**   Descri‡Æo: Incluir mais de um documento referenciado de forma automatizada
***************************************************************************************************/

{include/i-prgvrs.i upc-cd4035-U01 12.01.19.001} 
{tools/fc-handle-obj.i}

DEF INPUT PARAM p-ind-event  AS CHAR          no-undo.
DEF INPUT PARAM p-ind-object AS CHAR          no-undo.
DEF INPUT PARAM p-wgh-object AS HANDLE        no-undo.
DEF INPUT PARAM p-wgh-frame  AS WIDGET-HANDLE no-undo.
DEF INPUT PARAM p-cod-table  AS CHAR          no-undo.
DEF INPUT PARAM p-row-table  AS ROWID         no-undo.

def new global shared var wh-cd4035-bt-del-3                AS widget-handle no-undo.
def new global shared var wh-cd4035-bt-gerar-nf-adc         AS widget-handle no-undo.
def new global shared var wh-cd4035-f-page-3                AS widget-handle no-undo. 
def new global shared var wh-cd4035-Br3                     AS widget-handle no-undo. 
def new global shared var wh-cd4035-cb-tipo-inf             AS widget-handle no-undo. 

DEF NEW GLOBAL SHARED VAR v_cod_usuar_corren      AS CHARACTER FORMAT "x(12)" LABEL "Usuÿrio Corrente" COLUMN-LABEL "Usuÿrio Corrente" NO-UNDO.
/* --------------------------------------------------------------------- */
DEF VAR c-handle-obj             AS CHAR                      NO-UNDO.
DEF VAR ct                       AS INT                       NO-UNDO.
def var i-pagina                 as INTEGER                   no-undo.
/* --------------------------------------------------------------------- */
IF p-ind-event  = "INITIALIZE" AND
   p-ind-object = "CONTAINER"  THEN 
DO:
   c-handle-obj         = fc-handle-obj("fPage3", p-wgh-frame:PARENT).
   wh-cd4035-f-page-3   = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).

   c-handle-obj = fc-handle-obj("bt-del-3,cb-tipo-inf,Br3",wh-cd4035-f-page-3:PARENT).      
   wh-cd4035-bt-del-3           = WIDGET-HANDLE(ENTRY(01,c-handle-obj)) NO-ERROR.
   wh-cd4035-cb-tipo-inf        = WIDGET-HANDLE(ENTRY(02,c-handle-obj)) NO-ERROR.
   wh-cd4035-Br3                = WIDGET-HANDLE(ENTRY(03,c-handle-obj)) NO-ERROR.
      
   IF VALID-HANDLE(wh-cd4035-bt-del-3) THEN 
   DO:
      CREATE BUTTON wh-cd4035-bt-gerar-nf-adc
      ASSIGN ROW       = wh-cd4035-bt-del-3:ROW
             COLUMN    = wh-cd4035-bt-del-3:COL   + 10
             WIDTH     = wh-cd4035-bt-del-3:WIDTH 
             HEIGHT    = wh-cd4035-bt-del-3:HEIGHT
             FRAME     = wh-cd4035-bt-del-3:FRAME
             SENSITIVE = wh-cd4035-bt-del-3:SENSITIVE
             VISIBLE   = wh-cd4035-bt-del-3:VISIBLE
             LABEL     = "*Gerar Por NF"
             TOOLTIP   = "Gerar Por Nota Fiscal x Filtro"
             TRIGGERS:
                  ON CHOOSE PERSISTENT RUN upc/upc-cd4035-u01.p (INPUT "wh-cd4035-bt-gerar-nf-adc",   
                                                                 INPUT "upc-cd4035-u01"    ,  
                                                                 INPUT p-wgh-object        ,  
                                                                 INPUT p-wgh-frame         ,  
                                                                 INPUT p-cod-table         ,  
                                                                 INPUT p-row-table         ).     
             END TRIGGERS.             
   END.
END. 

IF  p-ind-event   = "wh-cd4035-bt-gerar-nf-adc" AND
    p-ind-object  = "upc-cd4035-u01"            THEN 
DO:
    CURRENT-WINDOW:SENSITIVE = NO.
    RUN upc/upc-cd0405a.w.
    CURRENT-WINDOW:SENSITIVE = YES.

    ASSIGN wh-cd4035-cb-tipo-inf:SCREEN-VALUE = "1".

    APPLY "VALUE-CHANGED" TO wh-cd4035-cb-tipo-inf.

    ASSIGN wh-cd4035-cb-tipo-inf:SCREEN-VALUE = "3".

    APPLY "VALUE-CHANGED" TO wh-cd4035-cb-tipo-inf.
END.
