/*******************************************************
** Autor...: Leonardo Correia Santos de Oliveira
** Empresa.: YAMANA
** Programa: mv0501-upc
** UPC cadastrada para programa: MV0501
** Objetivo: Imprimi rellatorio especifico
*******************************************************
*******************************************************
** Alteracao...: Thiago Coutinho
** Empresa.....: CSX Solution
** Programa....: mv0501-upc
** UPC cadastrada para programa: MV0501
** Objetivo: Migracao para 2.06 e deletar persistent
*******************************************************/

{include/i-prgvrs.i mv0501-upc 2.06.00.000}
/** Parƒmetros **/
def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-row-table  as rowid         no-undo.

def var c-objeto as CHARACTER      no-undo.
def var h_frame  as widget-handle  no-undo. 
DEF VAR h-handle AS HANDLE       NO-UNDO.

def new global shared var wh-button  as widget-handle no-undo.
def new global shared var wh-fill    as widget-handle no-undo.
def new global shared var tx-label   as widget-handle no-undo.
def new global shared var h_campo    as widget-handle no-undo.

DEF NEW GLOBAL SHARED VAR h-fPage2            AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fPage4            AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fPage6            AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tgQuebraOM        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR lbQuebraPagina      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fi-ord-ini        AS WIDGET-HANDLE NO-UNDO.


DEF NEW GLOBAL SHARED VAR h-MV0501-upc-01     AS HANDLE         NO-UNDO.


if p-ind-event = "YAMANA":U then do:

    RETURN "NOK".
END.

/** Colocado para deletar objeto preso em mem¢ria **/
if p-ind-event = "BEFORE-DESTROY-INTERFACE":U then do:

    IF VALID-HANDLE(h-MV0501-upc-01) THEN
        DELETE PROCEDURE h-MV0501-upc-01.

    IF VALID-HANDLE(h-MV0501-upc-01) THEN
        ASSIGN h-MV0501-upc-01 = ?.
END.


IF p-ind-event = "AFTER-INITIALIZE":U THEN DO:
    RUN findWidget (INPUT  "fPage2",
                    INPUT  "FRAME",
                    INPUT  p-wgh-frame,
                    OUTPUT h-fPage2).
    RUN findWidget (INPUT  "fPage4",
                    INPUT  "FRAME",
                    INPUT  p-wgh-frame,
                    OUTPUT h-fPage4).
    RUN findWidget (INPUT  "fPage6",
                    INPUT  "FRAME",
                    INPUT  p-wgh-frame,
                    OUTPUT h-fPage6).
    RUN findWidget (INPUT  "tgQuebraOM",
                    INPUT  "TOGGLE-BOX",
                    INPUT  h-fPage6,
                    OUTPUT h-tgQuebraOM).
    RUN findWidget (INPUT  "fi-ord-ini",
                    INPUT  "FILL-IN",
                    INPUT  h-fPage2,
                    OUTPUT h-fi-ord-ini).
                    
    RUN hideText (INPUT "Estado", INPUT h-fPage4, OUTPUT h-handle).
    RUN hideText (INPUT "Situa‡Æo", INPUT h-fPage4, OUTPUT h-handle).
    
    RUN pi-hide-field (INPUT "tgTarefas", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgDurabilidade", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgObs", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgLinhas", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgManut", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgRequisicoes", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    
    /*RUN pi-hide-field (INPUT "fi-ord-ini", INPUT "FILL-IN", INPUT h-fPage1).*/
           
    RUN pi-hide-field (INPUT "fi-nr-linhas", INPUT "FILL-IN", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "fiNUmFicha",   INPUT "FILL-IN", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "fi-sub-sist",  INPUT "FILL-IN", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "fiDescSubSit", INPUT "FILL-IN", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgRequisicoes", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgEstoque", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgCompras", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgAprovado", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgReprovado", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgAberta", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgFechada", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgPendente", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "tgComOrdem", INPUT "TOGGLE-BOX", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "RECT-73", INPUT "RECTANGLE", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "RECT-74", INPUT "RECTANGLE", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "RECT-75", INPUT "RECTANGLE", INPUT h-fPage4).
    RUN pi-hide-field (INPUT "RECT-76", INPUT "RECTANGLE", INPUT h-fPage4).
    
    RUN pi-hide-field (INPUT "tgQuebraOM", INPUT "TOGGLE-BOX", INPUT h-fPage6).
    RUN pi-hide-field (INPUT "RECT-12", INPUT "RECTANGLE", INPUT h-fPage4).

    
    RUN mvp/mv0501-upc.p PERSISTENT SET h-MV0501-upc-01 (INPUT "",            
                                                      INPUT "",            
                                                      INPUT p-wgh-object,  
                                                      INPUT p-wgh-frame,   
                                                      INPUT "",            
                                                      INPUT p-row-table).
    
    CREATE TOGGLE-BOX lbQuebraPagina
        ASSIGN FRAME     = h-fPage4
               ROW       = 8.85
               COL       = 11
               WIDTH     = 21.00
               HEIGHT    = 0.83
               NAME      = "tgQuebraPagina"
               VISIBLE   = YES
               SENSITIVE = YES
               LABEL     = "Inserir Quebra de P gina"
               TRIGGERS:
               ON VALUE-CHANGED PERSISTENT RUN pi-executa IN h-MV0501-upc-01.
               END TRIGGERS.       
    h-fPage2:MOVE-TO-TOP().
               

END.



PROCEDURE pi-executa:
    ASSIGN h-tgQuebraOM:CHECKED = lbQuebraPagina:CHECKED.
END.


PROCEDURE pi-hide-field:
    DEFINE INPUT  PARAMETER c-nome          AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER c-tipo-campo    AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER h-page          AS HANDLE     NO-UNDO.
    
    DEFINE VARIABLE h-handle-campo          AS HANDLE       NO-UNDO.
    
    RUN findWidget (INPUT  c-nome,
                    INPUT  c-tipo-campo,
                    INPUT  h-page,
                    OUTPUT h-handle-campo).
    ASSIGN h-handle-campo:VISIBLE = NO.
END.

PROCEDURE findWidget:
    
    DEFINE INPUT  PARAMETER c-widget-name  AS CHAR   NO-UNDO.
    DEFINE INPUT  PARAMETER c-widget-type  AS CHAR   NO-UNDO.
    DEFINE INPUT  PARAMETER h-start-widget AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER h-widget       AS HANDLE NO-UNDO.
    
    DO WHILE VALID-HANDLE(h-start-widget):
        if h-start-widget:NAME = c-widget-name AND
           h-start-widget:TYPE = c-widget-type THEN DO:
            ASSIGN h-widget = h-start-widget:HANDLE.
            LEAVE.
        END.
    
        IF h-start-widget:TYPE = "field-group":u OR
           h-start-widget:TYPE = "frame":u OR
           h-start-widget:TYPE = "dialog-box":u THEN DO:
            RUN findWidget (INPUT  c-widget-name,
                            INPUT  c-widget-type,
                            INPUT  h-start-widget:FIRST-CHILD,
                            OUTPUT h-widget).
    
            IF VALID-HANDLE(h-widget) THEN
                LEAVE.
        END.
        ASSIGN h-start-widget = h-start-widget:NEXT-SIBLING.
    END.
END PROCEDURE.


PROCEDURE hideText:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER c-text  AS CHAR   NO-UNDO.
    DEFINE INPUT  PARAMETER h-start-widget AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER h-widget       AS HANDLE NO-UNDO.
    
    DO WHILE VALID-HANDLE(h-start-widget):
        IF h-start-widget:TYPE = "LITERAL":U AND h-start-widget:SCREEN-VALUE = c-text THEN DO:
            ASSIGN h-start-widget:VISIBLE = NO.
        END.
    
        IF h-start-widget:TYPE = "field-group":u OR
           h-start-widget:TYPE = "frame":u OR
           h-start-widget:TYPE = "dialog-box":u THEN DO:
            RUN hideText (INPUT c-text,
                          INPUT h-start-widget:FIRST-CHILD,
                          OUTPUT h-widget).
    
            IF VALID-HANDLE(h-widget) THEN
                LEAVE.
        END.

        ASSIGN h-start-widget = h-start-widget:NEXT-SIBLING.
    END.
END PROCEDURE.

