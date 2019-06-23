/*******************************************************
** Autor...: Gustavo Eduardo Tamanini
** Empresa.: Yamana
** Programa: MV0104-UPC
** UPC cadastrada para programa: MV0104
** Objetivo: 
*******************************************************/
{include/i-prgvrs.i MV0104-UPC 2.06.00.000}
/** Parƒmetros **/
def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-row-table  as rowid         no-undo.

/** Widget-handle **/
define new global shared var wh-EspFunc-mv0104 as widget-handle no-undo.
/** Handle **/
define variable h-mv0104-upc as handle no-undo.

if  p-ind-event  = "AFTER-INITIALIZE":U and
    p-ind-object = "CONTAINER":U        then do:

    run mvp/mv0104-upc.p persistent set h-mv0104-upc (input "",
                                                      input "",
                                                      input p-wgh-object,
                                                      input p-wgh-frame,
                                                      input "",
                                                      input p-row-table).

    /** Cria BotÆo para a vincula‡Æo da especilidade ao funcion rio **/
    create button wh-EspFunc-mv0104
    assign frame              = p-wgh-frame
           row                = 1.13
           col                = 70.72
           width              = 4
           height             = 1.18
           CONVERT-3D-COLORS  = NO
           visible            = yes
           sensitive          = yes
           name               = "wh-EspFunc-mv0104":U
           triggers:
                on choose persistent run piAbrePrograma in h-mv0104-upc.
           end triggers.
    wh-EspFunc-mv0104:load-image("image\toolbar\im-forne").
end.

procedure piAbrePrograma:
    run mvp/esmv0111.w.
end procedure.

/*------------------------------------------------------------------------------
  purpose:  getFildHandle
    notes:  Procedure responsavel para fazer pesquisa de campos em tela
------------------------------------------------------------------------------*/
PROCEDURE getFildHandle:
    /* defini‡Æo de parƒmetros */
    DEFINE INPUT  PARAMETER  pWghFrame    AS WIDGET-HANDLE NO-UNDO.
    DEFINE INPUT  PARAMETER  pIndEvent    AS CHARACTER     NO-UNDO.
    DEFINE INPUT  PARAMETER  pObjType     AS CHARACTER     NO-UNDO.
    DEFINE INPUT  PARAMETER  pObjName     AS CHARACTER     NO-UNDO.
    DEFINE INPUT  PARAMETER  pApresMsg    AS LOGICAL       NO-UNDO.
    DEFINE OUTPUT PARAMETER  phObj        AS HANDLE        NO-UNDO.
    /* defini‡Æo */
    DEFINE VARIABLE wgh-obj AS WIDGET-HANDLE NO-UNDO.

    ASSIGN wgh-obj = pWghFrame:FIRST-CHILD.

    DO  WHILE VALID-HANDLE(wgh-obj):              
        IF  pApresMsg = YES THEN
            MESSAGE "Nome do Objeto" wgh-obj:NAME SKIP
                    "Type do Objeto" wgh-obj:TYPE SKIP
                    "P-Ind-Event"    pIndEvent VIEW-AS ALERT-BOX.
        IF  wgh-obj:TYPE = pObjType AND
            wgh-obj:NAME = pObjName THEN DO:
            ASSIGN phObj = wgh-obj:HANDLE.
            LEAVE.
        END.
        IF  wgh-obj:TYPE = "field-group" THEN
            ASSIGN wgh-obj = wgh-obj:FIRST-CHILD.
        ELSE 
            ASSIGN wgh-obj = wgh-obj:NEXT-SIBLING.
    END.
END PROCEDURE.
