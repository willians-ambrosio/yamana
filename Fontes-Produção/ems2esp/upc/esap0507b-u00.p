/*------------------------------------------------------------------------
    File        : esap0507b-u00
    Purpose     :

    Syntax      :

    Description : Programa de UPC

    Author(s)   : Bruno Bertulli
    Created     : 08/2013
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*----- DEFINICAO DE FUNCOES -----*/
{tools/fc-handle-obj.i}
{tools/fc-falso.i}
{utp/ut-glob.i}

/*----- DEFINICAO DE PARAMETROS -----*/
DEF INPUT PARAMETER p-ind-event   AS CHARACTER      NO-UNDO.
DEF INPUT PARAMETER p-ind-object  AS CHARACTER      NO-UNDO.
DEF INPUT PARAMETER p-wgh-object  AS HANDLE         NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame   AS WIDGET-HANDLE  NO-UNDO.
DEF INPUT PARAMETER p-cod-table   AS CHARACTER      NO-UNDO.
DEF INPUT PARAMETER p-row-table   AS ROWID          NO-UNDO.

/*----- DEFINICAO DE VARIAVEIS GLOBAIS -----*/

DEF NEW GLOBAL SHARED VAR wh-ap0507b-bt-modificar   AS WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-ap0507b-bt-copiar      AS WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-ap0507b-bt-eliminar    AS WIDGET-HANDLE  NO-UNDO.

/*-----DEFINICAO DE VARIAVEIS -----*/
DEF                   VAR c-handle-obj                AS CHARACTER      NO-UNDO.

/*-----DEFINICAO DE BUFFER ------*/

/*
MESSAGE
   'p-ind-event            ' p-ind-event            SKIP
   'p-ind-object           ' p-ind-object           SKIP
   'p-wgh-object:FILE-NAME ' p-wgh-object:FILE-NAME SKIP
   'p-wgh-frame:NAME       ' p-wgh-frame:NAME       SKIP
   'p-cod-table            ' p-cod-table            SKIP
   'STRING(p-row-table)    ' STRING(p-row-table)    SKIP
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

IF  p-ind-event  = 'CHANGE-PAGE' AND 
    p-ind-object = "CONTAINER"     THEN DO:

    ASSIGN c-handle-obj            = fc-handle-obj("bt-modificar,bt-copiar,bt-eliminar", p-wgh-frame).
    ASSIGN wh-ap0507b-bt-modificar = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
    ASSIGN wh-ap0507b-bt-copiar    = WIDGET-HANDLE(ENTRY(2,c-handle-obj)) NO-ERROR.
    ASSIGN wh-ap0507b-bt-eliminar  = WIDGET-HANDLE(ENTRY(3,c-handle-obj)) NO-ERROR.
    
    IF VALID-HANDLE(wh-ap0507b-bt-modificar) THEN
        wh-ap0507b-bt-modificar:SENSITIVE = NO.

    IF VALID-HANDLE(wh-ap0507b-bt-copiar) THEN
        wh-ap0507b-bt-copiar:SENSITIVE = NO.

    IF VALID-HANDLE(wh-ap0507b-bt-eliminar) THEN
        wh-ap0507b-bt-eliminar:SENSITIVE = NO.

END.

IF VALID-HANDLE(wh-ap0507b-bt-modificar) THEN
    wh-ap0507b-bt-modificar:SENSITIVE = NO.

IF VALID-HANDLE(wh-ap0507b-bt-copiar) THEN
    wh-ap0507b-bt-copiar:SENSITIVE = NO.

IF VALID-HANDLE(wh-ap0507b-bt-eliminar) THEN
    wh-ap0507b-bt-eliminar:SENSITIVE = NO.

