/*------------------------------------------------------------------------
    File        : ap1001-u01
    Purpose     :

    Syntax      :

    Description : Programa de UPC

    Author(s)   : Bruno Bertulli
    Created     : 10/2012
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
def buffer portador for ems5.portador.

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

DEF NEW GLOBAL SHARED VAR wh-ap1001-i-portador      AS WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-ap1001-i-conta-cosmos  AS WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-ap1001-cb-filiais      AS WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-ap1001-da-pagto        AS WIDGET-HANDLE  NO-UNDO. 

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


    ASSIGN c-handle-obj             = fc-handle-obj("i-portador,i-conta-cosmos,cb-filiais,da-pagto", p-wgh-frame).
    ASSIGN wh-ap1001-i-portador     = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
    ASSIGN wh-ap1001-i-conta-cosmos = WIDGET-HANDLE(ENTRY(2,c-handle-obj)) NO-ERROR.
    ASSIGN wh-ap1001-cb-filiais     = WIDGET-HANDLE(ENTRY(3,c-handle-obj)) NO-ERROR.
    ASSIGN wh-ap1001-da-pagto       = WIDGET-HANDLE(ENTRY(4,c-handle-obj)) NO-ERROR.
    
    IF VALID-HANDLE(wh-ap1001-da-pagto) THEN DO:
        wh-ap1001-da-pagto:SENSITIVE = YES.
    END.

    /*
    MESSAGE 'wh-ap1001-i-conta-cosmos ' VALID-HANDLE (wh-ap1001-i-conta-cosmos)
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        */

    IF VALID-HANDLE (wh-ap1001-i-portador) THEN DO:
        find first portador
             where portador.cod_portador = wh-ap1001-i-portador:SCREEN-VALUE no-lock no-error.
        IF AVAILABLE portador THEN DO:
            if  portador.cod_banco = "745" then do:
                IF VALID-HANDLE (wh-ap1001-i-conta-cosmos) = YES THEN DO:
                    wh-ap1001-i-conta-cosmos:SENSITIVE = NO.
                    wh-ap1001-cb-filiais:SENSITIVE = NO.
                    wh-ap1001-cb-filiais:SCREEN-VALUE = '000 - Nenhum'.

                    FIND FIRST es-conta-cosmos NO-LOCK
                        WHERE es-conta-cosmos.ep-codigo = i-ep-codigo-usuario NO-ERROR.
                    IF AVAILABLE es-conta-cosmos THEN DO:
                        wh-ap1001-i-conta-cosmos:SCREEN-VALUE = STRING (es-conta-cosmos.conta-cosmos).
                    END.
                    ELSE DO:
                        wh-ap1001-i-conta-cosmos:SCREEN-VALUE = '000000000'.
                        run utp/ut-msgs.p (input "show",
                                           input 17006,
                                           input "Empresa < " + i-ep-codigo-usuario + " > n∆o cadastrada no programa espec°fico de Conta Cosmos ESAP003 !").
                    END.
                END.
            END.
            ELSE DO:
                IF VALID-HANDLE (wh-ap1001-i-conta-cosmos) = YES THEN 
                    wh-ap1001-i-conta-cosmos:SCREEN-VALUE = '000000000'.
            END.
        END.
        ELSE DO:
            IF VALID-HANDLE (wh-ap1001-i-conta-cosmos) = YES THEN 
                wh-ap1001-i-conta-cosmos:SCREEN-VALUE = '000000000'.
        END.
    END.
END.


