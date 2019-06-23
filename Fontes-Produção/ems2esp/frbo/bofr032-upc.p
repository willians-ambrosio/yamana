/*******************************************************
** Autor...: Leonardo Correia Santos de Oliveira
** Empresa.: Manufatura
** Programa: BRFR032
** UPC     : BRFR032-UPC
** Objetivo: 
*******************************************************/
{include/i-prgvrs.i BRFR032-UPC 2.06.00.000}  /*** 010000 ***/
{include/i-epc200.i}

/** Globais **/
def input param p-ind-event  as char no-undo.
def input-output param table for tt-epc.

def new global shared var wh-cod-Matr           as widget-handle no-undo.
def new global shared var wh-ep-codigo          as widget-handle no-undo.
def new global shared var wh-cod-estabel        as widget-handle no-undo.
def new global shared var wh-fi-cod-especialid  as widget-handle no-undo.
def new global shared var wh-fi-descricao       as widget-handle no-undo.

DEF VAR c-cod-matr  AS CHAR NO-UNDO.
DEF VAR i-ep-codigo AS INT  NO-UNDO.

/** Variaveis **/
def var h-bo as handle no-undo.

IF  p-ind-event = "afterUpdateRecord":U OR
    p-ind-event = "afterCreateRecord":U THEN DO:

    /** Busca tabela de Extens∆o para Atualizar o Numero da Especialidade OU
        Criar novo registro caso n∆o exista, para armazenar Numero da Especialidade  **/
    FOR FIRST mmv-func-relac-especialid
        WHERE mmv-func-relac-especialid.cod-matr      = wh-cod-Matr:SCREEN-VALUE
        AND mmv-func-relac-especialid.ep-codigo       = wh-ep-codigo:SCREEN-VALUE EXCLUSIVE-LOCK:
        ASSIGN mmv-func-relac-especialid.cod-especialid = wh-fi-cod-especialid:SCREEN-VALUE
               mmv-func-relac-especialid.cod-matr    = wh-cod-Matr:SCREEN-VALUE
               mmv-func-relac-especialid.ep-codigo   = wh-ep-codigo:SCREEN-VALUE
               mmv-func-relac-especialid.cod-estabel = wh-cod-estabel:SCREEN-VALUE.
    END.
        
    IF NOT AVAIL mmv-func-relac-especialid THEN DO:
          CREATE mmv-func-relac-especialid.
          ASSIGN mmv-func-relac-especialid.cod-matr        = wh-cod-Matr:SCREEN-VALUE
                 mmv-func-relac-especialid.ep-codigo       = wh-ep-codigo:SCREEN-VALUE
                 mmv-func-relac-especialid.cod-especialid  = wh-fi-cod-especialid:SCREEN-VALUE
                 mmv-func-relac-especialid.cod-estabel     = wh-cod-estabel:SCREEN-VALUE.
    END.
END.

if  p-ind-event = "beforeUpdateRecord":U or
    p-ind-event = "beforeCreateRecord":U then do:
    find first tt-epc
         where tt-epc.cod-event     = p-ind-event
         and   tt-epc.cod-parameter = "OBJECT-HANDLE" no-lock no-error.
    if avail tt-epc then do:
        /** Handle da BOFR032 **/
        assign h-bo = widget-handle(tt-epc.val-parameter).

        run getCharField in h-bo (input "cod-matr":U,
                                  output c-cod-matr).

        run getIntField in h-bo (input "ep-codigo":U,
                                 output i-ep-codigo).

        if wh-fi-cod-especialid:SCREEN-VALUE <> ""  then do:
            /** 1 - Retorna Erro caso NAO encontre Especialidade cadastrado **/
            if not can-find(first mmv-especialid-func where
                                  mmv-especialid-func.cod-especialid = wh-fi-cod-especialid:SCREEN-VALUE no-lock) then do:
                create tt-epc.
                assign tt-epc.cod-event     = "ERROR"
                       tt-epc.cod-parameter = "EPC-ERROR":U
                       tt-epc.val-parameter = "Especialidade " + wh-fi-cod-especialid:SCREEN-VALUE + " n∆o cadastrada.".
            end.
        END.

        /**  L¢gica para gravar os dados**/
/*         if wh-fi-cod-especialid:SCREEN-VALUE = "" then do:                                   */
/*                 create tt-epc.                                                               */
/*                 assign tt-epc.cod-event     = "ERROR"                                        */
/*                        tt-epc.cod-parameter = "EPC-ERROR":U                                  */
/*                        tt-epc.val-parameter = "Especialidade deve ser diferente de branco.". */
/*                                                                                              */
/*         END.                                                                                 */
    END.
END.

/** Deleta tabela especifica **/
if p-ind-event = "afterDeleteRecord":U then do:    
    FOR FIRST mmv-func-relac-especialid
        WHERE mmv-func-relac-especialid.cod-matr      = wh-cod-Matr:SCREEN-VALUE
        AND mmv-func-relac-especialid.ep-codigo       = wh-ep-codigo:SCREEN-VALUE EXCLUSIVE-LOCK:
        ASSIGN mmv-func-relac-especialid.cod-especialid = wh-fi-cod-especialid:SCREEN-VALUE
               mmv-func-relac-especialid.cod-matr       = wh-cod-Matr:SCREEN-VALUE
               mmv-func-relac-especialid.ep-codigo      = wh-ep-codigo:SCREEN-VALUE
               mmv-func-relac-especialid.cod-estabel    = wh-cod-estabel:SCREEN-VALUE.

        delete mmv-func-relac-especialid.
    END.
END.
