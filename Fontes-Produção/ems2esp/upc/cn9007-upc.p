/*****************************************************************************************
** Programa: 
** Autor...: Log¡stica (log339640)
** Data....: 07/2008
** OBS.....: 
** Objetivo: 
*****************************************************************************************/

{include/i-prgvrs.i cn9007-upc 2.00.00.000}  /*** 010000 ***/

{include/i-epc200.i}

DEF INPUT PARAM p-ind-event  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

def new global shared var c-alter-origem  as character format "x(76)" no-undo.
def new global shared var c-alterado      as character format "x(76)" no-undo.


if p-ind-event = "Atualiza-valores" then do:
    create tt-epc.
    assign tt-epc.cod-event     = "Novos-Valores"
           tt-epc.cod-parameter = "de-val-origem"
           tt-epc.val-parameter = c-alter-origem.

    create tt-epc.
    assign tt-epc.cod-event     = "Novos-Valores"
           tt-epc.cod-parameter = "de-val-atual"
           tt-epc.val-parameter = c-alterado.
end.
