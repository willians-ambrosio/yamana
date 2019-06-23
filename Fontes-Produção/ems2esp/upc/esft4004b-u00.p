/**============================================================**
** Altera»’o...: 
** Empresa.....: Sergio Luiz Neto da Silveira / DSC
** Data........: 02/02/2015
** Objetivo....: UPC na tela do programa FT4004
** ............:  
**=============================================================**/
{include/i-prgvrs.i ft4004-u00 2.06.00.001}


/** Par³metros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

RUN esupc/esft4004b-u01.p(INPUT p-ind-event,
                          INPUT p-ind-object,
                          INPUT p-wgh-object,
                          INPUT p-wgh-frame,
                          INPUT p-cod-table,
                          INPUT p-row-table).

RUN esupc/esft4004b-u02.p(INPUT p-ind-event,
                          INPUT p-ind-object,
                          INPUT p-wgh-object,
                          INPUT p-wgh-frame,
                          INPUT p-cod-table,
                          INPUT p-row-table).

