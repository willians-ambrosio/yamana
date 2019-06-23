/*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

{include/i-prgvrs.i FT4004-upc 1.00.00.000}  /*** 010000 ***/
{include/i-epc200.i1}

/***************** Defini¯Êo de Parametros ************************************/
def input param p-ind-event      as char          no-undo.
def input param p-ind-object     as char          no-undo.
def input param p-wgh-object     as handle        no-undo.
def input param p-wgh-frame      as widget-handle no-undo.
def input param p-cod-table      as char          no-undo.
def input param p-row-table      as rowid         no-undo.

DEF NEW GLOBAL SHARED VAR p-wgh-object-ft4004 as handle        no-undo.

/* Buffers */
DEF BUFFER bf-wt-docto    FOR wt-docto.

/*
message 
"Evento..........:" string(p-ind-event)         skip  
        "Objeto..........:" string(p-ind-object)        skip      
        "Handle do Objeto:" string(p-wgh-object:NAME)   skip 
        "Handle da Frame.:" string(p-wgh-frame:NAME)    skip 
        "Tabela..........:" p-cod-table                 skip      
        "Rowid...........:" string(p-row-table)         skip      
        "p-wgh-object....:" p-wgh-object:file-name.*/


IF  p-ind-event             = "AFTER-DISPLAY"
AND string(p-ind-object)    = "CONTAINER"
AND p-wgh-object:file-name  = "ftp/ft4004.w" THEN DO:

    ASSIGN p-wgh-object-ft4004 = p-wgh-object.
END.
