/*-----------------------------------------------------------------------------------
    PROGRAMA : tw-emitente.p
    OBJETIVO : Trigger de Write para a tabela emitente
    AUTOR    : Thiago Coutinho (CSX)
    DATA     : 10/05/2012
-----------------------------------------------------------------------------------*/
{include/i-prgvrs.i tw-emitente.p 2.06.00.001}

/* Variaveis de Parƒmetros */ 
DEF PARAMETER BUFFER p-table     FOR emitente.
DEF PARAMETER BUFFER p-old-table FOR emitente.      

/*** cria novos resgistros ***/
FIND FIRST dist-emitente
     WHERE dist-emitente.cod-emitente = p-table.cod-emitente NO-ERROR.
IF NOT AVAIL dist-emitente THEN DO:
    CREATE dist-emitente.
    ASSIGN dist-emitente.cod-emitente = p-table.cod-emitente.
END.
