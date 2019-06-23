/*****************************************************************************************
***
**       Programa: prghur/upc/twpy102-u00.p
**
**       Data....: Out/2018
**
**       Cliente.: YAMANA
**
**       Autor...: Willians Ambrosio - Grupo DKP
**
**       Objetivo: Envio de Email x Altera‡Æo de Cargo
******************************************************************************************/
{include/buffers_rh.i}

{include/i-prgvrs.i twpy102-u00 12.1.21.001}

def parameter buffer p-table     for histor_sal_func.
def parameter buffer p-old-table for histor_sal_func.


RUN prghur/upc/twpy102-u01.p (BUFFER p-table,
                              BUFFER p-old-table).

RETURN "OK".
