/*****************************************************************************
**  Programa: esnfere2001-u00.p
**     Autor: Alberto Duzi
**      Data: 15/10/2009
** Descricao: UPC para Inserir Botao de Busca de DANFE para Digitacao
**            Automatica - Recebimento Fisico
** Altera‡Æo: 18/10/2009
******************************************************************************/

DEFINE INPUT PARAM p-ind-event      AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-ind-object     AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-wgh-object     AS HANDLE           NO-UNDO.
DEFINE INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE    NO-UNDO.
DEFINE INPUT PARAM p-cod-table      AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-row-table      AS ROWID            NO-UNDO.

/* --- Cria Botao para Busca de DANFE - Recebimento Fisico --- */
RUN dsc\ra\upc\esnfere2001-u01.p (INPUT p-ind-event,
                                  INPUT p-ind-object,
                                  INPUT p-wgh-object,
                                  INPUT p-wgh-frame,
                                  INPUT p-cod-table,
                                  INPUT p-row-table).

/*
RUN upc\re2001-upc.p (INPUT p-ind-event,
                      INPUT p-ind-object,
                      INPUT p-wgh-object,
                      INPUT p-wgh-frame,
                      INPUT p-cod-table,
                      INPUT p-row-table).
  */

 
