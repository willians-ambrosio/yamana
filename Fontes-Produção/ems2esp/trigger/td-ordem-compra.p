/******************************************************************************/
/*  Empresa ...........: Datasul S’o Paulo                                    */
/*  Autor..............: Sergio Luiz Neto da Silveira                         */
/*  Data ..............: 06/09/2016                                           */
/*  Programa ..........: trigger/td-ordem-compra.p                            */
/*  Objetivo ..........: Trigger de Delete para criar LOG de elimina‡Æo de OC */
/******************************************************************************/

/*****************************************************************************/
/*     PARAMENTROS DE ENTRADA DOS OBJETOS,EVENTOS E TABELAS                  */
/*****************************************************************************/

{include/i-prgvrs.i td-ordem-compra 2.06.00.000}
    
{utp/ut-glob.i} 

DEFINE PARAMETER BUFFER p-table FOR ordem-compra.

CREATE es-ordem-compra-eliminada.
BUFFER-COPY p-table EXCEPT ep-codigo TO es-ordem-compra-eliminada.
ASSIGN es-ordem-compra-eliminada.data        = TODAY
       es-ordem-compra-eliminada.hora        = STRING(TIME,"hh:mm:ss")
       es-ordem-compra-eliminada.cod-usuario = c-seg-usuario
       es-ordem-compra-eliminada.ep-codigo   = INTEGER(p-table.ep-codigo).

RETURN "OK":U.


