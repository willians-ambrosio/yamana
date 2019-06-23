/******************************************************************************/
/*  Empresa ...........: Datasul S’o Paulo                                    */
/*  Autor..............: Sergio Luiz Neto da Silveira                         */
/*  Data ..............: 06/09/2016                                           */
/*  Programa ..........: trigger/td-pedido-compr.p                            */
/*  Objetivo ..........: Trigger de Delete para criar LOG de elimina‡Æo de PC */
/******************************************************************************/

/*****************************************************************************/
/*     PARAMENTROS DE ENTRADA DOS OBJETOS,EVENTOS E TABELAS                  */
/*****************************************************************************/

{include/i-prgvrs.i td-pedido-compr 2.06.00.000}
    
{utp/ut-glob.i} 

DEFINE PARAMETER BUFFER p-table FOR pedido-compr.

CREATE es-pedido-compr-eliminado.
BUFFER-COPY p-table TO es-pedido-compr-eliminado.
ASSIGN es-pedido-compr-eliminado.data        = TODAY
       es-pedido-compr-eliminado.hora        = STRING(TIME,"hh:mm:ss")
       es-pedido-compr-eliminado.cod-usuario = c-seg-usuario.

RETURN "OK":U.
