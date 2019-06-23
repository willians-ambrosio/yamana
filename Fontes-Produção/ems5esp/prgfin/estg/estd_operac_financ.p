/******************************************************************************/
/*  Empresa ...........: DSC PRAXIS                                           */
/*  Autor..............: Sergio Luiz Neto da Silveira                         */
/*  Data ..............: 06/09/2016                                           */
/*  Programa ..........: trigger/td-operac_financ.p                           */
/*  Objetivo ..........: Trigger de DELETE da tabela operac_financ            */
/******************************************************************************/

/*****************************************************************************/
/*     PARAMENTROS DE ENTRADA DOS OBJETOS,EVENTOS E TABELAS                  */
/*****************************************************************************/

{include/i-prgvrs.i td-operac_financ 12.1.17.000}
    
{utp/ut-glob.i} 

DEFINE PARAMETER BUFFER p-table FOR operac_financ.

FIND FIRST es_operac_financ OF p-table
     EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE(es_operac_financ) THEN 
    DELETE es_operac_financ.



RETURN "OK":U.
