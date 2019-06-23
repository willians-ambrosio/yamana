/*-----------------------------------------------------------------------------------
    PROGRAMA : tw-ext-nota-fiscal-nfe.p
    OBJETIVO : Trigger de White para a tabela  ext-nota-fiscal-nfe
    AUTOR    : Wellington Aparecido (DSC)
    DATA     : 08/08/2008
-----------------------------------------------------------------------------------*/
{include/i-prgvrs.i tw-ext-nota-fiscal-nfe.P 2.06.00.001}

/* Variaveis de Parƒmetros */ 
DEF PARAMETER BUFFER p-table     FOR ext-nota-fiscal-nfe.
DEF PARAMETER BUFFER p-old-table FOR ext-nota-fiscal-nfe.      

/*** Processando, Autorizadas ou contingenciadas ***/
IF(p-table.codigo = 1  OR
   p-table.codigo = 2  OR 
   p-table.codigo = 10)THEN DO: 
  /* ---> AUTORIZADO <--- */
    FIND FIRST nota-fiscal EXCLUSIVE-LOCK
         WHERE nota-fiscal.cod-estabel = p-table.cod-estabel
           AND nota-fiscal.serie       = p-table.serie
           AND nota-fiscal.nr-nota-fis = p-table.nr-nota-fis NO-ERROR.
    ASSIGN nota-fiscal.ind-sit-nota = 2.
END.

