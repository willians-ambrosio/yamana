/********************************************************************
* cd9731.i - Objetivo: Verificar se integra‡Æo com o m¢dulo de 
                       investimento est  dispon¡vel.
*                Data: 04/01/2001
********************************************************************/
{cdp/cdcfgmat.i}

def var l-integra-cn-in as logical no-undo init no.

find first param-global no-lock no-error.

&IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
    if  param-global.modulo-in and param-global.modulo-cn then
        assign l-integra-cn-in = yes.
&else
    if param-global.modulo-in and param-global.modulo-cn then do:
       if can-find (funcao where 
                    funcao.cd-funcao = "integra_cn_in" and
                    funcao.ativo = yes) then
          assign l-integra-cn-in = yes.
    end.   
&ENDIF



/******** cd9731.i ************************************************/   
