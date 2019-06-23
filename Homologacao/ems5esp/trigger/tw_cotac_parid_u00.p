/******************************************************************
**
** Programa: tw_cotac_parid_U00
**
** Objetivo: Trigger de Cota‡Æo de Moeda (Chamadora)
**
**    Autor: Willians Moreira Ambrosio - DKP
**
**     Data: Jan/2019
******************************************************************/
{include/i-prgvrs.i tw_cotac_parid_U00 12.01.21.000}

Def Parameter Buffer p_table     For COTAC_PARID.
Def Parameter Buffer p_old_table For COTAC_PARID.
/* ----------------------------------------------------------- */
RUN trigger/tw_cotac_parid_u01.p (BUFFER p_table    ,
                                  BUFFER p_old_table).

RETURN RETURN-VALUE.


