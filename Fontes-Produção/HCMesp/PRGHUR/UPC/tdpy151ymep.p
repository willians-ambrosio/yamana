/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_rh.i}

{include/i-prgvrs.i tdpy151ymep 12.1.13.001}

/*****************************************************************************************
***
**       Programa: prghur/upc/tdpy151ymep.p
**
**       Data....: Maio 2017
**
**       Cliente.: YAMANA
**       Objetivo: Eliminar relacionamentos da tabela habilit_rescis
******************************************************************************************/

DEFINE PARAMETER BUFFER p-table FOR habilit_rescis.


FIND FIRST func_desligto                                                                        
     WHERE func_desligto.cdn_empresa     = p-table.cdn_empresa     AND                                            
           func_desligto.cdn_estab       = p-table.cdn_estab       AND                                           
           func_desligto.cdn_funcionario = p-table.cdn_funcionario
     EXCLUSIVE-LOCK NO-ERROR.                                                        
IF AVAILABLE(func_desligto) THEN  
   DELETE func_desligto.                                           

RETURN "OK":U.
