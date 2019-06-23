/********************************************************************************
** Copyright DATASUL S.A. (2008)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
**
**
** Autor: Marcos Rodrigo Minharo
** Finalidade: Tratamento do campo Centro de Custo, cujo formato:
**                    * release 2.11 ou superior: CHARACTER(20)
**                    * release inferior Ö 2.11: CHARACTER(08)
*******************************************************************************/
/* Include comentada porque o centro de custo com 20 d°gitos ainda n∆o foi liberado

{include/i_cdrel_hr.i}

&if "{&cd_rel_hr}"  < "2.11" &then "X(08)" &EndIF
&if "{&cd_rel_hr}" >= "2.11" &then "X(20)" &EndIF
*/

"X(08)"
