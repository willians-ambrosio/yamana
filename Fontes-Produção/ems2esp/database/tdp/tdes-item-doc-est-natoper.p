/*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Gatilho de Dicionario.: DELETE/es-item-doc-est-natoper
** Data de Criaá∆o.......: 08/12/2009
**
*******************************************************************************/

TRIGGER PROCEDURE FOR DELETE OF es-item-doc-est-natoper.

{include/i-prgvrs.i TDes-item-doc-est-natoper 2.00.00.000}  /*** 010000 ***/











/** N∆o elimine a linha abaixo, Ç o indicador de vers∆o do gatilho **/
def var c-versao-mg97   as char init "01.00.00" no-undo.

/** Fim das validaá‰es de eliminaá∆o **/

/** N∆o elimine a linha abaixo, Ç a chamada EPC **/
{include/i-epc102.i es-item-doc-est-natoper}

/** Inicio da eliminaá∆o em cascata **/



/** C:/tmp/Triggesr2/ems2/database/tgad/tdp/tdad442.p **/

