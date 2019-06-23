/*******************************************************************************
** Programa: boin028-upc.p
** Autor...: Log­stica (log339640)
** Data....: 06/2008
** OBS.....: UPC utilizada pelo BO boiv028
** Objetivo: acessar a tabela pedido de compra, verificar se este ² um pedido 
             emergencial, e caso afirmativo retornar "Yes" para o programa 
             BOIV028.p, caso contrÿrio retornar o valor que jÿ havia na 
             variÿvel l-erro-advert
*******************************************************************************/

/************************************************************************
**
**  i-prgvrs.i - Programa para criacao do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

/*Altera»’o - 08/09/2006 - tech1007 - Alterado para possuir a defini»’o dos pr²processadores logo no in­cio do programa*/
/**** Altera»’o efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
/*************************************************
* i_dbvers.i - Include de vers’o de banco de dados   
**************************************************/

/* Preprocessadores que identificam os bancos do Produto EMS 5 */

/* Preprocessadores que identificam os bancos do Produto EMS 2 */
/*RAC Incorporado na 2.04*/

/* Preprocessadores que identificam os bancos do Produto HR */
/*Esta include estÿ sendo liberada vazia para o EMS 2
 para n’o ocorrer erros de compila»’o*/
 


/* Fim */


/* Fim */
    

/* Fim */
    
.    
/* Fim */

 
/*Fim altera»’o 08/09/2006*/

def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.000[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.000"
       c-prg-obj = "boiv028-upc".

/* Alteracao - 02/10/2006 - Nakamura - Incluida chamada a include que verifica a integridade dos programas do registro de produto */
/*{include/i-ctrlrp.i {1}}*/

/*Altera»’o - 08/09/2006 - tech1007 - Altera»’o para exibir o nome do programa que executou o programa que serÿ exibido no extrato de vers’o
                                      Solicita»’o realizada na FO 1239827*/

/*Fim altera»’o 08/09/2006*/


if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "boiv028-upc"
        no-lock no-error.
        
   if not avail prog_dtsul then do:
          if  c-prg-obj begins "btb":U then
              assign c-prg-obj = "btb~/":U + c-prg-obj.
          else if c-prg-obj begins "men":U then
                  assign c-prg-obj = "men~/":U + c-prg-obj.
          else if c-prg-obj begins "sec":U then
                  assign c-prg-obj = "sec~/":U + c-prg-obj.
          else if c-prg-obj begins "utb":U then
                  assign c-prg-obj = "utb~/":U + c-prg-obj.
          find prog_dtsul where
               prog_dtsul.nom_prog_ext begins c-prg-obj no-lock no-error.
   end .            /*if*/
    
    output to value(c-arquivo-log) append.

    /*Altera»’o - 08/09/2006 - tech1007 - Altera»’o para exibir o nome do programa que executou o programa que serÿ exibido no extrato de vers’o
                                      Solicita»’o realizada na FO 1239827*/
    
        /*FO 1329.898 - tech1139 - 01/08/2006 */
        PUT "boiv028-upc" AT 1 "2.00.00.000" AT 69 STRING(TODAY,'99/99/99') AT 84 STRING(TIME,'HH:MM:SS':U) AT 94 SKIP.
        /*FO 1329.898 - tech1139 - 01/08/2006 */
    
    /*Fim altera»’o 08/09/2006*/
                                                  
    if  avail prog_dtsul then do:
        if  prog_dtsul.nom_prog_dpc <> "" then
            put "DPC : ":U at 5 prog_dtsul.nom_prog_dpc  at 12 skip.
        if  prog_dtsul.nom_prog_appc <> "" then
            put "APPC: ":U at 5 prog_dtsul.nom_prog_appc at 12 skip.
        if  prog_dtsul.nom_prog_upc <> "" then
            put "UPC : ":U at 5 prog_dtsul.nom_prog_upc  at 12 skip.
    end.
    output close.        
end.  
error-status:error = no.
/***************************************************
** i_dbtype.i - Tipo de Gerenciadores utilizados
***************************************************/


        
    /* Preprocessadores que identificam os bancos do Produto EMS 5 */
                    
    /* Preprocessadores que identificam os bancos do Produto EMS 2 */
                                                                        
    /* Preprocessadores que identificam os bancos do Produto HR 2 */
            

/* Fim */

 

/*alteracao Anderson(tech540) em 04/02/2003 Include com a definicao 
da temp table utilizada nas includes btb008za.i1 e btb008za.i2 para 
execucao de programas via rpc*/
def temp-table tt-control-prog NO-UNDO
    field cod-versao-integracao as integer       format '999'
    field cod-erro              as integer       format '99999'
    field desc-erro             as character     format 'x(60)'
    field wgh-servid-rpc        as widget-handle format '>>>>>>9'.
 
 
/*fim alteracao Anderson 04/02/2003*/

/* altera»’o feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */

DEFINE NEW GLOBAL SHARED VARIABLE hWenController AS HANDLE     NO-UNDO.
/*Constante utilizada apenas para verificar se a variÿvel acima foi definida */ 

/* fim da alatera»’o */

/* Altera»’o realizada por tech38629 - 19/07/2006 - Defini»’o do pre-processador para o facelift */
/****************************************************************/
/* i_fclpreproc.i                                               */
/* Criado por: tech38629                                        */
/* Data de cria»’o: 19/07/2006                                  */
/* Descri»’o: Define o pr²-processador que indica a utiliza»’o  */
/*            do facelift                                       */
/****************************************************************/

 
/* Fim da altera»’o */

   /*** 010000 ***/

  /***************************************************************
**
** I-EPC200.I - Include de defini¯Êo da temp-table padrÊo usada
**              para EPCs em Pontos Estratýgicos de programas
**          
** Parümetros : 
** {1}  - nome do programa a ser localizado na tabela prog_dtsul     
***************************************************************/

/* definicao da temp-table */
/***************************************************************
**
** I-EPC200.I1 - Padroniza a temp-table usada para os epcs
**
***************************************************************/ 

/* begin_temp_table_definition */

define temp-table tt-epc no-undo
   field cod-event     as char format "x(12)"
   field cod-parameter as char format "x(32)"
   field val-parameter as char format "x(54)"
   index  id is primary cod-parameter cod-event ascending.    
   
/* end_temp_table_definition */   
    

  def var c-nom-prog-dpc-mg97  as char init "" no-undo.   
  def var c-nom-prog-appc-mg97 as char init "" no-undo.
  def var c-nom-prog-upc-mg97  as char init "" no-undo.
  def var raw-rowObject        as raw          no-undo.

   
find prog_dtsul where prog_dtsul.cod_prog_dtsul = "" no-lock no-error.
if  avail prog_dtsul then do:
    assign c-nom-prog-dpc-mg97  = prog_dtsul.nom_prog_dpc
           c-nom-prog-appc-mg97 = prog_dtsul.nom_prog_appc
           c-nom-prog-upc-mg97  = prog_dtsul.nom_prog_upc.
end.          
/* i-epc200.i */
 

DEF INPUT PARAM p-ind-event  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

define variable nr-ped-compra like pedido-compr.num-pedido no-undo.
define variable l-erro-advert as   logical init no         no-undo.

if p-ind-event = "Validate" then do:
    for each  tt-epc no-lock 
        where tt-epc.cod-event = p-ind-event:

        if tt-epc.cod-parameter = "nr-ped-compra" then
            assign nr-ped-compra = int(tt-epc.val-parameter).

        if tt-epc.cod-parameter = "erro-advert" then
            assign l-erro-advert = logical(tt-epc.val-parameter).
    end.

    find first pedido-compr no-lock
         where pedido-compr.num-pedido = nr-ped-compra no-error.
    if avail pedido-compr then do:
        create tt-epc.
        assign tt-epc.cod-event     = p-ind-event
               tt-epc.cod-parameter = "return-erro-advert".
        if pedido-compr.emergencial then
            assign tt-epc.val-parameter = "yes".
        else assign tt-epc.val-parameter = string(l-erro-advert).
    end.
end.

