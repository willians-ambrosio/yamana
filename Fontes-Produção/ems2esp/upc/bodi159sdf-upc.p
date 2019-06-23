/*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/************************************************************************
**
{include/i-prgvrs.i BODI159SDF-UPC-SUG-NATUR 2.00.00.004 } /*** 010004 ***/
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

/*************************************************
* i_prdvers.i - Include de versão do produto   
**************************************************/


/* Fim */

 

/*Altera‡Æo - 08/09/2006 - tech1007 - Alterado para possuir a defini‡Æo dos pr‚processadores logo no in¡cio do programa*/
/**** Altera‡Æo efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
/*************************************************
* i_dbvers.i - Include de versão de banco de dados   
**************************************************/

/* Preprocessadores que identificam os bancos do Produto EMS 5 */

/* Preprocessadores que identificam os bancos do Foundation */

/* Preprocessadores que identificam os bancos do Produto EMS 2 */

/* Preprocessadores que identificam os bancos do Produto HR 2 */

/* Fim */
 
/*Fim altera‡Æo 08/09/2006*/


/**************************************************************************************************************/
/**************************************************************************************************************/
/**************************************************************************************************************/
/*INI - Chamada da SuperProcedure respons vel por setar as Informa‡äes do Contexto de SessÆo do Usu rios (tech14043)*/

Define Variable hSPContexto             As Handle       No-undo.
Define Variable hSession                As Handle       No-undo.
Define Variable isbCont                 As Integer      No-undo.
Define Variable cToken                  As Character    No-undo.

/*INI - Verificar se a SP ja foi carregado */
Do isbCont = 1 TO Num-entries(Session:Super-procedures):
    hSession = Widget-handle(Entry(isbCont,Session:Super-procedures)).
    If hSession:Name = "utp/ut-GlobalSession.p"
    Then Do:
        hSPContexto = hSession.
    End.
End.
/*FIM - Verificar se a SP ja foi carregado */

If Not Valid-handle(hSPContexto)
Then Do:
    Run utp/ut-globalsession.p Persistent Set hSPContexto.
    Session:Add-super-procedure (hSPContexto).
End.
 

/*FIM - Chamada da SuperProcedure respons vel por setar as Informa‡äes do Contexto de SessÆo do Usu rios (tech14043)*/
/**************************************************************************************************************/
/**************************************************************************************************************/
/**************************************************************************************************************/


def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.001[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.001"
       c-prg-obj = "BODI159SDF-UPC-SUG-NATUR".

/* Alteracao - 02/10/2006 - Nakamura - Incluida chamada a include que verifica a integridade dos programas do registro de produto */
/*{include/i-ctrlrp.i {1}}*/

/*Altera‡Æo - 08/09/2006 - tech1007 - Altera‡Æo para exibir o nome do programa que executou o programa que ser  exibido no extrato de versÆo
                                      Solicita‡Æo realizada na FO 1239827*/

def var c-prg-obj-pai as char no-undo.

IF VALID-HANDLE(THIS-PROCEDURE:INSTANTIATING-PROCEDURE) THEN DO:
    ASSIGN c-prg-obj-pai = THIS-PROCEDURE:INSTANTIATING-PROCEDURE:FILE-NAME.
END.
ELSE DO:
    ASSIGN c-prg-obj-pai = "".
END.

IF NUM-ENTRIES(c-prg-obj-pai, "~/") > 1 THEN DO:
    ASSIGN c-prg-obj-pai = ENTRY(NUM-ENTRIES(c-prg-obj-pai, "~/"),c-prg-obj-pai, "~/").
END.
IF NUM-ENTRIES(c-prg-obj-pai, ".") > 1 THEN DO:
    ASSIGN c-prg-obj-pai = ENTRY(1,c-prg-obj-pai, ".").
END.

/*Fim altera‡Æo 08/09/2006*/


if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "BODI159SDF-UPC-SUG-NATUR"
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

    /*Altera‡Æo - 08/09/2006 - tech1007 - Altera‡Æo para exibir o nome do programa que executou o programa que ser  exibido no extrato de versÆo
                                      Solicita‡Æo realizada na FO 1239827*/
    
        PUT "BODI159SDF-UPC-SUG-NATUR" AT 1 "2.00.00.001" AT 39 c-prg-obj-pai AT 54 STRING(TODAY,'99/99/99') AT 84 STRING(TIME,'HH:MM:SS':U) AT 94 SKIP.
    
    /*Fim altera‡Æo 08/09/2006*/

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


/* altera‡Æo feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */

DEFINE NEW GLOBAL SHARED VARIABLE hWenController AS HANDLE     NO-UNDO.
/*Constante utilizada apenas para verificar se a vari vel acima foi definida */ 

/* fim da alatera‡Æo */

/* Altera‡Æo realizada por tech38629 - 19/07/2006 - Defini‡Æo do pre-processador para o facelift */
/****************************************************************/
/* i_fclpreproc.i                                               */
/* Criado por: tech38629                                        */
/* Data de cria‡Æo: 19/07/2006                                  */
/* Descri‡Æo: Define o pr‚-processador que indica a utiliza‡Æo  */
/*            do facelift                                       */
/****************************************************************/

 
/* Fim da altera‡Æo */

  /*** 010001 ***/

  /***************************************************************
**
** I-EPC200.I - Include de defini»’o da temp-table padr’o usada
**              para EPCs em Pontos Estrat²gicos de programas
**          
** Par³metros : 
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
  /*Defini‡Æo tt-EPC*/
/*
**  BODI159.i
**
**  ped-venda - Pedidos de Venda
**
**  Ultima altera‡Æo : 31/03/99 - GeraBO
*/
 
DEFINE TEMP-TABLE tt-ped-venda no-undo like ped-venda
    field r-rowid  as rowid.
 
  /*Defini‡Æo Temp-table DBO*/

DEF INPUT PARAM p-ind-event AS CHAR   NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEF VAR h-BO                 AS HANDLE NO-UNDO.
DEF VAR h-boesp              AS HANDLE NO-UNDO.
DEF VAR c-natur-oper-pedido  AS CHARACTER NO-UNDO.

IF  p-ind-event = "AfterSetDefaultCustomer" THEN DO:

    FOR FIRST tt-epc
        WHERE tt-epc.cod-event = p-ind-event
        AND   tt-epc.cod-parameter = "OBJECT-HANDLE" NO-LOCK:

        ASSIGN h-bo = WIDGET-HANDLE(tt-epc.val-parameter).

        RUN outputTable IN h-bo (OUTPUT TABLE tt-ped-venda).

        FOR FIRST tt-ped-venda:

            RUN esbo/boes001.p PERSISTENT SET h-boesp.

            RUN getNaturezaPedido2 IN h-boesp (INPUT tt-ped-venda.nome-abrev, 
                                               INPUT tt-ped-venda.nr-pedcli, 
                                               INPUT tt-ped-venda.cod-estabel, 
                                               INPUT TABLE tt-ped-venda,
                                               OUTPUT c-natur-oper-pedido).

            IF c-natur-oper-pedido <> "" THEN
                ASSIGN tt-ped-venda.nat-operacao = c-natur-oper-pedido.

            RUN inputTable IN h-bo (INPUT TABLE tt-ped-venda).

            DELETE OBJECT h-boesp.
            ASSIGN h-boesp = ?.
        END.
    END.    
END.


