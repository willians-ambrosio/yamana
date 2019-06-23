/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa         for ems2cadme.empresa.

/************************************************************************
**
**  i-prgvrs.i - Programa para criacao do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

/*Alteraá∆o - 08/09/2006 - tech1007 - Alterado para possuir a definiá∆o dos prÇprocessadores logo no in°cio do programa*/
/**** Alteraá∆o efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
/*************************************************
* i_dbvers.i - Include de vers∆o de banco de dados   
**************************************************/

/* Preprocessadores que identificam os bancos do Produto EMS 5 */

/* Preprocessadores que identificam os bancos do Produto EMS 2 */
/*RAC Incorporado na 2.04*/

/* Preprocessadores que identificam os bancos do Produto HR */
/*Esta include est† sendo liberada vazia para o EMS 2
 para n∆o ocorrer erros de compilaá∆o*/
 


/* Fim */


/* Fim */
    

/* Fim */
    
.    
/* Fim */

 
/*Fim alteraá∆o 08/09/2006*/

def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.026[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.026"
       c-prg-obj = "ESIN0803RP".

/* Alteracao - 02/10/2006 - Nakamura - Incluida chamada a include que verifica a integridade dos programas do registro de produto */
/*{include/i-ctrlrp.i {1}}*/

/*Alteraá∆o - 08/09/2006 - tech1007 - Alteraá∆o para exibir o nome do programa que executou o programa que ser† exibido no extrato de vers∆o
                                      Solicitaá∆o realizada na FO 1239827*/

/*Fim alteraá∆o 08/09/2006*/


if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "ESIN0803RP"
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

    /*Alteraá∆o - 08/09/2006 - tech1007 - Alteraá∆o para exibir o nome do programa que executou o programa que ser† exibido no extrato de vers∆o
                                      Solicitaá∆o realizada na FO 1239827*/
    
        /*FO 1329.898 - tech1139 - 01/08/2006 */
        PUT "ESIN0803RP" AT 1 "2.00.00.026" AT 69 STRING(TODAY,'99/99/99') AT 84 STRING(TIME,'HH:MM:SS':U) AT 94 SKIP.
        /*FO 1329.898 - tech1139 - 01/08/2006 */
    
    /*Fim alteraá∆o 08/09/2006*/
                                                  
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

/* alteraá∆o feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */

DEFINE NEW GLOBAL SHARED VARIABLE hWenController AS HANDLE     NO-UNDO.
/*Constante utilizada apenas para verificar se a vari†vel acima foi definida */ 

/* fim da alateraá∆o */

/* Alteraá∆o realizada por tech38629 - 19/07/2006 - Definiá∆o do pre-processador para o facelift */
/****************************************************************/
/* i_fclpreproc.i                                               */
/* Criado por: tech38629                                        */
/* Data de criaá∆o: 19/07/2006                                  */
/* Descriá∆o: Define o prÇ-processador que indica a utilizaá∆o  */
/*            do facelift                                       */
/****************************************************************/

 
/* Fim da alteraá∆o */

   /*** 010026 ***/
 
/*******************************************************************************
**
**   Programa: ESIN0803.P
**
**   Data....: Fevereiro de 1996 
**
**   Autor...: DATASUL S.A.
**
**   Objetivo: Posicionamento Economico                    
**
******************************************************************************/

/*****************************************************************************
**
**  I-RPVAR.I - Variaveis para Impressío do Cabecalho Padrío (ex-CD9500.I)
**
*****************************************************************************/

/*************************************************
* i_dbvers.i - Include de vers∆o de banco de dados   
**************************************************/

/* Preprocessadores que identificam os bancos do Produto EMS 5 */

/* Preprocessadores que identificam os bancos do Produto EMS 2 */
/*RAC Incorporado na 2.04*/

/* Preprocessadores que identificam os bancos do Produto HR */
/*Esta include est† sendo liberada vazia para o EMS 2
 para n∆o ocorrer erros de compilaá∆o*/
 


/* Fim */


/* Fim */
    

/* Fim */
    
.    
/* Fim */

 

define var c-empresa       as character format "x(40)"      no-undo.
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var c-sistema       as character format "x(25)"      no-undo.
define var i-numper-x      as integer   format "ZZ"         no-undo.
define var da-iniper-x     as date      format "99/99/9999" no-undo.
define var da-fimper-x     as date      format "99/99/9999" no-undo.
define var c-rodape        as character                     no-undo.
define var v_num_count     as integer                       no-undo.
define var c-arq-control   as character                     no-undo.
define var i-page-size-rel as integer                       no-undo.
define var c-programa      as character format "x(08)"      no-undo.
define var c-versao        as character format "x(04)"      no-undo.
define var c-revisao       as character format "999"        no-undo.
define var c-impressora   as character                      no-undo.
define var c-layout       as character                      no-undo.

/*Definiá‰es inclu°das para corrigir problema de vari†veis j† definidas pois */
/*as vari†veis e temp-tables eram definidas na include --rpout.i que pode ser*/
/*executada mais de uma vez dentro do mesmo programa (FO 1.120.458) */
/*11/02/2005 - EdÇsio <tech14207>*/
/*-------------------------------------------------------------------------------------------*/
DEF VAR h-procextimpr                               AS HANDLE   NO-UNDO. 
DEF VAR i-num_lin_pag                               AS INT      NO-UNDO.    
DEF VAR c_process-impress                           AS CHAR     NO-UNDO.   
DEF VAR c-cod_pag_carac_conver                      AS CHAR     NO-UNDO.   

/*tech14207
FO 1663218
Inclu°das as definiá‰es das vari†veis e funá‰es
*/

    /*tech868*/
    
        /*Alteracao 03/04/2008 - tech40260 - FO 1746516 -  Feito validaá∆o para verificar se a variavel h_pdf_controller j† foi definida 
                                                       anteriormente, evitando erro de duplicidade*/

        
            DEFINE VARIABLE h_pdf_controller     AS HANDLE NO-UNDO.
    
                
            DEFINE VARIABLE v_cod_temp_file_pdf  AS CHAR   NO-UNDO.
    
            DEFINE VARIABLE v_cod_relat          AS CHAR   NO-UNDO.
            DEFINE VARIABLE v_cod_file_config    AS CHAR   NO-UNDO.
    
            FUNCTION allowPrint RETURNS LOGICAL IN h_pdf_controller.
       
            FUNCTION allowSelect RETURNS LOGICAL IN h_pdf_controller.
       
            FUNCTION useStyle RETURNS LOGICAL IN h_pdf_controller.
       
            FUNCTION usePDF RETURNS LOGICAL IN h_pdf_controller.
       
            FUNCTION getPrintFileName RETURNS CHARACTER IN h_pdf_controller.
       
            RUN btb/btb920aa.p PERSISTENT SET h_pdf_controller.
        
        /*Alteracao 03/04/2008 - tech40260 - FO 1746516 -  Feito validaá∆o para verificar se a variavel h_pdf_controller j† foi definida 
                                                       anteriormente, evitando erro de duplicidade*/
    /*tech868*/
    

/*tech14207*/
/*tech30713 - fo:1262674 - Definiá∆o de no-undo na temp-table*/
DEFINE TEMP-TABLE tt-configur_layout_impres_inicio NO-UNDO
    FIELD num_ord_funcao_imprsor    LIKE configur_layout_impres.num_ord_funcao_imprsor
    FIELD cod_funcao_imprsor        LIKE configur_layout_impres.cod_funcao_imprsor
    FIELD cod_opc_funcao_imprsor    LIKE configur_layout_impres.cod_opc_funcao_imprsor
    FIELD num_carac_configur        LIKE configur_tip_imprsor.num_carac_configur
    INDEX ordem num_ord_funcao_imprsor .

/*tech30713 - fo:1262674 - Definiá∆o de no-undo na temp-table*/
DEFINE TEMP-TABLE tt-configur_layout_impres_fim NO-UNDO
    FIELD num_ord_funcao_imprsor    LIKE configur_layout_impres.num_ord_funcao_imprsor
    FIELD cod_funcao_imprsor        LIKE configur_layout_impres.cod_funcao_imprsor
    FIELD cod_opc_funcao_imprsor    LIKE configur_layout_impres.cod_opc_funcao_imprsor
    FIELD num_carac_configur        LIKE configur_tip_imprsor.num_carac_configur
    INDEX ordem num_ord_funcao_imprsor .
/*-------------------------------------------------------------------------------------------*/

define buffer b_ped_exec_style for ped_exec.
define buffer b_servid_exec_style for servid_exec.

    define new shared stream str-rp.

 
 
/* i-rpvar.i */
/*Alteraá∆o 20/07/2007 - tech1007 - Definiá∆o da vari†vel utilizada para impress∆o em PDF*/
DEFINE VARIABLE v_output_file        AS CHAR   NO-UNDO.
 
/***   Include para os PrÇ-Processadores do Aplicativo de Distribuiá∆o    ***/
/*** Serve para o desenvolvimento tratar o conceito de miniflexibilizaá∆o ***/ 

/*** Funcoes de Uso Geral ***/
/* Utilizado para Teste de Release */
/*** Funcoes Liberadas na 2.02 ***/
/* Recebimento Fisico *//* Unidade de Negocio *//* Verifica Controle de Verba no Investimento *//* Melhorias em Controle de Contratos *//* Melhorias da Aprovacao Eletronica *//* Desatualizacao AP *//* Conciliacao Transitoria Fornecedores *//* Consulta Multi-Moeda em Controle Contratos *//* Conversao da Tabela ext-ord-per da 2.01 p/ 2.02 *//* Tipo de Ressuprimento de Estoque *//* Consumo e Estatistica por Estabelec *//* Importacao x MRP *//* Suspensao IPI Importacao */
/* novo campo cod-tax especifico para compras no item-mat *//* Conta Cont†bil Investimentos */
/*** Funcoes Liberadas na 2.04 ***/
/* Fator Multiplicativo/Divisivo Conversao Moedas *//* Ident Imposto do Item Conforme a Natureza *//* Inclusao Impostos nas Despesas da Nota (MAT e DIS) *//* Selecao Estabelecimentos no Recebimento *//* Contas Transitorias por Estabelecimento *//* Fechamento por Estabelecimento *//* Componentes Usados em Certificado *//* Tipo de Compra na Natureza de Operacao para Recebimento *//* Tratamento de Multiplas Referencias por Lote *//* Estorno Devolucao AP/CR *//* Consumo e Estatistica por Estabelec - Fase II *//* Especie Fiscal no Recebimento *//* Operacao Triangular - Materiais *//* Rateio Despesas Recebimento Internacional *//* Reporte Automatico do Acabado *//* Melhorias em Contratos (Permissoes, Aprovacao, etc...) *//* ParÉmetros Item/Fam°lia por Estabelecimento *//* Nota Fiscal de Simples Remessa e Despesas da Nota Fiscal Complementar */
/*** Funcoes Pendentes ***/
/*&glob bf_mat_devol_cli_inter       yes   /* Devolucao de Cliente do Internacional */*/
/*&glob bf_mat_estorno_cr            yes   /* Estorno Contas a Receber */*/
/*&glob bf_mat_custo_on_line         yes   /* Custo On-line */*/
  

{utp/ut-glob.i}


/****************************************************************
* ESIN0803.i5
* Declaraá∆o Vari†veis
****************************************************************/

def var h-acomp              as handle no-undo.
def var i-imp                as integer.
def var c-un                 as character                       format "x(03)".
def var l-imp                as logical. 
def var i-moeda-param        as integer                         init 1.
def var i-empresa          like empresa.ep-codigo               init 0.
def var c-it-impressao     like base-mensal.it-codigo.  

def var i-emp-ini like empresa.ep-codigo init "".
def var i-emp-fim like empresa.ep-codigo init "ZZZ".

def var c-nivel as char format 'X(30)' no-undo.
def var c-imob  as char format 'X(20)' no-undo.
def var c-moeda as char format 'X(15)' no-undo.
def var c-lista-nivel as char no-undo.
def var c-lista-imob  as char no-undo.
def var c-lista-moeda as char no-undo.

/****
def var i-pro-ini          like proj-inv.num-projeto            init 0.
def var i-pro-fim          like proj-inv.num-projeto            init 999.
def var i-ord-ini          like ordem-inv.num-ordem             init 0.
def var i-ord-fim          like ordem-inv.num-ordem             init 999.
def var i-sec-ini          like secao-inv.num-secao             init 0.
def var i-sec-fim          like secao-inv.num-secao             init 9.
def var i-esp-ini          like especialidade.cod-especialidade init 0.
def var i-esp-fim          like especialidade.cod-especialidade init 99.
def var i-sub-ini          like sub-espec.cod-sub-espec         init 0.
def var i-sub-fim          like sub-espec.cod-sub-espec         init 99.
def var i-ori-ini          like orig-despesa.cod-origem         init 0.
def var i-ori-fim          like orig-despesa.cod-origem         init 9.
def var c-it-ini           like base-mensal.it-codigo.
def var c-it-fim           like base-mensal.it-codigo           init "ZZZZZZZZZZZZZZZZ".
def var i-sig-ini          like proj-inv.sigla                  init "".
def var i-sig-fim          like proj-inv.sigla                  init "ZZZZZZZZZZZZ".
def var c-est-ini          like estabelec.cod-estabel           init "".
def var c-est-fim          like estabelec.cod-estabel           init "ZZZ".
*****/

def var de-qt-est-reest   like estim-mat.quant-estim no-undo.
def var de-vl-est-reest   as dec format "->>>>,>>>,>>>,>>9.99" no-undo.
def var de-vl-compromis   as dec format "->>>>,>>>,>>>,>>9.99" no-undo.
def var de-vl-real-acum   as dec format "->>>>,>>>,>>>,>>9.99" no-undo.
def var de-vl-real-mes    as dec format "->>>>,>>>,>>>,>>9.99" no-undo.
def var de-vl-real-ano    as dec format "->>>>,>>>,>>>,>>9.99" no-undo.
def var de-vl-saldo       as dec format "->>>>,>>>,>>>,>>9.99" no-undo.

def var c-cod-est-exec      like base-mensal.cod-est-exec no-undo.
def var i-ep-codigo         like base-mensal.ep-codigo no-undo.
def var i-num-projeto       like base-mensal.num-projeto format ">>>>9" no-undo.
def var i-num-ordem         like base-mensal.num-ordem no-undo.
def var i-num-secao         like base-mensal.num-secao no-undo.
def var i-cod-especialidade like base-mensal.cod-especialidade no-undo. 
def var i-cod-sub-espec     like base-mensal.cod-sub-espec no-undo.
def var i-cod-origem        like base-mensal.cod-origem no-undo.
def var c-it-codigo         like base-mensal.it-codigo no-undo.

def var i-cont              as integer no-undo.

def var c-descricao         as character format "x(36)" no-undo.
def var c-documento         like base-mensal.it-codigo no-undo.
def var c-desc-docto        like c-descricao no-undo.
def var c-pedido            like c-documento no-undo.
def var c-desc-ped          like c-desc-docto no-undo.
def var i-cod-emitente      like base-mensal.cod-emitente no-undo.
def var c-tipo-reg          like base-mensal.tipo-reg no-undo.
def var l-imprime           as logical no-undo.
def var l-tem-item          as logical no-undo.
def var l-tem-docto         as logical no-undo.
def var l-tem-origem        as logical no-undo.
def var i-tipo              as integer format "99" no-undo.
def var i-niv               as integer no-undo.
def var i-quebra            as integer no-undo.
def var i-detalhe           as integer format "9" no-undo.


def temp-table t-descricao
    field ep-codigo         like sub-div-ordem.ep-codigo
    field cod-est-exec      like sub-div-ordem.cod-est-exec
    field num-projeto       like sub-div-ordem.num-projeto format ">>>>9"
    field num-ordem         like sub-div-ordem.num-ordem
    field num-secao         like sub-div-ordem.num-secao
    field cod-especialidade like sub-div-ordem.cod-especialidade
    field cod-sub-espec     like sub-div-ordem.cod-sub-espec
    field cod-origem        like sub-div-ordem.cod-origem
    field tipo              as int  format "99"
    field descricao         as char format "x(40)"
    index codigo ep-codigo
                 cod-est-exec
                 num-projeto
                 num-ordem
                 num-secao
                 cod-especialidade
                 cod-sub-espec
                 cod-origem.

  
 
/* Variaveis de cabecalho */
def var i-emp-cab          like empresa.ep-codigo.
def var c-emp-cab          like empresa.nome.
def var i-est-cab          like sub-div-ordem.cod-est-exec.
def var c-est-cab          like estabelec.nome.
def var i-sig-cab          like proj-inv.sigla.

def temp-table t-base    
    field ep-codigo         like sub-div-ordem.ep-codigo
    field cod-est-exec      like sub-div-ordem.cod-est-exec
    field num-projeto       like sub-div-ordem.num-projeto format ">>>>9"
    field num-ordem         like sub-div-ordem.num-ordem
    field num-secao         like sub-div-ordem.num-secao
    field cod-especialidade like sub-div-ordem.cod-especialidade
    field cod-sub-espec     like sub-div-ordem.cod-sub-espec
    field cod-origem        like sub-div-ordem.cod-origem
    field it-codigo         like estim-mat.it-codigo
    field desc-item         like c-descricao
    field cod-emitente      like base-mensal.cod-emitente
    field pedido            as character format "x(16)"
    field desc-ped          as character format "x(40)"
    field documento         as character format "x(16)"
    field desc-docto        as character format "x(40)"
    field tipo              as integer   format "99"
    field detalhe           like i-detalhe
    field vl-est-reest      as de format "->>>>,>>>,>>>,>>9.99"
    field vl-compromis      as de format "->>>>,>>>,>>>,>>9.99"
    field vl-real-acum      as de format "->>>>,>>>,>>>,>>9.99"
    field vl-real-mes       as de format "->>>>,>>>,>>>,>>9.99"
    field vl-real-ano       as de format "->>>>,>>>,>>>,>>9.99"
    index codigo ep-codigo
                 cod-est-exec
                 num-projeto
                 num-ordem 
                 num-secao
                 cod-especialidade
                 cod-sub-espec
                 cod-origem
                 it-codigo
                 pedido
                 cod-emitente
                 tipo
                 detalhe
                 documento.

def buffer b-base for t-base.

def new shared temp-table t-real
    field tipo             as int format "99"
    field detalhe          as int format "9"
    field it-codigo        like plano-aprov.it-codigo
    field cod-emitente     like base-mensal.cod-emitente
    field documento        as char format "x(16)"
    field desc-docto       as char format "x(40)"
    field vl-compromis     like base-mensal.vl-compromis extent 0
    field vl-real-mes      like base-mensal.vl-realizado extent 0
    field vl-real-ano      like base-mensal.vl-realizado extent 0
    field vl-real-acum     like base-mensal.vl-realizado extent 0.

def var c-lb-ep-codigo         as char no-undo format "x(3)".
def var c-lb-cod-est-exec      as char no-undo format "x(3)".
def var c-lb-num-projeto       as char no-undo format "x(4)".
def var c-lb-num-ordem         as char no-undo format "x(3)".
def var c-lb-num-secao         as char no-undo format "x(3)".
def var c-lb-cod-especialidade as char no-undo format "x(3)".
def var c-lb-cod-sub-espec     as char no-undo format "x(3)".
def var c-lb-cod-origem        as char no-undo format "x(3)".
def var c-lb-it-codigo         as char no-undo format "x(4)".
def var c-lb-documento         as char no-undo format "x(16)".
def var c-lb-desc-item         as char no-undo format "x(9)".
def var c-lb-vl-est-reest      as char no-undo format "x(16)".
def var c-lb-vl-compromis      as char no-undo format "x(16)".
def var c-lb-vl-real-mes       as char no-undo format "x(12)".
def var c-lb-vl-real-ano       as char no-undo format "x(12)".
def var c-lb-vl-real-acum      as char no-undo format "x(9)".
def var c-lb-de-vl-saldo       as char no-undo format "x(17)".
def var c-lb-vl-mil            as char no-undo format "x(18)".
def var c-literal              as char no-undo.

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Emp",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-ep-codigo = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Exe",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-cod-est-exec = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Proj",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-num-projeto = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Ord",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-num-ordem = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Sec",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-num-secao = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Esp",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-cod-especialidade = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Sub",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-cod-sub-espec = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Ori",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-cod-origem = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Item",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-it-codigo = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Pedido/Documento",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-documento = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Descriá∆o",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-desc-item = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Vl_Estim/Reestim",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-vl-est-reest = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Vl_Compromissado",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-vl-compromis = trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Mes_Corrente",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-vl-real-mes = c-lb-vl-real-mes + trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Ano_Corrente",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-vl-real-ano = c-lb-vl-real-ano + trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Acumulado",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-vl-real-acum = c-lb-vl-real-acum + trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Saldo_Documento",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-de-vl-saldo = c-lb-de-vl-saldo + trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "(_Valores_em_mil_)",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-vl-mil = c-lb-vl-mil + trim(return-value).

def var c-traco as char no-undo.
assign c-traco = fill("-",233).

def var c-traco1 as char no-undo.
assign c-traco1 = fill("-",204).

def var c-traco2 as char no-undo.
assign c-traco2 = fill("-",09).

form header
     c-lb-ep-codigo         no-label at 1
     c-lb-cod-est-exec      no-label at 5
     c-lb-num-projeto       no-label at 10
     c-lb-num-ordem         no-label at 15
     c-lb-num-secao         no-label at 19
     c-lb-cod-especialidade no-label at 23
     c-lb-cod-sub-espec     no-label at 27
     c-lb-cod-origem        no-label at 31
     c-lb-it-codigo         no-label at 40
     c-lb-documento         no-label at 52
     c-lb-desc-item         no-label at 69
     c-lb-vl-est-reest      no-label at 116
     c-lb-vl-compromis      no-label at 137
     c-lb-vl-real-mes       no-label at 162
     c-lb-vl-real-ano       no-label at 183
     c-lb-vl-real-acum      no-label at 207
     c-lb-de-vl-saldo       no-label at 222 
     c-traco1               format "x(207)" no-label at 1
     c-lb-vl-mil            at 208 
     c-traco2               format "x(10)" no-label at 228
     with stream-io page-top no-box width 239 frame f-labels1.

form header
     c-lb-ep-codigo         no-label at 1
     c-lb-cod-est-exec      no-label at 5
     c-lb-num-projeto       no-label at 10
     c-lb-num-ordem         no-label at 15
     c-lb-num-secao         no-label at 19
     c-lb-cod-especialidade no-label at 23
     c-lb-cod-sub-espec     no-label at 27
     c-lb-cod-origem        no-label at 31
     c-lb-it-codigo         no-label at 40
     c-lb-documento         no-label at 52
     c-lb-desc-item         no-label at 69
     c-lb-vl-est-reest      no-label at 116
     c-lb-vl-compromis      no-label at 137
     c-lb-vl-real-mes       no-label at 162
     c-lb-vl-real-ano       no-label at 183
     c-lb-vl-real-acum      no-label at 207
     c-lb-de-vl-saldo       no-label at 222 
     c-traco                format "x(236)" no-label at 1
     with stream-io page-top no-box width 239 frame f-labels2.

form t-base.ep-codigo         no-label 
     t-base.cod-est-exec      no-label 
     t-base.num-projeto       no-label 
     t-base.num-ordem         no-label space(3)
     t-base.num-secao         no-label space(2)
     t-base.cod-especialidade no-label space(2)
     t-base.cod-sub-espec     no-label space(3)
     t-base.cod-origem        no-label 
     t-base.it-codigo         no-label 
     t-base.documento         no-label 
     t-base.desc-item         no-label format 'x(30)'
     t-base.vl-est-reest      no-label 
     t-base.vl-compromis      no-label 
     t-base.vl-real-mes       no-label 
     t-base.vl-real-ano       no-label 
     t-base.vl-real-acum      no-label 
     de-vl-saldo              no-label
     with stream-io no-box width 239 frame f-detalhe.




/**  Fim esin0803.i5 **********************************/

 

/* Definicao temp-table tt-param */
define temp-table tt-param
   field destino          as integer
   field arquivo          as character format "x(50)"
   field usuario          as character format "x(12)"
   field data-exec        as date
   field hora-exec        as integer
   field classifica       as integer
   field desc-classifica  as character format "x(40)"
   field c-est-ini        as character 
   field c-est-fim        as character 
   field i-pro-ini        as integer   
   field i-pro-fim        as integer   
   field i-sig-ini        as character 
   field i-sig-fim        as character 
   field i-ord-ini        as integer   
   field i-ord-fim        as integer   
   field i-sec-ini        as integer   
   field i-sec-fim        as integer   
   field i-esp-ini        as integer   
   field i-esp-fim        as integer   
   field i-sub-ini        as integer   
   field i-sub-fim        as integer   
   field i-ori-ini        as integer 
   field i-ori-fim        as integer 
   field i-emp-ini        as CHAR 
   field i-emp-fim        as CHAR
   FIELD d-data-ini       AS DATE
   FIELD d-data-fim       AS DATE
   field i-nivel          as int 
   field i-imob           as int 
   field i-moeda          as int 
   field i-mes            as integer  
   field i-ano            as integer
   field da-data-base     as date   
   field l-divmil         as logical.  

def temp-table tt-raw-digita
   field raw-digita as raw.

DEF TEMP-TABLE tt-tipo-reg
    FIELD c-tipo-reg AS CHAR.

def input parameter raw-param  as raw no-undo.
def input parameter table     for tt-raw-digita.

def var c-acomp  as char no-undo.
def var c-divmil as char format "x(03)" no-undo.

create tt-param.
raw-transfer raw-param to tt-param.               

find first param-global no-lock no-error.

find first param-inv where
           param-inv.ep-codigo = i-ep-codigo-usuario no-lock no-error.

def var c-lb-tot-pedido    as char format "x(15)" no-undo.
def var c-lb-tot-item      as char format "x(15)" no-undo.
def var c-lb-tot-origem    as char format "x(15)" no-undo.
def var c-lb-tot-sub-espec as char format "x(15)" no-undo.
def var c-lb-tot-espec     as char format "x(15)" no-undo.
def var c-lb-tot-secao     as char format "x(15)" no-undo.
def var c-lb-tot-proj      as char format "x(15)" no-undo.
def var c-lb-tot-ordem     as char format "x(15)" no-undo.
def var c-lb-data          as char format "x(15)" no-undo.

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "TOTAL_PEDIDO",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-tot-pedido = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "TOTAL_ITEM",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-tot-item = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "TOTAL_ORIGEM",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-tot-origem = trim(return-value).        
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "TOTAL-SUB-ESPEC",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-tot-sub-espec = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "TOTAL_ESPECIALIDADE",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-tot-espec = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "TOTAL_SECAO",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-tot-secao = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "TOTAL_PROJETO",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-tot-proj = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "TOTAL_ORDEM",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-tot-ordem = trim(return-value).


/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "INVESTIMENTO",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
/* Inicializacao das variaveis de cabecalho */
assign c-programa     = "ESIN0803"
       c-versao       = "2.00"
       c-revisao      = "000"
       c-sistema      = trim(return-value).

/* Nivel Apres. */
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Projeto",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-nivel = "1-" + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Ordem",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-nivel = c-lista-nivel + " ,2-" + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Seá∆o",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-nivel = c-lista-nivel + " ,3-" + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Especialidade",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-nivel = c-lista-nivel + " ,4-" + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Sub-especial",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-nivel = c-lista-nivel + " ,5-" + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Origem",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-nivel = c-lista-nivel + " ,6-" + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Material",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-nivel = c-lista-nivel + " ,7-" + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Geral_Pedido",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-nivel = c-lista-nivel + " ,8-" + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Geral_Detalhado",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-nivel = c-lista-nivel + " ,9-" + trim(return-value).
assign c-nivel = entry(tt-param.i-nivel,c-lista-nivel).

/* Parametro Imob */
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Imobilizado",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-imob = "1-" + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "N∆o",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-imob = c-lista-imob + " ,2-" + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Ambos",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-imob = c-lista-imob + " ,3-" + trim(return-value).
assign c-imob = entry(tt-param.i-imob,c-lista-imob).

/* Moeda */
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Principal",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-moeda = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Alternativa",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lista-moeda = c-lista-moeda + " ," + trim(return-value) + " 1".
assign c-lista-moeda = c-lista-moeda + " ," + trim(return-value) + " 2".
assign c-moeda = entry(tt-param.i-moeda,c-lista-moeda).

/*Definiá∆o de vari†vel para traduá∆o de p†gina.*/
Define Variable vPagina  As Char FORMAT "x(6)" No-undo. 

def var de-mil as integer no-undo.
if tt-param.l-divmil then
   assign de-mil = 1000.
else
   assign de-mil = 1.      

if i-nivel > 5 then do:
   /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Posicionamento_Econìmico_Material",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
   assign c-titulo-relat = return-value.
end.   
else do:
   /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Posicionamento_Econìmico_Geral",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    assign c-titulo-relat = return-value.
end.    

run INIC-FORMS.  /* gera cabecalho e rodape de 233 cols */
/**************************************************************************
**
** I-RPOUT - Define saÌda para impress„o do relatÛrio - ex. cd9520.i
** Parametros: {&stream} = nome do stream de saida no formato "stream nome"
**             {&append} = append    
**             {&tofile} = nome da vari·vel ou campo com arquivo de destino
**             {&pagesize} = tamanho da pagina
***************************************************************************/

/*As definiá‰es foram transferidas para a include i-rpvar.i (FO 1.120.458) */
/*11/02/2005 - EdÇsio <tech14207>*/

 def new global shared var c-dir-spool-servid-exec as char no-undo.                     
 def new global shared var i-num-ped-exec-rpw as int no-undo.                           
                                                                                   
/*variaveis processador externo impress∆o localizaá∆o*/


 /* procedimento necess†rio para permitir redirecionar saida para arquivo temporario no pdf sem aparecer na pagina de parametros */

    ASSIGN v_output_file = tt-param.arquivo.





/*tech14178 inicio definiá‰es PDF */

    
    /*tech868*/    
    /*tech868*/
        IF NUM-ENTRIES(tt-param.arquivo,"|":U) > 1  THEN DO:
            ASSIGN v_cod_relat = ENTRY(2,tt-param.arquivo,"|":U).
            ASSIGN v_cod_file_config = ENTRY(3,tt-param.arquivo,"|":U).
            ASSIGN tt-param.arquivo = ENTRY(1,tt-param.arquivo,"|":U).
    
            RUN pi_prepare_permissions IN h_pdf_controller(INPUT v_cod_relat).
            RUN pi_set_format IN h_pdf_controller(INPUT IF v_cod_relat <> "":U THEN "PDF":U ELSE "Texto":U).
            RUN pi_set_file_config IN h_pdf_controller(INPUT  v_cod_file_config).
            
        END.
        
        IF usePDF() AND tt-param.destino = 2 THEN DO:
            IF entry(num-entries(tt-param.arquivo,".":U),tt-param.arquivo,".":U) <> "pdf" THEN
                assign tt-param.arquivo = replace(tt-param.arquivo,".":U + entry(num-entries(tt-param.arquivo,".":U),tt-param.arquivo,".":U),".pdf":U).

        END.

        IF usePDF() AND tt-param.destino <> 1 THEN /*tech14178 muda o nome do arquivo para salvar temporario quando n∆o Ç impressora*/
            ASSIGN v_output_file = tt-param.arquivo + ".pdt".



        
        
    /*tech868*/
    





    IF usePDF() AND tt-param.destino = 1  THEN /*pega arquivo tempor†rio randomico para ser usado como impressora */
        ASSIGN v_cod_temp_file_pdf = getPrintFileName().


/*tech14178 fim definiá‰es PDF */

 
    
/*29/12/2004 - tech1007 - Verfica se o arquivo informado tem extensao rtf, se tiver troca para .lst*/


if  tt-param.destino = 1 then do:
   
    if num-entries(tt-param.arquivo,":") = 2 then do:
   .                            
    
        assign c-impressora = substring(tt-param.arquivo,1,index(tt-param.arquivo,":") - 1).
        assign c-layout     = substring(tt-param.arquivo,index(tt-param.arquivo,":") + 1,length(tt-param.arquivo) - index(tt-param.arquivo,":")). 
    .                            

    find layout_impres no-lock
        where layout_impres.nom_impressora    = c-impressora
        and   layout_impres.cod_layout_impres = c-layout no-error.
    find imprsor_usuar no-lock
        where imprsor_usuar.nom_impressora = c-impressora
        and   imprsor_usuar.cod_usuario    = tt-param.usuario
        use-index imprsrsr_id no-error.
    find impressora  of imprsor_usuar no-lock no-error.
    find tip_imprsor of impressora    no-lock no-error.

    /*Alterado 26/04/2005 - tech1007 - Alterado para n∆o ocasionar problemas na convers∆o do mapa de caracteres*/
    IF AVAILABLE tip_imprsor THEN DO:
        ASSIGN c-cod_pag_carac_conver = tip_imprsor.cod_pag_carac_conver.
    END.
    IF AVAILABLE layout_impres THEN DO:
        ASSIGN i-num_lin_pag = layout_impres.num_lin_pag.
    END.
    /*Fim alteracao - tech1007*/

    
    if  i-num-ped-exec-rpw <> 0 then do:
        find b_ped_exec_style where b_ped_exec_style.num_ped_exec = i-num-ped-exec-rpw no-lock no-error. 
        find b_servid_exec_style of b_ped_exec_style no-lock no-error.
        find servid_exec_imprsor of b_servid_exec_style
          where servid_exec_imprsor.nom_impressora = imprsor_usuar.nom_impressora no-lock no-error.

        if  available b_servid_exec_style and b_servid_exec_style.ind_tip_fila_exec = 'UNIX'
        then do:
            /*tech14178 joga para arquivo a ser convertido para PDF */
                IF usePDF() THEN DO:
                    output  through value(v_cod_temp_file_pdf)
                            page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
                    RUN pi_set_print_device IN h_pdf_controller(INPUT servid_exec_imprsor.nom_disposit_so).
                END.
                ELSE
            
            output  through value(servid_exec_imprsor.nom_disposit_so)
                   page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
        end /* if */.
        else do:

            /*Alterado 26/04/2005 - tech1007 - As variaveis i-num_lin_pag e c-cod_pag_carac_conver estao sendo alteradas antes dos testes */
            ASSIGN 
                c-arq-control = servid_exec_imprsor.nom_disposit_so.
            /*Fim alteracao 26/04/2005*/

            IF VALID-HANDLE(h-procextimpr) AND h-procextimpr:FILE-NAME = c_process-impress THEN
                RUN pi_before_output IN h-procextimpr 
                    (INPUT c-impressora,
                     INPUT c-layout,
                     INPUT tt-param.usuario,
                     INPUT-OUTPUT c-arq-control,
                     INPUT-OUTPUT i-num_lin_pag,
                     INPUT-OUTPUT c-cod_pag_carac_conver).
            
            /*tech14178 joga para arquivo a ser convertido para PDF */
                IF usePDF() THEN DO:
                    output   to value(v_cod_temp_file_pdf)
                            page-size value(i-num_lin_pag) convert target c-cod_pag_carac_conver.
                    RUN pi_set_print_device IN h_pdf_controller(INPUT servid_exec_imprsor.nom_disposit_so).
                END.
                ELSE
            
                    output   to value(c-arq-control)
                           page-size value(i-num_lin_pag) convert target c-cod_pag_carac_conver.
        end /* else */.
    end.
    else do:

        /*Alterado 26/04/2005 - tech1007 - As variaveis i-num_lin_pag e c-cod_pag_carac_conver estao sendo alteradas antes dos testes */
        ASSIGN 
            c-arq-control = imprsor_usuar.nom_disposit_so.
        /*Fim alteracao 26/04/2005*/

        IF VALID-HANDLE(h-procextimpr) AND h-procextimpr:FILE-NAME = c_process-impress THEN
            RUN pi_before_output IN h-procextimpr 
                (INPUT c-impressora,
                 INPUT c-layout,
                 INPUT tt-param.usuario,
                 INPUT-OUTPUT c-arq-control,
                 INPUT-OUTPUT i-num_lin_pag,
                 INPUT-OUTPUT c-cod_pag_carac_conver).

        if i-num_lin_pag = 0 then do:
            /*tech14178 joga para arquivo a ser convertido para PDF */
                IF usePDF() THEN DO:
                /* sem salta pˇgina */
                    output   
                            to value(v_cod_temp_file_pdf)
                            page-size 0
                            convert target c-cod_pag_carac_conver . 
                    RUN pi_set_print_device IN h_pdf_controller(INPUT imprsor_usuar.nom_disposit_so).
                END.
                ELSE
            
                /* sem salta pˇgina */
                    output   
                            to value(c-arq-control)
                            page-size 0
                            convert target c-cod_pag_carac_conver . 
        end.
        else do:
            /*tech14178 joga para arquivo a ser convertido para PDF */
                IF usePDF() THEN DO:
                /* sem salta pˇgina */
                    output   
                            to value(v_cod_temp_file_pdf)
                            paged page-size value(i-num_lin_pag) 
                            convert target c-cod_pag_carac_conver .
                    RUN pi_set_print_device IN h_pdf_controller(INPUT imprsor_usuar.nom_disposit_so).
                END.
                ELSE
            
                    /* com salta p·gina */
                    output  
                            to value(c-arq-control)
                            paged page-size value(i-num_lin_pag) 
                            convert target c-cod_pag_carac_conver .
        end.
    end.

    for each configur_layout_impres NO-LOCK 
        where configur_layout_impres.num_id_layout_impres = layout_impres.num_id_layout_impres
        by configur_layout_impres.num_ord_funcao_imprsor:

        find configur_tip_imprsor no-lock
            where configur_tip_imprsor.cod_tip_imprsor        = layout_impres.cod_tip_imprsor
            and   configur_tip_imprsor.cod_funcao_imprsor     = configur_layout_impres.cod_funcao_imprsor
            and   configur_tip_imprsor.cod_opc_funcao_imprsor = configur_layout_impres.cod_opc_funcao_imprsor
            use-index cnfgrtpm_id no-error.
        CREATE tt-configur_layout_impres_inicio.    
        BUFFER-COPY configur_tip_imprsor TO tt-configur_layout_impres_inicio
            ASSIGN tt-configur_layout_impres_inicio.num_ord_funcao_imprsor = configur_layout_impres.num_ord_funcao_imprsor.
    end.

    IF VALID-HANDLE(h-procextimpr) AND h-procextimpr:FILE-NAME = c_process-impress THEN
        RUN pi_after_output IN h-procextimpr (INPUT-OUTPUT TABLE tt-configur_layout_impres_inicio).

    FOR EACH tt-configur_layout_impres_inicio EXCLUSIVE-LOCK
        BY tt-configur_layout_impres_inicio.num_ord_funcao_imprsor :
        do v_num_count = 1 to extent(tt-configur_layout_impres_inicio.num_carac_configur):            
          case tt-configur_layout_impres_inicio.num_carac_configur[v_num_count]:
            when 0 then put  control null.
            when ? then leave.
            otherwise   put  control CODEPAGE-CONVERT(chr(tt-configur_layout_impres_inicio.num_carac_configur[v_num_count]),
                                                               session:cpinternal, 
                                                               c-cod_pag_carac_conver).
          end case.
        end.
        DELETE tt-configur_layout_impres_inicio.
    END.
  end.
  else do:
    
        assign c-impressora  = entry(1,tt-param.arquivo,":").
        assign c-layout      = entry(2,tt-param.arquivo,":"). 
        if num-entries(tt-param.arquivo,":") = 4 then
          assign c-arq-control = entry(3,tt-param.arquivo,":") + ":" + entry(4,tt-param.arquivo,":").
        else 
          assign c-arq-control = entry(3,tt-param.arquivo,":").
    .                            

    find layout_impres no-lock
        where layout_impres.nom_impressora    = c-impressora
        and   layout_impres.cod_layout_impres = c-layout no-error.
    find imprsor_usuar no-lock
        where imprsor_usuar.nom_impressora = c-impressora
        and   imprsor_usuar.cod_usuario    = tt-param.usuario
        use-index imprsrsr_id no-error.
    find impressora  of imprsor_usuar no-lock no-error.
    find tip_imprsor of impressora    no-lock no-error.

    /*Alterado 26/04/2005 - tech1007 - Alterado para n∆o ocasionar problemas na convers∆o do mapa de caracteres*/
    IF AVAILABLE tip_imprsor THEN DO:
        ASSIGN c-cod_pag_carac_conver = tip_imprsor.cod_pag_carac_conver.
    END.
    IF AVAILABLE layout_impres THEN DO:
        ASSIGN i-num_lin_pag = layout_impres.num_lin_pag.
    END.
    /*Fim alteracao - tech1007*/

    
    
    /*tech14178 adiciona extens∆o PDT para que o arquivo a ser convertido n∆o fique com o mesmo nome do arquivo final*/
        IF usePDF() THEN 
            ASSIGN c-arq-control = c-arq-control + ".pdt":U.
    

    


    if  i-num-ped-exec-rpw <> 0 then do:
        find b_ped_exec_style where b_ped_exec_style.num_ped_exec = i-num-ped-exec-rpw no-lock no-error. 
        find b_servid_exec_style of b_ped_exec_style no-lock no-error.
        find servid_exec_imprsor of b_servid_exec_style 
          where servid_exec_imprsor.nom_impressora = imprsor_usuar.nom_impressora no-lock no-error.

        if  available b_servid_exec_style and b_servid_exec_style.ind_tip_fila_exec = 'UNIX'
        then do:
            output  to value(c-dir-spool-servid-exec + "~/" + c-arq-control)
                   page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
        end /* if */.
        else do:
            output   to value(c-dir-spool-servid-exec + "~/" + c-arq-control)
                   page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
        end /* else */.
    end.
    else do:
        /*Alterado 26/04/2005 - tech1007 - Removido pois o assign est† sendo realizado antes dos testes
        ASSIGN 
            i-num_lin_pag = layout_impres.num_lin_pag
            c-cod_pag_carac_conver = tip_imprsor.cod_pag_carac_conver.
        Fim alteracao 26/04/2005*/    

        IF VALID-HANDLE(h-procextimpr) AND h-procextimpr:FILE-NAME = c_process-impress THEN
            RUN pi_before_output IN h-procextimpr 
                (INPUT c-impressora,
                 INPUT c-layout,
                 INPUT tt-param.usuario,
                 INPUT-OUTPUT c-arq-control,
                 INPUT-OUTPUT i-num_lin_pag,
                 INPUT-OUTPUT c-cod_pag_carac_conver).
        if i-num_lin_pag = 0 then do:
            /* sem salta pˇgina */
            output   
                    to value(c-arq-control)
                    page-size 0
                    convert target c-cod_pag_carac_conver . 
        end.
        else do:
            /* com salta p·gina */
            output  
                    to value(c-arq-control)
                    paged page-size value(layout_impres.num_lin_pag) 
                    convert target tip_imprsor.cod_pag_carac_conver.
        end.
    end.
    
    /*tech14178 guarda o nome do arquivo a ser convertido para pdf  */
            if  i-num-ped-exec-rpw <> 0 THEN
                RUN pi_set_print_filename IN h_pdf_controller (INPUT c-dir-spool-servid-exec + "~/" + c-arq-control).
            ELSE
                RUN pi_set_print_filename IN h_pdf_controller (INPUT c-arq-control).
    


    for each configur_layout_impres NO-LOCK 
        where configur_layout_impres.num_id_layout_impres = layout_impres.num_id_layout_impres
        by configur_layout_impres.num_ord_funcao_imprsor:

        find configur_tip_imprsor no-lock
            where configur_tip_imprsor.cod_tip_imprsor        = layout_impres.cod_tip_imprsor
            and   configur_tip_imprsor.cod_funcao_imprsor     = configur_layout_impres.cod_funcao_imprsor
            and   configur_tip_imprsor.cod_opc_funcao_imprsor = configur_layout_impres.cod_opc_funcao_imprsor
            use-index cnfgrtpm_id no-error.
        CREATE tt-configur_layout_impres_inicio.    
        BUFFER-COPY configur_tip_imprsor TO tt-configur_layout_impres_inicio
            ASSIGN tt-configur_layout_impres_inicio.num_ord_funcao_imprsor = configur_layout_impres.num_ord_funcao_imprsor.
    end.

    IF VALID-HANDLE(h-procextimpr) AND h-procextimpr:FILE-NAME = c_process-impress THEN                
        RUN pi_after_output IN h-procextimpr (INPUT-OUTPUT TABLE tt-configur_layout_impres_inicio).

    FOR EACH tt-configur_layout_impres_inicio EXCLUSIVE-LOCK
        BY tt-configur_layout_impres_inicio.num_ord_funcao_imprsor :
        do v_num_count = 1 to extent(tt-configur_layout_impres_inicio.num_carac_configur):
          case tt-configur_layout_impres_inicio.num_carac_configur[v_num_count]:
            when 0 then put  control null.
            when ? then leave.
            otherwise   put  control CODEPAGE-CONVERT(chr(tt-configur_layout_impres_inicio.num_carac_configur[v_num_count]),
                                                               session:cpinternal, 
                                                               c-cod_pag_carac_conver).
                            
          end case.
        end.
        DELETE tt-configur_layout_impres_inicio.
    END.
  end.  
end.
else do:
    
        if  i-num-ped-exec-rpw <> 0 then do:
            
              /*Alterado 14/02/2005 - tech1007 - Alterado para que quando for gerar RTF em batch
                o tamanho da p†gina seja 42*/
              
                      output  
                             to value(c-dir-spool-servid-exec + "~/" + v_output_file) 
                             paged page-size 64
                             convert target "iso8859-1" .
              
              /*Fim alteracao 14/02/2005*/
            
        end.                             
        else do:
            /* Sa°da para RTF - tech981 20/10/2004 */
            
                
                        output  
                               to value(v_output_file) 
                               paged page-size 64 
                               convert target "iso8859-1" .                
                
            
        end.    
    
end.

/* i-rpout */
   

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Posicionamento Economico",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
run pi-inicializar in h-acomp (input RETURN-VALUE).

run pi-acompanhar in h-acomp (input c-acomp).

for each t-base exclusive-lock:
    delete t-base.
end.

for each t-descricao exclusive-lock:
    delete t-descricao.
end.

assign i-moeda-param = if tt-param.i-moeda = 1 then param-inv.moeda-inv
                       else if tt-param.i-moeda = 2 then param-inv.moeda1
                       else param-inv.moeda2.

FOR EACH tt-tipo-reg:
    DELETE tt-tipo-reg.
END.

CREATE tt-tipo-reg.
ASSIGN tt-tipo-reg.c-tipo-reg = "21".
IF tt-param.i-nivel > 6 THEN DO:
    CREATE tt-tipo-reg.
    ASSIGN tt-tipo-reg.c-tipo-reg = "41".
END.
for each sub-div-ordem no-lock
    where sub-div-ordem.ep-codigo         >= tt-param.i-emp-ini
    and   sub-div-ordem.ep-codigo         <= tt-param.i-emp-fim
    and   sub-div-ordem.cod-est-exec      >= tt-param.c-est-ini
    and   sub-div-ordem.cod-est-exec      <= tt-param.c-est-fim
    and   sub-div-ordem.num-projeto       >= tt-param.i-pro-ini 
    and   sub-div-ordem.num-projeto       <= tt-param.i-pro-fim
    and   sub-div-ordem.num-ordem         >= tt-param.i-ord-ini
    and   sub-div-ordem.num-ordem         <= tt-param.i-ord-fim
    and   sub-div-ordem.num-secao         >= tt-param.i-sec-ini
    and   sub-div-ordem.num-secao         <= tt-param.i-sec-fim
    and   sub-div-ordem.cod-especialidade >= tt-param.i-esp-ini
    and   sub-div-ordem.cod-especialidade <= tt-param.i-esp-fim
    and   sub-div-ordem.cod-sub-espec     >= tt-param.i-sub-ini
    and   sub-div-ordem.cod-sub-espec     <= tt-param.i-sub-fim
    and   sub-div-ordem.cod-origem        >= tt-param.i-ori-ini
    and   sub-div-ordem.cod-origem        <= tt-param.i-ori-fim
    break by sub-div-ordem.ep-codigo:

    if first-of(sub-div-ordem.ep-codigo) then do:
        find param-inv where
             param-inv.ep-codigo = sub-div-ordem.ep-codigo
             no-lock no-error.
    end.

    FOR FIRST proj-inv FIELDS(sigla descricao descricao-2)
        where proj-inv.ep-codigo    = sub-div-ordem.ep-codigo 
        and   proj-inv.cod-est-exec = sub-div-ordem.cod-est-exec
        and   proj-inv.num-projeto  = sub-div-ordem.num-projeto no-lock:
    END.

    if  not avail proj-inv then
        next.

    IF proj-inv.sigla  < i-sig-ini OR
       proj-inv.sigla  > i-sig-fim THEN NEXT.

    /*   Grava temp-table com a descricao das chaves de acesso   */

    assign i-ep-codigo         = sub-div-ordem.ep-codigo
           c-cod-est-exec      = sub-div-ordem.cod-est-exec
           i-num-projeto       = sub-div-ordem.num-projeto
           i-num-ordem         = 0
           i-num-secao         = 0
           i-cod-especialidade = 0
           i-cod-sub-espec     = 0
           i-cod-origem        = 0
           i-tipo              = 01
           c-descricao         = proj-inv.descricao + proj-inv.descricao-2.

    run grava-descricao.

    FOR FIRST ordem-inv FIELDS(ind-imobil descricao descricao-2)
         where ordem-inv.ep-codigo    = sub-div-ordem.ep-codigo
         and   ordem-inv.cod-est-exec = sub-div-ordem.cod-est-exec
         and   ordem-inv.num-projeto  = sub-div-ordem.num-projeto
         and   ordem-inv.num-ordem    = sub-div-ordem.num-ordem no-lock:
    END.

    if  ordem-inv.ind-imobil = 1 
        and tt-param.i-imob = 2 then
        next.

    if  ordem-inv.ind-imobil = 2 
        and tt-param.i-imob = 1 then 
        next.

    assign i-num-ordem = sub-div-ordem.num-ordem
           i-tipo      = 02
           c-descricao = ordem-inv.descricao + ordem-inv.descricao-2.

    run grava-descricao.

    FOR first secao-inv FIELDS(descricao descricao-2)
         where secao-inv.ep-codigo    = sub-div-ordem.ep-codigo
         and   secao-inv.cod-est-exec = sub-div-ordem.cod-est-exec
         and   secao-inv.num-projeto  = sub-div-ordem.num-projeto
         and   secao-inv.num-ordem    = sub-div-ordem.num-ordem
         and   secao-inv.num-secao    = sub-div-ordem.num-secao no-lock:
    END.

    assign i-num-secao = sub-div-ordem.num-secao
           i-tipo      = 03
           c-descricao = secao-inv.descricao + secao-inv.descricao-2.

    run grava-descricao.

    FOR first especialidade FIELDS(descricao descricao-2)
         where especialidade.ep-codigo         = sub-div-ordem.ep-codigo
         and   especialidade.cod-especialidade = sub-div-ordem.cod-especialidade no-lock:
    END.

    assign i-cod-especialidade = sub-div-ordem.cod-especialidade
           i-tipo              = 04
           c-descricao         = especialidade.descricao + 
                                 especialidade.descricao-2 when avail especialidade.

    run grava-descricao.

    FOR first sub-espec FIELDS(descricao descricao-2)
         where sub-espec.ep-codigo         = sub-div-ordem.ep-codigo
         and   sub-espec.cod-especialidade = sub-div-ordem.cod-especialidade
         and   sub-espec.cod-sub-espec     = sub-div-ordem.cod-sub-espec NO-LOCK:
    END.

    assign i-cod-sub-espec = sub-div-ordem.cod-sub-espec when avail sub-espec
           i-tipo          = 05
           c-descricao     = sub-espec.descricao + sub-espec.descricao-2 when avail sub-espec.

    run grava-descricao.

    FOR first orig-despesa FIELDS(descricao descricao-2)
         where orig-despesa.ep-codigo  = sub-div-ordem.ep-codigo
         and   orig-despesa.cod-origem = sub-div-ordem.cod-origem NO-LOCK:
    END.

    assign i-cod-origem = sub-div-ordem.cod-origem 
           i-tipo       = 06
           c-descricao  = orig-despesa.descricao + orig-despesa.descricao-2 when avail orig-despesa.

    run grava-descricao.

    /*  Grava valores de estimativa na temp-table t-base     */

    for each estim-mat 
        where estim-mat.ep-codigo         = sub-div-ordem.ep-codigo
        and   estim-mat.cod-est-exec      = sub-div-ordem.cod-est-exec
        and   estim-mat.num-projeto       = sub-div-ordem.num-projeto
        and   estim-mat.num-ordem         = sub-div-ordem.num-ordem
        and   estim-mat.num-secao         = sub-div-ordem.num-secao
        and   estim-mat.cod-especialidade = sub-div-ordem.cod-especialidade
        and   estim-mat.cod-sub-espec     = sub-div-ordem.cod-sub-espec
        and   estim-mat.cod-origem        = sub-div-ordem.cod-origem
        no-lock:

        assign i-ep-codigo         = estim-mat.ep-codigo
               c-cod-est-exec      = estim-mat.cod-est-exec
               i-num-projeto       = estim-mat.num-projeto
               i-num-ordem         = estim-mat.num-ordem
               i-num-secao         = estim-mat.num-secao
               i-cod-especialidade = estim-mat.cod-especialidade
               i-cod-sub-espec     = estim-mat.cod-sub-espec
               i-cod-origem        = estim-mat.cod-origem.

        if  estim-mat.quant-reestim > 0 then
            assign de-qt-est-reest = estim-mat.quant-reestim.
        else 
            assign de-qt-est-reest = estim-mat.quant-estim.

        if  estim-mat.vl-unit-reestim[tt-param.i-moeda] > 0 then
            assign de-vl-est-reest = 
                  (estim-mat.vl-unit-reestim[tt-param.i-moeda] *
                   estim-mat.quant-reestim).
        else
            assign de-vl-est-reest = 
                  (estim-mat.vl-unit-estim[tt-param.i-moeda] *
                   estim-mat.quant-estim).

        assign de-vl-compromis = 0
               de-vl-real-acum = 0
               de-vl-real-mes  = 0
               de-vl-real-ano  = 0.

        assign c-it-codigo    = estim-mat.it-codigo
               i-tipo         = tt-param.i-nivel
               i-detalhe      = 0
               c-documento    = ""
               c-desc-docto   = ""
               i-cod-emitente = 0
               c-pedido       = ""
               c-desc-ped     = "".

        FOR FIRST item FIELDS(desc-item)
            where item.it-codigo = estim-mat.it-codigo NO-LOCK:
        END.
        if avail item then
            assign c-descricao = item.desc-item.
        else
            assign c-descricao = estim-mat.descricao.

        if tt-param.i-nivel > 6 then
            assign i-tipo = 07.

        run atualizar.

    end.

    /*   Grava totais de compromissado e realizado na t-base   */
    FOR EACH tt-tipo-reg, /*para melhorar performance*/
        each base-mensal     
        where base-mensal.ep-codigo          = sub-div-ordem.ep-codigo
        and  base-mensal.tipo-reg            = tt-tipo-reg.c-tipo-reg /*"21"
        or   (base-mensal.tipo-reg           = "41" 
        and   tt-param.i-nivel               > 6))*/
        and   base-mensal.mo-codigo          = i-moeda-param
        and   base-mensal.cod-est-exec       = sub-div-ordem.cod-est-exec
        and   base-mensal.num-projeto        = sub-div-ordem.num-projeto
        and   base-mensal.num-ordem          = sub-div-ordem.num-ordem
        and   base-mensal.num-secao          = sub-div-ordem.num-secao
        and   base-mensal.cod-especialidade  = sub-div-ordem.cod-especialidade
        and   base-mensal.cod-sub-espec      = sub-div-ordem.cod-sub-espec
        and   base-mensal.cod-origem         = sub-div-ordem.cod-origem
        no-lock:

        assign de-vl-real-mes  = 0
               de-vl-real-ano  = 0
               de-vl-real-acum = 0.

        do  i-cont = 1 to 12:
            if tt-param.i-imob = 1 or tt-param.i-imob = 3 then
                assign de-vl-compromis = de-vl-compromis +
                                         base-mensal.vl-compromis[i-cont].
                       de-vl-real-acum = de-vl-real-acum +
                                         base-mensal.vl-realizado[i-cont].
            if tt-param.i-imob = 2 or tt-param.i-imob = 3 then
                assign de-vl-compromis = de-vl-compromis +
                                         base-mensal.vl-compromis-ni[i-cont]
                       de-vl-real-acum = de-vl-real-acum +
                                         base-mensal.vl-realizado-ni[i-cont].
        end.

        if  base-mensal.ano = i-ano and 
            (tt-param.i-imob = 1 or tt-param.i-imob = 3) then do:
            assign de-vl-real-mes = de-vl-real-mes +
                                    base-mensal.vl-realizado[i-mes].

            do  i-cont = 1 to 12:
                assign de-vl-real-ano = de-vl-real-ano +
                                        base-mensal.vl-realizado[i-cont].
            end.
        end.
        if  base-mensal.ano = i-ano and 
            (tt-param.i-imob = 2 or tt-param.i-imob = 3) then do:
            assign de-vl-real-mes = de-vl-real-mes +
                                    base-mensal.vl-realizado-ni[i-mes].
            do  i-cont = 1 to 12:
                assign de-vl-real-ano = de-vl-real-ano +
                                        base-mensal.vl-realizado-ni[i-cont].
            end.
        end.              

        assign de-qt-est-reest = 0
               de-vl-est-reest = 0.

        assign i-ep-codigo         = base-mensal.ep-codigo
               c-cod-est-exec      = base-mensal.cod-est-exec
               i-num-projeto       = base-mensal.num-projeto
               i-num-ordem         = base-mensal.num-ordem
               i-num-secao         = base-mensal.num-secao
               i-cod-especialidade = base-mensal.cod-especialidade
               i-cod-sub-espec     = base-mensal.cod-sub-espec
               i-cod-origem        = base-mensal.cod-origem.

        assign c-it-codigo    = ""
               i-tipo         = tt-param.i-nivel
               i-detalhe      = 0
               c-descricao    = ""
               c-documento    = ""
               c-desc-docto   = ""
               c-pedido       = ""
               i-cod-emitente = 0
               c-desc-ped     = "".

        if base-mensal.tipo-reg = "21" and
           tt-param.i-nivel > 6 then
            assign i-tipo = 06.

        if base-mensal.tipo-reg = "41"  then do:
            assign c-it-codigo  = base-mensal.it-codigo
                   i-tipo       = 07.
            find item 
                where item.it-codigo = base-mensal.it-codigo 
                no-lock no-error.
            if avail item then
                assign c-descricao = item.desc-item.
        end.       

        run atualizar.

    end.

    if tt-param.i-nivel < 8 then 
        next.

    /*   Grava valores de realizacao das Ordens de Manutencao no t-base   */

    for each ordem-man no-lock  
        where ordem-man.ep-codigo         = sub-div-ordem.ep-codigo
        and   ordem-man.cod-est-exec      = sub-div-ordem.cod-est-exec
        and   ordem-man.num-projeto       = sub-div-ordem.num-projeto
        and   ordem-man.num-ordem         = sub-div-ordem.num-ordem
        and   ordem-man.num-secao         = sub-div-ordem.num-secao
        and   ordem-man.cod-especialidade = sub-div-ordem.cod-especialidade
        and   ordem-man.cod-sub-espec     = sub-div-ordem.cod-sub-espec
        and   ordem-man.cod-origem        = sub-div-ordem.cod-origem:

        assign de-vl-compromis = ordem-man.vl-material[tt-param.i-moeda] +
                                 ordem-man.vl-mob[tt-param.i-moeda] +
                                 ordem-man.vl-servico[tt-param.i-moeda]
               de-vl-real-acum = ordem-man.vl-material[tt-param.i-moeda] +
                                 ordem-man.vl-mob[tt-param.i-moeda] +
                                 ordem-man.vl-servico[tt-param.i-moeda].

        if tt-param.i-moeda = 1 then
           assign de-vl-compromis = de-vl-compromis + dec(substring(ordem-man.char-1,1,18))
                  de-vl-real-acum = de-vl-real-acum + dec(substring(ordem-man.char-1,1,18)).
        else if tt-param.i-moeda = 2 then
           assign de-vl-compromis = de-vl-compromis + dec(substring(ordem-man.char-1,19,18))
                  de-vl-real-acum = de-vl-real-acum + dec(substring(ordem-man.char-1,1,18)).
        else
           assign de-vl-compromis = de-vl-compromis + dec(substring(ordem-man.char-1,37,18))
                  de-vl-real-acum = de-vl-real-acum + dec(substring(ordem-man.char-1,1,18)).               


        if int(substr(ordem-man.ano-mes,1,4)) = i-ano then do:
            if int(substr(ordem-man.ano-mes,5,2)) = i-mes then 
                assign de-vl-real-mes = de-vl-real-acum.

            assign de-vl-real-ano = de-vl-real-acum. 
        end.    

        assign i-ep-codigo         = ordem-man.ep-codigo
               c-cod-est-exec      = ordem-man.cod-est-exec
               i-num-projeto       = ordem-man.num-projeto
               i-num-ordem         = ordem-man.num-ordem
               i-num-secao         = ordem-man.num-secao
               i-cod-especialidade = ordem-man.cod-especialidade
               i-cod-sub-espec     = ordem-man.cod-sub-espec
               i-cod-origem        = ordem-man.cod-origem.

        if ordem-man.log-2 then
           assign c-documento = "MI ".
        else
           assign c-documento = "OP ".

        assign c-documento    = c-documento + string(ordem-man.cod-estabel,"xxx") + " " +
                                string(ordem-man.num-ord-man)
               c-desc-docto   = ordem-man.descricao
               c-it-codigo    = ""
               c-descricao    = ""
               i-cod-emitente = 0
               c-pedido       = ""
               c-desc-ped     = ""
               i-tipo         = 06
               i-detalhe      = 1.

        run atualizar.
    end.


    /*  Grava valores de Movimentos de Apropriacao no t-base      */

    for each movto-apr no-lock 
        where movto-apr.ep-codigo         = sub-div-ordem.ep-codigo
        and   movto-apr.cod-est-exec      = sub-div-ordem.cod-est-exec
        and   movto-apr.num-projeto       = sub-div-ordem.num-projeto
        and   movto-apr.num-ordem         = sub-div-ordem.num-ordem
        and   movto-apr.num-secao         = sub-div-ordem.num-secao
        and   movto-apr.cod-especialidade = sub-div-ordem.cod-especialidade
        and   movto-apr.cod-sub-espec     = sub-div-ordem.cod-sub-espec
        and   movto-apr.cod-origem        = sub-div-ordem.cod-origem:

        FOR first ord-ped 
            where ord-ped.ep-codigo      = movto-apr.ep-codigo
            and   ord-ped.num-pedido     = movto-apr.num-pedido
            and   ord-ped.seq-pedido     = 0 
            AND   ord-ped.cod-estabel    = movto-apr.cod-estabel
            and   ord-ped.cod-area = 0
            and   ord-ped.num-ord-comp = movto-apr.int-1
            and   ord-ped.num-ord-magnus = movto-apr.num-ord-magnus no-lock:
        END.

        if avail ord-ped then
            next.

        assign de-vl-compromis = movto-apr.vl-mat[tt-param.i-moeda] +
                                 movto-apr.vl-mob[tt-param.i-moeda] 
               de-vl-real-acum = movto-apr.vl-mat[tt-param.i-moeda] +
                                 movto-apr.vl-mob[tt-param.i-moeda] .

        if year(movto-apr.dt-trans) = i-ano then do:
            if month(movto-apr.dt-trans) = i-mes then 
                assign de-vl-real-mes = de-vl-real-acum.

            assign de-vl-real-ano = de-vl-real-acum. 
        end.

        if  movto-apr.tipo-trans = 2 then
            assign de-vl-compromis = de-vl-compromis * -1
                   de-vl-real-acum = de-vl-real-acum * -1
                   de-vl-real-mes  = de-vl-real-mes  * -1
                   de-vl-real-ano  = de-vl-real-ano  * -1.                         

        assign i-ep-codigo         = movto-apr.ep-codigo
               c-cod-est-exec      = movto-apr.cod-est-exec
               i-num-projeto       = movto-apr.num-projeto
               i-num-ordem         = movto-apr.num-ordem
               i-num-secao         = movto-apr.num-secao
               i-cod-especialidade = movto-apr.cod-especialidade
               i-cod-sub-espec     = movto-apr.cod-sub-espec
               i-cod-origem        = movto-apr.cod-origem.

        assign c-documento  = string(movto-apr.esp-docto, "xxx") + " " +
                              string(movto-apr.cod-estabel,"xxx") + " " +
                              substring(movto-apr.nro-docto,1,8)
               c-desc-docto   = ""
               c-it-codigo    = movto-apr.it-codigo
               c-descricao    = ""
               i-cod-emitente = 0
               c-pedido       = ""
               c-desc-ped     = ""
               i-detalhe      = 1.

        if movto-apr.transacao     = "REQ" 
            or movto-apr.transacao = "DIV"
            then
            assign i-tipo = 07.
        else
            assign i-tipo = 06.

        FOR FIRST ITEM FIELDS(desc-item)
            where item.it-codigo = movto-apr.it-codigo NO-LOCK:
        END.
        if avail item then
            assign c-desc-docto = item.desc-item.

        FOR FIRST emitente FIELDS(nome-emit)
            where emitente.cod-emitente = movto-apr.cod-emitente NO-LOCK:
        END.
        if avail emitente then
            assign c-desc-docto = emitente.nome-emit.

        run atualizar.

    end.

    /*  Grava valores dos movimentos de compras no t-base      */

    for each ord-ped no-lock
        where ord-ped.ep-codigo         = sub-div-ordem.ep-codigo
        AND   ord-ped.num-ord-magnus    = sub-div-ordem.num-ord-magnus
        /*********
        and   ord-ped.cod-est-exec      = sub-div-ordem.cod-est-exec
        and   ord-ped.num-projeto       = sub-div-ordem.num-projeto
        and   ord-ped.num-ordem         = sub-div-ordem.num-ordem
        and   ord-ped.num-secao         = sub-div-ordem.num-secao
        and   ord-ped.cod-especialidade = sub-div-ordem.cod-especialidade
        and   ord-ped.cod-sub-espec     = sub-div-ordem.cod-sub-espec
        and   ord-ped.cod-origem        = sub-div-ordem.cod-origem
        **********/
        and   ord-ped.cod-sit-ped       <> "C"
        and   ord-ped.cod-sit-ped       <> "E"
        and   ord-ped.cod-sit-comp      <> "E"
        and   ord-ped.num-pedido        <> 0:


        for each t-real:
            delete t-real.
        end.

        run inp/spp/esin0803a.p (rowid(ord-ped),
                           input table tt-param, 
                           input-output table t-real).

        for each t-real:

            assign i-ep-codigo         = ord-ped.ep-codigo
                   c-cod-est-exec      = ord-ped.cod-est-exec
                   i-num-projeto       = ord-ped.num-projeto
                   i-num-ordem         = ord-ped.num-ordem
                   i-num-secao         = ord-ped.num-secao
                   i-cod-especialidade = ord-ped.cod-especialidade
                   i-cod-sub-espec     = ord-ped.cod-sub-espec
                   i-cod-origem        = ord-ped.cod-origem.

            assign c-it-codigo     = t-real.it-codigo
                   i-cod-emitente  = t-real.cod-emitente
                   c-documento     = t-real.documento
                   c-desc-docto    = t-real.desc-docto
                   de-vl-compromis = t-real.vl-compromis
                   de-vl-real-mes  = t-real.vl-real-mes
                   de-vl-real-ano  = t-real.vl-real-ano
                   de-vl-real-acum = t-real.vl-real-acum.

            assign c-pedido     = 
                   string(ord-ped.num-pedido, ">>>>>>>>9") + " " +
                   string(ord-ped.num-ord-comp,">>>>>9,99") 
                   c-desc-ped   = ""
                   c-descricao  = ""
                   i-tipo       = t-real.tipo
                   i-detalhe    = t-real.detalhe.

            find emitente 
                where emitente.cod-emitente = i-cod-emitente
                no-lock no-error.
                if avail emitente then
                    assign c-desc-ped = emitente.nome-emit.

            run atualizar.

        end.
        
    end.
end.

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Aguarde",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-acomp = trim(return-value) + "...".
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Gerando o relatorio",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-acomp = c-acomp + trim(return-value).

run pi-acompanhar in h-acomp (input c-acomp).
    
IF  tt-param.l-divmil THEN DO:
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Sim",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    ASSIGN c-divmil =  RETURN-VALUE.
    view frame f-labels1.
END.
ELSE DO:
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "N∆o",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    ASSIGN c-divmil =  return-value.
    view frame f-labels2. 
END.

for each t-base no-lock
    break by t-base.ep-codigo
          by t-base.cod-est-exec
          by t-base.num-projeto
          by t-base.num-ordem
          by t-base.num-secao
          by t-base.cod-especialidade
          by t-base.cod-sub-espec
          by t-base.cod-origem
          by t-base.it-codigo 
          by t-base.pedido
          by t-base.cod-emitente
          by t-base.tipo
          by t-base.detalhe
          by t-base.documento:

    /******                
    view frame f-cabper1.
    view frame f-rodape1.
    view frame f-labels.
    *****/

    assign i-ep-codigo         = t-base.ep-codigo
           c-cod-est-exec      = t-base.cod-est-exec
           i-num-projeto       = t-base.num-projeto
           i-num-ordem         = t-base.num-ordem
           i-num-secao         = t-base.num-secao
           i-cod-especialidade = t-base.cod-especialidade
           i-cod-sub-espec     = t-base.cod-sub-espec
           i-cod-origem        = t-base.cod-origem
           i-tipo              = t-base.tipo
           c-it-codigo         = t-base.it-codigo
           c-pedido            = t-base.pedido
           i-cod-emitente      = t-base.cod-emitente.

    hide frame f-detalhe.

    assign i-niv = t-base.tipo.

    if t-base.tipo < 7 then
        run busca-descricao.

    if t-base.tipo = 06 and
       t-base.detalhe = 1 then
        assign l-tem-origem = yes.

    if t-base.tipo = 07 and
       tt-param.i-nivel > 7 then
        assign l-tem-item = yes.

    if  t-base.tipo = 08 and
        tt-param.i-nivel > 8 then
        assign l-tem-docto = yes.

    assign l-imprime = no.

    if i-niv >= tt-param.i-nivel or
       t-base.detalhe = 1 then 
        assign l-imprime = yes.

    assign de-vl-saldo = (t-base.vl-compromis - 
                          t-base.vl-real-acum) / de-mil.
    
    hide frame f-detalhe.
    display t-base.ep-codigo         when i-niv = 1
            t-base.cod-est-exec      when i-niv = 1
            t-base.num-projeto       when i-niv = 1
            t-base.num-ordem         when i-niv = 2
            t-base.num-secao         when i-niv = 3
            t-base.cod-especialidade when i-niv = 4
            t-base.cod-sub-espec     when i-niv = 5
            t-base.cod-origem        when i-niv = 6 and
                                          t-base.detalhe = 0
            t-descricao.descricao    when i-niv < 7
            @ t-base.desc-item
            t-base.it-codigo         when i-niv = 7 and
                                          t-base.detalhe = 0                                        

            t-base.desc-item         when i-niv = 7 and
                                          t-base.detalhe = 0
            t-base.pedido            when i-niv = 8
            @ t-base.documento
            t-base.desc-ped          when i-niv = 8
            @ t-base.desc-item
            t-base.documento         when t-base.detalhe = 1
            t-base.desc-docto        when t-base.detalhe = 1
            @ t-base.desc-item
            t-base.vl-est-reest / de-mil when l-imprime and
                                            t-base.vl-est-reest <> 0
            @ t-base.vl-est-reest
            t-base.vl-compromis / de-mil when l-imprime 
            @ t-base.vl-compromis
            t-base.vl-real-mes  / de-mil when l-imprime            
            @ t-base.vl-real-mes
            t-base.vl-real-ano  / de-mil when l-imprime            
            @ t-base.vl-real-ano
            t-base.vl-real-acum / de-mil when l-imprime            
            @ t-base.vl-real-acum
            de-vl-saldo                when l-imprime and
                                            t-base.detalhe = 0
            with frame f-detalhe.
    down with frame f-detalhe.  
    hide frame f-detalhe.

    IF  (l-imprime AND i-niv <= 6 AND tt-param.i-nivel < 7)
    OR  (tt-param.i-nivel >= 7 AND i-niv = 6 AND t-base.documento = "") THEN
        RUN imprime-unid-negoc.
    
    assign i-quebra = 0.

    if last-of(t-base.pedido) or
       last-of(t-base.cod-emitente) then
        assign i-quebra = 1.
    if last-of(t-base.it-codigo) then
        assign i-quebra = 2.
    if last-of(t-base.cod-origem) then
        assign i-quebra = 3.
    if last-of(t-base.cod-sub-espec) then
        assign i-quebra = 4.
    if last-of(t-base.cod-especialidade) then
        assign i-quebra = 5.
    if last-of(t-base.num-secao) then
        assign i-quebra = 6.
    if last-of(t-base.num-ordem) then
        assign i-quebra = 7.
    if last-of(t-base.num-projeto) then
        assign i-quebra = 8.

    if  i-quebra <> 0 then DO:
        run imprime-totais.
    END.
end.

run imprime-final.    
/**************************************************************************
**
** I-RPCLO - Define sa°da para impress∆o do relat¢rio - ex. cd9540.i
** Parametros: {&stream} = nome do stream de saida no formato "stream nome"
***************************************************************************/
IF VALID-HANDLE(h-procextimpr) AND h-procextimpr:FILE-NAME = c_process-impress THEN                
    RUN pi_before_close IN h-procextimpr (INPUT-OUTPUT TABLE tt-configur_layout_impres_fim).

FOR EACH tt-configur_layout_impres_fim EXCLUSIVE-LOCK
    BY tt-configur_layout_impres_fim.num_ord_funcao_imprsor :
    do v_num_count = 1 to extent(tt-configur_layout_impres_fim.num_carac_configur):
      case tt-configur_layout_impres_fim.num_carac_configur[v_num_count]:
        when 0 then put  control null.
        when ? then leave.
        OTHERWISE PUT  control CODEPAGE-CONVERT(chr(tt-configur_layout_impres_fim.num_carac_configur[v_num_count]),
                                                         session:cpinternal, 
                                                         c-cod_pag_carac_conver).
      end CASE.
    END.
    DELETE tt-configur_layout_impres_fim.
END.

/* N∆o gerar p†gina em branco - tech14207 11/02/2005 */


output  close.

/* Sa°da para RTF - tech981 20/10/2004 */

/* fim: Sa°da para RTF */

/*tech14178 procedimentos de convers∆o PDF */
/*tech868*/

    IF usePDF() THEN DO:
        IF tt-param.destino = 1 THEN DO:
            RUN pi_print IN h_pdf_controller.
        END.
        ELSE DO:
            /*tech868*/
                IF i-num-ped-exec-rpw <> 0 THEN
                    RUN pi_convert IN h_pdf_controller (INPUT c-dir-spool-servid-exec + "~/" + v_output_file, IF tt-param.destino = 3 THEN YES ELSE NO). /* indica se vai para terminal, pois regras de nomenclatura s∆o diferentes */
                ELSE
                    RUN pi_convert IN h_pdf_controller (INPUT v_output_file, IF tt-param.destino = 3 THEN YES ELSE NO).
            /*tech868*/
        END.
    END.



IF VALID-HANDLE(h-procextimpr) AND h-procextimpr:FILE-NAME = c_process-impress THEN DO:
    RUN pi_after_close IN h-procextimpr (INPUT c-arq-control).
    DELETE PROCEDURE h-procextimpr NO-ERROR.
END.
/* i-rpout */
 

run pi-finalizar in h-acomp.   

/***  Fim do relatorio  ***/

/*    Procedure interna   Imprime-totais        */

procedure IMPRIME-FINAL.

    def var c-lb-selec   as char no-undo.
    def var c-lb-empresa as char no-undo.
    def var c-lb-estabel as char no-undo.
    def var c-lb-projeto as char no-undo.
    def var c-lb-sigla   as char no-undo.
    def var c-lb-ordem   as char no-undo.
    def var c-lb-secao   as char no-undo.
    def var c-lb-espec   as char no-undo.
    def var c-lb-subesp  as char no-undo.
    def var c-lb-origem  as char no-undo.
    def var c-lb-impr    as char no-undo.
    def var c-lb-dest    as char no-undo.
    def var c-lb-usuar   as char no-undo.
    def var c-destino    as char no-undo.
    def var c-lb-param   as char no-undo.
    def var c-lb-nivel   as char no-undo.
    def var c-lb-imob    as char no-undo.
    def var c-lb-moeda   as char no-undo.
    def var c-lb-mes     as char no-undo.
    def var c-lb-ref     as char no-undo.
    def var c-lb-div-val as char no-undo.


    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "SELEÄ«O",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
    assign c-lb-selec = return-value.
    /*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "proj-inv":U, 
                    input "ep-codigo":U, 
                    input integer("1":U)).

/* ut-field.i */
 
    assign c-lb-empresa = return-value.
    /*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "proj-inv":U, 
                    input "cod-est-exec":U, 
                    input integer("1":U)).

/* ut-field.i */
 
    assign c-lb-estabel = return-value.
    /*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "proj-inv":U, 
                    input "num-projeto":U, 
                    input integer("1":U)).

/* ut-field.i */
 
    assign c-lb-projeto = return-value.
    /*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "proj-inv":U, 
                    input "sigla":U, 
                    input integer("1":U)).

/* ut-field.i */
 
    assign c-lb-sigla = return-value.
    /*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "sub-div-ordem":U, 
                    input "num-ordem":U, 
                    input integer("1":U)).

/* ut-field.i */
 
    assign c-lb-ordem = return-value.
    /*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "sub-div-ordem":U, 
                    input "num-secao":U, 
                    input integer("1":U)).

/* ut-field.i */
 
    assign c-lb-secao = return-value.
    /*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "sub-div-ordem":U, 
                    input "cod-especialidade":U, 
                    input integer("1":U)).

/* ut-field.i */
 
    assign c-lb-espec = return-value.
    /*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "sub-div-ordem":U, 
                    input "cod-sub-espec":U, 
                    input integer("1":U)).

/* ut-field.i */
 
    assign c-lb-subesp = return-value.
    /*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "sub-div-ordem":U, 
                    input "cod-origem":U, 
                    input integer("1":U)).

/* ut-field.i */
 
    assign c-lb-origem = return-value.
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "IMPRESS«O",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
    assign c-lb-impr = return-value.
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Destino",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
    assign c-lb-dest = return-value.
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Usu†rio",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
    assign c-lb-usuar = return-value.
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "PAR∂METROS",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
    assign c-lb-param = trim(return-value).
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Data",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
    assign c-lb-data = trim(return-value).

    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "N°vel_Apresentaá∆o",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
    assign c-lb-nivel = trim(return-value).
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "ParÉmetro_Imobilizado",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
    assign c-lb-imob = trim(return-value).
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Moeda",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
    assign c-lb-moeda = trim(return-value).
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Realizado_Màs",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
    
    assign c-lb-mes = trim(return-value).
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Ref_Base_Mensal",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
    
    assign c-lb-ref = trim(return-value).
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Valor_dividido_por_1000",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
    
    assign c-lb-div-val = trim(return-value).


    
        ASSIGN c-destino = /******************************************************************
**
** var00002.i  Vari†vel: Destino - Template de Relat¢rios
**
******************************************************************/

 
 






/***************************************************************************
**  ind01-10.i - define as funcoes de um indicador
**  Para indicadores de 1 a 10 items
**
**  Funcoes disponiveis
**  01: view-as Combo-box
**  02: view-as radio-set
**  03: lista com os itens separados por virgula
**  04 n: retorna o item n da lista
**  05: retorna o numero de items da lista
**  06: retorna a posicao do item (numero)
**  07: valores para a propriedade Radio-Buttons de um Radio-Set
***************************************************************************/

/* verifica parametros ****************************************************/




/* &if lookup("{1}", "01,02,03,04,05,06,07") = 0 &then
    &message *** ({&file-name}): Parametro incorreto: {1} !
    &message *** Deveria ser: 01, 02, 03, 04, 05, 06, 07 
&endif  */
    

  


/* monta lista de items para LISTA (03), NUM (04), ITEM(05), IND(06) ************************/

          
               
     
               
     
     
     
     
     
     
     


/* funcao Combo-box (01) *************************************************************/


/* funcao Radio-set (02) *************************************************************/


/* funcao Lista (03) **********************************************************/


/* funcao NUM (05) ************************************************************/


/* funcao Item n (04) *********************************************************/    


     entry(tt-param.destino, "Impressora,Arquivo,Terminal")



/* funcao IND string (06) ****************************************************/



/* valores para a propriedade Radio-Buttons de um Radio-Set *******************/




    

    

    

    








/* fim */
 

/* fim */
 .

    

  page.
  view frame f-cabec.
  view frame f-rodape.

  IF tt-param.l-divmil THEN  
     hide  frame f-labels1.
  ELSE
     hide frame f-labels2.

  put unformatted
    c-lb-selec           skip(1)
    c-lb-empresa         at 5  ": "
    tt-param.i-emp-ini   at 30 "|< >| " at 47 tt-param.i-emp-fim
    c-lb-estabel         at 5  ": "
    tt-param.c-est-ini   at 30 "|< >| " at 47 tt-param.c-est-fim
    c-lb-projeto         at 5  ": "
    tt-param.i-pro-ini   at 30 "|< >| " at 47 tt-param.i-pro-fim
    c-lb-sigla           at 5  ": "
    tt-param.i-sig-ini   at 30 "|< >| " at 47 tt-param.i-sig-fim
    c-lb-ordem           at 5  ": "
    tt-param.i-ord-ini   at 30 "|< >| " at 47 tt-param.i-ord-fim
    c-lb-secao           at 5  ": "
    tt-param.i-sec-ini   at 30 "|< >| " at 47 tt-param.i-sec-fim
    c-lb-espec           at 5  ": "
    tt-param.i-esp-ini   at 30 "|< >| " at 47 tt-param.i-esp-fim
    c-lb-subesp          at 5  ": "
    tt-param.i-sub-ini   at 30 "|< >| " at 47 tt-param.i-sub-fim
    c-lb-origem          at 5  ": "
    tt-param.i-ori-ini   at 30 "|< >| " at 47 tt-param.i-ori-fim
    c-lb-data            at 5  ": "
    tt-param.d-data-ini FORMAT "99/99/9999"   at 30 "|< >| " at 47 tt-param.d-data-fim FORMAT "99/99/9999" skip(2)
    
    c-lb-param skip
    c-lb-nivel           at 5  ": " c-nivel
    c-lb-imob            at 5  ": " c-imob
    c-lb-moeda           at 5  ": " c-moeda
    c-lb-mes             at 5  ": " + string(tt-param.i-mes,"99") + "/" + string(tt-param.i-ano,"9999") 
    c-lb-ref             at 5  ": " + string(da-data-base,"99/99/9999") 
    c-lb-div-val         AT 5  ": " + c-divmil skip(2)
    
    c-lb-impr            skip(1)
    c-lb-dest            at 5  ": " c-destino + " - " + tt-param.arquivo
    c-lb-usuar           at 5  ": " tt-param.usuario skip(2).    


end procedure. /* Fim IMPRIME-FINAL */

procedure imprime-totais.

    if l-tem-docto then do:        
        assign c-descricao = trim(c-lb-tot-pedido) + " ... " + c-pedido
               i-tipo      = 08
               l-tem-docto = no.
        run busca-totais.
    end.
    if i-quebra > 1 and
       l-tem-item then do:        
        assign c-descricao    = trim(c-lb-tot-item) + " ............" + c-it-codigo
               i-tipo         = 07
               l-tem-item     = no
               l-tem-docto    = no
               c-pedido       = ""
               i-cod-emitente = 0.
        run busca-totais.
    end.
    if i-quebra > 2 and 
       (i-niv  > 6 or
        l-tem-origem) then do:
        assign c-it-codigo  = ""
               c-descricao  = trim(c-lb-tot-origem) + " .........." + 
                               string(i-cod-origem)
               i-tipo       = 06
               l-tem-origem = no
               l-tem-docto  = no
               l-tem-item   = no.
        run busca-totais.
    end.
    if i-quebra > 3 and
       i-niv  > 5 then do:
        assign i-cod-origem = 0
               c-descricao = trim(c-lb-tot-sub-espec) + " ......." + 
                             string(i-cod-sub-espec)
               i-tipo      = 05.
        run busca-totais.
    end.
    if i-quebra > 4 and
       i-niv  > 4 then do: 
        assign i-cod-sub-espec = 0
               c-descricao = trim(c-lb-tot-espec) + " ..." + 
                             string(i-cod-especialidade)
               i-tipo      = 04.
        run busca-totais.
    end.
    if i-quebra > 5 and
       i-niv  > 3 then do:
        assign i-cod-especialidade = 0
               c-descricao = trim(c-lb-tot-secao) + " ..........." + 
                              string(i-num-secao)
               i-tipo      = 03.
        run busca-totais.
    end.
    if i-quebra > 6 and
       i-niv  > 2 then do:
        assign i-num-secao = 0
               c-descricao = trim(c-lb-tot-ordem) + " ..........." + 
                              string(i-num-ordem)
               i-tipo      = 02.
        run busca-totais.
    end.
    if i-quebra > 7 and
       i-niv  > 1 then do:

        assign i-num-ordem = 0
               c-descricao = trim(c-lb-tot-proj) + " ........." + 
                              string(i-num-projeto)
               i-tipo      = 01.
        run busca-totais.
    end.

end procedure.

procedure atualizar.

    if tt-param.i-nivel < 8 then
        assign c-documento = ""
               c-desc-docto = ""
               c-pedido = ""
               c-desc-ped = ""
               i-cod-emitente = 0.
    if tt-param.i-nivel < 7 then
        assign c-it-codigo = ""
               c-descricao = "".
    if tt-param.i-nivel < 6 then
        assign i-cod-origem = 0.
    if tt-param.i-nivel < 5 then
        assign i-cod-sub-espec = 0.
    if tt-param.i-nivel < 4 then
        assign i-cod-especialidade = 0.
    if tt-param.i-nivel < 3 then
        assign i-num-secao = 0.
    if tt-param.i-nivel < 2 then
        assign i-num-ordem = 0.

    run registros.

    if i-tipo = 07 then
        assign de-vl-compromis = 0
               de-vl-real-mes  = 0 
               de-vl-real-ano  = 0 
               de-vl-real-acum = 0.

    if i-tipo < 08 and
       i-detalhe = 0 then do:
        assign i-niv = i-tipo.
        do i-cont = 1 to 6:

            assign i-tipo = i-niv - i-cont.

            if i-tipo = 0 then
                leave.
            if i-tipo = 06 then
                assign c-documento    = ""
                       c-desc-docto   = ""
                       c-it-codigo    = ""
                       c-descricao    = ""
                       c-pedido       = ""
                       c-desc-ped     = ""
                       i-cod-emitente = 0.
            if i-tipo = 05 then
                assign i-cod-origem = 0.
            if i-tipo = 04 then
                assign i-cod-sub-espec = 0.
            if i-tipo = 03 then
                assign i-cod-especialidade = 0.
            if i-tipo = 02 then
                assign i-num-secao = 0.
            if i-tipo = 01 then 
                assign i-num-ordem = 0.

            run registros.
        end.
    end.

    assign de-vl-est-reest = 0
           de-vl-compromis = 0
           de-vl-real-mes = 0
           de-vl-real-ano = 0
           de-vl-real-acum = 0.

end procedure.

procedure registros:

    find first t-base use-index codigo 
         where t-base.ep-codigo         = i-ep-codigo
         and   t-base.cod-est-exec      = c-cod-est-exec 
         and   t-base.num-projeto       = i-num-projeto 
         and   t-base.num-ordem         = i-num-ordem 
         and   t-base.num-secao         = i-num-secao 
         and   t-base.cod-especialidade = i-cod-especialidade 
         and   t-base.cod-sub-espec     = i-cod-sub-espec 
         and   t-base.cod-origem        = i-cod-origem 
         and   t-base.it-codigo         = c-it-codigo
         and   t-base.tipo              = i-tipo
         and   t-base.detalhe           = i-detalhe
         and   t-base.pedido            = c-pedido
         and   t-base.cod-emitente      = i-cod-emitente
         no-error.

    if  not avail t-base or 
        i-detalhe = 1 then do:
        create t-base.                           
        assign t-base.ep-codigo          = i-ep-codigo
               t-base.cod-est-exec       = c-cod-est-exec
               t-base.num-projeto        = i-num-projeto
               t-base.num-ordem          = i-num-ordem
               t-base.num-secao          = i-num-secao
               t-base.cod-especialidade  = i-cod-especialidade
               t-base.cod-sub-espec      = i-cod-sub-espec
               t-base.cod-origem         = i-cod-origem
               t-base.it-codigo          = c-it-codigo
               t-base.desc-item          = c-descricao
               t-base.tipo               = i-tipo
               t-base.detalhe            = i-detalhe
               t-base.cod-emitente       = i-cod-emitente
               t-base.pedido             = c-pedido
               t-base.desc-ped           = c-desc-ped
               t-base.documento          = c-documento
               t-base.desc-docto         = c-desc-docto.
    end.

    assign t-base.vl-est-reest = t-base.vl-est-reest + de-vl-est-reest
           t-base.vl-compromis = t-base.vl-compromis + de-vl-compromis
           t-base.vl-real-mes  = t-base.vl-real-mes + de-vl-real-mes
           t-base.vl-real-ano  = t-base.vl-real-ano + de-vl-real-ano
           t-base.vl-real-acum = t-base.vl-real-acum + de-vl-real-acum.

end procedure.

procedure grava-descricao.

    run busca-descricao.

    if not avail t-descricao then do:

        create t-descricao.
        assign t-descricao.ep-codigo          = i-ep-codigo
               t-descricao.cod-est-exec       = c-cod-est-exec
               t-descricao.num-projeto        = i-num-projeto
               t-descricao.num-ordem          = i-num-ordem
               t-descricao.num-secao          = i-num-secao
               t-descricao.cod-especialidade  = i-cod-especialidade
               t-descricao.cod-sub-espec      = i-cod-sub-espec
               t-descricao.cod-origem         = i-cod-origem
               t-descricao.tipo               = i-tipo
               t-descricao.descricao          = c-descricao.

    end.  

end procedure.

procedure busca-descricao.

    find first t-descricao
        where t-descricao.ep-codigo         = i-ep-codigo
        and   t-descricao.cod-est-exec      = c-cod-est-exec
        and   t-descricao.num-projeto       = i-num-projeto 
        and   t-descricao.num-ordem         = i-num-ordem 
        and   t-descricao.num-secao         = i-num-secao 
        and   t-descricao.cod-especialidade = i-cod-especialidade 
        and   t-descricao.cod-sub-espec     = i-cod-sub-espec 
        and   t-descricao.cod-origem        = i-cod-origem
        and   t-descricao.tipo              = i-tipo
        no-lock no-error.

end procedure.

procedure busca-totais.
    find first b-base use-index codigo 
         where b-base.ep-codigo         = i-ep-codigo
         and   b-base.cod-est-exec      = c-cod-est-exec 
         and   b-base.num-projeto       = i-num-projeto 
         and   b-base.num-ordem         = i-num-ordem 
         and   b-base.num-secao         = i-num-secao 
         and   b-base.cod-especialidade = i-cod-especialidade 
         and   b-base.cod-sub-espec     = i-cod-sub-espec 
         and   b-base.cod-origem        = i-cod-origem 
         and   b-base.it-codigo         = c-it-codigo
         and   b-base.tipo              = i-tipo
         and   b-base.pedido            = c-pedido
         and   b-base.cod-emitente      = i-cod-emitente
         NO-LOCK no-error.
    if  avail b-base then do:
        assign de-vl-saldo = (b-base.vl-compromis - 
                              b-base.vl-real-acum) / de-mil.
        display c-descricao @ t-base.desc-item
                b-base.vl-est-reest / de-mil @ t-base.vl-est-reest
                b-base.vl-compromis / de-mil @ t-base.vl-compromis
                b-base.vl-real-mes / de-mil @ t-base.vl-real-mes
                b-base.vl-real-ano / de-mil @ t-base.vl-real-ano
                b-base.vl-real-acum / de-mil @ t-base.vl-real-acum
                de-vl-saldo
                with frame f-detalhe.
        down with frame f-detalhe.  
        hide frame f-detalhe.
    end.
end procedure.      

PROCEDURE imprime-unid-negoc.
   
   
   
   RETURN "OK":U.
   
END PROCEDURE.


/**********************************************************************
*  esin0803.i4
*  Cabeáalho
***********************************************************************/

procedure INIC-FORMS.

  /****************************************************************************
   **  Identico: I-RPCAB.I - Form do Cabeáalho Padr∆o e RodapÇ (ex-CD9500.F)
   **              {&STREAM} - indica o nome da stream (opcional)
   ****************************************************************************/
 

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "P†gina:",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
Assign vPagina = Return-value.


  /* Rodape */
  c-rodape = "DATASUL - " + c-sistema + " - " + c-prg-obj + " - V:" + c-prg-vrs.
  c-rodape = fill("-", 233 - length(c-rodape)) + c-rodape.
  form header
       c-rodape format 'x(233)'
       with stream-io width 233 no-labels no-box page-bottom frame f-rodape.

  /* Cabecalho */
  
     
        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            vPagina at 222 page-number  at 229 format ">>>>9" skip
            fill("-", 213) format "x(211)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabec.

        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            vPagina at 222 page-number  at 229 format ">>>>9" skip
            "Periodo:" i-numper-x at 10 "-"
            da-iniper-x at 15 "a" da-fimper-x
            fill("-", 163) format "x(161)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabper.
     
  

end procedure.

/* Fim esin0803.i4 *************************/
 

/* fim do programa */ 


