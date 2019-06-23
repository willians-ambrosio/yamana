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
def var c-prg-vrs as char init "[[[2.00.00.017[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.017"
       c-prg-obj = "ESIN0801RP".

/* Alteracao - 02/10/2006 - Nakamura - Incluida chamada a include que verifica a integridade dos programas do registro de produto */
/*{include/i-ctrlrp.i {1}}*/

/*Alteraá∆o - 08/09/2006 - tech1007 - Alteraá∆o para exibir o nome do programa que executou o programa que ser† exibido no extrato de vers∆o
                                      Solicitaá∆o realizada na FO 1239827*/

/*Fim alteraá∆o 08/09/2006*/


if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "ESIN0801RP"
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
        PUT "ESIN0801RP" AT 1 "2.00.00.017" AT 69 STRING(TODAY,'99/99/99') AT 84 STRING(TIME,'HH:MM:SS':U) AT 94 SKIP.
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

   /*** 010017 ***/
 
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
 
/*******************************************************************************
**
**   Programa: ESIN0801.P
**
**   Data....: Junho de 1997.
**
**   Autor...: DATASUL S.A.
**
**   Objetivo: COD - Caderno Operacional Detalhado
**
**   Versao..: Baseado na vers∆o I.00.005
**
*******************************************************************************/

/*******************************************************************************
**
**  ESIN0801.I2 - Definicao das temp-tables utilizadas no ESIN0801.P
**
*******************************************************************************/

def temp-table t-cod    
    field ep-codigo         like sub-div-ordem.ep-codigo
    field cod-est-exec      like sub-div-ordem.cod-est-exec
    field num-projeto       like sub-div-ordem.num-projeto
    field num-ordem         like sub-div-ordem.num-ordem
    field num-secao         like sub-div-ordem.num-secao
    field cod-especialidade like sub-div-ordem.cod-especialidade
    field cod-sub-espec     like sub-div-ordem.cod-sub-espec
    field cod-origem        like sub-div-ordem.cod-origem
    field tipo              as int
    field it-codigo         like estim-mat.it-codigo
    FIELD dt-trans          AS DATE
    field num-ord-magnus    like sub-div-ordem.num-ord-magnus
    field descricao         as char format "x(36)"
    field un                as char format "x(03)"
    field de-qt-est-reest   like estim-mat.quant-estim
    field de-vl-est-reest   like estim-mat.vl-unit-estim[1]
    field de-tot-est-reest  as de 
    field de-qt-comp        like ord-ped.quant-comp
    field de-vl-comp        as de format "->,>>>,>>9.9999" /*like ord-ped.vl-item[1]*/  
    field de-tot-comp       as de format "->>>,>>>,>>9.99"
    field de-vl-des-tec     as de 
    field de-vl-des-eco     as de 
    field de-vl-estim       as de 
    field de-fim-proj       as de  format "->>,>>>,>>9.99"
    field de-vl-tend        as de 
    field de-vl-verba       like sub-div-ordem.vl-estimado[1] 
    field ind-desv-tec      like estim-mat.ind-desv-tec       
    field flag              as char format "!"
    field ch-tipo           as int
    field ch-ori            as int
    field ch-sub            as int
    field ch-esp            as int
    field ch-sec            as int
    field ch-ord            as int
    field ch-pro            as int
    index codigo ep-codigo
                 cod-est-exec
                 num-projeto
                 num-ordem
                 num-secao
                 cod-especialidade
                 cod-sub-espec
                 cod-origem
                 tipo
                 it-codigo.

def temp-table t-tot-tipo
    field ep-codigo         like t-cod.ep-codigo         
    field cod-est-exec      like t-cod.cod-est-exec      
    field num-projeto       like t-cod.num-projeto       
    field num-ordem         like t-cod.num-ordem         
    field num-secao         like t-cod.num-secao         
    field cod-especialidade like t-cod.cod-especialidade 
    field cod-sub-espec     like t-cod.cod-sub-espec     
    field cod-origem        like t-cod.cod-origem 
    field tipo              as int
    field ch-tipo           as int
    field de-tot-est-reest  as de format "->>>,>>>,>>9.99"
    field de-tot-compromis  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-tecn  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-econ  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-estim  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-final  as de format "->>>,>>>,>>9.99"
    field de-tot-tendencia  as de format "->>>,>>>,>>9.99"
    index codigo ep-codigo
                 cod-est-exec
                 num-projeto
                 num-ordem
                 num-secao
                 cod-especialidade
                 cod-sub-espec
                 cod-origem
                 tipo
    index chave ch-tipo.

def temp-table t-tot-ori
    field ep-codigo         like t-cod.ep-codigo         
    field cod-est-exec      like t-cod.cod-est-exec      
    field num-projeto       like t-cod.num-projeto       
    field num-ordem         like t-cod.num-ordem         
    field num-secao         like t-cod.num-secao         
    field cod-especialidade like t-cod.cod-especialidade 
    field cod-sub-espec     like t-cod.cod-sub-espec     
    field cod-origem        like t-cod.cod-origem 
    field ch-ori            as int
    field de-tot-est-reest  as de format "->>>,>>>,>>9.99"
    field de-tot-compromis  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-tecn  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-econ  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-estim  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-final  as de format "->>>,>>>,>>9.99"
    field de-tot-tendencia  as de format "->>>,>>>,>>9.99"
    index codigo ep-codigo
                 cod-est-exec
                 num-projeto
                 num-ordem
                 num-secao
                 cod-especialidade
                 cod-sub-espec
                 cod-origem
    index chave ch-ori.

def temp-table t-tot-sub
    field ep-codigo         like t-cod.ep-codigo         
    field cod-est-exec      like t-cod.cod-est-exec      
    field num-projeto       like t-cod.num-projeto       
    field num-ordem         like t-cod.num-ordem         
    field num-secao         like t-cod.num-secao         
    field cod-especialidade like t-cod.cod-especialidade 
    field cod-sub-espec     like t-cod.cod-sub-espec     
    field ch-sub            as int
    field de-tot-est-reest  as de format "->>>,>>>,>>9.99"
    field de-tot-compromis  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-tecn  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-econ  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-estim  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-final  as de format "->>>,>>>,>>9.99"
    field de-tot-tendencia  as de format "->>>,>>>,>>9.99"
    index codigo ep-codigo
                 cod-est-exec
                 num-projeto
                 num-ordem
                 num-secao
                 cod-especialidade
                 cod-sub-espec
    index chave ch-sub.

def temp-table t-tot-esp
    field ep-codigo         like t-cod.ep-codigo         
    field cod-est-exec      like t-cod.cod-est-exec      
    field num-projeto       like t-cod.num-projeto       
    field num-ordem         like t-cod.num-ordem         
    field num-secao         like t-cod.num-secao         
    field cod-especialidade like t-cod.cod-especialidade 
    field ch-esp            as int
    field de-tot-est-reest  as de format "->>>,>>>,>>9.99"
    field de-tot-compromis  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-tecn  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-econ  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-estim  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-final  as de format "->>>,>>>,>>9.99"
    field de-tot-tendencia  as de format "->>>,>>>,>>9.99"
    index codigo ep-codigo 
                 cod-est-exec
                 num-projeto
                 num-ordem
                 num-secao
                 cod-especialidade
    index chave ch-esp.

def temp-table t-tot-sec
    field ep-codigo         like t-cod.ep-codigo         
    field cod-est-exec      like t-cod.cod-est-exec      
    field num-projeto       like t-cod.num-projeto       
    field num-ordem         like t-cod.num-ordem         
    field num-secao         like t-cod.num-secao         
    field ch-sec            as int
    field de-tot-est-reest  as de format "->>>,>>>,>>9.99"
    field de-tot-compromis  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-tecn  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-econ  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-estim  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-final  as de format "->>>,>>>,>>9.99"
    field de-tot-tendencia  as de format "->>>,>>>,>>9.99"
    index codigo ep-codigo
                 cod-est-exec
                 num-projeto
                 num-ordem
                 num-secao
    index chave ch-sec.

def temp-table t-tot-ord
    field ep-codigo         like t-cod.ep-codigo         
    field cod-est-exec      like t-cod.cod-est-exec      
    field num-projeto       like t-cod.num-projeto       
    field num-ordem         like t-cod.num-ordem         
    field ch-ord            as int
    field de-tot-est-reest  as de format "->>>,>>>,>>9.99"
    field de-tot-compromis  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-tecn  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-econ  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-estim  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-final  as de format "->>>,>>>,>>9.99"
    field de-tot-tendencia  as de format "->>>,>>>,>>9.99"
    index codigo ep-codigo
                 cod-est-exec
                 num-projeto
                 num-ordem
    index chave ch-ord.

def temp-table t-tot-pro
    field ep-codigo         like t-cod.ep-codigo         
    field cod-est-exec      like t-cod.cod-est-exec      
    field num-projeto       like t-cod.num-projeto       
    field ch-pro            as int
    field de-tot-est-reest  as de format "->>>,>>>,>>9.99"
    field de-tot-compromis  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-tecn  as de format "->>>,>>>,>>9.99"
    field de-tot-desv-econ  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-estim  as de format "->>>,>>>,>>9.99"
    field de-tot-sal-final  as de format "->>>,>>>,>>9.99"
    field de-tot-tendencia  as de format "->>>,>>>,>>9.99"
    index codigo ep-codigo
                 cod-est-exec
                 num-projeto
    index chave ch-pro.
    
define temp-table tt-param
    field destino          as integer
    field arquivo          as char    format "x(40)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field i-empresa        as CHAR
    field c-est-exec       as character
    field i-projeto        as integer
    field c-sigla          as character
    field d-data-ini       as date
    field d-data-fim       as date   
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
    field c-item-ini       as character
    field c-item-fim       as character
    field l-item           as logical
    field l-descricao      as logical
    field l-aprop          as logical
    field l-autor          as logical
    field l-mob            as logical
    field l-duplic         as logical
    field l-ord-prod       as logical
    field l-sem-planej     as logical    
    field l-vl-menor       as logical
    field l-vl-maior       as logical
    field de-vl-tenden     as decimal
    field i-moeda          as integer 
    field moeda            as integer
    field c-desc-moeda     as char format "x(40)"
   
       field l-divmil      as logical
   
   .

def temp-table tt-raw-digita
    field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* fim */
  /* definicao das temp-tables */

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
 


    DEF VAR c-desc-divmil AS CHAR NO-UNDO.


def var i-ch-tipo                 as int.
def var i-ch-ori                  as int.
def var i-ch-sub                  as int.
def var i-ch-esp                  as int.
def var i-ch-sec                  as int.
def var i-ch-ord                  as int.
def var i-ch-pro                  as int.
def var i-tot                     as int init 3.
def var l-imp                     as logical.
def var l-ord-prod                as logical no-undo.
def var de-aux-qt1                as decimal.
def var de-aux-vl1                as decimal.
def var de-aux-vl2                as decimal.
def var c-nome-emp                as character format "x(40)".
def var c-rodape1                 as character.
def var c-hora-proc               as character format "x(5)".
def var c-descricao               as character format "x(36)".
def var c-opcao                   as character no-undo.
def var c-it-aux                  as character format "x(16)".  
def var de-vl-tenden              as de format "->>,>>>,>>>,>>9.99".
def var c-nome-est                like estabelec.nome format "x(20)".
def var i-emp-cab                 like proj-inv.ep-codigo.       
def var c-emp-cab                 like empresa.nome format "x(50)".
def var i-est-cab                 like t-cod.cod-est-exec.    
def var c-est-cab                 like estabelec.nome.
def var i-proj-cab                like t-cod.num-projeto.       
def var c-proj-cab1               like proj-inv.descricao.        
def var c-proj-cab2               like proj-inv.descricao-2.
def var i-sig-cab                 like proj-inv.sigla.    
def var i-ord-cab                 like t-cod.num-ordem.   
def var c-ord-cab1                like ordem-inv.descricao.           
def var c-ord-cab2                like ordem-inv.descricao-2.
def var i-sec-cab                 like t-cod.num-secao.            
def var c-sec-cab1                like secao-inv.descricao.
def var c-sec-cab2                like secao-inv.descricao-2. 
def var c-moeda                   as char format "x(13)" extent 3. 
def var i-cont                    as integer.
def var i-moeda                   as integer init 1.
def var i-aux                     as integer.
def var i-put                     as integer.
def var i-empresa                 like empresa.ep-codigo init 0.
def var c-est-exec                like estabelec.cod-estabel init "".
def var i-projeto                 like proj-inv.num-projeto init 0.
def var i-sigla                   like proj-inv.sigla init "".
DEF VAR d-data-ini                LIKE plano-aprov.dt-sit-plano.
DEF VAR d-data-fim                LIKE plano-aprov.dt-sit-plano.
def var i-ord-ini                 like ordem-inv.num-ordem init 0.
def var i-ord-fim                 like ordem-inv.num-ordem init 999.
def var i-sec-ini                 like secao-inv.num-secao init 0.
def var i-sec-fim                 like secao-inv.num-secao init 9.
def var i-esp-ini                 like especialidade.cod-especialidade init 0.
def var i-esp-fim                 like especialidade.cod-especialidade init 99.
def var i-sub-ini                 like sub-espec.cod-sub-espec init 0.
def var i-sub-fim                 like sub-espec.cod-sub-espec init 99.
def var i-ori-ini                 like orig-despesa.cod-origem init 0.
def var i-ori-fim                 like orig-despesa.cod-origem init 9.
def var c-item-ini                like item.it-codigo init "".
def var c-item-fim                like item.it-codigo init "ZZZZZZZZZZZZZZZZ".
def var l-vl-maior                as logical.
def var l-vl-menor                as logical.
def var de-valor                  as decimal no-undo.
def var l-descricao               as logical.
def var l-cab                     as logical.
def var l-mod-ce                  as logical.
def var l-item                    as logical.
def var l-sem-planej              as logical.
def var l-duplic                  as logical.
def var l-aprop                   as logical.
def var l-autor                   as logical.
def var l-mob                     as logical.
def var da-data-proc              as date format "99/99/9999".
def var de-vl-verba               like ordem-inv.vl-verba[1].
def var de-vl-horas               like ordem-inv.vl-verba[1].
def var h-acomp                   as handle no-undo. 
def var c-titulo                  as char format "x(40)".     
def var c-modulo                  as char format "x(15)".
def var moeda-desc                as char format "x(15)".
def var de-tot-qt-horas           as dec no-undo. 

/***************************************************************
**
** tt-edit.i  - Def. Temp Table para Impressao de Editores
**
***************************************************************/

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format 'x(80)':U
    index editor-id is primary unique 
          linha.
/* tt-edit.i */
      

form tt-editor.conteudo at 18 format "x(80)"
     with stream-io no-label no-box width 132 frame f-narrativa-ordem.

/* Definiá∆o de Vari†veis para Traduá∆o */

def var c-especialidade           as char format "x(15)".
def var c-sub                     as char format "x(18)".
def var c-origem                  as char format "x(15)".
def var c-verba                   as char format "x(12)".   
def var c-revisado                as char format "x(15)". 
def var c-emp                     as char format "x(14)".
def var c-ordem                   as char format "x(14)".
def var c-executor                as char format "x(13)".
def var c-secao                   as char format "x(07)".
def var c-projeto                 as char format "x(12)".
def var c-sigla                   as char format "x(07)". 
def var c-mensagem                as char format "x(50)". 
def var c-estimado                as char format "x(19)".
def var c-compromisso             as char format "x(13)".
def var c-desvio                  as char format "x(06)".
def var c-saldos                  as char format "x(06)".
def var c-tot-ori                 as char format "x(15)".
def var c-tot-sub                 as char format "x(28)".
def var c-tot-espec               as char format "x(23)".
def var c-tot-sec                 as char format "x(14)".
def var c-tot-item                as char format "x(16)".
def var c-tot-cont                as char format "x(25)".
def var c-tot-pagto               as char format "x(24)".
def var c-tot-obra                as char format "x(25)".
def var c-tot-ord                 as char format "x(14)".
def var c-tot-proj                as char format "x(16)".
def var c-destino                 as char format "x(15)" no-undo.

def var c-lb-it-codigo         as char format "x(16)" no-undo.
def var c-lb-descricao         as char format "x(26)" no-undo.
def var c-lb-un                as char format "x(2)"  no-undo.
def var c-lb-qt-est-reest      as char format "x(15)" no-undo.
def var c-lb-de-vl-est-reest   as char format "x(15)" no-undo.
def var c-lb-de-tot-est-reest  as char format "x(15)" no-undo.
def var c-lb-de-qt-comp        as char format "x(15)" no-undo.
def var c-lb-de-vl-comp        as char format "x(15)" no-undo.
def var c-lb-de-tot-comp       as char format "x(15)" no-undo.
def var c-lb-de-vl-des-tec     as char format "x(15)" no-undo.
def var c-lb-de-vl-des-eco     as char format "x(15)" no-undo.
def var c-lb-de-vl-estim       as char format "x(15)" no-undo.
def var c-lb-de-fim-proj       as char format "x(14)" no-undo.
def var c-lb-de-vl-tend        as char format "x(14)" no-undo.

/* Fim das Definiá‰es                   */
def var de-mil as integer no-undo.
assign de-mil = 1000.

    if NOT tt-param.l-divmil then
       assign de-mil = 1.


assign i-empresa      = tt-param.i-empresa
       c-est-exec     = tt-param.c-est-exec
       i-projeto      = tt-param.i-projeto
       i-sigla        = tt-param.c-sigla
       d-data-ini     = tt-param.d-data-ini
       d-data-fim     = tt-param.d-data-fim
       i-ord-ini      = tt-param.i-ord-ini
       i-ord-fim      = tt-param.i-ord-fim
       i-sec-ini      = tt-param.i-sec-ini
       i-sec-fim      = tt-param.i-sec-fim
       i-esp-ini      = tt-param.i-esp-ini
       i-esp-fim      = tt-param.i-esp-fim
       i-sub-ini      = tt-param.i-sub-ini
       i-sub-fim      = tt-param.i-sub-fim
       i-ori-ini      = tt-param.i-ori-ini
       i-ori-fim      = tt-param.i-ori-fim
       c-item-ini     = tt-param.c-item-ini
       c-item-fim     = tt-param.c-item-fim
       l-item         = tt-param.l-item
       l-descricao    = tt-param.l-descricao
       l-aprop        = tt-param.l-aprop
       l-autor        = tt-param.l-autor
       l-mob          = tt-param.l-mob
       l-duplic       = tt-param.l-duplic
       l-ord-prod     = tt-param.l-ord-prod
       l-sem-planej   = tt-param.l-sem-planej
       l-vl-menor     = tt-param.l-vl-menor
       l-vl-maior     = tt-param.l-vl-maior
       de-vl-tenden   = tt-param.de-vl-tenden.

if tt-param.i-moeda = 1 then do:
   /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Principal",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
   assign c-moeda[1] = string(tt-param.i-moeda)
          i-moeda    = tt-param.i-moeda.
          moeda-desc = return-value.
end.
if tt-param.i-moeda = 2 then do:
   /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Alternativa_1",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
   assign c-moeda[2] = string(tt-param.i-moeda)
          i-moeda    = tt-param.i-moeda.
          moeda-desc = return-value.
end.
if tt-param.i-moeda = 3 then do:
   /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Alternativa_2",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
   assign c-moeda[3] = string(tt-param.i-moeda)
          i-moeda    = tt-param.i-moeda.
          moeda-desc = return-value.
end.
form
   i-empresa    colon 100 
   c-est-exec   colon 100  
   i-projeto    colon 100  
   i-sigla      colon 100
   d-data-ini   colon 100 
   "|<"         colon 133
   ">|"         colon 140
   d-data-fim   colon 145 no-label
   i-ord-ini    colon 100 
   "|<"         colon 133
   ">|"         colon 140
   i-ord-fim    colon 145 no-label
   i-sec-ini    colon 100 
   "|<"         colon 133
   ">|"         colon 140 
   i-sec-fim    colon 145 no-label 
   i-esp-ini    colon 100 
   "|<"         colon 133
   ">|"         colon 140 
   i-esp-fim    colon 145 no-label 
   i-sub-ini    colon 100 
   "|<"         colon 133
   ">|"         colon 140 
   i-sub-fim    colon 145 no-label 
   i-ori-ini    colon 100 
   "|<"         colon 133
   ">|"         colon 140 
   i-ori-fim    colon 145 no-label  
   c-item-ini   colon 100 
   "|<"         colon 133
   ">|"         colon 140 
   c-item-fim   colon 145 no-label  skip(02)
   l-item       colon 100 
   l-descricao  colon 100 
   l-aprop      colon 100 
   l-autor      colon 100 
   l-duplic     colon 100   
   l-ord-prod   colon 100 
   l-mob        colon 100 
   l-sem-planej colon 100
   l-vl-menor   colon 100 
   l-vl-maior   colon 100 
   de-vl-tenden colon 100 
   moeda-desc   colon 100 skip(02)
   c-destino    colon 100
   tt-param.arquivo no-label skip
   tt-param.usuario colon 100    
   
       skip
       c-desc-divmil colon 100
   
   with stream-io width 233 no-box row 7 side-labels frame f-selec-param. 

def var c-lb-sim                  as char no-undo.
def var c-lb-nao                  as char no-undo.   

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Sim",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-sim = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "N∆o",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-nao = trim(return-value).
assign l-item:format in frame f-selec-param = c-lb-sim + "/" + c-lb-nao.
assign l-aprop:format in frame f-selec-param = c-lb-sim + "/" + c-lb-nao.
assign l-autor:format in frame f-selec-param = c-lb-sim + "/" + c-lb-nao.
assign l-duplic:format in frame f-selec-param = c-lb-sim + "/" + c-lb-nao.
assign l-mob:format in frame f-selec-param = c-lb-sim + "/" + c-lb-nao.
assign l-descricao:format in frame f-selec-param = c-lb-sim + "/" + c-lb-nao.
assign l-ord-prod:format in frame f-selec-param = c-lb-sim + "/" + c-lb-nao.
assign l-sem-planej:format in frame f-selec-param = c-lb-sim + "/" + c-lb-nao.
assign l-vl-maior:format in frame f-selec-param = c-lb-sim + "/" + c-lb-nao.
assign l-vl-menor:format in frame f-selec-param = c-lb-sim + "/" + c-lb-nao.


form header    
    c-emp    at 01
    ":"
    i-emp-cab                
    space(2)
    c-emp-cab       
    space(2)
    c-executor
    ":"
    i-est-cab               
    space(2)
    c-est-cab        
    space(2)  
    c-projeto  ":"
    i-proj-cab         
    space(2)
    c-proj-cab1  
    space(2) 
    c-sigla    ":"
    i-sig-cab    
    space(2)
    c-ordem    at 01 
    ":" 
    i-ord-cab       
    space(2)
    c-ord-cab1   
    space(18)
    c-secao   
    ":"
    i-sec-cab   
    space(2)
    c-sec-cab1    
    space(0)
    c-sec-cab2   
    c-mensagem at 151 skip 
    "*----------------"  at 48
    c-estimado
    "----------------* *------------------"
    c-compromisso
    "---------------* *-----------"
    c-desvio
    "----------* *------------------"
    c-saldos    
    "-----------------*" skip
    c-lb-it-codigo         format "x(16)"
    c-lb-descricao         format "x(26)"           
    c-lb-un                format "x(2)"
    c-lb-qt-est-reest      format "x(17)"
    c-lb-de-vl-est-reest   format "x(18)"
    c-lb-de-tot-est-reest  format "x(18)"
    c-lb-de-qt-comp        format "x(18)"
    c-lb-de-vl-comp        format "x(15)"
    c-lb-de-tot-comp       format "x(15)"
    c-lb-de-vl-des-tec     format "x(15)"
    c-lb-de-vl-des-eco     format "x(15)"
    c-lb-de-vl-estim       format "x(15)"
    c-lb-de-fim-proj       format "x(14)"
    c-lb-de-vl-tend        format "x(14)" skip
    "---------------- --------------------------"
    "-- ----------------- ------------------ ------------------" 
    "------------------ --------------- ---------------"
    "--------------- --------------- ---------------"
    "-------------- --------------"
    with stream-io width 233 page-top no-box frame f-quebra.   



    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Divide_por_1000_(Mil)",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    assign c-desc-divmil:label in frame f-selec-param = return-value.

/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "sub-div-ordem":U, 
                    input "ep-codigo":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign i-empresa:label in frame f-selec-param = return-value.
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "sub-div-ordem":U, 
                    input "cod-est-exec":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign c-est-exec:label in frame f-selec-param = return-value.
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "sub-div-ordem":U, 
                    input "num-projeto":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign i-projeto:label in frame f-selec-param = return-value.
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
 
assign i-sigla:label in frame f-selec-param = return-value.
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
 
assign i-ord-ini:label in frame f-selec-param = return-value. 
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
 
assign i-sec-ini:label in frame f-selec-param = return-value.
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
 
assign i-esp-ini:label in frame f-selec-param = return-value.  
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
 
assign i-sub-ini:label in frame f-selec-param = return-value.
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
 
assign i-ori-ini:label in frame f-selec-param = return-value.
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mgind":U, 
                    input "item":U, 
                    input "it-codigo":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign c-item-ini:label in frame f-selec-param = return-value.   
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mgind":U, 
                    input "item":U, 
                    input "it-codigo":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign l-item:label in frame f-selec-param = return-value.  
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mgind":U, 
                    input "item":U, 
                    input "descricao-1":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign l-descricao:label in frame f-selec-param = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Data",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign d-data-ini:label in frame f-selec-param = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Documentos_Sem_Planejamento",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign l-sem-planej:label in frame f-selec-param = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Duplicatas",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign l-duplic:label in frame f-selec-param = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Apropriaá∆o_Cont†bil",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign l-aprop:label in frame f-selec-param = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Autorizaá∆o_de_Pagamento",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign l-autor:label in frame f-selec-param = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Ordens_de_Produá∆o",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign l-ord-prod:label in frame f-selec-param = trim(return-value).    
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "M∆o-de-Obra_Manutená∆o",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign l-mob:label in frame f-selec-param = trim(return-value).    
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Valor_Tendància_Menor_que_Estimado/Reestimado",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign l-vl-menor:label in frame f-selec-param = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Valor_Tendància_Maior_que_Estimado/Reestimado",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign l-vl-maior:label in frame f-selec-param = trim(return-value).    
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Valor_Tendància_Acima_de",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign de-vl-tenden:label in frame f-selec-param = trim(return-value).  
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Moeda",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign moeda-desc:label in frame f-selec-param = trim(return-value).  
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "COD_-_Caderno_Operacional_Detalhado",
                    input "*",
                    input "c") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-titulo = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "INVESTIMENTO",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-modulo = return-value.
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "sub-div-ordem":U, 
                    input "ep-codigo":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign c-emp = return-value. 
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "sub-div-ordem":U, 
                    input "cod-est-exec":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign c-executor = return-value.  
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "especialidade":U, 
                    input "cod-especialidade":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign c-especialidade = return-value.
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "sub-espec":U, 
                    input "cod-sub-espec":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign c-sub = return-value.   
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "orig-despesa":U, 
                    input "cod-origem":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign c-origem = return-value.
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "ordem-inv":U, 
                    input "vl-verba":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign c-verba = return-value. 
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Valor_Revisado",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign  c-revisado = return-value.  
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
 
assign c-projeto = return-value.
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
 
assign c-sigla = return-value.  
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "ordem-inv":U, 
                    input "num-ordem":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign c-ordem = return-value. 
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "secao-inv":U, 
                    input "num-secao":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign c-secao = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "(Valores_em_mil,_exceto_os_valores_unit†rios)",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-mensagem = return-value.


    IF  NOT tt-param.l-divmil THEN
        assign c-mensagem = "".



/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "ESTIMADO/REESTIMADO",
                    input "*",
                    input "c") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-estimado = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "COMPROMISSADO",
                    input "*",
                    input "c") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-compromisso = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "DESVIO",
                    input "*",
                    input "c") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-desvio = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "SALDOS",
                    input "*",
                    input "c") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-saldos = trim(return-value). 
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Total_Origem",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-tot-ori = return-value. 
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Total_Sub-especialidade",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-tot-sub = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Total_Especialidade",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-tot-espec = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Total_Seá∆o",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-tot-sec = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Total_do_Item",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-tot-item = return-value. 
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Total_Aprop._Cont†bil",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-tot-cont = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Total_Autoriz._Pagto",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-tot-pagto = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Total_M∆o-de-Obra_Man.",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-tot-obra = return-value.  
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Total_Ordem",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-tot-ord = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Total_Projeto",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-tot-proj = return-value.

/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mgind":U, 
                    input "item":U, 
                    input "it-codigo":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign c-lb-it-codigo = trim(return-value).
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mgind":U, 
                    input "item":U, 
                    input "desc-item":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign c-lb-descricao = trim(return-value).
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mgind":U, 
                    input "item":U, 
                    input "un":U, 
                    input integer("1":U)).

/* ut-field.i */
 
assign c-lb-un = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Tendància",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-de-vl-tend = fill(" " , 14 - length(trim(return-value))) + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Fim_Projeto",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-de-fim-proj = fill(" " , 14 - length(trim(return-value))) + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Estimativa",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-de-vl-estim = fill(" " , 15 - length(trim(return-value))) + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Econìmico",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-de-vl-des-eco = fill(" " , 15 - length(trim(return-value))) + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "TÇcnico",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-de-vl-des-tec = fill(" " , 15 - length(trim(return-value))) + trim(return-value).
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mgind":U, 
                    input "recebimento":U, 
                    input "valor-total":U, 
                    input integer("2":U)).

/* ut-field.i */
 
assign c-lb-de-tot-comp = fill(" " , 15 - length(trim(return-value))) + trim(return-value).
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mgind":U, 
                    input "recebimento":U, 
                    input "valor-total":U, 
                    input integer("2":U)).

/* ut-field.i */
 
assign c-lb-de-tot-est-reest = fill(" " , 18 - length(trim(return-value))) + trim(return-value).
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "ord-ped":U, 
                    input "vl-item":U, 
                    input integer("2":U)).

/* ut-field.i */
 
assign c-lb-de-vl-comp = fill(" " , 15 - length(trim(return-value))) + trim(return-value).
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "estim-mat":U, 
                    input "quant-estim":U, 
                    input integer("2":U)).

/* ut-field.i */
 
assign c-lb-qt-est-reest = fill(" " , 18 - length(trim(return-value))) + trim(return-value).
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "estim-mat":U, 
                    input "vl-unit-estim":U, 
                    input integer("2":U)).

/* ut-field.i */
 
assign c-lb-de-vl-est-reest = fill(" " , 18 - length(trim(return-value))) + trim(return-value).
/*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "ord-ped":U, 
                    input "quant-comp":U, 
                    input integer("2":U)).

/* ut-field.i */
 
assign c-lb-de-qt-comp = fill(" " , 18 - length(trim(return-value))) + trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Destino",
                    input "*",
                    input "l") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-destino:label in frame f-selec-param = trim(return-value).

form
    t-cod.it-codigo              
    t-cod.descricao   format "x(26)"           
    t-cod.un          format "x(2)"
    t-cod.de-qt-est-reest       format "->>>,>>>,>>9.9999"
    t-cod.de-vl-est-reest       format "->>>>,>>>,>>9.9999"
    t-cod.de-tot-est-reest      format "->>>>>>,>>>,>>9.99"
    t-cod.de-qt-comp            format "->>>>,>>>,>>9.9999"
    t-cod.de-vl-comp            format "->,>>>,>>9.9999"
    t-cod.de-tot-comp           format "->>>,>>>,>>9.99"
    t-cod.de-vl-des-tec         format "->>>,>>>,>>9.99"
    t-cod.de-vl-des-eco         format "->>>,>>>,>>9.99"
    t-cod.de-vl-estim           format "->>>,>>>,>>9.99"
    t-cod.de-fim-proj           format "->>,>>>,>>9.99"
    t-cod.de-vl-tend            format "->>,>>>,>>9.99" 
    with stream-io width 233 no-labels no-box frame f-dados.    


form header
    fill("-", 233) format "x(233)" skip
    c-empresa c-titulo-relat at 100
    "Folha:" at 223 page-number  at 230 format ">>>9" skip
    fill("-",211) format "x(211)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 235 no-labels no-box page-top frame f-cabper1.


find first param-global no-lock no-error.
if  not avail param-global then do:
    run utp/ut-msgs.p (input "show",
                       input 16,
                       input "").
    return error.
end.
assign c-empresa = grupo
       l-mod-ce  = no.

if  param-global.modulo-ce = yes then
    assign l-mod-ce = yes.

find first param-inv no-lock no-error.
if  not avail param-inv then do:
    run utp/ut-msgs.p (input "show",
                       input 6471,
                       input "").
    return error.
end.

assign c-programa     = "ESIN0801RP"
       c-versao       = ""
       c-revisao      = ""
       c-sistema      = c-modulo 
       c-titulo-relat = c-titulo
.

    ASSIGN c-destino       = /******************************************************************
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


        assign i-tot = 0.
        if  l-item = yes then
            assign i-tot = i-tot + 1.
        if  l-aprop = yes then
            assign i-tot = i-tot + 1.
        if  l-autor = yes then
            assign i-tot = i-tot + 1.     



form header
    fill("-", 233) format "x(233)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number  at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 233 no-labels no-box page-top frame f-cabec.

form header
    fill("-", 233) format "x(233)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number  at 128 format ">>>>9" skip
    "Periodo:" i-numper-x at 10 "-"
    da-iniper-x at 15 "a" da-fimper-x
    fill("-", 74) format "x(72)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 233 no-labels no-box page-top frame f-cabper.



c-rodape = "DATASUL - " + c-sistema + " - " + c-programa + " - V:" + c-versao
         + "." + c-revisao.
c-rodape = fill("-", 233 - length(c-rodape)) + c-rodape.

form header
    c-rodape format "x(233)"
    with stream-io width 233 no-labels no-box page-bottom frame f-rodape.
  /* Definiá∆o cabeáalho e rodapÇ */               

/******************************************************************************
**  cd9999.i3 - define os campos ct-initial e sc-initial para usar valor
**              inicial da conta contabil independente do tipo (inteiro
**              ou caracter).
**  Versao: G.00 Alcides 07/10/93
*****************************************************************************/

def var ct-initial      like movto-estoq.ct-codigo no-undo.
def var sc-initial      like movto-estoq.sc-codigo no-undo.

/* fim */
 

c-rodape  = "DATASUL - " + c-sistema + " - " + c-programa + " - V:" + c-prg-vrs.
c-rodape1 = fill("-", 233 - length(c-rodape)) + c-rodape.

form header
    c-rodape1 format "x(233)"
    with stream-io width 233 no-labels no-box page-bottom frame f-rodape1.


/*--------------------------- IMPRESSAO --------------------------------------*/

run utp/ut-trfrrp.p (input frame f-rodape1:handle).
run utp/ut-trfrrp.p (input frame f-cabper1:handle).
run utp/ut-trfrrp.p (input frame f-dados:handle).
run utp/ut-trfrrp.p (input frame f-quebra:handle).
run utp/ut-trfrrp.p (input frame f-selec-param:handle).
run utp/ut-trfrrp.p (input frame f-narrativa-ordem:handle).
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
 

        view frame f-cabper1.
        view frame f-rodape1.
        view frame f-quebra.

        /*******************************************************************************
**
**   ESIN0801.I - Rotina de Impressao.
**
******************************************************************************/
assign l-imp = no.

for each t-cod exclusive-lock:
    delete t-cod.
end.

for each t-tot-tipo exclusive-lock:
    delete t-tot-tipo.
end.

for each t-tot-ori exclusive-lock:
    delete t-tot-ori.
end.

for each t-tot-sub exclusive-lock:
    delete t-tot-sub.
end.

for each t-tot-esp exclusive-lock:
    delete t-tot-esp.
end.

for each t-tot-sec exclusive-lock:
    delete t-tot-sec.
end.

for each t-tot-ord exclusive-lock:
    delete t-tot-ord.
end.

for each t-tot-pro exclusive-lock:
    delete t-tot-pro.
end.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp (input c-titulo).
for each sub-div-ordem use-index emp-est-proj
    where sub-div-ordem.ep-codigo          = i-empresa
    and   sub-div-ordem.cod-est-exec       = c-est-exec
    and   sub-div-ordem.num-projeto        = i-projeto 
    and   sub-div-ordem.num-ordem         >= i-ord-ini
    and   sub-div-ordem.num-ordem         <= i-ord-fim
    and   sub-div-ordem.num-secao         >= i-sec-ini
    and   sub-div-ordem.num-secao         <= i-sec-fim
    and   sub-div-ordem.cod-especialidade >= i-esp-ini
    and   sub-div-ordem.cod-especialidade <= i-esp-fim
    and   sub-div-ordem.cod-sub-espec     >= i-sub-ini
    and   sub-div-ordem.cod-sub-espec     <= i-sub-fim
    and   sub-div-ordem.cod-origem        >= i-ori-ini
    and   sub-div-ordem.cod-origem        <= i-ori-fim
    no-lock:
    
    
    find first proj-inv use-index emp-est-proj
        where proj-inv.ep-codigo    = sub-div-ordem.ep-codigo
        and   proj-inv.cod-est-exec = sub-div-ordem.cod-est-exec
        and   proj-inv.num-projeto  = sub-div-ordem.num-projeto
        and   proj-inv.sigla        = i-sigla
        no-lock no-error.
    if  not avail proj-inv then
        next.
  
    if  l-item = yes then do:
        for each estim-mat use-index emp-ordm
            where estim-mat.ep-codigo      = sub-div-ordem.ep-codigo
            and   estim-mat.num-ord-magnus = sub-div-ordem.num-ord-magnus
            and   estim-mat.it-codigo     >= c-item-ini
            and   estim-mat.it-codigo     <= c-item-fim
            no-lock:

            if  estim-mat.grupo-estim = "" then
                assign c-it-aux = estim-mat.it-codigo.
            else
                assign c-it-aux = estim-mat.grupo-estim.

            find first item where item.it-codigo = c-it-aux no-lock no-error.

            create t-cod.                           
            assign t-cod.dt-trans           = estim-mat.dt-atualizacao
                   t-cod.ep-codigo          = sub-div-ordem.ep-codigo
                   t-cod.cod-est-exec       = sub-div-ordem.cod-est-exec
                   t-cod.num-projeto        = sub-div-ordem.num-projeto
                   t-cod.num-ordem          = sub-div-ordem.num-ordem
                   t-cod.num-secao          = sub-div-ordem.num-secao
                   t-cod.cod-especialidade  = sub-div-ordem.cod-especialidade
                   t-cod.cod-sub-espec      = sub-div-ordem.cod-sub-espec
                   t-cod.cod-origem         = sub-div-ordem.cod-origem
                   t-cod.num-ord-magnus     = sub-div-ordem.num-ord-magnus
                   t-cod.de-qt-est-reest    = estim-mat.quant-estim    
                   t-cod.de-vl-est-reest    = estim-mat.vl-unit-estim[i-moeda]
                   t-cod.de-fim-proj        = estim-mat.sl-fim-proj[i-moeda]
                   t-cod.ind-desv-tec       = estim-mat.ind-desv-tec
                   t-cod.tipo               = 1
                   t-cod.it-codigo          = estim-mat.it-codigo.
                                        
            if  estim-mat.grupo-estim  = " " then 
                assign t-cod.it-codigo = estim-mat.it-codigo
                       t-cod.descricao = item.descricao-1 +
                                         item.descricao-2
                       t-cod.un        = item.un.
            else
                assign t-cod.it-codigo = estim-mat.it-codigo
                       t-cod.descricao = estim-mat.descricao.           


            if  sub-div-ordem.vl-reestimado[i-moeda] > 0 then
                assign t-cod.de-vl-verba = sub-div-ordem.vl-reestimado[i-moeda]
                       t-cod.flag        = "R".
            else
                assign t-cod.de-vl-verba = sub-div-ordem.vl-estimado[i-moeda]
                       t-cod.flag        = "E".

            if  estim-mat.quant-reestim > 0 then
                assign t-cod.de-qt-est-reest = estim-mat.quant-reestim. 

            if  estim-mat.vl-unit-reestim[i-moeda] > 0 then
                assign t-cod.de-vl-est-reest = 
                       estim-mat.vl-unit-reestim[i-moeda].

            assign t-cod.de-tot-est-reest = t-cod.de-qt-est-reest *
                                            t-cod.de-vl-est-reest.
        end.    

        for each plano-aprov use-index emp-ordm
            where plano-aprov.ep-codigo       = sub-div-ordem.ep-codigo
            and   plano-aprov.num-ord-magnus  = sub-div-ordem.num-ord-magnus
            and  (plano-aprov.num-solicitacao <> 0 or l-sem-planej)
            and   plano-aprov.it-codigo     >= c-item-ini
            and   plano-aprov.it-codigo     <= c-item-fim
            and   plano-aprov.cod-sit-solic  <> "E"
            /* GIL
            AND   plano-aprov.dt-atualizacao >= d-data-ini 
            AND   plano-aprov.dt-atualizacao <= d-data-fim
            */ no-lock:
            
            if plano-aprov.grupo-estim <> " " then
               assign c-it-aux = plano-aprov.grupo-estim.
            else
               assign c-it-aux = plano-aprov.it-codigo.
               
            find first item where item.it-codigo = c-it-aux no-lock no-error.   

            find first ord-ped use-index emp-ordm
                 where ord-ped.ep-codigo      = plano-aprov.ep-codigo
                 and   ord-ped.num-ord-magnus = plano-aprov.num-ord-magnus
                 and   ord-ped.it-codigo      = c-it-aux
                 and   ord-ped.seq-planej     = plano-aprov.seq-planej
                 and   ord-ped.num-ord-comp   = plano-aprov.num-ord-comp
                 and   ord-ped.seq-comp       = plano-aprov.seq-comp
                 and   ord-ped.cod-area       = 0 
                 and   ord-ped.cod-sit-ped   <> "C"
                 and   ord-ped.cod-sit-ped   <> "E"
                 and   ord-ped.num-pedido    <> 0
                 and   ord-ped.cod-sit-comp  <> "E"
                 /* GIL
                 AND   ord-ped.dt-atualizacao >= d-data-ini
                 AND   ord-ped.dt-atualizacao <= d-data-fim
                 */
                 no-lock no-error.
            if  not avail ord-ped then 
                next.      

            find first estim-mat
                 where estim-mat.ep-codigo     = sub-div-ordem.ep-codigo
                 and   estim-mat.cod-est-exec  = sub-div-ordem.cod-est-exec
                 and   estim-mat.num-projeto   = sub-div-ordem.num-projeto
                 and   estim-mat.num-ordem     = sub-div-ordem.num-ordem
                 and   estim-mat.num-secao     = sub-div-ordem.num-secao
                 and   estim-mat.cod-espec     = sub-div-ordem.cod-espec
                 and   estim-mat.cod-sub-espec = sub-div-ordem.cod-sub-espec
                 and   estim-mat.cod-origem    = sub-div-ordem.cod-origem
                 and   estim-mat.it-codigo     = plano-aprov.it-codigo
                 no-lock no-error.
                
            find first t-cod use-index codigo
                where t-cod.ep-codigo         = sub-div-ordem.ep-codigo
                and   t-cod.cod-est-exec      = sub-div-ordem.cod-est-exec
                and   t-cod.num-projeto       = sub-div-ordem.num-projeto
                and   t-cod.num-ordem         = sub-div-ordem.num-ordem
                and   t-cod.num-secao         = sub-div-ordem.num-secao
                and   t-cod.cod-especialidade = sub-div-ordem.cod-especialidade
                and   t-cod.cod-sub-espec     = sub-div-ordem.cod-sub-espec
                and   t-cod.cod-origem        = sub-div-ordem.cod-origem
                and   t-cod.it-codigo         = plano-aprov.it-codigo
                no-lock no-error.

            if  not avail t-cod then do:               

                create t-cod.                           
                assign t-cod.dt-trans          = plano-aprov.dt-atualizacao
                       t-cod.ep-codigo         = sub-div-ordem.ep-codigo
                       t-cod.cod-est-exec      = sub-div-ordem.cod-est-exec
                       t-cod.num-projeto       = sub-div-ordem.num-projeto
                       t-cod.num-ordem         = sub-div-ordem.num-ordem
                       t-cod.num-secao         = sub-div-ordem.num-secao
                       t-cod.cod-especialidade = sub-div-ordem.cod-especialidade
                       t-cod.cod-sub-espec     = sub-div-ordem.cod-sub-espec
                       t-cod.cod-origem        = sub-div-ordem.cod-origem
                       t-cod.num-ord-magnus    = sub-div-ordem.num-ord-magnus
                       t-cod.tipo              = 1.
    
                if  plano-aprov.grupo-estim  = " " then do:
                    assign t-cod.it-codigo = plano-aprov.it-codigo.
                    if  avail item then
                        assign  t-cod.descricao = item.descricao-1 +
                                                  item.descricao-2
                                t-cod.un        = item.un.
                end.
                else
                    assign t-cod.it-codigo = plano-aprov.it-codigo
                           t-cod.descricao = estim-mat.descricao. 
       
                if  sub-div-ordem.vl-reestimado[i-moeda] > 0 then
                    assign t-cod.de-vl-verba = 
                           sub-div-ordem.vl-reestimado[i-moeda]
                           t-cod.flag        = "R".
                else
                    assign t-cod.de-vl-verba = 
                           sub-div-ordem.vl-estimado[i-moeda]
                           t-cod.flag        = "E".
            end.

            assign de-aux-qt1 = 0
                   de-aux-vl1 = 0.
            for each ord-ped 
                where ord-ped.ep-codigo      = t-cod.ep-codigo
                and   ord-ped.num-ord-magnus = t-cod.num-ord-magnus
                and   ord-ped.it-codigo      = c-it-aux
                and   ord-ped.seq-planej     = plano-aprov.seq-planej
                and   ord-ped.num-ord-comp   = plano-aprov.num-ord-comp
                and   ord-ped.seq-comp       = plano-aprov.seq-comp
                and   ord-ped.cod-area       = 0 
                and   ord-ped.cod-sit-ped   <> "C" 
                and   ord-ped.cod-sit-ped   <> "E"
                and   ord-ped.cod-sit-comp  <> "E"                
                and   ord-ped.num-pedido    <> 0
                /* GIL
                AND   ord-ped.dt-atualizacao >= d-data-ini
                AND   ord-ped.dt-atualizacao <= d-data-fim
                */
                no-lock:
                
                if  ord-ped.cod-sit-comp = "F" then do:
                    for each movto-nf use-index emp-ordm
                        where movto-nf.ep-codigo      = ord-ped.ep-codigo 
                        and   movto-nf.num-ord-magnus = ord-ped.num-ord-magnus
                        and   movto-nf.num-pedido     = ord-ped.num-pedido
                        and   movto-nf.seq-pedido     = ord-ped.seq-pedido
                        and   movto-nf.cod-area       = ord-ped.cod-area
                        and   movto-nf.num-ord-comp   = ord-ped.num-ord-comp
                        and   movto-nf.seq-comp       = ord-ped.seq-comp 
                        /* GIL
                        AND   movto-nf.dt-atualizacao >= d-data-ini
                        AND   movto-nf.dt-atualizacao <= d-data-fim
                        */ no-lock:
                        
                        if  movto-nf.tipo-trans = 1 then
                            assign de-aux-qt1 = de-aux-qt1 + movto-nf.quant-nf
                                   de-aux-vl1 = de-aux-vl1 + movto-nf.vl-nota[i-moeda].
                        else
                            assign de-aux-qt1 = de-aux-qt1 - movto-nf.quant-nf
                                   de-aux-vl1 = de-aux-vl1 - movto-nf.vl-nota[i-moeda].
                    end.

                    assign de-aux-vl1 = 
                           (ord-ped.vl-item[i-moeda] / ord-ped.quant-comp) *
                           de-aux-qt1.
                end.
                else
                    assign de-aux-qt1 = de-aux-qt1 + ord-ped.quant-comp 
                           de-aux-vl1 = de-aux-vl1 + ord-ped.vl-item[i-moeda].
                           
                if de-aux-vl1 = ? then assign de-aux-vl1 = 0.    
                 
            end.
            
            assign t-cod.de-qt-comp  = t-cod.de-qt-comp  + de-aux-qt1
                   t-cod.de-tot-comp = t-cod.de-tot-comp + de-aux-vl1
                   t-cod.de-vl-comp  = t-cod.de-tot-comp / 
                                       t-cod.de-qt-comp.

                                       
            if t-cod.de-vl-comp = ? then
               assign t-cod.de-vl-comp = 0.
        end.

    end.

    if  l-aprop = yes then do:
        for each movto-apr use-index  emp-est-proj where
                 movto-apr.ep-codigo         = sub-div-ordem.ep-codigo       and
                 movto-apr.cod-est-exec      = sub-div-ordem.cod-est-exec    and
                 movto-apr.num-projeto       = sub-div-ordem.num-projeto     and
                 movto-apr.num-ordem         = sub-div-ordem.num-ordem       and
                 movto-apr.num-secao         = sub-div-ordem.num-secao       and
                 movto-apr.cod-especialidade = sub-div-ordem.cod-especialida and
                 movto-apr.cod-sub-espec     = sub-div-ordem.cod-sub-espec   and
                 movto-apr.cod-origem        = sub-div-ordem.cod-origem      and
                (movto-apr.esp-docto         = "DIV" or 
                 movto-apr.esp-docto         = "REQ")
             /* GIL
             and movto-apr.dt-trans         >= tt-param.d-data-ini
             and movto-apr.dt-trans         <= tt-param.d-data-fim
             */ no-lock:
                 
            create t-cod.
            assign t-cod.dt-trans           = movto-apr.dt-trans
                   t-cod.ep-codigo          = sub-div-ordem.ep-codigo
                   t-cod.cod-est-exec       = sub-div-ordem.cod-est-exec
                   t-cod.num-projeto        = sub-div-ordem.num-projeto
                   t-cod.num-ordem          = sub-div-ordem.num-ordem
                   t-cod.num-secao          = sub-div-ordem.num-secao
                   t-cod.cod-especialidade  = sub-div-ordem.cod-especialidade
                   t-cod.cod-sub-espec      = sub-div-ordem.cod-sub-espec
                   t-cod.cod-origem         = sub-div-ordem.cod-origem
                   t-cod.it-codigo          = "DIV " +  
                                              string(serie-docto,"xxx") + " " +
                                              string(nro-docto)
                   t-cod.descricao          = movto-apr.observacao
                   t-cod.un                 = " "
                   t-cod.num-ord-magnus     = sub-div-ordem.num-ord-magnus
                   t-cod.de-qt-est-reest    = 0                        
                   t-cod.de-vl-est-reest    = 0                          
                   t-cod.de-tot-est-reest   = 0
                   t-cod.de-fim-proj        = 0
                   t-cod.de-tot-comp        = movto-apr.vl-mat[i-moeda] + movto-apr.vl-mob[i-moeda]
                   t-cod.de-qt-comp         = movto-apr.quant-mov
                   t-cod.tipo               = 2.
            
                   
            if  movto-apr.quant-mov <= 0 then
                assign t-cod.de-qt-comp = 1.
                
            if  movto-apr.tipo-trans = 2 then
                assign t-cod.de-tot-comp = t-cod.de-tot-comp * -1.
             
            assign t-cod.de-vl-comp = t-cod.de-tot-comp / t-cod.de-qt-comp.
                                
            if  sub-div-ordem.vl-reestimado[i-moeda] > 0 then
                assign t-cod.de-vl-verba = sub-div-ordem.vl-reestimado[i-moeda]
                       t-cod.flag        = "R".
            else
                assign t-cod.de-vl-verba = sub-div-ordem.vl-estimado[i-moeda]
                       t-cod.flag        = "E".
        end.
    end.
    if  l-autor = yes or l-duplic then do:
        for each movto-apr use-index emp-est-proj where
                 movto-apr.ep-codigo         = sub-div-ordem.ep-codigo      and
                 movto-apr.cod-est-exec      = sub-div-ordem.cod-est-exec   and
                 movto-apr.num-projeto       = sub-div-ordem.num-projeto    and
                 movto-apr.num-ordem         = sub-div-ordem.num-ordem      and
                 movto-apr.num-secao         = sub-div-ordem.num-secao      and
                 movto-apr.cod-especialidade = sub-div-ordem.cod-especialida and
                 movto-apr.cod-sub-espec     = sub-div-ordem.cod-sub-espec  and
                 movto-apr.cod-origem        = sub-div-ordem.cod-origem     and
                 movto-apr.num-pedido        = 0                            and
                (movto-apr.transacao         = "PEF" or 
                 movto-apr.transacao         = "IMD")
             /* GIL
             and movto-apr.dt-trans         >= tt-param.d-data-ini
             and movto-apr.dt-trans         <= tt-param.d-data-fim
             */ no-lock:
            
            
            if movto-apr.transacao = "PEF" and not l-autor then next.           
            if movto-apr.transacao = "IMD" and not l-duplic then next.

            create t-cod.

            if movto-apr.transacao = "PEF" then
               assign t-cod.it-codigo = "PEF ".
            else
               assign t-cod.it-codigo = movto-apr.esp-docto + " ".
               
            assign t-cod.it-codigo = t-cod.it-codigo + 
                                     string(serie-docto,"xxx") + " " +
                                     string(nro-docto).
             
            assign t-cod.dt-trans           = movto-apr.dt-trans
                   t-cod.ep-codigo          = sub-div-ordem.ep-codigo
                   t-cod.cod-est-exec       = sub-div-ordem.cod-est-exec
                   t-cod.num-projeto        = sub-div-ordem.num-projeto
                   t-cod.num-ordem          = sub-div-ordem.num-ordem
                   t-cod.num-secao          = sub-div-ordem.num-secao
                   t-cod.cod-especialidade  = sub-div-ordem.cod-especialidade
                   t-cod.cod-sub-espec      = sub-div-ordem.cod-sub-espec
                   t-cod.cod-origem         = sub-div-ordem.cod-origem
                   t-cod.descricao          = movto-apr.observacao
                   t-cod.un                 = " "
                   t-cod.num-ord-magnus     = sub-div-ordem.num-ord-magnus
                   t-cod.de-qt-est-reest    = 0                        
                   t-cod.de-vl-est-reest    = 0                          
                   t-cod.de-tot-est-reest   = 0
                   t-cod.de-qt-comp         = 1
                   t-cod.de-fim-proj        = 0
                   t-cod.de-tot-comp        = movto-apr.vl-mat[i-moeda] + movto-apr.vl-mob[tt-param.i-moeda] 
                   t-cod.de-vl-comp         = movto-apr.vl-mat[i-moeda] + movto-apr.vl-mob[tt-param.i-moeda]
                   t-cod.tipo               = 3.

            if  movto-apr.tipo-trans = 2 then
                assign t-cod.de-tot-comp = t-cod.de-tot-comp * -1.
            
            if  sub-div-ordem.vl-reestimado[i-moeda] > 0 then
                assign t-cod.de-vl-verba = sub-div-ordem.vl-reestimado[i-moeda]
                       t-cod.flag        = "R".
            else
                assign t-cod.de-vl-verba = sub-div-ordem.vl-estimado[i-moeda]
                       t-cod.flag        = "E".
        end.
           
    end.


    if  l-mob then do:
        assign de-vl-horas = 0.

        for each  ordem-man use-index emp-est-proj
            where ordem-man.ep-codigo    = sub-div-ordem.ep-codigo 
            and   ordem-man.cod-est-exec = sub-div-ordem.cod-est-exec
            and   ordem-man.num-projeto  = sub-div-ordem.num-projeto
            and   ordem-man.num-ordem    = sub-div-ordem.num-ordem
            and   ordem-man.num-secao    = sub-div-ordem.num-secao
            and   ordem-man.cod-especialidade = sub-div-ordem.cod-especialida
            and   ordem-man.cod-sub-espec     = sub-div-ordem.cod-sub-espec
            and   ordem-man.cod-origem        = sub-div-ordem.cod-origem
            and   ordem-man.log-2             = yes
            no-lock break by ordem-man.num-ord-man:
                       
            assign de-vl-horas = de-vl-horas +
                                 ordem-man.vl-material[i-moeda] +
                                 ordem-man.vl-servico[i-moeda] +
                                 ordem-man.vl-mob[i-moeda]
                   de-tot-qt-horas = de-tot-qt-horas + ordem-man.horas-report.


            if  last-of(ordem-man.num-ord-man) then do:
                create t-cod.
                assign t-cod.dt-trans          = ordem-man.dt-cadastro
                       t-cod.ep-codigo         = sub-div-ordem.ep-codigo
                       t-cod.cod-est-exec      = sub-div-ordem.cod-est-exec
                       t-cod.num-projeto       = sub-div-ordem.num-projeto
                       t-cod.num-ordem         = sub-div-ordem.num-ordem
                       t-cod.num-secao         = sub-div-ordem.num-secao
                       t-cod.cod-especialidade = sub-div-ordem.cod-especialidade
                       t-cod.cod-sub-espec     = sub-div-ordem.cod-sub-espec
                       t-cod.cod-origem        = sub-div-ordem.cod-origem
                       t-cod.it-codigo = "MOM " +
                                         string(ordem-man.cod-estabel,"xxx") +
                                         " " +
                                         string(ordem-man.num-ord-man,
                                         ">>>,>>>,>>9")
                       t-cod.descricao          = ordem-man.descricao
                       t-cod.un                 = " "
                       t-cod.num-ord-magnus     = sub-div-ordem.num-ord-magnus
                       t-cod.de-qt-est-reest    = 0                        
                       t-cod.de-vl-est-reest    = 0                          
                       t-cod.de-tot-est-reest   = 0
                       t-cod.de-qt-comp         = ordem-man.horas-report
                       t-cod.de-fim-proj        = 0
                       t-cod.de-tot-comp        = de-vl-horas
                       t-cod.de-vl-comp         = de-vl-horas /
                                                  ordem-man.horas-report 
                       t-cod.tipo               = 4
                       de-vl-horas              = 0.
            end.
        end.
    end.
    if  l-ord-prod then do:
        assign de-valor = 0
               de-tot-qt-horas = 0.

        for each  ordem-man use-index emp-est-proj
            where ordem-man.ep-codigo    = sub-div-ordem.ep-codigo 
            and   ordem-man.cod-est-exec = sub-div-ordem.cod-est-exec
            and   ordem-man.num-projeto  = sub-div-ordem.num-projeto
            and   ordem-man.num-ordem    = sub-div-ordem.num-ordem
            and   ordem-man.num-secao    = sub-div-ordem.num-secao
            and   ordem-man.cod-especialidade = sub-div-ordem.cod-especialida
            and   ordem-man.cod-sub-espec     = sub-div-ordem.cod-sub-espec
            and   ordem-man.cod-origem        = sub-div-ordem.cod-origem
            and   ordem-man.log-2             = no
            no-lock break by ordem-man.num-ord-man:

            assign de-valor = de-valor +
                   ordem-man.vl-material[i-moeda] +
                   ordem-man.vl-servico[i-moeda] +
                   ordem-man.vl-mob[i-moeda]
                   de-tot-qt-horas = de-tot-qt-horas + ordem-man.dec-1.
                   
            if tt-param.i-moeda = 1 then
               assign de-valor = de-valor + dec(substring(ordem-man.char-1,1,18)).
               else if tt-param.i-moeda = 2 then
                    assign de-valor = de-valor + dec(substring(ordem-man.char-1,19,18)).
               else
                    assign de-valor = de-valor + dec(substring(ordem-man.char-1,37,18)).       

            if  last-of(ordem-man.num-ord-man) then do:
                create t-cod.
                assign t-cod.dt-trans          = ordem-man.dt-cadastro
                       t-cod.ep-codigo         = sub-div-ordem.ep-codigo
                       t-cod.cod-est-exec      = sub-div-ordem.cod-est-exec
                       t-cod.num-projeto       = sub-div-ordem.num-projeto
                       t-cod.num-ordem         = sub-div-ordem.num-ordem
                       t-cod.num-secao         = sub-div-ordem.num-secao
                       t-cod.cod-especialidade = sub-div-ordem.cod-especialidade
                       t-cod.cod-sub-espec     = sub-div-ordem.cod-sub-espec
                       t-cod.cod-origem        = sub-div-ordem.cod-origem
                       t-cod.it-codigo = "OP  " +
                                         string(ordem-man.cod-estabel,"xxx") +
                                         " " +
                                         string(ordem-man.num-ord-man,
                                         ">>>,>>>,>>9")
                       t-cod.descricao          = ordem-man.descricao
                       t-cod.un                 = " "
                       t-cod.num-ord-magnus     = sub-div-ordem.num-ord-magnus
                       t-cod.de-qt-est-reest    = 0                        
                       t-cod.de-vl-est-reest    = 0                          
                       t-cod.de-tot-est-reest   = 0
                       t-cod.de-qt-comp         = de-tot-qt-horas
                       t-cod.de-fim-proj        = 0
                       t-cod.de-tot-comp        = de-valor
                       t-cod.de-vl-comp         = de-valor / de-tot-qt-horas 
                       t-cod.tipo               = 5
                       de-valor                 = 0.
            end.
        end.
    end.
    run pi-acompanhar in h-acomp (input string(sub-div-ordem.num-ordem )).
 end.
 run pi-finalizar in h-acomp.

/*******************************************************************************
**                                                                         
**  ESIN0801.I1 - Gera totais para impressao 
**
*******************************************************************************/

    assign i-ch-tipo = 0
           i-ch-ori  = 0
           i-ch-sub  = 0
           i-ch-esp  = 0
           i-ch-sec  = 0
           i-ch-ord  = 0.

    for each t-cod no-lock use-index codigo
       WHERE t-cod.dt-trans >= tt-param.d-data-ini
         AND t-cod.dt-trans <= tt-param.d-data-fim:
        
        if  (t-cod.de-vl-tend / de-mil) < de-vl-tenden then do:
            delete t-cod.
            next.
        end.     

        if  l-vl-menor = no 
        and t-cod.de-vl-tend < t-cod.de-tot-est-reest then do:
            delete t-cod.
            next.
        end.

        if  l-vl-maior = no 
        and t-cod.de-vl-tend > t-cod.de-tot-est-reest then do:
            delete t-cod.
            next.
        end.
        if  t-cod.tipo = 1 
        and t-cod.ind-desv-tec then do:
        
            /* calculo do desvio tecnico */
            assign t-cod.de-vl-des-tec = t-cod.de-vl-est-reest * 
                                     (t-cod.de-qt-comp - t-cod.de-qt-est-reest).

            /* calculo do desvio economico */
            assign t-cod.de-vl-des-eco = t-cod.de-qt-comp *
                                    (t-cod.de-vl-comp - t-cod.de-vl-est-reest).

        end.
 
        if  t-cod.de-vl-comp = ? then
            assign t-cod.de-vl-comp = 0.
        if  t-cod.de-vl-des-eco = ? then
            assign t-cod.de-vl-des-eco = 0.
      
        /* calculo dos saldos */
        
        if  t-cod.tipo = 1 then do:
            /* estimativa */
            assign t-cod.de-vl-estim = t-cod.de-tot-est-reest -
                                       t-cod.de-tot-comp.

            /* tendencia */
            if  t-cod.de-tot-est-reest > t-cod.de-tot-comp then 
                assign t-cod.de-vl-tend = t-cod.de-tot-est-reest + 
                                          t-cod.de-fim-proj.
            else
                assign t-cod.de-vl-tend = t-cod.de-tot-comp + 
                                          t-cod.de-fim-proj.
        end.
        else do:
            /* estimativa */
            assign t-cod.de-vl-estim = 0.

            /* tendencia */
            assign t-cod.de-vl-tend = t-cod.de-tot-comp. 
        end.
        
        
        /*------------------ TOTALIZA POR TIPO ------------------*/
        find first t-tot-tipo use-index codigo where
                   t-tot-tipo.ep-codigo         = t-cod.ep-codigo         and
                   t-tot-tipo.cod-est-exec      = t-cod.cod-est-exec      and
                   t-tot-tipo.num-projeto       = t-cod.num-projeto       and
                   t-tot-tipo.num-ordem         = t-cod.num-ordem         and
                   t-tot-tipo.num-secao         = t-cod.num-secao         and
                   t-tot-tipo.cod-especialidade = t-cod.cod-especialidade and
                   t-tot-tipo.cod-sub-espec     = t-cod.cod-sub-espec     and
                   t-tot-tipo.cod-origem        = t-cod.cod-origem        and
                   t-tot-tipo.tipo              = t-cod.tipo no-error.
        if  not avail t-tot-tipo then do:
            create t-tot-tipo.
            assign i-ch-tipo                    = i-ch-tipo + 1
                   t-tot-tipo.ch-tipo           = i-ch-tipo
                   t-tot-tipo.ep-codigo         = t-cod.ep-codigo         
                   t-tot-tipo.cod-est-exec      = t-cod.cod-est-exec      
                   t-tot-tipo.num-projeto       = t-cod.num-projeto       
                   t-tot-tipo.num-ordem         = t-cod.num-ordem         
                   t-tot-tipo.num-secao         = t-cod.num-secao         
                   t-tot-tipo.cod-especialidade = t-cod.cod-especialidade 
                   t-tot-tipo.cod-sub-espec     = t-cod.cod-sub-espec     
                   t-tot-tipo.cod-origem        = t-cod.cod-origem
                   t-tot-tipo.tipo              = t-cod.tipo.
        end.            
        assign t-tot-tipo.de-tot-est-reest = t-tot-tipo.de-tot-est-reest +
                                            (t-cod.de-tot-est-reest / de-mil)
               t-tot-tipo.de-tot-compromis = t-tot-tipo.de-tot-compromis +
                                            (t-cod.de-tot-comp / de-mil)
               t-tot-tipo.de-tot-desv-tecn = t-tot-tipo.de-tot-desv-tecn +
                                            (t-cod.de-vl-des-tec / de-mil)
               t-tot-tipo.de-tot-desv-econ = t-tot-tipo.de-tot-desv-econ +
                                            (t-cod.de-vl-des-eco / de-mil)
               t-tot-tipo.de-tot-sal-estim = t-tot-tipo.de-tot-sal-estim +
                                            (t-cod.de-vl-estim / de-mil)
               t-tot-tipo.de-tot-sal-final = t-tot-tipo.de-tot-sal-final +
                                            (t-cod.de-fim-proj / de-mil)
               t-tot-tipo.de-tot-tendencia = t-tot-tipo.de-tot-tendencia +
                                            (t-cod.de-vl-tend / de-mil)
               t-cod.ch-tipo               = i-ch-tipo.
        /*------------------- FIM TOTAL POR TIPO ---------------------*/

        /*------------------ TOTALIZA POR ORIGEM ------------------*/
        find first t-tot-ori use-index codigo where
                   t-tot-ori.ep-codigo         = t-cod.ep-codigo         and
                   t-tot-ori.cod-est-exec      = t-cod.cod-est-exec      and
                   t-tot-ori.num-projeto       = t-cod.num-projeto       and
                   t-tot-ori.num-ordem         = t-cod.num-ordem         and
                   t-tot-ori.num-secao         = t-cod.num-secao         and
                   t-tot-ori.cod-especialidade = t-cod.cod-especialidade and
                   t-tot-ori.cod-sub-espec     = t-cod.cod-sub-espec     and
                   t-tot-ori.cod-origem        = t-cod.cod-origem no-error.
        if  not avail t-tot-ori then do:
            create t-tot-ori.
            assign i-ch-ori                    = i-ch-ori + 1
                   t-tot-ori.ch-ori            = i-ch-ori
                   t-tot-ori.ep-codigo         = t-cod.ep-codigo         
                   t-tot-ori.cod-est-exec      = t-cod.cod-est-exec      
                   t-tot-ori.num-projeto       = t-cod.num-projeto       
                   t-tot-ori.num-ordem         = t-cod.num-ordem         
                   t-tot-ori.num-secao         = t-cod.num-secao         
                   t-tot-ori.cod-especialidade = t-cod.cod-especialidade 
                   t-tot-ori.cod-sub-espec     = t-cod.cod-sub-espec     
                   t-tot-ori.cod-origem        = t-cod.cod-origem.
        end.            
        assign t-tot-ori.de-tot-est-reest = t-tot-ori.de-tot-est-reest +
                                            (t-cod.de-tot-est-reest / de-mil)
               t-tot-ori.de-tot-compromis = t-tot-ori.de-tot-compromis +
                                            (t-cod.de-tot-comp / de-mil)
               t-tot-ori.de-tot-desv-tecn = t-tot-ori.de-tot-desv-tecn +
                                            (t-cod.de-vl-des-tec / de-mil)
               t-tot-ori.de-tot-desv-econ = t-tot-ori.de-tot-desv-econ +
                                            (t-cod.de-vl-des-eco / de-mil)
               t-tot-ori.de-tot-sal-estim = t-tot-ori.de-tot-sal-estim +
                                            (t-cod.de-vl-estim / de-mil)
               t-tot-ori.de-tot-sal-final = t-tot-ori.de-tot-sal-final +
                                            (t-cod.de-fim-proj / de-mil)
               t-tot-ori.de-tot-tendencia = t-tot-ori.de-tot-tendencia +
                                            (t-cod.de-vl-tend / de-mil)
               t-cod.ch-ori               = i-ch-ori.
        /*------------------- FIM TOTAL POR ORIGEM ---------------------*/

        /*------------------- TOTALIZA POR SUB-ESPECIALIDADE ---------------*/
        find first t-tot-sub use-index codigo where 
                   t-tot-sub.ep-codigo         = t-cod.ep-codigo         and
                   t-tot-sub.cod-est-exec      = t-cod.cod-est-exec      and
                   t-tot-sub.num-projeto       = t-cod.num-projeto       and
                   t-tot-sub.num-ordem         = t-cod.num-ordem         and
                   t-tot-sub.num-secao         = t-cod.num-secao         and
                   t-tot-sub.cod-especialidade = t-cod.cod-especialidade and
                   t-tot-sub.cod-sub-espec     = t-cod.cod-sub-espec no-error.
        if  not avail t-tot-sub then do:
            create t-tot-sub.
            assign i-ch-sub                    = i-ch-sub + 1
                   t-tot-sub.ch-sub            = i-ch-sub
                   t-tot-sub.ep-codigo         = t-cod.ep-codigo         
                   t-tot-sub.cod-est-exec      = t-cod.cod-est-exec      
                   t-tot-sub.num-projeto       = t-cod.num-projeto       
                   t-tot-sub.num-ordem         = t-cod.num-ordem         
                   t-tot-sub.num-secao         = t-cod.num-secao         
                   t-tot-sub.cod-especialidade = t-cod.cod-especialidade 
                   t-tot-sub.cod-sub-espec     = t-cod.cod-sub-espec.
        end.            
        assign t-tot-sub.de-tot-est-reest = t-tot-sub.de-tot-est-reest +
                                            (t-cod.de-tot-est-reest / de-mil)
               t-tot-sub.de-tot-compromis = t-tot-sub.de-tot-compromis +
                                            (t-cod.de-tot-comp / de-mil)
               t-tot-sub.de-tot-desv-tecn = t-tot-sub.de-tot-desv-tecn +
                                            (t-cod.de-vl-des-tec / de-mil)
               t-tot-sub.de-tot-desv-econ = t-tot-sub.de-tot-desv-econ +
                                            (t-cod.de-vl-des-eco / de-mil)
               t-tot-sub.de-tot-sal-estim = t-tot-sub.de-tot-sal-estim +
                                            (t-cod.de-vl-estim / de-mil)
               t-tot-sub.de-tot-sal-final = t-tot-sub.de-tot-sal-final +
                                            (t-cod.de-fim-proj / de-mil)
               t-tot-sub.de-tot-tendencia = t-tot-sub.de-tot-tendencia +
                                            (t-cod.de-vl-tend / de-mil)
               t-cod.ch-sub               = i-ch-sub.
        /*------------------ FIM TOTAL POR SUB-ESPECIALIDADE ---------------*/

        /*-------------------- TOTALIZA POR ESPECIALIDADE ----------------*/
        find first t-tot-esp use-index codigo where 
                   t-tot-esp.ep-codigo         = t-cod.ep-codigo         and
                   t-tot-esp.cod-est-exec      = t-cod.cod-est-exec      and
                   t-tot-esp.num-projeto       = t-cod.num-projeto       and
                   t-tot-esp.num-ordem         = t-cod.num-ordem         and
                   t-tot-esp.num-secao         = t-cod.num-secao         and
                   t-tot-esp.cod-especialidade = t-cod.cod-especialidade 
                   no-error.
        if  not avail t-tot-esp then do:
            create t-tot-esp.
            assign i-ch-esp                    = i-ch-esp + 1
                   t-tot-esp.ch-esp            = i-ch-esp
                   t-tot-esp.ep-codigo         = t-cod.ep-codigo         
                   t-tot-esp.cod-est-exec      = t-cod.cod-est-exec      
                   t-tot-esp.num-projeto       = t-cod.num-projeto       
                   t-tot-esp.num-ordem         = t-cod.num-ordem         
                   t-tot-esp.num-secao         = t-cod.num-secao         
                   t-tot-esp.cod-especialidade = t-cod.cod-especialidade. 
        end.            
        assign t-tot-esp.de-tot-est-reest = t-tot-esp.de-tot-est-reest +
                                            (t-cod.de-tot-est-reest / de-mil)
               t-tot-esp.de-tot-compromis = t-tot-esp.de-tot-compromis +
                                            (t-cod.de-tot-comp / de-mil)
               t-tot-esp.de-tot-desv-tecn = t-tot-esp.de-tot-desv-tecn +
                                            (t-cod.de-vl-des-tec / de-mil)
               t-tot-esp.de-tot-desv-econ = t-tot-esp.de-tot-desv-econ +
                                            (t-cod.de-vl-des-eco / de-mil)
               t-tot-esp.de-tot-sal-estim = t-tot-esp.de-tot-sal-estim +
                                            (t-cod.de-vl-estim / de-mil)
               t-tot-esp.de-tot-sal-final = t-tot-esp.de-tot-sal-final +
                                            (t-cod.de-fim-proj / de-mil)
               t-tot-esp.de-tot-tendencia = t-tot-esp.de-tot-tendencia +
                                            (t-cod.de-vl-tend / de-mil)
               t-cod.ch-esp               = i-ch-esp.
        /*-------------------- FIM TOTAL POR ESPECIALIDADE ----------------*/
        
        /*------------------------ TOTALIZA POR SECAO ---------------------*/
        find first t-tot-sec use-index codigo where 
                   t-tot-sec.ep-codigo         = t-cod.ep-codigo         and
                   t-tot-sec.cod-est-exec      = t-cod.cod-est-exec      and
                   t-tot-sec.num-projeto       = t-cod.num-projeto       and
                   t-tot-sec.num-ordem         = t-cod.num-ordem         and
                   t-tot-sec.num-secao         = t-cod.num-secao no-error.
        if  not avail t-tot-sec then do:
            create t-tot-sec.
            assign i-ch-sec                    = i-ch-sec + 1
                   t-tot-sec.ch-sec            = i-ch-sec
                   t-tot-sec.ep-codigo         = t-cod.ep-codigo         
                   t-tot-sec.cod-est-exec      = t-cod.cod-est-exec      
                   t-tot-sec.num-projeto       = t-cod.num-projeto       
                   t-tot-sec.num-ordem         = t-cod.num-ordem         
                   t-tot-sec.num-secao         = t-cod.num-secao.        
        end.            
        assign t-tot-sec.de-tot-est-reest = t-tot-sec.de-tot-est-reest +
                                            (t-cod.de-tot-est-reest / de-mil)
               t-tot-sec.de-tot-compromis = t-tot-sec.de-tot-compromis +
                                            (t-cod.de-tot-comp / de-mil)
               t-tot-sec.de-tot-desv-tecn = t-tot-sec.de-tot-desv-tecn +
                                            (t-cod.de-vl-des-tec / de-mil)
               t-tot-sec.de-tot-desv-econ = t-tot-sec.de-tot-desv-econ +
                                            (t-cod.de-vl-des-eco / de-mil)
               t-tot-sec.de-tot-sal-estim = t-tot-sec.de-tot-sal-estim +
                                            (t-cod.de-vl-estim / de-mil)
               t-tot-sec.de-tot-sal-final = t-tot-sec.de-tot-sal-final +
                                            (t-cod.de-fim-proj / de-mil)
               t-tot-sec.de-tot-tendencia = t-tot-sec.de-tot-tendencia +
                                            (t-cod.de-vl-tend / de-mil)
               t-cod.ch-sec               = i-ch-sec.
        /*------------------------ FIM TOTAL POR SECAO --------------------*/

        /*---------------------- TOTALIZA POR ORDEM ---------------------*/
        find first t-tot-ord use-index codigo where 
                   t-tot-ord.ep-codigo         = t-cod.ep-codigo         and
                   t-tot-ord.cod-est-exec      = t-cod.cod-est-exec      and
                   t-tot-ord.num-projeto       = t-cod.num-projeto       and
                   t-tot-ord.num-ordem         = t-cod.num-ordem no-error.
        if  not avail t-tot-ord then do:
            create t-tot-ord.
            assign i-ch-ord                    = i-ch-ord + 1
                   t-tot-ord.ch-ord            = i-ch-ord
                   t-tot-ord.ep-codigo         = t-cod.ep-codigo         
                   t-tot-ord.cod-est-exec      = t-cod.cod-est-exec      
                   t-tot-ord.num-projeto       = t-cod.num-projeto       
                   t-tot-ord.num-ordem         = t-cod.num-ordem.        
        end.            
        assign t-tot-ord.de-tot-est-reest = t-tot-ord.de-tot-est-reest +
                                            (t-cod.de-tot-est-reest / de-mil)
               t-tot-ord.de-tot-compromis = t-tot-ord.de-tot-compromis +
                                            (t-cod.de-tot-comp / de-mil)
               t-tot-ord.de-tot-desv-tecn = t-tot-ord.de-tot-desv-tecn +
                                            (t-cod.de-vl-des-tec / de-mil)
               t-tot-ord.de-tot-desv-econ = t-tot-ord.de-tot-desv-econ +
                                            (t-cod.de-vl-des-eco / de-mil)
               t-tot-ord.de-tot-sal-estim = t-tot-ord.de-tot-sal-estim +
                                            (t-cod.de-vl-estim / de-mil)
               t-tot-ord.de-tot-sal-final = t-tot-ord.de-tot-sal-final +
                                            (t-cod.de-fim-proj / de-mil)
               t-tot-ord.de-tot-tendencia = t-tot-ord.de-tot-tendencia +
                                            (t-cod.de-vl-tend / de-mil)
               t-cod.ch-ord               = i-ch-ord.
        /*------------------------ FIM TOTAL POR ORDEM --------------------*/
 
        /*---------------------- TOTALIZA POR PROJETO -------------------*/
        find first t-tot-pro use-index codigo where 
                   t-tot-pro.ep-codigo         = t-cod.ep-codigo         and
                   t-tot-pro.cod-est-exec      = t-cod.cod-est-exec      and
                   t-tot-pro.num-projeto       = t-cod.num-projeto no-error.
        if  not avail t-tot-pro then do:
            create t-tot-pro.
            assign i-ch-pro                    = i-ch-pro + 1
                   t-tot-pro.ch-pro            = i-ch-pro
                   t-tot-pro.ep-codigo         = t-cod.ep-codigo         
                   t-tot-pro.cod-est-exec      = t-cod.cod-est-exec      
                   t-tot-pro.num-projeto       = t-cod.num-projeto.      
        end.            
        assign t-tot-pro.de-tot-est-reest = t-tot-pro.de-tot-est-reest +
                                            (t-cod.de-tot-est-reest / de-mil)
               t-tot-pro.de-tot-compromis = t-tot-pro.de-tot-compromis +
                                            (t-cod.de-tot-comp / de-mil)
               t-tot-pro.de-tot-desv-tecn = t-tot-pro.de-tot-desv-tecn +
                                            (t-cod.de-vl-des-tec / de-mil)
               t-tot-pro.de-tot-desv-econ = t-tot-pro.de-tot-desv-econ +
                                            (t-cod.de-vl-des-eco / de-mil)
               t-tot-pro.de-tot-sal-estim = t-tot-pro.de-tot-sal-estim +
                                            (t-cod.de-vl-estim / de-mil)
               t-tot-pro.de-tot-sal-final = t-tot-pro.de-tot-sal-final +
                                            (t-cod.de-fim-proj / de-mil)
               t-tot-pro.de-tot-tendencia = t-tot-pro.de-tot-tendencia +
                                            (t-cod.de-vl-tend / de-mil)
               t-cod.ch-pro               = i-ch-pro.
        /*------------------------ FIM TOTAL POR PROJETO ------------------*/
    end.

/* fim do include */
  /* gera totais */

/******************************************************************************
**
**   ESIN0801.I3 - Imprime COD
**
******************************************************************************/

for each t-cod no-lock use-index codigo
   WHERE t-cod.dt-trans >= tt-param.d-data-ini
     AND t-cod.dt-trans <= tt-param.d-data-fim
    break by t-cod.ep-codigo
          by t-cod.cod-est-exec
          by t-cod.num-projeto
          by t-cod.num-ordem
          by t-cod.num-secao
          by t-cod.cod-especialidade
          by t-cod.cod-sub-espec
          by t-cod.cod-origem
          by t-cod.tipo
          by t-cod.it-codigo:

    find first proj-inv use-index emp-est-proj
         where proj-inv.ep-codigo    = t-cod.ep-codigo
         and   proj-inv.cod-est-exec = t-cod.cod-est-exec
         and   proj-inv.num-projeto  = t-cod.num-projeto
         no-lock no-error.

    find first ordem-inv use-index emp-est-proj
         where ordem-inv.ep-codigo    = t-cod.ep-codigo
         and   ordem-inv.cod-est-exec = t-cod.cod-est-exec
         and   ordem-inv.num-projeto  = t-cod.num-projeto
         and   ordem-inv.num-ordem    = t-cod.num-ordem
         no-lock no-error.

    find first empresa 
         where empresa.ep-codigo = t-cod.ep-codigo
         no-lock no-error.

    find first estabelec
         where estabelec.cod-estabel = t-cod.cod-est-exec
         no-lock no-error.

    find first secao-inv use-index emp-esp-proj
         where secao-inv.ep-codigo    = t-cod.ep-codigo
         and   secao-inv.cod-est-exec = t-cod.cod-est-exec
         and   secao-inv.num-projeto  = t-cod.num-projeto
         and   secao-inv.num-ordem    = t-cod.num-ordem
         and   secao-inv.num-secao    = t-cod.num-secao
         no-lock no-error.

    find first especialidade use-index emp-espec 
         where especialidade.ep-codigo         = t-cod.ep-codigo
         and   especialidade.cod-especialidade = t-cod.cod-especialidade  
         no-lock no-error.

    find first sub-espec use-index emp-esp-sub
         where sub-espec.ep-codigo         = t-cod.ep-codigo
         and   sub-espec.cod-especialidade = t-cod.cod-especialidade
         and   sub-espec.cod-sub-espec     = t-cod.cod-sub-espec
         no-lock no-error.

    find first orig-despesa use-index emp-orig
         where orig-despesa.ep-codigo  = t-cod.ep-codigo
         and   orig-despesa.cod-origem = t-cod.cod-origem
         no-lock no-error.

    assign i-emp-cab   = t-cod.ep-codigo    
           c-emp-cab   = empresa.nome      
           i-est-cab   = t-cod.cod-est-exec  
           c-est-cab   = estabelec.nome     
           i-sig-cab   = proj-inv.sigla    
           i-proj-cab  = t-cod.num-projeto  
           c-proj-cab1 = proj-inv.descricao 
           c-proj-cab2 = proj-inv.descricao-2
           i-ord-cab   = t-cod.num-ordem   
           c-ord-cab1  = ordem-inv.descricao 
           c-ord-cab2  = ordem-inv.descricao-2
           i-sec-cab   = t-cod.num-secao 
           c-sec-cab1  = secao-inv.descricao 
           c-sec-cab2  = secao-inv.descricao-2. 

    view frame f-quebra.
    
    if  first-of(t-cod.cod-especialidade)
    or  first-of(t-cod.cod-sub-espec)
    or  first-of(t-cod.cod-origem) then do:
         put skip(1).
    end.

    if  first-of(t-cod.cod-especialidade) then do:
        put c-especialidade at 1 ":"
            t-cod.cod-especialidade    
            space(1)
            especialidade.descricao   format "x(20)"   
            space(1)
            especialidade.descricao-2 format "x(20)".
    end.

    if  first-of(t-cod.cod-sub-espec) then do:
        put c-sub at 63 ":"
            t-cod.cod-sub-espec      
            space(1)
            sub-espec.descricao   format "x(20)"    
            space(1)
            sub-espec.descricao-2 format "x(20)".
    end.

    if  first-of(t-cod.cod-origem) then do:
        put c-origem at 129 ":"
            t-cod.cod-origem             
            space(1)
            orig-despesa.descricao   format "x(20)"      
            space(1)
            orig-despesa.descricao-2 format "x(20)".     
        if  t-cod.flag = "R" then do:
            put c-revisado at 191 ":"
            (t-cod.de-vl-verba / de-mil) format "->>>,>>>,>>9.99".
        end.    
       if  t-cod.flag = "E" then do:
           put c-verba at 194    ":"
            (t-cod.de-vl-verba / de-mil) format "->>>,>>>,>>9.99".
       end.
     
    end.
    
    if  first-of(t-cod.cod-especialidade)
    or  first-of(t-cod.cod-sub-espec)
    or  first-of(t-cod.cod-origem) then do:
         put skip(1).
    end.
    

    disp t-cod.it-codigo              
         t-cod.descricao              
         t-cod.un  
         t-cod.de-qt-est-reest 
         t-cod.de-vl-est-reest       
         (t-cod.de-tot-est-reest / de-mil) @ t-cod.de-tot-est-reest
         t-cod.de-qt-comp            
         t-cod.de-vl-comp           
         (t-cod.de-tot-comp / de-mil)      @ t-cod.de-tot-comp
         (t-cod.de-vl-des-tec / de-mil)    @ t-cod.de-vl-des-tec
         (t-cod.de-vl-des-eco / de-mil)    @ t-cod.de-vl-des-eco
         (t-cod.de-vl-estim / de-mil)      @ t-cod.de-vl-estim
         (t-cod.de-fim-proj / de-mil)      @ t-cod.de-fim-proj
         (t-cod.de-vl-tend / de-mil)       @ t-cod.de-vl-tend
        with stream-io frame f-dados.
    down with stream-io frame f-dados.

    if  l-item      = yes 
    and l-descricao = yes
    and t-cod.tipo  = 1 then do:
        find first narrativa 
            where narrativa.it-codigo = t-cod.it-codigo
            no-lock no-error.
        if  avail narrativa and narrativa.descricao <> " " then do :
            put unformatted.
            run pi-print-editor (input left-trim(narrativa.descricao), input 80).
            for each tt-editor with stream-io frame f-narrativa-ordem:
                disp tt-editor.conteudo with stream-io frame f-narrativa-ordem.
                down with stream-io frame f-narrativa-ordem.
            end.    
        end.
    end.

    assign l-imp = yes.

    if  last-of(t-cod.tipo) 
    and i-tot > 1 then do:
        find first t-tot-tipo use-index chave  where 
                   t-tot-tipo.ch-tipo = t-cod.ch-tipo no-lock no-error.
        if  avail t-tot-tipo then do: 
            if  t-cod.tipo = 1 then
                put c-tot-item               at 68
                    ":".
            if  t-cod.tipo = 2 then
                put c-tot-cont               at 59
                ":".
            if  t-cod.tipo = 3 then
                put c-tot-pagto              at 60
                ":".
            if  t-cod.tipo = 4 then
                put c-tot-obra               at 59
                ":".

            put t-tot-tipo.de-tot-est-reest at 88
                t-tot-tipo.de-tot-compromis at 139
                t-tot-tipo.de-tot-desv-tecn at 155
                t-tot-tipo.de-tot-desv-econ at 171
                t-tot-tipo.de-tot-sal-estim at 187
                t-tot-tipo.de-tot-sal-final at 202
                t-tot-tipo.de-tot-tendencia at 217 skip(1).
        end.
    end.

    if  last-of(t-cod.cod-origem) then do:
        find first t-tot-ori use-index chave where 
                   t-tot-ori.ch-ori = t-cod.ch-ori no-lock no-error.
        if  avail t-tot-ori then 
            put c-tot-ori                  at 69
                ":"
                t-tot-ori.de-tot-est-reest at 88
                t-tot-ori.de-tot-compromis at 139
                t-tot-ori.de-tot-desv-tecn at 155
                t-tot-ori.de-tot-desv-econ at 171
                t-tot-ori.de-tot-sal-estim at 187
                t-tot-ori.de-tot-sal-final at 202
                t-tot-ori.de-tot-tendencia at 217.
    end.         

    if  last-of(t-cod.cod-sub-espec) then do:
        find first t-tot-sub use-index chave where
                   t-tot-sub.ch-sub = t-cod.ch-sub no-lock no-error.
        if  avail t-tot-sub then 
            put c-tot-sub                       at 56 
                ":"
                t-tot-sub.de-tot-est-reest      at 88
                t-tot-sub.de-tot-compromis      at 139
                t-tot-sub.de-tot-desv-tecn      at 155
                t-tot-sub.de-tot-desv-econ      at 171
                t-tot-sub.de-tot-sal-estim      at 187
                t-tot-sub.de-tot-sal-final      at 202
                t-tot-sub.de-tot-tendencia      at 217.
    end.
    
    if  last-of(t-cod.cod-especialidade) then do:
        find first t-tot-esp use-index chave where 
                   t-tot-esp.ch-esp = t-cod.ch-esp no-lock no-error.
        if  avail t-tot-esp then 
            put c-tot-espec                     at 61
                ":"
                t-tot-esp.de-tot-est-reest      at 88
                t-tot-esp.de-tot-compromis      at 139
                t-tot-esp.de-tot-desv-tecn      at 155
                t-tot-esp.de-tot-desv-econ      at 171
                t-tot-esp.de-tot-sal-estim      at 187
                t-tot-esp.de-tot-sal-final      at 202
                t-tot-esp.de-tot-tendencia      at 217.
    end.

    if  last-of(t-cod.num-secao) then do:
        assign l-imp = no.
        find first t-tot-sec use-index chave where 
                   t-tot-sec.ch-sec = t-cod.ch-sec no-lock no-error.
        if  avail t-tot-sec then 
            put c-tot-sec                       at 70 
                ":"
                t-tot-sec.de-tot-est-reest      at 88
                t-tot-sec.de-tot-compromis      at 139
                t-tot-sec.de-tot-desv-tecn      at 155
                t-tot-sec.de-tot-desv-econ      at 171
                t-tot-sec.de-tot-sal-estim      at 187
                t-tot-sec.de-tot-sal-final      at 202
                t-tot-sec.de-tot-tendencia      at 217.
        if  not last-of(t-cod.num-ordem) then
            page.
    end.
       
    if  last-of(t-cod.num-ordem) then do:
        assign l-imp = no.
        find first t-tot-ord use-index chave where 
                   t-tot-ord.ch-ord = t-cod.ch-ord no-lock no-error.
        if  avail t-tot-ord then 
            put c-tot-ord                       at 70
                ":"
                t-tot-ord.de-tot-est-reest      at 88
                t-tot-ord.de-tot-compromis      at 139
                t-tot-ord.de-tot-desv-tecn      at 155
                t-tot-ord.de-tot-desv-econ      at 171
                t-tot-ord.de-tot-sal-estim      at 187
                t-tot-ord.de-tot-sal-final      at 202
                t-tot-ord.de-tot-tendencia      at 217.
        if  not last(t-cod.num-projeto) then 
            page.
    end.

    if  last(t-cod.num-projeto) then do:
        find first t-tot-pro use-index chave where 
                   t-tot-pro.ch-pro = t-cod.ch-pro no-lock no-error.
        if  avail t-tot-pro then 
            put c-tot-proj                      at 68
                ":"
                t-tot-pro.de-tot-est-reest      at 88
                t-tot-pro.de-tot-compromis      at 139
                t-tot-pro.de-tot-desv-tecn      at 155
                t-tot-pro.de-tot-desv-econ      at 171
                t-tot-pro.de-tot-sal-estim      at 187
                t-tot-pro.de-tot-sal-final      at 202
                t-tot-pro.de-tot-tendencia      at 217.
    end.
end.

/* fim do include */
  /* imprime COD */

/* fim do include */ 
  /* impressao */

        hide frame f-quebra.
        page.        

       
           IF  tt-param.l-divmil = YES THEN DO:
               /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Sim",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
               assign c-desc-divmil = return-value.
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
 
               assign c-desc-divmil = return-value.
           END.
       
    
        disp i-empresa
             c-est-exec
             i-projeto
             i-sigla
             d-data-ini
             d-data-fim
             i-ord-ini
             i-ord-fim
             i-sec-ini
             i-sec-fim
             i-esp-ini
             i-esp-fim
             i-sub-ini
             i-sub-fim
             i-ori-ini
             i-ori-fim
             c-item-ini
             c-item-fim
             l-item
             l-sem-planej
             l-duplic
             l-descricao
             l-aprop
             l-autor
             l-mob
             l-ord-prod
             l-vl-menor
             l-vl-maior
             de-vl-tenden 
             moeda-desc
             c-destino 
             tt-param.arquivo no-label
             tt-param.usuario
            
                c-desc-divmil
            
             with stream-io frame f-selec-param.
        if  l-item = no then do:
            assign l-descricao = no.
            disp "" @ l-descricao
                 with stream-io frame f-selec-param.
        end.

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
 



/**************************************************************
**
**  PI-EDIT.I - Def. Proc. Interna para Impressao de Editores
**
**************************************************************/

procedure pi-print-editor:
    def input param c-editor    as char    no-undo.
    def input param i-len       as integer no-undo.

    def var i-linha  as integer no-undo.
    def var i-aux    as integer no-undo.
    def var c-aux    as char    no-undo.
    def var c-ret    as char    no-undo.

    /*Alteraá∆o 06/11/2007 - tech38629 - FO1630139 - Alteraá∆o para evitar que 
    o laáo entre em loop infinito quando o valor recebido como parÉmetro for ? */
    IF c-editor = ? or i-len = ? THEN
        RETURN.
    /*  FO1630139 - Fim da Alteraá∆o */

    for each tt-editor:
        delete tt-editor.
    end.

    assign c-ret = chr(255) + chr(255).

    do  while c-editor <> "":
        if  c-editor <> "" then do:
            assign i-aux = index(c-editor, chr(10)).
            /*Alteraá∆o 03/08/2007 - tech14187 - FO1562655 - Alteraá∆o para considerar o CHR(13) como quebra de linha tambÇm. */
            IF i-aux = 0 THEN DO:
                assign i-aux = index(c-editor, chr(13)).
            END.
            if  i-aux > i-len or (i-aux = 0 and length(c-editor) > i-len) then
                assign i-aux = r-index(c-editor, " ", i-len + 1).
            if  i-aux = 0 then
                assign c-aux = substr(c-editor, 1, i-len)
                       c-editor = substr(c-editor, i-len + 1).
            else
                assign c-aux = substr(c-editor, 1, i-aux - 1)
                       c-editor = substr(c-editor, i-aux + 1).
            if  i-len = 0 then
                assign entry(1, c-ret, chr(255)) = c-aux.
            else do:
                assign i-linha = i-linha + 1.
                create tt-editor.
                assign tt-editor.linha    = i-linha
                       tt-editor.conteudo = c-aux.
            end.
        end.
        if  i-len = 0 then
            return c-ret.
    end.
    return c-ret.
end procedure.

/* pi-edit.i */
 
