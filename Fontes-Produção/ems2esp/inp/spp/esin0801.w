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
def var c-prg-vrs as char init "[[[2.00.00.010[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.010"
       c-prg-obj = "ESIN0801".

/* Alteracao - 02/10/2006 - Nakamura - Incluida chamada a include que verifica a integridade dos programas do registro de produto */
/*{include/i-ctrlrp.i {1}}*/

/*Alteraá∆o - 08/09/2006 - tech1007 - Alteraá∆o para exibir o nome do programa que executou o programa que ser† exibido no extrato de vers∆o
                                      Solicitaá∆o realizada na FO 1239827*/

/*Fim alteraá∆o 08/09/2006*/


if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "ESIN0801"
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
        PUT "ESIN0801" AT 1 "2.00.00.010" AT 69 STRING(TODAY,'99/99/99') AT 84 STRING(TIME,'HH:MM:SS':U) AT 94 SKIP.
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

   /*** 010010 ***/
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
 

/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

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

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
    field raw-digita as raw.

/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */


/* Name of first Frame and/or Browse and/or first Query                 */

/* Standard List Definitions                                            */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */




/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE de-vl-tenden AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Tendància Acima de (em Mil)" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(6)":U INITIAL "Moeda" 
      VIEW-AS TEXT 
     SIZE 7.43 BY .67 NO-UNDO.

DEFINE VARIABLE i-moeda AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Principal", 1,
"Alternativa 1", 2,
"Alternativa 2", 3
     SIZE 41.43 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 43.43 BY 1.5.

DEFINE VARIABLE l-aprop AS LOGICAL INITIAL yes 
     LABEL "Apropriaá∆o Cont†bil" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.57 BY .71 NO-UNDO.

DEFINE VARIABLE l-autor AS LOGICAL INITIAL yes 
     LABEL "Autorizaá∆o de Pagamento" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.72 BY .63 NO-UNDO.

DEFINE VARIABLE l-descricao AS LOGICAL INITIAL no 
     LABEL "Descriá∆o Completa" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.57 BY .79 NO-UNDO.

DEFINE VARIABLE l-DivMil AS LOGICAL INITIAL yes 
     LABEL "Divide por 1000 (Mil)" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.14 BY .83 NO-UNDO.

DEFINE VARIABLE l-duplic AS LOGICAL INITIAL yes 
     LABEL "Duplicatas" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.86 BY .63 NO-UNDO.

DEFINE VARIABLE l-item AS LOGICAL INITIAL yes 
     LABEL "Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.72 BY .79 NO-UNDO.

DEFINE VARIABLE l-mob AS LOGICAL INITIAL yes 
     LABEL "M∆o-de-Obra Manutená∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.72 BY .75 NO-UNDO.

DEFINE VARIABLE l-ord-prod AS LOGICAL INITIAL yes 
     LABEL "Ordens de Produá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.72 BY .75 NO-UNDO.

DEFINE VARIABLE l-sem-planej AS LOGICAL INITIAL yes 
     LABEL "Documentos Sem Planejamento(Solicitaá∆o = 0)" 
     VIEW-AS TOGGLE-BOX
     SIZE 48.86 BY .83 NO-UNDO.

DEFINE VARIABLE l-vl-maior AS LOGICAL INITIAL yes 
     LABEL "Valor Tendància Maior que Estimado/Reestimado" 
     VIEW-AS TOGGLE-BOX
     SIZE 52 BY .75 NO-UNDO.

DEFINE VARIABLE l-vl-menor AS LOGICAL INITIAL yes 
     LABEL "Valor Tendància Menor que Estimado/Reestimado" 
     VIEW-AS TOGGLE-BOX
     SIZE 52 BY .75 NO-UNDO.

DEFINE VARIABLE c-est-exec AS CHARACTER FORMAT "X(03)":U 
     LABEL "Estab Exec" 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-item-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 21.72 BY .88 NO-UNDO.

DEFINE VARIABLE c-item-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 19.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-sigla AS CHARACTER FORMAT "X(15)":U 
     LABEL "Sigla" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE d-data-fim AS DATE FORMAT "99/99/9999":U INITIAL TODAY
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE d-data-ini AS DATE FORMAT "99/99/9999":U INITIAL TODAY
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE i-empresa AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88 NO-UNDO.

DEFINE VARIABLE i-esp-fim AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .79 NO-UNDO.

DEFINE VARIABLE i-esp-ini AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Especialidade":R16 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .79 NO-UNDO.

DEFINE VARIABLE i-ord-fim AS INTEGER FORMAT ">>9":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE i-ord-ini AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Ordem" 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88 NO-UNDO.

DEFINE VARIABLE i-ori-fim AS INTEGER FORMAT " 9":U INITIAL 9 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .88 NO-UNDO.

DEFINE VARIABLE i-ori-ini AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Origem" 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .88 NO-UNDO.

DEFINE VARIABLE i-projeto AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Projeto" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE i-sec-fim AS INTEGER FORMAT " 9":U INITIAL 9 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .88 NO-UNDO.

DEFINE VARIABLE i-sec-ini AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Seá∆o" 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .88 NO-UNDO.

DEFINE VARIABLE i-sub-fim AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .79 NO-UNDO.

DEFINE VARIABLE i-sub-ini AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Sub Especialidade":R16 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .79 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-57
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-58
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-59
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-60
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-61
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-62
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-63
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-64
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-65
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-66
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-67
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-68
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0  
     SIZE 78.72 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 11.38
     FGCOLOR 0 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0  
     SIZE .43 BY 11.21
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0  
     SIZE .43 BY 11.17
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0  
     SIZE 78.72 BY .13
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configuraá∆o da impressora"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execuá∆o" NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     rt-folder AT ROW 2.5 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     im-pg-imp AT ROW 1.5 COL 33.57
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-par
     l-item AT ROW 1.04 COL 8
     l-descricao AT ROW 2.04 COL 8
     i-moeda AT ROW 2.38 COL 35.57 NO-LABEL
     l-aprop AT ROW 3.04 COL 8
     l-autor AT ROW 4.04 COL 8
     l-DivMil AT ROW 4.04 COL 53.29
     l-duplic AT ROW 5.04 COL 8
     l-ord-prod AT ROW 6.04 COL 8
     l-mob AT ROW 7.04 COL 8
     l-sem-planej AT ROW 8.04 COL 8
     l-vl-menor AT ROW 9.04 COL 8
     l-vl-maior AT ROW 10.04 COL 8
     de-vl-tenden AT ROW 10.83 COL 33 COLON-ALIGNED
     FILL-IN-8 AT ROW 1.71 COL 33 COLON-ALIGNED NO-LABEL
     RECT-10 AT ROW 1.96 COL 34.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.75
         SIZE 77 BY 11.

DEFINE FRAME f-pg-sel
     i-empresa AT ROW 1.42 COL 18.72 COLON-ALIGNED
     c-est-exec AT ROW 2.42 COL 18.72 COLON-ALIGNED
     i-projeto AT ROW 3.42 COL 18.72 COLON-ALIGNED
     c-sigla AT ROW 3.42 COL 37.14 COLON-ALIGNED
     d-data-ini AT ROW 4.42 COL 18.72 COLON-ALIGNED
     d-data-fim AT ROW 4.42 COL 48.43 COLON-ALIGNED NO-LABEL
     i-ord-ini AT ROW 5.42 COL 18.72 COLON-ALIGNED
     i-ord-fim AT ROW 5.42 COL 48.43 COLON-ALIGNED NO-LABEL
     i-sec-ini AT ROW 6.42 COL 18.72 COLON-ALIGNED
     i-sec-fim AT ROW 6.42 COL 48.43 COLON-ALIGNED NO-LABEL
     i-esp-ini AT ROW 7.42 COL 18.72 COLON-ALIGNED
     i-esp-fim AT ROW 7.42 COL 48.43 COLON-ALIGNED NO-LABEL
     i-sub-ini AT ROW 8.42 COL 18.72 COLON-ALIGNED
     i-sub-fim AT ROW 8.42 COL 48.43 COLON-ALIGNED NO-LABEL
     i-ori-ini AT ROW 9.42 COL 18.72 COLON-ALIGNED
     i-ori-fim AT ROW 9.42 COL 48.43 COLON-ALIGNED NO-LABEL
     c-item-ini AT ROW 10.42 COL 18.72 COLON-ALIGNED
     c-item-fim AT ROW 10.42 COL 48.43 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 5.42 COL 43.29
     IMAGE-2 AT ROW 5.42 COL 47.29
     IMAGE-57 AT ROW 6.42 COL 43.29
     IMAGE-58 AT ROW 6.42 COL 47.29
     IMAGE-59 AT ROW 7.42 COL 43.29
     IMAGE-60 AT ROW 8.42 COL 47.29
     IMAGE-61 AT ROW 8.42 COL 43.29
     IMAGE-62 AT ROW 7.42 COL 47.29
     IMAGE-63 AT ROW 9.42 COL 43.29
     IMAGE-64 AT ROW 10.42 COL 47.29
     IMAGE-65 AT ROW 10.42 COL 43.29
     IMAGE-66 AT ROW 9.42 COL 47.29
     IMAGE-67 AT ROW 4.42 COL 47.29
     IMAGE-68 AT ROW 4.42 COL 43.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 76.86 BY 10.83.


/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Window
 */


/* *************************  Create Window  ************************** */


IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "COD - Caderno Operacional Detalhado"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 28.63
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.63
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE C-Win = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */



/* ************************* Included-Libraries *********************** */




/*-------------------------------------------------------------------------
    Library     : containr.i  
    Purpose     : Default Main Block code and Method Procedures
                  for UIB-generated ADM Container procedures.

    Syntax      : {src/adm/method/containr.i}

    Description :

    Author(s)   :
    Created     :
    HISTORY:
-------------------------------------------------------------------------*/
/***********************  DEFINITIONS  ***********************************/

/* System Variable Definitions ---                                       */
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

/********************************************************************************
** Programa : include/i-sysvar.i
**
** Data : 02/06/1999
**
** Criaá∆o : John Cleber Jaraceski
**
** Objetivo : Definicao das System Variables
**
** Ultima Alt : ?
*******************************************************************************/


        
    DEFINE VARIABLE c-programa-mg97       AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-versao-mg97         AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-modulo-mg97         AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-titulo-prog-mg97    AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-nom-manual-hlp-mg97 AS CHARACTER FORMAT "x(06)":U NO-UNDO.
    DEFINE VARIABLE c-cod-mod-mg97        AS CHARACTER                  NO-UNDO.
    DEFINE VARIABLE d-data-contrato       AS DATE                       NO-UNDO.
    DEFINE VARIABLE i-num-topico-hlp-mg97 AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE i-user-conectados     AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE i-licenca-usuar       AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE l-acesso-livre        AS LOGICAL                    NO-UNDO.


/* include/i-sysvar.i ---                                                     */

 

/* Local Variable Definitions ---                                        */
DEFINE VARIABLE i-ctrl-tab-page   AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-ctrl-tab-folder AS INTEGER   NO-UNDO.
DEFINE VARIABLE c-state-folder    AS CHARACTER NO-UNDO.



    





/* Dialog program to run to set runtime attributes - if not defined in master */



/* +++ This is the list of attributes whose values are to be returned
   by get-attribute-list, that is, those whose values are part of the
   definition of the object instance and should be passed to init-object
   by the UIB-generated code in adm-create-objects. */



/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 1.5
         WIDTH              = 38.43.
/* END WINDOW DEFINITION */
                                                                        */





/* ************************* Included-Libraries *********************** */




/*-------------------------------------------------------------------------
    File        : smart.i  
    Purpose     : Provides basic SmartObject functionality.

    Syntax      : {src/adm/method/smart.i}

    Description :

    Author(s)   :
    Created     :
    Notes       :
--------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

def var c-nom-prog-upc-mg97  as char format "x(50)" no-undo.
def var c-nom-prog-appc-mg97 as char format "x(50)" no-undo.
def var c-nom-prog-dpc-mg97  as char format "x(50)" no-undo.
def var c-ctrl-tab           as char                no-undo.
def var h-ctrl-tab           as handle              no-undo.
def var wh-entry-field       as widget-handle       no-undo.

/* vari†vel que identifica dialogs - n∆o pode-se utilizar pre-processador */
/* porque ela n∆o Ç a primeira include do method libraries                */
/* Vari†vel criada para saber quando um programa Ç dialog.                */
/* Se for YES Ç dialog. A vari†vel foi criada devido a problemas          */
/* com traduá∆o - 1085750 - Valdir tech14187                              */
DEFINE VARIABLE adm-dialog AS LOGICAL INITIAL NO   NO-UNDO.



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

 

/* FO 1540.701 - corp340521 - 22/07/2008 */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 2.93
         WIDTH              = 35.14.
/* END WINDOW DEFINITION */
                                                                        */





/* ************************* Included-Libraries *********************** */

/****************************************************************************
     PROCEDURE: attribut.i

       PURPOSE: holds general-use variable and table definitions
                for ADM Method Libraries

       REMARKS:

    PARAMETERS: NONE

      HISTORY:
*****************************************************************************/

/* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1994-6 - All Rights Reserved. */

/* Make sure not already included */


/* The new Progress widget attribute ADM-DATA is used to store ADM
   attributes and other ADM-specific information. This is new to 8.1, 
   so use PRIVATE-DATA to preserve the ability to compile with 8.0.
   Also there is a new keyword UNLESS-HIDDEN which allows a DISPLAY/ENABLE
   to bypass fields which are hidden. This is used in building alternate
   layouts. */
/* &IF PROVERSION GE "8.1":U &THEN   */
/*   &GLOB    adm-data      ADM-DATA */
/*   &GLOB    unless-hidden          */
/* &ELSE                             */

/* O teste de vers∆o do progress foi retirado pois na vers∆o 10 passaria a causar erros, 
j† que o teste usa string e neste caso 10 Ç menor que 8. Tivemos alguns problemas j† ao testar
a vers∆o beta e foi cadastrado um chamado de Bug - SW */

      
/* &ENDIF */

DEFINE VAR adm-object-hdl       AS HANDLE NO-UNDO. /* current object's handle */
DEFINE VAR adm-query-opened        AS LOGICAL NO-UNDO INIT NO.
DEFINE VAR adm-row-avail-state     AS LOGICAL NO-UNDO INIT ?.
DEFINE VAR adm-initial-lock        AS CHARACTER NO-UNDO INIT "NO-LOCK":U.
DEFINE VAR adm-new-record          AS LOGICAL NO-UNDO INIT no.
DEFINE VAR adm-updating-record     AS LOGICAL NO-UNDO INIT no.
DEFINE VAR adm-check-modified-all  AS LOGICAL NO-UNDO INIT no.

DEFINE NEW GLOBAL SHARED VAR adm-broker-hdl    AS HANDLE  NO-UNDO.



 
 

/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */

/* The code to assign the object handle (which becomes the ADM-OBJECT-HANDLE
   attribute below) for containers and for other objects has been combined
   here. Note that setting adm-object-hdl later in user code (including the
   main block of a MLI) will have no effect on the value of the attribute.
   To override these default settings (which should be appropriate for 
   virtually all objects) user code must 
     RUN set-attribute-list ('ADM-OBJECT-HANDLE=...').

   For SmartContainers, set the handle to the Frame handle if the
   Container Type is FRAME or DIALOG-BOX, else to WINDOW, unless the
   Container is "virtual" (no visualization), in which case leave it unknown.

   For other objects, set the handle to the default Frame handle if 
   there is one.
*/


  


/* Traduá∆o de Hard-Coded View-as */ 

    






/* If the broker handle either isn't valid or isn't the right process
   (it's possible the handle has been reused), then start the broker. 
   (But don't let the broker try to start itself!) */

RUN get-attribute IN adm-broker-hdl ('TYPE':U) NO-ERROR.
IF RETURN-VALUE NE "ADM-Broker":U THEN 
DO: 
    RUN adm/objects/broker.p PERSISTENT set adm-broker-hdl. 
    RUN set-broker-owner IN adm-broker-hdl (THIS-PROCEDURE).
END.


/* Initialize all the attributes which all SmartObjects must have. */

THIS-PROCEDURE:ADM-DATA = 
     'ADM1.1~`':U +         /* Version attribute */
     'Window~`':U +      /* Type attribute */
     '~`':U +       /* Container-Type attribute */
   
     'NO ~`':U +
   
     '~`':U +    /* External-Tables attribute */
     '~`':U +    /* Internal-Tables attribute */
   
     '~`':U +     /* Enabled-Tables attribute */
   
     (IF adm-object-hdl = ? THEN "":U ELSE STRING(adm-object-hdl))
        + "~`":U +    /* Adm-Object-Handle attribute */
   
     'Layout,Hide-on-Init~`':U +  /* Attribute-List attribute */
   
   
     '~`':U + /* Supported-Links attribute */
   
     '~`':U +  /* ADM-Dispatch-Qualifier attr */
     '~`~`~`~`~`~`~`~`~`~`~`':U +   /* Placeholders for ADM-Parent, Layout,
                                      Enabled, Hidden, COntainer-Hidden,
                                      Initialized, Fields-Enabled, Current-Page,
                                      ADM-New-Record, UIB-Mode, 
                                      ADM-Deactivate-Links */
    /* PLUS THERE IS AN EXTRA TICK FOR THE DUMMY PREPROC
       which marks the end of the list. Do not disturb. */ 
     IF THIS-PROCEDURE:ADM-DATA = "":U OR THIS-PROCEDURE:ADM-DATA = ? 
         THEN "^^":U             /* plus placeholders for user-defined attrs. */
     /* Or if there are already attributes defined, don't throw them away. */
     ELSE "^":U + ENTRY(2, THIS-PROCEDURE:ADM-DATA, "^":U) + 
          "^":U + ENTRY(3, THIS-PROCEDURE:ADM-DATA, "^":U).


/* An "apply-layout" method is not necessary if there are no layout-cases */

  

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */




PROCEDURE adm-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Applies "ENTRY" to the first enabled field or other 
               object in the SmartObject.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR c_Handle AS CHAR NO-UNDO.
  ASSIGN c_Handle = "".
  RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, 
                                         INPUT 'TABLEIO-SOURCE':U,
                                         OUTPUT c_Handle ).
  IF c_Handle <> "" THEN                                       
  RUN broker-apply-entry IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */









PROCEDURE adm-destroy :
/* -----------------------------------------------------------
      Purpose:     Basic routine to delete a procedure and its
                   CONTAINED descendents
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   

 
        /***************************************************************
**
** I-EPC100.I - EPC para Evento DESTROY de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "DESTROY":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "DESTROY":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame f-relat:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "DESTROY":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC100 */
 
 

 RUN broker-destroy IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-disable :
/* -----------------------------------------------------------
      Purpose:     Disables all enabled objects in the frame.
                   Note that this includes db fields if any.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   
    /* EPC Before Disable do Container */
    
           /***************************************************************
**
** I-EPC009.I - EPC para Evento Before DISABLE de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "BEFORE-DISABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "BEFORE-DISABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame f-relat:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "BEFORE-DISABLE", 
                                    input "CONTAINER",
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC009.I */

 
    

    

    /* EPC Disable do Container */
    
           /***************************************************************
**
** I-EPC009.I - EPC para Evento DISABLE de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "DISABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "DISABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame f-relat:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "DISABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC009.I */

 
    


    RUN set-attribute-list ('ENABLED=no':U).

    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-edit-attribute-list :
/* -----------------------------------------------------------
      Purpose:    Runs the dialog to get runtime parameter settings
      Parameters:  <none>
      Notes:       Generally run by the UIB in design mode
    -------------------------------------------------------------*/   
  /* Must be defined in the Object*/
      RUN adm/support/contnrd.w (INPUT THIS-PROCEDURE).
  

      RETURN. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-enable :
/* -----------------------------------------------------------
      Purpose:    Enable an object - all components except db fields,
                  which are enabled using enable-fields.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   
   /* EPC Before Enable do Container */
   
          /***************************************************************
**
** I-EPC008.I - EPC para Evento Before ENABLE de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "BEFORE-ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "BEFORE-ENABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame f-relat:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "BEFORE-ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC008 */

 
   

   

   /* EPC Enable do Container */
   
          /***************************************************************
**
** I-EPC008.I - EPC para Evento ENABLE de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "ENABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame f-relat:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC008 */

 
   

   RUN set-attribute-list ('ENABLED=yes':U).

   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-exit :
/* -----------------------------------------------------------
      Purpose: Passes an exit request to its container    
      Parameters:  <none>
      Notes:  The convention is that the standard routine always
          passes an exit request to its CONTAINER-SOURCE. The container 
          that is actually able to initiate the exit should define
          a local version and *not* call the standard one.    
          That local-exit is built into the SmartWindow template.
    -------------------------------------------------------------*/   

     RUN notify ('exit':U).

  RETURN.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-hide :
/* -----------------------------------------------------------
      Purpose:     Hides an object and sets any active links which
                   are dependent on hide/view off.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   
  RUN broker-hide IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-initialize :
/* -----------------------------------------------------------
      Purpose:     Enables and Views an object unless its attributes
                   indicate this should not be done.
                   Cascades 'initialize' to descendents.
      Parameters:  <none>
      Notes:       
   -------------------------------------------------------------*/   
   /* alteraªío feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */

   /* est† verificaá∆o se faz necess†ria devido aos programas */
      


    /* Tenta identificar pelo ADM-CONTAINER */
    

    /* Se ainda nío identificou se ≤ window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
        /* Tenta identificar pelo PROCEDURE-TYPE */
        
        
            

                                
            
            
        
    
    

    /* Se ainda nío identificou se ≤ window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
    
    /*
    &IF DEFINED(WenIsWindow) &THEN
        RUN registerWindow IN hWenController (THIS-PROCEDURE, CURRENT-WINDOW).
    &ELSEIF DEFINED(WenIsDialog) &THEN
        RUN registerWindow IN hWenController (THIS-PROCEDURE, CURRENT-WINDOW).
    &ENDIF
    */

    /* Se tem janela */
    

        ON 'U9':U ANYWHERE
        DO:
            IF VALID-HANDLE(hWenController) THEN DO:
                RUN registerWindow IN hWenController (THIS-PROCEDURE, SELF).
            END.
        END.

    

    /* Se tem dialog */
    


            /* criados pelos DataViewer n∆o utilizarem a include i-prgvrs */ 
   /* e dessa forma n∆o chamarem a include i-wendef.i que define essa veri†vel. */

   /* fim da alateraá∆o */

   /* EPC Before Initialize do Container */ 
   
          /***************************************************************
**
** I-EPC007.I - EPC para Evento Before INITIALIZE de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "BEFORE-INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "BEFORE-INITIALIZE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame f-relat:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "BEFORE-INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC007.I */

 
   

   /* EPC Before Initialize do Viewer */ 
   

   /* EPC Before Initialize do Browser */
   

   RUN broker-initialize IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.
   /*   Alteraá∆o para corrigir o problema de algumas viewers n∆o mostrar
        o primeiro registro quando o programa Ç inicializado               
        Data : 20/Fev/97  - J.Carlos (PGS)  -  Egolf e SÇrgio (DATASUL)  */  
   
        
   
                                                               /*
    Nome :  C-PAGE.i       J.Carlos - 21/Fev/97
    Funªío : Guardar a pagina e o container-source da VIEWER.
*/

   def var c_Aux-var as char no-undo.
   RUN get-link-handle IN adm-broker-hdl (INPUT  THIS-PROCEDURE,
                                          INPUT  "CONTAINER-SOURCE":U,
                                          OUTPUT c_Aux-var).
   RUN set-attribute-list ("W-Container-Source = ":U + string(c_Aux-var)).
   RUN What-is-the-Page IN adm-broker-hdl (INPUT THIS-PROCEDURE).
   RUN set-attribute-list ("W-Page = ":U + RETURN-VALUE). 
 

   
        run get-link-handle in adm-broker-hdl
             (input this-procedure,
              input 'page':U,
              output c-ctrl-tab).
        assign h-ctrl-tab = if c-ctrl-tab <> "" then widget-handle(c-ctrl-tab) else ?.
   

   /* EPC - Initialize do Container */ 
   
          /***************************************************************
**
** I-EPC007.I - EPC para Evento INITIALIZE de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "INITIALIZE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame f-relat:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-relat:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC007.I */

 
   

   /* EPC - Initialize do Viewer */ 
   

   /* EPC - Initialize do Browser */
   

   
       RUN get-attribute IN THIS-PROCEDURE ("ApplyFillIn":U).
       IF ENTRY(1, RETURN-VALUE, "|":U) = "YES":U THEN
          RUN ApplyFillIn IN WIDGET-HANDLE(ENTRY(2, RETURN-VALUE, "|":U)).
   

   /*Traduá∆o dos campos de tela*/
   
   /*final da traduá∆o dos campos de tela*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-show-errors :
/* -----------------------------------------------------------
      Purpose:  Display system error messages on a runtime error.
      Parameters:  <none>
      Notes:    A localization of this method can look at the message
                number to display a custom error or suppress standard
                error display.
    -------------------------------------------------------------*/

    DEFINE VARIABLE        cntr                  AS INTEGER   NO-UNDO.

    DO cntr = 1 TO ERROR-STATUS:NUM-MESSAGES:
        MESSAGE ERROR-STATUS:GET-MESSAGE(cntr).
    END.

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-UIB-mode :
/*--------------------------------------------------------------------------
  Purpose     : Set the objects attributes in "UIB Mode".  This is the
                "mode" it will have in design-mode in the UIB.
  Notes       :
  ------------------------------------------------------------------------*/

  RUN broker-UIB-mode IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-view :
/* -----------------------------------------------------------
      Purpose:     Views an object and sets active links on.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   

  RUN broker-view IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE dispatch :
/* -----------------------------------------------------------
      Purpose:    Determines whether to run the LOCAL or STANDARD (adm-)
                  or no-prefix version of a method in the current procedure.
      Parameters: INPUT base method name (with no prefix),
      Notes:      In addition, if the developer has defined a custom prefix
                  as ADM-DISPATCH-QUALIFIER, then a method with this prefix
                  will be searched for after "local-" and before "adm-".
                  If the preprocessor ADM-SHOW-DISPATCH-ERRORS is defined
                  then the show-errors method will be dispatched if a
                  method name is not found in any form. This can be 
                  useful for debugging purposes.
    -------------------------------------------------------------*/   

    DEFINE INPUT PARAMETER p-method-name    AS CHARACTER NO-UNDO.

    RUN broker-dispatch IN adm-broker-hdl 
        (THIS-PROCEDURE, p-method-name) NO-ERROR.
    IF RETURN-VALUE = "ADM-ERROR":U THEN RETURN "ADM-ERROR":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE get-attribute :
/* -----------------------------------------------------------
      Purpose:     Returns the value of a std variable or attribute-table entry.
      Parameters:  INPUT attribute name, RETURN-VALUE (string)
      Notes:       
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-attr-name    AS CHARACTER NO-UNDO.

  RUN broker-get-attribute IN adm-broker-hdl
      (INPUT THIS-PROCEDURE, INPUT p-attr-name) NO-ERROR.

  RETURN RETURN-VALUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE get-attribute-list :
/* -----------------------------------------------------------
      Purpose:     Returns a list of all settable object attributes.
      Parameters:  OUTPUT comma-separated attribute list
      Notes:       This procedure does not return a list of *all*
                   attributes, but only those which are defined and
                   set by users (e.g., not HIDDEN, ENABLED... ).
                   In Version 8.1., an INPUT parameter has been added
                   to broker-get-attribute-list to allow a caller to
                   specify a particular list of attributes to return.
                   This standard call does not specify a list, so
                   the attributes in the ADM-ATTRIBUTE-LIST attribute
                   are returned.
    -------------------------------------------------------------*/   

  DEFINE OUTPUT PARAMETER p-attr-list AS CHARACTER NO-UNDO.

  RUN broker-get-attribute-list IN adm-broker-hdl
      (INPUT THIS-PROCEDURE, 
       INPUT ?,           /* Use the defined list of attributes to return */
       OUTPUT p-attr-list) NO-ERROR.

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE new-state :
/* -----------------------------------------------------------
   Purpose:     Stub to send state message off to the broker process.
   Parameters:  state name (CHARACTER) - may also contain one or more
                link names to pass state message through, as part of a
                comma-separated list.
   Notes:       
-------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  RUN broker-new-state IN adm-broker-hdl (THIS-PROCEDURE, p-state) NO-ERROR.

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE notify :
/* -----------------------------------------------------------
   Purpose:     Stub to pass notify command to broker process
   Parameters:  method name (CHARACTER) - may also include one or more
                link types to pass message through as part of commas-separated
                list.
   Notes:       
-------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER p-method AS CHARACTER NO-UNDO.

  RUN broker-notify IN adm-broker-hdl (THIS-PROCEDURE, p-method) NO-ERROR.
  IF RETURN-VALUE = "ADM-ERROR":U THEN 
      RETURN "ADM-ERROR":U.  

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-trad-widgets :
/*------------------------------------------------------------------------------
  Purpose:    Traduá∆o dos hard-coded view-as de atributos 
  Parameters: p-wh-frame - handle do frame
  Notes:       
------------------------------------------------------------------------------*/

  define input param p-wh-frame as widget-handle no-undo.

  define var wh-child     as widget-handle no-undo. 
  define var c-aux        as char          no-undo.
  define var i-aux        as integer       no-undo.  
  define var c-contexto   as char          no-undo init "".

  /* FO 1540.701 - corp340521 - 22/07/2008  */

  
  
  assign p-wh-frame:BGCOLOR = ?
         p-wh-frame:FONT    = 1.
  
  assign p-wh-frame = p-wh-frame:FIRST-CHILD
         wh-child   = p-wh-frame:FIRST-CHILD.
  
  

  do  while valid-handle(wh-child):

      
  

      case wh-child:type:
          when "RADIO-SET" then do:
              if  wh-child:table <> ? then do:
                  assign c-aux = wh-child:radio-buttons.
                  if  wh-child:private-data <> "" 
                  and wh-child:private-data <> ? then 
                      assign c-contexto = wh-child:private-data. 
                  else
                      assign c-contexto = "*".  
                  do  i-aux = 1 to num-entries(wh-child:radio-buttons):
                      if  (i-aux mod 2) <> 0 then do:
                          run utp/ut-liter.p (input replace(entry(i-aux, wh-child:radio-buttons), chr(32), "_"),
                                              input c-contexto,
                                              input "R"). 
                          assign entry(i-aux, c-aux) = return-value.
                      end.
                  end.                                              
                  assign wh-child:radio-buttons = c-aux.
              end.
          end.
          when "BUTTON" then do:
              if  wh-child:label <> ?
              and wh-child:label <> "" then do:
                  run utp/ut-liter.p (input replace(wh-child:label, chr(32), "_"),
                                      input "",
                                      input "C"). 
                  assign wh-child:label = trim(return-value).
              end. 
              if  wh-child:help <> "" 
              and wh-child:help <> ? then do:
                  run utp/ut-liter.p (input replace(wh-child:help, chr(32), "_"),
                                      input "",
                                      input "R"). 
                  assign wh-child:help = return-value
                         wh-child:tooltip = trim(return-value).
              end.         

          end.
      end case.
      assign wh-child = wh-child:next-sibling.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




/* FO 1540.701 - corp340521 - 22/07/2008 */




PROCEDURE set-attribute-list :
/* -----------------------------------------------------------
      Purpose:     Accepts the value of the complete object attribute list
                   and runs procedures to set individual attributes.
      Parameters:  INPUT comma-separated attribute list.
      Notes:       Not all attributes are settable. Those which are a
                   part of an event such as enable/disable (which set
                   ENABLED on/off) or hide/view (which set HIDDEN on/off)
                   can be queried through get-attribute but cannot be set.
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-attr-list    AS CHARACTER NO-UNDO.

  RUN broker-set-attribute-list IN adm-broker-hdl
      (INPUT THIS-PROCEDURE, INPUT p-attr-list) NO-ERROR.

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE set-position :
/* -----------------------------------------------------------
  Purpose:     Moves an object to a specified position.
  Parameters:  ROW and COLUMN 
  Notes:       
-------------------------------------------------------------*/

    DEFINE INPUT PARAMETER p-row    AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER p-col    AS DECIMAL NO-UNDO.

    IF VALID-HANDLE(adm-object-hdl) THEN
    DO:     
      /* If this is a Window or a Dialog box which is being positioned,
         then the special value 0 means to center the object in that
         dimension (0,0 means center on the screen - 0 can be used to
         signal this because 0 is an invalid row or column position). */
      
      /* Set object's position */
      ASSIGN adm-object-hdl:ROW    =   p-row 
             adm-object-hdl:COLUMN =   p-col.
    END.  
    RETURN.

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */




 

/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */

/* Initialize page number and object handle attributes. */
RUN set-attribute-list ("CURRENT-PAGE=0,ADM-OBJECT-HANDLE=":U +
    STRING(adm-object-hdl)). 


/* Best default for GUI applications - this will apply to the whole session: */
PAUSE 0 BEFORE-HIDE.

on  CTRL-TAB anywhere do:
    if valid-handle(focus) then
      apply "leave" to focus.
    assign c-state-folder = "no".

    if  valid-handle(h-ctrl-tab) then do:
        run get-attribute in h-ctrl-tab('folder-labels':U).
        assign i-ctrl-tab-folder = NUM-ENTRIES(return-value,'|':U).

        run get-attribute('current-page':U).
        assign i-ctrl-tab-page = int(return-value) + 1.

        do while c-state-folder = "no":
            run pi-state-folder in h-ctrl-tab (input i-ctrl-tab-page).
            assign c-state-folder = RETURN-VALUE.

            if c-state-folder = "no" 
            then assign i-ctrl-tab-page = i-ctrl-tab-page + 1.
            else run select-page(i-ctrl-tab-page).

            if i-ctrl-tab-page > i-ctrl-tab-folder 
            then do: 
                assign i-ctrl-tab-page = 1.
                leave.
            end.
        end.

        do while c-state-folder = "no":
            run pi-state-folder in h-ctrl-tab (input i-ctrl-tab-page).
            assign c-state-folder = RETURN-VALUE.

            if c-state-folder = "no" 
            then assign i-ctrl-tab-page = i-ctrl-tab-page + 1.
            else run select-page(i-ctrl-tab-page).

            if i-ctrl-tab-page > i-ctrl-tab-folder 
            then do: 
                assign i-ctrl-tab-page = 1.
                leave.
            end.
        end.
    end.
end.

on  SHIFT-CTRL-TAB anywhere do:
    if valid-handle(focus) then
      apply "leave" to focus.
    assign c-state-folder = "no".

    if  valid-handle(h-ctrl-tab) then do:
        run get-attribute in h-ctrl-tab('folder-labels':U).
        assign i-ctrl-tab-folder = NUM-ENTRIES(return-value,'|':U).

        run get-attribute('current-page':U).
        assign i-ctrl-tab-page = int(return-value) - 1.

        do while c-state-folder = "no":
            run pi-state-folder in h-ctrl-tab (input i-ctrl-tab-page).
            assign c-state-folder = RETURN-VALUE.

            if c-state-folder = "no" 
            then assign i-ctrl-tab-page = i-ctrl-tab-page - 1.
            else run select-page (i-ctrl-tab-page).

            if i-ctrl-tab-page < 1
            then do: 
                assign i-ctrl-tab-page = i-ctrl-tab-folder.
                leave.
            end.
        end.

        do while c-state-folder = "no":
            run pi-state-folder in h-ctrl-tab (input i-ctrl-tab-page).
            assign c-state-folder = RETURN-VALUE.

            if c-state-folder = "no" 
            then assign i-ctrl-tab-page = i-ctrl-tab-page - 1.
            else run select-page(i-ctrl-tab-page).

            if i-ctrl-tab-page < 1
            then do: 
                assign i-ctrl-tab-page = i-ctrl-tab-folder.
                leave.
            end.
        end.
    end.
end.

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */




PROCEDURE adm-change-page :
/* -----------------------------------------------------------
      Purpose:    Views objects on a newly selected page, initializing
                  them if the page has not yet been seen.
      Parameters: <none>
      Notes:      In character mode, when switching from the main window
                  to a page which is another window (in GUI), the
                  main window's default frame must be hidden; and when
                  returning it must be viewed. This is done below.
-------------------------------------------------------------*/   

  /* EPC - Change Page de Container */ 
  
        /***************************************************************
**
** I-EPC014.I - EPC para Evento CHANGE-PAGE de Container 
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.


    
    
    

/* I-EPC014.I */
 
  

  RUN broker-change-page IN adm-broker-hdl (INPUT THIS-PROCEDURE) NO-ERROR.

  /* EPC - After Change Page de Container */ 
  
        /***************************************************************
**
** I-EPC014.I - EPC para Evento After CHANGE-PAGE de Container 
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.


    
    
    

/* I-EPC014.I */
 
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE delete-page :
/* -----------------------------------------------------------
      Purpose:     Destroys all objects on the current page.
      Parameters:  INPUT page number
      Notes:       
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-page# AS INTEGER NO-UNDO.

  RUN broker-delete-page IN adm-broker-hdl 
      (INPUT THIS-PROCEDURE, INPUT p-page#).

  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE init-object :
/* -----------------------------------------------------------
   Purpose:     RUNS an object procedure PERSISTENT and initializes
                default links
   Parameters:  INPUT procedure name, parent handle, attribute-list,
                OUTPUT procedure handle
   Notes:       init-object calls are generated by the UIB 
                in adm-create-objects
-------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER  p-proc-name   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER  p-parent-hdl  AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER  p-attr-list   AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER p-proc-hdl    AS HANDLE    NO-UNDO.

  RUN broker-init-object IN adm-broker-hdl
      (INPUT THIS-PROCEDURE, INPUT p-proc-name, INPUT p-parent-hdl,
       INPUT p-attr-list, OUTPUT p-proc-hdl) NO-ERROR.
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE init-pages :
/* -----------------------------------------------------------
      Purpose:     Initializes one or more pages in a paging
                   control without actually viewing them. 
                   This can be used either for initializing pages
                   at startup without waiting for them to be
                   selected, or for creating additional or
                   replacement pages after startup.
      Parameters:  INPUT comma-separated list of page numbers
      Notes:       Generally this method does not need to be used,
                   unless the user specifically wants to incur the
                   overhead of creating and initializing pages before
                   they are first viewed. When one page in a multi-page
                   SmartContainer has a SmartLink dependency on another
                   page, the UIB will automatically generate the calls
                   to init-pages to assure that the right other pages have been
                   initialized when a page is selected for the first time.
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-page-list      AS CHARACTER NO-UNDO.  

  RUN broker-init-pages IN adm-broker-hdl (INPUT THIS-PROCEDURE,
      INPUT p-page-list) NO-ERROR.

  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-retorna-appc :
/*------------------------------------------------------------------------------
  Purpose:  Retorna o nome do programa APPC   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  return c-nom-prog-appc-mg97.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-retorna-dpc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  return c-nom-prog-dpc-mg97.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-retorna-upc :
/*------------------------------------------------------------------------------
  Purpose:  Retonra o nome do programa UPC    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  return c-nom-prog-upc-mg97.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-retorna-vars-hlp :
/*------------------------------------------------------------------------------
  Purpose:   Retorna variaveis de acesso ao Help
  Parameters: p-num-topico-hlp - numero do topico do programa
              p-nom-manual-hlp - nome do arquivo hlp do modulo do programa
  Notes:       
------------------------------------------------------------------------------*/

define output parameter p-num-topico-hlp as integer no-undo.
define output parameter p-nom-manual-hlp as char format "x(06)" no-undo.

assign p-num-topico-hlp = i-num-topico-hlp-mg97
       p-nom-manual-hlp = c-nom-manual-hlp-mg97.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE select-page :
/* -----------------------------------------------------------
      Purpose:     Makes a new set of objects associated with a page number
                   current, by hiding the previous page, if any,
                   creating the objects in the new page if the page hasn't
                   been initialized, and viewing the new page.
      Parameters:  INPUT new page number
      Notes:       
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-page#     AS INTEGER   NO-UNDO.

  RUN broker-select-page IN adm-broker-hdl (INPUT THIS-PROCEDURE,
      INPUT p-page#) NO-ERROR.

  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE view-page :
/* -----------------------------------------------------------
      Purpose:     Makes a new set of objects associated with a page number
                   current, without hiding the previous page, if any,
                   creating the objects in the new page if the page hasn't
                   been initialized, and viewing the new page.
      Parameters:  INPUT new page number
      Notes:       This method does not reset the value of adm-current-page,
                   because the new page is being viewed without hiding the
                   old one. adm-current-page is the most recently "selected"
                   page.
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-page#      AS INTEGER   NO-UNDO.

  RUN broker-view-page IN adm-broker-hdl (INPUT THIS-PROCEDURE,
      INPUT p-page#).

  END PROCEDURE.
/* This ENDIF statement needs to stay here (or with the last procedure in the
   include file) to balance the &IF adm-container at the top: */


/* _UIB-CODE-BLOCK-END */




 



/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define variable wh-pesquisa             as handle               no-undo.
define variable c-arq-old               as char                 no-undo.
define variable c-arq-old-batch         as char                 no-undo.

define variable c-imp-old               as char                 no-undo.

/*tech868*/

    


/*tech14178 funá‰es PDF */
/*tech868*/

    /*Alteracao 03/04/2008 - tech40260 - FO 1746516 -  Feito validaá∆o para verificar se a variavel h_pdf_controller j† foi definida 
                                                       anteriormente, evitando erro de duplicidade*/

    
        DEFINE VARIABLE h_pdf_controller     AS HANDLE NO-UNDO.

        
        FUNCTION allowPrint RETURNS LOGICAL IN h_pdf_controller.
        
        FUNCTION allowSelect RETURNS LOGICAL IN h_pdf_controller.
        
        FUNCTION useStyle RETURNS LOGICAL IN h_pdf_controller.
        
        FUNCTION usePDF RETURNS LOGICAL IN h_pdf_controller.
    
    
    /*Alteracao 03/04/2008 - tech40260 - FO 1746516 -  Feito validaá∆o para verificar se a variavel h_pdf_controller j† foi definida 
                                                       anteriormente, evitando erro de duplicidade*/






/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 1.83
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 



/* ************************* Included-Libraries *********************** */

assign frame f-relat:visible = no
       frame f-relat:font = 1.

    assign frame f-pg-sel:visible = no
           frame f-pg-sel:font = 1.



    assign frame f-pg-par:visible = no
           frame f-pg-par:font = 1.



    assign frame f-pg-imp:visible = no
           frame f-pg-imp:font = 1.



/***************************************************************************
**
**  include para validar return e go nos programas de relatorio do magnus 97
**
**  Sergio Weber Junior
****************************************************************************/

    
    ON  GO OF frame f-pg-sel ANYWHERE DO:    
        if  self:type <> "editor":U 
            or (self:type = "editor":U 
            and keyfunction(lastkey) <> "RETURN":U) then do: 
            apply "choose":U to bt-executar in frame f-relat.
        end.
      END.              
    
    
    
    
    ON  GO OF frame f-pg-par ANYWHERE DO:    
        if  self:type <> "editor":U 
        or (self:type = "editor":U 
        and keyfunction(lastkey) <> "RETURN":U) then do: 
            apply "choose":U to bt-executar in frame f-relat.
        end.
      END.              
    
    
    
    
    
    ON  GO OF frame f-pg-imp ANYWHERE DO:    
        if  self:type <> "editor":U 
        or (self:type = "editor":U 
        and keyfunction(lastkey) <> "RETURN") then do: 
            apply "choose":U to bt-executar in frame f-relat.
        end.
      END.              
    

 

/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */


PROCEDURE pi-trata-state :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Purpose:     Trata as mudanáas de estado (State-Changed)
  Parameters:  INPUT Handle da procedure pai
               INPUT C¢digo do Estado
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl   AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state        AS CHAR   NO-UNDO.
  
  case entry(1, p-state, '|'):
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



 
{utp/ut-glob.i}


/* _UIB-CODE-BLOCK-END */





/* ***********  Runtime Attributes and AppBuilder Settings  *********** */


/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execuá∆o".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
ASSIGN 
       c-item-ini:HIDDEN IN FRAME f-pg-sel           = TRUE.

ASSIGN 
       d-data-ini:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */



/* Setting information for Queries and Browse Widgets fields            */


/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */



/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */


 



/* ************************  Control Triggers  ************************ */


ON END-ERROR OF C-Win /* COD - Caderno Operacional Detalhado */
OR ENDKEY OF C-Win ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON WINDOW-CLOSE OF C-Win /* COD - Caderno Operacional Detalhado */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   /*************************************************************************
**
** AJUDA.I - Include padr∆o para chamada do Help
**
**************************************************************************/


    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).


RETURN NO-APPLY.

/* include/ajuda.i */
 
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    /*****************************************************************
**
** I-RPARQ - Choose of bt-Arquivo no template de relat¢rio
**
*****************************************************************/

    def var c-arq-conv  as char no-undo.

    /* tech1139 - FO 1223.694  - 02/11/2005 */
    assign c-arq-conv = replace(input frame f-pg-imp c-arquivo, "/", CHR(92)).
    /* tech1139 - FO 1223.694  - 02/11/2005 */

    
/*tech14178 modificado para apresentar dialog com extens∆o PDF quando o mesmo estiver sendo usado */
/*tech868*/
    
    IF NOT usePDF() THEN


    
        SYSTEM-DIALOG GET-FILE c-arq-conv
           FILTERS "*.lst" "*.lst",
                   "*.*" "*.*"
           ASK-OVERWRITE 
           DEFAULT-EXTENSION "lst"
           INITIAL-DIR "spool" 
           SAVE-AS
           USE-FILENAME
           UPDATE l-ok.

/*tech868*/
   ELSE
       SYSTEM-DIALOG GET-FILE c-arq-conv
          FILTERS "*.pdf" "*.pdf",
                  "*.*" "*.*"
          ASK-OVERWRITE 
          DEFAULT-EXTENSION "pdf"
          INITIAL-DIR "spool" 
          SAVE-AS
          USE-FILENAME
          UPDATE l-ok.




    if  l-ok = yes then do:
        /* tech1139 - FO 1223.694  - 02/11/2005 */
        assign c-arquivo = replace(c-arq-conv, CHR(92), "/"). 
        /* tech1139 - FO 1223.694  - 02/11/2005 */
        display c-arquivo with frame f-pg-imp.
    end.

/* i-rparq */
 
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Cancelar */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   /***************************************************************
**
** I-RPIMP.I - Choose of bt-config-impr do template de relat¢rio
**
***************************************************************/ 
def var c-arquivo-temp as char no-undo.
def var c-impressora as char no-undo.
def var c-arq as char no-undo.
def var c-layout as char no-undo.
def var c-ant as char no-undo.

assign c-ant = c-arquivo:screen-value in frame f-pg-imp
       c-arquivo-temp =  replace(c-arquivo:screen-value in frame f-pg-imp,":",",").
if c-arquivo:screen-value in frame f-pg-imp <> "" then do:
  if num-entries(c-arquivo-temp) = 4 then
    assign c-impressora = entry(1,c-arquivo-temp)
           c-layout     = entry(2,c-arquivo-temp)
           c-arq        = entry(3,c-arquivo-temp) + ":" + entry(4,c-arquivo-temp).
  if num-entries(c-arquivo-temp) = 3 then
    assign c-impressora = entry(1,c-arquivo-temp)
           c-layout     = entry(2,c-arquivo-temp)
           c-arq        = entry(3,c-arquivo-temp).
  if num-entries(c-arquivo-temp) = 2 then
    assign c-impressora = entry(1,c-arquivo-temp)
           c-layout     = entry(2,c-arquivo-temp)
           c-arq        = "".
end.         

run utp/ut-impr.w (input-output c-impressora, input-output c-layout, input-output c-arq).

if c-arq = "" then
  assign c-arquivo = c-impressora + ":" + c-layout.
else
  assign c-arquivo = c-impressora + ":" + c-layout + ":" + c-arq.  



if c-arquivo = ":" then
  assign c-arquivo = c-ant.

assign c-imp-old = c-arquivo.
  
disp c-arquivo with frame f-pg-imp.

/* i-rpimp */
 
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF c-sigla IN FRAME f-pg-sel /* Sigla */
DO:
  find first proj-inv where 
             proj-inv.ep-codigo = input frame f-pg-sel i-empresa and
             proj-inv.cod-est-exec = input frame f-pg-sel c-est-exec and
             proj-inv.sigla = input frame f-pg-sel c-sigla no-lock no-error.
   if avail proj-inv then do:
      assign i-projeto:screen-value = string(proj-inv.num-projeto).
      assign i-projeto = proj-inv.num-projeto.
   end.   
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF i-projeto IN FRAME f-pg-sel /* Projeto */
DO:
   find first proj-inv where 
              proj-inv.ep-codigo = input frame f-pg-sel i-empresa and
              proj-inv.cod-est-exec = input frame f-pg-sel c-est-exec and
              proj-inv.num-projeto = input frame f-pg-sel i-projeto no-lock no-error.
     if avail proj-inv then do:
         assign c-sigla:screen-value = proj-inv.sigla.
         assign c-sigla = proj-inv.sigla.
     end.
   
END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */




ON VALUE-CHANGED OF l-item IN FRAME f-pg-par /* Item */
DO:
  if input frame f-pg-par l-item = yes then do:
     assign input frame f-pg-par l-descricao:sensitive in frame f-pg-par = yes.
  end.
  else do:
     assign l-descricao:screen-value = string(no)
            l-descricao:sensitive in frame f-pg-par = no.
  end.
 
END.

/* _UIB-CODE-BLOCK-END */




ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = yes.
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */




ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   /****************************************************************
**
** I-RPRSE.I - Gatilho "Value-Changed" de rs-execucao 
**
*****************************************************************/

ASSIGN rs-execucao.

IF rs-execucao = 2 THEN DO:
    IF rs-destino:SCREEN-VALUE IN FRAME f-pg-imp <> "1":U THEN
        ASSIGN rs-destino:SCREEN-VALUE IN FRAME f-pg-imp = "2":U
               c-arquivo       = IF c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = "":U
                                 THEN IF c-arquivo = "" 
                                      THEN c-arq-old
                                      ELSE c-arquivo
                                 ELSE c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp
               c-arq-old       = c-arquivo
               c-arq-old-batch = SUBSTRING(c-arquivo, R-INDEX(c-arquivo, "/":U) + 1)
               c-arquivo:screen-value = c-arq-old-batch.
    
    
    APPLY "VALUE-CHANGED":U TO rs-destino IN FRAME f-pg-imp.

    IF c-arq-old-batch NE "" THEN /* Por Thiago Garcia ref. FO 901.132 */
        
    
    ASSIGN c-arquivo.
    
    rs-destino:DISABLE(c-terminal) IN FRAME f-pg-imp.
    /*Alterado tech1007 - 14/02/2005 - Removido pois a opá∆o de RTF foi removida
      do rs-destino
    &IF "{&RTF}":U = "YES":U &THEN
        rs-destino:DISABLE(c-rtf) IN FRAME f-pg-imp.
    &ENDIF
    Fim alteracao tech1007 - 14/02/2005*/
END.
ELSE DO:
    IF rs-destino:SCREEN-VALUE IN FRAME f-pg-imp <> "1":U THEN
        ASSIGN c-arquivo       = IF c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = "":U
                                 THEN c-arquivo
                                 ELSE c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp
               c-arq-old-batch = c-arquivo.
    
    APPLY "VALUE-CHANGED":U TO rs-destino IN FRAME f-pg-imp.
    
    rs-destino:ENABLE(c-terminal) IN FRAME f-pg-imp.
    /*Alterado tech1007 - 14/02/2005 - Removido pois a opá∆o de RTF foi removida
      do rs-destino
    &IF "{&RTF}":U = "YES":U &THEN
        rs-destino:ENABLE(c-rtf) IN FRAME f-pg-imp.
    &ENDIF
    Fim alteracao tech1007 - 14/02/2005*/
    ASSIGN c-arquivo.
END.
 
END.

/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = C-Win 
       THIS-PROCEDURE:CURRENT-WINDOW = C-Win.

/***********************************************************************
**  /*   */
**  UT9000.I - Definiá∆o das vari†veis de ambiente do Magnus 97
**  {1} = programa provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

/* System Variable Definitions ---                                       */
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

/********************************************************************************
** Programa : include/i-sysvar.i
**
** Data : 02/06/1999
**
** Criaá∆o : John Cleber Jaraceski
**
** Objetivo : Definicao das System Variables
**
** Ultima Alt : ?
*******************************************************************************/



/* include/i-sysvar.i ---                                                     */

 
def var c_cod_empres_usuar as char no-undo.
def var c_nom_razao_social as char no-undo.



    /*rodar pi-rsocial persistent para verificaá∆o empresa usuario*/
    if not valid-handle(h-rsocial)
    or h-rsocial:type <> "procedure":U
    or h-rsocial:file-name <> "utp/ut-rsocial.p":U then do:
        if l-achou-prog then
            run utp/ut-rsocial.p persistent set h-rsocial.
    end.
    if l-achou-prog then
        run pi-rsocial in h-rsocial (output c_cod_empres_usuar, output c_nom_razao_social).

    assign c-programa-mg97 = caps("ESIN0801")
           c-versao-mg97   = "2.00.00.010".
    find prog_dtsul no-lock
        where prog_dtsul.cod_prog_dtsul = c-programa-mg97 no-error.
    if  avail prog_dtsul then do:
        assign c-titulo-prog-mg97    = prog_dtsul.des_prog_dtsul
               c-nom-prog-upc-mg97   = prog_dtsul.nom_prog_upc
               c-nom-prog-appc-mg97  = prog_dtsul.nom_prog_appc
               c-nom-prog-dpc-mg97   = prog_dtsul.nom_prog_dpc
               i-num-topico-hlp-mg97 = prog_dtsul.num_topico.
       
          if session:window-system <> "TTY" then 
            assign i-template          = prog_dtsul.idi_template.
       

        
        find procedimento no-lock
            where procedimento.cod_proced = prog_dtsul.cod_proced no-error.
        if  avail procedimento then do:
            find modul_dtsul no-lock
                where modul_dtsul.cod_modul_dtsul = procedimento.cod_modul_dtsul no-error. 
            if  avail modul_dtsul then do:
                assign c-modulo-mg97         = caps(modul_dtsul.nom_modul_dtsul_menu)
                       c-cod-mod-mg97        = caps(modul_dtsul.cod_modul_dtsul)
                       c-nom-manual-hlp-mg97 = "dochlp~/" + string(modul_dtsul.num_manual_documen, "999999") + ".hlp".
            end.
        end.
    end.                                                      
    else do:
        assign c-titulo-prog-mg97    = caps(c-programa-mg97)
               c-nom-prog-upc-mg97   = ""
               c-nom-prog-appc-mg97  = ""
               i-num-topico-hlp-mg97 = 0
               c-nom-manual-hlp-mg97 = "dochlp~/000000.hlp".
    end.                 
     
    
         assign C-Win:title = if l-achou-prog then
                                       c-titulo-prog-mg97
                                     + " - " 
                                     + c-programa-mg97 
                                     + " - " 
                                     + c-versao-mg97  
                                     + " - " 
                                     + c_cod_empres_usuar
                                     + " - " 
                                     + c_nom_razao_social
                                     else 
                                       c-titulo-prog-mg97
                                     + " - " 
                                     + c-programa-mg97 
                                     + " - " 
                                     + c-versao-mg97.
    
    
 if today > 03/01/1998 then do:    
 /******************************* Validaá∆o ***********************************/   

    /* Verificaá∆o do registro do produto */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfreg.p (output l-acesso-livre).
      if l-acesso-livre = no then do:
        run utp/ut-msgs.p (input "show",
                           input 8525,
                           input "").      
        apply "close" to this-procedure.
        return.
      end.    
    end.  

    /* Verificaá∆o da data de validade do contrato */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfvld.p (output d-data-contrato).
      if d-data-contrato < today then do:
        run utp/ut-msgs.p (input "show",
                           input 8536,
                           input string(d-data-contrato)).      
        apply "close" to this-procedure.
        return.
      end.
    end.  

    /* Verificaá∆o do acesso ao modulo do programa com base no contrato */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfmod.p (input c-cod-mod-mg97, output l-acesso-livre).
      if l-acesso-livre = no then do:
        run utp/ut-msgs.p (input "show",
                           input 8527,
                           input c-cod-mod-mg97).      
        apply "close" to this-procedure.
        return.
      end.  
    end.  
    
    /* Verificaá∆o de usu†rios ativos */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfusr.p (output i-user-conectados, output i-licenca-usuar).
      if i-user-conectados > i-licenca-usuar then do:
        run utp/ut-msgs.p (input "show",
                           input 8532,
                           input string(i-user-conectados) + "~~" +
                                 string(i-licenca-usuar)).      
        apply "close" to this-procedure.
        return.
      end.
    end.  

/******************************************************************************/
 end.
    
    /* Verificaá∆o da seguranáa e login informado */
    /*--------------------------------------------------------------------------
    File        : UTP/UT-VFSEC.I
    Purpose     : Verificaá∆o da Seguranáa

    Syntax      :

    Description : Verificar a seguranáa

    Author(s)   : Fabiano
    Created     : 31/12/1997
    Notes       :
------------------------------------------------------------------------*/
/* N∆o faz a validaá∆o para programas do tipo V† Para */
if index(replace(program-name(1),"~\","~/"),"go/g") = 0 then do:
  run men/men901za.p (input c-programa-mg97).
  if  return-value = "2012" then do:
      run utp/ut-msgs.p (input "show",
                         input 2858,
                         input c-programa-mg97).
      if "Window" = "SmartDialog" or this-procedure:persistent = no then
        return "adm-error".
      else do:     
        delete procedure this-procedure.
        return.
      end.  
  end.                       

  if  return-value = "2014" then do:
      run utp/ut-msgs.p (input "show",
                         input 3045,
                         input c-programa-mg97).
      if "Window" = "SmartDialog" then
        return "adm-error".
      else do:
        delete procedure this-procedure.
        return.
      end.  
  end.
end.  

/* ut-vfsec.i */
 
    
    /* Inicio do log de execuá∆o de programas */
    /*************************************************************************
**                                                                        
**  I-LOGINI.I - Inicia Log de Execuá∆o
**
*************************************************************************/

run btb/btb918zb.p (input c-programa-mg97,
                    input-output rw-log-exec,
                    input yes).

/* i-logini */

  
    
    
      if session:window-system <> "TTY" then do:
       /********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

/********************************************************************************
** Programa : include/i-trswin.i
**
** Data : 29/12/97
**
** Criaá∆o : John Cleber Jaraceski
**
** Objetivo : Realizar alteracoes em todos os programas que possuam interface 
**            com o usuario (window/dialog). 
**            Estas alteracoes sao :
**              - Centralizar Window
**              - Desabilitar MAX - RESIZE
**              - Ocultar MAX - MIN
**              - Tornar uma Window Modal
**
** Ultima Alt : 29/12/1997
*******************************************************************************/

/* Transformacao Window *****************************************************/


    /*
     * Alteraá∆o 010007
     * De acordo com o KBase Progress P96679 na vers∆o 10 as dll n∆o entendem (?)
     * e devem receber (0).
     */
    DEFINE VARIABLE i-hwnd AS INTEGER     NO-UNDO.
    
    ASSIGN i-hwnd = C-Win:HWND.
    
    IF i-hwnd EQ ? AND PROVERSION GE "10" THEN
        ASSIGN i-hwnd = 0.

    /*
     * Fim da alteraá∆o 010007,
     * No restante do programa onde se usava {&WINDOW-NAME}:HWND
     * passou-se a usar i-hwnd.
     */

    case i-template:
        when 2 then do: /* Cadastro Simples */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.

            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.
        end.
        when 3 then do: /* Cadastro Simples - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.
        end.
        when 4 then do: /* Cadastro Complexo */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 5 then do: /* Cadastro Complexo - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 6 then do: /* Cadastro Simples - Inclusao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 7 then do: /* Cadastro PaiXFilho - Atualiza Filho */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 8 then do: /* Cadastro PaiXFilho - Atualiza Ambos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
            
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 9 then do: /* Inclui/Modifica Pai */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            /* TECH14187 - FO 1466145 - Programas incorretos s∆o convertidos para MODAL */
            /*** Tornar Window Modal ***/
            
                assign h-pai = source-procedure
                       h-pai = h-pai:current-window
                       .
            
            /*Alteraá∆o - 31/05/2007 - tech1007 - FO1466145 - Modificaá∆o para que n∆o ocorram erros ao executar os programas de ZOOM
            ê importante documentar que foi encontrado um BUG e em alguns casos a funá∆o SOURSE-PROCEDURE retorna o programa errado.
            Esse BUG foi solucionado no Porgress 10.1A*/
            IF NOT VALID-HANDLE(h-pai) THEN DO:
                assign h-pai           = SESSION:FIRST-CHILD
                       h-pai           = h-pai:NEXT-SIBLING  
                    .
            END.
            IF VALID-HANDLE(h-pai) THEN DO:
                assign h-pai:sensitive = no.
            END.
            /*Fim alteraá∆o 31/05/2007*/
            /* TECH14187 - FO1466145 - FIM ALTERAÄ«O */
            
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 10 then do: /* Inclui/Modifica Filho */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            /* TECH14187 - FO 1466145 - Programas incorretos s∆o convertidos para MODAL */
            /*** Tornar Window Modal ***/
            
                assign h-pai = source-procedure
                       h-pai = h-pai:current-window
                       .
            
            /*Alteraá∆o - 31/05/2007 - tech1007 - FO1466145 - Modificaá∆o para que n∆o ocorram erros ao executar os programas de ZOOM
            ê importante documentar que foi encontrado um BUG e em alguns casos a funá∆o SOURSE-PROCEDURE retorna o programa errado.
            Esse BUG foi solucionado no Porgress 10.1A*/
            IF NOT VALID-HANDLE(h-pai) THEN DO:
                assign h-pai           = SESSION:FIRST-CHILD
                       h-pai           = h-pai:NEXT-SIBLING  
                    .
            END.
            IF VALID-HANDLE(h-pai) THEN DO:
                assign h-pai:sensitive = no.
            END.
            /*Fim alteraá∆o 31/05/2007*/
            /* TECH14187 - FO1466145 - FIM ALTERAÄ«O */
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 11 then do: /* Formacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 12 then do: /* Parametros Unicos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 13 then do: /* Pesquisa */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            /* TECH14187 - FO 1466145 - Programas incorretos s∆o convertidos para MODAL */
            /*** Tornar Window Modal ***/
            
                assign h-pai = source-procedure
                       h-pai = h-pai:current-window
                       .
            
            /*Alteraá∆o - 31/05/2007 - tech1007 - FO1466145 - Modificaá∆o para que n∆o ocorram erros ao executar os programas de ZOOM
            ê importante documentar que foi encontrado um BUG e em alguns casos a funá∆o SOURSE-PROCEDURE retorna o programa errado.
            Esse BUG foi solucionado no Porgress 10.1A*/
            IF NOT VALID-HANDLE(h-pai) THEN DO:
                assign h-pai           = SESSION:FIRST-CHILD
                       h-pai           = h-pai:NEXT-SIBLING  
                    .
            END.
            IF VALID-HANDLE(h-pai) THEN DO:
                assign h-pai:sensitive = no.
            END.
            /*Fim alteraá∆o 31/05/2007*/
            /* TECH14187 - FO1466145 - FIM ALTERAÄ«O */
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 14 then do: /* Consulta Simples */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 15 then do: /* Consulta Complexa */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 16 then do: /* Consulta Relacionamento */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 17 then do: /* Relatorio */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 20 then do: /* Janela Detalhe */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD.

            if  h-pai:handle = C-Win:handle then
                assign h-pai           = h-pai:NEXT-SIBLING
                       h-pai:SENSITIVE = no.
            else 
                assign  h-pai = C-Win:handle
                        h-pai = h-pai:parent.

            h-pai:sensitive = no.
  
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 21 then do: /* Janela Mestre */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.        
        end.
        when 26 then do: /* Importacao/Exportacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.
        end.
        when 29 then do: /* Relatorio Gerado Pelo DataViewer */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.
        end.
        when 30 then do: /* Formacao Sem Navegacao */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            /* TECH14187 - FO 1466145 - Programas incorretos s∆o convertidos para MODAL */
            /*** Tornar Window Modal ***/
            
                assign h-pai = source-procedure
                       h-pai = h-pai:current-window
                       .
            
            /*Alteraá∆o - 31/05/2007 - tech1007 - FO1466145 - Modificaá∆o para que n∆o ocorram erros ao executar os programas de ZOOM
            ê importante documentar que foi encontrado um BUG e em alguns casos a funá∆o SOURSE-PROCEDURE retorna o programa errado.
            Esse BUG foi solucionado no Porgress 10.1A*/
            IF NOT VALID-HANDLE(h-pai) THEN DO:
                assign h-pai           = SESSION:FIRST-CHILD
                       h-pai           = h-pai:NEXT-SIBLING  
                    .
            END.
            IF VALID-HANDLE(h-pai) THEN DO:
                assign h-pai:sensitive = no.
            END.
            /*Fim alteraá∆o 31/05/2007*/
            /* TECH14187 - FO1466145 - FIM ALTERAÄ«O */
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.
        end.
        when 31 then do: /* Estrutura */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            /* TECH14187 - FO 1466145 - Programas incorretos s∆o convertidos para MODAL */
            /*** Tornar Window Modal ***/
            
                assign h-pai = source-procedure
                       h-pai = h-pai:current-window
                       .
            
            /*Alteraá∆o - 31/05/2007 - tech1007 - FO1466145 - Modificaá∆o para que n∆o ocorram erros ao executar os programas de ZOOM
            ê importante documentar que foi encontrado um BUG e em alguns casos a funá∆o SOURSE-PROCEDURE retorna o programa errado.
            Esse BUG foi solucionado no Porgress 10.1A*/
            IF NOT VALID-HANDLE(h-pai) THEN DO:
                assign h-pai           = SESSION:FIRST-CHILD
                       h-pai           = h-pai:NEXT-SIBLING  
                    .
            END.
            IF VALID-HANDLE(h-pai) THEN DO:
                assign h-pai:sensitive = no.
            END.
            /*Fim alteraá∆o 31/05/2007*/
            /* TECH14187 - FO1466145 - FIM ALTERAÄ«O */
    
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.
        end.
        when 32 then do: /* Digitaá∆o Rapida */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
            
            assign C-Win:HIDDEN = yes
            C-Win:HIDDEN = no.
            apply "ENTRY":U to C-Win.
        end.
    end case.

/* Transformacao Window *****************************************************/

 
      end. 
    



/* ut9000.i */
 

/* inicializaá‰es do template de relat¢rio */
/***********************************************************************
**
**  i-RPINI.I - InicializaªÑes do Template de RelatΩrio
**
***********************************************************************/

/* Preprocessor para eliminaªío de arquivos temporˇrios */
assign C-Win:virtual-width-chars  = C-Win:width-chars
       C-Win:virtual-height-chars = C-Win:height-chars
       C-Win:min-width-chars      = C-Win:width-chars
       C-Win:max-width-chars      = C-Win:width-chars
       C-Win:min-height-chars     = C-Win:height-chars
       C-Win:max-height-chars     = C-Win:height-chars.


    assign c-arquivo:row in frame f-pg-imp    = c-arquivo:row in frame f-pg-imp - 0.06
           c-arquivo:height in frame f-pg-imp = 1.
    
    find usuar_mestre where usuar_mestre.cod_usuario = c-seg-usuario no-lock no-error.
    
    if avail usuar_mestre
        then assign c-arquivo = if length(usuar_mestre.nom_subdir_spool) <> 0
                                    then caps(replace(usuar_mestre.nom_dir_spool, " ", "~/") + "~/" + replace(usuar_mestre.nom_subdir_spool, " ", "~/") + "~/" + c-programa-mg97 + "~.lst":U)
                                    else caps(replace(usuar_mestre.nom_dir_spool, " ", "~/") + "~/" + c-programa-mg97 + "~.lst":U)
                    c-arq-old = c-arquivo.
        else assign c-arquivo = caps("spool~/":U + c-programa-mg97 + "~.lst":U)
                    c-arq-old = c-arquivo.

    run utp/ut-liter.p (input "Terminal":U, 
                        input "*",
                        input "R").
    
    assign c-terminal = return-value
                        rs-destino = 3.

    /*Alterado tech1007 - 14/02/2005 - C¢digo removido pois o template foi alterado
      para que a opá∆o de RTF n∆o seja uma opá∆o de destino
    &IF "{&RTF}":U = "YES":U &THEN
        rs-destino:ADD-LAST("RTF", 4) IN FRAME f-pg-imp.
        
        run utp/ut-liter.p (input "RTF":U, 
                            input "*",
                            input "R").
        
        assign c-rtf      = return-value.
    &ENDIF
    Fim alteracao tech1007 - 14/02/2005*/
           

/* i-rpini */
 

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/*****************************************************************
**
**  I-RPLBL.I - Cria os labels para os DumbFolder do relatΩrio
**
*******************************************************************/

def var wh-label-sel     as widget-handle no-undo.
def var wh-label-cla     as widget-handle no-undo.
def var wh-label-par     as widget-handle no-undo.
def var wh-label-dig     as widget-handle no-undo.
def var wh-label-imp     as widget-handle no-undo.
def var wh-group         as widget-handle no-undo.
def var wh-child         as widget-handle no-undo.
def var c-list-folders   as char          no-undo.
def var i-current-folder as integer       no-undo.
def var i-new-folder     as integer       no-undo.
def var c-aux            as char no-undo.
/* Alterado em 31/05/2005 - tech1139 - Alteraá‰es FO 1152.814*/
DEF VAR c-modelo-aux     AS CHAR          NO-UNDO.
DEF VAR l-rtf            AS LOGICAL  INITIAL NO NO-UNDO.
def var i-aux            as integer no-undo.


/*tech14178 carrega parametros e define bot∆o para inicializaá∆o do PDF */
/*tech868*/

RUN btb/btb920aa.p PERSISTENT SET h_pdf_controller.

RUN pi_prepare_permissions IN h_pdf_controller(INPUT c-programa-mg97).

RUN pi_define_objetos IN h_pdf_controller (INPUT FRAME f-pg-imp:HANDLE,
                                           INPUT rs-destino:HANDLE,
                                           INPUT c-arquivo:HANDLE,
                                           INPUT (bt-arquivo:ROW IN FRAME f-pg-imp  - (bt-arquivo:HEIGHT IN FRAME f-pg-imp + 0.1)),
                                           INPUT (bt-arquivo:COLUMN IN FRAME f-pg-imp)).

/* FIM PDF */








/* Alterado em 31/05/2005 - tech1139 - Alteraá‰es FO 1152.814*/
ON  CLOSE OF THIS-PROCEDURE DO:
    /*************************************************************************
**
** I-LOGFIN.I - Encerra o Log de Execuªío
**
**************************************************************************/

/*************************************************************************
**
** I-LOGFIN1.I - Encerra o Log de Execucao
**
**************************************************************************/

run btb/btb918zb.p (input c-programa-mg97,
                    input-output rw-log-exec,
                    input no).
 

/* Transformacao Window */

if session:window-system <> "TTY":U then do:
    case i-template:
        when 9 or when 10 or when 20 or when 30 or when 31 then do: 
            assign h-pai:sensitive = yes.
            apply "ENTRY":U to h-pai.
        end.
        when 13 then do:
            assign h-pai:sensitive = yes.
            apply "ENTRY":U to h-pai.
            run pi-entry-atributos-chave.
        end.
    end case.
end.  

/* Transformacao Window */
/* Eliminaªío de arquivos temporˇrios */

if i-template = 17 and avail tt-param then
  if tt-param.destino = 3 and search(tt-param.arquivo) <> ? then
     os-delete value(tt-param.arquivo). 


/* Fim da eliminaªío de arquivos temporˇrios */

/* i-logfin */
   
    RUN disable_ui.
END.

    ON "LEAVE" OF c-arquivo IN FRAME f-pg-imp DO:
        IF rs-execucao = 1 THEN
            ASSIGN c-arq-old = c-arquivo:SCREEN-VALUE.
        ELSE
            ASSIGN c-arq-old-batch = c-arquivo:SCREEN-VALUE.
    END.
    
    ON "ENTER":U OF c-arquivo IN FRAME f-pg-imp OR
       "RETURN":U OF c-arquivo IN FRAME f-pg-imp OR
       "CTRL-ENTER":U OF c-arquivo IN FRAME f-pg-imp OR
       "CTRL-J":U OF c-arquivo IN FRAME f-pg-imp OR
       "CTRL-Z":U OF c-arquivo IN FRAME f-pg-imp do:
        RETURN NO-APPLY.
    END.
    
/* tech1139 - FO 1223.694  - 02/11/2005 */
    
    
     ON "~\" OF c-arquivo IN FRAME f-pg-imp DO:
         APPLY "/" TO c-arquivo IN FRAME f-pg-imp.
         RETURN NO-APPLY.
     END.

    
/* tech1139 - FO 1223.694  - 02/11/2005 */

    /*Alterado 07/03/2005 - tech14187 - Realizado teste de preprocessador para
    verificar se o RTF est† ativo*/
   
    /*Fim alteracao 07/03/2005*/


    /* 04/03/2005 Alterado para funcionalidade de RTF sem a opcao de RTF como destino*/  
    ON "VALUE-CHANGED":U OF rs-destino IN FRAME f-pg-imp DO:
        CASE rs-destino:SCREEN-VALUE IN FRAME f-pg-imp:
            WHEN "1" THEN DO:

                /* Alterado em 31/05/2005 - tech1139 - Alteraá‰es FO 1152.814*/
                
                /* Alterado em 31/05/2005 - tech1139 - Alteraá‰es FO 1152.814*/

                ASSIGN c-arquivo                                = c-imp-old
                       c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-imp-old
                       c-arquivo:SENSITIVE IN FRAME f-pg-imp    = NO
                       c-arquivo:VISIBLE IN FRAME f-pg-imp      = YES
                       bt-arquivo:VISIBLE IN FRAME f-pg-imp     = NO
                       bt-config-impr:VISIBLE IN FRAME f-pg-imp = YES
                /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                verificar se o RTF est† ativo*/
                       
                /*Fim alteracao 17/02/2005*/
                       .
                if c-imp-old = "" then
                   run pi-impres-pad.
            END.
            WHEN "2" THEN DO:
                ASSIGN c-arquivo = IF rs-execucao = 1 
                                   THEN c-arq-old
                                   ELSE c-arq-old-batch
                       c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-arquivo
                       c-arquivo:SENSITIVE IN FRAME f-pg-imp    = YES
                       c-arquivo:VISIBLE IN FRAME f-pg-imp      = YES
                       bt-arquivo:VISIBLE IN FRAME f-pg-imp     = YES
                       bt-config-impr:VISIBLE IN FRAME f-pg-imp = NO
                       /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                         verificar se o RTF est† ativo*/
                       
                       /*Fim alteracao 17/02/2005*/
                       .

                /* Alterado em 31/05/2005 - tech1139 - Alteraá‰es FO 1152.814*/
                
                /* Alterado em 31/05/2005 - tech1139 - Alteraá‰es FO 1152.814*/
                
            END.
            WHEN "3" THEN DO:
                ASSIGN c-arquivo                                = ""
                       c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-arquivo
                       c-arquivo:VISIBLE IN FRAME f-pg-imp      = NO
                       bt-arquivo:VISIBLE IN FRAME f-pg-imp     = NO                       
                       bt-config-impr:VISIBLE IN FRAME f-pg-imp = NO
                       /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                         verificar se o RTF est† ativo*/
                       
                       /*Fim alteracao 17/02/2005*/
                       .
                /*Alterado 15/02/2005 - tech1007 - Teste para funcionar corretamente no WebEnabler*/
                
                /*Fim alteracao 15/02/2005*/                                        

                /* Alterado em 31/05/2005 - tech1139 - Alteraá‰es FO 1152.814*/
                
                /* Alterado em 31/05/2005 - tech1139 - Alteraá‰es FO 1152.814*/

            END.
        END CASE.
        
        /*Fim alteracao 04/03/2005*/
    END.


/********************************************
** HELP FRAME
********************************************/
ON HELP OF FRAME F-RELAT DO:
    /*************************************************************************
**
** AJUDA.I - Include padr∆o para chamada do Help
**
**************************************************************************/


    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).


RETURN NO-APPLY.

/* include/ajuda.i */
 
END.

ON HELP OF FRAME F-PG-SEL DO:
    /*************************************************************************
**
** AJUDA.I - Include padr∆o para chamada do Help
**
**************************************************************************/


    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).


RETURN NO-APPLY.

/* include/ajuda.i */
 
END.



ON HELP OF FRAME F-PG-PAR DO:
    /*************************************************************************
**
** AJUDA.I - Include padr∆o para chamada do Help
**
**************************************************************************/


    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).


RETURN NO-APPLY.

/* include/ajuda.i */
 
END.



ON HELP OF FRAME F-PG-IMP DO:
    /*************************************************************************
**
** AJUDA.I - Include padr∆o para chamada do Help
**
**************************************************************************/


    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).


RETURN NO-APPLY.

/* include/ajuda.i */
 
END.

/********************************************************** 
** Traduªío pˇgina 0 - frame f-relat 
**********************************************************/
do  with frame f-relat:
    
    assign wh-group = frame f-relat:handle
           wh-group = wh-group:first-child.
    do  while valid-handle(wh-group):
        assign wh-child = wh-group:first-child.
        do  while valid-handle(wh-child):
            
            case wh-child:type:
                when "RADIO-SET":U then 
                    run pi-trad-radio-set (input wh-child).
                when "FILL-IN":U then
                    run pi-trad-fill-in (input wh-child).
                when "TOGGLE-BOX":U then
                    run pi-trad-toggle-box (input wh-child).
                when "COMBO-BOX":U then
                    run pi-trad-combo-box (input wh-child).
                when "BUTTON":U then
                    run pi-trad-button (input wh-child).
                when "EDITOR":U then
                    run pi-trad-editor (input wh-child).
            end case.
            assign wh-child = wh-child:next-sibling.
        end.
        assign wh-group = wh-group:next-sibling.
    end. 
end.     
/********************************************************** 
** Traduá∆o p†gina seleá∆o - frame f-pg-sel
**********************************************************/

    run utp/ut-liter.p (input "Seleá∆o",
                        input "*",
                        input "R").
    create text wh-label-sel
        assign frame        = frame f-relat:handle
               format       = "x(09)"
               font         = 1
               screen-value = return-value
               width        = 8
               row          = 1.8
               col          = im-pg-sel:col in frame f-relat + 1.86
               visible      = yes
         
         triggers:
             on mouse-select-click
                apply "mouse-select-click":U to im-pg-sel in frame f-relat.           
         end triggers.                   
     do  with frame f-pg-sel:
         
         assign wh-group = frame f-pg-sel:handle
                wh-group = wh-group:first-child.
         do  while valid-handle(wh-group):
             assign wh-child = wh-group:first-child.
             do  while valid-handle(wh-child):
                 
                 case wh-child:type:
                    when "RADIO-SET":U then 
                        run pi-trad-radio-set (input wh-child).
                    when "FILL-IN":U then
                        run pi-trad-fill-in (input wh-child).
                    when "TOGGLE-BOX":U then
                        run pi-trad-toggle-box (input wh-child).
                    when "COMBO-BOX":U then
                        run pi-trad-combo-box (input wh-child).
                    when "BUTTON":U then
                        run pi-trad-button (input wh-child).
                    when "TEXT":U then
                        run pi-trad-text (input wh-child).
                    when "EDITOR":U then
                        run pi-trad-editor (input wh-child).
                 end case.
                 assign wh-child = wh-child:next-sibling.
             end.
             assign wh-group = wh-group:next-sibling.
         end. 
     
     end.     
         

/********************************************************** 
** Traduá∆o p†gina classificaá∆o - frame f-pg-cla
**********************************************************/

/********************************************************** 
** Traduá∆o p†gina parÉmetros - frame f-pg-par
**********************************************************/

    run utp/ut-liter.p (input "ParÉmetros",
                        input "*",
                        input "R").
    create text wh-label-par
        assign frame        = frame f-relat:handle
               format       = "x(10)"
               font         = 1
               screen-value = return-value
               width        = 11
               row          = 1.8
               col          = im-pg-par:col in frame f-relat + 1.7
               visible      = yes
         
         triggers:
             on mouse-select-click
                apply "mouse-select-click":U to im-pg-par in frame f-relat.           
         end triggers.                              
     do  with frame f-pg-par:
         
         assign wh-group = frame f-pg-par:handle
                wh-group = wh-group:first-child.
         do  while valid-handle(wh-group):
             assign wh-child = wh-group:first-child.
             do  while valid-handle(wh-child):
                 
                 case wh-child:type:
                    when "RADIO-SET":U then 
                        run pi-trad-radio-set (input wh-child).
                    when "FILL-IN":U then
                        run pi-trad-fill-in (input wh-child).
                    when "TOGGLE-BOX":U then
                        run pi-trad-toggle-box (input wh-child).
                    when "COMBO-BOX":U then
                        run pi-trad-combo-box (input wh-child).
                    when "BUTTON":U then
                        run pi-trad-button (input wh-child).
                    when "TEXT":U then
                        run pi-trad-text (input wh-child).
                    when "EDITOR":U then
                        run pi-trad-editor (input wh-child).
                 end case.
                 assign wh-child = wh-child:next-sibling.
             end.
             assign wh-group = wh-group:next-sibling.
         end. 
     
     end.     

/********************************************************** 
** Traduá∆o p†gina digitaá∆o - frame f-pg-dig
**********************************************************/

/********************************************************** 
** Traduá∆o p†gina impress∆o - frame f-pg-imp
**********************************************************/

    run utp/ut-liter.p (input "Impress∆o",
                        input "*",
                        input "R").
    create text wh-label-imp
        assign frame        = frame f-relat:handle
               format       = "x(09)"
               font         = 1
               screen-value = return-value
               width        = 10
               row          = 1.8
               col          = im-pg-imp:col in frame f-relat + 1.7
               visible      = yes
         
         triggers:
             on mouse-select-click
                apply "mouse-select-click":U to im-pg-imp in frame f-relat.           
         end triggers.                   
     do  with frame f-pg-imp:
         
         assign wh-group = frame f-pg-imp:handle
                wh-group = wh-group:first-child.
         do  while valid-handle(wh-group):
             assign wh-child = wh-group:first-child.
             do  while valid-handle(wh-child):
                 
                 case wh-child:type:
                    when "RADIO-SET":U then 
                        run pi-trad-radio-set (input wh-child).
                    when "FILL-IN":U then
                        run pi-trad-fill-in (input wh-child).
                    when "TOGGLE-BOX":U then
                        run pi-trad-toggle-box (input wh-child).
                    when "COMBO-BOX":U then
                        run pi-trad-combo-box (input wh-child).
                    when "BUTTON":U then
                        run pi-trad-button (input wh-child).
                    when "TEXT":U then
                        run pi-trad-text (input wh-child).
                    when "EDITOR":U then
                        run pi-trad-editor (input wh-child).
                 end case.
                 assign wh-child = wh-child:next-sibling.
             end.
             assign wh-group = wh-group:next-sibling.
         end. 
     
     end.     
         

/********************************************************** 
** Troca de pˇgina por CTRL-TAB e SHIFT-CTRL-TAB
**********************************************************/


    assign c-list-folders = c-list-folders + "im-pg-sel,":U.



    assign c-list-folders = c-list-folders + "im-pg-par,":U.



    assign c-list-folders = c-list-folders + "im-pg-imp":U.

if  substring(c-list-folders,length(c-list-folders)) = "," then 
    assign c-list-folders = substring(c-list-folders,1,length(c-list-folders) - 1 ).
on  CTRL-TAB,SHIFT-CTRL-TAB anywhere do:
    define variable h_handle  as handle no-undo.       
    define variable c_imagem  as character no-undo.
    define variable l_direita as logical no-undo.            
    l_direita = last-event:label = 'CTRL-TAB':U.
        
    block1:
    repeat:        
        if  l_direita then do:
            if  i-current-folder = num-entries(c-list-folders) then
                i-current-folder = 1.
            else
                i-current-folder = i-current-folder + 1.
        end.
        else do:
            if  i-current-folder = 1 then
                i-current-folder = num-entries(c-list-folders).
            else
                i-current-folder = i-current-folder - 1.
        end.
    
        assign c_imagem = entry(i-current-folder,c-list-folders)
               h_handle = frame f-relat:first-child
               h_handle = h_handle:first-child.
        do  while valid-handle(h_handle):
            if  h_handle:type = 'image':U and
                h_handle:name =  c_imagem then do:
                if  h_handle:sensitive = no then 
                    next block1.
                apply 'mouse-select-click':U to h_handle.
                leave block1.
            end.
            h_handle = h_handle:next-sibling.
        end.
    end.
end.
/********************************************************** 
** Procedure de troca de pˇgina por CTRL-TAB 
**********************************************************/
procedure pi-first-child:
        
    define input parameter wh-entry-folder as widget-handle.
    
    

    assign wh-entry-folder = wh-entry-folder:first-child
           wh-entry-folder = wh-entry-folder:first-child.
    do  while(valid-handle(wh-entry-folder)):
        if  wh-entry-folder:sensitive = yes 
        and wh-entry-folder:type <> 'rectangle':U 
        and wh-entry-folder:type <> 'image':U
        and wh-entry-folder:type <> 'browse':U then do:
            apply 'entry':U to wh-entry-folder.
            leave.
        end.
        else
            assign wh-entry-folder = wh-entry-folder:next-sibling.    
    end.
end.
/********************************************************** 
** Procedures de Traducao 
**********************************************************/
procedure pi-trad-radio-set:
   
    def input param wh-objeto    as widget-handle no-undo.
  
    assign c-aux = wh-objeto:radio-buttons.
    do  i-aux = 1 to num-entries(wh-objeto:radio-buttons):
        if  (i-aux mod 2) <> 0 then do:
            run utp/ut-liter.p (input replace(entry(i-aux, wh-objeto:radio-buttons), chr(32), "_"),
                                input "",
                                input "R"). 
            assign entry(i-aux, c-aux) = return-value.
        end.
    end.                                              
    assign wh-objeto:radio-buttons = c-aux.
    
    if  wh-objeto:help <> "" 
    and wh-objeto:help <> ? then do:
        run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:help = return-value.
    end.  
end.
procedure pi-trad-fill-in:
   
    def input param wh-objeto    as widget-handle no-undo.
    
        if  wh-objeto:label <> ?
        and wh-objeto:label <> "" then do:
            run utp/ut-liter.p (input replace(wh-objeto:label, chr(32), "_"),
                                input "",
                                input "L"). 
            assign wh-objeto:label = return-value.
        end. 
        if  wh-objeto:help <> "" 
        and wh-objeto:help <> ? then do:
            run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                                input "",
                                input "R"). 
            assign wh-objeto:help = return-value.
        end.         
    
end.
procedure pi-trad-editor:
    def input param wh-objeto    as widget-handle no-undo.
    
         /* editor nío tem label, entío traduz apenas o help */
        if  wh-objeto:help <> "" 
        and wh-objeto:help <> ? then do:
            run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                                input "",
                                input "R"). 
            assign wh-objeto:help = return-value.
        end.         
end.
procedure pi-trad-toggle-box:
   
    def input param wh-objeto    as widget-handle no-undo.
    
    if  wh-objeto:label <> ?
    and wh-objeto:label <> "" then do:
        run utp/ut-liter.p (input replace(wh-objeto:label, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:label = return-value.
    end. 
    if  wh-objeto:help <> "" 
    and wh-objeto:help <> ? then do:
        run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:help = return-value.
    end.         
    
end.
procedure pi-trad-combo-box:
                        /* nota: nío traduz conteúdo */
    
    def input param wh-objeto    as widget-handle no-undo.
    
    if  wh-objeto:label <> ?
    and wh-objeto:label <> "" then do:
        run utp/ut-liter.p (input replace(wh-objeto:label, chr(32), "_"),
                            input "",
                            input "L"). 
        assign wh-objeto:label = return-value.
    end. 
    if  wh-objeto:help <> "" 
    and wh-objeto:help <> ? then do:
        run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:help = return-value.
    end.         
    
end.
procedure pi-trad-button:
    
    def input param wh-objeto    as widget-handle no-undo.
    
    if  wh-objeto:label <> ?
    and wh-objeto:label <> "" then do:
        run utp/ut-liter.p (input replace(wh-objeto:label, chr(32), "_"),
                            input "",
                            input "C"). 
        assign wh-objeto:label = return-value.
    end. 
    if  wh-objeto:help <> "" 
    and wh-objeto:help <> ? then do:
        run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:help    = return-value
               wh-objeto:tooltip = trim(return-value).
    end.         
    
end.
procedure pi-trad-text:
    
    def input param wh-objeto    as widget-handle no-undo.
    
    if  wh-objeto:screen-value <> ?
    and wh-objeto:screen-value <> "" then do:
        run utp/ut-liter.p (input replace(wh-objeto:screen-value, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:screen-value = return-value.
               wh-objeto:width = length(return-value).
    end.
    else do:
        if  wh-objeto:private-data <> ?
        and wh-objeto:private-data <> "" then do:
            run utp/ut-liter.p (input replace(wh-objeto:private-data, chr(32), "_"),
                                input "",
                                input "R"). 
            assign wh-objeto:screen-value = " " + return-value.
                   wh-objeto:width = length(return-value) + 1.
        end.
    
    end.
    
end.
procedure pi-trad-browse:
    
    def input param wh-objeto    as widget-handle no-undo.
    def var wh-column            as widget-handle no-undo.
    if  wh-objeto:help <> "" 
    and wh-objeto:help <> ? then do:
        run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:help = return-value.
    end.         
    if  wh-objeto:title <> "" 
    and wh-objeto:title <> ? then do:
        run utp/ut-liter.p (input replace(wh-objeto:title, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:title = return-value.
    end.         
    assign wh-column = wh-objeto:first-column.
    do  while wh-column <> ?:
        run utp/ut-liter.p (input replace(wh-column:label, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-column:label = return-value.
        assign wh-column = wh-column:next-column.
    end.
end.
/* i-rplbl */

procedure pi-impres-pad:
do with frame f-pg-imp:
    find layout_impres_padr no-lock
         where layout_impres_padr.cod_usuario = c-seg-usuario
            and layout_impres_padr.cod_proced = c-programa-mg97  use-index lytmprsp_id  no-error. /*cl_default_procedure_user of layout_impres_padr*/
    if  not avail layout_impres_padr
    then do:
        find layout_impres_padr no-lock
             where layout_impres_padr.cod_usuario = "*"
               and layout_impres_padr.cod_proced = c-programa-mg97  use-index lytmprsp_id  no-error. /*cl_default_procedure of layout_impres_padr*/
        if  avail layout_impres_padr
        then do:
            find imprsor_usuar no-lock
                 where imprsor_usuar.nom_impressora = layout_impres_padr.nom_impressora
                   and imprsor_usuar.cod_usuario = string(c-seg-usuario)
                 use-index imprsrsr_id  no-error. /*cl_layout_current_user of imprsor_usuar*/
        end . /* if */
        if  not avail imprsor_usuar
            or not avail layout_impres_padr /* Por Thiago Garcia ref. FO 901.132 */
        then do:
            find layout_impres_padr no-lock
                 where layout_impres_padr.cod_usuario = c-seg-usuario
                   and layout_impres_padr.cod_proced = "*"
                 use-index lytmprsp_id  no-error. /*cl_default_user of layout_impres_padr*/
        end . /* if */
    end . /* if */
    if  avail layout_impres_padr
    then do:
        assign c-arquivo:screen-value in frame f-pg-imp = layout_impres_padr.nom_impressora
                                    + ":"
                                    + layout_impres_padr.cod_layout_impres.
    end . /* if */
    else do:
         c-arquivo:screen-value in frame f-pg-imp = "".
    end . /* else */
end . /* do dflt */
end.
/*pi-impres-pad */
/*Alterado 16/02/2005 - tech1007 - Procedure criada para controlar os widgets da
  funcionalidade de RTF*/

/*Fim alteracao 16/02/2005*/

/* define procedure externa para execucao do programa de visualizacao do relatorio */

PROCEDURE WinExec EXTERNAL "kernel32.dll":U:
  DEF INPUT  PARAM prg_name                          AS CHARACTER.
  DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE.

 

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.



/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.

   .
  
    /*****************************************************************
**
**  I-RPMBL.I - Main Block do Template de RelatΩrio
**  {1} =  Par≥metro Opcional, indica a imagem que deve ser aplicado
**         o "mouse-select-click" 
*******************************************************************/
 
apply "value-changed":U to rs-destino in frame f-pg-imp.

    apply "mouse-select-click":U to im-pg-sel in frame f-relat.


 VIEW C-Win. /*View na window do relat¢rio*/                                                                          
 APPLY "ENTRY":U TO FRAME f-relat. /*Transferindo focus para a frame principal do relat¢rio*/                         
 APPLY "ENTRY":U TO C-Win. /*Transferindo focus para janela afim de evitar a vinda do men£ para a frente da janela*/  


/*Traduá∆o dos objetos de tela*/

/*fim traduá∆o dos objetos de tela*/
/*Alteracao tech14207*/
    run pi-first-child (input frame f-pg-sel:handle).

/*tech 14207*/
/* i-rpmbl */
 
  
    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */


PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  /* row-head.i - */
  DEFINE VARIABLE tbl-list           AS CHARACTER INIT "":U NO-UNDO.
  DEFINE VARIABLE rowid-list         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE row-avail-cntr     AS INTEGER INIT 0 NO-UNDO.
  DEFINE VARIABLE row-avail-rowid    AS ROWID NO-UNDO.
  DEFINE VARIABLE row-avail-enabled  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE link-handle        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE record-source-hdl  AS HANDLE NO-UNDO.
  DEFINE VARIABLE different-row      AS LOGICAL NO-UNDO INIT no.
  DEFINE VARIABLE key-name           AS CHARACTER INIT ? NO-UNDO.
  DEFINE VARIABLE key-value          AS CHARACTER INIT ? NO-UNDO.
 
  /* Check that the previous record hasn't been modifed. */
  RUN check-modified IN THIS-PROCEDURE ('check':U) NO-ERROR.
  
  /* If nothing's been modified but we're in an update, then the record
     we're getting is the same one we're just finishing up with
     (update-complete state after an Add, for instance). So ignore it. */
  IF adm-updating-record THEN RETURN.

  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = IF RETURN-VALUE = "YES":U THEN yes ELSE no.  
  
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'RECORD-SOURCE':U,
      OUTPUT link-handle) NO-ERROR.
  IF link-handle = "":U THEN     /* There's no active record source */
      RETURN.
  ASSIGN record-source-hdl = WIDGET-HANDLE(ENTRY(1,link-handle)).
  IF NUM-ENTRIES(link-handle) > 1 THEN  /* A list indicates multiple sources */
      MESSAGE "row-available in ":U THIS-PROCEDURE:FILE-NAME 
          "encountered more than one RECORD-SOURCE.":U SKIP
          "The first - ":U record-source-hdl:file-name " - will be used.":U
             VIEW-AS ALERT-BOX ERROR.
  
  /* Get the key needed by this Record-Target. */         
  RUN get-attribute ('Key-Name':U).
  key-name = RETURN-VALUE.
  IF key-name NE ? THEN DO:
    RUN send-key IN record-source-hdl (INPUT key-name, OUTPUT key-value)
      NO-ERROR.
    IF key-value NE ? THEN  /* At design time this won't succeed, so skip it. */
      RUN set-attribute-list (SUBSTITUTE ('Key-Value="&1"':U, key-value)).
  END.
 

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  /* row-end.i */
IF VALID-HANDLE (adm-object-hdl) THEN  /* If there's a Frame, etc. then */
    RUN dispatch IN THIS-PROCEDURE ('display-fields':U). /* display the fields*/
/* Note: open-query does its own notify of row-available */
RUN notify IN THIS-PROCEDURE ('row-available':U).


 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW C-Win.
  
  DISPLAY l-item l-descricao i-moeda l-aprop l-autor l-DivMil l-duplic 
          l-ord-prod l-mob l-sem-planej l-vl-menor l-vl-maior de-vl-tenden 
          FILL-IN-8 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE l-item RECT-10 l-descricao i-moeda l-aprop l-autor l-DivMil l-duplic 
         l-ord-prod l-mob l-sem-planej l-vl-menor l-vl-maior de-vl-tenden 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  
  DISPLAY i-empresa c-est-exec i-projeto c-sigla d-data-ini d-data-fim i-ord-ini 
          i-ord-fim i-sec-ini i-sec-fim i-esp-ini i-esp-fim i-sub-ini i-sub-fim 
          i-ori-ini i-ori-fim c-item-ini c-item-fim 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-57 IMAGE-58 IMAGE-59 IMAGE-60 IMAGE-61 IMAGE-62 
         IMAGE-63 IMAGE-64 IMAGE-65 IMAGE-66 IMAGE-67 IMAGE-68 i-empresa 
         c-est-exec i-projeto c-sigla d-data-ini d-data-fim i-ord-ini i-ord-fim 
         i-sec-ini i-sec-fim i-esp-ini i-esp-fim i-sub-ini i-sub-fim i-ori-ini 
         i-ori-fim c-item-ini c-item-fim 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN i-empresa:SCREEN-VALUE IN FRAME f-pg-sel = i-ep-codigo-usuario.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

do  on error undo, return error
    on stop  undo, return error:     

      /***************************************************************
**
** I-RPEXA.I - Saida A na PI-EXECUTAR do template de relatΩrio
**
***************************************************************/

def var c-arq-aux    as char no-undo.

if  input frame f-pg-imp rs-destino  = 2 
and input frame f-pg-imp rs-execucao = 1 then do:
    
    assign c-arq-aux = input frame f-pg-imp c-arquivo
           c-arq-aux = replace(c-arq-aux, "/", "~\").
    if  r-index(c-arq-aux, "~\") > 0 then do:
        assign file-info:file-name = substring(c-arq-aux,1,r-index(c-arq-aux, "~\")).
        if  file-info:full-pathname = ? or not file-info:file-type matches "*D*" then do:
            run utp/ut-msgs.p (input "show":U, 
                               input 5749, 
                               input "").
            apply 'mouse-select-click':U to im-pg-imp in frame f-relat.
            apply 'entry':U to c-arquivo in frame f-pg-imp.                   
            return error.
        end.
    end.
    
    assign file-info:file-name = c-arq-aux.
    if file-info:file-type matches "*D*" then do:
        run utp/ut-msgs.p (input "show":U, input 73, input "").
        apply 'mouse-select-click':U to im-pg-imp in frame f-relat.
        apply 'entry':U to c-arquivo in frame f-pg-imp.                   
        return error.
    end.
end.    

/* i-rpexa */
 

   
    if  input frame f-pg-imp rs-destino = 2 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        if  return-value = "nok" then do:
            run utp/ut-msgs.p (input "show",
                               input 73,
                               input "").
            apply 'mouse-select-click' to im-pg-imp in frame f-relat.
            apply 'entry' to c-arquivo in frame f-pg-imp.                   
            return error.
        end.
    end.
   


   if input frame f-pg-sel i-empresa = 0 then do:
        /*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "base-mensal":U, 
                    input "ep-codigo":U, 
                    input integer("1":U)).

/* ut-field.i */
 
        run utp/ut-msgs.p (input "show",
                           input 2333,
                           input return-value).
            apply 'mouse-select-click' to im-pg-sel in frame f-relat.
            apply 'entry' to i-empresa in frame f-pg-sel.             
            return error.
    end.

    if input frame f-pg-sel c-est-exec = "" then do:
        /*************************************************************
**
**  UT-FIELD.I - Chamada Padr∆o para UT-FIELD.P que retornar†
**               as propriedades do campo.
**                                        
*************************************************************/

run utp/ut-field.p (input "mginv":U, 
                    input "base-mensal":U, 
                    input "cod-est-exec":U, 
                    input integer("1":U)).

/* ut-field.i */
 
        run utp/ut-msgs.p (input "show",
                           input 164,
                           input return-value).
            apply 'mouse-select-click' to im-pg-sel in frame f-relat.
            apply 'entry' to c-est-exec in frame f-pg-sel.             
            return error.
    end.    
    
    if input frame f-pg-sel i-ord-ini > input frame f-pg-sel i-ord-fim then do:
            run utp/ut-msgs.p (input "show",
                               input 509,
                               input string(input i-ord-ini) + "~~" +
                                     string(input i-ord-fim)).
            apply 'mouse-select-click' to im-pg-sel in frame f-relat.
            apply 'entry' to i-ord-ini in frame f-pg-sel.             
            return error.
    end.
    if input frame f-pg-sel i-projeto = 0 then do:
       find first proj-inv where 
           proj-inv.ep-codigo = input frame f-pg-sel i-empresa and
           proj-inv.cod-est-exec = input frame f-pg-sel c-est-exec and
           proj-inv.sigla = input frame f-pg-sel c-sigla no-lock no-error.
       if not avail proj-inv then do:
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
 
          run utp/ut-msgs.p (input "show",
                               input 56,
                               input return-value).
            apply 'mouse-select-click' to im-pg-sel in frame f-relat.
            apply 'entry' to c-sigla in frame f-pg-sel.             
            return error.
       end.
       assign i-projeto:screen-value = string(proj-inv.num-projeto).
       assign i-projeto = proj-inv.num-projeto.
     end.   
     else do:
       find first proj-inv where 
            proj-inv.ep-codigo = input frame f-pg-sel i-empresa and
            proj-inv.cod-est-exec = input frame f-pg-sel c-est-exec and
            proj-inv.num-projeto = input frame f-pg-sel i-projeto no-lock no-error.
            if not avail proj-inv then do:
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
 
                run utp/ut-msgs.p (input "show",
                                   input 47,
                                   input return-value).
                apply 'mouse-select-click' to im-pg-sel in frame f-relat.
                apply 'entry' to i-projeto in frame f-pg-sel.             
                return error.
            end.
            assign c-sigla:screen-value = proj-inv.sigla.
            assign c-sigla = proj-inv.sigla.
       end.   
    if input frame f-pg-sel i-sec-ini > input frame f-pg-sel i-sec-fim then do:
            run utp/ut-msgs.p (input "show",
                               input 887,
                               input "").
            apply 'mouse-select-click' to im-pg-sel in frame f-relat.
            apply 'entry' to i-sec-ini in frame f-pg-sel.             
            return error.
    end.
    
    if input frame f-pg-sel i-esp-ini > input frame f-pg-sel i-esp-fim then do:
            run utp/ut-msgs.p (input "show",
                               input 6390,
                               input string(input i-esp-ini) + "~~" +
                                     string(input i-esp-fim)).
            apply 'mouse-select-click' to im-pg-sel in frame f-relat.
            apply 'entry' to i-esp-ini in frame f-pg-sel.             
            return error.
    end. 
    
    if input frame f-pg-sel i-sub-ini > input frame f-pg-sel i-sub-fim then do:
            run utp/ut-msgs.p (input "show",
                               input 6391,
                               input string(input i-sub-ini) + "~~" +
                                     string(input i-sub-fim)).
            apply 'mouse-select-click' to im-pg-sel in frame f-relat.
            apply 'entry' to i-sub-ini in frame f-pg-sel.             
            return error.
    end.
    
    if input frame f-pg-sel i-ori-ini > input frame f-pg-sel i-ori-fim then do:
            run utp/ut-msgs.p (input "show",
                               input 6392,
                               input string(input i-ori-ini) + "~~" +
                                     string(input i-ori-fim)).
            apply 'mouse-select-click' to im-pg-sel in frame f-relat.
            apply 'entry' to i-ori-ini in frame f-pg-sel.             
            return error.
    end.
    
    if input frame f-pg-sel c-item-ini > input frame f-pg-sel c-item-fim then do:
            run utp/ut-msgs.p (input "show",
                               input 52,
                               (input c-item-ini) + "~~" +
                                input c-item-fim).
            apply 'mouse-select-click' to im-pg-sel in frame f-relat.
            apply 'entry' to c-item-ini in frame f-pg-sel.             
            return error.
    end.              
    
        
    /* Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p†gina 
       com problemas e colocar o focus no campo com problemas             */    
         
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time 
           tt-param.i-empresa       = input frame f-pg-sel i-empresa
           tt-param.c-est-exec      = input frame f-pg-sel c-est-exec
           tt-param.i-projeto       = input frame f-pg-sel i-projeto
           tt-param.c-sigla         = input frame f-pg-sel c-sigla
           tt-param.d-data-ini      = input frame f-pg-sel d-data-ini
           tt-param.d-data-fim      = input frame f-pg-sel d-data-fim
           tt-param.i-ord-ini       = input frame f-pg-sel i-ord-ini
           tt-param.i-ord-fim       = input frame f-pg-sel i-ord-fim
           tt-param.i-sec-ini       = input frame f-pg-sel i-sec-ini
           tt-param.i-sec-fim       = input frame f-pg-sel i-sec-fim
           tt-param.i-esp-ini       = input frame f-pg-sel i-esp-ini
           tt-param.i-esp-fim       = input frame f-pg-sel i-esp-fim
           tt-param.i-sub-ini       = input frame f-pg-sel i-sub-ini
           tt-param.i-sub-fim       = input frame f-pg-sel i-sub-fim
           tt-param.i-ori-ini       = input frame f-pg-sel i-ori-ini
           tt-param.i-ori-fim       = input frame f-pg-sel i-ori-fim
           tt-param.c-item-ini      = input frame f-pg-sel c-item-ini
           tt-param.c-item-fim      = input frame f-pg-sel c-item-fim
           tt-param.l-item          = input frame f-pg-par l-item
           tt-param.l-descricao     = input frame f-pg-par l-descricao
           tt-param.l-aprop         = input frame f-pg-par l-aprop
           tt-param.l-autor         = input frame f-pg-par l-autor
           tt-param.l-mob           = input frame f-pg-par l-mob
           tt-param.l-duplic        = input frame f-pg-par l-duplic
           tt-param.l-ord-prod      = input frame f-pg-par l-ord-prod
           tt-param.l-sem-planej    = input frame f-pg-par l-sem-planej           
           tt-param.l-vl-menor      = input frame f-pg-par l-vl-menor
           tt-param.l-vl-maior      = input frame f-pg-par l-vl-maior
           tt-param.de-vl-tenden    = input frame f-pg-par de-vl-tenden
           tt-param.i-moeda         = input frame f-pg-par i-moeda
           tt-param.moeda           = input frame f-pg-par i-moeda
           tt-param.c-desc-moeda    = entry((tt-param.moeda - 1) * 2 + 1,
                                            i-moeda:radio-buttons in frame f-pg-par)
          
              tt-param.l-divmil     = input frame f-pg-par l-DivMil
          
           .
           
    if  tt-param.destino = 1 then           
        assign tt-param.arquivo = "".
    else
    if  tt-param.destino = 2 then 
        assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    else
        assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

    /* Coloque aqui a l¢gica de gravaá∆o dos parÉmtros e seleá∆o na temp-table
       tt-param */ 

    /***************************************************************
**
** I-RPEXB.I - Saida B na PI-EXECUTAR do template de relatΩrio
**
***************************************************************/

def var c-impressora as char no-undo.
def var c-layout as char no-undo.
if  tt-param.destino = 1 then do:
  assign tt-param.arquivo = c-arquivo:screen-value in frame f-pg-imp
         c-arquivo = c-arquivo:screen-value in frame f-pg-imp.
  if num-entries(tt-param.arquivo,":") = 2 then do:
    assign c-impressora = substring(c-arquivo:screen-value in frame f-pg-imp,1,index(c-arquivo:screen-value in frame f-pg-imp,":") - 1).
    assign c-layout = substring(c-arquivo:screen-value in frame f-pg-imp,index(c-arquivo:screen-value in frame f-pg-imp,":") + 1,length(c-arquivo:screen-value in frame f-pg-imp) - index(c-arquivo:screen-value in frame f-pg-imp,":")). 
    find imprsor_usuar no-lock
         where imprsor_usuar.nom_impressora = c-impressora
         and imprsor_usuar.cod_usuario = c-seg-usuario
         use-index imprsrsr_id no-error.
    if not avail imprsor_usuar then do:
      run utp/ut-msgs.p (input "show":U, input 4306, input c-seg-usuario + "~~O usu†rio '" + c-seg-usuario + "' n∆o possu° nenhuma impressora associada"). /* Daniel Kasemodel - 13/09/2006 - ALterado o n£mero da mensagem: 4306 -> 32169, Inserido o parÉmetro "~~" */
      return error.
    end.       
    find layout_impres no-lock
         where layout_impres.nom_impressora = c-impressora
         and layout_impres.cod_layout_impres = c-layout no-error.
    if not avail layout_impres then do:
      run utp/ut-msgs.p (input "show":U, input 4306, input c-seg-usuario + "~~A impressora nao possu° nenhum tipo de layout cadastrado"). /* Daniel Kasemodel - 13/09/2006 - Inserido o parÉmetro "~~" */
      return error.
    end.       
  end.  
  else do:
    if num-entries(c-arquivo,":") < 2 then do:
      run utp/ut-msgs.p (input "show":U, input 4306, input c-seg-usuario + "~~A impressora n∆o existe"). /* Daniel Kasemodel - 13/09/2006 - ALterado o n£mero da mensagem: 4306 -> 32169, Inserido o parÉmetro "~~" */
      return error.
    end.
    assign tt-param.arquivo = c-arquivo:screen-value in frame f-pg-imp.
    assign c-impressora = entry(1,c-arquivo,":").
    assign c-layout = entry(2,c-arquivo,":"). 
    find imprsor_usuar no-lock
         where imprsor_usuar.nom_impressora = c-impressora
         and imprsor_usuar.cod_usuario = c-seg-usuario
         use-index imprsrsr_id no-error.
    if not avail imprsor_usuar then do:
      run utp/ut-msgs.p (input "show":U, input 4306, input c-seg-usuario + "~~O usu†rio '" + c-seg-usuario + "' n∆o possu° nenhuma impressora associada"). /* Daniel Kasemodel - 13/09/2006 - ALterado o n£mero da mensagem: 4306 -> 32169, Inserido o parÉmetro "~~" */
      return error.
    end.       
    find layout_impres no-lock
         where layout_impres.nom_impressora = c-impressora
         and layout_impres.cod_layout_impres = c-layout no-error.
    if not avail layout_impres then do:
      run utp/ut-msgs.p (input "show", input 4306, input c-seg-usuario + "~~O layout informado n∆o existe"). /* Daniel Kasemodel - 13/09/2006 - ALterado o n£mero da mensagem: 4306 -> 32169, Inserido o parÉmetro "~~" */
      return error.
    end.       
  end.
end.  
/* i-rpexb */
 

    if  session:set-wait-state("general") then.

    /*****************************************************************
**
** I-RPRUN.I - Roda o programa RP do relatΩrio
** {1} = Nome do programa no formato xxp/xx9999.rp.p
*****************************************************************/
def var i-num-ped-exec-rpw as integer no-undo.
  
/*tech1478 procedimentos PDF */
/*tech868*/
   
   
   IF tt-param.destino = 1 AND usePDF() AND NOT allowPrint() THEN DO: /* so deixa imprimir pdf se relatorio estiver parametrizado para tal */
       run utp/ut-msgs.p (input "show",
                          input 32552,
                          input "").
       RETURN ERROR.
   END.
   /* n∆o deixa usar pdf e rtf ao mesmo tempo */
    
        
    DEFINE VARIABLE c-arquivo AS CHAR NO-UNDO.
    DEFINE VARIABLE c-sequencial-pdf AS CHAR NO-UNDO INITIAL "0":U.
    
    /* tratamento para permitir que relatorios sejam executados com destino terminal ao mesmo tempo TEM QUE SER ANTES DE GUARDAR*/
    IF usePDF() AND tt-param.destino = 3 THEN
        ASSIGN tt-param.arquivo = tt-param.arquivo + STRING(TIME * RANDOM(1,10000)) + ".tmp":U.



    ASSIGN c-arquivo = tt-param.arquivo. /* a formaá∆o de parametros em conjunto com o nome de arquivo s¢ Ç necess†ria para a raw-param, o btb911zb.p n∆o recebe a formaá∆o de parametros pdf */
    


    IF usePDF() THEN DO:
        IF rs-execucao:screen-value in frame f-pg-imp = "2" AND tt-param.destino = 2 THEN
            RUN pi_choose_file_config IN h_pdf_controller(OUTPUT c-sequencial-pdf).
        ASSIGN tt-param.arquivo = tt-param.arquivo + "|":U + c-programa-mg97 + "|":U + c-sequencial-pdf.
    END.


/*tech14178 fim procedimentos pdf*/

raw-transfer tt-param    to raw-param.


if rs-execucao:screen-value in frame f-pg-imp = "2" then do:
  run btb/btb911zb.p (input c-programa-mg97,
                      input "inp/spp/esin0801rp.p",
                      input c-versao-mg97,
                      input 97,
                      input c-arquivo,
                      input tt-param.destino,
                      input raw-param,
                      input table tt-raw-digita,
                      output i-num-ped-exec-rpw).
  if i-num-ped-exec-rpw <> 0 then                     
    run utp/ut-msgs.p (input "show":U, input 4169, input string(i-num-ped-exec-rpw)).                      
end.                      
else do:                                         
  run inp/spp/esin0801rp.p (input raw-param,
           input table tt-raw-digita).
end.


/*tech868*/
    IF usePDF() THEN
        ASSIGN tt-param.arquivo = c-arquivo.


/* i-rprun.i */
 

    /***************************************************************
**
** I-RPEXC.I - Saida C na PI-EXECUTAR do template de relat¢rio
**
***************************************************************/ 


/* i-rpexc */
 

    if  session:set-wait-state("") then.
    
    /**************************************************************************
**
** I-RPTRM - Realiza o destino de impress∆o Terminal
**
***************************************************************************/
DEF VAR cEditor     AS CHAR.
DEF VAR vLog        AS LOGICAL.
DEF VAR vLogNotepad AS LOGICAL INITIAL YES. /* pdf */


/*Alterado 15/02/2005 - tech1007 - Alterado teste para que n∆o seja aberto o arquivo
  LST quando for gerado o arquivo RTF*/
IF tt-param.destino = 3 THEN DO:
     /* RTF */
     
     
     /*tech868*/
     IF usePDF() THEN
         ASSIGN vLogNotepad = NO.
     
     
     IF vLogNotepad = YES THEN DO:
         GET-KEY-VALUE SECTION "Datasul_EMS2":U KEY "Show-Report-Program":U VALUE cEditor.

         IF  SEARCH(cEditor) = ? THEN DO:
             ASSIGN  cEditor = OS-GETENV("windir") + "~\notepad.exe"
                     vLog    = YES.
             IF  SEARCH(cEditor) = ? THEN DO:
                 ASSIGN  cEditor = OS-GETENV("windir") + "~\write.exe".
                 IF  SEARCH(cEditor) = ? THEN DO:
                     RUN utp/ut-msgs.p (INPUT "show",
                                        INPUT 27576,
                                        INPUT tt-param.arquivo).
                     ASSIGN  vLog    = NO.
                 END.
             END.
         END.

         RUN winexec (INPUT cEditor + CHR(32) + tt-param.arquivo, INPUT 1).
     END.
        
     /*tech868*/
     /*tech1478 abre acrobat reader pro cara visualizar o arquivo */
         IF usePDF() THEN DO:
             RUN pi_call_adobe IN h_pdf_controller (INPUT tt-param.arquivo).
         END.
     
    
END.


/* Alterado 25/05/2005 - tech14207 - Alterado para corrigir o erro 142 gerado quando se utiliza a opá∆o RTF com o cursor no editor
do browser br-digita*/

IF tt-param.destino = 3 THEN DO:
   
END.

/*Fim alteracao 15/02/2005*/
/* i-rptrm */
 
    
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P†gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*****************************************************************
**
** I-RPTRP.I - Troca de P†gina no Template de Relat¢rio
**
******************************************************************/
self:move-to-top() in frame f-relat.

case self:name:
    when "im-pg-sel" then do with frame f-relat:
        
            view frame f-pg-sel.
            
            /* Facelift */
            

            run pi-first-child (input frame f-pg-sel:handle).
            im-pg-sel:load-image("image/im-fldup":U) .
            assign im-pg-sel:height = 1.20
                   im-pg-sel:row    = 1.50.
        
        
        
            hide frame f-pg-par.

            /* Facelift */
            

            im-pg-par:load-image("image/im-flddn":U) .
            im-pg-par:move-to-bottom() .
            assign im-pg-par:height = 1
                   im-pg-par:row    = 1.6.
        
        
        
            hide frame f-pg-imp.
              
            /* Facelift */
            

            im-pg-imp:load-image("image/im-flddn":U) .
            im-pg-imp:move-to-bottom() .
            assign im-pg-imp:height = 1
                   im-pg-imp:row    = 1.6.
        
    end.
    when "im-pg-cla" then do with frame f-relat:
        
            hide frame f-pg-sel.
              
            /* Facelift */
            

            im-pg-sel:load-image("image/im-flddn":U) .
            im-pg-sel:move-to-bottom() .
            assign im-pg-sel:height = 1
                   im-pg-sel:row    = 1.6.
        
        
        
            hide frame f-pg-par.
              
            /* Facelift */
            
              
            im-pg-par:load-image("image/im-flddn":U) .
            im-pg-par:move-to-bottom() .
            assign im-pg-par:height = 1
                   im-pg-par:row    = 1.6.
        
        
        
            hide frame f-pg-imp.

            /* Facelift */
            

            im-pg-imp:load-image("image/im-flddn":U) .
            im-pg-imp:move-to-bottom() .
            assign im-pg-imp:height = 1
                   im-pg-imp:row    = 1.6.
        
    end.
    when "im-pg-par" then do with frame f-relat:
        
            hide frame f-pg-sel.

            /* Facelift */
            

            im-pg-sel:load-image("image/im-flddn":U) .
            im-pg-sel:move-to-bottom() .
            assign im-pg-sel:height = 1
                   im-pg-sel:row    = 1.6.
        
        
        
            view frame f-pg-par.
            
            /* Facelift */
            

            run pi-first-child (input frame f-pg-par:handle).
              im-pg-par:load-image("image/im-fldup":U) .
            assign im-pg-par:height = 1.20
                   im-pg-par:row    = 1.5.
        
        
        
            hide frame f-pg-imp.

            /* Facelift */
            

            im-pg-imp:load-image("image/im-flddn":U) .
            im-pg-imp:move-to-bottom() .
            assign im-pg-imp:height = 1
                   im-pg-imp:row    = 1.6.
        
    end.
    when "im-pg-dig" then do with frame f-relat:
        
            hide frame f-pg-sel.

            /* Facelift */
            

            im-pg-sel:load-image("image/im-flddn":U) .
            im-pg-sel:move-to-bottom() .
            assign im-pg-sel:height = 1
                   im-pg-sel:row    = 1.6.
        
        
        
            hide frame f-pg-par.
            
            /* Facelift */
            

            im-pg-par:load-image("image/im-flddn":U) .
            im-pg-par:move-to-bottom() .
            assign im-pg-par:height = 1
                   im-pg-par:row    = 1.6.
        
        
        
            hide frame f-pg-imp.
            
            /* Facelift */
            

            im-pg-imp:load-image("image/im-flddn":U) .
            im-pg-imp:move-to-bottom() .
            assign im-pg-imp:height = 1
                   im-pg-imp:row    = 1.6.
        
    end.
    when "im-pg-imp" then do with frame f-relat:
        
            hide frame f-pg-sel.
            
            /* Facelift */
            
            
            im-pg-sel:load-image("image/im-flddn":U) .
            im-pg-sel:move-to-bottom() .
            assign im-pg-sel:height = 1
                   im-pg-sel:row    = 1.6.
        
        
        
            hide frame f-pg-par.

            /* Facelift */
            

            im-pg-par:load-image("image/im-flddn":U) .
            im-pg-par:move-to-bottom() .
            assign im-pg-par:height = 1
                   im-pg-par:row    = 1.6.
        
        
        
            view frame f-pg-imp.
            
            /* Facelift */
            

            run pi-first-child (input frame f-pg-imp:handle).
              im-pg-imp:load-image("image/im-fldup":U) .
            assign im-pg-imp:height = 1.20
                   im-pg-imp:row    = 1.5.
        
    end.
end case.
assign i-current-folder = lookup(self:name,c-list-folders,",").
  /***************************************************************
**
** I-EPC014.I - EPC para Evento CHANGE-PAGE de Container 
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.


    
    
        
            if  entry(i-current-folder,c-list-folders,",") = "im-pg-sel":U then do:
                 /* DPC */
                if  c-nom-prog-dpc-mg97 <> "" and
                    c-nom-prog-dpc-mg97 <> ? then do:                  
                    run value(c-nom-prog-dpc-mg97) (input "CHANGE-PAGE":U, 
                                                    input "CONTAINER":U,
                                                    input frame f-pg-sel:handle ,
                                                    input frame f-pg-sel:handle,
                                                    input "",
                    
                                                    input ?).
                    
                end.
                 /* APPC */
                if  c-nom-prog-appc-mg97 <> "" and
                    c-nom-prog-appc-mg97 <> ? then do:           
                    run value(c-nom-prog-appc-mg97) (input "CHANGE-PAGE":U, 
                                                     input "CONTAINER":U,
                                                     input frame f-pg-sel:handle ,
                                                     input frame f-pg-sel:handle,
                                                     input "",
                    
                                                     input ?).
                    
                end.
                 /* UPC */
                if  c-nom-prog-upc-mg97 <> "" and
                    c-nom-prog-upc-mg97 <> ? then do:                  
                    run value(c-nom-prog-upc-mg97) (input "CHANGE-PAGE":U, 
                                                    input "CONTAINER":U,
                                                    input frame f-pg-sel:handle ,
                                                    input frame f-pg-sel:handle,
                                                    input "",
                    
                                                    input ?).
                    
                        
                end.
            end.  
        
         
        
        
            if  entry(i-current-folder,c-list-folders,",") = "im-pg-par":U then do:
                 /* DPC */
                if  c-nom-prog-dpc-mg97 <> "" and
                    c-nom-prog-dpc-mg97 <> ? then do:                  
                    run value(c-nom-prog-dpc-mg97) (input "CHANGE-PAGE":U, 
                                                    input "CONTAINER":U,
                                                    input frame f-pg-par:handle ,
                                                    input frame f-pg-par:handle,
                                                    input "",
                    
                                                    input ?).
                    
                end.
                
                 /* APPC */
                if  c-nom-prog-appc-mg97 <> "" AND 
                    c-nom-prog-appc-mg97 <> ? then do:           
                    run value(c-nom-prog-appc-mg97) (input "CHANGE-PAGE":U, 
                                                     input "CONTAINER":U,
                                                     input frame f-pg-par:handle ,
                                                     input frame f-pg-par:handle,
                                                     input "",
                    
                                                     input ?).
                    
                end.
                
                 /* UPC */
                if  c-nom-prog-upc-mg97 <> "" AND
                    c-nom-prog-upc-mg97 <> ? then do:                  
                    run value(c-nom-prog-upc-mg97) (input "CHANGE-PAGE":U, 
                                                    input "CONTAINER":U,
                                                    input frame f-pg-par:handle ,
                                                    input frame f-pg-par:handle,
                                                    input "",
                     
                                                    input ?).
                     
                end.     
            end.           
        
        
        
            if  entry(i-current-folder,c-list-folders,",") = "im-pg-imp":U then do:
                 /* DPC */
                if  c-nom-prog-dpc-mg97 <> "" and
                    c-nom-prog-dpc-mg97 <> ? then do:                  
                    run value(c-nom-prog-dpc-mg97) (input "CHANGE-PAGE":U, 
                                                    input "CONTAINER":U,
                                                    input frame f-pg-imp:handle ,
                                                    input frame f-pg-imp:handle,
                                                    input "",
                    
                                                    input ?).
                    
                end.
                
                 /* APPC */
                if  c-nom-prog-appc-mg97 <> "" and
                    c-nom-prog-appc-mg97 <> ? then do:           
                    run value(c-nom-prog-appc-mg97) (input "CHANGE-PAGE":U, 
                                                     input "CONTAINER":U,
                                                     input frame f-pg-imp:handle ,
                                                     input frame f-pg-imp:handle,
                                                     input "",
                    
                                                     input ?).
                    
                end.
                 /* UPC */
                if  c-nom-prog-upc-mg97 <> "" and
                    c-nom-prog-upc-mg97 <> ? then do:                  
                    run value(c-nom-prog-upc-mg97) (input "CHANGE-PAGE":U, 
                                                    input "CONTAINER":U,
                                                    input frame f-pg-imp:handle ,
                                                    input frame f-pg-imp:handle,
                                                    input "",
                    
                                                    input ?).
                    
                end.
            end.  
       
    
    

/* I-EPC014.I */
 
/* i-rptrp.i */
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this Window, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */


