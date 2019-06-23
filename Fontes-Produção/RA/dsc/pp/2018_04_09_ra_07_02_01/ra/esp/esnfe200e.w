&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win2 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
  Alteraá∆o: 20/05/2014 - LÇa Campos - 
             Find do emitente passa a ser pelo nome-abrev e n∆o mais
             pelo cgc (Devido ao cadastro de inativo com cnpj duplicado)    
             Alteracao: 05/09/14 - Lea - So executa a pi-carrega-infs-item se a nota
                              Nao estiver implantada. Para corrigir erro
                              de deposito ch-6664
                            - Se os campos deposito e localizaªío estiver
                             preenchido nao traz o sugerido no esra010.p
                           - Se nota estiver implantada nío atualiza os
                             campos de-fat-conv e fi-de-quantidade com o
                             sugerido, tras apenas o que atualizou na tabela
            05/12/14 - Lea - Inclus∆o da nr-seq-fat  no  FIND FIRST it-nota-fisc 
                             (Erro, encontrava sempre a sequencia 10)
            02/02/15 - Passa parametro rowid(nfe-it-nota-fisc-rec) para esra010.p
          
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
{cdp\cdcfgdis.i} /* --- Definiá∆o vers∆o do EMS --- */

{dsc\ra\include\variaveis-nfe-receb.i}

{dsc\ra\include\buffers.i}

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAM p_rowid_item AS ROWID                  NO-UNDO.
DEFINE VAR c-narrativa AS CHAR  FORMAT "x(2000)" NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR c-campo         AS CHAR         NO-UNDO.
DEF VAR pr-rowid        AS ROWID        NO-UNDO.
DEF VAR pi-num-parc     AS CHAR         NO-UNDO.
DEF VAR c-return        AS CHAR         NO-UNDO. 

DEFINE VARIABLE wh-pesquisa AS HANDLE NO-UNDO.
DEFINE VARIABLE l-implanta  AS LOGICAL NO-UNDO.

DEFINE BUFFER bf-nfe-it-nota-fisc-rec   FOR nfe-it-nota-fisc-rec.
DEFINE BUFFER bf2-nfe-it-nota-fisc-rec  FOR nfe-it-nota-fisc-rec.
DEFINE BUFFER bf-nfe-nota-fiscal-rec    FOR nfe-nota-fiscal-rec.

DEFINE VARIABLE l-item-pref-excelente AS LOGICAL     NO-UNDO.
DEFINE VARIABLE c-deposito            AS CHARACTER   NO-UNDO.

DEFINE VARIABLE c-aux-conta-contabil NO-UNDO LIKE conta-contab.conta-contabil.

DEFINE VARIABLE de-fator-conv AS DECIMAL     NO-UNDO.
DEFINE VARIABLE l-item-fornec AS LOGICAL     NO-UNDO.

DEFINE VARIABLE c-desc-conta AS CHARACTER   NO-UNDO.

DEFINE VARIABLE h-boin176 AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-zoom-cod-conta  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-zoom-desc-conta AS CHARACTER   NO-UNDO.

def new global shared var v_rec_cta_ctbl_integr as RECID format ">>>>>>9":U initial ? no-undo.
DEFINE VARIABLE hProgramZoom AS HANDLE      NO-UNDO.

DEFINE VARIABLE l-ccusto AS LOGICAL     NO-UNDO.
DEFINE VARIABLE c-aux-conta-nova   AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-impostos fi-ordem-xml fi-c-it-codigo ~
fi-c-natur-oper fi-de-quantidade fi-c-un fi-de-qt-do-forn fi-c-un-forn ~
fi-de-pre-unit fi-i-num-pedido fi-i-num-ordem fi-i-parcela-oc ~
fi-de-preco-total bt-ordem fi-i-cod-emite-terc fi-c-ser-terc ~
fi-nro-docto-refer fi-c-nat-terc fi-i-nro-docto-terc fi-serie-docto-refer ~
fi-de-seq-terc fi-seq-docto-refer fi-c-class-fiscal fi-c-conta-contabil ~
fi-c-cod-refer fi-i-nr-ord-produ bt-narrativa fi-c-cod-depos ~
fi-c-cod-localiz fi-c-ser-lote fi-da-dt-vali-lote bt-ser-lote btn-ok ~
btn-cancelar fi-c-text-3 fi-c-text fi-c-text-2 fi-ct-codigo ~
fi-cod-unid-negoc RECT-34 IMAGE-19 c-descricao-3 c-descricao-4 RECT-28 ~
c-descricao c-descricao-2 c-descricao-5 c-descricao-6 c-descricao-7 RECT-29 ~
c-descricao-8 
&Scoped-Define DISPLAYED-OBJECTS fi-ordem-xml fi-i-seq fi-c-it-codigo-forn ~
fi-c-it-codigo fi-c-desc-item fi-i-cfop fi-c-natur-oper fi-c-denominacao ~
fi-c-esp-nat fi-de-quantidade fi-c-un fi-de-qt-do-forn fi-c-un-forn ~
fi-de-pre-unit fi-de-pre-unit-xml fi-i-num-pedido fi-de-pre-ordem ~
fi-i-num-ordem fi-de-desconto-ordem fi-de-desconto-xml fi-i-parcela-oc ~
fi-de-preco-total tg-fifo-oc fi-i-cod-emite-terc fi-c-ser-terc ~
fi-nro-docto-refer fi-c-nat-terc fi-i-nro-docto-terc fi-serie-docto-refer ~
fi-de-seq-terc fi-seq-docto-refer fi-c-class-fiscal fi-c-desc-class-fiscal ~
fi-c-conta-contabil fi-c-desc-conta-contabil fi-c-cod-refer fi-c-desc-refer ~
fi-i-nr-ord-produ fi-c-cod-depos fi-c-nome-depos fi-c-cod-localiz ~
fi-c-ser-lote fi-da-dt-vali-lote fi-c-text-3 fi-c-text fi-c-text-2 ~
fi-de-total-nosso fi-de-frete fi-ct-codigo fi-ct-codigo-desc fi-sc-codigo ~
fi-sc-codigo-desc fi-cod-unid-negoc fi-unid-neg-descri 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-busca-nota 
     IMAGE-UP FILE "dsc/ra/img/lupa2.bmp":U
     LABEL "Busca" 
     SIZE 4 BY .96 TOOLTIP "Busca Documento Referenciado".

DEFINE BUTTON bt-doc-nota 
     IMAGE-UP FILE "dsc\ra\img\im-relac.bmp":U
     LABEL "+" 
     SIZE 4 BY .96 TOOLTIP "Clique para Selecionar mais de um Docto/Nota".

DEFINE BUTTON bt-estrutura 
     IMAGE-UP FILE "dsc/ra/img/ii-emp.bmp":U
     LABEL "+" 
     SIZE 4 BY .96 TOOLTIP "Clique para definir documento de origem por estrutura".

DEFINE BUTTON bt-impostos 
     LABEL "Impostos" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-narrativa 
     IMAGE-UP FILE "dsc/ra/img/im-narr.bmp":U
     LABEL "Narrativa" 
     SIZE 4 BY .96 TOOLTIP "Inserir narrativa do item".

DEFINE BUTTON bt-ordem 
     IMAGE-UP FILE "dsc\ra\img\im-relac.bmp":U
     LABEL "+" 
     SIZE 4 BY .96 TOOLTIP "Clique para Selecionar mais de uma Ordem de Compra".

DEFINE BUTTON bt-ser-lote 
     IMAGE-UP FILE "dsc\ra\img\im-relac.bmp":U
     LABEL "+" 
     SIZE 4 BY .96 TOOLTIP "Clique para Selecionar mais de uma SÇrie/Lote".

DEFINE BUTTON btn-cancelar AUTO-END-KEY 
     IMAGE-UP FILE "dsc\ra\img\cancelar.bmp":U
     LABEL "Cancelar" 
     SIZE 10 BY 1.13 TOOLTIP "Cancelar".

DEFINE BUTTON btn-ok DEFAULT 
     IMAGE-UP FILE "dsc\ra\img\salvar.bmp":U
     LABEL "esnfe200OK" 
     SIZE 10 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE fi-c-class-fiscal AS CHARACTER FORMAT "9999.99.99":U 
     LABEL "Classif. Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-cod-depos AS CHARACTER FORMAT "X(3)":U 
     LABEL "Dep¢sito" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-cod-localiz AS CHARACTER FORMAT "X(20)":U 
     LABEL "Localizaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-cod-refer AS CHARACTER FORMAT "X(8)":U 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-conta-contabil AS CHARACTER FORMAT "x(16)":U 
     LABEL "Conta Cont†bil" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-denominacao AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 58 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-desc-class-fiscal AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-desc-conta-contabil AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-desc-item AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 74 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-desc-refer AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-esp-nat AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-it-codigo AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-it-codigo-forn AS CHARACTER FORMAT "X(60)":U 
     LABEL "Item Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-nat-terc AS CHARACTER FORMAT "X(8)":U 
     LABEL "Natur. Entrega" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-natur-oper AS CHARACTER FORMAT "X(8)":U 
     LABEL "Nat. Oper" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-nome-depos AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 81 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-ser-lote AS CHARACTER FORMAT "X(19)":U 
     LABEL "SÇrie/Lote" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-ser-terc AS CHARACTER FORMAT "X(5)":U 
     LABEL "Serie Entrega" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-text AS CHARACTER FORMAT "X(100)":U INITIAL "Docto Referenciado" 
      VIEW-AS TEXT 
     SIZE 14.57 BY .5 NO-UNDO.

DEFINE VARIABLE fi-c-text-2 AS CHARACTER FORMAT "X(100)":U INITIAL "Informaá‰es Adicionais do Item" 
      VIEW-AS TEXT 
     SIZE 21.43 BY .5 NO-UNDO.

DEFINE VARIABLE fi-c-text-3 AS CHARACTER FORMAT "X(100)":U INITIAL "Operaá∆o Triangular" 
      VIEW-AS TEXT 
     SIZE 14.57 BY .5 NO-UNDO.

DEFINE VARIABLE fi-c-un AS CHARACTER FORMAT "XX":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-un-forn AS CHARACTER FORMAT "XX":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-unid-negoc AS CHARACTER FORMAT "x(3)":U 
     LABEL "Unid Negocio" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ct-codigo AS CHARACTER FORMAT "x(20)":U 
     LABEL "Conta" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ct-codigo-desc AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE fi-da-dt-vali-lote AS DATE FORMAT "99/99/9999":U 
     LABEL "Validade Lote" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-de-desconto-ordem AS DECIMAL FORMAT ">,>>>,>>>,>>9.99999":U INITIAL 0 
     LABEL "Desconto" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-de-desconto-xml AS DECIMAL FORMAT ">,>>>,>>>,>>9.99999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-de-frete AS DECIMAL FORMAT ">>>,>>>,>>9.9999":U INITIAL 0 
     LABEL "VL Frete" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-de-pre-ordem AS DECIMAL FORMAT ">,>>>,>>>,>>9.99999":U INITIAL 0 
     LABEL "Preáo Ordem" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-de-pre-unit AS DECIMAL FORMAT ">,>>>,>>>,>>9.99999":U INITIAL 0 
     LABEL "Unit†rio" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-de-pre-unit-xml AS DECIMAL FORMAT ">,>>>,>>>,>>9.99999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-de-preco-total AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-de-qt-do-forn AS DECIMAL FORMAT ">>>,>>>,>>9.9999":U INITIAL 0 
     LABEL "Qt Fornec / XML" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-de-quantidade AS DECIMAL FORMAT ">>>>>,>>9.9999":U INITIAL 0 
     LABEL "Nossa Qtde" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-de-seq-terc AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Seq Entrega" 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-de-total-nosso AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Preáo Total" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-cfop AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "CFOP" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-cod-emite-terc AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Emit.Entrega" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-nr-ord-produ AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Ordem Produá∆o" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-nro-docto-terc AS CHARACTER FORMAT "X(16)":U 
     LABEL "Nota Entrega" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-num-ordem AS INTEGER FORMAT "zzzzz999":U INITIAL 0 
     LABEL "Ordem Compra" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-num-pedido AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Pedido" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-parcela-oc AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Parcela" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-seq AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Sequància" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nro-docto-refer AS CHARACTER FORMAT "X(16)":U 
     LABEL "Nr. Documento" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ordem-xml AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 10.57 BY .67
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fi-sc-codigo AS CHARACTER FORMAT "x(20)":U 
     LABEL "Centro Custo" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sc-codigo-desc AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE fi-seq-docto-refer AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Seq. Docto" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie-docto-refer AS CHARACTER FORMAT "X(5)":U 
     LABEL "SÇrie Docto" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-unid-neg-descri AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 53 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-19
     FILENAME "dsc\ra\img\dsc.bmp":U
     SIZE 4.86 BY 1.42.

DEFINE RECTANGLE c-descricao
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 108.43 BY 2.42.

DEFINE RECTANGLE c-descricao-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77.14 BY 4.5.

DEFINE RECTANGLE c-descricao-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52.57 BY 3.25.

DEFINE RECTANGLE c-descricao-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30.57 BY 3.5.

DEFINE RECTANGLE c-descricao-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 108.43 BY 1.21.

DEFINE RECTANGLE c-descricao-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 4.5.

DEFINE RECTANGLE c-descricao-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55.43 BY 3.25.

DEFINE RECTANGLE c-descricao-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30.57 BY 1.79.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77.43 BY 5.5.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 108.43 BY 2.33.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 108.86 BY 1.92
     BGCOLOR 7 .

DEFINE VARIABLE tg-fifo-oc AS LOGICAL INITIAL no 
     LABEL "FIFO" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .88 TOOLTIP "FIFO em Ordens de Compra" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     bt-impostos AT ROW 16.96 COL 87 WIDGET-ID 298
     fi-ordem-xml AT ROW 8.5 COL 79 COLON-ALIGNED NO-LABEL WIDGET-ID 278
     fi-i-seq AT ROW 1.38 COL 14 COLON-ALIGNED WIDGET-ID 106
     fi-c-it-codigo-forn AT ROW 1.38 COL 76 COLON-ALIGNED WIDGET-ID 108
     fi-c-it-codigo AT ROW 2.29 COL 14 COLON-ALIGNED WIDGET-ID 134
     fi-c-desc-item AT ROW 2.29 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 136
     fi-i-cfop AT ROW 3.71 COL 14 COLON-ALIGNED WIDGET-ID 194
     fi-c-natur-oper AT ROW 3.71 COL 32.86 COLON-ALIGNED WIDGET-ID 112
     fi-c-denominacao AT ROW 3.71 COL 42.86 COLON-ALIGNED NO-LABEL WIDGET-ID 118
     fi-c-esp-nat AT ROW 3.71 COL 100.86 COLON-ALIGNED NO-LABEL WIDGET-ID 120
     fi-de-quantidade AT ROW 5.33 COL 14 COLON-ALIGNED WIDGET-ID 146
     fi-c-un AT ROW 5.33 COL 28.86 COLON-ALIGNED NO-LABEL WIDGET-ID 150
     fi-de-qt-do-forn AT ROW 6.21 COL 16.14 COLON-ALIGNED WIDGET-ID 144
     fi-c-un-forn AT ROW 6.21 COL 28.86 COLON-ALIGNED NO-LABEL WIDGET-ID 148
     fi-de-pre-unit AT ROW 5.38 COL 46.14 COLON-ALIGNED WIDGET-ID 152
     fi-de-pre-unit-xml AT ROW 5.38 COL 62.43 COLON-ALIGNED NO-LABEL WIDGET-ID 266
     fi-i-num-pedido AT ROW 5.71 COL 91.86 COLON-ALIGNED WIDGET-ID 192
     fi-de-pre-ordem AT ROW 6.25 COL 46.14 COLON-ALIGNED WIDGET-ID 264
     fi-i-num-ordem AT ROW 6.58 COL 91.86 COLON-ALIGNED WIDGET-ID 142
     fi-de-desconto-ordem AT ROW 7.13 COL 46.14 COLON-ALIGNED WIDGET-ID 274
     fi-de-desconto-xml AT ROW 7.13 COL 62.43 COLON-ALIGNED NO-LABEL WIDGET-ID 276
     fi-i-parcela-oc AT ROW 7.5 COL 91.86 COLON-ALIGNED WIDGET-ID 216
     fi-de-preco-total AT ROW 8.42 COL 62.43 COLON-ALIGNED NO-LABEL WIDGET-ID 156
     bt-ordem AT ROW 8.42 COL 104.86 HELP
          "Clique para Selecionar mais de uma Ordem de Compra" WIDGET-ID 220
     tg-fifo-oc AT ROW 8.5 COL 93.86 HELP
          "FIFO em Ordens de Compra" WIDGET-ID 226
     fi-i-cod-emite-terc AT ROW 10 COL 14 COLON-ALIGNED WIDGET-ID 262
     fi-c-ser-terc AT ROW 10 COL 38 COLON-ALIGNED WIDGET-ID 256
     fi-nro-docto-refer AT ROW 10 COL 78.43 COLON-ALIGNED WIDGET-ID 198
     bt-busca-nota AT ROW 10 COL 99 HELP
          "Busca Documento Referenciado" WIDGET-ID 228
     fi-c-nat-terc AT ROW 10.88 COL 14 COLON-ALIGNED WIDGET-ID 254
     fi-i-nro-docto-terc AT ROW 10.88 COL 38 COLON-ALIGNED WIDGET-ID 258
     fi-serie-docto-refer AT ROW 10.88 COL 78.57 COLON-ALIGNED WIDGET-ID 200
     bt-doc-nota AT ROW 11.67 COL 100.86 HELP
          "Clique para Selecionar mais de uma Nota" WIDGET-ID 222
     bt-estrutura AT ROW 11.67 COL 104.86 HELP
          "Clique para definir documento de origem por estrutura" WIDGET-ID 230
     fi-de-seq-terc AT ROW 11.75 COL 38 COLON-ALIGNED WIDGET-ID 260
     fi-seq-docto-refer AT ROW 11.75 COL 78.57 COLON-ALIGNED WIDGET-ID 206
     fi-c-class-fiscal AT ROW 13.21 COL 14 COLON-ALIGNED WIDGET-ID 186
     fi-c-desc-class-fiscal AT ROW 13.21 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 188
     fi-c-conta-contabil AT ROW 14.21 COL 14 COLON-ALIGNED WIDGET-ID 166
     fi-c-desc-conta-contabil AT ROW 14.21 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 184
     fi-c-cod-refer AT ROW 17.33 COL 14 COLON-ALIGNED WIDGET-ID 168
     fi-c-desc-refer AT ROW 17.33 COL 24 COLON-ALIGNED NO-LABEL WIDGET-ID 182
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.14 BY 22.71
         FONT 1 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     fi-i-nr-ord-produ AT ROW 14.21 COL 94 COLON-ALIGNED WIDGET-ID 212
     bt-narrativa AT ROW 15.42 COL 104.86 WIDGET-ID 244
     fi-c-cod-depos AT ROW 18.75 COL 14 COLON-ALIGNED WIDGET-ID 174
     fi-c-nome-depos AT ROW 18.75 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     fi-c-cod-localiz AT ROW 19.75 COL 14 COLON-ALIGNED WIDGET-ID 176
     fi-c-ser-lote AT ROW 19.75 COL 46 COLON-ALIGNED WIDGET-ID 178
     fi-da-dt-vali-lote AT ROW 19.75 COL 87.86 COLON-ALIGNED WIDGET-ID 180
     bt-ser-lote AT ROW 19.71 COL 104.86 HELP
          "Clique para Selecionar mais de uma SÇrie/Lote" WIDGET-ID 224
     btn-ok AT ROW 21.38 COL 34.43 WIDGET-ID 242
     btn-cancelar AT ROW 21.38 COL 63.14 WIDGET-ID 48
     fi-c-text-3 AT ROW 9.5 COL 2.43 NO-LABEL WIDGET-ID 250
     fi-c-text AT ROW 9.5 COL 58.14 NO-LABEL WIDGET-ID 204
     fi-c-text-2 AT ROW 12.96 COL 80.14 NO-LABEL WIDGET-ID 210
     fi-de-total-nosso AT ROW 8.42 COL 46.14 COLON-ALIGNED WIDGET-ID 280
     fi-de-frete AT ROW 8.38 COL 14 COLON-ALIGNED WIDGET-ID 282
     fi-ct-codigo AT ROW 14.29 COL 14 COLON-ALIGNED WIDGET-ID 284
     fi-ct-codigo-desc AT ROW 14.29 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 286
     fi-sc-codigo AT ROW 15.21 COL 14 COLON-ALIGNED WIDGET-ID 288
     fi-sc-codigo-desc AT ROW 15.21 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 290
     fi-cod-unid-negoc AT ROW 16.21 COL 14 COLON-ALIGNED WIDGET-ID 292
     fi-unid-neg-descri AT ROW 16.21 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 294
     "Pedido / Ordem de Compra" VIEW-AS TEXT
          SIZE 19 BY .5 AT ROW 4.79 COL 80.86 WIDGET-ID 238
     "Narrativa:" VIEW-AS TEXT
          SIZE 7 BY 1 AT ROW 15.42 COL 97.43 WIDGET-ID 246
          FONT 7
     "Nosso Preáo" VIEW-AS TEXT
          SIZE 10.57 BY .5 AT ROW 4.79 COL 48.57 WIDGET-ID 270
     "Preáo XML" VIEW-AS TEXT
          SIZE 9 BY .5 AT ROW 4.79 COL 64.86 WIDGET-ID 272
     RECT-34 AT ROW 20.96 COL 1.14 WIDGET-ID 104
     IMAGE-19 AT ROW 21.21 COL 103.43 WIDGET-ID 190
     c-descricao-3 AT ROW 9.67 COL 57.43 WIDGET-ID 196
     c-descricao-4 AT ROW 13 COL 79.43 WIDGET-ID 208
     RECT-28 AT ROW 13 COL 1.57 WIDGET-ID 40
     c-descricao AT ROW 1.08 COL 1.57 WIDGET-ID 26
     c-descricao-2 AT ROW 5 COL 1.57 WIDGET-ID 124
     c-descricao-5 AT ROW 3.58 COL 1.57 WIDGET-ID 232
     c-descricao-6 AT ROW 5 COL 79 WIDGET-ID 234
     c-descricao-7 AT ROW 9.67 COL 1.57 WIDGET-ID 248
     RECT-29 AT ROW 18.5 COL 1.57 WIDGET-ID 268
     c-descricao-8 AT ROW 16.63 COL 79.43 WIDGET-ID 296
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.14 BY 22.71
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win2 ASSIGN
         HIDDEN             = YES
         TITLE              = "esnfe200e - Item da Nota Fiscal"
         HEIGHT             = 22.42
         WIDTH              = 110.14
         MAX-HEIGHT         = 30.75
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 30.75
         VIRTUAL-WIDTH      = 195.14
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win2:LOAD-ICON("dsc/ra/img/dsc.ico":U) THEN
    MESSAGE "Unable to load icon: dsc/ra/img/dsc.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win2 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win2
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR BUTTON bt-busca-nota IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-doc-nota IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-estrutura IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-c-denominacao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-c-desc-class-fiscal IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-c-desc-conta-contabil IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-c-desc-item IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-c-desc-refer IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-c-esp-nat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-c-it-codigo-forn IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-c-nome-depos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-c-text IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-c-text-2 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-c-text-3 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-ct-codigo-desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-de-desconto-ordem IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-de-desconto-xml IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-de-frete IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-de-pre-ordem IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-de-pre-unit-xml IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-de-total-nosso IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-i-cfop IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-i-seq IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sc-codigo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sc-codigo-desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-unid-neg-descri IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-fifo-oc IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win2)
THEN W-Win2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win2 W-Win2
ON END-ERROR OF W-Win2 /* esnfe200e - Item da Nota Fiscal */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win2 W-Win2
ON WINDOW-CLOSE OF W-Win2 /* esnfe200e - Item da Nota Fiscal */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-nota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-nota W-Win2
ON CHOOSE OF bt-busca-nota IN FRAME F-Main /* Busca */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        /* --- Verifica Natureza de Operacao --- */
        IF NOT CAN-FIND(FIRST natur-oper WHERE natur-oper.nat-operacao = INPUT fi-c-natur-oper
                                         NO-LOCK) THEN DO:
            ASSIGN c-mensagem-erro = "1,"                               +
                                     "Natureza de Operaá∆o inv†lida!,"  +
                                     "Preencha a Natureza de Operaá∆o corretamente.".

/*             RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
            RUN pi_mensagem_erro IN h_bonfe001 (INPUT c-mensagem-erro,
                                                OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
/*             DELETE PROCEDURE h_bonfe001. */
            
            ASSIGN c-mensagem-erro = "".
            APPLY "ENTRY" TO fi-c-natur-oper.
            RETURN NO-APPLY.
        END.

        /* --- Devolucao de Clientes --- */
        IF i-tipo-nfe = 2 THEN DO:

            RUN dsc\ra\esp\esnfe200z19.w (INPUT p_rowid_item,
                                          INPUT (INPUT fi-c-it-codigo),
                                          INPUT (INPUT fi-c-natur-oper),
                                          INPUT (INPUT fi-de-pre-unit-xml),
                                          OUTPUT pr-rowid).
        
            FIND FIRST it-nota-fisc WHERE ROWID(it-nota-fisc) = pr-rowid
                                    NO-LOCK NO-ERROR.
            IF AVAIL it-nota-fisc THEN DO:
            
                ASSIGN fi-nro-docto-refer:SCREEN-VALUE      = it-nota-fisc.nr-nota-fis
                       fi-serie-docto-refer:SCREEN-VALUE    = it-nota-fisc.serie
                       fi-seq-docto-refer:SCREEN-VALUE      = STRING(it-nota-fisc.nr-seq-fat) .
                RUN pi-sugestao-conta.


            END.
                       
        END.
        ELSE DO:
    
            RUN dsc\ra\esp\esnfe200z17.w (INPUT p_rowid_item,
                                          INPUT (INPUT fi-c-it-codigo),
                                          INPUT (INPUT fi-c-natur-oper),
                                          OUTPUT pr-rowid).
            IF pr-rowid = ? THEN RETURN NO-APPLY.
            FIND FIRST saldo-terc WHERE ROWID(saldo-terc) = pr-rowid
                                  NO-LOCK NO-ERROR.
            IF NOT AVAIL saldo-terc THEN RETURN NO-APPLY.

            
    
            
            ASSIGN fi-c-it-codigo:SCREEN-VALUE          = saldo-terc.it-codigo
                   fi-c-cod-refer:SCREEN-VALUE          = saldo-terc.cod-refer
                   fi-c-cod-depos:SCREEN-VALUE          = saldo-terc.cod-depos
                   fi-c-cod-localiz:SCREEN-VALUE        = saldo-terc.cod-localiz
                   fi-c-ser-lote:SCREEN-VALUE           = saldo-terc.lote
                   fi-nro-docto-refer:SCREEN-VALUE      = saldo-terc.nro-docto
                   fi-serie-docto-refer:SCREEN-VALUE    = saldo-terc.serie-docto
                   fi-seq-docto-refer:SCREEN-VALUE      = STRING(saldo-terc.sequencia)
                   c-depos = saldo-terc.cod-depos . /*RBA - Atualiza deposito com o deposito do saldo-terc*/

            RUN pi-sugestao-conta.
                     
            IF saldo-terc.nr-ord-produ <> 0 THEN 
                ASSIGN fi-i-nr-ord-produ:SCREEN-VALUE   = STRING(saldo-terc.nr-ord-produ).
    
            FIND FIRST saldo-estoq WHERE saldo-estoq.it-codigo  = saldo-terc.it-codigo
                                     AND saldo-estoq.lote       = saldo-terc.lote
                                   NO-LOCK NO-ERROR.
            IF AVAIL saldo-estoq THEN ASSIGN fi-da-dt-vali-lote:SCREEN-VALUE = STRING(saldo-estoq.dt-vali-lote).
    
            
/*             APPLY "LEAVE" TO fi-c-it-codigo. */
    
            
            IF c-depos <> "" THEN /*RBA - Atualiza deposito com o deposito do saldo-terc*/
                ASSIGN fi-c-cod-depos:SCREEN-VALUE = c-depos.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-doc-nota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-doc-nota W-Win2
ON CHOOSE OF bt-doc-nota IN FRAME F-Main /* + */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        /* --- Relaciona Nota de Terceiro --- */
/*         RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
        RUN pi_relaciona_doc_nota IN h_bonfe001 (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                                 INPUT (INPUT fi-c-it-codigo),
                                                 INPUT (INPUT fi-c-un),
                                                 INPUT (INPUT fi-de-quantidade),
                                                 INPUT (INPUT fi-c-natur-oper),
                                                 INPUT (INPUT fi-nro-docto-refer),
                                                 INPUT (INPUT fi-serie-docto-refer),
                                                 INPUT (INPUT fi-seq-docto-refer),
                                                 INPUT (INPUT fi-de-pre-unit),
                                                 INPUT (INPUT fi-de-pre-unit-xml),
                                                 INPUT 1, /*Relac Normal*/
                                                 OUTPUT c-mensagem-erro).
/*         DELETE PROCEDURE h_bonfe001. */
        
        /* --- Verifica ERRO --- */
        IF c-mensagem-erro <> "" THEN DO:
/*             RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
            RUN pi_mensagem_erro IN h_bonfe001 (INPUT c-mensagem-erro,
                                                OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
/*             DELETE PROCEDURE h_bonfe001. */
            
            /* --- Se for Mensagem de Erro nao Prossegue --- */
            IF i-acao = 1 THEN DO:
                ASSIGN c-mensagem-erro = "".
                RETURN NO-APPLY.
            END.
        END.
            
        RUN pi-hab-desab.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-estrutura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-estrutura W-Win2
ON CHOOSE OF bt-estrutura IN FRAME F-Main /* + */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        /* --- Relaciona Nota de Terceiro --- */
/*         RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
        RUN pi_relaciona_doc_nota IN h_bonfe001 (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                                 INPUT (INPUT fi-c-it-codigo),
                                                 INPUT (INPUT fi-c-un),
                                                 INPUT (INPUT fi-de-quantidade),
                                                 INPUT (INPUT fi-c-natur-oper),
                                                 INPUT (INPUT fi-nro-docto-refer),
                                                 INPUT (INPUT fi-serie-docto-refer),
                                                 INPUT (INPUT fi-seq-docto-refer),
                                                 INPUT (INPUT fi-de-pre-unit),
                                                 INPUT 2, /*Relac por Estrutura*/
                                                 OUTPUT c-mensagem-erro).
/*         DELETE PROCEDURE h_bonfe001. */
        
        /* --- Verifica ERRO --- */
        IF c-mensagem-erro <> "" THEN DO:
/*             RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
            RUN pi_mensagem_erro IN h_bonfe001 (INPUT c-mensagem-erro,
                                                OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
/*             DELETE PROCEDURE h_bonfe001. */
            
            /* --- Se for Mensagem de Erro nao Prossegue --- */
            IF i-acao = 1 THEN DO:
                ASSIGN c-mensagem-erro = "".
                RETURN NO-APPLY.
            END.
        END.
            
        RUN pi-hab-desab.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-impostos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-impostos W-Win2
ON CHOOSE OF bt-impostos IN FRAME F-Main /* Impostos */
DO:

  IF fi-c-natur-oper:SCREEN-VALUE = "" THEN
      MESSAGE "ê necess†rio digitar a Natureza de Operaá∆o para prosseguir."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.                               
  ELSE
      RUN dsc/ra/esp/esnfe200e04.w(INPUT p_rowid_item, 
                                   INPUT fi-c-natur-oper:SCREEN-VALUE) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-narrativa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-narrativa W-Win2
ON CHOOSE OF bt-narrativa IN FRAME F-Main /* Narrativa */
DO:
   RUN dsc\ra\esp\esnfe200i.w (INPUT p_rowid_item,
                               INPUT-OUTPUT c-narrativa).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ordem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ordem W-Win2
ON CHOOSE OF bt-ordem IN FRAME F-Main /* + */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        /* --- Relaciona Ordem de Compra --- */
/*         RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
        RUN pi_relaciona_ordem IN h_bonfe001 (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                              INPUT (INPUT fi-c-it-codigo),
                                              INPUT (INPUT fi-c-un),                          
                                              INPUT (INPUT fi-de-quantidade /*fi-de-qt-do-forn*/ /*fi-de-quantidade*/),
                                              INPUT (INPUT fi-c-natur-oper),
                                              INPUT (INPUT fi-de-preco-total),
                                              INPUT (INPUT fi-i-num-pedido),
                                              INPUT (INPUT fi-i-num-ordem),
                                              INPUT (INPUT fi-i-parcela-oc),
                                              OUTPUT c-mensagem-erro).
/*         DELETE PROCEDURE h_bonfe001. */
        
        /* --- Verifica ERRO --- */
        IF c-mensagem-erro <> "" THEN DO:
/*             RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
            RUN pi_mensagem_erro IN h_bonfe001 (INPUT c-mensagem-erro,
                                                OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
/*             DELETE PROCEDURE h_bonfe001. */
            
            /* --- Se for Mensagem de Erro nao Prossegue --- */
            IF i-acao = 1 THEN DO:
                ASSIGN c-mensagem-erro = "".
                RETURN NO-APPLY.
            END.
        END.
            
        RUN pi-hab-desab.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ser-lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ser-lote W-Win2
ON CHOOSE OF bt-ser-lote IN FRAME F-Main /* + */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        /* --- Relaciona Serie --- */
/*         RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
        RUN pi_relaciona_ser_lote IN h_bonfe001 (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                                 INPUT (INPUT fi-c-it-codigo),
                                                 INPUT (INPUT fi-c-un),
                                                 INPUT (INPUT fi-de-quantidade),
                                                 INPUT (INPUT fi-c-ser-lote),
                                                 INPUT (INPUT fi-da-dt-vali-lote),
                                                 OUTPUT c-mensagem-erro).
/*         DELETE PROCEDURE h_bonfe001. */
        
        /* --- Verifica ERRO --- */
        IF c-mensagem-erro <> "" THEN DO:
/*             RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
            RUN pi_mensagem_erro IN h_bonfe001 (INPUT c-mensagem-erro,
                                                OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
/*             DELETE PROCEDURE h_bonfe001. */
            
            /* --- Se for Mensagem de Erro nao Prossegue --- */
            IF i-acao = 1 THEN DO:
                ASSIGN c-mensagem-erro = "".
                RETURN NO-APPLY.
            END.
        END.
            
        RUN pi-hab-desab.

/*         RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
        RUN pi_valida_upc IN h_bonfe001 (INPUT "esnfe200e",
                                         INPUT 'RetornoSerLote',
                                         INPUT 'CONTAINER',
                                         INPUT FRAME F-Main:HANDLE,
                                         INPUT FRAME F-Main:HANDLE,
                                         INPUT '',
                                         INPUT p_rowid_item).
/*         DELETE PROCEDURE h_bonfe001. */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancelar W-Win2
ON CHOOSE OF btn-cancelar IN FRAME F-Main /* Cancelar */
DO:
    FIND FIRST bf-nfe-it-nota-fisc-rec WHERE ROWID(bf-nfe-it-nota-fisc-rec) = p_rowid_item
                                       NO-LOCK NO-ERROR.
    IF AVAIL bf-nfe-it-nota-fisc-rec AND bf-nfe-it-nota-fisc-rec.it-codigo = "" THEN DO:
        FOR EACH nfe-relac-doc-nota-rec WHERE nfe-relac-doc-nota-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso
                                          AND nfe-relac-doc-nota-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item:
            DELETE nfe-relac-doc-nota-rec.
        END.

        FOR EACH nfe-relac-ser-lote-rec WHERE nfe-relac-ser-lote-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso
                                          AND nfe-relac-ser-lote-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item:
            DELETE nfe-relac-ser-lote-rec.
        END.

        FOR EACH nfe-relac-ordem-rec WHERE nfe-relac-ordem-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso
                                       AND nfe-relac-ordem-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item:
            DELETE nfe-relac-ordem-rec.
        END.

    END.
    RUN pi-deleta-bo.
    ASSIGN c-narrativa = "".
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok W-Win2
ON CHOOSE OF btn-ok IN FRAME F-Main /* esnfe200OK */
DO:
    ASSIGN c-return = "ok".
    FIND FIRST prog_dtsul WHERE 
               prog_dtsul.cod_prog_dtsul = "esnfe200e" NO-LOCK NO-ERROR.

    IF AVAIL prog_dtsul THEN DO:
        IF SEARCH(prog_dtsul.nom_prog_upc) <> ? THEN
            RUN VALUE(prog_dtsul.nom_prog_upc) (INPUT "AntesAtualizaNota",
                                                INPUT "CONTAINER",
                                                INPUT FRAME F-Main:HANDLE,
                                                INPUT FRAME F-Main:HANDLE,
                                                INPUT '',
                                                INPUT p_rowid_item).
        ASSIGN c-return = RETURN-VALUE.
    END.

    IF c-return = "" THEN
        RETURN NO-APPLY.
    
    DO WITH FRAME {&FRAME-NAME}:
/*         RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */

        &IF '{&bf_dis_versao_ems}' > '2.07' &THEN
           ASSIGN c-aux-conta-nova =  (INPUT fi-ct-codigo).
        &ELSE
           ASSIGN c-aux-conta-nova = (INPUT fi-c-conta-contabil).
        &ENDIF
        
        /* --- Verifica Natureza de Operacao --- */
        IF NOT CAN-FIND(FIRST natur-oper WHERE natur-oper.nat-operacao = INPUT fi-c-natur-oper
                                         NO-LOCK) THEN DO:
            ASSIGN c-mensagem-erro = "1,"                               +
                                     "Natureza de Operaá∆o inv†lida!,"  +
                                     "Preencha a Natureza de Operaá∆o corretamente.".
     

            RUN pi_mensagem_erro IN h_bonfe001 (INPUT c-mensagem-erro,
                                                OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.

            ASSIGN c-mensagem-erro = "".
            APPLY "ENTRY" TO fi-c-natur-oper.
            RETURN NO-APPLY.
        END.

        RUN pi_valida_dados IN h_bonfe001 (INPUT  ROWID(bf-nfe-it-nota-fisc-rec),
                                           INPUT (INPUT fi-i-seq),              /* 01 */
                                           INPUT (INPUT fi-c-it-codigo-forn),   /* 02 */
                                           INPUT (INPUT fi-c-it-codigo),        /* 03 */    
                                           INPUT (INPUT fi-i-num-pedido),       /* 04 */ 
                                           INPUT (INPUT fi-i-num-ordem),        /* 05 */ 
                                           INPUT (INPUT fi-i-parcela-oc),       /* 06 */ 
                                           INPUT (INPUT tg-fifo-oc),            /* 07 */ 
                                           INPUT (INPUT fi-de-qt-do-forn),      /* 08 */ 
                                           INPUT (INPUT fi-c-un-forn),          /* 09 */ 
                                           INPUT (INPUT fi-de-quantidade),      /* 10 */ 
                                           INPUT (INPUT fi-c-un),               /* 11 */ 
                                           INPUT (INPUT fi-de-pre-unit),        /* 12 */ 
                                           INPUT (INPUT fi-de-preco-total),     /* 13 */ 
                                           INPUT (INPUT fi-i-cfop),             /* 14 */ 
                                           INPUT (INPUT fi-c-natur-oper),       /* 15 */ 
                                           INPUT (INPUT fi-c-esp-nat),          /* 16 */ 
                                           INPUT (INPUT fi-c-class-fiscal),     /* 17 */ 
                                           INPUT (c-aux-conta-nova),            /* 18 */ 
                                           INPUT (INPUT fi-c-cod-refer),        /* 19 */ 
                                           INPUT (INPUT fi-c-cod-depos),        /* 20 */ 
                                           INPUT (INPUT fi-c-cod-localiz),      /* 21 */ 
                                           INPUT (INPUT fi-c-ser-lote),         /* 22 */ 
                                           INPUT (INPUT fi-da-dt-vali-lote),    /* 23 */ 
                                           INPUT (INPUT fi-i-nr-ord-produ),     /* 24 */ 
                                           INPUT (INPUT fi-nro-docto-refer),    /* 25 */ 
                                           INPUT (INPUT fi-serie-docto-refer),  /* 26 */ 
                                           INPUT (INPUT fi-seq-docto-refer),    /* 27 */ 
                                           OUTPUT c-mensagem-erro).



        IF c-mensagem-erro = "" THEN DO:

            /*
            FIND FIRST emitente NO-LOCK
                WHERE emitente.CGC = bf-nfe-nota-fiscal-rec.EMIT-CNPJ NO-ERROR.*/

            FIND emitente WHERE
                 emitente.nome-abrev = bf-nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.


            RUN pi_valida_fator_conversao IN h_bonfe001 (INPUT  emitente.cod-emitente,
                                                         INPUT  (INPUT fi-c-it-codigo-forn),
                                                         INPUT  (INPUT fi-c-it-codigo),     
                                                         OUTPUT de-fator-conv,
                                                         OUTPUT l-item-fornec).       
                
                
            IF l-item-fornec THEN DO:
                
                FIND FIRST nfe-it-param-rec NO-LOCK
                    WHERE nfe-it-param-rec.cod-parametro      = "param_global" 
                    AND   nfe-it-param-rec.cod-item-parametro = "valida_qt_fator"  NO-ERROR.

                IF AVAIL nfe-it-param-rec                           AND
                   nfe-it-param-rec.valor-1-item-parametro = "SIM"  THEN DO:
                
                    IF INPUT fi-de-quantidade <> (INPUT fi-de-qt-do-forn) / de-fator-conv THEN DO:
                    
                            
        
                            ASSIGN c-mensagem-erro =  '1,Quantidade Invalida!,O ITEM ja possui um relacionamento ITEM X Fornecedor, porem a quantidade digitada nao confere com o fator de conversao, favor verificar a quantidade ou o cadastro.'.
        
                    END.
                END.

            END.
                


        END.
        


/*         DELETE PROCEDURE h_bonfe001. */

        /* --- Verifica ERRO --- */


        IF c-mensagem-erro <> "" THEN 
        DO:

            
/*             RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */

            RUN pi_mensagem_erro IN h_bonfe001 (INPUT c-mensagem-erro,
                                                OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.

/*             DELETE PROCEDURE h_bonfe001. */

            /* --- Se for Mensagem de Erro nao Prossegue --- */

            
            IF i-acao = 1 THEN 
            DO:
                ASSIGN c-campo = SUBSTR(c-mensagem-erro, LENGTH(c-mensagem-erro) - 4, LENGTH(c-mensagem-erro)).

                CASE TRIM(c-campo):
                    WHEN "(01)" THEN APPLY "ENTRY" TO fi-i-seq.
                    WHEN "(02)" THEN APPLY "ENTRY" TO fi-c-it-codigo-forn.
                    WHEN "(03)" THEN APPLY "ENTRY" TO fi-c-it-codigo.
                    WHEN "(04)" THEN APPLY "ENTRY" TO fi-i-num-pedido.
                    WHEN "(05)" THEN APPLY "ENTRY" TO fi-i-num-ordem.
                    WHEN "(06)" THEN APPLY "ENTRY" TO fi-i-parcela-oc.
                    WHEN "(07)" THEN APPLY "ENTRY" TO tg-fifo-oc.
                    WHEN "(08)" THEN APPLY "ENTRY" TO fi-de-qt-do-forn.
                    WHEN "(09)" THEN APPLY "ENTRY" TO fi-c-un-forn.
                    WHEN "(10)" THEN APPLY "ENTRY" TO fi-de-quantidade.
                    WHEN "(11)" THEN APPLY "ENTRY" TO fi-c-un.
                    WHEN "(12)" THEN APPLY "ENTRY" TO fi-de-pre-unit.
                    WHEN "(13)" THEN APPLY "ENTRY" TO fi-de-preco-total.
                    WHEN "(14)" THEN APPLY "ENTRY" TO fi-i-cfop.
                    WHEN "(15)" THEN APPLY "ENTRY" TO fi-c-natur-oper.
                    WHEN "(16)" THEN APPLY "ENTRY" TO fi-c-esp-nat.
                    WHEN "(17)" THEN APPLY "ENTRY" TO fi-c-class-fiscal.
                    WHEN "(18)" THEN APPLY "ENTRY" TO fi-c-conta-contabil.
                    WHEN "(19)" THEN APPLY "ENTRY" TO fi-c-cod-refer.
                    WHEN "(20)" THEN APPLY "ENTRY" TO fi-c-cod-depos.
                    WHEN "(21)" THEN APPLY "ENTRY" TO fi-c-cod-localiz.
                    WHEN "(22)" THEN APPLY "ENTRY" TO fi-c-ser-lote.
                    WHEN "(23)" THEN APPLY "ENTRY" TO fi-da-dt-vali-lote.
                    WHEN "(24)" THEN APPLY "ENTRY" TO fi-i-nr-ord-produ.
                    WHEN "(25)" THEN APPLY "ENTRY" TO fi-nro-docto-refer.
                    WHEN "(26)" THEN APPLY "ENTRY" TO fi-serie-docto-refer.
                    WHEN "(27)" THEN APPLY "ENTRY" TO fi-seq-docto-refer.
                END CASE.

                ASSIGN c-mensagem-erro = "".

                RETURN NO-APPLY.
            END.
        END.

        
        IF TRIM(c-mensagem-erro) = "" OR i-acao <> 1 THEN 
        DO:
            /* --- Atualiza Tabela --- */

            RUN pi-atualiza-nota (INPUT p_rowid_item).
        END.
    END.
    RUN pi-deleta-bo.
    ASSIGN c-narrativa = "".
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-c-class-fiscal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-class-fiscal W-Win2
ON F5 OF fi-c-class-fiscal IN FRAME F-Main /* Classif. Fiscal */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        W-Win2:SENSITIVE = NO.

        RUN dsc\ra\esp\esnfe200z03.w (OUTPUT pr-rowid).

        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        IF pr-rowid = ? THEN RETURN NO-APPLY.
        FIND FIRST classif-fisc WHERE ROWID(classif-fisc) = pr-rowid
                                NO-LOCK NO-ERROR.
        IF NOT AVAIL classif-fisc THEN RETURN NO-APPLY.
        
        ASSIGN fi-c-class-fiscal:SCREEN-VALUE       = classif-fisc.class-fiscal
               fi-c-desc-class-fiscal:SCREEN-VALUE  = classif-fisc.descricao.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-class-fiscal W-Win2
ON LEAVE OF fi-c-class-fiscal IN FRAME F-Main /* Classif. Fiscal */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        IF LENGTH(TRIM(fi-c-class-fiscal:SCREEN-VALUE)) <> 10 THEN
            ASSIGN fi-c-class-fiscal:SCREEN-VALUE = TRIM(REPLACE(fi-c-class-fiscal:SCREEN-VALUE,".","")) + FILL("0", 8 - LENGTH(TRIM(REPLACE(fi-c-class-fiscal:SCREEN-VALUE,".","")))).
        FIND FIRST classif-fisc WHERE classif-fisc.class-fiscal = REPLACE(fi-c-class-fiscal:SCREEN-VALUE,".","") NO-LOCK NO-ERROR.
        IF AVAIL classif-fisc THEN
            ASSIGN fi-c-desc-class-fiscal:SCREEN-VALUE = classif-fisc.descricao.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-class-fiscal W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-c-class-fiscal IN FRAME F-Main /* Classif. Fiscal */
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-c-cod-depos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-cod-depos W-Win2
ON F5 OF fi-c-cod-depos IN FRAME F-Main /* Dep¢sito */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        
        W-Win2:SENSITIVE = NO.

        RUN dsc\ra\esp\esnfe200z06.w (OUTPUT pr-rowid).
        
        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        IF pr-rowid = ? THEN RETURN NO-APPLY.
        FIND FIRST deposito WHERE ROWID(deposito) = pr-rowid
                            NO-LOCK NO-ERROR.
        IF NOT AVAIL deposito THEN RETURN NO-APPLY.
        ASSIGN fi-c-cod-depos:SCREEN-VALUE = deposito.cod-depos.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-cod-depos W-Win2
ON LEAVE OF fi-c-cod-depos IN FRAME F-Main /* Dep¢sito */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST deposito WHERE deposito.cod-depos = input fi-c-cod-depos
                            NO-LOCK NO-ERROR.
        IF AVAIL deposito THEN ASSIGN fi-c-nome-depos:SCREEN-VALUE = deposito.nome.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-cod-depos W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-c-cod-depos IN FRAME F-Main /* Dep¢sito */
DO:
    APPLY "F5" TO SELF.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-c-cod-localiz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-cod-localiz W-Win2
ON F5 OF fi-c-cod-localiz IN FRAME F-Main /* Localizaá∆o */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        IF NOT AVAIL bf-nfe-nota-fiscal-rec THEN RETURN NO-APPLY.
        
        W-Win2:SENSITIVE = NO.

        RUN dsc\ra\esp\esnfe200z09.w (INPUT  bf-nfe-nota-fiscal-rec.cod-estabel, 
                                      INPUT (INPUT fi-c-cod-depos),
                                      OUTPUT pr-rowid).

        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        IF pr-rowid = ? THEN RETURN NO-APPLY.
        FIND FIRST localizacao WHERE ROWID(localizacao) = pr-rowid
                               NO-LOCK NO-ERROR.
        IF NOT AVAIL localizacao THEN RETURN NO-APPLY.
        ASSIGN fi-c-cod-localiz:SCREEN-VALUE = localizacao.cod-localiz.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-cod-localiz W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-c-cod-localiz IN FRAME F-Main /* Localizaá∆o */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-c-cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-cod-refer W-Win2
ON F5 OF fi-c-cod-refer IN FRAME F-Main /* Referància */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        W-Win2:SENSITIVE = NO.

        RUN dsc\ra\esp\esnfe200z05.w (OUTPUT pr-rowid).

        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        IF pr-rowid = ? THEN RETURN NO-APPLY.
        FIND FIRST referencia WHERE ROWID(referencia) = pr-rowid
                              NO-LOCK NO-ERROR.
        IF NOT AVAIL referencia THEN RETURN NO-APPLY.
            
        ASSIGN fi-c-cod-refer:SCREEN-VALUE  = referencia.cod-refer
               fi-c-desc-refer:SCREEN-VALUE = referencia.descricao.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-cod-refer W-Win2
ON LEAVE OF fi-c-cod-refer IN FRAME F-Main /* Referància */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST referencia WHERE referencia.cod-refer = input fi-c-cod-refer
                              NO-LOCK NO-ERROR.
        IF AVAIL referencia THEN ASSIGN fi-c-desc-refer:SCREEN-VALUE = referencia.descricao.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-cod-refer W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-c-cod-refer IN FRAME F-Main /* Referància */
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-c-conta-contabil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-conta-contabil W-Win2
ON F5 OF fi-c-conta-contabil IN FRAME F-Main /* Conta Cont†bil */
DO:
    DO WITH FRAME {&FRAME-NAME} :

        W-Win2:SENSITIVE = NO.

        RUN dsc\ra\esp\esnfe200z04.w (OUTPUT pr-rowid).


        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        IF pr-rowid = ? THEN RETURN NO-APPLY.
        FIND FIRST conta-contab WHERE ROWID(conta-contab) = pr-rowid
                                NO-LOCK NO-ERROR.
        IF NOT AVAIL conta-contab THEN RETURN NO-APPLY.
        ASSIGN fi-c-conta-contabil:SCREEN-VALUE         = conta-contab.conta-contabil
               fi-c-desc-conta-contabil:SCREEN-VALUE    = conta-contab.titulo.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-conta-contabil W-Win2
ON LEAVE OF fi-c-conta-contabil IN FRAME F-Main /* Conta Cont†bil */
DO:
    DO WITH FRAME {&FRAME-NAME}:

        FIND FIRST estabelec NO-LOCK
            WHERE estabelec.cod-estabel = bf-nfe-nota-fiscal-rec.cod-estabel NO-ERROR.
        IF AVAIL estabelec THEN DO:
            ASSIGN c-aux-conta-contabil = REPLACE(input fi-c-conta-contabil,".","").
            FIND FIRST conta-contab no-lock
                WHERE conta-contab.ep-codigo = estabelec.ep-codigo
                AND   conta-contab.conta-contabil = c-aux-conta-contabil NO-ERROR.
            IF AVAIL conta-contab THEN ASSIGN fi-c-desc-conta-contabil:SCREEN-VALUE = conta-contab.titulo.

        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-conta-contabil W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-c-conta-contabil IN FRAME F-Main /* Conta Cont†bil */
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-c-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-it-codigo W-Win2
ON F5 OF fi-c-it-codigo IN FRAME F-Main /* Item */
DO:
/*     W-Win2:SENSITIVE = NO. */

    /*RUN dsc\ra\esp\esnfe200z01.w (OUTPUT pr-rowid).*/
    {include/zoomvar.i &prog-zoom=inzoom/z02in172.w
                       &campo=fi-c-it-codigo
                       &campozoom=it-codigo
                       &frame="F-Main"}
                       
/*     W-Win2:MOVE-TO-TOP().   */
/*     W-Win2:SENSITIVE = YES. */

    /*IF pr-rowid = ? THEN RETURN NO-APPLY.*/
    IF fi-c-it-codigo:SCREEN-VALUE <> "" THEN
        FIND item WHERE ITEM.it-codigo = fi-c-it-codigo:SCREEN-VALUE NO-LOCK NO-ERROR.
    /*FIND item WHERE ROWID(item) = pr-rowid NO-LOCK NO-ERROR.*/
    IF NOT AVAILABLE item THEN RETURN NO-APPLY.
    ASSIGN fi-c-it-codigo:SCREEN-VALUE = item.it-codigo
           fi-c-desc-item:SCREEN-VALUE = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-it-codigo W-Win2
ON LEAVE OF fi-c-it-codigo IN FRAME F-Main /* Item */
DO:
  RUN pi-carrega-infs-item.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-it-codigo W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-c-it-codigo IN FRAME F-Main /* Item */
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-c-natur-oper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-natur-oper W-Win2
ON F5 OF fi-c-natur-oper IN FRAME F-Main /* Nat. Oper */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        W-Win2:SENSITIVE = NO.

        RUN dsc\ra\esp\esnfe200z02.w (OUTPUT pr-rowid).

        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        IF pr-rowid = ? THEN RETURN NO-APPLY.
        FIND FIRST natur-oper WHERE ROWID(natur-oper) = pr-rowid
                              NO-LOCK NO-ERROR.
        IF NOT AVAIL natur-oper THEN RETURN NO-APPLY.

        ASSIGN fi-c-natur-oper:SCREEN-VALUE     = natur-oper.nat-operacao
               fi-c-denominacao:SCREEN-VALUE    = natur-oper.denominacao
               fi-c-esp-nat:SCREEN-VALUE        = natur-oper.especie-doc.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-natur-oper W-Win2
ON LEAVE OF fi-c-natur-oper IN FRAME F-Main /* Nat. Oper */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        
        FIND FIRST natur-oper WHERE natur-oper.nat-operacao = input fi-c-natur-oper
                              NO-LOCK NO-ERROR.
        IF AVAIL natur-oper THEN DO:
            ASSIGN fi-c-denominacao:SCREEN-VALUE = natur-oper.denominacao
                   fi-c-esp-nat:SCREEN-VALUE     = natur-oper.especie-doc.

            IF natur-oper.tipo-compra = 4 THEN DO:
                ASSIGN fi-c-cod-depos  :SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
                       fi-c-cod-localiz:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
                       fi-c-cod-depos  :SENSITIVE IN FRAME {&FRAME-NAME} = NO
                       fi-c-cod-localiz:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

            END.
            ELSE DO:
                ASSIGN fi-c-cod-depos  :SENSITIVE IN FRAME {&FRAME-NAME} = YES
                       fi-c-cod-localiz:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
            END.
            /* --- Verifica Tipo da Nota para Docto Referenciado e Nota Fiscal --- */

            FIND FIRST nfe-it-param-rec no-lock
                    WHERE nfe-it-param-rec.cod-parametro      = "param_manutencao_itens"
                    AND   nfe-it-param-rec.cod-item-parametro = 'deposito' NO-ERROR.
                    
            IF  nfe-it-param-rec.valor-1-item-parametro = "NAO" THEN
                fi-c-cod-depos  :SENSITIVE IN FRAME {&FRAME-NAME} = NO.
            
            RUN pi-carrega-nota (INPUT natur-oper.nat-operacao).
                            
            /*Operaá∆o Triangular*/
            IF fi-c-natur-oper:SCREEN-VALUE <> "" THEN DO:
                
                IF AVAIL natur-oper AND natur-oper.log-oper-triang = YES THEN DO:

                    FIND FIRST nfe-it-nota-fisc-rec NO-LOCK
                        WHERE rowid(nfe-it-nota-fisc-rec) = p_rowid_item NO-ERROR.

                    FIND FIRST nfe-nota-fiscal-rec OF nfe-it-nota-fisc-rec NO-LOCK NO-ERROR.

                    /*
                    FIND FIRST emitente  no-lock
                        WHERE emitente.cgc = nfe-nota-fiscal-rec.cgc NO-ERROR.*/

                    FIND emitente WHERE
                         emitente.nome-abrev = nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.


                    ASSIGN fi-i-cod-emite-terc :sensitive = yes.
                           fi-c-nat-terc       :sensitive = yes.
                           fi-c-ser-terc       :sensitive = yes.
                           fi-i-nro-docto-terc :sensitive = yes.
                           fi-de-seq-terc      :sensitive = yes.

/*                     ASSIGN fi-i-cod-emite-terc :screen-value = if fi-i-cod-emite-terc :screen-value = "0" then string(emitente.cod-emitente)  else fi-i-cod-emite-terc :screen-value  */
/*                            fi-c-nat-terc       :screen-value = if fi-c-nat-terc       :screen-value = ""  then natur-oper.nat-vinculada       else fi-c-nat-terc       :screen-value  */
/*                            fi-c-ser-terc       :screen-value = if fi-c-ser-terc       :screen-value = ""  then nfe-nota-fiscal-rec.ide-Serie  else fi-c-ser-terc       :screen-value  */
/*                            fi-i-nro-docto-terc :screen-value = if fi-i-nro-docto-terc :screen-value = ""  then nfe-nota-fiscal-rec.ide-nnf    else fi-i-nro-docto-terc :screen-value  */
/*                            fi-de-seq-terc      :screen-value = if fi-de-seq-terc      :screen-value = "0" then STRING(INPUT fi-i-seq * 10)    else fi-de-seq-terc      :screen-value. */

                END.
                ELSE DO:
                    ASSIGN fi-i-cod-emite-terc :sensitive = NO
                           fi-c-nat-terc       :sensitive = NO
                           fi-c-ser-terc       :sensitive = NO
                           fi-i-nro-docto-terc :sensitive = NO
                           fi-de-seq-terc      :sensitive = NO
                           fi-i-cod-emite-terc :screen-value = "0" 
                           fi-c-nat-terc       :screen-value = ""  
                           fi-c-ser-terc       :screen-value = ""  
                           fi-i-nro-docto-terc :screen-value = ""  
                           fi-de-seq-terc      :screen-value = "0" .

                END.

            END.
            ELSE DO:
                
                ASSIGN fi-i-cod-emite-terc :sensitive = no
                       fi-c-nat-terc       :sensitive = no
                       fi-c-ser-terc       :sensitive = no
                       fi-i-nro-docto-terc :sensitive = no
                       fi-de-seq-terc      :sensitive = no
                       fi-i-cod-emite-terc :screen-value = "0"  
                       fi-c-nat-terc       :screen-value = ""   
                       fi-c-ser-terc       :screen-value = ""   
                       fi-i-nro-docto-terc :screen-value = ""   
                       fi-de-seq-terc      :screen-value = "0" .
            END.

        END.
        ELSE DO:
        
            ASSIGN fi-c-denominacao:SCREEN-VALUE = ""
                   fi-c-esp-nat:SCREEN-VALUE     = "".

            ASSIGN fi-i-cod-emite-terc :sensitive = no.
                           fi-c-nat-terc       :sensitive = no.
                           fi-c-ser-terc       :sensitive = no.
                           fi-i-nro-docto-terc :sensitive = no.
                           fi-de-seq-terc      :sensitive = no.

        END.

/*         RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
        RUN pi_valida_upc IN h_bonfe001 (INPUT "esnfe200e",
                                         INPUT 'DisplayFields',      
                                         INPUT 'CONTAINER',         
                                         INPUT FRAME F-Main:HANDLE,
                                         INPUT FRAME F-Main:HANDLE,
                                         INPUT '',                  
                                         INPUT p_rowid_item).
/*         DELETE PROCEDURE h_bonfe001. */

        
        APPLY "LEAVE" TO fi-c-class-fiscal.
        APPLY "LEAVE" TO fi-c-conta-contabil.
        APPLY "LEAVE" TO fi-c-cod-refer.
        APPLY "LEAVE" TO fi-c-cod-depos.
        APPLY "LEAVE" TO fi-c-cod-localiz.

        
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-natur-oper W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-c-natur-oper IN FRAME F-Main /* Nat. Oper */
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-c-un
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-un W-Win2
ON F5 OF fi-c-un IN FRAME F-Main
DO:
    do with frame {&frame-name} :
        W-Win2:SENSITIVE = NO.

        RUN dsc\ra\esp\esnfe200z12.w (OUTPUT pr-rowid).

        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        IF pr-rowid = ? THEN RETURN NO-APPLY.
        FIND FIRST tab-unidade WHERE ROWID(tab-unidade) = pr-rowid
                               NO-LOCK NO-ERROR.
        IF NOT AVAIL tab-unidade THEN RETURN NO-APPLY.
        ASSIGN fi-c-un:SCREEN-VALUE = tab-unidade.un.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-un W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-c-un IN FRAME F-Main
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-c-un-forn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-un-forn W-Win2
ON F5 OF fi-c-un-forn IN FRAME F-Main
DO:
    do with frame {&frame-name} :
        W-Win2:SENSITIVE = NO.

        RUN dsc\ra\esp\esnfe200z12.w (OUTPUT pr-rowid).

        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        IF pr-rowid = ? THEN RETURN NO-APPLY.
        FIND FIRST tab-unidade WHERE ROWID(tab-unidade) = pr-rowid
                               NO-LOCK NO-ERROR.
        IF NOT AVAIL tab-unidade THEN RETURN NO-APPLY.
        ASSIGN fi-c-un-forn:SCREEN-VALUE = tab-unidade.un.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-un-forn W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-c-un-forn IN FRAME F-Main
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-unid-negoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-unid-negoc W-Win2
ON F5 OF fi-cod-unid-negoc IN FRAME F-Main /* Unid Negocio */
DO:
    
    
/*     RUN getZoomConta in h-boin176 (OUTPUT c-zoom-cod-conta ,  */
/*                                    OUTPUT c-zoom-desc-conta). */


    /*{include/zoomvar.i &prog-zoom=inzoom/z01in745.w
                       &campo=fi-cod-unid-negoc
                       &campozoom=cod-unid-negoc
                       &frame=F-Main
                       &campo2=fi-unid-neg-descri
                       &campozoom2=des-unid-negoc
                       &frame2=F-main}*/
                       
    

   {method/zoomfields.i &ProgramZoom="inzoom/z01in745.w"
                        &FieldZoom1="cod-unid-negoc"
                        &FieldScreen1="fi-cod-unid-negoc"
                        &Frame1="F-Main"
                        &FieldZoom2="des-unid-negoc"
                        &FieldScreen2="fi-unid-neg-descri"
                        &Frame2="F-Main"
                        &EnableImplant="NO"}
   

    
    

    
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-unid-negoc W-Win2
ON LEAVE OF fi-cod-unid-negoc IN FRAME F-Main /* Unid Negocio */
DO:
    DO WITH FRAME {&FRAME-NAME}:

        FIND FIRST unid-negoc NO-LOCK
            WHERE unid-negoc.cod-unid-negoc = fi-cod-unid-negoc:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.
        IF AVAIL unid-negoc THEN DO:
            
            ASSIGN fi-unid-neg-descri:SCREEN-VALUE IN FRAME {&FRAME-NAME} = unid-negoc.des-unid-negoc.

        END.
        ELSE DO:

            ASSIGN fi-unid-neg-descri:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

        END.                                                                   
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-unid-negoc W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-cod-unid-negoc IN FRAME F-Main /* Unid Negocio */
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo W-Win2
ON F5 OF fi-ct-codigo IN FRAME F-Main /* Conta */
DO:
    
    
/*     RUN getZoomConta in h-boin176 (OUTPUT c-zoom-cod-conta ,  */
/*                                    OUTPUT c-zoom-desc-conta). */


    IF '{&bf_dis_versao_ems}' >= '2.08' THEN DO:
        RUN dsc/ra/esp5/esra012.p(INPUT "conta",
                                  OUTPUT c-zoom-cod-conta, 
                                  OUTPUT c-zoom-desc-conta).
    
    
        
            assign fi-ct-codigo     :screen-value in frame {&FRAME-NAME} = c-zoom-cod-conta
                   fi-ct-codigo-desc:screen-value in frame {&FRAME-NAME} = c-zoom-desc-conta.
    
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo W-Win2
ON LEAVE OF fi-ct-codigo IN FRAME F-Main /* Conta */
DO:
    DO WITH FRAME {&FRAME-NAME}:

        ASSIGN c-desc-conta = "".


        IF '{&bf_dis_versao_ems}' >= '2.08' THEN DO:
            RUN dsc/ra/esp5/esra011.p (INPUT  input fi-ct-codigo,
                                       INPUT  "conta",
                                       INPUT  p_rowid_item,
                                       INPUT  bf-nfe-nota-fiscal-rec.cod-estabel,
                                       OUTPUT l-ccusto,
                                       OUTPUT c-desc-conta).
    
            ASSIGN fi-ct-codigo-desc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-desc-conta.
    
            IF l-ccusto = YES THEN DO:
                
                ASSIGN fi-sc-codigo:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
            END.
            ELSE DO:
                ASSIGN fi-sc-codigo:SENSITIVE IN FRAME {&FRAME-NAME}         = NO
                       fi-sc-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = ""
                       fi-sc-codigo-desc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    
            END.
        END.


       
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-ct-codigo IN FRAME F-Main /* Conta */
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-de-pre-unit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-de-pre-unit W-Win2
ON VALUE-CHANGED OF fi-de-pre-unit IN FRAME F-Main /* Unit†rio */
DO:
  RUN pi-nosso-preco.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-de-preco-total
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-de-preco-total W-Win2
ON LEAVE OF fi-de-preco-total IN FRAME F-Main
DO:
    do with frame {&frame-name} :
        APPLY "LEAVE" TO fi-de-quantidade.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-de-quantidade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-de-quantidade W-Win2
ON LEAVE OF fi-de-quantidade IN FRAME F-Main /* Nossa Qtde */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        IF (INPUT fi-de-preco-total / INPUT fi-de-quantidade) >= 0 THEN
            ASSIGN fi-de-pre-unit:SCREEN-VALUE = STRING(INPUT fi-de-preco-total / INPUT fi-de-quantidade).
        ELSE
            ASSIGN fi-de-pre-unit:SCREEN-VALUE = "0".

        RUN pi-nosso-preco.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-de-total-nosso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-de-total-nosso W-Win2
ON LEAVE OF fi-de-total-nosso IN FRAME F-Main /* Preáo Total */
DO:
    do with frame {&frame-name} :
        APPLY "LEAVE" TO fi-de-quantidade.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-i-cfop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-cfop W-Win2
ON F5 OF fi-i-cfop IN FRAME F-Main /* CFOP */
DO:
    /*DO WITH FRAME {&FRAME-NAME} :
        W-Win2:SENSITIVE = NO.

        RUN dsc\ra\esp\esnfe200z08.w (OUTPUT pr-rowid).

        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        IF pr-rowid = ? THEN RETURN NO-APPLY.
        &IF '{&bf_dis_versao_ems}' >= '2.05':U &THEN
            FIND FIRST cfop-natur WHERE ROWID(cfop-natur) = pr-rowid
                                  NO-LOCK NO-ERROR.
            IF NOT AVAIL cfop-natur THEN RETURN NO-APPLY.
            ASSIGN fi-i-cfop:SCREEN-VALUE = cfop-natur.cod-cfop.
        &ELSE
            FIND FIRST ped-curva WHERE ROWID(ped-curva) = pr-rowid
                                 NO-LOCK NO-ERROR.
            IF NOT AVAIL ped-curva THEN RETURN NO-APPLY.
            ASSIGN fi-i-cfop:SCREEN-VALUE = ped-curva.it-codigo.
        &ENDIF
    END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-cfop W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-i-cfop IN FRAME F-Main /* CFOP */
DO:
  /*APPLY "F5" TO SELF.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-i-cod-emite-terc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-cod-emite-terc W-Win2
ON F5 OF fi-i-cod-emite-terc IN FRAME F-Main /* Emit.Entrega */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/Z02AD098.w
                       &campo = fi-i-cod-emite-terc
                       &campozoom = cod-emitente
                       &frame="F-Main"}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-cod-emite-terc W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-i-cod-emite-terc IN FRAME F-Main /* Emit.Entrega */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-i-nr-ord-produ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-nr-ord-produ W-Win2
ON F5 OF fi-i-nr-ord-produ IN FRAME F-Main /* Ordem Produá∆o */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        W-Win2:SENSITIVE = YES.

        RUN dsc\ra\esp\esnfe200z14.w (INPUT p_rowid_item,
                               OUTPUT pr-rowid).

        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        FIND FIRST ord-prod WHERE ROWID(ord-prod) = pr-rowid
                            NO-LOCK NO-ERROR.
        IF AVAIL ord-prod THEN
            ASSIGN fi-i-nr-ord-produ:SCREEN-VALUE = STRING(ord-prod.nr-ord-produ).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-nr-ord-produ W-Win2
ON LEAVE OF fi-i-nr-ord-produ IN FRAME F-Main /* Ordem Produá∆o */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        FIND FIRST ord-prod WHERE ord-prod.nr-ord-produ = INPUT fi-i-nr-ord-produ
                            NO-LOCK NO-ERROR.
        IF AVAIL ord-prod THEN
            ASSIGN fi-c-conta-contabil:SCREEN-VALUE = STRING(ord-prod.conta-ordem).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-nr-ord-produ W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-i-nr-ord-produ IN FRAME F-Main /* Ordem Produá∆o */
DO:
    APPLY "F5" TO SELF.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-i-num-ordem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-num-ordem W-Win2
ON F5 OF fi-i-num-ordem IN FRAME F-Main /* Ordem Compra */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        W-Win2:SENSITIVE = YES.

        RUN dsc\ra\esp\esnfe200z11.w (INPUT p_rowid_item,
                                      INPUT 2,
                                      INPUT (INPUT fi-c-it-codigo),
                                      OUTPUT pr-rowid,
                                      OUTPUT pi-num-parc).

        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        FIND FIRST ordem-compra WHERE ROWID(ordem-compra) = pr-rowid
                                NO-LOCK NO-ERROR.
        IF AVAIL ordem-compra THEN DO:
            ASSIGN fi-i-num-pedido:SCREEN-VALUE = STRING(ordem-compra.num-pedido)
                   fi-i-num-ordem:SCREEN-VALUE  = STRING(ordem-compra.numero-ordem)
                   fi-c-it-codigo:SCREEN-VALUE  = ordem-compra.it-codigo
                   fi-i-parcela-oc:SCREEN-VALUE = pi-num-parc.

            APPLY "LEAVE" TO fi-c-it-codigo.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-num-ordem W-Win2
ON LEAVE OF fi-i-num-ordem IN FRAME F-Main /* Ordem Compra */
DO:

    DO WITH FRAME {&FRAME-NAME} :
        
        FIND FIRST ordem-compra NO-LOCK 
            WHERE ordem-compra.numero-ordem = INPUT fi-i-num-ordem
            AND   ordem-compra.situacao     = 2 /* --- Confirmada --- */ NO-ERROR.
        
        IF AVAIL ordem-compra THEN DO:
            
            ASSIGN fi-i-num-pedido:SCREEN-VALUE     = STRING(ordem-compra.num-pedido)
                   fi-c-conta-contabil:SCREEN-VALUE = ordem-compra.conta-contabil
                   fi-i-nr-ord-produ:SCREEN-VALUE   = IF fi-i-nr-ord-produ:SCREEN-VALUE = '0' THEN STRING(ordem-compra.ordem-servic) ELSE fi-i-nr-ord-produ:SCREEN-VALUE
                   fi-de-desconto-ordem:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = string(ordem-compra.valor-descto).
            ASSIGN c-narrativa                    = ordem-compra.narrativa
                   fi-de-pre-ordem:SCREEN-VALUE   = STRING(ordem-compra.preco-unit).

            &IF '{&bf_dis_versao_ems}' > '2.07' &THEN
               ASSIGN fi-ct-codigo:SCREEN-VALUE      = ordem-compra.ct-codigo
                      fi-sc-codigo:SCREEN-VALUE      = ordem-compra.sc-codigo 
                      fi-cod-unid-negoc:SCREEN-VALUE = ordem-compra.cod-unid-negoc.

               APPLY "LEAVE" TO fi-ct-codigo.
               APPLY "LEAVE" TO fi-sc-codigo.
               APPLY "LEAVE" TO fi-cod-unid-negoc.
            &ENDIF
            
                
            ASSIGN de-quant-variacao = 0.
            /* --- Valida Variacao --- */
            
            FIND FIRST param-re no-lock
                WHERE param-re.usuario = c-seg-usuario NO-ERROR.
                
            IF AVAIL param-re            AND
               param-re.aceita-var = YES THEN DO:
                
                ASSIGN l-erro = NO.
                    
                FIND FIRST item-uni-estab no-lock
                    WHERE item-uni-estab.it-codigo    = INPUT fi-c-it-codigo
                    AND item-uni-estab.cod-estabel  = bf-nfe-nota-fiscal-rec.cod-estabel NO-ERROR.
                    
                IF AVAIL item-uni-estab THEN DO:
                    
                    IF item-uni-estab.var-qtd-re <> 0 THEN DO:
                        
                        ASSIGN de-quant-variacao = INPUT fi-de-quantidade * (item-uni-estab.var-qtd-re / 100) /* variaá∆o percentual */
                               l-erro            = YES.
                    END.
                    
                    IF item-uni-estab.lim-var-qtd <> 0 THEN DO:
                        
                        ASSIGN de-quant-variacao    = item-uni-estab.lim-var-qtd /* variaá∆o qtde */
                               l-erro               = YES.
                    END.

                    IF l-erro = NO THEN DO:
                        
                        FIND FIRST item-mat no-lock
                            WHERE item-mat.it-codigo = INPUT fi-c-it-codigo NO-ERROR.
                            
                        IF AVAIL item-mat THEN DO:
                            
                            IF item-mat.var-qtd-re <> 0 THEN DO:
                                
                                ASSIGN de-quant-variacao = INPUT fi-de-quantidade * (item-mat.var-qtd-re / 100). /* variaá∆o percentual */

                            END.
                                
                            IF item-mat.lim-var-qtd <> 0 THEN DO:
                                
                                ASSIGN de-quant-variacao = item-mat.lim-var-qtd. /* variaá∆o qtde */
                            END.

                        END.

                    END.

                END.

            END.
                
            FIND FIRST prazo-compra NO-LOCK 
                WHERE prazo-compra.numero-ordem     = INPUT fi-i-num-ordem
                AND   prazo-compra.situacao         = 2 /* --- Confirmada --- */
                AND  (prazo-compra.quant-saldo - 
                      prazo-compra.dec-1)          >= (INPUT fi-de-quantidade - de-quant-variacao) NO-ERROR.
                
            IF AVAIL prazo-compra AND fi-i-parcela-oc:SCREEN-VALUE = "" OR fi-i-parcela-oc:SCREEN-VALUE = '0' THEN 
                ASSIGN fi-i-parcela-oc:SCREEN-VALUE = STRING(prazo-compra.parcela).
                
            FIND FIRST ord-prod NO-LOCK 
                WHERE ord-prod.nr-ord-produ = ordem-compra.ordem-servic NO-ERROR.
                
            IF AVAIL ord-prod THEN 
                ASSIGN fi-c-conta-contabil:SCREEN-VALUE = STRING(ord-prod.conta-ordem).


            IF fi-c-it-codigo:screen-value = "" THEN DO:                         
                ASSIGN fi-c-it-codigo:screen-value      = ordem-compra.it-codigo.
            END.
            RUN pi-carrega-infs-item.

        END.
        ELSE 
            ASSIGN fi-de-pre-ordem:SCREEN-VALUE = '0'
                   fi-de-desconto-ordem:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = '0'.

        ASSIGN  fi-i-num-ordem = INPUT fi-i-num-ordem.
        ASSIGN  fi-i-parcela-oc = INPUT fi-i-parcela-oc.
        RUN pi-origem-ordem.
        RUN pi-nosso-preco.
            
    END.

    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-num-ordem W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-i-num-ordem IN FRAME F-Main /* Ordem Compra */
DO:
    APPLY "F5" TO SELF.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-i-num-pedido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-num-pedido W-Win2
ON F5 OF fi-i-num-pedido IN FRAME F-Main /* Pedido */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        W-Win2:SENSITIVE = YES.

        RUN dsc\ra\esp\esnfe200z11.w (INPUT p_rowid_item,
                                      INPUT 1,
                                      INPUT (INPUT fi-c-it-codigo),
                                      OUTPUT pr-rowid,
                                      OUTPUT pi-num-parc).

        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        FIND FIRST ordem-compra WHERE ROWID(ordem-compra) = pr-rowid
                                NO-LOCK NO-ERROR.
        IF AVAIL ordem-compra THEN DO:
            ASSIGN fi-i-num-pedido:SCREEN-VALUE = STRING(ordem-compra.num-pedido)
                   fi-i-num-ordem:SCREEN-VALUE  = STRING(ordem-compra.numero-ordem)
                   fi-c-it-codigo:SCREEN-VALUE  = ordem-compra.it-codigo
                   fi-i-parcela-oc:SCREEN-VALUE = pi-num-parc.

            APPLY "LEAVE" TO fi-c-it-codigo.

            IF bf-nfe-it-nota-fisc-rec.item-cod-depos <> c-deposito THEN DO: /*Se o depÛsito do item for diferente do deposito sugerido*/
                FIND FIRST nfe-it-param-rec
                    WHERE nfe-it-param-rec.cod-parametro          = "param_global"
                      AND nfe-it-param-rec.cod-item-parametro     = "alerta_deposito"
                      AND nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-LOCK NO-ERROR.

                IF AVAIL nfe-it-param-rec THEN DO: /*Se n∆o estiver esse parÉmetro criado, ent∆o nunca mostrar† a mensagem de confirmaá∆o de troca de dep¢sito*/
                    
                   MESSAGE "Novos dados foram encontrados para o item " SKIP
                           "Deposito Atual: " bf-nfe-it-nota-fisc-rec.item-cod-depos SKIP
                           "Deposito Novo: " c-deposito SKIP
                           "Confirma alteracao?"
                       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-dep AS LOGICAL.
        
                   IF l-dep THEN 
                       ASSIGN fi-c-cod-depos:SCREEN-VALUE   = c-deposito.
                END.
                ELSE
                    ASSIGN fi-c-cod-depos:SCREEN-VALUE   = c-deposito.
            END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-num-pedido W-Win2
ON LEAVE OF fi-i-num-pedido IN FRAME F-Main /* Pedido */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        IF INPUT fi-i-num-ordem = 0 THEN 
        DO:

            FOR EACH ordem-compra NO-LOCK WHERE 
                     ordem-compra.it-codigo  = INPUT fi-c-it-codigo  AND 
                     ordem-compra.num-pedido = INPUT fi-i-num-pedido AND 
                     ordem-compra.situacao   = 2,
                EACH prazo-compra WHERE 
                     prazo-compra.numero-ordem = ordem-compra.numero-ordem  AND 
                     prazo-compra.situacao     = 2 /* --- Confirmada --- */ AND 
                    (prazo-compra.quant-saldo - prazo-compra.dec-1) > 0    NO-LOCK 
                    BY prazo-compra.data-entrega:

                    ASSIGN fi-i-num-ordem:SCREEN-VALUE = STRING(prazo-compra.numero-ordem).

                    LEAVE.


            END.

        END.

        APPLY "LEAVE" TO fi-i-num-ordem.

/*         APPLY "LEAVE" TO fi-c-it-codigo. */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-num-pedido W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-i-num-pedido IN FRAME F-Main /* Pedido */
DO:
    APPLY "F5" TO SELF.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-i-parcela-oc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-parcela-oc W-Win2
ON F5 OF fi-i-parcela-oc IN FRAME F-Main /* Parcela */
DO:

    DO WITH FRAME {&FRAME-NAME} :
        W-Win2:SENSITIVE = YES.

        RUN dsc\ra\esp\esnfe200z11.w (INPUT p_rowid_item,
                                      INPUT 2,
                                      INPUT (INPUT fi-c-it-codigo),
                                      OUTPUT pr-rowid,
                                      OUTPUT pi-num-parc).

        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        FIND FIRST ordem-compra WHERE ROWID(ordem-compra) = pr-rowid
                                NO-LOCK NO-ERROR.
        IF AVAIL ordem-compra THEN DO:
            ASSIGN fi-i-num-pedido:SCREEN-VALUE = STRING(ordem-compra.num-pedido)
                   fi-i-num-ordem:SCREEN-VALUE  = STRING(ordem-compra.numero-ordem)
                   fi-c-it-codigo:SCREEN-VALUE  = ordem-compra.it-codigo
                   fi-i-parcela-oc:SCREEN-VALUE = pi-num-parc.

            APPLY "LEAVE" TO fi-c-it-codigo.
        END.
    END.
   /* DO WITH FRAME {&FRAME-NAME} :
        W-Win2:SENSITIVE = YES.

        RUN dsc\ra\esp\esnfe200z13.w (INPUT p_rowid_item,
                                      INPUT (INPUT fi-i-num-pedido),
                                      INPUT (INPUT fi-i-num-ordem),
                                      OUTPUT pr-rowid).

        W-Win2:MOVE-TO-TOP().
        W-Win2:SENSITIVE = YES.

        FIND FIRST prazo-compra WHERE ROWID(prazo-compra) = pr-rowid
                                NO-LOCK NO-ERROR.
        IF AVAIL prazo-compra THEN
            ASSIGN fi-i-parcela-oc:SCREEN-VALUE = STRING(prazo-compra.parcela).
    END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-parcela-oc W-Win2
ON LEAVE OF fi-i-parcela-oc IN FRAME F-Main /* Parcela */
DO:
   ASSIGN  fi-i-num-ordem = INPUT fi-i-num-ordem.
   ASSIGN  fi-i-parcela-oc = INPUT fi-i-parcela-oc.
   RUN pi-origem-ordem.
   RUN pi-nosso-preco.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-parcela-oc W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-i-parcela-oc IN FRAME F-Main /* Parcela */
DO:
    APPLY "F5" TO SELF.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nro-docto-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nro-docto-refer W-Win2
ON LEAVE OF fi-nro-docto-refer IN FRAME F-Main /* Nr. Documento */
DO:
  RUN pi-sugestao-conta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-sc-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sc-codigo W-Win2
ON F5 OF fi-sc-codigo IN FRAME F-Main /* Centro Custo */
DO:
    IF '{&bf_dis_versao_ems}' >= '2.08' THEN DO:
        RUN dsc/ra/esp5/esra012.p(INPUT "ccusto",
                                  OUTPUT c-zoom-cod-conta, 
                                  OUTPUT c-zoom-desc-conta).
    
    
        
            assign fi-sc-codigo     :screen-value in frame {&FRAME-NAME} = c-zoom-cod-conta
                   fi-sc-codigo-desc:screen-value in frame {&FRAME-NAME} = c-zoom-desc-conta.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sc-codigo W-Win2
ON LEAVE OF fi-sc-codigo IN FRAME F-Main /* Centro Custo */
DO:
  DO WITH FRAME {&FRAME-NAME}:

        ASSIGN c-desc-conta = "".


        IF '{&bf_dis_versao_ems}' >= '2.08' THEN DO:
        
            RUN dsc/ra/esp5/esra011.p (INPUT  input fi-sc-codigo,
                                       INPUT  "ccusto",
                                       INPUT  p_rowid_item,
                                       INPUT  bf-nfe-nota-fiscal-rec.cod-estabel,
                                       OUTPUT l-ccusto,
                                       OUTPUT c-desc-conta).
    
            ASSIGN fi-sc-codigo-desc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-desc-conta.
        END.
        
       
    END.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sc-codigo W-Win2
ON MOUSE-SELECT-DBLCLICK OF fi-sc-codigo IN FRAME F-Main /* Centro Custo */
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-seq-docto-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-seq-docto-refer W-Win2
ON LEAVE OF fi-seq-docto-refer IN FRAME F-Main /* Seq. Docto */
DO:
  RUN pi-sugestao-conta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-serie-docto-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-serie-docto-refer W-Win2
ON LEAVE OF fi-serie-docto-refer IN FRAME F-Main /* SÇrie Docto */
DO:
  RUN pi-sugestao-conta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-fifo-oc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-fifo-oc W-Win2
ON VALUE-CHANGED OF tg-fifo-oc IN FRAME F-Main /* FIFO */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        IF INPUT tg-fifo-oc = YES THEN DO:
            ASSIGN fi-i-num-pedido:SENSITIVE    = NO
                   fi-i-num-ordem:SENSITIVE     = NO
                   fi-i-parcela-oc:SENSITIVE    = NO
                   bt-ordem:SENSITIVE           = NO
                   fi-i-num-pedido:SCREEN-VALUE = "0"
                   fi-i-num-ordem:SCREEN-VALUE  = "0"
                   fi-i-parcela-oc:SCREEN-VALUE = "0".
        END.
        ELSE DO:
            ASSIGN fi-i-num-pedido:SENSITIVE = YES
                   fi-i-num-ordem:SENSITIVE  = YES
                   fi-i-parcela-oc:SENSITIVE = YES
                   bt-ordem:SENSITIVE        = YES.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win2 


/* ***************************  Main Block  *************************** */

                  
  
RUN pi-inicializa-bo.
RUN pi-inicializa.
RUN pi-nosso-preco.
 

fi-c-it-codigo:LOAD-MOUSE-POINTER       ('IMAGE/lupa.cur').
fi-i-num-pedido:LOAD-MOUSE-POINTER      ('IMAGE/lupa.cur').
fi-i-num-ordem:LOAD-MOUSE-POINTER       ('IMAGE/lupa.cur').
fi-i-parcela-oc:LOAD-MOUSE-POINTER      ('IMAGE/lupa.cur').
fi-c-un-forn:LOAD-MOUSE-POINTER         ('IMAGE/lupa.cur').
fi-c-un:LOAD-MOUSE-POINTER              ('IMAGE/lupa.cur').
/*fi-i-cfop:LOAD-MOUSE-POINTER            ('IMAGE/lupa.cur').*/
fi-c-natur-oper:LOAD-MOUSE-POINTER      ('IMAGE/lupa.cur').
fi-c-class-fiscal:LOAD-MOUSE-POINTER    ('IMAGE/lupa.cur').
fi-c-conta-contabil:LOAD-MOUSE-POINTER  ('IMAGE/lupa.cur').
fi-c-cod-refer:LOAD-MOUSE-POINTER       ('IMAGE/lupa.cur').
fi-c-cod-depos:LOAD-MOUSE-POINTER       ('IMAGE/lupa.cur').
fi-c-cod-localiz:LOAD-MOUSE-POINTER     ('IMAGE/lupa.cur').
fi-i-nr-ord-produ:LOAD-MOUSE-POINTER    ('IMAGE/lupa.cur').
fi-i-cod-emite-terc:LOAD-MOUSE-POINTER    ('IMAGE/lupa.cur').
fi-ct-codigo:LOAD-MOUSE-POINTER       ('IMAGE/lupa.cur').
fi-sc-codigo:LOAD-MOUSE-POINTER       ('IMAGE/lupa.cur').


{dsc/ra/include/i-versao-ra.i 2}

    

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win2  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win2  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win2)
  THEN DELETE WIDGET W-Win2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win2  _DEFAULT-ENABLE
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
  DISPLAY fi-ordem-xml fi-i-seq fi-c-it-codigo-forn fi-c-it-codigo 
          fi-c-desc-item fi-i-cfop fi-c-natur-oper fi-c-denominacao fi-c-esp-nat 
          fi-de-quantidade fi-c-un fi-de-qt-do-forn fi-c-un-forn fi-de-pre-unit 
          fi-de-pre-unit-xml fi-i-num-pedido fi-de-pre-ordem fi-i-num-ordem 
          fi-de-desconto-ordem fi-de-desconto-xml fi-i-parcela-oc 
          fi-de-preco-total tg-fifo-oc fi-i-cod-emite-terc fi-c-ser-terc 
          fi-nro-docto-refer fi-c-nat-terc fi-i-nro-docto-terc 
          fi-serie-docto-refer fi-de-seq-terc fi-seq-docto-refer 
          fi-c-class-fiscal fi-c-desc-class-fiscal fi-c-conta-contabil 
          fi-c-desc-conta-contabil fi-c-cod-refer fi-c-desc-refer 
          fi-i-nr-ord-produ fi-c-cod-depos fi-c-nome-depos fi-c-cod-localiz 
          fi-c-ser-lote fi-da-dt-vali-lote fi-c-text-3 fi-c-text fi-c-text-2 
          fi-de-total-nosso fi-de-frete fi-ct-codigo fi-ct-codigo-desc 
          fi-sc-codigo fi-sc-codigo-desc fi-cod-unid-negoc fi-unid-neg-descri 
      WITH FRAME F-Main IN WINDOW W-Win2.
  ENABLE bt-impostos fi-ordem-xml fi-c-it-codigo fi-c-natur-oper 
         fi-de-quantidade fi-c-un fi-de-qt-do-forn fi-c-un-forn fi-de-pre-unit 
         fi-i-num-pedido fi-i-num-ordem fi-i-parcela-oc fi-de-preco-total 
         bt-ordem fi-i-cod-emite-terc fi-c-ser-terc fi-nro-docto-refer 
         fi-c-nat-terc fi-i-nro-docto-terc fi-serie-docto-refer fi-de-seq-terc 
         fi-seq-docto-refer fi-c-class-fiscal fi-c-conta-contabil 
         fi-c-cod-refer fi-i-nr-ord-produ bt-narrativa fi-c-cod-depos 
         fi-c-cod-localiz fi-c-ser-lote fi-da-dt-vali-lote bt-ser-lote btn-ok 
         btn-cancelar fi-c-text-3 fi-c-text fi-c-text-2 fi-ct-codigo 
         fi-cod-unid-negoc RECT-34 IMAGE-19 c-descricao-3 c-descricao-4 RECT-28 
         c-descricao c-descricao-2 c-descricao-5 c-descricao-6 c-descricao-7 
         RECT-29 c-descricao-8 
      WITH FRAME F-Main IN WINDOW W-Win2.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win2 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .
        
/*     RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
    RUN pi_valida_upc IN h_bonfe001 (INPUT "esnfe200e",
                                     INPUT 'enableObject',
                                     INPUT 'CONTAINER',
                                     INPUT THIS-PROCEDURE,
                                     INPUT FRAME F-Main:HANDLE,
                                     INPUT '',
                                     INPUT p_rowid_item).
/*     DELETE PROCEDURE h_bonfe001. */
    
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win2 
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
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win2 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    DO WITH FRAME {&FRAME-NAME} :
        /* -- Habilita e Desabilita campos --- */
        RUN pi-inicializa.

        
        RUN pi-conta.
        RUN pi-hab-desab.

        
         

        FIND FIRST bf-nfe-nota-fiscal-rec WHERE bf-nfe-nota-fiscal-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso
                                          NO-LOCK NO-ERROR.

        FIND FIRST nfe-dfe NO-LOCK
            WHERE nfe-dfe.chave-acesso = bf-nfe-nota-fiscal-rec.chave-acesso NO-ERROR.

        IF AVAIL bf-nfe-nota-fiscal-rec THEN DO:
            /* --- Se a Nota foi Implantada Bloqueia atualizacao da Nota --- */
            IF nfe-dfe.sit-erp >= 2 THEN ASSIGN btn-ok:SENSITIVE = NO.

            
        END.
        /*Comeáa assim para depois ser feito a validaá∆o pela natureza e parametros do RE*/    
        ASSIGN tg-fifo-oc:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        

        APPLY "LEAVE" TO fi-c-it-codigo.
        APPLY "LEAVE" TO fi-c-natur-oper.
        APPLY "LEAVE" TO fi-i-num-ordem.
        APPLY "LEAVE" TO fi-c-cod-depos.
        APPLY "LEAVE" TO fi-ct-codigo.
        APPLY "LEAVE" TO fi-sc-codigo.
        APPLY "LEAVE" TO fi-cod-unid-negoc.


        RUN pi-nosso-preco.



        ASSIGN fi-de-frete :screen-value in frame {&frame-name}  = string(bf-nfe-it-nota-fisc-rec.item-vfrete).
            
        
/*         RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
        RUN pi_valida_upc IN h_bonfe001 (INPUT "esnfe200e",
                                         INPUT 'Initialize',
                                         INPUT 'CONTAINER',
                                         INPUT THIS-PROCEDURE,
                                         INPUT FRAME F-Main:HANDLE,
                                         INPUT '',
                                         INPUT p_rowid_item).
/*         DELETE PROCEDURE h_bonfe001. */

        
        
    END.
    
    APPLY "LEAVE" TO fi-c-cod-depos IN FRAME {&FRAME-NAME}.
    /* Code placed here will execute AFTER standard behavior.    */

    IF fi-c-cod-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN
        fi-c-nome-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-nota W-Win2 
PROCEDURE pi-atualiza-nota :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAM p_rowid_item AS ROWID NO-UNDO.

    
    DO WITH FRAME {&FRAME-NAME}:

        FIND FIRST nfe-it-nota-fisc-rec WHERE 
                   ROWID(nfe-it-nota-fisc-rec) = p_rowid_item EXCLUSIVE-LOCK NO-ERROR.

        FIND FIRST bf-nfe-nota-fiscal-rec WHERE 
                   bf-nfe-nota-fiscal-rec.chave-acesso = nfe-it-nota-fisc-rec.chave-acesso NO-LOCK NO-ERROR.

        
        ASSIGN nfe-it-nota-fisc-rec.seq-item             = INPUT fi-i-seq
               nfe-it-nota-fisc-rec.item-cprod           = INPUT fi-c-it-codigo-forn
               nfe-it-nota-fisc-rec.it-codigo            = INPUT fi-c-it-codigo
               nfe-it-nota-fisc-rec.item-xPed            = INPUT fi-i-num-pedido
               nfe-it-nota-fisc-rec.item-num-ordem       = INPUT fi-i-num-ordem
               nfe-it-nota-fisc-rec.item-parcela-oc      = INPUT fi-i-parcela-oc
               nfe-it-nota-fisc-rec.item-fifo-oc         = INPUT tg-fifo-oc
               nfe-it-nota-fisc-rec.item-qCom            = INPUT fi-de-qt-do-forn
               nfe-it-nota-fisc-rec.item-un              = INPUT fi-c-un
               nfe-it-nota-fisc-rec.item-qtde            = INPUT fi-de-quantidade
               nfe-it-nota-fisc-rec.item-uCom            = INPUT fi-c-un-forn
               nfe-it-nota-fisc-rec.item-vUnCom          = INPUT fi-de-pre-unit
               nfe-it-nota-fisc-rec.item-vProd           = INPUT fi-de-preco-total
               nfe-it-nota-fisc-rec.item-cfop            = INPUT fi-i-cfop
               nfe-it-nota-fisc-rec.item-nat-operacao    = INPUT fi-c-natur-oper
               nfe-it-nota-fisc-rec.item-esp-nat         = INPUT fi-c-esp-nat
               nfe-it-nota-fisc-rec.item-ncm             = INPUT fi-c-class-fiscal
               nfe-it-nota-fisc-rec.item-conta-contabil  = INPUT fi-c-conta-contabil
               nfe-it-nota-fisc-rec.item-cod-refer       = INPUT fi-c-cod-refer
               nfe-it-nota-fisc-rec.item-cod-depos       = INPUT fi-c-cod-depos
               nfe-it-nota-fisc-rec.item-cod-localiz     = INPUT fi-c-cod-localiz
               nfe-it-nota-fisc-rec.item-ser-lote        = INPUT fi-c-ser-lote
               nfe-it-nota-fisc-rec.item-dt-vali-lote    = INPUT fi-da-dt-vali-lote
               nfe-it-nota-fisc-rec.item-nr-ord-produ    = INPUT fi-i-nr-ord-produ
               nfe-it-nota-fisc-rec.narrativa            =       c-narrativa
               nfe-it-nota-fisc-rec.cod-emit-terc        = input fi-i-cod-emite-terc  
               nfe-it-nota-fisc-rec.nat-terc             = input fi-c-nat-terc        
               nfe-it-nota-fisc-rec.serie-terc           = input fi-c-ser-terc        
               nfe-it-nota-fisc-rec.nro-docto-terc       = input fi-i-nro-docto-terc  
               nfe-it-nota-fisc-rec.seq-terc             = input fi-de-seq-terc   
               nfe-it-nota-fisc-rec.ct-codigo            = INPUT fi-ct-codigo
               nfe-it-nota-fisc-rec.sc-codigo            = INPUT fi-sc-codigo
               nfe-it-nota-fisc-rec.cod-unid-negoc       = INPUT fi-cod-unid-negoc.
        
        

        /* --- Retona o Tipo de Nota de acordo com a Natureza --- */

/*         RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */

        RUN pi_valida_tipo_nota IN h_bonfe001 (INPUT  INPUT fi-c-natur-oper,
                                               OUTPUT i-tipo-nfe,
                                               OUTPUT i-tp-oper-terc).

/*         DELETE PROCEDURE h_bonfe001. */

        

        

        /* --- Devolucao de Clientes --- */

        IF i-tipo-nfe = 2 THEN 
        DO:
            ASSIGN nfe-it-nota-fisc-rec.item-serie-nf    = INPUT fi-serie-docto-refer
                   nfe-it-nota-fisc-rec.item-nr-nota-fis = INPUT fi-nro-docto-refer.

            IF NOT CAN-FIND(FIRST nfe-relac-doc-nota-rec WHERE 
                                  nfe-relac-doc-nota-rec.chave-acesso  = nfe-it-nota-fisc-rec.chave-acesso AND 
                                  nfe-relac-doc-nota-rec.seq-item      = INPUT fi-i-seq                       AND 
                                  nfe-relac-doc-nota-rec.cod-estabel   = bf-nfe-nota-fiscal-rec.cod-estabel   AND 
                                  nfe-relac-doc-nota-rec.serie         = INPUT fi-serie-docto-refer           AND 
                                  nfe-relac-doc-nota-rec.nr-docto-nota = INPUT fi-nro-docto-refer             AND 
                                  nfe-relac-doc-nota-rec.sequencia     = INPUT fi-seq-docto-refer             AND 
                                  nfe-relac-doc-nota-rec.it-codigo     = INPUT fi-c-it-codigo                 NO-LOCK) THEN 
            DO:
                FIND FIRST it-nota-fisc NO-LOCK WHERE 
                           it-nota-fisc.cod-estabel = bf-nfe-nota-fiscal-rec.cod-estabel AND 
                           it-nota-fisc.serie         = INPUT fi-serie-docto-refer       AND 
                           it-nota-fisc.nr-nota-fis   = INPUT fi-nro-docto-refer         AND 
                           it-nota-fisc.nr-seq-fat    = INPUT fi-seq-docto-refer NO-ERROR.

                IF AVAIL it-nota-fisc THEN 
                DO:
                    
                    ASSIGN nfe-it-nota-fisc-rec.item-serie-comp  = it-nota-fisc.serie
                           nfe-it-nota-fisc-rec.item-nro-comp    = it-nota-fisc.nr-nota-fis      
                           nfe-it-nota-fisc-rec.item-nat-comp    = it-nota-fisc.nat-operacao
                           nfe-it-nota-fisc-rec.item-seq-comp    = it-nota-fisc.nr-seq-fat 
                           nfe-it-nota-fisc-rec.item-data-comp   = it-nota-fisc.dt-emis-nota.
                END.
            END. 
        END.
        
        /* --- Retorno Terceiros --- */

        IF i-tipo-nfe = 3 THEN 
        DO:

            

            ASSIGN nfe-it-nota-fisc-rec.item-serie-docto = INPUT fi-serie-docto-refer
                   nfe-it-nota-fisc-rec.item-nro-docto   = INPUT fi-nro-docto-refer
                   nfe-it-nota-fisc-rec.item-seq-comp    = INPUT fi-seq-docto-refer.
            /*
            FIND FIRST emitente WHERE 
                       emitente.cgc = bf-nfe-nota-fiscal-rec.cgc NO-LOCK NO-ERROR.*/

            FIND emitente WHERE
                 emitente.nome-abrev = bf-nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.

            IF AVAIL emitente THEN 
            DO:
                /* --- Busca Nota de Remessa --- */
                

                FIND FIRST saldo-terc WHERE 
                           saldo-terc.cod-emitente   = emitente.cod-emitente              AND 
                           saldo-terc.it-codigo      = INPUT fi-c-it-codigo               AND 
                           saldo-terc.cod-estabel    = bf-nfe-nota-fiscal-rec.cod-estabel AND 
                           saldo-terc.serie-docto    = INPUT fi-serie-docto-refer         AND 
                           saldo-terc.nro-docto      = INPUT fi-nro-docto-refer           AND 
                           saldo-terc.sequencia      = INPUT fi-seq-docto-refer           NO-LOCK NO-ERROR.

                IF AVAIL saldo-terc THEN 
                DO:

                    
                    ASSIGN nfe-it-nota-fisc-rec.item-serie-comp  = saldo-terc.serie-docto
                           nfe-it-nota-fisc-rec.item-nro-comp    = saldo-terc.nro-docto
                           nfe-it-nota-fisc-rec.item-nat-comp    = saldo-terc.nat-operacao
                           nfe-it-nota-fisc-rec.item-seq-comp    = saldo-terc.sequencia
                           nfe-it-nota-fisc-rec.item-data-comp   = saldo-terc.dt-retorno
                           nfe-it-nota-fisc-rec.item-serie-docto = saldo-terc.serie-docto
                           nfe-it-nota-fisc-rec.item-nro-docto   = saldo-terc.nro-docto.



                    


                END.
            END.
        END. /* IF i-tipo-nfe = 3..... */

/*         RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */

        RUN pi_valida_upc IN h_bonfe001 (INPUT "esnfe200e",
                                         INPUT 'AtualizaNota',      
                                         INPUT 'CONTAINER',         
                                         INPUT FRAME F-Main:HANDLE,
                                         INPUT FRAME F-Main:HANDLE,
                                         INPUT '',                  
                                         INPUT p_rowid_item).

/*         DELETE PROCEDURE h_bonfe001. */


    END.

    

     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-infs-item W-Win2 
PROCEDURE pi-carrega-infs-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-cod-depos-aux   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cod-localiz-aux AS CHARACTER   NO-UNDO.

    DO WITH FRAME {&FRAME-NAME} :
        
        FIND FIRST ITEM NO-LOCK 
            WHERE ITEM.it-codigo = INPUT fi-c-it-codigo NO-ERROR.
            
        IF AVAIL ITEM THEN DO:
            ASSIGN fi-c-desc-item:SCREEN-VALUE   = ITEM.desc-item
                   fi-c-un:SCREEN-VALUE          = ITEM.un. 

            FIND FIRST nfe-dfe NO-LOCK
                 WHERE nfe-dfe.chave-acesso = bf-nfe-nota-fiscal-rec.chave-acesso NO-ERROR.
    
            /*Fator de conversao*/
            FIND FIRST nfe-param-rec NO-LOCK
                WHERE nfe-param-rec.cod-parametro = "param_manutencao_itens" NO-ERROR.
                
            IF AVAIL nfe-param-rec THEN DO:
                FIND FIRST nfe-it-param-rec NO-LOCK 
                    WHERE nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro
                    AND   nfe-it-param-rec.cod-item-parametro = "converte_item_fornec" NO-ERROR.
                    
                IF AVAIL nfe-it-param-rec AND TRIM(nfe-it-param-rec.valor-1-item-parametro) = "SIM" THEN DO:
                    
                    FIND FIRST bf-nfe-it-nota-fisc-rec NO-LOCK
                        WHERE ROWID(bf-nfe-it-nota-fisc-rec) = p_rowid_item NO-ERROR.
                        
                    FIND FIRST bf-nfe-nota-fiscal-rec OF bf-nfe-it-nota-fisc-rec NO-LOCK NO-ERROR.
                            
                    FIND emitente WHERE
                         emitente.nome-abrev = bf-nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.
                        
                    IF AVAIL emitente THEN DO:
                        FIND FIRST item-fornec NO-LOCK
                            WHERE item-fornec.cod-emitente = emitente.cod-emitente             
                            AND   item-fornec.it-codigo    = INPUT fi-c-it-codigo  NO-ERROR.
                                
                        IF AVAIL item-fornec THEN DO:
                            IF NOT CAN-FIND(nfe-item-fornec OF item-fornec) THEN DO:
                                CREATE nfe-item-fornec.
                                BUFFER-COPY item-fornec TO nfe-item-fornec. 
                            END.
                                    
                            FIND FIRST nfe-item-fornec OF item-fornec NO-LOCK NO-ERROR.
    
                            FIND FIRST nfe-dfe NO-LOCK
                                 WHERE nfe-dfe.chave-acesso = bf-nfe-nota-fiscal-rec.chave-acesso NO-ERROR.
    
                            IF AVAIL nfe-dfe AND nfe-dfe.sit-erp = 1 THEN
                               ASSIGN de-fat-conv                                          = item-fornec.fator-conver * EXP(10,(item-fornec.num-casa-dec * (-1)))
                                      fi-de-quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INPUT FRAME {&FRAME-NAME} fi-de-qt-do-forn / de-fat-conv).
                        END.
                    END.
                END.
            END.

            IF AVAIL nfe-dfe AND nfe-dfe.sit-erp = 1 THEN DO:
                FIND FIRST ordem-compra NO-LOCK 
                    WHERE ordem-compra.numero-ordem = INPUT fi-i-num-ordem
                    AND   ordem-compra.it-codigo    = INPUT fi-c-it-codigo
                    AND   ordem-compra.situacao     = 2 /* --- Confirmada --- */ NO-ERROR.
        
                IF NOT AVAIL ordem-compra THEN DO:
                    RUN dsc/ra/esp/esra010.p(INPUT  INPUT fi-c-it-codigo,
                                             INPUT  bf-nfe-nota-fiscal-rec.cod-estabel,
                                             INPUT  0,
                                             OUTPUT c-cod-depos-aux, 
                                             OUTPUT c-cod-localiz-aux,
                                             INPUT  p_rowid_item ).
                                             
                    FIND emitente WHERE
                         emitente.nome-abrev = bf-nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.
                         

                    IF ITEM.tipo-contr = 1 OR       /* --- FISICO        --- */
                       ITEM.tipo-contr = 4 THEN DO: /* --- DEBITO DIRETO --- */
                        IF INPUT fi-c-it-codigo <> "" THEN DO:

                            IF AVAIL emitente THEN DO:
                                /*&IF '{&bf_dis_versao_ems}' >= '2.08' &THEN
                                    ASSIGN fi-ct-codigo:SCREEN-VALUE      = ITEM.ct-codigo
                                           fi-sc-codigo:SCREEN-VALUE      = ITEM.sc-codigo 
                                           fi-cod-unid-negoc:SCREEN-VALUE = ITEM.cod-unid-negoc.
                                &ELSE 
                                    ASSIGN fi-c-conta-contabil:SCREEN-VALUE = ITEM.conta-aplicacao.
                                &ENDIF*/

                                FOR EACH nfe-item-fornec
                                    WHERE nfe-item-fornec.cod-emitente = emitente.cod-emitente
                                      AND nfe-item-fornec.item-do-forn   = input fi-c-it-codigo-forn USE-INDEX it-forn NO-LOCK:

                                    IF nfe-item-fornec.it-codigo <> INPUT fi-c-it-codigo THEN DO:
                                        &IF '{&bf_dis_versao_ems}' >= '2.08' &THEN

                                            ASSIGN fi-ct-codigo:SCREEN-VALUE      = ITEM.ct-codigo
                                                   fi-sc-codigo:SCREEN-VALUE      = ITEM.sc-codigo 
                                                   fi-cod-unid-negoc:SCREEN-VALUE = ITEM.cod-unid-negoc.
                                        &ELSE 
                                            ASSIGN fi-c-conta-contabil:SCREEN-VALUE = ITEM.conta-aplicacao.
                                        &ENDIF
                                    END.
                                END.
                            END.

                            IF fi-ct-codigo:SCREEN-VALUE = "" THEN DO:
                                &IF '{&bf_dis_versao_ems}' >= '2.08' &THEN
                                    ASSIGN fi-ct-codigo:SCREEN-VALUE      = ITEM.ct-codigo
                                           fi-sc-codigo:SCREEN-VALUE      = ITEM.sc-codigo 
                                           fi-cod-unid-negoc:SCREEN-VALUE = ITEM.cod-unid-negoc.
                                &ENDIF
                            END.
                        END.
                    END.
                    /* Passei a atualizar o deposito e localizacao aqui por conta da NGK que quando mudava o item estava permanecendo com o deposito do item antigo*/
                    ASSIGN fi-c-cod-depos:SCREEN-VALUE   = c-cod-depos-aux
                           fi-c-cod-localiz:SCREEN-VALUE = c-cod-localiz-aux.
                END.
    
                ELSE DO:
                    RUN dsc/ra/esp/esra010.p(INPUT  INPUT fi-c-it-codigo,
                                             INPUT  bf-nfe-nota-fiscal-rec.cod-estabel,
                                             INPUT  ordem-compra.numero-ordem,
                                             OUTPUT c-cod-depos-aux,      
                                             OUTPUT c-cod-localiz-aux,
                                             INPUT  p_rowid_item ).  
    
                    ASSIGN c-deposito = c-cod-depos-aux.
    
                    /* Trata apenas quando informa Ordem de Compra */
                    FIND FIRST bf-nfe-it-nota-fisc-rec NO-LOCK
                                WHERE ROWID(bf-nfe-it-nota-fisc-rec) = p_rowid_item NO-ERROR.
          
                    IF AVAIL bf-nfe-it-nota-fisc-rec THEN 
                        ASSIGN fi-c-cod-depos:SCREEN-VALUE   = c-cod-depos-aux
                               fi-c-cod-localiz:SCREEN-VALUE = c-cod-localiz-aux.
                END.                                                 
            END. /* nfe-dfe = 1 */
        END.
        ELSE
            ASSIGN fi-c-desc-item:SCREEN-VALUE = "".
            
        RUN pi-hab-desab.
        APPLY "LEAVE" TO fi-c-natur-oper.
    END.

    RUN pi_valida_upc IN h_bonfe001 (INPUT "esnfe200e",
                                     INPUT 'CarregaInfsItem',
                                     INPUT 'CONTAINER',
                                     INPUT FRAME F-Main:HANDLE,
                                     INPUT FRAME F-Main:HANDLE,
                                     INPUT '',
                                     INPUT p_rowid_item).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-infs-ordem W-Win2 
PROCEDURE pi-carrega-infs-ordem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/



    DO WITH FRAME {&FRAME-NAME} :
        
        FIND FIRST ordem-compra NO-LOCK 
            WHERE ordem-compra.numero-ordem = INPUT fi-i-num-ordem
            AND   ordem-compra.situacao     = 2 /* --- Confirmada --- */ NO-ERROR.
        
        IF AVAIL ordem-compra THEN DO:
            ASSIGN fi-i-num-pedido:SCREEN-VALUE     = STRING(ordem-compra.num-pedido)
                   fi-c-conta-contabil:SCREEN-VALUE = ordem-compra.conta-contabil
                   fi-c-cod-depos:SCREEN-VALUE      = ordem-compra.dep-almoxar
                   fi-i-nr-ord-produ:SCREEN-VALUE   = STRING(ordem-compra.ordem-servic)
                   fi-de-pre-ordem:SCREEN-VALUE     = STRING(ordem-compra.preco-unit /*- ordem-compra.valor-descto*/ )
                   fi-de-desconto-ordem:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = string(ordem-compra.valor-descto).
                
            ASSIGN de-quant-variacao = 0.
            /* --- Valida Variacao --- */
            
            FIND FIRST param-re no-lock
                WHERE param-re.usuario = c-seg-usuario NO-ERROR.
                
            IF AVAIL param-re            AND
               param-re.aceita-var = YES THEN DO:
                
                ASSIGN l-erro = NO.
                    
                FIND FIRST item-uni-estab no-lock
                    WHERE item-uni-estab.it-codigo    = INPUT fi-c-it-codigo
                    AND item-uni-estab.cod-estabel  = bf-nfe-nota-fiscal-rec.cod-estabel NO-ERROR.
                    
                IF AVAIL item-uni-estab THEN DO:
                    
                    IF item-uni-estab.var-qtd-re <> 0 THEN DO:
                        
                        ASSIGN de-quant-variacao = INPUT fi-de-quantidade * (item-uni-estab.var-qtd-re / 100) /* variaá∆o percentual */
                               l-erro            = YES.
                    END.
                    
                    IF item-uni-estab.lim-var-qtd <> 0 THEN DO:
                        
                        ASSIGN de-quant-variacao    = item-uni-estab.lim-var-qtd /* variaá∆o qtde */
                               l-erro               = YES.
                    END.

                    IF l-erro = NO THEN DO:
                        
                        FIND FIRST item-mat no-lock
                            WHERE item-mat.it-codigo = INPUT fi-c-it-codigo NO-ERROR.
                            
                        IF AVAIL item-mat THEN DO:
                            
                            IF item-mat.var-qtd-re <> 0 THEN DO:
                                
                                ASSIGN de-quant-variacao = INPUT fi-de-quantidade * (item-mat.var-qtd-re / 100). /* variaá∆o percentual */

                            END.
                                
                            IF item-mat.lim-var-qtd <> 0 THEN DO:
                                
                                ASSIGN de-quant-variacao = item-mat.lim-var-qtd. /* variaá∆o qtde */
                            END.

                        END.

                    END.

                END.

            END.
                
            FIND FIRST prazo-compra NO-LOCK 
                WHERE prazo-compra.numero-ordem     = INPUT fi-i-num-ordem
                AND   prazo-compra.situacao         = 2 /* --- Confirmada --- */
                AND  (prazo-compra.quant-saldo - 
                      prazo-compra.dec-1)          >= (INPUT fi-de-quantidade - de-quant-variacao) NO-ERROR.
                
            IF AVAIL prazo-compra THEN 
                ASSIGN fi-i-parcela-oc:SCREEN-VALUE = STRING(prazo-compra.parcela).
                
            FIND FIRST ord-prod NO-LOCK 
                WHERE ord-prod.nr-ord-produ = ordem-compra.ordem-servic NO-ERROR.
                
            IF AVAIL ord-prod THEN 
                ASSIGN fi-c-conta-contabil:SCREEN-VALUE = STRING(ord-prod.conta-ordem).


            IF fi-c-it-codigo:screen-value = "" THEN DO:

                ASSIGN fi-c-it-codigo:screen-value      = ordem-compra.it-codigo.
                /*APPLY "LEAVE" TO fi-c-it-codigo.*/
                  RUN pi-carrega-infs-item.

            END.

        END.
        ELSE 
            ASSIGN fi-de-pre-ordem:SCREEN-VALUE = '0'
                   fi-de-desconto-ordem:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = '0'.
            
    END.


    RUN pi_valida_upc IN h_bonfe001 (INPUT "esnfe200e",
                                     INPUT 'CarregaInfsOrdem',
                                     INPUT 'CONTAINER',
                                     INPUT FRAME F-Main:HANDLE,
                                     INPUT FRAME F-Main:HANDLE,
                                     INPUT '',
                                     INPUT p_rowid_item).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-nota W-Win2 
PROCEDURE pi-carrega-nota :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAM p_nat_operacao      AS CHAR FORMAT "x(06)"  NO-UNDO.

    

    DO WITH FRAME {&FRAME-NAME} :
        /* --- Retona o Tipo de Nota de acordo com a Natureza --- */
/*         RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
        RUN pi_valida_tipo_nota IN h_bonfe001 (INPUT  p_nat_operacao,
                                               OUTPUT i-tipo-nfe,
                                               OUTPUT i-tp-oper-terc).
/*         DELETE PROCEDURE h_bonfe001. */

        /*preco do xml*/
        ASSIGN fi-de-pre-unit-xml:SCREEN-VALUE = string(bf-nfe-it-nota-fisc-rec.preco-unit-xml)
               fi-de-desconto-xml:SCREEN-VALUE = string(bf-nfe-it-nota-fisc-rec.item-vdesc) .



        
        /* --- Compra --- */
        IF i-tipo-nfe = 1 THEN DO:
            /* --- Verifica Relacionamento de Ordem --- */
            FIND FIRST nfe-relac-ordem-rec WHERE nfe-relac-ordem-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso
                                             AND nfe-relac-ordem-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item
                                           NO-LOCK NO-ERROR.
            IF NOT AVAIL nfe-relac-ordem-rec THEN DO:
                /* --- Verifica se Usuario esta flegado FIFO --- */
                
                FIND FIRST param-re WHERE param-re.usuario = c-seg-usuario
                                    NO-LOCK NO-ERROR.

                
                IF AVAIL param-re       AND
                   param-re.log-1 = YES THEN ASSIGN tg-fifo-oc:SENSITIVE = YES.
                                        ELSE ASSIGN tg-fifo-oc:SENSITIVE = NO.
    
                APPLY "VALUE-CHANGED" TO tg-fifo-oc.
            END.
        END.

        /* --- Devolucao de Clientes --- */
        IF i-tipo-nfe = 2 THEN
            ASSIGN fi-c-text:SCREEN-VALUE       = "Nota Fiscal"
                   fi-nro-docto-refer:LABEL     = "Nr. Nota Fiscal"
                   fi-serie-docto-refer:LABEL   = "Serie NF".
        
        /* --- Retorno Terceiros --- */
        IF i-tipo-nfe = 3 THEN DO:
            ASSIGN fi-c-text:SCREEN-VALUE       = "Docto Referenciado"
                   fi-nro-docto-refer:LABEL     = "Nr. Documento"
                   fi-serie-docto-refer:LABEL   = "Serie Docto".
                
            /*
            FIND FIRST emitente WHERE emitente.cgc = bf-nfe-nota-fiscal-rec.cgc
                                NO-LOCK NO-ERROR.*/

            FIND emitente WHERE
                 emitente.nome-abrev = bf-nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.


            IF AVAIL emitente THEN DO:
                /* --- Busca Nota de Remessa --- */

                
                ASSIGN c-item-nro-docto-ini     = IF TRIM(bf-nfe-it-nota-fisc-rec.item-nro-docto)   <> "" THEN bf-nfe-it-nota-fisc-rec.item-nro-docto   ELSE ""
                       c-item-nro-docto-fim     = IF TRIM(bf-nfe-it-nota-fisc-rec.item-nro-docto)   <> "" THEN bf-nfe-it-nota-fisc-rec.item-nro-docto   ELSE "ZZZZZZZZZZZZZZZZ"
                       c-item-serie-docto-ini   = IF TRIM(bf-nfe-it-nota-fisc-rec.item-serie-docto) <> "" THEN bf-nfe-it-nota-fisc-rec.item-serie-docto ELSE ""
                       c-item-serie-docto-fim   = IF TRIM(bf-nfe-it-nota-fisc-rec.item-serie-docto) <> "" THEN bf-nfe-it-nota-fisc-rec.item-serie-docto ELSE "ZZZZZ".
                           
                FOR EACH saldo-terc WHERE saldo-terc.cod-emitente    = emitente.cod-emitente
                                      AND saldo-terc.it-codigo       = INPUT fi-c-it-codigo
                                      AND saldo-terc.cod-estabel     = bf-nfe-nota-fiscal-rec.cod-estabel
                                      AND saldo-terc.serie-docto    >= c-item-serie-docto-ini
                                      AND saldo-terc.serie-docto    <= c-item-serie-docto-fim
                                      AND saldo-terc.nro-docto      >= c-item-nro-docto-ini
                                      AND saldo-terc.nro-docto      <= c-item-nro-docto-fim
                                    NO-LOCK BY saldo-terc.sequencia :
                    

                    IF (saldo-terc.quantidade - saldo-terc.dec-1) <= 0 THEN NEXT.
                        
                    ASSIGN de-qtde-it-terc = 0.
/*                     FOR EACH bf2-nfe-it-nota-fisc-rec WHERE bf2-nfe-it-nota-fisc-rec.chave-acesso     = bf-nfe-it-nota-fisc-rec.chave-acesso-nfe */
/*                                                         AND bf2-nfe-it-nota-fisc-rec.it-codigo        = saldo-terc.it-codigo                     */
/*                                                         AND bf2-nfe-it-nota-fisc-rec.item-serie-comp  = saldo-terc.serie-docto                   */
/*                                                         AND bf2-nfe-it-nota-fisc-rec.item-nro-comp    = saldo-terc.nro-docto                     */
/*                                                         AND bf2-nfe-it-nota-fisc-rec.item-nat-comp    = saldo-terc.nat-operacao                  */
/*                                                         AND bf2-nfe-it-nota-fisc-rec.item-seq-comp    = saldo-terc.sequencia                     */
/*                                                         AND bf2-nfe-it-nota-fisc-rec.item-data-comp   = saldo-terc.dt-retorno                    */
/*                                                       NO-LOCK :                                                                                  */
/*                         ASSIGN de-qtde-it-terc = de-qtde-it-terc + bf2-nfe-it-nota-fisc-rec.item-qtde.                                           */
/*                     END.                                                                                                                         */
                        
                     

                    IF (INPUT fi-de-quantidade + de-qtde-it-terc) >  
                       (saldo-terc.quantidade - saldo-terc.dec-1) THEN NEXT.
                        
                    IF TRIM(fi-c-cod-refer:SCREEN-VALUE)            = ""    THEN
                        ASSIGN fi-c-cod-refer:SCREEN-VALUE          = saldo-terc.cod-refer.
                    IF TRIM(fi-c-cod-depos:SCREEN-VALUE)            = ""    THEN
                        ASSIGN fi-c-cod-depos:SCREEN-VALUE          = saldo-terc.cod-depos.
                    IF TRIM(fi-c-cod-localiz:SCREEN-VALUE)          = ""    THEN
                        ASSIGN fi-c-cod-localiz:SCREEN-VALUE        = saldo-terc.cod-localiz.
                    IF TRIM(fi-c-ser-lote:SCREEN-VALUE)             = ""    THEN
                        ASSIGN fi-c-ser-lote:SCREEN-VALUE           = saldo-terc.lote.
                    IF TRIM(fi-i-nr-ord-produ:SCREEN-VALUE)         = "0"   THEN
                        ASSIGN fi-i-nr-ord-produ:SCREEN-VALUE       = STRING(saldo-terc.nr-ord-produ).
                    IF TRIM(fi-nro-docto-refer:SCREEN-VALUE)        = ""    THEN
                        ASSIGN fi-nro-docto-refer:SCREEN-VALUE      = saldo-terc.nro-docto.
                    IF TRIM(fi-serie-docto-refer:SCREEN-VALUE)      = ""    THEN
                        ASSIGN fi-serie-docto-refer:SCREEN-VALUE    = saldo-terc.serie-docto.
                    IF TRIM(fi-seq-docto-refer:SCREEN-VALUE)        = "0"   THEN
                        ASSIGN fi-seq-docto-refer:SCREEN-VALUE      = STRING(saldo-terc.sequencia).

                    IF saldo-terc.cod-depos <> "" THEN /*RBA - Atualiza deposito com o deposito do saldo-terc*/
                        ASSIGN c-depos = saldo-terc.cod-depos.

                    FIND FIRST saldo-estoq WHERE saldo-estoq.it-codigo  = saldo-terc.it-codigo
                                             AND saldo-estoq.lote       = saldo-terc.lote
                                           NO-LOCK NO-ERROR.
                    IF AVAIL saldo-estoq THEN DO:
                        IF fi-da-dt-vali-lote:SCREEN-VALUE = "?" THEN
                            ASSIGN fi-da-dt-vali-lote:SCREEN-VALUE = STRING(saldo-estoq.dt-vali-lote).
                    END.
                END.
            END.
                                                            
            /* --- Retorno Beneficiamento --- */
            IF i-tp-oper-terc  = 2 THEN DO:
                /* --- Verifica Parametros Globais --- */
                FIND FIRST nfe-param-rec WHERE nfe-param-rec.cod-parametro = "param_global"
                                         NO-LOCK NO-ERROR.
                IF AVAIL nfe-param-rec THEN DO:
                    /* --- Verifica Deposito Retorno Beneficiamento --- */
                    FIND FIRST nfe-it-param-rec WHERE nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro
                                                  AND nfe-it-param-rec.cod-item-parametro = "depos_retorno_benef"
                                                NO-LOCK NO-ERROR.
                    IF AVAIL nfe-it-param-rec                               AND
                       TRIM(nfe-it-param-rec.valor-1-item-parametro) <> ""  THEN
                        ASSIGN fi-c-cod-depos:SCREEN-VALUE = nfe-it-param-rec.valor-1-item-parametro.
                END.
            END.

            APPLY "LEAVE" TO fi-c-cod-depos.
            APPLY "LEAVE" TO fi-c-cod-localiz.
            APPLY "LEAVE" TO fi-c-cod-refer.
        END.


        IF i-tipo-nfe = 3 AND  c-depos <> "" THEN /*RBA - Atualiza deposito com o deposito do saldo-terc*/
            ASSIGN fi-c-cod-depos:SCREEN-VALUE = c-depos.

        /* --- Desabilita Campos --- */
        IF i-tipo-nfe <> 2 AND
           i-tipo-nfe <> 3 THEN DO:
            ASSIGN fi-nro-docto-refer:SENSITIVE     = NO
                   fi-serie-docto-refer:SENSITIVE   = NO
                   fi-seq-docto-refer:SENSITIVE     = NO
                   bt-doc-nota:SENSITIVE            = NO
                   bt-estrutura:SENSITIVE           = NO
                   bt-busca-nota:SENSITIVE          = NO.
        END.
        ELSE DO:
            ASSIGN fi-nro-docto-refer:SENSITIVE     = YES
                   fi-serie-docto-refer:SENSITIVE   = YES
                   bt-doc-nota:SENSITIVE            = YES
                   bt-estrutura:SENSITIVE           = YES.

            /* --- Devolucao de Clientes --- */
            IF i-tipo-nfe = 2 THEN ASSIGN fi-seq-docto-refer:SENSITIVE  = YES
/*                                        fi-seq-docto-refer:HIDDEN     = YES -> Retirado para aceitar a sequencia  */
                                          bt-busca-nota:SENSITIVE       = YES
                                          bt-estrutura:SENSITIVE        = NO.
                              ELSE ASSIGN fi-seq-docto-refer:SENSITIVE  = YES
                                          fi-seq-docto-refer:HIDDEN     = NO
                                          bt-busca-nota:SENSITIVE       = YES
                                          bt-estrutura:SENSITIVE        = YES.
        END.

        FIND FIRST ITEM WHERE ITEM.it-codigo = INPUT fi-c-it-codigo
                        NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN DO:
            /* --- Se nao for Controle por Serie ou Lote Desabilita --- */
            IF ITEM.tipo-con-est <> 1 AND
               ITEM.tipo-con-est <> 2 AND
               ITEM.tipo-con-est <> 3 THEN ASSIGN bt-ser-lote:SENSITIVE = NO.
                                      ELSE ASSIGN bt-ser-lote:SENSITIVE = YES.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-conta W-Win2 
PROCEDURE pi-conta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


              
&IF '{&bf_dis_versao_ems}' <= '2.07' &THEN

    ASSIGN fi-c-conta-contabil       :visible in FRAME {&frame-name} = YES
           fi-c-desc-conta-contabil  :visible in FRAME {&frame-name} = YES
           fi-ct-codigo              :visible in FRAME {&frame-name} = NO
           fi-ct-codigo-desc         :visible in FRAME {&frame-name} = NO
           fi-sc-codigo              :visible in FRAME {&frame-name} = NO
           fi-sc-codigo-desc         :visible in FRAME {&frame-name} = NO
           fi-cod-unid-negoc         :visible in FRAME {&frame-name} = NO
           fi-unid-neg-descri        :visible in FRAME {&frame-name} = NO.


&ELSE
        ASSIGN fi-c-conta-contabil       :visible in FRAME {&frame-name} = NO
               fi-c-desc-conta-contabil  :visible in FRAME {&frame-name} = NO
               fi-ct-codigo              :visible in FRAME {&frame-name} = yes
               fi-ct-codigo-desc         :visible in FRAME {&frame-name} = yes
               fi-sc-codigo              :visible in FRAME {&frame-name} = yes
               fi-sc-codigo-desc         :visible in FRAME {&frame-name} = YES                                 
               fi-cod-unid-negoc         :visible in FRAME {&frame-name} = YES
               fi-unid-neg-descri        :visible in FRAME {&frame-name} = YES.


&ENDIF              
              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-deleta-bo W-Win2 
PROCEDURE pi-deleta-bo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DELETE PROCEDURE h_bonfe001.

DELETE PROCEDURE h-boin176.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-hab-desab W-Win2 
PROCEDURE pi-hab-desab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR c-campos    AS CHAR FORMAT "x(30)" EXTENT 22
    INIT["item","pedido_compra","ordem_compra","parcela_oc","fifo_oc","qtde_fornecedor","un_fornecedor",
         "preco_unitario","quantidade","un","preco_total","natureza","classif_fiscal","conta_contabil",
         "referencia","deposito","localizacao","serie_lote","validade_lote","ordem_producao"]   NO-UNDO.

    /*Habilita/Desabilita Campos Controle de estoque*/
    DO WITH FRAME {&FRAME-NAME} :
        FIND FIRST ITEM NO-LOCK
            WHERE  ITEM.it-codigo = INPUT fi-c-it-codigo NO-ERROR.
            
        IF AVAIL ITEM THEN DO: 
            
            CASE ITEM.tipo-con-est:
                WHEN 1 THEN ASSIGN fi-c-ser-lote:SENSITIVE         = NO
                                   fi-da-dt-vali-lote:SENSITIVE    = NO
                                   fi-c-cod-refer:SENSITIVE        = NO.
                
                WHEN 2 THEN ASSIGN fi-c-ser-lote:SENSITIVE         = YES
                                   fi-da-dt-vali-lote:SENSITIVE    = NO
                                   fi-c-cod-refer:SENSITIVE        = NO.
                    
                WHEN 3 THEN ASSIGN fi-c-ser-lote:SENSITIVE         = YES
                                   fi-da-dt-vali-lote:SENSITIVE    = YES
                                   fi-c-cod-refer:SENSITIVE        = NO.
               
                WHEN 4 THEN ASSIGN fi-c-ser-lote:SENSITIVE         = NO
                                   fi-da-dt-vali-lote:SENSITIVE    = NO
                                   fi-c-cod-refer:SENSITIVE        = YES.
                    
                OTHERWISE ASSIGN fi-c-ser-lote:SENSITIVE           = NO
                                 fi-da-dt-vali-lote:SENSITIVE      = NO
                                 fi-c-cod-refer:SENSITIVE          = NO.
                    
            END CASE.
        END.
        ELSE 
            ASSIGN fi-c-ser-lote:SENSITIVE         = NO
                   fi-da-dt-vali-lote:SENSITIVE    = NO
                   fi-c-cod-refer:SENSITIVE        = NO.
        
        /* --- Para Relacionamento de Ordem --- */
            
        FIND FIRST nfe-relac-ordem-rec NO-LOCK
            WHERE nfe-relac-ordem-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso
            AND   nfe-relac-ordem-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item NO-ERROR.

        IF AVAIL nfe-relac-ordem-rec THEN
            
            ASSIGN fi-i-num-pedido:SCREEN-VALUE     = "0"
                   fi-i-num-ordem:SCREEN-VALUE      = "0"
                   fi-i-parcela-oc:SCREEN-VALUE     = "0"
                   fi-i-nr-ord-produ:SCREEN-VALUE   = "0"
                   fi-i-num-pedido:SENSITIVE        = NO
                   fi-i-num-ordem:SENSITIVE         = NO
                   fi-i-parcela-oc:SENSITIVE        = NO
                   tg-fifo-oc:SENSITIVE             = NO
                   fi-c-it-codigo:SENSITIVE         = NO
                   fi-de-quantidade:SENSITIVE       = NO
                   fi-i-nr-ord-produ:SENSITIVE      = NO.
        ELSE
            ASSIGN fi-i-num-pedido:SENSITIVE    = YES
                   fi-i-num-ordem:SENSITIVE     = YES
                   fi-i-parcela-oc:SENSITIVE    = YES
                   tg-fifo-oc:SENSITIVE         = YES
                   fi-i-nr-ord-produ:SENSITIVE  = YES.

        IF INPUT fi-c-natur-oper = "" THEN tg-fifo-oc:SENSITIVE  = NO.
        ELSE DO:
            
            RUN pi_valida_tipo_nota IN h_bonfe001 (INPUT  INPUT fi-c-natur-oper,
                                                   OUTPUT i-tipo-nfe,
                                                   OUTPUT i-tp-oper-terc).

        
        
            IF i-tipo-nfe = 1 THEN DO: /* --- Compra --- */
                /* --- Verifica Relacionamento de Ordem --- */
                FIND FIRST nfe-relac-ordem-rec WHERE nfe-relac-ordem-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso
                                                 AND nfe-relac-ordem-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item
                                               NO-LOCK NO-ERROR.
                IF NOT AVAIL nfe-relac-ordem-rec THEN DO:
                    /* --- Verifica se Usuario esta flegado FIFO --- */
                    
                    FIND FIRST param-re WHERE param-re.usuario = c-seg-usuario
                                        NO-LOCK NO-ERROR.
    
                    
                    IF AVAIL param-re       AND
                       param-re.log-1 = YES THEN ASSIGN tg-fifo-oc:SENSITIVE = YES.
                                            ELSE ASSIGN tg-fifo-oc:SENSITIVE = NO.
        
                    APPLY "VALUE-CHANGED" TO tg-fifo-oc.
                END.
            END.
    
            
        END.

        /* --- Para Relacionamento de SÇrie/Lote --- */
        FIND FIRST nfe-relac-ser-lote-rec NO-LOCK 
            WHERE nfe-relac-ser-lote-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso
            AND   nfe-relac-ser-lote-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item NO-ERROR.
            
        IF AVAIL nfe-relac-ser-lote-rec THEN
            ASSIGN fi-c-cod-depos:SCREEN-VALUE      = STRING(nfe-relac-ser-lote-rec.cod-depos)
                   fi-c-cod-localiz:SCREEN-VALUE    = STRING(nfe-relac-ser-lote-rec.cod-localiz)
                   fi-c-ser-lote:SCREEN-VALUE       = STRING(nfe-relac-ser-lote-rec.ser-lote)
                   fi-da-dt-vali-lote:SCREEN-VALUE  = STRING(nfe-relac-ser-lote-rec.dt-vali-lote)
                   fi-c-cod-depos:SENSITIVE         = NO
                   fi-c-cod-localiz:SENSITIVE       = NO
                   fi-c-ser-lote:SENSITIVE          = NO
                   fi-da-dt-vali-lote:SENSITIVE     = NO
                   fi-c-it-codigo:SENSITIVE         = NO
                   fi-de-quantidade:SENSITIVE       = NO.
        ELSE
            ASSIGN fi-c-cod-depos:SENSITIVE         = YES
                   fi-c-cod-localiz:SENSITIVE       = YES
                   fi-c-ser-lote:SENSITIVE          = YES
                   fi-da-dt-vali-lote:SENSITIVE     = YES.

        /* --- Para Relacionamento de Documento/Nota --- */
        FIND FIRST nfe-relac-doc-nota-rec NO-LOCK 
            WHERE nfe-relac-doc-nota-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso
            AND   nfe-relac-doc-nota-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item NO-ERROR.
            
        IF AVAIL nfe-relac-doc-nota-rec THEN
            ASSIGN fi-nro-docto-refer:SCREEN-VALUE      = ""
                   fi-serie-docto-refer:SCREEN-VALUE    = ""
                   fi-seq-docto-refer:SCREEN-VALUE      = "0"
                   fi-i-nr-ord-produ:SCREEN-VALUE       = "0"
                   fi-nro-docto-refer:SENSITIVE         = NO
                   fi-serie-docto-refer:SENSITIVE       = NO
                   fi-seq-docto-refer:SENSITIVE         = NO
                   fi-i-nr-ord-produ:SENSITIVE          = NO
                   fi-c-it-codigo:SENSITIVE             = NO
                   fi-de-quantidade:SENSITIVE           = NO
                   bt-busca-nota:SENSITIVE              = NO.
            
        ELSE DO:
            IF CAN-FIND(FIRST natur-oper WHERE natur-oper.nat-operacao = INPUT fi-c-natur-oper ) THEN DO:
                
                /* --- Retona o Tipo de Nota de acordo com a Natureza --- */
/*                 RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
                
                RUN pi_valida_tipo_nota IN h_bonfe001 (INPUT (INPUT fi-c-natur-oper),
                                                       OUTPUT i-tipo-nfe,
                                                       OUTPUT i-tp-oper-terc).
                    
/*                 DELETE PROCEDURE h_bonfe001. */
                    
                IF i-tipo-nfe = 2   OR   /* --- Devolucao de Clientes --- */
                   i-tipo-nfe = 3   THEN /* --- Retorno Terceiros --- */
                    
                    ASSIGN fi-nro-docto-refer:SENSITIVE         = YES
                           fi-serie-docto-refer:SENSITIVE       = YES
                           fi-seq-docto-refer:SENSITIVE         = YES
                           bt-busca-nota:SENSITIVE              = YES.
            END.
        END.
            
        IF NOT AVAIL nfe-relac-ordem-rec    AND
           NOT AVAIL nfe-relac-ser-lote-rec AND
           NOT AVAIL nfe-relac-doc-nota-rec THEN
            
            ASSIGN fi-c-it-codigo:SENSITIVE             = YES
                   fi-de-quantidade:SENSITIVE           = YES.
            
        FIND FIRST nfe-param-rec no-lock
            WHERE nfe-param-rec.cod-parametro = "param_manutencao_itens" NO-ERROR.
            
        IF AVAIL nfe-param-rec THEN DO:
            
            DO i-aux = 1 TO 22 :
                /* --- Verifica Parametros de Manutencao de Itens --- */
                
                FIND FIRST nfe-it-param-rec no-lock
                    WHERE nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro
                    AND   nfe-it-param-rec.cod-item-parametro = c-campos[i-aux] NO-ERROR.
                    
                IF AVAIL nfe-it-param-rec                           AND
                   nfe-it-param-rec.valor-1-item-parametro <> "SIM" THEN DO:
                    
                    CASE c-campos[i-aux] :
                        
                        WHEN "item"             THEN ASSIGN fi-c-it-codigo:SENSITIVE        = NO.
                        WHEN "pedido_compra"    THEN ASSIGN fi-i-num-pedido:SENSITIVE       = NO.
                        WHEN "ordem_compra"     THEN ASSIGN fi-i-num-ordem:SENSITIVE        = NO.
                        WHEN "parcela_oc"       THEN ASSIGN fi-i-parcela-oc:SENSITIVE       = NO.
                        WHEN "fifo_oc"          THEN ASSIGN tg-fifo-oc:SENSITIVE            = NO.
                        WHEN "qtde_fornecedor"  THEN ASSIGN fi-de-qt-do-forn:SENSITIVE      = NO.
                        WHEN "un_fornecedor"    THEN ASSIGN fi-c-un-forn:SENSITIVE          = NO.
                        WHEN "preco_unitario"   THEN ASSIGN fi-de-pre-unit:SENSITIVE        = NO.
                        WHEN "quantidade"       THEN ASSIGN fi-de-quantidade:SENSITIVE      = NO.
                        WHEN "un"               THEN ASSIGN fi-c-un:SENSITIVE               = NO.
                        WHEN "preco_total"      THEN ASSIGN fi-de-preco-total:SENSITIVE     = NO.
                        /*WHEN "cfop"             THEN ASSIGN fi-i-cfop:SENSITIVE             = NO.*/
                        WHEN "natureza"         THEN ASSIGN fi-c-natur-oper:SENSITIVE       = NO.
                        WHEN "classif_fiscal"   THEN ASSIGN fi-c-class-fiscal:SENSITIVE     = NO.
                        WHEN "conta_contabil"   THEN ASSIGN fi-c-conta-contabil:SENSITIVE   = NO.
                        WHEN "referencia"       THEN ASSIGN fi-c-cod-refer:SENSITIVE        = NO.
                        WHEN "deposito"         THEN ASSIGN fi-c-cod-depos:SENSITIVE        = NO.
                        WHEN "localizacao"      THEN ASSIGN fi-c-cod-localiz:SENSITIVE      = NO.
                        WHEN "serie_lote"       THEN ASSIGN fi-c-ser-lote:SENSITIVE         = NO.
                        WHEN "validade_lote"    THEN ASSIGN fi-da-dt-vali-lote:SENSITIVE    = NO.
                        WHEN "ordem_producao"   THEN ASSIGN fi-i-nr-ord-produ:SENSITIVE     = NO.
                            
                    END CASE.
                END.
            END.
        END.
        
        /*Operaá∆o Triangular*/
        
        IF fi-c-natur-oper:SCREEN-VALUE <> "" THEN DO:
            
            FIND FIRST natur-oper NO-LOCK
                WHERE natur-oper.nat-operacao = fi-c-natur-oper:SCREEN-VALUE NO-ERROR.
                
            IF AVAIL natur-oper AND natur-oper.log-oper-triang = YES THEN DO:
                
                ASSIGN fi-i-cod-emite-terc :sensitive = yes.
                       fi-c-nat-terc       :sensitive = yes.
                       fi-c-ser-terc       :sensitive = yes.
                       fi-i-nro-docto-terc :sensitive = yes.
                       fi-de-seq-terc      :sensitive = yes.


            END.
            ELSE DO:
                
                ASSIGN fi-i-cod-emite-terc :sensitive = no.
                       fi-c-nat-terc       :sensitive = no.
                       fi-c-ser-terc       :sensitive = no.
                       fi-i-nro-docto-terc :sensitive = no.
                       fi-de-seq-terc      :sensitive = no.

            END.
        END.
        ELSE DO:
            ASSIGN fi-i-cod-emite-terc :sensitive = no.
                   fi-c-nat-terc       :sensitive = no.
                   fi-c-ser-terc       :sensitive = no.
                   fi-i-nro-docto-terc :sensitive = no.
                   fi-de-seq-terc      :sensitive = no.

        END.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-inicializa W-Win2 
PROCEDURE pi-inicializa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST bf-nfe-it-nota-fisc-rec WHERE ROWID(bf-nfe-it-nota-fisc-rec) = p_rowid_item
                                       NO-LOCK NO-ERROR.
    IF AVAIL bf-nfe-it-nota-fisc-rec THEN DO:
        FIND FIRST bf-nfe-nota-fiscal-rec WHERE bf-nfe-nota-fiscal-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso
                                          NO-LOCK NO-ERROR.

        
        ASSIGN fi-i-seq             = bf-nfe-it-nota-fisc-rec.seq-item
               fi-c-it-codigo-forn  = bf-nfe-it-nota-fisc-rec.item-cprod
               fi-c-it-codigo       = bf-nfe-it-nota-fisc-rec.it-codigo
               fi-i-num-pedido      = bf-nfe-it-nota-fisc-rec.item-xPed
               fi-i-num-ordem       = bf-nfe-it-nota-fisc-rec.item-num-ordem
               fi-i-parcela-oc      = bf-nfe-it-nota-fisc-rec.item-parcela-oc
               tg-fifo-oc           = bf-nfe-it-nota-fisc-rec.item-fifo-oc
               fi-de-qt-do-forn     = bf-nfe-it-nota-fisc-rec.item-qCom
               fi-c-un              = bf-nfe-it-nota-fisc-rec.item-un
               fi-de-quantidade     = bf-nfe-it-nota-fisc-rec.item-qtde
               fi-c-un-forn         = bf-nfe-it-nota-fisc-rec.item-uCom
               fi-de-pre-unit       = bf-nfe-it-nota-fisc-rec.item-vUnCom
               fi-de-preco-total    = bf-nfe-it-nota-fisc-rec.item-vProd
               fi-i-cfop            = bf-nfe-it-nota-fisc-rec.item-cfop
               fi-c-natur-oper      = bf-nfe-it-nota-fisc-rec.item-nat-operacao
               fi-c-esp-nat         = bf-nfe-it-nota-fisc-rec.item-esp-nat
               fi-c-class-fiscal    = bf-nfe-it-nota-fisc-rec.item-ncm
               fi-c-conta-contabil  = bf-nfe-it-nota-fisc-rec.item-conta-contabil
               fi-c-cod-refer       = bf-nfe-it-nota-fisc-rec.item-cod-refer
               fi-c-cod-depos       = bf-nfe-it-nota-fisc-rec.item-cod-depos
               fi-c-cod-localiz     = bf-nfe-it-nota-fisc-rec.item-cod-localiz
               fi-c-ser-lote        = bf-nfe-it-nota-fisc-rec.item-ser-lote
               fi-da-dt-vali-lote   = bf-nfe-it-nota-fisc-rec.item-dt-vali-lote
               fi-i-nr-ord-produ    = bf-nfe-it-nota-fisc-rec.item-nr-ord-produ
               fi-seq-docto-refer   = bf-nfe-it-nota-fisc-rec.item-seq-comp
               fi-de-total-nosso  :screen-value in frame {&frame-name}  = string(bf-nfe-it-nota-fisc-rec.item-vUnCom * bf-nfe-it-nota-fisc-rec.item-qtde)
               fi-de-frete        :screen-value in frame {&frame-name}  = string(bf-nfe-it-nota-fisc-rec.item-vfrete)
               fi-i-cod-emite-terc:screen-value in frame {&frame-name}  = string(bf-nfe-it-nota-fisc-rec.cod-emit-terc).
               fi-c-nat-terc      :screen-value in frame {&frame-name}  = bf-nfe-it-nota-fisc-rec.nat-terc     . 
               fi-c-ser-terc      :screen-value in frame {&frame-name}  = bf-nfe-it-nota-fisc-rec.serie-terc   .   
               fi-i-nro-docto-terc:screen-value in frame {&frame-name}  = bf-nfe-it-nota-fisc-rec.nro-docto-terc.
               fi-de-seq-terc     :screen-value in frame {&frame-name}  = string(bf-nfe-it-nota-fisc-rec.seq-terc).
               fi-ct-codigo       :screen-value in frame {&frame-name}  = bf-nfe-it-nota-fisc-rec.ct-codigo.
               fi-sc-codigo       :screen-value in frame {&frame-name}  = bf-nfe-it-nota-fisc-rec.sc-codigo.
               fi-cod-unid-negoc  :screen-value in frame {&frame-name}  = bf-nfe-it-nota-fisc-rec.cod-unid-negoc.


               




        /* --- Retona o Tipo de Nota de acordo com a Natureza --- */
/*         RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001. */
        RUN pi_valida_tipo_nota IN h_bonfe001 (INPUT  bf-nfe-it-nota-fisc-rec.item-nat-operacao,
                                               OUTPUT i-tipo-nfe,
                                               OUTPUT i-tp-oper-terc).
/*         DELETE PROCEDURE h_bonfe001. */
        
        /* --- Devolucao de Clientes --- */
        IF i-tipo-nfe = 2 THEN
            ASSIGN fi-nro-docto-refer   = bf-nfe-it-nota-fisc-rec.item-nr-nota-fis
                   fi-serie-docto-refer = bf-nfe-it-nota-fisc-rec.item-serie-nf.
        /* --- Retorno Terceiros --- */
        IF i-tipo-nfe = 3 THEN
            ASSIGN fi-nro-docto-refer   = bf-nfe-it-nota-fisc-rec.item-nro-docto
                   fi-serie-docto-refer = bf-nfe-it-nota-fisc-rec.item-serie-docto.

        IF bf-nfe-it-nota-fisc-rec.item-num-ordem = 0 THEN DO:
            FIND FIRST ordem-compra WHERE ordem-compra.it-codigo  = bf-nfe-it-nota-fisc-rec.it-codigo
                                      AND ordem-compra.num-pedido = bf-nfe-it-nota-fisc-rec.item-xPed
                                      AND ordem-compra.situacao   = 2 /* --- Confirmada --- */
                                    NO-LOCK NO-ERROR.

            IF AVAIL ordem-compra THEN
                ASSIGN fi-i-num-ordem                   = ordem-compra.numero-ordem
                       fi-de-pre-ordem     :SCREEN-VALUE IN FRAME {&FRAME-NAME}     = string(ordem-compra.preco-unit /*- ordem-compra.valor-descto */ )
                       fi-de-desconto-ordem:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = string(ordem-compra.valor-descto).
        END.
        ELSE DO:

            
            FIND FIRST ordem-compra no-lock
                WHERE ordem-compra.numero-ordem  = bf-nfe-it-nota-fisc-rec.item-num-ordem NO-ERROR.
                
            IF AVAIL ordem-compra THEN DO:
            
                ASSIGN fi-de-pre-ordem:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = string(ordem-compra.preco-unit /*- ordem-compra.valor-descto*/ )
                       fi-de-desconto-ordem:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = string(ordem-compra.valor-descto).
                
            END.



        END.

        
        RUN pi-origem-ordem.



        FIND FIRST ITEM WHERE ITEM.it-codigo = bf-nfe-it-nota-fisc-rec.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN ASSIGN fi-c-desc-item = ITEM.desc-item.

        FIND FIRST natur-oper WHERE natur-oper.nat-operacao = fi-c-natur-oper NO-LOCK NO-ERROR.

        IF AVAIL natur-oper THEN DO:

             IF natur-oper.tipo-compra = 4 THEN DO:
                ASSIGN fi-c-cod-depos  :SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
                       fi-c-cod-localiz:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
                       fi-c-cod-depos  :SENSITIVE IN FRAME {&FRAME-NAME} = NO
                       fi-c-cod-localiz:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

            END.
            ELSE DO:
                ASSIGN fi-c-cod-depos  :SENSITIVE IN FRAME {&FRAME-NAME} = YES
                       fi-c-cod-localiz:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

                ASSIGN fi-c-denominacao = natur-oper.denominacao
                       fi-c-esp-nat     = natur-oper.especie-doc.
            END.


        END.

        FIND FIRST nfe-it-param-rec no-lock
                    WHERE nfe-it-param-rec.cod-parametro      = "param_manutencao_itens"
                    AND   nfe-it-param-rec.cod-item-parametro = 'deposito' NO-ERROR.
                    
            IF  nfe-it-param-rec.valor-1-item-parametro = "NAO" THEN
                fi-c-cod-depos  :SENSITIVE IN FRAME {&FRAME-NAME} = NO.

        FIND FIRST classif-fisc WHERE classif-fisc.class-fiscal = REPLACE(fi-c-class-fiscal,".","") NO-LOCK NO-ERROR.
        IF AVAIL classif-fisc THEN ASSIGN fi-c-desc-class-fiscal = classif-fisc.descricao.

        FIND FIRST referencia WHERE referencia.cod-refer = fi-c-cod-refer NO-LOCK NO-ERROR.
        IF AVAIL referencia THEN ASSIGN fi-c-desc-refer = referencia.descricao.

        FIND FIRST deposito WHERE deposito.cod-depos = fi-c-cod-depos NO-LOCK NO-ERROR.
        IF AVAIL deposito THEN ASSIGN fi-c-nome-depos = deposito.nome.
    
        FIND FIRST conta-contab WHERE conta-contab.conta-contabil = fi-c-conta-contabil NO-LOCK NO-ERROR.
        IF AVAIL conta-contab THEN ASSIGN fi-c-desc-conta-contabil = conta-contab.titulo.
    END.
        
    APPLY "LEAVE" TO fi-c-cod-depos.

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-inicializa-bo W-Win2 
PROCEDURE pi-inicializa-bo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001.

RUN inbo\boin176.p PERSISTEN SET h-boin176.
                    
                    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-nosso-preco W-Win2 
PROCEDURE pi-nosso-preco :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


ASSIGN fi-de-total-nosso:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = string(INPUT FRAME {&FRAME-NAME} fi-de-quantidade * 
                                                                         INPUT FRAME {&FRAME-NAME} fi-de-pre-ordem).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-origem-ordem W-Win2 
PROCEDURE pi-origem-ordem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        DEFINE VARIABLE i-contador      AS INTEGER.
        DEFINE VARIABLE xped-char-aux   LIKE nfe-it-nota-fisc-rec.item-xped-char.

        ASSIGN xped-char-aux = ''.

        DO i-contador = 1 TO LENGTH(bf-nfe-it-nota-fisc-rec.item-xped-char):

           IF (ASC(SUBSTRING(bf-nfe-it-nota-fisc-rec.item-xped-char,i-contador,1)) >= 48) AND
              (ASC(SUBSTRING(bf-nfe-it-nota-fisc-rec.item-xped-char,i-contador,1)) <= 57) THEN 
                ASSIGN xped-char-aux = xped-char-aux + (SUBSTRING(bf-nfe-it-nota-fisc-rec.item-xped-char,i-contador,1)).
        END.
        
        IF /*bf-nfe-it-nota-fisc-rec.item-num-ordem*/  fi-i-num-ordem:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = xped-char-aux /*leia int(bf-nfe-it-nota-fisc-rec.item-xped-char)*/ AND
           /*bf-nfe-it-nota-fisc-rec.item-parcela-oc*/ fi-i-parcela-oc  = bf-nfe-it-nota-fisc-rec.item-nitemped        AND
      /*bf-nfe-it-nota-fisc-rec.item-xped-char leia*/  xped-char-aux    <> "" THEN DO:
        
           
            ASSIGN fi-ordem-xml = "Origem XML".
            ASSIGN fi-ordem-xml:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Origem XML".
        END.
        ELSE DO:
        
            ASSIGN fi-ordem-xml:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                   fi-ordem-xml = "".    .
        END.

        DISP fi-ordem-xml 
            with FRAME {&FRAME-NAME} .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-sugestao-conta W-Win2 
PROCEDURE pi-sugestao-conta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


IF fi-c-conta-contabil:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" OR 
          fi-ct-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:

    FOR EACH it-nota-fisc NO-LOCK
        WHERE it-nota-fisc.cod-estabel = bf-nfe-nota-fiscal-rec.cod-estabel
        AND   it-nota-fisc.serie       = fi-serie-docto-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND   it-nota-fisc.nr-nota-fis = fi-nro-docto-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND   it-nota-fisc.nr-seq-fat  = int(fi-seq-docto-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME}):
    
    
       FIND FIRST nota-fiscal OF it-nota-fisc NO-LOCK NO-ERROR.
           
       FIND FIRST movto-estoq NO-LOCK
           where movto-estoq.cod-estabel  = it-nota-fisc.cod-estabel
           and   movto-estoq.serie-docto  = it-nota-fisc.serie      
           and   movto-estoq.nro-docto    = it-nota-fisc.nr-nota-fis
           and   movto-estoq.cod-emitente = nota-fiscal.cod-emitente 
           and   movto-estoq.nat-operacao = it-nota-fisc.nat-operacao 
           AND   movto-estoq.sequen-nf    = it-nota-fisc.nr-seq-fat 
           AND   movto-estoq.tipo-trans   = 1 NO-ERROR.
    
       IF AVAIL movto-estoq THEN DO:

       
           ASSIGN fi-c-conta-contabil:SCREEN-VALUE IN FRAME {&FRAME-NAME} = movto-estoq.conta-contabil.
    
           &IF '{&bf_dis_versao_ems}' >= '2.08' &THEN
                   ASSIGN fi-ct-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = movto-estoq.ct-codigo
                          fi-sc-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = movto-estoq.sc-codigo 
                          fi-cod-unid-negoc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = movto-estoq.cod-unid-negoc.
    
          &ENDIF
       END.
       MESSAGE "akientrei"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       ASSIGN fi-c-class-fiscal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = it-nota-fisc.class-fiscal.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win2  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win2 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

