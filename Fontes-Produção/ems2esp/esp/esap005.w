&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emsfnd           PROGRESS
          ems2cademp       PROGRESS
          ems2cadme        PROGRESS
          ems2movemp       PROGRESS
          ems5             PROGRESS
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESAP005 2.06.00.005}
    
{cdp/cdcfgmat.i}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


DEFINE VARIABLE c-observa AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-tipo-compra AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-documentos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES docum-est item-doc-est usuar_mestre ~
dupli-apagar tit_ap movto_tit_ap emitente natur-oper cfop-natur ITEM

/* Definitions for BROWSE br-documentos                                 */
&Scoped-define FIELDS-IN-QUERY-br-documentos docum-est.cod-estabel docum-est.esp-docto docum-est.nat-operacao natur-oper.denominacao natur-oper.cod-cfop cfop-natur.des-cfop docum-est.serie-docto docum-est.nro-docto docum-est.cod-emitente emitente.nome-emit docum-est.usuario usuar_mestre.nom_usuario item-doc-est.sequencia item-doc-est.it-codigo ITEM.desc-item item-doc-est.quantidade item-doc-est.preco-unit[1] item-doc-est.preco-total[1] docum-est.dt-emissao docum-est.dt-trans docum-est.dt-atualiza movto_tit_ap.dat_gerac_movto movto_tit_ap.hra_gerac_movto fi-observa (docum-est.cod-observa) @ c-observa fi-tipo-compra (natur-oper.tipo-compra) @ c-tipo-compra LOGICAL (docum-est.nat-operacao BEGINS '3') &if "{&bf_mat_versao_ems}" >= "2.07" &then TRIM(docum-est.cod-chave-aces-nf-eletro) &else TRIM(SUBSTRING(docum-est.char-1,93,60)) &endif /* c-chave-nfe-aux = REPLACE(REPLACE(REPLACE(REPLACE(c-chave-nfe-aux,".",""),"/",""),"-",""),",",""). */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-documentos   
&Scoped-define SELF-NAME br-documentos
&Scoped-define QUERY-STRING-br-documentos FOR EACH docum-est NO-LOCK       WHERE docum-est.usuario     >= INPUT FRAME {&FRAME-NAME} fi-cod_usuario-ini       AND   docum-est.usuario     <= INPUT FRAME {&FRAME-NAME} fi-cod_usuario-fim       AND   docum-est.dt-trans    >= INPUT FRAME {&FRAME-NAME} fi-dt-trans-ini       AND   docum-est.dt-trans    <= INPUT FRAME {&FRAME-NAME} fi-dt-trans-fim       AND   docum-est.cod-estabel >= INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini       AND   docum-est.cod-estabel <= INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fim       AND   docum-est.dt-atualiza >= INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-ini       AND   docum-est.dt-atualiza <= INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-fim       , ~
       EACH item-doc-est OF docum-est NO-LOCK       , ~
       FIRST usuar_mestre NO-LOCK WHERE usuar_mestre.cod_usuario = docum-est.usuario OUTER-JOIN       , ~
       FIRST dupli-apagar NO-LOCK OUTER-JOIN             WHERE dupli-apagar.serie-docto  = docum-est.serie-docto             AND   dupli-apagar.nro-docto    = docum-est.nro-docto             AND   dupli-apagar.cod-emitente = docum-est.cod-emitente             AND   dupli-apagar.nat-operacao = docum-est.nat-operacao       , ~
       FIRST tit_ap NO-LOCK OUTER-JOIN             where tit_ap.cod_empresa    = dupli-apagar.ep-codigo             and tit_ap.cdn_fornecedor     = docum-est.cod-emitente             and tit_ap.cod_estab    = docum-est.cod-estabel             and tit_ap.cod_espec_docto        = dupli-apagar.cod-esp             and tit_ap.cod_ser_docto          = dupli-apagar.serie-docto             and tit_ap.cod_tit_ap       = dupli-apagar.nr-duplic             and tit_ap.cod_parcela        = dupli-apagar.parcela       , ~
       FIRST movto_tit_ap OF tit_ap NO-LOCK OUTER-JOIN       , ~
       FIRST emitente OF docum-est NO-LOCK OUTER-JOIN       , ~
       FIRST natur-oper OF docum-est NO-LOCK OUTER-JOIN       , ~
       FIRST cfop-natur OF natur-oper NO-LOCK       , ~
       FIRST ITEM OF item-doc-est NO-LOCK       INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-documentos OPEN QUERY {&SELF-NAME} FOR EACH docum-est NO-LOCK       WHERE docum-est.usuario     >= INPUT FRAME {&FRAME-NAME} fi-cod_usuario-ini       AND   docum-est.usuario     <= INPUT FRAME {&FRAME-NAME} fi-cod_usuario-fim       AND   docum-est.dt-trans    >= INPUT FRAME {&FRAME-NAME} fi-dt-trans-ini       AND   docum-est.dt-trans    <= INPUT FRAME {&FRAME-NAME} fi-dt-trans-fim       AND   docum-est.cod-estabel >= INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini       AND   docum-est.cod-estabel <= INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fim       AND   docum-est.dt-atualiza >= INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-ini       AND   docum-est.dt-atualiza <= INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-fim       , ~
       EACH item-doc-est OF docum-est NO-LOCK       , ~
       FIRST usuar_mestre NO-LOCK WHERE usuar_mestre.cod_usuario = docum-est.usuario OUTER-JOIN       , ~
       FIRST dupli-apagar NO-LOCK OUTER-JOIN             WHERE dupli-apagar.serie-docto  = docum-est.serie-docto             AND   dupli-apagar.nro-docto    = docum-est.nro-docto             AND   dupli-apagar.cod-emitente = docum-est.cod-emitente             AND   dupli-apagar.nat-operacao = docum-est.nat-operacao       , ~
       FIRST tit_ap NO-LOCK OUTER-JOIN             where tit_ap.cod_empresa    = dupli-apagar.ep-codigo             and tit_ap.cdn_fornecedor     = docum-est.cod-emitente             and tit_ap.cod_estab    = docum-est.cod-estabel             and tit_ap.cod_espec_docto        = dupli-apagar.cod-esp             and tit_ap.cod_ser_docto          = dupli-apagar.serie-docto             and tit_ap.cod_tit_ap       = dupli-apagar.nr-duplic             and tit_ap.cod_parcela        = dupli-apagar.parcela       , ~
       FIRST movto_tit_ap OF tit_ap NO-LOCK OUTER-JOIN       , ~
       FIRST emitente OF docum-est NO-LOCK OUTER-JOIN       , ~
       FIRST natur-oper OF docum-est NO-LOCK OUTER-JOIN       , ~
       FIRST cfop-natur OF natur-oper NO-LOCK       , ~
       FIRST ITEM OF item-doc-est NO-LOCK       INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-documentos docum-est item-doc-est ~
usuar_mestre dupli-apagar tit_ap movto_tit_ap emitente natur-oper ~
cfop-natur ITEM
&Scoped-define FIRST-TABLE-IN-QUERY-br-documentos docum-est
&Scoped-define SECOND-TABLE-IN-QUERY-br-documentos item-doc-est
&Scoped-define THIRD-TABLE-IN-QUERY-br-documentos usuar_mestre
&Scoped-define FOURTH-TABLE-IN-QUERY-br-documentos dupli-apagar
&Scoped-define FIFTH-TABLE-IN-QUERY-br-documentos tit_ap
&Scoped-define SIXTH-TABLE-IN-QUERY-br-documentos movto_tit_ap
&Scoped-define SEVENTH-TABLE-IN-QUERY-br-documentos emitente
&Scoped-define EIGHTH-TABLE-IN-QUERY-br-documentos natur-oper
&Scoped-define NINTH-TABLE-IN-QUERY-br-documentos cfop-natur
&Scoped-define TENTH-TABLE-IN-QUERY-br-documentos ITEM


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-documentos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 RECT-2 IMAGE-7 IMAGE-8 ~
IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-13 IMAGE-12 IMAGE-14 bt-excel ~
fi-cod_usuario-ini fi-cod_usuario-fim fi-dt-trans-ini fi-dt-trans-fim ~
fi-dt-atualiza-ini fi-dt-atualiza-fim bt-buscar fi-cod-estabel-ini ~
fi-cod-estabel-fim br-documentos fi-total-notas fi-total-itens 
&Scoped-Define DISPLAYED-OBJECTS fi-cod_usuario-ini fi-cod_usuario-fim ~
fi-dt-trans-ini fi-dt-trans-fim fi-dt-atualiza-ini fi-dt-atualiza-fim ~
fi-cod-estabel-ini fi-cod-estabel-fim fi-total-notas fi-total-itens 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fi-observa w-livre 
FUNCTION fi-observa RETURNS CHARACTER
  ( i-cod-observa AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fi-tipo-compra w-livre 
FUNCTION fi-tipo-compra RETURNS CHARACTER
  ( i-tipo-compra AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-buscar 
     LABEL "Buscar" 
     SIZE 9 BY 1.25.

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Excel" 
     SIZE 4.72 BY 1.13 TOOLTIP "Gera Excel".

DEFINE VARIABLE fi-cod-estabel-fim AS CHARACTER FORMAT "X(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-ini AS CHARACTER FORMAT "X(3)" 
     LABEL "Estab":R7 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod_usuario-fim AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod_usuario-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Usu rio":R9 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-atualiza-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-atualiza-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Atualiza‡Æo":R17 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-trans-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-trans-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Transa‡Æo":R17 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-total-itens AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Total Itens Notas" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-total-notas AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Total de Notas" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89.86 BY 4.46.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89.86 BY 9.13.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-documentos FOR 
      docum-est, 
      item-doc-est, 
      usuar_mestre, 
      dupli-apagar, 
      tit_ap, 
      movto_tit_ap, 
      emitente, 
      natur-oper, 
      cfop-natur, 
      ITEM SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-documentos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-documentos w-livre _FREEFORM
  QUERY br-documentos NO-LOCK DISPLAY
      docum-est.cod-estabel     FORMAT "x(3)":U
      docum-est.esp-docto       FORMAT ">9":U
      docum-est.nat-operacao    FORMAT "x(06)":U
      natur-oper.denominacao    FORMAT "x(30)":U
      natur-oper.cod-cfop       FORMAT "x(06)":U
      cfop-natur.des-cfop       FORMAT "x(30)":U
      docum-est.serie-docto     FORMAT "x(5)":U
      docum-est.nro-docto       FORMAT "x(16)":U
      docum-est.cod-emitente    FORMAT ">>>>>>>>9":U
      emitente.nome-emit        FORMAT "x(25)":U
      docum-est.usuario         FORMAT "x(12)":U
      usuar_mestre.nom_usuario
      item-doc-est.sequencia
      item-doc-est.it-codigo
      ITEM.desc-item            FORMAT "x(30)":U
      item-doc-est.quantidade
      item-doc-est.preco-unit[1]
      item-doc-est.preco-total[1]
      docum-est.dt-emissao
      docum-est.dt-trans
      docum-est.dt-atualiza
      movto_tit_ap.dat_gerac_movto
      movto_tit_ap.hra_gerac_movto
      fi-observa (docum-est.cod-observa) @ c-observa FORMAT "X(15)" COLUMN-LABEL "Observa‡Æo"
      fi-tipo-compra (natur-oper.tipo-compra) @ c-tipo-compra FORMAT "X(15)" COLUMN-LABEL "Tipo Compra"
      LOGICAL (docum-est.nat-operacao BEGINS '3') COLUMN-LABEL "Importado" FORMAT "SIM/NÇO"
      &if  "{&bf_mat_versao_ems}"  >=  "2.07" &then
          TRIM(docum-est.cod-chave-aces-nf-eletro)      FORMAT "X(60)" COLUMN-LABEL "Chave Acesso"
      &else
          TRIM(SUBSTRING(docum-est.char-1,93,60))       FORMAT "X(60)" COLUMN-LABEL "Chave Acesso"
      &endif

/* c-chave-nfe-aux = REPLACE(REPLACE(REPLACE(REPLACE(c-chave-nfe-aux,".",""),"/",""),"-",""),",",""). */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 88 BY 7.5
         TITLE "Documentos" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-excel AT ROW 1.21 COL 68 WIDGET-ID 52
     fi-cod_usuario-ini AT ROW 2.75 COL 16 COLON-ALIGNED HELP
          "C¢digo Usu rio" WIDGET-ID 6
     fi-cod_usuario-fim AT ROW 2.75 COL 39.43 COLON-ALIGNED HELP
          "C¢digo Usu rio" NO-LABEL WIDGET-ID 8
     fi-dt-trans-ini AT ROW 3.71 COL 17 COLON-ALIGNED WIDGET-ID 30
     fi-dt-trans-fim AT ROW 3.71 COL 39.43 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     fi-dt-atualiza-ini AT ROW 4.71 COL 17 COLON-ALIGNED WIDGET-ID 56
     fi-dt-atualiza-fim AT ROW 4.71 COL 39.43 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     bt-buscar AT ROW 5.38 COL 79 WIDGET-ID 26
     fi-cod-estabel-ini AT ROW 5.71 COL 23 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" WIDGET-ID 40
     fi-cod-estabel-fim AT ROW 5.71 COL 39.29 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" NO-LABEL WIDGET-ID 46
     br-documentos AT ROW 7.29 COL 2 WIDGET-ID 200
     fi-total-notas AT ROW 15.04 COL 38.57 COLON-ALIGNED WIDGET-ID 50
     fi-total-itens AT ROW 15.04 COL 71.29 COLON-ALIGNED WIDGET-ID 48
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.54 COL 1.14 WIDGET-ID 2
     RECT-2 AT ROW 7.17 COL 1.14 WIDGET-ID 4
     IMAGE-7 AT ROW 2.75 COL 32.29 WIDGET-ID 22
     IMAGE-8 AT ROW 2.75 COL 38.29 WIDGET-ID 24
     IMAGE-9 AT ROW 3.71 COL 32.29 WIDGET-ID 32
     IMAGE-10 AT ROW 3.71 COL 38.29 WIDGET-ID 34
     IMAGE-11 AT ROW 5.71 COL 32.29 WIDGET-ID 44
     IMAGE-13 AT ROW 4.71 COL 32.29 WIDGET-ID 60
     IMAGE-12 AT ROW 5.71 COL 38.29 WIDGET-ID 42
     IMAGE-14 AT ROW 4.71 COL 38.29 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 15.29 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Documentos por Usu rio"
         HEIGHT             = 15.29
         WIDTH              = 90
         MAX-HEIGHT         = 20.21
         MAX-WIDTH          = 104.29
         VIRTUAL-HEIGHT     = 20.21
         VIRTUAL-WIDTH      = 104.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-documentos fi-cod-estabel-fim f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-documentos
/* Query rebuild information for BROWSE br-documentos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH docum-est NO-LOCK
      WHERE docum-est.usuario     >= INPUT FRAME {&FRAME-NAME} fi-cod_usuario-ini
      AND   docum-est.usuario     <= INPUT FRAME {&FRAME-NAME} fi-cod_usuario-fim
      AND   docum-est.dt-trans    >= INPUT FRAME {&FRAME-NAME} fi-dt-trans-ini
      AND   docum-est.dt-trans    <= INPUT FRAME {&FRAME-NAME} fi-dt-trans-fim
      AND   docum-est.cod-estabel >= INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini
      AND   docum-est.cod-estabel <= INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fim
      AND   docum-est.dt-atualiza >= INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-ini
      AND   docum-est.dt-atualiza <= INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-fim
      , EACH item-doc-est OF docum-est NO-LOCK
      , FIRST usuar_mestre NO-LOCK WHERE usuar_mestre.cod_usuario = docum-est.usuario OUTER-JOIN
      , FIRST dupli-apagar NO-LOCK OUTER-JOIN
            WHERE dupli-apagar.serie-docto  = docum-est.serie-docto
            AND   dupli-apagar.nro-docto    = docum-est.nro-docto
            AND   dupli-apagar.cod-emitente = docum-est.cod-emitente
            AND   dupli-apagar.nat-operacao = docum-est.nat-operacao
      , FIRST tit_ap NO-LOCK OUTER-JOIN
            where tit_ap.cod_empresa    = dupli-apagar.ep-codigo
            and tit_ap.cdn_fornecedor     = docum-est.cod-emitente
            and tit_ap.cod_estab    = docum-est.cod-estabel
            and tit_ap.cod_espec_docto        = dupli-apagar.cod-esp
            and tit_ap.cod_ser_docto          = dupli-apagar.serie-docto
            and tit_ap.cod_tit_ap       = dupli-apagar.nr-duplic
            and tit_ap.cod_parcela        = dupli-apagar.parcela
      , FIRST movto_tit_ap OF tit_ap NO-LOCK OUTER-JOIN
      , FIRST emitente OF docum-est NO-LOCK OUTER-JOIN
      , FIRST natur-oper OF docum-est NO-LOCK OUTER-JOIN
      , FIRST cfop-natur OF natur-oper NO-LOCK
      , FIRST ITEM OF item-doc-est NO-LOCK
      INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "ems2movemp.docum-est.nro-docto = ""0000001""
 AND ems2movemp.docum-est.nat-operacao = ""2949"""
     _Query            is OPENED
*/  /* BROWSE br-documentos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Documentos por Usu rio */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Documentos por Usu rio */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-buscar w-livre
ON CHOOSE OF bt-buscar IN FRAME f-cad /* Buscar */
DO:
    DEFINE VARIABLE iContItens AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iContNotas AS INTEGER     NO-UNDO.

    CLOSE QUERY br-documentos.

    OPEN QUERY br-documentos FOR EACH docum-est NO-LOCK
      WHERE docum-est.usuario     >= INPUT FRAME {&FRAME-NAME} fi-cod_usuario-ini
      AND   docum-est.usuario     <= INPUT FRAME {&FRAME-NAME} fi-cod_usuario-fim
      AND   docum-est.dt-trans    >= INPUT FRAME {&FRAME-NAME} fi-dt-trans-ini
      AND   docum-est.dt-trans    <= INPUT FRAME {&FRAME-NAME} fi-dt-trans-fim
      AND   docum-est.cod-estabel >= INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini
      AND   docum-est.cod-estabel <= INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fim
      AND   docum-est.dt-atualiza >= INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-ini
      AND   docum-est.dt-atualiza <= INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-fim
      , EACH item-doc-est OF docum-est NO-LOCK
      , FIRST usuar_mestre NO-LOCK WHERE usuar_mestre.cod_usuario = docum-est.usuario OUTER-JOIN
      , FIRST dupli-apagar NO-LOCK OUTER-JOIN
            WHERE dupli-apagar.serie-docto  = docum-est.serie-docto 
            AND   dupli-apagar.nro-docto    = docum-est.nro-docto 
            AND   dupli-apagar.cod-emitente = docum-est.cod-emitente 
            AND   dupli-apagar.nat-operacao = docum-est.nat-operacao
        , FIRST tit_ap NO-LOCK OUTER-JOIN
              where tit_ap.cod_empresa    = dupli-apagar.ep-codigo
              and tit_ap.cdn_fornecedor     = docum-est.cod-emitente
              and tit_ap.cod_estab    = docum-est.cod-estabel
              and tit_ap.cod_espec_docto        = dupli-apagar.cod-esp
              and tit_ap.cod_ser_docto          = dupli-apagar.serie-docto
              and tit_ap.cod_tit_ap       = dupli-apagar.nr-duplic
              and tit_ap.cod_parcela        = dupli-apagar.parcela
        , FIRST movto_tit_ap OF tit_ap NO-LOCK OUTER-JOIN
        , FIRST emitente OF docum-est NO-LOCK OUTER-JOIN
        , FIRST natur-oper OF docum-est NO-LOCK OUTER-JOIN
        , FIRST cfop-natur OF natur-oper NO-LOCK
        , FIRST ITEM OF item-doc-est NO-LOCK
      INDEXED-REPOSITION.  

    ASSIGN iContItens = 0.
    ASSIGN iContNotas = 0.
    FOR EACH docum-est NO-LOCK
        WHERE docum-est.usuario     >= INPUT FRAME {&FRAME-NAME} fi-cod_usuario-ini
        AND   docum-est.usuario     <= INPUT FRAME {&FRAME-NAME} fi-cod_usuario-fim
        AND   docum-est.dt-trans    >= INPUT FRAME {&FRAME-NAME} fi-dt-trans-ini
        AND   docum-est.dt-trans    <= INPUT FRAME {&FRAME-NAME} fi-dt-trans-fim
        AND   docum-est.cod-estabel >= INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini
        AND   docum-est.cod-estabel <= INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fim
        AND   docum-est.dt-atualiza >= INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-ini
        AND   docum-est.dt-atualiza <= INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-fim:
        ASSIGN iContNotas = iContNotas + 1.

        FOR EACH item-doc-est OF docum-est NO-LOCK
          , EACH usuar_mestre NO-LOCK WHERE usuar_mestre.cod_usuario = docum-est.usuario
            , FIRST emitente OF docum-est NO-LOCK
            , FIRST natur-oper OF docum-est NO-LOCK
            , FIRST cfop-natur OF natur-oper NO-LOCK
            , FIRST ITEM OF item-doc-est NO-LOCK
            :
            ASSIGN iContItens = iContItens + 1.
        END.
    END.

    fi-total-notas:SCREEN-VALUE IN FRAME {&frame-name} = STRING (iContNotas).
    fi-total-itens:SCREEN-VALUE IN FRAME {&frame-name} = STRING (iContItens).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel w-livre
ON CHOOSE OF bt-excel IN FRAME f-cad /* Excel */
DO:
    DEFINE VARIABLE excelappl   AS COM-HANDLE  NO-UNDO.
    DEFINE VARIABLE cArquivo    AS CHARACTER   NO-UNDO.
  
    ASSIGN cArquivo = Session:TEMP-DIRECTORY + "esap005.csv".

    OUTPUT TO VALUE (cArquivo) NO-CONVERT.

    PUT UNFORMATTED 
        "Est"
        ";Esp"
        ";Natureza"
        ";Denomina‡Æo"
        ";CFOP"
        ";Desc. CFOP"
        ";Serie"
        ";Documento"
        ";Emitente"
        ";Nome Emit"
        ";Usu rio"
        ";Nome Usu rio"
        ";Seq."
        ";Item"
        ";Desc Item"
        ";Nossa Qtde"
        ";Pre‡o Unit rio"
        ";Pre‡o Total"
        ";Dt.EmissÆo"
        ";Dt.Transa‡Æo"
        ";Dt.Atualiza‡Æo"
        ";Dt.Maq."
        ";Hr.Maq."
        ";Observa‡Æo"
        ";Tipo Compra"
        ";Importado"
        ";Chave Acesso"
        SKIP.

    FOR EACH docum-est NO-LOCK
        WHERE docum-est.usuario     >= INPUT FRAME {&FRAME-NAME} fi-cod_usuario-ini
        AND   docum-est.usuario     <= INPUT FRAME {&FRAME-NAME} fi-cod_usuario-fim
        AND   docum-est.dt-trans    >= INPUT FRAME {&FRAME-NAME} fi-dt-trans-ini
        AND   docum-est.dt-trans    <= INPUT FRAME {&FRAME-NAME} fi-dt-trans-fim
        AND   docum-est.cod-estabel >= INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini
        AND   docum-est.cod-estabel <= INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fim
        AND   docum-est.dt-atualiza >= INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-ini
        AND   docum-est.dt-atualiza <= INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-fim:
        FOR EACH item-doc-est OF docum-est NO-LOCK
          , EACH usuar_mestre NO-LOCK WHERE usuar_mestre.cod_usuario = docum-est.usuario:

            FIND FIRST dupli-apagar NO-LOCK
                 WHERE dupli-apagar.serie-docto  = docum-est.serie-docto 
                   AND dupli-apagar.nro-docto    = docum-est.nro-docto 
                   AND dupli-apagar.cod-emitente = docum-est.cod-emitente 
                   AND dupli-apagar.nat-operacao = docum-est.nat-operacao NO-ERROR.

            FIND FIRST tit_ap NO-LOCK
                 where tit_ap.cod_empresa     = dupli-apagar.ep-codigo
                   and tit_ap.cdn_fornecedor  = docum-est.cod-emitente
                   and tit_ap.cod_estab       = docum-est.cod-estabel
                   and tit_ap.cod_espec_docto = dupli-apagar.cod-esp
                   and tit_ap.cod_ser_docto   = dupli-apagar.serie-docto
                   and tit_ap.cod_tit_ap      = dupli-apagar.nr-duplic
                   and tit_ap.cod_parcela     = dupli-apagar.parcela NO-ERROR.

            FIND FIRST movto_tit_ap OF tit_ap NO-LOCK NO-ERROR.

            FIND FIRST emitente OF docum-est NO-LOCK NO-ERROR.

            FIND FIRST natur-oper OF docum-est NO-LOCK NO-ERROR.

            FIND FIRST cfop-natur OF natur-oper NO-LOCK NO-ERROR.

            FIND FIRST ITEM OF item-doc-est NO-LOCK NO-ERROR.

            PUT UNFORMATTED 
                docum-est.cod-estabel         FORMAT "x(3)":U
                ";" docum-est.esp-docto       FORMAT ">9":U
                ";" docum-est.nat-operacao    FORMAT "x(06)":U
                ";" natur-oper.denominacao    FORMAT "x(30)":U
                ";" natur-oper.cod-cfop       FORMAT "x(06)":U
                ";" cfop-natur.des-cfop       FORMAT "x(30)":U
                ";" docum-est.serie-docto     FORMAT "x(5)":U
                ";" docum-est.nro-docto       FORMAT "x(16)":U
                ";" docum-est.cod-emitente    FORMAT ">>>>>>>>9":U
                ";" emitente.nome-emit        FORMAT "x(25)":U
                ";" docum-est.usuario         FORMAT "x(12)":U
                ";" usuar_mestre.nom_usuario
                ";" item-doc-est.sequencia
                ";" item-doc-est.it-codigo
                ";" ITEM.desc-item            FORMAT "x(30)":U
                ";" item-doc-est.quantidade
                ";" item-doc-est.preco-unit[1]
                ";" item-doc-est.preco-total[1]
                ";" docum-est.dt-emissao
                ";" docum-est.dt-trans
                ";" docum-est.dt-atualiza
                ";" (IF AVAILABLE movto_tit_ap THEN movto_tit_ap.dat_gerac_movto ELSE ?)
                ";" (IF AVAILABLE movto_tit_ap THEN movto_tit_ap.hra_gerac_movto ELSE "")
                ";" fi-observa (docum-est.cod-observa) FORMAT "X(15)"
                ";" fi-tipo-compra (natur-oper.tipo-compra) FORMAT "X(15)" 
                ";" LOGICAL (docum-est.nat-operacao BEGINS '3') FORMAT "SIM/NÇO"
                &if  "{&bf_mat_versao_ems}"  >=  "2.07" &then
                    ";'" TRIM(docum-est.cod-chave-aces-nf-eletro) "'"     FORMAT "X(60)"
                &else
                    ";'" TRIM(SUBSTRING(docum-est.char-1,93,60))  "'"     FORMAT "X(60)" 
                &endif
                SKIP.
        /* c-chave-nfe-aux = REPLACE(REPLACE(REPLACE(REPLACE(c-chave-nfe-aux,".",""),"/",""),"-",""),",",""). */
        END.
    END.
    OUTPUT CLOSE.

    CREATE "excel.application" excelappl.
       excelappl:VISIBLE = TRUE. 
       excelappl:workbooks:OPEN(cArquivo).
       excelappl:range("A1:AA1"):Interior:colorindex = 3.
       excelappl:range("A1:AA1"):FONT:colorindex = 2.
       excelappl:range("A1:AA5000"):FONT:SIZE = 12.
       excelappl:range("A1:AA5000"):COLUMNS:autofit.
/*        excelappl:range("E1:E5000"):Style = "Currency". */
/*        excelappl:range("J1:K5000"):Style = "Currency". */
    RELEASE OBJECT excelappl.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-documentos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-excel:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  DISPLAY fi-cod_usuario-ini fi-cod_usuario-fim fi-dt-trans-ini fi-dt-trans-fim 
          fi-dt-atualiza-ini fi-dt-atualiza-fim fi-cod-estabel-ini 
          fi-cod-estabel-fim fi-total-notas fi-total-itens 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-1 RECT-2 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 IMAGE-11 
         IMAGE-13 IMAGE-12 IMAGE-14 bt-excel fi-cod_usuario-ini 
         fi-cod_usuario-fim fi-dt-trans-ini fi-dt-trans-fim fi-dt-atualiza-ini 
         fi-dt-atualiza-fim bt-buscar fi-cod-estabel-ini fi-cod-estabel-fim 
         br-documentos fi-total-notas fi-total-itens 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "ESAP005" "2.06.00.005"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  fi-dt-trans-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING (TODAY,"99/99/9999").
  fi-dt-trans-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING (TODAY,"99/99/9999").

  APPLY "choose" TO bt-buscar.

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "docum-est"}
  {src/adm/template/snd-list.i "item-doc-est"}
  {src/adm/template/snd-list.i "usuar_mestre"}
  {src/adm/template/snd-list.i "dupli-apagar"}
  {src/adm/template/snd-list.i "tit_ap"}
  {src/adm/template/snd-list.i "movto_tit_ap"}
  {src/adm/template/snd-list.i "emitente"}
  {src/adm/template/snd-list.i "natur-oper"}
  {src/adm/template/snd-list.i "cfop-natur"}
  {src/adm/template/snd-list.i "ITEM"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fi-observa w-livre 
FUNCTION fi-observa RETURNS CHARACTER
  ( i-cod-observa AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE c-Observa AS CHARACTER   NO-UNDO.

    CASE i-cod-observa:
        WHEN 1 THEN ASSIGN c-Observa = "1-Ind£stria".
        WHEN 2 THEN ASSIGN c-Observa = "2-Com‚rcio".
        WHEN 3 THEN ASSIGN c-Observa = "3-Devolu‡Æo Cliente".
        WHEN 4 THEN ASSIGN c-Observa = "4-Servi‡os".
    END CASE.

    RETURN c-Observa.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fi-tipo-compra w-livre 
FUNCTION fi-tipo-compra RETURNS CHARACTER
  ( i-tipo-compra AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE c-tipo-compra AS CHARACTER   NO-UNDO.

    CASE i-tipo-compra:
        WHEN 1 THEN ASSIGN c-tipo-compra = "1-Normal".
        WHEN 2 THEN ASSIGN c-tipo-compra = "2-Frete".
        WHEN 3 THEN ASSIGN c-tipo-compra = "3-Devolu‡Æo Cliente".
        WHEN 4 THEN ASSIGN c-tipo-compra = "4-Material Agregado".
        WHEN 5 THEN ASSIGN c-tipo-compra = "5-Servi‡os Comunica‡äes".
        WHEN 6 THEN ASSIGN c-tipo-compra = "6-Energia El‚trica".
    END CASE.

    RETURN c-tipo-compra.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

