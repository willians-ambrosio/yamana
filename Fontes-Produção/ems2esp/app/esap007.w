&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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
{include/i-prgvrs.i ESAP007 2.06.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var dt-fora-prazo as date no-undo.

def temp-table tt-titulo-erro no-undo
    field ep-codigo   like tit_ap.cod_empresa
    field cod-fornec  like tit_ap.cdn_fornecedor
    field cod-estabel like tit_ap.cod_estab
    field cod-esp     like tit_ap.cod_espec_docto
    field serie       like tit_ap.cod_ser_docto
    field nr-docto    like tit_ap.cod_tit_ap
    field parcela     like tit_ap.cod_parcela
    field cod-erro    as int
    field desc-erro   as char format "x(100)".

def temp-table tt-tit_ap no-undo
    field selecionado as log format "Sim/NÆo"
    field ep-codigo   like tit_ap.cod_empresa
    field cod-estabel like tit_ap.cod_estab
    field cod-fornec  like tit_ap.cdn_fornecedor
    field nome-abrev  like ems5.fornecedor.nom_abrev
    field cod-esp     like tit_ap.cod_espec_docto
    field serie       like tit_ap.cod_ser_docto
    field nr-docto    like tit_ap.cod_tit_ap
    field parcela     like tit_ap.cod_parcela
    field dt-vencimen like tit_ap.dat_vencto_tit
    field vl-original like tit_ap.val_origin_tit_ap
    field valor-saldo like tit_ap.val_sdo_tit_ap
    field excecao     as log
    field dt-maquina  as date
    field fora-prazo  as log
    index idx_1 is primary is unique ep-codigo
                                     cod-fornec
                                     cod-estabel
                                     cod-esp
                                     serie
                                     nr-docto
                                     parcela
    index idx_2 selecionado
    index idx_3 cod-estabel
                cod-fornec
                cod-esp
                serie
                nr-docto
                parcela.

def temp-table tt_tit_ap_alteracao_base_aux_3 no-undo
    field ttv_cod_usuar_corren             as character format "x(12)" label "Usu rio Corrente" column-label "Usu rio Corrente"
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_num_id_tit_ap                as integer format "999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field ttv_rec_tit_ap                   as recid format ">>>>>>9" initial ?
    field tta_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_espec_docto              as character format "x(3)" label "Esp‚cie Documento" column-label "Esp‚cie"
    field tta_cod_ser_docto                as character format "x(3)" label "S‚rie Documento" column-label "S‚rie"
    field tta_cod_tit_ap                   as character format "x(10)" label "T¡tulo" column-label "T¡tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field ttv_dat_transacao                as date format "99/99/9999" initial today label "Data Transa‡Æo" column-label "Data Transa‡Æo"
    field ttv_cod_refer                    as character format "x(10)" label "Referˆncia" column-label "Referˆncia"
    field tta_val_sdo_tit_ap               as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Saldo" column-label "Valor Saldo"
    field tta_dat_emis_docto               as date format "99/99/9999" initial today label "Data  EmissÆo" column-label "Dt EmissÆo"
    field tta_dat_vencto_tit_ap            as date format "99/99/9999" initial today label "Data Vencimento" column-label "Dt Vencto"
    field tta_dat_prev_pagto               as date format "99/99/9999" initial today label "Data Prevista Pgto" column-label "Dt Prev Pagto"
    field tta_dat_ult_pagto                as date format "99/99/9999" initial ? label "Data éltimo Pagto" column-label "Data éltimo Pagto"
    field tta_num_dias_atraso              as integer format ">9" initial 0 label "Dias Atraso" column-label "Dias Atr"
    field tta_val_perc_multa_atraso        as decimal format ">9.99" decimals 2 initial 00.00 label "Perc Multa Atraso" column-label "Multa Atr"
    field tta_val_juros_dia_atraso         as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Juros" column-label "Vl Juros"
    field tta_val_perc_juros_dia_atraso    as decimal format ">9.999999" decimals 6 initial 00.00 label "Perc Jur Dia Atraso" column-label "Perc Dia"
    field tta_dat_desconto                 as date format "99/99/9999" initial ? label "Data Desconto" column-label "Dt Descto"
    field tta_val_perc_desc                as decimal format ">9.9999" decimals 4 initial 0 label "Percentual Desconto" column-label "Perc Descto"
    field tta_val_desconto                 as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Desconto" column-label "Valor Desconto"
    field tta_cod_portador                 as character format "x(5)" label "Portador" column-label "Portador"
    field ttv_cod_portador_mov             as character format "x(5)" label "Portador Movto" column-label "Portador Movto"
    field tta_log_pagto_bloqdo             as logical format "Sim/NÆo" initial no label "Bloqueia Pagamento" column-label "Pagto Bloqdo"
    field tta_cod_seguradora               as character format "x(8)" label "Seguradora" column-label "Seguradora"
    field tta_cod_apol_seguro              as character format "x(12)" label "Ap¢lice Seguro" column-label "Apolice Seguro"
    field tta_cod_arrendador               as character format "x(6)" label "Arrendador" column-label "Arrendador"
    field tta_cod_contrat_leas             as character format "x(12)" label "Contrato Leasing" column-label "Contr Leas"
    field tta_ind_tip_espec_docto          as character format "X(17)" initial "Normal" label "Tipo Esp‚cie" column-label "Tipo Esp‚cie"
    field tta_cod_indic_econ               as character format "x(8)" label "Moeda" column-label "Moeda"
    field tta_num_seq_refer                as integer format ">>>9" initial 0 label "Sequˆncia" column-label "Seq"
    field ttv_ind_motiv_alter_val_tit_ap   as character format "X(09)" initial "Altera‡Æo" label "Motivo Altera‡Æo" column-label "Motivo Altera‡Æo"
    field ttv_wgh_lista                    as widget-handle extent 15 format ">>>>>>9"
    field ttv_log_gera_ocor_alter_valores  as logical format "Sim/NÆo" initial no
    field tta_cb4_tit_ap_bco_cobdor        as Character format "x(50)" label "Titulo Bco Cobrador" column-label "Titulo Bco Cobrador"
    field tta_cod_histor_padr              as character format "x(8)" label "Hist½rico Padr’o" column-label "Hist½rico Padr’o"
    field tta_des_histor_padr              as character format "x(40)" label "Descri‡Æo" column-label "Descri‡Æo Hist½rico Padr’o"
    field tta_ind_sit_tit_ap               as character format "X(13)" label "Situa‡Æo" column-label "Situa‡Æo"
    field tta_cod_forma_pagto              as character format "x(3)" label "Forma Pagamento" column-label "F Pagto"
    field tta_cod_tit_ap_bco_cobdor        as character format "x(20)" label "T¡tulo Banco Cobdor" column-label "T¡tulo Banco Cobdor"
    field tta_cod_estab_ext                as character format "x(8)" label "Estabelecimento Exte" column-label "Estabelecimento Ext"
    field tta_num_ord_invest               as integer format ">>>>,>>9" initial 0 label "Ordem Investimento" column-label "Ordem Investimento"
    field ttv_num_ped_compra               as integer format ">>>>>,>>9" initial 0 label "Ped Compra" column-label "Ped Compra"
    field tta_num_ord_compra               as integer format ">>>>>9,99" initial 0 label "Ordem Compra" column-label "Ordem Compra"
    field ttv_num_event_invest             as integer format ">,>>9" label "Evento Investimento" column-label "Evento Investimento"
    field ttv_val_1099                     as decimal format "->>,>>>,>>>,>>9.99" decimals 2
    field tta_cod_tax_ident_number         as character format "x(15)" label "Tax Id Number" column-label "Tax Id Number"
    field ttv_ind_tip_trans_1099_tt        as character format "X(50)" label "Tipo Transacao 1099" column-label "Tipo Transacao 1099"
    field ttv_log_atualiz_tit_impto_vinc   as logical format "Sim/NÆo" initial no
    index tt_titap_id                     
          tta_cod_estab                    ascending
          tta_cdn_fornecedor               ascending
          tta_cod_espec_docto              ascending
          tta_cod_ser_docto                ascending
          tta_cod_tit_ap                   ascending
          tta_cod_parcela                  ascending
    .

def temp-table tt_tit_ap_alteracao_rateio no-undo
    field ttv_rec_tit_ap                   as recid format ">>>>>>9" initial ?
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_refer                    as character format "x(10)" label "Referˆncia" column-label "Referˆncia"
    field tta_num_seq_refer                as integer format ">>>9" initial 0 label "Sequˆncia" column-label "Seq"
    field tta_cod_tip_fluxo_financ         as character format "x(12)" label "Tipo Fluxo Financ" column-label "Tipo Fluxo Financ"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont bil" column-label "Conta Cont bil"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg½cio" column-label "Un Neg"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field tta_val_aprop_ctbl               as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Aprop Ctbl" column-label "Vl Aprop Ctbl"
    field ttv_ind_tip_rat                  as character format "X(08)"
    field tta_num_id_tit_ap                as integer format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field tta_num_id_aprop_ctbl_ap         as integer format "9999999999" initial 0 label "Id Aprop Ctbl AP" column-label "Id Aprop Ctbl AP"
    index tt_aprpctba_id                   is primary unique
          tta_cod_estab                    ascending
          tta_cod_refer                    ascending
          tta_num_seq_refer                ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
          tta_cod_unid_negoc               ascending
          tta_cod_plano_ccusto             ascending
          tta_cod_ccusto                   ascending
          tta_cod_tip_fluxo_financ         ascending
          ttv_rec_tit_ap                   ascending
    .

def temp-table tt_log_erros_tit_ap_alteracao no-undo
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_espec_docto              as character format "x(3)" label "Esp‚cie Documento" column-label "Esp‚cie"
    field tta_cod_ser_docto                as character format "x(3)" label "S‚rie Documento" column-label "S‚rie"
    field tta_cod_tit_ap                   as character format "x(10)" label "T¡tulo" column-label "T¡tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field tta_num_id_tit_ap                as integer format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field ttv_num_mensagem                 as integer format ">>>>,>>9" label "N£mero" column-label "N£mero Mensagem"
    field ttv_cod_tip_msg_dwb              as character format "x(12)" label "Tipo Mensagem" column-label "Tipo Mensagem"
    field ttv_des_msg_erro                 as character format "x(60)" label "Mensagem Erro" column-label "Inconsist¼ncia"
    field ttv_des_msg_ajuda_1              as character format "x(360)"
    field ttv_wgh_focus                    as widget-handle format ">>>>>>9"
    .

def temp-table tt_log_erros_tit_ap_total no-undo like tt_log_erros_tit_ap_alteracao.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-tit-ap

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-tit_ap

/* Definitions for BROWSE br-tit-ap                                     */
&Scoped-define FIELDS-IN-QUERY-br-tit-ap selecionado ep-codigo cod-estabel cod-fornec nome-abrev cod-esp serie nr-docto parcela dt-maquina dt-vencimen vl-original valor-saldo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-tit-ap   
&Scoped-define SELF-NAME br-tit-ap
&Scoped-define QUERY-STRING-br-tit-ap FOR EACH tt-tit_ap
&Scoped-define OPEN-QUERY-br-tit-ap OPEN QUERY {&SELF-NAME} FOR EACH tt-tit_ap.
&Scoped-define TABLES-IN-QUERY-br-tit-ap tt-tit_ap
&Scoped-define FIRST-TABLE-IN-QUERY-br-tit-ap tt-tit_ap


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-tit-ap}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 rt-button-2 bt-filtro ~
br-tit-ap bt-todos bt-nenhum bt-todos-ex bt-nenhum-ex fi-dt-vencimen ~
bt-exportar bt-importar bt-executar bf-fechar 
&Scoped-Define DISPLAYED-OBJECTS fi-dt-vencimen 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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
DEFINE BUTTON bf-fechar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-exportar 
     LABEL "Exportar" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-filtro 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-importar 
     LABEL "Importar" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-nenhum 
     LABEL "Nenhum" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-nenhum-ex 
     LABEL "Nenhum Exce‡Æo" 
     SIZE 12.57 BY 1.

DEFINE BUTTON bt-todos 
     LABEL "Todos" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-todos-ex 
     LABEL "Todos Exce‡Æo" 
     SIZE 12 BY 1.

DEFINE VARIABLE fi-dt-vencimen AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt. Vencimento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 11.58.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.46
     BGCOLOR 7 .

DEFINE RECTANGLE rt-button-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-tit-ap FOR 
      tt-tit_ap SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-tit-ap
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-tit-ap w-livre _FREEFORM
  QUERY br-tit-ap DISPLAY
      selecionado width 3   column-label "Sel"
ep-codigo   width 3
cod-estabel width 3
cod-fornec  width 6
nome-abrev  width 14
cod-esp     width 2.5
serie       width 3.5
nr-docto    width 9
parcela
dt-maquina  width 7.5 format "99/99/99" column-label "Dt.Maq."
dt-vencimen width 7.5 format "99/99/99"
vl-original width 10
valor-saldo width 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 88 BY 8.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-filtro AT ROW 1.17 COL 2.14
     br-tit-ap AT ROW 3.08 COL 1.86
     bt-todos AT ROW 12 COL 3
     bt-nenhum AT ROW 12 COL 16
     bt-todos-ex AT ROW 12 COL 29
     bt-nenhum-ex AT ROW 12 COL 42
     fi-dt-vencimen AT ROW 12 COL 77 COLON-ALIGNED
     bt-exportar AT ROW 13.08 COL 3
     bt-importar AT ROW 13.13 COL 16
     bt-executar AT ROW 14.63 COL 3
     bf-fechar AT ROW 14.63 COL 16
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.67 COL 1
     rt-button-2 AT ROW 14.38 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 15.38
         DEFAULT-BUTTON bt-executar.


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
         TITLE              = "Altera Data Vencto"
         HEIGHT             = 15.38
         WIDTH              = 90
         MAX-HEIGHT         = 19.42
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 19.42
         VIRTUAL-WIDTH      = 90
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
/* BROWSE-TAB br-tit-ap bt-filtro f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-tit-ap
/* Query rebuild information for BROWSE br-tit-ap
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-tit_ap.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-tit-ap */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Altera Data Vencto */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Altera Data Vencto */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bf-fechar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bf-fechar w-livre
ON CHOOSE OF bf-fechar IN FRAME f-cad /* Fechar */
DO:
    apply 'close' to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-tit-ap
&Scoped-define SELF-NAME br-tit-ap
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tit-ap w-livre
ON MOUSE-SELECT-DBLCLICK OF br-tit-ap IN FRAME f-cad
DO:
    if not avail tt-tit_ap then return no-apply.

    assign tt-tit_ap.selecionado = not tt-tit_ap.selecionado.

    if br-tit-ap:refresh() then.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tit-ap w-livre
ON ROW-DISPLAY OF br-tit-ap IN FRAME f-cad
DO:
    if  tt-tit_ap.dt-vencimen < today or
        tt-tit_ap.fora-prazo then
        assign tt-tit_ap.selecionado :fgcolor in browse {&browse-name} = 12
               tt-tit_ap.ep-codigo   :fgcolor in browse {&browse-name} = 12
               tt-tit_ap.cod-estabel :fgcolor in browse {&browse-name} = 12
               tt-tit_ap.cod-fornec  :fgcolor in browse {&browse-name} = 12
               tt-tit_ap.nome-abrev  :fgcolor in browse {&browse-name} = 12
               tt-tit_ap.cod-esp     :fgcolor in browse {&browse-name} = 12
               tt-tit_ap.serie       :fgcolor in browse {&browse-name} = 12
               tt-tit_ap.nr-docto    :fgcolor in browse {&browse-name} = 12
               tt-tit_ap.parcela     :fgcolor in browse {&browse-name} = 12
               tt-tit_ap.dt-vencimen :fgcolor in browse {&browse-name} = 12
               tt-tit_ap.vl-original :fgcolor in browse {&browse-name} = 12
               tt-tit_ap.valor-saldo :fgcolor in browse {&browse-name} = 12
               tt-tit_ap.dt-maquina  :fgcolor in browse {&browse-name} = 12.

    if  tt-tit_ap.excecao then
        assign tt-tit_ap.selecionado :fgcolor in browse {&browse-name} = 9
               tt-tit_ap.ep-codigo   :fgcolor in browse {&browse-name} = 9
               tt-tit_ap.cod-estabel :fgcolor in browse {&browse-name} = 9
               tt-tit_ap.cod-fornec  :fgcolor in browse {&browse-name} = 9
               tt-tit_ap.nome-abrev  :fgcolor in browse {&browse-name} = 9
               tt-tit_ap.cod-esp     :fgcolor in browse {&browse-name} = 9
               tt-tit_ap.serie       :fgcolor in browse {&browse-name} = 9
               tt-tit_ap.nr-docto    :fgcolor in browse {&browse-name} = 9
               tt-tit_ap.parcela     :fgcolor in browse {&browse-name} = 9
               tt-tit_ap.dt-vencimen :fgcolor in browse {&browse-name} = 9
               tt-tit_ap.vl-original :fgcolor in browse {&browse-name} = 9
               tt-tit_ap.valor-saldo :fgcolor in browse {&browse-name} = 9
               tt-tit_ap.dt-maquina  :fgcolor in browse {&browse-name} = 9.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-livre
ON CHOOSE OF bt-executar IN FRAME f-cad /* Executar */
DO:
    if not avail tt-tit_ap then return no-apply.

    run pi-verifica-permissao.
    if return-value <> "OK" then return no-apply.

    do on error undo, return no-apply:
        run pi-executar.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exportar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exportar w-livre
ON CHOOSE OF bt-exportar IN FRAME f-cad /* Exportar */
DO:
    if not avail tt-tit_ap then return no-apply.
    do on error undo, return no-apply:
        run pi-exportar.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro w-livre
ON CHOOSE OF bt-filtro IN FRAME f-cad
DO:
    run pi-verifica-permissao.
    if return-value <> "OK" then return no-apply.

    assign {&window-name}:sensitive = no.
    do on error undo, return no-apply:
        run app/esap007a.w(input-output table tt-tit_ap).
    end.
    assign {&window-name}:sensitive = yes.

    run pi-alt-fora-prazo.

    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-importar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-importar w-livre
ON CHOOSE OF bt-importar IN FRAME f-cad /* Importar */
DO:
    run pi-verifica-permissao.
    if return-value <> "OK" then return no-apply.

    do on error undo, return no-apply:
        run pi-importar.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-livre
ON CHOOSE OF bt-nenhum IN FRAME f-cad /* Nenhum */
DO:
    FOR EACH tt-tit_ap EXCLUSIVE-LOCK:
        ASSIGN tt-tit_ap.selecionado = NO.
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum-ex
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum-ex w-livre
ON CHOOSE OF bt-nenhum-ex IN FRAME f-cad /* Nenhum Exce‡Æo */
DO:
    FOR EACH tt-tit_ap EXCLUSIVE-LOCK:
        if not can-find(es-fornec-ap no-lock
                        where es-fornec-ap.cod-emitente = tt-tit_ap.cod-fornec) then next.
        assign tt-tit_ap.selecionado = no.
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-livre
ON CHOOSE OF bt-todos IN FRAME f-cad /* Todos */
DO:
    FOR EACH tt-tit_ap EXCLUSIVE-LOCK:
        ASSIGN tt-tit_ap.selecionado = YES.
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos-ex
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos-ex w-livre
ON CHOOSE OF bt-todos-ex IN FRAME f-cad /* Todos Exce‡Æo */
DO:
    FOR EACH tt-tit_ap EXCLUSIVE-LOCK:
        if not can-find(es-fornec-ap no-lock
                        where es-fornec-ap.cod-emitente = tt-tit_ap.cod-fornec) then next.
        assign tt-tit_ap.selecionado = yes.
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
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
             bt-filtro:HANDLE IN FRAME f-cad , 'AFTER':U ).
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
  DISPLAY fi-dt-vencimen 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-1 rt-button-2 bt-filtro br-tit-ap bt-todos bt-nenhum 
         bt-todos-ex bt-nenhum-ex fi-dt-vencimen bt-exportar bt-importar 
         bt-executar bf-fechar 
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

  {utp/ut9000.i "ESAP007" "2.06.00.001"}

  assign fi-dt-vencimen = today.
  run pi-vencimento(input        "201",
                    input        "201",
                    input-output fi-dt-vencimen).
  if fi-dt-vencimen = ? then return.
  assign fi-dt-vencimen:screen-value = string(fi-dt-vencimen).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-alt-fora-prazo w-livre 
PROCEDURE pi-alt-fora-prazo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var i-dias       as int  no-undo.
    def var i-dias-uteis as int  no-undo.
    def var i-tipo       as int  no-undo.
    def var c-ep-codigo  as char no-undo.

    find first es-param-alt-venc no-lock no-error.

    for each tt-tit_ap exclusive-lock:
        assign i-dias-uteis = 0.
        do i-dias = 0 to 30:
            assign c-ep-codigo = tt-tit_ap.ep-codigo.
            run pi-de-para-empresa(input-output c-ep-codigo).

            run pi-calendario(input c-ep-codigo,
                              input tt-tit_ap.cod-estabel,
                              input tt-tit_ap.dt-maquina + i-dias,
                              output i-tipo).
            if  i-tipo = 0 then
                return.

            if  i-tipo = 1 then
                assign i-dias-uteis = i-dias-uteis + 1.

            if  i-dias-uteis > es-param-alt-venc.fora-prazo then do:
                assign dt-fora-prazo = tt-tit_ap.dt-maquina + i-dias.
                leave.
            end.
        end.
        assign tt-tit_ap.fora-prazo = no.
        if  tt-tit_ap.dt-vencimen < dt-fora-prazo then
            assign tt-tit_ap.fora-prazo = yes.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calendario w-livre 
PROCEDURE pi-calendario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input  param p-ep-codigo   as char no-undo.
    def input  param p-cod-estabel as char no-undo.
    def input  param p-data        as date no-undo.
    def output param p-tipo        as int  no-undo.

    find calen-coml no-lock
         where calen-coml.ep-codigo   = p-ep-codigo
           and calen-coml.cod-estabel = p-cod-estabel
           and calen-coml.data        = p-data no-error.
    if  not avail calen-coml then do:
        RUN utp/ut-msgs.p (INPUT "show":U, INPUT 6543,
                           INPUT "estabelecimento "
                               + p-cod-estabel
                               + " empresa "
                               + p-ep-codigo
                               + " na data "
                               + string(p-data,"99/99/9999")).
        return.
    end.

    assign p-tipo = calen-coml.tipo-dia.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-livre 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var v_cod_refer     as char       no-undo.
    def var h-acomp         as handle     no-undo.
    def var ExcelAppl       as com-handle no-undo.
    def var ChWorkSheet     as com-handle no-undo.
    def var i-linha         as int        no-undo.
    def var v_log_refer_uni as log        no-undo.
    def var h_apb767zf      as handle     no-undo.

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp(input "Aguarde ...Alterando Titulos").

    empty temp-table tt_log_erros_tit_ap_total.

    run prgfin/apb/apb767zf.py persistent set h_apb767zf.

    for each tt-tit_ap no-lock use-index idx_3
       where tt-tit_ap.selecionado = yes,
       first tit_ap no-lock use-index titap_id
       where tit_ap.cod_estab       = tt-tit_ap.cod-estabel
         and tit_ap.cdn_fornecedor  = tt-tit_ap.cod-fornec
         and tit_ap.cod_espec_docto = tt-tit_ap.cod-esp
         and tit_ap.cod_ser_docto   = tt-tit_ap.serie
         and tit_ap.cod_tit_ap      = tt-tit_ap.nr-docto
         and tit_ap.cod_parcela     = tt-tit_ap.parcela:
        assign i-linha = i-linha + 1.

        empty temp-table tt_tit_ap_alteracao_base_aux_3.
        empty temp-table tt_tit_ap_alteracao_rateio.
        empty temp-table tt_log_erros_tit_ap_alteracao.

        /*---------- Cria Referˆncia ----------*/
        assign v_log_refer_uni = no
               v_cod_refer     = "".
        repeat while not v_log_refer_uni:
            run pi_retorna_sugestao_referencia(input "T" /*l_T*/ ,
                                               input today,
                                               output v_cod_refer).

            run pi_verifica_refer_unica_apb(input  tit_ap.cod_estab,
                                            input  v_cod_refer,
                                            output v_log_refer_uni).
        end.

        run pi-acompanhar in h-acomp(input "Linha : " + string(i-linha)).

        create tt_tit_ap_alteracao_base_aux_3.
        assign tt_tit_ap_alteracao_base_aux_3.ttv_cod_usuar_corren            = v_cod_usuar_corren
               tt_tit_ap_alteracao_base_aux_3.tta_cod_empresa                 = tit_ap.cod_empresa                
               tt_tit_ap_alteracao_base_aux_3.tta_cod_estab                   = tit_ap.cod_estab                  
               tt_tit_ap_alteracao_base_aux_3.tta_num_id_tit_ap               = tit_ap.num_id_tit_ap              
               tt_tit_ap_alteracao_base_aux_3.ttv_rec_tit_ap                  = recid(tit_ap)
               tt_tit_ap_alteracao_base_aux_3.tta_cdn_fornecedor              = tit_ap.cdn_fornecedor             
               tt_tit_ap_alteracao_base_aux_3.tta_cod_espec_docto             = tit_ap.cod_espec_docto            
               tt_tit_ap_alteracao_base_aux_3.tta_cod_ser_docto               = tit_ap.cod_ser_docto              
               tt_tit_ap_alteracao_base_aux_3.tta_cod_tit_ap                  = tit_ap.cod_tit_ap                 
               tt_tit_ap_alteracao_base_aux_3.tta_cod_parcela                 = tit_ap.cod_parcela                
               tt_tit_ap_alteracao_base_aux_3.ttv_dat_transacao               = tit_ap.dat_transacao              
               tt_tit_ap_alteracao_base_aux_3.ttv_cod_refer                   = v_cod_refer
               tt_tit_ap_alteracao_base_aux_3.tta_val_sdo_tit_ap              = tit_ap.val_sdo_tit_ap             
               tt_tit_ap_alteracao_base_aux_3.tta_dat_emis_docto              = tit_ap.dat_emis_docto             
               tt_tit_ap_alteracao_base_aux_3.tta_dat_vencto_tit_ap           = input frame f-cad fi-dt-vencimen
               tt_tit_ap_alteracao_base_aux_3.tta_dat_prev_pagto              = input frame f-cad fi-dt-vencimen
               tt_tit_ap_alteracao_base_aux_3.tta_dat_ult_pagto               = tit_ap.dat_ult_pagto              
               tt_tit_ap_alteracao_base_aux_3.tta_num_dias_atraso             = tit_ap.num_dias_atraso            
               tt_tit_ap_alteracao_base_aux_3.tta_val_perc_multa_atraso       = tit_ap.val_perc_multa_atraso      
               tt_tit_ap_alteracao_base_aux_3.tta_val_juros_dia_atraso        = tit_ap.val_juros_dia_atraso       
               tt_tit_ap_alteracao_base_aux_3.tta_val_perc_juros_dia_atraso   = tit_ap.val_perc_juros_dia_atraso  
               tt_tit_ap_alteracao_base_aux_3.tta_dat_desconto                = tit_ap.dat_desconto               
               tt_tit_ap_alteracao_base_aux_3.tta_val_perc_desc               = tit_ap.val_perc_desc              
               tt_tit_ap_alteracao_base_aux_3.tta_val_desconto                = tit_ap.val_desconto               
               tt_tit_ap_alteracao_base_aux_3.tta_cod_portador                = tit_ap.cod_portador               
               tt_tit_ap_alteracao_base_aux_3.ttv_cod_portador_mov            = "" /* tit_ap.cod_portador */
               tt_tit_ap_alteracao_base_aux_3.tta_log_pagto_bloqdo            = tit_ap.log_pagto_bloqdo           
               tt_tit_ap_alteracao_base_aux_3.tta_cod_seguradora              = tit_ap.cod_seguradora             
               tt_tit_ap_alteracao_base_aux_3.tta_cod_apol_seguro             = tit_ap.cod_apol_seguro            
               tt_tit_ap_alteracao_base_aux_3.tta_cod_arrendador              = tit_ap.cod_arrendador             
               tt_tit_ap_alteracao_base_aux_3.tta_cod_contrat_leas            = tit_ap.cod_contrat_leas           
               tt_tit_ap_alteracao_base_aux_3.tta_ind_tip_espec_docto         = tit_ap.ind_tip_espec_docto        
               tt_tit_ap_alteracao_base_aux_3.tta_cod_indic_econ              = tit_ap.cod_indic_econ             
               tt_tit_ap_alteracao_base_aux_3.tta_num_seq_refer               = tit_ap.num_seq_refer
               tt_tit_ap_alteracao_base_aux_3.ttv_ind_motiv_alter_val_tit_ap  = "Altera‡Æo"
               tt_tit_ap_alteracao_base_aux_3.ttv_wgh_lista                   = ?
               tt_tit_ap_alteracao_base_aux_3.ttv_log_gera_ocor_alter_valores = no
               tt_tit_ap_alteracao_base_aux_3.tta_cb4_tit_ap_bco_cobdor       = tit_ap.cb4_tit_ap_bco_cobdor      
               tt_tit_ap_alteracao_base_aux_3.tta_cod_histor_padr             = ""
               tt_tit_ap_alteracao_base_aux_3.tta_des_histor_padr             = ""
               tt_tit_ap_alteracao_base_aux_3.tta_ind_sit_tit_ap              = tit_ap.ind_sit_tit_ap             
               tt_tit_ap_alteracao_base_aux_3.tta_cod_forma_pagto             = tit_ap.cod_forma_pagto            
               tt_tit_ap_alteracao_base_aux_3.tta_cod_estab_ext               = "".

        run pi-acompanhar in h-acomp(input "Linha : " + string(i-linha) + " alterando lote").

        run pi_main_code_api_integr_ap_alter_tit_ap_4 in h_apb767zf (input 1,
                                                                     input "APB",
                                                                     input "",
                                                                     input-output table tt_tit_ap_alteracao_base_aux_3,
                                                                     input-output table tt_tit_ap_alteracao_rateio,
                                                                     output table tt_log_erros_tit_ap_alteracao).

        for each tt_log_erros_tit_ap_alteracao:
            create tt_log_erros_tit_ap_total.
            buffer-copy tt_log_erros_tit_ap_alteracao to tt_log_erros_tit_ap_total.
        end.
    end.

    delete procedure h_apb767zf.

    run pi-inicializar in h-acomp(input "Gerando Excel ...").

    create "excel.application" ExcelAppl.
    ExcelAppl:WorkBooks:add().

    assign i-linha                                      = 1
           ExcelAppl:Range("A" + string(i-linha)):value = "Emp"
           ExcelAppl:Range("B" + string(i-linha)):value = "Estab"
           ExcelAppl:Range("C" + string(i-linha)):value = "Fornec"
           ExcelAppl:Range("D" + string(i-linha)):value = "Nome Abrev"
           ExcelAppl:Range("E" + string(i-linha)):value = "Esp"
           ExcelAppl:Range("F" + string(i-linha)):value = "Serie"
           ExcelAppl:Range("G" + string(i-linha)):value = "Docto"
           ExcelAppl:Range("H" + string(i-linha)):value = "Parc"
           ExcelAppl:Range("I" + string(i-linha)):value = "Dt Maquina"
           ExcelAppl:Range("J" + string(i-linha)):value = "Dt Venc Orig"
           ExcelAppl:Range("K" + string(i-linha)):value = "Dt Venc Novo"
           ExcelAppl:Range("L" + string(i-linha)):value = "Vl Original"
           ExcelAppl:Range("M" + string(i-linha)):value = "Vl Saldo".

    for each tt-tit_ap no-lock use-index idx_3
       where tt-tit_ap.selecionado = yes,
       first tit_ap no-lock use-index titap_id
       where tit_ap.cod_estab       = tt-tit_ap.cod-estabel
         and tit_ap.cdn_fornecedor  = tt-tit_ap.cod-fornec
         and tit_ap.cod_espec_docto = tt-tit_ap.cod-esp
         and tit_ap.cod_ser_docto   = tt-tit_ap.serie
         and tit_ap.cod_tit_ap      = tt-tit_ap.nr-docto
         and tit_ap.cod_parcela     = tt-tit_ap.parcela:
        find first tt_log_erros_tit_ap_total no-lock
             where tt_log_erros_tit_ap_total.tta_cdn_fornecedor  = tt-tit_ap.cod-fornec
               and tt_log_erros_tit_ap_total.tta_cod_estab       = tt-tit_ap.cod-estabel
               and tt_log_erros_tit_ap_total.tta_cod_espec_docto = tt-tit_ap.cod-esp
               and tt_log_erros_tit_ap_total.tta_cod_ser_docto   = tt-tit_ap.serie
               and tt_log_erros_tit_ap_total.tta_cod_tit_ap      = tt-tit_ap.nr-docto
               and tt_log_erros_tit_ap_total.tta_cod_parcela     = tt-tit_ap.parcela no-error.
        if  avail tt_log_erros_tit_ap_total then next.

        run pi-acompanhar in h-acomp(input "Alterados - Linha : " + string(i-linha)).

        assign i-linha                                             = i-linha + 1
               ExcelAppl:Range("A" + string(i-linha)):value        = tt-tit_ap.ep-codigo
               ExcelAppl:Range("B" + string(i-linha)):numberformat = "@"
               ExcelAppl:Range("B" + string(i-linha)):value        = tt-tit_ap.cod-estabel
               ExcelAppl:Range("C" + string(i-linha)):value        = tt-tit_ap.cod-fornec
               ExcelAppl:Range("D" + string(i-linha)):value        = tt-tit_ap.nome-abrev
               ExcelAppl:Range("E" + string(i-linha)):numberformat = "@"
               ExcelAppl:Range("E" + string(i-linha)):value        = tt-tit_ap.cod-esp
               ExcelAppl:Range("F" + string(i-linha)):numberformat = "@"
               ExcelAppl:Range("F" + string(i-linha)):value        = tt-tit_ap.serie
               ExcelAppl:Range("G" + string(i-linha)):numberformat = "@"
               ExcelAppl:Range("G" + string(i-linha)):value        = tt-tit_ap.nr-docto
               ExcelAppl:Range("H" + string(i-linha)):value        = tt-tit_ap.parcela
               ExcelAppl:Range("I" + string(i-linha)):value        = tt-tit_ap.dt-maquina
               ExcelAppl:Range("J" + string(i-linha)):value        = tit_ap.dat_vencto_tit_ap
               ExcelAppl:Range("K" + string(i-linha)):value        = tit_ap.dat_vencto_tit
               ExcelAppl:Range("L" + string(i-linha)):numberformat = "#.##0,00"
               ExcelAppl:Range("L" + string(i-linha)):value        = tt-tit_ap.vl-original
               ExcelAppl:Range("M" + string(i-linha)):numberformat = "#.##0,00"
               ExcelAppl:Range("M" + string(i-linha)):value        = tt-tit_ap.valor-saldo.

        if  tt-tit_ap.fora-prazo = yes then
            assign ExcelAppl:Range("A" + string(i-linha) + ":M" + string(i-linha) ):font:ColorIndex = 3.

        if  tt-tit_ap.excecao then
            assign ExcelAppl:Range("A" + string(i-linha) + ":M" + string(i-linha) ):font:ColorIndex = 5.

        assign tt-tit_ap.dt-vencimen = tit_ap.dat_vencto_tit
               tt-tit_ap.vl-original = tit_ap.val_origin_tit_ap
               tt-tit_ap.valor-saldo = tit_ap.val_sdo_tit_ap.
    end.

    ExcelAppl:WorkSheets:item(1):select.
    ExcelAppl:WorkSheets:item(1):name = "Alterados".

    if  can-find(first tt_log_erros_tit_ap_total) then do:
        ExcelAppl:sheets:add.
        ExcelAppl:WorkSheets:item(1):select.
        ExcelAppl:WorkSheets:item(1):name = "NÆo Alterados".

        assign i-linha                                      = 1
               ExcelAppl:Range("A" + string(i-linha)):value = "Emp"
               ExcelAppl:Range("B" + string(i-linha)):value = "Estab"
               ExcelAppl:Range("C" + string(i-linha)):value = "Fornec"
               ExcelAppl:Range("D" + string(i-linha)):value = "Nome Abrev"
               ExcelAppl:Range("E" + string(i-linha)):value = "Esp"
               ExcelAppl:Range("F" + string(i-linha)):value = "Serie"
               ExcelAppl:Range("G" + string(i-linha)):value = "Docto"
               ExcelAppl:Range("H" + string(i-linha)):value = "Parc"
               ExcelAppl:Range("I" + string(i-linha)):value = "Dt Vencimento"
               ExcelAppl:Range("J" + string(i-linha)):value = "Dt Maquina"
               ExcelAppl:Range("K" + string(i-linha)):value = "Vl Original"
               ExcelAppl:Range("L" + string(i-linha)):value = "Vl Saldo"
               ExcelAppl:Range("M" + string(i-linha)):value = "Ocorrencia".

        for each tt-tit_ap no-lock use-index idx_3
           where tt-tit_ap.selecionado = yes,
           first tit_ap no-lock use-index titap_id
           where tit_ap.cod_estab       = tt-tit_ap.cod-estabel
             and tit_ap.cdn_fornecedor  = tt-tit_ap.cod-fornec
             and tit_ap.cod_espec_docto = tt-tit_ap.cod-esp
             and tit_ap.cod_ser_docto   = tt-tit_ap.serie
             and tit_ap.cod_tit_ap      = tt-tit_ap.nr-docto
             and tit_ap.cod_parcela     = tt-tit_ap.parcela:
            find first tt_log_erros_tit_ap_total no-lock
                where tt_log_erros_tit_ap_total.tta_cdn_fornecedor  = tt-tit_ap.cod-fornec
                and   tt_log_erros_tit_ap_total.tta_cod_estab       = tt-tit_ap.cod-estabel
                and   tt_log_erros_tit_ap_total.tta_cod_espec_docto = tt-tit_ap.cod-esp
                and   tt_log_erros_tit_ap_total.tta_cod_ser_docto   = tt-tit_ap.serie
                and   tt_log_erros_tit_ap_total.tta_cod_tit_ap      = tt-tit_ap.nr-docto
                and   tt_log_erros_tit_ap_total.tta_cod_parcela     = tt-tit_ap.parcela no-error.
            if  not avail tt_log_erros_tit_ap_total then next.

            run pi-acompanhar in h-acomp(input "Não Alterados Linha : " + string(i-linha)).

            assign i-linha                                             = i-linha + 1
                   ExcelAppl:Range("A" + string(i-linha)):value        = tt-tit_ap.ep-codigo
                   ExcelAppl:Range("B" + string(i-linha)):numberformat = "@"
                   ExcelAppl:Range("B" + string(i-linha)):value        = tt-tit_ap.cod-estabel
                   ExcelAppl:Range("C" + string(i-linha)):value        = tt-tit_ap.cod-fornec
                   ExcelAppl:Range("D" + string(i-linha)):value        = tt-tit_ap.nome-abrev
                   ExcelAppl:Range("E" + string(i-linha)):numberformat = "@"
                   ExcelAppl:Range("E" + string(i-linha)):value        = tt-tit_ap.cod-esp
                   ExcelAppl:Range("F" + string(i-linha)):numberformat = "@"
                   ExcelAppl:Range("F" + string(i-linha)):value        = tt-tit_ap.serie
                   ExcelAppl:Range("G" + string(i-linha)):numberformat = "@"
                   ExcelAppl:Range("G" + string(i-linha)):value        = tt-tit_ap.nr-docto
                   ExcelAppl:Range("H" + string(i-linha)):value        = tt-tit_ap.parcela
                   ExcelAppl:Range("I" + string(i-linha)):value        = tit_ap.dat_vencto_tit
                   ExcelAppl:Range("J" + string(i-linha)):value        = tt-tit_ap.dt-maquina
                   ExcelAppl:Range("K" + string(i-linha)):numberformat = "#.##0,00"
                   ExcelAppl:Range("K" + string(i-linha)):value        = tt-tit_ap.vl-original
                   ExcelAppl:Range("L" + string(i-linha)):numberformat = "#.##0,00"
                   ExcelAppl:Range("L" + string(i-linha)):value        = tt-tit_ap.valor-saldo
                   ExcelAppl:Range("M" + string(i-linha)):value        = string(tt_log_erros_tit_ap_total.ttv_num_mensagem)
                                                                       + " - "
                                                                       + tt_log_erros_tit_ap_total.ttv_des_msg_erro.

            if  tt-tit_ap.fora-prazo = yes then
                assign ExcelAppl:Range("A" + string(i-linha) + ":M" + string(i-linha) ):font:ColorIndex = 3.

            if  tt-tit_ap.excecao then
                assign ExcelAppl:Range("A" + string(i-linha) + ":M" + string(i-linha) ):font:ColorIndex = 5.

            assign tt-tit_ap.dt-vencimen = tit_ap.dat_vencto_tit
                   tt-tit_ap.vl-original = tit_ap.val_origin_tit_ap
                   tt-tit_ap.valor-saldo = tit_ap.val_sdo_tit_ap.
        end.
    end.

    ExcelAppl:Cells:select. /* precisa selecionar todas primeiro */
    ExcelAppl:Cells:EntireColumn:AutoFit.

    ExcelAppl:visible = true.

    release object ExcelAppl    no-error.
    release object chWorksheet  no-error.

    run pi-alt-fora-prazo.

    run pi-finalizar in h-acomp.

    {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-de-para-empresa w-livre 
PROCEDURE pi-de-para-empresa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Tratar tradu‡Æo de empresa se houver */
    def input-output param p-empresa as char no-undo.

    find first trad_org_ext no-lock
         where trad_org_ext.cod_matriz_trad_org_ext = "EMS2"
           and trad_org_ext.cod_tip_unid_organ      = "998" /* Empresa */
           and trad_org_ext.cod_unid_organ          = p-empresa no-error.
    if  not avail trad_org_ext then do:
        run utp/ut-msgs.p(input "show":U, input 17006,
                          input "Matriz de Tradu‡Æo~~Matriz de Tradu‡Æo para empresa " + p-empresa + " Inexistente").
        return.
    end.

    assign p-empresa = if  avail trad_org_ext then trad_org_ext.cod_unid_organ_ext else p-empresa.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-exportar w-livre 
PROCEDURE pi-exportar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var h-acomp     as handle       no-undo.
    def var ExcelAppl   as com-handle   no-undo.
    def var ChWorkSheet as com-handle   no-undo.
    def var i-linha     as integer      no-undo.

    run utp/ut-acomp.p persistent set h-acomp.

    {utp/ut-liter.i Exportando}

    run pi-inicializar in h-acomp(input "Aguarde ...").

    create "excel.application" ExcelAppl.
    ExcelAppl:WorkBooks:add().

    assign i-linha                                      = 1
           ExcelAppl:Range("A" + string(i-linha)):value = "Sel"
           ExcelAppl:Range("B" + string(i-linha)):value = "Emp"
           ExcelAppl:Range("C" + string(i-linha)):value = "Estab"
           ExcelAppl:Range("D" + string(i-linha)):value = "Fornec"
           ExcelAppl:Range("E" + string(i-linha)):value = "Nome Abrev"
           ExcelAppl:Range("F" + string(i-linha)):value = "Esp"
           ExcelAppl:Range("G" + string(i-linha)):value = "Serie"
           ExcelAppl:Range("H" + string(i-linha)):value = "Docto"
           ExcelAppl:Range("I" + string(i-linha)):value = "Parc"
           ExcelAppl:Range("J" + string(i-linha)):value = "Dt Maquina"
           ExcelAppl:Range("K" + string(i-linha)):value = "Dt Vencimento"
           ExcelAppl:Range("L" + string(i-linha)):value = "Vl Original"
           ExcelAppl:Range("M" + string(i-linha)):value = "Vl Saldo".

    for each tt-tit_ap no-lock:
        run pi-acompanhar in h-acomp(input "Linha : " + string(i-linha)).

        assign i-linha                                             = i-linha + 1
               ExcelAppl:Range("A" + string(i-linha)):value        = if tt-tit_ap.selecionado then "Sim" else "NÆo"
               ExcelAppl:Range("B" + string(i-linha)):value        = tt-tit_ap.ep-codigo
               ExcelAppl:Range("C" + string(i-linha)):numberformat = "@"
               ExcelAppl:Range("C" + string(i-linha)):value        = tt-tit_ap.cod-estabel
               ExcelAppl:Range("D" + string(i-linha)):value        = tt-tit_ap.cod-fornec
               ExcelAppl:Range("E" + string(i-linha)):value        = tt-tit_ap.nome-abrev
               ExcelAppl:Range("F" + string(i-linha)):numberformat = "@"
               ExcelAppl:Range("F" + string(i-linha)):value        = tt-tit_ap.cod-esp
               ExcelAppl:Range("G" + string(i-linha)):numberformat = "@"
               ExcelAppl:Range("G" + string(i-linha)):value        = tt-tit_ap.serie
               ExcelAppl:Range("H" + string(i-linha)):numberformat = "@"
               ExcelAppl:Range("H" + string(i-linha)):value        = tt-tit_ap.nr-docto
               ExcelAppl:Range("I" + string(i-linha)):numberformat = "@"
               ExcelAppl:Range("I" + string(i-linha)):value        = tt-tit_ap.parcela
               ExcelAppl:Range("J" + string(i-linha)):value        = tt-tit_ap.dt-maquina
               ExcelAppl:Range("K" + string(i-linha)):value        = tt-tit_ap.dt-vencimen
               ExcelAppl:Range("L" + string(i-linha)):numberformat = "#.##0,00"
               ExcelAppl:Range("L" + string(i-linha)):value        = tt-tit_ap.vl-original
               ExcelAppl:Range("M" + string(i-linha)):numberformat = "#.##0,00"
               ExcelAppl:Range("M" + string(i-linha)):value        = tt-tit_ap.valor-saldo.

        if  tt-tit_ap.fora-prazo = yes then
            assign ExcelAppl:Range("A" + string(i-linha) + ":M" + string(i-linha) ):font:ColorIndex = 3.

        if  tt-tit_ap.excecao then
            assign ExcelAppl:Range("A" + string(i-linha) + ":M" + string(i-linha) ):font:ColorIndex = 5.
    end.

    ExcelAppl:Cells:SELECT. /* precisa selecionar todas primeiro */
    ExcelAppl:Cells:EntireColumn:AutoFit.

    ExcelAppl:visible = true.

    release object ExcelAppl    no-error.
    release object chWorksheet  no-error.

    run pi-finalizar in h-acomp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-importar w-livre 
PROCEDURE pi-importar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var c-arquivo   as char       no-undo.
    def var l-conf      as logical    no-undo.
    def var c-linha     as char       no-undo.
    def var i-linha     as integer    no-undo.

    system-dialog get-file c-arquivo
        filters "Arquivo CSV *.csv" "*.csv"
        must-exist
        update l-conf.

    if  not l-conf then do:
        message 'Nenhum arquivo selecionado' view-as alert-box info buttons ok.
        return no-apply.
    end.

    find first es-param-alt-venc no-lock no-error.

    empty temp-table tt-tit_ap.
    input from value(c-arquivo) no-echo no-convert.

    repeat:
        import unformatted c-linha.

        assign i-linha = i-linha + 1.

        if  entry(1,c-linha,";") = "Sel" then next.

        if  entry(1,c-linha,";") <> "Sim" and
            entry(1,c-linha,";") <> "NÆo" and
            entry(1,c-linha,";") <> "Nao" then
            next.

        if  num-entries(c-linha,";") <> 13 then next.

        FIND FIRST tit_ap no-lock use-index titap_id
             where /* tit_ap.cod_empresa     = entry(2,c-linha,";")
             and */   tit_ap.cod_estab       = entry(3,c-linha,";")
             and   tit_ap.cdn_fornecedor  = int(entry(4,c-linha,";"))
             and   tit_ap.cod_espec_docto = entry(6,c-linha,";")
             and   tit_ap.cod_ser_docto   = entry(7,c-linha,";")
             and   tit_ap.cod_tit_ap      = entry(8,c-linha,";")
             and   tit_ap.cod_parcela     = entry(9,c-linha,";") no-error.
        if  not avail tit_ap then next.

        create tt-tit_ap.
        assign tt-tit_ap.selecionado = if entry(1,c-linha,";") = "Sim" then yes else no
               tt-tit_ap.ep-codigo   = entry(2,c-linha,";")
               tt-tit_ap.cod-estabel = entry(3,c-linha,";")
               tt-tit_ap.cod-fornec  = int(entry(4,c-linha,";"))
               tt-tit_ap.nome-abrev  = entry(5,c-linha,";")
               tt-tit_ap.cod-esp     = entry(6,c-linha,";")
               tt-tit_ap.serie       = entry(7,c-linha,";")
               tt-tit_ap.nr-docto    = entry(8,c-linha,";")
               tt-tit_ap.parcela     = entry(9,c-linha,";")
               tt-tit_ap.dt-maquina  = date(entry(10,c-linha,";"))
               tt-tit_ap.dt-vencimen = date(entry(11,c-linha,";"))
               tt-tit_ap.vl-original = dec(entry(12,c-linha,";"))
               tt-tit_ap.valor-saldo = dec(entry(13,c-linha,";")).

        find es-fornec-ap no-lock
            where es-fornec-ap.cod-emitente = tt-tit_ap.cod-fornec no-error.
        assign tt-tit_ap.excecao = avail es-fornec-ap.
    end.

    run pi-alt-fora-prazo.

    {&OPEN-QUERY-{&BROWSE-NAME}}

    MESSAGE "Arquivo Carregado !" VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-vencimento w-livre 
PROCEDURE pi-vencimento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input        param p-ep-codigo   as int  no-undo.
    def input        param p-cod-estabel as char no-undo.
    def input-output param p-data        as date no-undo.

    def var i-tipo   as integer no-undo.
    def var i-dias   as integer no-undo.
    def var i-dias-2 as integer no-undo.
    def var d-data   as date    no-undo.

    /* --- Main Block --- */
    if weekday(p-data) = 5 then do:
        run pi-calendario(input p-ep-codigo,
                          input p-cod-estabel,
                          input p-data,
                          output i-tipo).
        if i-tipo = 0 then do:
            assign p-data = ?.
            return.
        end.
        if i-tipo = 1 then return.
    end.

    if weekday(p-data) = 6 then do:
        if p-data <> today then do:
            do i-dias = 0 to (p-data - today):
                assign p-data = p-data - 1.
                run pi-calendario(input p-ep-codigo,
                                  input p-cod-estabel,
                                  input p-data,
                                  output i-tipo).
                if i-tipo = 0 then do:
                    assign p-data = ?.
                    return.
                end.
                if i-tipo = 1 then return.
            end.
        end.
    end.

    do i-dias = 0 to 100:
        assign p-data = p-data + 1.
        if weekday(p-data) = 5 then do:
            run pi-calendario(input p-ep-codigo,
                              input p-cod-estabel,
                              input p-data,
                              output i-tipo).
            if i-tipo = 0 then do:
                assign p-data = ?.
                return.
            end.
            if i-tipo = 1 then return.

            assign d-data = p-data.
            do i-dias-2 = 0 to (d-data - today):
                assign d-data = d-data - 1.
                run pi-calendario(input p-ep-codigo,
                                  input p-cod-estabel,
                                  input d-data,
                                  output i-tipo).
                if i-tipo = 0 then do:
                    assign d-data = ?.
                    return.
                end.
                if i-tipo = 1 then do:
                    assign p-data = d-data.
                    return.
                end.
            end.
        end.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-verifica-permissao w-livre 
PROCEDURE pi-verifica-permissao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var l-avail  as log no-undo.
    def var l-altera as log no-undo.

    assign l-avail = no.
    for each estabelecimento no-lock
       where estabelecimento.cod_empresa = v_cod_empres_usuar,
       first usuar_financ_estab_apb no-lock
       where usuar_financ_estab_apb.cod_usuario = c-seg-usuario
         and usuar_financ_estab_apb.cod_estab   = estabelecimento.cod_estab:
        assign l-avail = yes.
        
        if  not l-altera and
            usuar_financ_estab_apb.log_habilit_alter_vencto then
            assign l-altera = yes.
    end.

    if  not l-avail then do:
        run utp/ut-msgs.p(input "show", input 15461, input caps(c-seg-usuario)).
        return.
    end.

    if  not l-altera then do:
        {utp/ut-liter.i alterar_data_vencimento}
        run utp/ut-msgs.p(input "show", input 25729,
                          input caps(c-seg-usuario) + "~~" + trim(return-value)).
        return.
    end.

    return "OK".
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
  {src/adm/template/snd-list.i "tt-tit_ap"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_retorna_sugestao_referencia w-livre  _ADM-SEND-RECORDS
PROCEDURE pi_retorna_sugestao_referencia:

    /************************ Parameter Definition Begin ************************/

    def Input param p_ind_tip_atualiz
        as character
        format "X(08)"
        no-undo.
    def Input param p_dat_refer
        as date
        format "99/99/9999"
        no-undo.
    def output param p_cod_refer
        as character
        format "x(10)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_des_dat                        as character       no-undo. /*local*/
    def var v_num_aux                        as integer         no-undo. /*local*/
    def var v_num_aux_2                      as integer         no-undo. /*local*/
    def var v_num_cont                       as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign v_des_dat   = string(p_dat_refer,"99999999")
           p_cod_refer = substring(v_des_dat,7,2)
                       + substring(v_des_dat,3,2)
                       + substring(v_des_dat,1,2)
                       + substring(p_ind_tip_atualiz,1,1)
           v_num_aux_2 = integer(this-procedure:handle).

    do  v_num_cont = 1 to 3:
        assign v_num_aux   = (random(0,v_num_aux_2) mod 26) + 97
               p_cod_refer = p_cod_refer + chr(v_num_aux).
    end.
END PROCEDURE. /* pi_retorna_sugestao_referencia */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_verifica_refer_unica_apb w-livre  _ADM-SEND-RECORDS
PROCEDURE pi_verifica_refer_unica_apb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_estab
    &IF "{&EMSFIN_version}" >= "" AND "{&EMSFIN_version}" < "5.07A" &THEN
        as character
        format "x(3)"
    &ENDIF
    &IF "{&EMSFIN_version}" >= "5.07A" AND "{&EMSFIN_version}" < "9.99" &THEN
        as Character
        format "x(5)"
    &ENDIF
        no-undo.
    def Input param p_cod_refer
        as character
        format "x(10)"
        no-undo.
    def output param p_log_refer_uni
        as logical
        format "Sim/NÆo"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************** Buffer Definition Begin *************************/

    &if "{&emsfin_version}" >= "5.01" &then
    def buffer b_antecip_pef_pend
        for antecip_pef_pend.
    &endif
    &if "{&emsfin_version}" >= "5.01" &then
    def buffer b_lote_impl_tit_ap
        for lote_impl_tit_ap.
    &endif
    &if "{&emsfin_version}" >= "5.01" &then
    def buffer b_lote_pagto
        for lote_pagto.
    &endif
    &if "{&emsfin_version}" >= "5.01" &then
    def buffer b_movto_tit_ap
        for movto_tit_ap.
    &endif


    /*************************** Buffer Definition End **************************/

    assign p_log_refer_uni = yes.

        find first b_antecip_pef_pend no-lock
             where b_antecip_pef_pend.cod_estab = p_cod_estab
               and b_antecip_pef_pend.cod_refer = p_cod_refer /*cl_verifica_refer_uni of b_antecip_pef_pend*/ no-error.
    if  avail b_antecip_pef_pend
    then do:
        assign p_log_refer_uni = no.
    end /* if */.
    else do:
            find first b_lote_impl_tit_ap no-lock
                 where b_lote_impl_tit_ap.cod_estab = p_cod_estab
                   and b_lote_impl_tit_ap.cod_refer = p_cod_refer /*cl_verifica_refer_uni of b_lote_impl_tit_ap*/ no-error.
        if  avail b_lote_impl_tit_ap
        then do:
            assign p_log_refer_uni = no.
        end /* if */.
        else do:
                find first b_lote_pagto no-lock
                     where b_lote_pagto.cod_estab_refer = p_cod_estab
                       and b_lote_pagto.cod_refer = p_cod_refer /*cl_verifica_refer_uni of b_lote_pagto*/ no-error.
            if  avail b_lote_pagto
            then do:
                assign p_log_refer_uni = no.
            end /* if */.
            else do:
                find first b_movto_tit_ap no-lock
                     where b_movto_tit_ap.cod_estab = p_cod_estab
                       and b_movto_tit_ap.cod_refer = p_cod_refer no-error.
                if  avail b_movto_tit_ap
                then do:
                    assign p_log_refer_uni = no.
                end /* if */.
            end /* else */.
        end /* else */.
    end /* else */.

    &if defined(BF_FIN_BCOS_HISTORICOS) &then
        if  can-find(first his_movto_tit_ap_histor no-lock
                     where his_movto_tit_ap_histor.cod_estab = p_cod_estab
                       and his_movto_tit_ap_histor.cod_refer = p_cod_refer) then
            assign p_log_refer_uni = no.
    &endif
END PROCEDURE. /* pi_verifica_refer_unica_apb */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

