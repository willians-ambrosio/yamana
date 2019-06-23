&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESAP001 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
def new global shared var i-ep-codigo-usuario like mguni.empresa.ep-codigo no-undo.
def new global shared var c-seg-usuario       as char format 'x(12)' no-undo.

/* Local Variable Definitions ---                                       */
def var h-handle-call as handle no-undo.

def temp-table tt-digita no-undo
    field id            as int
    field sel           as char format "x(3)" label "Selec"
    field cod-estabel   like tit_ap.cod_estab
    field cod-fornec    like tit_ap.cdn_fornecedor
    field nome-abrev    like ems5.fornecedor.nom_abrev
    field cod-esp       like tit_ap.cod_espec_docto
    field serie         like tit_ap.cod_ser_docto
    field nr-docto      like tit_ap.cod_tit_ap
    field parcela       like tit_ap.cod_parcela
    field portador      like tit_ap.cod_portador
    field dt-vencimen   like tit_ap.dat_vencto_tit_ap
/*     field dt-prev-pag   like tit-ap.dt-prev-pag */
    field dt-emissao    like tit_ap.dat_emis_docto
    field valor-saldo   like tit_ap.val_sdo_tit_ap
    field vl-original   like tit_ap.val_origin_tit_ap
    field dt-modifica   like tit_ap.dat_vencto_tit_ap
    .

def temp-table tt-digita-aux no-undo
    field id            as int
    field sel           as char format "x(3)" label "Selec"
    field cod-estabel   like tit_ap.cod_estab        
    field cod-fornec    like tit_ap.cdn_fornecedor   
    field nome-abrev    like ems5.fornecedor.nom_abrev    
    field cod-esp       like tit_ap.cod_espec_docto  
    field serie         like tit_ap.cod_ser_docto    
    field nr-docto      like tit_ap.cod_tit_ap       
    field parcela       like tit_ap.cod_parcela      
    field portador      like tit_ap.cod_portador     
    field dt-vencimen   like tit_ap.dat_vencto_tit_ap
/*     field dt-prev-pag   like tit-ap.dt-prev-pag */
    field dt-emissao    like tit_ap.dat_emis_docto   
    field valor-saldo   like tit_ap.val_sdo_tit_ap   
    field vl-original   like tit_ap.val_origin_tit_ap
    field dt-modifica   like tit_ap.dat_vencto_tit_ap
    .

/* Parameters Definitions ---                                           */
/* def input param table for tt-param. */
/* def input-output param table for tt-digita. */

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

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita-aux

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita-aux.sel tt-digita-aux.cod-estabel tt-digita-aux.cod-fornec tt-digita-aux.nome-abrev tt-digita-aux.cod-esp tt-digita-aux.serie tt-digita-aux.nr-docto tt-digita-aux.parcela tt-digita-aux.dt-vencimen tt-digita-aux.valor-saldo tt-digita-aux.vl-original   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita   
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-digita-aux
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY {&SELF-NAME} FOR EACH tt-digita-aux.
&Scoped-define TABLES-IN-QUERY-br-digita tt-digita-aux
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-digita-aux


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-digita}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-5 RECT-33 RECT-34 br-digita ~
d-dt-vencimento c-campo bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS d-dt-vencimento c-campo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-nenhum 
     IMAGE-UP FILE "image/im-nenhum.bmp":U NO-FOCUS
     LABEL "Nenhum" 
     SIZE 4 BY 1.25 TOOLTIP "Desmarca os t¡tulos selecionados para pagamento".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "Modifica" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-sav 
     IMAGE-UP FILE "image/im-sav":U
     IMAGE-INSENSITIVE FILE "image/ii-sav":U
     LABEL "Marcar" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON bt-todos 
     IMAGE-UP FILE "image/im-todos.bmp":U NO-FOCUS
     LABEL "Todos" 
     SIZE 4 BY 1.25 TOOLTIP "Seleciona todos os t¡tulos para pagamento".

DEFINE VARIABLE c-campo AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 62 BY 2.25 NO-UNDO.

DEFINE VARIABLE d-dt-vencimento AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt. Vencimento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 9.75.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 3.71.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 6 BY 9.25.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-digita-aux SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita w-window _FREEFORM
  QUERY br-digita DISPLAY
      tt-digita-aux.sel
tt-digita-aux.cod-estabel
tt-digita-aux.cod-fornec 
tt-digita-aux.nome-abrev 
tt-digita-aux.cod-esp    
tt-digita-aux.serie      
tt-digita-aux.nr-docto  
tt-digita-aux.parcela    
tt-digita-aux.dt-vencimen
tt-digita-aux.valor-saldo
tt-digita-aux.vl-original
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 70 BY 9.29 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     bt-nenhum AT ROW 3 COL 75 HELP
          "Desmarca os registros selecionados" WIDGET-ID 8
     br-digita AT ROW 1.46 COL 2.86 WIDGET-ID 200
     bt-sav AT ROW 6.13 COL 75 HELP
          "Confirma altera‡äes" WIDGET-ID 22
     d-dt-vencimento AT ROW 11.33 COL 15.57 COLON-ALIGNED WIDGET-ID 2
     bt-todos AT ROW 1.67 COL 75 HELP
          "Seleciona todos os registros" WIDGET-ID 10
     c-campo AT ROW 12.33 COL 17.72 NO-LABEL WIDGET-ID 32
     bt-ok AT ROW 15.17 COL 3
     bt-cancelar AT ROW 15.17 COL 14
     bt-ajuda AT ROW 15.17 COL 69.72
     "Historico:" VIEW-AS TEXT
          SIZE 9 BY .67 AT ROW 12.42 COL 8.72 WIDGET-ID 34
     RECT-1 AT ROW 14.96 COL 2
     RECT-5 AT ROW 1.5 COL 73.86 WIDGET-ID 12
     RECT-33 AT ROW 1.25 COL 2 WIDGET-ID 24
     RECT-34 AT ROW 11.21 COL 2 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.86 BY 16.33 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert Custom SmartWindow title>"
         HEIGHT             = 15.67
         WIDTH              = 81.14
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.13
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB br-digita RECT-34 F-Main */
/* SETTINGS FOR BUTTON bt-nenhum IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-sav IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-todos IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-digita-aux.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* <insert Custom SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* <insert Custom SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-window
ON MOUSE-SELECT-DBLCLICK OF br-digita IN FRAME F-Main
DO:
    run pi-selecao in this-procedure (input 3, /* manutencao em unico registro */
                                      input tt-digita-aux.id).
    self:refresh().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-window
ON ROW-DISPLAY OF br-digita IN FRAME F-Main
DO:
    def var i-font-size as integer no-undo.
    def var i-color-linha as integer no-undo.
    
    if  avail tt-digita-aux and tt-digita-aux.sel = "SIM" then
        assign i-color-linha = 9
               i-font-size   = 12.
    else
        assign i-color-linha = 0
               i-font-size   = 10.

    assign bt-todos :sensitive in frame {&frame-name} = yes
           bt-nenhum:sensitive in frame {&frame-name} = yes
           bt-sav   :sensitive in frame {&frame-name} = yes.

    assign tt-digita-aux.sel             :fgcolor in browse br-digita = i-color-linha
           tt-digita-aux.cod-estabel     :fgcolor in browse br-digita = i-color-linha 
           tt-digita-aux.cod-fornec      :fgcolor in browse br-digita = i-color-linha 
           tt-digita-aux.nome-abrev      :fgcolor in browse br-digita = i-color-linha 
           tt-digita-aux.cod-esp         :fgcolor in browse br-digita = i-color-linha 
           tt-digita-aux.serie           :fgcolor in browse br-digita = i-color-linha
           tt-digita-aux.nr-docto        :fgcolor in browse br-digita = i-color-linha 
           tt-digita-aux.parcela         :fgcolor in browse br-digita = i-color-linha 
           tt-digita-aux.dt-vencimen     :fgcolor in browse br-digita = i-color-linha
/*            tt-digita-aux.dt-prev-pag     :fgcolor in browse br-digita = i-color-linha */
           tt-digita-aux.valor-saldo     :fgcolor in browse br-digita = i-color-linha  
           tt-digita-aux.vl-original     :fgcolor in browse br-digita = i-color-linha  
           . 

    assign tt-digita-aux.sel             :font    in browse br-digita = i-font-size
           tt-digita-aux.cod-estabel     :font    in browse br-digita = i-font-size 
           tt-digita-aux.cod-fornec      :font    in browse br-digita = i-font-size 
           tt-digita-aux.nome-abrev      :font    in browse br-digita = i-font-size 
           tt-digita-aux.cod-esp         :font    in browse br-digita = i-font-size 
           tt-digita-aux.serie           :font    in browse br-digita = i-font-size
           tt-digita-aux.nr-docto        :font    in browse br-digita = i-font-size 
           tt-digita-aux.parcela         :font    in browse br-digita = i-font-size 
           tt-digita-aux.dt-vencimen     :font    in browse br-digita = i-font-size
/*            tt-digita-aux.dt-prev-pag     :font    in browse br-digita = i-font-size */
           tt-digita-aux.valor-saldo     :font    in browse br-digita = i-font-size  
           tt-digita-aux.vl-original     :font    in browse br-digita = i-font-size
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
    IF VALID-HANDLE(h-handle-call) THEN
        RUN pi-recebe-titulos IN h-handle-call (INPUT TABLE tt-digita, INPUT TABLE tt_log_erros_tit_ap_total).

    apply "close":U to this-procedure.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-window
ON CHOOSE OF bt-nenhum IN FRAME F-Main /* Nenhum */
DO:
    /* manutencao em todos registros - desmarcar */    

    IF br-digita:QUERY:CURRENT-RESULT-ROW IN FRAME {&FRAME-NAME} > 0 THEN DO:
        RUN pi-selecao IN THIS-PROCEDURE (INPUT 2,
                                          INPUT 0).
            IF VALID-HANDLE({&browse-name}:HANDLE) THEN
                {&browse-name}:REFRESH().
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* Modifica */
DO:
    RUN utp/ut-msgs.p (INPUT "show":U, 
                       INPUT  27100,
                       INPUT "Confirma altera‡Æo da Dt. Vencimento?~~" +
                             "Confirma altera‡Æo da Dt. Vencimento?"                        + CHR(10) + 
                             "Atencao, este procedimento iraÿalterar a Data Vencimento "    + CHR(10) + 
                             "para " + d-dt-vencimento:SCREEN-VALUE IN FRAME {&FRAME-NAME}  + CHR(10) ).     

    IF RETURN-VALUE <> "yes" THEN  RETURN NO-APPLY.
    RUN pi-modifica.
    RUN pi-procuraselecao.

    IF VALID-HANDLE(h-handle-call) THEN
        RUN pi-recebe-titulos IN h-handle-call (INPUT TABLE tt-digita, INPUT TABLE tt_log_erros_tit_ap_total).

    apply "close":U to this-procedure.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sav
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sav w-window
ON CHOOSE OF bt-sav IN FRAME F-Main /* Marcar */
DO:
    RUN pi-selecao IN THIS-PROCEDURE (INPUT 3, /* manutencao em unico registro */
                                      INPUT tt-digita-aux.id).
/*     {&OPEN-QUERY-br-digita} */

    br-digita:REFRESH().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-window
ON CHOOSE OF bt-todos IN FRAME F-Main /* Todos */
DO:
  
    IF br-digita:QUERY:CURRENT-RESULT-ROW IN FRAME {&FRAME-NAME} > 0 THEN DO:
        RUN pi-selecao IN THIS-PROCEDURE (INPUT 1, /* manutencao em todos registros - marcar */
                                          INPUT 0).
        IF VALID-HANDLE({&browse-name}:HANDLE) THEN
            {&browse-name}:REFRESH().
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{utp/ut9000.i "ESAP001A" "2.06.00.000"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY d-dt-vencimento c-campo 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 RECT-5 RECT-33 RECT-34 br-digita d-dt-vencimento c-campo bt-ok 
         bt-cancelar bt-ajuda 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
  {utp/ut9000.i "ESAP001A" "2.06.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega w-window 
PROCEDURE pi-carrega :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input param p-handle as handle no-undo.
    def input param table for tt-digita.

    assign h-handle-call = p-handle.

    /*----deleta tt do browse---------*/
    for each tt-digita-aux:
        delete tt-digita-aux.
    end.

    for each tt-digita no-lock:
        create tt-digita-aux.
        buffer-copy tt-digita to tt-digita-aux.
    end.

    /*---deleta tt do relatorio------ */
    for each tt-digita:
        delete tt-digita.
    end.

    assign br-digita:refreshable in frame {&frame-name} = yes.

    {&OPEN-QUERY-br-digita}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-modifica w-window 
PROCEDURE pi-modifica :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var v_cod_refer     as char       no-undo.
    def var h-acomp         as handle     no-undo.
    def var i-linha         as int        no-undo.
    def var v_log_refer_uni as log        no-undo.
    def var h_apb767zf      as handle     no-undo.

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp(input "Aguarde ...Alterando Titulos").

    empty temp-table tt_log_erros_tit_ap_total.

    run prgfin/apb/apb767zf.py persistent set h_apb767zf.

    for each tt-digita-aux
       where tt-digita-aux.sel = "SIM"
         and tt-digita-aux.id > 0,
       first tit_ap no-lock use-index titap_id
       where tit_ap.cod_estab       = tt-digita-aux.cod-estabel
         and tit_ap.cdn_fornecedor  = tt-digita-aux.cod-fornec
         and tit_ap.cod_espec_docto = tt-digita-aux.cod-esp
         and tit_ap.cod_ser_docto   = tt-digita-aux.serie
         and tit_ap.cod_tit_ap      = tt-digita-aux.nr-docto
         and tit_ap.cod_parcela     = tt-digita-aux.parcela:
        assign i-linha = i-linha + 1.

        empty temp-table tt_tit_ap_alteracao_base_aux_3.
        empty temp-table tt_tit_ap_alteracao_rateio.
        empty temp-table tt_log_erros_tit_ap_alteracao.

        /*---------- Cria Referˆncia ----------*/
        assign v_log_refer_uni = no
               v_cod_refer     = "".
        /*
        repeat while not v_log_refer_uni:
            /* bisneto 20/007/2015
            run pi_retorna_sugestao_referencia(input "T" /*l_T*/ ,
                                               input today,
                                               output v_cod_refer).

            run pi_verifica_refer_unica_apb(input  tit_ap.cod_estab,
                                            input  v_cod_refer,
                                            output v_log_refer_uni).
            */
        end.
        */
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
               tt_tit_ap_alteracao_base_aux_3.ttv_dat_transacao               = TODAY /* tit_ap.dat_transacao yyy */
               tt_tit_ap_alteracao_base_aux_3.ttv_cod_refer                   = v_cod_refer
               tt_tit_ap_alteracao_base_aux_3.tta_val_sdo_tit_ap              = tit_ap.val_sdo_tit_ap             
               tt_tit_ap_alteracao_base_aux_3.tta_dat_emis_docto              = tit_ap.dat_emis_docto             
               tt_tit_ap_alteracao_base_aux_3.tta_dat_vencto_tit_ap           = input frame {&frame-name} d-dt-vencimento
               tt_tit_ap_alteracao_base_aux_3.tta_dat_prev_pagto              = input frame {&frame-name} d-dt-vencimento
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
               tt_tit_ap_alteracao_base_aux_3.tta_des_histor_padr             = "A data de vencimento do documento foi alterada na data "
                                                                                + string(date(today)) + " Por " + c-seg-usuario + " de: "
                                                                                + string(tt-digita-aux.dt-vencimen) + " para: "
                                                                                + string(input frame {&frame-name} d-dt-vencimento )
                                                                                + string(input frame {&frame-name} c-campo )
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

    run pi-finalizar in h-acomp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-procuraselecao w-window 
PROCEDURE pi-procuraselecao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    empty temp-table tt-digita.

    for each tt-digita-aux
       where tt-digita-aux.sel = "SIM"
         and tt-digita-aux.id > 0:
        create tt-digita.
        assign tt-digita.id          = tt-digita-aux.id          
               tt-digita.sel         = tt-digita-aux.sel         
               tt-digita.cod-estabel = tt-digita-aux.cod-estabel 
               tt-digita.cod-fornec  = tt-digita-aux.cod-fornec  
               tt-digita.nome-abrev  = tt-digita-aux.nome-abrev  
               tt-digita.cod-esp     = tt-digita-aux.cod-esp     
               tt-digita.serie       = tt-digita-aux.serie       
               tt-digita.nr-docto    = tt-digita-aux.nr-docto    
               tt-digita.parcela     = tt-digita-aux.parcela     
               tt-digita.portador    = tt-digita-aux.portador    
               tt-digita.dt-vencimen = tt-digita-aux.dt-vencimen 
    /*            tt-digita.dt-prev-pag = tt-digita-aux.dt-prev-pag */
               tt-digita.dt-emissao  = tt-digita-aux.dt-emissao  
               tt-digita.valor-saldo = tt-digita-aux.valor-saldo 
               tt-digita.vl-original = tt-digita-aux.vl-original
               tt-digita.dt-modifica = date(d-dt-vencimento:screen-value in frame {&frame-name})
               .
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-selecao w-window 
PROCEDURE pi-selecao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:   p-acao = 1 - seleciona todos os registros
                p-acao = 2 - desmarca todos os registros
                p-acao = 3 - seleciona unico registro

                p-row = registro selecionado
  Notes:       
------------------------------------------------------------------------------*/
    def input param p-acao   as int no-undo.
    def input param p-row    as int no-undo.

    case p-acao:
        when 1 then do:
            for each tt-digita-aux exclusive-lock.
                assign tt-digita-aux.sel = "SIM".
            end.
        end.
        when 2 then do:
            for each tt-digita-aux exclusive-lock.
                assign tt-digita-aux.sel = "NAO".
            end.
        end.
        when 3 then do:
            find first tt-digita-aux where tt-digita-aux.id = p-row exclusive-lock no-error.
            if avail tt-digita-aux then
                if tt-digita-aux.sel = "SIM" then assign tt-digita-aux.sel = "NAO". else assign tt-digita-aux.sel = "SIM".
            else 
                return "NOK".
        end.
     otherwise
        return "NOK".
    end case.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_retorna_sugestao_referencia w-window  _ADM-SEND-RECORDS
PROCEDURE pi_retorna_sugestao_referencia :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-digita-aux"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_verifica_refer_unica_apb w-window  _ADM-SEND-RECORDS
PROCEDURE pi_verifica_refer_unica_apb :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-digita-aux"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-digita-aux"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
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

