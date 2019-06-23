&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5_esp         PROGRESS
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/****************************************************************************************** 
**             Programa: apb001-w01.w
**            Autor: Vando Ribeiro
**       Fornecedor: DKP
**            Data: 01/11/2018
**  Change/Chamado: 
**        Objetivo: Aprovar Atualizaá∆o de Documentos manuais do Contas a Pagar e 
                    movimentos manuais do Caixa e Bancos.
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** 
** Data         Autor                   Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
** 
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA:
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{utp/ut-glob.i}
{utp/utapi019.i}
{include/i-prgvrs.i apb001-w01 1.00.00.000}

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE c-mess-top         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mess             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mess-base        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mot-reprov       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-confirma-titulos AS LOGICAL     NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario          AS CHARACTER                          NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE v_rec_lote_impl_tit_ap AS RECID FORMAT ">>>>>>9":U INITIAL ? NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-prg-aprov-movto      AS LOGICAL NO-UNDO. 

def new shared temp-table tt_log_erros_atualiz no-undo
    field tta_cod_estab                         as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_refer                         as character format "x(10)" label "Referància" column-label "Referància"
    field tta_num_seq_refer                     as integer format ">>>9" initial 0 label "Sequància" column-label "Seq"
    field ttv_num_mensagem                      as integer format ">>>>,>>9" label "Número" column-label "Número Mensagem"
    field ttv_des_msg_erro                      as character format "x(60)" label "Mensagem Erro" column-label "Inconsistància"
    field ttv_des_msg_ajuda                     as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_ind_tip_relacto                   as character format "X(15)" label "Tipo Relacionamento" column-label "Tipo Relac"
    field ttv_num_relacto                       as integer format ">>>>,>>9" label "Relacionamento" column-label "Relacionamento".

DEFINE TEMP-TABLE tt_esp_pend_lote_ap           LIKE esp_pend_lote_ap
    FIELD cdn_fornecedor                        LIKE item_lote_impl_ap.cdn_fornecedor   
    FIELD cod_espec_docto                       LIKE item_lote_impl_ap.cod_espec_docto  
    FIELD cod_ser_docto                         LIKE item_lote_impl_ap.cod_ser_docto    
    FIELD cod_tit_ap                            LIKE item_lote_impl_ap.cod_tit_ap       
    FIELD cod_parcela                           LIKE item_lote_impl_ap.cod_parcela      
    FIELD dat_emis_docto                        LIKE item_lote_impl_ap.dat_emis_docto   
    FIELD dat_vencto_tit_ap                     LIKE item_lote_impl_ap.dat_vencto_tit_ap
    FIELD val_tit_ap                            LIKE item_lote_impl_ap.val_tit_ap
    FIELD des_text_histor                       LIKE item_lote_impl_ap.des_text_histor.

DEFINE TEMP-TABLE tt_esp_pend_movto_cta_corren  LIKE esp_pend_movto_cta_corren
    FIELD val_movto_cta_corren                  LIKE movto_cta_corren.val_movto_cta_corren
    FIELD dat_transacao                         LIKE movto_cta_corren.dat_transacao
    FIELD des_histor_movto_cta_corren           LIKE movto_cta_corren.des_histor_movto_cta_corren
    FIELD cod_usuar_ult_atualiz                 LIKE movto_cta_corren.cod_usuar_ult_atualiz
    FIELD ind_fluxo_movto_cta_corren            LIKE movto_cta_corren.ind_fluxo_movto_cta_corren.

DEFINE TEMP-TABLE tt-tit
    FIELD cod_estab                             LIKE esp_pend_lote_ap.cod_estab
    FIELD cod_refer                             LIKE esp_pend_lote_ap.cod_refer
    FIELD dt_aprovacao1                         LIKE esp_pend_lote.dt_aprovacao1
    FIELD dt_aprovacao2                         LIKE esp_pend_lote.dt_aprovacao2
    FIELD cod_usuario                           LIKE esp_pend_lote.cod_usuario.

DEFINE TEMP-TABLE tt_tit_ap_par
    FIELD cod_estab         LIKE tit_ap.cod_estab                
    FIELD cdn_fornecedor    LIKE tit_ap.cdn_fornecedor
    FIELD cod_espec_docto   LIKE tit_ap.cod_espec_docto          
    FIELD cod_ser_docto     LIKE tit_ap.cod_ser_docto            
    FIELD cod_tit_ap        LIKE tit_ap.cod_tit_ap               
    FIELD val_origin_tit_ap LIKE tit_ap.val_origin_tit_ap
    FIELD dat_emis_docto    LIKE tit_ap.dat_emis_docto.

DEFINE TEMP-TABLE tt_esp_hist_aprov_tit         LIKE esp_hist_aprov_tit.

DEFINE BUFFER   b_esp_aprovador FOR esp_aprovador.
DEFINE BUFFER   b-item_lote_impl_ap FOR item_lote_impl_ap.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME brw-doc-apb-a

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES esp_hist_aprov_tit tt_esp_pend_lote_ap ~
esp_hist_aprov_movto_cc tt_esp_pend_movto_cta_corren

/* Definitions for BROWSE brw-doc-apb-a                                 */
&Scoped-define FIELDS-IN-QUERY-brw-doc-apb-a esp_hist_aprov_tit.cod_estab esp_hist_aprov_tit.cod_refer esp_hist_aprov_tit.num_seq_refer esp_hist_aprov_tit.cod_usuario_aprov1 esp_hist_aprov_tit.dt_aprovacao1 esp_hist_aprov_tit.cod_usuario_aprov2 esp_hist_aprov_tit.dt_aprovacao2 esp_hist_aprov_tit.cdn_fornecedor esp_hist_aprov_tit.cod_espec_docto esp_hist_aprov_tit.cod_ser_docto esp_hist_aprov_tit.cod_tit_ap esp_hist_aprov_tit.cod_parcela esp_hist_aprov_tit.dat_emis_docto esp_hist_aprov_tit.dat_vencto_tit_ap esp_hist_aprov_tit.val_tit_ap esp_hist_aprov_tit.cod_usuario esp_hist_aprov_tit.des_text_histor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw-doc-apb-a   
&Scoped-define SELF-NAME brw-doc-apb-a
&Scoped-define QUERY-STRING-brw-doc-apb-a FOR EACH esp_hist_aprov_tit                                WHERE esp_hist_aprov_tit.dt_aprovacao1 <> ?                                  AND esp_hist_aprov_tit.dt_aprovacao2 <> ?                                      NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brw-doc-apb-a OPEN QUERY {&SELF-NAME} FOR EACH esp_hist_aprov_tit                                WHERE esp_hist_aprov_tit.dt_aprovacao1 <> ?                                  AND esp_hist_aprov_tit.dt_aprovacao2 <> ?                                      NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brw-doc-apb-a esp_hist_aprov_tit
&Scoped-define FIRST-TABLE-IN-QUERY-brw-doc-apb-a esp_hist_aprov_tit


/* Definitions for BROWSE brw-doc-apb-p                                 */
&Scoped-define FIELDS-IN-QUERY-brw-doc-apb-p tt_esp_pend_lote_ap.cod_estab tt_esp_pend_lote_ap.cod_refer tt_esp_pend_lote_ap.num_seq_refer tt_esp_pend_lote_ap.cod_usuario_aprov1 tt_esp_pend_lote_ap.dt_aprovacao1 tt_esp_pend_lote_ap.cod_usuario_aprov2 tt_esp_pend_lote_ap.dt_aprovacao2 tt_esp_pend_lote_ap.cdn_fornecedor tt_esp_pend_lote_ap.cod_espec_docto tt_esp_pend_lote_ap.cod_ser_docto tt_esp_pend_lote_ap.cod_tit_ap tt_esp_pend_lote_ap.cod_parcela tt_esp_pend_lote_ap.dat_emis_docto tt_esp_pend_lote_ap.dat_vencto_tit_ap tt_esp_pend_lote_ap.val_tit_ap tt_esp_pend_lote_ap.cod_usuario tt_esp_pend_lote_ap.des_text_histor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw-doc-apb-p   
&Scoped-define SELF-NAME brw-doc-apb-p
&Scoped-define QUERY-STRING-brw-doc-apb-p FOR EACH tt_esp_pend_lote_ap NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brw-doc-apb-p OPEN QUERY {&SELF-NAME} FOR EACH tt_esp_pend_lote_ap NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brw-doc-apb-p tt_esp_pend_lote_ap
&Scoped-define FIRST-TABLE-IN-QUERY-brw-doc-apb-p tt_esp_pend_lote_ap


/* Definitions for BROWSE brw-doc-apb-r                                 */
&Scoped-define FIELDS-IN-QUERY-brw-doc-apb-r esp_hist_aprov_tit.cod_estab esp_hist_aprov_tit.cod_refer esp_hist_aprov_tit.num_seq_refer esp_hist_aprov_tit.dt_reprov esp_hist_aprov_tit.cod_usuario_reprov esp_hist_aprov_tit.cdn_fornecedor esp_hist_aprov_tit.cod_espec_docto esp_hist_aprov_tit.cod_ser_docto esp_hist_aprov_tit.cod_tit_ap esp_hist_aprov_tit.cod_parcela esp_hist_aprov_tit.dat_emis_docto esp_hist_aprov_tit.dat_vencto_tit_ap esp_hist_aprov_tit.val_tit_ap esp_hist_aprov_tit.cod_usuario esp_hist_aprov_tit.des_text_histor esp_hist_aprov_tit.mot-reprov   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw-doc-apb-r   
&Scoped-define SELF-NAME brw-doc-apb-r
&Scoped-define QUERY-STRING-brw-doc-apb-r FOR EACH esp_hist_aprov_tit                                WHERE esp_hist_aprov_tit.dt_reprovacao <> ?                                      NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brw-doc-apb-r OPEN QUERY {&SELF-NAME} FOR EACH esp_hist_aprov_tit                                WHERE esp_hist_aprov_tit.dt_reprovacao <> ?                                      NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brw-doc-apb-r esp_hist_aprov_tit
&Scoped-define FIRST-TABLE-IN-QUERY-brw-doc-apb-r esp_hist_aprov_tit


/* Definitions for BROWSE brw-doc-cmg-a                                 */
&Scoped-define FIELDS-IN-QUERY-brw-doc-cmg-a esp_hist_aprov_movto_cc.cod_cta_corren esp_hist_aprov_movto_cc.dat_movto_cta_corren esp_hist_aprov_movto_cc.num_seq_movto_cta_corren esp_hist_aprov_movto_cc.dt_aprovacao1 esp_hist_aprov_movto_cc.dt_aprovacao2 esp_hist_aprov_movto_cc.cod_usuario_aprov1 esp_hist_aprov_movto_cc.cod_usuario_aprov2 esp_hist_aprov_movto_cc.ind_fluxo_movto_cta_corren esp_hist_aprov_movto_cc.val_movto_cta_corren esp_hist_aprov_movto_cc.dat_transacao esp_hist_aprov_movto_cc.des_text_histor esp_hist_aprov_movto_cc.cod_usuario esp_hist_aprov_movto_cc.dat_transacao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw-doc-cmg-a   
&Scoped-define SELF-NAME brw-doc-cmg-a
&Scoped-define QUERY-STRING-brw-doc-cmg-a FOR EACH esp_hist_aprov_movto_cc                            WHERE esp_hist_aprov_movto_cc.dt_aprovacao1 <> ?                              AND esp_hist_aprov_movto_cc.dt_aprovacao2 <> ?                              NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brw-doc-cmg-a OPEN QUERY {&SELF-NAME} FOR EACH esp_hist_aprov_movto_cc                            WHERE esp_hist_aprov_movto_cc.dt_aprovacao1 <> ?                              AND esp_hist_aprov_movto_cc.dt_aprovacao2 <> ?                              NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brw-doc-cmg-a esp_hist_aprov_movto_cc
&Scoped-define FIRST-TABLE-IN-QUERY-brw-doc-cmg-a esp_hist_aprov_movto_cc


/* Definitions for BROWSE brw-doc-cmg-p                                 */
&Scoped-define FIELDS-IN-QUERY-brw-doc-cmg-p tt_esp_pend_movto_cta_corren.cod_cta_corren tt_esp_pend_movto_cta_corren.dat_movto_cta_corren tt_esp_pend_movto_cta_corren.num_seq_movto_cta_corren tt_esp_pend_movto_cta_corren.dt_aprovacao1 tt_esp_pend_movto_cta_corren.cod_usuario_aprov1 tt_esp_pend_movto_cta_corren.dt_aprovacao2 tt_esp_pend_movto_cta_corren.cod_usuario_aprov2 tt_esp_pend_movto_cta_corren.val_movto_cta_corren tt_esp_pend_movto_cta_corren.ind_fluxo_movto_cta_corren tt_esp_pend_movto_cta_corren.dat_transacao tt_esp_pend_movto_cta_corren.cod_usuar_ult_atualiz tt_esp_pend_movto_cta_corren.cod_usuario tt_esp_pend_movto_cta_corren.des_histor_movto_cta_corren   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw-doc-cmg-p   
&Scoped-define SELF-NAME brw-doc-cmg-p
&Scoped-define QUERY-STRING-brw-doc-cmg-p FOR EACH tt_esp_pend_movto_cta_corren NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brw-doc-cmg-p OPEN QUERY {&SELF-NAME} FOR EACH tt_esp_pend_movto_cta_corren NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brw-doc-cmg-p tt_esp_pend_movto_cta_corren
&Scoped-define FIRST-TABLE-IN-QUERY-brw-doc-cmg-p tt_esp_pend_movto_cta_corren


/* Definitions for BROWSE brw-doc-cmg-r                                 */
&Scoped-define FIELDS-IN-QUERY-brw-doc-cmg-r esp_hist_aprov_movto_cc.cod_cta_corren esp_hist_aprov_movto_cc.dat_movto_cta_corren esp_hist_aprov_movto_cc.num_seq_movto_cta_corren esp_hist_aprov_movto_cc.dt_reprov esp_hist_aprov_movto_cc.cod_usuario_reprov esp_hist_aprov_movto_cc.val_movto_cta_corren esp_hist_aprov_movto_cc.ind_fluxo_movto_cta_corren esp_hist_aprov_movto_cc.dat_transacao esp_hist_aprov_movto_cc.des_text_histor esp_hist_aprov_movto_cc.mot-reprov esp_hist_aprov_movto_cc.cod_usuario esp_hist_aprov_movto_cc.dat_transacao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw-doc-cmg-r   
&Scoped-define SELF-NAME brw-doc-cmg-r
&Scoped-define QUERY-STRING-brw-doc-cmg-r FOR EACH esp_hist_aprov_movto_cc                            WHERE esp_hist_aprov_movto_cc.dt_reprovacao <> ?                              NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brw-doc-cmg-r OPEN QUERY {&SELF-NAME} FOR EACH esp_hist_aprov_movto_cc                            WHERE esp_hist_aprov_movto_cc.dt_reprovacao <> ?                              NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brw-doc-cmg-r esp_hist_aprov_movto_cc
&Scoped-define FIRST-TABLE-IN-QUERY-brw-doc-cmg-r esp_hist_aprov_movto_cc


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-brw-doc-apb-a}~
    ~{&OPEN-QUERY-brw-doc-apb-p}~
    ~{&OPEN-QUERY-brw-doc-apb-r}~
    ~{&OPEN-QUERY-brw-doc-cmg-a}~
    ~{&OPEN-QUERY-brw-doc-cmg-p}~
    ~{&OPEN-QUERY-brw-doc-cmg-r}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-25 RECT-26 rt-button-2 ~
tp-modulo tp-movto c-cod_usuario c-nom_usuario brw-doc-cmg-r brw-doc-cmg-p ~
brw-doc-cmg-a brw-doc-apb-r brw-doc-apb-p brw-doc-apb-a bt-reprovar ~
bt-aprovar 
&Scoped-Define DISPLAYED-OBJECTS tp-modulo tp-movto c-cod_usuario ~
c-nom_usuario 

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
       SUB-MENU  mi-programa    LABEL "&Arquivo"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-aprovar AUTO-GO 
     LABEL "Aprovar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-reprovar AUTO-GO 
     LABEL "Reprovar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE c-cod_usuario AS CHARACTER FORMAT "X(12)":U 
     LABEL "Aprovador" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE c-nom_usuario AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .88 NO-UNDO.

DEFINE VARIABLE tp-modulo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Contas a Pagar", 1,
"Caixa e Bancos", 2
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tp-movto AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pendentes", 1,
"Aprovados", 2,
"Reprovados", 3
     SIZE 42 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.14 BY 1.5.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 1.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 97 BY 1.46
     BGCOLOR 7 .

DEFINE RECTANGLE rt-button-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 97 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brw-doc-apb-a FOR 
      esp_hist_aprov_tit SCROLLING.

DEFINE QUERY brw-doc-apb-p FOR 
      tt_esp_pend_lote_ap SCROLLING.

DEFINE QUERY brw-doc-apb-r FOR 
      esp_hist_aprov_tit SCROLLING.

DEFINE QUERY brw-doc-cmg-a FOR 
      esp_hist_aprov_movto_cc SCROLLING.

DEFINE QUERY brw-doc-cmg-p FOR 
      tt_esp_pend_movto_cta_corren SCROLLING.

DEFINE QUERY brw-doc-cmg-r FOR 
      esp_hist_aprov_movto_cc SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brw-doc-apb-a
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw-doc-apb-a w-livre _FREEFORM
  QUERY brw-doc-apb-a NO-LOCK DISPLAY
      esp_hist_aprov_tit.cod_estab          FORMAT "x(04)"
        esp_hist_aprov_tit.cod_refer          FORMAT "x(10)"
        esp_hist_aprov_tit.num_seq_refer      FORMAT ">>>9"
        esp_hist_aprov_tit.cod_usuario_aprov1 FORMAT "x(12)"
        esp_hist_aprov_tit.dt_aprovacao1      FORMAT "99/99/9999 HH:MM"
        esp_hist_aprov_tit.cod_usuario_aprov2 FORMAT "x(12)"
        esp_hist_aprov_tit.dt_aprovacao2      FORMAT "99/99/9999 HH:MM"
        esp_hist_aprov_tit.cdn_fornecedor     FORMAT ">>>>>>9"
        esp_hist_aprov_tit.cod_espec_docto    FORMAT "x(2)"
        esp_hist_aprov_tit.cod_ser_docto      FORMAT "X(3)"
        esp_hist_aprov_tit.cod_tit_ap         FORMAT "x(10)"
        esp_hist_aprov_tit.cod_parcela        FORMAT "x(2)"
        esp_hist_aprov_tit.dat_emis_docto     FORMAT "99/99/9999"
        esp_hist_aprov_tit.dat_vencto_tit_ap  FORMAT "99/99/9999"
        esp_hist_aprov_tit.val_tit_ap         FORMAT "->>>,>>>,>>9.99"
        esp_hist_aprov_tit.cod_usuario        FORMAT "x(12)"
        esp_hist_aprov_tit.des_text_histor    FORMAT "x(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 97 BY 11.25
         TITLE "Documentos Aprovador Contas a Pagar" FIT-LAST-COLUMN.

DEFINE BROWSE brw-doc-apb-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw-doc-apb-p w-livre _FREEFORM
  QUERY brw-doc-apb-p NO-LOCK DISPLAY
      tt_esp_pend_lote_ap.cod_estab          FORMAT "x(04)"
            tt_esp_pend_lote_ap.cod_refer          FORMAT "x(10)"
            tt_esp_pend_lote_ap.num_seq_refer      FORMAT ">>>9"
            tt_esp_pend_lote_ap.cod_usuario_aprov1 FORMAT "x(12)"
            tt_esp_pend_lote_ap.dt_aprovacao1      FORMAT "99/99/9999 HH:MM"
            tt_esp_pend_lote_ap.cod_usuario_aprov2 FORMAT "x(12)"
            tt_esp_pend_lote_ap.dt_aprovacao2      FORMAT "99/99/9999 HH:MM"
            tt_esp_pend_lote_ap.cdn_fornecedor     FORMAT ">>>>>>9"
            tt_esp_pend_lote_ap.cod_espec_docto    FORMAT "x(2)"
            tt_esp_pend_lote_ap.cod_ser_docto      FORMAT "X(3)"
            tt_esp_pend_lote_ap.cod_tit_ap         FORMAT "x(10)"
            tt_esp_pend_lote_ap.cod_parcela        FORMAT "x(2)"
            tt_esp_pend_lote_ap.dat_emis_docto     FORMAT "99/99/9999"
            tt_esp_pend_lote_ap.dat_vencto_tit_ap  FORMAT "99/99/9999"
            tt_esp_pend_lote_ap.val_tit_ap         FORMAT "->>>,>>>,>>9.99"
            tt_esp_pend_lote_ap.cod_usuario        FORMAT "x(12)"
            tt_esp_pend_lote_ap.des_text_histor    FORMAT "x(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 97 BY 11.25
         TITLE "Documentos Pendentes Contas a Pagar" FIT-LAST-COLUMN.

DEFINE BROWSE brw-doc-apb-r
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw-doc-apb-r w-livre _FREEFORM
  QUERY brw-doc-apb-r NO-LOCK DISPLAY
      esp_hist_aprov_tit.cod_estab          FORMAT "x(04)"
        esp_hist_aprov_tit.cod_refer          FORMAT "x(10)"
        esp_hist_aprov_tit.num_seq_refer      FORMAT ">>>9"
        esp_hist_aprov_tit.dt_reprov          FORMAT "99/99/9999 HH:MM"
        esp_hist_aprov_tit.cod_usuario_reprov FORMAT "x(12)"
        esp_hist_aprov_tit.cdn_fornecedor     FORMAT ">>>>>>9"
        esp_hist_aprov_tit.cod_espec_docto    FORMAT "x(2)"
        esp_hist_aprov_tit.cod_ser_docto      FORMAT "X(3)"
        esp_hist_aprov_tit.cod_tit_ap         FORMAT "x(10)"
        esp_hist_aprov_tit.cod_parcela        FORMAT "x(2)"
        esp_hist_aprov_tit.dat_emis_docto     FORMAT "99/99/9999"
        esp_hist_aprov_tit.dat_vencto_tit_ap  FORMAT "99/99/9999"
        esp_hist_aprov_tit.val_tit_ap         FORMAT "->>>,>>>,>>9.99"
        esp_hist_aprov_tit.cod_usuario        FORMAT "x(12)"
        esp_hist_aprov_tit.des_text_histor    FORMAT "x(50)"
        esp_hist_aprov_tit.mot-reprov         FORMAT "x(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 97 BY 11.25
         TITLE "Documentos Reprovados Contas a Pagar" FIT-LAST-COLUMN.

DEFINE BROWSE brw-doc-cmg-a
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw-doc-cmg-a w-livre _FREEFORM
  QUERY brw-doc-cmg-a NO-LOCK DISPLAY
      esp_hist_aprov_movto_cc.cod_cta_corren              format "x(10)"
        esp_hist_aprov_movto_cc.dat_movto_cta_corren        format "99/99/9999"
        esp_hist_aprov_movto_cc.num_seq_movto_cta_corren    format ">>>>9"
        esp_hist_aprov_movto_cc.dt_aprovacao1               FORMAT "99/99/9999 HH:MM"
        esp_hist_aprov_movto_cc.dt_aprovacao2               FORMAT "99/99/9999 HH:MM"
        esp_hist_aprov_movto_cc.cod_usuario_aprov1          format "x(12)"
        esp_hist_aprov_movto_cc.cod_usuario_aprov2          format "x(12)"
        esp_hist_aprov_movto_cc.ind_fluxo_movto_cta_corren  FORMAT "X(3)"
        esp_hist_aprov_movto_cc.val_movto_cta_corren        format "->>>,>>>,>>9.99"
        esp_hist_aprov_movto_cc.dat_transacao               format "99/99/9999"
        esp_hist_aprov_movto_cc.des_text_histor             format "X(50)"
        esp_hist_aprov_movto_cc.cod_usuario                 format "x(12)"
        esp_hist_aprov_movto_cc.dat_transacao               format "99/99/9999"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 97 BY 11.25
         TITLE "Movimentos Aprovados Caixa e Bancos" FIT-LAST-COLUMN.

DEFINE BROWSE brw-doc-cmg-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw-doc-cmg-p w-livre _FREEFORM
  QUERY brw-doc-cmg-p NO-LOCK DISPLAY
      tt_esp_pend_movto_cta_corren.cod_cta_corren              format "x(10)"
            tt_esp_pend_movto_cta_corren.dat_movto_cta_corren        format "99/99/9999"
            tt_esp_pend_movto_cta_corren.num_seq_movto_cta_corren    format ">>>>9"
            tt_esp_pend_movto_cta_corren.dt_aprovacao1               FORMAT "99/99/9999 HH:MM"
            tt_esp_pend_movto_cta_corren.cod_usuario_aprov1          format "x(12)"
            tt_esp_pend_movto_cta_corren.dt_aprovacao2               FORMAT "99/99/9999 HH:MM"
            tt_esp_pend_movto_cta_corren.cod_usuario_aprov2          format "x(12)"
            tt_esp_pend_movto_cta_corren.val_movto_cta_corren        format "->>>,>>>,>>9.99"
            tt_esp_pend_movto_cta_corren.ind_fluxo_movto_cta_corren  FORMAT "X(3)"
            tt_esp_pend_movto_cta_corren.dat_transacao               format "99/99/9999"
            tt_esp_pend_movto_cta_corren.cod_usuar_ult_atualiz       format "X(12)"
            tt_esp_pend_movto_cta_corren.cod_usuario                 format "x(12)"
            tt_esp_pend_movto_cta_corren.des_histor_movto_cta_corren format "X(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 97 BY 11.25
         TITLE "Movimentos Pendentes Caixa e Bancos" FIT-LAST-COLUMN.

DEFINE BROWSE brw-doc-cmg-r
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw-doc-cmg-r w-livre _FREEFORM
  QUERY brw-doc-cmg-r NO-LOCK DISPLAY
      esp_hist_aprov_movto_cc.cod_cta_corren              format "x(10)"
        esp_hist_aprov_movto_cc.dat_movto_cta_corren        format "99/99/9999"
        esp_hist_aprov_movto_cc.num_seq_movto_cta_corren    format ">>>>9"
        esp_hist_aprov_movto_cc.dt_reprov                   FORMAT "99/99/9999 HH:MM"
        esp_hist_aprov_movto_cc.cod_usuario_reprov          format "x(12)"
        esp_hist_aprov_movto_cc.val_movto_cta_corren        format "->>>,>>>,>>9.99"
        esp_hist_aprov_movto_cc.ind_fluxo_movto_cta_corren  FORMAT "X(3)"
        esp_hist_aprov_movto_cc.dat_transacao               format "99/99/9999"
        esp_hist_aprov_movto_cc.des_text_histor             format "X(50)"
        esp_hist_aprov_movto_cc.mot-reprov                  format "X(50)"
        esp_hist_aprov_movto_cc.cod_usuario                 format "x(12)"
        esp_hist_aprov_movto_cc.dat_transacao               format "99/99/9999"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 97 BY 11.25
         TITLE "Movimentos Reprovados Caixa e Bancos" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     tp-modulo AT ROW 3.25 COL 6 NO-LABEL WIDGET-ID 2
     tp-movto AT ROW 3.25 COL 51 NO-LABEL WIDGET-ID 6
     c-cod_usuario AT ROW 4.75 COL 21 COLON-ALIGNED WIDGET-ID 14
     c-nom_usuario AT ROW 4.75 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     brw-doc-cmg-r AT ROW 6 COL 2 WIDGET-ID 700
     brw-doc-cmg-p AT ROW 6 COL 2 WIDGET-ID 500
     brw-doc-cmg-a AT ROW 6 COL 2 WIDGET-ID 600
     brw-doc-apb-r AT ROW 6 COL 2 WIDGET-ID 500
     brw-doc-apb-p AT ROW 6 COL 2 WIDGET-ID 800
     brw-doc-apb-a AT ROW 6 COL 2 WIDGET-ID 500
     bt-reprovar AT ROW 17.67 COL 76 WIDGET-ID 20
     bt-aprovar AT ROW 17.67 COL 87 WIDGET-ID 18
     rt-button AT ROW 1.25 COL 2
     RECT-25 AT ROW 3 COL 2 WIDGET-ID 10
     RECT-26 AT ROW 3 COL 40 WIDGET-ID 12
     rt-button-2 AT ROW 17.5 COL 2 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 105.43 BY 18.83 WIDGET-ID 100.


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
         TITLE              = "Aprovaá∆o de Documento AP / Movtos do Caixa e Bancos"
         HEIGHT             = 18.83
         WIDTH              = 101.86
         MAX-HEIGHT         = 22.54
         MAX-WIDTH          = 105.43
         VIRTUAL-HEIGHT     = 22.54
         VIRTUAL-WIDTH      = 105.43
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
/* BROWSE-TAB brw-doc-cmg-r c-nom_usuario f-cad */
/* BROWSE-TAB brw-doc-cmg-p brw-doc-cmg-r f-cad */
/* BROWSE-TAB brw-doc-cmg-a brw-doc-cmg-p f-cad */
/* BROWSE-TAB brw-doc-apb-r brw-doc-cmg-a f-cad */
/* BROWSE-TAB brw-doc-apb-p brw-doc-apb-r f-cad */
/* BROWSE-TAB brw-doc-apb-a brw-doc-apb-p f-cad */
ASSIGN 
       c-cod_usuario:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       c-nom_usuario:READ-ONLY IN FRAME f-cad        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw-doc-apb-a
/* Query rebuild information for BROWSE brw-doc-apb-a
     _START_FREEFORM
    OPEN QUERY {&SELF-NAME} FOR EACH esp_hist_aprov_tit
                               WHERE esp_hist_aprov_tit.dt_aprovacao1 <> ?
                                 AND esp_hist_aprov_tit.dt_aprovacao2 <> ?
                                     NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brw-doc-apb-a */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw-doc-apb-p
/* Query rebuild information for BROWSE brw-doc-apb-p
     _START_FREEFORM

OPEN QUERY {&SELF-NAME} FOR EACH tt_esp_pend_lote_ap NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brw-doc-apb-p */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw-doc-apb-r
/* Query rebuild information for BROWSE brw-doc-apb-r
     _START_FREEFORM
    OPEN QUERY {&SELF-NAME} FOR EACH esp_hist_aprov_tit
                               WHERE esp_hist_aprov_tit.dt_reprovacao <> ?
                                     NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brw-doc-apb-r */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw-doc-cmg-a
/* Query rebuild information for BROWSE brw-doc-cmg-a
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH esp_hist_aprov_movto_cc
                           WHERE esp_hist_aprov_movto_cc.dt_aprovacao1 <> ?
                             AND esp_hist_aprov_movto_cc.dt_aprovacao2 <> ?
                             NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brw-doc-cmg-a */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw-doc-cmg-p
/* Query rebuild information for BROWSE brw-doc-cmg-p
     _START_FREEFORM

OPEN QUERY {&SELF-NAME} FOR EACH tt_esp_pend_movto_cta_corren NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brw-doc-cmg-p */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw-doc-cmg-r
/* Query rebuild information for BROWSE brw-doc-cmg-r
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH esp_hist_aprov_movto_cc
                           WHERE esp_hist_aprov_movto_cc.dt_reprovacao <> ?
                             NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brw-doc-cmg-r */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Aprovaá∆o de Documento AP / Movtos do Caixa e Bancos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Aprovaá∆o de Documento AP / Movtos do Caixa e Bancos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brw-doc-apb-p
&Scoped-define SELF-NAME brw-doc-apb-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brw-doc-apb-p w-livre
ON MOUSE-SELECT-CLICK OF brw-doc-apb-p IN FRAME f-cad /* Documentos Pendentes Contas a Pagar */
DO:
    /*
    MESSAGE tt_esp_pend_lote_ap.cod_estab
            tt_esp_pend_lote_ap.cod_refer
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-aprovar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-aprovar w-livre
ON CHOOSE OF bt-aprovar IN FRAME f-cad /* Aprovar */
DO: 
    IF INPUT tp-modulo = 1 AND brw-doc-apb-p:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN RETURN NO-APPLY.
    IF INPUT tp-modulo = 2 AND brw-doc-cmg-p:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN RETURN NO-APPLY.

    IF INPUT tp-modulo = 1 AND brw-doc-apb-p:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} > 0 
         AND tt_esp_pend_lote_ap.dt_aprovacao1 = ? THEN
    DO:
        RUN utp/ut-msgs.p (INPUT "show":U, INPUT 27100, 
                           INPUT  "Confirmaá∆o da Aprovaá∆o~~Confirma a aprovaá∆o do lote do(s) t°tulo(s)?").
        
        IF RETURN-VALUE <> "YES" AND RETURN-VALUE <> "OK" THEN
            RETURN NO-APPLY.
    END.

    IF INPUT tp-modulo = 2 AND brw-doc-cmg-p:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} > 0 
        AND tt_esp_pend_movto_cta_corren.dt_aprovacao1 = ? OR
        (INPUT tp-modulo = 2 AND brw-doc-cmg-p:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} > 0 
        AND tt_esp_pend_movto_cta_corren.dt_aprovacao2 = ?) THEN
    DO:
        RUN utp/ut-msgs.p (INPUT "show":U, INPUT 27100, 
                           INPUT  "Confirmaá∆o da Aprovaá∆o~~Confirma a aprovaá∆o do movimento do Caixa e Bancos?").
        
        IF RETURN-VALUE <> "YES" AND RETURN-VALUE <> "OK" THEN
            RETURN NO-APPLY.
    END.

    IF INPUT tp-modulo = 1 AND brw-doc-apb-p:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} > 0 
         AND tt_esp_pend_lote_ap.dt_aprovacao1 <> ?
         AND tt_esp_pend_lote_ap.dt_aprovacao2 = ? THEN
    DO:
        RUN utp/ut-msgs.p (INPUT "show":U, INPUT 27100, 
                           INPUT  "Confirmaá∆o da Aprovaá∆o/Atualizaá∆o~~Confirma a aprovaá∆o do lote e a atualizaá∆o do(s) t°tulo(s)?").
        
        IF RETURN-VALUE <> "YES" AND RETURN-VALUE <> "OK" THEN
            RETURN NO-APPLY.
    END.

    DEFINE VARIABLE c-assunto AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dt-aprov-atu AS DATETIME  NO-UNDO.
    DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.
    EMPTY TEMP-TABLE tt-tit.

    /* T°tulo */ 
    IF INPUT tp-modulo = 1 THEN
    DO:
        /*IF brw-doc-apb-p:SELECT-FOCUSED-ROW() THEN.*/

        IF brw-doc-apb-p:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} > 0 THEN
        DO:
            DO i-cont = 1 TO brw-doc-apb-p:NUM-SELECTED-ROWS:
                brw-doc-apb-p:FETCH-SELECTED-ROW(i-cont).
    
                FIND tt-tit WHERE
                     tt-tit.cod_estab = tt_esp_pend_lote_ap.cod_estab AND
                     tt-tit.cod_refer = tt_esp_pend_lote_ap.cod_refer NO-ERROR.
                IF NOT AVAIL tt-tit THEN
                DO:
                    CREATE tt-tit.
                    BUFFER-COPY tt_esp_pend_lote_ap TO tt-tit.
                END.
            END.
            /*verifica duplicidade/parecidos*/
            FOR EACH tt-tit:
                FOR EACH esp_pend_lote_ap
                   WHERE esp_pend_lote_ap.cod_estab = tt-tit.cod_estab
                     AND esp_pend_lote_ap.cod_refer = tt-tit.cod_refer EXCLUSIVE-LOCK:
    
                    FIND item_lote_impl_ap OF esp_pend_lote_ap NO-LOCK NO-ERROR.
                    EMPTY TEMP-TABLE tt_tit_ap_par.
                    FOR EACH b-item_lote_impl_ap NO-LOCK
                       WHERE b-item_lote_impl_ap.cod_estab            = item_lote_impl_ap.cod_estab
                         AND b-item_lote_impl_ap.cdn_fornecedor       = item_lote_impl_ap.cdn_fornecedor
                         AND YEAR(b-item_lote_impl_ap.dat_emis_docto) = YEAR(item_lote_impl_ap.dat_emis_docto)
                         AND b-item_lote_impl_ap.val_tit_ap           = item_lote_impl_ap.val_tit_ap:

                        CREATE tt_tit_ap_par.
                        ASSIGN
                            tt_tit_ap_par.cod_estab          = b-item_lote_impl_ap.cod_estab        
                            tt_tit_ap_par.cdn_fornecedor     = b-item_lote_impl_ap.cdn_fornecedor   
                            tt_tit_ap_par.cod_espec_docto    = b-item_lote_impl_ap.cod_espec_docto  
                            tt_tit_ap_par.cod_ser_docto      = b-item_lote_impl_ap.cod_ser_docto    
                            tt_tit_ap_par.cod_tit_ap         = b-item_lote_impl_ap.cod_tit_ap       
                            tt_tit_ap_par.val_origin_tit_ap  = b-item_lote_impl_ap.val_tit_ap
                            tt_tit_ap_par.dat_emis_docto     = b-item_lote_impl_ap.dat_emis_docto.
                    END.

                    FOR EACH tit_ap NO-LOCK
                       WHERE tit_ap.cod_estab            = item_lote_impl_ap.cod_estab           
                         AND tit_ap.cdn_fornecedor       = item_lote_impl_ap.cdn_fornecedor      
                         AND YEAR(tit_ap.dat_emis_docto) = YEAR(item_lote_impl_ap.dat_emis_docto)
                         AND tit_ap.ind_origin_tit_ap    = "APB" 
                         AND tit_ap.log_tit_ap_estordo   = NO
                         AND tit_ap.val_origin_tit_ap    = item_lote_impl_ap.val_tit_ap:

                        CREATE tt_tit_ap_par.
                        ASSIGN
                            tt_tit_ap_par.cod_estab          = tit_ap.cod_estab        
                            tt_tit_ap_par.cdn_fornecedor     = tit_ap.cdn_fornecedor   
                            tt_tit_ap_par.cod_espec_docto    = tit_ap.cod_espec_docto  
                            tt_tit_ap_par.cod_ser_docto      = tit_ap.cod_ser_docto    
                            tt_tit_ap_par.cod_tit_ap         = tit_ap.cod_tit_ap       
                            tt_tit_ap_par.val_origin_tit_ap  = tit_ap.val_origin_tit_ap
                            tt_tit_ap_par.dat_emis_docto     = tit_ap.dat_emis_docto.
                    END.

                    IF TEMP-TABLE tt_tit_ap_par:HAS-RECORDS THEN
                        RUN prgfin\upc\add_item_lote_impl_tit_ap_u02.w (INPUT TABLE tt_tit_ap_par,
                                                                        OUTPUT c-confirma-titulos).
                    IF NOT c-confirma-titulos THEN
                        RETURN NO-APPLY.
                END.
            END.

            RUN pi-aprova-tit.
    
            APPLY "VALUE-CHANGED" TO tp-movto.
        END.
    END.
    /* Caixa e Bancos */
    IF INPUT tp-modulo = 2 THEN
    DO:
        {prgfin\apb\apb001-i06.i}

        IF brw-doc-cmg-p:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} > 0 THEN
        DO i-cont = 1 TO brw-doc-cmg-p:NUM-SELECTED-ROWS:
            brw-doc-cmg-p:FETCH-SELECTED-ROW(i-cont).
    
            dt-aprov-atu = tt_esp_pend_movto_cta_corren.dt_aprovacao1.
    
            IF tt_esp_pend_movto_cta_corren.dt_aprovacao1 = ? THEN 
            DO:
                /*primeira aprovaá∆o*/
                {prgfin\apb\apb001-i04.i}
                IF esp_aprovador.nivel_aprovador <> 1 THEN
                DO:
                    RUN utp/ut-msgs.p ("show",17006,"Apresentou Erro na Aprovaá∆o~~Somente aprovador de N°vel 1 pode fazer a primeira a provaá∆o de movimento do Caixa e Bancos.").
                    RETURN NO-APPLY.
                END.
                FIND FIRST esp_pend_movto_cta_corren OF tt_esp_pend_movto_cta_corren EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL esp_pend_movto_cta_corren THEN
                    ASSIGN esp_pend_movto_cta_corren.dt_aprovacao1      = NOW
                           esp_pend_movto_cta_corren.cod_usuario_aprov1 = c-seg-usuario.

                c-mess = "<tr>"
                 + "<td>" + tt_esp_pend_movto_cta_corren.cod_cta_corren +                                    "</td>"
                 + "<td>" + STRING(tt_esp_pend_movto_cta_corren.dat_movto_cta_corren, "99/99/9999") +        "</td>"
                 + "<td>" + STRING(tt_esp_pend_movto_cta_corren.num_seq_movto_cta_corren, ">>>9") +          "</td>"
                 + "<td>" + tt_esp_pend_movto_cta_corren.ind_fluxo_movto_cta_corren +                        "</td>"
                 + "<td>" + STRING(tt_esp_pend_movto_cta_corren.val_movto_cta_corren, "->>>>,>>>,>>9.99") +  "</td>"
                 + "<td>" + STRING(tt_esp_pend_movto_cta_corren.dat_transacao , "99/99/9999") +              "</td>"
                 + "<td>" + tt_esp_pend_movto_cta_corren.des_histor_movto_cta_corren +                       "</td>"
                 + "<td>" + tt_esp_pend_movto_cta_corren.cod_usuario +                                       "</td>"
                 + "<td>" + STRING(tt_esp_pend_movto_cta_corren.dt_digitacao) +                              "</td>"
                 + "</tr>".

                ASSIGN c-assunto = "Pendància de Movimentos Para Aprovaá∆o".
                
                FOR EACH b_esp_aprovador NO-LOCK
                   WHERE b_esp_aprovador.aprov_caixa_banco
                     AND b_esp_aprovador.nivel_aprovador = 2:

                    /*manda e-mail para aprovador de nivel 2*/
                    RUN pi-envia-email (INPUT TRIM(b_esp_aprovador.email),
                                        INPUT c-assunto,
                                        INPUT c-mess-top + c-mess + c-mess-base,
                                        INPUT "CMG").
                END.
            END.
    
            IF tt_esp_pend_movto_cta_corren.dt_aprovacao2 = ? AND dt-aprov-atu <> ? THEN 
            DO:
                /*segunda aprovaá∆o*/
                {prgfin\apb\apb001-i06.i}
                IF esp_aprovador.nivel_aprovador <> 2 THEN
                DO:
                    RUN utp/ut-msgs.p ("show",17006,"Apresentou Erro na Aprovaá∆o~~Somente aprovador de N°vel 2 pode fazer a segunda aprovaá∆o de movimento do Caixa e Bancos").
                    RETURN NO-APPLY.
                END.

                FIND FIRST esp_pend_movto_cta_corren OF tt_esp_pend_movto_cta_corren EXCLUSIVE-LOCK NO-ERROR.

                FIND movto_cta_corren OF esp_pend_movto_cta_corren NO-LOCK NO-ERROR.
                FIND esp_hist_aprov_movto_cc OF esp_pend_movto_cta_corren EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAIL esp_hist_aprov_movto THEN
                DO:
                    CREATE esp_hist_aprov_movto_cc.
                    ASSIGN
                        esp_hist_aprov_movto_cc.cod_cta_corren           = esp_pend_movto_cta_corren.cod_cta_corren          
                        esp_hist_aprov_movto_cc.dat_movto_cta_corren     = esp_pend_movto_cta_corren.dat_movto_cta_corren    
                        esp_hist_aprov_movto_cc.num_seq_movto_cta_corren = esp_pend_movto_cta_corren.num_seq_movto_cta_corren.
                END.
                ASSIGN
                    esp_hist_aprov_movto_cc.dt_aprovacao1              = esp_pend_movto_cta_corren.dt_aprovacao1       
                    esp_hist_aprov_movto_cc.cod_usuario_aprov1         = esp_pend_movto_cta_corren.cod_usuario_aprov1  
                    esp_hist_aprov_movto_cc.dt_aprovacao2              = esp_pend_movto_cta_corren.dt_aprovacao2       
                    esp_hist_aprov_movto_cc.cod_usuario_aprov2         = esp_pend_movto_cta_corren.cod_usuario_aprov2  
                    esp_hist_aprov_movto_cc.cod_usuario_reprov         = esp_pend_movto_cta_corren.cod_usuario_reprov  
                    esp_hist_aprov_movto_cc.dt_reprovacao              = esp_pend_movto_cta_corren.dt_reprovacao       
                    esp_hist_aprov_movto_cc.mot-reprov                 = esp_pend_movto_cta_corren.mot-reprov          
                    esp_hist_aprov_movto_cc.cod_usuario                = esp_pend_movto_cta_corren.cod_usuario         
                    esp_hist_aprov_movto_cc.dt_digitacao               = esp_pend_movto_cta_corren.dt_digitacao        
                    esp_hist_aprov_movto_cc.des_text_histor            = movto_cta_corren.des_histor_movto_cta_corren
                    esp_hist_aprov_movto_cc.dat_transacao              = movto_cta_corren.dat_transacao                
                    esp_hist_aprov_movto_cc.val_movto_cta_corren       = movto_cta_corren.val_movto_cta_corren         
                    esp_hist_aprov_movto_cc.ind_fluxo_movto_cta_corren = movto_cta_corren.ind_fluxo_movto_cta_corren.

                c-mess = "<tr>"
                 + "<td>" + esp_hist_aprov_movto_cc.cod_cta_corren +                                    "</td>"
                 + "<td>" + STRING(esp_hist_aprov_movto_cc.dat_movto_cta_corren, "99/99/9999") +        "</td>"
                 + "<td>" + STRING(esp_hist_aprov_movto_cc.num_seq_movto_cta_corren, ">>>9") +          "</td>"
                 + "<td>" + esp_hist_aprov_movto_cc.ind_fluxo_movto_cta_corren +                        "</td>"
                 + "<td>" + STRING(esp_hist_aprov_movto_cc.val_movto_cta_corren, "->>>>,>>>,>>9.99") +  "</td>"
                 + "<td>" + STRING(esp_hist_aprov_movto_cc.dat_transacao , "99/99/9999") +              "</td>"
                 + "<td>" + esp_hist_aprov_movto_cc.des_text_histor +                                   "</td>"
                 + "<td>" + esp_hist_aprov_movto_cc.cod_usuario +                                       "</td>"
                 + "<td>" + STRING(esp_hist_aprov_movto_cc.dt_digitacao) +                              "</td>"
                 + "</tr>".

                IF AVAIL esp_pend_movto_cta_corren THEN
                    DELETE esp_pend_movto_cta_corren.

                ASSIGN c-assunto = "Aprovaá∆o de Movimento no Caixa e Bancos".
                /*manda e-mail para o usuario que cadastrou o movto no CMG*/
                RUN pi-envia-email (INPUT TRIM(esp_hist_aprov_movto_cc.cod_usuario) + "@yamana.com",
                                    INPUT c-assunto,
                                    INPUT c-mess-top + c-mess + c-mess-base,
                                    INPUT "CMG").
            END.
        END.
    END.
    APPLY "VALUE-CHANGED" TO tp-movto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-reprovar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-reprovar w-livre
ON CHOOSE OF bt-reprovar IN FRAME f-cad /* Reprovar */
DO: 
    DEFINE VARIABLE c-assunto AS CHARACTER   NO-UNDO.

    IF INPUT tp-modulo = 1 AND brw-doc-apb-p:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN RETURN NO-APPLY.
    IF INPUT tp-modulo = 2 AND brw-doc-cmg-p:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN RETURN NO-APPLY.

    IF INPUT tp-modulo = 1 THEN
        RUN prgfin\apb\apb001-d01.w (INPUT tt_esp_pend_lote_ap.cod_estab,
                                     INPUT tt_esp_pend_lote_ap.cod_refer,  
                                     INPUT tt_esp_pend_lote_ap.cdn_fornecedor,
                                     OUTPUT c-mot-reprov).
    ELSE
        RUN prgfin\apb\apb001-d02.w (INPUT tt_esp_pend_movto_cta_corren.cod_cta_corren,
                                     INPUT STRING(tt_esp_pend_movto_cta_corren.dat_movto_cta_corren, "99/99/9999"),  
                                     OUTPUT c-mot-reprov).

    IF c-mot-reprov = "" THEN
        RETURN NO-APPLY.

    EMPTY TEMP-TABLE tt-tit.
    DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.

    EMPTY TEMP-TABLE tt-tit.

    IF INPUT tp-modulo = 1 AND brw-doc-apb-p:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} > 0 THEN
    DO:
        {prgfin\apb\apb001-i07.i}
        ASSIGN c-assunto = "Reprovaá∆o de T°tulo no Contas a Pagar".

        DO i-cont = 1 TO brw-doc-apb-p:NUM-SELECTED-ROWS:
            brw-doc-apb-p:FETCH-SELECTED-ROW(i-cont).

            FIND tt-tit WHERE
                 tt-tit.cod_estab = tt_esp_pend_lote_ap.cod_estab AND
                 tt-tit.cod_refer = tt_esp_pend_lote_ap.cod_refer NO-ERROR.
            IF NOT AVAIL tt-tit THEN
            DO:
                CREATE tt-tit.
                BUFFER-COPY tt_esp_pend_lote_ap TO tt-tit.
            END.
        END.

        FOR EACH tt-tit:

            EMPTY TEMP-TABLE tt_esp_hist_aprov_tit.
            c-mess = "".
            FOR EACH esp_pend_lote_ap
               WHERE esp_pend_lote_ap.cod_estab = tt-tit.cod_estab
                 AND esp_pend_lote_ap.cod_refer = tt-tit.cod_refer NO-LOCK:

                FIND item_lote_impl_ap OF esp_pend_lote_ap NO-LOCK NO-ERROR.
                CREATE tt_esp_hist_aprov_tit.
                ASSIGN
                    tt_esp_hist_aprov_tit.cod_estab          = esp_pend_lote_ap.cod_estab    
                    tt_esp_hist_aprov_tit.cod_refer          = esp_pend_lote_ap.cod_refer    
                    tt_esp_hist_aprov_tit.num_seq_refer      = esp_pend_lote_ap.num_seq_refer
                    tt_esp_hist_aprov_tit.dt_aprovacao1      = esp_pend_lote_ap.dt_aprovacao1     
                    tt_esp_hist_aprov_tit.cod_usuario_aprov1 = esp_pend_lote_ap.cod_usuario_aprov1
                    tt_esp_hist_aprov_tit.dt_aprovacao2      = esp_pend_lote_ap.dt_aprovacao2      
                    tt_esp_hist_aprov_tit.cod_usuario_aprov2 = esp_pend_lote_ap.cod_usuario_aprov2 
                    tt_esp_hist_aprov_tit.cod_usuario_reprov = c-seg-usuario
                    tt_esp_hist_aprov_tit.dt_reprovacao      = NOW
                    tt_esp_hist_aprov_tit.mot-reprov         = c-mot-reprov
                    tt_esp_hist_aprov_tit.cod_usuario        = esp_pend_lote_ap.cod_usuario       
                    tt_esp_hist_aprov_tit.dt_digitacao       = esp_pend_lote_ap.dt_digitacao.

                FIND ems5.fornecedor OF item_lote_impl_ap NO-LOCK NO-ERROR.
                IF AVAIL item_lote_impl_ap THEN
                ASSIGN
                    tt_esp_hist_aprov_tit.cdn_fornecedor    = item_lote_impl_ap.cdn_fornecedor   
                    tt_esp_hist_aprov_tit.cod_espec_docto   = item_lote_impl_ap.cod_espec_docto  
                    tt_esp_hist_aprov_tit.cod_ser_docto     = item_lote_impl_ap.cod_ser_docto    
                    tt_esp_hist_aprov_tit.cod_tit_ap        = item_lote_impl_ap.cod_tit_ap       
                    tt_esp_hist_aprov_tit.cod_parcela       = item_lote_impl_ap.cod_parcela      
                    tt_esp_hist_aprov_tit.dat_emis_docto    = item_lote_impl_ap.dat_emis_docto   
                    tt_esp_hist_aprov_tit.dat_vencto_tit_ap = item_lote_impl_ap.dat_vencto_tit_ap
                    tt_esp_hist_aprov_tit.val_tit_ap        = item_lote_impl_ap.val_tit_ap       
                    tt_esp_hist_aprov_tit.des_text_histor   = item_lote_impl_ap.des_text_histor. 

                c-mess = c-mess + "<tr>"
                    + "<td>" + item_lote_impl_ap.cod_empresa +                                           "</td>"
                    + "<td>" + item_lote_impl_ap.cod_estab +                                             "</td>"
                    + "<td>" + STRING(item_lote_impl_ap.cdn_fornecedor) + " - " + ems5.fornecedor.nom_abrev + "</td>"
                    + "<td>" + item_lote_impl_ap.cod_tit_ap +                                            "</td>"
                    + "<td>" + item_lote_impl_ap.cod_ser_docto +                                         "</td>"
                    + "<td>" + item_lote_impl_ap.cod_espec_docto +                                       "</td>"
                    + "<td>" + item_lote_impl_ap.cod_parcela +                                           "</td>"
                    + "<td>" + STRING(item_lote_impl_ap.val_tit_ap, "->>>>,>>>,>>9.99") +                "</td>"
                    + "<td>" + STRING(item_lote_impl_ap.dat_emis_docto, "99/99/9999") +                  "</td>"
                    + "<td>" + STRING(item_lote_impl_ap.dat_vencto_tit_ap, "99/99/9999") +               "</td>"
                    + "<td>" + tt_esp_hist_aprov_tit.mot-reprov +                                        "</td>"
                    + "<td>" + esp_pend_lote_ap.cod_usuario +                                            "</td>"
                    + "<td>" + STRING(esp_pend_lote_ap.dt_digitacao, "99/99/9999 HH:MM") +                     "</td>"
                    + "</tr>".
            END.

            FOR EACH esp_pend_lote_ap
               WHERE esp_pend_lote_ap.cod_estab = tt-tit.cod_estab
                 AND esp_pend_lote_ap.cod_refer = tt-tit.cod_refer EXCLUSIVE-LOCK:

                DELETE esp_pend_lote_ap.
            END.
            FOR EACH tt_esp_hist_aprov_tit:
                FIND esp_hist_aprov_tit OF tt_esp_hist_aprov_tit NO-LOCK NO-ERROR.
                IF NOT AVAIL esp_hist_aprov_tit THEN
                DO:
                    CREATE esp_hist_aprov_tit.
                    BUFFER-COPY tt_esp_hist_aprov_tit TO esp_hist_aprov_tit NO-ERROR.
                END.
            END.

            RUN pi-envia-email (INPUT TRIM(tt-tit.cod_usuario) + "@yamana.com",
                                INPUT c-assunto,
                                INPUT c-mess-top + c-mess + c-mess-base,
                                INPUT "APB").
        END.
    END.
    
    IF INPUT tp-modulo = 2 AND brw-doc-cmg-p:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} > 0 THEN
    DO:
        {prgfin\apb\apb001-i08.i}
        ASSIGN c-assunto = "Reprovaá∆o de Movimento no Caixa e Bancos".

        DO i-cont = 1 TO brw-doc-cmg-p:NUM-SELECTED-ROWS:
            brw-doc-cmg-p:FETCH-SELECTED-ROW(i-cont).
    
            FIND FIRST esp_pend_movto_cta_corren OF tt_esp_pend_movto_cta_corren EXCLUSIVE-LOCK NO-ERROR.
    
            FIND movto_cta_corren OF esp_pend_movto_cta_corren NO-LOCK NO-ERROR.
            FIND esp_hist_aprov_movto_cc OF esp_pend_movto_cta_corren EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAIL esp_hist_aprov_movto THEN
            DO:
                CREATE esp_hist_aprov_movto_cc.
                ASSIGN
                    esp_hist_aprov_movto_cc.cod_cta_corren           = esp_pend_movto_cta_corren.cod_cta_corren          
                    esp_hist_aprov_movto_cc.dat_movto_cta_corren     = esp_pend_movto_cta_corren.dat_movto_cta_corren    
                    esp_hist_aprov_movto_cc.num_seq_movto_cta_corren = esp_pend_movto_cta_corren.num_seq_movto_cta_corren.
            END.
            ASSIGN
                esp_hist_aprov_movto_cc.dt_aprovacao1              = esp_pend_movto_cta_corren.dt_aprovacao1       
                esp_hist_aprov_movto_cc.cod_usuario_aprov1         = esp_pend_movto_cta_corren.cod_usuario_aprov1  
                esp_hist_aprov_movto_cc.dt_aprovacao2              = esp_pend_movto_cta_corren.dt_aprovacao2       
                esp_hist_aprov_movto_cc.cod_usuario_aprov2         = esp_pend_movto_cta_corren.cod_usuario_aprov2  
                esp_hist_aprov_movto_cc.cod_usuario_reprov         = c-seg-usuario  
                esp_hist_aprov_movto_cc.dt_reprovacao              = NOW
                esp_hist_aprov_movto_cc.mot-reprov                 = c-mot-reprov
                esp_hist_aprov_movto_cc.cod_usuario                = esp_pend_movto_cta_corren.cod_usuario         
                esp_hist_aprov_movto_cc.dt_digitacao               = esp_pend_movto_cta_corren.dt_digitacao        
                esp_hist_aprov_movto_cc.des_text_histor            = movto_cta_corren.des_histor_movto_cta_corren
                esp_hist_aprov_movto_cc.dat_transacao              = movto_cta_corren.dat_transacao                
                esp_hist_aprov_movto_cc.val_movto_cta_corren       = movto_cta_corren.val_movto_cta_corren         
                esp_hist_aprov_movto_cc.ind_fluxo_movto_cta_corren = movto_cta_corren.ind_fluxo_movto_cta_corren.

            c-mess = "<tr>"
             + "<td>" + esp_hist_aprov_movto_cc.cod_cta_corren +                                    "</td>"
             + "<td>" + STRING(esp_hist_aprov_movto_cc.dat_movto_cta_corren, "99/99/9999") +        "</td>"
             + "<td>" + STRING(esp_hist_aprov_movto_cc.num_seq_movto_cta_corren, ">>>9") +          "</td>"
             + "<td>" + esp_hist_aprov_movto_cc.ind_fluxo_movto_cta_corren +                        "</td>"
             + "<td>" + STRING(esp_hist_aprov_movto_cc.val_movto_cta_corren, "->>>>,>>>,>>9.99") +  "</td>"
             + "<td>" + STRING(esp_hist_aprov_movto_cc.dat_transacao , "99/99/9999") +              "</td>"
             + "<td>" + esp_hist_aprov_movto_cc.mot-reprov +                                        "</td>"
             + "<td>" + esp_hist_aprov_movto_cc.cod_usuario +                                       "</td>"
             + "<td>" + STRING(esp_hist_aprov_movto_cc.dt_digitacao) +                              "</td>"
             + "</tr>".

            RUN pi-envia-email (INPUT TRIM(esp_hist_aprov_movto_cc.cod_usuario) + "@yamana.com",
                                INPUT c-assunto,
                                INPUT c-mess-top + c-mess + c-mess-base,
                                INPUT "APB").

            IF AVAIL esp_pend_movto_cta_corren THEN
                ASSIGN esp_pend_movto_cta_corren.dt_reprovacao      = NOW
                       esp_pend_movto_cta_corren.cod_usuario_reprov = c-seg-usuario
                       esp_pend_movto_cta_corren.mot-reprov         = c-mot-reprov.

            DELETE esp_pend_movto_cta_corren.
        END.
    END.
    APPLY "VALUE-CHANGED" TO tp-movto.
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
ON MENU-DROP OF MENU mi-programa /* Arquivo */
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


&Scoped-define SELF-NAME tp-modulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tp-modulo w-livre
ON VALUE-CHANGED OF tp-modulo IN FRAME f-cad
DO:
    ASSIGN tp-movto:SCREEN-VALUE = "1".
    FIND esp_aprovador WHERE
         esp_aprovador.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.

    IF INPUT tp-modulo = 1 THEN
    DO:
        IF NOT esp_aprovador.aprov_contas_pagar THEN
        DO:
            ASSIGN tp-modulo:SCREEN-VALUE = "2".
            RUN utp/ut-msgs.p (INPUT "show":U, INPUT 27979, 
                               INPUT "Aprovador Inv†lido~~O usu†rio " + c-seg-usuario + " n∆o Ç aprovador de pendàncias do Caixa e Bancos.").

        END.
        APPLY "VALUE-CHANGED" TO tp-movto.
    END.
    ELSE
    DO:
        IF NOT esp_aprovador.aprov_caixa_bancos THEN
        DO:
            ASSIGN tp-modulo:SCREEN-VALUE = "1".
            RUN utp/ut-msgs.p (INPUT "show":U, INPUT 27979, 
                               INPUT "Aprovador Inv†lido~~O usu†rio " + c-seg-usuario + " n∆o Ç aprovador de pendàncias do Contas a Pagar.").
        END.
        APPLY "VALUE-CHANGED" TO tp-movto.
    END.

    ASSIGN bt-aprovar:SENSITIVE = YES
           bt-reprovar:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tp-movto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tp-movto w-livre
ON VALUE-CHANGED OF tp-movto IN FRAME f-cad
DO:
    CASE INPUT tp-movto:
        WHEN 1 THEN
        DO:
            IF INPUT tp-modulo = 1 THEN
            DO:
                brw-doc-apb-p:HIDDEN = NO.
                brw-doc-apb-a:HIDDEN = YES.
                brw-doc-apb-r:HIDDEN = YES.
                brw-doc-cmg-p:HIDDEN = YES.
                brw-doc-cmg-a:HIDDEN = YES.
                brw-doc-cmg-r:HIDDEN = YES.
                EMPTY TEMP-TABLE tt_esp_pend_lote_ap.
                RUN pi-carrega-browse-apb (1).
                {&OPEN-QUERY-brw-doc-apb-p}
            END.
            ELSE
            DO:
                brw-doc-apb-p:HIDDEN = YES.
                brw-doc-apb-a:HIDDEN = YES.
                brw-doc-apb-r:HIDDEN = YES.
                brw-doc-cmg-p:HIDDEN = NO.
                brw-doc-cmg-a:HIDDEN = YES.
                brw-doc-cmg-r:HIDDEN = YES.
                EMPTY TEMP-TABLE tt_esp_pend_movto_cta_corren.
                RUN pi-carrega-browse-cmg (1).
                {&OPEN-QUERY-brw-doc-cmg-p}
            END.
            ASSIGN bt-aprovar:SENSITIVE = YES
                   bt-reprovar:SENSITIVE = YES.
        END.
        WHEN 2 THEN
        DO:
            IF INPUT tp-modulo = 1 THEN
            DO:
                brw-doc-apb-p:HIDDEN = YES.
                brw-doc-apb-a:HIDDEN = NO.
                brw-doc-apb-r:HIDDEN = YES.
                brw-doc-cmg-p:HIDDEN = YES.
                brw-doc-cmg-a:HIDDEN = YES.
                brw-doc-cmg-r:HIDDEN = YES.
                EMPTY TEMP-TABLE tt_esp_pend_lote_ap.
                RUN pi-carrega-browse-apb (2).
                {&OPEN-QUERY-brw-doc-apb-a}
            END.
            ELSE
            DO:
                brw-doc-apb-p:HIDDEN = YES.
                brw-doc-apb-a:HIDDEN = YES.
                brw-doc-apb-r:HIDDEN = YES.
                brw-doc-cmg-p:HIDDEN = YES.
                brw-doc-cmg-a:HIDDEN = NO.
                brw-doc-cmg-r:HIDDEN = YES.
                EMPTY TEMP-TABLE tt_esp_pend_movto_cta_corren.
                RUN pi-carrega-browse-cmg (2).
                {&OPEN-QUERY-brw-doc-cmg-a}
            END.
            ASSIGN bt-aprovar:SENSITIVE = NO
                   bt-reprovar:SENSITIVE = NO.
        END.
        WHEN 3 THEN
        DO:
            IF INPUT tp-modulo = 1 THEN
            DO:
                brw-doc-apb-p:HIDDEN = YES.
                brw-doc-apb-a:HIDDEN = YES.
                brw-doc-apb-r:HIDDEN = NO.
                brw-doc-cmg-p:HIDDEN = YES.
                brw-doc-cmg-a:HIDDEN = YES.
                brw-doc-cmg-r:HIDDEN = YES.
                EMPTY TEMP-TABLE tt_esp_pend_lote_ap.
                RUN pi-carrega-browse-apb (3).
                {&OPEN-QUERY-brw-doc-apb-r}
            END.
            ELSE
            DO:
                brw-doc-apb-p:HIDDEN = YES.
                brw-doc-apb-a:HIDDEN = YES.
                brw-doc-apb-r:HIDDEN = YES.
                brw-doc-cmg-p:HIDDEN = YES.
                brw-doc-cmg-a:HIDDEN = YES.
                brw-doc-cmg-r:HIDDEN = NO.
                EMPTY TEMP-TABLE tt_esp_pend_movto_cta_corren.
                RUN pi-carrega-browse-cmg (3).
                {&OPEN-QUERY-brw-doc-cmg-r}
            END.
            ASSIGN bt-aprovar:SENSITIVE = NO
                   bt-reprovar:SENSITIVE = NO.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brw-doc-apb-a
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
       RUN set-position IN h_p-exihel ( 1.38 , 81.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             tp-modulo:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY tp-modulo tp-movto c-cod_usuario c-nom_usuario 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-25 RECT-26 rt-button-2 tp-modulo tp-movto c-cod_usuario 
         c-nom_usuario brw-doc-cmg-r brw-doc-cmg-p brw-doc-cmg-a brw-doc-apb-r 
         brw-doc-apb-p brw-doc-apb-a bt-reprovar bt-aprovar 
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
    
    {utp/ut9000.i "apb001-w02" "12.1.19.001"}
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    FIND esp_aprovador WHERE
         esp_aprovador.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
    IF AVAIL esp_aprovador 
        AND TODAY >= esp_aprovador.data_vigen_ini
        AND TODAY <= esp_aprovador.data_vigen_fim
        AND (esp_aprovador.aprov_contas_pagar OR esp_aprovador.aprov_caixa_bancos) THEN
    DO:
        FIND usuar_mestre WHERE
             usuar_mestre.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
        ASSIGN 
            c-cod_usuario:SCREEN-VALUE = c-seg-usuario
            c-nom_usuario:SCREEN-VALUE = usuar_mestre.nom_usuario.
    END.
    ELSE
    DO:
        RUN utp/ut-msgs.p ("show",17006,"Aprovador Inv†lido~~O usu†rio " + c-seg-usuario + " n∆o Ç aprovador ou est† fora do per°odo de aprovaá∆o." + CHR(13) + CHR(13) +
                                        "Para ter acesso ao cadastro de aprovaá∆o de documentos, o usuario " +
                                        "deve estar cadastrado no programa de aprovadores com per°odo v†lido e com acesso ao moduloo APB e/ou CMG.").
        APPLY "CLOSE" TO THIS-PROCEDURE.
    END.

    IF esp_aprovador.aprov_contas_pagar THEN 
        ASSIGN tp-modulo:SCREEN-VALUE = "1".
    ELSE
        ASSIGN tp-modulo:SCREEN-VALUE = "2".

    APPLY "VALUE-CHANGED" TO tp-movto.

    run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-aprova-tit w-livre 
PROCEDURE pi-aprova-tit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-assunto AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dt-aprov-atu AS DATETIME  NO-UNDO.
FOR EACH tt-tit:

    dt-aprov-atu = tt-tit.dt_aprovacao1.

    IF tt-tit.dt_aprovacao1 = ? THEN 
    DO:
        /*primeira aprovaá∆o*/
        {prgfin\apb\apb001-i03.i}
        IF esp_aprovador.nivel_aprovador <> 1 THEN
        DO:
            RUN utp/ut-msgs.p ("show",17006,"Apresentou Erro na Aprovaá∆o~~Somente aprovador de N°vel 1 pode fazer a primeira a provaá∆o do(s) t°tulo(s).").
            RETURN NO-APPLY.
        END.
        
        FOR EACH esp_pend_lote_ap
           WHERE esp_pend_lote_ap.cod_estab = tt-tit.cod_estab
             AND esp_pend_lote_ap.cod_refer = tt-tit.cod_refer EXCLUSIVE-LOCK:

            ASSIGN esp_pend_lote_ap.dt_aprovacao1 = NOW
                   esp_pend_lote_ap.cod_usuario_aprov1 = c-seg-usuario.

            FIND item_lote_impl_ap OF esp_pend_lote_ap NO-LOCK NO-ERROR.
            FIND ems5.fornecedor OF item_lote_impl_ap NO-LOCK NO-ERROR.

            c-mess = c-mess + "<tr>"
                + "<td>" + item_lote_impl_ap.cod_empresa +                                           "</td>"
                + "<td>" + item_lote_impl_ap.cod_estab +                                             "</td>"
                + "<td>" + STRING(item_lote_impl_ap.cdn_fornecedor) + " - " + ems5.fornecedor.nom_abrev + "</td>"
                + "<td>" + item_lote_impl_ap.cod_tit_ap +                                            "</td>"
                + "<td>" + item_lote_impl_ap.cod_ser_docto +                                         "</td>"
                + "<td>" + item_lote_impl_ap.cod_espec_docto +                                       "</td>"
                + "<td>" + item_lote_impl_ap.cod_parcela +                                           "</td>"
                + "<td>" + STRING(item_lote_impl_ap.val_tit_ap, "->>>>,>>>,>>9.99") +                "</td>"
                + "<td>" + STRING(item_lote_impl_ap.dat_emis_docto, "99/99/9999") +                  "</td>"
                + "<td>" + STRING(item_lote_impl_ap.dat_vencto_tit_ap, "99/99/9999") +               "</td>"
                + "<td>" + item_lote_impl_ap.des_text_histor +                                       "</td>"
                + "<td>" + esp_pend_lote_ap.cod_usuario +                                            "</td>"
                + "<td>" + STRING(esp_pend_lote_ap.dt_digitacao, "99/99/9999 HH:MM") +                     "</td>"
                + "</tr>".
        END.
        ASSIGN c-assunto = "Pendància de T°tulos para Aprovaá∆o".
        FOR EACH b_esp_aprovador NO-LOCK
           WHERE b_esp_aprovador.aprov_contas_pagar
             AND b_esp_aprovador.nivel_aprovador = 2:

            /*manda e-mail para aprovador de nivel 2*/
            RUN pi-envia-email (INPUT TRIM(b_esp_aprovador.email),
                                INPUT c-assunto,
                                INPUT c-mess-top + c-mess + c-mess-base,
                                INPUT "APB").
        END.
    END.
    IF tt-tit.dt_aprovacao2 = ? AND dt-aprov-atu <> ? THEN 
    DO:
        /*segunda aprovaá∆o*/
        {prgfin\apb\apb001-i05.i}
        IF esp_aprovador.nivel_aprovador <> 2 THEN
        DO:
            RUN utp/ut-msgs.p ("show",17006,"Apresentou Erro na Atualizaá∆o~~Somente aprovador de N°vel 2 pode fazer a segunda a provaá∆o e a atualizaá∆o do(s) t°tulo(s).").
            RETURN NO-APPLY.
        END.

        FIND FIRST lote_impl_tit_ap NO-LOCK 
             WHERE lote_impl_tit_ap.cod_estab = tt-tit.cod_estab
               AND lote_impl_tit_ap.cod_refer = tt-tit.cod_refer NO-ERROR.
        IF AVAIL lote_impl_tit_ap THEN
            ASSIGN v_rec_lote_impl_tit_ap = RECID(lote_impl_tit_ap).
        ELSE
        DO:
            ASSIGN v_rec_lote_impl_tit_ap = ?.
            RETURN NO-APPLY.
        END.

        EMPTY TEMP-TABLE tt_esp_hist_aprov_tit.
        c-mess = "".
        FOR EACH esp_pend_lote_ap
           WHERE esp_pend_lote_ap.cod_estab = tt-tit.cod_estab
             AND esp_pend_lote_ap.cod_refer = tt-tit.cod_refer NO-LOCK:
    
            FIND item_lote_impl_ap OF esp_pend_lote_ap NO-LOCK NO-ERROR.
            CREATE tt_esp_hist_aprov_tit.
            ASSIGN
                tt_esp_hist_aprov_tit.cod_estab          = esp_pend_lote_ap.cod_estab    
                tt_esp_hist_aprov_tit.cod_refer          = esp_pend_lote_ap.cod_refer    
                tt_esp_hist_aprov_tit.num_seq_refer      = esp_pend_lote_ap.num_seq_refer
                tt_esp_hist_aprov_tit.dt_aprovacao1      = esp_pend_lote_ap.dt_aprovacao1     
                tt_esp_hist_aprov_tit.cod_usuario_aprov1 = esp_pend_lote_ap.cod_usuario_aprov1
                tt_esp_hist_aprov_tit.dt_aprovacao2      = NOW          
                tt_esp_hist_aprov_tit.cod_usuario_aprov2 = c-seg-usuario
                tt_esp_hist_aprov_tit.cod_usuario_reprov = esp_pend_lote_ap.cod_usuario_reprov
                tt_esp_hist_aprov_tit.dt_reprovacao      = esp_pend_lote_ap.dt_reprovacao     
                tt_esp_hist_aprov_tit.mot-reprov         = esp_pend_lote_ap.mot-reprov        
                tt_esp_hist_aprov_tit.cod_usuario        = esp_pend_lote_ap.cod_usuario       
                tt_esp_hist_aprov_tit.dt_digitacao       = esp_pend_lote_ap.dt_digitacao.
    
            FIND ems5.fornecedor OF item_lote_impl_ap NO-LOCK NO-ERROR.
            IF AVAIL item_lote_impl_ap THEN
            ASSIGN
                tt_esp_hist_aprov_tit.cdn_fornecedor    = item_lote_impl_ap.cdn_fornecedor   
                tt_esp_hist_aprov_tit.cod_espec_docto   = item_lote_impl_ap.cod_espec_docto  
                tt_esp_hist_aprov_tit.cod_ser_docto     = item_lote_impl_ap.cod_ser_docto    
                tt_esp_hist_aprov_tit.cod_tit_ap        = item_lote_impl_ap.cod_tit_ap       
                tt_esp_hist_aprov_tit.cod_parcela       = item_lote_impl_ap.cod_parcela      
                tt_esp_hist_aprov_tit.dat_emis_docto    = item_lote_impl_ap.dat_emis_docto   
                tt_esp_hist_aprov_tit.dat_vencto_tit_ap = item_lote_impl_ap.dat_vencto_tit_ap
                tt_esp_hist_aprov_tit.val_tit_ap        = item_lote_impl_ap.val_tit_ap       
                tt_esp_hist_aprov_tit.des_text_histor   = item_lote_impl_ap.des_text_histor. 
    
            c-mess = c-mess + "<tr>"
                + "<td>" + item_lote_impl_ap.cod_empresa +                                           "</td>"
                + "<td>" + item_lote_impl_ap.cod_estab +                                             "</td>"
                + "<td>" + STRING(item_lote_impl_ap.cdn_fornecedor) + " - " + ems5.fornecedor.nom_abrev + "</td>"
                + "<td>" + item_lote_impl_ap.cod_tit_ap +                                            "</td>"
                + "<td>" + item_lote_impl_ap.cod_ser_docto +                                         "</td>"
                + "<td>" + item_lote_impl_ap.cod_espec_docto +                                       "</td>"
                + "<td>" + item_lote_impl_ap.cod_parcela +                                           "</td>"
                + "<td>" + STRING(item_lote_impl_ap.val_tit_ap, "->>>>,>>>,>>9.99") +                "</td>"
                + "<td>" + STRING(item_lote_impl_ap.dat_emis_docto, "99/99/9999") +                  "</td>"
                + "<td>" + STRING(item_lote_impl_ap.dat_vencto_tit_ap, "99/99/9999") +               "</td>"
                + "<td>" + item_lote_impl_ap.des_text_histor +                                       "</td>"
                + "<td>" + esp_pend_lote_ap.cod_usuario +                                            "</td>"
                + "<td>" + STRING(esp_pend_lote_ap.dt_digitacao, "99/99/9999 HH:MM") +                     "</td>"
                + "</tr>".
        END.

        l-prg-aprov-movto = YES.
        RUN prgfin/apb/apb739za.py (Input "On-Line").
        l-prg-aprov-movto = NO.

        IF RETURN-VALUE = "OK" THEN DO:
            FOR EACH tt_esp_hist_aprov_tit:
                FIND esp_hist_aprov_tit OF tt_esp_hist_aprov_tit NO-LOCK NO-ERROR.
                IF NOT AVAIL esp_hist_aprov_tit THEN
                DO:
                    CREATE esp_hist_aprov_tit.
                    BUFFER-COPY tt_esp_hist_aprov_tit TO esp_hist_aprov_tit NO-ERROR.
                END.
            END.
            FOR EACH esp_pend_lote_ap
               WHERE esp_pend_lote_ap.cod_estab = tt-tit.cod_estab
                 AND esp_pend_lote_ap.cod_refer = tt-tit.cod_refer EXCLUSIVE-LOCK:

                DELETE esp_pend_lote_ap.
            END.
            ASSIGN c-assunto = "Aprovaá∆o de T°tulo no Contas a Pagar".

            /*manda e-mail para o usuario que cadastrou o titulo*/
            RUN pi-envia-email (INPUT TRIM(tt-tit.cod_usuario) + "@yamana.com",
                                INPUT c-assunto,
                                INPUT c-mess-top + c-mess + c-mess-base,
                                INPUT "APB").
        END.
        ELSE
        DO:
            RUN utp/ut-msgs.p ("show",17006,"Apresentou Erro na Atualizaá∆o~~Apresentou o erro " + RETURN-VALUE + " na atualizaá∆o do(s) t°tulo(s).").
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-browse-apb w-livre 
PROCEDURE pi-carrega-browse-apb :
DEFINE INPUT PARAMETER p-tp-movto AS INTEGER.

CASE p-tp-movto:
    WHEN 1 THEN
        FOR EACH esp_pend_lote_ap NO-LOCK
           WHERE esp_pend_lote_ap.dt_reprovacao = ?
             AND (esp_pend_lote_ap.dt_aprovacao1 = ?
              OR esp_pend_lote_ap.dt_aprovacao2 = ?):
            {prgfin\apb\apb001-i01.i}
        END.
    WHEN 2 THEN
        FOR EACH esp_pend_lote_ap NO-LOCK
           WHERE esp_pend_lote_ap.dt_aprovacao1 <> ?
             AND esp_pend_lote_ap.dt_aprovacao2 <> ?:
            {prgfin\apb\apb001-i01.i}
        END.
    WHEN 3 THEN
        FOR EACH esp_pend_lote_ap
           WHERE esp_pend_lote_ap.dt_reprovacao <> ?:
            {prgfin\apb\apb001-i01.i}
        END.
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-browse-cmg w-livre 
PROCEDURE pi-carrega-browse-cmg :
DEFINE INPUT PARAMETER p-tp-movto AS INTEGER.

CASE p-tp-movto:
    WHEN 1 THEN
        FOR EACH esp_pend_movto_cta_corren
           WHERE esp_pend_movto_cta_corren.dt_reprovacao = ?
             AND (esp_pend_movto_cta_corren.dt_aprovacao1 = ?
              OR esp_pend_movto_cta_corren.dt_aprovacao2 = ?):
            {prgfin\apb\apb001-i02.i}
        END.
    WHEN 2 THEN
        FOR EACH esp_pend_movto_cta_corren
           WHERE esp_pend_movto_cta_corren.dt_aprovacao1 <> ?
             AND esp_pend_movto_cta_corren.dt_aprovacao2 <> ?:
            {prgfin\apb\apb001-i02.i}
        END.
    WHEN 3 THEN
    DO:
        FOR EACH esp_pend_movto_cta_corren
           WHERE esp_pend_movto_cta_corren.dt_reprovacao <> ?:
            {prgfin\apb\apb001-i02.i}
        END.
    END.
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-envia-email w-livre 
PROCEDURE pi-envia-email :
DEFINE INPUT PARAMETER p-dest      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p-assunto   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p-mess      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p-modulo    AS CHARACTER NO-UNDO.

    RUN utp/utapi019.p PERSISTENT SET h-utapi019.
    FIND FIRST param_email NO-LOCK NO-ERROR.
    
    EMPTY TEMP-TABLE tt-envio2.
    EMPTY TEMP-TABLE tt-mensagem.
    EMPTY TEMP-TABLE tt-erros.

    CREATE tt-envio2.
    ASSIGN tt-envio2.versao-integracao = 1
           tt-envio2.exchange          = param_email.log_servid_exchange
           tt-envio2.servidor          = param_email.cod_servid_e_mail
           tt-envio2.porta             = param_email.num_porta
           tt-envio2.destino           = p-dest
           tt-envio2.assunto           = p-assunto
           tt-envio2.remetente         = "SustencaoYamana@yamana.com"
           tt-envio2.copia             = ""
           tt-envio2.importancia       = 1
           tt-envio2.log-enviada       = NO
           tt-envio2.log-lida          = NO
           tt-envio2.acomp             = NO
           tt-envio2.formato           = "html".

    CREATE tt-mensagem.                                       
    ASSIGN tt-mensagem.seq-mensagem = 1                         
           tt-mensagem.mensagem     = p-mess. 

    RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                   INPUT  TABLE tt-mensagem,
                                   OUTPUT TABLE tt-erros).
    IF TEMP-TABLE tt-erros:HAS-RECORDS THEN
    DO:
        OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "erro-email-" + p-modulo + "-" + STRING(TODAY, "99999999") + "-" + STRING(ETIME, "99999999") + ".txt").
        FOR EACH tt-erros:
            DISP tt-erros WITH SCROLLABLE.
        END.
        OUTPUT CLOSE.
    END.

DELETE PROCEDURE h-utapi019.

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
  {src/adm/template/snd-list.i "esp_hist_aprov_movto_cc"}
  {src/adm/template/snd-list.i "tt_esp_pend_movto_cta_corren"}
  {src/adm/template/snd-list.i "esp_hist_aprov_tit"}
  {src/adm/template/snd-list.i "tt_esp_pend_lote_ap"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
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

