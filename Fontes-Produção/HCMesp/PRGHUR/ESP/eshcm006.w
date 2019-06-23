&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var v_cdn_avpes_padr_relat       like avpes_reg_mestre.cdn_avpes_padr           no-undo.
def var v_cdn_avpes_layout_relat     like avpes_layout_relat.cdn_avpes_layout_relat no-undo.
def var v_des_marcar                 as char label "X"                no-undo.
def var v_des_marcar_nao             as char label "X"                no-undo.
def var v_log_sim                    as char label "X"                no-undo.
def var v_log_nao                    as char label "X"                no-undo.
def var v_val_escala                 as integer                                     no-undo.
def var v_num_intervalo              as int  format ">>9"                           no-undo.
def var v_cont                       as int                                         no-undo.
def var v_num_coluna                 as int                                         no-undo.
def var v_des_proficiencia           as char format "x(20)"                         no-undo.
def var v_des_superior               as char format "x(80)"                         no-undo.
def var v_des_inferior               as char format "x(81)"                         no-undo.
def var v_des_valor                  as char format "x(50)"                         no-undo.
def var v_valor_escala               as integer                                     no-undo.
def var v_dat_referencia             as date format "99/99/9999"                    no-undo.
def var v_dat_final                  as date format "99/99/9999"                    no-undo.
def var v_dat_inicial                as date format "99/99/9999"                    no-undo.
def var v_sal_func                   like histor_sal_func.val_salario_hora          no-undo.
def var v_cdn_estab                  like funcionario.cdn_estab                     no-undo.
def var v_cdn_funcionario            like funcionario.cdn_funcionario               no-undo.
def var v_cdn_cargo                  like cargo.cdn_cargo_basic                     no-undo.
def var v_cdn_tip_cargo              like cargo_basic.cdn_tip_cargo                 no-undo.
def var v_des_tip_cargo              as char format "x(40)"                         no-undo.
def var v_cdn_niv_cargo              like cargo.cdn_niv_cargo                       no-undo.
def var v_cdn_funcionario_comp       like funcionario.cdn_funcionario               no-undo.
def var v_num_pessoa_fisic           like funcionario.num_pessoa_fisic              no-undo.
def var v_num_pessoa_fisic_bolsista  like funcionario.num_pessoa_fisic no-undo.
def var v_num_pessoa_fisic_treindo like funcionario.num_pessoa_fisic no-undo.
def var v_num_pessoa_fisic_avaldor   like funcionario.num_pessoa_fisic              no-undo.
def var v_num_pessoa_fisic_comp      like funcionario.num_pessoa_fisic              no-undo.


def var v_des_curso_trein            like curso_trein.des_curso_trein               no-undo.
def var v_des_idi_sit_neces_trein_func as char format "x(15)"                       no-undo.
def var v_lista_situacao             as char                                        no-undo.
def var v_lista_tipo_nivel           as char                                        no-undo.
def var v_des_tipo_nivel             as char format "x(15)"                         no-undo.
def var v_des_sit_objorg             as char format "x(15)"                         no-undo.
def var v_lista_sit_objorg           as char                                        no-undo.
def var v_des_tip_objorg             as char format "x(15)"                         no-undo.
def var v_lista_tip_objorg           as char                                        no-undo.
def var v_lista_sit_trein            as char                                        no-undo.
def var v_des_idi_sit_trein          as char format "x(15)"                         no-undo.
def var v_log_avaliou                as logical                                     no-undo.
def var v_log_auto_aval              as logical                                     no-undo.
def var v_nom_cargo_empres_ant       as char format "x(25)"                         no-undo.
def var v_num_seq1                    as int                                        no-undo.

DEF VAR v_num_avpes_emitid LIKE  avpes_emitid.num_avpes_emitid.
DEF VAR v_cdn_campo_impres LIKE  avpes_anexo.cdn_campo_impres_avpes. 
DEF VAR v_dat_refer1       LIKE  avpes_emitid.dat_refer_respos_avpes    .
DEF VAR v_val_respos       LIKE  respos_avpes_func.val_respos_efetd_func_avpes.


DEF TEMP-TABLE tt-imprime
    FIELD tt-cdn-empresa   LIKE  funcionario.cdn_empresa                             
    FIELD tt-cdn-estab     LIKE  funcionario.cdn_estab.    

def temp-table tt-dados  no-undo
  field cdn_empresa                 like funcionario.cdn_empresa 
  field cdn_estab                   like funcionario.cdn_estab
  field cdn_funcionario             like funcionario.cdn_funcionario
  field dat_admis_func              like funcionario.dat_admis_func
  field cod_rh_ccusto               like funcionario.cod_rh_ccusto
  field cod_unid_lotac              like funcionario.cod_unid_lotac 
  FIELD des_unid_lotac              LIKE unid_lotac.des_unid_lotac
  field nom_pessoa_fisic              as char format "x(23)" 
  field cdn_cargo_basic               as integer format ">>>>9"  label "      "
  field des_cargo_basic               as char format "x(20)"
  field cdn_niv_cargo               like funcionario.cdn_niv_cargo label "   "
  field cdn_estab_avaldor           like avpes_emitid.cdn_estab_func_respos_avpes
  field cdn_funcionario_avaldor     like avpes_emitid.cdn_func_respos_avpes
  field num_pessoa_avaldor          like avpes_emitid.num_pessoa_fisic_avaldor
  field dat_refer_respos              as date format "99/99/9999"
  field val_pontuac_avpes             as decimal format ">>9.99" 
  field des_restdo                    as char format "x(10)" 
  FIELD tipo-aval                     AS INT /* 1- Grupo de Assunto, 2- Pergunta */  
  FIELD cdn_grp_avpes               LIKE avpes_mestre_grp.cdn_grp_avpes
  FIELD des_grp_avpes               LIKE avpes_mestre_grp.des_grp_avpes
  FIELD des_impres_item_avpes       LIKE avpes_item.des_impres_item_avpes
  FIELD val_pontuac_item_avpes      LIKE respos_avpes_func.val_pontuac_item_avpes /*val_respos_efetd_func_avpes*/
  FIELD cdn_item_avpes              LIKE avpes_item.cdn_item_avpes
  FIELD des_impres_sub_item_avpes   LIKE avpes_sub_item.des_impres_sub_item_avpes 
  FIELD cdn_sub_item_avpes          LIKE avpes_sub_item.cdn_sub_item_avpes 
  FIELD num_avpes_emitid            LIKE respos_avpes_func.num_avpes_emitid  
  FIELD c-status                      AS c
  index i-pergunta cdn_empresa
                   cdn_estab
                   cod_rh_ccusto 
                   cdn_item_avpes.

def temp-table tt-elimina   no-undo
  field cdn_empresa                 like funcionario.cdn_empresa 
  field cdn_estab                   like funcionario.cdn_estab
  /*FIELD cdn_grp_avpes               LIKE avpes_mestre_grp.cdn_grp_avpes
  FIELD cdn_item_avpes              LIKE avpes_item.cdn_item_avpes
  FIELD cdn_sub_item_avpes          LIKE avpes_sub_item.cdn_sub_item_avpes */
  FIELD num_avpes_emitid            LIKE respos_avpes_func.num_avpes_emitid  
  FIELD c-status                      AS c
  INDEX i-elimina num_avpes_emitid
                  cdn_empresa     
                  cdn_estab.

DEF TEMP-TABLE tt-des_grp_avpes  no-undo
    FIELD  des_grp_avpes LIKE tt-dados.des_grp_avpes
    FIELD  media         AS   DEC FORMAT ">>>>>>>>9.99".
                   
DEF TEMP-TABLE  tt-des_impres_item_avpes  no-undo
    FIELD des_impres_item_avpes LIKE tt-dados.des_impres_item_avpes
    FIELD media-item            AS   DEC FORMAT ">>>>>>>>9.99"
    FIELD cdn_item_avpes        LIKE avpes_item.cdn_item_avpes.
                   
DEF VAR v_nom_orig_inform AS CHAR.
                   
/* Variaveis para Gerar em Excel */
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
def var chWorkbook as com-handle no-undo.
def var chWorksheet as com-handle no-undo.

DEF VAR i-linha AS INT.

DEF VAR i-nr-grupo     AS INT  format ">>>>>>>>>>>>9"  .
DEF VAR i-nr-pergunta  AS INT  format ">>>>>>>>>>>>9"  .
DEF VAR tot-grupo      AS INT  format ">>>>>>>>>>>>9"  .
DEF VAR tot-pergunta   AS INT  format ">>>>>>>>>>>>9.99"  .
DEF VAR tot-ger-grupo  AS INT  format ">>>>>>>>>>>>9.99"  .
DEF VAR tot-nr-grupo   AS INT  format ">>>>>>>>>>>>9"  .

DEF VAR tot-empresa      AS dec  format ">>>>>>>>>>>>9.99"  .  
DEF VAR i-nr-tot-empresa AS INT  format ">>>>>>>>>>>>9"  .

DEF VAR i-tt-des-gr AS INT FORMAT ">>>>>>>>9".
DEF VAR i-media     AS INT FORMAT ">>>>>>>>9".

DEF VAR tot-ger-emp  AS dec  format ">>>>>>>>>>>>9.99" . 
DEF VAR tot-nr-emp   AS INT  FORMAT ">>>>>>>>9".

DEF VAR d-tt-perc    AS DEC  format ">>>>>>>>>>>>9.99".
DEF VAR i-tot-perc   AS dec  format ">>>>>>>>>>>>9.99" .
DEF VAR tot-nr-pesq  AS INT  FORMAT ">>>>>>>>9".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BtnOK BtnCancel RECT-20 RECT-32 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-area-ini 
       MENU-ITEM m_1_-_Mina     LABEL "1 - Mina"      
       MENU-ITEM m_2_-_ADM      LABEL "2 - Beneficiamento / Planta"
       MENU-ITEM m_3_-_Laboratrio LABEL "3 - Geologia / Planejamento"
       MENU-ITEM m_4_-_Servios_Tcnicos LABEL "4 - Manutená∆o"
       MENU-ITEM m_5_-_Financeiro LABEL "5 - Laborat¢rio"
       MENU-ITEM m_6_-_Manuteno LABEL "6 - Serviáos TÇcnicos"
       MENU-ITEM m_7_-_Contr    LABEL "7 - Controladoria"
       MENU-ITEM m_8_-_Projeto_Explorao LABEL "8 - SSMA"      
       MENU-ITEM m_9_-_Projeto_Expanso LABEL "9 - Administrativa"
       MENU-ITEM m_10_-_Expl    LABEL "10 - Projeto Exploraá∆o"
       MENU-ITEM m_11_-_Geologia LABEL "11 - Projeto Expans∆o"
       MENU-ITEM m_12_-_Benef   LABEL "12 - Operaá‰es / Serv. Tec. (corporativo)"
       MENU-ITEM m_13_-_Operaes LABEL "13 - Exploraá∆o (corporativo)"
       MENU-ITEM m_14_-         LABEL "14 - Financeiro / Jur°dico (corporativo)"
       MENU-ITEM m_15_-_SSMA    LABEL "15 - NRA (nenhuma resposta anterior)".

DEFINE MENU POPUP-MENU-categoria-ini 
       MENU-ITEM m_1_-_Operacional LABEL "1 - Operacional"
       MENU-ITEM m_2_-_Gestor__Lider LABEL "2 - Apoio / Administrativa"
       MENU-ITEM m_3_-_Apoio__Administrativo LABEL "3 - TÇcnica"   
       MENU-ITEM m_4_-_Es       LABEL "4 - Especialista (Analistas/Engenheiros/Ge¢logos)"
       MENU-ITEM m_5_-_Tcnica   LABEL "5 - Gestor / L°der".

DEFINE MENU POPUP-MENU-categoria-ini-2 
       MENU-ITEM m_1_-_Operacional-2 LABEL "1 - Menos que Ensino Fundamental"
       MENU-ITEM m_2_-_Gestor__Lider-2 LABEL "2 - Ensino Fundamental"
       MENU-ITEM m_3_-_Apoio__Administrativo-2 LABEL "3 - Ensino MÇdio"
       MENU-ITEM m_4_-_Especialista-2 LABEL "4 - TÇcnico"   
       MENU-ITEM m_5_-_Tcnica-2 LABEL "5 - Ensino Superior"
       MENU-ITEM m_6_-_Ps_Graduao__Especialiao LABEL "6 - P¢s Graduaá∆o e / ou Especializaá∆o".

DEFINE MENU POPUP-MENU-categoria-ini-3 
       MENU-ITEM m_1_-_Operacional-3 LABEL "1 - Operacional"
       MENU-ITEM m_2_-_Gestor__Lider-3 LABEL "2 - Gestor / Lider"
       MENU-ITEM m_3_-_Apoio__Administrativo-3 LABEL "3 - Apoio / Administrativo"
       MENU-ITEM m_4_-_Especialista-3 LABEL "4 - Especialista"
       MENU-ITEM m_5_-_Tcnica-3 LABEL "5 - TÇcnica"   .

DEFINE MENU POPUP-MENU-categoria-ini-4 
       MENU-ITEM m_1_-_Operacional-4 LABEL "1 - (menos de 1 ano)"
       MENU-ITEM m_2_-_Gestor__Lider-4 LABEL "2 - (1 a 2 anos)"
       MENU-ITEM m_3_-_Apoio__Administrativo-4 LABEL "3 - (2 a 3 anos)"
       MENU-ITEM m_4_-_Especialista-4 LABEL "4 - (acima de 3 anos)".

DEFINE MENU POPUP-MENU-categoria-ini-5 
       MENU-ITEM m_1_-_Operacional-5 LABEL "1 - Operacional"
       MENU-ITEM m_2_-_Gestor__Lider-5 LABEL "2 - Gestor / Lider"
       MENU-ITEM m_3_-_Apoio__Administrativo-5 LABEL "3 - Apoio / Administrativo"
       MENU-ITEM m_4_-_Especialista-5 LABEL "4 - Especialista"
       MENU-ITEM m_5_-_Tcnica-5 LABEL "5 - TÇcnica"   .

DEFINE MENU POPUP-MENU-categoria-ini-6 
       MENU-ITEM m_1_-_at_20_anos LABEL "1 - (atÇ 20 anos)"
       MENU-ITEM m_2_-_20_-_25_anos LABEL "2 - (21 - 25 anos)"
       MENU-ITEM m_3_-_25_-_30_anos LABEL "3 - (26 - 30 anos)"
       MENU-ITEM m_4_-_30_-_40_anos LABEL "4 - (31 - 40 anos)"
       MENU-ITEM m_5_-_40_a_50_anos LABEL "5 - (41 - 50 anos)"
       MENU-ITEM m_6_-_acima_de_50_anos LABEL "6 - (acima de 51 anos)".

DEFINE MENU POPUP-MENU-categoria-ini-7 
       MENU-ITEM m_1_-_Operacional-7 LABEL "1 - Operacional"
       MENU-ITEM m_2_-_Gestor__Lider-7 LABEL "2 - Gestor / Lider"
       MENU-ITEM m_3_-_Apoio__Administrativo-7 LABEL "3 - Apoio / Administrativo"
       MENU-ITEM m_4_-_Especialista-7 LABEL "4 - Especialista"
       MENU-ITEM m_5_-_Tcnica-6 LABEL "5 - TÇcnica"   .

DEFINE MENU POPUP-MENU-categoria-ini-8 
       MENU-ITEM m_1_-_Operacional-8 LABEL "1 - Operacional"
       MENU-ITEM m_2_-_Gestor__Lider-8 LABEL "2 - Gestor / Lider"
       MENU-ITEM m_3_-_Apoio__Administrativo-8 LABEL "3 - Apoio / Administrativo"
       MENU-ITEM m_4_-_Especialista-8 LABEL "4 - Especialista"
       MENU-ITEM m_5_-_Tcnica-7 LABEL "5 - TÇcnica"   .

DEFINE MENU POPUP-MENU-categoria-ini-9 
       MENU-ITEM m_1_-_Feminino LABEL "1 - Feminino"  
       MENU-ITEM m_2_-_Masculino LABEL "2 - Masculino" .

DEFINE MENU POPUP-MENU-unidade-ini 
       MENU-ITEM m_teste        LABEL "1 - MFB"       
       MENU-ITEM m_teste2       LABEL "2 - JMC"       
       MENU-ITEM m_3_-_C1       LABEL "3 - C1"        
       MENU-ITEM m_4_-_Serra_da_Borda LABEL "4 - SBMM"      
       MENU-ITEM m_5_-_ErnestoPau_a_Pique LABEL "5 - Ernesto / Pau a Pique"
       MENU-ITEM m_6_-_Maraca   LABEL "6 - SV"        
       MENU-ITEM m_7_-_Pilar    LABEL "7 - MMIC"      
       MENU-ITEM m_8_-_YDM      LABEL "8 - Pilar"     
       MENU-ITEM m_9_-_SV       LABEL "9 - YDM"       
       MENU-ITEM m_10_-_Outros  LABEL "10 - Outros"   .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Sair" 
     SIZE 13 BY 1.13
     BGCOLOR 8 FONT 1.

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "Imprimir" 
     SIZE 13 BY 1.13
     BGCOLOR 8 FONT 1.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 12.75.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 1.71.

DEFINE VARIABLE area-fim AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE area-ini AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Area" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE categoria-fim AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE categoria-ini AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Categoria Profissional" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE faixa-fim AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE faixa-ini AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Faixa Et†ria" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-fim AS DATE FORMAT "99/99/9999":U INITIAL 11/30/07 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-ini AS DATE FORMAT "99/99/9999":U INITIAL 11/12/07 
     LABEL "Periodo da Avaliaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa AS CHARACTER FORMAT "x(3)":U INITIAL "" 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-estab-fim AS CHARACTER FORMAT "x(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-estab-ini AS CHARACTER FORMAT "x(5)":U INITIAL "" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-texto AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 58 BY .67 NO-UNDO.

DEFINE VARIABLE formacao-fim AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE formacao-ini AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Formaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE sexo-fim AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE sexo-ini AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Sexo" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE tempo-fim AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE tempo-ini AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Tempo de Trabalho" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE unidade-fim AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE unidade-ini AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Unidade" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_avpes_padr_ini AS INTEGER FORMAT ">>>,>>9" INITIAL 8 
     LABEL "C¢digo Avaliaá∆o":R20 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-33
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-34
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-71
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-72
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-tipo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Grupo de Assunto", 1,
"Pergunta", 2
     SIZE 33 BY .75 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BtnOK AT ROW 14 COL 23
     BtnCancel AT ROW 14 COL 42
     RECT-20 AT ROW 1 COL 1
     RECT-32 AT ROW 13.75 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72.43 BY 14.46
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel.

DEFINE FRAME f-pg-sel
     fi-empresa AT ROW 1.25 COL 16 COLON-ALIGNED
     fi-estab-ini AT ROW 2.25 COL 16 COLON-ALIGNED
     fi-estab-fim AT ROW 2.25 COL 45 COLON-ALIGNED NO-LABEL
     unidade-ini AT ROW 3.25 COL 16 COLON-ALIGNED
     unidade-fim AT ROW 3.25 COL 45 COLON-ALIGNED NO-LABEL
     area-ini AT ROW 4.25 COL 16 COLON-ALIGNED
     area-fim AT ROW 4.25 COL 45 COLON-ALIGNED NO-LABEL
     categoria-ini AT ROW 5.25 COL 16 COLON-ALIGNED
     categoria-fim AT ROW 5.25 COL 45 COLON-ALIGNED NO-LABEL
     formacao-ini AT ROW 6.25 COL 16 COLON-ALIGNED
     formacao-fim AT ROW 6.25 COL 45 COLON-ALIGNED NO-LABEL
     tempo-ini AT ROW 7.25 COL 16 COLON-ALIGNED
     tempo-fim AT ROW 7.25 COL 45 COLON-ALIGNED NO-LABEL
     faixa-ini AT ROW 8.25 COL 16 COLON-ALIGNED
     faixa-fim AT ROW 8.25 COL 45 COLON-ALIGNED NO-LABEL
     sexo-ini AT ROW 9.25 COL 16 COLON-ALIGNED
     sexo-fim AT ROW 9.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-dt-ini AT ROW 10.25 COL 16 COLON-ALIGNED
     fi-dt-fim AT ROW 10.25 COL 45 COLON-ALIGNED NO-LABEL
     v_cdn_avpes_padr_ini AT ROW 11.25 COL 16 COLON-ALIGNED HELP
          "C¢digo da Avaliaá∆o"
     rs-tipo AT ROW 11.5 COL 34 NO-LABEL
     fi-texto AT ROW 12.5 COL 4 COLON-ALIGNED NO-LABEL
     IMAGE-19 AT ROW 10.25 COL 33
     IMAGE-20 AT ROW 10.25 COL 40
     IMAGE-21 AT ROW 3.25 COL 33
     IMAGE-22 AT ROW 3.25 COL 40
     IMAGE-23 AT ROW 4.25 COL 33
     IMAGE-24 AT ROW 4.25 COL 40
     IMAGE-25 AT ROW 5.25 COL 33
     IMAGE-26 AT ROW 5.25 COL 40
     IMAGE-27 AT ROW 6.25 COL 33
     IMAGE-28 AT ROW 6.25 COL 40
     IMAGE-29 AT ROW 7.25 COL 33
     IMAGE-30 AT ROW 7.25 COL 40
     IMAGE-31 AT ROW 8.25 COL 33
     IMAGE-32 AT ROW 8.25 COL 40
     IMAGE-33 AT ROW 9.25 COL 33
     IMAGE-34 AT ROW 9.25 COL 40
     IMAGE-71 AT ROW 2.25 COL 33
     IMAGE-72 AT ROW 2.25 COL 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1.21
         SIZE 69 BY 12.29
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<Pesquisa de Clima  Organizacional - ESHCM006>"
         HEIGHT             = 14.46
         WIDTH              = 70.86
         MAX-HEIGHT         = 31.29
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 31.29
         VIRTUAL-WIDTH      = 182.86
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME f-pg-sel:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
ASSIGN 
       area-ini:POPUP-MENU IN FRAME f-pg-sel       = MENU POPUP-MENU-area-ini:HANDLE.

ASSIGN 
       categoria-ini:POPUP-MENU IN FRAME f-pg-sel       = MENU POPUP-MENU-categoria-ini:HANDLE.

ASSIGN 
       faixa-fim:POPUP-MENU IN FRAME f-pg-sel       = MENU POPUP-MENU-categoria-ini-7:HANDLE.

ASSIGN 
       faixa-ini:POPUP-MENU IN FRAME f-pg-sel       = MENU POPUP-MENU-categoria-ini-6:HANDLE.

ASSIGN 
       formacao-fim:POPUP-MENU IN FRAME f-pg-sel       = MENU POPUP-MENU-categoria-ini-3:HANDLE.

ASSIGN 
       formacao-ini:POPUP-MENU IN FRAME f-pg-sel       = MENU POPUP-MENU-categoria-ini-2:HANDLE.

ASSIGN 
       sexo-fim:POPUP-MENU IN FRAME f-pg-sel       = MENU POPUP-MENU-categoria-ini-8:HANDLE.

ASSIGN 
       sexo-ini:POPUP-MENU IN FRAME f-pg-sel       = MENU POPUP-MENU-categoria-ini-9:HANDLE.

ASSIGN 
       tempo-fim:POPUP-MENU IN FRAME f-pg-sel       = MENU POPUP-MENU-categoria-ini-5:HANDLE.

ASSIGN 
       tempo-ini:POPUP-MENU IN FRAME f-pg-sel       = MENU POPUP-MENU-categoria-ini-4:HANDLE.

ASSIGN 
       unidade-ini:POPUP-MENU IN FRAME f-pg-sel       = MENU POPUP-MENU-unidade-ini:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Pesquisa de Clima  Organizacional - ESHCM006> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Pesquisa de Clima  Organizacional - ESHCM006> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Sair */
DO:
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* Imprimir */
DO:
 
  RUN pi-executa.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME v_cdn_avpes_padr_ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_avpes_padr_ini C-Win
ON F5 OF v_cdn_avpes_padr_ini IN FRAME f-pg-sel /* C¢digo Avaliaá∆o */
DO:
  /*{include/zoomvar.i &prog-zoom=object/sopm/zoom/z01pm132.w
                     &campo=v_cdn_avpes_padr
                     &campozoom=cdn_avpes_padr
                     &campo2=v_des_avpes_padr
                     &campozoom2=des_avpes_padr}
    */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_avpes_padr_ini C-Win
ON LEAVE OF v_cdn_avpes_padr_ini IN FRAME f-pg-sel /* C¢digo Avaliaá∆o */
DO:
   assign v_nom_orig_inform = "DSP".

   /*FIND    avpes_reg_mestre 
     WHERE avpes_reg_mestre.cdn_avpes_padr = input frame f-pg-sel v_cdn_avpes_padr 
       AND avpes_reg_mestre.nom_orig_inform = "DSP" NO-LOCK NO-ERROR.

   IF AVAIL avpes_reg_mestre THEN
      ASSIGN v_des_avpes_padr:SCREEN-VALUE = avpes_reg_mestre.des_avpes_padr.
      */
   /*
   {include/leave.i &tabela=avpes_reg_mestre
                    &atributo-ref=des_avpes_padr
                    &variavel-ref=v_des_avpes_padr
                    &where="avpes_reg_mestre.cdn_avpes_padr = input frame f-pg-par v_cdn_avpes_padr and
                            avpes_reg_mestre.nom_orig_inform = v_nom_orig_inform"}

     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_avpes_padr_ini C-Win
ON MOUSE-SELECT-DBLCLICK OF v_cdn_avpes_padr_ini IN FRAME f-pg-sel /* C¢digo Avaliaá∆o */
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_avpes_padr_ini C-Win
ON TAB OF v_cdn_avpes_padr_ini IN FRAME f-pg-sel /* C¢digo Avaliaá∆o */
DO:
  apply 'leave' to self.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

  
/*
ASSIGN fi-dt-corte = TODAY.
*/
       

       
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

      


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

APPLY 'entry' TO INPUT fi-empresa.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE BtnOK BtnCancel RECT-20 RECT-32 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY fi-empresa fi-estab-ini fi-estab-fim unidade-ini unidade-fim area-ini 
          area-fim categoria-ini categoria-fim formacao-ini formacao-fim 
          tempo-ini tempo-fim faixa-ini faixa-fim sexo-ini sexo-fim fi-dt-ini 
          fi-dt-fim v_cdn_avpes_padr_ini rs-tipo fi-texto 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE fi-empresa fi-estab-ini fi-estab-fim unidade-ini unidade-fim area-ini 
         area-fim categoria-ini categoria-fim formacao-ini formacao-fim 
         tempo-ini tempo-fim faixa-ini faixa-fim sexo-ini sexo-fim fi-dt-ini 
         fi-dt-fim v_cdn_avpes_padr_ini rs-tipo fi-texto IMAGE-19 IMAGE-20 
         IMAGE-21 IMAGE-22 IMAGE-23 IMAGE-24 IMAGE-25 IMAGE-26 IMAGE-27 
         IMAGE-28 IMAGE-29 IMAGE-30 IMAGE-31 IMAGE-32 IMAGE-33 IMAGE-34 
         IMAGE-71 IMAGE-72 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executa C-Win 
PROCEDURE pi-executa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME f-pg-sel:                                      
   
FOR EACH tt-dados:
    DELETE tt-dados.
END.

FOR EACH tt-elimina:
    DELETE tt-elimina.
END.

ASSIGN fi-empresa             fi-dt-ini        fi-dt-fim              fi-estab-ini
       fi-estab-fim           rs-tipo          v_cdn_avpes_padr_ini   unidade-ini     
       unidade-fim            area-ini         area-fim               categoria-ini
       categoria-fim          tempo-ini        tempo-fim              faixa-ini    
       faixa-fim              sexo-ini         sexo-fim               formacao-ini
       formacao-fim.


        for each avpes_emitid where avpes_emitid.cdn_avpes_padr         = v_cdn_avpes_padr_ini
                              and   avpes_emitid.log_avpes_finaliz
                              and   avpes_emitid.dat_refer_respos_avpes >= fi-dt-ini 
                              and   avpes_emitid.dat_refer_respos_avpes <= fi-dt-fim
                              and   avpes_emitid.cdn_empresa             = fi-empresa 
                              and   avpes_emitid.cdn_estab              >= fi-estab-ini
                              and   avpes_emitid.cdn_estab              <= fi-estab-fim  
                              no-lock:

          assign v_num_avpes_emitid = avpes_emitid.num_avpes_emitid.
          if avpes_emitid.cdn_funcionario <> 0 then do: 
             for each funcionario where
                      funcionario.cdn_empresa        = avpes_emitid.cdn_empresa
                and   funcionario.cdn_estab          = avpes_emitid.cdn_estab
                and   funcionario.cdn_funcionario    = avpes_emitid.cdn_funcionario 
               no-lock
                      break by funcionario.cdn_empresa
                            by funcionario.cdn_estab
                            by funcionario.nom_pessoa_fisic: 
                 
                 run pi-recebe-dados.
             end.
          END.
          ELSE DO:
              RUN pi-recebe-dados.
          END.

        END.  
         
END.

FOR EACH tt-dados
    WHERE tt-dados.cdn_grp_avpes = 55 NO-LOCK:
     /* BY tt-dados.num_avpes_emitid
      BY tt-dados.cdn_grp_avpes  
      BY tt-dados.cdn_item_avpes
      BY tt-dados.cdn_sub_item_avpes: */
    
    /* dados gerais - dados cadastrais */
    /*IF tt-dados.cdn_grp_avpes = 55 /*56*/  THEN DO: */
       
       /* unidade */
       IF cdn_item_avpes = 293 /*296*/ THEN DO:
          IF cdn_sub_item_avpes >= unidade-ini AND 
             cdn_sub_item_avpes <= unidade-fim THEN
             ASSIGN tt-dados.c-status = "sim".
          ELSE 
             ASSIGN tt-dados.c-status = "nao".
       END.

       /* area */
       IF cdn_item_avpes = 294 /*297*/ THEN DO:
          IF cdn_sub_item_avpes >= area-ini AND 
             cdn_sub_item_avpes <= area-fim THEN
             ASSIGN tt-dados.c-status = "sim".
          ELSE 
             ASSIGN tt-dados.c-status = "nao".
       END.

       /* categoria */
       IF cdn_item_avpes = 295 /*298*/ THEN DO:
          IF cdn_sub_item_avpes >= categoria-ini AND 
             cdn_sub_item_avpes <= categoria-fim THEN
             ASSIGN tt-dados.c-status = "sim".
          ELSE 
             ASSIGN tt-dados.c-status = "nao".
       END.

       /* formaá∆o */
       IF cdn_item_avpes = 296 /*299*/ THEN DO:
          IF cdn_sub_item_avpes >= formacao-ini AND 
             cdn_sub_item_avpes <= formacao-fim THEN
             ASSIGN tt-dados.c-status = "sim".
          ELSE 
             ASSIGN tt-dados.c-status = "nao".
       END.

       /* tempo */
       IF cdn_item_avpes = 297 /*300*/ THEN DO:
          IF cdn_sub_item_avpes >= tempo-ini AND 
             cdn_sub_item_avpes <= tempo-fim THEN
             ASSIGN tt-dados.c-status = "sim".
          ELSE 
             ASSIGN tt-dados.c-status = "nao".
       END.

       /* faixa etaria */
       IF cdn_item_avpes = 298 /*301*/ THEN DO:
          IF cdn_sub_item_avpes >= faixa-ini AND 
             cdn_sub_item_avpes <= faixa-fim THEN
             ASSIGN tt-dados.c-status = "sim".
          ELSE 
             ASSIGN tt-dados.c-status = "nao".
       END.

       /* sexo */
       IF cdn_item_avpes = 299 /*302*/ THEN DO:
          IF cdn_sub_item_avpes >= sexo-ini AND 
             cdn_sub_item_avpes <= sexo-fim THEN
             ASSIGN tt-dados.c-status = "sim".
          ELSE 
             ASSIGN tt-dados.c-status = "nao".
       END.

       /* status = "n∆o" p/ itens fora da seleá∆o */
       IF tt-dados.c-status = "nao" THEN DO:
       
          /* cria registro para eliminar pesquisa q n∆o estiver na selecao */
          FIND FIRST tt-elimina 
              WHERE tt-elimina.num_avpes_emitid = tt-dados.num_avpes_emitid
                AND tt-elimina.cdn_empresa      = tt-dados.cdn_empresa
                AND tt-elimina.cdn_estab        = tt-dados.cdn_estab no-error.

          IF NOT AVAIL tt-elimina THEN DO:
             CREATE tt-elimina.
             ASSIGN tt-elimina.num_avpes_emitid = tt-dados.num_avpes_emitid
                    tt-elimina.cdn_empresa      = tt-dados.cdn_empresa  
                    tt-elimina.cdn_estab        = tt-dados.cdn_estab.
          END.

       END.

    /*END. */
    
END.

/*
OUTPUT TO d:\temp\20199917\doctos\dados.txt.
FOR EACH tt-dados
     BY tt-dados.num_avpes_emitid
     BY tt-dados.cdn_grp_avpes  
     BY tt-dados.cdn_item_avpes
     BY tt-dados.cdn_sub_item_avpes:
    DISP 
        tt-dados.num_avpes_emitid 
        tt-dados.cdn_grp_avpes              
        tt-dados.des_grp_avpes              
        tt-dados.cdn_item_avpes 
        tt-dados.cdn_sub_item_avpes
        tt-dados.c-status.
END.
*/

/*
FOR EACH tt-elimina:
    DISP tt-elimina.
END.
*/
  
FOR EACH tt-elimina:
    FOR EACH tt-dados 
       WHERE tt-dados.num_avpes_emitid = tt-elimina.num_avpes_emitid
         AND tt-dados.cdn_empresa      = tt-elimina.cdn_empresa     
         AND tt-dados.cdn_estab        = tt-elimina.cdn_estab:
      DELETE tt-dados.                                     
    END.
    DELETE tt-elimina.
END.

/* elimina registro fora da selecao - grupo - 55  /*56*/ - geral */
FOR EACH tt-dados 
   WHERE  tt-dados.cdn_grp_avpes  = 55 /*56 */ :
   DELETE tt-dados.
END.

DO WITH FRAME f-pg-sel:
   ASSIGN fi-texto:SCREEN-VALUE = "Gerando Excel..." .
END.

 
RUN pi-imprime. 

DO WITH FRAME f-pg-sel:
   ASSIGN fi-texto:SCREEN-VALUE = "Processamento Concluido !!!" .
END.

 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime C-Win 
PROCEDURE pi-imprime :
DO:
 
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE iTotalNumberOfOrders    AS INTEGER.
DEFINE VARIABLE iMonth                  AS INTEGER.
DEFINE VARIABLE dAnnualQuota            AS DECIMAL.
DEFINE VARIABLE dTotalSalesAmount       AS DECIMAL.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 3.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.

DEF VAR COUNT  AS INT.                               

/************************************************************/
/*          Abre planilha Excel                             */
CREATE "Excel.Application" chExcelApplication.
chworkbook  = chexcelapplication:workbooks:add.
chworksheet = chexcelapplication:sheets:item(1).
chExcelApplication:Visible = true.
/************************************************************/

/* nr de pesquisas respondidas */
ASSIGN tot-nr-pesq = 0.
FOR EACH   tt-dados
    BREAK BY tt-dados.num_avpes_emitid:

    IF FIRST-OF(tt-dados.num_avpes_emitid) THEN DO:
       ASSIGN tot-nr-pesq = tot-nr-pesq + 1.
    END.
END.


IF rs-tipo = 1  THEN DO: /* Grupo de Assunto */

    /*************** Imprime alteraá‰es **************************/
    /*************** Cabeªalho ***********************************/
    chWorkSheet:Range("a1"):Value = "Pesquisa de Clima Organizacional   -         " + 
                                    "     Tipo:  " + String(v_cdn_avpes_padr_ini)   .    
    chWorkSheet:Range("a1"):Font:Size = 12.
    chWorkSheet:Range("a1"):Font:Bold = TRUE.
    chWorkSheet:Range("a3"):Value = "Emp  ".   
    chWorkSheet:Range("b3"):Value = "Estab".   
    /*
    chWorkSheet:Range("c3"):Value = "Grupo ".  
    chWorkSheet:Range("d3"):Value = "Item    ".  
    chWorkSheet:Range("e3"):Value = "Descriá∆o".
    chWorkSheet:Range("f3"):Value = "Resposta   ".  
    chWorkSheet:Range("g3"):Value = " ".
    */
    chWorkSheet:Range("c3"):Value = "Grupo de Assunto".
    chWorkSheet:Range("d3"):Value = "MÇdia Grupo ".       
    chWorkSheet:Range("e3"):Value = "   %  ". 

    chWorkSheet:Range("a3:e3"):Font:Bold = TRUE.
    chWorkSheet:Columns("a"):ColumnWidth = 05.
    chWorkSheet:Columns("b"):ColumnWidth = 05.
    chWorkSheet:Columns("C"):ColumnWidth = 50.
    chWorkSheet:Columns("d"):ColumnWidth = 12.
    chWorkSheet:Columns("e"):ColumnWidth = 07.
    /*
    chWorkSheet:Columns("f"):ColumnWidth = 10.
    chWorkSheet:Columns("g"):ColumnWidth = 20.
    chWorkSheet:Columns("h"):ColumnWidth = 30.
    chWorkSheet:Columns("i"):ColumnWidth = 12.
    chWorkSheet:Columns("j"):ColumnWidth = 7.
    */
    
    ASSIGN i-linha = 5.

    /* zera variaveis */
    ASSIGN i-nr-pergunta    = 0
           tot-pergunta     = 0
           tot-grupo        = 0
           i-nr-grupo       = 0
           tot-ger-grupo    = 0
           tot-nr-grupo     = 0
           tot-empresa      = 0
           i-nr-tot-empresa = 0
           i-tt-des-gr      = 0
           i-media          = 0
           tot-ger-emp      = 0
           tot-nr-emp       = 0
           i-tot-perc       = 0.

    /**** cria a media total ****/

    FOR EACH tt-des_grp_avpes:
        DELETE tt-des_grp_avpes.
    END.

    FOR EACH   tt-dados
      BREAK BY tt-dados.cdn_empresa
            BY tt-dados.cdn_estab     
            BY tt-dados.cod_unid_lotac
            BY tt-dados.cod_rh_ccusto 
            BY tt-dados.cdn_funcionario
            BY tt-dados.des_grp_avpes
            BY tt-dados.cdn_item_avpes :
        
        ASSIGN i-nr-pergunta = i-nr-pergunta + 1
               tot-pergunta  = tot-pergunta  + tt-dados.val_pontuac_item_avpes.
                 
        IF LAST-OF (tt-dados.des_grp_avpes)  THEN DO:
           ASSIGN tot-grupo  = (tot-pergunta / i-nr-pergunta) + tot-grupo
                  i-nr-grupo = i-nr-grupo + 1 .

            ASSIGN tot-pergunta  = 0
                   i-nr-pergunta = 0  .
        END.
        
        IF LAST-OF (tt-dados.des_grp_avpes) THEN DO:  /* des_grp_avpes */
           CREATE tt-des_grp_avpes.
           ASSIGN tt-des_grp_avpes.des_grp_avpes = tt-dados.des_grp_avpes
                  tt-des_grp_avpes.media         = tot-grupo / i-nr-grupo.
                  
           ASSIGN tot-ger-grupo = (tot-grupo / i-nr-grupo) + tot-ger-grupo
                  tot-nr-grupo  = tot-nr-grupo + 1
                  tot-grupo     = 0 
                  i-nr-grupo    = 0.       
        END.                            
        
        IF LAST-OF (tt-dados.cdn_funcionario) THEN DO:
           ASSIGN tot-empresa      = (tot-ger-grupo / tot-nr-grupo) + tot-empresa
                  i-nr-tot-empresa = 1 + i-nr-tot-empresa
                  tot-ger-grupo    = 0
                  tot-nr-grupo     = 0.
        END.   

        IF LAST-OF(tt-dados.cdn_empresa) THEN DO:
           
           FOR EACH tt-des_grp_avpes 
               BREAK BY  tt-des_grp_avpes.des_grp_avpes:

               ASSIGN i-tt-des-gr = i-tt-des-gr + 1
                      i-media     = tt-des_grp_avpes.media  + i-media. 

               IF LAST-OF( tt-des_grp_avpes.des_grp_avpes) THEN DO:
                 
                  ASSIGN tot-ger-emp = tot-ger-emp +  (i-media / i-tt-des-gr)
                         tot-nr-emp  = tot-nr-emp  +  1.
                  ASSIGN i-tt-des-gr = 0
                         i-media     = 0.      
               END.

           END.   
           ASSIGN i-tot-perc =  truncate(tot-ger-emp / tot-nr-emp, 2).
        END.         
                            
    END.                               

    /* zera variaveis */
    ASSIGN i-nr-pergunta    = 0
           tot-pergunta     = 0
           tot-grupo        = 0
           i-nr-grupo       = 0
           tot-ger-grupo    = 0
           tot-nr-grupo     = 0
           tot-empresa      = 0
           i-nr-tot-empresa = 0
           i-tt-des-gr      = 0
           i-media          = 0
           tot-ger-emp      = 0
           tot-nr-emp       = 0.

    FOR EACH   tt-dados
      BREAK BY tt-dados.cdn_empresa
            BY tt-dados.cdn_estab     
            BY tt-dados.cod_unid_lotac
            BY tt-dados.cod_rh_ccusto 
            BY tt-dados.cdn_funcionario
            BY tt-dados.des_grp_avpes
            BY tt-dados.cdn_item_avpes :
    
        ASSIGN i-nr-pergunta = i-nr-pergunta + 1
               tot-pergunta  = tot-pergunta  + tt-dados.val_pontuac_item_avpes.

        IF FIRST-OF (tt-dados.cdn_funcionario) THEN DO:
           ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-dados.cdn_empresa .
                  chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-dados.cdn_estab.  
                  /*
                  chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-dados.cdn_grp_avpes   . 
                  chExcelApplication:range( "D" + STRING(i-linha) ):value = tt-dados.cdn_sub_item_avpes. 
                  chExcelApplication:range( "E" + STRING(i-linha) ):value = tt-dados.des_impres_item_avpes.
                  chExcelApplication:range( "f" + STRING(i-linha) ):value = tt-dados.val_pontuac_item_avpes. /*val_respos_efeti_func_avpes. */
                  chExcelApplication:range( "g" + STRING(i-linha) ):value = tt-dados.des_impres_sub_item_avpes.
                  */
        END.
                       
        IF LAST-OF (tt-dados.des_grp_avpes)  THEN DO:
           ASSIGN tot-grupo     = (tot-pergunta / i-nr-pergunta) + tot-grupo
                  i-nr-grupo    = i-nr-grupo + 1 .         
           ASSIGN tot-pergunta  = 0
                  i-nr-pergunta = 0  .

        END.
        
        IF LAST-OF (tt-dados.des_grp_avpes) THEN DO:  /* des_grp_avpes */
           ASSIGN /* chExcelApplication:range( "D" + STRING(i-linha) ):value = tt-dados.cdn_sub_item_avpes. */
                  chExcelApplication:range( "c" + STRING(i-linha) ):value = tt-dados.des_grp_avpes.
                  chExcelApplication:range( "d" + STRING(i-linha) ):value = tot-grupo / i-nr-grupo.
                  chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="##0,00" .

           ASSIGN i-linha = i-linha + 1.

           ASSIGN tot-ger-grupo = (tot-grupo / i-nr-grupo) + tot-ger-grupo
                  tot-nr-grupo  = tot-nr-grupo + 1
                  tot-grupo     = 0 
                  i-nr-grupo    = 0.       
        END.
        
        IF LAST-OF (tt-dados.cdn_funcionario) THEN DO: /* cod_unid_lotac */

           ASSIGN d-tt-perc = TRUNCATE(tot-ger-grupo / tot-nr-grupo, 2).
                 
           chExcelApplication:range( "c" + STRING(i-linha) ):value = "* MÇdia => " .
           chExcelApplication:range( "d" + STRING(i-linha) ):value = d-tt-perc .
           chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="##0,00" .
           chExcelApplication:range( "e" + STRING(i-linha) ):value = (d-tt-perc / i-tot-perc) * 100.
           chExcelApplication:Range( "e" + STRING(i-linha)):NumberFormat="##0,00" .
                                                                               
           ASSIGN i-linha          = i-linha + 2.
           ASSIGN tot-empresa      = (tot-ger-grupo / tot-nr-grupo) + tot-empresa
                  i-nr-tot-empresa = 1 + i-nr-tot-empresa
                  tot-ger-grupo    = 0
                  tot-nr-grupo     = 0.
          
        END.

        IF LAST-OF(tt-dados.cdn_empresa) THEN DO:

           ASSIGN i-linha = i-linha + 2.
           chExcelApplication:range( "c" + STRING(i-linha) ):value = "*** MEDIA TOTAL ***".
           ASSIGN i-linha = i-linha + 2.
           
           FOR EACH tt-des_grp_avpes 
               BREAK BY  tt-des_grp_avpes.des_grp_avpes:
           
               ASSIGN i-tt-des-gr = i-tt-des-gr + 1
                      i-media     = tt-des_grp_avpes.media  + i-media. 

               IF LAST-OF( tt-des_grp_avpes.des_grp_avpes) THEN DO:
                  chExcelApplication:range( "c" + STRING(i-linha) ):value = tt-des_grp_avpes.des_grp_avpes.
                  chExcelApplication:range( "d" + STRING(i-linha) ):value = (i-media / i-tt-des-gr).
                  chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="##0,00" .

                  ASSIGN tot-ger-emp = tot-ger-emp +  (i-media / i-tt-des-gr)
                         tot-nr-emp  = tot-nr-emp  +  1.
                  ASSIGN i-linha     = i-linha + 1.
                  ASSIGN i-tt-des-gr = 0
                         i-media     = 0.      
               END.

           END.
     
           ASSIGN i-linha = i-linha + 2.
           chExcelApplication:range( "c" + STRING(i-linha) ):value = "* MÇdia Geral => " .
           chExcelApplication:range( "d" + STRING(i-linha) ):value = tot-ger-emp / tot-nr-emp.
           chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="##0,00" .
            ASSIGN i-linha = i-linha + 2.
           chExcelApplication:range( "c" + STRING(i-linha) ):value = "* Nr.Pesquisas Avaliadas => " .
           chExcelApplication:range( "d" + STRING(i-linha) ):value = tot-nr-pesq.
           chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="####0" .


        END.                           
            
    END.


END.

IF rs-tipo = 2  THEN DO: /* Pergunta */
                                    
    /*************** Imprime alteraá‰es **************************/
    /*************** Cabeªalho ***********************************/
    chWorkSheet:Range("a1"):Value = "Pesquisa de Clima Organizacional   -    "        + 
                                    "     Tipo:  " + String(v_cdn_avpes_padr_ini) .
    chWorkSheet:Range("a1"):Font:Size = 12.
    chWorkSheet:Range("a1"):Font:Bold = TRUE.
    chWorkSheet:Range("a3"):Value = "Emp  ".   
    chWorkSheet:Range("b3"):Value = "Estab".     
    /***
    chWorkSheet:Range("c3"):Value = "Grupo ".        
    chWorkSheet:Range("d3"):Value = "Item    ".      
    chWorkSheet:Range("e3"):Value = "Descriá∆o".     
    chWorkSheet:Range("f3"):Value = "Resposta   ".   
    chWorkSheet:Range("g3"):Value = "Funcionario ".   
    ****/
    chWorkSheet:Range("c3"):Value = "Grupo de Assunto".
    chWorkSheet:Range("d3"):Value = "Pergunta            ".    
    chWorkSheet:Range("e3"):Value = "MÇdia Pergunta".    
   
    chWorkSheet:Range("a3:e3"):Font:Bold = TRUE.
    chWorkSheet:Columns("a"):ColumnWidth = 05.
    chWorkSheet:Columns("b"):ColumnWidth = 05.
    chWorkSheet:Columns("C"):ColumnWidth = 30.
    chWorkSheet:Columns("d"):ColumnWidth = 45.
    chWorkSheet:Columns("e"):ColumnWidth = 15.
    /*
    chWorkSheet:Columns("f"):ColumnWidth = 15.
    chWorkSheet:Columns("g"):ColumnWidth = 15.
    chWorkSheet:Columns("h"):ColumnWidth = 15.
    chWorkSheet:Columns("i"):ColumnWidth = 15.
    chWorkSheet:Columns("j"):ColumnWidth = 15.
    */
    ASSIGN i-linha = 5.   

     ASSIGN i-nr-pergunta = 0
            tot-grupo     = 0
            tot-pergunta  = 0
            tot-ger-grupo = 0
            tot-nr-grupo  = 0. 

    FOR EACH tt-des_impres_item_avpes:
        DELETE tt-des_impres_item_avpes.
    END.                                

    FOR EACH   tt-dados
      BREAK BY tt-dados.cdn_empresa
            BY tt-dados.cdn_estab     
            BY tt-dados.cod_unid_lotac   
            BY tt-dados.cod_rh_ccusto
            BY tt-dados.cdn_funcionario
            BY tt-dados.des_grp_avpes
            BY tt-dados.cdn_item_avpes :

        ASSIGN i-nr-pergunta = i-nr-pergunta + 1
               tot-pergunta  = tot-pergunta  + tt-dados.val_pontuac_item_avpes.

        IF FIRST-OF (tt-dados.des_grp_avpes) THEN DO:
           ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-dados.cdn_empresa .
                  chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-dados.cdn_estab.
                  /*
                  chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-dados.cdn_grp_avpes   .      
                  chExcelApplication:range( "D" + STRING(i-linha) ):value = tt-dados.cdn_sub_item_avpes.        
                  chExcelApplication:range( "E" + STRING(i-linha) ):value = tt-dados.des_impres_item_avpes. 
                  chExcelApplication:range( "F" + STRING(i-linha) ):value = tt-dados.val_pontuac_item_avpes.
                  chExcelApplication:range( "G" + STRING(i-linha) ):value = tt-dados.des_impres_sub_item_avpes. 
                  */
                  chExcelApplication:range( "c" + STRING(i-linha) ):value = tt-dados.des_grp_avpes.
                  chExcelApplication:range( "d" + STRING(i-linha) ):value = tt-dados.des_impres_item_avpes.
        END.

           
        IF /* LAST-OF (tt-dados.cdn_emp)     AND 
           LAST-OF (tt-dados.cdn_estab)     AND        
           LAST-OF (tt-dados.cod_rh_ccusto) AND        
           LAST-OF (tt-dados.des_grp_avpes) AND */        
           LAST-OF (tt-dados.cdn_item_avpes) THEN DO: 

            
           ASSIGN /* chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-dados.cdn_empresa .
                  chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-dados.cdn_estab.
                  chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-dados.cod_rh_ccusto .  */
                  /* chExcelApplication:range( "D" + STRING(i-linha) ):value = tt-dados.cdn_sub_item_avpes.      
                  chExcelApplication:range( "g" + STRING(i-linha) ):value = tt-dados.des_impres_sub_item_avpes.*/
                  chExcelApplication:range( "d" + STRING(i-linha) ):value = tt-dados.des_impres_item_avpes. 
                  chExcelApplication:range( "e" + STRING(i-linha) ):value = tot-pergunta / i-nr-pergunta. 
                  chExcelApplication:Range( "e" + STRING(i-linha)):NumberFormat="##0,00".
                 /*  chExcelApplication:range( "f" + STRING(i-linha) ):value = tt-dados.val_respos_efetd_func_avpes. */
                 /* chExcelApplication:Range( "C" + STRING(i-linha)):NumberFormat="###.###.##0,00" */   
           
           CREATE tt-des_impres_item_avpes.
           ASSIGN tt-des_impres_item_avpes.des_impres_item_avpes = tt-dados.des_impres_item_avpes
                  media-item                                     = tot-pergunta / i-nr-pergunta
                  tt-des_impres_item_avpes.cdn_item_avpes        = tt-dados.cdn_item_avpes.
                  
           ASSIGN tot-ger-grupo = (tot-pergunta / i-nr-pergunta) + tot-ger-grupo
                  tot-nr-grupo  = tot-nr-grupo + 1
                  i-nr-pergunta = 0    
                  tot-grupo     = 0    
                  tot-pergunta  = 0.   
          
           ASSIGN i-linha = i-linha + 1.

        END.
       
        
        ASSIGN i-tt-des-gr = 0
               i-media     = 0.


        IF LAST-OF(tt-dados.cdn_empresa) THEN DO:

           ASSIGN i-linha = i-linha + 2.
           chExcelApplication:range( "d" + STRING(i-linha) ):value = "*** MEDIA EMPRESA - PERGUNTA ***".
           ASSIGN i-linha = i-linha + 2.

           FOR EACH tt-des_impres_item_avpes 
               BREAK BY  tt-des_impres_item_avpes.cdn_item_avpes :

               ASSIGN i-tt-des-gr = i-tt-des-gr + 1
                      i-media     = media-item  + i-media. 

               IF LAST-OF(tt-des_impres_item_avpes.cdn_item_avpes ) THEN DO:
                  chExcelApplication:range( "d" + STRING(i-linha) ):value = tt-des_impres_item_avpes.des_impres_item_avpes.
                  chExcelApplication:range( "e" + STRING(i-linha) ):value = i-media / i-tt-des-gr.
                  chExcelApplication:Range( "e" + STRING(i-linha)):NumberFormat="##0,00" .

                  ASSIGN i-linha = i-linha + 1.
                  ASSIGN i-tt-des-gr = 0
                         i-media     = 0.      
               END.

           END.
           
        END.
           
    END.

END.
           
/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-recebe-dados C-Win 
PROCEDURE pi-recebe-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

           
               for each avpes_anexo where avpes_anexo.cdn_avpes_padr = avpes_emitid.cdn_avpes_padr no-lock
                                    by avpes_anexo.num_seq_impres_avpes:
                   assign v_cdn_campo_impres = avpes_anexo.cdn_campo_impres_avpes.

                   find avpes_reg_mestre where avpes_reg_mestre.cdn_avpes_padr = avpes_emitid.cdn_avpes_padr 
                                         no-lock no-error.
                   if avail avpes_reg_mestre then do:        
                      assign v_cdn_avpes_padr_relat     = avpes_reg_mestre.cdn_avpes_padr
                             v_cdn_avpes_layout_relat   = avpes_reg_mestre.cdn_avpes_layout_relat
                             v_dat_referencia           = avpes_emitid.dat_refer_respos_avpes - (avpes_anexo.qtd_meses_impres_histor * 30)
                             v_dat_inicial              = date(month(v_dat_referencia),1,year(v_dat_referencia))
                             v_dat_final                = avpes_emitid.dat_refer_respos_avpes
                             v_cdn_estab                = avpes_emitid.cdn_estab
                             v_cdn_funcionario          = avpes_emitid.cdn_funcionario
                             v_num_pessoa_fisic         = avpes_emitid.num_pessoa_fisic
                             v_num_pessoa_fisic_avaldor = avpes_emitid.num_pessoa_fisic_avaldor
                             v_dat_refer1               = avpes_emitid.dat_refer_respos_avpes
                             v_log_auto_aval            = avpes_anexo.log_emite_auto_avpes.     
                      if v_cdn_campo_impres = 2 then 
                         run pi-respostas. /* Esta procedure mostra as respostas das avaliaá‰es, n∆o foi criada um 
                                              ds0501.i para n∆o duplicar a l¢gica do ds0502.i */
                   end.
               end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-respostas C-Win 
PROCEDURE pi-respostas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

         for each avpes_grp where avpes_grp.cdn_avpes_padr = v_cdn_avpes_padr_relat no-lock 
                               by avpes_grp.num_seq_impres_avpes:
                find first avpes_mestre_grp where avpes_mestre_grp.cdn_grp_avpes = avpes_grp.cdn_grp_avpes 
                                            no-lock no-error.
                /* grupo geral n∆o entra */
                /*IF avpes_mestre_grp.cdn_grp_avpes = 56 THEN
                   NEXT.*/
               
               for each avpes_grp_item where 
                        avpes_grp_item.cdn_grp_avpes = avpes_grp.cdn_grp_avpes no-lock
                   by avpes_grp_item.num_seq_impres_avpes:
                   find first avpes_item no-lock where 
                              avpes_item.cdn_item_avpes = avpes_grp_item.cdn_item_avpes /*AND 
                              avpes_item.idi_tip_item   = 2*/  no-error.   /* coloquei ind-tip = 2 */
                   if avail avpes_item then do:       

                      for each avpes_sub_item no-lock where
                                  avpes_sub_item.cdn_item_avpes = avpes_item.cdn_item_avpes 
                               /*by avpes_sub_item.num_seq_impres_avpes*/:

                       /*DO WITH FRAME f-pg-sel:
                          ASSIGN fi-texto:SCREEN-VALUE = "Gerando Dados..." + " Item Avpes:  " + string(avpes_item.cdn_item_avpes )   
                                         + "    Avpes:  "  + string(v_num_avpes_emitid). 
                       END.
                       */

                          
                      FIND FIRST /*FOR EACH*/  respos_avpes_func where 
                           respos_avpes_func.num_avpes_emitid = v_num_avpes_emitid and
                           respos_avpes_func.cdn_grp_avpes    = avpes_grp_item.cdn_grp_avpes and
                           respos_avpes_func.cdn_item_avpes   = avpes_grp_item.cdn_item_avpes NO-LOCK NO-ERROR.

                      IF AVAIL respos_avpes_func THEN DO:
                      
                      if avpes_item.idi_tip_item = 2 then do: /**** Tipo Escala    ****/
                            assign v_valor_escala = ((avpes_item.val_interv_item_avpes * avpes_item.qtd_interv_item_avpes) + avpes_item.val_inic_interv).

                            /*****
                            tt-dados.num_avpes_emitid    
                            tt-dados.cdn_grp_avpes       
                            tt-dados.cdn_item_avpes      
                            tt-dados.cdn_sub_item_avpes  *******/
                            
                            /* teste para melhorar o processamento - 10/01/2008 
                              FIND FIRST tt-dados 
                                WHERE tt-dados.cdn_empresa        = avpes_emitid.cdn_empresa  
                                  AND tt-dados.num_avpes_emitid   = respos_avpes_func.num_avpes_emitid
                                  AND tt-dados.cdn_grp_avpes      = avpes_mestre_grp.cdn_grp_avpes
                                  AND tt-dados.cdn_item_avpes     = avpes_grp_item.cdn_item_avpes
                                  AND tt-dados.cdn_sub_item_avpes = respos_avpes_func.cdn_sub_item_avpes NO-ERROR.
                            
                            IF NOT AVAIL tt-dados THEN DO: */
                            
                               CREATE tt-dados.
                               ASSIGN tt-dados.cdn_empresa                  = avpes_emitid.cdn_empresa                     
                                   tt-dados.cdn_estab                    = avpes_emitid.cdn_estab
                                   /* tt-dados.cdn_funcionario           = funcionario.cdn_funcionario                
                                   tt-dados.dat_admis_func               = funcionario.dat_admis_func                 
                                   tt-dados.nom_pessoa_fisic             = funcionario.nom_pessoa_fisic    
                                   tt-dados.cod_rh_ccusto                = funcionario.cod_rh_ccusto 
                                   tt-dados.cod_unid_lotac               = funcionario.cod_unid_lotac 
                                   tt-dados.cdn_niv_cargo                = funcionario.cdn_niv_cargo 
                                   tt-dados.cod_unid_lotac               = funcionario.cod_unid_lotac */              
                                   tt-dados.cdn_estab_avaldor            = avpes_emitid.cdn_estab_func_respos_avpes   
                                   tt-dados.cdn_funcionario_avaldor      = avpes_emitid.cdn_func_respos_avpes         
                                   tt-dados.num_pessoa_avaldor           = avpes_emitid.num_pessoa_fisic_avaldor  
                                   tipo-aval                             = rs-tipo /* 1- Grupo de Assunto, 2- Pergunta */
                                   tt-dados.cdn_grp_avpes                = avpes_mestre_grp.cdn_grp_avpes
                                   tt-dados.des_grp_avpes                = avpes_mestre_grp.des_grp_avpes
                                   tt-dados.des_impres_item_avpes        = avpes_item.des_impres_item_avpes
                                   tt-dados.val_pontuac_item_avpes       = respos_avpes_func.val_pontuac_item_avpes /*respos_avpes_func.val_respos_efetd_func_avpes*/
                                   tt-dados.cdn_item_avpes               = avpes_grp_item.cdn_item_avpes /*avpes_item.cdn_item_avpes */
                                   tt-dados.des_impres_sub_item_avpes    = avpes_sub_item.des_impres_sub_item_avpes
                                   tt-dados.cdn_sub_item_avpes           = respos_avpes_func.cdn_sub_item_avpes /*avpes_sub_item.cdn_sub_item_avpes */
                                   tt-dados.num_avpes_emitid             = respos_avpes_func.num_avpes_emitid.

                               /*DO WITH FRAME f-pg-sel:
                                  ASSIGN fi-texto:SCREEN-VALUE = "Gerando Dados..." + " Est:  " + string(tt-dados.cdn_estab)   
                                         + "    Avpes:  "  + string(tt-dados.num_avpes_emitid). 
                               END. */



                           /* END. */

                      end.  
                      END.
                      END.

                   end. /**** If avpes_item ****/  
               end. /*** For each avpes_item ****/ 

               
            end. /**** For each avpes_avpes_grp *****/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

