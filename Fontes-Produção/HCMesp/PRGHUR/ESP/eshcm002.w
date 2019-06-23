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

def temp-table tt-dados
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
  FIELD des_grp_avpes               LIKE avpes_mestre_grp.des_grp_avpes
  FIELD des_impres_item_avpes       LIKE avpes_item.des_impres_item_avpes
  FIELD val_respos_efetd_func_avpes LIKE respos_avpes_func.val_respos_efetd_func_avpes
  FIELD cdn_item_avpes              LIKE avpes_item.cdn_item_avpes
/*   index i-estab cdn_empresa     */
/*                 cdn_estab       */
/*                 cdn_funcionario */
  index i-pergunta cdn_empresa
                   cdn_estab
                   cod_rh_ccusto 
                   cdn_item_avpes.


DEF VAR v_nom_orig_inform AS CHAR.



/* Variaveis para Gerar em Excel */
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
def var chWorkbook as com-handle no-undo.
def var chWorksheet as com-handle no-undo.

DEF VAR i-linha AS INT.

DEF VAR i-nr-grupo     AS INT  format ">>>>>>>>>>>>9"  .
DEF VAR i-nr-pergunta  AS INT  format ">>>>>>>>>>>>9"  .
DEF VAR tot-grupo      AS INT  format ">>>>>>>>>>>>9"  .
DEF VAR tot-pergunta   AS INT  format ">>>>>>>>>>>>9"  .
DEF VAR tot-ger-grupo  AS INT  format ">>>>>>>>>>>>9"  .
DEF VAR tot-nr-grupo   AS INT  format ">>>>>>>>>>>>9"  .

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

DEFINE VARIABLE fi-cc-fim AS CHARACTER FORMAT "X(08)":U INITIAL "99999999" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cc-ini AS CHARACTER FORMAT "X(08)":U INITIAL "00000000" 
     LABEL "Centro de Custo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/07 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-ini AS DATE FORMAT "99/99/9999":U INITIAL 05/01/07 
     LABEL "Periodo da Avaliaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa AS CHARACTER FORMAT "x(3)":U INITIAL "" 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE unid-lotac-fim AS CHARACTER FORMAT "X(11)":U INITIAL "99999999999" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE unid-lotac-ini AS CHARACTER FORMAT "X(11)":U INITIAL "00000000000" 
     LABEL "Lotaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_avaliador_fim AS INTEGER FORMAT ">>>>>>>9":U INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_avaliador_ini AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL " Funcion†rio" 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_avpes_padr_fim AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_avpes_padr_ini AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "C¢digo Avaliaá∆o":R20 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_avaldor_fim AS CHARACTER FORMAT "x(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_avaldor_ini AS CHARACTER FORMAT "x(5)":U INITIAL "" 
     LABEL " Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_fim AS CHARACTER FORMAT "x(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_ini AS CHARACTER FORMAT "x(5)":U INITIAL "" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_funcionario_fim AS INTEGER FORMAT ">>>>>>>9" INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_funcionario_ini AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL " Funcion†rio" 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-47
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-48
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

DEFINE VARIABLE rs-tipo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Grupo de Assunto", 1,
"Pergunta", 2
     SIZE 49 BY .5 NO-UNDO.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 6.75.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 2.75.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 3.


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
     fi-cc-ini AT ROW 2.25 COL 16 COLON-ALIGNED
     fi-cc-fim AT ROW 2.25 COL 45 COLON-ALIGNED NO-LABEL
     unid-lotac-ini AT ROW 3.25 COL 16 COLON-ALIGNED
     unid-lotac-fim AT ROW 3.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-dt-ini AT ROW 4.25 COL 16 COLON-ALIGNED
     fi-dt-fim AT ROW 4.25 COL 45 COLON-ALIGNED NO-LABEL
     v_cdn_avpes_padr_ini AT ROW 5.25 COL 16 COLON-ALIGNED HELP
          "C¢digo da Avaliaá∆o"
     v_cdn_avpes_padr_fim AT ROW 5.25 COL 45 COLON-ALIGNED HELP
          "C¢digo da Avaliaá∆o" NO-LABEL
     rs-tipo AT ROW 6.75 COL 18 NO-LABEL
     v_cdn_estab_ini AT ROW 8.25 COL 16 COLON-ALIGNED
     v_cdn_estab_fim AT ROW 8.25 COL 46 COLON-ALIGNED NO-LABEL
     v_cdn_funcionario_ini AT ROW 9.25 COL 16 COLON-ALIGNED
     v_cdn_funcionario_fim AT ROW 9.25 COL 46 COLON-ALIGNED NO-LABEL
     v_cdn_estab_avaldor_ini AT ROW 11 COL 16 COLON-ALIGNED
     v_cdn_estab_avaldor_fim AT ROW 11 COL 48 NO-LABEL
     v_cdn_avaliador_ini AT ROW 12 COL 16 COLON-ALIGNED
     v_cdn_avaliador_fim AT ROW 12 COL 46 COLON-ALIGNED NO-LABEL
     IMAGE-16 AT ROW 2.25 COL 41
     IMAGE-18 AT ROW 2.25 COL 34
     IMAGE-19 AT ROW 3.25 COL 34
     IMAGE-20 AT ROW 3.25 COL 41
     IMAGE-47 AT ROW 11 COL 34
     IMAGE-48 AT ROW 11 COL 41
     IMAGE-59 AT ROW 8.25 COL 34
     IMAGE-60 AT ROW 8.25 COL 41
     IMAGE-61 AT ROW 9.25 COL 34
     IMAGE-62 AT ROW 9.25 COL 41
     IMAGE-63 AT ROW 12 COL 34
     IMAGE-64 AT ROW 12 COL 41
     IMAGE-65 AT ROW 4.25 COL 34
     IMAGE-66 AT ROW 4.25 COL 41
     IMAGE-67 AT ROW 5.25 COL 41
     IMAGE-68 AT ROW 5.25 COL 34
     RECT-29 AT ROW 1 COL 1
     RECT-31 AT ROW 10.5 COL 1
     RECT-33 AT ROW 7.5 COL 1
     "  Avaliado" VIEW-AS TEXT
          SIZE 9.14 BY .67 AT ROW 7.5 COL 7
     "  Avaliador" VIEW-AS TEXT
          SIZE 10 BY .67 AT ROW 10.25 COL 7
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1.25
         SIZE 69 BY 12.25
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
         TITLE              = "<Avaliaá∆o por Competància - eshcm002.w>"
         HEIGHT             = 14.46
         WIDTH              = 72
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
/* SETTINGS FOR FILL-IN v_cdn_estab_avaldor_fim IN FRAME f-pg-sel
   ALIGN-L                                                              */
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
ON END-ERROR OF C-Win /* <Avaliaá∆o por Competància - eshcm002.w> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Avaliaá∆o por Competància - eshcm002.w> */
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
&Scoped-define SELF-NAME v_cdn_avpes_padr_fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_avpes_padr_fim C-Win
ON F5 OF v_cdn_avpes_padr_fim IN FRAME f-pg-sel
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_avpes_padr_fim C-Win
ON LEAVE OF v_cdn_avpes_padr_fim IN FRAME f-pg-sel
DO:
   assign v_nom_orig_inform = "DSP".
   /*
   FIND    avpes_reg_mestre 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_avpes_padr_fim C-Win
ON MOUSE-SELECT-DBLCLICK OF v_cdn_avpes_padr_fim IN FRAME f-pg-sel
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_avpes_padr_fim C-Win
ON TAB OF v_cdn_avpes_padr_fim IN FRAME f-pg-sel
DO:
  apply 'leave' to self.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DISPLAY fi-empresa fi-cc-ini fi-cc-fim unid-lotac-ini unid-lotac-fim fi-dt-ini 
          fi-dt-fim v_cdn_avpes_padr_ini v_cdn_avpes_padr_fim rs-tipo 
          v_cdn_estab_ini v_cdn_estab_fim v_cdn_funcionario_ini 
          v_cdn_funcionario_fim v_cdn_estab_avaldor_ini v_cdn_estab_avaldor_fim 
          v_cdn_avaliador_ini v_cdn_avaliador_fim 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE fi-empresa fi-cc-ini fi-cc-fim unid-lotac-ini unid-lotac-fim fi-dt-ini 
         fi-dt-fim v_cdn_avpes_padr_ini v_cdn_avpes_padr_fim rs-tipo 
         v_cdn_estab_ini v_cdn_estab_fim v_cdn_funcionario_ini 
         v_cdn_funcionario_fim v_cdn_estab_avaldor_ini v_cdn_estab_avaldor_fim 
         v_cdn_avaliador_ini v_cdn_avaliador_fim IMAGE-16 IMAGE-18 IMAGE-19 
         IMAGE-20 IMAGE-47 IMAGE-48 IMAGE-59 IMAGE-60 IMAGE-61 IMAGE-62 
         IMAGE-63 IMAGE-64 IMAGE-65 IMAGE-66 IMAGE-67 IMAGE-68 RECT-29 RECT-31 
         RECT-33 
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

ASSIGN fi-empresa               fi-cc-ini                fi-cc-fim  
       fi-dt-ini                fi-dt-fim                rs-tipo
       unid-lotac-ini           unid-lotac-fim           v_cdn_avpes_padr_ini
       v_cdn_avpes_padr_fim     v_cdn_estab_ini          v_cdn_estab_fim          
       v_cdn_funcionario_ini    v_cdn_funcionario_fim    v_cdn_estab_avaldor_ini 
       v_cdn_estab_avaldor_fim  v_cdn_avaliador_ini      v_cdn_avaliador_fim.


        for each avpes_emitid where avpes_emitid.cdn_avpes_padr              >= v_cdn_avpes_padr_ini
                              AND   avpes_emitid.cdn_avpes_padr              <= v_cdn_avpes_padr_fim
                              and   avpes_emitid.log_avpes_finaliz           = YES 
                              and   avpes_emitid.dat_refer_respos_avpes      >= fi-dt-ini 
                              and   avpes_emitid.dat_refer_respos_avpes      <= fi-dt-fim
                              and   avpes_emitid.cdn_empresa                  = fi-empresa
                              and   avpes_emitid.cdn_estab                   >= v_cdn_estab_ini
                              and   avpes_emitid.cdn_estab                   <= v_cdn_estab_fim 
                              and   avpes_emitid.cdn_funcionario             >= v_cdn_funcionario_ini
                              and   avpes_emitid.cdn_funcionario             <= v_cdn_funcionario_fim
                              /* and   avpes_emitid.num_pessoa_fisic            >= v_num_pessoa_fisic_ini
                              and   avpes_emitid.num_pessoa_fisic            <= v_num_pessoa_fisic_fim */
                              and   avpes_emitid.cdn_estab_func_respos_avpes >= v_cdn_estab_avaldor_ini
                              and   avpes_emitid.cdn_estab_func_respos_avpes <= v_cdn_estab_avaldor_fim 
                              /*
                              and   avpes_emitid.cdn_func_respos_avpes       >= tt-param.cdn_avaliador_ini
                              and   avpes_emitid.cdn_func_respos_avpes       <= tt-param.cdn_avaliador_fim
                              and   avpes_emitid.num_pessoa_fisic_avaldor    >= tt-param.num_pessoa_fisic_avaldor_ini
                              and   avpes_emitid.num_pessoa_fisic_avaldor    <= tt-param.num_pessoa_fisic_avaldor_fim
                            
                              and   avpes_emitid.num_avpes_emitid            >= tt-param.num_avpes_emitid_ini
                              and   avpes_emitid.num_avpes_emitid            <= tt-param.num_avpes_emitid_fim 
                              and   avpes_emitid.num_control_emis_avpes      >= tt-param.num_control_emis_ini 
                              and   avpes_emitid.num_control_emis_avpes      <= tt-param.num_control_emis_fim 
                              and ((tt-param.nom_orig_inform                  = "ATP"     
                              and   avpes_emitid.cdn_curso_trein             >= tt-param.cdn_curso_ini
                              and   avpes_emitid.cdn_curso_trein             <= tt-param.cdn_curso_fim
                              and   avpes_emitid.cdn_turma_trein             >= tt-param.cdn_turma_ini
                              and   avpes_emitid.cdn_turma_trein             <= tt-param.cdn_turma_fim
                              and   avpes_emitid.cdn_bolsa_estudo            >= tt-param.cdn_bolsa_ini 
                              and   avpes_emitid.cdn_bolsa_estudo            <= tt-param.cdn_bolsa_fim) 
                              or   (tt-param.nom_orig_inform                  = "DSP")) */ no-lock:

          assign v_num_avpes_emitid = avpes_emitid.num_avpes_emitid.
          if avpes_emitid.cdn_funcionario <> 0 then do: 
             for each funcionario where
                      funcionario.cdn_empresa       = avpes_emitid.cdn_empresa
                and   funcionario.cdn_estab         = avpes_emitid.cdn_estab
                and   funcionario.cdn_funcionario   = avpes_emitid.cdn_funcionario 
                AND   (funcionario.cod_unid_lotac) >= unid-lotac-ini 
                and   (funcionario.cod_unid_lotac) <= unid-lotac-fim
                /* and   funcionario.cod_unid_lotac >= tt-param.cod_unid_lotac_ini
                and   funcionario.cod_unid_lotac <= tt-param.cod_unid_lotac_fim */
                and int(funcionario.cod_rh_ccusto)   >= int(fi-cc-ini) 
                and int(funcionario.cod_rh_ccusto)   <= int(fi-cc-fim)  no-lock
                      break by funcionario.cdn_empresa
                            by funcionario.cdn_estab
                            by funcionario.nom_pessoa_fisic: 

                 
                 run pi-recebe-dados.
             end.
          END.
        END.  
         
END.

RUN pi-imprime.
 
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

IF rs-tipo = 1  THEN DO: /* Grupo de Assunto */

    /*************** Imprime alteraá‰es **************************/
    /*************** Cabeªalho ***********************************/
    chWorkSheet:Range("a1"):Value = "Avaliaá∆o por Competància - Grupo de Assunto     " + 
                                    "     Tipo:  " + String(v_cdn_avpes_padr_ini)       + 
                                    "   a    "     + String(v_cdn_avpes_padr_fim).
    chWorkSheet:Range("a1"):Font:Size = 12.
    chWorkSheet:Range("a1"):Font:Bold = TRUE.
    chWorkSheet:Range("a3"):Value = "Emp  ".   
    chWorkSheet:Range("b3"):Value = "Estab".   
    chWorkSheet:Range("c3"):Value = "Lotaá∆o ".  
    chWorkSheet:Range("d3"):Value = "Descriá∆o Lotaá∆o    ".  
    chWorkSheet:Range("e3"):Value = "Centro Custo".   
    chWorkSheet:Range("f3"):Value = "Grupo de Assunto".
    chWorkSheet:Range("g3"):Value = "MÇdia Grupo ".       
   
    chWorkSheet:Range("a3:g3"):Font:Bold = TRUE.
    chWorkSheet:Columns("a"):ColumnWidth = 05.
    chWorkSheet:Columns("b"):ColumnWidth = 05.
    chWorkSheet:Columns("C"):ColumnWidth = 08.
    chWorkSheet:Columns("d"):ColumnWidth = 30.
    chWorkSheet:Columns("e"):ColumnWidth = 12.
    chWorkSheet:Columns("f"):ColumnWidth = 20.
    chWorkSheet:Columns("g"):ColumnWidth = 12.
    
    ASSIGN i-linha = 5.

     ASSIGN i-nr-pergunta = 0
            tot-grupo     = 0
            tot-pergunta  = 0
            i-nr-grupo    = 0
            tot-ger-grupo = 0
            tot-nr-grupo  = 0. 

    FOR EACH   tt-dados
      BREAK BY tt-dados.cdn_empresa
            BY tt-dados.cdn_estab     
            BY tt-dados.cod_unid_lotac
            BY tt-dados.cod_rh_ccusto 
            BY tt-dados.des_grp_avpes
            BY tt-dados.cdn_item_avpes :

        ASSIGN i-nr-pergunta = i-nr-pergunta + 1
               tot-pergunta  = tot-pergunta  + tt-dados.val_respos_efetd_func_avpes.

        /* IF FIRST-OF (tt-dados.des_grp_avpes) THEN DO:
           ASSIGN i-nr-grupo = i-nr-grupo + 1.
        END. */

        IF FIRST-OF (tt-dados.cod_unid_lotac) THEN DO:
           ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-dados.cdn_empresa .
                  chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-dados.cdn_estab.     
                  chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-dados.cod_unid_lotac    . 
                  chExcelApplication:range( "D" + STRING(i-linha) ):value = tt-dados.des_unid_lotac. 
                  chExcelApplication:range( "E" + STRING(i-linha) ):value = tt-dados.cod_rh_ccusto .
        END.
                       
        IF /* LAST-OF (tt-dados.cdn_emp)     AND 
           LAST-OF (tt-dados.cdn_estab)     AND        
           LAST-OF (tt-dados.cod_rh_ccusto) AND 
           LAST-OF (tt-dados.des_grp_avpes)  AND*/      
           LAST-OF (tt-dados.cdn_item_avpes)  THEN DO:
             
           ASSIGN tot-grupo  = (tot-pergunta / i-nr-pergunta) + tot-grupo
                  i-nr-grupo = i-nr-grupo + 1 .

            /*ASSIGN  chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-dados.cdn_empresa .
                  chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-dados.cdn_estab.
                  chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-dados.cod_rh_ccusto . 
                  chExcelApplication:range( "D" + STRING(i-linha) ):value = tt-dados.des_grp_avpes. 
                  chExcelApplication:range( "e" + STRING(i-linha) ):value = tt-dados.des_impres_item_avpes. 
                  chExcelApplication:range( "f" + STRING(i-linha) ):value = (tot-pergunta / i-nr-pergunta). */
                 /*  chExcelApplication:range( "f" + STRING(i-linha) ):value = tt-dados.val_respos_efetd_func_avpes. */
                 /* chExcelApplication:Range( "C" + STRING(i-linha)):NumberFormat="###.###.##0,00" */    

           
            ASSIGN  tot-pergunta  = 0
                   i-nr-pergunta = 0  .
            
           /* ASSIGN i-linha = i-linha + 1. */

        END.
        
        IF LAST-OF (tt-dados.des_grp_avpes) THEN DO:
           ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = tt-dados.des_grp_avpes.
                  chExcelApplication:range( "g" + STRING(i-linha) ):value = tot-grupo / i-nr-grupo.
                  chExcelApplication:Range( "g" + STRING(i-linha)):NumberFormat="##0,00" .
                  
           ASSIGN i-linha = i-linha + 1.
         

           ASSIGN tot-ger-grupo = (tot-grupo / i-nr-grupo) + tot-ger-grupo
                  tot-nr-grupo  = tot-nr-grupo + 1
                  tot-grupo  = 0 
                  i-nr-grupo = 0.       
                 
        END.

        IF LAST-OF (tt-dados.cod_unid_lotac) THEN DO:
           chExcelApplication:range( "f" + STRING(i-linha) ):value = "* Media Total  => " .
           chExcelApplication:range( "g" + STRING(i-linha) ):value = tot-ger-grupo / tot-nr-grupo.
           chExcelApplication:Range( "g" + STRING(i-linha)):NumberFormat="##0,00" .
                                       
           ASSIGN i-linha = i-linha + 2.

           ASSIGN tot-ger-grupo = 0
                  tot-nr-grupo  = 0.


        END.




            
    END.


END.

IF rs-tipo = 2  THEN DO: /* Pergunta */
                                    
    /*************** Imprime alteraá‰es **************************/
    /*************** Cabeªalho ***********************************/
    chWorkSheet:Range("a1"):Value = "Avaliaá∆o por Competància - Pergunta "        + 
                                    "     Tipo:  " + String(v_cdn_avpes_padr_ini)  + 
                                    "   a    "     + String(v_cdn_avpes_padr_fim).
    chWorkSheet:Range("a1"):Font:Size = 12.
    chWorkSheet:Range("a1"):Font:Bold = TRUE.
    chWorkSheet:Range("a3"):Value = "Emp  ".   
    chWorkSheet:Range("b3"):Value = "Estab".     
    chWorkSheet:Range("c3"):Value = "Lotaá∆o ".  
    chWorkSheet:Range("d3"):Value = "Descriá∆o Lotaá∆o      ".  
    chWorkSheet:Range("e3"):Value = "Centro Custo".   
    chWorkSheet:Range("f3"):Value = "Grupo de Assunto".
    chWorkSheet:Range("g3"):Value = "Pergunta            ".    
    chWorkSheet:Range("h3"):Value = "MÇdia Pergunta".   
    chWorkSheet:Range("i3"):Value = "Nota Perg. Func".
   
    chWorkSheet:Range("a3:i3"):Font:Bold = TRUE.
    chWorkSheet:Columns("a"):ColumnWidth = 05.
    chWorkSheet:Columns("b"):ColumnWidth = 05.
    chWorkSheet:Columns("C"):ColumnWidth = 08.
    chWorkSheet:Columns("d"):ColumnWidth = 30.
    chWorkSheet:Columns("e"):ColumnWidth = 20.
    chWorkSheet:Columns("f"):ColumnWidth = 15.
    chWorkSheet:Columns("g"):ColumnWidth = 15.
    chWorkSheet:Columns("h"):ColumnWidth = 15.
    chWorkSheet:Columns("i"):ColumnWidth = 15.
   
    ASSIGN i-linha = 5.   

     ASSIGN i-nr-pergunta = 0
            tot-grupo     = 0
            tot-pergunta  = 0
            tot-ger-grupo = 0
            tot-nr-grupo  = 0. 

    FOR EACH   tt-dados
      BREAK BY tt-dados.cdn_empresa
            BY tt-dados.cdn_estab     
            BY tt-dados.cod_unid_lotac   
            BY tt-dados.cod_rh_ccusto 
            BY tt-dados.des_grp_avpes
            BY tt-dados.cdn_item_avpes :

        ASSIGN i-nr-pergunta = i-nr-pergunta + 1
               tot-pergunta  = tot-pergunta  + tt-dados.val_respos_efetd_func_avpes.

        IF FIRST-OF (tt-dados.des_grp_avpes) THEN DO:
           ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-dados.cdn_empresa .
                  chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-dados.cdn_estab.
                  chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-dados.cod_unid_lotac . 
                  chExcelApplication:range( "D" + STRING(i-linha) ):value = tt-dados.des_unid_lotac.
                  chExcelApplication:range( "E" + STRING(i-linha) ):value = tt-dados.cod_rh_ccusto . 
                  chExcelApplication:range( "F" + STRING(i-linha) ):value = tt-dados.des_grp_avpes.
                  chExcelApplication:range( "G" + STRING(i-linha) ):value = tt-dados.des_impres_item_avpes.
        END.

        

                       
        IF /* LAST-OF (tt-dados.cdn_emp)     AND 
           LAST-OF (tt-dados.cdn_estab)     AND        
           LAST-OF (tt-dados.cod_rh_ccusto) AND        
           LAST-OF (tt-dados.des_grp_avpes) AND */        
           LAST-OF (tt-dados.cdn_item_avpes) THEN DO: 


           ASSIGN /* chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-dados.cdn_empresa .
                  chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-dados.cdn_estab.
                  chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-dados.cod_rh_ccusto . 
                  chExcelApplication:range( "D" + STRING(i-linha) ):value = tt-dados.des_grp_avpes. */
                  chExcelApplication:range( "g" + STRING(i-linha) ):value = tt-dados.des_impres_item_avpes. 
                  chExcelApplication:range( "H" + STRING(i-linha) ):value = tot-pergunta / i-nr-pergunta. 
                  chExcelApplication:Range( "h" + STRING(i-linha)):NumberFormat="##0,00".
                  chExcelApplication:range( "i" + STRING(i-linha) ):value = tt-dados.val_respos_efetd_func_avpes .  /* teste cida */
                 /* chExcelApplication:Range( "C" + STRING(i-linha)):NumberFormat="###.###.##0,00" */    

           ASSIGN  tot-ger-grupo = (tot-pergunta / i-nr-pergunta) + tot-ger-grupo
                  tot-nr-grupo  = tot-nr-grupo + 1
                  i-nr-pergunta = 0    
                  tot-grupo     = 0    
                  tot-pergunta  = 0.   
          
           ASSIGN i-linha = i-linha + 1.

        END.

        IF LAST-OF (tt-dados.des_grp_avpes) THEN DO:
           chExcelApplication:range( "g" + STRING(i-linha) ):value = "* Media Total  => " .
           chExcelApplication:range( "h" + STRING(i-linha) ):value = tot-ger-grupo / tot-nr-grupo.
           chExcelApplication:Range( "h" + STRING(i-linha)):NumberFormat="##0,00" .
                                       
           ASSIGN i-linha = i-linha + 2.

           ASSIGN tot-ger-grupo = 0
                  tot-nr-grupo  = 0.


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

         def var v_des_grp_avpes as char no-undo.
         def var v_val_respos_efetd_func_avpes as dec no-undo.

         for each avpes_grp where avpes_grp.cdn_avpes_padr = v_cdn_avpes_padr_relat no-lock
             by avpes_grp.num_seq_impres_avpes:

             find first avpes_mestre_grp where avpes_mestre_grp.cdn_grp_avpes = avpes_grp.cdn_grp_avpes no-lock no-error.

             if avail avpes_mestre_grp then
                 assign v_des_grp_avpes = avpes_mestre_grp.des_grp_avpes.
             else
                 assign v_des_grp_avpes = "".

             for each avpes_grp_item where 
                 avpes_grp_item.cdn_grp_avpes = avpes_grp.cdn_grp_avpes no-lock
                 by avpes_grp_item.num_seq_impres_avpes:

                 find first avpes_item no-lock where 
                            avpes_item.cdn_item_avpes = avpes_grp_item.cdn_item_avpes no-error.

                   if avail avpes_item then do:        
                     
                      find respos_avpes_func no-lock where 
                           respos_avpes_func.num_avpes_emitid = v_num_avpes_emitid and
                           respos_avpes_func.cdn_grp_avpes = avpes_grp_item.cdn_grp_avpes and
                           respos_avpes_func.cdn_item_avpes = avpes_grp_item.cdn_item_avpes no-error.

                      if avail respos_avpes_func then
                          assign v_val_respos_efetd_func_avpes = respos_avpes_func.val_respos_efetd_func_avpes.
                      else
                          assign v_val_respos_efetd_func_avpes = 0.
                      
                      if avpes_item.idi_tip_item = 5 then do: /**** Tipo Escala    ****/
                            assign v_valor_escala = ((avpes_item.val_interv_item_avpes * avpes_item.qtd_interv_item_avpes) + avpes_item.val_inic_interv).
                        
                            CREATE tt-dados.
                            ASSIGN tt-dados.cdn_empresa             = funcionario.cdn_empresa                    
                                   tt-dados.cdn_estab               = funcionario.cdn_estab                      
                                   tt-dados.cdn_funcionario         = funcionario.cdn_funcionario                
                                   tt-dados.dat_admis_func          = funcionario.dat_admis_func                 
                                   tt-dados.nom_pessoa_fisic        = funcionario.nom_pessoa_fisic    
                                   tt-dados.cod_rh_ccusto           = funcionario.cod_rh_ccusto 
                                   tt-dados.cod_unid_lotac          = funcionario.cod_unid_lotac 
                                   /*cdn_cargo_basic           as integer format ">>>>9"  label "      "     
                                   des_cargo_basic           as char format "x(20)" */                       
                                   tt-dados.cdn_niv_cargo           = funcionario.cdn_niv_cargo 
                                   tt-dados.cod_unid_lotac          = funcionario.cod_unid_lotac                 
                                   /*des_unid_lotac            as char format "x(20)"         */               
                                   tt-dados.cdn_estab_avaldor       = avpes_emitid.cdn_estab_func_respos_avpes   
                                   tt-dados.cdn_funcionario_avaldor = avpes_emitid.cdn_func_respos_avpes         
                                   tt-dados.num_pessoa_avaldor      = avpes_emitid.num_pessoa_fisic_avaldor
                                   /*dat_refer_respos          as date format "99/99/9999"                   
                                   val_pontuac_avpes         as decimal format ">>9.99"                    
                                   des_restdo                as char format "x(10)" */                       
                                   tipo-aval                             = rs-tipo /* 1- Grupo de Assunto, 2- Pergunta */ 
                                   tt-dados.des_grp_avpes                = v_des_grp_avpes
                                   tt-dados.des_impres_item_avpes        = avpes_item.des_impres_item_avpes
                                   tt-dados.val_respos_efetd_func_avpes  = v_val_respos_efetd_func_avpes
                                   tt-dados.cdn_item_avpes               = avpes_item.cdn_item_avpes.

                            FIND unid_lotac
                               WHERE unid_lotac.cod_unid_lotac = funcionario.cod_unid_lotac
                               NO-LOCK NO-ERROR.
                            IF AVAIL unid_lotac THEN
                               ASSIGN tt-dados.des_unid_lotac = unid_lotac.des_unid_lotac.
                            
                      end.  

                   end. /**** If avpes_item ****/  
               end. /*** For each avpes_item ****/ 

               
            end. /**** For each avpes_avpes_grp *****/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

