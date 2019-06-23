&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

/*****************************************************************************
** Programa..............: esfas002 
** Descricao.............: Gera Arquivos para Importa‡Æo no Ativo Fixo
** Versao................: 1.00.00.000
** Procedimento..........: esfas002
** Nome Externo..........: esfas002
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 27/06/2013
*****************************************************************************/

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

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.

DEFINE VARIABLE h-acomp AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE tt-tabela
    FIELD tipo          AS CHAR
    FIELD campo         AS CHAR
    FIELD formato       AS CHAR
    FIELD obrigatorio   AS LOGICAL
    FIELD titulo        AS CHAR
    FIELD posicao       AS INTE
    FIELD cod_cenar_ctbl   like finalid_utiliz_cenar.cod_cenar_ctbl  
    FIELD cod_finalid_econ like finalid_utiliz_cenar.cod_finalid_econ
    INDEX Idx01 AS PRIMARY
      posicao
    INDEX Idx02 AS UNIQUE
      tipo
      campo
      formato
      obrigatorio
      titulo
      cod_cenar_ctbl  
      cod_finalid_econ
      .

DEFINE TEMP-TABLE tt-geracao LIKE tt-tabela.

DEFINE TEMP-TABLE tt-val-cenario LIKE finalid_utiliz_cenar.

def temp-table tt-bem-pat no-undo
    field nr-seq-bem                as int  format '>>>>>>>9'              init 0            /* gerar */
    field cod_empresa               like bem_pat.cod_empresa               init '""'
    field cod_cta_pat               like bem_pat.cod_cta_pat               init '""'
    field num_bem_pat               like bem_pat.num_bem_pat               init 0
    field num_seq_bem_pat           like bem_pat.num_seq_bem_pat           init 1
    field des_bem_pat               like bem_pat.des_bem_pat               init '""'         /* 40 posi‡oes da narrativa */
    field cb3_ident_visual          like bem_pat.cb3_ident_visual          init '"?"' /* plaqueta */
    field qtd_bem_pat_represen      like bem_pat.qtd_bem_pat_represen      init 1
    field ind_periodic_invent       like bem_pat.ind_periodic_invent       init '"nenhuma"'
    field dat_aquis_bem_pat         like bem_pat.dat_aquis_bem_pat         init TODAY
    field cod_estab                 like bem_pat.cod_estab                 init '""'
    field cod_espec_bem             like bem_pat.cod_espec_bem             init '""'
    field cod_marca                 like bem_pat.cod_marca                 init '""'
    field cod-modelo                as char format 'x(08)'                 init '""'
    field cod_licenc_uso            like bem_pat.cod_licenc_uso            init '""'
    field cod_especif_tec           like bem_pat.cod_especif_tec           init '""'
    field cod_estado_fisic_bem_pat  like bem_pat.cod_estado_fisic_bem_pat  init '""'
    field cod_arrendador            like bem_pat.cod_arrendador            init '""'
    field cod_contrat_leas          like bem_pat.cod_contrat_leas          init '""'
    field cdn_fornecedor            like bem_pat.cdn_fornecedor            init 0
    field cod_localiz               like bem_pat.cod_localiz               init '""'
    field cod_usuario               like bem_pat.cod_usuario               init '""'
    field dat_ult_invent            like bem_pat.dat_ult_invent            init TODAY
    field des_narrat_bem_pat        like bem_pat.des_narrat_bem_pat        init '""'
    field seguradora                as char format 'x(08)'                 init '""'
    field apolice-seguro            as char format 'x(12)'                 init '""'
    field dat_ini_valid_apolice     as date format '99/99/9999'            init ?
    field dat_fim_valid_apolice     as date format '99/99/9999'            init ?
    field premio-seguro             as int  format '>>>>>>>>>>>9'          init 0
    field seguradora1               as char format 'x(08)'                 init '""'
    field apolice-seguro1           as char format 'x(12)'                 init '""'
    field dat_ini_valid_apolice1    as date format '99/99/9999'            init ?
    field dat_fim_valid_apolice1    as date format '99/99/9999'            init ?
    field premio-seguro1            as int  format '>>>>>>>>>>>9'          init 0
    field seguradora2               as char format 'x(08)'                 init '""'
    field apolice-seguro2           as char format 'x(12)'                 init '""'
    field dat_ini_valid_apolice2    as date format '99/99/9999'            init ?
    field dat_fim_valid_apolice2    as date format '99/99/9999'            init ?
    field premio-seguro2            as int  format '>>>>>>>>>>>9'          init 0
    field cod_docto_entr            like bem_pat.cod_docto_entr            init '""'
    field num_item_docto_entr       like bem_pat.num_item_docto_entr       init 0
    field num_pessoa_jurid_gartia   like bem_pat.num_pessoa_jurid_gartia   init 0
    field dat_inic_valid_gartia_bem like bem_pat.dat_inic_valid_gartia_bem init ?
    field dat_fim_valid_gartia_bem  like bem_pat.dat_fim_valid_gartia_bem  init ?
    field des_termo_gartia_bem_pat  like bem_pat.des_termo_gartia_bem_pat  init '""'
    field cod_grp_calc              like bem_pat.cod_grp_calc              init '""'
    field dat-movto                 as date format '99/99/9999'            init ?
    field val_perc_bxa              like bem_pat.val_perc_bxa              init 0
    field dat-ini-calc-dpr          as date format '99/99/9999'            init TODAY
    field dat_calc_pat              like bem_pat.dat_calc_pat              init TODAY
    field cod_ser_nota              like bem_pat.cod_ser_nota              init '""'
    field bem-importado             as log                                 init no
    field credita-PIS               as log                                 init no
    field credita-COFINS            as log                                 init no
    field nr-parc-cred-PIS-COFINS   as int  format '>9'                    init 0
    field parc-descontadas          as int  format '>9'                    init 0
    field val-credito-PIS           as dec  format '>>>>>>>>9.99'          init 0
    field val-credito-COFINS        as dec  format '>>>>>>>>9.99'          init 0
    field credita-CSLL              as log                                 init no
    field exec-credito-CSLL         as int  format '>9'                    init 0
    field val-base-PIS              as dec  format '>>>>>>>>9.99'          init 0
    field val-base-COFINS           as dec  format '>>>>>>>>9.99'          init 0
    index codigo as primary unique cod_empresa cod_cta_pat num_bem_pat num_seq_bem_pat
    index cod-seq as unique nr-seq-bem.

def temp-table tt-valores no-undo
    field nr-seq-bem         as int  format '>>>>>>>9'        init 0            /* gerar */
    field seq-incorp         as int  format '>>>>>>>9'        init 0
    field cenario-contabil   as char format 'x(08)'           init '""'  /* Fiscal - Corrente ; Fiscal - Dolar */
    field finalidade         as char format 'x(10)'           init '""' /* "Corrente" e "Dolar" */
    field val-orig           as dec  format '>>>>>>>>>>>9,99' init 0
    field correcao-monet     as dec  format '>>>>>>>>>>>9,99' init 0
    field dpr-val-orig       as dec  format '>>>>>>>>>>>9,99' init 0
    field dpr-correcao-monet as dec  format '>>>>>>>>>>>9,99' init 0
    field correcao-monet-dpr as dec  format '>>>>>>>>>>>9,99' init 0
    field dpr-incentivada    as dec  format '>>>>>>>>>>>9,99' init 0
    field dpr-incentiv-cm    as dec  format '>>>>>>>>>>>9,99' init 0
    field cm-dpr-incentiv    as dec  format '>>>>>>>>>>>9,99' init 0
    field amort-vo           as dec  format '>>>>>>>>>>>9,99' init 0
    field amort-cm           as dec  format '>>>>>>>>>>>9,99' init 0
    field cm-amort           as dec  format '>>>>>>>>>>>9,99' init 0
    field amort-incentiv     as dec  format '>>>>>>>>>>>9,99' init 0
    field amort-incentiv-cm  as dec  format '>>>>>>>>>>>9,99' init 0
    field cm-amort-incentiv  as dec  format '>>>>>>>>>>>9,99' init 0
    field perc-dpr           as dec  format '>>>>9,9999'      init 0 /* =100/(meses/12) sendo o meses a quantidade de meses total para depreciar */
    field perc-dpr-incentiv  as dec  format '>>>>9,9999'      init 0
    field perc-dpr-red-saldo as dec  format '>>>>9,9999'      init 0
    index codigo as primary unique nr-seq-bem cenario-contabil finalidade.

def temp-table tt-alocacoes no-undo
    field nr-seq-bem       as int  format '>>>>>>>9' init 0           /* gerar */
    field cod-plano-ccusto as char format 'x(08)'    init '""'
    field cod-ccusto       as char format 'x(11)'    init '""'
    field unid-negoc       as char format 'x(03)'    init '""'
    field perc-apropriacao as int  format '>>>>>9'   init 100
    field ccusto-un-princ  as log                    init yes
    index codigo as primary unique nr-seq-bem.

def var i-cont as int no-undo.

def stream saida1.
def stream saida2.
def stream saida3.

def var v_cod_dwb_file
    as character
    format "x(40)":U
    label "Arquivo"
    column-label "Arquivo"
    no-undo.

DEFINE VARIABLE iPosicao AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLinha AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f_main
&Scoped-define BROWSE-NAME br-campos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-tabela finalid_utiliz_cenar tt-geracao ~
tt-val-cenario

/* Definitions for BROWSE br-campos                                     */
&Scoped-define FIELDS-IN-QUERY-br-campos tt-tabela.tipo trim (tt-tabela.titulo) trim (tt-tabela.formato) tt-tabela.cod_cenar_ctbl tt-tabela.cod_finalid_econ tt-tabela.obrigatorio   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-campos   
&Scoped-define SELF-NAME br-campos
&Scoped-define QUERY-STRING-br-campos FOR EACH tt-tabela NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-campos OPEN QUERY {&SELF-NAME} FOR EACH tt-tabela NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-campos tt-tabela
&Scoped-define FIRST-TABLE-IN-QUERY-br-campos tt-tabela


/* Definitions for BROWSE br-cenar-finalid                              */
&Scoped-define FIELDS-IN-QUERY-br-cenar-finalid finalid_utiliz_cenar.cod_empresa finalid_utiliz_cenar.cod_cenar_ctbl finalid_utiliz_cenar.cod_finalid_econ   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-cenar-finalid   
&Scoped-define SELF-NAME br-cenar-finalid
&Scoped-define QUERY-STRING-br-cenar-finalid FOR EACH finalid_utiliz_cenar NO-LOCK       WHERE finalid_utiliz_cenar.cod_empresa     = v_cod_empres_usuar       BY finalid_utiliz_cenar.cod_cenar_ctbl INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-cenar-finalid OPEN QUERY {&SELF-NAME} FOR EACH finalid_utiliz_cenar NO-LOCK       WHERE finalid_utiliz_cenar.cod_empresa     = v_cod_empres_usuar       BY finalid_utiliz_cenar.cod_cenar_ctbl INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-cenar-finalid finalid_utiliz_cenar
&Scoped-define FIRST-TABLE-IN-QUERY-br-cenar-finalid finalid_utiliz_cenar


/* Definitions for BROWSE br-geracao                                    */
&Scoped-define FIELDS-IN-QUERY-br-geracao tt-geracao.posicao tt-geracao.tipo trim (tt-geracao.titulo) tt-geracao.cod_cenar_ctbl tt-geracao.cod_finalid_econ trim (tt-geracao.formato) tt-geracao.obrigatorio   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-geracao   
&Scoped-define SELF-NAME br-geracao
&Scoped-define QUERY-STRING-br-geracao FOR EACH tt-geracao NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-geracao OPEN QUERY {&SELF-NAME} FOR EACH tt-geracao NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-geracao tt-geracao
&Scoped-define FIRST-TABLE-IN-QUERY-br-geracao tt-geracao


/* Definitions for BROWSE br-valores                                    */
&Scoped-define FIELDS-IN-QUERY-br-valores tt-val-cenario.cod_empresa tt-val-cenario.cod_cenar_ctbl tt-val-cenario.cod_finalid_econ   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-valores   
&Scoped-define SELF-NAME br-valores
&Scoped-define QUERY-STRING-br-valores FOR EACH tt-val-cenario       WHERE tt-val-cenario.cod_empresa = v_cod_empres_usuar NO-LOCK     BY tt-val-cenario.cod_cenar_ctbl INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-valores OPEN QUERY {&SELF-NAME} FOR EACH tt-val-cenario       WHERE tt-val-cenario.cod_empresa = v_cod_empres_usuar NO-LOCK     BY tt-val-cenario.cod_cenar_ctbl INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-valores tt-val-cenario
&Scoped-define FIRST-TABLE-IN-QUERY-br-valores tt-val-cenario


/* Definitions for FRAME f_main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f_main ~
    ~{&OPEN-QUERY-br-campos}~
    ~{&OPEN-QUERY-br-cenar-finalid}~
    ~{&OPEN-QUERY-br-geracao}~
    ~{&OPEN-QUERY-br-valores}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-busca-arq rt_rgf rt_cxcf RECT-8 RECT-9 ~
RECT-10 RECT-11 br-cenar-finalid br-valores bt-add-1 bt-del1 rd-sel-campos ~
rd-sel-geracao br-campos br-geracao bt-add-2 bt-del-2 bt-gera-modelo ~
fi-arq-entrada fi-dir-saida bt-ok bt_cancelar Btn_Help bt-busca-dir 
&Scoped-Define DISPLAYED-OBJECTS rd-sel-campos rd-sel-geracao ~
fi-arq-entrada fi-dir-saida 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add-1 
     IMAGE-UP FILE "adeicon/next-au.bmp":U
     LABEL "Add" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-add-2 
     IMAGE-UP FILE "adeicon/next-au.bmp":U
     LABEL "Add" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-busca-arq 
     IMAGE-UP FILE "image/im-sea1.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-sea1.bmp":U NO-FOCUS
     LABEL "Busca Arquivo" 
     SIZE 4 BY 1.13 TOOLTIP "Busca Arquivo".

DEFINE BUTTON bt-busca-dir 
     IMAGE-UP FILE "image/im-sea1.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-sea1.bmp":U NO-FOCUS
     LABEL "Busca Diretorio" 
     SIZE 4 BY 1.13 TOOLTIP "Busca Diretorio".

DEFINE BUTTON bt-del-2 
     IMAGE-UP FILE "adeicon/prev-au.bmp":U
     LABEL "Del" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-del1 
     IMAGE-UP FILE "adeicon/prev-au.bmp":U
     LABEL "Del" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-gera-modelo 
     IMAGE-UP FILE "adeicon/u-fields.bmp":U
     LABEL "Gera Modelo" 
     SIZE 7 BY 2.08 TOOLTIP "Gera Modelo".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "Gerar" 
     SIZE 10 BY 1 TOOLTIP "Gerar"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-arq-entrada AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 66 BY .79 NO-UNDO.

DEFINE VARIABLE fi-dir-saida AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 66 BY .79 NO-UNDO.

DEFINE VARIABLE rd-sel-campos AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "TODOS", "TODOS",
"Valores", "Valores",
"Bem", "Bem",
"Alocacoes", "Alocacoes"
     SIZE 43.72 BY .75 NO-UNDO.

DEFINE VARIABLE rd-sel-geracao AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "TODOS", "TODOS",
"Valores", "Valores",
"Bem", "Bem",
"Alocacoes", "Alocacoes"
     SIZE 43.57 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113.72 BY 10.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 6.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 2.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 2.

DEFINE RECTANGLE rt_cxcf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 114 BY 1.42.

DEFINE RECTANGLE rt_rgf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 114 BY 1
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-campos FOR 
      tt-tabela SCROLLING.

DEFINE QUERY br-cenar-finalid FOR 
      finalid_utiliz_cenar SCROLLING.

DEFINE QUERY br-geracao FOR 
      tt-geracao SCROLLING.

DEFINE QUERY br-valores FOR 
      tt-val-cenario SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-campos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-campos C-Win _FREEFORM
  QUERY br-campos NO-LOCK DISPLAY
      tt-tabela.tipo                format "x(08)"   column-label "Tipo"
      trim (tt-tabela.titulo)       format "x(20)"   column-label "T¡tulo"
      trim (tt-tabela.formato)      format "x(10)"   column-label "Formato"
      tt-tabela.cod_cenar_ctbl      format "x(10)"   column-label "Cen rio" 
      tt-tabela.cod_finalid_econ    format "x(10)"   column-label "Finalidade"
      tt-tabela.obrigatorio         format "SIM/NAO" column-label "Obrigatorio"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 52.72 BY 8.5
         FONT 1
         TITLE ".: Campos :." FIT-LAST-COLUMN TOOLTIP "Campos".

DEFINE BROWSE br-cenar-finalid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-cenar-finalid C-Win _FREEFORM
  QUERY br-cenar-finalid NO-LOCK DISPLAY
      finalid_utiliz_cenar.cod_empresa          FORMAT "x(3)":U
      finalid_utiliz_cenar.cod_cenar_ctbl       FORMAT "x(8)":U
      finalid_utiliz_cenar.cod_finalid_econ     FORMAT "x(10)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 36.72 BY 5
         FONT 1
         TITLE ".: Cen rio X Finalidade :." FIT-LAST-COLUMN TOOLTIP "Cen rio X Finalidade".

DEFINE BROWSE br-geracao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-geracao C-Win _FREEFORM
  QUERY br-geracao NO-LOCK DISPLAY
      tt-geracao.posicao             FORMAT 999       COLUMN-LABEL "Pos"
      tt-geracao.tipo                format "x(08)"   column-label "Tipo"
      trim (tt-geracao.titulo)       format "x(30)"   column-label "T¡tulo"
      tt-geracao.cod_cenar_ctbl      format "x(10)"   column-label "Cen rio" 
      tt-geracao.cod_finalid_econ    format "x(10)"   column-label "Finalidade"
      trim (tt-geracao.formato)      format "x(10)"   column-label "Formato"
      tt-geracao.obrigatorio         format "SIM/NAO" column-label "Obrigatorio"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 52.72 BY 8.5
         FONT 1
         TITLE ".: Gera‡Æo :." FIT-LAST-COLUMN TOOLTIP "Gera‡Æo".

DEFINE BROWSE br-valores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-valores C-Win _FREEFORM
  QUERY br-valores NO-LOCK DISPLAY
      tt-val-cenario.cod_empresa          FORMAT "x(3)":U
      tt-val-cenario.cod_cenar_ctbl       FORMAT "x(8)":U
      tt-val-cenario.cod_finalid_econ     FORMAT "x(10)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 36.72 BY 5
         FONT 1
         TITLE ".: Valores para Gera‡Æo :." FIT-LAST-COLUMN TOOLTIP "Valores para Gera‡Æo".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f_main
     bt-busca-arq AT ROW 19.83 COL 70.43 WIDGET-ID 154
     br-cenar-finalid AT ROW 2.83 COL 2.29 WIDGET-ID 400
     br-valores AT ROW 2.83 COL 44.86 WIDGET-ID 500
     bt-add-1 AT ROW 4.08 COL 40 WIDGET-ID 38
     bt-del1 AT ROW 5.58 COL 40 WIDGET-ID 40
     rd-sel-campos AT ROW 9.13 COL 10.29 NO-LABEL WIDGET-ID 176
     rd-sel-geracao AT ROW 9.13 COL 69.43 NO-LABEL WIDGET-ID 168
     br-campos AT ROW 10.13 COL 2.29 WIDGET-ID 200
     br-geracao AT ROW 10.13 COL 60.86 WIDGET-ID 300
     bt-add-2 AT ROW 13.17 COL 56 WIDGET-ID 164
     bt-del-2 AT ROW 14.67 COL 56 WIDGET-ID 166
     bt-gera-modelo AT ROW 19.17 COL 104.57 WIDGET-ID 188
     fi-arq-entrada AT ROW 20 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     fi-dir-saida AT ROW 22.33 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 142
     bt-ok AT ROW 24.04 COL 3 WIDGET-ID 26
     bt_cancelar AT ROW 24.04 COL 13.86 WIDGET-ID 36
     Btn_Help AT ROW 24.04 COL 103.86 WIDGET-ID 24
     bt-busca-dir AT ROW 22.17 COL 70.43 WIDGET-ID 156
     "Sele‡Æo:" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 9.21 COL 2.86 WIDGET-ID 182
     "Exemplo de arquivo:" VIEW-AS TEXT
          SIZE 16 BY .54 AT ROW 8.58 COL 2.29 WIDGET-ID 162
     "Arquivo para conversÆo:" VIEW-AS TEXT
          SIZE 20 BY .54 AT ROW 19.08 COL 2.29 WIDGET-ID 148
     "Selecionar Cen rios:" VIEW-AS TEXT
          SIZE 16 BY .54 AT ROW 2.04 COL 2.29 WIDGET-ID 186
     "Diret¢rio para gera‡Æo dos Arquivos:" VIEW-AS TEXT
          SIZE 26 BY .54 AT ROW 21.33 COL 2.29 WIDGET-ID 158
     "Sele‡Æo:" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 9.21 COL 62 WIDGET-ID 174
     rt_rgf AT ROW 1 COL 1 WIDGET-ID 4
     rt_cxcf AT ROW 23.83 COL 1 WIDGET-ID 2
     RECT-8 AT ROW 19.25 COL 1.29 WIDGET-ID 6
     RECT-9 AT ROW 21.58 COL 1.29 WIDGET-ID 144
     RECT-10 AT ROW 8.88 COL 1.29 WIDGET-ID 160
     RECT-11 AT ROW 2.33 COL 1.43 WIDGET-ID 184
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 24.25
         FONT 1
         CANCEL-BUTTON bt_cancelar WIDGET-ID 100.


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
         TITLE              = ".: Gera Arquivos para Importa‡Æo no Ativo Fixo :."
         HEIGHT             = 24.25
         WIDTH              = 114
         MAX-HEIGHT         = 42.42
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 42.42
         VIRTUAL-WIDTH      = 274.29
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
/* SETTINGS FOR FRAME f_main
   FRAME-NAME                                                           */
/* BROWSE-TAB br-cenar-finalid RECT-11 f_main */
/* BROWSE-TAB br-valores br-cenar-finalid f_main */
/* BROWSE-TAB br-campos rd-sel-geracao f_main */
/* BROWSE-TAB br-geracao br-campos f_main */
ASSIGN 
       bt-busca-arq:PRIVATE-DATA IN FRAME f_main     = 
                "Erase".

ASSIGN 
       bt-busca-dir:PRIVATE-DATA IN FRAME f_main     = 
                "Erase".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-campos
/* Query rebuild information for BROWSE br-campos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-tabela NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-campos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-cenar-finalid
/* Query rebuild information for BROWSE br-cenar-finalid
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH finalid_utiliz_cenar NO-LOCK
      WHERE finalid_utiliz_cenar.cod_empresa     = v_cod_empres_usuar
      BY finalid_utiliz_cenar.cod_cenar_ctbl INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "finalid_utiliz_cenar.cod_cenar_ctbl|yes"
     _Where[1]         = "finalid_utiliz_cenar.cod_empresa = v_cod_empres_usuar"
     _Query            is OPENED
*/  /* BROWSE br-cenar-finalid */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-geracao
/* Query rebuild information for BROWSE br-geracao
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-geracao NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-geracao */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-valores
/* Query rebuild information for BROWSE br-valores
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-val-cenario
      WHERE tt-val-cenario.cod_empresa = v_cod_empres_usuar NO-LOCK
    BY tt-val-cenario.cod_cenar_ctbl INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "finalid_utiliz_cenar.cod_cenar_ctbl|yes"
     _Where[1]         = "finalid_utiliz_cenar.cod_empresa = v_cod_empres_usuar"
     _Query            is OPENED
*/  /* BROWSE br-valores */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* .: Gera Arquivos para Importa‡Æo no Ativo Fixo :. */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* .: Gera Arquivos para Importa‡Æo no Ativo Fixo :. */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add-1 C-Win
ON CHOOSE OF bt-add-1 IN FRAME f_main /* Add */
DO:

    DO icont = 1 TO br-cenar-finalid:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
        if br-cenar-finalid:fetch-selected-row(icont) then do:
            IF  br-cenar-finalid:IS-ROW-SELECTED(br-cenar-finalid:FOCUSED-ROW) in frame {&FRAME-NAME} THEN DO:
                IF AVAILABLE finalid_utiliz_cenar THEN DO:
                    FIND FIRST tt-val-cenario OF finalid_utiliz_cenar NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE tt-val-cenario THEN DO:
                        CREATE tt-val-cenario.
                        BUFFER-COPY finalid_utiliz_cenar TO tt-val-cenario.
                    END.
                END.
            END.
        END.
    END.

    CLOSE QUERY br-valores.
    OPEN QUERY br-valores FOR EACH tt-val-cenario NO-LOCK INDEXED-REPOSITION.

    FOR EACH tt-tabela
        WHERE tt-tabela.tipo = "Valores":
        DELETE tt-tabela.
    END.
    FOR EACH tt-geracao
        WHERE tt-geracao.tipo = "Valores":
        DELETE tt-geracao.
    END.

    RUN pi-cria-valores.

    ASSIGN iCont = 0.
    FOR EACH tt-geracao:
        ASSIGN iCont = iCont + 1.
        ASSIGN tt-geracao.posicao = iCont.
    END.

    FOR EACH tt-tabela
        WHERE tt-tabela.obrigatorio = YES
        AND   tt-tabela.tipo = "Valores":

        FOR EACH tt-val-cenario:
            FIND FIRST tt-geracao OF tt-tabela NO-LOCK 
                WHERE tt-geracao.cod_cenar_ctbl   = tt-val-cenario.cod_cenar_ctbl  
                AND   tt-geracao.cod_finalid_econ = tt-val-cenario.cod_finalid_econ
                NO-ERROR.
            IF NOT AVAILABLE tt-geracao THEN DO:
                FIND LAST tt-geracao NO-LOCK NO-ERROR.
                ASSIGN iPosicao = tt-geracao.posicao + 1.
                CREATE tt-geracao.
                BUFFER-COPY tt-tabela TO tt-geracao.
                ASSIGN tt-geracao.posicao          = iPosicao
                       tt-geracao.cod_cenar_ctbl   = tt-val-cenario.cod_cenar_ctbl  
                       tt-geracao.cod_finalid_econ = tt-val-cenario.cod_finalid_econ
                       .
            END.
        END.

        DELETE tt-tabela.
    END.

    APPLY "VALUE-CHANGED" TO rd-sel-campos.
    APPLY "VALUE-CHANGED" TO rd-sel-geracao.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add-2 C-Win
ON CHOOSE OF bt-add-2 IN FRAME f_main /* Add */
DO:

    FIND FIRST tt-val-cenario NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt-val-cenario THEN DO:
        run utp/ut-msgs.p (input "show":U,
                           input 17006,
                           input "Selecione um Cen rio e Finalidade!~~Selecione pelo menos um Cen rio e Finalidade.").
        UNDO, RETURN.
    END.

    DO icont = 1 TO br-campos:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
        if br-campos:fetch-selected-row(icont) then do:
            IF  br-campos:IS-ROW-SELECTED(br-campos:FOCUSED-ROW) in frame {&FRAME-NAME} THEN DO:
                IF AVAILABLE tt-tabela THEN DO:
                    FIND FIRST tt-geracao OF tt-tabela NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE tt-geracao THEN DO:
                        IF tt-tabela.tipo = "Valores" THEN DO:
                            FOR EACH tt-val-cenario:

                                FIND FIRST tt-geracao OF tt-tabela NO-LOCK 
                                    WHERE tt-geracao.cod_cenar_ctbl   = tt-val-cenario.cod_cenar_ctbl  
                                    AND   tt-geracao.cod_finalid_econ = tt-val-cenario.cod_finalid_econ
                                    NO-ERROR.
                                IF NOT AVAILABLE tt-geracao THEN DO:
                                    FIND LAST tt-geracao NO-LOCK NO-ERROR.
                                    ASSIGN iPosicao = tt-geracao.posicao + 1.
                                    CREATE tt-geracao.
                                    BUFFER-COPY tt-tabela TO tt-geracao.
                                    ASSIGN tt-geracao.posicao          = iPosicao
                                           tt-geracao.cod_cenar_ctbl   = tt-val-cenario.cod_cenar_ctbl  
                                           tt-geracao.cod_finalid_econ = tt-val-cenario.cod_finalid_econ
                                           .
                                END.
                            END.
                        END.
                        ELSE DO:
                            FIND LAST tt-geracao NO-LOCK NO-ERROR.
                            ASSIGN iPosicao = tt-geracao.posicao + 1.
                            CREATE tt-geracao.
                            BUFFER-COPY tt-tabela TO tt-geracao.
                            ASSIGN tt-geracao.posicao = iPosicao.
                        END.

                    END.

                    DELETE tt-tabela.
                END.
            END.
        END.
    END.

    APPLY "VALUE-CHANGED" TO rd-sel-campos.
    APPLY "VALUE-CHANGED" TO rd-sel-geracao.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-arq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-arq C-Win
ON CHOOSE OF bt-busca-arq IN FRAME f_main /* Busca Arquivo */
DO:
    system-dialog GET-FILE v_cod_dwb_file
        title "Importar" /*l_importar*/ 
        filters '*.csv'  '*.csv'
        save-as
        create-test-file.
    assign fi-arq-entrada:screen-value in frame {&FRAME-NAME} = v_cod_dwb_file.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-dir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-dir C-Win
ON CHOOSE OF bt-busca-dir IN FRAME f_main /* Busca Diretorio */
DO:

    SYSTEM-DIALOG GET-DIR v_cod_dwb_file
        INITIAL-DIR SESSION:TEMP-DIRECTORY.

    assign fi-dir-saida:screen-value in frame {&FRAME-NAME} = v_cod_dwb_file + "\".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del-2 C-Win
ON CHOOSE OF bt-del-2 IN FRAME f_main /* Del */
DO:
    DO icont = 1 TO br-geracao:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
        if br-geracao:fetch-selected-row(icont) then do:
            IF  br-geracao:IS-ROW-SELECTED(br-geracao:FOCUSED-ROW) in frame {&FRAME-NAME} THEN DO:
                IF AVAILABLE tt-geracao AND tt-geracao.obrigatorio = NO THEN DO:

                    FIND FIRST tt-tabela OF tt-geracao NO-LOCK 
                        WHERE tt-tabela.posicao          = 0
                        AND   tt-tabela.cod_cenar_ctbl   = ""
                        AND   tt-tabela.cod_finalid_econ = ""
                        NO-ERROR.
                    IF NOT AVAILABLE tt-tabela THEN DO:
                        CREATE tt-tabela.
                        BUFFER-COPY tt-geracao TO tt-tabela.
                        ASSIGN tt-tabela.posicao          = 0
                               tt-tabela.cod_cenar_ctbl   = ""
                               tt-tabela.cod_finalid_econ = ""
                               .
                    END.

                    FIND CURRENT tt-geracao EXCLUSIVE-LOCK NO-ERROR.
                    DELETE tt-geracao.

                    FOR EACH tt-geracao
                        WHERE tt-geracao.tipo  = tt-tabela.tipo
                        AND   tt-geracao.campo = tt-tabela.campo:
                        DELETE tt-geracao.
                    END.

                END.
            END.
        END.
    END.

    ASSIGN iCont = 0.
    FOR EACH tt-geracao:
        ASSIGN iCont = iCont + 1.
        ASSIGN tt-geracao.posicao = iCont.
    END.

    APPLY "VALUE-CHANGED" TO rd-sel-campos.
    APPLY "VALUE-CHANGED" TO rd-sel-geracao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del1 C-Win
ON CHOOSE OF bt-del1 IN FRAME f_main /* Del */
DO:
    DO icont = 1 TO br-valores:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
        if br-valores:fetch-selected-row(icont) then do:
            IF  br-valores:IS-ROW-SELECTED(br-valores:FOCUSED-ROW) in frame {&FRAME-NAME} THEN DO:
                IF AVAILABLE tt-val-cenario THEN DO:

                    FOR EACH tt-geracao
                        WHERE tt-geracao.cod_cenar_ctbl   = tt-val-cenario.cod_cenar_ctbl
                        AND   tt-geracao.cod_finalid_econ = tt-val-cenario.cod_finalid_econ:
                        DELETE tt-geracao.
                    END.

                    FIND CURRENT tt-val-cenario EXCLUSIVE-LOCK NO-ERROR.
                    DELETE tt-val-cenario.
                END.
            END.
        END.
    END.

    ASSIGN iCont = 0.
    FOR EACH tt-geracao:
        ASSIGN iCont = iCont + 1.
        ASSIGN tt-geracao.posicao = iCont.
    END.

    CLOSE QUERY br-valores.
    OPEN QUERY br-valores FOR EACH tt-val-cenario NO-LOCK INDEXED-REPOSITION.

    APPLY "VALUE-CHANGED" TO rd-sel-campos.
    APPLY "VALUE-CHANGED" TO rd-sel-geracao.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-gera-modelo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-gera-modelo C-Win
ON CHOOSE OF bt-gera-modelo IN FRAME f_main /* Gera Modelo */
DO:

    OUTPUT TO VALUE (session:TEMP-DIRECTORY + "esfas003.csv") NO-CONVERT.

    FOR EACH tt-geracao:
        PUT UNFORMAT TRIM (tt-geracao.titulo).
        IF tt-geracao.tipo = "Valores" THEN 
            PUT UNFORMAT " " tt-geracao.cod_cenar + "-" + tt-geracao.cod_finalid.
        PUT UNFORMAT ";".
    END.
    PUT SKIP.

    OUTPUT CLOSE.

    OS-COMMAND NO-WAIT VALUE (session:TEMP-DIRECTORY + "esfas003.csv") NO-ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok C-Win
ON CHOOSE OF bt-ok IN FRAME f_main /* Gerar */
DO:
    DEFINE VARIABLE iErro AS INTEGER NO-UNDO.

    FIND FIRST tt-val-cenario NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt-val-cenario THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Selecione um Cen rio X Finalidade!~~Selecione pelo menos um Cen rio X Finalidade.").
        APPLY "ENTRY" TO br-cenar-finalid.
        RETURN NO-APPLY.
    END.

    IF INPUT FRAME {&FRAME-NAME} fi-arq-entrada = "" THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Arquivo de entrada em branco!~~Informe um arquivo CSV para gera‡Æo dos arquivos do Ativo Fixo.").
        APPLY "ENTRY" TO fi-arq-entrada.
        RETURN NO-APPLY.
    END.

    IF INDEX (INPUT FRAME {&FRAME-NAME} fi-arq-entrada,'CSV') = 0 THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Arquivo de entrada nÆo ‚ um CSV!~~Informe um arquivo CSV para gera‡Æo dos arquivos do Ativo Fixo.").
        APPLY "ENTRY" TO fi-arq-entrada.
        RETURN NO-APPLY.
    END.

    IF SEARCH (INPUT FRAME {&FRAME-NAME} fi-arq-entrada) = ? THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Arquivo de entrada nÆo encontrado!~~Informe um arquivo CSV para gera‡Æo dos arquivos do Ativo Fixo.").
        APPLY "ENTRY" TO fi-arq-entrada.
        RETURN NO-APPLY.
    END.

    IF INPUT FRAME {&FRAME-NAME} fi-dir-saida = "" THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Diret¢rio de sa¡da em branco!~~Informe um diret¢rio para gera‡Æo dos arquivos do Ativo Fixo.").
        APPLY "ENTRY" TO fi-dir-saida.
        RETURN NO-APPLY.
    END.

    ASSIGN iCont = 0
           iErro = 0.
    INPUT FROM VALUE(SEARCH(INPUT FRAME {&FRAME-NAME} fi-arq-entrada)) NO-CONVERT NO-ECHO.
    REPEAT:
        IMPORT UNFORMAT cLinha.

        ASSIGN iCont = iCont + 1.

        IF iCont = 1 AND NOT cLinha BEGINS "Empresa" THEN DO:
            ASSIGN iErro = 1.
            LEAVE.
        END.

        FIND LAST tt-geracao NO-LOCK NO-ERROR.
        IF tt-geracao.posicao <> NUM-ENTRIES(cLinha,";") THEN DO:
            ASSIGN iErro = 2.
            LEAVE.
        END.

    END.
    INPUT CLOSE.

    IF iErro = 1 THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Cabe‡alho do Arquivo incorreto!~~O arquivo informado nÆo possui o cabe‡alho correto para importa‡Æo.").
        RETURN NO-APPLY.
    END.
    IF iErro = 2 THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Quantidade de colunas incorreto!~~A quantidade de colunas do arquivo informado nÆo confere com o Layout de gera‡Æo. Erro na linha: " + STRING (iCont)).
        RETURN NO-APPLY.
    END.

    do on error undo, return no-apply:
        run pi_executar.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME f_main /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_cancelar C-Win
ON CHOOSE OF bt_cancelar IN FRAME f_main /* Fechar */
DO:
  apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-arq-entrada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arq-entrada C-Win
ON LEAVE OF fi-arq-entrada IN FRAME f_main
DO:

    if index (SELF:screen-value in frame {&frame-name},'CSV') = 0 then do:
        assign SELF:screen-value in frame {&frame-name} = session:temp-directory + "Bens.csv".
    end.
    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sel-campos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sel-campos C-Win
ON VALUE-CHANGED OF rd-sel-campos IN FRAME f_main
DO:
    CLOSE QUERY br-campos.

    IF SELF:SCREEN-VALUE = "TODOS" THEN
        OPEN QUERY br-campos FOR EACH tt-tabela NO-LOCK INDEXED-REPOSITION.
    ELSE
        OPEN QUERY br-campos FOR EACH tt-tabela NO-LOCK 
            WHERE tt-tabela.tipo = SELF:SCREEN-VALUE INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sel-geracao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sel-geracao C-Win
ON VALUE-CHANGED OF rd-sel-geracao IN FRAME f_main
DO:
    CLOSE QUERY br-geracao.

    IF SELF:SCREEN-VALUE = "TODOS" THEN
        OPEN QUERY br-geracao FOR EACH tt-geracao NO-LOCK INDEXED-REPOSITION.
    ELSE
        OPEN QUERY br-geracao FOR EACH tt-geracao NO-LOCK 
            WHERE tt-geracao.tipo = SELF:SCREEN-VALUE INDEXED-REPOSITION.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-campos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

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

    {include/i_dbinst.i}
    {include/i_dbtype.i}
    {include/i_fcldef.i}

    {include/i_fclwin.i c-win}
    {include/i_fclfrm.i f_main }

  RUN enable_UI.
  
  run pi_inicio.

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
  DISPLAY rd-sel-campos rd-sel-geracao fi-arq-entrada fi-dir-saida 
      WITH FRAME f_main IN WINDOW C-Win.
  ENABLE bt-busca-arq rt_rgf rt_cxcf RECT-8 RECT-9 RECT-10 RECT-11 
         br-cenar-finalid br-valores bt-add-1 bt-del1 rd-sel-campos 
         rd-sel-geracao br-campos br-geracao bt-add-2 bt-del-2 bt-gera-modelo 
         fi-arq-entrada fi-dir-saida bt-ok bt_cancelar Btn_Help bt-busca-dir 
      WITH FRAME f_main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f_main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-arquivo C-Win 
PROCEDURE pi-carrega-arquivo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    empty temp-table tt-bem-pat.
    empty temp-table tt-valores.
    empty temp-table tt-alocacoes.

    assign i-cont  = 0.

    INPUT FROM VALUE(SEARCH(INPUT FRAME {&FRAME-NAME} fi-arq-entrada)) NO-CONVERT NO-ECHO.

    REPEAT:

        IMPORT UNFORMAT cLinha.

        IF cLinha BEGINS "Empresa" THEN NEXT.

        RUN pi-acompanhar IN h-acomp (INPUT "Processando: " + STRING (i-cont)).

        ASSIGN i-cont = i-cont + 1.

        create tt-bem-pat.
        assign tt-bem-pat.nr-seq-bem = i-cont.

        create tt-alocacoes.
        assign tt-alocacoes.nr-seq-bem = i-cont.

        FOR EACH tt-geracao:
            RUN pi-carrega-bem-pat.

            IF tt-geracao.tipo = "Alocacoes" AND tt-geracao.campo = "tt-alocacoes.cod-plano-ccusto" THEN ASSIGN tt-alocacoes.cod-plano-ccusto = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
            IF tt-geracao.tipo = "Alocacoes" AND tt-geracao.campo = "tt-alocacoes.cod-ccusto"       THEN ASSIGN tt-alocacoes.cod-ccusto       = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
            IF tt-geracao.tipo = "Alocacoes" AND tt-geracao.campo = "tt-alocacoes.unid-negoc"       THEN ASSIGN tt-alocacoes.unid-negoc       = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
            IF tt-geracao.tipo = "Alocacoes" AND tt-geracao.campo = "tt-alocacoes.perc-apropriacao" THEN ASSIGN tt-alocacoes.perc-apropriacao = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
            IF tt-geracao.tipo = "Alocacoes" AND tt-geracao.campo = "tt-alocacoes.ccusto-un-princ"  THEN ASSIGN tt-alocacoes.ccusto-un-princ  = LOGICAL (ENTRY (tt-geracao.posicao,cLinha,";")).
        END.

        FOR EACH tt-val-cenario:
            CREATE tt-valores.
            ASSIGN tt-valores.nr-seq-bem       = i-cont
                   tt-valores.cenario-contabil = '"' + tt-val-cenario.cod_cenar_ctbl   + '"'
                   tt-valores.finalidade       = '"' + tt-val-cenario.cod_finalid_econ + '"'.

            FOR EACH tt-geracao
                WHERE tt-geracao.cod_cenar_ctbl   = tt-val-cenario.cod_cenar_ctbl
                AND   tt-geracao.cod_finalid_econ = tt-val-cenario.cod_finalid_econ:
                RUN pi-carrega-valores.
            END.
        END.
    END.

    INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-bem-pat C-Win 
PROCEDURE pi-carrega-bem-pat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_empresa"               THEN ASSIGN tt-bem-pat.cod_empresa               = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_cta_pat"               THEN ASSIGN tt-bem-pat.cod_cta_pat               = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.num_bem_pat"               THEN ASSIGN tt-bem-pat.num_bem_pat               = INTEGER (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.num_seq_bem_pat"           THEN ASSIGN tt-bem-pat.num_seq_bem_pat           = INTEGER (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.des_bem_pat"               THEN ASSIGN tt-bem-pat.des_bem_pat               = '"' + TRIM (SUBSTRING (ENTRY (tt-geracao.posicao,cLinha,";"),1,40)) + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cb3_ident_visual"          THEN ASSIGN tt-bem-pat.cb3_ident_visual          = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.qtd_bem_pat_represen"      THEN ASSIGN tt-bem-pat.qtd_bem_pat_represen      = INTEGER (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.ind_periodic_invent"       THEN ASSIGN tt-bem-pat.ind_periodic_invent       = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.dat_aquis_bem_pat"         THEN ASSIGN tt-bem-pat.dat_aquis_bem_pat         = DATE (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_estab"                 THEN ASSIGN tt-bem-pat.cod_estab                 = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_espec_bem"             THEN ASSIGN tt-bem-pat.cod_espec_bem             = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_marca"                 THEN ASSIGN tt-bem-pat.cod_marca                 = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod-modelo"                THEN ASSIGN tt-bem-pat.cod-modelo                = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_licenc_uso"            THEN ASSIGN tt-bem-pat.cod_licenc_uso            = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_especif_tec"           THEN ASSIGN tt-bem-pat.cod_especif_tec           = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_estado_fisic_bem_pat"  THEN ASSIGN tt-bem-pat.cod_estado_fisic_bem_pat  = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_arrendador"            THEN ASSIGN tt-bem-pat.cod_arrendador            = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_contrat_leas"          THEN ASSIGN tt-bem-pat.cod_contrat_leas          = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cdn_fornecedor"            THEN ASSIGN tt-bem-pat.cdn_fornecedor            = INTEGER (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_localiz"               THEN ASSIGN tt-bem-pat.cod_localiz               = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_usuario"               THEN ASSIGN tt-bem-pat.cod_usuario               = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.dat_ult_invent"            THEN ASSIGN tt-bem-pat.dat_ult_invent            = DATE (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.des_narrat_bem_pat"        THEN ASSIGN tt-bem-pat.des_narrat_bem_pat        = '"' + TRIM (SUBSTRING (ENTRY (tt-geracao.posicao,cLinha,";"),1,2000)) + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.seguradora"                THEN ASSIGN tt-bem-pat.seguradora                = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.apolice-seguro"            THEN ASSIGN tt-bem-pat.apolice-seguro            = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.dat_ini_valid_apolice"     THEN ASSIGN tt-bem-pat.dat_ini_valid_apolice     = DATE (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.dat_fim_valid_apolice"     THEN ASSIGN tt-bem-pat.dat_fim_valid_apolice     = DATE (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.premio-seguro"             THEN ASSIGN tt-bem-pat.premio-seguro             = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.seguradora1"               THEN ASSIGN tt-bem-pat.seguradora1               = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.apolice-seguro1"           THEN ASSIGN tt-bem-pat.apolice-seguro1           = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.dat_ini_valid_apolice1"    THEN ASSIGN tt-bem-pat.dat_ini_valid_apolice1    = DATE (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.dat_fim_valid_apolice1"    THEN ASSIGN tt-bem-pat.dat_fim_valid_apolice1    = DATE (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.premio-seguro1"            THEN ASSIGN tt-bem-pat.premio-seguro1            = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.seguradora2"               THEN ASSIGN tt-bem-pat.seguradora2               = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.apolice-seguro2"           THEN ASSIGN tt-bem-pat.apolice-seguro2           = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.dat_ini_valid_apolice2"    THEN ASSIGN tt-bem-pat.dat_ini_valid_apolice2    = DATE (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.dat_fim_valid_apolice2"    THEN ASSIGN tt-bem-pat.dat_fim_valid_apolice2    = DATE (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.premio-seguro2"            THEN ASSIGN tt-bem-pat.premio-seguro2            = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_docto_entr"            THEN ASSIGN tt-bem-pat.cod_docto_entr            = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.num_item_docto_entr"       THEN ASSIGN tt-bem-pat.num_item_docto_entr       = INTEGER (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.num_pessoa_jurid_gartia"   THEN ASSIGN tt-bem-pat.num_pessoa_jurid_gartia   = INTEGER (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.dat_inic_valid_gartia_bem" THEN ASSIGN tt-bem-pat.dat_inic_valid_gartia_bem = DATE (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.dat_fim_valid_gartia_bem"  THEN ASSIGN tt-bem-pat.dat_fim_valid_gartia_bem  = DATE (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.des_termo_gartia_bem_pat"  THEN ASSIGN tt-bem-pat.des_termo_gartia_bem_pat  = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_grp_calc"              THEN ASSIGN tt-bem-pat.cod_grp_calc              = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.dat-movto"                 THEN ASSIGN tt-bem-pat.dat-movto                 = DATE (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.val_perc_bxa"              THEN ASSIGN tt-bem-pat.val_perc_bxa              = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.dat-ini-calc-dpr"          THEN ASSIGN tt-bem-pat.dat-ini-calc-dpr          = DATE (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.dat_calc_pat"              THEN ASSIGN tt-bem-pat.dat_calc_pat              = DATE (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.cod_ser_nota"              THEN ASSIGN tt-bem-pat.cod_ser_nota              = '"' + ENTRY (tt-geracao.posicao,cLinha,";") + '"'.
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.bem-importado"             THEN ASSIGN tt-bem-pat.bem-importado             = LOGICAL (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.credita-PIS"               THEN ASSIGN tt-bem-pat.credita-PIS               = LOGICAL (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.credita-COFINS"            THEN ASSIGN tt-bem-pat.credita-COFINS            = LOGICAL (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.nr-parc-cred-PIS-COFINS"   THEN ASSIGN tt-bem-pat.nr-parc-cred-PIS-COFINS   = INTEGER (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.parc-descontadas"          THEN ASSIGN tt-bem-pat.parc-descontadas          = INTEGER (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.val-credito-PIS"           THEN ASSIGN tt-bem-pat.val-credito-PIS           = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.val-credito-COFINS"        THEN ASSIGN tt-bem-pat.val-credito-COFINS        = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.credita-CSLL"              THEN ASSIGN tt-bem-pat.credita-CSLL              = LOGICAL (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.exec-credito-CSLL"         THEN ASSIGN tt-bem-pat.exec-credito-CSLL         = INTEGER (ENTRY (tt-geracao.posicao,cLinha,";")).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.val-base-PIS"              THEN ASSIGN tt-bem-pat.val-base-PIS              = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
IF tt-geracao.tipo = "Bem" AND tt-geracao.campo = "tt-bem-pat.val-base-COFINS"           THEN ASSIGN tt-bem-pat.val-base-COFINS           = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-valores C-Win 
PROCEDURE pi-carrega-valores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.val-orig"           THEN ASSIGN tt-valores.val-orig           = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.correcao-monet"     THEN ASSIGN tt-valores.correcao-monet     = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.dpr-val-orig"       THEN ASSIGN tt-valores.dpr-val-orig       = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.dpr-correcao-monet" THEN ASSIGN tt-valores.dpr-correcao-monet = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.correcao-monet-dpr" THEN ASSIGN tt-valores.correcao-monet-dpr = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.dpr-incentivada"    THEN ASSIGN tt-valores.dpr-incentivada    = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.dpr-incentiv-cm"    THEN ASSIGN tt-valores.dpr-incentiv-cm    = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.cm-dpr-incentiv"    THEN ASSIGN tt-valores.cm-dpr-incentiv    = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.amort-vo"           THEN ASSIGN tt-valores.amort-vo           = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.amort-cm"           THEN ASSIGN tt-valores.amort-cm           = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.cm-amort"           THEN ASSIGN tt-valores.cm-amort           = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.amort-incentiv"     THEN ASSIGN tt-valores.amort-incentiv     = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.amort-incentiv-cm"  THEN ASSIGN tt-valores.amort-incentiv-cm  = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.cm-amort-incentiv"  THEN ASSIGN tt-valores.cm-amort-incentiv  = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.perc-dpr"           THEN ASSIGN tt-valores.perc-dpr           = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.perc-dpr-incentiv"  THEN ASSIGN tt-valores.perc-dpr-incentiv  = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).
    IF tt-geracao.tipo = "Valores" AND tt-geracao.campo = "tt-valores.perc-dpr-red-saldo" THEN ASSIGN tt-valores.perc-dpr-red-saldo = TRUNCATE (DECIMAL (ENTRY (tt-geracao.posicao,cLinha,";")),2).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-alocacoes C-Win 
PROCEDURE pi-cria-alocacoes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Alocacoes" tt-tabela.campo = "tt-alocacoes.cod-plano-ccusto" tt-tabela.formato = "x(8)      " tt-tabela.titulo = "Plano Centros Custo" tt-tabela.obrigatorio = yes.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Alocacoes" tt-tabela.campo = "tt-alocacoes.cod-ccusto"       tt-tabela.formato = "x(11)     " tt-tabela.titulo = "Centro Custo       " tt-tabela.obrigatorio = yes.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Alocacoes" tt-tabela.campo = "tt-alocacoes.unid-negoc"       tt-tabela.formato = "x(3)      " tt-tabela.titulo = "Unid Neg¢cio       " tt-tabela.obrigatorio = yes.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Alocacoes" tt-tabela.campo = "tt-alocacoes.perc-apropriacao" tt-tabela.formato = "9(06),9999" tt-tabela.titulo = "Perc Apropria‡Æo   " tt-tabela.obrigatorio = NO.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Alocacoes" tt-tabela.campo = "tt-alocacoes.ccusto-un-princ"  tt-tabela.formato = "yes/no    " tt-tabela.titulo = "CCusto/UN Principal" tt-tabela.obrigatorio = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-bem-pat C-Win 
PROCEDURE pi-cria-bem-pat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_empresa"               tt-tabela.formato = "x(3)      " tt-tabela.titulo = "Empresa                        " tt-tabela.obrigatorio = yes.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_cta_pat"               tt-tabela.formato = "x(18)     " tt-tabela.titulo = "Conta Patrimonial              " tt-tabela.obrigatorio = yes.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.num_bem_pat"               tt-tabela.formato = "9(09)     " tt-tabela.titulo = "Bem Patrimonial                " tt-tabela.obrigatorio = yes.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.num_seq_bem_pat"           tt-tabela.formato = "9(05)     " tt-tabela.titulo = "Sequˆncia Bem                  " tt-tabela.obrigatorio = yes.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.des_bem_pat"               tt-tabela.formato = "x(40)     " tt-tabela.titulo = "Descri‡Æo Bem Pat              " tt-tabela.obrigatorio = yes.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cb3_ident_visual"          tt-tabela.formato = "x(20)     " tt-tabela.titulo = "N£mero Plaqueta                " tt-tabela.obrigatorio = NO.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.qtd_bem_pat_represen"      tt-tabela.formato = "9(08)     " tt-tabela.titulo = "Quantidade Bens Representados  " tt-tabela.obrigatorio = yes.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.ind_periodic_invent"       tt-tabela.formato = "x(14)     " tt-tabela.titulo = "Periodicidade                  " tt-tabela.obrigatorio = NO.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.dat_aquis_bem_pat"         tt-tabela.formato = "99/99/9999" tt-tabela.titulo = "Data Aquisi‡Æo                 " tt-tabela.obrigatorio = yes.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_estab"                 tt-tabela.formato = "x(3)      " tt-tabela.titulo = "Estabelecimento                " tt-tabela.obrigatorio = yes.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_espec_bem"             tt-tabela.formato = "x(6)      " tt-tabela.titulo = "Esp‚cie Bem Patrimonial        " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_marca"                 tt-tabela.formato = "x(6)      " tt-tabela.titulo = "Marca                          " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod-modelo"                tt-tabela.formato = "x(8)      " tt-tabela.titulo = "Modelo                         " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_licenc_uso"            tt-tabela.formato = "x(12)     " tt-tabela.titulo = "Licen‡a Uso                    " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_especif_tec"           tt-tabela.formato = "x(8)      " tt-tabela.titulo = "Especifica‡Æo T‚cnica          " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_estado_fisic_bem_pat"  tt-tabela.formato = "x(8)      " tt-tabela.titulo = "Estado F¡sico                  " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_arrendador"            tt-tabela.formato = "x(6)      " tt-tabela.titulo = "Arrendador                     " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_contrat_leas"          tt-tabela.formato = "x(12)     " tt-tabela.titulo = "Contrato Leasing               " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cdn_fornecedor"            tt-tabela.formato = "9(06)     " tt-tabela.titulo = "Fornecedor                     " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_localiz"               tt-tabela.formato = "x(12)     " tt-tabela.titulo = "Localiza‡Æo                    " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_usuario"               tt-tabela.formato = "x(12)     " tt-tabela.titulo = "Respons vel                    " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.dat_ult_invent"            tt-tabela.formato = "99/99/9999" tt-tabela.titulo = "éltimo Invent rio              " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.des_narrat_bem_pat"        tt-tabela.formato = "x(2000)   " tt-tabela.titulo = "Narrativa Bem                  " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.seguradora"                tt-tabela.formato = "x(8)      " tt-tabela.titulo = "Seguradora                     " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.apolice-seguro"            tt-tabela.formato = "x(12)     " tt-tabela.titulo = "Ap¢lice Seguro                 " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.dat_ini_valid_apolice"     tt-tabela.formato = "99/99/9999" tt-tabela.titulo = "In¡cio Valid Ap¢lice           " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.dat_fim_valid_apolice"     tt-tabela.formato = "99/99/9999" tt-tabela.titulo = "Fim Validade Ap¢lice           " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.premio-seguro"             tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Prˆmio Seguro                  " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.seguradora1"               tt-tabela.formato = "x(8)      " tt-tabela.titulo = "Seguradora (1)                 " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.apolice-seguro1"           tt-tabela.formato = "x(12)     " tt-tabela.titulo = "Ap¢lice Seguro (1)             " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.dat_ini_valid_apolice1"    tt-tabela.formato = "99/99/9999" tt-tabela.titulo = "In¡cio Valid Ap¢lice (1)       " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.dat_fim_valid_apolice1"    tt-tabela.formato = "99/99/9999" tt-tabela.titulo = "Fim Validade Ap¢lice (1)       " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.premio-seguro1"            tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Prˆmio Seguro (1)              " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.seguradora2"               tt-tabela.formato = "x(8)      " tt-tabela.titulo = "Seguradora (2)                 " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.apolice-seguro2"           tt-tabela.formato = "x(12)     " tt-tabela.titulo = "Ap¢lice Seguro (2)             " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.dat_ini_valid_apolice2"    tt-tabela.formato = "99/99/9999" tt-tabela.titulo = "In¡cio Valid Ap¢lice (2)       " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.dat_fim_valid_apolice2"    tt-tabela.formato = "99/99/9999" tt-tabela.titulo = "Fim Validade Ap¢lice (2)       " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.premio-seguro2"            tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Prˆmio Seguro (2)              " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_docto_entr"            tt-tabela.formato = "x(16)     " tt-tabela.titulo = "Docto Entrada                  " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.num_item_docto_entr"       tt-tabela.formato = "9(06)     " tt-tabela.titulo = "Numero Item                    " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.num_pessoa_jurid_gartia"   tt-tabela.formato = "9(09)     " tt-tabela.titulo = "Pessoa Garantia                " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.dat_inic_valid_gartia_bem" tt-tabela.formato = "99/99/9999" tt-tabela.titulo = "Inicio Garantia                " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.dat_fim_valid_gartia_bem"  tt-tabela.formato = "99/99/9999" tt-tabela.titulo = "Fim Garantia                   " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.des_termo_gartia_bem_pat"  tt-tabela.formato = "x(2000)   " tt-tabela.titulo = "Termo Garantia                 " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_grp_calc"              tt-tabela.formato = "x(6)      " tt-tabela.titulo = "Grupo C lculo                  " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.dat-movto"                 tt-tabela.formato = "99/99/9999" tt-tabela.titulo = "Data Movimento                 " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.val_perc_bxa"              tt-tabela.formato = "9(3),99   " tt-tabela.titulo = "Perc Baixado                   " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.dat-ini-calc-dpr"          tt-tabela.formato = "99/99/9999" tt-tabela.titulo = "In¡cio C lculo Dpr             " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.dat_calc_pat"              tt-tabela.formato = "99/99/9999" tt-tabela.titulo = "Data C lculo                   " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.cod_ser_nota"              tt-tabela.formato = "x(5)      " tt-tabela.titulo = "S‚rie Nota                     " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.bem-importado"             tt-tabela.formato = "yes/no    " tt-tabela.titulo = "Bem Importado                  " tt-tabela.obrigatorio = NO.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.credita-PIS"               tt-tabela.formato = "yes/no    " tt-tabela.titulo = "Credita PIS                    " tt-tabela.obrigatorio = NO.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.credita-COFINS"            tt-tabela.formato = "yes/no    " tt-tabela.titulo = "Credita COFINS                 " tt-tabela.obrigatorio = NO.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.nr-parc-cred-PIS-COFINS"   tt-tabela.formato = "9(02)     " tt-tabela.titulo = "Nro Parcelas Cr‚dito PIS/COFINS" tt-tabela.obrigatorio = NO.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.parc-descontadas"          tt-tabela.formato = "9(02)     " tt-tabela.titulo = "Parcelas Descontadas           " tt-tabela.obrigatorio = NO.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.val-credito-PIS"           tt-tabela.formato = "9(9),99   " tt-tabela.titulo = "Valor Cr‚dito PIS              " tt-tabela.obrigatorio = NO.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.val-credito-COFINS"        tt-tabela.formato = "9(9),99   " tt-tabela.titulo = "Valor Cr‚dito COFINS           " tt-tabela.obrigatorio = no.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.credita-CSLL"              tt-tabela.formato = "yes/no    " tt-tabela.titulo = "Credita CSLL                   " tt-tabela.obrigatorio = NO.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.exec-credito-CSLL"         tt-tabela.formato = "9(02)     " tt-tabela.titulo = "Exerc¡cios Cr‚dito CSLL        " tt-tabela.obrigatorio = NO.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.val-base-PIS"              tt-tabela.formato = "9(9),99   " tt-tabela.titulo = "Valor Base PIS                 " tt-tabela.obrigatorio = no.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Bem" tt-tabela.campo = "tt-bem-pat.val-base-COFINS"           tt-tabela.formato = "9(9),99   " tt-tabela.titulo = "Valor Base COFINS              " tt-tabela.obrigatorio = no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-valores C-Win 
PROCEDURE pi-cria-valores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.seq-incorp"         tt-tabela.formato = "9(08)     " tt-tabela.titulo = "Sequˆncia Incorp      " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.cenario-contabil"   tt-tabela.formato = "x(8)      " tt-tabela.titulo = "Cen rio Cont bil      " tt-tabela.obrigatorio = yes.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.finalidade"         tt-tabela.formato = "x(10)     " tt-tabela.titulo = "Finalidade            " tt-tabela.obrigatorio = yes.
    */

    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.val-orig"           tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Valor Original        " tt-tabela.obrigatorio = YES.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.correcao-monet"     tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Corre‡Æo Monet ria    " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.dpr-val-orig"       tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Dpr Valor Original    " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.dpr-correcao-monet" tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Dpr Corre‡Æo Monet    " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.correcao-monet-dpr" tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Corre‡Æo Monet Dpr    " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.dpr-incentivada"    tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Deprecia‡Æo Incentiv  " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.dpr-incentiv-cm"    tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Dpr Incentiv CM       " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.cm-dpr-incentiv"    tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "CM Dpr Incentivada    " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.amort-vo"           tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Amortiza‡Æo VO        " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.amort-cm"           tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Amortiza‡Æo CM        " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.cm-amort"           tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "CM Amortiza‡Æo        " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.amort-incentiv"     tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Amortizacao Incentiv  " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.amort-incentiv-cm"  tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "Amort Incentiv CM     " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.cm-amort-incentiv"  tt-tabela.formato = "9(12),99  " tt-tabela.titulo = "CM Amort Incentvda    " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.perc-dpr"           tt-tabela.formato = "9(5),9999 " tt-tabela.titulo = "Percentual Dpr        " tt-tabela.obrigatorio = YES.
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.perc-dpr-incentiv"  tt-tabela.formato = "9(5),9999 " tt-tabela.titulo = "Perc Dpr Incentivada  " tt-tabela.obrigatorio = no .
    CREATE tt-tabela. ASSIGN tt-tabela.tipo = "Valores" tt-tabela.campo = "tt-valores.perc-dpr-red-saldo" tt-tabela.formato = "9(05),9999" tt-tabela.titulo = "Perc Dpr Redu‡Æo Saldo" tt-tabela.obrigatorio = no .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_executar C-Win 
PROCEDURE pi_executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp (input "Inicializando").

    run pi-carrega-arquivo.

    output stream saida1 to VALUE (INPUT FRAME {&FRAME-NAME} fi-dir-saida + 'bens.dat').
    output stream saida2 to VALUE (INPUT FRAME {&FRAME-NAME} fi-dir-saida + 'valores.dat').
    output stream saida3 to VALUE (INPUT FRAME {&FRAME-NAME} fi-dir-saida + 'alocacoes.dat').

    for each tt-bem-pat use-index cod-seq no-lock:

        RUN pi-acompanhar IN h-acomp (INPUT "Gerando arquivo Bens: " + STRING (tt-bem-pat.num_bem_pat)).

        put stream saida1 unformatted
            trim(string(tt-bem-pat.nr-seq-bem                )) space
            trim(string(tt-bem-pat.cod_empresa               )) space
            trim(string(tt-bem-pat.cod_cta_pat               )) space
            trim(string(tt-bem-pat.num_bem_pat               )) space
            trim(string(tt-bem-pat.num_seq_bem_pat           )) space
            trim(string(tt-bem-pat.des_bem_pat               )) space
            trim(string(tt-bem-pat.cb3_ident_visual          )) space
            trim(string(tt-bem-pat.qtd_bem_pat_represen      )) space
            trim(string(tt-bem-pat.ind_periodic_invent       )) space
            trim(string(tt-bem-pat.dat_aquis_bem_pat         )) space
            trim(string(tt-bem-pat.cod_estab                 )) space
            trim(string(tt-bem-pat.cod_espec_bem             )) space
            trim(string(tt-bem-pat.cod_marca                 )) space
            trim(string(tt-bem-pat.cod-modelo                )) space
            trim(string(tt-bem-pat.cod_licenc_uso            )) space
            trim(string(tt-bem-pat.cod_especif_tec           )) space
            trim(string(tt-bem-pat.cod_estado_fisic_bem_pat  )) space
            trim(string(tt-bem-pat.cod_arrendador            )) space
            trim(string(tt-bem-pat.cod_contrat_leas          )) space
            trim(string(tt-bem-pat.cdn_fornecedor            )) space
            trim(string(tt-bem-pat.cod_localiz               )) space
            trim(string(tt-bem-pat.cod_usuario               )) space
            trim(string(tt-bem-pat.dat_ult_invent            )) space
            trim(string(tt-bem-pat.des_narrat_bem_pat        )) space
            trim(string(tt-bem-pat.seguradora                )) space
            trim(string(tt-bem-pat.apolice-seguro            )) space
            trim(string(tt-bem-pat.dat_ini_valid_apolice     )) space
            trim(string(tt-bem-pat.dat_fim_valid_apolice     )) space
            trim(string(tt-bem-pat.premio-seguro             )) space
            trim(string(tt-bem-pat.seguradora1               )) space
            trim(string(tt-bem-pat.apolice-seguro1           )) space
            trim(string(tt-bem-pat.dat_ini_valid_apolice1    )) space
            trim(string(tt-bem-pat.dat_fim_valid_apolice1    )) space
            trim(string(tt-bem-pat.premio-seguro1            )) space
            trim(string(tt-bem-pat.seguradora2               )) space
            trim(string(tt-bem-pat.apolice-seguro2           )) space
            trim(string(tt-bem-pat.dat_ini_valid_apolice2    )) space
            trim(string(tt-bem-pat.dat_fim_valid_apolice2    )) space
            trim(string(tt-bem-pat.premio-seguro2            )) space
            trim(string(tt-bem-pat.cod_docto_entr            )) space
            trim(string(tt-bem-pat.num_item_docto_entr       )) space
            trim(string(tt-bem-pat.num_pessoa_jurid_gartia   )) space
            trim(string(tt-bem-pat.dat_inic_valid_gartia_bem )) space
            trim(string(tt-bem-pat.dat_fim_valid_gartia_bem  )) space
            trim(string(tt-bem-pat.des_termo_gartia_bem_pat  )) space
            trim(string(tt-bem-pat.cod_grp_calc              )) space
            trim(string(tt-bem-pat.dat-movto                 )) space
            trim(string(tt-bem-pat.val_perc_bxa              )) space
            trim(string(tt-bem-pat.dat-ini-calc-dpr          )) space
            trim(string(tt-bem-pat.dat_calc_pat              )) space
            trim(string(tt-bem-pat.cod_ser_nota              )) space
            trim(string(tt-bem-pat.bem-importado             )) space
            trim(string(tt-bem-pat.credita-PIS               )) space
            trim(string(tt-bem-pat.credita-COFINS            )) space
            trim(string(tt-bem-pat.nr-parc-cred-PIS-COFINS   )) space
            trim(string(tt-bem-pat.parc-descontadas          )) space
            trim(string(tt-bem-pat.val-credito-PIS           )) space
            trim(string(tt-bem-pat.val-credito-COFINS        )) space
            trim(string(tt-bem-pat.credita-CSLL              )) space
            trim(string(tt-bem-pat.exec-credito-CSLL         )) SPACE
            trim(string(tt-bem-pat.val-base-PIS              )) space
            trim(string(tt-bem-pat.val-base-COFINS           )) skip.
    end.

    for each tt-valores no-lock:

        RUN pi-acompanhar IN h-acomp (INPUT "Gerando arquivo Valores: " + STRING (tt-valores.nr-seq-bem)).

        put stream saida2 unformatted
            trim(string(tt-valores.nr-seq-bem        )) space
            trim(string(tt-valores.seq-incorp        )) space
            trim(string(tt-valores.cenario-contabil  )) space
            trim(string(tt-valores.finalidade        )) space
            trim(string(tt-valores.val-orig          )) space
            trim(string(tt-valores.correcao-monet    )) space
            trim(string(tt-valores.dpr-val-orig      )) space
            trim(string(tt-valores.dpr-correcao-monet)) space
            trim(string(tt-valores.correcao-monet-dpr)) space
            trim(string(tt-valores.dpr-incentivada   )) space
            trim(string(tt-valores.dpr-incentiv-cm   )) space
            trim(string(tt-valores.cm-dpr-incentiv   )) space
            trim(string(tt-valores.amort-vo          )) space
            trim(string(tt-valores.amort-cm          )) space
            trim(string(tt-valores.cm-amort          )) space
            trim(string(tt-valores.amort-incentiv    )) space
            trim(string(tt-valores.amort-incentiv-cm )) space
            trim(string(tt-valores.cm-amort-incentiv )) space
            trim(string(tt-valores.perc-dpr          )) space
            trim(string(tt-valores.perc-dpr-incentiv )) space
            trim(string(tt-valores.perc-dpr-red-saldo)) skip.
    end.

    for each tt-alocacoes no-lock:

        RUN pi-acompanhar IN h-acomp (INPUT "Gerando arquivo Aloca‡äes: " + STRING (tt-alocacoes.nr-seq-bem)).

        put stream saida3 unformatted
            trim(string(tt-alocacoes.nr-seq-bem      )) space
            trim(string(tt-alocacoes.cod-plano-ccusto)) space
            trim(string(tt-alocacoes.cod-ccusto      )) space
            trim(string(tt-alocacoes.unid-negoc      )) space
            trim(string(tt-alocacoes.perc-apropriacao)) space
            trim(string(tt-alocacoes.ccusto-un-princ )) skip.
    end.

    output stream saida1 close.
    output stream saida2 close.
    output stream saida3 close.

    run pi-finalizar in h-acomp. 

    MESSAGE "Arquivos gerados com sucesso no diret¢rio: " + INPUT FRAME {&FRAME-NAME} fi-dir-saida
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_inicio C-Win 
PROCEDURE pi_inicio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.

    assign fi-arq-entrada:screen-value in frame {&frame-name} = session:TEMP-DIRECTORY  + "Bens.csv".
    assign fi-dir-saida:screen-value in frame {&frame-name}   = session:TEMP-DIRECTORY.

    EMPTY TEMP-TABLE tt-tabela.
    EMPTY TEMP-TABLE tt-geracao.
    EMPTY TEMP-TABLE tt-val-cenario.

    CLOSE QUERY br-valores.

    OPEN QUERY br-valores FOR EACH tt-val-cenario NO-LOCK INDEXED-REPOSITION.

    RUN pi-cria-bem-pat.

    RUN pi-cria-valores.

    RUN pi-cria-alocacoes.

    ASSIGN iCont = 0.
    FOR EACH tt-tabela
        WHERE tt-tabela.obrigatorio = YES:

        ASSIGN iCont = iCont + 1.
        CREATE tt-geracao.
        BUFFER-COPY tt-tabela TO tt-geracao.
        ASSIGN tt-geracao.posicao = iCont.
        DELETE tt-tabela.

    END.

    CLOSE QUERY br-campos.

    OPEN QUERY br-campos FOR EACH tt-tabela NO-LOCK INDEXED-REPOSITION.

    CLOSE QUERY br-geracao.

    OPEN QUERY br-geracao FOR EACH tt-geracao NO-LOCK INDEXED-REPOSITION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

