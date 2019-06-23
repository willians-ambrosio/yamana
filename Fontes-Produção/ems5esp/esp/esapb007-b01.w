&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5_esp         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i B99XX999 9.99.99.999}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> MUT}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/browserd.w

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
define variable c-lista-valor as character init '':U no-undo.

DEFINE VARIABLE c-cod_empresa-ini        like es-relat-cat.cod_empresa          .   
DEFINE VARIABLE c-cod_empresa-fim        like es-relat-cat.cod_empresa          .  
DEFINE VARIABLE i-cdn_fornecedor-ini     like es-relat-cat.cdn_fornecedor       .
DEFINE VARIABLE i-cdn_fornecedor-fim     like es-relat-cat.cdn_fornecedor       .  
DEFINE VARIABLE c-cod_espec_docto-ini    like es-relat-cat.cod_espec_docto      .
DEFINE VARIABLE c-cod_espec_docto-fim    like es-relat-cat.cod_espec_docto      .  
DEFINE VARIABLE c-cod_plano_ccusto-ini   like es-relat-cat.cod_plano_ccusto     .
DEFINE VARIABLE c-cod_plano_ccusto-fim   like es-relat-cat.cod_plano_ccusto     .  
DEFINE VARIABLE c-cod_ccusto-ini         like es-relat-cat.cod_ccusto           .
DEFINE VARIABLE c-cod_ccusto-fim         like es-relat-cat.cod_ccusto           .  
DEFINE VARIABLE c-cod_plano_cta_ctbl-ini like es-relat-cat.cod_plano_cta_ctbl   .
DEFINE VARIABLE c-cod_plano_cta_ctbl-fim like es-relat-cat.cod_plano_cta_ctbl   .  
DEFINE VARIABLE c-cod_cta_ctbl-ini       like es-relat-cat.cod_cta_ctbl         .
DEFINE VARIABLE c-cod_cta_ctbl-fim       like es-relat-cat.cod_cta_ctbl         .

DEFINE VARIABLE c-cod_empresa-ini-aux        like es-relat-cat.cod_empresa          .   
DEFINE VARIABLE c-cod_empresa-fim-aux        like es-relat-cat.cod_empresa          .  
DEFINE VARIABLE i-cdn_fornecedor-ini-aux     like es-relat-cat.cdn_fornecedor       .
DEFINE VARIABLE i-cdn_fornecedor-fim-aux     like es-relat-cat.cdn_fornecedor       .  
DEFINE VARIABLE c-cod_espec_docto-ini-aux    like es-relat-cat.cod_espec_docto      .
DEFINE VARIABLE c-cod_espec_docto-fim-aux    like es-relat-cat.cod_espec_docto      .  
DEFINE VARIABLE c-cod_plano_ccusto-ini-aux   like es-relat-cat.cod_plano_ccusto     .
DEFINE VARIABLE c-cod_plano_ccusto-fim-aux   like es-relat-cat.cod_plano_ccusto     .  
DEFINE VARIABLE c-cod_ccusto-ini-aux         like es-relat-cat.cod_ccusto           .
DEFINE VARIABLE c-cod_ccusto-fim-aux         like es-relat-cat.cod_ccusto           .  
DEFINE VARIABLE c-cod_plano_cta_ctbl-ini-aux like es-relat-cat.cod_plano_cta_ctbl   .
DEFINE VARIABLE c-cod_plano_cta_ctbl-fim-aux like es-relat-cat.cod_plano_cta_ctbl   .  
DEFINE VARIABLE c-cod_cta_ctbl-ini-aux       like es-relat-cat.cod_cta_ctbl         .
DEFINE VARIABLE c-cod_cta_ctbl-fim-aux       like es-relat-cat.cod_cta_ctbl         .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-relat-cat

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table es-relat-cat.cod_empresa ~
es-relat-cat.cdn_fornecedor es-relat-cat.cod_espec_docto ~
es-relat-cat.cod_plano_ccusto es-relat-cat.cod_ccusto ~
es-relat-cat.cod_plano_cta_ctbl es-relat-cat.cod_cta_ctbl ~
es-relat-cat.cat-code es-relat-cat.categoria-code 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH es-relat-cat WHERE ~{&KEY-PHRASE} ~
      AND (((es-relat-cat.cod_empresa          = ? and c-cod_empresa-ini = ?          or         ~
   es-relat-cat.cod_empresa          = ? and c-cod_empresa-fim = ?)         or ~
  (es-relat-cat.cod_empresa         >= c-cod_empresa-ini-aux                and   ~
   es-relat-cat.cod_empresa         <= c-cod_empresa-fim-aux))             and ~
 ~
 ((es-relat-cat.cdn_fornecedor       = ? and i-cdn_fornecedor-ini = ?       or         ~
   es-relat-cat.cdn_fornecedor       = ? and i-cdn_fornecedor-fim = ?)      or ~
  (es-relat-cat.cdn_fornecedor      >= i-cdn_fornecedor-ini-aux             and   ~
   es-relat-cat.cdn_fornecedor      <= i-cdn_fornecedor-fim-aux))         and ~
 ~
 ((es-relat-cat.cod_espec_docto      = ? and c-cod_espec_docto-ini = ?      or         ~
   es-relat-cat.cod_espec_docto      = ? and c-cod_espec_docto-fim = ?)     or ~
  (es-relat-cat.cod_espec_docto     >= c-cod_espec_docto-ini-aux            and   ~
   es-relat-cat.cod_espec_docto     <= c-cod_espec_docto-fim-aux))          and ~
 ~
 ((es-relat-cat.cod_plano_ccusto     = ? and c-cod_plano_ccusto-ini = ?     or         ~
   es-relat-cat.cod_plano_ccusto     = ? and c-cod_plano_ccusto-fim = ?)    or ~
  (es-relat-cat.cod_plano_ccusto    >= c-cod_plano_ccusto-ini-aux           and   ~
   es-relat-cat.cod_plano_ccusto    <= c-cod_plano_ccusto-fim-aux))         and ~
 ~
 ((es-relat-cat.cod_ccusto           = ? and c-cod_ccusto-ini = ?           or         ~
   es-relat-cat.cod_ccusto           = ? and c-cod_ccusto-fim = ?)          or ~
  (es-relat-cat.cod_ccusto          >= c-cod_ccusto-ini-aux                 and   ~
   es-relat-cat.cod_ccusto          <= c-cod_ccusto-fim-aux))               and ~
 ~
 ((es-relat-cat.cod_plano_cta_ctbl   = ? and c-cod_plano_cta_ctbl-ini = ?   or         ~
   es-relat-cat.cod_plano_cta_ctbl   = ? and c-cod_plano_cta_ctbl-fim = ?)  or ~
  (es-relat-cat.cod_plano_cta_ctbl  >= c-cod_plano_cta_ctbl-ini-aux         and   ~
   es-relat-cat.cod_plano_cta_ctbl  <= c-cod_plano_cta_ctbl-fim-aux))       and ~
 ~
 ((es-relat-cat.cod_cta_ctbl         = ? and c-cod_cta_ctbl-ini = ?         or         ~
   es-relat-cat.cod_cta_ctbl         = ? and c-cod_cta_ctbl-fim = ?)        or ~
  (es-relat-cat.cod_cta_ctbl        >= c-cod_cta_ctbl-ini-aux               and   ~
   es-relat-cat.cod_cta_ctbl        <= c-cod_cta_ctbl-fim-aux)))   NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH es-relat-cat WHERE ~{&KEY-PHRASE} ~
      AND (((es-relat-cat.cod_empresa          = ? and c-cod_empresa-ini = ?          or         ~
   es-relat-cat.cod_empresa          = ? and c-cod_empresa-fim = ?)         or ~
  (es-relat-cat.cod_empresa         >= c-cod_empresa-ini-aux                and   ~
   es-relat-cat.cod_empresa         <= c-cod_empresa-fim-aux))             and ~
 ~
 ((es-relat-cat.cdn_fornecedor       = ? and i-cdn_fornecedor-ini = ?       or         ~
   es-relat-cat.cdn_fornecedor       = ? and i-cdn_fornecedor-fim = ?)      or ~
  (es-relat-cat.cdn_fornecedor      >= i-cdn_fornecedor-ini-aux             and   ~
   es-relat-cat.cdn_fornecedor      <= i-cdn_fornecedor-fim-aux))         and ~
 ~
 ((es-relat-cat.cod_espec_docto      = ? and c-cod_espec_docto-ini = ?      or         ~
   es-relat-cat.cod_espec_docto      = ? and c-cod_espec_docto-fim = ?)     or ~
  (es-relat-cat.cod_espec_docto     >= c-cod_espec_docto-ini-aux            and   ~
   es-relat-cat.cod_espec_docto     <= c-cod_espec_docto-fim-aux))          and ~
 ~
 ((es-relat-cat.cod_plano_ccusto     = ? and c-cod_plano_ccusto-ini = ?     or         ~
   es-relat-cat.cod_plano_ccusto     = ? and c-cod_plano_ccusto-fim = ?)    or ~
  (es-relat-cat.cod_plano_ccusto    >= c-cod_plano_ccusto-ini-aux           and   ~
   es-relat-cat.cod_plano_ccusto    <= c-cod_plano_ccusto-fim-aux))         and ~
 ~
 ((es-relat-cat.cod_ccusto           = ? and c-cod_ccusto-ini = ?           or         ~
   es-relat-cat.cod_ccusto           = ? and c-cod_ccusto-fim = ?)          or ~
  (es-relat-cat.cod_ccusto          >= c-cod_ccusto-ini-aux                 and   ~
   es-relat-cat.cod_ccusto          <= c-cod_ccusto-fim-aux))               and ~
 ~
 ((es-relat-cat.cod_plano_cta_ctbl   = ? and c-cod_plano_cta_ctbl-ini = ?   or         ~
   es-relat-cat.cod_plano_cta_ctbl   = ? and c-cod_plano_cta_ctbl-fim = ?)  or ~
  (es-relat-cat.cod_plano_cta_ctbl  >= c-cod_plano_cta_ctbl-ini-aux         and   ~
   es-relat-cat.cod_plano_cta_ctbl  <= c-cod_plano_cta_ctbl-fim-aux))       and ~
 ~
 ((es-relat-cat.cod_cta_ctbl         = ? and c-cod_cta_ctbl-ini = ?         or         ~
   es-relat-cat.cod_cta_ctbl         = ? and c-cod_cta_ctbl-fim = ?)        or ~
  (es-relat-cat.cod_cta_ctbl        >= c-cod_cta_ctbl-ini-aux               and   ~
   es-relat-cat.cod_cta_ctbl        <= c-cod_cta_ctbl-fim-aux)))   NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table es-relat-cat
&Scoped-define FIRST-TABLE-IN-QUERY-br-table es-relat-cat


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fill_cod_empresa_ini fill_cod_empresa_fim ~
bt-confirma IMAGE-2 IMAGE-3 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 ~
IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-15 IMAGE-16 IMAGE-17 IMAGE-18 ~
fill_cdn_fornecedor_ini fill_cdn_fornecedor_fim fill_cod_espec_docto_ini ~
fill_cod_espec_docto_fim fill_cod_plano_ccusto_ini ~
fill_cod_plano_ccusto_fim fill_cod_ccusto_ini fill_cod_ccusto_fim ~
fill_cod_plano_cta_ctbl_ini fill_cod_plano_cta_ctbl_fim ~
fill_cod_cta_ctbl_ini fill_cod_cta_ctbl_fim br-table 
&Scoped-Define DISPLAYED-OBJECTS fill_cod_empresa_ini fill_cod_empresa_fim ~
fill_cdn_fornecedor_ini fill_cdn_fornecedor_fim fill_cod_espec_docto_ini ~
fill_cod_espec_docto_fim fill_cod_plano_ccusto_ini ~
fill_cod_plano_ccusto_fim fill_cod_ccusto_ini fill_cod_ccusto_fim ~
fill_cod_plano_cta_ctbl_ini fill_cod_plano_cta_ctbl_fim ~
fill_cod_cta_ctbl_ini fill_cod_cta_ctbl_fim 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
cat-code||y|ems5_esp.es-relat-cat.cat-code
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cat-code"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "Button 1" 
     SIZE 5.14 BY 1.

DEFINE VARIABLE fill_cdn_fornecedor_fim AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fill_cdn_fornecedor_ini AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fill_cod_ccusto_fim AS CHARACTER FORMAT "x(11)" INITIAL "ZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16.43 BY .88 NO-UNDO.

DEFINE VARIABLE fill_cod_ccusto_ini AS CHARACTER FORMAT "x(11)" 
     LABEL "Centro Custo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fill_cod_cta_ctbl_fim AS CHARACTER FORMAT "x(20)" INITIAL "ZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 27.43 BY .88 NO-UNDO.

DEFINE VARIABLE fill_cod_cta_ctbl_ini AS CHARACTER FORMAT "x(20)" 
     LABEL "Conta Cont bil" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE fill_cod_empresa_fim AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fill_cod_empresa_ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fill_cod_espec_docto_fim AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fill_cod_espec_docto_ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Esp‚cie Documento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fill_cod_plano_ccusto_fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .88 NO-UNDO.

DEFINE VARIABLE fill_cod_plano_ccusto_ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Plano Centros Custo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fill_cod_plano_cta_ctbl_fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .88 NO-UNDO.

DEFINE VARIABLE fill_cod_plano_cta_ctbl_ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Plano Contas" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-11
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-12
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-15
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-16
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-17
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-18
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-2
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-3
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-5
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-6
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-7
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-8
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-9
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      es-relat-cat SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      es-relat-cat.cod_empresa FORMAT "x(3)":U
      es-relat-cat.cdn_fornecedor FORMAT ">>>,>>>,>>9":U
      es-relat-cat.cod_espec_docto FORMAT "x(3)":U
      es-relat-cat.cod_plano_ccusto FORMAT "x(8)":U
      es-relat-cat.cod_ccusto FORMAT "x(11)":U
      es-relat-cat.cod_plano_cta_ctbl FORMAT "x(8)":U
      es-relat-cat.cod_cta_ctbl FORMAT "x(20)":U
      es-relat-cat.cat-code FORMAT "x(16)":U
      es-relat-cat.categoria-code FORMAT "999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 80 BY 9
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fill_cod_empresa_ini AT ROW 1 COL 29.14 COLON-ALIGNED HELP
          "C¢digo Empresa"
     fill_cod_empresa_fim AT ROW 1 COL 42.72 COLON-ALIGNED HELP
          "C¢digo Empresa" NO-LABEL WIDGET-ID 2
     bt-confirma AT ROW 1 COL 76
     fill_cdn_fornecedor_ini AT ROW 2 COL 19.14 COLON-ALIGNED HELP
          "C¢digo Fornecedor" WIDGET-ID 14
     fill_cdn_fornecedor_fim AT ROW 2 COL 42.57 COLON-ALIGNED HELP
          "C¢digo Fornecedor" NO-LABEL WIDGET-ID 20
     fill_cod_espec_docto_ini AT ROW 3 COL 29 COLON-ALIGNED HELP
          "C¢digo Esp‚cie Documento" WIDGET-ID 24
     fill_cod_espec_docto_fim AT ROW 3 COL 42.57 COLON-ALIGNED HELP
          "C¢digo Esp‚cie Documento" NO-LABEL WIDGET-ID 30
     fill_cod_plano_ccusto_ini AT ROW 4 COL 23 COLON-ALIGNED HELP
          "C¢digo Plano Centros Custo" WIDGET-ID 34
     fill_cod_plano_ccusto_fim AT ROW 4 COL 42.57 COLON-ALIGNED HELP
          "C¢digo Plano Centros Custo" NO-LABEL WIDGET-ID 40
     fill_cod_ccusto_ini AT ROW 5 COL 20 COLON-ALIGNED HELP
          "C¢digo Centro Custo" WIDGET-ID 42
     fill_cod_ccusto_fim AT ROW 5 COL 42.57 COLON-ALIGNED HELP
          "C¢digo Centro Custo" NO-LABEL WIDGET-ID 48
     fill_cod_plano_cta_ctbl_ini AT ROW 5.96 COL 23 COLON-ALIGNED HELP
          "C¢digo Plano Contas" WIDGET-ID 56
     fill_cod_plano_cta_ctbl_fim AT ROW 6 COL 42.57 COLON-ALIGNED HELP
          "C¢digo Plano Contas" NO-LABEL WIDGET-ID 62
     fill_cod_cta_ctbl_ini AT ROW 7 COL 14 COLON-ALIGNED HELP
          "C¢digo Conta Cont bil" WIDGET-ID 64
     fill_cod_cta_ctbl_fim AT ROW 7 COL 42.57 COLON-ALIGNED HELP
          "C¢digo Conta Cont bil" NO-LABEL WIDGET-ID 70
     br-table AT ROW 8.29 COL 1
     IMAGE-2 AT ROW 1 COL 41.86
     IMAGE-3 AT ROW 1 COL 36.14 WIDGET-ID 8
     IMAGE-5 AT ROW 2 COL 36 WIDGET-ID 16
     IMAGE-6 AT ROW 2 COL 41.72 WIDGET-ID 18
     IMAGE-7 AT ROW 3 COL 36 WIDGET-ID 26
     IMAGE-8 AT ROW 3 COL 41.72 WIDGET-ID 28
     IMAGE-9 AT ROW 4 COL 36 WIDGET-ID 36
     IMAGE-10 AT ROW 4 COL 41.72 WIDGET-ID 38
     IMAGE-11 AT ROW 5 COL 36 WIDGET-ID 46
     IMAGE-12 AT ROW 5 COL 41.72 WIDGET-ID 44
     IMAGE-15 AT ROW 5.96 COL 36 WIDGET-ID 58
     IMAGE-16 AT ROW 5.96 COL 41.72 WIDGET-ID 60
     IMAGE-17 AT ROW 6.96 COL 36 WIDGET-ID 66
     IMAGE-18 AT ROW 6.96 COL 41.72 WIDGET-ID 68
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 16.58
         WIDTH              = 80.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-brwzoo.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB br-table fill_cod_cta_ctbl_fim F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "ems5_esp.es-relat-cat"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "(((es-relat-cat.cod_empresa          = ? and c-cod_empresa-ini = ?          or        
   es-relat-cat.cod_empresa          = ? and c-cod_empresa-fim = ?)         or
  (es-relat-cat.cod_empresa         >= c-cod_empresa-ini-aux                and  
   es-relat-cat.cod_empresa         <= c-cod_empresa-fim-aux))             and

 ((es-relat-cat.cdn_fornecedor       = ? and i-cdn_fornecedor-ini = ?       or        
   es-relat-cat.cdn_fornecedor       = ? and i-cdn_fornecedor-fim = ?)      or
  (es-relat-cat.cdn_fornecedor      >= i-cdn_fornecedor-ini-aux             and  
   es-relat-cat.cdn_fornecedor      <= i-cdn_fornecedor-fim-aux))         and

 ((es-relat-cat.cod_espec_docto      = ? and c-cod_espec_docto-ini = ?      or        
   es-relat-cat.cod_espec_docto      = ? and c-cod_espec_docto-fim = ?)     or
  (es-relat-cat.cod_espec_docto     >= c-cod_espec_docto-ini-aux            and  
   es-relat-cat.cod_espec_docto     <= c-cod_espec_docto-fim-aux))          and

 ((es-relat-cat.cod_plano_ccusto     = ? and c-cod_plano_ccusto-ini = ?     or        
   es-relat-cat.cod_plano_ccusto     = ? and c-cod_plano_ccusto-fim = ?)    or
  (es-relat-cat.cod_plano_ccusto    >= c-cod_plano_ccusto-ini-aux           and  
   es-relat-cat.cod_plano_ccusto    <= c-cod_plano_ccusto-fim-aux))         and

 ((es-relat-cat.cod_ccusto           = ? and c-cod_ccusto-ini = ?           or        
   es-relat-cat.cod_ccusto           = ? and c-cod_ccusto-fim = ?)          or
  (es-relat-cat.cod_ccusto          >= c-cod_ccusto-ini-aux                 and  
   es-relat-cat.cod_ccusto          <= c-cod_ccusto-fim-aux))               and

 ((es-relat-cat.cod_plano_cta_ctbl   = ? and c-cod_plano_cta_ctbl-ini = ?   or        
   es-relat-cat.cod_plano_cta_ctbl   = ? and c-cod_plano_cta_ctbl-fim = ?)  or
  (es-relat-cat.cod_plano_cta_ctbl  >= c-cod_plano_cta_ctbl-ini-aux         and  
   es-relat-cat.cod_plano_cta_ctbl  <= c-cod_plano_cta_ctbl-fim-aux))       and

 ((es-relat-cat.cod_cta_ctbl         = ? and c-cod_cta_ctbl-ini = ?         or        
   es-relat-cat.cod_cta_ctbl         = ? and c-cod_cta_ctbl-fim = ?)        or
  (es-relat-cat.cod_cta_ctbl        >= c-cod_cta_ctbl-ini-aux               and  
   es-relat-cat.cod_cta_ctbl        <= c-cod_cta_ctbl-fim-aux)))  "
     _FldNameList[1]   = ems5_esp.es-relat-cat.cod_empresa
     _FldNameList[2]   = ems5_esp.es-relat-cat.cdn_fornecedor
     _FldNameList[3]   = ems5_esp.es-relat-cat.cod_espec_docto
     _FldNameList[4]   = ems5_esp.es-relat-cat.cod_plano_ccusto
     _FldNameList[5]   = ems5_esp.es-relat-cat.cod_ccusto
     _FldNameList[6]   = ems5_esp.es-relat-cat.cod_plano_cta_ctbl
     _FldNameList[7]   = ems5_esp.es-relat-cat.cod_cta_ctbl
     _FldNameList[8]   = ems5_esp.es-relat-cat.cat-code
     _FldNameList[9]   = ems5_esp.es-relat-cat.categoria-code
     _Query            is NOT OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br-table IN FRAME F-Main
DO:
    RUN New-State('DblClick':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-ENTRY OF br-table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  
  run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).
  run seta-valor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-LEAVE OF br-table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).
  run new-state('Value-Changed|':U + string(this-procedure)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma B-table-Win
ON CHOOSE OF bt-confirma IN FRAME F-Main /* Button 1 */
DO:
    assign input frame {&frame-name} FILL_cod_empresa_ini FILL_cod_empresa_fim FILL_cdn_fornecedor_ini FILL_cdn_fornecedor_fim FILL_cod_espec_docto_ini FILL_cod_espec_docto_fim FILL_cod_plano_ccusto_ini FILL_cod_plano_ccusto_fim FILL_cod_ccusto_ini FILL_cod_ccusto_fim FILL_cod_plano_cta_ctbl_ini FILL_cod_plano_cta_ctbl_fim FILL_cod_cta_Ctbl_ini FILL_cod_cta_ctbl_fim.
    ASSIGN c-cod_empresa-ini          =   FILL_cod_empresa_ini     
           c-cod_empresa-fim          =   FILL_cod_empresa_fim 
           i-cdn_fornecedor-ini       =   FILL_cdn_fornecedor_ini
           i-cdn_fornecedor-fim       =   FILL_cdn_fornecedor_fim
           c-cod_espec_docto-ini      =   FILL_cod_espec_docto_ini
           c-cod_espec_docto-fim      =   FILL_cod_espec_docto_fim
           c-cod_plano_ccusto-ini     =   FILL_cod_plano_ccusto_ini
           c-cod_plano_ccusto-fim     =   FILL_cod_plano_ccusto_fim
           c-cod_ccusto-ini           =   FILL_cod_ccusto_ini
           c-cod_ccusto-fim           =   FILL_cod_ccusto_fim
           c-cod_plano_cta_ctbl-ini   =   FILL_cod_plano_cta_ctbl_ini 
           c-cod_plano_cta_ctbl-fim   =   FILL_cod_plano_cta_ctbl_fim
           c-cod_cta_ctbl-ini         =   FILL_cod_cta_Ctbl_ini
           c-cod_cta_ctbl-fim         =   FILL_cod_cta_Ctbl_fim.


    IF c-cod_empresa-ini = ? OR c-cod_empresa-ini = "" THEN
        ASSIGN c-cod_empresa-ini      = ?
               c-cod_empresa-ini-aux  = "".
    ELSE
        ASSIGN c-cod_empresa-ini-aux =  c-cod_empresa-ini.

    IF c-cod_empresa-fim = ? OR c-cod_empresa-fim = "" THEN DO:
        IF c-cod_empresa-ini = ? THEN
            ASSIGN c-cod_empresa-ini-aux = ?
                   c-cod_empresa-fim-aux = ?.
        ELSE
            ASSIGN c-cod_empresa-fim-aux = "".
    END.
    ELSE
        ASSIGN c-cod_empresa-fim-aux = c-cod_empresa-fim.
        /* ---------------- */

    IF i-cdn_fornecedor-ini = ? OR i-cdn_fornecedor-ini = 0 THEN
        ASSIGN i-cdn_fornecedor-ini-aux =   0
               i-cdn_fornecedor-ini     =   ?.
    ELSE
        ASSIGN i-cdn_fornecedor-ini-aux    =   i-cdn_fornecedor-ini.

    IF i-cdn_fornecedor-fim = ? THEN DO:
        IF i-cdn_fornecedor-ini = ? THEN
            ASSIGN i-cdn_fornecedor-ini-aux = ?
                   i-cdn_fornecedor-fim-aux = ?.
        ELSE
            ASSIGN i-cdn_fornecedor-fim-aux = 0.

    END.
    ELSE
        ASSIGN i-cdn_fornecedor-fim-aux =   i-cdn_fornecedor-fim.
        /* ------------------- */

    IF c-cod_espec_docto-ini = ? OR c-cod_espec_docto-ini = ""  THEN
        ASSIGN c-cod_espec_docto-ini = ?
               c-cod_espec_docto-ini-aux = "".
    ELSE
        ASSIGN c-cod_espec_docto-ini-aux = c-cod_espec_docto-ini.

    IF c-cod_espec_docto-fim = ? THEN DO:
        IF c-cod_espec_docto-ini = ? THEN
            ASSIGN c-cod_espec_docto-ini-aux = ?
                   c-cod_espec_docto-fim-aux = ?.
        ELSE
            ASSIGN c-cod_espec_docto-fim-aux = "".
    END.
    ELSE
        ASSIGN c-cod_espec_docto-fim-aux = c-cod_espec_docto-fim.
    /* -------------------------- */

    IF c-cod_plano_ccusto-ini = ? OR c-cod_plano_ccusto-ini = ""  THEN
        ASSIGN c-cod_plano_ccusto-ini = ?
               c-cod_plano_ccusto-ini-aux = "".
    ELSE
        ASSIGN c-cod_plano_ccusto-ini-aux = c-cod_plano_ccusto-ini.

    IF c-cod_plano_ccusto-fim = ? THEN DO:
        IF c-cod_plano_ccusto-ini = ? THEN
            ASSIGN c-cod_plano_ccusto-ini-aux = ?
                   c-cod_plano_ccusto-fim-aux = ?.
        ELSE
            ASSIGN c-cod_plano_ccusto-fim-aux = "".
    END.
    ELSE
        ASSIGN c-cod_plano_ccusto-fim-aux = c-cod_plano_ccusto-fim.
    /* -------------------------- */

    IF c-cod_ccusto-ini = ? OR c-cod_ccusto-ini = ""  THEN
        ASSIGN c-cod_ccusto-ini = ?
               c-cod_ccusto-ini-aux = "".
    ELSE
        ASSIGN c-cod_ccusto-ini-aux = c-cod_ccusto-ini.

    IF c-cod_ccusto-fim = ? THEN DO:
        IF c-cod_ccusto-ini = ? THEN
            ASSIGN c-cod_ccusto-ini-aux = ?
                   c-cod_ccusto-fim-aux = ?.
        ELSE
            ASSIGN c-cod_ccusto-fim-aux = "".
    END.
    ELSE
        ASSIGN c-cod_ccusto-fim-aux = c-cod_ccusto-fim.
    /* -------------------------- */

    IF c-cod_plano_cta_ctbl-ini = ? OR c-cod_plano_cta_ctbl-ini = ""  THEN
        ASSIGN c-cod_plano_cta_ctbl-ini = ?
               c-cod_plano_cta_ctbl-ini-aux = "".
    ELSE
        ASSIGN c-cod_plano_cta_ctbl-ini-aux = c-cod_plano_cta_ctbl-ini.

    IF c-cod_plano_cta_ctbl-fim = ? THEN DO:
        IF c-cod_plano_cta_ctbl-ini = ? THEN
            ASSIGN c-cod_plano_cta_ctbl-ini-aux = ?
                   c-cod_plano_cta_ctbl-fim-aux = ?.
        ELSE
            ASSIGN c-cod_plano_cta_ctbl-fim-aux = "".
    END.
    ELSE
        ASSIGN c-cod_plano_cta_ctbl-fim-aux = c-cod_plano_cta_ctbl-fim.
    /* -------------------------- */

    IF c-cod_cta_ctbl-ini = ? OR c-cod_cta_ctbl-ini = ""  THEN
        ASSIGN c-cod_cta_ctbl-ini = ?
               c-cod_cta_ctbl-ini-aux = "".
    ELSE
        ASSIGN c-cod_cta_ctbl-ini-aux = c-cod_cta_ctbl-ini.

    IF c-cod_cta_ctbl-fim = ? THEN DO:
        IF c-cod_cta_ctbl-ini = ? THEN
            ASSIGN c-cod_cta_ctbl-ini-aux = ?
                   c-cod_cta_ctbl-fim-aux = ?.
        ELSE
            ASSIGN c-cod_cta_ctbl-fim-aux = "".
    END.
    ELSE
        ASSIGN c-cod_cta_ctbl-fim-aux = c-cod_cta_ctbl-fim.
    /* -------------------------- */

  RUN dispatch IN THIS-PROCEDURE ('open-query':U).
  apply 'value-changed':U to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR Filter-Value AS CHAR NO-UNDO.

  /* Copy 'Filter-Attributes' into local variables. */
  RUN get-attribute ('Filter-Value':U).
  Filter-Value = RETURN-VALUE.

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply 'value-changed':U to {&browse-name} in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-retorna-valor B-table-Win 
PROCEDURE pi-retorna-valor :
DEFINE INPUT PARAMETER P-CAMPO AS CHARACTER NO-UNDO.

    DEFINE VARIABLE P-VALOR AS CHAR INIT "" NO-UNDO.

    if  avail ems5_esp.es-relat-cat then do:
        case p-campo:
            when "cod_empresa" then
                assign p-valor = string(es-relat-cat.cod_empresa).
            when "cdn_fornecedor" then
                assign p-valor = string(es-relat-cat.cdn_fornecedor).
            when "cod_plano_ccusto" then
                assign p-valor = string(es-relat-cat.cod_plano_ccusto).
            when "cod_ccusto" then
                assign p-valor = string(es-relat-cat.cod_ccusto).
            when "cod_espec_docto" then
                assign p-valor = string(es-relat-cat.cod_espec_docto).
            when "cod_plano_cta_ctbl" then
                assign p-valor = string(es-relat-cat.cod_plano_cta_ctbl).
            when "cod_cta_ctbl" then
                assign p-valor = string(es-relat-cat.cod_cta_ctbl).
            when "cat-code" then
                assign p-valor = string(es-relat-cat.cat-code).
        end.
    end.
    return p-valor.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cat-code" "es-relat-cat" "cat-code"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "es-relat-cat"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "RetornaValorCampo" B-table-Win _INLINE
/* Actions: ? ? ? ? support/brwrtval.p */
/* Procedure desativada */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

