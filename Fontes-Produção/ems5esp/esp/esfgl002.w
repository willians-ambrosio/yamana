&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

/*****************************************************************************
** Programa..............: esfgl002
** Descricao.............: C¢pia de Rateio Cont†bil
** Versao................: 1.00.00.000
** Procedimento..........: esfgl002
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 10/06/2013
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

DEFINE BUFFER empresa FOR ems5.empresa.

/* Local Variable Definitions ---                                       */

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def new global shared var v_rec_empresa
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_estabelecimento
    as recid
    format ">>>>>>9":U
    no-undo.


DEFINE TEMP-TABLE tt-rat_ctbl
    FIELD cod_empresa-de        LIKE rat_ctbl.cod_empresa 
    FIELD cod_rat_ctbl-de       LIKE rat_ctbl.cod_rat_ctbl
    FIELD des_rat_ctbl-de       LIKE rat_ctbl.des_rat_ctbl
    FIELD cod_empresa-para      LIKE rat_ctbl.cod_empresa 
    FIELD cod_rat_ctbl-para     LIKE rat_ctbl.cod_rat_ctbl
    FIELD des_rat_ctbl-para     LIKE rat_ctbl.des_rat_ctbl
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f_main
&Scoped-define BROWSE-NAME br-de-para-rat

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-rat_ctbl item_rat_ctbl rat_ctbl_dest ~
rat_ctbl_orig rat_ctbl_dest_mapa rat_ctbl

/* Definitions for BROWSE br-de-para-rat                                */
&Scoped-define FIELDS-IN-QUERY-br-de-para-rat tt-rat_ctbl.cod_empresa-de tt-rat_ctbl.cod_rat_ctbl-de tt-rat_ctbl.cod_empresa-para tt-rat_ctbl.cod_rat_ctbl-para tt-rat_ctbl.des_rat_ctbl-para   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-de-para-rat tt-rat_ctbl.cod_rat_ctbl-para ~
  tt-rat_ctbl.des_rat_ctbl-para   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-de-para-rat tt-rat_ctbl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-de-para-rat tt-rat_ctbl
&Scoped-define SELF-NAME br-de-para-rat
&Scoped-define QUERY-STRING-br-de-para-rat FOR EACH tt-rat_ctbl NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-de-para-rat OPEN QUERY {&SELF-NAME} FOR EACH tt-rat_ctbl NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-de-para-rat tt-rat_ctbl
&Scoped-define FIRST-TABLE-IN-QUERY-br-de-para-rat tt-rat_ctbl


/* Definitions for BROWSE br-itens-dest-rat                             */
&Scoped-define FIELDS-IN-QUERY-br-itens-dest-rat item_rat_ctbl.cod_empresa ~
item_rat_ctbl.cod_rat_ctbl item_rat_ctbl.num_seq_rat_ctbl ~
rat_ctbl_dest.num_ord_seq_rat_ctbl rat_ctbl_dest.cod_cta_ctbl ~
rat_ctbl_dest.ind_rat_ctbl_dest_estab rat_ctbl_dest.cod_estab ~
rat_ctbl_dest.ind_rat_ctbl_dest_ccusto 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens-dest-rat 
&Scoped-define QUERY-STRING-br-itens-dest-rat FOR EACH item_rat_ctbl NO-LOCK, ~
      EACH rat_ctbl_dest OF item_rat_ctbl NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-itens-dest-rat OPEN QUERY br-itens-dest-rat FOR EACH item_rat_ctbl NO-LOCK, ~
      EACH rat_ctbl_dest OF item_rat_ctbl NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-itens-dest-rat item_rat_ctbl ~
rat_ctbl_dest
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens-dest-rat item_rat_ctbl
&Scoped-define SECOND-TABLE-IN-QUERY-br-itens-dest-rat rat_ctbl_dest


/* Definitions for BROWSE br-itens-orig-rat                             */
&Scoped-define FIELDS-IN-QUERY-br-itens-orig-rat item_rat_ctbl.cod_empresa ~
item_rat_ctbl.cod_rat_ctbl item_rat_ctbl.num_seq_rat_ctbl ~
rat_ctbl_orig.num_ord_seq_rat_ctbl rat_ctbl_orig.cod_cta_ctbl_inic ~
rat_ctbl_orig.cod_cta_ctbl_fim rat_ctbl_orig.cod_estab_inic ~
rat_ctbl_orig.cod_estab_fim 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens-orig-rat 
&Scoped-define QUERY-STRING-br-itens-orig-rat FOR EACH item_rat_ctbl NO-LOCK, ~
      EACH rat_ctbl_orig OF item_rat_ctbl NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-itens-orig-rat OPEN QUERY br-itens-orig-rat FOR EACH item_rat_ctbl NO-LOCK, ~
      EACH rat_ctbl_orig OF item_rat_ctbl NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-itens-orig-rat item_rat_ctbl ~
rat_ctbl_orig
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens-orig-rat item_rat_ctbl
&Scoped-define SECOND-TABLE-IN-QUERY-br-itens-orig-rat rat_ctbl_orig


/* Definitions for BROWSE br-mapa                                       */
&Scoped-define FIELDS-IN-QUERY-br-mapa rat_ctbl_dest_mapa.cod_empresa ~
rat_ctbl_dest_mapa.cod_rat_ctbl rat_ctbl_dest_mapa.num_seq_rat_ctbl ~
rat_ctbl_dest_mapa.num_ord_seq_rat_ctbl rat_ctbl_dest_mapa.cod_estab ~
rat_ctbl_dest_mapa.cod_mapa_distrib_ccusto 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-mapa 
&Scoped-define QUERY-STRING-br-mapa FOR EACH rat_ctbl_dest_mapa NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-mapa OPEN QUERY br-mapa FOR EACH rat_ctbl_dest_mapa NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-mapa rat_ctbl_dest_mapa
&Scoped-define FIRST-TABLE-IN-QUERY-br-mapa rat_ctbl_dest_mapa


/* Definitions for BROWSE br-rateio                                     */
&Scoped-define FIELDS-IN-QUERY-br-rateio rat_ctbl.cod_empresa ~
rat_ctbl.cod_rat_ctbl rat_ctbl.des_rat_ctbl rat_ctbl.dat_inic_valid ~
rat_ctbl.dat_fim_valid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-rateio 
&Scoped-define QUERY-STRING-br-rateio FOR EACH rat_ctbl NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-rateio OPEN QUERY br-rateio FOR EACH rat_ctbl NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-rateio rat_ctbl
&Scoped-define FIRST-TABLE-IN-QUERY-br-rateio rat_ctbl


/* Definitions for FRAME f_main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f_main ~
    ~{&OPEN-QUERY-br-de-para-rat}~
    ~{&OPEN-QUERY-br-itens-dest-rat}~
    ~{&OPEN-QUERY-br-itens-orig-rat}~
    ~{&OPEN-QUERY-br-mapa}~
    ~{&OPEN-QUERY-br-rateio}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-cod_rat_ctbl-de fi-cod_rat_ctbl-ate ~
bt-filtro fi-cod_empresa-nova bt-busca-empr-dest fi-cod_estab-novo ~
bt-busca-estab-dest br-de-para-rat bt-copia-rateio bt-altera-estab br-mapa ~
bt_ok bt_cancelar Btn_Help br-rateio br-itens-dest-rat br-itens-orig-rat ~
bt-add-mod bt-del-mod rt_cxcf RECT-1 RECT-3 RECT-4 RECT-5 RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS fi-cod_rat_ctbl-de fi-cod_rat_ctbl-ate ~
fi-cod_empresa-nova fi-cod_estab-novo fi-cod_empresa-orig fi-desc-empr-orig ~
fi-desc-empr-dest fi-desc-estab-novo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add-mod 
     IMAGE-UP FILE "adeicon/next-au.bmp":U
     LABEL "Add Modulo" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-altera-estab 
     LABEL "Altera Estab" 
     SIZE 15 BY 1.13 TOOLTIP "Altera Estab".

DEFINE BUTTON bt-busca-empr-dest 
     IMAGE-UP FILE "image/im-zoo.bmp":U
     LABEL "Busca Empresa Destino" 
     SIZE 3.72 BY .92 TOOLTIP "Busca Empresa Destino".

DEFINE BUTTON bt-busca-empr-orig 
     IMAGE-UP FILE "image/im-zoo.bmp":U
     LABEL "Busca Empresa Origem" 
     SIZE 3.72 BY .92 TOOLTIP "Busca Empresa Origem".

DEFINE BUTTON bt-busca-estab-dest 
     IMAGE-UP FILE "image/im-zoo.bmp":U
     LABEL "Busca Estab Dest" 
     SIZE 3.72 BY .92.

DEFINE BUTTON bt-copia-rateio 
     LABEL "Copia Rateio" 
     SIZE 15 BY 1.13 TOOLTIP "Copia Rateio".

DEFINE BUTTON bt-del-mod 
     IMAGE-UP FILE "adeicon/prev-au.bmp":U
     LABEL "Del Modulo" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-filtro 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "Filtro" 
     SIZE 3.72 BY .92 TOOLTIP "Filtro".

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-cod_empresa-nova AS CHARACTER FORMAT "x(3)" 
     LABEL "NOVA Empresa" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod_empresa-orig AS CHARACTER FORMAT "x(3)" 
     LABEL "Empresa Origem" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod_estab-novo AS CHARACTER FORMAT "x(3)" 
     LABEL "NOVO Estabel" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod_rat_ctbl-ate AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     LABEL "AtÇ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod_rat_ctbl-de AS CHARACTER FORMAT "x(8)" 
     LABEL "Rateio Cont†bil" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-empr-dest AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-empr-orig AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-estab-novo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31.86 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 8.5.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56.72 BY 8.5.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 6.5.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 6.5.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56.72 BY 6.5.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56.72 BY 6.5.

DEFINE RECTANGLE rt_cxcf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 138 BY 1.42.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-de-para-rat FOR 
      tt-rat_ctbl SCROLLING.

DEFINE QUERY br-itens-dest-rat FOR 
      item_rat_ctbl, 
      rat_ctbl_dest SCROLLING.

DEFINE QUERY br-itens-orig-rat FOR 
      item_rat_ctbl, 
      rat_ctbl_orig SCROLLING.

DEFINE QUERY br-mapa FOR 
      rat_ctbl_dest_mapa SCROLLING.

DEFINE QUERY br-rateio FOR 
      rat_ctbl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-de-para-rat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-de-para-rat C-Win _FREEFORM
  QUERY br-de-para-rat NO-LOCK DISPLAY
      tt-rat_ctbl.cod_empresa-de    FORMAT "x(3)":U
      tt-rat_ctbl.cod_rat_ctbl-de   FORMAT "x(8)":U
      tt-rat_ctbl.cod_empresa-para  FORMAT "x(3)":U
      tt-rat_ctbl.cod_rat_ctbl-para FORMAT "x(8)":U
      tt-rat_ctbl.des_rat_ctbl-para FORMAT "x(40)":U
          ENABLE
          tt-rat_ctbl.cod_rat_ctbl-para
          tt-rat_ctbl.des_rat_ctbl-para
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 50 BY 5.75
         FONT 1
         TITLE "De-Para Rateio" ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN TOOLTIP "De-Para Rateio".

DEFINE BROWSE br-itens-dest-rat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens-dest-rat C-Win _STRUCTURED
  QUERY br-itens-dest-rat NO-LOCK DISPLAY
      item_rat_ctbl.cod_empresa FORMAT "x(3)":U WIDTH 7
      item_rat_ctbl.cod_rat_ctbl FORMAT "x(8)":U WIDTH 7
      item_rat_ctbl.num_seq_rat_ctbl FORMAT ">,>>9":U WIDTH 7
      rat_ctbl_dest.num_ord_seq_rat_ctbl FORMAT ">>9":U WIDTH 5
      rat_ctbl_dest.cod_cta_ctbl FORMAT "x(20)":U WIDTH 14
      rat_ctbl_dest.ind_rat_ctbl_dest_estab FORMAT "X(16)":U
      rat_ctbl_dest.cod_estab FORMAT "x(3)":U WIDTH 4
      rat_ctbl_dest.ind_rat_ctbl_dest_ccusto FORMAT "X(16)":U WIDTH 15.86
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 6
         FONT 1
         TITLE "Itens Destino Rateio" FIT-LAST-COLUMN.

DEFINE BROWSE br-itens-orig-rat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens-orig-rat C-Win _STRUCTURED
  QUERY br-itens-orig-rat NO-LOCK DISPLAY
      item_rat_ctbl.cod_empresa FORMAT "x(3)":U WIDTH 7
      item_rat_ctbl.cod_rat_ctbl FORMAT "x(8)":U WIDTH 7
      item_rat_ctbl.num_seq_rat_ctbl FORMAT ">,>>9":U WIDTH 7
      rat_ctbl_orig.num_ord_seq_rat_ctbl FORMAT ">>9":U WIDTH 5
      rat_ctbl_orig.cod_cta_ctbl_inic FORMAT "x(20)":U WIDTH 14
      rat_ctbl_orig.cod_cta_ctbl_fim FORMAT "x(20)":U WIDTH 14
      rat_ctbl_orig.cod_estab_inic FORMAT "x(3)":U WIDTH 9
      rat_ctbl_orig.cod_estab_fim FORMAT "x(3)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 6
         FONT 1
         TITLE "Itens Origem Rateio" FIT-LAST-COLUMN.

DEFINE BROWSE br-mapa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-mapa C-Win _STRUCTURED
  QUERY br-mapa NO-LOCK DISPLAY
      rat_ctbl_dest_mapa.cod_empresa FORMAT "x(3)":U WIDTH 7
      rat_ctbl_dest_mapa.cod_rat_ctbl FORMAT "x(8)":U WIDTH 7
      rat_ctbl_dest_mapa.num_seq_rat_ctbl FORMAT ">,>>9":U WIDTH 7
      rat_ctbl_dest_mapa.num_ord_seq_rat_ctbl FORMAT ">>9":U WIDTH 5
      rat_ctbl_dest_mapa.cod_estab FORMAT "x(3)":U
      rat_ctbl_dest_mapa.cod_mapa_distrib_ccusto FORMAT "x(8)":U
            WIDTH 18
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 55 BY 6
         FONT 1
         TITLE "Mapas" FIT-LAST-COLUMN TOOLTIP "Mapas".

DEFINE BROWSE br-rateio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-rateio C-Win _STRUCTURED
  QUERY br-rateio NO-LOCK DISPLAY
      rat_ctbl.cod_empresa FORMAT "x(3)":U
      rat_ctbl.cod_rat_ctbl FORMAT "x(8)":U
      rat_ctbl.des_rat_ctbl FORMAT "x(40)":U WIDTH 40
      rat_ctbl.dat_inic_valid FORMAT "99/99/9999":U
      rat_ctbl.dat_fim_valid FORMAT "99/99/9999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 79 BY 5.75
         FONT 1
         TITLE "Rateios" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f_main
     fi-cod_rat_ctbl-de AT ROW 2.29 COL 13 COLON-ALIGNED HELP
          "C¢digo Rateio Cont†bil" WIDGET-ID 78
     fi-cod_rat_ctbl-ate AT ROW 2.29 COL 32 COLON-ALIGNED HELP
          "C¢digo Rateio Cont†bil" WIDGET-ID 80
     bt-filtro AT ROW 2.33 COL 54 WIDGET-ID 82
     fi-cod_empresa-nova AT ROW 1.38 COL 93 COLON-ALIGNED HELP
          "C¢digo Empresa" WIDGET-ID 88
     bt-busca-empr-dest AT ROW 1.33 COL 102.29 WIDGET-ID 86
     fi-cod_estab-novo AT ROW 2.38 COL 93 COLON-ALIGNED HELP
          "C¢digo Estabelecimento" WIDGET-ID 34
     bt-busca-estab-dest AT ROW 2.33 COL 102.29 WIDGET-ID 32
     br-de-para-rat AT ROW 3.5 COL 88 WIDGET-ID 600
     bt-copia-rateio AT ROW 10.75 COL 93 WIDGET-ID 100
     bt-altera-estab AT ROW 10.75 COL 115.14 WIDGET-ID 98
     fi-cod_empresa-orig AT ROW 1.29 COL 13 COLON-ALIGNED HELP
          "C¢digo Empresa" WIDGET-ID 28
     bt-busca-empr-orig AT ROW 1.25 COL 22.29 WIDGET-ID 66
     fi-desc-empr-orig AT ROW 1.29 COL 24.14 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     br-mapa AT ROW 16.42 COL 83 WIDGET-ID 500
     fi-desc-empr-dest AT ROW 1.38 COL 104.14 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     fi-desc-estab-novo AT ROW 2.38 COL 104.14 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     bt_ok AT ROW 23.04 COL 3 WIDGET-ID 26
     bt_cancelar AT ROW 23.04 COL 13.86 WIDGET-ID 36
     Btn_Help AT ROW 23.04 COL 128 WIDGET-ID 24
     br-rateio AT ROW 3.5 COL 2 WIDGET-ID 200
     br-itens-dest-rat AT ROW 16.42 COL 2 WIDGET-ID 400
     br-itens-orig-rat AT ROW 9.83 COL 2 WIDGET-ID 300
     bt-add-mod AT ROW 5 COL 83 WIDGET-ID 38
     bt-del-mod AT ROW 6.5 COL 83 WIDGET-ID 40
     rt_cxcf AT ROW 22.83 COL 1 WIDGET-ID 2
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 48
     RECT-3 AT ROW 1 COL 82.29 WIDGET-ID 52
     RECT-4 AT ROW 9.58 COL 1 WIDGET-ID 64
     RECT-5 AT ROW 16.17 COL 1 WIDGET-ID 84
     RECT-6 AT ROW 9.58 COL 82.29 WIDGET-ID 94
     RECT-7 AT ROW 16.17 COL 82.29 WIDGET-ID 96
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 138 BY 23.25
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
         TITLE              = "C¢pia Rateio Cont†bil (ESFGL002)"
         HEIGHT             = 23.25
         WIDTH              = 138
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-de-para-rat bt-busca-estab-dest f_main */
/* BROWSE-TAB br-mapa fi-desc-empr-orig f_main */
/* BROWSE-TAB br-rateio Btn_Help f_main */
/* BROWSE-TAB br-itens-dest-rat br-rateio f_main */
/* BROWSE-TAB br-itens-orig-rat br-itens-dest-rat f_main */
/* SETTINGS FOR BUTTON bt-busca-empr-orig IN FRAME f_main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod_empresa-orig IN FRAME f_main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-empr-dest IN FRAME f_main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-empr-orig IN FRAME f_main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-estab-novo IN FRAME f_main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-de-para-rat
/* Query rebuild information for BROWSE br-de-para-rat
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-rat_ctbl NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-de-para-rat */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens-dest-rat
/* Query rebuild information for BROWSE br-itens-dest-rat
     _TblList          = "item_rat_ctbl,rat_ctbl_dest OF item_rat_ctbl"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > item_rat_ctbl.cod_empresa
"item_rat_ctbl.cod_empresa" ? ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > item_rat_ctbl.cod_rat_ctbl
"item_rat_ctbl.cod_rat_ctbl" ? ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > item_rat_ctbl.num_seq_rat_ctbl
"item_rat_ctbl.num_seq_rat_ctbl" ? ? "integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > rat_ctbl_dest.num_ord_seq_rat_ctbl
"rat_ctbl_dest.num_ord_seq_rat_ctbl" ? ? "integer" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > rat_ctbl_dest.cod_cta_ctbl
"rat_ctbl_dest.cod_cta_ctbl" ? ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = rat_ctbl_dest.ind_rat_ctbl_dest_estab
     _FldNameList[7]   > rat_ctbl_dest.cod_estab
"rat_ctbl_dest.cod_estab" ? ? "character" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > rat_ctbl_dest.ind_rat_ctbl_dest_ccusto
"rat_ctbl_dest.ind_rat_ctbl_dest_ccusto" ? ? "character" ? ? ? ? ? ? no ? no no "15.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-itens-dest-rat */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens-orig-rat
/* Query rebuild information for BROWSE br-itens-orig-rat
     _TblList          = "item_rat_ctbl,rat_ctbl_orig OF item_rat_ctbl"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > item_rat_ctbl.cod_empresa
"item_rat_ctbl.cod_empresa" ? ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > item_rat_ctbl.cod_rat_ctbl
"item_rat_ctbl.cod_rat_ctbl" ? ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > item_rat_ctbl.num_seq_rat_ctbl
"item_rat_ctbl.num_seq_rat_ctbl" ? ? "integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > rat_ctbl_orig.num_ord_seq_rat_ctbl
"rat_ctbl_orig.num_ord_seq_rat_ctbl" ? ? "integer" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > rat_ctbl_orig.cod_cta_ctbl_inic
"rat_ctbl_orig.cod_cta_ctbl_inic" ? ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > rat_ctbl_orig.cod_cta_ctbl_fim
"rat_ctbl_orig.cod_cta_ctbl_fim" ? ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > rat_ctbl_orig.cod_estab_inic
"rat_ctbl_orig.cod_estab_inic" ? ? "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = rat_ctbl_orig.cod_estab_fim
     _Query            is OPENED
*/  /* BROWSE br-itens-orig-rat */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-mapa
/* Query rebuild information for BROWSE br-mapa
     _TblList          = "rat_ctbl_dest_mapa"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > rat_ctbl_dest_mapa.cod_empresa
"rat_ctbl_dest_mapa.cod_empresa" ? ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > rat_ctbl_dest_mapa.cod_rat_ctbl
"rat_ctbl_dest_mapa.cod_rat_ctbl" ? ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > rat_ctbl_dest_mapa.num_seq_rat_ctbl
"rat_ctbl_dest_mapa.num_seq_rat_ctbl" ? ? "integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > rat_ctbl_dest_mapa.num_ord_seq_rat_ctbl
"rat_ctbl_dest_mapa.num_ord_seq_rat_ctbl" ? ? "integer" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = rat_ctbl_dest_mapa.cod_estab
     _FldNameList[6]   > rat_ctbl_dest_mapa.cod_mapa_distrib_ccusto
"rat_ctbl_dest_mapa.cod_mapa_distrib_ccusto" ? ? "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-mapa */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-rateio
/* Query rebuild information for BROWSE br-rateio
     _TblList          = "rat_ctbl"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = rat_ctbl.cod_empresa
     _FldNameList[2]   = rat_ctbl.cod_rat_ctbl
     _FldNameList[3]   > rat_ctbl.des_rat_ctbl
"rat_ctbl.des_rat_ctbl" ? ? "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = rat_ctbl.dat_inic_valid
     _FldNameList[5]   = rat_ctbl.dat_fim_valid
     _Query            is OPENED
*/  /* BROWSE br-rateio */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* C¢pia Rateio Cont†bil (ESFGL002) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* C¢pia Rateio Cont†bil (ESFGL002) */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add-mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add-mod C-Win
ON CHOOSE OF bt-add-mod IN FRAME f_main /* Add Modulo */
DO:
    DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.

    DO icont = 1 TO br-rateio:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
        if br-rateio:fetch-selected-row(icont) then do:
            IF  br-rateio:IS-ROW-SELECTED(br-rateio:FOCUSED-ROW) in frame {&FRAME-NAME} THEN DO:
                IF AVAILABLE rat_ctbl THEN DO:
                    FIND FIRST tt-rat_ctbl
                        WHERE tt-rat_ctbl.cod_empresa-de    = rat_ctbl.cod_empresa
                        AND   tt-rat_ctbl.cod_rat_ctbl-de   = rat_ctbl.cod_rat_ctbl NO-ERROR.
                    IF NOT AVAILABLE tt-rat_ctbl THEN DO:
                        CREATE tt-rat_ctbl.
                        ASSIGN tt-rat_ctbl.cod_empresa-de    = rat_ctbl.cod_empresa
                               tt-rat_ctbl.cod_rat_ctbl-de   = rat_ctbl.cod_rat_ctbl
                               tt-rat_ctbl.des_rat_ctbl-de   = rat_ctbl.des_rat_ctbl
                               tt-rat_ctbl.cod_empresa-para  = INPUT frame {&FRAME-NAME} fi-cod_empresa-nova
                               tt-rat_ctbl.cod_rat_ctbl-para = rat_ctbl.cod_rat_ctbl
                               tt-rat_ctbl.des_rat_ctbl-para = rat_ctbl.des_rat_ctbl
                               .
                    END.
                END.
            END.
        END.
    END.

    CLOSE QUERY br-de-para-rat.

    OPEN QUERY br-de-para-rat FOR EACH tt-rat_ctbl NO-LOCK INDEXED-REPOSITION.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-altera-estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-altera-estab C-Win
ON CHOOSE OF bt-altera-estab IN FRAME f_main /* Altera Estab */
DO:

    DISABLE TRIGGERS FOR DUMP OF rat_ctbl_orig.
    DISABLE TRIGGERS FOR DUMP OF rat_ctbl_dest.
    DISABLE TRIGGERS FOR DUMP OF rat_ctbl_dest_mapa.
    DISABLE TRIGGERS FOR LOAD OF rat_ctbl_orig.
    DISABLE TRIGGERS FOR LOAD OF rat_ctbl_dest.
    DISABLE TRIGGERS FOR LOAD OF rat_ctbl_dest_mapa.

    DEFINE VARIABLE l-altera AS LOGICAL     NO-UNDO.

    ASSIGN l-altera = YES.

    IF INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo = "" THEN DO:
        ASSIGN l-altera = NO.

        run utp/ut-msgs.p (input "show":U,
                           input 17006,
                           input "Estabelecimento n∆o informado!~~Informe um estabelecimento v†lido.").
        APPLY "ENTRY" TO fi-cod_estab-novo.
        UNDO, RETURN.
    END.

    find FIRST estabelecimento NO-LOCK
        where estabelecimento.cod_empresa = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-nova:SCREEN-VALUE
        AND   estabelecimento.cod_estab   = INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo:SCREEN-VALUE
        no-error.
    IF NOT AVAILABLE estabelecimento THEN DO:
        ASSIGN l-altera = NO.

        run utp/ut-msgs.p (input "show":U,
                           input 17006,
                           input "Estabelecimento inv†lido!~~Informe um estabelecimento v†lido para a NOVA empresa.").
        APPLY "ENTRY" TO fi-cod_estab-novo.
        UNDO, RETURN.
    END.

    FOR EACH rat_ctbl_dest_mapa NO-LOCK
        WHERE rat_ctbl_dest_mapa.cod_empresa   = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-orig
        AND   rat_ctbl_dest_mapa.cod_rat_ctbl >= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-de
        AND   rat_ctbl_dest_mapa.cod_rat_ctbl <= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-ate

        :
        FIND FIRST mapa_distrib_ccusto NO-LOCK
            WHERE mapa_distrib_ccusto.cod_estab               = INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo
            AND   mapa_distrib_ccusto.cod_mapa_distrib_ccusto = rat_ctbl_dest_mapa.cod_mapa_distrib_ccusto
            NO-ERROR.
        IF NOT AVAILABLE mapa_distrib_ccusto THEN DO:
            ASSIGN l-altera = NO.

            run utp/ut-msgs.p (input "show":U,
                               input 17006,
                               input "Mapa Distribuiá∆o Centro de Custo invalido!~~N∆o existe Mapa de Distribuiá∆o de Centro de Custo < " 
                                   + rat_ctbl_dest_mapa.cod_mapa_distrib_ccusto + " > para o Estabelecimento < " 
                                   + INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo + " > .").
            APPLY "ENTRY" TO fi-cod_estab-novo.
            LEAVE.
        END.
    END.

    IF l-altera = YES THEN DO:
        run utp/ut-msgs.p (input "show",
                           input 27100,
                           input "ATENÄ«O! Alterar os campos de Estabelecimento ?~~Confirma a alteraá∆o de todos os campos de Estabelecimento para < " 
                               + INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo + " > ? TODOS os rateios do filtro ser∆o alterados ! ").
        IF LOGICAL (RETURN-VALUE) = YES THEN DO:

            FOR EACH item_rat_ctbl NO-LOCK
                WHERE item_rat_ctbl.cod_empresa   = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-orig
                AND   item_rat_ctbl.cod_rat_ctbl >= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-de
                AND   item_rat_ctbl.cod_rat_ctbl <= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-ate:

                FOR EACH rat_ctbl_orig OF item_rat_ctbl:
                    ASSIGN rat_ctbl_orig.cod_estab_inic = INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo
                           rat_ctbl_orig.cod_estab_fim  = INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo.
                END.

                FOR EACH rat_ctbl_dest OF item_rat_ctbl:
                    IF rat_ctbl_dest.ind_rat_ctbl_dest_estab = "Informa" THEN

                        ASSIGN rat_ctbl_dest.cod_estab = INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo.
                END.

                FOR EACH rat_ctbl_dest_mapa OF item_rat_ctbl:
                    ASSIGN rat_ctbl_dest_mapa.cod_estab = INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo.
                END.
            END.

            RUN pi-open-query.
        END.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-empr-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-empr-dest C-Win
ON CHOOSE OF bt-busca-empr-dest IN FRAME f_main /* Busca Empresa Destino */
DO:

    run prgint/utb/utb069ka.p /*prg_sea_empresa*/.

    if  v_rec_empresa <> ?
    then do:
        find empresa where recid(empresa) = v_rec_empresa no-lock no-error.
        assign fi-cod_empresa-nova:screen-value in frame {&FRAME-NAME} = string(empresa.cod_empresa).
        assign fi-desc-empr-dest:screen-value in frame {&FRAME-NAME} = empresa.nom_razao_social.
    end /* if */.
  
    APPLY "LEAVE" TO fi-cod_empresa-nova.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-empr-orig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-empr-orig C-Win
ON CHOOSE OF bt-busca-empr-orig IN FRAME f_main /* Busca Empresa Origem */
DO:

    run prgint/utb/utb069ka.p /*prg_sea_empresa*/.

    if  v_rec_empresa <> ?
    then do:
        find empresa where recid(empresa) = v_rec_empresa no-lock no-error.
        assign fi-cod_empresa-orig:screen-value in frame {&FRAME-NAME} = string(empresa.cod_empresa).
        assign fi-desc-empr-orig:screen-value in frame {&FRAME-NAME} = empresa.nom_razao_social.

        APPLY "CHOOSE" TO bt-filtro.
    end /* if */.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-estab-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-estab-dest C-Win
ON CHOOSE OF bt-busca-estab-dest IN FRAME f_main /* Busca Estab Dest */
DO:
    run esp/esutb071na.p (Input INPUT FRAME {&FRAME-NAME} fi-cod_empresa-nova).

    if  v_rec_estabelecimento <> ? then do:
        find estabelecimento where recid(estabelecimento) = v_rec_estabelecimento no-lock no-error.
        assign fi-cod_estab-novo:screen-value in frame {&FRAME-NAME} = string(estabelecimento.cod_estab).

        assign fi-desc-estab-novo:screen-value in frame {&FRAME-NAME} = estabelecimento.nom_pessoa.

    end /* if */.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-copia-rateio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-copia-rateio C-Win
ON CHOOSE OF bt-copia-rateio IN FRAME f_main /* Copia Rateio */
DO:
    DEFINE VARIABLE l-cria AS LOGICAL     NO-UNDO.

    DEFINE BUFFER BF-rat_ctbl           FOR rat_ctbl.           
    DEFINE BUFFER BF-item_rat_ctbl      FOR item_rat_ctbl.           
    DEFINE BUFFER BF-rat_ctbl_orig      FOR rat_ctbl_orig.      
    DEFINE BUFFER BF-rat_ctbl_dest      FOR rat_ctbl_dest.      
    DEFINE BUFFER BF-rat_ctbl_dest_mapa FOR rat_ctbl_dest_mapa. 

    ASSIGN l-cria = YES.

    FIND FIRST empresa NO-LOCK 
        WHERE empresa.cod_empresa = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-nova
        NO-ERROR.
    IF NOT AVAILABLE empresa THEN DO:
        ASSIGN l-cria = NO.

        run utp/ut-msgs.p (input "show":U,
                           input 17006,
                           input "NOVA empresa inv†lida!~~Informe uma NOVA empresa v†lida.").
        APPLY "ENTRY" TO fi-cod_empresa-nova.
        UNDO, RETURN.
    END.

    FOR EACH tt-rat_ctbl NO-LOCK:
        IF tt-rat_ctbl.cod_rat_ctbl-para = "" THEN DO:
            ASSIGN l-cria = NO.

            run utp/ut-msgs.p (input "show":U,
                               input 17006,
                               input "NOVO Rateio n∆o informado!~~Informe um NOVO nome do Rateio.").
            APPLY "ENTRY" TO br-de-para-rat.
            UNDO, RETURN.
        END.
    END.

    find FIRST estabelecimento NO-LOCK
        where estabelecimento.cod_empresa = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-nova:SCREEN-VALUE
        AND   estabelecimento.cod_estab   = INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo:SCREEN-VALUE
        no-error.
    IF NOT AVAILABLE estabelecimento THEN DO:
        ASSIGN l-cria = NO.

        run utp/ut-msgs.p (input "show":U,
                           input 17006,
                           input "Estabelecimento inv†lido!~~Informe um estabelecimento v†lido para a NOVA empresa.").
        APPLY "ENTRY" TO fi-cod_estab-novo.
        UNDO, RETURN.
    END.

    FOR EACH tt-rat_ctbl NO-LOCK:
        FIND FIRST BF-rat_ctbl NO-LOCK
            WHERE BF-rat_ctbl.cod_empresa  = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-nova
            AND   BF-rat_ctbl.cod_rat_ctbl = tt-rat_ctbl.cod_rat_ctbl-para NO-ERROR.
        IF AVAILABLE BF-rat_ctbl THEN DO:
            ASSIGN l-cria = NO.

            run utp/ut-msgs.p (input "show":U,
                               input 17006,
                               input "NOVO < " + tt-rat_ctbl.cod_rat_ctbl-para + " > Rateio j† existe!~~Informe um NOVO nome do Rateio ainda n∆o existente na empresa < " + INPUT FRAME {&FRAME-NAME} fi-cod_empresa-nova + " >.").
            APPLY "ENTRY" TO br-de-para-rat.
            UNDO, RETURN.
        END.
    END.

    FOR EACH tt-rat_ctbl NO-LOCK:
        FIND rat_ctbl NO-LOCK
            WHERE rat_ctbl.cod_empresa  = tt-rat_ctbl.cod_empresa-de
            AND   rat_ctbl.cod_rat_ctbl = tt-rat_ctbl.cod_rat_ctbl-de NO-ERROR.

        FOR EACH rat_ctbl_dest_mapa OF rat_ctbl NO-LOCK:
            FIND FIRST mapa_distrib_ccusto NO-LOCK
                WHERE mapa_distrib_ccusto.cod_empresa             = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-nova
                AND   mapa_distrib_ccusto.cod_estab               = INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo
                AND   mapa_distrib_ccusto.cod_mapa_distrib_ccusto = rat_ctbl_dest_mapa.cod_mapa_distrib_ccusto
                NO-ERROR.
            IF NOT AVAILABLE mapa_distrib_ccusto THEN DO:
                ASSIGN l-cria = NO.
    
                run utp/ut-msgs.p (input "show":U,
                                   input 17006,
                                   input "Mapa Distribuiá∆o Centro de Custo invalido!~~N∆o existe Mapa de Distribuiá∆o de Centro de Custo < " 
                                       + rat_ctbl_dest_mapa.cod_mapa_distrib_ccusto + " > para o Estabelecimento < " 
                                       + INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo + " > .").
                APPLY "ENTRY" TO fi-cod_estab-novo.
                LEAVE.
            END.
        END.
    END.

    IF l-cria = YES THEN DO:

        run utp/ut-msgs.p (input "show",
                           input 27100,
                           input "Copia Rateios selecionados ?~~Confirma a c¢pia dos Rateios selecinados ?").
        IF LOGICAL (RETURN-VALUE) = YES THEN DO:

            FOR EACH tt-rat_ctbl NO-LOCK:

                FIND rat_ctbl NO-LOCK
                    WHERE rat_ctbl.cod_empresa  = tt-rat_ctbl.cod_empresa-de
                    AND   rat_ctbl.cod_rat_ctbl = tt-rat_ctbl.cod_rat_ctbl-de NO-ERROR.

                CREATE BF-rat_ctbl.
                BUFFER-COPY rat_ctbl EXCEPT cod_empresa cod_rat_ctbl TO BF-rat_ctbl.
                ASSIGN BF-rat_ctbl.cod_empresa  = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-nova  
                       BF-rat_ctbl.cod_rat_ctbl = tt-rat_ctbl.cod_rat_ctbl-para
                       BF-rat_ctbl.des_rat_ctbl = tt-rat_ctbl.des_rat_ctbl-para.

                FOR EACH item_rat_ctbl OF rat_ctbl NO-LOCK:
                    CREATE BF-item_rat_ctbl.
                        BUFFER-COPY item_rat_ctbl EXCEPT cod_empresa cod_rat_ctbl TO BF-item_rat_ctbl.
                        ASSIGN BF-item_rat_ctbl.cod_empresa  = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-nova  
                               BF-item_rat_ctbl.cod_rat_ctbl = tt-rat_ctbl.cod_rat_ctbl-para.

                    FOR EACH rat_ctbl_orig OF item_rat_ctbl NO-LOCK:
                        CREATE BF-rat_ctbl_orig.
                        BUFFER-COPY rat_ctbl_orig EXCEPT cod_empresa cod_rat_ctbl TO BF-rat_ctbl_orig.
                        ASSIGN BF-rat_ctbl_orig.cod_empresa    = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-nova  
                               BF-rat_ctbl_orig.cod_rat_ctbl   = tt-rat_ctbl.cod_rat_ctbl-para
                               BF-rat_ctbl_orig.cod_estab_inic = INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo
                               BF-rat_ctbl_orig.cod_estab_fim  = INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo.
                    END.

                    FOR EACH rat_ctbl_dest OF item_rat_ctbl NO-LOCK:
                        CREATE BF-rat_ctbl_dest.
                        BUFFER-COPY rat_ctbl_dest EXCEPT cod_empresa cod_rat_ctbl TO BF-rat_ctbl_dest.
                        ASSIGN BF-rat_ctbl_dest.cod_empresa  = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-nova  
                               BF-rat_ctbl_dest.cod_rat_ctbl = tt-rat_ctbl.cod_rat_ctbl-para.

                        IF BF-rat_ctbl_dest.ind_rat_ctbl_dest_estab = "Informa" THEN
                            ASSIGN BF-rat_ctbl_dest.cod_estab = INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo.

                    END.

                    FOR EACH rat_ctbl_dest_mapa OF item_rat_ctbl NO-LOCK:
                        CREATE BF-rat_ctbl_dest_mapa.
                        BUFFER-COPY rat_ctbl_dest_mapa EXCEPT cod_empresa cod_rat_ctbl TO BF-rat_ctbl_dest_mapa.
                        ASSIGN BF-rat_ctbl_dest_mapa.cod_empresa  = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-nova  
                               BF-rat_ctbl_dest_mapa.cod_rat_ctbl = tt-rat_ctbl.cod_rat_ctbl-para
                               BF-rat_ctbl_dest_mapa.cod_estab    = INPUT FRAME {&FRAME-NAME} fi-cod_estab-novo.
                    END.
                END.
            END.

            MESSAGE "Rateios copiados com sucesso !"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RUN pi-open-query.

            CLOSE QUERY br-de-para-rat.

            EMPTY TEMP-TABLE tt-rat_ctbl.

            OPEN QUERY br-de-para-rat FOR EACH tt-rat_ctbl NO-LOCK INDEXED-REPOSITION.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del-mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del-mod C-Win
ON CHOOSE OF bt-del-mod IN FRAME f_main /* Del Modulo */
DO:
    
    IF AVAILABLE tt-rat_ctbl THEN DO:
        FIND CURRENT tt-rat_ctbl EXCLUSIVE-LOCK NO-ERROR.
        DELETE tt-rat_ctbl.
    END.

    CLOSE QUERY br-de-para-rat.

    OPEN QUERY br-de-para-rat FOR EACH tt-rat_ctbl NO-LOCK INDEXED-REPOSITION.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro C-Win
ON CHOOSE OF bt-filtro IN FRAME f_main /* Filtro */
DO:
    run pi-open-query.
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
ON CHOOSE OF bt_cancelar IN FRAME f_main /* Cancelar */
DO:
  apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_ok C-Win
ON CHOOSE OF bt_ok IN FRAME f_main /* OK */
DO:
    /*
    IF INPUT FRAME {&FRAME-NAME} fi-cenario-origem = "" THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Cen†rio em branco!~~Informe um cen†rio v†lido para o campo Cen†rio Cont†bil de Origem.").
        APPLY "ENTRY" TO fi-cenario-origem.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST cenar_ctbl NO-LOCK 
            WHERE cenar_ctbl.cod_cenar_ctbl = INPUT FRAME {&FRAME-NAME} fi-cenario-origem NO-ERROR.
        IF NOT AVAILABLE cenar_ctbl THEN DO:
            run utp/ut-msgs.p (input "show":U, 
                               input 17006, 
                               input "Cen†rio inv†lido!~~Informe um cen†rio v†lido para o campo Cen†rio Cont†bil de Origem.").
            APPLY "ENTRY" TO fi-cenario-origem.
            RETURN NO-APPLY.
        END.
    END.
    */

    apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod_empresa-nova
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod_empresa-nova C-Win
ON LEAVE OF fi-cod_empresa-nova IN FRAME f_main /* NOVA Empresa */
DO:
    find FIRST empresa
        where empresa.cod_empresa = INPUT frame {&FRAME-NAME} fi-cod_empresa-nova
        no-lock no-error.
    IF AVAILABLE empresa THEN DO:
        assign fi-desc-empr-dest:screen-value in frame {&FRAME-NAME} = empresa.nom_razao_social.
    END.
    ELSE DO:
        assign fi-desc-empr-dest:screen-value in frame {&FRAME-NAME} = "".
    END.

    IF INPUT frame {&FRAME-NAME} fi-cod_empresa-nova <> INPUT frame {&FRAME-NAME} fi-cod_empresa-orig THEN
        bt-altera-estab:SENSITIVE = NO.
    ELSE 
        bt-altera-estab:SENSITIVE = YES.

    FOR EACH tt-rat_ctbl:
        ASSIGN tt-rat_ctbl.cod_empresa-para = INPUT frame {&FRAME-NAME} fi-cod_empresa-nova.
    END.


    CLOSE QUERY br-de-para-rat.

    EMPTY TEMP-TABLE tt-rat_ctbl.

    OPEN QUERY br-de-para-rat FOR EACH tt-rat_ctbl NO-LOCK INDEXED-REPOSITION.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod_empresa-orig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod_empresa-orig C-Win
ON LEAVE OF fi-cod_empresa-orig IN FRAME f_main /* Empresa Origem */
DO:
    find FIRST empresa
        where empresa.cod_empresa = INPUT frame {&FRAME-NAME} fi-cod_empresa-orig
        no-lock no-error.
    IF AVAILABLE empresa THEN DO:
        assign fi-desc-empr-orig:screen-value in frame {&FRAME-NAME} = empresa.nom_razao_social.
    END.
    ELSE DO:
        assign fi-desc-empr-orig:screen-value in frame {&FRAME-NAME} = "".
    END.

    APPLY "CHOOSE" TO bt-filtro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod_estab-novo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod_estab-novo C-Win
ON LEAVE OF fi-cod_estab-novo IN FRAME f_main /* NOVO Estabel */
DO:
    find estabelecimento 
        where estabelecimento.cod_empresa = fi-cod_empresa-nova:screen-value in frame {&FRAME-NAME}
        AND   estabelecimento.cod_estab   = fi-cod_estab-novo:screen-value in frame {&FRAME-NAME}
        no-lock no-error.
    IF AVAILABLE estabelecimento THEN DO:
        assign fi-desc-estab-novo:screen-value in frame {&FRAME-NAME} = estabelecimento.nom_pessoa.
    END.
    ELSE DO:
        assign fi-desc-estab-novo:screen-value in frame {&FRAME-NAME} = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod_rat_ctbl-ate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod_rat_ctbl-ate C-Win
ON LEAVE OF fi-cod_rat_ctbl-ate IN FRAME f_main /* AtÇ */
DO:
    APPLY "CHOOSE" TO bt-filtro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod_rat_ctbl-de
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod_rat_ctbl-de C-Win
ON LEAVE OF fi-cod_rat_ctbl-de IN FRAME f_main /* Rateio Cont†bil */
DO:
    APPLY "CHOOSE" TO bt-filtro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-de-para-rat
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
  DISPLAY fi-cod_rat_ctbl-de fi-cod_rat_ctbl-ate fi-cod_empresa-nova 
          fi-cod_estab-novo fi-cod_empresa-orig fi-desc-empr-orig 
          fi-desc-empr-dest fi-desc-estab-novo 
      WITH FRAME f_main IN WINDOW C-Win.
  ENABLE fi-cod_rat_ctbl-de fi-cod_rat_ctbl-ate bt-filtro fi-cod_empresa-nova 
         bt-busca-empr-dest fi-cod_estab-novo bt-busca-estab-dest 
         br-de-para-rat bt-copia-rateio bt-altera-estab br-mapa bt_ok 
         bt_cancelar Btn_Help br-rateio br-itens-dest-rat br-itens-orig-rat 
         bt-add-mod bt-del-mod rt_cxcf RECT-1 RECT-3 RECT-4 RECT-5 RECT-6 
         RECT-7 
      WITH FRAME f_main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f_main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-open-query C-Win 
PROCEDURE pi-open-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CLOSE QUERY br-rateio.
    CLOSE QUERY br-itens-orig-rat.
    CLOSE QUERY br-itens-dest-rat.
    CLOSE QUERY br-mapa.
    /*
    CLOSE QUERY br-de-para-rat.
    */

    OPEN QUERY br-rateio FOR EACH rat_ctbl NO-LOCK
        WHERE rat_ctbl.cod_empresa   = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-orig
        AND   rat_ctbl.cod_rat_ctbl >= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-de
        AND   rat_ctbl.cod_rat_ctbl <= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-ate
        INDEXED-REPOSITION.

    OPEN QUERY br-itens-orig-rat FOR EACH item_rat_ctbl NO-LOCK
        WHERE item_rat_ctbl.cod_empresa   = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-orig
        AND   item_rat_ctbl.cod_rat_ctbl >= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-de
        AND   item_rat_ctbl.cod_rat_ctbl <= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-ate
        ,EACH rat_ctbl_orig OF item_rat_ctbl NO-LOCK 
        INDEXED-REPOSITION.

    OPEN QUERY br-itens-dest-rat FOR EACH item_rat_ctbl NO-LOCK
        WHERE item_rat_ctbl.cod_empresa   = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-orig
        AND   item_rat_ctbl.cod_rat_ctbl >= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-de
        AND   item_rat_ctbl.cod_rat_ctbl <= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-ate
        ,EACH rat_ctbl_dest OF item_rat_ctbl NO-LOCK 
        INDEXED-REPOSITION.

    OPEN QUERY br-mapa FOR EACH rat_ctbl_dest_mapa NO-LOCK 
        WHERE rat_ctbl_dest_mapa.cod_empresa   = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-orig
        AND   rat_ctbl_dest_mapa.cod_rat_ctbl >= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-de
        AND   rat_ctbl_dest_mapa.cod_rat_ctbl <= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-ate
        INDEXED-REPOSITION.

        /*
    EMPTY TEMP-TABLE tt-rat_ctbl.

    FOR EACH rat_ctbl NO-LOCK
        WHERE rat_ctbl.cod_empresa   = INPUT FRAME {&FRAME-NAME} fi-cod_empresa-orig
        AND   rat_ctbl.cod_rat_ctbl >= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-de
        AND   rat_ctbl.cod_rat_ctbl <= INPUT FRAME {&FRAME-NAME} fi-cod_rat_ctbl-ate:
        CREATE tt-rat_ctbl.
        ASSIGN tt-rat_ctbl.cod_empresa-de    = rat_ctbl.cod_empresa
               tt-rat_ctbl.cod_rat_ctbl-de   = rat_ctbl.cod_rat_ctbl
               tt-rat_ctbl.des_rat_ctbl-de   = rat_ctbl.des_rat_ctbl
               tt-rat_ctbl.cod_empresa-para  = INPUT frame {&FRAME-NAME} fi-cod_empresa-nova
               tt-rat_ctbl.cod_rat_ctbl-para = rat_ctbl.cod_rat_ctbl
               tt-rat_ctbl.des_rat_ctbl-para = rat_ctbl.des_rat_ctbl
               .
    END.

    OPEN QUERY br-de-para-rat FOR EACH tt-rat_ctbl NO-LOCK INDEXED-REPOSITION.
    */

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

    ASSIGN fi-cod_empresa-orig:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v_cod_empres_usuar.
    ASSIGN fi-cod_empresa-nova:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v_cod_empres_usuar.

    RUN pi-open-query.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

