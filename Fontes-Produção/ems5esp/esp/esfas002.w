&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

/*****************************************************************************
** Programa..............: esfas002
** Descricao.............: Parametro Equaliza‡Æo Cen rio
** Versao................: 1.00.00.000
** Procedimento..........: esfas002
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 27/05/2013
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

def buffer empresa       FOR ems5.empresa.

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt-empresa-de     LIKE ems5.empresa.
DEFINE TEMP-TABLE tt-modulo-de      LIKE modul_dtsul.
DEFINE TEMP-TABLE tt-empresa-para   LIKE ems5.empresa.
DEFINE TEMP-TABLE tt-modulo-para    LIKE modul_dtsul.
DEFINE TEMP-TABLE tt-grupo-de       LIKE grp_usuar.
DEFINE TEMP-TABLE tt-grupo-para     LIKE grp_usuar.


def new global shared var v_rec_cenar_ctbl
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f_main
&Scoped-define BROWSE-NAME br-empresa-de

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-empresa-de tt-empresa-para tt-grupo-de ~
tt-grupo-para tt-modulo-de tt-modulo-para

/* Definitions for BROWSE br-empresa-de                                 */
&Scoped-define FIELDS-IN-QUERY-br-empresa-de tt-empresa-de.cod_empresa tt-empresa-de.nom_abrev   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-empresa-de   
&Scoped-define SELF-NAME br-empresa-de
&Scoped-define QUERY-STRING-br-empresa-de FOR EACH tt-empresa-de INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-empresa-de OPEN QUERY {&SELF-NAME} FOR EACH tt-empresa-de INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-empresa-de tt-empresa-de
&Scoped-define FIRST-TABLE-IN-QUERY-br-empresa-de tt-empresa-de


/* Definitions for BROWSE br-empresa-para                               */
&Scoped-define FIELDS-IN-QUERY-br-empresa-para tt-empresa-para.cod_empresa tt-empresa-para.nom_abrev   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-empresa-para   
&Scoped-define SELF-NAME br-empresa-para
&Scoped-define QUERY-STRING-br-empresa-para FOR EACH tt-empresa-para INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-empresa-para OPEN QUERY {&SELF-NAME} FOR EACH tt-empresa-para INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-empresa-para tt-empresa-para
&Scoped-define FIRST-TABLE-IN-QUERY-br-empresa-para tt-empresa-para


/* Definitions for BROWSE br-grupo-de                                   */
&Scoped-define FIELDS-IN-QUERY-br-grupo-de tt-grupo-de.cod_grp_usuar tt-grupo-de.des_grp_usuar   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-grupo-de   
&Scoped-define SELF-NAME br-grupo-de
&Scoped-define QUERY-STRING-br-grupo-de FOR EACH tt-grupo-de INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-grupo-de OPEN QUERY {&SELF-NAME} FOR EACH tt-grupo-de INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-grupo-de tt-grupo-de
&Scoped-define FIRST-TABLE-IN-QUERY-br-grupo-de tt-grupo-de


/* Definitions for BROWSE br-grupo-para                                 */
&Scoped-define FIELDS-IN-QUERY-br-grupo-para tt-grupo-para.cod_grp_usuar tt-grupo-para.des_grp_usuar   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-grupo-para   
&Scoped-define SELF-NAME br-grupo-para
&Scoped-define QUERY-STRING-br-grupo-para FOR EACH tt-grupo-para INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-grupo-para OPEN QUERY {&SELF-NAME} FOR EACH tt-grupo-para INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-grupo-para tt-grupo-para
&Scoped-define FIRST-TABLE-IN-QUERY-br-grupo-para tt-grupo-para


/* Definitions for BROWSE br-modulo-de                                  */
&Scoped-define FIELDS-IN-QUERY-br-modulo-de tt-modulo-de.cod_modul_dtsul tt-modulo-de.des_modul_dtsul   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-modulo-de   
&Scoped-define SELF-NAME br-modulo-de
&Scoped-define QUERY-STRING-br-modulo-de FOR EACH tt-modulo-de INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-modulo-de OPEN QUERY {&SELF-NAME} FOR EACH tt-modulo-de INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-modulo-de tt-modulo-de
&Scoped-define FIRST-TABLE-IN-QUERY-br-modulo-de tt-modulo-de


/* Definitions for BROWSE br-modulo-para                                */
&Scoped-define FIELDS-IN-QUERY-br-modulo-para tt-modulo-para.cod_modul_dtsul tt-modulo-para.des_modul_dtsul   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-modulo-para   
&Scoped-define SELF-NAME br-modulo-para
&Scoped-define QUERY-STRING-br-modulo-para FOR EACH tt-modulo-para INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-modulo-para OPEN QUERY {&SELF-NAME} FOR EACH tt-modulo-para INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-modulo-para tt-modulo-para
&Scoped-define FIRST-TABLE-IN-QUERY-br-modulo-para tt-modulo-para


/* Definitions for FRAME f_main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f_main ~
    ~{&OPEN-QUERY-br-empresa-de}~
    ~{&OPEN-QUERY-br-empresa-para}~
    ~{&OPEN-QUERY-br-grupo-de}~
    ~{&OPEN-QUERY-br-grupo-para}~
    ~{&OPEN-QUERY-br-modulo-de}~
    ~{&OPEN-QUERY-br-modulo-para}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt_rgf rt_cxcf RECT-1 RECT-2 RECT-3 RECT-4 ~
br-modulo-de br-modulo-para bt-busca-cenario-origem fi-cenario-origem ~
bt-busca-cenario-destino fi-cenario-destino bt-add-mod bt-del-mod ~
br-empresa-de br-empresa-para bt-add-emp bt-del-emp br-grupo-de ~
br-grupo-para bt-add-grp bt-del-grp bt_ok bt_cancelar Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS fi-cenario-origem fi-cenario-destino 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add-emp 
     IMAGE-UP FILE "adeicon/next-au.bmp":U
     LABEL "Add Empresa" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-add-grp 
     IMAGE-UP FILE "adeicon/next-au.bmp":U
     LABEL "Add Modulo" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-add-mod 
     IMAGE-UP FILE "adeicon/next-au.bmp":U
     LABEL "Add Modulo" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-busca-cenario-destino 
     IMAGE-UP FILE "adeicon/props.bmp":U
     LABEL "Busca Cenario Destino" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-busca-cenario-origem 
     IMAGE-UP FILE "adeicon/props.bmp":U
     LABEL "Busca Cenario Origem" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-del-emp 
     IMAGE-UP FILE "adeicon/prev-au.bmp":U
     LABEL "Del Empresa" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-del-grp 
     IMAGE-UP FILE "adeicon/prev-au.bmp":U
     LABEL "Del Modulo" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-del-mod 
     IMAGE-UP FILE "adeicon/prev-au.bmp":U
     LABEL "Del Modulo" 
     SIZE 4 BY 1.25.

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

DEFINE VARIABLE fi-cenario-destino AS CHARACTER FORMAT "x(8)" 
     LABEL "Cen rio Cont bil de Destino" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .79 NO-UNDO.

DEFINE VARIABLE fi-cenario-origem AS CHARACTER FORMAT "x(8)" 
     LABEL "Cen rio Cont bil de Origem" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 6.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 7.17.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 2.46.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 6.

DEFINE RECTANGLE rt_cxcf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 116 BY 1.42.

DEFINE RECTANGLE rt_rgf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 116 BY 1.5
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-empresa-de FOR 
      tt-empresa-de SCROLLING.

DEFINE QUERY br-empresa-para FOR 
      tt-empresa-para SCROLLING.

DEFINE QUERY br-grupo-de FOR 
      tt-grupo-de SCROLLING.

DEFINE QUERY br-grupo-para FOR 
      tt-grupo-para SCROLLING.

DEFINE QUERY br-modulo-de FOR 
      tt-modulo-de SCROLLING.

DEFINE QUERY br-modulo-para FOR 
      tt-modulo-para SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-empresa-de
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-empresa-de C-Win _FREEFORM
  QUERY br-empresa-de NO-LOCK DISPLAY
      tt-empresa-de.cod_empresa FORMAT "x(3)":U
      tt-empresa-de.nom_abrev FORMAT "x(15)":U WIDTH 26
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 33 BY 6.75
         FONT 1
         TITLE "Empresas" FIT-LAST-COLUMN.

DEFINE BROWSE br-empresa-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-empresa-para C-Win _FREEFORM
  QUERY br-empresa-para NO-LOCK DISPLAY
      tt-empresa-para.cod_empresa FORMAT "x(3)":U
      tt-empresa-para.nom_abrev FORMAT "x(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 33 BY 6.75
         FONT 1
         TITLE "Empresas Equaliza‡Æo Cen rio" FIT-LAST-COLUMN TOOLTIP "Empresas Equaliza‡Æo Branco".

DEFINE BROWSE br-grupo-de
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-grupo-de C-Win _FREEFORM
  QUERY br-grupo-de NO-LOCK DISPLAY
      tt-grupo-de.cod_grp_usuar    FORMAT "x(3)":U
      tt-grupo-de.des_grp_usuar    FORMAT "x(32)":U WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 33 BY 5.5
         FONT 1
         TITLE "Grupos" FIT-LAST-COLUMN.

DEFINE BROWSE br-grupo-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-grupo-para C-Win _FREEFORM
  QUERY br-grupo-para NO-LOCK DISPLAY
      tt-grupo-para.cod_grp_usuar    FORMAT "x(3)":U
      tt-grupo-para.des_grp_usuar    FORMAT "x(32)":U WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 33 BY 5.5
         FONT 1
         TITLE "Grupos Autorizados Cen rio" FIT-LAST-COLUMN TOOLTIP "Grupos Autorizados Cen rio".

DEFINE BROWSE br-modulo-de
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-modulo-de C-Win _FREEFORM
  QUERY br-modulo-de NO-LOCK DISPLAY
      tt-modulo-de.cod_modul_dtsul FORMAT "x(3)":U
      tt-modulo-de.des_modul_dtsul FORMAT "x(32)":U WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 33 BY 5.5
         FONT 1
         TITLE "Modulos" FIT-LAST-COLUMN.

DEFINE BROWSE br-modulo-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-modulo-para C-Win _FREEFORM
  QUERY br-modulo-para NO-LOCK DISPLAY
      tt-modulo-para.cod_modul_dtsul FORMAT "x(3)":U
      tt-modulo-para.des_modul_dtsul FORMAT "x(32)":U WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 33 BY 5.5
         FONT 1
         TITLE "Modulos Equaliza‡Æo Cen rio" FIT-LAST-COLUMN TOOLTIP "Modulos Equaliza‡Æo Cen rio".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f_main
     br-modulo-de AT ROW 2.75 COL 2 WIDGET-ID 300
     br-modulo-para AT ROW 2.75 COL 41 WIDGET-ID 400
     bt-busca-cenario-origem AT ROW 2.83 COL 110.43 WIDGET-ID 56
     fi-cenario-origem AT ROW 2.92 COL 96.72 COLON-ALIGNED HELP
          "C¢digo Cen rio Cont bil" WIDGET-ID 46
     bt-busca-cenario-destino AT ROW 3.83 COL 110.43 WIDGET-ID 58
     fi-cenario-destino AT ROW 3.92 COL 96.72 COLON-ALIGNED HELP
          "C¢digo Cen rio Cont bil" WIDGET-ID 54
     bt-add-mod AT ROW 4.58 COL 36 WIDGET-ID 38
     bt-del-mod AT ROW 6.08 COL 36 WIDGET-ID 40
     br-empresa-de AT ROW 8.75 COL 2 WIDGET-ID 200
     br-empresa-para AT ROW 8.75 COL 41 WIDGET-ID 500
     bt-add-emp AT ROW 11.21 COL 36 WIDGET-ID 42
     bt-del-emp AT ROW 12.71 COL 36 WIDGET-ID 44
     br-grupo-de AT ROW 16.08 COL 2 WIDGET-ID 600
     br-grupo-para AT ROW 16.08 COL 41 WIDGET-ID 700
     bt-add-grp AT ROW 17.92 COL 36 WIDGET-ID 60
     bt-del-grp AT ROW 19.42 COL 36 WIDGET-ID 62
     bt_ok AT ROW 22.21 COL 3 WIDGET-ID 26
     bt_cancelar AT ROW 22.21 COL 13.86 WIDGET-ID 36
     Btn_Help AT ROW 22.21 COL 105.72 WIDGET-ID 24
     rt_rgf AT ROW 1 COL 1 WIDGET-ID 4
     rt_cxcf AT ROW 22 COL 1 WIDGET-ID 2
     RECT-1 AT ROW 2.5 COL 1 WIDGET-ID 48
     RECT-2 AT ROW 8.58 COL 1 WIDGET-ID 50
     RECT-3 AT ROW 2.54 COL 75.14 WIDGET-ID 52
     RECT-4 AT ROW 15.83 COL 1 WIDGET-ID 64
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116.14 BY 22.42
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
         TITLE              = "Parametro Equaliza‡Æo Cen rio (ESFAS002)"
         HEIGHT             = 22.42
         WIDTH              = 116.14
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
/* BROWSE-TAB br-modulo-de RECT-4 f_main */
/* BROWSE-TAB br-modulo-para br-modulo-de f_main */
/* BROWSE-TAB br-empresa-de bt-del-mod f_main */
/* BROWSE-TAB br-empresa-para br-empresa-de f_main */
/* BROWSE-TAB br-grupo-de bt-del-emp f_main */
/* BROWSE-TAB br-grupo-para br-grupo-de f_main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-empresa-de
/* Query rebuild information for BROWSE br-empresa-de
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-empresa-de INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-empresa-de */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-empresa-para
/* Query rebuild information for BROWSE br-empresa-para
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-empresa-para INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-empresa-para */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-grupo-de
/* Query rebuild information for BROWSE br-grupo-de
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-grupo-de INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-grupo-de */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-grupo-para
/* Query rebuild information for BROWSE br-grupo-para
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-grupo-para INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-grupo-para */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-modulo-de
/* Query rebuild information for BROWSE br-modulo-de
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-modulo-de INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-modulo-de */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-modulo-para
/* Query rebuild information for BROWSE br-modulo-para
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-modulo-para INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-modulo-para */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Parametro Equaliza‡Æo Cen rio (ESFAS002) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Parametro Equaliza‡Æo Cen rio (ESFAS002) */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add-emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add-emp C-Win
ON CHOOSE OF bt-add-emp IN FRAME f_main /* Add Empresa */
DO:
    IF AVAILABLE tt-empresa-de THEN DO:
        CREATE tt-empresa-para.
        BUFFER-COPY tt-empresa-de TO tt-empresa-para.

        DELETE tt-empresa-de.
    END.

    RUN pi-open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add-grp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add-grp C-Win
ON CHOOSE OF bt-add-grp IN FRAME f_main /* Add Modulo */
DO:
    IF AVAILABLE tt-grupo-de THEN DO:
        CREATE tt-grupo-para.
        BUFFER-COPY tt-grupo-de TO tt-grupo-para.

        DELETE tt-grupo-de.
    END.

    RUN pi-open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add-mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add-mod C-Win
ON CHOOSE OF bt-add-mod IN FRAME f_main /* Add Modulo */
DO:
    IF AVAILABLE tt-modulo-de THEN DO:
        CREATE tt-modulo-para.
        BUFFER-COPY tt-modulo-de TO tt-modulo-para.

        DELETE tt-modulo-de.
    END.

    RUN pi-open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-cenario-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-cenario-destino C-Win
ON CHOOSE OF bt-busca-cenario-destino IN FRAME f_main /* Busca Cenario Destino */
DO:
    /* fn_generic_zoom_variable */
    if  search("prgint/utb/utb076ka.r") = ? and search("prgint/utb/utb076ka.p") = ? then do:
        message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb076ka.p"
               view-as alert-box error buttons ok.
        return.
    end.
    else
        run prgint/utb/utb076ka.p /*prg_sea_cenar_ctbl*/.

    if  v_rec_cenar_ctbl <> ?
    then do:
        find cenar_ctbl where recid(cenar_ctbl) = v_rec_cenar_ctbl no-lock no-error.
        assign fi-cenario-destino:screen-value in frame {&FRAME-NAME} = string(cenar_ctbl.cod_cenar_ctbl).

        apply "entry" to fi-cenario-destino in frame {&FRAME-NAME}.
    end /* if */.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-cenario-origem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-cenario-origem C-Win
ON CHOOSE OF bt-busca-cenario-origem IN FRAME f_main /* Busca Cenario Origem */
DO:
    /* fn_generic_zoom_variable */
    if  search("prgint/utb/utb076ka.r") = ? and search("prgint/utb/utb076ka.p") = ? then do:
        message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb076ka.p"
               view-as alert-box error buttons ok.
        return.
    end.
    else
        run prgint/utb/utb076ka.p /*prg_sea_cenar_ctbl*/.

    if  v_rec_cenar_ctbl <> ?
    then do:
        find cenar_ctbl where recid(cenar_ctbl) = v_rec_cenar_ctbl no-lock no-error.
        assign fi-cenario-origem:screen-value in frame {&FRAME-NAME} = string(cenar_ctbl.cod_cenar_ctbl).

        apply "entry" to fi-cenario-origem in frame {&FRAME-NAME}.
    end /* if */.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del-emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del-emp C-Win
ON CHOOSE OF bt-del-emp IN FRAME f_main /* Del Empresa */
DO:
    IF AVAILABLE tt-empresa-para THEN DO:
        CREATE tt-empresa-de.
        BUFFER-COPY tt-empresa-para TO tt-empresa-de.

        DELETE tt-empresa-para.
    END.

    RUN pi-open-query.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del-grp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del-grp C-Win
ON CHOOSE OF bt-del-grp IN FRAME f_main /* Del Modulo */
DO:
    IF AVAILABLE tt-grupo-para THEN DO:
        CREATE tt-grupo-de.
        BUFFER-COPY tt-grupo-para TO tt-grupo-de.

        DELETE tt-grupo-para.
    END.

    RUN pi-open-query.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del-mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del-mod C-Win
ON CHOOSE OF bt-del-mod IN FRAME f_main /* Del Modulo */
DO:
    IF AVAILABLE tt-modulo-para THEN DO:
        CREATE tt-modulo-de.
        BUFFER-COPY tt-modulo-para TO tt-modulo-de.

        DELETE tt-modulo-para.
    END.

    RUN pi-open-query.
  
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
    IF INPUT FRAME {&FRAME-NAME} fi-cenario-origem = "" THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Cen rio em branco!~~Informe um cen rio v lido para o campo Cen rio Cont bil de Origem.").
        APPLY "ENTRY" TO fi-cenario-origem.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST cenar_ctbl NO-LOCK 
            WHERE cenar_ctbl.cod_cenar_ctbl = INPUT FRAME {&FRAME-NAME} fi-cenario-origem NO-ERROR.
        IF NOT AVAILABLE cenar_ctbl THEN DO:
            run utp/ut-msgs.p (input "show":U, 
                               input 17006, 
                               input "Cen rio inv lido!~~Informe um cen rio v lido para o campo Cen rio Cont bil de Origem.").
            APPLY "ENTRY" TO fi-cenario-origem.
            RETURN NO-APPLY.
        END.
    END.

    IF INPUT FRAME {&FRAME-NAME} fi-cenario-destino <> "" THEN DO:
        FIND FIRST cenar_ctbl NO-LOCK
            WHERE cenar_ctbl.cod_cenar_ctbl = INPUT FRAME {&FRAME-NAME} fi-cenario-destino NO-ERROR.
        IF NOT AVAILABLE cenar_ctbl THEN DO:
            run utp/ut-msgs.p (input "show":U, 
                               input 17006, 
                               input "Cen rio inv lido!~~Informe um cen rio v lido para o campo Cen rio Cont bil de Destino.").
            APPLY "ENTRY" TO fi-cenario-destino.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            run utp/ut-msgs.p (input "show":U,
                               input 19085,
                               input "ATEN€ÇO! Cen rio diferente de branco! Caso o Cen rio Destino for diferente de branco a contabiliza‡Æo nÆo ser  equalizada! Ser  apenas para o Cen rio informado!").
        END.
    END.

    FIND FIRST es-param-cenario EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE es-param-cenario THEN DO:
        ASSIGN es-param-cenario.modulos = "".
        FOR EACH tt-modulo-para:
            IF es-param-cenario.modulos = "" THEN
                ASSIGN es-param-cenario.modulos = tt-modulo-para.cod_modul_dts.
            ELSE 
                ASSIGN es-param-cenario.modulos = es-param-cenario.modulos 
                                                + "," + tt-modulo-para.cod_modul_dts.
        END.

        ASSIGN es-param-cenario.empresas = "".
        FOR EACH tt-empresa-para:
            IF es-param-cenario.empresas = "" THEN
                ASSIGN es-param-cenario.empresas = tt-empresa-para.cod_empresa.
            ELSE
                ASSIGN es-param-cenario.empresas = es-param-cenario.empresas 
                                                 + "," + tt-empresa-para.cod_empresa.
        END.

        ASSIGN es-param-cenario.grupos = "".
        FOR EACH tt-grupo-para:
            IF es-param-cenario.grupos = "" THEN
                ASSIGN es-param-cenario.grupos = tt-grupo-para.cod_grp_usuar.
            ELSE
                ASSIGN es-param-cenario.grupos = es-param-cenario.grupos
                                                 + "," + tt-grupo-para.cod_grp_usuar.
        END.

        ASSIGN es-param-cenario.cod_cenar_ctbl_origem  = INPUT FRAME {&FRAME-NAME} fi-cenario-origem
               es-param-cenario.cod_cenar_ctbl_destino = INPUT FRAME {&FRAME-NAME} fi-cenario-destino.
    END.

    apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-empresa-de
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
  DISPLAY fi-cenario-origem fi-cenario-destino 
      WITH FRAME f_main IN WINDOW C-Win.
  ENABLE rt_rgf rt_cxcf RECT-1 RECT-2 RECT-3 RECT-4 br-modulo-de br-modulo-para 
         bt-busca-cenario-origem fi-cenario-origem bt-busca-cenario-destino 
         fi-cenario-destino bt-add-mod bt-del-mod br-empresa-de br-empresa-para 
         bt-add-emp bt-del-emp br-grupo-de br-grupo-para bt-add-grp bt-del-grp 
         bt_ok bt_cancelar Btn_Help 
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
    
    CLOSE QUERY br-modulo-de.
    CLOSE QUERY br-modulo-para.
    CLOSE QUERY br-empresa-de.
    CLOSE QUERY br-empresa-para.
    CLOSE QUERY br-grupo-de.
    CLOSE QUERY br-grupo-para.

    OPEN QUERY br-modulo-de    FOR EACH tt-modulo-de        INDEXED-REPOSITION.
    OPEN QUERY br-empresa-de   FOR EACH tt-empresa-de       INDEXED-REPOSITION.
    OPEN QUERY br-grupo-de     FOR EACH tt-grupo-de         INDEXED-REPOSITION.
    OPEN QUERY br-modulo-para  FOR EACH tt-modulo-para      INDEXED-REPOSITION.
    OPEN QUERY br-empresa-para FOR EACH tt-empresa-para     INDEXED-REPOSITION.
    OPEN QUERY br-grupo-para   FOR EACH tt-grupo-para       INDEXED-REPOSITION.

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
    DEFINE VARIABLE iCont       AS INTEGER     NO-UNDO.

    EMPTY TEMP-TABLE tt-empresa-de.
    EMPTY TEMP-TABLE tt-empresa-para.
    EMPTY TEMP-TABLE tt-modulo-de.
    EMPTY TEMP-TABLE tt-modulo-para.
    EMPTY TEMP-TABLE tt-grupo-de.
    EMPTY TEMP-TABLE tt-grupo-para.

    FOR EACH empresa NO-LOCK:
        CREATE tt-empresa-de.
        BUFFER-COPY empresa TO tt-empresa-de.
    END.

    FOR EACH modul_dtsul NO-LOCK:
        CREATE tt-modulo-de.
        BUFFER-COPY modul_dtsul TO tt-modulo-de.
    END.

    FOR EACH grp_usuar NO-LOCK:
        IF grp_usuar.cod_grp_usuar = "*" THEN NEXT.
        CREATE tt-grupo-de.
        BUFFER-COPY grp_usuar TO tt-grupo-de.
    END.

    FIND FIRST es-param-cenario NO-LOCK NO-ERROR.
    IF NOT AVAILABLE es-param-cenario THEN
        CREATE es-param-cenario.

    DO iCont = 1 TO NUM-ENTRIES(es-param-cenario.modulos,","):
        FIND FIRST modul_dtsul NO-LOCK
            WHERE modul_dtsul.cod_modul_dts = ENTRY(iCont,es-param-cenario.modulos,",") NO-ERROR.
        IF AVAILABLE modul_dtsul THEN DO:
            CREATE tt-modulo-para.
            BUFFER-COPY modul_dtsul TO tt-modulo-para.
        END.
    END.

    DO iCont = 1 TO NUM-ENTRIES(es-param-cenario.empresas,","):
        FIND FIRST empresa NO-LOCK
            WHERE empresa.cod_empresa = ENTRY(iCont,es-param-cenario.empresas,",") NO-ERROR.
        IF AVAILABLE empresa THEN DO:
            CREATE tt-empresa-para.
            BUFFER-COPY empresa TO tt-empresa-para.
        END.
    END.

    DO iCont = 1 TO NUM-ENTRIES(es-param-cenario.grupos,","):
        FIND FIRST grp_usuar NO-LOCK
            WHERE grp_usuar.cod_grp_usuar = ENTRY(iCont,es-param-cenario.grupos,",") NO-ERROR.
        IF AVAILABLE grp_usuar THEN DO:
            CREATE tt-grupo-para.
            BUFFER-COPY grp_usuar TO tt-grupo-para.
        END.
    END.

    FOR EACH tt-empresa-de:
        FIND FIRST tt-empresa-para OF tt-empresa-de NO-LOCK NO-ERROR.
        IF AVAILABLE tt-empresa-para THEN
            DELETE tt-empresa-de.
    END.

    FOR EACH tt-modulo-de:
        FIND FIRST tt-modulo-para NO-LOCK
             where tt-modulo-para.cod_modul_dtsul = tt-modulo-de.cod_modul_dtsul NO-ERROR.
        IF AVAILABLE tt-modulo-para THEN
            DELETE tt-modulo-de.
    END.

    FOR EACH tt-grupo-de:
        FIND FIRST tt-grupo-para NO-LOCK
             where tt-grupo-para.cod_grp_usuar = tt-grupo-de.cod_grp_usuar NO-ERROR.
        IF AVAILABLE tt-grupo-para THEN
            DELETE tt-grupo-de.
    END.

    RUN pi-open-query.

    ASSIGN fi-cenario-origem :SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-param-cenario.cod_cenar_ctbl_origem 
           fi-cenario-destino:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-param-cenario.cod_cenar_ctbl_destino
           .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

