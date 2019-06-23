&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5esp          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

/*****************************************************************************
** Programa..............: esjd007
** Descricao.............: Consulta JDE
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 06/08/2014
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

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

def var v_cod_dwb_file
    as character
    format "x(40)":U
    label "Arquivo"
    column-label "Arquivo"
    no-undo.

DEF temp-table tt-erro no-undo
    field i-sequen      as int             
    field cd-erro       as int
    field mensagem      as char format "x(155)"
    FIELD cod_cta_ctbl  LIKE es-cross-reference-jde.cod_cta_ctbl
    FIELD cod_ccusto    LIKE es-cross-reference-jde.cod_ccusto
    FIELD cod_estab     LIKE estabelecimento.cod_estab
    FIELD tipo          AS   CHARACTER format "x(20)"
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f_main
&Scoped-define BROWSE-NAME br-jde

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-cross-reference-jde

/* Definitions for BROWSE br-jde                                        */
&Scoped-define FIELDS-IN-QUERY-br-jde ~
es-cross-reference-jde.Legacy-Company-Number ~
es-cross-reference-jde.cod_cta_ctbl es-cross-reference-jde.cod_ccusto ~
es-cross-reference-jde.Source-Account-Number ~
es-cross-reference-jde.Date-Last-MODIFIED 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-jde 
&Scoped-define QUERY-STRING-br-jde FOR EACH es-cross-reference-jde NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-jde OPEN QUERY br-jde FOR EACH es-cross-reference-jde NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-jde es-cross-reference-jde
&Scoped-define FIRST-TABLE-IN-QUERY-br-jde es-cross-reference-jde


/* Definitions for FRAME f_main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f_main ~
    ~{&OPEN-QUERY-br-jde}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt_rgf rt_cxcf RECT-14 IMAGE-15 IMAGE-16 ~
IMAGE-17 IMAGE-18 IMAGE-19 IMAGE-20 IMAGE-21 IMAGE-22 fi-legacy-ini ~
fi-legacy-fin fi-cod_cta_ctbl-ini fi-cod_cta_ctbl-fim fi-cod_ccusto-ini ~
fi-cod_ccusto-fin br-filtro fi-Date-ini fi-Date-fin br-jde bt_fechar ~
bt_cancelar Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS fi-legacy-ini fi-legacy-fin ~
fi-cod_cta_ctbl-ini fi-cod_cta_ctbl-fim fi-cod_ccusto-ini fi-cod_ccusto-fin ~
fi-Date-ini fi-Date-fin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON br-filtro 
     IMAGE-UP FILE "adeicon/check.bmp":U
     LABEL "Filtro" 
     SIZE 5 BY 1 TOOLTIP "Filtro".

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_fechar AUTO-GO 
     LABEL "Fechar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-cod_ccusto-fin AS CHARACTER FORMAT "x(11)" INITIAL "999999" 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .88.

DEFINE VARIABLE fi-cod_ccusto-ini AS CHARACTER FORMAT "x(11)" INITIAL "" 
     LABEL "Centro Custo" 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .88.

DEFINE VARIABLE fi-cod_cta_ctbl-fim AS CHARACTER FORMAT "x(20)" INITIAL "99999999" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .88.

DEFINE VARIABLE fi-cod_cta_ctbl-ini AS CHARACTER FORMAT "x(20)" INITIAL "10000000" 
     LABEL "Conta Cont bil" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .88.

DEFINE VARIABLE fi-Date-fin AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .88.

DEFINE VARIABLE fi-Date-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/1800 
     LABEL "Data Modifica‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .88.

DEFINE VARIABLE fi-legacy-fin AS CHARACTER FORMAT "x(05)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6.72 BY .88.

DEFINE VARIABLE fi-legacy-ini AS CHARACTER FORMAT "x(05)" 
     LABEL "Legacy Company Number" 
     VIEW-AS FILL-IN 
     SIZE 6.72 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

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

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 15.83.

DEFINE RECTANGLE rt_cxcf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42.

DEFINE RECTANGLE rt_rgf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.5
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-jde FOR 
      es-cross-reference-jde SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-jde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-jde C-Win _STRUCTURED
  QUERY br-jde NO-LOCK DISPLAY
      es-cross-reference-jde.Legacy-Company-Number FORMAT "x(8)":U
            WIDTH 20
      es-cross-reference-jde.cod_cta_ctbl FORMAT "x(20)":U
      es-cross-reference-jde.cod_ccusto FORMAT "x(11)":U
      es-cross-reference-jde.Source-Account-Number FORMAT "x(20)":U
            WIDTH 14.29
      es-cross-reference-jde.Date-Last-MODIFIED FORMAT "99/99/9999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 88 BY 15.25
         FONT 1
         TITLE "JDE" TOOLTIP "JDE".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f_main
     fi-legacy-ini AT ROW 2.63 COL 27 COLON-ALIGNED
     fi-legacy-fin AT ROW 2.63 COL 42.86 COLON-ALIGNED NO-LABEL
     fi-cod_cta_ctbl-ini AT ROW 3.58 COL 18.29 COLON-ALIGNED HELP
          "C¢digo Conta Cont bil"
     fi-cod_cta_ctbl-fim AT ROW 3.58 COL 42.86 COLON-ALIGNED HELP
          "C¢digo Conta Cont bil" NO-LABEL
     fi-cod_ccusto-ini AT ROW 4.54 COL 23 COLON-ALIGNED HELP
          "C¢digo Centro Custo"
     fi-cod_ccusto-fin AT ROW 4.54 COL 42.86 COLON-ALIGNED HELP
          "C¢digo Centro Custo" NO-LABEL
     br-filtro AT ROW 5.21 COL 85.29
     fi-Date-ini AT ROW 5.5 COL 23 COLON-ALIGNED
     fi-Date-fin AT ROW 5.5 COL 42.86 COLON-ALIGNED NO-LABEL
     br-jde AT ROW 6.88 COL 2
     bt_fechar AT ROW 22.79 COL 3
     bt_cancelar AT ROW 22.79 COL 13.86
     Btn_Help AT ROW 22.79 COL 78.86
     rt_rgf AT ROW 1 COL 1
     rt_cxcf AT ROW 22.58 COL 1
     RECT-14 AT ROW 6.63 COL 1
     IMAGE-15 AT ROW 3.54 COL 36
     IMAGE-16 AT ROW 3.54 COL 41.72
     IMAGE-17 AT ROW 4.5 COL 36
     IMAGE-18 AT ROW 4.5 COL 41.72
     IMAGE-19 AT ROW 5.46 COL 36
     IMAGE-20 AT ROW 5.46 COL 41.72
     IMAGE-21 AT ROW 2.58 COL 36
     IMAGE-22 AT ROW 2.58 COL 41.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 23
         FONT 1
         CANCEL-BUTTON bt_cancelar.


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
         TITLE              = ".: Consulta planilha JDE - ESJD007 :."
         HEIGHT             = 23
         WIDTH              = 90
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
/* BROWSE-TAB br-jde fi-Date-fin f_main */
ASSIGN 
       br-jde:ALLOW-COLUMN-SEARCHING IN FRAME f_main = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-jde
/* Query rebuild information for BROWSE br-jde
     _TblList          = "ems5esp.es-cross-reference-jde"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ems5esp.es-cross-reference-jde.Legacy-Company-Number
"Legacy-Company-Number" ? ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = ems5esp.es-cross-reference-jde.cod_cta_ctbl
     _FldNameList[3]   = ems5esp.es-cross-reference-jde.cod_ccusto
     _FldNameList[4]   > ems5esp.es-cross-reference-jde.Source-Account-Number
"Source-Account-Number" ? ? "character" ? ? ? ? ? ? no ? no no "14.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = ems5esp.es-cross-reference-jde.Date-Last-MODIFIED
     _Query            is OPENED
*/  /* BROWSE br-jde */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* .: Consulta planilha JDE - ESJD007 :. */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* .: Consulta planilha JDE - ESJD007 :. */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME br-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-filtro C-Win
ON CHOOSE OF br-filtro IN FRAME f_main /* Filtro */
DO:
    CLOSE QUERY br-jde.
    OPEN QUERY br-jde FOR EACH es-cross-reference-jde NO-LOCK 
        WHERE es-cross-reference-jde.Legacy-Company-Number >= INPUT FRAME {&FRAME-NAME} fi-legacy-ini
        AND   es-cross-reference-jde.Legacy-Company-Number <= INPUT FRAME {&FRAME-NAME} fi-legacy-fin
        AND   es-cross-reference-jde.cod_cta_ctbl          >= INPUT FRAME {&FRAME-NAME} fi-cod_cta_ctbl-ini
        AND   es-cross-reference-jde.cod_cta_ctbl          <= INPUT FRAME {&FRAME-NAME} fi-cod_cta_ctbl-fim
        AND   es-cross-reference-jde.cod_ccusto            >= INPUT FRAME {&FRAME-NAME} fi-cod_ccusto-ini
        AND   es-cross-reference-jde.cod_ccusto            <= INPUT FRAME {&FRAME-NAME} fi-cod_ccusto-fin
        AND   es-cross-reference-jde.Date-Last-MODIFIED    >= INPUT FRAME {&FRAME-NAME} fi-Date-ini
        AND   es-cross-reference-jde.Date-Last-MODIFIED    <= INPUT FRAME {&FRAME-NAME} fi-Date-fin
        INDEXED-REPOSITION.
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


&Scoped-define SELF-NAME bt_fechar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_fechar C-Win
ON CHOOSE OF bt_fechar IN FRAME f_main /* Fechar */
DO:
    apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-jde
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
  DISPLAY fi-legacy-ini fi-legacy-fin fi-cod_cta_ctbl-ini fi-cod_cta_ctbl-fim 
          fi-cod_ccusto-ini fi-cod_ccusto-fin fi-Date-ini fi-Date-fin 
      WITH FRAME f_main IN WINDOW C-Win.
  ENABLE rt_rgf rt_cxcf RECT-14 IMAGE-15 IMAGE-16 IMAGE-17 IMAGE-18 IMAGE-19 
         IMAGE-20 IMAGE-21 IMAGE-22 fi-legacy-ini fi-legacy-fin 
         fi-cod_cta_ctbl-ini fi-cod_cta_ctbl-fim fi-cod_ccusto-ini 
         fi-cod_ccusto-fin br-filtro fi-Date-ini fi-Date-fin br-jde bt_fechar 
         bt_cancelar Btn_Help 
      WITH FRAME f_main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f_main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

