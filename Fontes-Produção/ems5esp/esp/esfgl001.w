&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5esp            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF BUFFER b-grp_usuar         FOR grp_usuar.
DEF BUFFER b-es-perm-canc-ctbl FOR es-perm-canc-ctbl.

def var c-desc-grp as char format "x(40)" no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES grp_usuar es-perm-canc-ctbl

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 grp_usuar.cod_grp_usuar ~
grp_usuar.des_grp_usuar 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH grp_usuar NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH grp_usuar NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 grp_usuar
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 grp_usuar


/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 es-perm-canc-ctbl.cod_grp_usuar ~
fn-desc-grp() @ c-desc-grp 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4 
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH es-perm-canc-ctbl NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY BROWSE-4 FOR EACH es-perm-canc-ctbl NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 es-perm-canc-ctbl
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 es-perm-canc-ctbl


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 BROWSE-2 BROWSE-4 bt-add ~
bt-del Btn_OK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-grp Dialog-Frame 
FUNCTION fn-desc-grp RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon/next-u.bmp":U
     LABEL "add" 
     SIZE 7 BY 2.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon/prev-u.bmp":U
     LABEL "Del" 
     SIZE 7 BY 2.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Fechar" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 105.57 BY 13.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 105.57 BY 2.13
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      grp_usuar SCROLLING.

DEFINE QUERY BROWSE-4 FOR 
      es-perm-canc-ctbl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      grp_usuar.cod_grp_usuar FORMAT "x(3)":U WIDTH 5.43
      grp_usuar.des_grp_usuar FORMAT "x(32)":U WIDTH 35.86
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 45 BY 10 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 Dialog-Frame _STRUCTURED
  QUERY BROWSE-4 NO-LOCK DISPLAY
      es-perm-canc-ctbl.cod_grp_usuar COLUMN-LABEL "Grp Permis" FORMAT "x(3)":U
            WIDTH 10.86
      fn-desc-grp() @ c-desc-grp COLUMN-LABEL "Descricao" FORMAT "x(40)":U
            WIDTH 29.43
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 44 BY 10 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-2 AT ROW 3 COL 4 WIDGET-ID 200
     BROWSE-4 AT ROW 3 COL 60 WIDGET-ID 300
     bt-add AT ROW 4.5 COL 51 WIDGET-ID 2
     bt-del AT ROW 8.5 COL 51 WIDGET-ID 4
     Btn_OK AT ROW 14.5 COL 90
     "Grupos do EMS" VIEW-AS TEXT
          SIZE 15 BY .67 AT ROW 1.5 COL 7 WIDGET-ID 10
     "Grupos com permissÆo para cancelamento Cont bil" VIEW-AS TEXT
          SIZE 45 BY .67 AT ROW 1.54 COL 59.72 WIDGET-ID 12
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 6
     RECT-2 AT ROW 14 COL 1 WIDGET-ID 8
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Permissao de Cancelamento Contabil - esfgl001"
         DEFAULT-BUTTON Btn_OK WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 RECT-2 Dialog-Frame */
/* BROWSE-TAB BROWSE-4 BROWSE-2 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "grp_usuar"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > grp_usuar.cod_grp_usuar
"grp_usuar.cod_grp_usuar" ? ? "character" ? ? ? ? ? ? no ? no no "5.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > grp_usuar.des_grp_usuar
"grp_usuar.des_grp_usuar" ? ? "character" ? ? ? ? ? ? no ? no no "35.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _TblList          = "ems5esp.es-perm-canc-ctbl"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ems5esp.es-perm-canc-ctbl.cod_grp_usuar
"cod_grp_usuar" "Grp Permis" ? "character" ? ? ? ? ? ? no ? no no "10.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"fn-desc-grp() @ c-desc-grp" "Descricao" "x(40)" ? ? ? ? ? ? ? no ? no no "29.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Permissao de Cancelamento Contabil - esfgl001 */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add Dialog-Frame
ON CHOOSE OF bt-add IN FRAME Dialog-Frame /* add */
DO:
   IF AVAIL grp_usuar THEN
   DO:
       IF NOT CAN-FIND(es-perm-canc-ctbl WHERE
                       es-perm-canc-ctbl.cod_grp_usuar = grp_usuar.cod_grp_usuar) THEN
       DO:
           CREATE es-perm-canc-ctbl.
           ASSIGN es-perm-canc-ctbl.cod_grp_usuar = grp_usuar.cod_grp_usuar.


            OPEN QUERY BROWSE-4 FOR EACH es-perm-canc-ctbl NO-LOCK INDEXED-REPOSITION.

       END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del Dialog-Frame
ON CHOOSE OF bt-del IN FRAME Dialog-Frame /* Del */
DO:
  
   IF AVAIL es-perm-canc-ctbl THEN
   DO:
       FIND b-es-perm-canc-ctbl OF es-perm-canc-ctbl NO-ERROR.
       IF AVAIL b-es-perm-canc-ctbl THEN 
       DO:
           DELETE b-es-perm-canc-ctbl.
           OPEN QUERY BROWSE-4 FOR EACH es-perm-canc-ctbl NO-LOCK INDEXED-REPOSITION.

       END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  ENABLE RECT-1 RECT-2 BROWSE-2 BROWSE-4 bt-add bt-del Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-grp Dialog-Frame 
FUNCTION fn-desc-grp RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  IF AVAIL es-perm-canc-ctbl THEN
      FIND b-grp_usuar NO-LOCK WHERE
           b-grp_usuar.cod_grp_usuar = es-perm-canc-ctbl.cod_grp_usuar NO-ERROR.

  IF AVAIL b-grp_usuar THEN
     RETURN b-grp_usuar.des_grp_usuar.
  ELSE
     RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

