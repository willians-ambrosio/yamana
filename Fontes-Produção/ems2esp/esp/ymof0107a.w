&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i D99XX999 9.99.99.999}

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


DEFINE INPUT-OUTPUT PARAMETER c-empresa-i    AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER c-empresa-f    AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER c-estab-i      AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER c-estab-f      AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER c-cfa-i        AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER c-cfa-f        AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER c-item-i       AS CHARACTER  FORMAT "x(16)" NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER c-item-f       AS CHARACTER  FORMAT "x(16)" NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER c-descricao-i  AS CHARACTER FORMAT "x(40)"  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER c-descricao-f  AS CHARACTER FORMAT "x(40)"  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER d-implat-i     AS DATE  FORMAT "99/99/9999"      NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER d-implat-f     AS DATE  FORMAT "99/99/9999"       NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER c-requisitante-i AS CHARACTER FORMAT "x(12)"  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER c-requisitante-f AS CHARACTER FORMAT "x(12)"   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 ~
IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 ~
IMAGE-14 RECT-1 f-empresa-i f-empresa-f f-estab-i f-estab-f f-cfa-i F-CFA-F ~
f-item-i f-item-f f-desc-i f-desc-f f-implatacao-i f-implatacao-f ~
requisitante-i requisitante-f bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS f-empresa-i f-empresa-f f-estab-i ~
f-estab-f f-cfa-i F-CFA-F f-item-i f-item-f f-desc-i f-desc-f ~
f-implatacao-i f-implatacao-f requisitante-i requisitante-f 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE F-CFA-F AS CHARACTER FORMAT "X(4)":U INITIAL "ZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE f-cfa-i AS CHARACTER FORMAT "X(4)":U 
     LABEL "CFA" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE f-desc-f AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 28.29 BY .88 NO-UNDO.

DEFINE VARIABLE f-desc-i AS CHARACTER FORMAT "X(16)":U 
     LABEL "Descri‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 28.29 BY .88 NO-UNDO.

DEFINE VARIABLE f-empresa-f AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE f-empresa-i AS CHARACTER FORMAT "X(3)":U 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE f-estab-f AS CHARACTER FORMAT "X(256)":U INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE f-estab-i AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE f-implatacao-f AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .88 NO-UNDO.

DEFINE VARIABLE f-implatacao-i AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Implata‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .88 NO-UNDO.

DEFINE VARIABLE f-item-f AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE f-item-i AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE requisitante-f AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 20.43 BY .88 NO-UNDO.

DEFINE VARIABLE requisitante-i AS CHARACTER FORMAT "X(12)":U 
     LABEL "Responsavel" 
     VIEW-AS FILL-IN 
     SIZE 20.43 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 11.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     f-empresa-i AT ROW 1.67 COL 30.72 COLON-ALIGNED WIDGET-ID 2
     f-empresa-f AT ROW 1.67 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     f-estab-i AT ROW 2.75 COL 31 COLON-ALIGNED WIDGET-ID 34
     f-estab-f AT ROW 2.75 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     f-cfa-i AT ROW 3.75 COL 29.14 COLON-ALIGNED WIDGET-ID 12
     F-CFA-F AT ROW 3.75 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     f-item-i AT ROW 4.83 COL 21.43 COLON-ALIGNED WIDGET-ID 58
     f-item-f AT ROW 4.83 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     f-desc-i AT ROW 5.92 COL 11.43 COLON-ALIGNED WIDGET-ID 66
     f-desc-f AT ROW 5.92 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     f-implatacao-i AT ROW 7.04 COL 25.86 COLON-ALIGNED WIDGET-ID 74
     f-implatacao-f AT ROW 7.04 COL 44.43 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     requisitante-i AT ROW 8.21 COL 19 COLON-ALIGNED WIDGET-ID 82
     requisitante-f AT ROW 8.21 COL 44.72 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     bt-ok AT ROW 12.54 COL 3
     bt-cancela AT ROW 12.54 COL 14
     bt-ajuda AT ROW 12.54 COL 68.86
     rt-buttom AT ROW 12.29 COL 1
     IMAGE-1 AT ROW 1.75 COL 41.57 WIDGET-ID 46
     IMAGE-2 AT ROW 1.75 COL 43.57 WIDGET-ID 48
     IMAGE-3 AT ROW 2.75 COL 41.57 WIDGET-ID 50
     IMAGE-4 AT ROW 2.75 COL 43.57 WIDGET-ID 52
     IMAGE-5 AT ROW 3.75 COL 41.57 WIDGET-ID 54
     IMAGE-6 AT ROW 3.75 COL 43.57 WIDGET-ID 56
     IMAGE-7 AT ROW 4.92 COL 41.57 WIDGET-ID 62
     IMAGE-8 AT ROW 4.92 COL 43.57 WIDGET-ID 64
     IMAGE-9 AT ROW 6.04 COL 41.43 WIDGET-ID 70
     IMAGE-10 AT ROW 6.04 COL 43.43 WIDGET-ID 72
     IMAGE-11 AT ROW 7.04 COL 41.43 WIDGET-ID 78
     IMAGE-12 AT ROW 7.04 COL 43.43 WIDGET-ID 76
     IMAGE-13 AT ROW 8.29 COL 41.43 WIDGET-ID 84
     IMAGE-14 AT ROW 8.29 COL 43.43 WIDGET-ID 86
     RECT-1 AT ROW 1.25 COL 1 WIDGET-ID 90
     SPACE(0.28) SKIP(1.46)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert SmartDialog title>"
         DEFAULT-BUTTON bt-ok WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* <insert SmartDialog title> */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
  

 ASSIGN   c-empresa-i      = INPUT FRAME {&FRAME-NAME} f-empresa-i
          c-empresa-f      = INPUT FRAME {&FRAME-NAME} f-empresa-f
          c-estab-i        = INPUT FRAME {&FRAME-NAME} f-estab-i
          c-estab-f        = INPUT FRAME {&FRAME-NAME} f-estab-f
          c-cfa-i          = INPUT FRAME {&FRAME-NAME} f-cfa-i
          c-cfa-f          = INPUT FRAME {&FRAME-NAME} f-cfa-f
          c-item-i         = INPUT FRAME {&FRAME-NAME} f-item-i
          c-item-f         = INPUT FRAME {&FRAME-NAME} f-item-f 
          c-descricao-i    = INPUT FRAME {&FRAME-NAME} f-desc-i
          c-descricao-f    = INPUT FRAME {&FRAME-NAME} f-desc-f
          d-implat-i       = INPUT FRAME {&FRAME-NAME} f-implatacao-i
          d-implat-f       = INPUT FRAME {&FRAME-NAME} f-implatacao-f
          c-requisitante-i = INPUT FRAME {&FRAME-NAME} requisitante-i
          c-requisitante-f = INPUT FRAME {&FRAME-NAME} requisitante-f.










END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-empresa-i
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY f-empresa-i f-empresa-f f-estab-i f-estab-f f-cfa-i F-CFA-F f-item-i 
          f-item-f f-desc-i f-desc-f f-implatacao-i f-implatacao-f 
          requisitante-i requisitante-f 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 
         IMAGE-8 IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 RECT-1 
         f-empresa-i f-empresa-f f-estab-i f-estab-f f-cfa-i F-CFA-F f-item-i 
         f-item-f f-desc-i f-desc-f f-implatacao-i f-implatacao-f 
         requisitante-i requisitante-f bt-ok bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "ymof0107A - selecao dos itens " "1.00.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

