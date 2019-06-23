&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgmov            PROGRESS
*/
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
{include/i-prgvrs.i UPC-CD0405AA 12.01.19.000}

/* Chamada a include do gerenciador de licen�as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m�dulo>:  Informar qual o m�dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m�dulo dever� ser MUT                    */

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
DEFINE INPUT-OUTPUT PARAMETER pSerDoctoIni AS CHARACTER NO-UNDO.  
DEFINE INPUT-OUTPUT PARAMETER pSerDoctoFin AS CHARACTER NO-UNDO.  
DEFINE INPUT-OUTPUT PARAMETER pNroDoctoIni AS CHARACTER NO-UNDO.  
DEFINE INPUT-OUTPUT PARAMETER pNroDoctoFin AS CHARACTER NO-UNDO.  
DEFINE INPUT-OUTPUT PARAMETER pNatOperIni  AS CHARACTER NO-UNDO.  
DEFINE INPUT-OUTPUT PARAMETER pNatOperFin  AS CHARACTER NO-UNDO.  
DEFINE INPUT-OUTPUT PARAMETER pItCodigoIni AS CHARACTER NO-UNDO.  
DEFINE INPUT-OUTPUT PARAMETER pItCodigoFin AS CHARACTER NO-UNDO.  
DEFINE INPUT-OUTPUT PARAMETER pProporcao   AS DECIMAL   NO-UNDO.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES docum-est item-doc-est

/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define QUERY-STRING-D-Dialog FOR EACH docum-est SHARE-LOCK, ~
      EACH item-doc-est OF docum-est SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH docum-est SHARE-LOCK, ~
      EACH item-doc-est OF docum-est SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog docum-est item-doc-est
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog docum-est
&Scoped-define SECOND-TABLE-IN-QUERY-D-Dialog item-doc-est


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 ~
IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 RECT-1 RECT-2 fi-serie-docto-ini ~
fi-serie-docto-fin fi-nr-docto-ini fi-nr-docto-fin fi-nat-operacao-ini ~
fi-nat-operacao-fin fi-it-codigo-ini fi-it-codigo-fin fi-porcao bt-ok ~
bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-serie-docto-ini fi-serie-docto-fin ~
fi-nr-docto-ini fi-nr-docto-fin fi-nat-operacao-ini fi-nat-operacao-fin ~
fi-it-codigo-ini fi-it-codigo-fin fi-porcao 

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

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nat-operacao-fin AS CHARACTER FORMAT "x(06)" INITIAL "ZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nat-operacao-ini AS CHARACTER FORMAT "x(06)" 
     LABEL "Nat Opera��o":R15 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-docto-fin AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-docto-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Documento":R11 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-porcao AS DECIMAL FORMAT ">>9.99":U INITIAL 100 
     LABEL "Propor��o %" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie-docto-fin AS CHARACTER FORMAT "x(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie-docto-ini AS CHARACTER FORMAT "x(5)" 
     LABEL "S�rie":R7 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-2
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-3
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-4
     FILENAME "image\ii-las":U
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

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 4.38.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 1.96.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 70.57 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY D-Dialog FOR 
      docum-est, 
      item-doc-est SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-serie-docto-ini AT ROW 1.46 COL 19.86 COLON-ALIGNED WIDGET-ID 2
     fi-serie-docto-fin AT ROW 1.46 COL 44.57 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-nr-docto-ini AT ROW 2.38 COL 11.86 COLON-ALIGNED WIDGET-ID 6
     fi-nr-docto-fin AT ROW 2.38 COL 44.57 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi-nat-operacao-ini AT ROW 3.29 COL 18.86 COLON-ALIGNED WIDGET-ID 10
     fi-nat-operacao-fin AT ROW 3.29 COL 44.57 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     fi-it-codigo-ini AT ROW 4.21 COL 15 COLON-ALIGNED WIDGET-ID 14
     fi-it-codigo-fin AT ROW 4.21 COL 44.57 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     fi-porcao AT ROW 6 COL 30 COLON-ALIGNED WIDGET-ID 18
     bt-ok AT ROW 7.83 COL 2
     bt-cancela AT ROW 7.83 COL 13
     bt-ajuda AT ROW 7.83 COL 60.72
     rt-buttom AT ROW 7.58 COL 1
     IMAGE-1 AT ROW 1.46 COL 31 WIDGET-ID 20
     IMAGE-2 AT ROW 1.46 COL 43.72 WIDGET-ID 22
     IMAGE-3 AT ROW 2.38 COL 31 WIDGET-ID 24
     IMAGE-4 AT ROW 2.38 COL 43.72 WIDGET-ID 26
     IMAGE-5 AT ROW 3.29 COL 31 WIDGET-ID 28
     IMAGE-6 AT ROW 3.29 COL 43.72 WIDGET-ID 30
     IMAGE-7 AT ROW 4.21 COL 31 WIDGET-ID 32
     IMAGE-8 AT ROW 4.21 COL 43.72 WIDGET-ID 34
     RECT-1 AT ROW 1.13 COL 1.29 WIDGET-ID 36
     RECT-2 AT ROW 5.54 COL 1.43 WIDGET-ID 38
     SPACE(0.14) SKIP(1.78)
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
   NOT-VISIBLE FRAME-NAME L-To-R                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "mgmov.docum-est,mgmov.item-doc-est OF mgmov.docum-est"
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
  ASSIGN pSerDoctoIni = fi-serie-docto-ini :SCREEN-VALUE IN FRAME {&FRAME-NAME} 
         pSerDoctoFin = fi-serie-docto-fin :SCREEN-VALUE IN FRAME {&FRAME-NAME} 
         pNroDoctoIni = fi-nr-docto-ini    :SCREEN-VALUE IN FRAME {&FRAME-NAME} 
         pNroDoctoFin = fi-nr-docto-fin    :SCREEN-VALUE IN FRAME {&FRAME-NAME} 
         pNatOperIni  = fi-nat-operacao-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
         pNatOperFin  = fi-nat-operacao-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
         pItCodigoIni = fi-it-codigo-ini   :SCREEN-VALUE IN FRAME {&FRAME-NAME} 
         pItCodigoFin = fi-it-codigo-fin   :SCREEN-VALUE IN FRAME {&FRAME-NAME} 
         pProporcao   = DEC(fi-porcao      :SCREEN-VALUE IN FRAME {&FRAME-NAME}).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DISPLAY fi-serie-docto-ini fi-serie-docto-fin fi-nr-docto-ini fi-nr-docto-fin 
          fi-nat-operacao-ini fi-nat-operacao-fin fi-it-codigo-ini 
          fi-it-codigo-fin fi-porcao 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 
         IMAGE-8 RECT-1 RECT-2 fi-serie-docto-ini fi-serie-docto-fin 
         fi-nr-docto-ini fi-nr-docto-fin fi-nat-operacao-ini 
         fi-nat-operacao-fin fi-it-codigo-ini fi-it-codigo-fin fi-porcao bt-ok 
         bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
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

  {utp/ut9000.i "UPC-CD0405AA" "12.01.19.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN fi-serie-docto-ini :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(pSerDoctoIni )
         fi-serie-docto-fin :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(pSerDoctoFin )
         fi-nr-docto-ini    :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(pNroDoctoIni )
         fi-nr-docto-fin    :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(pNroDoctoFin )
         fi-nat-operacao-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(pNatOperIni  )
         fi-nat-operacao-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(pNatOperFin  )
         fi-it-codigo-ini   :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(pItCodigoIni )
         fi-it-codigo-fin   :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(pItCodigoFin )
         fi-porcao          :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(pProporcao   ).

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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "docum-est"}
  {src/adm/template/snd-list.i "item-doc-est"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

