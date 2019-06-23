&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i AP0804FC 2.00.00.001}  /*** 010001 ***/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

def input-output param p-nome-ini      as char no-undo.
def input-output param p-nome-fim      as char no-undo.
def input-output param p-esp-ini       as char no-undo.
def input-output param p-esp-fim       as char no-undo.
def input-output param p-portador-ini  as inte no-undo.
def input-output param p-portador-fim  as inte no-undo.
def input-output param p-bordero-ini   as inte no-undo.
def input-output param p-bordero-fim   as inte no-undo.
def input-output param p-emissao-ini   as date no-undo.
def input-output param p-emissao-fim   as date no-undo.
def output       param p-cancela       as logi no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 c-nome-ini IMAGE-1 IMAGE-2 c-nome-fim ~
c-esp-ini IMAGE-21 IMAGE-22 c-esp-fim i-portador-ini IMAGE-23 IMAGE-24 ~
i-portador-fim i-bordero-ini IMAGE-25 IMAGE-26 i-bordero-fim dt-emissao-ini ~
IMAGE-27 IMAGE-28 dt-emissao-fim rt-buttom bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS c-nome-ini c-nome-fim c-esp-ini c-esp-fim ~
i-portador-ini i-portador-fim i-bordero-ini i-bordero-fim dt-emissao-ini ~
dt-emissao-fim 

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

DEFINE VARIABLE c-esp-fim AS CHARACTER FORMAT "X(22)":U INITIAL "ZZ" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-esp-ini AS CHARACTER FORMAT "X(22)":U INITIAL "AA" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-fim AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-ini AS CHARACTER FORMAT "X(12)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE dt-emissao-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE dt-emissao-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE i-bordero-fim AS INTEGER FORMAT ">>>>>>>9":U INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE i-bordero-ini AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE i-portador-fim AS INTEGER FORMAT ">>>>9":U INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE i-portador-ini AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-2
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-21
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-22
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-23
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-24
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-25
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-26
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-27
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-28
     FILENAME "image\ii-las":U
     SIZE 3.57 BY 1.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 5.29.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 59 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     c-nome-ini AT ROW 1.17 COL 18 COLON-ALIGNED
     c-nome-fim AT ROW 1.17 COL 38.86 COLON-ALIGNED NO-LABEL
     c-esp-ini AT ROW 2.17 COL 18 COLON-ALIGNED
     c-esp-fim AT ROW 2.17 COL 38.86 COLON-ALIGNED NO-LABEL
     i-portador-ini AT ROW 3.17 COL 18 COLON-ALIGNED
     i-portador-fim AT ROW 3.17 COL 38.86 COLON-ALIGNED NO-LABEL
     i-bordero-ini AT ROW 4.17 COL 18 COLON-ALIGNED
     i-bordero-fim AT ROW 4.17 COL 38.86 COLON-ALIGNED NO-LABEL
     dt-emissao-ini AT ROW 5.17 COL 18 COLON-ALIGNED
     dt-emissao-fim AT ROW 5.17 COL 38.86 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 6.75 COL 2
     bt-cancela AT ROW 6.75 COL 12.43
     bt-ajuda AT ROW 6.75 COL 49.29
     RECT-6 AT ROW 1 COL 1
     IMAGE-1 AT ROW 1.17 COL 35.57
     IMAGE-2 AT ROW 1.17 COL 38
     IMAGE-21 AT ROW 2.17 COL 35.57
     IMAGE-22 AT ROW 2.17 COL 38
     IMAGE-23 AT ROW 3.17 COL 35.57
     IMAGE-24 AT ROW 3.17 COL 38
     IMAGE-25 AT ROW 4.17 COL 35.57
     IMAGE-26 AT ROW 4.17 COL 38
     IMAGE-27 AT ROW 5.17 COL 35.57
     IMAGE-28 AT ROW 5.17 COL 38
     rt-buttom AT ROW 6.5 COL 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Filtro Documentos"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   NOT-VISIBLE                                                          */
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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Filtro Documentos */
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


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela D-Dialog
ON CHOOSE OF bt-cancela IN FRAME D-Dialog /* Cancelar */
DO:
    
     assign p-cancela = yes.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:

    if  input frame {&FRAME-NAME} c-nome-ini >
        input frame {&FRAME-NAME} c-nome-fim then do:
        run utp/ut-msgs.p (input "show",
                           input 252,
                           input " ").
        apply 'entry' to c-nome-ini in frame {&FRAME-NAME}.
        return no-apply.
    end.
    
    if  input frame {&FRAME-NAME} c-esp-ini >
        input frame {&FRAME-NAME} c-esp-fim then do:
        run utp/ut-msgs.p (input "show",
                           input 252,
                           input " ").
        apply 'entry' to c-esp-ini in frame {&FRAME-NAME}.
        return no-apply.
    end.

    if  input frame {&FRAME-NAME} i-portador-ini >
        input frame {&FRAME-NAME} i-portador-fim then do:
        run utp/ut-msgs.p (input "show",
                           input 252,
                           input " ").
        apply 'entry' to i-portador-ini in frame {&FRAME-NAME}.
        return no-apply.
    end.

    if  input frame {&FRAME-NAME} i-bordero-ini >
        input frame {&FRAME-NAME} i-bordero-fim then do:
        run utp/ut-msgs.p (input "show",
                           input 252,
                           input " ").
        apply 'entry' to i-bordero-ini in frame {&FRAME-NAME}.
        return no-apply.
    end.
    
    if  input frame {&FRAME-NAME} dt-emissao-ini >
        input frame {&FRAME-NAME} dt-emissao-fim then do:
        run utp/ut-msgs.p (input "show",
                           input 252,
                           input " ").
        apply 'entry' to dt-emissao-ini in frame {&FRAME-NAME}.
        return no-apply.
    end.

    assign p-nome-ini      = input frame {&FRAME-NAME} c-nome-ini
           p-nome-fim      = input frame {&FRAME-NAME} c-nome-fim
           p-esp-ini       = input frame {&FRAME-NAME} c-esp-ini
           p-esp-fim       = input frame {&FRAME-NAME} c-esp-fim
           p-portador-ini  = input frame {&FRAME-NAME} i-portador-ini
           p-portador-fim  = input frame {&FRAME-NAME} i-portador-fim
           p-bordero-ini   = input frame {&FRAME-NAME} i-bordero-ini
           p-bordero-fim   = input frame {&FRAME-NAME} i-bordero-fim
           p-emissao-ini   = input frame {&FRAME-NAME} dt-emissao-ini
           p-emissao-fim   = input frame {&FRAME-NAME} dt-emissao-fim
           p-cancela       = no.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{utp/ut-liter.i Nome_Abreviado}
assign c-nome-ini:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Esp‚cie}
assign c-esp-ini:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Portador}
assign i-portador-ini:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Border“}
assign i-bordero-ini:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Data_EmissÆo}
assign dt-emissao-ini:label in frame {&FRAME-NAME} = trim(return-value).

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog _DEFAULT-ENABLE
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
  DISPLAY c-nome-ini c-nome-fim c-esp-ini c-esp-fim i-portador-ini 
          i-portador-fim i-bordero-ini i-bordero-fim dt-emissao-ini 
          dt-emissao-fim 
      WITH FRAME D-Dialog.
  ENABLE RECT-6 c-nome-ini IMAGE-1 IMAGE-2 c-nome-fim c-esp-ini IMAGE-21 
         IMAGE-22 c-esp-fim i-portador-ini IMAGE-23 IMAGE-24 i-portador-fim 
         i-bordero-ini IMAGE-25 IMAGE-26 i-bordero-fim dt-emissao-ini IMAGE-27 
         IMAGE-28 dt-emissao-fim rt-buttom bt-ok bt-cancela bt-ajuda 
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

{utp/ut9000.i "AP0804FC" "2.00.00.001"}

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view D-Dialog 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
    Purpose:     Override standard ADM method
    Notes:       
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

    assign c-nome-ini:screen-value     in frame {&FRAME-NAME} = p-nome-ini
           c-nome-fim:screen-value     in frame {&FRAME-NAME} = p-nome-fim
           c-esp-ini:screen-value      in frame {&FRAME-NAME} = p-esp-ini
           c-esp-fim:screen-value      in frame {&FRAME-NAME} = p-esp-fim
           i-portador-ini:screen-value in frame {&FRAME-NAME} = string(p-portador-ini)
           i-portador-fim:screen-value in frame {&FRAME-NAME} = string(p-portador-fim)
           i-bordero-ini:screen-value  in frame {&FRAME-NAME} = string(p-bordero-ini)
           i-bordero-fim:screen-value  in frame {&FRAME-NAME} = string(p-bordero-fim)
           dt-emissao-ini:screen-value in frame {&FRAME-NAME} = string(p-emissao-ini)
           dt-emissao-fim:screen-value in frame {&FRAME-NAME} = string(p-emissao-fim).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog _ADM-SEND-RECORDS
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


