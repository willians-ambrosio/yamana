&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME ap0804d
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS ap0804d 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i AP0804D 2.00.00.013}  /*** 010013 ***/


/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

def input-output parameter c-aberto       as logical init no.
def input-output parameter c-matriz       as logical init no.
def input-output parameter l-saldo-aberto as logical init no.
def input-output parameter c-cod-est      like mov-ap.cod-estabel.
def input-output parameter c-ini-esp      like mov-ap.cod-esp.
def input-output parameter c-fim-esp      like mov-ap.cod-esp.
def input-output parameter dt-ini         like mov-ap.dt-transacao.
def input-output parameter dt-fim         like mov-ap.dt-transacao.
def input-output parameter i-cod-moeda    as integer init 0.
def input-output parameter i-portador-ini as integer.
def input-output parameter i-portador-fim as integer.

def var c-data as date no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME ap0804d

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-28 c-cod-estabel RECT-2 cod-esp-ini ~
IMAGE-4 IMAGE-5 cod-esp-fim cod-port-ini IMAGE-44 IMAGE-45 cod-port-fim ~
dt-emissao-ini IMAGE-6 IMAGE-7 dt-emissao-fim rt-buttom bt-ok bt-cancela ~
bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS c-cod-estabel c-desc-estabel cod-esp-ini ~
cod-esp-fim cod-port-ini cod-port-fim dt-emissao-ini dt-emissao-fim 

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

DEFINE VARIABLE c-cod-estabel AS CHARACTER FORMAT "x(3)" 
     LABEL "":R7 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-estabel AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 33.43 BY .88 NO-UNDO.

DEFINE VARIABLE cod-esp-fim AS CHARACTER FORMAT "!!" INITIAL "ZZ" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE cod-esp-ini AS CHARACTER FORMAT "!!" INITIAL "AA" 
     LABEL "":R9 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE cod-port-fim AS INTEGER FORMAT ">>>>9" INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE cod-port-ini AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "":R9 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE dt-emissao-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE dt-emissao-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "":R18 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-4
     FILENAME "image\ii-fir":U
     SIZE 3.43 BY 1.08.

DEFINE IMAGE IMAGE-44
     FILENAME "image\ii-fir":U
     SIZE 3.43 BY 1.08.

DEFINE IMAGE IMAGE-45
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.08.

DEFINE IMAGE IMAGE-5
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.08.

DEFINE IMAGE IMAGE-6
     FILENAME "image\ii-fir":U
     SIZE 3.43 BY 1.08.

DEFINE IMAGE IMAGE-7
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.08.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60.72 BY 3.46.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60.72 BY 1.46.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 60.72 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME ap0804d
     c-cod-estabel AT ROW 1.25 COL 18.29 COLON-ALIGNED HELP
          "F5 para zoom"
     c-desc-estabel AT ROW 1.25 COL 24.57 COLON-ALIGNED NO-LABEL
     cod-esp-ini AT ROW 2.92 COL 18.29 COLON-ALIGNED
     cod-esp-fim AT ROW 2.92 COL 43.43 COLON-ALIGNED NO-LABEL
     cod-port-ini AT ROW 3.88 COL 18.29 COLON-ALIGNED
     cod-port-fim AT ROW 3.88 COL 43.43 COLON-ALIGNED NO-LABEL
     dt-emissao-ini AT ROW 4.83 COL 18.29 COLON-ALIGNED
     dt-emissao-fim AT ROW 4.83 COL 43.43 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 6.46 COL 1.72
     bt-cancela AT ROW 6.46 COL 12.72
     bt-ajuda AT ROW 6.46 COL 51.14
     RECT-28 AT ROW 1 COL 1
     RECT-2 AT ROW 2.63 COL 1
     IMAGE-4 AT ROW 2.92 COL 34.57
     IMAGE-5 AT ROW 2.92 COL 41.43
     IMAGE-44 AT ROW 3.88 COL 34.57
     IMAGE-45 AT ROW 3.88 COL 41.43
     IMAGE-6 AT ROW 4.83 COL 34.57
     IMAGE-7 AT ROW 4.83 COL 41.43
     rt-buttom AT ROW 6.25 COL 1.14
     SPACE(0.00) SKIP(0.11)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Sele‡Æo"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX ap0804d
   L-To-R                                                               */
ASSIGN 
       FRAME ap0804d:SCROLLABLE       = FALSE
       FRAME ap0804d:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-desc-estabel IN FRAME ap0804d
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX ap0804d
/* Query rebuild information for DIALOG-BOX ap0804d
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX ap0804d */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB ap0804d 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME ap0804d
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap0804d ap0804d
ON WINDOW-CLOSE OF FRAME ap0804d /* Sele‡Æo */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda ap0804d
ON CHOOSE OF bt-ajuda IN FRAME ap0804d /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok ap0804d
ON CHOOSE OF bt-ok IN FRAME ap0804d /* OK */
DO:
    find first estabelec
         where estabelec.cod-estabel = input frame {&frame-name} c-cod-estabel no-lock no-error.

    if not avail estabelec then do:
       run utp/ut-msgs.p (input "show",
                          input 2061,
                          input "").
       apply 'entry' to c-cod-estabel in frame {&frame-name}.
       return no-apply.
    end.

    assign c-cod-est      = input frame {&frame-name} c-cod-estabel
           c-ini-esp      = input frame {&frame-name} cod-esp-ini
           c-fim-esp      = input frame {&frame-name} cod-esp-fim
           dt-ini         = input frame {&frame-name} dt-emissao-ini
           dt-fim         = input frame {&frame-name} dt-emissao-fim
           i-portador-ini = input frame {&frame-name} cod-port-ini
           i-portador-fim = input frame {&frame-name} cod-port-fim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estabel ap0804d
ON LEAVE OF c-cod-estabel IN FRAME ap0804d
DO:
    
    find first estabelec no-lock    
         where estabelec.cod-estabel = input frame {&FRAME-NAME} c-cod-estabel no-error.
    
    if  avail estabelec then 
        assign c-desc-estabel:screen-value in frame {&FRAME-NAME} = estabelec.nome.
    else
        assign c-desc-estabel:screen-value in frame {&FRAME-NAME} = "".
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK ap0804d 


/* ***************************  Main Block  *************************** */
    
{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects ap0804d _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available ap0804d _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI ap0804d _DEFAULT-DISABLE
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
  HIDE FRAME ap0804d.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI ap0804d _DEFAULT-ENABLE
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
  DISPLAY c-cod-estabel c-desc-estabel cod-esp-ini cod-esp-fim cod-port-ini 
          cod-port-fim dt-emissao-ini dt-emissao-fim 
      WITH FRAME ap0804d.
  ENABLE RECT-28 c-cod-estabel RECT-2 cod-esp-ini IMAGE-4 IMAGE-5 cod-esp-fim 
         cod-port-ini IMAGE-44 IMAGE-45 cod-port-fim dt-emissao-ini IMAGE-6 
         IMAGE-7 dt-emissao-fim rt-buttom bt-ok bt-cancela bt-ajuda 
      WITH FRAME ap0804d.
  VIEW FRAME ap0804d.
  {&OPEN-BROWSERS-IN-QUERY-ap0804d}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy ap0804d 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize ap0804d 
PROCEDURE local-initialize :
{utp/ut9000.i "AP0804D" "2.00.00.013"}

/*------------------------------------------------------------------------------
    Purpose:     Override standard ADM method
    Notes:       
------------------------------------------------------------------------------*/

    do  with frame {&frame-name}:
    
        {utp/ut-liter.i Data_Transacao}
        assign dt-emissao-ini:label  = trim(return-value).
        
        {utp/ut-field.i mgadm titulo cod-Port 1}
        assign cod-port-ini:label  = trim(return-value).
        
        {utp/ut-field.i mgadm titulo cod-esp 1}
        assign cod-esp-ini:label  = trim(return-value).
        
        {utp/ut-field.i mgadm titulo cod-estabel 1}
        assign c-cod-estabel:label  = trim(return-value).
    
    end.            
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view ap0804d 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
    Purpose:     Override standard ADM method
    Notes:       
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .
    
    do  with frame {&FRAME-NAME}:    
        assign c-cod-estabel:screen-value  = string(c-cod-est)
               cod-esp-ini:screen-value    = string(c-ini-esp)
               cod-esp-fim:screen-value    = string(c-fim-esp)
               dt-emissao-ini:screen-value = string(dt-ini)
               dt-emissao-fim:screen-value = string(dt-fim)
               cod-port-ini:screen-value   = string(i-portador-ini)
               cod-port-fim:screen-value   = string(i-portador-fim).
    end.
    
    apply 'leave' to c-cod-estabel in frame {&FRAME-NAME}.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validar ap0804d 
PROCEDURE pi-validar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records ap0804d _ADM-SEND-RECORDS
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed ap0804d 
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


