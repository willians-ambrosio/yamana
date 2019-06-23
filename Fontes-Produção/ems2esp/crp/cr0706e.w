&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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
{include/i-prgvrs.i CR0706E 2.00.00.003}  /*** 010003 ***/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{cdp/cdcfgfin.i}

{crp/cr0570.i}


def input-output param l-pendente         as log  no-undo.
def input-output param l-depositado       as log  no-undo.
def input-output param l-caucao           as log  no-undo.
def input-output param l-compensado       as log  no-undo.
def input-output param l-substituido      as log  no-undo.
def input-output param l-devolvido        as log  no-undo.
def input-output param l-descontado       as log  no-undo.
DEF INPUT-OUTPUT PARAM l-cancelado        AS LOG  NO-UNDO.
def input-output param dt-emissao-inicial as date no-undo.
def input-output param dt-emissao-final   as date no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb-pendente tb-depositado tb-devolvido ~
tb-caucao tb-compensado tb-substituido tb-descontado dt-emissao-ini ~
dt-emissao-fim bt-ok bt-cancela bt-ajuda IMAGE-1 IMAGE-27 RECT-14 RECT-17 ~
rt-buttom 
&Scoped-Define DISPLAYED-OBJECTS tb-pendente tb-cancelado tb-depositado ~
tb-devolvido tb-caucao tb-compensado tb-substituido tb-descontado ~
dt-emissao-ini dt-emissao-fim 

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

DEFINE VARIABLE dt-emissao-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.72 BY 1 NO-UNDO.

DEFINE VARIABLE dt-emissao-ini AS DATE FORMAT "99/99/9999":U INITIAL &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/001  &ENDIF
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 58.29 BY 7.42.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59.72 BY 9.58.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 59.72 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tb-cancelado AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.29 BY .88 NO-UNDO.

DEFINE VARIABLE tb-caucao AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.29 BY .88 NO-UNDO.

DEFINE VARIABLE tb-compensado AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.29 BY .88 NO-UNDO.

DEFINE VARIABLE tb-depositado AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.29 BY .88 NO-UNDO.

DEFINE VARIABLE tb-descontado AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.29 BY .88 NO-UNDO.

DEFINE VARIABLE tb-devolvido AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.29 BY .88 NO-UNDO.

DEFINE VARIABLE tb-pendente AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.14 BY .88 NO-UNDO.

DEFINE VARIABLE tb-substituido AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.29 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     tb-pendente AT ROW 2.04 COL 6.57
     tb-cancelado AT ROW 2.04 COL 31
     tb-depositado AT ROW 2.92 COL 6.57
     tb-devolvido AT ROW 3.79 COL 6.57
     tb-caucao AT ROW 4.71 COL 6.57
     tb-compensado AT ROW 5.67 COL 6.57
     tb-substituido AT ROW 6.58 COL 6.57
     tb-descontado AT ROW 7.58 COL 6.57
     dt-emissao-ini AT ROW 9.13 COL 16.86 COLON-ALIGNED
     dt-emissao-fim AT ROW 9.13 COL 38.86 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 10.96 COL 1.72
     bt-cancela AT ROW 10.96 COL 12.14
     bt-ajuda AT ROW 10.96 COL 49.86
     IMAGE-1 AT ROW 9.13 COL 32
     IMAGE-27 AT ROW 9.13 COL 37.86
     RECT-14 AT ROW 1.46 COL 2
     RECT-17 AT ROW 1 COL 1
     rt-buttom AT ROW 10.71 COL 1
     "Situaá∆o do Cheque:" VIEW-AS TEXT
          SIZE 14.72 BY .67 AT ROW 1.08 COL 3.57
          FONT 4
     SPACE(42.84) SKIP(10.38)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Seleá∆o de Cheques"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
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
   L-To-R                                                               */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb-cancelado IN FRAME D-Dialog
   NO-ENABLE                                                            */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Seleá∆o de Cheques */
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
  assign input frame {&frame-name} tb-pendente 
                                   tb-depositado
                                   tb-caucao
                                   tb-compensado
                                   tb-substituido 
                                   tb-devolvido 
                                   tb-descontado
                                   dt-emissao-ini
                                   dt-emissao-fim.
  IF  l-func-bx-ch-recib
  AND i-pais-impto-usuario = 1 THEN do:
      assign input frame {&frame-name} tb-cancelado
             l-cancelado = tb-cancelado.
  END.

 assign l-pendente         =  tb-pendente
        l-depositado       =  tb-depositado
        l-caucao           =  tb-caucao
        l-compensado       =  tb-compensado
        l-substituido      =  tb-substituido
        l-devolvido        =  tb-devolvido
        l-descontado       =  tb-descontado
        dt-emissao-inicial =  dt-emissao-ini
        dt-emissao-final   =  dt-emissao-fim.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

{utp/ut-liter.i Data_de_Emiss∆o MCR L}
assign dt-emissao-ini:label in frame {&FRAME-NAME} = return-value.

{utp/ut-liter.i Pendente MCR L}
assign tb-pendente:label in frame {&FRAME-NAME} = return-value.

{utp/ut-liter.i Devolvido MCR L}
assign tb-devolvido:label in frame {&FRAME-NAME} = return-value.

{utp/ut-liter.i Depositado MCR L}
assign tb-depositado:label in frame {&FRAME-NAME} = return-value.

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
  DISPLAY tb-pendente tb-cancelado tb-depositado tb-devolvido tb-caucao 
          tb-compensado tb-substituido tb-descontado dt-emissao-ini 
          dt-emissao-fim 
      WITH FRAME D-Dialog.
  ENABLE tb-pendente tb-depositado tb-devolvido tb-caucao tb-compensado 
         tb-substituido tb-descontado dt-emissao-ini dt-emissao-fim bt-ok 
         bt-cancela bt-ajuda IMAGE-1 IMAGE-27 RECT-14 RECT-17 rt-buttom 
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

{utp/ut9000.i "CR0706E" "2.00.00.003"}

{utp/ut-liter.i Data_de_Emiss∆o MCR L}
assign dt-emissao-ini:label in frame {&FRAME-NAME} = return-value.

{utp/ut-liter.i Pendente MCR L}
assign tb-pendente:label in frame {&FRAME-NAME} = return-value.

{utp/ut-liter.i Devolvido MCR L}
assign tb-devolvido:label in frame {&FRAME-NAME} = return-value.

{utp/ut-liter.i Depositado MCR L}
assign tb-depositado:label in frame {&FRAME-NAME} = return-value.

{utp/ut-liter.i Cauá∆o MCR L}
assign tb-caucao:label in frame {&FRAME-NAME} = return-value.

{utp/ut-liter.i Compensado MCR L}
assign tb-compensado:label in frame {&FRAME-NAME} = return-value.

{utp/ut-liter.i Substituido MCR L}
assign tb-substituido:label in frame {&FRAME-NAME} = return-value.

{utp/ut-liter.i Descontado MCR L}
assign tb-descontado:label in frame {&FRAME-NAME} = return-value.

{utp/ut-liter.i Cancelado MCR L}
assign tb-cancelado:label in frame {&FRAME-NAME} = return-value.

/* Dispatch standard ADM method.                             */

RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

/* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view D-Dialog 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */

 RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  assign tb-pendente:screen-value    in frame {&FRAME-NAME} = string(l-pendente)
         tb-depositado:screen-value  in frame {&FRAME-NAME} = string(l-depositado)
         tb-caucao:screen-value      in frame {&FRAME-NAME} = string(l-caucao)
         tb-compensado:screen-value  in frame {&FRAME-NAME} = string(l-compensado)
         tb-substituido:screen-value in frame {&FRAME-NAME} = string(l-substituido)
         tb-devolvido:screen-value   in frame {&FRAME-NAME} = string(l-devolvido)
         tb-descontado:screen-value  in frame {&FRAME-NAME} = string(l-descontado)
         dt-emissao-ini:screen-value in frame {&FRAME-NAME} = string(dt-emissao-inicial)
         dt-emissao-fim:screen-value in frame {&FRAME-NAME} = string(dt-emissao-final).

  IF  l-func-bx-ch-recib
  AND i-pais-impto-usuario = 1 THEN do:
      ENABLE tb-cancelado WITH FRAME {&FRAME-NAME}.
      ASSIGN tb-cancelado:screen-value  in frame {&FRAME-NAME} = string(l-cancelado).
  END.

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

