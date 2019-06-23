&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YMCD0206A 2.06.00.001}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
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

DEFINE TEMP-TABLE tt-empresas
    FIELD ep-codigo LIKE es-emp-desativada.ep-codigo.

DEFINE TEMP-TABLE tt-empresas-ativa
    FIELD ep-codigo LIKE es-emp-desativada.ep-codigo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME b-empresas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-empresas tt-empresas-ativa

/* Definitions for BROWSE b-empresas                                    */
&Scoped-define FIELDS-IN-QUERY-b-empresas tt-empresas.ep-codigo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-b-empresas   
&Scoped-define SELF-NAME b-empresas
&Scoped-define QUERY-STRING-b-empresas FOR EACH tt-empresas BY tt-empresas.ep-codigo
&Scoped-define OPEN-QUERY-b-empresas OPEN QUERY b-empresas FOR EACH tt-empresas BY tt-empresas.ep-codigo.
&Scoped-define TABLES-IN-QUERY-b-empresas tt-empresas
&Scoped-define FIRST-TABLE-IN-QUERY-b-empresas tt-empresas


/* Definitions for BROWSE b-empresas-ativa                              */
&Scoped-define FIELDS-IN-QUERY-b-empresas-ativa tt-empresas-ativa.ep-codigo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-b-empresas-ativa   
&Scoped-define SELF-NAME b-empresas-ativa
&Scoped-define QUERY-STRING-b-empresas-ativa FOR EACH tt-empresas-ativa
&Scoped-define OPEN-QUERY-b-empresas-ativa OPEN QUERY b-empresas-ativa FOR EACH tt-empresas-ativa.
&Scoped-define TABLES-IN-QUERY-b-empresas-ativa tt-empresas-ativa
&Scoped-define FIRST-TABLE-IN-QUERY-b-empresas-ativa tt-empresas-ativa


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-b-empresas}~
    ~{&OPEN-QUERY-b-empresas-ativa}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 b-empresas b-empresas-ativa ~
bt-incluir bt-deletar bt-ok bt-cancelar bt-ajuda 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-deletar 
     IMAGE-UP FILE "adeicon/prev-au.bmp":U
     LABEL "Button 2" 
     SIZE 5 BY 1.13.

DEFINE BUTTON bt-incluir 
     IMAGE-UP FILE "adeicon/next-au.bmp":U
     LABEL "Button 1" 
     SIZE 5 BY 1.13.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 69 BY 1.38
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY b-empresas FOR 
      tt-empresas SCROLLING.

DEFINE QUERY b-empresas-ativa FOR 
      tt-empresas-ativa SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE b-empresas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS b-empresas w-window _FREEFORM
  QUERY b-empresas DISPLAY
      tt-empresas.ep-codigo COLUMN-LABEL "Empresa"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 30 BY 8.75
         TITLE "Empresas Gerais" FIT-LAST-COLUMN.

DEFINE BROWSE b-empresas-ativa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS b-empresas-ativa w-window _FREEFORM
  QUERY b-empresas-ativa DISPLAY
      tt-empresas-ativa.ep-codigo COLUMN-LABEL "Empresa"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 30 BY 8.75
         TITLE "Empresas Inativas" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     b-empresas AT ROW 1.75 COL 3 WIDGET-ID 200
     b-empresas-ativa AT ROW 1.75 COL 41.14 WIDGET-ID 300
     bt-incluir AT ROW 3.88 COL 34.57 WIDGET-ID 4
     bt-deletar AT ROW 7.71 COL 34.57 WIDGET-ID 6
     bt-ok AT ROW 12.21 COL 3
     bt-cancelar AT ROW 12.21 COL 14
     bt-ajuda AT ROW 12.21 COL 60.14
     RECT-1 AT ROW 12 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 12.58 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "Empresas Ativas"
         HEIGHT             = 12.58
         WIDTH              = 70.72
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.13
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB b-empresas RECT-1 F-Main */
/* BROWSE-TAB b-empresas-ativa b-empresas F-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE b-empresas
/* Query rebuild information for BROWSE b-empresas
     _START_FREEFORM
OPEN QUERY b-empresas FOR EACH tt-empresas BY tt-empresas.ep-codigo.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE b-empresas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE b-empresas-ativa
/* Query rebuild information for BROWSE b-empresas-ativa
     _START_FREEFORM
OPEN QUERY b-empresas-ativa FOR EACH tt-empresas-ativa.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE b-empresas-ativa */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Empresas Ativas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Empresas Ativas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-deletar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-deletar w-window
ON CHOOSE OF bt-deletar IN FRAME F-Main /* Button 2 */
DO:
  
    DEFINE BUFFER b-tt-empresas       FOR tt-empresas.
    DEFINE BUFFER b-tt-empresas-ativa FOR tt-empresas-ativa.
  
    FIND FIRST es-emp-desativada 
         WHERE es-emp-desativada.ep-codigo = tt-empresas-ativa.ep-codigo NO-ERROR.
    IF AVAIL(es-emp-desativada) THEN DO:
        DELETE es-emp-desativada.

        CREATE tt-empresas.
        ASSIGN tt-empresas.ep-codigo = tt-empresas-ativa.ep-codigo.

        FIND b-tt-empresas-ativa WHERE b-tt-empresas-ativa.ep-codigo = tt-empresas-ativa.ep-codigo NO-ERROR.
        IF AVAIL(b-tt-empresas-ativa) THEN
            DELETE b-tt-empresas-ativa.

    END.

    {&OPEN-QUERY-b-empresas}
    {&OPEN-QUERY-b-empresas-ativa}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-incluir w-window
ON CHOOSE OF bt-incluir IN FRAME F-Main /* Button 1 */
DO:

    DEFINE BUFFER b-tt-empresas FOR tt-empresas.
  
    FIND FIRST es-emp-desativada 
         WHERE es-emp-desativada.ep-codigo = tt-empresas.ep-codigo NO-ERROR.
    IF NOT AVAIL(es-emp-desativada) THEN DO:
        CREATE es-emp-desativada.
        ASSIGN es-emp-desativada.ep-codigo = tt-empresas.ep-codigo.

        CREATE tt-empresas-ativa.
        ASSIGN tt-empresas-ativa.ep-codigo = tt-empresas.ep-codigo.

        FIND b-tt-empresas WHERE b-tt-empresas.ep-codigo = tt-empresas.ep-codigo NO-ERROR.
        IF AVAIL(b-tt-empresas) THEN
            DELETE b-tt-empresas.

    END.

    {&OPEN-QUERY-b-empresas}
    {&OPEN-QUERY-b-empresas-ativa}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME b-empresas
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  ENABLE RECT-1 b-empresas b-empresas-ativa bt-incluir bt-deletar bt-ok 
         bt-cancelar bt-ajuda 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
  {utp/ut9000.i "YMCD0206A" "2.06.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN pi-busca-empresas.


    {&OPEN-QUERY-b-empresas}
    {&OPEN-QUERY-b-empresas-ativa}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-empresas w-window 
PROCEDURE pi-busca-empresas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH ems2cadme.empresa FIELDS(ep-codigo data-inicio data-fim) NO-LOCK
   /*WHERE empresa.data-inicio <= TODAY
     AND empresa.data-fim    >= TODAY*/ ,
    EACH bco_empres
    WHERE bco_empres.cod_bco_logic = "ems2cademp"
      AND bco_empres.cod_empresa   = empresa.ep-codigo NO-LOCK:

    FIND FIRST es-emp-desativada 
         WHERE es-emp-desativada.ep-codigo = bco_empres.cod_empresa NO-ERROR.
    IF NOT AVAIL(es-emp-desativada) THEN DO:
        CREATE tt-empresas.
        ASSIGN tt-empresas.ep-codigo = bco_empres.cod_empresa.
    END.
    ELSE DO:
        CREATE tt-empresas-ativa.
        ASSIGN tt-empresas-ativa.ep-codigo = bco_empres.cod_empresa.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-empresas-ativa"}
  {src/adm/template/snd-list.i "tt-empresas"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

