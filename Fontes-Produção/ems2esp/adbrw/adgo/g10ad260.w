&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emsfnd           PROGRESS
          ems2movme        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME g04ad260
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS g04ad260 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i G10AD260 2.00.00.007}  /*** 010007 ***/
 
 

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i g10ad260 MUT}
&ENDIF

/*------------------------------------------------------------------------
 
  File: 
 
  Description: 
 
  Input Parameters:
      <none>
 
  Output Parameters:
      <none>
 
  Author: Vanei
 
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
&glob version 1.00.000
 
/* Parameters Definitions ---                                           */
define output parameter     p-row-tabela    as rowid    no-undo.
 
/* Local Variable Definitions ---                                       */
 
&IF "{&mguni_version}" >= "2.07A" &THEN
def new global shared var i-emp-selecao LIKE empresa.ep-codigo no-undo.
&ELSE
def new global shared var i-emp-selecao as integer no-undo.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME g04ad260

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tit-ap

/* Definitions for DIALOG-BOX g04ad260                                  */
&Scoped-define QUERY-STRING-g04ad260 FOR EACH tit-ap SHARE-LOCK
&Scoped-define OPEN-QUERY-g04ad260 OPEN QUERY g04ad260 FOR EACH tit-ap SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-g04ad260 tit-ap
&Scoped-define FIRST-TABLE-IN-QUERY-g04ad260 tit-ap


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tit-ap.cod-estabel tit-ap.cod-fornec ~
tit-ap.cod-esp tit-ap.serie tit-ap.nr-docto tit-ap.parcela 
&Scoped-define ENABLED-TABLES tit-ap
&Scoped-define FIRST-ENABLED-TABLE tit-ap
&Scoped-Define ENABLED-OBJECTS bt-ok bt-cancela bt-ajuda RECT-1 rt-button 
&Scoped-Define DISPLAYED-FIELDS tit-ap.cod-estabel tit-ap.cod-fornec ~
tit-ap.cod-esp tit-ap.serie tit-ap.nr-docto tit-ap.parcela 
&Scoped-define DISPLAYED-TABLES tit-ap
&Scoped-define FIRST-DISPLAYED-TABLE tit-ap


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-bt-ajuda 
       MENU-ITEM mi-sobre       LABEL "Sobre..."      .


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

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 5.33.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 42.14 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY g04ad260 FOR 
      tit-ap SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME g04ad260
     tit-ap.cod-estabel AT ROW 1.21 COL 16.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.43 BY .88
     tit-ap.cod-fornec AT ROW 2.21 COL 16.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     tit-ap.cod-esp AT ROW 3.21 COL 16.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .88
     tit-ap.serie AT ROW 4.21 COL 16.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .88
     tit-ap.nr-docto AT ROW 5.21 COL 16.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.43 BY .88
     tit-ap.parcela AT ROW 5.21 COL 33.86 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .88
     bt-ok AT ROW 6.71 COL 1.72
     bt-cancela AT ROW 6.71 COL 12.14
     bt-ajuda AT ROW 6.71 COL 32.57
     RECT-1 AT ROW 1 COL 1
     rt-button AT ROW 6.46 COL 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "V  Para T¡tulo"
         DEFAULT-BUTTON bt-ok CANCEL-BUTTON bt-cancela.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB g04ad260 
/* ************************* Included-Libraries *********************** */
 
{src/adm/method/containr.i}
{include/d-vapara.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX g04ad260
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME g04ad260:SCROLLABLE       = FALSE
       FRAME g04ad260:HIDDEN           = TRUE.

ASSIGN 
       bt-ajuda:POPUP-MENU IN FRAME g04ad260       = MENU POPUP-MENU-bt-ajuda:HANDLE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX g04ad260
/* Query rebuild information for DIALOG-BOX g04ad260
     _TblList          = "emsfnd.tit-ap"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX g04ad260 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME g04ad260
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL g04ad260 g04ad260
ON GO OF FRAME g04ad260 /* V  Para T¡tulo */
DO:
   find tit-ap no-lock 
      where tit-ap.ep-codigo   = STRING (i-emp-selecao)
        and tit-ap.cod-estabel = input frame {&frame-name} tit-ap.cod-estabel
        and tit-ap.cod-fornec  = input frame {&frame-name} tit-ap.cod-fornec
        and tit-ap.cod-esp     = input frame {&frame-name} tit-ap.cod-esp 
        and tit-ap.serie       = input frame {&frame-name} tit-ap.serie
        and tit-ap.nr-docto    = input frame {&frame-name} tit-ap.nr-docto
        and tit-ap.parcela     = input frame {&frame-name} tit-ap.parcela
        no-error.
  if  not avail tit-ap then do:
      {utp/ut-table.i mgadm tit-ap 1}
      run utp/ut-msgs.p (input "show",
                         input 2,
                         input return-value).
      return no-apply.
  end.
  assign p-row-tabela = rowid(tit-ap).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL g04ad260 g04ad260
ON WINDOW-CLOSE OF FRAME g04ad260 /* V  Para T¡tulo */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda g04ad260
ON CHOOSE OF bt-ajuda IN FRAME g04ad260 /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre g04ad260
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK g04ad260 


/* ***************************  Main Block  *************************** */
 
assign p-row-tabela = ?.
 
{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects g04ad260  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available g04ad260  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI g04ad260  _DEFAULT-DISABLE
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
  HIDE FRAME g04ad260.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI g04ad260  _DEFAULT-ENABLE
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
  IF AVAILABLE tit-ap THEN 
    DISPLAY tit-ap.cod-estabel tit-ap.cod-fornec tit-ap.cod-esp tit-ap.serie 
          tit-ap.nr-docto tit-ap.parcela 
      WITH FRAME g04ad260.
  ENABLE tit-ap.cod-estabel tit-ap.cod-fornec tit-ap.cod-esp tit-ap.serie 
         tit-ap.nr-docto tit-ap.parcela bt-ok bt-cancela bt-ajuda RECT-1 
         rt-button 
      WITH FRAME g04ad260.
  VIEW FRAME g04ad260.
  {&OPEN-BROWSERS-IN-QUERY-g04ad260}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy g04ad260 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize g04ad260 
PROCEDURE local-initialize :
{utp/ut9000.i "G10AD260" "2.00.00.007"}
 
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 
  /* Code placed here will execute PRIOR to standard behavior. */
 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
 
  /* Code placed here will execute AFTER standard behavior.    */
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records g04ad260  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tit-ap"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed g04ad260 
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

