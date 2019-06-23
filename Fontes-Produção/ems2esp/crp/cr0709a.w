&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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
{include/i-prgvrs.i CR0709A 2.00.00.025}  /*** 010025 ***/

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


{crp/crapi012.i}

def new global shared var gr-mov-tit as rowid no-undo.

def buffer b-conta-contab for conta-contab.
def buffer b-portador     for ems2cadme.portador.
def buffer b-cta-corrente for cta-corrente.
def buffer b-titulo       for titulo.
def buffer b-mov-tit      for mov-tit.

def var c-formato as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-conta-contabil

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-conta-contabil

/* Definitions for BROWSE br-conta-contabil                             */
&Scoped-define FIELDS-IN-QUERY-br-conta-contabil string(tt-cod-conta-contabil, c-formato) @ tt-cod-conta-contabil tt-titulo-conta tt-natureza tt-valor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-conta-contabil   
&Scoped-define FIELD-PAIRS-IN-QUERY-br-conta-contabil
&Scoped-define SELF-NAME br-conta-contabil
&Scoped-define OPEN-QUERY-br-conta-contabil OPEN QUERY {&SELF-NAME} FOR EACH tt-conta-contabil.
&Scoped-define TABLES-IN-QUERY-br-conta-contabil tt-conta-contabil
&Scoped-define FIRST-TABLE-IN-QUERY-br-conta-contabil tt-conta-contabil


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-conta-contabil}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-conta-contabil rt-buttom bt-ok 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 12 BY 1.08
     BGCOLOR 8 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 69 BY 1.67
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-conta-contabil FOR 
      tt-conta-contabil SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-conta-contabil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-conta-contabil D-Dialog _FREEFORM
  QUERY br-conta-contabil DISPLAY
      string(tt-cod-conta-contabil, c-formato) @ tt-cod-conta-contabil format "x(20)"
tt-titulo-conta format "x(44)"
tt-natureza
tt-valor format "->,>>>,>>>,>>9.99"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 69 BY 5.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-conta-contabil AT ROW 1 COL 1
     bt-ok AT ROW 7.5 COL 2.72
     rt-buttom AT ROW 7.17 COL 1
     SPACE(0.13) SKIP(0.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Contas Cont†beis dos Movimentos do T°tulo"
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
   L-To-R                                                               */
/* BROWSE-TAB br-conta-contabil 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-conta-contabil
/* Query rebuild information for BROWSE br-conta-contabil
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-conta-contabil.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-conta-contabil */
&ANALYZE-RESUME

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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Contas Cont†beis dos Movimentos do T°tulo */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-conta-contabil
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */ 
find first param-global no-lock no-error.
if  avail param-global then do:
    assign c-formato = param-global.formato-conta-contabil.
end.
    
for each tt-conta-contabil:
    delete tt-conta-contabil.
end.

create tt-mov-tit.
assign tt-mov-tit.gr-mov-tit       = gr-mov-tit
       tt-mov-tit.cod-versao-integ = 1
       tt-mov-tit.l-vid-rel        = yes.

run crp/crapi012.p (input  table tt-mov-tit,
                    output table tt-conta-contabil,
                    output table tt-erro).
    
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
  ENABLE br-conta-contabil rt-buttom bt-ok 
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
{utp/ut9000.i "CR0709A" "2.00.00.025"}

/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  {utp/ut-liter.i Conta_Contabil MCR L}
  assign tt-conta-contabil.tt-cod-conta-contabil:label in browse {&browse-name} = return-value.

  {utp/ut-liter.i Lanáamento MCR L}
  assign tt-conta-contabil.tt-natureza:label in browse {&browse-name} = return-value.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-criar-tt-conta-contabil D-Dialog 
PROCEDURE pi-criar-tt-conta-contabil :
/***********************************************************************************
* Objetivo: 
***********************************************************************************/
                
    define input parameter p-conta-contabil like conta-contab.conta-contabil no-undo.
    define input parameter p-natureza       as integer                       no-undo.
    define input parameter p-valor          like titulo.vl-original          no-undo.
                    
    define var c-titulo-conta like conta-contab.titulo no-undo.
                    
    find first tt-conta-contabil
         where tt-conta-contabil.tt-cod-conta-contabil = p-conta-contabil
         and   tt-conta-contabil.tt-natureza           = {adinc/i01ad042.i 04 p-natureza} 
         no-error.
                    
    if  not avail tt-conta-contabil then do:
               
        find first conta-contab
             where conta-contab.ep-codigo    = mov-tit.ep-codigo
             and conta-contab.conta-contabil = p-conta-contabil
             no-lock no-error.
                        
        if avail conta-contab then do:
           assign c-titulo-conta = conta-contab.titulo.
        end.
        else do:
           {utp/ut-liter.i Conta_Cont†bil_n∆o_cadastrada MCR L}
           assign c-titulo-conta = trim(return-value).
        end.
                        
        create tt-conta-contabil.
        assign tt-conta-contabil.tt-cod-conta-contabil = p-conta-contabil
               tt-conta-contabil.tt-titulo-conta       = c-titulo-conta
               tt-conta-contabil.tt-natureza           = {adinc/i01ad042.i 04 p-natureza}
               tt-conta-contabil.tt-valor              = p-valor.
                   
    end.
    else do:
        assign tt-conta-contabil.tt-valor = tt-conta-contabil.tt-valor + p-valor.
    end.
                        
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-conta-contabil"}

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


