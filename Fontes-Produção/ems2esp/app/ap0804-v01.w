&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgadm            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i AP0804-V01 2.00.00.002}  /*** 010002 ***/


/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
&Scop adm-attribute-dlg support/viewerd.w

/*Tratamento para independància de objetos*/
&glob ORIGINALNAME 'advwr\v02ad098.w'

/* miniflexibilizaá∆o */
{include\i_dbvers.i}

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
define var v-row-parent as rowid no-undo.
define new global shared var gr-emitente as rowid NO-UNDO.


/* definiá∆o de buffers de tabela */
define buffer b-emitente for emitente.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS emitente.nome-emit emitente.cod-gr-forn ~
emitente.nome-matriz emitente.nome-mic-reg emitente.atividade ~
emitente.data-implant emitente.linha-produt emitente.cod-transp ~
emitente.emite-etiq emitente.ind-rendiment emitente.ven-sabado ~
emitente.ven-domingo emitente.ven-feriado emitente.emissao-ped 
&Scoped-define ENABLED-TABLES emitente
&Scoped-define FIRST-ENABLED-TABLE emitente
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 RECT-9 RECT-10 
&Scoped-Define DISPLAYED-FIELDS emitente.nome-emit emitente.cod-gr-forn ~
emitente.nome-matriz emitente.nome-mic-reg emitente.atividade ~
emitente.data-implant emitente.linha-produt emitente.cod-transp ~
emitente.emite-etiq emitente.ind-rendiment emitente.ven-sabado ~
emitente.ven-domingo emitente.ven-feriado emitente.emissao-ped 
&Scoped-define DISPLAYED-TABLES emitente
&Scoped-define FIRST-DISPLAYED-TABLE emitente
&Scoped-Define DISPLAYED-OBJECTS c-gr-forn c-nome-transp ~
tg-vencto-dia-nao-util text-2 text-3 text-4 text-1 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-gr-forn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-transp AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE text-1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .63
     FONT 2 NO-UNDO.

DEFINE VARIABLE text-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .63
     FONT 2 NO-UNDO.

DEFINE VARIABLE text-3 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .54
     FONT 2 NO-UNDO.

DEFINE VARIABLE text-4 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .54
     FONT 2 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20 BY 2.79.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20 BY 2.79.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20 BY 2.79.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20 BY 2.79.

DEFINE VARIABLE tg-vencto-dia-nao-util AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 32.86 BY .63 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     emitente.nome-emit AT ROW 1.25 COL 23.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 41.43 BY .88
     emitente.cod-gr-forn AT ROW 2.25 COL 23.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     c-gr-forn AT ROW 2.25 COL 27.29 COLON-ALIGNED NO-LABEL
     emitente.nome-matriz AT ROW 3.17 COL 23.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .88
     emitente.nome-mic-reg AT ROW 3.17 COL 66.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .88
     emitente.atividade AT ROW 4.17 COL 23.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .88
     emitente.data-implant AT ROW 4.17 COL 66.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .88
     emitente.linha-produt AT ROW 5.17 COL 66.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.43 BY .88
     emitente.cod-transp AT ROW 5.25 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     c-nome-transp AT ROW 5.25 COL 29 COLON-ALIGNED NO-LABEL
     emitente.emite-etiq AT ROW 6.54 COL 3
          VIEW-AS TOGGLE-BOX
          SIZE 18.29 BY .75
     emitente.ind-rendiment AT ROW 6.54 COL 24.14
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .63
     tg-vencto-dia-nao-util AT ROW 6.54 COL 46.57
     emitente.ven-sabado AT ROW 8.38 COL 5 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Prorroga", 1,
"Antecipa", 2,
"Mantem", 3
          SIZE 15.14 BY 1.92
     emitente.ven-domingo AT ROW 8.38 COL 25.57 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Prorroga", 1,
"Antecipa", 2,
"Mantem", 3
          SIZE 15.14 BY 1.92
     emitente.ven-feriado AT ROW 8.38 COL 47.57 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Prorroga", 1,
"Antecipa", 2,
"Mantem", 3
          SIZE 15.14 BY 1.92
     emitente.emissao-ped AT ROW 8.38 COL 67 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Formulario", 1,
"Magnetico", 2,
"Telex", 3
          SIZE 15.14 BY 1.92
     text-2 AT ROW 7.63 COL 1.57 COLON-ALIGNED NO-LABEL
     text-3 AT ROW 7.63 COL 22.29 COLON-ALIGNED NO-LABEL
     text-4 AT ROW 7.63 COL 42.72 COLON-ALIGNED NO-LABEL
     text-1 AT ROW 7.63 COL 63.86 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 7.88 COL 3
     RECT-8 AT ROW 7.88 COL 23.57
     RECT-9 AT ROW 7.88 COL 44.14
     RECT-10 AT ROW 7.88 COL 64.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgadm.emitente
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 9.67
         WIDTH              = 86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-gr-forn IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nome-transp IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN text-1 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN text-2 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN text-3 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN text-4 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-vencto-dia-nao-util IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       tg-vencto-dia-nao-util:HIDDEN IN FRAME f-main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME emitente.cod-gr-forn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cod-gr-forn V-table-Win
ON F5 OF emitente.cod-gr-forn IN FRAME f-main /* Grupo */
DO:
    assign l-implanta = yes.
    {include/zoomvar.i &prog-zoom=adzoom/z01ad131.w
                       &campo=emitente.cod-gr-forn
                       &campozoom=cod-gr-forn}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cod-gr-forn V-table-Win
ON LEAVE OF emitente.cod-gr-forn IN FRAME f-main /* Grupo */
DO:
  assign l-implanta = yes.
  {include/leave.i &tabela=grupo-forn
                   &atributo-ref=descricao
                   &variavel-ref=c-gr-forn
                   &where="grupo-forn.cod-gr-forn = input frame {&frame-name} emitente.cod-gr-forn"}


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cod-gr-forn V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.cod-gr-forn IN FRAME f-main /* Grupo */
DO:
    apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.cod-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cod-transp V-table-Win
ON F5 OF emitente.cod-transp IN FRAME f-main /* Transportador Padr∆o */
DO:
   assign l-implanta = yes.
  {include/zoom.i &prog-zoom="adzoom/z01ad268.w"
                  &tabela=emitente
                  &atributo=cod-transp}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cod-transp V-table-Win
ON LEAVE OF emitente.cod-transp IN FRAME f-main /* Transportador Padr∆o */
DO:
  assign l-implanta = yes.
  {include/leave.i &tabela=transporte
                   &atributo-ref=nome-abrev
                   &variavel-ref=c-nome-transp
                   &where="transporte.cod-transp = input frame {&frame-name}
                   emitente.cod-transp"}

  if input frame {&FRAME-NAME} emitente.cod-transp <> 0 then do:
     find first transporte
          where transporte.cod-transp = input frame {&frame-name} emitente.cod-transp
          no-lock no-error.
     if not avail transporte then do:
         run utp/ut-msgs.p (input "show":U,
                            input 2190,
                            input '').
         apply 'entry' to emitente.cod-transp in frame {&FRAME-NAME}.
         return 'ADM-ERROR':U.
     end.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cod-transp V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.cod-transp IN FRAME f-main /* Transportador Padr∆o */
DO:
  apply "f5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.nome-matriz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.nome-matriz V-table-Win
ON ENTRY OF emitente.nome-matriz IN FRAME f-main /* Matriz */
DO:
    assign l-implanta = no.
    run get-attribute('adm-new-record').
    if return-value = 'yes' then do:
        RUN Get-Field-Screen-Value IN adm-broker-hdl
            (INPUT THIS-PROCEDURE,
             INPUT 'nome-abrev').
        assign emitente.nome-matriz:screen-value in frame {&FRAME-NAME} = return-value.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.nome-matriz V-table-Win
ON F5 OF emitente.nome-matriz IN FRAME f-main /* Matriz */
DO:
  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom=adzoom/z08ad098.w
                     &campo=emitente.nome-matriz
                     &campozoom=nome-matriz}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.nome-matriz V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.nome-matriz IN FRAME f-main /* Matriz */
DO:
  apply 'f5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.nome-mic-reg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.nome-mic-reg V-table-Win
ON F5 OF emitente.nome-mic-reg IN FRAME f-main /* Microrregi∆o */
DO:
  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom=dizoom/z01di107.w
                     &campo=emitente.nome-mic-reg
                     &campozoom=nome-mic-reg}  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.nome-mic-reg V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.nome-mic-reg IN FRAME f-main /* Microrregi∆o */
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  {utp/ut-rtlbl.i mgadm emitente emissao-ped text-1}
  {utp/ut-rtlbl.i mgadm emitente ven-sabado  text-2}
  {utp/ut-rtlbl.i mgadm emitente ven-domingo text-3} 
  {utp/ut-rtlbl.i mgadm emitente ven-feriado text-4}

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  if emitente.cod-transp:load-mouse-pointer('image/lupa.cur') then. 
  if emitente.nome-matriz:load-mouse-pointer('image/lupa.cur') then.
  if emitente.nome-mic-reg:load-mouse-pointer('image/lupa.cur') then.
  if emitente.cod-gr-forn:load-mouse-pointer('image/lupa.cur') then.

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "emitente"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "emitente"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {utp/ut-rtlbl.i mgadm emitente emissao-ped text-1}
  {utp/ut-rtlbl.i mgadm emitente ven-sabado  text-2}
  {utp/ut-rtlbl.i mgadm emitente ven-domingo text-3} 
  {utp/ut-rtlbl.i mgadm emitente ven-feriado text-4}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry V-table-Win 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN get-attribute('adm-new-record').
  if return-value = 'yes' then 
     assign emitente.data-implant:screen-value in frame {&FRAME-NAME} = string(today).



  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  define var c-matriz-anterior like emitente.nome-matriz no-undo.
  assign c-matriz-anterior = if avail emitente then emitente.nome-matriz else "":U.

  {include/i-valid.i}
  /* Ponha na pi-validate todas as validaá‰es */
  /* N∆o gravar nada no registro antes do dispatch do assign-record e 
  nem na PI-validate. */  
  /* Dispatch standard ADM method.                             */

  if  tg-vencto-dia-nao-util:visible in frame {&FRAME-NAME} = yes then
      assign tg-vencto-dia-nao-util = input frame {&FRAME-NAME} tg-vencto-dia-nao-util.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  /* Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
  /* Code placed here will execute AFTER standard behavior.    */

  run pi-versao (input 5).

  if c-matriz-anterior <> "" and c-matriz-anterior <> emitente.nome-matriz then do:
     {utp/ut-liter.i Alterando_Matriz}
     status input trim(return-value).
     find b-emitente 
          where b-emitente.nome-abrev = emitente.nome-matriz
          no-lock no-error.
     for each titulo use-index emitente
        where titulo.cod-emitente = emitente.cod-emitente EXCLUSIVE-LOCK:
        assign titulo.matriz = b-emitente.cod-emitente.
     end.
     for each tit-ap use-index fornec
         where tit-ap.cod-fornec = emitente.cod-emitente EXCLUSIVE-LOCK:
         assign tit-ap.matriz = b-emitente.cod-emitente.
     end.
     status default.
  end.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  &if  defined(ADM-MODIFY-FIELDS) &then
      disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
  &endif

  run pi-versao (input 3). /* miniflexibilizaá∆o */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  if avail emitente then do:
     assign gr-emitente = rowid(emitente).

     find first transporte 
          where transporte.cod-transp = emitente.cod-transp 
          no-lock no-error.
     assign c-nome-transp = if avail transporte 
                            then transporte.nome-abrev 
                            else "":U.

     find first grupo-fornec 
          where grupo-fornec.cod-gr-forn = emitente.cod-gr-forn 
          no-lock no-error.
          assign c-gr-forn = if avail grupo-fornec
                        then grupo-fornec.descricao 
                        else "":U.
  end.   

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  run pi-versao (input 4).

  /* Code placed here will execute AFTER standard behavior.    */
  {utp/ut-rtlbl.i mgadm emitente emissao-ped text-1}
  {utp/ut-rtlbl.i mgadm emitente ven-sabado  text-2}
  {utp/ut-rtlbl.i mgadm emitente ven-domingo text-3}
  {utp/ut-rtlbl.i mgadm emitente ven-feriado text-4}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  &if  defined(ADM-MODIFY-FIELDS) &then
      enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
  &endif

  run pi-versao (input 2). /* miniflexibilizaá∆o */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut-liter.i Micro_Regi∆o MCD L}

  Assign emitente.nome-mic-reg:label in frame {&FRAME-NAME} = Return-Value.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  run pi-versao (input 1). /* miniflexibilizaá∆o */    

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     define input parameter v-row-parent-externo as rowid no-undo.

     assign v-row-parent = v-row-parent-externo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Validaá∆o de dicion†rio */

    /* nome n∆o pode ser branco */
    if input frame {&FRAME-NAME} emitente.nome-emit = '' then do:
        {include/i-vldprg.i}
        run utp/ut-msgs.p (input "show":U, input 2820, input '').
        apply 'entry' to emitente.nome-emit in frame {&FRAME-NAME}.
        return 'ADM-ERROR':U.
    end.

    /* matriz n∆o encontrada */
    RUN Get-Field-Screen-Value IN adm-broker-hdl
    (INPUT THIS-PROCEDURE,
     INPUT 'nome-abrev').
    if input frame {&FRAME-NAME} emitente.nome-matriz <> return-value then do:
        find first b-emitente 
             where b-emitente.nome-abrev = input frame {&FRAME-NAME} emitente.nome-matriz
             no-lock no-error.
        if not avail b-emitente then do:
            {include/i-vldprg.i}
            run utp/ut-msgs.p (input "show":U, input 6334, input '').
            apply 'entry' to emitente.nome-matriz in frame {&FRAME-NAME}.
            return 'ADM-ERROR':U.
        end.
    end.

    /* validad transportador */
    if input frame {&FRAME-NAME} emitente.cod-transp <> 0 then do:
        find first transporte
             where transporte.cod-transp = input frame {&frame-name} emitente.cod-transp
             no-lock no-error.
        if not avail transporte then do:
            {include/i-vldprg.i}
            run utp/ut-msgs.p (input "show":U, 
                               input 2190, 
                               input '').
            apply 'entry' to emitente.cod-transp in frame {&FRAME-NAME}.
            return 'ADM-ERROR':U.
        end.
    end.

    /* valida micro-regiao * n∆o Ç mandat¢rio */
    if input frame {&FRAME-NAME} emitente.nome-mic-reg <> '' then do:
        find first micro-reg
             where micro-reg.nome-mic-reg = input frame {&FRAME-NAME} emitente.nome-mic-reg
             no-lock no-error.
        if not avail micro-reg then do:
            {include/i-vldprg.i}
            run utp/ut-msgs.p (input "show":U, input 4266, input '').
            apply 'entry' to emitente.nome-mic-reg in frame {&FRAME-NAME}.
            return 'ADM-ERROR':U.
        end.
    end.

    /* data inv†lida */
    if input frame {&FRAME-NAME} emitente.data-implant:format <> "99/99/9999" then do:
       {include/i-vldprg.i}
       run utp/ut-msgs.p (input "show":U, input 173, input '').
       apply 'entry' to emitente.data-implant in frame {&FRAME-NAME}.
       return 'ADM-ERROR':U.
    end.

   /* verifica grupo de fornecedor */

   find first grupo-forn 
         where grupo-forn.cod-gr-forn = input frame {&FRAME-NAME} emitente.cod-gr-forn 
         no-lock no-error.
    if not avail grupo-forn then do:
       {include/i-vldprg.i}
       run utp\ut-msgs.p (input "show",
                          input 4278,
                          input "").
       apply 'entry' to emitente.cod-gr-forn.
       return 'ADM-ERROR':U.
   end. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-versao V-table-Win 
PROCEDURE pi-versao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input param p-ind as int no-undo.

    &if "{&mgadm_version}" >= "2.02" &then    
        case p-ind:
            when 1 then do: /* initialize */
                {utp/ut-liter.i Vencimento_Igual_Data_Fluxo * R}
                assign tg-vencto-dia-nao-util:label in frame {&FRAME-NAME} = return-value  
                       tg-vencto-dia-nao-util:visible in frame {&FRAME-NAME} = yes.  
            end.
            when 2 then do: /* enable */
                assign tg-vencto-dia-nao-util:sensitive in frame {&FRAME-NAME} = yes.  
            end.
            when 3 then do: /* disable */
                assign tg-vencto-dia-nao-util:sensitive in frame {&FRAME-NAME} = no.  
            end.
            when 4 then do: /* display */
                if avail emitente then
                   assign tg-vencto-dia-nao-util:screen-value in frame {&FRAME-NAME} = string(emitente.vencto-dia-nao-util).
            end.
            when 5 then do: /* assign-records */
                assign emitente.vencto-dia-nao-util = tg-vencto-dia-nao-util.  
            end.
        end case.    
    &endif.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "emitente"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

