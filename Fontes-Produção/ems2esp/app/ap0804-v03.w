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
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i AP0804-V03 2.00.00.009}  /*** 010009 ***/
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
/* Miniflexibilizaá∆o */
{include/i_dbvers.i}

&Scop adm-attribute-dlg support/viewerd.w

/*Tratamento para independància de objetos*/
&glob ORIGINALNAME 'advwr~\v04ad098.w'

def var v-row-parent as rowid no-undo.
def var c-formato    as char  no-undo.
def var trad    as char  no-undo.
def new global shared var i-natureza  as int   no-undo.

DEF VAR h-cd0421             AS HANDLE NO-UNDO.
DEF VAR l-retem-matriz-impto AS LOGICAL NO-UNDO.
DEF VAR v-cod-forn-matriz    LIKE emitente.cod-emitente NO-UNDO.

def buffer b-emitente for emitente.

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
&Scoped-Define ENABLED-FIELDS emitente.cgc emitente.ins-municipal ~
emitente.ins-estadual emitente.agente-retencao emitente.flag-pag 
&Scoped-define ENABLED-TABLES emitente
&Scoped-define FIRST-ENABLED-TABLE emitente
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 
&Scoped-Define DISPLAYED-FIELDS emitente.cgc emitente.ins-municipal ~
emitente.ins-estadual emitente.agente-retencao emitente.flag-pag 
&Scoped-define DISPLAYED-TABLES emitente
&Scoped-define FIRST-DISPLAYED-TABLE emitente
&Scoped-Define DISPLAYED-OBJECTS cb-natureza l-retem-pagto c-inscr-inss ~
l-contr-vl-max-inss c-situacao 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-MODIFY-FIELDS cb-natureza l-retem-pagto 

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
DEFINE VARIABLE cb-natureza AS CHARACTER FORMAT "X(20)" INITIAL "0" 
     LABEL "" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24.57 BY 1 NO-UNDO.

DEFINE VARIABLE c-inscr-inss AS CHARACTER FORMAT "X(20)":U 
     LABEL "Inscriá∆o INSS/CEI" 
     VIEW-AS FILL-IN 
     SIZE 20.43 BY 1 NO-UNDO.

DEFINE VARIABLE c-situacao AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 12.57 BY .63
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 82.86 BY 6.58.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 82.86 BY 1.5.

DEFINE VARIABLE l-contr-vl-max-inss AS LOGICAL INITIAL no 
     LABEL "Controla Valor M†x INSS" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .83 NO-UNDO.

DEFINE VARIABLE l-retem-pagto AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     cb-natureza AT ROW 1.29 COL 33.43 COLON-ALIGNED
     emitente.cgc AT ROW 2.42 COL 33.43 COLON-ALIGNED
          LABEL "CNPJ":R9
          VIEW-AS FILL-IN 
          SIZE 20.43 BY .88
     emitente.ins-municipal AT ROW 3.42 COL 33.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.43 BY .88
     emitente.ins-estadual AT ROW 4.42 COL 33.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.43 BY .88
     emitente.agente-retencao AT ROW 5.5 COL 35.43
          LABEL "Retem Iva":L18
          VIEW-AS TOGGLE-BOX
          SIZE 20.86 BY .83
     l-retem-pagto AT ROW 5.5 COL 57
     c-inscr-inss AT ROW 6.42 COL 33.43 COLON-ALIGNED
     l-contr-vl-max-inss AT ROW 6.5 COL 57
     emitente.flag-pag AT ROW 8.54 COL 24 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "OK", 1,
"Suspenso para pagamentos", 2
          SIZE 43.29 BY .63
     c-situacao AT ROW 7.79 COL 2 COLON-ALIGNED NO-LABEL
     RECT-2 AT ROW 1 COL 1.57
     RECT-3 AT ROW 8.04 COL 1.57
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
         HEIGHT             = 8.54
         WIDTH              = 83.43.
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
       FRAME f-main:HIDDEN           = TRUE
       FRAME f-main:HEIGHT           = 8.54
       FRAME f-main:WIDTH            = 83.43.

/* SETTINGS FOR TOGGLE-BOX emitente.agente-retencao IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN c-inscr-inss IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-situacao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb-natureza IN FRAME f-main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN emitente.cgc IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX l-contr-vl-max-inss IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX l-retem-pagto IN FRAME f-main
   NO-ENABLE 3                                                          */
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

&Scoped-define SELF-NAME cb-natureza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-natureza V-table-Win
ON VALUE-CHANGED OF cb-natureza IN FRAME f-main
DO:
     apply 'entry':U to emitente.cgc in frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.cgc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cgc V-table-Win
ON ENTRY OF emitente.cgc IN FRAME f-main /* CNPJ */
DO:

   find first param-global no-lock no-error.

   do  with frame {&FRAME-NAME}:
           assign input frame {&frame-name} cb-natureza
               i-natureza = {adinc/i03ad098.i 06 cb-natureza}.

        if emitente.cgc:screen-value = "" then do:
           assign emitente.cgc:screen-value in frame {&FRAME-NAME} = "".
        end.

        if  emitente.cgc:screen-value <> "" then do:      
            if  i-natureza = 1 then do:
                assign emitente.cgc:format in frame {&FRAME-NAME} = param-global.formato-id-pessoal.
            end.
            else do:
                if  i-natureza = 2 then do:
                    assign emitente.cgc:format in frame {&FRAME-NAME} = param-global.formato-id-federal.
                end.
                else do:
                    assign emitente.cgc:format in frame {&FRAME-NAME} = "x(19)".
                end.
            end.
        end.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cgc V-table-Win
ON LEAVE OF emitente.cgc IN FRAME f-main /* CNPJ */
DO:

     assign c-formato = input frame {&FRAME-NAME} emitente.cgc no-error.

     if  ERROR-STATUS:get-number(1) <> 0 then
         assign emitente.cgc:format in frame {&FRAME-NAME} = "x(19)".
     else
         apply 'entry':U to emitente.cgc in frame {&FRAME-NAME}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.ins-estadual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.ins-estadual V-table-Win
ON ENTRY OF emitente.ins-estadual IN FRAME f-main /* Inscriá∆o Estadual */
DO:
   do  with frame {&FRAME-NAME}:

        assign input frame {&frame-name} cb-natureza
               i-natureza = {adinc/i03ad098.i 06 cb-natureza}.

        if  emitente.cgc:screen-value <> "" then       
            assign emitente.ins-estadual:format in frame {&frame-name} = "x(19)".
   end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.ins-estadual V-table-Win
ON LEAVE OF emitente.ins-estadual IN FRAME f-main /* Inscriá∆o Estadual */
DO:
     assign c-formato = input frame {&frame-name} emitente.ins-estadual no-error.
     if  ERROR-STATUS:get-number(1) <> 0 then
         assign emitente.ins-estadual:format in frame {&frame-name} = "x(19)":U.
     else
         apply 'entry':U to emitente.ins-estadual in frame {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  {utp/ut-liter.i Situaá∆o * R}.
  assign c-situacao = trim(return-value).

  {utp/ut-liter.i Natureza * L}.
  assign cb-natureza:label in frame {&frame-name} = return-value.

  {utp/ut-liter.i Retem_Pagto * R}.
  assign l-retem-pagto:label in frame {&frame-name} = return-value.


  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).
  &ENDIF

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

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  {utp/ut-liter.i Situaá∆o * R}
  assign c-situacao = trim(return-value).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry V-table-Win 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  {utp/ut-liter.i Situaá∆o * R}
  assign c-situacao = trim(return-value).

  disp c-situacao with frame {&FRAME-NAME}. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  {include/i-valid.i}

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  if RETURN-VALUE = 'ADM-ERROR':U then 
     return 'ADM-ERROR':U.


  assign input frame {&FRAME-NAME} cb-natureza
         emitente.natureza = {adinc/i03ad098.i 06 cb-natureza}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy V-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  DELETE PROCEDURE h-cd0421.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  &if  defined(ADM-MODIFY-FIELDS) &then
      disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
  &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  if  avail emitente and adm-new-record = no then 
      assign cb-natureza:screen-value in frame {&frame-name} = {adinc/i03ad098.i 04 emitente.natureza}.
  else if not avail emitente and adm-new-record = yes then 
      assign cb-natureza:screen-value in frame {&frame-name} = {adinc/i03ad098.i 04 01}.
  else if avail emitente and adm-new-record = yes then 
      assign cb-natureza:screen-value in frame {&frame-name} = {adinc/i03ad098.i 04 i-natureza}.      

  if avail emitente then do:      
     if  emitente.natureza = 1 then
         assign emitente.cgc:format in frame {&FRAME-NAME} = if avail param-global 
                                                             then param-global.formato-id-pessoal
                                                             else "x(19)":U.
     else if  emitente.natureza = 2 then 
         assign emitente.cgc:format in frame {&FRAME-NAME} = if avail param-global 
                                                             then param-global.formato-id-federal
                                                             else "x(19)":U.
     else
         assign emitente.cgc:format in frame {&FRAME-NAME} = "x(19)":U.


     display emitente.cgc emitente.ins-estadual with frame {&frame-name}.

    /*------ IN 87 -----*/
    IF  i-pais-impto-usuario = 1 then do:   /* Se for Brasil */
        assign l-contr-vl-max-inss = no
               c-inscr-inss        = ''.
        &if '{&mgadm_version}' >= '2.05' &then
            assign l-contr-vl-max-inss = emitente.log-controla-val-max-inss
                   c-inscr-inss        = emitente.cod-inscr-inss.
        &else
            IF  num-entries(emitente.char-2,chr(10)) > 1 then do:
                if  entry(1,emitente.char-2,chr(10)) = 'yes' then
                    assign l-contr-vl-max-inss = yes.
                else
                    assign l-contr-vl-max-inss = no.
                assign c-inscr-inss = entry(2,emitente.char-2,chr(10)).
            end.
            ELSE
                assign c-inscr-inss = "".
        &endif                           
        disp l-contr-vl-max-inss
             c-inscr-inss with frame {&frame-name}.

        /*--- Limite de Retená∆o PIS/COFINS/CSLL ---*/
        IF  emitente.nome-abrev = ""
        OR  emitente.nome-abrev = emitente.nome-matriz THEN DO:
            &if '{&mgadm_version}' >= '2.06' &then
                assign l-retem-pagto:checked = emitente.retem-pagto.
            &else
                assign l-retem-pagto:checked = (upper(substr(emitente.char-1,30,1)) = "S").
            &endif
        END.
        ELSE DO:
            RUN pi-ret-matriz-cod-forn IN h-cd0421 (INPUT emitente.cod-emitente,
                                                    OUTPUT v-cod-forn-matriz,
                                                    output l-retem-matriz-impto).
            assign l-retem-pagto:checked = l-retem-matriz-impto.
        END.
    end.

  end.

  {utp/ut-liter.i Situaá∆o * R}
  assign c-situacao = trim(return-value).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  &if  defined(ADM-MODIFY-FIELDS) &then
      enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
  &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  find first param-global no-lock no-error.

  RUN cdp/cd0421.p PERSISTENT SET h-cd0421.

  {utp/ut-liter.i Situaá∆o * R}
  assign c-situacao = trim(return-value).

&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
ASSIGN cAuxTraducao001 = {adinc/i03ad098.i 03}.
RUN utp/ut-lstit.p (INPUT-OUTPUT cAuxTraducao001).
ASSIGN cb-natureza:LIST-ITEM-PAIRS in frame {&FRAME-NAME} = cAuxTraducao001.
&else
ASSIGN cb-natureza:list-items in frame {&frame-name} = {adinc/i03ad098.i 03}.
&endif
ASSIGN 
         emitente.cgc:label     in frame {&FRAME-NAME} = param-global.label-cgc.

  disable c-inscr-inss with frame {&frame-name}.

  IF  i-pais-impto-usuario = 1 THEN
      ASSIGN c-inscr-inss:VISIBLE  IN FRAME {&FRAME-NAME} = YES
             l-retem-pagto:VISIBLE IN FRAME {&FRAME-NAME} = YES.
  ELSE
      ASSIGN c-inscr-inss:VISIBLE  IN FRAME {&FRAME-NAME} = NO
             l-retem-pagto:VISIBLE IN FRAME {&FRAME-NAME} = NO.

  if param-global.label-cgc = "" then do:
     {utp/ut-liter.i CNPJ/CPF * R}.
     assign emitente.cgc:label in frame {&FRAME-NAME} = trim(return-value).
  end.    

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

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
    Notes: N∆o fazer assign aqui. Nes0ta procedure
    devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
    ainda n∆o foi criado.       
------------------------------------------------------------------------------*/

    define var i-erro as integer no-undo.

    {include/i-vldfrm.i}

    assign input frame {&frame-name} cb-natureza
           i-natureza = {adinc/i03ad098.i 06 cb-natureza}.

    if  emitente.cgc:screen-value <> ""
    and i-natureza <> 3 
    and i-natureza <> 4 then do:    
        if  i-natureza = 1 then
            assign emitente.cgc:format in frame {&FRAME-NAME} =  param-global.formato-id-pessoal
                   c-formato = input frame {&FRAME-NAME} emitente.cgc no-error.    
        if  i-natureza = 2 then
            assign emitente.cgc:format in frame {&FRAME-NAME} =  param-global.formato-id-federal
                   c-formato = input frame {&FRAME-NAME} emitente.cgc no-error.
    end.               
    else
        assign emitente.cgc:format in frame {&FRAME-NAME} =  "x(19)"
               c-formato = input frame {&FRAME-NAME} emitente.cgc no-error.

    if  ERROR-STATUS:get-number(1) <> 0 then do:
        {include/i-vldprg.i}
        run utp/ut-msgs.p (input "show",
                           input 2744,
                           input "").
        apply 'entry' to emitente.cgc in frame {&frame-name}.
        return 'ADM-ERROR':U.
    end.   

    assign input frame {&frame-name} cb-natureza
           i-natureza = {adinc/i03ad098.i 06 cb-natureza}.

    find empresa where empresa.ep-codigo = i-ep-codigo-usuario no-lock no-error.
    if  avail empresa and empresa.pais = "Brasil" then do: 
        do  on error undo, return 'adm-error':
            run cdp/cd6666.p (input input frame {&FRAME-NAME} emitente.cgc, 
                              input i-natureza).
            if  return-value = 'NOK' then do:
                {include/i-vldprg.i}
                apply 'entry' to emitente.cgc in frame {&frame-name}.
                return "adm-error":U.
            end.
        end.          
    end.

    /*---- Verifica se o CGC j† existe para outro emitente ------*/
    if (i-natureza = 1 or i-natureza = 2) then do:
        find first b-emitente no-lock
             where b-emitente.cgc           = input frame {&FRAME-NAME}  emitente.cgc
             and   b-emitente.cod-emitente <> emitente.cod-emitente no-error.

        if  avail b-emitente then do:
            if  param-global.bloqueio-cgc = 2 then do:
                {include/i-vldprg.i}
                run utp/ut-msgs.p (input "show",
                                   input 9021,
                                   input "").
                if  return-value = "no" then do:
                    {include/i-vldprg.i}
                    apply 'entry' to emitente.cgc in frame {&frame-name}.
                    return "adm-error":U.
                end.
            end.
            if  param-global.bloqueio-cgc = 3 then do:
                {include/i-vldprg.i}
                run utp/ut-msgs.p (input "show",
                                   input 973,
                                   input "").
                apply 'entry' to emitente.cgc in frame {&frame-name}.
                return "adm-error":U.
            end.
        end.
    end.

    if  param-global.id-estadual-obrigatorio = yes and i-natureza = 2 then do:
        if  input frame {&frame-name} emitente.ins-estadual = '' then do:
            {include/i-vldprg.i}
            run utp/ut-msgs.p (input "show",
                               input 8599,
                               input "").
            apply 'entry' to emitente.ins-estadual in frame {&frame-name}.
            return 'ADM-ERROR':U.
        end.
    end.   

    assign emitente.cgc:format          in frame {&FRAME-NAME} = "x(19)"
           emitente.ins-estadual:format in frame {&frame-name} = "x(19)".

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

