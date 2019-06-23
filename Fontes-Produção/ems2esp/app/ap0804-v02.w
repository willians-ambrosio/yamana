&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cadme        PROGRESS
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
def buffer banco    for ems2cadme.banco.
def buffer portador for ems2cadme.portador.

{include/i-prgvrs.i AP0804-V02 2.00.00.007}  /*** 010007 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i ap0804-v02 MAP}
&ENDIF

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
&glob ORIGINALNAME 'advwr~\v03ad098.w'

/* miniflexibilizaá∆o */
{include/i_dbvers.i}                
{cdp/cdcfgfin.i}

&if "{&mgadm_version}" >= "2.02" &then                
    {cdp/cd0401f.i}                
&endif.    


def var v-row-parent     as rowid              no-undo.
def var i-modalidade     as integer format "9" no-undo.

DEF VAR l-funcao-portador-ap AS LOG            NO-UNDO.

def var c-formato-agencia as char format "x(9)"  no-undo.
def var c-formato-conta   as char format "x(20)" no-undo.
def var i-cb-pagto        as integer             no-undo.
def var i-cb-modalidade   as integer             no-undo.

/* verifica funá∆o */
{include/getdefinedfunction.i}
ASSIGN l-funcao-portador-ap = &if defined(BF_FIN_PORTADOR_AP) &then yes &else GetDefinedFunction("SPP-PORTADOR-AP") &endif.

def new global shared var i-ep-codigo-usuario  like ems2cadme.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS emitente.portador emitente.cod-banco ~
emitente.taxa-financ emitente.nr-dias-taxa emitente.tp-desp-padrao ~
emitente.cod-cond-pag emitente.bonificacao emitente.dias-comp 
&Scoped-define ENABLED-TABLES emitente
&Scoped-define FIRST-ENABLED-TABLE emitente
&Scoped-Define ENABLED-OBJECTS rt-varias 
&Scoped-Define DISPLAYED-FIELDS emitente.portador emitente.cod-banco ~
emitente.taxa-financ emitente.nr-dias-taxa emitente.tp-desp-padrao ~
emitente.cod-cond-pag emitente.bonificacao emitente.dias-comp 
&Scoped-define DISPLAYED-TABLES emitente
&Scoped-define FIRST-DISPLAYED-TABLE emitente
&Scoped-Define DISPLAYED-OBJECTS cb-modalidade c-nome-banco C-agencia ~
cb-pagto c-desc-tipo c-desc-cond 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS cb-modalidade 
&Scoped-define ADM-ASSIGN-FIELDS cb-modalidade 
&Scoped-define ADM-MODIFY-FIELDS cb-modalidade cb-pagto 

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
DEFINE BUTTON bt-varias-contas 
     LABEL "V†rias Contas Correntes" 
     SIZE 20.72 BY 1.13.

DEFINE VARIABLE cb-modalidade AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE cb-pagto AS CHARACTER FORMAT "X(25)" INITIAL "0" 
     LABEL "":R18 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "item1" 
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE C-agencia AS CHARACTER FORMAT "x(9)" INITIAL "000000-00" 
     LABEL "":R9 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88.

DEFINE VARIABLE c-conta-corren AS CHARACTER FORMAT "x(20)" INITIAL "0000000000-00" 
     LABEL "":R17 
     VIEW-AS FILL-IN 
     SIZE 15.14 BY .88.

DEFINE VARIABLE c-desc-cond AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 24.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-tipo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-banco AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.29 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-varias
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21.57 BY 1.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     emitente.portador AT ROW 1.42 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     cb-modalidade AT ROW 1.42 COL 29.43 COLON-ALIGNED NO-LABEL
     bt-varias-contas AT ROW 2.13 COL 56
     emitente.cod-banco AT ROW 2.42 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     c-nome-banco AT ROW 2.42 COL 29.43 COLON-ALIGNED NO-LABEL
     C-agencia AT ROW 3.42 COL 22 COLON-ALIGNED
     c-conta-corren AT ROW 3.42 COL 60 COLON-ALIGNED
     emitente.taxa-financ AT ROW 4.42 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.29 BY .88
     emitente.nr-dias-taxa AT ROW 4.42 COL 60 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     cb-pagto AT ROW 5.42 COL 22 COLON-ALIGNED
     emitente.tp-desp-padrao AT ROW 6.58 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.86 BY .88
     c-desc-tipo AT ROW 6.58 COL 27.14 COLON-ALIGNED NO-LABEL
     emitente.cod-cond-pag AT ROW 7.5 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.86 BY .88
     c-desc-cond AT ROW 7.5 COL 27.14 COLON-ALIGNED NO-LABEL
     emitente.bonificacao AT ROW 8.5 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     emitente.dias-comp AT ROW 8.5 COL 60 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     rt-varias AT ROW 2 COL 55.57
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
         WIDTH              = 79.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

/* {utp/ut-glob.i} */
{src/adm/method/viewer.i}
{include/c-viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-varias-contas IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       bt-varias-contas:HIDDEN IN FRAME f-main           = TRUE.

/* SETTINGS FOR FILL-IN C-agencia IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-conta-corren IN FRAME f-main
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN c-desc-cond IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-tipo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nome-banco IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb-modalidade IN FRAME f-main
   NO-ENABLE 1 2 3                                                      */
/* SETTINGS FOR COMBO-BOX cb-pagto IN FRAME f-main
   NO-ENABLE 3                                                          */
ASSIGN 
       rt-varias:HIDDEN IN FRAME f-main           = TRUE.

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

&Scoped-define SELF-NAME bt-varias-contas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-varias-contas V-table-Win
ON CHOOSE OF bt-varias-contas IN FRAME f-main /* V†rias Contas Correntes */
DO:

  run pi-versao (input 4).  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME C-agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-agencia V-table-Win
ON LEAVE OF C-agencia IN FRAME f-main
DO:
   assign c-formato-agencia = input frame {&FRAME-NAME} c-agencia no-error.

  if  ERROR-STATUS:ERROR then do:
      assign c-agencia:format in frame {&FRAME-NAME} = "x(9)".
  end.
  else do:
      if c-agencia:screen-value = "" then
         assign c-agencia:screen-value = "000000-00".
      else
         run pi-valida-formato (input "agencia").
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-conta-corren
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-conta-corren V-table-Win
ON LEAVE OF c-conta-corren IN FRAME f-main
DO:
  assign c-formato-conta = input frame {&FRAME-NAME} c-conta-corren no-error.
  if  ERROR-STATUS:ERROR then do:
      assign c-conta-corren:format in frame {&FRAME-NAME} = "x(20)".
  end.
  else do:
     if c-conta-corren:screen-value = "" then
        c-conta-corren:screen-value = "0000000000-00". 
      else
        run pi-valida-formato (input "conta corrente").  
  end.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-modalidade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-modalidade V-table-Win
ON VALUE-CHANGED OF cb-modalidade IN FRAME f-main
DO:
  assign input frame {&FRAME-NAME} cb-modalidade
         i-cb-modalidade = {adinc/i03ad209.i 06 cb-modalidade}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-pagto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-pagto V-table-Win
ON VALUE-CHANGED OF cb-pagto IN FRAME f-main
DO:
  assign input frame {&FRAME-NAME} cb-pagto
         i-cb-pagto = {adinc/i22ad098.i 06 cb-pagto}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.cod-banco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cod-banco V-table-Win
ON F5 OF emitente.cod-banco IN FRAME f-main /* Banco */
DO:
  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="adzoom/z01ad018.w"
                     &campo=emitente.cod-banco
                     &campozoom=cod-banco}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cod-banco V-table-Win
ON LEAVE OF emitente.cod-banco IN FRAME f-main /* Banco */
DO:
  {include/leave.i &tabela=banco
                   &atributo-ref=nome-banco
                   &variavel-ref=c-nome-banco
                   &where="banco.cod-banco = input frame {&frame-name}
                   emitente.cod-banco"}                       

/*if  emitente.cod-banco <> 0 then do:
 *     find banco
 *         where banco.cod-banco = input frame {&frame-name} emitente.cod-banco
 *         no-lock no-error.
 *     if  not avail banco then
 *         {utp/ut-liter.i Banco_n∆o_Cadastrado * L}
 *         assign  c-nome-banco:screen-value in frame {&frame-name} = return-value.
 *  
 * end.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cod-banco V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.cod-banco IN FRAME f-main /* Banco */
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.cod-cond-pag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cod-cond-pag V-table-Win
ON F5 OF emitente.cod-cond-pag IN FRAME f-main /* Condiá∆o Pagamento */
DO:
  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="adzoom/z01ad039.w"
                     &campo=emitente.cod-cond-pag
                     &campozoom=cod-cond-pag}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cod-cond-pag V-table-Win
ON LEAVE OF emitente.cod-cond-pag IN FRAME f-main /* Condiá∆o Pagamento */
DO:
  {include/leave.i &tabela=cond-pagto
                   &atributo-ref=descricao
                   &variavel-ref=c-desc-cond
                   &where="cond-pagto.cod-cond-pag = input frame {&frame-name}
                   emitente.cod-cond-pag}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cod-cond-pag V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.cod-cond-pag IN FRAME f-main /* Condiá∆o Pagamento */
DO:
  apply "f5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.portador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.portador V-table-Win
ON F5 OF emitente.portador IN FRAME f-main /* Portador */
DO:
  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="adzoom/z01ad209.w"
                     &campo=emitente.portador
                     &campozoom=cod-portador}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.portador V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.portador IN FRAME f-main /* Portador */
DO:
  apply 'f5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.tp-desp-padrao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.tp-desp-padrao V-table-Win
ON F5 OF emitente.tp-desp-padrao IN FRAME f-main /* Tp Desp Padr∆o */
DO:
  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="adzoom/z01ad259.w"
                  &campo=emitente.tp-desp-padrao
                  &campozoom=tp-codigo}                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.tp-desp-padrao V-table-Win
ON LEAVE OF emitente.tp-desp-padrao IN FRAME f-main /* Tp Desp Padr∆o */
DO:
  {include/leave.i &tabela=tipo-rec-desp
                   &atributo-ref=descricao
                   &variavel-ref=c-desc-tipo
                   &where="tipo-rec-desp.tp-codigo = input frame {&frame-name}
                   emitente.tp-desp-padrao"}                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.tp-desp-padrao V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.tp-desp-padrao IN FRAME f-main /* Tp Desp Padr∆o */
DO:
  apply "f5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  if emitente.portador:load-mouse-pointer('image/lupa.cur') in frame {&frame-name} then.  
  if emitente.tp-desp-padrao:load-mouse-pointer('image/lupa.cur') in frame {&frame-name} then.   
  if emitente.cod-cond-pag:load-mouse-pointer('image/lupa.cur') in frame {&frame-name} then.
  if emitente.cod-banco:load-mouse-pointer('image/lupa.cur') in frame {&frame-name} then.  

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

  assign c-agencia:screen-value in frame {&frame-name} = "000000-00"
         c-conta-corren:screen-value in frame {&frame-name} = "0000000000-00".

  assign input frame {&FRAME-NAME} cb-pagto.


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

 {include/i-valid.i}

  assign input frame {&FRAME-NAME} cb-modalidade
         i-cb-modalidade = {adinc/i03ad209.i 06 cb-modalidade}.

 assign input frame {&FRAME-NAME} cb-pagto
        i-cb-pagto     = {adinc/i22ad098.i 06 cb-pagto}
        c-agencia      = input frame {&FRAME-NAME} c-agencia
        c-conta-corren = input frame {&FRAME-NAME} c-conta-corren.

 RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).

 if i-pais-impto-usuario = 1 then do:
    assign emitente.agencia = c-agencia +
           fill (" ", length(c-agencia:format in frame {&frame-name}) - 1 - length(c-agencia)) 

           emitente.conta-corren = c-conta-corren +
           fill (" ", length(c-conta-corren:format in frame {&frame-name}) - 1 - length(c-conta-corren)).
 end.

 assign emitente.tp-pagto = i-cb-pagto       
        modalidade   = i-cb-modalidade.

 run pi-versao (input 6).

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

  assign c-agencia:sensitive          in frame {&FRAME-NAME} = no
         c-conta-corren:sensitive     in frame {&FRAME-NAME} = no.

  /* Code placed here will execute AFTER standard behavior.    */
  &if  defined(ADM-MODIFY-FIELDS) &then
      disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
  &endif

  run pi-versao (input 3).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
    Purpose:     Override standard ADM method
    Notes:       
------------------------------------------------------------------------------*/
    if available emitente then do:
       find first banco 
            where banco.cod-banco = emitente.cod-banco 
            no-lock no-error.
       {utp/ut-liter.i Banco_n∆o_Cadastrado * L}
       assign c-nome-banco = if avail banco then banco.nome-banco else return-value.     

       find first tipo-rec-desp
            where tipo-rec-desp.tp-codigo = emitente.tp-desp-padrao
            no-lock no-error.
       assign c-desc-tipo = if avail tipo-rec-desp then tipo-rec-desp.descricao else "":U.

       find first cond-pagto
            where cond-pagto.cod-cond-pag = emitente.cod-cond-pag
            no-lock no-error.
       assign c-desc-cond = if avail cond-pagto then cond-pagto.descricao else "":U.     
    end.  

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .


    do with frame {&FRAME-NAME}:
       if  avail emitente then do:

          assign C-agencia:screen-value in frame {&FRAME-NAME} = emitente.agencia.
          assign c-conta-corren:screen-value in frame {&frame-name} = emitente.conta-corren.    
          if  emitente.tp-pagto = 0 then 
             assign cb-pagto:screen-value = {adinc/i22ad098.i 04 1}.
          else  
              assign cb-pagto:screen-value = {adinc/i22ad098.i 04 emitente.tp-pagto}.

          IF l-funcao-portador-ap = NO THEN DO:
              if  emitente.modalidade <> 0 then
                 assign cb-modalidade:screen-value = {adinc/i03ad209.i 04 emitente.modalidade}.
              else
                  assign cb-modalidade:screen-value = {adinc/i03ad209.i 04 1}.
          END.
          ELSE DO:
              if  &if "{&mgadm_version}" >= "2.06b" &then
                    emitente.modalidade-ap 
                  &else
                    int(SUBSTR(emitente.char-1,36,2)) 
                  &endif <> 0 THEN DO:
                  &if "{&mgadm_version}" >= "2.06b" &then
                     assign cb-modalidade:screen-value = {adinc/i03ad209.i 04 emitente.modalidade-ap}.
                  &else
                     assign cb-modalidade:screen-value = {adinc/i03ad209.i 04 int(SUBSTR(emitente.char-1,36,2))}.
                  &endif
              END.
              else
                  assign cb-modalidade:screen-value = {adinc/i03ad209.i 04 1}.

              &if "{&mgadm_version}" >= "2.06b" &then
                  ASSIGN emitente.portador:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(emitente.portador-ap).
              &else
                  ASSIGN emitente.portador:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(int(SUBSTR(emitente.char-1,31,5))).
              &endif
          END.
       end.
       else do:
            assign cb-pagto:screen-value      = {adinc/i22ad098.i 04 1}
                   cb-modalidade:screen-value = {adinc/i03ad209.i 04 1}.                 
       end.

        if i-pais-impto-usuario = 3 then
           assign cb-pagto:screen-value = {adinc/i22ad098.i 04 5}.

        run pi-versao (input 5).   

    end.  

    run pi-valida-formato (input "agencia").
    run pi-valida-formato (input "conta corrente").

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
  /* Dispatch standard ADM method.  
  */

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  assign c-agencia:sensitive          in frame {&FRAME-NAME} = yes
         c-conta-corren:sensitive     in frame {&FRAME-NAME} = yes.

  &if  defined(ADM-MODIFY-FIELDS) &then
      enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
  &endif

  run pi-versao (input 2).

 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  {utp/ut-field.i mgadm emitente agencia 1}
  assign c-agencia:label in frame {&frame-name} = return-value.

  {utp/ut-field.i mgadm emitente conta-corren 1}
  assign c-conta-corren:label in frame {&frame-name} = return-value.


  do  with frame {&frame-name}:
      assign cb-modalidade:list-items = {adinc/i03ad209.i 03}      
             cb-pagto:list-items      = {adinc/i22ad098.i 03}.
  end.


  {utp/ut-liter.i Tipo_Pagamento * L}
  assign cb-pagto:label in frame {&frame-name} = return-value.

  run pi-versao (input 1).  


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-formato V-table-Win 
PROCEDURE pi-valida-formato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param c-campo as char.

case c-campo:
   when "agencia" then do:
      if i-pais-impto-usuario = 1 then  do:
         if c-agencia:screen-value in frame {&frame-name} <> "" then
            assign c-agencia:format in frame {&FRAME-NAME}   = "999999-xx".
         else
            assign c-agencia:screen-value in frame {&FRAME-NAME}   = "000000-00".   
      end.      
      else 
         assign c-agencia:format in frame {&FRAME-NAME}   = "x(9)".
   end.
   when "conta corrente" then do:
      if i-pais-impto-usuario = 1 then 
         if c-conta-corren:screen-value in frame {&frame-name} <> "" then
            assign c-conta-corren:format in frame {&FRAME-NAME} = "9999999999-xx". 
         else
            assign c-conta-corren:screen-value in frame {&frame-name} = "0000000000-00".
      else         
            assign c-conta-corren:format in frame {&FRAME-NAME} = "x(20)".
   end.
end.
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

  {include/i-vldfrm.i}

  if c-agencia:screen-value in frame {&FRAME-NAME} <> "000000-00"  OR 
     c-conta-corren:screen-value in frame {&FRAME-NAME} <> "0000000000-00" then do:

     run pi-valida-formato ("agencia").
     assign c-formato-agencia = input frame {&FRAME-NAME} c-agencia no-error.

     if  ERROR-STATUS:ERROR then do:
         {include/i-vldprg.i}
         run utp/ut-msgs.p (input "show",
                            input 15543, 
                            input "").  
         apply 'entry' to c-agencia in frame {&FRAME-NAME}.
         return "adm-error":U.                   
     end.   

     run pi-valida-formato ("conta corrente").
     assign c-formato-conta = input frame {&FRAME-NAME} c-conta-corren no-error.

     if  ERROR-STATUS:ERROR then do:
         {include/i-vldprg.i}
         run utp/ut-msgs.p (input "show",
                            input 15543, 
                            input "").
         apply 'entry' to c-conta-corren in frame {&FRAME-NAME}.
         return "adm-error":U.                   
     end.   

  end. 

  assign input frame {&FRAME-NAME} cb-modalidade
         i-modalidade = {adinc/i03ad209.i 06 cb-modalidade}.  

  if  input frame {&FRAME-NAME} emitente.portador <> 0 then do:
      find first portador
           where portador.ep-codigo    = i-ep-codigo-usuario
           and   portador.cod-portador = input frame {&FRAME-NAME} emitente.portador
           and   portador.modalidade   = i-modalidade
           no-lock no-error.

      if  not avail portador then do:
          {include/i-vldprg.i}
          run utp/ut-msgs.p (input "show":U, 
                             input 927, 
                             input string(input frame {&FRAME-NAME} emitente.portador) + "~~" +
                                   string(i-modalidade) + "~~" +
                                   string(i-ep-codigo-usuario)).
          apply 'entry' to emitente.portador in frame {&FRAME-NAME}.
          return 'ADM-ERROR':U.
      end.

      if  portador.bancario = yes and i-modalidade <> 1 then do:
          run utp/ut-msgs.p (input "show":U, 
                             input 1867, 
                             input "").
          apply 'entry' to emitente.portador in frame {&FRAME-NAME}.
          return 'ADM-ERROR':U.
      end.

  end.

  /* valida se taxa financeira Ç nulo */
  if input frame {&FRAME-NAME} emitente.taxa-finan <= 0 and 
     input frame {&FRAME-NAME} emitente.taxa-finan >= 100 then do:
    {include/i-vldprg.i}
    run utp/ut-msgs.p (input "show":U, input 129, input '').
    apply 'entry' to emitente.taxa-finan in frame {&FRAME-NAME}.
    return 'ADM-ERROR':U.
  end.

  /* valida tipo de despesa se for diferente de 0 */
  if input frame {&FRAME-NAME} emitente.tp-desp-padrao <> 0 then do:
     find first tipo-rec-desp
          where tipo-rec-desp.tp-codigo = input frame {&FRAME-NAME} emitente.tp-desp-padrao
          and   tipo-rec-desp.tipo      = 2
          no-lock no-error.
     if not avail tipo-rec-desp then do:
        {include/i-vldprg.i}
        run utp/ut-msgs.p (input "show":U, input 3866, input '').
        apply 'entry' to emitente.tp-desp-padrao in frame {&FRAME-NAME}.
        return 'ADM-ERROR':U.
     end.
  end.   

  /* valida condicao de pagto se for diferente de 0 */
  if input frame {&FRAME-NAME} emitente.cod-cond-pag <> 0 then do:
     find first cond-pagto
          where cond-pagto.cod-cond-pag = input frame {&FRAME-NAME} emitente.cod-cond-pag
          no-lock no-error.
     if not avail cond-pagto then do:
        {include/i-vldprg.i}
        run utp/ut-msgs.p (input "show":U, input 1088, input '').
        apply 'entry' to emitente.cod-cond-pag in frame {&FRAME-NAME}.
        return 'ADM-ERROR':U.
     end.
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
    def input param p-ind as int        no-undo.

    &if "{&mgadm_version}" >= "2.02" &then
        case p-ind:
            when 1 then do: /* initialize */
                assign bt-varias-contas:visible     in frame {&FRAME-NAME} = yes
                       rt-varias:visible            in frame {&FRAME-NAME} = yes 
                       bt-varias-contas:sensitive   in frame {&FRAME-NAME} = yes.
            end.
            when 2 then do: /* enable */
                assign emitente.cod-banco:sensitive in frame {&FRAME-NAME} = no
                       c-agencia:sensitive          in frame {&FRAME-NAME} = no
                       c-conta-corren:sensitive     in frame {&FRAME-NAME} = no
                       bt-varias-contas:sensitive   in frame {&FRAME-NAME} = yes.
            end.
            when 3 then do: /* disable */
                assign bt-varias-contas:sensitive   in frame {&FRAME-NAME} = no.

                for each tt-cta-emitente:
                    delete tt-cta-emitente.
                end.
            end.
            when 4 then do: /* on choose bt-varias-contas */
                for each tt-cta-emitente:
                    delete tt-cta-emitente.
                end.
                if avail emitente then do:
                    find first tt-cta-emitente no-lock
                        where tt-cta-emitente.cod-emitente = emitente.cod-emitente no-error.
                    if  not avail tt-cta-emitente then do:  
                        if  avail emitente then do:
                            for each cta-emitente
                               where cta-emitente.cod-emitente = emitente.cod-emitente no-lock:
                                 create tt-cta-emitente.
                                 assign tt-cta-emitente.cod-emitente   = cta-emitente.cod-emitente
                                        tt-cta-emitente.cod-banco      = cta-emitente.cod-banco
                                        tt-cta-emitente.agencia        = cta-emitente.agencia
                                        tt-cta-emitente.conta-corrente = cta-emitente.conta-corrente
                                        tt-cta-emitente.descricao      = cta-emitente.descricao
                                        tt-cta-emitente.preferencial   = cta-emitente.preferencial.
                                 find first banco
                                    where banco.cod-banco = tt-cta-emitente.cod-banco no-lock no-error.
                                 if avail banco then
                                    assign tt-cta-emitente.nome = banco.nome-banco.        
                            end.
                        end.
                    end.
                end.

                run  cdp/cd0401f.w (input-output table tt-cta-emitente).

                find first tt-cta-emitente 
                    where tt-cta-emitente.preferencial = yes no-lock no-error.
                if  avail tt-cta-emitente then do:
                    assign emitente.cod-banco:screen-value in frame {&FRAME-NAME} = string(tt-cta-emitente.cod-banco)
                           c-agencia:screen-value          in frame {&FRAME-NAME} = tt-cta-emitente.agencia
                           c-conta-corren:screen-value     in frame {&FRAME-NAME} = tt-cta-emitente.conta-corrente.

                    apply 'leave' to emitente.cod-banco in frame {&FRAME-NAME}.
                end.
            end.
            when 5 then do: /* display */
                if  avail emitente then do:
                    find first cta-emitente
                        where cta-emitente.cod-emitente = emitente.cod-emitente
                        and   cta-emitente.preferencial = yes no-lock no-error.
                    if  avail cta-emitente then
                        assign emitente.cod-banco:screen-value in frame {&FRAME-NAME} = string(cta-emitente.cod-banco)
                               c-agencia:screen-value          in frame {&FRAME-NAME} = cta-emitente.agencia
                               c-conta-corren:screen-value     in frame {&FRAME-NAME} = cta-emitente.conta-corrente.
                end.    
            end.
            when 6 then do: /* assign */
                if  adm-new-record = yes then do:
                    for each tt-cta-emitente :
                        assign tt-cta-emitente.cod-emitente = emitente.cod-emitente.
                        create cta-emitente.
                        assign cta-emitente.cod-emitente   = tt-cta-emitente.cod-emitente
                               cta-emitente.cod-banco      = tt-cta-emitente.cod-banco
                               cta-emitente.agencia        = tt-cta-emitente.agencia
                               cta-emitente.conta-corrente = tt-cta-emitente.conta-corrente
                               cta-emitente.descricao      = tt-cta-emitente.descricao
                               cta-emitente.preferencial   = tt-cta-emitente.preferencial.
                    end.
                end.
                else do:
                   for each cta-emitente
                        where cta-emitente.cod-emitente = emitente.cod-emitente exclusive-lock:
                        delete cta-emitente.
                    end.
                    for each tt-cta-emitente :
                        create cta-emitente.
                        assign cta-emitente.cod-emitente   = emitente.cod-emitente
                               cta-emitente.cod-banco      = tt-cta-emitente.cod-banco
                               cta-emitente.agencia        = tt-cta-emitente.agencia
                               cta-emitente.conta-corrente = tt-cta-emitente.conta-corrente
                               cta-emitente.descricao      = tt-cta-emitente.descricao
                               cta-emitente.preferencial   = tt-cta-emitente.preferencial.
                    end.
                end.
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

