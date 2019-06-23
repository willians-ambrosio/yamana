&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
def buffer pais       for ems2cadme.pais.
def buffer unid-feder for ems2cadme.unid-feder.

{include/i-prgvrs.i AP0804-V04 2.00.00.003}  /*** 010003 ***/

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
&glob ORIGINALNAME 'advwr\v05ad098.w'

/* global variable definitions */
{cdp/cdcfgfin.i}
/* Parameters Definitions ---                                           */


/* Local Variable Definitions ---                                       */

{include/i_dbvers.i}

def var v-row-parent as rowid no-undo.
def var c-cep like emitente.cep no-undo.
def shared var i-natureza  as int  no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS emitente.endereco emitente.endereco2 ~
emitente.bairro emitente.cep emitente.cidade emitente.estado emitente.pais ~
emitente.caixa-postal 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}endereco ~{&FP2}endereco ~{&FP3}~
 ~{&FP1}endereco2 ~{&FP2}endereco2 ~{&FP3}~
 ~{&FP1}bairro ~{&FP2}bairro ~{&FP3}~
 ~{&FP1}cep ~{&FP2}cep ~{&FP3}~
 ~{&FP1}cidade ~{&FP2}cidade ~{&FP3}~
 ~{&FP1}estado ~{&FP2}estado ~{&FP3}~
 ~{&FP1}pais ~{&FP2}pais ~{&FP3}~
 ~{&FP1}caixa-postal ~{&FP2}caixa-postal ~{&FP3}
&Scoped-define ENABLED-TABLES emitente
&Scoped-define FIRST-ENABLED-TABLE emitente
&Scoped-Define ENABLED-OBJECTS t-end-compl 
&Scoped-Define DISPLAYED-FIELDS emitente.endereco emitente.endereco2 ~
emitente.bairro emitente.cep emitente.cidade emitente.estado emitente.pais ~
emitente.caixa-postal 
&Scoped-Define DISPLAYED-OBJECTS end-completo t-end-compl 

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
DEFINE VARIABLE end-completo AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 63.29 BY 2.88 NO-UNDO.

DEFINE VARIABLE t-end-compl AS CHARACTER FORMAT "x(15)":U INITIAL "End. Completo" 
      VIEW-AS TEXT 
     SIZE 10.72 BY .67 TOOLTIP "Endereáo Completo" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     end-completo AT ROW 1.21 COL 20.14 NO-LABEL
     emitente.endereco AT ROW 4.25 COL 18.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 41.43 BY .88
     emitente.endereco2 AT ROW 5.25 COL 18.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 41.43 BY .88
     emitente.bairro AT ROW 6.25 COL 18.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26.43 BY .88
     emitente.cep AT ROW 6.25 COL 66 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.72 BY .88
     emitente.cidade AT ROW 7.25 COL 18.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26.43 BY .88
     emitente.estado AT ROW 7.25 COL 66.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .88
     emitente.pais AT ROW 8.25 COL 18.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.43 BY .88
     emitente.caixa-postal AT ROW 8.25 COL 66 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .88
     t-end-compl AT ROW 1.29 COL 7.14 COLON-ALIGNED NO-LABEL
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
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 8.5
         WIDTH              = 85.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR end-completo IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME emitente.cep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cep V-table-Win
ON ENTRY OF emitente.cep IN FRAME f-main /* CEP */
DO:              
     assign emitente.cep:format in frame {&FRAME-NAME} = param-global.formato-cep.                                                      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cep V-table-Win
ON LEAVE OF emitente.cep IN FRAME f-main /* CEP */
DO:
  assign c-cep = input frame {&FRAME-NAME} emitente.cep no-error.
  if  ERROR-STATUS:get-number(1) <> 0 or
      input frame {&FRAME-NAME} emitente.cep = "" then
      assign emitente.cep:format in frame {&FRAME-NAME} = "x(10)".
  else
      assign emitente.cep:format in frame {&FRAME-NAME} = param-global.formato-cep.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.estado V-table-Win
ON F5 OF emitente.estado IN FRAME f-main /* UF */
DO:
  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="unzoom/z01un007.w"
                     &campo=emitente.estado
                     &campozoom=estado}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.estado V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.estado IN FRAME f-main /* UF */
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.pais
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.pais V-table-Win
ON F5 OF emitente.pais IN FRAME f-main /* Pa°s */
DO:
  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="unzoom/z01un006.w"
                     &campo=emitente.pais
                     &campozoom=nome-pais}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.pais V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.pais IN FRAME f-main /* Pa°s */
DO:
  apply 'f5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  if emitente.pais:load-mouse-pointer('image/lupa.cur') then.
  if emitente.estado:load-mouse-pointer('image/lupa.cur') then.
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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

  &if defined(bf_fin_4linhas_end) &then
      assign end-completo:screen-value in frame {&frame-name} = emitente.endereco_text.
  &endif

  
  apply 'leave' to mgadm.emitente.cep in frame {&FRAME-NAME}.

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

  if adm-new-record = yes then
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
    
  assign emitente.cep:format in frame {&FRAME-NAME} = param-global.formato-cep.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  
  &if defined(bf_fin_4linhas_end) &then
      assign end-completo:visible in frame {&frame-name} = yes
             t-end-compl:visible in frame {&frame-name} = yes.
  &else
      assign end-completo:visible in frame {&frame-name} = no
             t-end-compl:visible in frame {&frame-name} = no.
  &endif
  
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
    
  if  i-natureza <> 3 
  and i-natureza <> 4 then do: 

      if   input frame {&FRAME-NAME} emitente.cep = "" then do:
          {include/i-vldprg.i}
          run utp/ut-msgs.p (input "show",
                             input 192,
                             input "").
          apply 'entry' to emitente.cep in frame {&frame-name}.
          return 'ADM-ERROR':U.
      end.
      
      assign emitente.cep:format in frame {&FRAME-NAME} = param-global.formato-cep
             c-cep = input frame {&FRAME-NAME} emitente.cep no-error.
                  
      if  ERROR-STATUS:get-number(1) <> 0 then do:
          {include/i-vldprg.i}
          run utp/ut-msgs.p (input "show",
                             input 4334 /*2744*/ ,
                             input "").
          apply 'entry' to emitente.cep in frame {&frame-name}.
          return 'ADM-ERROR':U.
      end.   
  end.      
            
  find first pais 
       where pais.nome-pais = input frame {&FRAME-NAME} emitente.pais 
       no-lock no-error.
  if not avail pais then do:
     {include/i-vldprg.i}
     run utp/ut-msgs.p (input "show",
                        input  964,
                        input "").
     apply "ENTRY" to emitente.pais.
     return "ADM-ERROR":U.                            
  end.
  
  find first unid-feder 
       where unid-feder.pais   = input frame {&FRAME-NAME} emitente.pais
       and   unid-feder.estado = input frame {&FRAME-NAME} emitente.estado
       no-lock no-error.
  if not avail unid-feder then do:
     {include/i-vldprg.i}
     run utp/ut-msgs.p (input "show",
                        input  965,
                        input "").
     apply "ENTRY" to emitente.estado.
     return "ADM-ERROR":U.                             
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
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


