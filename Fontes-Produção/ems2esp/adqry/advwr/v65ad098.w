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
def buffer unid-feder for ems2cadme.unid-feder.
def buffer pais       for ems2cadme.pais.

{include/i-prgvrs.i V65AD098 2.00.00.008}  /*** 010008 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i V65AD098 MUT}
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
&glob ORIGINALNAME 'advwr\v28ad098.w'
{cdp/cdcfgfin.i}
/* {utp/ut-glob.i} */
{include/i_dbvers.i}

def var c-nome-emit         as char     no-undo.
def var i-cod-emitente      as int      no-undo.
def var c-formato-cep       as char     no-undo.
def var c-formato-cep-cobr  as char     no-undo.

define buffer b-emitente for emitente.

def var c-endereco-ant     like emitente.endereco no-undo.
def var c-bairro-ant       like emitente.bairro no-undo.
def var c-cidade-ant       like emitente.cidade no-undo.
def var c-estado-ant       like emitente.estado no-undo.
def var c-pais-ant         like emitente.pais no-undo.
def var c-cep-ant          like emitente.cep no-undo.
def var c-caixa-postal-ant like emitente.caixa-postal no-undo.

def new global shared var l-altera     as log  initial no no-undo.
def new global shared var l-cria-ambos as log  initial no no-undo.

def var h_v27ad098     as handle no-undo.
def var h_v29ad098     as handle no-undo.                  
def var c-cgc          like emitente.cgc no-undo.
def var c-ins-estadual like emitente.ins-estadual.
def var c-e-mail       like emitente.e-mail.
def var i-natureza     like emitente.natureza.

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
&Scoped-Define ENABLED-FIELDS emitente.pais emitente.endereco emitente.cep ~
emitente.bairro emitente.caixa-postal emitente.cidade emitente.estado ~
emitente.end-cobranca emitente.estado-cob emitente.endereco-cob ~
emitente.pais-cob emitente.cep-cob emitente.bairro-cob emitente.cx-post-cob ~
emitente.cidade-cob 
&Scoped-define ENABLED-TABLES emitente
&Scoped-define FIRST-ENABLED-TABLE emitente
&Scoped-Define ENABLED-OBJECTS RECT-26 RECT-27 end-completo ~
end-completo-cob t-end-compl t-end-compl-cob 
&Scoped-Define DISPLAYED-FIELDS emitente.pais emitente.endereco ~
emitente.cep emitente.bairro emitente.caixa-postal emitente.cidade ~
emitente.estado emitente.end-cobranca emitente.estado-cob ~
emitente.endereco-cob emitente.pais-cob emitente.cep-cob ~
emitente.bairro-cob emitente.cx-post-cob emitente.cidade-cob 
&Scoped-define DISPLAYED-TABLES emitente
&Scoped-define FIRST-DISPLAYED-TABLE emitente
&Scoped-Define DISPLAYED-OBJECTS end-completo end-completo-cob ~
c-desc-emitente t-end-compl t-end-compl-cob 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,ENDERECO-COB,List-5,List-6 */
&Scoped-define ADM-ASSIGN-FIELDS emitente.bairro-cob emitente.cidade-cob 
&Scoped-define ENDERECO-COB emitente.estado-cob emitente.endereco-cob ~
emitente.pais-cob emitente.cep-cob emitente.bairro-cob emitente.cx-post-cob ~
emitente.cidade-cob 

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
     VIEW-AS EDITOR MAX-CHARS 2000 SCROLLBAR-VERTICAL
     SIZE 72.29 BY 1.96.

DEFINE VARIABLE end-completo-cob AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 2000 SCROLLBAR-VERTICAL
     SIZE 65.29 BY 1.96 NO-UNDO.

DEFINE VARIABLE c-desc-emitente AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY .88 NO-UNDO.

DEFINE VARIABLE t-end-compl AS CHARACTER FORMAT "X(15)":U 
     LABEL "End. Compl." 
      VIEW-AS TEXT 
     SIZE 1 BY .67 NO-UNDO.

DEFINE VARIABLE t-end-compl-cob AS CHARACTER FORMAT "X(15)":U 
     LABEL "End. Compl. Cob." 
      VIEW-AS TEXT 
     SIZE .86 BY .67 NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87.57 BY 5.38.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87.72 BY 6.54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     end-completo AT ROW 1.29 COL 15.86 NO-LABEL
     emitente.pais AT ROW 3.29 COL 70.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .88
     emitente.endereco AT ROW 3.33 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 37.57 BY .88
     emitente.cep AT ROW 4.29 COL 70.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .88
     emitente.bairro AT ROW 4.33 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 31 BY .88
     emitente.caixa-postal AT ROW 5.29 COL 70.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .88
     emitente.cidade AT ROW 5.33 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 31 BY .88
     emitente.estado AT ROW 5.33 COL 50.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .88
     end-completo-cob AT ROW 6.79 COL 22.72 NO-LABEL
     emitente.end-cobranca AT ROW 8.92 COL 20.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     c-desc-emitente AT ROW 8.92 COL 30.29 COLON-ALIGNED NO-LABEL
     emitente.estado-cob AT ROW 8.92 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .88
     emitente.endereco-cob AT ROW 9.92 COL 20.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 37.57 BY .88
     emitente.pais-cob AT ROW 9.92 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .88
     emitente.cep-cob AT ROW 10.92 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .88
     emitente.bairro-cob AT ROW 10.96 COL 20.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 31 BY .88
     emitente.cx-post-cob AT ROW 11.92 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .88
     emitente.cidade-cob AT ROW 12 COL 20.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26.14 BY .88
     t-end-compl AT ROW 1.38 COL 13 COLON-ALIGNED
     t-end-compl-cob AT ROW 6.88 COL 19.72 COLON-ALIGNED
     RECT-26 AT ROW 1.04 COL 1.43
     RECT-27 AT ROW 6.46 COL 1.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE NO-VALIDATE THREE-D 
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
         HEIGHT             = 12
         WIDTH              = 88.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

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

/* SETTINGS FOR FILL-IN emitente.bairro-cob IN FRAME f-main
   2 4                                                                  */
/* SETTINGS FOR FILL-IN c-desc-emitente IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN emitente.cep-cob IN FRAME f-main
   4                                                                    */
/* SETTINGS FOR FILL-IN emitente.cidade-cob IN FRAME f-main
   2 4                                                                  */
/* SETTINGS FOR FILL-IN emitente.cx-post-cob IN FRAME f-main
   4                                                                    */
ASSIGN 
       end-completo:READ-ONLY IN FRAME f-main        = TRUE.

ASSIGN 
       end-completo-cob:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR FILL-IN emitente.endereco-cob IN FRAME f-main
   4                                                                    */
/* SETTINGS FOR FILL-IN emitente.estado-cob IN FRAME f-main
   4                                                                    */
/* SETTINGS FOR FILL-IN emitente.pais-cob IN FRAME f-main
   4                                                                    */
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

&Scoped-define SELF-NAME emitente.cep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cep V-table-Win
ON ENTRY OF emitente.cep IN FRAME f-main /* CEP */
DO:
  find first param-global no-lock no-error.
  if avail param-global then
     assign emitente.cep:format     in frame {&frame-name} = param-global.formato-cep.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cep V-table-Win
ON LEAVE OF emitente.cep IN FRAME f-main /* CEP */
DO:
   assign c-formato-cep = input frame {&FRAME-NAME} emitente.cep no-error.
   if  ERROR-STATUS:ERROR then do:
       assign emitente.cep:format in frame {&FRAME-NAME} = "x(12)".
   end.
   else do:
       apply 'entry':U to emitente.cep in frame {&FRAME-NAME}.
   end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.cep-cob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cep-cob V-table-Win
ON ENTRY OF emitente.cep-cob IN FRAME f-main /* CEP */
DO:
  find first param-global no-lock no-error.
  if avail param-global then
     assign emitente.cep-cob:format in frame {&frame-name} = param-global.formato-cep.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.cep-cob V-table-Win
ON LEAVE OF emitente.cep-cob IN FRAME f-main /* CEP */
DO:
   assign c-formato-cep-cobr = input frame {&FRAME-NAME} emitente.cep-cob no-error.
   if  ERROR-STATUS:ERROR then do:
       assign emitente.cep-cob:format in frame {&FRAME-NAME} = "x(12)".
   end.
   else do:
       apply 'entry':U to emitente.cep-cob in frame {&FRAME-NAME}.
   end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.end-cobranca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.end-cobranca V-table-Win
ON F5 OF emitente.end-cobranca IN FRAME f-main /* Cliente Cobranáa */
DO:

  assign l-implanta = no.
  {include/zoomvar.i &prog-zoom="adzoom\z02ad098.w"
                     &campo=emitente.end-cobranca
                     &campozoom=cod-emitente}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.end-cobranca V-table-Win
ON LEAVE OF emitente.end-cobranca IN FRAME f-main /* Cliente Cobranáa */
DO:

     find first b-emitente no-lock
          where b-emitente.cod-emitente = input frame {&FRAME-NAME} emitente.end-cobranca 
          and   b-emitente.identific   <> 2 no-error.

     if  avail b-emitente then do:
         assign c-desc-emitente:screen-value in frame {&FRAME-NAME} = b-emitente.nome-emit.
     end.
     else do:
         assign c-desc-emitente:screen-value in frame {&FRAME-NAME} = "".
     end.

     RUN Get-Field-Screen-Value IN adm-broker-hdl (INPUT this-procedure, INPUT "cod-emitente").
     if avail emitente then do:
        if  input frame {&FRAME-NAME} emitente.end-cobranca <> 0 and
            emitente.end-cobranca:screen-value in frame {&frame-name} <> return-value then do with frame {&frame-name}:
            &if  defined(ENDERECO-COB) &then
                 disable {&ENDERECO-COB} with frame {&frame-name}.
            &endif
            assign emitente.endereco-cob:screen-value = " "
                   emitente.bairro-cob:screen-value = " "
                   emitente.cidade-cob:screen-value = " "
                   emitente.estado-cob:screen-value = " "
                   emitente.pais-cob:screen-value = " "
                   emitente.cep-cob:screen-value = " "
                   emitente.cx-post-cob:screen-value = " ".
        end.
        else do:
            assign emitente.endereco-cob:screen-value = emitente.endereco-cob
                   emitente.bairro-cob:screen-value = emitente.bairro-cob
                   emitente.cidade-cob:screen-value = emitente.cidade-cob
                   emitente.estado-cob:screen-value = emitente.estado-cob
                   emitente.pais-cob:screen-value = emitente.pais-cob
                   emitente.cep-cob:screen-value = emitente.cep-cob
                   emitente.cx-post-cob:screen-value = emitente.cx-post-cob.
            if  input frame {&FRAME-NAME} emitente.end-cobranca = 0 then
                assign c-desc-emitente:screen-value in frame {&FRAME-NAME} = "".
            &if  defined(ENDERECO-COB) &then
                 enable {&ENDERECO-COB} with frame {&frame-name}.
            &endif
        end.
     end.    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.end-cobranca V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.end-cobranca IN FRAME f-main /* Cliente Cobranáa */
DO:

  apply "F5" to self.

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
ON LEAVE OF emitente.estado IN FRAME f-main /* UF */
DO:

    FIND FIRST UNID-FEDER 
         WHERE UNID-FEDER.estado = input frame {&FRAME-NAME} EMITENTE.estado NO-LOCK NO-ERROR.

    if  avail unid-feder then do:
        assign emitente.pais:screen-value in frame {&FRAME-NAME} = unid-feder.pais.
    end.
    else do:
        assign emitente.pais:screen-value in frame {&FRAME-NAME} = "".
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.estado V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.estado IN FRAME f-main /* UF */
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emitente.estado-cob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.estado-cob V-table-Win
ON F5 OF emitente.estado-cob IN FRAME f-main /* UF */
DO:

   assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="unzoom/z01un007.w"
                     &campo=emitente.estado-cob
                     &campozoom=estado}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.estado-cob V-table-Win
ON LEAVE OF emitente.estado-cob IN FRAME f-main /* UF */
DO:

    FIND FIRST UNID-FEDER 
         WHERE UNID-FEDER.estado = input frame {&FRAME-NAME} EMITENTE.estado-cob NO-LOCK NO-ERROR.

    if  avail unid-feder then do:
        assign emitente.pais-cob:screen-value in frame {&FRAME-NAME} = unid-feder.pais.
    end.
    else do:
        assign emitente.pais-cob:screen-value in frame {&FRAME-NAME} = "".
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.estado-cob V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.estado-cob IN FRAME f-main /* UF */
DO:
  apply "F5" to self.
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


&Scoped-define SELF-NAME emitente.pais-cob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.pais-cob V-table-Win
ON F5 OF emitente.pais-cob IN FRAME f-main /* Pa°s */
DO:

  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="unzoom/z01un006.w"
                     &campo=emitente.pais-cob
                     &campozoom=nome-pais}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emitente.pais-cob V-table-Win
ON MOUSE-SELECT-DBLCLICK OF emitente.pais-cob IN FRAME f-main /* Pa°s */
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  if emitente.pais:load-mouse-pointer         ("image\lupa.cur") in frame {&frame-name} then.
  if emitente.estado:load-mouse-pointer       ("image\lupa.cur") in frame {&frame-name} then.
  if emitente.pais-cob:load-mouse-pointer     ("image\lupa.cur") in frame {&frame-name} then.
  if emitente.estado-cob:load-mouse-pointer   ("image\lupa.cur") in frame {&frame-name} then.  
  if emitente.end-cobranca:load-mouse-pointer ("image\lupa.cur") in frame {&frame-name} then.  

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
  apply 'leave' to emitente.end-cobranca in frame {&frame-name}.


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

  apply 'leave' to emitente.end-cobranca in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
    Purpose:     Override standard ADM method
    Notes:       
------------------------------------------------------------------------------*/

    if return-value = "adm-error":U then
       return "adm-error":U.      

    assign c-endereco-ant     = emitente.endereco
           c-bairro-ant       = emitente.bairro
           c-cidade-ant       = emitente.cidade
           c-estado-ant       = emitente.estado
           c-pais-ant         = emitente.pais
           c-cep-ant          = emitente.cep
           c-caixa-postal-ant = emitente.caixa-postal.

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
       return 'ADM-ERROR':U.

    {utp/ut-liter.i Padr∆o}  
    assign emitente.cod-entrega = trim(return-value).   

    run get-attribute ('adm-new-record').
    if  return-value = "yes" then do: 
        create loc-entr.   
        assign loc-entr.nome-abrev   = emitente.nome-abrev
               loc-entr.cod-entrega  = emitente.cod-entrega
               loc-entr.endereco     = emitente.endereco
               loc-entr.bairro       = emitente.bairro
               loc-entr.cidade       = emitente.cidade
               loc-entr.estado       = emitente.estado
               loc-entr.cep          = emitente.cep
               loc-entr.caixa-postal = emitente.caixa-postal
               loc-entr.pais         = emitente.pais
               loc-entr.cgc          = c-cgc
               loc-entr.ins-estadual = c-ins-estadual
               loc-entr.zip-code     = emitente.zip-code
               loc-entr.e-mail       = c-e-mail.
        end.
    else do:        

        if  c-endereco-ant     <> emitente.endereco      or
            c-bairro-ant       <> emitente.bairro        or
            c-cidade-ant       <> emitente.cidade        or
            c-estado-ant       <> emitente.estado        or
            c-pais-ant         <> emitente.pais          or
            c-cep-ant          <> emitente.cep           or
            c-caixa-postal-ant <> emitente.caixa-postal  then do:

            find first loc-entr use-index ch-entrega
                 where loc-entr.nome-abrev  = emitente.nome-abrev
                 and   loc-entr.cod-entrega = emitente.cod-entrega  exclusive-lock no-error.

            if  avail loc-entr then do:

                RUN utp/ut-msgs.p (input "show",
                                   input 17255,
                                   input "").

                if  return-value = 'yes' then do:
                    assign loc-entr.endereco     = emitente.endereco
                           loc-entr.bairro       = emitente.bairro
                           loc-entr.cidade       = emitente.cidade
                           loc-entr.estado       = emitente.estado
                           loc-entr.cep          = emitente.cep
                           loc-entr.caixa-postal = emitente.caixa-postal
                           loc-entr.pais         = emitente.pais
                           loc-entr.cgc          = c-cgc
                           loc-entr.ins-estadual = c-ins-estadual
                           loc-entr.zip-code     = emitente.zip-code
                           loc-entr.e-mail       = c-e-mail
                           l-altera              = yes.
                end.
            end.
            else do:
                create loc-entr.   
                assign loc-entr.nome-abrev   = emitente.nome-abrev
                       loc-entr.cod-entrega  = emitente.cod-entrega
                       loc-entr.endereco     = emitente.endereco
                       loc-entr.bairro       = emitente.bairro
                       loc-entr.cidade       = emitente.cidade
                       loc-entr.estado       = emitente.estado
                       loc-entr.cep          = emitente.cep
                       loc-entr.caixa-postal = emitente.caixa-postal
                       loc-entr.pais         = emitente.pais
                       loc-entr.cgc          = c-cgc
                       loc-entr.ins-estadual = c-ins-estadual
                       loc-entr.zip-code     = emitente.zip-code
                       loc-entr.e-mail       = c-e-mail
                       l-cria-ambos          = yes
                       l-altera              = no.
            end.
        end.     
        else do:
            find first loc-entr use-index ch-entrega
                where loc-entr.nome-abrev  = emitente.nome-abrev
                and   loc-entr.cod-entrega = emitente.cod-entrega exclusive-lock no-error.
            if not avail loc-entr then do:    
              create loc-entr.   
              assign loc-entr.nome-abrev   = emitente.nome-abrev
                     loc-entr.cod-entrega  = emitente.cod-entrega
                     loc-entr.endereco     = emitente.endereco
                     loc-entr.bairro       = emitente.bairro
                     loc-entr.cidade       = emitente.cidade
                     loc-entr.estado       = emitente.estado
                     loc-entr.cep          = emitente.cep
                     loc-entr.caixa-postal = emitente.caixa-postal
                     loc-entr.pais         = emitente.pais
                     loc-entr.cgc          = c-cgc
                     loc-entr.ins-estadual = c-ins-estadual
                     loc-entr.zip-code     = emitente.zip-code
                     loc-entr.e-mail       = c-e-mail
                     l-cria-ambos          = yes
                     l-altera              = no.
            end.
        end.    
        assign loc-entr.cgc          = c-cgc
               loc-entr.ins-estadual = c-ins-estadual
               loc-entr.zip-code     = emitente.zip-code
               loc-entr.e-mail       = emitente.e-mail.
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

  if  avail emitente then do:
      find first b-emitente no-lock
           where b-emitente.cod-emitente = emitente.end-cobranca no-error.

      if  avail b-emitente then do:
          assign c-desc-emitente:screen-value in frame {&FRAME-NAME} = b-emitente.nome-emit.
      end.
      else do:
          assign c-desc-emitente:screen-value in frame {&FRAME-NAME} = "".
      end.
     RUN Get-Field-Screen-Value IN adm-broker-hdl (INPUT this-procedure, INPUT "cod-emitente").
     if  input frame {&FRAME-NAME} emitente.end-cobranca <> 0 and
         emitente.end-cobranca:screen-value in frame {&frame-name} <> return-value then do with frame {&frame-name}:
         assign emitente.endereco-cob:screen-value = b-emitente.endereco-cob
                emitente.bairro-cob:screen-value = b-emitente.bairro-cob
                emitente.cidade-cob:screen-value = b-emitente.cidade-cob
                emitente.estado-cob:screen-value = b-emitente.estado-cob
                emitente.pais-cob:screen-value = b-emitente.pais-cob
                emitente.cep-cob:screen-value = b-emitente.cep-cob
                emitente.cx-post-cob:screen-value = b-emitente.cx-post-cob.

         &if defined(bf_fin_4linhas_end) &then
            assign end-completo:screen-value in frame {&frame-name} = emitente.endereco_text
                   end-completo-cob:screen-value in frame {&frame-name}  = b-emitente.endereco-cob-text.
         &endif       

     end.    
     else do:
         assign emitente.endereco-cob:screen-value = emitente.endereco-cob
                emitente.bairro-cob:screen-value = emitente.bairro-cob
                emitente.cidade-cob:screen-value = emitente.cidade-cob
                emitente.estado-cob:screen-value = emitente.estado-cob
                emitente.pais-cob:screen-value = emitente.pais-cob
                emitente.cep-cob:screen-value = emitente.cep-cob
                emitente.cx-post-cob:screen-value = emitente.cx-post-cob.

         &if defined(bf_fin_4linhas_end) &then
            assign end-completo:screen-value in frame {&frame-name} = emitente.endereco_text
                   end-completo-cob:screen-value in frame {&frame-name}  = emitente.endereco-cob-text.
         &endif       

         if  input frame {&FRAME-NAME} emitente.end-cobranca = 0 then
             assign c-desc-emitente:screen-value in frame {&FRAME-NAME} = "".
     end.
  end.    

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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

   &if  defined(bf_fin_4linhas_end) &then
       assign end-completo:visible in frame {&frame-name} = yes
              end-completo-cob:visible in frame {&frame-name} = yes
              t-end-compl:visible in frame {&frame-name} = yes
              t-end-compl-cob:visible in frame {&frame-name} = yes.
   &else
       assign end-completo:visible in frame {&frame-name} = no
              end-completo-cob:visible in frame {&frame-name} = no
              t-end-compl:visible in frame {&frame-name} = no
              t-end-compl-cob:visible in frame {&frame-name} = no.
   &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-codigo-emitente V-table-Win 
PROCEDURE pi-codigo-emitente :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
------------------------------------------------------------------------------*/

    def input param p-end-cobranca as integer no-undo.     

    assign i-cod-emitente = p-end-cobranca.

    assign emitente.end-cobranca:screen-value in frame {&FRAME-NAME} = string(i-cod-emitente).
    apply 'leave' to emitente.end-cobranca in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-nome-emitente V-table-Win 
PROCEDURE pi-nome-emitente :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
------------------------------------------------------------------------------*/

    def input param p-nome-emit as char no-undo.  

    assign c-desc-emitente:screen-value in frame {&FRAME-NAME} = p-nome-emit.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-recebe-handle-comunic V-table-Win 
PROCEDURE pi-recebe-handle-comunic :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param h_handle as handle no-undo.

assign h_v29ad098 = h_handle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-recebe-handle-ender V-table-Win 
PROCEDURE pi-recebe-handle-ender :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param h_handle as handle no-undo.

assign h_v27ad098 = h_handle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate V-table-Win 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
   Purpose:Validar a viewer     
   Parameters:  <none>
   Notes: N∆o fazer assign aqui. Nesta procedure
   devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
   ainda n∆o foi criado.       
------------------------------------------------------------------------------*/

   {include/i-vldfrm.i}

   run pi-pega-valor in h_v27ad098 (output c-cgc, output c-ins-estadual, output i-natureza).
   run pi-pega-valor in h_v29ad098 (output c-e-mail).

   DO  WITH FRAME {&FRAME-NAME}:

       If emitente.endereco:screen-value = "" Then do:
           {include/i-vldprg.i}
           {utp/ut-table.i mguni endereco 1}
           RUN UTP/UT-MSGS.P (INPUT "SHOW",
                              INPUT  4860,
                              INPUT return-value).
           APPLY "ENTRY" TO emitente.endereco.
           RETURN "ADM-ERROR":U.
       end.                        

       if  i-pais-impto-usuario <> 3 then do: /* Estados Unidos */
           If  emitente.bairro:screen-value = "" Then do:
               {include/i-vldprg.i}
               {utp/ut-table.i mguni bairro 1}
               RUN UTP/UT-MSGS.P (INPUT "SHOW",
                                  INPUT  5104,
                                  INPUT return-value).
               APPLY "ENTRY" TO emitente.bairro.
               RETURN "ADM-ERROR":U.
           end.                        
       end.

       If emitente.cidade:screen-value = "" Then do:
           {include/i-vldprg.i}
           {utp/ut-table.i mguni cidade 1}
           RUN UTP/UT-MSGS.P (INPUT "SHOW",
                              INPUT  4861,
                              INPUT return-value).
           APPLY "ENTRY" TO emitente.cidade.
           RETURN "ADM-ERROR":U.
       end.

       FIND FIRST UNID-FEDER 
          WHERE UNID-FEDER.PAIS   = input frame {&FRAME-NAME} EMITENTE.PAIS
          AND   UNID-FEDER.ESTADO = input frame {&FRAME-NAME} EMITENTE.ESTADO NO-LOCK NO-ERROR.
       IF  NOT AVAIL UNID-FEDER THEN DO:
          {include/i-vldprg.i}  
          {utp/ut-table.i mguni unid-feder 1} 
          RUN UTP/UT-MSGS.P (INPUT "SHOW",
                             INPUT  965,
                             INPUT return-value).
          APPLY "ENTRY" TO EMITENTE.ESTADO.
          RETURN "ADM-ERROR":U.                             
       END.

       FIND FIRST PAIS 
            WHERE PAIS.NOME-PAIS = input frame {&FRAME-NAME} EMITENTE.PAIS NO-LOCK NO-ERROR.
       IF  NOT AVAIL PAIS THEN DO:
           {include/i-vldprg.i}
           {utp/ut-table.i mguni PAIS 1}
           RUN UTP/UT-MSGS.P (INPUT "SHOW",
                              INPUT  964,
                              INPUT return-value).
           APPLY "ENTRY" TO EMITENTE.PAIS.
           RETURN "ADM-ERROR":U.                            
       END.


       IF  input frame {&FRAME-NAME} EMITENTE.END-COBRANCA <> i-cod-emitente THEN DO:
           FIND FIRST B-EMITENTE
                WHERE B-EMITENTE.COD-EMITENTE = input frame {&FRAME-NAME} EMITENTE.END-COBRANCA 
                and   b-emitente.identific   <> 2 no-lock NO-ERROR.
           IF  NOT AVAIL B-EMITENTE THEN DO:
               {include/i-vldprg.i}
               {utp/ut-table.i mgadm emitente 1}    
               RUN UTP/UT-MSGS.P (INPUT "SHOW",
                                  INPUT  895,
                                  INPUT return-value).
               APPLY "ENTRY" TO EMITENTE.END-COBRANCA.
               RETURN "ADM-ERROR":U.                            
           END.
       END.                

       if  input frame {&frame-name} emitente.cep = "" AND (i-natureza = 1 OR i-natureza = 2 ) then do:
           {include/i-vldprg.i}
           {utp/ut-table.i mgadm emitente 1}    
           run utp/ut-msgs.p (input "show",
                              input  192,
                              input return-value).
           apply "entry" to emitente.cep.
           return "ADM-ERROR":U.
       end.

       if  input frame {&FRAME-NAME} emitente.pais-cob <> "" then do:
           FIND FIRST PAIS 
                WHERE PAIS.NOME-PAIS = input frame {&FRAME-NAME} EMITENTE.PAIS-COB NO-LOCK NO-ERROR.
           IF  NOT AVAIL PAIS THEN DO:
               {include/i-vldprg.i} 
               {utp/ut-table.i mguni PAIS 1}        
               RUN UTP/UT-MSGS.P (INPUT "SHOW",
                                  INPUT  964,
                                  INPUT return-value).
               APPLY "ENTRY" TO EMITENTE.PAIS-COB.
               RETURN "ADM-ERROR":U.                            
           END.
       end.

       if  input frame {&FRAME-NAME} emitente.pais-cob   <> "" and 
           input frame {&FRAME-NAME} emitente.estado-cob <> "" then do :
           FIND FIRST UNID-FEDER 
                WHERE UNID-FEDER.PAIS   = input frame {&FRAME-NAME} EMITENTE.PAIS-COB
                AND   UNID-FEDER.ESTADO = input frame {&FRAME-NAME} EMITENTE.ESTADO-COB NO-LOCK NO-ERROR.
           IF  NOT AVAIL UNID-FEDER THEN DO:
               {include/i-vldprg.i}
               {utp/ut-table.i mguni unid-feder 1}        
               RUN UTP/UT-MSGS.P (INPUT "SHOW",
                                  INPUT  965,
                                  INPUT return-value).
               APPLY "ENTRY" TO EMITENTE.ESTADO-COB.
               RETURN "ADM-ERROR":U.                             
           END.     
       end.
   END.

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

