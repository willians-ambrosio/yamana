&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgadm            PROGRESS
          movadm           PROGRESS
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
def buffer portador for ems2cadme.portador.

{include/i-prgvrs.i V10AD264 2.00.00.016}  /*** 010016 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i v10ad264 MUT}
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

/* Parameters Definitions ---                                           */

{include/i_dbvers.i} /* miniflexibilizaá∆o */

/* Local Variable Definitions ---                                       */
def var c-lista-tipo-origem     as char    no-undo.
def var c-lista-tipo-modalidade as char    no-undo.
def var c-lista-tipo-situacao   as char    no-undo.
def var de-dolar                as decimal no-undo.

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
&Scoped-define EXTERNAL-TABLES titulo
&Scoped-define FIRST-EXTERNAL-TABLE titulo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR titulo.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS titulo.cod-port titulo.agencia ~
titulo.titulo-banco titulo.nr-bordero titulo.cob-banc-eletr titulo.enviado 
&Scoped-define ENABLED-TABLES titulo
&Scoped-define FIRST-ENABLED-TABLE titulo
&Scoped-Define ENABLED-OBJECTS RECT-4 
&Scoped-Define DISPLAYED-FIELDS titulo.cod-port titulo.agencia ~
titulo.titulo-banco titulo.nr-bordero titulo.cob-banc-eletr titulo.enviado 
&Scoped-define DISPLAYED-TABLES titulo
&Scoped-define FIRST-DISPLAYED-TABLE titulo
&Scoped-Define DISPLAYED-OBJECTS fi-modalidade tg-cartorio l-conta-sdo-bco ~
dt-envio hora-envio 

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
<FOREIGN-KEYS></FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = ':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE dt-envio AS DATE FORMAT "99/99/9999":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-modalidade AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 14.43 BY .88 NO-UNDO.

DEFINE VARIABLE hora-envio AS CHARACTER FORMAT "99:99":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 48.29 BY 2.75.

DEFINE VARIABLE l-conta-sdo-bco AS LOGICAL INITIAL no 
     LABEL "IMD contra Conta Sdo de Banco ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .63.

DEFINE VARIABLE tg-cartorio AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.57 BY .63 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     titulo.cod-port AT ROW 1.75 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     fi-modalidade AT ROW 1.75 COL 28.57 COLON-ALIGNED NO-LABEL
     titulo.agencia AT ROW 2.75 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.57 BY .88
     titulo.titulo-banco AT ROW 3.75 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .88
     titulo.nr-bordero AT ROW 4.75 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .88
     titulo.cob-banc-eletr AT ROW 6 COL 23
          VIEW-AS TOGGLE-BOX
          SIZE 15.86 BY .63
     tg-cartorio AT ROW 6 COL 40.57
     l-conta-sdo-bco AT ROW 6.75 COL 23
     titulo.enviado AT ROW 7.75 COL 22.86 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Enviado a Banco", 1,
"Auto-Emiss∆o", 2,
"Nao Enviado", 3
          SIZE 20.14 BY 2.25
     dt-envio AT ROW 7.92 COL 51.86 COLON-ALIGNED
     hora-envio AT ROW 8.92 COL 51.86 COLON-ALIGNED
     RECT-4 AT ROW 7.5 COL 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgadm.titulo
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 9.42
         WIDTH              = 67.29.
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
       FRAME f-main:HEIGHT           = 9.42
       FRAME f-main:WIDTH            = 67.29.

/* SETTINGS FOR FILL-IN dt-envio IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-modalidade IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN hora-envio IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX l-conta-sdo-bco IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-cartorio IN FRAME f-main
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




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
      DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
      ASSIGN cAuxTraducao001 = {adinc/i01ad264.i 03}.
      RUN utp/ut-list.p (INPUT-OUTPUT cAuxTraducao001).
      ASSIGN  c-lista-tipo-origem     = cAuxTraducao001.
  &else
      ASSIGN c-lista-tipo-origem     = {adinc/i01ad264.i 03}.
  &endif
  &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
      DEFINE VARIABLE cAuxTraducao002 AS CHARACTER NO-UNDO.
      ASSIGN cAuxTraducao002 = {adinc/i03ad209.i 03}.
      RUN utp/ut-list.p (INPUT-OUTPUT cAuxTraducao002).
      ASSIGN  c-lista-tipo-modalidade = cAuxTraducao002.
  &else
      ASSIGN c-lista-tipo-modalidade = {adinc/i03ad209.i 03}.
  &endif
  &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
      DEFINE VARIABLE cAuxTraducao003 AS CHARACTER NO-UNDO.
      ASSIGN cAuxTraducao003 = {adinc/i03ad264.i 03}.
      RUN utp/ut-list.p (INPUT-OUTPUT cAuxTraducao003).
      ASSIGN  c-lista-tipo-situacao   = cAuxTraducao003.
  &else
      ASSIGN c-lista-tipo-situacao   = {adinc/i03ad264.i 03}.
  &endif

  {utp/ut-liter.i Data_Envio}
  assign dt-envio:label in frame {&FRAME-NAME} = trim(return-value).

  {utp/ut-liter.i Hora_Envio}
  assign hora-envio:label in frame {&FRAME-NAME} = trim(return-value).

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
  {src/adm/template/row-list.i "titulo"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "titulo"}

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  if avail titulo then
     assign fi-modalidade = entry(titulo.modalidade, c-lista-tipo-modalidade).

  run pi-versao (input 2).

  if  avail titulo then do:
      &if "{&mgadm_version}" >= "2.02" &then
            ASSIGN tg-cartorio = titulo.enviado-cartorio.
      &endif.
      if  titulo.enviado = 1 then do:
          assign dt-envio:hidden in frame {&FRAME-NAME} = no
                 hora-envio:hidden in frame {&FRAME-NAME} = no.

          find first mov-tit 
               where mov-tit.ep-codigo   = titulo.ep-codigo
               and   mov-tit.cod-estabel = titulo.cod-estabel
               and   mov-tit.cod-esp     = titulo.cod-esp
               and   mov-tit.serie       = titulo.serie
               and   mov-tit.nr-docto    = titulo.nr-docto
               and   mov-tit.parcela     = titulo.parcela
               and   mov-tit.transacao   = 14
               NO-LOCK no-error.

          if  avail mov-tit then do:
              assign dt-envio   = mov-tit.dt-envio
                     hora-envio = mov-tit.hora-envio.
          end.
          else do:
              assign dt-envio   = ?
                     hora-envio = "".
          end.
      end.
      else do:
          assign dt-envio:hidden    in frame {&FRAME-NAME} = yes
                 hora-envio:hidden  in frame {&FRAME-NAME} = yes.
      end.
      ASSIGN l-conta-sdo-bco = NO.
      l-conta-sdo-bco:HIDDEN IN FRAME {&FRAME-NAME} = YES.

      if titulo.tipo = 1 then do:
          l-conta-sdo-bco:HIDDEN IN FRAME {&FRAME-NAME} = NO.
          find first mov-tit
               where mov-tit.ep-codigo   = titulo.ep-codigo
               and   mov-tit.cod-estabel = titulo.cod-estabel
               and   mov-tit.cod-esp     = titulo.cod-esp
               and   mov-tit.serie       = titulo.serie
               and   mov-tit.nr-docto    = titulo.nr-docto
               and   mov-tit.parcela     = titulo.parcela
               and   mov-tit.transacao   = 14
               NO-LOCK no-error.
          if avail mov-tit then do:
             find portador use-index codigo
                  where portador.ep-codigo  = mov-tit.ep-codigo
                  and   portador.cod-port   = mov-tit.cod-port
                  and   portador.modalidade = mov-tit.modalidade
                  no-lock no-error.
             if avail portador 
             then do:
                if portador.bancario = yes 
                then do:
                   find cta-corrente use-index conta
                        where cta-corrente.cod-banco     = portador.cod-banco
                        and   cta-corrente.agencia       = portador.agencia
                        and   cta-corrente.conta-corren  = portador.conta-corren
                        no-lock no-error.
                   if  avail cta-corrente 
                   then do:
                      find first conta-contab use-index ch-conta-contabil
                           where conta-contab.ep-codigo     = mov-tit.ep-codigo
                           and   conta-contab.conta-contab  = mov-tit.conta-credito
                           no-lock no-error.
                      if avail conta-contab AND conta-contab.caixa-bancos = 3 
                      then DO:
                             ASSIGN l-conta-sdo-bco = yes.
                      END.
                   END.
                END.
             END.
          END.
      end.
  end.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

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
  run pi-versao (input 1).

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
    def input param p-int as integer no-undo.

    &if "{&mgadm_version}" >= "2.02" &then
        case p-int:
            when 1 then do: /* initialize */
                {utp/ut-liter.i Enviado_Ö_Cart¢rio * R}.
                assign tg-cartorio:visible in frame {&FRAME-NAME} = yes
                       tg-cartorio:label   in frame {&FRAME-NAME} = return-value.
            end.
            when 2 then do: /* display */
                if  avail titulo then
                    assign tg-cartorio:screen-value in frame {&FRAME-NAME} = string(titulo.enviado-cartorio).
            end.
        end case.
    &endif

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
  {src/adm/template/snd-list.i "titulo"}

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

