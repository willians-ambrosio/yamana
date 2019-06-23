&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5_esp         PROGRESS
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
{include/i-prgvrs.i YMCD0207 2.06.00.001}

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

DEFINE TEMP-TABLE tt-es-integra-retorno LIKE es-integra-retorno.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-integra-item

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-integra-item

/* Definitions for BROWSE br-integra-item                               */
&Scoped-define FIELDS-IN-QUERY-br-integra-item es-integra-item.dt-carga ~
es-integra-item.IdKlassmatt es-integra-item.IdSIN es-integra-item.codigo ~
es-integra-item.CodigoCompl es-integra-item.DsComp es-integra-item.DsRes ~
es-integra-item.dt-int-erp es-integra-item.EmpresaSol ~
es-integra-item.EstabelecimentoSol es-integra-item.Familia ~
es-integra-item.FormSupCtrleQtdeEstoque es-integra-item.FormSupDepart ~
es-integra-item.FormSupFamComEqpto es-integra-item.FormSupQtdeMinEstoque ~
es-integra-item.GrEstoque es-integra-item.modo es-integra-item.NCM ~
es-integra-item.NCMExc es-integra-item.Origem ~
es-integra-item.QuestFiscalData es-integra-item.QuestFiscalHora ~
es-integra-item.QuestFiscalUsu es-integra-item.Subgrupo ~
es-integra-item.Tipo es-integra-item.UM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-integra-item 
&Scoped-define QUERY-STRING-br-integra-item FOR EACH es-integra-item ~
      WHERE es-integra-item.dt-carga >= date(dt-carga-ini:screen-value) ~
 AND es-integra-item.dt-carga <= date(dt-carga-fim:screen-value) NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-integra-item OPEN QUERY br-integra-item FOR EACH es-integra-item ~
      WHERE es-integra-item.dt-carga >= date(dt-carga-ini:screen-value) ~
 AND es-integra-item.dt-carga <= date(dt-carga-fim:screen-value) NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-integra-item es-integra-item
&Scoped-define FIRST-TABLE-IN-QUERY-br-integra-item es-integra-item


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-integra-item}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 dt-carga-ini dt-carga-fim bt-filtro ~
br-integra-item bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS dt-carga-ini dt-carga-fim 

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

DEFINE BUTTON bt-filtro 
     LABEL "Busca" 
     SIZE 12 BY .88.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE dt-carga-fim LIKE es-integra-retorno.dt-carga
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE dt-carga-ini LIKE es-integra-retorno.dt-carga
     LABEL "Dt. Carga" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-integra-item FOR 
      es-integra-item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-integra-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-integra-item w-window _STRUCTURED
  QUERY br-integra-item NO-LOCK DISPLAY
      es-integra-item.dt-carga FORMAT "99/99/9999":U
      es-integra-item.IdKlassmatt FORMAT "->,>>>,>>9":U
      es-integra-item.IdSIN FORMAT "->,>>>,>>9":U
      es-integra-item.codigo FORMAT "x(16)":U
      es-integra-item.CodigoCompl FORMAT "x(20)":U
      es-integra-item.DsComp FORMAT "x(2000)":U
      es-integra-item.DsRes FORMAT "x(60)":U
      es-integra-item.dt-int-erp FORMAT "99/99/9999":U
      es-integra-item.EmpresaSol FORMAT "x(5)":U
      es-integra-item.EstabelecimentoSol FORMAT "x(5)":U
      es-integra-item.Familia FORMAT "x(10)":U
      es-integra-item.FormSupCtrleQtdeEstoque FORMAT "x(8)":U
      es-integra-item.FormSupDepart FORMAT "x(8)":U
      es-integra-item.FormSupFamComEqpto FORMAT "x(8)":U
      es-integra-item.FormSupQtdeMinEstoque FORMAT "x(8)":U
      es-integra-item.GrEstoque FORMAT ">9":U
      es-integra-item.modo FORMAT "->,>>>,>>9":U
      es-integra-item.NCM FORMAT "9999.99.99":U
      es-integra-item.NCMExc FORMAT ">,>>9.99999999":U
      es-integra-item.Origem FORMAT ">9":U
      es-integra-item.QuestFiscalData FORMAT "x(10)":U
      es-integra-item.QuestFiscalHora FORMAT "x(10)":U
      es-integra-item.QuestFiscalUsu FORMAT "x(12)":U
      es-integra-item.Subgrupo FORMAT "x(8)":U
      es-integra-item.Tipo FORMAT "99":U
      es-integra-item.UM FORMAT "x(2)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 77 BY 9.25 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     dt-carga-ini AT ROW 1.25 COL 11.29 COLON-ALIGNED HELP
          "" WIDGET-ID 2
          LABEL "Dt. Carga"
     dt-carga-fim AT ROW 1.29 COL 34.57 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 6
     bt-filtro AT ROW 1.29 COL 54 WIDGET-ID 8
     br-integra-item AT ROW 2.5 COL 2 WIDGET-ID 200
     bt-ok AT ROW 12.21 COL 3
     bt-cancelar AT ROW 12.21 COL 14
     bt-ajuda AT ROW 12.21 COL 69
     "<<   >>" VIEW-AS TEXT
          SIZE 7 BY .67 AT ROW 1.42 COL 27.86 WIDGET-ID 4
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
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta Integra Item"
         HEIGHT             = 12.58
         WIDTH              = 80
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
/* BROWSE-TAB br-integra-item bt-filtro F-Main */
/* SETTINGS FOR FILL-IN dt-carga-fim IN FRAME F-Main
   LIKE = ems5_esp.es-integra-retorno.dt-carga EXP-LABEL EXP-SIZE       */
/* SETTINGS FOR FILL-IN dt-carga-ini IN FRAME F-Main
   LIKE = ems5_esp.es-integra-retorno.dt-carga EXP-LABEL                */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-integra-item
/* Query rebuild information for BROWSE br-integra-item
     _TblList          = "ems5_esp.es-integra-item"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "ems5_esp.es-integra-item.dt-carga >= date(dt-carga-ini:screen-value)
 AND ems5_esp.es-integra-item.dt-carga <= date(dt-carga-fim:screen-value)"
     _FldNameList[1]   = ems5_esp.es-integra-item.dt-carga
     _FldNameList[2]   = ems5_esp.es-integra-item.IdKlassmatt
     _FldNameList[3]   = ems5_esp.es-integra-item.IdSIN
     _FldNameList[4]   = ems5_esp.es-integra-item.codigo
     _FldNameList[5]   = ems5_esp.es-integra-item.CodigoCompl
     _FldNameList[6]   = ems5_esp.es-integra-item.DsComp
     _FldNameList[7]   = ems5_esp.es-integra-item.DsRes
     _FldNameList[8]   = ems5_esp.es-integra-item.dt-int-erp
     _FldNameList[9]   = ems5_esp.es-integra-item.EmpresaSol
     _FldNameList[10]   = ems5_esp.es-integra-item.EstabelecimentoSol
     _FldNameList[11]   = ems5_esp.es-integra-item.Familia
     _FldNameList[12]   = ems5_esp.es-integra-item.FormSupCtrleQtdeEstoque
     _FldNameList[13]   = ems5_esp.es-integra-item.FormSupDepart
     _FldNameList[14]   = ems5_esp.es-integra-item.FormSupFamComEqpto
     _FldNameList[15]   = ems5_esp.es-integra-item.FormSupQtdeMinEstoque
     _FldNameList[16]   = ems5_esp.es-integra-item.GrEstoque
     _FldNameList[17]   = ems5_esp.es-integra-item.modo
     _FldNameList[18]   = ems5_esp.es-integra-item.NCM
     _FldNameList[19]   = ems5_esp.es-integra-item.NCMExc
     _FldNameList[20]   = ems5_esp.es-integra-item.Origem
     _FldNameList[21]   = ems5_esp.es-integra-item.QuestFiscalData
     _FldNameList[22]   = ems5_esp.es-integra-item.QuestFiscalHora
     _FldNameList[23]   = ems5_esp.es-integra-item.QuestFiscalUsu
     _FldNameList[24]   = ems5_esp.es-integra-item.Subgrupo
     _FldNameList[25]   = ems5_esp.es-integra-item.Tipo
     _FldNameList[26]   = ems5_esp.es-integra-item.UM
     _Query            is OPENED
*/  /* BROWSE br-integra-item */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Consulta Integra Item */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Consulta Integra Item */
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


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro w-window
ON CHOOSE OF bt-filtro IN FRAME F-Main /* Busca */
DO:
    OPEN QUERY br-integra-item FOR EACH es-integra-item ~
      WHERE es-integra-item.dt-carga >= date(dt-carga-ini:SCREEN-VALUE) ~
        AND es-integra-item.dt-carga <= date(dt-carga-fim:SCREEN-VALUE) NO-LOCK INDEXED-REPOSITION.
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


&Scoped-define BROWSE-NAME br-integra-item
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}


    ASSIGN dt-carga-ini:SCREEN-VALUE = string(TODAY - 30)
           dt-carga-fim:SCREEN-VALUE = string(TODAY).

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
  DISPLAY dt-carga-ini dt-carga-fim 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 dt-carga-ini dt-carga-fim bt-filtro br-integra-item bt-ok 
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
  
  {utp/ut9000.i "YMCD0207" "2.06.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN dt-carga-ini:SCREEN-VALUE = string(TODAY - 30)
         dt-carga-fim:SCREEN-VALUE = string(TODAY).



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
  {src/adm/template/snd-list.i "es-integra-item"}

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

