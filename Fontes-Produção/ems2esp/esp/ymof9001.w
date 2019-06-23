&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YMOF9001 12.01.19.000}

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
DEFINE TEMP-TABLE tt-es-klassmatt-integr LIKE es-klassmatt-integr.

DEFINE BUFFER bf-es-klassmatt-integr FOR es-klassmatt-integr.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE h-acomp             AS HANDLE    NO-UNDO.
DEFINE VARIABLE p-empresa-ini       AS CHARACTER NO-UNDO.
DEFINE VARIABLE p-empresa-fin       AS CHARACTER NO-UNDO.
DEFINE VARIABLE p-dt-trans-ini      AS DATE      NO-UNDO.
DEFINE VARIABLE p-dt-trans-fin      AS DATE      NO-UNDO.
DEFINE VARIABLE p-it-codigo-ini     AS CHARACTER NO-UNDO.
DEFINE VARIABLE p-it-codigo-fin     AS CHARACTER NO-UNDO.
DEFINE VARIABLE p-id-klassmatt-ini  AS INTEGER   NO-UNDO.
DEFINE VARIABLE p-id-klassmatt-fin  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSel                AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-enviado           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dt-trans-k          AS DATE      NO-UNDO.
DEFINE VARIABLE c-hr-trans          AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-transacao

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-es-klassmatt-integr

/* Definitions for BROWSE br-transacao                                  */
&Scoped-define FIELDS-IN-QUERY-br-transacao tt-es-klassmatt-integr.ep-codigo tt-es-klassmatt-integr.cod-estabel tt-es-klassmatt-integr.modo tt-es-klassmatt-integr.codigo tt-es-klassmatt-integr.dt-trans tt-es-klassmatt-integr.hr-trans tt-es-klassmatt-integr.IdKlassmatt tt-es-klassmatt-integr.statusRetorno tt-es-klassmatt-integr.log-retorno tt-es-klassmatt-integr.CodigoCompl tt-es-klassmatt-integr.DsComp tt-es-klassmatt-integr.DsRes tt-es-klassmatt-integr.dt-carga tt-es-klassmatt-integr.dt-int-erp tt-es-klassmatt-integr.dt-ret tt-es-klassmatt-integr.EmpresaSol tt-es-klassmatt-integr.EstabelecimentoSol tt-es-klassmatt-integr.Familia tt-es-klassmatt-integr.FormSupCtrleQtdeEstoque tt-es-klassmatt-integr.FormSupDepart tt-es-klassmatt-integr.FormSupFamComEqpto tt-es-klassmatt-integr.FormSupQtdeMinEstoque tt-es-klassmatt-integr.GrEstoque tt-es-klassmatt-integr.IdSIN tt-es-klassmatt-integr.NCM tt-es-klassmatt-integr.NCMExc tt-es-klassmatt-integr.Origem tt-es-klassmatt-integr.QuestFiscalData tt-es-klassmatt-integr.QuestFiscalHora tt-es-klassmatt-integr.QuestFiscalUsu tt-es-klassmatt-integr.Subgrupo tt-es-klassmatt-integr.Tipo tt-es-klassmatt-integr.UM   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-transacao   
&Scoped-define SELF-NAME br-transacao
&Scoped-define QUERY-STRING-br-transacao FOR EACH tt-es-klassmatt-integr NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-transacao OPEN QUERY {&SELF-NAME} FOR EACH tt-es-klassmatt-integr NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-transacao tt-es-klassmatt-integr
&Scoped-define FIRST-TABLE-IN-QUERY-br-transacao tt-es-klassmatt-integr


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-transacao}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-filtrar bt-reenvio br-transacao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-filtrar 
     IMAGE-UP FILE "image/filtro.bmp":U
     LABEL "Filtrar" 
     SIZE 7 BY 1.13 TOOLTIP "Filtrar".

DEFINE BUTTON bt-reenvio 
     IMAGE-UP FILE "image/im-send.bmp":U
     LABEL "Reenvio Manual" 
     SIZE 7 BY 1.13 TOOLTIP "Reenvio Manual".

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 142 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-transacao FOR 
      tt-es-klassmatt-integr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-transacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-transacao w-livre _FREEFORM
  QUERY br-transacao NO-LOCK DISPLAY
      tt-es-klassmatt-integr.ep-codigo          COLUMN-LABEL "Emp"      FORMAT "x(5)":U
      tt-es-klassmatt-integr.cod-estabel        COLUMN-LABEL "Estab"    FORMAT "x(5)":U
      tt-es-klassmatt-integr.modo               COLUMN-LABEL "Modo"     FORMAT "->,>>>,>>9":U
      tt-es-klassmatt-integr.codigo             COLUMN-LABEL "Item"     FORMAT "x(16)":U
      tt-es-klassmatt-integr.dt-trans                                   FORMAT "99/99/9999":U
      tt-es-klassmatt-integr.hr-trans                                   FORMAT "x(8)":U
      tt-es-klassmatt-integr.IdKlassmatt                                FORMAT "->,>>>,>>9":U
      tt-es-klassmatt-integr.statusRetorno      COLUMN-LABEL "Erro?"    FORMAT "x(1)":U
      tt-es-klassmatt-integr.log-retorno        COLUMN-LABEL "Mensagem" FORMAT "x(400)":U WIDTH 320
      tt-es-klassmatt-integr.CodigoCompl                                FORMAT "x(20)":U
      tt-es-klassmatt-integr.DsComp                                     FORMAT "x(2000)":U WIDTH 320
      tt-es-klassmatt-integr.DsRes                                      FORMAT "x(60)":U
      tt-es-klassmatt-integr.dt-carga                                   FORMAT "99/99/9999":U
      tt-es-klassmatt-integr.dt-int-erp                                 FORMAT "99/99/9999":U
      tt-es-klassmatt-integr.dt-ret                                     FORMAT "99/99/9999":U
      tt-es-klassmatt-integr.EmpresaSol                                 FORMAT "x(5)":U
      tt-es-klassmatt-integr.EstabelecimentoSol                         FORMAT "x(5)":U
      tt-es-klassmatt-integr.Familia                                    FORMAT "x(10)":U
      tt-es-klassmatt-integr.FormSupCtrleQtdeEstoque                    FORMAT "x(8)":U
      tt-es-klassmatt-integr.FormSupDepart                              FORMAT "x(8)":U
      tt-es-klassmatt-integr.FormSupFamComEqpto                         FORMAT "x(8)":U
      tt-es-klassmatt-integr.FormSupQtdeMinEstoque                      FORMAT "x(8)":U
      tt-es-klassmatt-integr.GrEstoque                                  FORMAT ">9":U
      tt-es-klassmatt-integr.IdSIN                                      FORMAT "->,>>>,>>9":U
      tt-es-klassmatt-integr.NCM                                        FORMAT "9999.99.99":U
      tt-es-klassmatt-integr.NCMExc                                     FORMAT ">,>>9.99999999":U
      tt-es-klassmatt-integr.Origem                                     FORMAT ">9":U
      tt-es-klassmatt-integr.QuestFiscalData                            FORMAT "x(10)":U
      tt-es-klassmatt-integr.QuestFiscalHora                            FORMAT "x(10)":U
      tt-es-klassmatt-integr.QuestFiscalUsu                             FORMAT "x(12)":U
      tt-es-klassmatt-integr.Subgrupo                                   FORMAT "x(8)":U
      tt-es-klassmatt-integr.Tipo                                       FORMAT "99":U
      tt-es-klassmatt-integr.UM                                         FORMAT "x(2)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 141.43 BY 21.71
         TITLE "Transa‡äes de Integra‡Æo Klassmatt x Totvs 12" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-filtrar AT ROW 1.17 COL 1.86 WIDGET-ID 4
     bt-reenvio AT ROW 1.17 COL 20.14 WIDGET-ID 6
     br-transacao AT ROW 2.5 COL 1.29 WIDGET-ID 200
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 142.86 BY 23.33 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 23.5
         WIDTH              = 143.29
         MAX-HEIGHT         = 30
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 30
         VIRTUAL-WIDTH      = 195.14
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-transacao bt-reenvio f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-transacao
/* Query rebuild information for BROWSE br-transacao
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-es-klassmatt-integr NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-transacao */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtrar w-livre
ON CHOOSE OF bt-filtrar IN FRAME f-cad /* Filtrar */
DO:
  RUN esp/ymof9000a.w(INPUT-OUTPUT p-empresa-ini,
                      INPUT-OUTPUT p-empresa-fin,
                      INPUT-OUTPUT p-dt-trans-ini,
                      INPUT-OUTPUT p-dt-trans-fin,
                      INPUT-OUTPUT p-it-codigo-ini,
                      INPUT-OUTPUT p-it-codigo-fin,
                      INPUT-OUTPUT p-id-klassmatt-ini,
                      INPUT-OUTPUT p-id-klassmatt-fin).

  RUN pi-limpa-tt.
  RUN pi-carrega-tt.
  RUN pi-atualiza-br.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-reenvio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-reenvio w-livre
ON CHOOSE OF bt-reenvio IN FRAME f-cad /* Reenvio Manual */
DO:

  ASSIGN l-enviado   = NO
         dt-trans-k  = TODAY
         c-hr-trans  = STRING(TIME,"HH:MM:SS").

  DO iSel = 1 TO br-transacao:num-selected-rows in frame {&frame-name}:
                 br-transacao:FETCH-SELECTED-ROW(iSel). 


     FIND FIRST es-integra-retorno WHERE    
                es-integra-retorno.IdKlassmatt     = tt-es-klassmatt-integr.IdKlassmatt AND
                es-integra-retorno.dt-ret          = ?                                  AND
                es-integra-retorno.dt-int-erp      = TODAY                              EXCLUSIVE-LOCK NO-ERROR.
     IF NOT AVAIL es-integra-retorno THEN
     DO:
        CREATE es-integra-retorno.                                                       
        ASSIGN es-integra-retorno.IdKlassmatt     = tt-es-klassmatt-integr.IdKlassmatt
               es-integra-retorno.dt-ret          = ?                                 
               es-integra-retorno.dt-int-erp      = TODAY                                         
               es-integra-retorno.codigo          = tt-es-klassmatt-integr.codigo
               es-integra-retorno.idsin           = tt-es-klassmatt-integr.idsin
               es-integra-retorno.ep-codigo       = tt-es-klassmatt-integr.empresasol         
               es-integra-retorno.cod-estabel     = tt-es-klassmatt-integr.estabelecimentosol               
               es-integra-retorno.dt-carga        = TODAY                                                                                           
               es-integra-retorno.log-retorno     = "Reenvio Manual de Transa‡Æo"   
               es-integra-retorno.statusRetorno   = "N". 
     END. 
     ELSE
         ASSIGN es-integra-retorno.dt-ret          = ?
                es-integra-retorno.statusRetorno   = "N"
                es-integra-retorno.log-retorno     = "Reenvio Manual de Transa‡Æo".

     FIND LAST bf-es-klassmatt-integr WHERE 
               bf-es-klassmatt-integr.idklassmatt = tt-es-klassmatt-integr.idklassmatt NO-LOCK NO-ERROR.
     IF AVAIL bf-es-klassmatt-integr THEN
     DO:
        FIND FIRST es-klassmatt-integr WHERE
                   es-klassmatt-integr.idklassmatt = bf-es-klassmatt-integr.idklassmatt AND
                   es-klassmatt-integr.dt-trans    = dt-trans-k                         AND
                   es-klassmatt-integr.hr-trans    = c-hr-trans                         EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL es-klassmatt-integr THEN
        DO:
           CREATE es-klassmatt-integr.
           BUFFER-COPY bf-es-klassmatt-integr TO es-klassmatt-integr
                                          ASSIGN es-klassmatt-integr.idklassmatt   = bf-es-klassmatt-integr.idklassmatt
                                                 es-klassmatt-integr.dt-trans      = dt-trans-k                 
                                                 es-klassmatt-integr.hr-trans      = c-hr-trans
                                                 es-klassmatt-integr.log-retorno   = es-integra-retorno.log-retorno  
                                                 es-klassmatt-integr.statusRetorno = es-integra-retorno.statusRetorno.
        END.
     END.

     RELEASE es-integra-retorno.

     ASSIGN l-enviado = YES.
  END.

  IF l-enviado THEN
     run utp/ut-msgs.p (input "show",
                        input 15825,
                        input "Reenvio Manual!~~Renvio manual realizado com sucesso!").

  RUN pi-limpa-tt.
  RUN pi-carrega-tt.
  RUN pi-atualiza-br.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-transacao
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 126.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-reenvio:HANDLE IN FRAME f-cad , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  ENABLE rt-button bt-filtrar bt-reenvio br-transacao 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "YMOF9001" "12.01.19.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN p-empresa-ini       = ""
         p-empresa-fin       = "ZZZZZ"
         p-dt-trans-ini      = TODAY
         p-dt-trans-fin      = TODAY
         p-it-codigo-ini     = ""
         p-it-codigo-fin     = "ZZZZZZZZZZZZZZZZ"
         p-id-klassmatt-ini  = 0
         p-id-klassmatt-fin  = 999999999.

  RUN pi-limpa-tt.
  RUN pi-carrega-tt.
  RUN pi-atualiza-br.

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-br w-livre 
PROCEDURE pi-atualiza-br :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

    OPEN QUERY br-transacao       FOR EACH tt-es-klassmatt-integr NO-LOCK
                                        BY tt-es-klassmatt-integr.dt-trans 
                                        BY tt-es-klassmatt-integr.hr-trans INDEXED-REPOSITION.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-tt w-livre 
PROCEDURE pi-carrega-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Filtrando, Aguarde...").

FOR EACH es-klassmatt-integr NO-LOCK WHERE                                          
         es-klassmatt-integr.ep-codigo   >= p-empresa-ini      AND                  
         es-klassmatt-integr.ep-codigo   <= p-empresa-fin      AND                  
         es-klassmatt-integr.dt-trans    >= p-dt-trans-ini     AND                  
         es-klassmatt-integr.dt-trans    <= p-dt-trans-fin     AND                                 
         es-klassmatt-integr.IdKlassmatt >= p-id-klassmatt-ini AND                  
         es-klassmatt-integr.IdKlassmatt <= p-id-klassmatt-fin AND
         es-klassmatt-integr.Codigo      >= p-it-codigo-ini    AND                  
         es-klassmatt-integr.Codigo      <= p-it-codigo-fin
      BY es-klassmatt-integr.IdKlassmatt:

    run pi-acompanhar in h-acomp (input "ID Klassmatt: " + string(es-klassmatt-integr.IdKlassmatt)).

    CREATE tt-es-klassmatt-integr.
    BUFFER-COPY es-klassmatt-integr TO tt-es-klassmatt-integr.
END.

run pi-finalizar in h-acomp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-limpa-tt w-livre 
PROCEDURE pi-limpa-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE tt-es-klassmatt-integr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-es-klassmatt-integr"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

