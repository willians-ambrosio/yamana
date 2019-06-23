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
{include/i-prgvrs.i YMOF9000 12.01.19.000}

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
DEFINE TEMP-TABLE tt-es-klassmatt-log-qf  LIKE es-klassmatt-log-qf.
DEFINE TEMP-TABLE tt-es-klassmatt-log-tab LIKE es-klassmatt-log-tab.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE p-empresa-ini       AS CHARACTER NO-UNDO.
DEFINE VARIABLE p-empresa-fin       AS CHARACTER NO-UNDO.
DEFINE VARIABLE p-dt-trans-ini      AS DATE      NO-UNDO.
DEFINE VARIABLE p-dt-trans-fin      AS DATE      NO-UNDO.
DEFINE VARIABLE p-it-codigo-ini     AS CHARACTER NO-UNDO.
DEFINE VARIABLE p-it-codigo-fin     AS CHARACTER NO-UNDO.
DEFINE VARIABLE p-id-klassmatt-ini  AS INTEGER   NO-UNDO.
DEFINE VARIABLE p-id-klassmatt-fin  AS INTEGER   NO-UNDO.
DEFINE VARIABLE p-dt-trans          AS DATE      NO-UNDO. 
DEFINE VARIABLE p-hr-trans          AS CHARACTER NO-UNDO.
DEFINE VARIABLE p-id-klassmatt      AS INTEGER   NO-UNDO.
DEFINE VARIABLE h-acomp             AS HANDLE    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-quest-klassmatt

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-es-klassmatt-log-qf ~
tt-es-klassmatt-log-tab

/* Definitions for BROWSE br-quest-klassmatt                            */
&Scoped-define FIELDS-IN-QUERY-br-quest-klassmatt tt-es-klassmatt-log-qf.seq tt-es-klassmatt-log-qf.pergunta tt-es-klassmatt-log-qf.DescricaoResposta tt-es-klassmatt-log-qf.resposta   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-quest-klassmatt   
&Scoped-define SELF-NAME br-quest-klassmatt
&Scoped-define QUERY-STRING-br-quest-klassmatt FOR EACH tt-es-klassmatt-log-qf NO-LOCK WHERE                                  tt-es-klassmatt-log-qf.dt-trans    = p-dt-trans       AND                                  tt-es-klassmatt-log-qf.hr-trans    = p-hr-trans       AND                                  tt-es-klassmatt-log-qf.idKlassmatt = p-id-klassmatt   INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-quest-klassmatt OPEN QUERY {&SELF-NAME} FOR EACH tt-es-klassmatt-log-qf NO-LOCK WHERE                                  tt-es-klassmatt-log-qf.dt-trans    = p-dt-trans       AND                                  tt-es-klassmatt-log-qf.hr-trans    = p-hr-trans       AND                                  tt-es-klassmatt-log-qf.idKlassmatt = p-id-klassmatt   INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-quest-klassmatt tt-es-klassmatt-log-qf
&Scoped-define FIRST-TABLE-IN-QUERY-br-quest-klassmatt tt-es-klassmatt-log-qf


/* Definitions for BROWSE br-tabklass                                   */
&Scoped-define FIELDS-IN-QUERY-br-tabklass tt-es-klassmatt-log-tab.EmpresaSol tt-es-klassmatt-log-tab.EstabelecimentoSol tt-es-klassmatt-log-tab.dt-trans tt-es-klassmatt-log-tab.hr-trans tt-es-klassmatt-log-tab.IdKlassmatt tt-es-klassmatt-log-tab.modo tt-es-klassmatt-log-tab.Codigo tt-es-klassmatt-log-tab.DsRes tt-es-klassmatt-log-tab.CodigoCompl tt-es-klassmatt-log-tab.DsComp tt-es-klassmatt-log-tab.Familia tt-es-klassmatt-log-tab.FormSupCtrleQtdeEstoque tt-es-klassmatt-log-tab.FormSupDepart tt-es-klassmatt-log-tab.FormSupFamComEqpto tt-es-klassmatt-log-tab.FormSupQtdeMinEstoque tt-es-klassmatt-log-tab.GrEstoque tt-es-klassmatt-log-tab.IdSIN tt-es-klassmatt-log-tab.NCM tt-es-klassmatt-log-tab.NCMExc tt-es-klassmatt-log-tab.Origem tt-es-klassmatt-log-tab.QuestFiscalData tt-es-klassmatt-log-tab.QuestFiscalHora tt-es-klassmatt-log-tab.QuestFiscalUsu tt-es-klassmatt-log-tab.Subgrupo tt-es-klassmatt-log-tab.Tipo tt-es-klassmatt-log-tab.UM   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-tabklass   
&Scoped-define SELF-NAME br-tabklass
&Scoped-define QUERY-STRING-br-tabklass FOR EACH tt-es-klassmatt-log-tab NO-LOCK                               BY tt-es-klassmatt-log-tab.dt-trans                               BY tt-es-klassmatt-log-tab.hr-trans INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-tabklass OPEN QUERY {&SELF-NAME} FOR EACH tt-es-klassmatt-log-tab NO-LOCK                               BY tt-es-klassmatt-log-tab.dt-trans                               BY tt-es-klassmatt-log-tab.hr-trans INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-tabklass tt-es-klassmatt-log-tab
&Scoped-define FIRST-TABLE-IN-QUERY-br-tabklass tt-es-klassmatt-log-tab


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-quest-klassmatt}~
    ~{&OPEN-QUERY-br-tabklass}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-filtrar br-tabklass ~
br-quest-klassmatt 

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

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 143 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-quest-klassmatt FOR 
      tt-es-klassmatt-log-qf SCROLLING.

DEFINE QUERY br-tabklass FOR 
      tt-es-klassmatt-log-tab SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-quest-klassmatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-quest-klassmatt w-livre _FREEFORM
  QUERY br-quest-klassmatt NO-LOCK DISPLAY
      tt-es-klassmatt-log-qf.seq               FORMAT "999":U
      tt-es-klassmatt-log-qf.pergunta          FORMAT "x(400)":U                         WIDTH 320
      tt-es-klassmatt-log-qf.DescricaoResposta FORMAT "x(200)":U
      tt-es-klassmatt-log-qf.resposta          COLUMN-LABEL "Resposta" FORMAT "x(400)":U WIDTH 320
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 142.43 BY 6.25
         TITLE "Questäes/Respostas" FIT-LAST-COLUMN.

DEFINE BROWSE br-tabklass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-tabklass w-livre _FREEFORM
  QUERY br-tabklass NO-LOCK DISPLAY
      tt-es-klassmatt-log-tab.EmpresaSol            COLUMN-LABEL "Emp"   FORMAT "x(5)":U             WIDTH 7
      tt-es-klassmatt-log-tab.EstabelecimentoSol    COLUMN-LABEL "Estab" FORMAT "x(5)":U   WIDTH 7
      tt-es-klassmatt-log-tab.dt-trans                                   FORMAT "99/99/9999":U
      tt-es-klassmatt-log-tab.hr-trans                                   FORMAT "x(8)":U
      tt-es-klassmatt-log-tab.IdKlassmatt                                FORMAT ">>>,>>>,>>9":U
      tt-es-klassmatt-log-tab.modo                  COLUMN-LABEL "Modo"  FORMAT "->,>>>,>>9":U
      tt-es-klassmatt-log-tab.Codigo                COLUMN-LABEL "Item"  FORMAT "x(16)":U
      tt-es-klassmatt-log-tab.DsRes                                      FORMAT "x(60)":U
      tt-es-klassmatt-log-tab.CodigoCompl                                FORMAT "x(20)":U
      tt-es-klassmatt-log-tab.DsComp                                     FORMAT "x(2000)":U WIDTH 320
      tt-es-klassmatt-log-tab.Familia                                    FORMAT "x(8)":U
      tt-es-klassmatt-log-tab.FormSupCtrleQtdeEstoque                    FORMAT "x(8)":U
      tt-es-klassmatt-log-tab.FormSupDepart                              FORMAT "x(8)":U
      tt-es-klassmatt-log-tab.FormSupFamComEqpto                         FORMAT "x(8)":U
      tt-es-klassmatt-log-tab.FormSupQtdeMinEstoque                      FORMAT "x(8)":U
      tt-es-klassmatt-log-tab.GrEstoque                                  FORMAT ">9":U
      tt-es-klassmatt-log-tab.IdSIN                                      FORMAT ">>>,>>>,>>9":U
      tt-es-klassmatt-log-tab.NCM                                        FORMAT "x(10)":U
      tt-es-klassmatt-log-tab.NCMExc                                     FORMAT ">>,>>9.99999999":U
      tt-es-klassmatt-log-tab.Origem                                     FORMAT ">>>,>>>,>>9":U
      tt-es-klassmatt-log-tab.QuestFiscalData                            FORMAT "x(8)":U
      tt-es-klassmatt-log-tab.QuestFiscalHora                            FORMAT "x(8)":U
      tt-es-klassmatt-log-tab.QuestFiscalUsu                             FORMAT "x(12)":U
      tt-es-klassmatt-log-tab.Subgrupo                                   FORMAT "x(8)":U
      tt-es-klassmatt-log-tab.Tipo                                       FORMAT "x(8)":U
      tt-es-klassmatt-log-tab.UM                                         FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 142.43 BY 15.54
         TITLE "Transa‡äes Recebidas via Webservice da Klassmatt (Integra‡Æo)" ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-filtrar AT ROW 1.17 COL 1.72 WIDGET-ID 4
     br-tabklass AT ROW 2.46 COL 1.29 WIDGET-ID 200
     br-quest-klassmatt AT ROW 18.08 COL 1.29 WIDGET-ID 300
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143.72 BY 28.13 WIDGET-ID 100.


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
         WIDTH              = 143.43
         MAX-HEIGHT         = 29.08
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 29.08
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
/* BROWSE-TAB br-tabklass bt-filtrar f-cad */
/* BROWSE-TAB br-quest-klassmatt br-tabklass f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-quest-klassmatt
/* Query rebuild information for BROWSE br-quest-klassmatt
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-es-klassmatt-log-qf NO-LOCK WHERE
                                 tt-es-klassmatt-log-qf.dt-trans    = p-dt-trans       AND
                                 tt-es-klassmatt-log-qf.hr-trans    = p-hr-trans       AND
                                 tt-es-klassmatt-log-qf.idKlassmatt = p-id-klassmatt   INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-quest-klassmatt */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-tabklass
/* Query rebuild information for BROWSE br-tabklass
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-es-klassmatt-log-tab NO-LOCK
                              BY tt-es-klassmatt-log-tab.dt-trans
                              BY tt-es-klassmatt-log-tab.hr-trans INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-tabklass */
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


&Scoped-define BROWSE-NAME br-tabklass
&Scoped-define SELF-NAME br-tabklass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tabklass w-livre
ON ENTRY OF br-tabklass IN FRAME f-cad /* Transa‡äes Recebidas via Webservice da Klassmatt (Integra‡Æo) */
DO:
   IF AVAIL tt-es-klassmatt-log-tab THEN
   DO:     
      OPEN QUERY br-quest-klassmatt FOR EACH tt-es-klassmatt-log-qf NO-LOCK WHERE
                                             tt-es-klassmatt-log-qf.dt-trans    = tt-es-klassmatt-log-tab.dt-trans        AND
                                             tt-es-klassmatt-log-qf.hr-trans    = tt-es-klassmatt-log-tab.hr-trans        AND
                                             tt-es-klassmatt-log-qf.idKlassmatt = tt-es-klassmatt-log-tab.idKlassmatt     INDEXED-REPOSITION.

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tabklass w-livre
ON ROW-DISPLAY OF br-tabklass IN FRAME f-cad /* Transa‡äes Recebidas via Webservice da Klassmatt (Integra‡Æo) */
DO:
   IF AVAIL tt-es-klassmatt-log-tab THEN
   DO:     
      OPEN QUERY br-quest-klassmatt FOR EACH tt-es-klassmatt-log-qf NO-LOCK WHERE
                                             tt-es-klassmatt-log-qf.dt-trans    = tt-es-klassmatt-log-tab.dt-trans        AND
                                             tt-es-klassmatt-log-qf.hr-trans    = tt-es-klassmatt-log-tab.hr-trans        AND
                                             tt-es-klassmatt-log-qf.idKlassmatt = tt-es-klassmatt-log-tab.idKlassmatt     INDEXED-REPOSITION.

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tabklass w-livre
ON VALUE-CHANGED OF br-tabklass IN FRAME f-cad /* Transa‡äes Recebidas via Webservice da Klassmatt (Integra‡Æo) */
DO:
   IF AVAIL tt-es-klassmatt-log-tab THEN
   DO:     
      OPEN QUERY br-quest-klassmatt FOR EACH tt-es-klassmatt-log-qf NO-LOCK WHERE
                                             tt-es-klassmatt-log-qf.dt-trans    = tt-es-klassmatt-log-tab.dt-trans        AND
                                             tt-es-klassmatt-log-qf.hr-trans    = tt-es-klassmatt-log-tab.hr-trans        AND
                                             tt-es-klassmatt-log-qf.idKlassmatt = tt-es-klassmatt-log-tab.idKlassmatt     INDEXED-REPOSITION.

   END.
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


&Scoped-define BROWSE-NAME br-quest-klassmatt
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
       RUN set-position IN h_p-exihel ( 1.17 , 127.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-filtrar:HANDLE IN FRAME f-cad , 'AFTER':U ).
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
  ENABLE rt-button bt-filtrar br-tabklass br-quest-klassmatt 
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

  {utp/ut9000.i "YMOF9000" "12.01.19.000"}

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
         p-id-klassmatt-fin  = 999999999
         p-dt-trans          = ?
         p-hr-trans          = ""
         p-id-klassmatt      = 0.

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

    OPEN QUERY br-tabklass        FOR EACH tt-es-klassmatt-log-tab NO-LOCK
                                        BY tt-es-klassmatt-log-tab.dt-trans 
                                        BY tt-es-klassmatt-log-tab.hr-trans INDEXED-REPOSITION.


    OPEN QUERY br-quest-klassmatt FOR EACH tt-es-klassmatt-log-qf NO-LOCK INDEXED-REPOSITION.




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

FOR EACH es-klassmatt-log-tab NO-LOCK WHERE                                          
         es-klassmatt-log-tab.EmpresaSol  >= p-empresa-ini      AND                  
         es-klassmatt-log-tab.EmpresaSol  <= p-empresa-fin      AND                  
         es-klassmatt-log-tab.dt-trans    >= p-dt-trans-ini     AND                  
         es-klassmatt-log-tab.dt-trans    <= p-dt-trans-fin     AND                  
         es-klassmatt-log-tab.Codigo      >= p-it-codigo-ini    AND                  
         es-klassmatt-log-tab.Codigo      <= p-it-codigo-fin    AND                  
         es-klassmatt-log-tab.IdKlassmatt >= p-id-klassmatt-ini AND                  
         es-klassmatt-log-tab.IdKlassmatt <= p-id-klassmatt-fin 
      BY es-klassmatt-log-tab.IdKlassmatt:

    run pi-acompanhar in h-acomp (input "ID Klassmatt: " + string(es-klassmatt-log-tab.IdKlassmatt)).

    CREATE tt-es-klassmatt-log-tab.
    BUFFER-COPY es-klassmatt-log-tab TO tt-es-klassmatt-log-tab.
                                                                                        
    FOR EACH es-klassmatt-log-qf NO-LOCK WHERE                                           
             es-klassmatt-log-qf.dt-trans    = es-klassmatt-log-tab.dt-trans   AND                      
             es-klassmatt-log-qf.hr-trans    = es-klassmatt-log-tab.hr-trans   AND                      
             es-klassmatt-log-qf.idKlassmatt = es-klassmatt-log-tab.IdKlassmatt:
        CREATE tt-es-klassmatt-log-qf.
        BUFFER-COPY es-klassmatt-log-qf TO tt-es-klassmatt-log-qf.
    END.
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
EMPTY TEMP-TABLE tt-es-klassmatt-log-tab.
EMPTY TEMP-TABLE tt-es-klassmatt-log-qf.

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
  {src/adm/template/snd-list.i "tt-es-klassmatt-log-tab"}
  {src/adm/template/snd-list.i "tt-es-klassmatt-log-qf"}

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

