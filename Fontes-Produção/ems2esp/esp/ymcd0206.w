&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5_esp         PROGRESS
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
{include/i-prgvrs.i YMCD0206 11.5.11.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i YMCD0206 MCD}
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


def var raw-param        as raw no-undo.        
                                                
def temp-table tt-raw-digita                    
   field raw-digita      as raw.                
/* Main-Block */  




define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-fila

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-fila-rep-item

/* Definitions for BROWSE br-fila                                       */
&Scoped-define FIELDS-IN-QUERY-br-fila es-fila-rep-item.ep-codigo ~
es-fila-rep-item.it-codigo es-fila-rep-item.desc-item es-fila-rep-item.un ~
es-fila-rep-item.fm-codigo es-fila-rep-item.dt-ult-tentativa ~
es-fila-rep-item.nr-tentativas es-fila-rep-item.mensagem-erro ~
es-fila-rep-item.ge-codigo es-fila-rep-item.fm-cod-com ~
es-fila-rep-item.cod-estabel es-fila-rep-item.class-fiscal ~
es-fila-rep-item.dec-1 es-fila-rep-item.quant-segur ~
es-fila-rep-item.tipo-contr es-fila-rep-item.cod-servico ~
es-fila-rep-item.codigo-orig es-fila-rep-item.codigo-refer ~
es-fila-rep-item.cod-depto 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-fila 
&Scoped-define QUERY-STRING-br-fila FOR EACH es-fila-rep-item ~
      WHERE es-fila-rep-item.ep-codigo >= c-emp-ini ~
 AND es-fila-rep-item.ep-codigo <= c-emp-fim ~
 AND es-fila-rep-item.it-codigo >= c-item-ini ~
 AND es-fila-rep-item.it-codigo <= c-item-fim NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-fila OPEN QUERY br-fila FOR EACH es-fila-rep-item ~
      WHERE es-fila-rep-item.ep-codigo >= c-emp-ini ~
 AND es-fila-rep-item.ep-codigo <= c-emp-fim ~
 AND es-fila-rep-item.it-codigo >= c-item-ini ~
 AND es-fila-rep-item.it-codigo <= c-item-fim NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-fila es-fila-rep-item
&Scoped-define FIRST-TABLE-IN-QUERY-br-fila es-fila-rep-item


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-fila}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-param rt-button IMAGE-1 bt-implanta ~
IMAGE-2 IMAGE-3 IMAGE-4 c-emp-ini c-emp-fim c-item-ini c-item-fim ~
bt-confirma br-fila 
&Scoped-Define DISPLAYED-OBJECTS c-emp-ini c-emp-fim c-item-ini c-item-fim 

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
       SUB-MENU  mi-programa    LABEL "&Arquivo"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "Button 1" 
     SIZE 5.14 BY 1.

DEFINE BUTTON bt-implanta 
     IMAGE-UP FILE "ems2/image/toolbar/im-imp.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Implantar" 
     SIZE 4 BY 1.25 TOOLTIP "Implantar Item na empresa corrente".

DEFINE BUTTON bt-param 
     IMAGE-UP FILE "image/toolbar/im-param2.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 5 BY 1.33.

DEFINE VARIABLE c-emp-fim AS CHARACTER FORMAT "X(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-emp-ini AS CHARACTER FORMAT "X(3)" 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-item-fim AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE c-item-ini AS CHARACTER FORMAT "X(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-2
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-3
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-4
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-fila FOR 
      es-fila-rep-item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-fila
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-fila w-livre _STRUCTURED
  QUERY br-fila NO-LOCK DISPLAY
      es-fila-rep-item.ep-codigo FORMAT "x(3)":U
      es-fila-rep-item.it-codigo FORMAT "x(16)":U
      es-fila-rep-item.desc-item FORMAT "x(60)":U WIDTH 30
      es-fila-rep-item.un FORMAT "xx":U
      es-fila-rep-item.fm-codigo FORMAT "x(8)":U
      es-fila-rep-item.dt-ult-tentativa FORMAT "99/99/9999 HH:MM:SS":U
      es-fila-rep-item.nr-tentativas FORMAT ">>>,>>>,>>9":U
      es-fila-rep-item.mensagem-erro FORMAT "x(2000)":U WIDTH 200
      es-fila-rep-item.ge-codigo FORMAT ">9":U
      es-fila-rep-item.fm-cod-com FORMAT "x(8)":U
      es-fila-rep-item.cod-estabel FORMAT "x(5)":U
      es-fila-rep-item.class-fiscal FORMAT "9999.99.99":U
      es-fila-rep-item.dec-1 FORMAT "->>>>>>>>>>>9.99999999":U
      es-fila-rep-item.quant-segur FORMAT ">>>>,>>9.9999":U
      es-fila-rep-item.tipo-contr FORMAT ">9":U
      es-fila-rep-item.cod-servico FORMAT ">>>>9":U
      es-fila-rep-item.codigo-orig FORMAT ">9":U
      es-fila-rep-item.codigo-refer FORMAT "x(20)":U
      es-fila-rep-item.cod-depto FORMAT ">>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 10.25
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-param AT ROW 1.08 COL 67 WIDGET-ID 22
     bt-implanta AT ROW 1.17 COL 1.86 HELP
          "Implantar Item na empresa corrente" WIDGET-ID 20
     c-emp-ini AT ROW 2.54 COL 28.14 COLON-ALIGNED WIDGET-ID 6
     c-emp-fim AT ROW 2.54 COL 42.43 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     c-item-ini AT ROW 3.54 COL 15.14 COLON-ALIGNED WIDGET-ID 14
     c-item-fim AT ROW 3.54 COL 42.43 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     bt-confirma AT ROW 3.54 COL 85.43 WIDGET-ID 2
     br-fila AT ROW 4.75 COL 1 WIDGET-ID 200
     rt-button AT ROW 1 COL 1
     IMAGE-1 AT ROW 2.54 COL 35.57 WIDGET-ID 8
     IMAGE-2 AT ROW 2.54 COL 41 WIDGET-ID 10
     IMAGE-3 AT ROW 3.54 COL 35.57 WIDGET-ID 16
     IMAGE-4 AT ROW 3.54 COL 41 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.04
         FONT 4 WIDGET-ID 100.


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
         HEIGHT             = 14.13
         WIDTH              = 90
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90
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
/* BROWSE-TAB br-fila bt-confirma f-cad */
ASSIGN 
       br-fila:NUM-LOCKED-COLUMNS IN FRAME f-cad     = 2.

ASSIGN 
       es-fila-rep-item.mensagem-erro:AUTO-RESIZE IN BROWSE br-fila = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-fila
/* Query rebuild information for BROWSE br-fila
     _TblList          = "ems5_esp.es-fila-rep-item"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "ems5_esp.es-fila-rep-item.ep-codigo >= c-emp-ini
 AND ems5_esp.es-fila-rep-item.ep-codigo <= c-emp-fim
 AND ems5_esp.es-fila-rep-item.it-codigo >= c-item-ini
 AND ems5_esp.es-fila-rep-item.it-codigo <= c-item-fim"
     _FldNameList[1]   = ems5_esp.es-fila-rep-item.ep-codigo
     _FldNameList[2]   = ems5_esp.es-fila-rep-item.it-codigo
     _FldNameList[3]   > ems5_esp.es-fila-rep-item.desc-item
"es-fila-rep-item.desc-item" ? ? "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = ems5_esp.es-fila-rep-item.un
     _FldNameList[5]   = ems5_esp.es-fila-rep-item.fm-codigo
     _FldNameList[6]   = ems5_esp.es-fila-rep-item.dt-ult-tentativa
     _FldNameList[7]   = ems5_esp.es-fila-rep-item.nr-tentativas
     _FldNameList[8]   > ems5_esp.es-fila-rep-item.mensagem-erro
"es-fila-rep-item.mensagem-erro" ? ? "character" ? ? ? ? ? ? no ? no no "200" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = ems5_esp.es-fila-rep-item.ge-codigo
     _FldNameList[10]   = ems5_esp.es-fila-rep-item.fm-cod-com
     _FldNameList[11]   = ems5_esp.es-fila-rep-item.cod-estabel
     _FldNameList[12]   = ems5_esp.es-fila-rep-item.class-fiscal
     _FldNameList[13]   = ems5_esp.es-fila-rep-item.dec-1
     _FldNameList[14]   = ems5_esp.es-fila-rep-item.quant-segur
     _FldNameList[15]   = ems5_esp.es-fila-rep-item.tipo-contr
     _FldNameList[16]   = ems5_esp.es-fila-rep-item.cod-servico
     _FldNameList[17]   = ems5_esp.es-fila-rep-item.codigo-orig
     _FldNameList[18]   = ems5_esp.es-fila-rep-item.codigo-refer
     _FldNameList[19]   = ems5_esp.es-fila-rep-item.cod-depto
     _Query            is OPENED
*/  /* BROWSE br-fila */
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


&Scoped-define BROWSE-NAME br-fila
&Scoped-define SELF-NAME br-fila
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-fila w-livre
ON MOUSE-SELECT-DBLCLICK OF br-fila IN FRAME f-cad
DO:
  IF AVAIL es-fila-rep-item THEN
      MESSAGE REPLACE(es-fila-rep-item.mensagem-erro,"|",CHR(13))
          VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Mensagens/Erros de Replica‡Æo".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma w-livre
ON CHOOSE OF bt-confirma IN FRAME f-cad /* Button 1 */
DO:
  assign input frame {&frame-name} c-emp-ini c-emp-fim c-item-ini c-item-fim.
  {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-implanta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-implanta w-livre
ON CHOOSE OF bt-implanta IN FRAME f-cad /* Implantar */
DO:

EMPTY TEMP-TABLE tt-param.

CREATE tt-param.
ASSIGN tt-param.usuario         = c-seg-usuario
       tt-param.destino         = 2 /* Arquivo */
       tt-param.data-exec       = TODAY 
       tt-param.hora-exec       = TIME.
       tt-param.arquivo         = SESSION:TEMP-DIRECTORY + "ymcd0203.tmp":U 
       .

RAW-TRANSFER tt-param TO raw-param.
RUN esp/ymcd0202rp.p (INPUT raw-param,
                      INPUT TABLE tt-raw-digita).



























  APPLY "choose" TO bt-confirma.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param w-livre
ON CHOOSE OF bt-param IN FRAME f-cad /* Button 1 */
DO:
  RUN esp/ymcd0206a.w.
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
ON MENU-DROP OF MENU mi-programa /* Arquivo */
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
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
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
  DISPLAY c-emp-ini c-emp-fim c-item-ini c-item-fim 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE bt-param rt-button IMAGE-1 bt-implanta IMAGE-2 IMAGE-3 IMAGE-4 
         c-emp-ini c-emp-fim c-item-ini c-item-fim bt-confirma br-fila 
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

  {utp/ut9000.i "YMCD0206" "11.5.11.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
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
  {src/adm/template/snd-list.i "es-fila-rep-item"}

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

