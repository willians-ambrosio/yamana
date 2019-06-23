&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME ap0804
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS ap0804 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i AP0804 2.00.00.020}  /*** 010020 ***/

/*------------------------------------------------------------------------

  File: ap0804.w

  Description: Consulta de fornecedores

  Input Parameters: 
      <none>

  Output Parameters: 
      <none>

  Version: 1.00.00

  History: Este programa representa os antigos programa ap0804, ap0807 e ap0814.
           O programa ap0814 era referente a consulta de matrizes, hoje para o usuario
           consultar um fornecedor Matriz, basta utilizar o recurso PESQUISA (ZOOM) do painel.
           Este zoom mostrar† o nome da matriz do emitente.
           Alguns F6 para detalhe foram transformados em pagias de folder e outros em sub-programas,
           sendo que alguns s∆o consultados via bot∆o detalhar e outros pelo bot∆o relacionamento.


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
&glob version 1.00.00
{cdp/cdcfgfin.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def new global shared var c-emitente  like emitente.cod-emitente.
def new global shared var gr-emitente as rowid no-undo.
def new global shared var i-natureza  as int   no-undo.

def var rw-emitente as rowid no-undo.

def buffer b-emitente for emitente.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-contato 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR ap0804 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-arquivo 
       MENU-ITEM mi-primeiro    LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM mi-anterior    LABEL "An&terior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM mi-proximo     LABEL "Pr&¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM mi-ultimo      LABEL "&Èltimo"        ACCELERATOR "CTRL-END"
       MENU-ITEM mi-va-para     LABEL "&V† para"       ACCELERATOR "CTRL-T"
       MENU-ITEM mi-pesquisa    LABEL "Pes&quisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-cadastro MENUBAR
       SUB-MENU  mi-arquivo     LABEL "&Arquivo"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b09ad180 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b09ad260 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q01ad098 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q01ad106 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v01ad106 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v02ad098 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v03ad098 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v04ad098 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v05ad098 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v29ad098 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v64ad098 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-contato 
     IMAGE-UP FILE "image~\gr-usu":U
     IMAGE-INSENSITIVE FILE "image~\gr-usu":U
     LABEL "Contato" 
     SIZE 4 BY 1.25 TOOLTIP "Contatos"
     FONT 4.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 88.57 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-contato AT ROW 1.33 COL 66 HELP
          "Contatos"
     rt-button AT ROW 1.21 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 8
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW ap0804 ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta Fornecedor"
         HEIGHT             = 17
         WIDTH              = 90
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-cadastro:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB ap0804 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-concom.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW ap0804
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   L-To-R                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(ap0804)
THEN ap0804:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME ap0804
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap0804 ap0804
ON END-ERROR OF ap0804 /* Consulta Fornecedor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap0804 ap0804
ON WINDOW-CLOSE OF ap0804 /* Consulta Fornecedor */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-contato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-contato ap0804
ON CHOOSE OF bt-contato IN FRAME f-cad /* Contato */
DO:
  RUN app/ap0804p.w .
  
  RUN dispatch IN THIS-PROCEDURE ('open-query':U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-contato ap0804
ON HELP OF bt-contato IN FRAME f-cad /* Contato */
DO:
  status default "Contatos do Fornecedor".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior ap0804
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo ap0804
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas ap0804
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo ap0804
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir ap0804
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa ap0804
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro ap0804
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo ap0804
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair ap0804
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
   RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre ap0804
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo ap0804
ON CHOOSE OF MENU-ITEM mi-ultimo /* Èltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para ap0804
ON CHOOSE OF MENU-ITEM mi-va-para /* V† para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK ap0804 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects ap0804  _ADM-CREATE-OBJECTS
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
             INPUT  'panel/p-navega.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navega ).
       RUN set-position IN h_p-navega ( 1.33 , 2.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 24.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.33 , 73.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'advwr/v64ad098.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v64ad098 ).
       RUN set-position IN h_v64ad098 ( 2.79 , 1.86 ) NO-ERROR.
       /* Size in UIB:  ( 3.54 , 88.14 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Fornecedor|Financeiro|Fiscal|Endereáo|Comunic|Estat°stica|Document|Pagtos' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 6.75 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 11.17 , 88.14 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adqry/q01ad098.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Key-Name = ,
                     ProgPesquisa = adzoom/z01ad098.w,
                     ProgVaPara = adgo/g01ad098.w,
                     ProgIncMod = ':U ,
             OUTPUT h_q01ad098 ).
       RUN set-position IN h_q01ad098 ( 1.00 , 54.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.67 , 8.29 ) */

       /* Links to SmartViewer h_v64ad098. */
       RUN add-link IN adm-broker-hdl ( h_q01ad098 , 'Record':U , h_v64ad098 ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartQuery h_q01ad098. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , h_q01ad098 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_q01ad098 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_q01ad098 ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804-v01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v02ad098 ).
       RUN set-position IN h_v02ad098 ( 8.00 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.67 , 84.14 ) */

       /* Links to SmartViewer h_v02ad098. */
       RUN add-link IN adm-broker-hdl ( h_q01ad098 , 'Record':U , h_v02ad098 ).
       RUN add-link IN adm-broker-hdl ( h_v64ad098 , 'group-assign':U , h_v02ad098 ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804-v02.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v03ad098 ).
       RUN set-position IN h_v03ad098 ( 8.46 , 6.57 ) NO-ERROR.
       /* Size in UIB:  ( 8.50 , 76.14 ) */

       /* Links to SmartViewer h_v03ad098. */
       RUN add-link IN adm-broker-hdl ( h_q01ad098 , 'Record':U , h_v03ad098 ).
       RUN add-link IN adm-broker-hdl ( h_v64ad098 , 'group-assign':U , h_v03ad098 ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804-v03.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v04ad098 ).
       RUN set-position IN h_v04ad098 ( 8.25 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.54 , 83.43 ) */

       /* Links to SmartViewer h_v04ad098. */
       RUN add-link IN adm-broker-hdl ( h_q01ad098 , 'Record':U , h_v04ad098 ).
       RUN add-link IN adm-broker-hdl ( h_v64ad098 , 'group-assign':U , h_v04ad098 ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804-v04.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v05ad098 ).
       RUN set-position IN h_v05ad098 ( 8.67 , 3.29 ) NO-ERROR.
       /* Size in UIB:  ( 8.25 , 82.43 ) */

       /* Links to SmartViewer h_v05ad098. */
       RUN add-link IN adm-broker-hdl ( h_q01ad098 , 'Record':U , h_v05ad098 ).
       RUN add-link IN adm-broker-hdl ( h_v64ad098 , 'group-assign':U , h_v05ad098 ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804-v05.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v29ad098 ).
       RUN set-position IN h_v29ad098 ( 8.50 , 7.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.79 , 62.43 ) */

       /* Links to SmartViewer h_v29ad098. */
       RUN add-link IN adm-broker-hdl ( h_q01ad098 , 'Record':U , h_v29ad098 ).

    END. /* Page 5 */

    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'advwr/v01ad106.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v01ad106 ).
       RUN set-position IN h_v01ad106 ( 9.00 , 6.72 ) NO-ERROR.
       /* Size in UIB:  ( 4.79 , 56.29 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adqry/q01ad106.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q01ad106 ).
       RUN set-position IN h_q01ad106 ( 8.25 , 70.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.67 , 8.29 ) */

       /* Links to SmartViewer h_v01ad106. */
       RUN add-link IN adm-broker-hdl ( h_q01ad106 , 'Record':U , h_v01ad106 ).

       /* Links to SmartQuery h_q01ad106. */
       RUN add-link IN adm-broker-hdl ( h_q01ad098 , 'Record':U , h_q01ad106 ).

    END. /* Page 6 */

    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804-b01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b09ad260 ).
       RUN set-position IN h_b09ad260 ( 8.38 , 2.72 ) NO-ERROR.
       /* Size in UIB:  ( 9.04 , 86.43 ) */

       /* Links to BrowseDigitacao h_b09ad260. */
       RUN add-link IN adm-broker-hdl ( h_q01ad098 , 'Record':U , h_b09ad260 ).
       RUN add-link IN adm-broker-hdl ( h_v64ad098 , 'group-assign':U , h_b09ad260 ).

    END. /* Page 7 */

    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adbrw/b09ad180.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b09ad180 ).
       RUN set-position IN h_b09ad180 ( 8.50 , 3.72 ) NO-ERROR.
       /* Size in UIB:  ( 8.21 , 84.57 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('6') NO-ERROR.

       /* Links to BrowserCadastro2 h_b09ad180. */
       RUN add-link IN adm-broker-hdl ( h_q01ad098 , 'Record':U , h_b09ad180 ).
       RUN add-link IN adm-broker-hdl ( h_v01ad106 , 'group-assign':U , h_b09ad180 ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 8 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available ap0804  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI ap0804  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(ap0804)
  THEN DELETE WIDGET ap0804.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI ap0804  _DEFAULT-ENABLE
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
  ENABLE rt-button bt-contato 
      WITH FRAME f-cad IN WINDOW ap0804.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW ap0804.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy ap0804 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit ap0804 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize ap0804 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  run pi-before-initialize.

  assign rw-emitente = gr-emitente.

{utp/ut9000.i "AP0804" "2.00.00.020"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  if gr-emitente <> ? then
   run PI-REPOSICIONA-QUERY in h_q01ad098(input gr-emitente).
  assign gr-emitente = rw-emitente. 

  /* Code placed here will execute AFTER standard behavior.    */
  /*  RUN set-prog-parent IN h_p-exihel (INPUT ""). */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_ems_xref ap0804 
PROCEDURE pi_ems_xref :
/*------------------------------------------------------------------------------
  Purpose:     Referºncias XRef
  Parameters:  <none> 
  Notes:       Nío altere/elimine o cΩdigo abaixo.
------------------------------------------------------------------------------*/
RUN adzoom/z01ad098.w.
RUN adgo/g01ad098.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RTB_xref_generator ap0804 
PROCEDURE RTB_xref_generator :
/* -----------------------------------------------------------
Purpose:    Generate RTB xrefs for SMARTOBJECTS.
Parameters: <none>
Notes:      This code is generated by the UIB.  DO NOT modify it.
            It is included for Roundtable Xref generation. Without
            it, Xrefs for SMARTOBJECTS could not be maintained by
            RTB.  It will in no way affect the operation of this
            program as it never gets executed.
-------------------------------------------------------------*/
  RUN "panel/p-navega.w *RTB-SmObj* ".
  RUN "panel/p-exihel.w *RTB-SmObj* ".
  RUN "advwr/v64ad098.w *RTB-SmObj* ".
  RUN "adm/objects/folder.w *RTB-SmObj* ".
  RUN "adqry/q01ad098.w *RTB-SmObj* ".
  RUN "app/ap0804-v01.w *RTB-SmObj* ".
  RUN "app/ap0804-v02.w *RTB-SmObj* ".
  RUN "app/ap0804-v03.w *RTB-SmObj* ".
  RUN "app/ap0804-v04.w *RTB-SmObj* ".
  RUN "app/ap0804-v05.w *RTB-SmObj* ".
  RUN "advwr/v01ad106.w *RTB-SmObj* ".
  RUN "adqry/q01ad106.w *RTB-SmObj* ".
  RUN "app/ap0804-b01.w *RTB-SmObj* ".
  RUN "adbrw/b09ad180.w *RTB-SmObj* ".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records ap0804  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed ap0804 
PROCEDURE state-changed :
/* -----------------------------------------------------------
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

