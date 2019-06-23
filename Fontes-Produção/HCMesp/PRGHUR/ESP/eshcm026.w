&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i ESHCM026 2.10.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR i-emp-ini AS CHAR NO-UNDO.
DEF VAR i-emp-fim AS CHAR NO-UNDO.
DEF VAR i-estab-ini AS CHAR NO-UNDO.
DEF VAR i-estab-fim AS CHAR NO-UNDO.
DEF VAR i-func-ini AS INT NO-UNDO.
DEF VAR i-func-fim AS INT NO-UNDO.
DEFINE VARIABLE c-evento-ini AS CHAR NO-UNDO.
DEFINE VARIABLE c-evento-fim AS CHAR NO-UNDO.
DEFINE VARIABLE c-ccusto-ini AS CHAR NO-UNDO.
DEFINE VARIABLE c-ccusto-fim AS CHAR NO-UNDO.
DEF VAR i-mes-ini AS INT NO-UNDO.
DEF VAR i-mes-fim AS INT NO-UNDO.
DEF VAR i-ano-ini AS INT NO-UNDO.
DEF VAR i-ano-fim AS INT NO-UNDO.

DEFINE VARIABLE i-linha AS INTEGER     NO-UNDO.
DEFINE VARIABLE i AS INTEGER     NO-UNDO.

DEF VAR excelappl AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE c-dir-log AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-tp-folha AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS f-ini-empresa f-fim-empresa f-ini-estab ~
f-fim-estab f-ini-func f-fim-func f-ini-evento f-fim-evento f-ini-ccusto ~
f-fim-ccusto f-ini-mes f-fim-mes f-ini-ano f-fim-ano bt-executar BtnCancel ~
rt-button RECT-2 IMAGE-5 IMAGE-6 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 ~
IMAGE-15 IMAGE-16 RECT-3 IMAGE-17 IMAGE-18 IMAGE-19 IMAGE-20 IMAGE-21 ~
IMAGE-22 
&Scoped-Define DISPLAYED-OBJECTS f-ini-empresa f-fim-empresa f-ini-estab ~
f-fim-estab f-ini-func f-fim-func f-ini-evento f-fim-evento f-ini-ccusto ~
f-fim-ccusto f-ini-mes f-fim-mes f-ini-ano f-fim-ano 

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
       SUB-MENU  mi-programa    LABEL "&ESHCM026"     
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE f-fim-ano AS INTEGER FORMAT "9999":U INITIAL 9999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-fim-ccusto AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-fim-empresa AS INTEGER FORMAT ">>9":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-fim-estab AS INTEGER FORMAT ">>9":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-fim-evento AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-fim-func AS INTEGER FORMAT ">>>>>>>9":U INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-fim-mes AS INTEGER FORMAT "99":U INITIAL 12 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-ini-ano AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Ano" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-ini-ccusto AS CHARACTER FORMAT "X(8)":U 
     LABEL "Centro Custo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-ini-empresa AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-ini-estab AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-ini-evento AS CHARACTER FORMAT "X(3)":U 
     LABEL "Evento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-ini-func AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Funcion rio" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-ini-mes AS INTEGER FORMAT "99":U INITIAL 1 
     LABEL "Mˆs" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89.72 BY 12.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89.72 BY 2.63.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     f-ini-empresa AT ROW 4.54 COL 20.86 COLON-ALIGNED WIDGET-ID 118
     f-fim-empresa AT ROW 4.5 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     f-ini-estab AT ROW 5.75 COL 21 COLON-ALIGNED WIDGET-ID 120
     f-fim-estab AT ROW 5.75 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     f-ini-func AT ROW 7 COL 21 COLON-ALIGNED WIDGET-ID 124
     f-fim-func AT ROW 7 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     f-ini-evento AT ROW 8.25 COL 21 COLON-ALIGNED WIDGET-ID 122
     f-fim-evento AT ROW 8.25 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     f-ini-ccusto AT ROW 9.5 COL 21 COLON-ALIGNED WIDGET-ID 116
     f-fim-ccusto AT ROW 9.5 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     f-ini-mes AT ROW 10.75 COL 21 COLON-ALIGNED WIDGET-ID 126
     f-fim-mes AT ROW 10.75 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 112
     f-ini-ano AT ROW 12 COL 21 COLON-ALIGNED WIDGET-ID 114
     f-fim-ano AT ROW 12 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     bt-executar AT ROW 15.75 COL 28 WIDGET-ID 48
     BtnCancel AT ROW 15.75 COL 46 WIDGET-ID 50
     rt-button AT ROW 1 COL 1
     RECT-2 AT ROW 2.75 COL 1 WIDGET-ID 10
     IMAGE-5 AT ROW 4.5 COL 40 WIDGET-ID 20
     IMAGE-6 AT ROW 4.5 COL 44.14 WIDGET-ID 22
     IMAGE-11 AT ROW 5.75 COL 40 WIDGET-ID 24
     IMAGE-12 AT ROW 5.75 COL 44.14 WIDGET-ID 26
     IMAGE-13 AT ROW 7 COL 40 WIDGET-ID 28
     IMAGE-14 AT ROW 7 COL 44.14 WIDGET-ID 30
     IMAGE-15 AT ROW 8.25 COL 40 WIDGET-ID 32
     IMAGE-16 AT ROW 8.25 COL 44.14 WIDGET-ID 34
     RECT-3 AT ROW 14.75 COL 1 WIDGET-ID 98
     IMAGE-17 AT ROW 9.5 COL 40 WIDGET-ID 128
     IMAGE-18 AT ROW 9.5 COL 44.14 WIDGET-ID 130
     IMAGE-19 AT ROW 10.75 COL 40 WIDGET-ID 132
     IMAGE-20 AT ROW 10.75 COL 44.14 WIDGET-ID 134
     IMAGE-21 AT ROW 12 COL 40 WIDGET-ID 136
     IMAGE-22 AT ROW 12 COL 44.14 WIDGET-ID 138
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.46
         FONT 1 WIDGET-ID 100.


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
         TITLE              = "Relat¢rio C lculo Movto Folha Pgto"
         HEIGHT             = 16.5
         WIDTH              = 90.43
         MAX-HEIGHT         = 26.13
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 26.13
         VIRTUAL-WIDTH      = 182.86
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
   FRAME-NAME Custom                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Relat¢rio C lculo Movto Folha Pgto */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Relat¢rio C lculo Movto Folha Pgto */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-livre
ON CHOOSE OF bt-executar IN FRAME f-cad /* Executar */
DO:
  RUN pi-valida-selecao.
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
ON MENU-DROP OF MENU mi-programa /* ESHCM026 */
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
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             f-ini-empresa:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY f-ini-empresa f-fim-empresa f-ini-estab f-fim-estab f-ini-func 
          f-fim-func f-ini-evento f-fim-evento f-ini-ccusto f-fim-ccusto 
          f-ini-mes f-fim-mes f-ini-ano f-fim-ano 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE f-ini-empresa f-fim-empresa f-ini-estab f-fim-estab f-ini-func 
         f-fim-func f-ini-evento f-fim-evento f-ini-ccusto f-fim-ccusto 
         f-ini-mes f-fim-mes f-ini-ano f-fim-ano bt-executar BtnCancel 
         rt-button RECT-2 IMAGE-5 IMAGE-6 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 
         IMAGE-15 IMAGE-16 RECT-3 IMAGE-17 IMAGE-18 IMAGE-19 IMAGE-20 IMAGE-21 
         IMAGE-22 
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

  {utp/ut9000.i "ESHCM026" "2.10.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-livre 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN i-emp-ini    = f-ini-empresa :SCREEN-VALUE IN FRAME {&FRAME-NAME}
       i-emp-fim    = f-fim-empresa :SCREEN-VALUE IN FRAME {&FRAME-NAME}
       i-estab-ini  = f-ini-estab   :SCREEN-VALUE IN FRAME {&FRAME-NAME}
       i-estab-fim  = f-fim-estab   :SCREEN-VALUE IN FRAME {&FRAME-NAME}
       i-func-ini   = int(f-ini-func:SCREEN-VALUE IN FRAME {&FRAME-NAME})
       i-func-fim   = int(f-fim-func:SCREEN-VALUE IN FRAME {&FRAME-NAME})
       c-evento-ini = f-ini-evento  :SCREEN-VALUE IN FRAME {&FRAME-NAME}
       c-evento-fim = f-fim-evento  :SCREEN-VALUE IN FRAME {&FRAME-NAME}
       c-ccusto-ini = f-ini-ccusto  :SCREEN-VALUE IN FRAME {&FRAME-NAME}
       c-ccusto-fim = f-fim-ccusto  :SCREEN-VALUE IN FRAME {&FRAME-NAME}
       i-mes-ini    = int(f-ini-mes:SCREEN-VALUE IN FRAME {&FRAME-NAME})
       i-mes-fim    = int(f-fim-mes:SCREEN-VALUE IN FRAME {&FRAME-NAME})
       i-ano-ini    = int(f-ini-ano:SCREEN-VALUE IN FRAME {&FRAME-NAME})
       i-ano-fim    = int(f-fim-ano:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

ASSIGN c-dir-log = SESSION:TEMP-DIRECTORY + "movtoCalcFP" + STRING(TODAY,"99999999") + ".xls".

RUN pi-inicia-excel.

DISP "Gerando Excel, Aguarde..." WITH FRAME f-a VIEW-AS DIALOG-BOX COLOR red/white.
     
excelappl:range("A1"):VALUE = "Empresa".
excelappl:range("B1"):VALUE = "Estabelecimento".
excelappl:range("C1"):VALUE = "Cod Func".
excelappl:range("D1"):VALUE = "Nome Funcion rio".
excelappl:range("E1"):VALUE = "Tipo MÆo Obra".
excelappl:range("F1"):VALUE = "Centro de Custo".
excelappl:range("G1"):VALUE = "Cod Evento".
excelappl:range("H1"):VALUE = "Descri‡Æo Evento".
excelappl:range("I1"):VALUE = "Valor Evento".
excelappl:range("J1"):VALUE = "Qtde Horas Evento".
excelappl:range("K1"):VALUE = "Mˆs".
excelappl:range("L1"):VALUE = "Ano".
excelappl:range("M1"):VALUE = "Tipo Folha".

ASSIGN i-linha = 2.

FOR EACH movto_calcul_func 
    WHERE movto_calcul_func.cdn_empresa      >= i-emp-ini   
    AND   movto_calcul_func.cdn_empresa      <= i-emp-fim   
    AND   movto_calcul_func.cdn_estab        >= i-estab-ini 
    AND   movto_calcul_func.cdn_estab        <= i-estab-fim 
    AND   movto_calcul_func.num_ano_refer_fp >= i-ano-ini
    AND   movto_calcul_func.num_ano_refer_fp <= i-ano-fim
    AND   movto_calcul_func.num_mes_refer_fp >= i-mes-ini
    AND   movto_calcul_func.num_mes_refer_fp <= i-mes-fim    
    AND   movto_calcul_func.cod_rh_ccusto    >= c-ccusto-ini
    AND   movto_calcul_func.cod_rh_ccusto    <= c-ccusto-fim,
        EACH funcionario OF movto_calcul_func
        WHERE funcionario.cdn_funcionario    >= i-func-ini
        AND   funcionario.cdn_funcionario    <= i-func-fim            
        BREAK BY movto_calcul_func.cdn_empresa:

   
         REPEAT i = 1 TO movto_calcul_func.qti_efp:
    
            FIND FIRST event_fp 
            WHERE event_fp.cdn_event_fp = movto_calcul_func.cdn_event_fp[i] NO-LOCK NO-ERROR.


            excelappl:range("A" + STRING(i-linha)):VALUE = funcionario.cdn_empresa.           
            excelappl:range("B" + STRING(i-linha)):VALUE = funcionario.cdn_estab.   
            excelappl:range("C" + STRING(i-linha)):VALUE = funcionario.cdn_funcionario.          
            excelappl:range("D" + STRING(i-linha)):VALUE = funcionario.nom_pessoa_fisic.
            excelappl:range("E" + STRING(i-linha)):VALUE = funcionario.cod_tip_mdo.
            excelappl:range("F" + STRING(i-linha)):VALUE = movto_calcul_func.cod_rh_ccusto.
            excelappl:range("G" + STRING(i-linha)):VALUE = event_fp.cdn_event_fp.
            excelappl:range("H" + STRING(i-linha)):VALUE = event_fp.des_event.  
            excelappl:range("I" + STRING(i-linha)):VALUE = movto_calcul_func.val_calcul[i].      
            excelappl:range("J" + STRING(i-linha)):VALUE = movto_calcul_func.qtd_hrs_demonst_efp[i]. 
            excelappl:range("K" + STRING(i-linha)):VALUE = movto_calcul_func.num_mes_refer_fp. 
            excelappl:range("L" + STRING(i-linha)):VALUE = movto_calcul_func.num_ano_refer_fp.

            CASE movto_calcul_func.idi_tip_fp:
             WHEN 1 THEN
              ASSIGN c-tp-folha = "Normal".
             WHEN 2 THEN
              ASSIGN c-tp-folha = "Adiantamento Normal".
             WHEN 3 THEN
              ASSIGN c-tp-folha = "13 Sal rios".
             WHEN 4 THEN
              ASSIGN c-tp-folha = "Adiantamento 13 Sal rios".
            END.

            excelappl:range("M" + STRING(i-linha)):VALUE = c-tp-folha.        

            ASSIGN i-linha = i-linha + 1.
    
         END.                            
    
END.

RUN pi-finaliza-excel.
       

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-finaliza-excel w-livre 
PROCEDURE pi-finaliza-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ExcelAppl:range("A2"):SELECT.
excelappl:ActiveWindow:FreezePanes = True.

Excelappl:ActiveWindow:Zoom = 80.
excelappl:Range("A1:M1"):Font:Bold = TRUE.
excelappl:range("A1:M1"):AutoFilter(,,,).
excelappl:Cells:SELECT.
excelappl:Cells:EntireColumn:AutoFit.
ExcelAppl:range("A1"):SELECT.


HIDE FRAME f-a.
excelappl:Workbooks:Item(1):SaveAs(c-dir-log,,,,,,).
excelappl:VISIBLE = TRUE.
RELEASE OBJECT excelappl NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-inicia-excel w-livre 
PROCEDURE pi-inicia-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CREATE "Excel.Application" excelappl.    
excelappl:VISIBLE = FALSE.
excelappl:APPLICATION:DisplayAlerts = FALSE.                                                                  
excelappl:Workbooks:ADD().                                           
excelappl:worksheets:ITEM(1):SELECT.
excelAppl:Sheets(1):NAME  = 'MovtoCalcFP'.
excelAppl:Sheets('MovtoCalcFP'):Select.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-selecao w-livre 
PROCEDURE pi-valida-selecao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF INPUT FRAME {&FRAME-NAME} f-fim-empresa < INPUT FRAME {&FRAME-NAME} f-ini-empresa THEN
DO:
   MESSAGE "Intervalo de Valores Inv lidos - Empresa" SKIP
           "Favor digitar novamente."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "ENTRY":U TO f-ini-empresa IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END.

IF INPUT FRAME {&FRAME-NAME} f-fim-estab < INPUT FRAME {&FRAME-NAME} f-ini-estab THEN
DO:
   MESSAGE "Intervalo de Valores Inv lidos - Estabelecimento" SKIP
           "Favor digitar novamente."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "ENTRY":U TO f-ini-estab IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END.

IF INPUT FRAME {&FRAME-NAME} f-fim-func < INPUT FRAME {&FRAME-NAME} f-ini-func THEN
DO:
   MESSAGE "Intervalo de Valores Inv lidos - Funcion rio" SKIP
           "Favor digitar novamente."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "ENTRY":U TO f-ini-func IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END.

IF INPUT FRAME {&FRAME-NAME} f-fim-evento < INPUT FRAME {&FRAME-NAME} f-ini-evento THEN
DO:
   MESSAGE "Intervalo de Valores Inv lidos - Evento" SKIP
           "Favor digitar novamente."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "ENTRY":U TO f-ini-evento IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END.

IF INPUT FRAME {&FRAME-NAME} f-fim-ccusto < INPUT FRAME {&FRAME-NAME} f-ini-ccusto THEN
DO:
   MESSAGE "Intervalo de Valores Inv lidos - C Custo" SKIP
           "Favor digitar novamente."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "ENTRY":U TO f-ini-ccusto IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END.

IF INPUT FRAME {&FRAME-NAME} f-fim-mes < INPUT FRAME {&FRAME-NAME} f-ini-mes THEN
DO:
   MESSAGE "Intervalo de Valores Inv lidos - Mˆs" SKIP
           "Favor digitar novamente."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "ENTRY":U TO f-ini-mes IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END.

IF INPUT FRAME {&FRAME-NAME} f-fim-ano < INPUT FRAME {&FRAME-NAME} f-ini-ano THEN
DO:
   MESSAGE "Intervalo de Valores Inv lidos - Ano" SKIP
           "Favor digitar novamente."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "ENTRY":U TO f-ini-ano IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END.

RUN pi-executar.


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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-livre, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
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

