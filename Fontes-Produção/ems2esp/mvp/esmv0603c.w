&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESMV0603C 2.03.00.000}  /*** 010002 ***/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESMV0603C
&GLOBAL-DEFINE Version        2.03.00.000

&GLOBAL-DEFINE WindowType    Detail

&GLOBAL-DEFINE Folder         no
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   

&GLOBAL-DEFINE page0Widgets   btOK btCancel btHelp2 ~
                              tgAtivos tgProprios tgInativos tgTerceiros ~
                              tgSomenteCorretivas rsPareto

{mvp/esmv0603.i} /** Defini��o da ttSelecao **/

/* Parameters Definitions ---                                           */
define input-output parameter table for ttSelecao.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 rtToolBar RECT-4 RECT-5 rsPareto ~
tgAtivos tgInativos tgProprios tgTerceiros tgSomenteCorretivas btOK ~
btCancel btHelp2 fiPareto fiTexto1 fiTipoManutencao 
&Scoped-Define DISPLAYED-OBJECTS rsPareto tgAtivos tgInativos tgProprios ~
tgTerceiros tgSomenteCorretivas fiPareto fiTexto1 fiTipoManutencao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE fiPareto AS CHARACTER FORMAT "X(256)":U INITIAL "Equipamentos" 
      VIEW-AS TEXT 
     SIZE 10 BY .67 NO-UNDO.

DEFINE VARIABLE fiTexto1 AS CHARACTER FORMAT "X(256)":U INITIAL "Equipamentos" 
      VIEW-AS TEXT 
     SIZE 10 BY .67 NO-UNDO.

DEFINE VARIABLE fiTipoManutencao AS CHARACTER FORMAT "X(256)":U INITIAL "Tipo Manuten��o" 
      VIEW-AS TEXT 
     SIZE 13 BY .67 NO-UNDO.

DEFINE VARIABLE rsPareto AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Equipamento", 1,
"Modelo", 2
     SIZE 28 BY 1.75 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 2.75.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 1.5.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 2.71.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 50 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tgAtivos AS LOGICAL INITIAL no 
     LABEL "Ativos" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgInativos AS LOGICAL INITIAL no 
     LABEL "Inativos" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tgProprios AS LOGICAL INITIAL no 
     LABEL "Pr�prios" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgSomenteCorretivas AS LOGICAL INITIAL no 
     LABEL "Somente Considerar Manuten��es Corretivas" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .83 NO-UNDO.

DEFINE VARIABLE tgTerceiros AS LOGICAL INITIAL no 
     LABEL "Terceiros" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     rsPareto AT ROW 1.79 COL 5 NO-LABEL WIDGET-ID 6
     tgAtivos AT ROW 4.75 COL 7.29
     tgInativos AT ROW 4.75 COL 27.57
     tgProprios AT ROW 5.75 COL 7.29
     tgTerceiros AT ROW 5.75 COL 27.57
     tgSomenteCorretivas AT ROW 7.88 COL 5.57
     btOK AT ROW 9.21 COL 2
     btCancel AT ROW 9.21 COL 13
     btHelp2 AT ROW 9.21 COL 40
     fiPareto AT ROW 1 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     fiTexto1 AT ROW 4 COL 1.43 COLON-ALIGNED NO-LABEL
     fiTipoManutencao AT ROW 7.13 COL 1.43 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 4.25 COL 1
     rtToolBar AT ROW 9 COL 1
     RECT-4 AT ROW 7.38 COL 1.14
     RECT-5 AT ROW 1.29 COL 1 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 50 BY 10.08
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 10.08
         WIDTH              = 50
         MAX-HEIGHT         = 12.75
         MAX-WIDTH          = 50
         VIRTUAL-HEIGHT     = 12.75
         VIRTUAL-WIDTH      = 50
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWindow 
/* ************************* Included-Libraries *********************** */

{window/window.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWindow
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON END-ERROR OF wWindow
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON WINDOW-CLOSE OF wWindow
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wWindow
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wWindow
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wWindow
ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
    run piGrava in this-procedure.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*--- L�gica para inicializa��o do programam ---*/
{window/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wWindow 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     afterInitializeInterface
  Parameters:  <none>
  Notes:       Override ap�s inicializa��o da tela.
------------------------------------------------------------------------------*/

/** Labels dos campos **/
{utp/ut-liter.i "Equipamento"}
assign fiTexto1:screen-value in frame fPage0 = return-value.
{utp/ut-liter.i "Ativos"}
assign tgAtivos:label in frame fPage0 = return-value.
{utp/ut-liter.i "Inativos"}
assign tgInativos:label in frame fPage0 = return-value.
{utp/ut-liter.i "Pr�prios"}
assign tgProprios:label in frame fPage0 = return-value.
{utp/ut-liter.i "Terceiros"}
assign tgTerceiros:label in frame fPage0 = return-value.
{utp/ut-liter.i "Tipo Manuten��o"}
assign fiTipoManutencao:screen-value in frame fPage0 = return-value.
{utp/ut-liter.i "Somente Considerar Manuten��es Corretivas"}
assign tgSomenteCorretivas:label in frame fPage0 = return-value.
{utp/ut-liter.i "Pareto"}
assign fiPareto:screen-value in frame fPage0 = return-value.

/** Busca Par�mtros **/
find first ttSelecao no-lock no-error.
if avail ttSelecao then do:
    assign tgAtivos:checked             in frame fPage0 = ttSelecao.lAtivos         
           tgProprios:checked           in frame fPage0 = ttSelecao.lProprios       
           tgInativos:checked           in frame fPage0 = ttSelecao.lInativos       
           tgTerceiros:checked          in frame fPage0 = ttSelecao.lTerceiros
           tgSomenteCorretivas:checked  in frame fpage0 = ttSelecao.lSomenteCorretivas
           .

    IF ttSelecao.iTipoDispo = 1 THEN DO:
        ASSIGN rsPareto:SENSITIVE IN FRAME fpage0 = NO 
               rsPareto = 1.
    END.
    ELSE DO:
        ASSIGN rsPareto:SENSITIVE IN FRAME fpage0 = YES
               rsPareto:SCREEN-VALUE IN FRAME fpage0 = STRING(ttSelecao.iPareto)
               rsPareto = ttSelecao.iPareto.
    END.
end.

RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGrava wWindow 
PROCEDURE piGrava :
/*------------------------------------------------------------------------------
  Purpose:     piGrava
  Parameters:  <none>
  Notes:       Grava os par�metros selecionados na temp-table
------------------------------------------------------------------------------*/

find first ttSelecao exclusive-lock no-error.
if avail ttSelecao then do:
    assign ttSelecao.lAtivos            = tgAtivos:checked              in frame fPage0    
           ttSelecao.lProprios          = tgProprios:checked            in frame fPage0    
           ttSelecao.lInativos          = tgInativos:checked            in frame fPage0    
           ttSelecao.lTerceiros         = tgTerceiros:checked           in frame fPage0
           ttSelecao.lSomenteCorretivas = tgSomenteCorretivas:checked   in frame fpage0
           ttSelecao.iPareto            = INPUT FRAME fpage0 rsPareto
        .
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

