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
{include/i-prgvrs.i esmi0603C 2.03.00.000}  /*** 010002 ***/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        esmi0603C
&GLOBAL-DEFINE Version        2.03.00.000

&GLOBAL-DEFINE WindowType    Detail

&GLOBAL-DEFINE Folder         no
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   

&GLOBAL-DEFINE page0Widgets   btOK btCancel btHelp2 ~
                              tgAtivos tgSuspenso tgVendidos tgInutilizado ~
                              tgSomenteCorretivas fiNivel-tag rsDisponibilidade

{mip/esmi0603.i} /** Defini‡Æo da ttSelecao **/

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
&Scoped-Define ENABLED-OBJECTS RECT-1 rtToolBar RECT-4 RECT-5 RECT-79 ~
fiNivel-tag tgAtivos tgVendidos tgSuspenso tgInutilizado rsDisponibilidade ~
tgSomenteCorretivas btOK btCancel btHelp2 fiAvalia fiTexto1 ~
fiTipoManutencao 
&Scoped-Define DISPLAYED-OBJECTS fiNivel-tag tgAtivos tgVendidos tgSuspenso ~
tgInutilizado rsDisponibilidade tgSomenteCorretivas fiAvalia fiTexto1 ~
fiTipoManutencao 

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

DEFINE VARIABLE fiAvalia AS CHARACTER FORMAT "X(256)":U INITIAL "Avaliar:" 
      VIEW-AS TEXT 
     SIZE 6 BY .67 NO-UNDO.

DEFINE VARIABLE fiNivel-tag AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "N¡vel TAG" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fiTexto1 AS CHARACTER FORMAT "X(256)":U INITIAL "Equipamentos" 
      VIEW-AS TEXT 
     SIZE 10 BY .67 NO-UNDO.

DEFINE VARIABLE fiTipoManutencao AS CHARACTER FORMAT "X(256)":U INITIAL "Tipo Manuten‡Æo" 
     LABEL "" 
      VIEW-AS TEXT 
     SIZE 13 BY .67 NO-UNDO.

DEFINE VARIABLE rsDisponibilidade AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Encerramento O.M.", 1,
"Horas Apropriadas", 2
     SIZE 25 BY 1.5 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY 2.75.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY 1.5.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY 2.5.

DEFINE RECTANGLE RECT-79
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY 1.75 TOOLTIP "Disponibilidade".

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 50 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tgAtivos AS LOGICAL INITIAL no 
     LABEL "Ativos" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgInutilizado AS LOGICAL INITIAL no 
     LABEL "Inutilizado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgSomenteCorretivas AS LOGICAL INITIAL no 
     LABEL "Somente Considerar Manuten‡äes Corretivas" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .83 NO-UNDO.

DEFINE VARIABLE tgSuspenso AS LOGICAL INITIAL no 
     LABEL "Suspenso" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgVendidos AS LOGICAL INITIAL no 
     LABEL "Vendidos" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     fiNivel-tag AT ROW 2 COL 20 COLON-ALIGNED
     tgAtivos AT ROW 4.5 COL 7.29
     tgVendidos AT ROW 4.5 COL 27.57
     tgSuspenso AT ROW 5.5 COL 7.29
     tgInutilizado AT ROW 5.5 COL 27.57
     rsDisponibilidade AT ROW 7.08 COL 15 NO-LABEL
     tgSomenteCorretivas AT ROW 9.67 COL 5.57
     btOK AT ROW 10.96 COL 2
     btCancel AT ROW 10.96 COL 13
     btHelp2 AT ROW 10.96 COL 40
     fiAvalia AT ROW 1 COL 2 COLON-ALIGNED NO-LABEL
     fiTexto1 AT ROW 3.75 COL 1.43 COLON-ALIGNED NO-LABEL
     fiTipoManutencao AT ROW 8.92 COL 1.57 COLON-ALIGNED
     "Disponibilidade" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 6.75 COL 3
     RECT-1 AT ROW 4 COL 1
     rtToolBar AT ROW 10.75 COL 1
     RECT-4 AT ROW 9.17 COL 1.14
     RECT-5 AT ROW 1.25 COL 1
     RECT-79 AT ROW 7 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 50 BY 11.21
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 11.13
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


/*--- L¢gica para inicializa‡Æo do programam ---*/
{window/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wWindow 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     afterInitializeInterface
  Parameters:  <none>
  Notes:       Override ap¢s inicializa‡Æo da tela.
------------------------------------------------------------------------------*/

/** Labels dos campos **/
{utp/ut-liter.i "Equipamento"}
assign fiTexto1:screen-value in frame fPage0 = return-value.
{utp/ut-liter.i "Ativos"}
assign tgAtivos:label in frame fPage0 = return-value.
{utp/ut-liter.i "Vendidos"}
assign tgVendidos:label in frame fPage0 = return-value.
{utp/ut-liter.i "Suspensos"}
assign tgSuspenso:label in frame fPage0 = return-value.
{utp/ut-liter.i "Inutilizado"}
assign tgInutilizado:label in frame fPage0 = return-value.
{utp/ut-liter.i "Tipo Manuten‡Æo"}
assign fiTipoManutencao:screen-value in frame fPage0 = return-value.
{utp/ut-liter.i "Somente Considerar Manuten‡äes Corretivas"}
assign tgSomenteCorretivas:label in frame fPage0 = return-value.

{utp/ut-liter.i "Avaliar"}
assign fiAvalia:screen-value in frame fPage0 = return-value.





/** Busca Parƒmtros **/
find first ttSelecao no-lock no-error.
if avail ttSelecao then do:
    assign tgAtivos:checked              in frame fPage0 = ttSelecao.lAtivos         
           tgInutilizado:checked         in frame fPage0 = ttSelecao.lInutilizado       
           tgVendidos:checked            in frame fPage0 = ttSelecao.lVendido       
           tgSuspenso:checked            in frame fPage0 = ttSelecao.lSuspenso
           tgSomenteCorretivas:checked   in frame fpage0 = ttSelecao.lSomenteCorretivas
           fiNivel-tag:SCREEN-VALUE      in frame fpage0 = STRING(ttSelecao.nivel-tag)
           rsDisponibilidade             = ttSelecao.i-disponibilidade

        .

end.

DISPLAY rsDisponibilidade WITH FRAME fPage0.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGrava wWindow 
PROCEDURE piGrava :
/*------------------------------------------------------------------------------
  Purpose:     piGrava
  Parameters:  <none>
  Notes:       Grava os parƒmetros selecionados na temp-table
------------------------------------------------------------------------------*/
find first ttSelecao exclusive-lock no-error.
if avail ttSelecao then do:
    assign ttSelecao.lAtivos              = tgAtivos:checked              in frame fPage0    
           ttSelecao.lSuspenso            = tgSuspenso:checked            in frame fPage0    
           ttSelecao.lVendido             = tgVendidos:checked            in frame fPage0    
           ttSelecao.lInutilizado         = tgInutilizado:checked           in frame fPage0
           ttSelecao.lSomenteCorretivas   = tgSomenteCorretivas:checked   in frame fpage0
           ttSelecao.nivel-tag            = int(fiNivel-tag:SCREEN-VALUE)
           ttSelecao.i-disponibilidade    = input frame fPage0 rsDisponibilidade
        .
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

