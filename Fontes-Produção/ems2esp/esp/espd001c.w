&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESPD001C 2.00.00.004 } /*** 010004 ***/
 
/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
 
/*&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESPD005C ESP}
&ENDIF*/
 
CREATE WIDGET-POOL.
 
/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESPD001C
&GLOBAL-DEFINE Version        2.00.00.004
 
&GLOBAL-DEFINE WindowType     Master
 
&GLOBAL-DEFINE Folder         NO
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   
 
&GLOBAL-DEFINE page0Widgets   btOK btCancel btHelp2
&GLOBAL-DEFINE page1Widgets   BROWSE-4 c-campo-ini c-campo-fim btCheck btMarca-faixa btDesmarca-faixa
&GLOBAL-DEFINE page2Widgets   cb-campo
&GLOBAL-DEFINE page3Widgets   c-campo
 
/* Parameters Definitions ---                                           */
DEFINE INPUT        PARAMETER c-cod-campo  AS CHARACTER FORMAT "X(22)"  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER c-desc-valor AS CHARACTER FORMAT "X(360)" NO-UNDO.
 
DEFINE VARIABLE c-desc-valor-ORI AS CHARACTER FORMAT "X(360)" NO-UNDO.
 
DEFINE TEMP-TABLE tt-campo NO-UNDO
    FIELD marcar     AS CHAR FORMAT "X(1)"
    FIELD cod-campo  AS CHAR FORMAT "X(30)"
    FIELD desc-campo AS CHAR FORMAT "X(60)"
    INDEX id IS PRIMARY cod-campo.
 
DEFINE TEMP-TABLE tt-selecao LIKE tt-campo.
 
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE wh-pesquisa                      AS HANDLE    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE    NO-UNDO.
DEFINE VARIABLE i                                AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPag                             AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-quant-str                      AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-qnt-cod                        AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-total-string                   AS INTEGER   NO-UNDO.
DEFINE VARIABLE c-codigo                         AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME BROWSE-4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-campo

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 tt-campo.marcar tt-campo.cod-campo fnDesc() @ tt-campo.desc-campo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH tt-campo BY tt-campo.cod-campo
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH tt-campo BY tt-campo.cod-campo.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 tt-campo
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 tt-campo


/* Definitions for FRAME fPage1                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage1 ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar btOK btCancel btHelp2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDesc wWindow 
FUNCTION fnDesc RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU smFile 
       MENU-ITEM miQueryJoins   LABEL "&Consultas"    
       MENU-ITEM miReportsJoins LABEL "&Relat¢rios"   
       RULE
       MENU-ITEM miExit         LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU smHelp 
       MENU-ITEM miContents     LABEL "&Conte£do"     
       MENU-ITEM miAbout        LABEL "&Sobre..."     .

DEFINE MENU mbMain MENUBAR
       SUB-MENU  smFile         LABEL "&Arquivo"      
       SUB-MENU  smHelp         LABEL "&Ajuda"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE BUTTON btCheck 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "" 
     SIZE 4 BY 1.17.

DEFINE BUTTON btDesmarca-faixa 
     LABEL "[...]" 
     SIZE 4 BY 1.17 TOOLTIP "Desmarcar Faixa".

DEFINE BUTTON btMarca-faixa 
     LABEL "[***]" 
     SIZE 4 BY 1.17 TOOLTIP "Selecionar Faixa".

DEFINE VARIABLE c-campo-fim AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY .88 NO-UNDO.

DEFINE VARIABLE c-campo-ini AS CHARACTER FORMAT "X(60)":U 
     LABEL "Fill 1" 
     VIEW-AS FILL-IN 
     SIZE 23 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE cb-campo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Combo 1" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE c-campo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 1" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      tt-campo SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 wWindow _FREEFORM
  QUERY BROWSE-4 DISPLAY
      tt-campo.marcar COLUMN-LABEL ""
      tt-campo.cod-campo
      fnDesc() @ tt-campo.desc-campo
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80 BY 9.5
         FONT 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 13.92 COL 2
     btCancel AT ROW 13.92 COL 13
     btHelp2 AT ROW 13.92 COL 80
     rtToolBar AT ROW 13.71 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.17
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage1
     btCheck AT ROW 1.38 COL 70.72
     btMarca-faixa AT ROW 1.38 COL 75 WIDGET-ID 10
     btDesmarca-faixa AT ROW 1.38 COL 79.29 WIDGET-ID 8
     c-campo-ini AT ROW 1.54 COL 12.57 COLON-ALIGNED WIDGET-ID 2
     c-campo-fim AT ROW 1.54 COL 44.57 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     BROWSE-4 AT ROW 3 COL 3 WIDGET-ID 200
     IMAGE-1 AT ROW 1.54 COL 37.57
     IMAGE-2 AT ROW 1.54 COL 43.57
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE NO-VALIDATE THREE-D 
         AT COL 3.57 ROW 1.5
         SIZE 84.43 BY 11.83
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage3
     c-campo AT ROW 5.5 COL 20 COLON-ALIGNED WIDGET-ID 14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 1.5
         SIZE 84.43 BY 11.83
         FONT 1 WIDGET-ID 300.

DEFINE FRAME fPage2
     cb-campo AT ROW 5.25 COL 22 COLON-ALIGNED WIDGET-ID 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 1.5
         SIZE 84.43 BY 11.83
         FONT 1 WIDGET-ID 100.


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
         HEIGHT             = 14.17
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
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU mbMain:HANDLE.
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
/* REPARENT FRAME */
ASSIGN FRAME fPage1:FRAME = FRAME fpage0:HANDLE
       FRAME fPage2:FRAME = FRAME fpage0:HANDLE
       FRAME fPage3:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* BROWSE-TAB BROWSE-4 c-campo-fim fPage1 */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR FRAME fPage3
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-campo BY tt-campo.cod-campo.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage1
/* Query rebuild information for FRAME fPage1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fPage1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage3
/* Query rebuild information for FRAME fPage3
     _Query            is NOT OPENED
*/  /* FRAME fPage3 */
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


&Scoped-define BROWSE-NAME BROWSE-4
&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 wWindow
ON MOUSE-SELECT-DBLCLICK OF BROWSE-4 IN FRAME fPage1
DO:
  IF AVAIL tt-campo THEN DO:
      /*- Se ja estiver marcado, ser† desmarcado -*/
      IF tt-campo.marcar = "*" THEN DO:
 
          ASSIGN tt-campo.marcar = "".
 
          FIND FIRST tt-selecao WHERE tt-selecao.cod-campo = tt-campo.cod-campo NO-LOCK NO-ERROR.
          IF AVAIL tt-selecao THEN DO:
              
              DELETE tt-selecao.
          END.
      END.
      /*- Se ja estiver desmarcado, ser† marcado -*/    
      ELSE DO:
 
          ASSIGN tt-campo.marcar = "*".
 
          FIND FIRST tt-selecao WHERE tt-selecao.cod-campo = tt-campo.cod-campo NO-LOCK NO-ERROR.
          IF NOT AVAIL tt-selecao THEN DO:
              
              CREATE tt-selecao.
              ASSIGN tt-selecao.cod-campo = tt-campo.cod-campo.
          END.
      END.
 
      BROWSE-4:REFRESH().
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wWindow
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    ASSIGN c-desc-valor = c-desc-valor-ORI.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btCheck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCheck wWindow
ON CHOOSE OF btCheck IN FRAME fPage1
DO:
    RUN pi-add IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDesmarca-faixa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDesmarca-faixa wWindow
ON CHOOSE OF btDesmarca-faixa IN FRAME fPage1 /* [...] */
DO:
  find first tt-campo no-lock no-error.
  if avail tt-campo then
      for each tt-campo:
        update tt-campo.marcar = "".
      end.
 
  {&OPEN-QUERY-BROWSE-4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wWindow
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btMarca-faixa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMarca-faixa wWindow
ON CHOOSE OF btMarca-faixa IN FRAME fPage1 /* [***] */
DO:
  find first tt-campo no-lock no-error.
  if avail tt-campo then
      for each tt-campo:
        update tt-campo.marcar = "*".
      end.
 
  {&OPEN-QUERY-BROWSE-4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wWindow
ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
    RUN pi-seleciona IN THIS-PROCEDURE.
 
    RUN pi-salvar    IN THIS-PROCEDURE.
    APPLY "CLOSE":U  TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME c-campo-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-campo-fim wWindow
ON F5 OF c-campo-fim IN FRAME fPage1
DO:
    CASE c-cod-campo:                     
        WHEN "Estabelecimento Origem" THEN RUN pi-zoom-estabelec-fim    IN THIS-PROCEDURE.
        WHEN "Grupo Cliente"          THEN RUN pi-zoom-gr-cli-fim       IN THIS-PROCEDURE.
        WHEN "Grupo Estoque"          THEN RUN pi-zoom-ge-codigo-fim    IN THIS-PROCEDURE.
        WHEN "Classificaá∆o Fiscal"   THEN RUN pi-zoom-class-fiscal-fim IN THIS-PROCEDURE.
        WHEN "C¢digo do Item"         THEN RUN pi-zoom-item-fim         IN THIS-PROCEDURE.
        WHEN "Fam°lia Material"       THEN RUN pi-zoom-fam-mat-fim      IN THIS-PROCEDURE.
        WHEN "Fam°lia Comercial"      THEN RUN pi-zoom-fam-com-fim      IN THIS-PROCEDURE.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-campo-fim wWindow
ON MOUSE-SELECT-DBLCLICK OF c-campo-fim IN FRAME fPage1
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-campo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-campo-ini wWindow
ON F5 OF c-campo-ini IN FRAME fPage1 /* Fill 1 */
DO:
    CASE c-cod-campo:                     
        WHEN "Estabelecimento Origem" THEN RUN pi-zoom-estabelec-ini    IN THIS-PROCEDURE.
        WHEN "Grupo Cliente"          THEN RUN pi-zoom-gr-cli-ini       IN THIS-PROCEDURE.
        WHEN "Grupo Estoque"          THEN RUN pi-zoom-ge-codigo-ini    IN THIS-PROCEDURE.
        WHEN "Classificaá∆o Fiscal"   THEN RUN pi-zoom-class-fiscal-ini IN THIS-PROCEDURE.
        WHEN "C¢digo do Item"         THEN RUN pi-zoom-item-ini         IN THIS-PROCEDURE.
        WHEN "Fam°lia Material"       THEN RUN pi-zoom-fam-mat-ini      IN THIS-PROCEDURE.
        WHEN "Fam°lia Comercial"      THEN RUN pi-zoom-fam-com-ini      IN THIS-PROCEDURE.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-campo-ini wWindow
ON MOUSE-SELECT-DBLCLICK OF c-campo-ini IN FRAME fPage1 /* Fill 1 */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME miAbout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL miAbout wWindow
ON CHOOSE OF MENU-ITEM miAbout /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*:T--- L¢gica para inicializaá∆o do programam ---*/
{window/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wWindow 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN c-desc-valor-ORI = c-desc-valor.
 
    IF c-desc-valor = "*" THEN
        ASSIGN c-desc-valor = "".
 
    ASSIGN c-campo-ini        :LABEL IN FRAME fPage1    = IF c-cod-campo = "Estabelecimento Origem"
                                                          THEN "Estabelecimento"
                                                          ELSE c-cod-campo
           tt-campo.cod-campo :LABEL IN BROWSE BROWSE-4 = IF c-cod-campo = "Estabelecimento Origem"
                                                          THEN "Estab"
                                                          ELSE c-cod-campo.

    IF c-cod-campo = "Estabelecimento Origem" THEN
        ASSIGN tt-campo.desc-campo:LABEL IN BROWSE BROWSE-4 = "Nome".
    ELSE
        ASSIGN tt-campo.desc-campo:LABEL IN BROWSE BROWSE-4 = "Descriá∆o".

    CASE c-cod-campo:
        WHEN "Estabelecimento Origem" THEN ASSIGN c-campo-ini:FORMAT IN FRAME fPage1 = "X(5)"
                                                  c-campo-fim:FORMAT IN FRAME fPage1 = "X(5)".
        WHEN "Grupo Cliente"          THEN ASSIGN c-campo-ini:FORMAT IN FRAME fPage1 = "X(2)"
                                                  c-campo-fim:FORMAT IN FRAME fPage1 = "X(2)".
        WHEN "Grupo Estoque"          THEN ASSIGN c-campo-ini:FORMAT IN FRAME fPage1 = "X(2)"
                                                  c-campo-fim:FORMAT IN FRAME fPage1 = "X(2)".
        WHEN "Classificaá∆o Fiscal"   THEN ASSIGN c-campo-ini:FORMAT IN FRAME fPage1 = "XXXX.XX.XX"
                                                  c-campo-fim:FORMAT IN FRAME fPage1 = "XXXX.XX.XX".
        WHEN "C¢digo do Item"         THEN ASSIGN c-campo-ini:FORMAT IN FRAME fPage1 = "X(16)"
                                                  c-campo-fim:FORMAT IN FRAME fPage1 = "X(16)".
        WHEN "Fam°lia Material"       THEN ASSIGN c-campo-ini:FORMAT IN FRAME fPage1 = "X(8)"
                                                  c-campo-fim:FORMAT IN FRAME fPage1 = "X(8)".
        WHEN "Fam°lia Comercial"      THEN ASSIGN c-campo-ini:FORMAT IN FRAME fPage1 = "X(8)"
                                                  c-campo-fim:FORMAT IN FRAME fPage1 = "X(8)".
    END CASE.

    IF c-desc-valor <> "" THEN DO:

        ASSIGN i-quant-str = NUM-ENTRIES(c-desc-valor,";").
        DO i = 1 TO i-quant-str:

            ASSIGN i-qnt-cod = INT(LENGTH(entry(1,c-desc-valor,";")))
                   i-total-string  = LENGTH(c-desc-valor).

            CREATE tt-campo.
            ASSIGN tt-campo.cod-campo = substr(c-desc-valor,1,INT(i-qnt-cod))
                   tt-campo.marcar = "*"
                   c-desc-valor = substr(c-desc-valor,INT(INT(i-qnt-cod) + 2),INT(i-total-string)). /* ENTRY(1,c-desc-valor,";"). */

        END. /* DO i = 1 TO NUM-ENTRIES(c-desc-valor,";") */
    END. /* IF c-desc-valor <> "" */

    DISABLE cb-campo
        WITH FRAME fPage2.

    DISABLE c-campo
        WITH FRAME fPage3.

    ENABLE c-campo-ini
           c-campo-fim
           BROWSE-4
           btCheck
           /*btDelete*/
        WITH FRAME fPage1.

    FRAME fPage1:HIDDEN = NO.
    FRAME fPage2:HIDDEN = YES.
    FRAME fPage3:HIDDEN = YES.
    FRAME fPage2:MOVE-TO-BOTTOM().
    FRAME fPage3:MOVE-TO-BOTTOM().
    FRAME fPage1:MOVE-TO-TOP().

    c-campo-ini:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME fPage1.
    c-campo-fim:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME fPage1.

    ASSIGN iPag = 1.
 
    ASSIGN c-desc-valor = c-desc-valor-ORI.
 
    {&OPEN-QUERY-BROWSE-4}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-add wWindow 
PROCEDURE pi-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*- Elimina os registros que n∆o foram selecionados -*/
    FOR EACH tt-campo WHERE tt-campo.marcar <> "*" EXCLUSIVE-LOCK:
        DELETE tt-campo.
    END.
 
    ASSIGN INPUT FRAME fPage1 c-campo-ini
           INPUT FRAME fPage1 c-campo-fim.
 
    CASE c-cod-campo:
 
        WHEN "Estabelecimento Origem" THEN DO:
 
            FOR EACH estabelec NO-LOCK
                WHERE estabelec.cod-estabel >= c-campo-ini:screen-value
                AND   estabelec.cod-estabel <= c-campo-fim:screen-value :
 
                FIND FIRST tt-campo WHERE tt-campo.cod-campo = estabelec.cod-estabel 
                                      AND tt-campo.marcar    = "*" NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-campo THEN DO:
                
                    CREATE tt-campo.
                    ASSIGN tt-campo.cod-campo = estabelec.cod-estabel.
                END.
            END. /* EACH estabelec */
            RUN pi-seleciona.
        END.
        WHEN "Grupo Cliente"          THEN DO:
 
            FOR EACH gr-cli NO-LOCK
                WHERE gr-cli.cod-gr-cli >= INT(c-campo-ini)
                AND   gr-cli.cod-gr-cli <= INT(c-campo-fim):
 
                FIND FIRST tt-campo WHERE tt-campo.cod-campo = STRING(gr-cli.cod-gr-cli) 
                                      AND tt-campo.marcar    = "*" NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-campo THEN DO:
                
                    CREATE tt-campo.
                    ASSIGN tt-campo.cod-campo = STRING(gr-cli.cod-gr-cli).
                END.
            END. /* EACH estabelec */
            RUN pi-seleciona.
        END.
        WHEN "Grupo Estoque"          THEN DO:
 
            FOR EACH grup-estoque NO-LOCK
                WHERE grup-estoque.ge-codigo >= INT(c-campo-ini)
                AND   grup-estoque.ge-codigo <= INT(c-campo-fim):
 
                FIND FIRST tt-campo WHERE tt-campo.cod-campo = STRING(grup-estoque.ge-codigo) 
                                      AND tt-campo.marcar    = "*" NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-campo THEN DO:
                
                    CREATE tt-campo.
                    ASSIGN tt-campo.cod-campo = STRING(grup-estoque.ge-codigo).
                END.
            END. /* EACH estabelec */
            RUN pi-seleciona.
        END.
        WHEN "Classificaá∆o Fiscal"   THEN DO:
 
            FOR EACH classif-fisc NO-LOCK
                WHERE classif-fisc.class-fiscal >= c-campo-ini:screen-value
                AND   classif-fisc.class-fiscal <= c-campo-fim:screen-value :
 
                FIND FIRST tt-campo WHERE tt-campo.cod-campo = classif-fisc.class-fiscal
                                      AND tt-campo.marcar    = "*" NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-campo THEN DO:
                
                    CREATE tt-campo.                                      
                    ASSIGN tt-campo.cod-campo = classif-fisc.class-fiscal.
                END.
            END. /* EACH estabelec */
            RUN pi-seleciona.
        END.
        WHEN "C¢digo do Item"         THEN DO:
            
            FOR EACH ITEM NO-LOCK
                WHERE ITEM.it-codigo >= c-campo-ini:screen-value
                AND   ITEM.it-codigo <= c-campo-fim:screen-value :
 
                FIND FIRST tt-campo WHERE tt-campo.cod-campo = ITEM.it-codigo 
                                      AND tt-campo.marcar    = "*" NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-campo THEN DO:
                
                    CREATE tt-campo.                           
                    ASSIGN tt-campo.cod-campo = ITEM.it-codigo.
                END.
            END. /* EACH estabelec */
            RUN pi-seleciona.
        END.
        WHEN "Fam°lia Material"       THEN DO:
 
            FOR EACH familia NO-LOCK
                WHERE familia.fm-codigo >= c-campo-ini:screen-value
                AND   familia.fm-codigo <= c-campo-fim:screen-value:
 
                FIND FIRST tt-campo WHERE tt-campo.cod-campo = familia.fm-codigo 
                                      AND tt-campo.marcar    = "*" NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-campo THEN DO:
                
                    CREATE tt-campo.                               
                    ASSIGN tt-campo.cod-campo = familia.fm-codigo. 
                END.
            END. /* EACH estabelec */
            RUN pi-seleciona.
        END.
        WHEN "Fam°lia Comercial"      THEN DO:
 
            FOR EACH fam-comerc NO-LOCK
                WHERE fam-comerc.fm-cod-com >= c-campo-ini:screen-value
                AND   fam-comerc.fm-cod-com <= c-campo-fim:screen-value:
 
                FIND FIRST tt-campo WHERE tt-campo.cod-campo = fam-comerc.fm-cod-com 
                                      AND tt-campo.marcar    = "*" NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-campo THEN DO:
                
                    CREATE tt-campo.                                  
                    ASSIGN tt-campo.cod-campo = fam-comerc.fm-cod-com.
                END.
            END. /* EACH estabelec */
            RUN pi-seleciona.
        END.
    END CASE.
 
    {&OPEN-QUERY-BROWSE-4}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-delete wWindow 
PROCEDURE pi-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
    ASSIGN INPUT FRAME fPage1 c-campo-ini
           INPUT FRAME fPage1 c-campo-fim.
 
    FOR EACH tt-campo
        WHERE tt-campo.cod-campo >= c-campo-ini
        AND   tt-campo.cod-campo <= c-campo-fim:
        DELETE tt-campo.
    END. /* EACH tt-campo */
 
    {&OPEN-QUERY-BROWSE-4}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-salvar wWindow 
PROCEDURE pi-salvar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
    ASSIGN c-desc-valor = "".
 
    IF iPag = 1 THEN DO:
        FOR EACH tt-selecao NO-LOCK:
            IF c-desc-valor = "" THEN
                ASSIGN c-desc-valor = tt-selecao.cod-campo.
            ELSE
                ASSIGN c-desc-valor = c-desc-valor + ";" + tt-selecao.cod-campo.
        END. /* EACH tt-campo */
    END. /* iPag = 1 */
    ELSE IF iPag = 2 AND 
            cb-campo:SCREEN-VALUE IN FRAME fPage2 = "Todos" THEN DO:
        ASSIGN c-desc-valor = "*".
    END.
    ELSE IF iPag = 2 THEN 
        ASSIGN c-desc-valor = cb-campo:SCREEN-VALUE IN FRAME fPage2.
    ELSE IF iPag = 3 THEN
        ASSIGN c-desc-valor = c-campo:SCREEN-VALUE IN FRAME fPage3.
    IF c-desc-valor = "" THEN
        ASSIGN c-desc-valor = "*".
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-seleciona wWindow 
PROCEDURE pi-seleciona :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
    /*- Faz a seleá∆o dos registros que foram marcados para a tabela seleá∆o -*/
    FOR EACH tt-campo WHERE tt-campo.marcar = "*" NO-LOCK:
 
        FIND FIRST tt-selecao WHERE tt-selecao.cod-campo = tt-campo.cod-campo NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-selecao THEN DO:
 
            CREATE tt-selecao.
            ASSIGN tt-selecao.cod-campo = tt-campo.cod-campo.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-estabelec-fim wWindow 
PROCEDURE pi-zoom-estabelec-fim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                       &campo=c-campo-fim
                       &campozoom=cod-estabel
                       &FRAME=fPage1
                       &EnableImplant="NO"}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-estabelec-ini wWindow 
PROCEDURE pi-zoom-estabelec-ini :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                       &campo=c-campo-ini
                       &campozoom=cod-estabel
                       &FRAME=fPage1
                       &EnableImplant="NO"}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-fam-com-fim wWindow 
PROCEDURE pi-zoom-fam-com-fim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/zoomvar.i &prog-zoom=dizoom/z01di050.w
                       &campo=c-campo-fim
                       &campozoom=fm-cod-com
                       &FRAME=fPage1
                       &EnableImplant="NO"}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-fam-com-ini wWindow 
PROCEDURE pi-zoom-fam-com-ini :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/zoomvar.i &prog-zoom=dizoom/z01di050.w
                       &campo=c-campo-ini
                       &campozoom=fm-cod-com
                       &FRAME=fPage1
                       &EnableImplant="NO"}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-fam-mat-fim wWindow 
PROCEDURE pi-zoom-fam-mat-fim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/zoomvar.i &prog-zoom=inzoom/z01in122.w
                       &campo=c-campo-fim
                       &campozoom=fm-codigo
                       &FRAME=fPage1
                       &EnableImplant="NO"}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-fam-mat-ini wWindow 
PROCEDURE pi-zoom-fam-mat-ini :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/zoomvar.i &prog-zoom=inzoom/z01in122.w
                       &campo=c-campo-ini
                       &campozoom=fm-codigo
                       &FRAME=fPage1
                       &EnableImplant="NO"}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-gr-cli-fim wWindow 
PROCEDURE pi-zoom-gr-cli-fim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/zoomvar.i &prog-zoom=adzoom/z01ad129.w
                       &campo=c-campo-fim
                       &campozoom=cod-gr-cli
                       &FRAME=fPage1
                       &EnableImplant="NO"}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-gr-cli-ini wWindow 
PROCEDURE pi-zoom-gr-cli-ini :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/zoomvar.i &prog-zoom=adzoom/z01ad129.w
                       &campo=c-campo-ini
                       &campozoom=cod-gr-cli
                       &FRAME=fPage1
                       &EnableImplant="NO"}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-ge-codigo-fim wWindow 
PROCEDURE pi-zoom-ge-codigo-fim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/zoomvar.i &prog-zoom=inzoom/z01in142.w
                       &campo=c-campo-fim
                       &campozoom=ge-codigo
                       &FRAME=fPage1
                       &EnableImplant="NO"}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-ge-codigo-ini wWindow 
PROCEDURE pi-zoom-ge-codigo-ini :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/zoomvar.i &prog-zoom=inzoom/z01in142.w
                       &campo=c-campo-ini
                       &campozoom=ge-codigo
                       &FRAME=fPage1
                       &EnableImplant="NO"}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-item-fim wWindow 
PROCEDURE pi-zoom-item-fim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                       &campo=c-campo-fim
                       &campozoom=it-codigo
                       &FRAME=fPage1
                       &EnableImplant="NO"}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-item-ini wWindow 
PROCEDURE pi-zoom-item-ini :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                       &campo=c-campo-ini
                       &campozoom=it-codigo
                       &FRAME=fPage1
                       &EnableImplant="NO"}
 
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDesc wWindow 
FUNCTION fnDesc RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 
    CASE c-cod-campo:
        WHEN "Estabelecimento Origem" THEN
            FOR FIRST estabelec NO-LOCK
                WHERE estabelec.cod-estabel = tt-campo.cod-campo:
                RETURN estabelec.nome.
            END. /* FIRST estabelec */
        WHEN "Grupo Cliente"          THEN
            FOR FIRST gr-cli NO-LOCK
                WHERE gr-cli.cod-gr-cli = INT(tt-campo.cod-campo):
                RETURN gr-cli.descricao.
            END. /* FIRST gr-cli */
        WHEN "Grupo Estoque"          THEN
            FOR FIRST grup-estoque NO-LOCK
                WHERE grup-estoque.ge-codigo = INT(tt-campo.cod-campo):
                RETURN grup-estoque.descricao.
            END. /* FIRST grup-estoque */
        WHEN "Classificaá∆o Fiscal"   THEN
            FOR FIRST classif-fisc NO-LOCK
                WHERE classif-fisc.class-fiscal = tt-campo.cod-campo:
                RETURN classif-fisc.descricao.
            END. /* FIRST classif-fisc */
        WHEN "C¢digo do Item"         THEN
            FOR FIRST ITEM NO-LOCK
                WHERE ITEM.it-codigo = tt-campo.cod-campo:
                RETURN ITEM.descricao-1.
            END. /* FIRST ITEM */
        WHEN "Fam°lia Material"       THEN
            FOR FIRST familia NO-LOCK
                WHERE familia.fm-codigo = tt-campo.cod-campo:
                RETURN familia.descricao.
            END. /* FIRST familia */
        WHEN "Fam°lia Comercial"      THEN
            FOR FIRST fam-comerc NO-LOCK
                WHERE fam-comerc.fm-cod-com = tt-campo.cod-campo:
                RETURN fam-comerc.descricao.
            END. /* FIRST fam-comerc */
    END CASE.
    
  RETURN "".
 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

