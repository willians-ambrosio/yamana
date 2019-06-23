&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emsfnd           PROGRESS
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/****************************************************************************************** 
**         Programa: rep006-w01.w
**            Autor: Vando Ribeiro
**       Fornecedor: Grupo DKP
**       Data: 05/11/2018
** Change/Chamado: 
**    Objetivo: Relaciona usuario do financeiro para receber e-mail de doctos atualizados fora do prazo.
**
******************************** CONTROLE DE ALTERA€åES *********************************
** 
** Data         Autor                   Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
** 
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}

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

DEFINE VARIABLE c-pesquisa AS CHARACTER   NO-UNDO.
DEFINE VARIABLE r-rowid-ini AS ROWID NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME brw-usu-disp

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES usuar_mestre esp_hist_email

/* Definitions for BROWSE brw-usu-disp                                  */
&Scoped-define FIELDS-IN-QUERY-brw-usu-disp usuar_mestre.cod_usuario ~
usuar_mestre.nom_usuario 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw-usu-disp 
&Scoped-define QUERY-STRING-brw-usu-disp FOR EACH usuar_mestre NO-LOCK ~
    BY usuar_mestre.cod_usuario INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brw-usu-disp OPEN QUERY brw-usu-disp FOR EACH usuar_mestre NO-LOCK ~
    BY usuar_mestre.cod_usuario INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brw-usu-disp usuar_mestre
&Scoped-define FIRST-TABLE-IN-QUERY-brw-usu-disp usuar_mestre


/* Definitions for BROWSE brw-usu-sel                                   */
&Scoped-define FIELDS-IN-QUERY-brw-usu-sel esp_hist_email.cod_usuario ~
usuar_mestre.nom_usuario 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw-usu-sel 
&Scoped-define QUERY-STRING-brw-usu-sel FOR EACH esp_hist_email NO-LOCK, ~
      EACH usuar_mestre OF esp_hist_email NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brw-usu-sel OPEN QUERY brw-usu-sel FOR EACH esp_hist_email NO-LOCK, ~
      EACH usuar_mestre OF esp_hist_email NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brw-usu-sel esp_hist_email usuar_mestre
&Scoped-define FIRST-TABLE-IN-QUERY-brw-usu-sel esp_hist_email
&Scoped-define SECOND-TABLE-IN-QUERY-brw-usu-sel usuar_mestre


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-brw-usu-disp}~
    ~{&OPEN-QUERY-brw-usu-sel}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-26 i-selecao c-codigo c-nome ~
brw-usu-disp bt-usu-disp brw-usu-sel bt-usu-sel 
&Scoped-Define DISPLAYED-OBJECTS i-selecao c-codigo c-nome 

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
DEFINE BUTTON bt-usu-disp 
     IMAGE-UP FILE "image/toolbar/im-abx1.bmp":U
     LABEL "" 
     SIZE 5 BY 2 TOOLTIP "Adicionar".

DEFINE BUTTON bt-usu-sel 
     IMAGE-UP FILE "image/toolbar/im-acm1.bmp":U
     LABEL "" 
     SIZE 5 BY 2 TOOLTIP "Remover".

DEFINE VARIABLE c-codigo AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .88 NO-UNDO.

DEFINE VARIABLE i-selecao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "C¢digo", 1,
"Nome", 2
     SIZE 19 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 1.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 82 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brw-usu-disp FOR 
      usuar_mestre SCROLLING.

DEFINE QUERY brw-usu-sel FOR 
      esp_hist_email, 
      usuar_mestre SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brw-usu-disp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw-usu-disp w-livre _STRUCTURED
  QUERY brw-usu-disp NO-LOCK DISPLAY
      usuar_mestre.cod_usuario COLUMN-LABEL "Cod. Usu rio" FORMAT "x(12)":U
      usuar_mestre.nom_usuario COLUMN-LABEL "Nome Usu rio" FORMAT "x(50)":U
            WIDTH 50
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 69 BY 7
         TITLE "Usu rios Dispon¡veis" FIT-LAST-COLUMN.

DEFINE BROWSE brw-usu-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw-usu-sel w-livre _STRUCTURED
  QUERY brw-usu-sel NO-LOCK DISPLAY
      esp_hist_email.cod_usuario COLUMN-LABEL "C¢d. Usu rio" FORMAT "x(12)":U
      usuar_mestre.nom_usuario FORMAT "x(50)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 69 BY 7
         TITLE "Usu rios Selecionados" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     i-selecao AT ROW 3.25 COL 9 NO-LABEL WIDGET-ID 2
     c-codigo AT ROW 3.25 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     c-nome AT ROW 3.25 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     brw-usu-disp AT ROW 4.75 COL 2 WIDGET-ID 200
     bt-usu-disp AT ROW 7.5 COL 75 WIDGET-ID 10
     brw-usu-sel AT ROW 12 COL 2 WIDGET-ID 300
     bt-usu-sel AT ROW 14.75 COL 75 WIDGET-ID 12
     "Busca" VIEW-AS TEXT
          SIZE 5 BY .67 AT ROW 2.58 COL 4 WIDGET-ID 16
     rt-button AT ROW 1 COL 1.72
     RECT-26 AT ROW 2.92 COL 2 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 83.57 BY 18.25 WIDGET-ID 100.


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
         TITLE              = "Relac Usu rios Finaceiro x Rec E-mail"
         HEIGHT             = 18.25
         WIDTH              = 83.57
         MAX-HEIGHT         = 18.96
         MAX-WIDTH          = 103.43
         VIRTUAL-HEIGHT     = 18.96
         VIRTUAL-WIDTH      = 103.43
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
/* BROWSE-TAB brw-usu-disp c-nome f-cad */
/* BROWSE-TAB brw-usu-sel bt-usu-disp f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw-usu-disp
/* Query rebuild information for BROWSE brw-usu-disp
     _TblList          = "emsfnd.usuar_mestre"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "emsfnd.usuar_mestre.cod_usuario|yes"
     _FldNameList[1]   > emsfnd.usuar_mestre.cod_usuario
"cod_usuario" "Cod. Usu rio" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > emsfnd.usuar_mestre.nom_usuario
"nom_usuario" "Nome Usu rio" ? "character" ? ? ? ? ? ? no ? no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brw-usu-disp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw-usu-sel
/* Query rebuild information for BROWSE brw-usu-sel
     _TblList          = "mgesp.esp_hist_email,emsfnd.usuar_mestre OF mgesp.esp_hist_email"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > mgesp.esp_hist_email.cod_usuario
"mgesp.esp_hist_email.cod_usuario" "C¢d. Usu rio" ? "character" ? ? ? ? ? ? no "C¢digo Usu rio" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = emsfnd.usuar_mestre.nom_usuario
     _Query            is OPENED
*/  /* BROWSE brw-usu-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Relac Usu rios Finaceiro x Rec E-mail */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Relac Usu rios Finaceiro x Rec E-mail */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brw-usu-disp
&Scoped-define SELF-NAME brw-usu-disp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brw-usu-disp w-livre
ON MOUSE-SELECT-DBLCLICK OF brw-usu-disp IN FRAME f-cad /* Usu rios Dispon¡veis */
DO:
    APPLY "choose" TO bt-usu-disp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brw-usu-sel
&Scoped-define SELF-NAME brw-usu-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brw-usu-sel w-livre
ON MOUSE-SELECT-DBLCLICK OF brw-usu-sel IN FRAME f-cad /* Usu rios Selecionados */
DO:
    APPLY "choose" TO bt-usu-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-usu-disp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-usu-disp w-livre
ON CHOOSE OF bt-usu-disp IN FRAME f-cad
DO:
  
    FIND esp_hist_email WHERE
         esp_hist_email.cod_usuario = usuar_mestre.cod_usuario NO-LOCK NO-ERROR.
    IF NOT AVAIL esp_hist_email THEN
    DO:
        CREATE esp_hist_email.
        ASSIGN esp_hist_email.cod_usuario = usuar_mestre.cod_usuario.
       {&OPEN-QUERY-brw-usu-sel}
    END.
    ELSE
    DO:
        RUN utp/ut-msgs.p ("show",17006,"Usu rio Inv lido~~ Usuario  " + esp_hist_email.cod_usuario + "-" + usuar_mestre.nom_usuario + "  j  selecionado.").
        
    
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-usu-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-usu-sel w-livre
ON CHOOSE OF bt-usu-sel IN FRAME f-cad
DO:
    FIND esp_hist_email WHERE
         esp_hist_email.cod_usuario = usuar_mestre.cod_usuario EXCLUSIVE NO-ERROR.
    IF AVAIL esp_hist_email THEN
    DO:
        DELETE esp_hist_email.
       {&OPEN-QUERY-brw-usu-sel}
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-codigo w-livre
ON ANY-KEY OF c-codigo IN FRAME f-cad
DO:
    DEFINE VARIABLE r-rowid AS ROWID NO-UNDO.

    c-pesquisa = TRIM(c-codigo:SCREEN-VALUE) + IF ASC(CHR(LASTKEY)) = 8 THEN "" ELSE CHR(LASTKEY).

    IF c-pesquisa <> "" THEN
    DO:
        FIND FIRST usuar_mestre WHERE
                   usuar_mestre.cod_usuario BEGINS c-pesquisa NO-LOCK NO-ERROR.
        IF AVAIL usuar_mestre THEN
        DO:
            r-rowid = ROWID(usuar_mestre).
            REPOSITION brw-usu-disp TO ROWID r-rowid NO-ERROR.
            brw-usu-disp:REFRESH().
        END.
    END.
    ELSE
    DO:
        IF r-rowid-ini <> ? THEN
        DO:
            REPOSITION brw-usu-disp TO ROWID r-rowid-ini NO-ERROR.
            brw-usu-disp:REFRESH().
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-nome
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nome w-livre
ON ANY-KEY OF c-nome IN FRAME f-cad
DO:
    DEFINE VARIABLE r-rowid AS ROWID NO-UNDO.

    c-pesquisa = TRIM(c-nome:SCREEN-VALUE) + IF ASC(CHR(LASTKEY)) = 8 THEN "" ELSE CHR(LASTKEY).

    IF c-pesquisa <> "" THEN
    DO:
        FIND FIRST usuar_mestre WHERE
                   usuar_mestre.nom_usuario BEGINS c-pesquisa NO-LOCK NO-ERROR.
        IF AVAIL usuar_mestre THEN
        DO:
            r-rowid = ROWID(usuar_mestre).
            REPOSITION brw-usu-disp TO ROWID r-rowid NO-ERROR.
            brw-usu-disp:REFRESH().
        END.
    END.
    ELSE
    DO:
        IF r-rowid-ini <> ? THEN
        DO:
            REPOSITION brw-usu-disp TO ROWID r-rowid-ini NO-ERROR.
            brw-usu-disp:REFRESH().
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-selecao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-selecao w-livre
ON VALUE-CHANGED OF i-selecao IN FRAME f-cad
DO:

    IF INPUT i-selecao = "1" THEN
    DO:

        ASSIGN c-codigo:HIDDEN = NO
               c-nome:HIDDEN = YES.

        OPEN QUERY brw-usu-disp FOR EACH usuar_mestre NO-LOCK BY usuar_mestre.cod_usuario INDEXED-REPOSITION.
        r-rowid-ini = ROWID(usuar_mestre).
    END.
    ELSE
    DO:
        ASSIGN c-codigo:HIDDEN = YES
               c-nome:HIDDEN = NO.

        OPEN QUERY brw-usu-disp FOR EACH usuar_mestre NO-LOCK BY usuar_mestre.nom_usuario INDEXED-REPOSITION.
        r-rowid-ini = ROWID(usuar_mestre).
    END.

    /*
    IF INPUT i-selecao = "1" THEN

        ASSIGN c-codigo:SENSITIVE = YES
               c-nome:SENSITIVE = NO.
    ELSE
        ASSIGN c-codigo:SENSITIVE = NO
               c-nome:SENSITIVE = YES.
    */
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


&Scoped-define BROWSE-NAME brw-usu-disp
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
       RUN set-position IN h_p-exihel ( 1.08 , 66.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             i-selecao:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY i-selecao c-codigo c-nome 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-26 i-selecao c-codigo c-nome brw-usu-disp bt-usu-disp 
         brw-usu-sel bt-usu-sel 
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

  {utp/ut9000.i "rep006-w01" "1.00.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  APPLY "VALUE-CHANGED" TO i-selecao.

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
  {src/adm/template/snd-list.i "esp_hist_email"}
  {src/adm/template/snd-list.i "usuar_mestre"}

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

