&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgind            PROGRESS
*/
&Scoped-define WINDOW-NAME wWindow


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-contrato-for NO-UNDO LIKE contrato-for
       field r-rowid as rowid.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCC0105 2.00.00.001 } /*** 010001 ***/

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESCC0105 MCC}
&ENDIF

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESCC0105
&GLOBAL-DEFINE Version        2.00.00.001

&GLOBAL-DEFINE WindowType     Master

&GLOBAL-DEFINE Folder         NO
&GLOBAL-DEFINE InitialPage    0
&GLOBAL-DEFINE FolderLabels   

&GLOBAL-DEFINE page0Widgets   btQueryJoins btReportsJoins btExit btHelp ~
                              bt-filtro br-perc-item bt-selecionar bt-todos ~
                              bt-nenhum tg-perc-todos de-perc-compra bt-atualiza-perc

/* Parameters Definitions ---                                           */
DEF INPUT PARAM pi-nr-contrato LIKE contrato-for.nr-contrato NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-perc-item NO-UNDO
    FIELD seq          AS INT
    FIELD marca        AS CHAR
    FIELD perc-compra  LIKE item-fornec.perc-compra
    FIELD num-seq-item LIKE item-contrat.num-seq-item
    FIELD it-codigo    LIKE item-fornec.it-codigo
    FIELD item-do-forn LIKE item-fornec.item-do-forn
    FIELD un           LIKE ITEM.un
    FIELD unid-med-for LIKE item-fornec.unid-med-for
    FIELD numero-ordem LIKE ordem-compra.numero-ordem
    FIELD cod-cond-pag LIKE item-fornec.cod-cond-pag
    FIELD lote-minimo  LIKE item-fornec.lote-minimo
    FIELD lote-mul-for LIKE item-fornec.lote-mul-for
    FIELD contr-forn   LIKE item-fornec.contr-forn.
    
DEF VAR i-num-seq-item-ini  AS INT  INIT 0                  NO-UNDO.
DEF VAR i-num-seq-item-fim  AS INT  INIT 9999               NO-UNDO.
DEF VAR c-it-codigo-ini     AS CHAR INIT ""                 NO-UNDO.
DEF VAR c-it-codigo-fim     AS CHAR INIT "ZZZZZZZZZZZZZZZZ" NO-UNDO.

DEF VAR i-perc-old          AS INTEGER                      NO-UNDO.

DEF BUFFER bf-perc-item  FOR tt-perc-item.
DEF BUFFER b-item-fornec FOR item-fornec.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME br-perc-item

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-perc-item

/* Definitions for BROWSE br-perc-item                                  */
&Scoped-define FIELDS-IN-QUERY-br-perc-item tt-perc-item.marca tt-perc-item.perc-compra tt-perc-item.num-seq-item tt-perc-item.it-codigo tt-perc-item.item-do-forn tt-perc-item.un tt-perc-item.unid-med-for tt-perc-item.numero-ordem tt-perc-item.cod-cond-pag tt-perc-item.lote-minimo tt-perc-item.lote-mul-for tt-perc-item.contr-forn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-perc-item tt-perc-item.perc-compra ~
tt-perc-item.contr-forn   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-perc-item tt-perc-item
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-perc-item tt-perc-item
&Scoped-define SELF-NAME br-perc-item
&Scoped-define QUERY-STRING-br-perc-item FOR EACH tt-perc-item
&Scoped-define OPEN-QUERY-br-perc-item OPEN QUERY {&SELF-NAME} FOR EACH tt-perc-item.
&Scoped-define TABLES-IN-QUERY-br-perc-item tt-perc-item
&Scoped-define FIRST-TABLE-IN-QUERY-br-perc-item tt-perc-item


/* Definitions for FRAME fpage0                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fpage0 ~
    ~{&OPEN-QUERY-br-perc-item}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar rtParent rt-atualiza-todos ~
bt-filtro btQueryJoins btReportsJoins btExit btHelp i-nr-contrato ~
c-des-contrat c-cod-estabel c-desc-estabel i-cod-emitente c-desc-emitente ~
br-perc-item bt-selecionar bt-todos bt-nenhum tg-perc-todos ~
bt-atualiza-perc de-perc-compra 
&Scoped-Define DISPLAYED-OBJECTS i-nr-contrato c-des-contrat c-cod-estabel ~
c-desc-estabel i-cod-emitente c-desc-emitente tg-perc-todos de-perc-compra 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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
DEFINE BUTTON bt-atualiza-perc 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-chck1.bmp":U
     LABEL "" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-filtro 
     IMAGE-UP FILE "image/im-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-nenhum 
     LABEL "Nenhum" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-selecionar 
     LABEL "Selecionar" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-todos 
     LABEL "Todos" 
     SIZE 15 BY 1.13.

DEFINE BUTTON btExit 
     IMAGE-UP FILE "image\im-exi":U
     IMAGE-INSENSITIVE FILE "image\ii-exi":U
     LABEL "Exit" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btHelp 
     IMAGE-UP FILE "image\im-hel":U
     IMAGE-INSENSITIVE FILE "image\ii-hel":U
     LABEL "Help" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btQueryJoins 
     IMAGE-UP FILE "image\im-joi":U
     IMAGE-INSENSITIVE FILE "image\ii-joi":U
     LABEL "Query Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btReportsJoins 
     IMAGE-UP FILE "image\im-pri":U
     IMAGE-INSENSITIVE FILE "image\ii-pri":U
     LABEL "Reports Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE VARIABLE c-cod-estabel AS CHARACTER FORMAT "x(5)" 
     LABEL "Estabelecimento":R18 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88.

DEFINE VARIABLE c-des-contrat AS CHARACTER FORMAT "x(32)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-emitente AS CHARACTER FORMAT "x(80)":U 
     VIEW-AS FILL-IN 
     SIZE 45.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-estabel AS CHARACTER FORMAT "x(40)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY .88 NO-UNDO.

DEFINE VARIABLE de-perc-compra AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Percentual" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-emitente AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE i-nr-contrato AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "N£mero Contrato" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-atualiza-todos
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 1.75.

DEFINE RECTANGLE rtParent
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 3.58.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.5
     BGCOLOR 7 .

DEFINE VARIABLE tg-perc-todos AS LOGICAL INITIAL no 
     LABEL "Atualiza Percentual para Todos os Itens" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .58 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-perc-item FOR 
      tt-perc-item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-perc-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-perc-item wWindow _FREEFORM
  QUERY br-perc-item DISPLAY
      tt-perc-item.marca COLUMN-LABEL "*" WIDTH 1
      tt-perc-item.perc-compra WIDTH 8
      tt-perc-item.num-seq-item
      tt-perc-item.it-codigo WIDTH 16
      tt-perc-item.item-do-forn WIDTH 16
      tt-perc-item.un WIDTH 3
      tt-perc-item.unid-med-for WIDTH 6 COLUMN-LABEL "UN Forn"
      tt-perc-item.numero-ordem COLUMN-LABEL "Ordem Compra"
      tt-perc-item.cod-cond-pag WIDTH 8
      tt-perc-item.lote-minimo
      tt-perc-item.lote-mul-for
      tt-perc-item.contr-forn COLUMN-LABEL "Contr Fornecimento" FORMAT "S/N"
ENABLE tt-perc-item.perc-compra
       tt-perc-item.contr-forn
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 88 BY 7
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     bt-filtro AT ROW 1.13 COL 2 WIDGET-ID 32
     btQueryJoins AT ROW 1.13 COL 74.72 HELP
          "Consultas relacionadas"
     btReportsJoins AT ROW 1.13 COL 78.72 HELP
          "Relat¢rios relacionados"
     btExit AT ROW 1.13 COL 82.72 HELP
          "Sair"
     btHelp AT ROW 1.13 COL 86.72 HELP
          "Ajuda"
     i-nr-contrato AT ROW 3 COL 21 COLON-ALIGNED WIDGET-ID 38
     c-des-contrat AT ROW 3 COL 34.14 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     c-cod-estabel AT ROW 4 COL 21 COLON-ALIGNED WIDGET-ID 2
     c-desc-estabel AT ROW 4 COL 30.14 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     i-cod-emitente AT ROW 5 COL 21 COLON-ALIGNED WIDGET-ID 34
     c-desc-emitente AT ROW 5 COL 34.14 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     br-perc-item AT ROW 6.5 COL 2 WIDGET-ID 200
     bt-selecionar AT ROW 13.5 COL 2 WIDGET-ID 16
     bt-todos AT ROW 13.5 COL 17 WIDGET-ID 18
     bt-nenhum AT ROW 13.5 COL 32 WIDGET-ID 20
     tg-perc-todos AT ROW 13.75 COL 57 WIDGET-ID 24
     bt-atualiza-perc AT ROW 14.38 COL 84 WIDGET-ID 28
     de-perc-compra AT ROW 14.5 COL 70 COLON-ALIGNED WIDGET-ID 26
     rtToolBar AT ROW 1 COL 1
     rtParent AT ROW 2.67 COL 2 WIDGET-ID 12
     rt-atualiza-todos AT ROW 14 COL 55 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 15
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Temp-Tables and Buffers:
      TABLE: tt-contrato-for T "?" NO-UNDO mgind contrato-for
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 15.04
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
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* BROWSE-TAB br-perc-item c-desc-emitente fpage0 */
ASSIGN 
       c-des-contrat:READ-ONLY IN FRAME fpage0        = TRUE.

ASSIGN 
       c-desc-estabel:READ-ONLY IN FRAME fpage0        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-perc-item
/* Query rebuild information for BROWSE br-perc-item
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-perc-item.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-perc-item */
&ANALYZE-RESUME

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


&Scoped-define BROWSE-NAME br-perc-item
&Scoped-define SELF-NAME br-perc-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-perc-item wWindow
ON ENTRY OF br-perc-item IN FRAME fpage0
DO:
    APPLY "VALUE-CHANGED" TO br-perc-item IN FRAME fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-perc-item wWindow
ON ROW-ENTRY OF br-perc-item IN FRAME fpage0
DO:
    /* Para facilitar a digitaá∆o do usu†rio, caso a linha corrente **
    ** n∆o esteja marcada e existam linhas marcadas com sequància   **
    ** maior que a atual, pula atÇ encontrar uma linha marcada.     */
    IF  AVAIL tt-perc-item
    AND tt-perc-item.marca = "" THEN DO:

        IF CAN-FIND(FIRST bf-perc-item
                    WHERE bf-perc-item.marca = "*"
                    AND   bf-perc-item.seq   > tt-perc-item.seq) THEN
        
        br-perc-item:SELECT-NEXT-ROW().
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-perc-item wWindow
ON ROW-LEAVE OF br-perc-item IN FRAME fpage0
DO:
    IF AVAIL tt-perc-item THEN DO:

        IF INPUT BROWSE br-perc-item tt-perc-item.perc-compra > 100 THEN DO:
            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT 17006,
                               INPUT "Percentual nao pode ser maior que 100%").
            RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN /*tt-perc-item.perc-compra = INPUT BROWSE br-perc-item tt-perc-item.perc-compra*/
                   tt-perc-item.contr-forn  = INPUT BROWSE br-perc-item tt-perc-item.contr-forn.

            RUN AlteraItemFornec (INPUT tt-perc-item.it-codigo,
                                  INPUT INPUT FRAME fpage0 i-cod-emitente,
                                  INPUT INPUT BROWSE br-perc-item tt-perc-item.perc-compra,
                                  INPUT tt-perc-item.contr-forn).

            /* Caso esteja na £ltima linha e seja executado o TAB, o cursor **
            ** permanece na £ltima linha e n∆o sai do browse. Por isso aqui **
            ** ser† feita a saida da £ltima linha do browse manualmente.    */
            /*IF tt-perc-item.seq = QUERY br-perc-item:NUM-RESULTS THEN DO:
                
            END.*/

            IF RETURN-VALUE = "OK":U THEN
                 ASSIGN tt-perc-item.perc-compra = INPUT BROWSE br-perc-item tt-perc-item.perc-compra
                        tt-perc-item.marca = "".


        END.
    END.
    OPEN QUERY br-perc-item FOR EACH tt-perc-item.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-perc-item wWindow
ON VALUE-CHANGED OF br-perc-item IN FRAME fpage0
DO:
    IF AVAIL tt-perc-item
    AND tt-perc-item.marca = "*" THEN
        ASSIGN tt-perc-item.perc-compra:READ-ONLY IN BROWSE br-perc-item = NO
               tt-perc-item.contr-forn :READ-ONLY IN BROWSE br-perc-item = NO.
    ELSE
        ASSIGN tt-perc-item.perc-compra:READ-ONLY IN BROWSE br-perc-item = YES
               tt-perc-item.contr-forn :READ-ONLY IN BROWSE br-perc-item = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-atualiza-perc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-atualiza-perc wWindow
ON CHOOSE OF bt-atualiza-perc IN FRAME fpage0
DO:
    IF INPUT FRAME fPage0 de-perc-compra > 100 THEN DO:
        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 17006,
                           INPUT "Percentual n∆o pode ser maior que 100%").
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FOR EACH tt-perc-item
            WHERE tt-perc-item.marca = "*":
/*             ASSIGN tt-perc-item.perc-compra = INPUT FRAME fPage0 de-perc-compra. */

            ASSIGN i-perc-old = INPUT FRAME fPage0 de-perc-compra.

            RUN AlteraItemFornec (INPUT tt-perc-item.it-codigo,
                                  INPUT INPUT FRAME fpage0 i-cod-emitente,
                                  INPUT INPUT FRAME fPage0 de-perc-compra,
                                  INPUT tt-perc-item.contr-forn).

            IF RETURN-VALUE = "OK":U THEN
                 ASSIGN tt-perc-item.perc-compra = INPUT FRAME fPage0 de-perc-compra
                        tt-perc-item.marca       = "".

        END.
    END.
    
    OPEN QUERY br-perc-item FOR EACH tt-perc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro wWindow
ON CHOOSE OF bt-filtro IN FRAME fpage0
DO:
  
    RUN ccp/esp/escc0105a.w (INPUT-OUTPUT c-it-codigo-ini,
                             INPUT-OUTPUT c-it-codigo-fim,
                             INPUT-OUTPUT i-num-seq-item-ini,
                             INPUT-OUTPUT i-num-seq-item-fim).

    RUN carregaPercItemContrat.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum wWindow
ON CHOOSE OF bt-nenhum IN FRAME fpage0 /* Nenhum */
DO:
    FOR EACH tt-perc-item:
        ASSIGN tt-perc-item.marca = "".

        APPLY "VALUE-CHANGED" TO br-perc-item IN FRAME fPage0.

    END.
    
    OPEN QUERY br-perc-item FOR EACH tt-perc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-selecionar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-selecionar wWindow
ON CHOOSE OF bt-selecionar IN FRAME fpage0 /* Selecionar */
DO:
    IF br-perc-item:NUM-SELECTED-ROWS = 1 
    AND AVAIL tt-perc-item THEN DO:

        IF tt-perc-item.marca = "*" THEN
            ASSIGN tt-perc-item.marca = "".
        ELSE
            ASSIGN tt-perc-item.marca = "*".

        APPLY "VALUE-CHANGED" TO br-perc-item IN FRAME fPage0.
    END.
    
    br-perc-item:REFRESH().

    /*OPEN QUERY br-perc-item FOR EACH tt-perc-item.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos wWindow
ON CHOOSE OF bt-todos IN FRAME fpage0 /* Todos */
DO:
    FOR EACH tt-perc-item:
        ASSIGN tt-perc-item.marca = "*".

        APPLY "VALUE-CHANGED" TO br-perc-item IN FRAME fPage0.

    END.
    
    OPEN QUERY br-perc-item FOR EACH tt-perc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit wWindow
ON CHOOSE OF btExit IN FRAME fpage0 /* Exit */
OR CHOOSE OF MENU-ITEM miExit IN MENU mbMain DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wWindow
ON CHOOSE OF btHelp IN FRAME fpage0 /* Help */
OR CHOOSE OF MENU-ITEM miContents IN MENU mbMain DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btQueryJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btQueryJoins wWindow
ON CHOOSE OF btQueryJoins IN FRAME fpage0 /* Query Joins */
OR CHOOSE OF MENU-ITEM miQueryJoins IN MENU mbMain DO:
    RUN showQueryJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReportsJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReportsJoins wWindow
ON CHOOSE OF btReportsJoins IN FRAME fpage0 /* Reports Joins */
OR CHOOSE OF MENU-ITEM miReportsJoins IN MENU mbMain DO:
    RUN showReportsJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estabel wWindow
ON LEAVE OF c-cod-estabel IN FRAME fpage0 /* Estabelecimento */
DO:
    FOR FIRST estabelec NO-LOCK
        WHERE estabelec.cod-estabel = INPUT FRAME fPage0 c-cod-estabel:

        ASSIGN c-desc-estabel = estabelec.nome.

        DISPLAY c-desc-estabel
            WITH FRAME fPage0.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-cod-emitente wWindow
ON LEAVE OF i-cod-emitente IN FRAME fpage0 /* Fornecedor */
DO:
    FOR FIRST emitente NO-LOCK
        WHERE emitente.cod-emitente = INPUT FRAME fPage0 i-cod-emitente:

        ASSIGN c-desc-emitente = emitente.nome-emit.

        DISPLAY c-desc-emitente 
            WITH FRAME fPage0.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-nr-contrato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-nr-contrato wWindow
ON LEAVE OF i-nr-contrato IN FRAME fpage0 /* N£mero Contrato */
DO:
    FOR FIRST contrato-for NO-LOCK
        WHERE contrato-for.nr-contrato = INPUT FRAME fPage0 i-nr-contrato:

        ASSIGN c-des-contrat = contrato-for.des-contrat.

        DISPLAY c-des-contrat
            WITH FRAME fPage0.
    END.
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


&Scoped-define SELF-NAME tg-perc-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-perc-todos wWindow
ON VALUE-CHANGED OF tg-perc-todos IN FRAME fpage0 /* Atualiza Percentual para Todos os Itens */
DO:
    ASSIGN de-perc-compra   :SENSITIVE IN FRAME fPage0 = INPUT FRAME fPage0 tg-perc-todos
           bt-atualiza-perc :SENSITIVE IN FRAME fPage0 = INPUT FRAME fPage0 tg-perc-todos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

    ASSIGN i-nr-contrato = pi-nr-contrato.
    
    FIND FIRST contrato-for NO-LOCK
        WHERE contrato-for.nr-contrato = i-nr-contrato NO-ERROR.
    IF AVAIL contrato-for THEN
        ASSIGN c-cod-estabel  = contrato-for.cod-estabel
               i-cod-emitente = contrato-for.cod-emitente.
    
        DISPLAY i-nr-contrato 
                c-cod-estabel
                i-cod-emitente
            WITH FRAME fPage0.

    APPLY "LEAVE" TO i-nr-contrato  IN FRAME fPage0.
    APPLY "LEAVE" TO c-cod-estabel  IN FRAME fPage0.
    APPLY "LEAVE" TO i-cod-emitente IN FRAME fPage0.

    APPLY "VALUE-CHANGED" TO tg-perc-todos IN FRAME fPage0.
    
    RUN carregaPercItemContrat.

    ASSIGN tt-perc-item.perc-compra:READ-ONLY IN BROWSE br-perc-item = YES
           tt-perc-item.contr-forn :READ-ONLY IN BROWSE br-perc-item = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AlteraItemFornec wWindow 
PROCEDURE AlteraItemFornec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DEF INPUT PARAM pc-it-codigo    AS CHAR NO-UNDO.
    DEF INPUT PARAM pi-cod-emitente AS INT  NO-UNDO.
    DEF INPUT PARAM pi-perc-compra  AS INT  NO-UNDO.
    DEF INPUT PARAM pl-contr-forn   AS LOG  NO-UNDO.

    DEFINE VARIABLE i-perc-aux      AS INTEGER     NO-UNDO.
    
    FOR EACH item-fornec EXCLUSIVE-LOCK
        WHERE item-fornec.it-codigo    = pc-it-codigo
        AND   item-fornec.cod-emitente = pi-cod-emitente:
        ASSIGN i-perc-aux = 0
               item-fornec.contr-forn  = pl-contr-forn.

        FOR EACH b-item-fornec NO-LOCK
            WHERE b-item-fornec.it-codigo    = item-fornec.it-codigo
            AND   b-item-fornec.cod-emitente <> item-fornec.cod-emitente:

            ASSIGN i-perc-aux = i-perc-aux + b-item-fornec.perc-compra.

        END.

        IF (pi-perc-compra + i-perc-aux) > 100 THEN DO:
            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT 17006,
                               INPUT "Percentual da relaá∆o Item x Fornecedor n∆o pode ser maior que 100%.").
            RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN item-fornec.perc-compra = pi-perc-compra.

        END.
    END.

    RETURN "OK":U.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carregaPercItemContrat wWindow 
PROCEDURE carregaPercItemContrat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DEF VAR i-seq AS INT INIT 1 NO-UNDO.
    
    EMPTY TEMP-TABLE tt-perc-item.

    FOR EACH contrato-for NO-LOCK
        WHERE contrato-for.nr-contrato = INPUT FRAME fPage0 i-nr-contrato,
        EACH item-contrat NO-LOCK
            WHERE item-contrat.nr-contrato   = contrato-for.nr-contrato
            AND   item-contrat.num-seq-item >= i-num-seq-item-ini
            AND   item-contrat.num-seq-item <= i-num-seq-item-fim
            AND   item-contrat.it-codigo    >= c-it-codigo-ini
            AND   item-contrat.it-codigo    <= c-it-codigo-fim,
            EACH item-fornec NO-LOCK
                WHERE item-fornec.it-codigo    = item-contrat.it-codigo
                AND   item-fornec.cod-emitente = contrato-for.cod-emitente:

        FIND FIRST ITEM NO-LOCK
            WHERE ITEM.it-codigo = item-contrat.it-codigo NO-ERROR.

        FIND FIRST ordem-compra NO-LOCK
            WHERE ordem-compra.it-codigo    = item-contrat.it-codigo
            AND   ordem-compra.nr-contrato  = contrato-for.nr-contrato 
            AND   ordem-compra.sequencia    = item-contrat.num-seq-item  NO-ERROR.

        CREATE tt-perc-item.
        ASSIGN tt-perc-item.seq          = i-seq
               tt-perc-item.marca        = ""
               tt-perc-item.perc-compra  = item-fornec.perc-compra
               tt-perc-item.num-seq-item = item-contrat.num-seq-item
               tt-perc-item.it-codigo    = item-contrat.it-codigo
               tt-perc-item.item-do-forn = item-fornec.item-do-forn
               tt-perc-item.un           = ITEM.un
               tt-perc-item.unid-med-for = item-fornec.unid-med-for
               tt-perc-item.numero-ordem = IF AVAIL ordem-compra THEN ordem-compra.numero-ordem ELSE 0
               tt-perc-item.cod-cond-pag = item-fornec.cod-cond-pag
               tt-perc-item.lote-minimo  = item-fornec.lote-minimo
               tt-perc-item.lote-mul-for = item-fornec.lote-mul-for
               tt-perc-item.contr-forn   = item-fornec.contr-forn.

        ASSIGN i-seq = i-seq + 1.
        
    END.
    
    OPEN QUERY br-perc-item FOR EACH tt-perc-item.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

