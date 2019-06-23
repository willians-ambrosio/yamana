&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
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
{include/i-prgvrs.i Zerar-saldos-rmv2 2.06.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt-requisicao
    FIELD nr-requisicao LIKE requisicao.nr-requisicao    COLUMN-LABEL "Nr Requis"        
    FIELD it-codigo     LIKE it-requisicao.it-codigo     COLUMN-LABEL "Item"              
    FIELD sequencia     LIKE it-requisicao.sequencia     COLUMN-LABEL "Seq"             
    FIELD descricao     LIKE requisicao.narrativa        COLUMN-LABEL "Narrativa"       
    FIELD dt-requisicao LIKE requisicao.dt-requisicao    COLUMN-LABEL "Dt Requis"        
    FIELD sc-codigo     LIKE requisicao.sc-codigo        COLUMN-LABEL "C.Custo"          
    FIELD qt-a-atender  AS DEC FORMAT "->>>,>>>,>>9.99"  COLUMN-LABEL "Qtde Saldo"                                     /* LIKE it-requisicao.qt-a-atender */
    FIELD zerar         AS CHAR FORMAT "x(1)"            COLUMN-LABEL "Sel"
    FIELD nome-abrev    LIKE requisicao.nome-abrev       COLUMN-LABEL "Nome Requisitante"
    FIELD usuario       LIKE requisicao.nome-abrev       COLUMN-LABEL "Usuario"
    FIELD dt-entrega    LIKE it-requisicao.dt-entrega    COLUMN-LABEL "Dt Entrega".

DEF TEMP-TABLE tt-req-zerada LIKE tt-requisicao.

/* {utp/ut-glob.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-requisicao

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-requisicao

/* Definitions for BROWSE br-requisicao                                 */
&Scoped-define FIELDS-IN-QUERY-br-requisicao tt-requisicao.zerar tt-requisicao.nr-requisicao tt-requisicao.sequencia tt-requisicao.it-codigo tt-requisicao.qt-a-atender tt-requisicao.dt-requisicao tt-requisicao.dt-entrega tt-requisicao.sc-codigo tt-requisicao.descricao tt-requisicao.nome-abrev   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-requisicao   
&Scoped-define SELF-NAME br-requisicao
&Scoped-define QUERY-STRING-br-requisicao FOR EACH tt-requisicao
&Scoped-define OPEN-QUERY-br-requisicao OPEN QUERY {&SELF-NAME} FOR EACH tt-requisicao.
&Scoped-define TABLES-IN-QUERY-br-requisicao tt-requisicao
&Scoped-define FIRST-TABLE-IN-QUERY-br-requisicao tt-requisicao


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-requisicao}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-2 IMAGE-5 IMAGE-6 IMAGE-11 ~
IMAGE-12 IMAGE-13 IMAGE-14 IMAGE-15 IMAGE-16 RECT-3 i-nr-requisicao-ini ~
i-nr-requisicao-fim c-it-codigo-ini c-it-codigo-fim bt-confirma ~
c-sc-codigo-ini c-sc-codigo-fim dt-requisicao-ini dt-requisicao-fim nr-dias ~
c-arquivo br-requisicao bt-todos bt-nenhum BUTTON-1 
&Scoped-Define DISPLAYED-OBJECTS i-nr-requisicao-ini i-nr-requisicao-fim ~
c-it-codigo-ini c-it-codigo-fim c-sc-codigo-ini c-sc-codigo-fim ~
dt-requisicao-ini dt-requisicao-fim nr-dias c-arquivo 

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
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "Button 1" 
     SIZE 6.72 BY 1.25.

DEFINE BUTTON bt-nenhum 
     LABEL "Nenhum" 
     SIZE 8 BY .75.

DEFINE BUTTON bt-todos 
     LABEL "Todos" 
     SIZE 8 BY .75.

DEFINE BUTTON BUTTON-1 
     LABEL "Zerar Saldos das RMs" 
     SIZE 19.57 BY 1.21.

DEFINE VARIABLE c-arquivo AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\temp~\zerar-rm.txt" 
     LABEL "Arq. Log" 
     VIEW-AS FILL-IN 
     SIZE 42 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-fim AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-sc-codigo-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-sc-codigo-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Centro Custo":R15 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE dt-requisicao-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE dt-requisicao-ini AS DATE FORMAT "99/99/9999" 
     LABEL "Data Requisi‡Æo":R18 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE i-nr-requisicao-fim AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE i-nr-requisicao-ini AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     LABEL "Requisi‡Æo":R12 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE nr-dias AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Dias em Aberto":R18 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Dias em aberto pela data de entrega do item da requisicao" NO-UNDO.

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

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89.72 BY 5.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89.72 BY 1.71.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-requisicao FOR 
      tt-requisicao SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-requisicao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-requisicao w-livre _FREEFORM
  QUERY br-requisicao DISPLAY
      tt-requisicao.zerar         COLUMN-LABEL "Sel"     
 tt-requisicao.nr-requisicao COLUMN-LABEL "Nr Requis"
 tt-requisicao.sequencia     COLUMN-LABEL "Seq"
 tt-requisicao.it-codigo     COLUMN-LABEL "Item"
 tt-requisicao.qt-a-atender  COLUMN-LABEL "Qtde Saldo"
 tt-requisicao.dt-requisicao COLUMN-LABEL "Dt Requis"
 tt-requisicao.dt-entrega    COLUMN-LABEL "Dt Entrega"
 tt-requisicao.sc-codigo     COLUMN-LABEL "C.Custo"
 tt-requisicao.descricao     COLUMN-LABEL "Descricao"
 tt-requisicao.nome-abrev    COLUMN-LABEL "Nome Requisitante"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 89.72 BY 9.63
         FONT 1
         TITLE "Requisicoes Aprovadas" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     i-nr-requisicao-ini AT ROW 2.75 COL 16.43 COLON-ALIGNED HELP
          "Permite informar o numero da requisi‡Æo de materiais ao estoque" WIDGET-ID 2
     i-nr-requisicao-fim AT ROW 2.75 COL 40 COLON-ALIGNED HELP
          "Permite informar o numero da requisi‡Æo de materiais ao estoque" NO-LABEL WIDGET-ID 18
     c-it-codigo-ini AT ROW 3.67 COL 13 COLON-ALIGNED HELP
          "C¢digo do Material a ser Requisitado" WIDGET-ID 4
     c-it-codigo-fim AT ROW 3.67 COL 40 COLON-ALIGNED HELP
          "C¢digo do Material a ser Requisitado" NO-LABEL WIDGET-ID 12
     bt-confirma AT ROW 4 COL 77.43 WIDGET-ID 90
     c-sc-codigo-ini AT ROW 4.58 COL 17.57 COLON-ALIGNED HELP
          "C¢digo do Centro de Custo" WIDGET-ID 8
     c-sc-codigo-fim AT ROW 4.58 COL 40 COLON-ALIGNED HELP
          "C¢digo do Centro de Custo" NO-LABEL WIDGET-ID 14
     dt-requisicao-ini AT ROW 5.5 COL 17.57 COLON-ALIGNED WIDGET-ID 6
     dt-requisicao-fim AT ROW 5.5 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     nr-dias AT ROW 6.42 COL 22.14 COLON-ALIGNED WIDGET-ID 102
     c-arquivo AT ROW 6.42 COL 40 COLON-ALIGNED WIDGET-ID 104
     br-requisicao AT ROW 7.58 COL 1 WIDGET-ID 200
     bt-todos AT ROW 17.5 COL 1.86 WIDGET-ID 94
     bt-nenhum AT ROW 17.5 COL 10.57 WIDGET-ID 96
     BUTTON-1 AT ROW 17.58 COL 69 WIDGET-ID 92
     rt-button AT ROW 1 COL 1
     RECT-2 AT ROW 2.5 COL 1 WIDGET-ID 10
     IMAGE-5 AT ROW 2.75 COL 33.43 WIDGET-ID 20
     IMAGE-6 AT ROW 2.75 COL 37.57 WIDGET-ID 22
     IMAGE-11 AT ROW 3.67 COL 33.43 WIDGET-ID 24
     IMAGE-12 AT ROW 3.67 COL 37.57 WIDGET-ID 26
     IMAGE-13 AT ROW 4.58 COL 33.43 WIDGET-ID 28
     IMAGE-14 AT ROW 4.58 COL 37.57 WIDGET-ID 30
     IMAGE-15 AT ROW 5.5 COL 33.43 WIDGET-ID 32
     IMAGE-16 AT ROW 5.5 COL 37.57 WIDGET-ID 34
     RECT-3 AT ROW 17.33 COL 1 WIDGET-ID 98
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 18.21
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
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 18.21
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
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-requisicao c-arquivo f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-requisicao
/* Query rebuild information for BROWSE br-requisicao
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-requisicao.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-requisicao */
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


&Scoped-define BROWSE-NAME br-requisicao
&Scoped-define SELF-NAME br-requisicao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-requisicao w-livre
ON MOUSE-SELECT-DBLCLICK OF br-requisicao IN FRAME f-cad /* Requisicoes Aprovadas */
DO:
  IF tt-requisicao.zerar = '*' THEN ASSIGN tt-requisicao.zerar = ''.
                               ELSE ASSIGN tt-requisicao.zerar = '*'.

  br-requisicao:REFRESH().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-requisicao w-livre
ON ROW-DISPLAY OF br-requisicao IN FRAME f-cad /* Requisicoes Aprovadas */
DO:
  IF tt-requisicao.zerar = '' THEN 
     ASSIGN tt-requisicao.zerar:FGCOLOR IN BROWSE br-requisicao         = 8      
            tt-requisicao.nr-requisicao:FGCOLOR IN BROWSE br-requisicao = 8   
            tt-requisicao.sequencia:FGCOLOR IN BROWSE br-requisicao     = 8   
            tt-requisicao.it-codigo:FGCOLOR IN BROWSE br-requisicao     = 8  
            tt-requisicao.qt-a-atender:FGCOLOR IN BROWSE br-requisicao  = 8
            tt-requisicao.dt-requisica:FGCOLOR IN BROWSE br-requisicao  = 8 
            tt-requisicao.dt-entrega:FGCOLOR IN BROWSE br-requisicao    = 8 
            tt-requisicao.sc-codigo:FGCOLOR IN BROWSE br-requisicao     = 8 
            tt-requisicao.descricao:FGCOLOR IN BROWSE br-requisicao     = 8.     
  ELSE
     ASSIGN tt-requisicao.zerar:FGCOLOR IN BROWSE br-requisicao         = ?      
            tt-requisicao.nr-requisicao:FGCOLOR IN BROWSE br-requisicao = ?   
            tt-requisicao.sequencia:FGCOLOR IN BROWSE br-requisicao     = ?   
            tt-requisicao.it-codigo:FGCOLOR IN BROWSE br-requisicao     = ?  
            tt-requisicao.qt-a-atender:FGCOLOR IN BROWSE br-requisicao  = ? 
            tt-requisicao.dt-requisica:FGCOLOR IN BROWSE br-requisicao  = ? 
            tt-requisicao.dt-entrega:FGCOLOR IN BROWSE br-requisicao    = ? 
            tt-requisicao.sc-codigo:FGCOLOR IN BROWSE br-requisicao     = ? 
            tt-requisicao.descricao:FGCOLOR IN BROWSE br-requisicao     = ?.  
            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma w-livre
ON CHOOSE OF bt-confirma IN FRAME f-cad /* Button 1 */
DO:                       
    DISP "Filtrando Dados..." WITH FRAME f-a VIEW-AS DIALOG-BOX COLOR blue/white.
    RUN pi-valida-selecao.

    FOR EACH tt-requisicao: DELETE tt-requisicao. END.
    FOR EACH tt-req-zerada: DELETE tt-req-zerada. END.

    FOR EACH requisicao WHERE requisicao.nr-requisicao >= INTEGER(i-nr-requisicao-ini:SCREEN-VALUE) AND
                              requisicao.nr-requisicao <= INTEGER(i-nr-requisicao-fim:SCREEN-VALUE) AND
                              requisicao.dt-requisicao >= INPUT FRAME {&FRAME-NAME} dt-requisicao-ini AND
                              requisicao.dt-requisicao <= INPUT FRAME {&FRAME-NAME} dt-requisicao-fim AND
                              requisicao.sc-codigo     >= c-sc-codigo-ini:SCREEN-VALUE              AND
                              requisicao.sc-codigo     <= c-sc-codigo-fim:SCREEN-VALUE              AND 
                              requisicao.tp-requis      = 1 /* Requisicao Material */ NO-LOCK:

        FOR EACH it-requisicao WHERE it-requisicao.nr-requisicao =  requisicao.nr-requisicao     AND
                                     it-requisicao.it-codigo     >= c-it-codigo-ini:SCREEN-VALUE AND
                                     it-requisicao.it-codigo     <= c-it-codigo-fim:SCREEN-VALUE AND 
                                     it-requisicao.qt-a-atender  <> 0 AND 
                                     it-requisicao.estado      = 1 /* Aprovada */  NO-LOCK:

              IF it-requisicao.dt-entrega + INPUT FRAME {&FRAME-NAME} nr-dias > TODAY OR 
                 INPUT FRAME {&FRAME-NAME} nr-dias = 0 THEN DO: /* Se colocar zero ir  considerar todas as RMs */

                  CREATE tt-requisicao.
                  ASSIGN tt-requisicao.nr-requisicao = requisicao.nr-requisicao
                         tt-requisicao.it-codigo     = it-requisicao.it-codigo
                         tt-requisicao.sequencia     = it-requisicao.sequencia
                         tt-requisicao.dt-requisicao = requisicao.dt-requisicao
                         tt-requisicao.dt-entrega    = it-requisicao.dt-entrega
                         tt-requisicao.descricao     = it-requisicao.narrativa
                         tt-requisicao.sc-codigo     = it-requisicao.sc-codigo
                         tt-requisicao.qt-a-atender  = it-requisicao.qt-a-atender
                         tt-requisicao.zerar         = '*'
                         tt-requisicao.nome-abrev    = requisicao.nome-abrev
                         tt-requisicao.usuario       = c-seg-usuario.
              END.
            
        END.
    END.

    HIDE FRAME f-a.
    OPEN QUERY br-requisicao FOR EACH tt-requisicao NO-LOCK.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-livre
ON CHOOSE OF bt-nenhum IN FRAME f-cad /* Nenhum */
DO:
   FOR EACH tt-requisicao:
     ASSIGN tt-requisicao.zerar = ''.
   END.

  IF CAN-FIND(FIRST tt-requisicao) = YES THEN br-requisicao:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-livre
ON CHOOSE OF bt-todos IN FRAME f-cad /* Todos */
DO:

   FOR EACH tt-requisicao:
     ASSIGN tt-requisicao.zerar = '*'.
   END.

   IF CAN-FIND(FIRST tt-requisicao) = YES THEN br-requisicao:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 w-livre
ON CHOOSE OF BUTTON-1 IN FRAME f-cad /* Zerar Saldos das RMs */
DO:

    MESSAGE 'Deseja Zerar os Saldos das RMs selecionadas ?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-zerar AS LOG.

    IF l-zerar = YES THEN DO:

        FOR EACH tt-requisicao WHERE tt-requisicao.zerar = '*':
            FIND FIRST it-requisicao WHERE it-requisicao.nr-requisicao = tt-requisicao.nr-requisicao AND
                                           it-requisicao.sequencia     = tt-requisicao.sequencia     AND
                                           it-requisicao.it-codigo     = tt-requisicao.it-codigo  NO-ERROR.
            IF AVAILABLE it-requisicao THEN
            DO:
                CREATE tt-req-zerada.
                BUFFER-COPY tt-requisicao TO tt-req-zerada.

                FIND FIRST requisicao OF it-requisicao NO-ERROR.

                ASSIGN it-requisicao.qt-a-atender = 0  /* Zera saldos dos itens da requisicao */
                       it-requisicao.narrativa    = it-requisicao.narrativa + chr(10) + chr(10) + '** Saldo Zerado automaticamente por ' + c-seg-usuario + ' em ' + STRING(TODAY,"99/99/9999").

                IF AVAIL requisicao THEN ASSIGN requisicao.situacao = 2. /* Fechada */

            END.
        END.    

        IF CAN-FIND(FIRST tt-requisicao WHERE tt-requisicao.zerar = '*') THEN DO:
           MESSAGE "Saldos de Requisi‡Æo de Materias selecionados zerado com sucesso."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.

           OUTPUT TO VALUE(INPUT FRAME {&FRAME-NAME} c-arquivo).
           FOR EACH tt-req-zerada NO-LOCK:

               ASSIGN tt-req-zerada.descricao = REPLACE(tt-req-zerada.descricao,CHR(10)," ").

               DISP tt-req-zerada.nr-requisicao  COLUMN-LABEL "Nr Requis"         
                    tt-req-zerada.it-codigo      COLUMN-LABEL "Item"              
                    tt-req-zerada.sequencia      COLUMN-LABEL "Seq"               
                    tt-req-zerada.descricao      COLUMN-LABEL "Narrativa"         
                    tt-req-zerada.dt-requisicao  COLUMN-LABEL "Dt Requis" 
                    tt-req-zerada.dt-entrega     COLUMN-LABEL "Dt Entrega"
                    tt-req-zerada.sc-codigo      COLUMN-LABEL "C.Custo"           
                    tt-req-zerada.qt-a-atender   COLUMN-LABEL "Qtde Zerada"        
                    tt-req-zerada.nome-abrev     COLUMN-LABEL "Nome Requisitante"      
                    tt-req-zerada.usuario        COLUMN-LABEL "Usuario"
                    WITH WIDTH 300 STREAM-IO.  
           END.
           OUTPUT CLOSE.
           DOS SILENT VALUE(INPUT FRAME {&FRAME-NAME} c-arquivo).

        END.
    
        /*OPEN QUERY br-requisicao FOR EACH tt-requisicao NO-LOCK.*/

        APPLY 'CHOOSE' TO bt-confirma IN FRAME {&FRAME-NAME}. 

    END. /* IF l-zerar = YES THEN DO: */
END.


/*
  doc-pend-aprov OF it-requisicao (nr-requisicao,sequencia,it-codigo)
*/

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
             i-nr-requisicao-ini:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY i-nr-requisicao-ini i-nr-requisicao-fim c-it-codigo-ini 
          c-it-codigo-fim c-sc-codigo-ini c-sc-codigo-fim dt-requisicao-ini 
          dt-requisicao-fim nr-dias c-arquivo 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-2 IMAGE-5 IMAGE-6 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 
         IMAGE-15 IMAGE-16 RECT-3 i-nr-requisicao-ini i-nr-requisicao-fim 
         c-it-codigo-ini c-it-codigo-fim bt-confirma c-sc-codigo-ini 
         c-sc-codigo-fim dt-requisicao-ini dt-requisicao-fim nr-dias c-arquivo 
         br-requisicao bt-todos bt-nenhum BUTTON-1 
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

  {utp/ut9000.i "zerar-saldos-rmv2" "2.06.00.001"}
    
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  ASSIGN dt-requisicao-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '01/' + STRING(MONTH(TODAY)) + '/' + STRING(YEAR(TODAY))
         dt-requisicao-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '31/12/9999'.

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
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
IF INPUT FRAME {&FRAME-NAME} i-nr-requisicao-fim < INPUT FRAME {&FRAME-NAME} i-nr-requisicao-ini THEN
DO:
   MESSAGE "Valores para N£mero de Requisi‡Æo inv lidos." SKIP
           "Favor digitar novamente."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "ENTRY":U TO i-nr-requisicao-ini IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END.

IF INPUT FRAME {&FRAME-NAME} c-it-codigo-fim < INPUT FRAME {&FRAME-NAME} c-it-codigo-ini THEN
DO:
   MESSAGE "Valores para C¢digo de Item inv lidos." SKIP
           "Favor digitar novamente."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "ENTRY":U TO c-it-codigo-ini IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END.

IF INPUT FRAME {&FRAME-NAME} c-sc-codigo-fim < INPUT FRAME {&FRAME-NAME} c-sc-codigo-ini THEN
DO:
   MESSAGE "Valores para Centro de Custo inv lidos." SKIP
           "Favor digitar novamente."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "ENTRY":U TO c-sc-codigo-ini IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END.

IF INPUT FRAME {&FRAME-NAME} dt-requisicao-fim < INPUT FRAME {&FRAME-NAME} dt-requisicao-ini THEN
DO:
   MESSAGE "Valores para Data de Requisi‡Æo inv lidos." SKIP
           "Favor digitar novamente."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "ENTRY":U TO dt-requisicao-ini IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END.

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
  {src/adm/template/snd-list.i "tt-requisicao"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

