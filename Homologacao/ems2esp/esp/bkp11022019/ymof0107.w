&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME w-livre


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-es-movto-ext-item-cfa NO-UNDO LIKE es-movto-ext-item-cfa.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YMOF0107 12.01.19.014}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
/*     {include/i-license-manager.i <programa> <m¢dulo>} */
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
def var h-acomp         as handle no-undo.    


DEFINE VARIABLE c-empresa-i       AS CHARACTER INIT ""            NO-UNDO.
DEFINE VARIABLE c-empresa-f       AS CHARACTER INIT "ZZZZZ"       NO-UNDO.
DEFINE VARIABLE c-estab-i         AS CHARACTER INIT ""            NO-UNDO.
DEFINE VARIABLE c-estab-f         AS CHARACTER INIT "ZZZZZ"       NO-UNDO.
DEFINE VARIABLE c-cfa-i           AS CHARACTER INIT ""            NO-UNDO.
DEFINE VARIABLE c-cfa-f           AS CHARACTER INIT "ZZZZZ"       NO-UNDO.
DEFINE VARIABLE c-item-i          AS CHARACTER INIT ""  FORMAT "X(16)"     NO-UNDO.
DEFINE VARIABLE c-item-f          AS CHARACTER INIT "ZZZZZZZZZZZZZZZ" FORMAT "X(16)"     NO-UNDO.
DEFINE VARIABLE c-descricao-i     AS CHARACTER INIT "" FORMAT "X(60)"     NO-UNDO.
DEFINE VARIABLE c-descricao-f     AS CHARACTER INIT "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" FORMAT "X(60)"     NO-UNDO.
DEFINE VARIABLE d-implat-i        AS DATE INIT 01/01/2001 FORMAT "99/99/9999"     NO-UNDO.
DEFINE VARIABLE d-implat-f        AS DATE INIT 12/31/9999 FORMAT "99/99/9999"     NO-UNDO.
DEFINE VARIABLE c-requisitante-i  AS CHARACTER INIT "" FORMAT "X(12)"     NO-UNDO.
DEFINE VARIABLE c-requisitante-f  AS CHARACTER INIT "ZZZZZZZZZZZZ" FORMAT "X(12)"     NO-UNDO.
/* DEFINE VARIABLE c-arq-model       AS CHARACTER FORMAT "x(40)"  NO-UNDO.  */
/* DEFINE VARIABLE c-arq-dest        AS CHARACTER FORMAT "x(40)"  NO-UNDO.  */
DEFINE VARIABLE c-empresa         AS CHARACTER   NO-UNDO.



DEF NEW GLOBAL SHARED VAR c-arq-model AS CHAR FORMAT "x(40)" INIT "D:/quarentena/tax/modelos/C†lculoCFA-v07.xlsm" NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-arq-dest  AS CHAR FORMAT "x(40)" NO-UNDO.





DEFINE VARIABLE c-arquivo-entrada AS CHARACTER   NO-UNDO.



DEFINE VARIABLE c-item-reclass    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-estab-reclass   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-resposta        AS LOGICAL     NO-UNDO.


 DEF TEMP-TABLE tt-erros
    FIELD erro AS CHAR FORMAT "x(100)"
    FIELD it-codigo AS CHAR FORMAT "x(16)".

    

DEF TEMP-TABLE tt-chancelador
    FIELD marcado          AS CHAR
    FIELD ep-codigo        AS CHAR
    FIELD IdSIN            AS INT
    FIELD IdKlassmatt      AS INT
    FIELD it-codigo        AS CHAR FORMAT "x(16)"
    FIELD desc-item        AS CHAR FORMAT "x(60)"
    FIELD cfa              AS CHAR FORMAT "x(4)"
    FIELD desc-cfa         AS CHAR FORMAT "x(60)"
    FIELD responsavel      AS CHAR FORMAT "x(12)"
    FIELD estatus          AS CHAR FORMAT "x(15)"
    FIELD rowid-movto-item AS ROWID.



DEFINE VARIABLE c-desc-cfa AS CHARACTER  FORMAT "x(35)"  NO-UNDO.

DEFINE VARIABLE i-linha-ini    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-coluna-ini   AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-qt-perguntas AS INTEGER     NO-UNDO.

DEFINE VARIABLE chExcelapp  AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorkbook  AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE      NO-UNDO.


DEF TEMP-TABLE tt-es-mov-ext-item-cfa LIKE es-movto-ext-item-cfa.


DEF BUFFER b-es-movto-ext-item-cfa FOR es-movto-ext-item-cfa.
DEF BUFFER b1-es-movto-ext-item-cfa FOR es-movto-ext-item-cfa.


/*  DEF SHARED VAR c-seg-usuario AS CHAR FORMAT "x(16)" NO-UNDO. */
/*  DEF SHARED VAR i-ep-codigo-usuario AS CHAR NO-UNDO  . */
 DEFINE VARIABLE c-hora AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE c-data AS CHARACTER FORMAT "x(10)"  NO-UNDO.

DEF TEMP-TABLE tt-1
   FIELD TAB   AS CHAR
   FIELD cod   AS INT
   FIELD DEScr AS CHAR FORMAT "x(40)" .

def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.


DEF TEMP-TABLE TT-VERSAO-INTEGR
FIELD cod-versao-integracao AS  INTEGER FORMAT "999" INIT 1     
FIELD ind-origem-msg        AS integer  FORMAT "99".    


DEF TEMP-TABLE TT-ERROS-GERAL
FIELD  identif-msg        AS   CHAR     FORMAT "x(60)"  
FIELD  num-sequencia-erro AS   INTEGER  FORMAT "999"    
FIELD  cod-erro           AS   Integer  FORMAT "99999"  
FIELD  des-erro           AS   Char     FORMAT "x(60)"  
FIELD  cod-maq-origem     AS   Integer  FORMAT "999"    
FIELD  num-processo           AS   Integer  FORMAT "999999999". 

DEF TEMP-TABLE TT-ITEM-CODIGO
FIELD it-codigo       AS char FORMAT  "x(16)"   
FIELD descricao       AS CHAR FORMAT "x(36)"    
FIELD new-it-codigo   AS CHAR FORMAT "x(16)"    
FIELD ge-codigo       AS INTEGER FORMAT ">9"    
FIELD cod-erro        AS INTEGER FORMAT "99999" 
FIELD des-erro        AS  CHAR FORMAT "x(60)"   
FIELD cod-maq-origem  AS INTEGER FORMAT "9999"
FIELD num-processo    AS   INTEGER FORMAT ">>>>>>>>9" INIT 0
FIELD num-sequencia   AS  INTEGER FORMAT ">>>>>9" INIT  0
FIELD ind-tipo-movto  AS INTEGER FORMAT "99"     INIT 1.



DEFINE TEMP-TABLE tt-rec-relatorio NO-UNDO 
    field it-codigo     like item.it-codigo
    field tabela        as char format "x(23)"
    field reg-lido      as int
    field hora-ini      as char format "x(10)"
    field hora-fim      as char format "x(10)".

DEF TEMP-TABLE tt-item1 LIKE ITEM.

DEFINE VARIABLE i-nr-randon AS INTEGER     NO-UNDO.



/**************************************************/
 DEF BUFFER b-item-para FOR ITEM.
DEF BUFFER b-item-class FOR ITEM.

 DEF TEMP-TABLE tt-item-de LIKE ITEM.


 def temp-table tt-item no-undo like item 
     field cod-maq-origem   as   integer FORMAT "9999"
     field num-processo     as   integer FORMAT ">>>>>>>>9" initial 0
     field num-sequencia    as   integer FORMAT ">>>>>9"    initial 0
     field ind-tipo-movto   as   integer FORMAT "99"        initial 1
     field cod-erro         as   integer FORMAT "99999" 
     field des-erro         as   char    FORMAT "x(60)"
     INDEX ch-codigo IS PRIMARY  cod-maq-origem
                                 num-processo
                                 num-sequencia.

 define temp-table tt-paramCD0205 no-undo
     field destino          as integer
     field arquivo          as char
     field usuario          as char
     field data-exec        as date
     field hora-exec        as integer
     field classifica       as integer
     field l-todos          as logical.

 define temp-table tt-altera no-undo
     field it-codigo     like item.it-codigo
     field descricao     as char FORMAT "x(60)"
     field un            like item.un
     field new-it-codigo like item.it-codigo
     field new-un        like item.un
     field fator-conv    as decimal FORMAT ">,>>9.99999" 
     field ge-codigo     like item.ge-codigo
     field old-ge-codigo like item.ge-codigo.

 define temp-table tt-elimina no-undo
     field it-codigo     like item.it-codigo
     field un            like item.un
     field descricao     as char FORMAT "x(60)".

 define temp-table tt-digitaCD0205 no-undo
     field it-codigo     like item.it-codigo
     field descricao     as char FORMAT "x(60)"
     field un            like item.un
     field new-it-codigo like item.it-codigo
     field new-un        like item.un
     field fator-conv    as decimal FORMAT ">,>>9.99999"
     field ge-codigo     like item.ge-codigo
     field old-ge-codigo like item.ge-codigo
     field tipo-operacao as char FORMAT "x".

 def temp-table tt-raw-digitaCD0205
    field raw-digita      as raw.

 def temp-table tt-raw-eliminaCD0205
     field raw-elimina      as raw.

DEFINE VARIABLE i-cod-depto   AS INTEGER     NO-UNDO.
DEFINE VARIABLE l-oriundo-jde AS LOGICAL     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-chancelador

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 tt-chancelador.marcado tt-chancelador.ep-codigo /* tt-chancelador.cod-estabel */ tt-chancelador.it-codigo desc-item tt-chancelador.cfa c-desc-cfa tt-chancelador.IdSIN tt-chancelador.IdKlassmatt   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3   
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH tt-chancelador WHERE tt-chancelador.ep-codigo = i-ep-codigo-usuario
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH tt-chancelador WHERE tt-chancelador.ep-codigo = i-ep-codigo-usuario.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 tt-chancelador
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 tt-chancelador


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 BUTTON-1 BUTTON-2 BUTTON-3 ~
BUTTON-4 BUTTON-5 BROWSE-3 bt-marca bt-todos bt-inverte bt-historico 

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
DEFINE BUTTON bt-historico 
     LABEL "Historico" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-inverte 
     LABEL "Inverte" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-marca 
     LABEL "Marcar" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-todos 
     LABEL "Todos" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "image\toolbar\im-ran.bmp":U
     LABEL "Button 1" 
     SIZE 4.86 BY 1.21.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "image/toolbar/im-imp.bmp":U
     LABEL "Button 2" 
     SIZE 4.86 BY 1.21.

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "image/toolbar/im-exp.bmp":U
     LABEL "Button 3" 
     SIZE 4.86 BY 1.21.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "image/toolbar/im-undo.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-undo.bmp":U
     LABEL "Button 4" 
     SIZE 4.86 BY 1.21.

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "image/toolbar/im-add.bmp":U
     LABEL "Button 5" 
     SIZE 4.86 BY 1.21.

DEFINE BUTTON BUTTON-6 
     IMAGE-UP FILE "image/toolbar/im-param.bmp":U
     LABEL "Button 6" 
     SIZE 4.86 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113.72 BY 1.88.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 115 BY 1.75
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      tt-chancelador SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 w-livre _FREEFORM
  QUERY BROWSE-3 DISPLAY
      tt-chancelador.marcado     LABEL ""
   tt-chancelador.ep-codigo      LABEL "Empresa"
/*    tt-chancelador.cod-estabel    LABEL "Estab" */
   tt-chancelador.it-codigo      LABEL "Item"
   desc-item      LABEL "Descriá∆o"  WIDTH 30
   tt-chancelador.cfa            LABEL "CFA" WIDTH 4
   c-desc-cfa     LABEL "Descriá∆o CFA" WIDTH 28
   tt-chancelador.IdSIN          LABEL "SIN" WIDTH 6
   tt-chancelador.IdKlassmatt    LABEL "IdKlassmatt"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 114.14 BY 16.75 ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     BUTTON-1 AT ROW 1.29 COL 2 WIDGET-ID 2
     BUTTON-2 AT ROW 1.29 COL 7.86 WIDGET-ID 4
     BUTTON-3 AT ROW 1.29 COL 14 WIDGET-ID 6
     BUTTON-4 AT ROW 1.29 COL 20.14 WIDGET-ID 8
     BUTTON-5 AT ROW 1.29 COL 40 WIDGET-ID 10
     BUTTON-6 AT ROW 1.29 COL 92.43 WIDGET-ID 12
     BROWSE-3 AT ROW 3.5 COL 2 WIDGET-ID 200
     bt-marca AT ROW 20.67 COL 2.72 WIDGET-ID 14
     bt-todos AT ROW 20.67 COL 18.43 WIDGET-ID 18
     bt-inverte AT ROW 20.67 COL 34 WIDGET-ID 20
     bt-historico AT ROW 20.67 COL 49.86 WIDGET-ID 24
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 20.38 COL 2.14 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 115.14 BY 21.25 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-es-movto-ext-item-cfa T "?" NO-UNDO mgesp es-movto-ext-item-cfa
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Chancela Tributaria"
         HEIGHT             = 21.25
         WIDTH              = 115.14
         MAX-HEIGHT         = 27.54
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 27.54
         VIRTUAL-WIDTH      = 194.86
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
/* BROWSE-TAB BROWSE-3 BUTTON-6 f-cad */
/* SETTINGS FOR BUTTON BUTTON-6 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-6:HIDDEN IN FRAME f-cad           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-chancelador WHERE tt-chancelador.ep-codigo = i-ep-codigo-usuario
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Chancela Tributaria */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Chancela Tributaria */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 w-livre
ON MOUSE-SELECT-DBLCLICK OF BROWSE-3 IN FRAME f-cad
DO:
  IF AVAIL tt-chancelador THEN DO:

      IF tt-chancelador.marcado = "*" THEN ASSIGN tt-chancelador.marcado = "" .
      ELSE  ASSIGN tt-chancelador.marcado = "*".
  END.

  BROWSE-3:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 w-livre
ON ROW-DISPLAY OF BROWSE-3 IN FRAME f-cad
DO:
  
    FIND es-cfa WHERE es-cfa.classe = tt-chancelador.cfa NO-LOCK NO-ERROR.

    IF AVAIL es-cfa THEN DO:
        
        ASSIGN c-desc-cfa = es-cfa.descricao.

    END.
    ELSE DO:
       c-desc-cfa = "CFA n∆o definida".
    END.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-historico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-historico w-livre
ON CHOOSE OF bt-historico IN FRAME f-cad /* Historico */
DO:

  IF NOT AVAIL tt-chancelador  THEN RETURN.

  RUN esp\ymof0107e.w (INPUT tt-chancelador.it-codigo,
                     INPUT tt-chancelador.ep-codigo).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inverte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inverte w-livre
ON CHOOSE OF bt-inverte IN FRAME f-cad /* Inverte */
DO:
  FOR EACH tt-chancelador:
     IF tt-chancelador.marcado = "" THEN ASSIGN tt-chancelador.marcado = "*" .
     ELSE ASSIGN tt-chancelador.marcado = "".
  END.
  BROWSE-3:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca w-livre
ON CHOOSE OF bt-marca IN FRAME f-cad /* Marcar */
DO:
  IF AVAIL tt-chancelador THEN DO:
      ASSIGN tt-chancelador.marcado = "*" .
      BROWSE-3:REFRESH().




  END.



 


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-livre
ON CHOOSE OF bt-todos IN FRAME f-cad /* Todos */
DO:
  FOR EACH tt-chancelador:
      ASSIGN tt-chancelador.marcado = "*" .
  END.
  BROWSE-3:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 w-livre
ON CHOOSE OF BUTTON-1 IN FRAME f-cad /* Button 1 */
DO:
  RUN esp\ymof0107a.w (
                    INPUT-OUTPUT  c-empresa-i,    
                    INPUT-OUTPUT  c-empresa-f,    
                    INPUT-OUTPUT  c-estab-i,      
                    INPUT-OUTPUT  c-estab-f,      
                    INPUT-OUTPUT  c-cfa-i,        
                    INPUT-OUTPUT  c-cfa-f,        
                    INPUT-OUTPUT  c-item-i,       
                    INPUT-OUTPUT  c-item-f,       
                    INPUT-OUTPUT  c-descricao-i,  
                    INPUT-OUTPUT  c-descricao-f,  
                    INPUT-OUTPUT  d-implat-i,     
                    INPUT-OUTPUT  d-implat-f,     
                    INPUT-OUTPUT  c-requisitante-i, 
                    INPUT-OUTPUT  c-requisitante-f ).

  IF c-empresa-i <> "" OR c-empresa-f <> "" THEN DO:
    BUTTON-4:LOAD-IMAGE-UP("image\toolbar\im-undo.bmp").
    ENABLE BUTTON-4 WITH FRAME f-cad.
  END.




END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 w-livre
ON CHOOSE OF BUTTON-2 IN FRAME f-cad /* Button 2 */
DO:  
  RUN pi-le-param-planilha.
  RUN pi-gera-planilha.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 w-livre
ON CHOOSE OF BUTTON-3 IN FRAME f-cad /* Button 3 */
DO:
  RUN esp\ymof0107d.w (INPUT-OUTPUT c-arquivo-entrada).
  IF trim(c-arquivo-entrada) = "" THEN DO:
      run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Planilha de entrada n∆o informada!!!" ).
      RETURN .
  END.

  run utp/ut-acomp.p persistent set h-acomp.
  {utp/ut-liter.i aaaaaaaaaaaaaaaaaa bbb c}
  run pi-inicializar in h-acomp (input "Alterando Codigos":U).

  run pi-acompanhar in h-acomp (input "Carrega Combo":U).
  RUN pi-carrega-combos.

  run pi-acompanhar in h-acomp (input "Planilha Entrada":U).
  RUN pi-le-plan-entrada.

  run pi-acompanhar in h-acomp (input "Fecha Planilha":U).
  RUN pi-fecha-planilha-le.
  RUN pi-carrega-browser.
  RUN pi-retorno-integra.
  run pi-finalizar in h-acomp.

  FIND FIRST es-integra-retorno NO-LOCK NO-ERROR.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 w-livre
ON CHOOSE OF BUTTON-4 IN FRAME f-cad /* Button 4 */
DO:
  RUN pi-carrega-browser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 w-livre
ON CHOOSE OF BUTTON-5 IN FRAME f-cad /* Button 5 */
DO:
  RUN esp\ymof0107c.w (INPUT-OUT    c-item-reclass,
                       INPUT-OUTPUT c-estab-reclass,
                       INPUT-OUTPUT c-empresa,
                       INPUT-OUTPU  l-resposta ).



  
  IF l-resposta THEN RUN pi-obsoleta-item.


  APPLY "choose" TO BUTTON-5.

  RUN pi-carrega-browser.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 w-livre
ON CHOOSE OF BUTTON-6 IN FRAME f-cad /* Button 6 */
DO:
  RUN esp\ymof0107b.w (INPUT-OUT c-arq-model,INPUT-OUTPUT c-arq-dest ).
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

    ASSIGN d-implat-i =  TODAY - 30.

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
       RUN set-position IN h_p-exihel ( 1.29 , 99.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             BUTTON-6:HANDLE IN FRAME f-cad , 'AFTER':U ).
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
  ENABLE rt-button RECT-1 BUTTON-1 BUTTON-2 BUTTON-3 BUTTON-4 BUTTON-5 BROWSE-3 
         bt-marca bt-todos bt-inverte bt-historico 
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
PROCEDURE local-initialize PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

    {utp/ut9000.i "YMOF0107" "12.01.19.001"}  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
  BUTTON-4:LOAD-IMAGE-UP("image\toolbar\im-undo.bmp").
  ENABLE BUTTON-4 WITH FRAME f-cad. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-planilha-le w-livre 
PROCEDURE pi-abre-planilha-le :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-senha AS CHARACTER   NO-UNDO.

FIND FIRST es-param-tax NO-LOCK  NO-ERROR.

CREATE "Excel.Application" chExcelapp.
chExcelapp:VISIBLE = TRUE.
chWorkbook         = chExcelapp:Workbooks:ADD(es-param-tax.arq-plan-tax) .
chWorksheet        = chExcelapp:Sheets:ITEM(4).

c-senha = chWorksheet:range("hh1"):VALUE.
chWorksheet:unprotect(c-senha). 

chWorksheet        = chExcelapp:Sheets:ITEM(1).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-planilha-le1 w-livre 
PROCEDURE pi-abre-planilha-le1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-senha AS CHARACTER   NO-UNDO.

FIND FIRST es-param-tax NO-LOCK  NO-ERROR.

CREATE "Excel.Application" chExcelapp.
chExcelapp:VISIBLE = TRUE.
chWorkbook         = chExcelapp:Workbooks:ADD(es-param-tax.arq-plan-tax) .
chWorksheet        = chExcelapp:Sheets:ITEM(1).
chWorksheet:unprotect("senha").
chWorksheet        = chExcelapp:Sheets:ITEM(4). 
chWorksheet:unprotect("SENHA").                 
chWorksheet        = chExcelapp:Sheets:ITEM(1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-altera-item w-livre 
PROCEDURE pi-altera-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

           IF AVAIL es-de-para-it-padr THEN DO:
               CREATE tt-item-codigo.                                                                      
               ASSIGN tt-item-codigo.it-codigo     = ITEM.it-codigo                                        
                      tt-item-codigo.descricao     = ITEM.desc-item                                        
                      tt-item-codigo.new-it-codigo = es-de-para-it-padr.it-codigo-padr     
                      tt-item-codigo.ge-codigo     = IF AVAIL es-cfa-ge THEN es-cfa-ge.ge-codigo ELSE 0.
           END.
           ELSE DO:
               CREATE tt-item-codigo.
               ASSIGN tt-item-codigo.it-codigo     = ITEM.it-codigo
                      tt-item-codigo.descricao     = ITEM.desc-item
                      tt-item-codigo.new-it-codigo = tt-altera.new-it-codigo.
                      tt-item-codigo.ge-codigo     = IF AVAIL es-cfa-ge THEN es-cfa-ge.ge-codigo ELSE 0.

           END.


           EMPTY TEMP-TABLE tt-erros-geral.
          
           CREATE TT-VERSAO-INTEGR.
           run cdp/cdapi345.p (INPUT TABLE   tt-versao-integr,          
                              OUTPUT TABLE  tt-erros-geral,            
                              INPUT  TABLE  tt-item-codigo,            
                              INPUT-OUTPUT TABLE TT-REC-RELATORIO).    

           EMPTY TEMP-TABLE tt-erros.


           FIND FIRST tt-erros-geral NO-LOCK NO-ERROR.
           IF AVAIL tt-erros-geral THEN DO:
              
               FOR EACH tt-erros-geral:
                   CREATE tt-erros.                                                                              
                   tt-erros.erro      = STRING(tt-erros-geral.cod-erro) + "-" + tt-erros-geral.des-erro + "-" + ITEM.it-codigo.       
                   tt-erros.it-codigo = ITEM.it-codigo .
               END.

               FOR EACH tt-erros:
                   PUT UNFORMAT tt-erros.ERRO SKIP.
               END.
               NEXT.                                                                                        
           END.
           ELSE
           DO:
              /* Begins Jan/2018 - Willians Ambrosio DKP */
              FOR EACH tt-item-codigo,
                 FIRST b-item-para NO-LOCK WHERE 
                       b-item-para.IT-CODIGO = tt-item-codigo.IT-CODIGO:        
            
                  FIND FIRST es-klassmatt-integr WHERE
                             es-klassmatt-integr.idklassmatt = 999999999                                   AND
                             es-klassmatt-integr.dt-trans    = TODAY                                       AND
                             es-klassmatt-integr.hr-trans    = REPLACE(STRING(TIME,"HH:MM:SS"),":","")     EXCLUSIVE-LOCK NO-ERROR.
                  IF NOT AVAIL es-klassmatt-integr THEN
                  DO:
                     CREATE es-klassmatt-integr.
                     BUFFER-COPY b-item-para          TO es-klassmatt-integr
                                                  ASSIGN es-klassmatt-integr.idklassmatt = 999999999                               
                                                         es-klassmatt-integr.dt-trans    = TODAY                                   
                                                         es-klassmatt-integr.hr-trans    = REPLACE(STRING(TIME,"HH:MM:SS"),":","") 
                                                         es-klassmatt-integr.log-retorno   = "YMOF0107.W - TIPO DE CONTROLE ATUALIZADO Tipo-Contr = "  + STRING(b-item-para.tipo-contr)      
                                                         es-klassmatt-integr.statusRetorno = "N".                                              
                  END.        
              END.
              /* End Jan/2018 - Willians Ambrosio DKP */
           END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-altera-tabelas-internas w-livre 
PROCEDURE pi-altera-tabelas-internas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST tt-altera NO-LOCK NO-ERROR.

        /* Tratar tabelas espec°ficas */
        DISABLE TRIGGERS FOR LOAD OF mab-evento-mat.
        FOR EACH mab-evento-mat FIELDS( it-codigo)
            WHERE mab-evento-mat.it-codigo = tt-altera.it-codigo:
            ASSIGN mab-evento-mat.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF ext-item-servico.
        FOR EACH ext-item-servico FIELDS( it-codigo)
            WHERE ext-item-servico.it-codigo = tt-altera.it-codigo:
            ASSIGN ext-item-servico.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF es-it-depto.
        FOR FIRST es-it-depto FIELDS( it-codigo)
            WHERE es-it-depto.it-codigo = tt-altera.it-codigo:
            ASSIGN es-it-depto.it-codigo = tt-altera.new-it-codigo
                   es-it-depto.cod-dept  = i-cod-depto.
        END.
        DISABLE TRIGGERS FOR LOAD OF ext-item-arq.
        FOR EACH ext-item-arq FIELDS( it-codigo)
            WHERE ext-item-arq.it-codigo = tt-altera.it-codigo:
            ASSIGN ext-item-arq.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF ext-audit-item-fam.
        FOR EACH ext-audit-item-fam FIELDS( it-codigo)
            WHERE ext-audit-item-fam.it-codigo = tt-altera.it-codigo:
            ASSIGN ext-audit-item-fam.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF ext-item-estab.
        FOR EACH ext-item-estab FIELDS( it-codigo)
            WHERE ext-item-estab.it-codigo = tt-altera.it-codigo:
            ASSIGN ext-item-estab.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF item-fornec-ext.
        FOR EACH item-fornec-ext FIELDS( it-codigo)
            WHERE item-fornec-ext.it-codigo = tt-altera.it-codigo:
            ASSIGN item-fornec-ext.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF ext-item-uni-estab01.
        FOR EACH ext-item-uni-estab01 FIELDS( it-codigo)
            WHERE ext-item-uni-estab01.it-codigo = tt-altera.it-codigo:
            ASSIGN ext-item-uni-estab01.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF ext-item-uni-estab.
        FOR EACH ext-item-uni-estab FIELDS( it-codigo)
            WHERE ext-item-uni-estab.it-codigo = tt-altera.it-codigo:
            ASSIGN ext-item-uni-estab.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF esp-necessidade-oc.
        FOR EACH esp-necessidade-oc FIELDS( it-codigo)
            WHERE esp-necessidade-oc.it-codigo = tt-altera.it-codigo:
            ASSIGN esp-necessidade-oc.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF log-ext-item-uni-estab01.
        FOR EACH log-ext-item-uni-estab01 FIELDS( it-codigo)
            WHERE log-ext-item-uni-estab01.it-codigo = tt-altera.it-codigo:
            ASSIGN log-ext-item-uni-estab01.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF ext-item-cfa.
        FOR EACH ext-item-cfa  FIELDS(ep-codigo it-codigo)
            WHERE ext-item-cfa.ep-codigo = i-ep-codigo-usuario
              AND ext-item-cfa.it-codigo = tt-altera.it-codigo:
            ASSIGN ext-item-cfa.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF ext-item-contrat.
        FOR EACH ext-item-contrat FIELDS( it-codigo)
            WHERE ext-item-contrat.it-codigo = tt-altera.it-codigo:
            ASSIGN ext-item-contrat.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF ext-ordem-compra.
        FOR EACH ext-ordem-compra  FIELDS( it-codigo)
            WHERE ext-ordem-compra.it-codigo = tt-altera.it-codigo:
            ASSIGN ext-ordem-compra.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF es-it-uf-estorno-cred.
        FOR EACH es-it-uf-estorno-cred  FIELDS( it-codigo)
            WHERE es-it-uf-estorno-cred.it-codigo = tt-altera.it-codigo:
            ASSIGN es-it-uf-estorno-cred.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF es-movto-ext-item-cfa.
        FOR EACH es-movto-ext-item-cfa  FIELDS(ep-codigo it-codigo)
            WHERE es-movto-ext-item-cfa.ep-codigo = i-ep-codigo-usuario
              AND es-movto-ext-item-cfa.it-codigo = tt-altera.it-codigo:
            ASSIGN es-movto-ext-item-cfa.it-codigo = tt-altera.new-it-codigo.
        END.
        DISABLE TRIGGERS FOR LOAD OF es-item-doc-est-natoper.
        FOR EACH es-item-doc-est-natoper FIELDS(ep-codigo it-codigo)
            WHERE es-item-doc-est-natoper.ep-codigo = i-ep-codigo-usuario
              AND es-item-doc-est-natoper.it-codigo = tt-altera.it-codigo:
            ASSIGN es-item-doc-est-natoper.it-codigo = tt-altera.new-it-codigo.
        END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-tabela-interna w-livre 
PROCEDURE pi-atualiza-tabela-interna :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-tipo-item w-livre 
PROCEDURE pi-atualiza-tipo-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-empresa  AS CHARACTER   NO-UNDO.
/* DEFINE INPUT PARAMETER p-estab    AS CHARACTER   NO-UNDO. */
DEFINE INPUT PARAMETER p-item     AS CHARACTER   NO-UNDO.

DEFINE VARIABLE i-conta01         AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-indent          AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-indent-01          AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-tipo            AS CHARACTER FORMAT "X(50)"  NO-UNDO.
DEFINE VARIABLE c-tipo-sped       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-tipo-sped1      AS CHARACTER   NO-UNDO.


FOR EACH es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.ep-codigo   = p-empresa
                                 AND es-movto-ext-item-cfa.it-codigo   = p-item 
                                  BY es-movto-ext-item-cfa.identificador DESC:
    ASSIGN i-indent = i-indent + 1.

    IF i-indent = 1 THEN DO:
        ASSIGN i-indent-01  = es-movto-ext-item-cfa.identificador.
        LEAVE.
    END.
END.

FOR EACH es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.ep-codigo      = p-empresa
                               AND   es-movto-ext-item-cfa.it-codigo      = p-item
                               AND   es-movto-ext-item-cfa.identificador  = i-indent-01
                               AND   (es-movto-ext-item-cfa.nr-seq         = 10 OR  es-movto-ext-item-cfa.nr-seq = 209) NO-LOCK:

    ASSIGN c-tipo = es-movto-ext-item-cfa.resposta.

END.


FIND ITEM WHERE ITEM.it-codigo = p-item EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL ITEM THEN DO:

    ASSIGN c-tipo-sped = ENTRY(1,c-tipo,"|").

    CASE c-tipo-sped:
        WHEN "10" THEN 
             ASSIGN  c-tipo-sped1 = "A".
        WHEN "99" THEN 
             ASSIGN  c-tipo-sped1 = "b".
        OTHERWISE
              ASSIGN  c-tipo-sped1 = c-tipo-sped .
    END CASE.

    substr(ITEM.char-2,212,1) = c-tipo-sped1.

  
   FOR EACH item-uni-estab WHERE 
            item-uni-estab.it-codigo = p-item EXCLUSIVE-LOCK:
      OVERLAY(item-uni-estab.char-1, 133, 1)  = c-tipo-sped1      .
   END.
END.



















END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-browser w-livre 
PROCEDURE pi-carrega-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE tt-chancelador.

FOR EACH ext-item-cfa WHERE 
         ext-item-cfa.ep-codigo   >= c-empresa-i AND   
         ext-item-cfa.ep-codigo   <= c-empresa-f AND   
         ext-item-cfa.classe      = "9999"       AND   
         ext-item-cfa.it-codigo   >= c-item-i    AND   
         ext-item-cfa.it-codigo   <= c-item-f,
   FIRST ITEM WHERE
         ITEM.it-codigo = ext-item-cfa.it-codigo AND   
         ITEM.desc-item >= c-descricao-i         AND   
         ITEM.desc-item <= c-descricao-f         AND   
         ITEM.data-implant >= d-implat-i         AND   
         ITEM.data-implant <= d-implat-f         NO-LOCK:

         CREATE tt-chancelador.                                
         ASSIGN tt-chancelador.it-codigo         = ITEM.it-codigo      
                tt-chancelador.desc-item         = ITEM.desc-item. 
                tt-chancelador.IdKlassmatt       = ext-item-cfa.IdKlassmatt.
                tt-chancelador.IdSIN             = ext-item-cfa.IdSIN.
                tt-chancelador.ep-codigo         = ext-item-cfa.ep-codigo.
                tt-chancelador.cfa               = "9999".

END.

{&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-combos w-livre 
PROCEDURE pi-carrega-combos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE chExcelapp     AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorkbook     AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorksheet    AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE i-linha-ini    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-coluna-ini   AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-qt-perguntas AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-tab          AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-coluna       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-linha        AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-dados        AS CHARACTER  FORMAT "x(40)"  NO-UNDO.

DEFINE VARIABLE c-nomearq      AS CHARACTER   NO-UNDO.





DEFINE VARIABLE i-conta AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-coluna1   AS CHARACTER INIT "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z"  NO-UNDO.



CREATE "Excel.Application" chExcelapp.
/* chExcelapp:VISIBLE = TRUE. */
chWorkbook         = chExcelapp:Workbooks:ADD(c-arquivo-entrada) .
chWorksheet        = chExcelapp:Sheets:ITEM(3).

i-conta = 0 .
 DO i-conta = 1 TO 20:
    i-tab = i-tab + 1.
    c-coluna = ENTRY(i-conta,c-coluna1,",").
     
      DO i-linha = 5 TO 200:
         c-coluna = ENTRY(i-conta + 1,c-coluna1,",").
         IF chWorksheet:range(c-coluna  + STRING(i-linha)):VALUE = "" OR chWorksheet:range(c-coluna  + STRING(i-linha)):VALUE = ? THEN LEAVE.
          c-coluna = ENTRY(i-conta,c-coluna1,",").

         CREATE tt-1.                                                                    
         ASSIGN tt-1.cod   = int(chWorksheet:range(c-coluna  + STRING(i-linha)):VALUE).
         c-coluna = ENTRY(i-conta + 1,c-coluna1,",").
         c-dados = chWorksheet:range(c-coluna  + STRING(i-linha)):VALUE.
         tt-1.descr = c-dados.  
         tt-1.TAB   = string(i-tab).
      END.
      i-conta = i-conta + 1.
      

 END.


 IF chExcelapp:Sheets:COUNT() < 4 THEN DO:

     

     run utp/ut-msgs.p (input "show",
                         input 17006,
                         input "Planilha  n∆o possue o layout correto favor verificar!!!" ).


     QUIT.
 END.



 IF chExcelapp:Sheets:ITEM(3):NAME <> "questionario" THEN DO:

     run utp/ut-msgs.p (input "show",
                         input 17006,
                         input "Planilha n∆o possui o layout correto favor verificar!!!" ).


     QUIT.
 END.

 chExcelapp:APPLICATION:DisplayAlerts = FALSE.
 c-nomearq = "c:\temp\ymof0107" + c-seg-usuario + ".xlsx".        
 /* OS-COMMAND SILENT DEL VALUE(c-nomearq) NO-ERROR.  */                
 chExcelapp:workbooks:item(1):SaveAs(c-nomearq,,,,,,,).           
 chExcelapp:workbooks:APPLICATION:QUIT.                           
 /* OS-COMMAND SILENT DEL value(c-nomearq) NO-ERROR.  */                

 chExcelapp:workbooks:APPLICATION:QUIT.

RELEASE OBJECT  chExcelapp  NO-ERROR. 
RELEASE OBJECT  chWorkbook  NO-ERROR. 
RELEASE OBJECT  chWorksheet NO-ERROR.

 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cd0205rp w-livre 
PROCEDURE pi-cd0205rp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE raw-paramCD0205 AS RAW      NO-UNDO.

    EMPTY TEMP-TABLE tt-paramCD0205.
    EMPTY TEMP-TABLE tt-digitaCD0205.
                                    
    create tt-paramCD0205.
    assign tt-paramCD0205.usuario    = c-seg-usuario
           tt-paramCD0205.destino    = 2 /* Arquivo*/
           tt-paramCD0205.data-exec  = TODAY 
           tt-paramCD0205.hora-exec  = TIME 
           tt-paramCD0205.l-todos    = NO 
           /*tt-paramCD0205.arquivo    = tt-paramCD0205.arquivo + "/CD0205" + STRING(TIME) + ".carga".*/
           tt-paramCD0205.arquivo    = SESSION:TEMP-DIRECTORY + "/CD0205" + STRING(TIME) + ".carga" .

    for each tt-altera:
        create tt-digitaCD0205.
        assign tt-digitaCD0205.it-codigo     = tt-altera.it-codigo
               tt-digitaCD0205.descricao     = 'TESTE DESC' /*tt-altera.descricao*/
               tt-digitaCD0205.un            = tt-altera.un
               tt-digitaCD0205.new-it-codigo = tt-altera.new-it-codigo 
               tt-digitaCD0205.new-un        = tt-altera.new-un
               tt-digitaCD0205.fator-conv    = tt-altera.fator-conv
               tt-digitaCD0205.ge-codigo     = tt-altera.ge-codigo     
               tt-digitaCD0205.old-ge-codigo = tt-altera.old-ge-codigo
               tt-digitaCD0205.tipo-operacao = "A".
    end.

    for each tt-elimina:
        create tt-digitaCD0205.
        assign tt-digitaCD0205.it-codigo     = tt-elimina.it-codigo   
               tt-digitaCD0205.un            = tt-elimina.un          
               tt-digitaCD0205.descricao     = tt-elimina.descricao
               tt-digitaCD0205.tipo-operacao = "E".
    end.

    /* Coloque aqui a l¢gica de gravaá∆o dos parÉmtros e seleá∆o na temp-table
       tt-param */ 

    raw-transfer tt-paramCD0205    to raw-paramCD0205.

    for each tt-raw-digitaCD0205:
        delete tt-raw-digitaCD0205.
    end.
    for each tt-digitaCD0205:
        create tt-raw-digitaCD0205.
        raw-transfer tt-digitaCD0205 to tt-raw-digitaCD0205.raw-digita.
    end.  


    run cdp/cd0205rp.p (input raw-paramCD0205,
                        input table tt-raw-digitaCD0205).

    /* Begins Jan/2018 - Willians Ambrosio DKP */
    FOR EACH tt-digitaCD0205,
       FIRST b-item-para NO-LOCK WHERE 
             b-item-para.IT-CODIGO = tt-digitaCD0205.IT-CODIGO:        

        FIND FIRST es-klassmatt-integr WHERE
                   es-klassmatt-integr.idklassmatt = 999999999                                   AND
                   es-klassmatt-integr.dt-trans    = TODAY                                       AND
                   es-klassmatt-integr.hr-trans    = REPLACE(STRING(TIME,"HH:MM:SS"),":","")     EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL es-klassmatt-integr THEN
        DO:
           CREATE es-klassmatt-integr.
           BUFFER-COPY b-item-para             TO es-klassmatt-integr
                                        ASSIGN es-klassmatt-integr.idklassmatt = 999999999                               
                                               es-klassmatt-integr.dt-trans    = TODAY                                   
                                               es-klassmatt-integr.hr-trans    = REPLACE(STRING(TIME,"HH:MM:SS"),":","") 
                                               es-klassmatt-integr.log-retorno   = "YMOF0107.W - TIPO DE CONTROLE ATUALIZADO Tipo-Contr = "  + STRING(b-item-para.tipo-contr)      
                                               es-klassmatt-integr.statusRetorno = "N".                                              
        END.        
    END.
    /* End Jan/2018 - Willians Ambrosio DKP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desobesoleta-item w-livre 
PROCEDURE pi-desobesoleta-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-item        AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER p-cod-estabel AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER p-classe      AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER p-ep-codi     AS CHARACTER   NO-UNDO.



FIND ITEM WHERE ITEM.it-codigo = p-item EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL ITEM THEN DO:


   
    FIND item-uni-estab WHERE item-uni-estab.it-codigo   = ITEM.it-codigo
                        AND   item-uni-estab.cod-estabel = p-cod-estabel EXCLUSIVE-LOCK NO-ERROR.








IF AVAIL item-uni-estab  THEN DO:
        
        ASSIGN ITEM.cod-obsoleto = 1.
               ITEM-uni-estab.cod-obsoleto = 1.


        FIND ext-item-cfa   WHERE ext-item-cfa.it-codigo   = ITEM.it-codigo
                            AND   ext-item-cfa.ep-codigo   = p-ep-codi
                            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL ext-item-cfa THEN DO:
            ASSIGN ext-item-cfa.classe = p-classe.
        END.


    END.
END.



/* {&OPEN-QUERY-{&BROWSE-NAME}} */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-fecha-planilha-le w-livre 
PROCEDURE pi-fecha-planilha-le :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-nomearq AS CHARACTER   NO-UNDO.


chExcelapp:APPLICATION:DisplayAlerts = FALSE.
c-nomearq = "c:\temp\ymof0107" + c-seg-usuario + ".xlsx".        
/* OS-COMMAND SILENT DEL VALUE(c-nomearq) NO-ERROR.   */               
chExcelapp:workbooks:item(1):SaveAs(c-nomearq,,,,,,,).           
chExcelapp:workbooks:APPLICATION:QUIT.                           
/* OS-COMMAND SILENT DEL value(c-nomearq) NO-ERROR.                  */



RELEASE OBJECT  chExcelapp  NO-ERROR. 
RELEASE OBJECT  chWorkbook  NO-ERROR. 
RELEASE OBJECT  chWorksheet NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-planilha w-livre 
PROCEDURE pi-gera-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE c-desc-item AS CHARACTER FORMAT "x(60)"   NO-UNDO.
DEFINE VARIABLE i-linha     AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-coluna1   AS CHARACTER INIT "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z"  NO-UNDO.
DEFINE VARIABLE c-coluna    AS CHAR  NO-UNDO.

DEFINE VARIABLE i-coluna    AS INTEGER     NO-UNDO.

DEFINE VARIABLE I-IDENT01   AS INTEGER     NO-UNDO.

DEFINE VARIABLE c-nomearq   AS CHARACTER   NO-UNDO.


EMPTY TEMP-TABLE tt-es-movto-ext-item-cfa.


FOR EACH tt-chancelador WHERE tt-chancelador.marcado   = "*" 
                        AND   tt-chancelador.ep-codigo = i-ep-codigo-usuario :


   /* anteriormente era pesquisado com TRIM causando lentid∆o - Maurilio 29-03-2016 */ 
   FIND FIRST es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.it-codigo   = tt-chancelador.it-codigo 
                                    AND   es-movto-ext-item-cfa.ep-codigo   = tt-chancelador.ep-codigo
                                    USE-INDEX pk-ident  NO-LOCK NO-ERROR.
   IF NOT AVAIL es-movto-ext-item-cfa THEN
       FIND FIRST es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.it-codigo   = chr(9) + tt-chancelador.it-codigo 
                                        AND   es-movto-ext-item-cfa.ep-codigo   = chr(9) + tt-chancelador.ep-codigo
                                        USE-INDEX pk-ident  NO-LOCK NO-ERROR.
   IF NOT AVAIL es-movto-ext-item-cfa THEN
       FIND FIRST es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.it-codigo   = " " + tt-chancelador.it-codigo 
                                        AND   es-movto-ext-item-cfa.ep-codigo   = " " + tt-chancelador.ep-codigo
                                        USE-INDEX pk-ident  NO-LOCK NO-ERROR.


    IF AVAIL es-movto-ext-item-cfa THEN DO:

        
        IF es-movto-ext-item-cfa.PERGUNTA-LITERAL = "RECLASSIFICAÄAO" THEN DO:
            ASSIGN I-IDENT01 = es-movto-ext-item-cfa.identificador - 1. 
        END.
        ELSE DO:
             ASSIGN I-IDENT01 = es-movto-ext-item-cfa.identificador.
        END.

        
        
         
        FOR EACH b-es-movto-ext-item-cfa WHERE b-es-movto-ext-item-cfa.it-codigo      = es-movto-ext-item-cfa.it-codigo
                                         AND   b-es-movto-ext-item-cfa.ep-codigo      = es-movto-ext-item-cfa.ep-codigo
                                         AND   b-es-movto-ext-item-cfa.classe         = es-movto-ext-item-cfa.classe
                                         AND   b-es-movto-ext-item-cfa.identificador  = I-IDENT01 NO-LOCK BY es-movto-ext-item-cfa.identificador  :
           CREATE tt-es-movto-ext-item-cfa.
           BUFFER-COPY b-es-movto-ext-item-cfa TO tt-es-movto-ext-item-cfa.
        END.
    END.
END.


RUN pi-abre-planilha-le1.


ASSIGN i-linha  = i-linha-ini.
       i-coluna = i-coluna-ini + 1.


FOR EACH tt-es-movto-ext-item-cfa BREAK  
    BY tt-es-movto-ext-item-cfa.it-codigo 
    BY tt-es-movto-ext-item-cfa.classe
    BY tt-es-movto-ext-item-cfa.identificador
    BY tt-es-movto-ext-item-cfa.nr-seq   :


    IF FIRST-OF(tt-es-movto-ext-item-cfa.it-codigo) THEN DO:
        chWorksheet:range("a" + STRING(i-linha)):VALUE = "N∆o".
        chWorksheet:range("b" + STRING(i-linha)):VALUE = tt-es-movto-ext-item-cfa.usuario.
        chWorksheet:range("c" + STRING(i-linha)):VALUE = tt-es-movto-ext-item-cfa.ep-codigo.
/*         chWorksheet:range("d" + STRING(i-linha)):VALUE = tt-es-movto-ext-item-cfa.cod-estabel. */
        chWorksheet:range("d" + STRING(i-linha)):VALUE = tt-es-movto-ext-item-cfa.it-codigo.
        
        FIND ITEM WHERE ITEM.it-codigo = trim(tt-es-movto-ext-item-cfa.it-codigo) NO-LOCK NO-ERROR.
        IF AVAIL ITEM  THEN 
           ASSIGN c-desc-item = ITEM.desc-item.        
        ELSE
           ASSIGN c-desc-item = "".

        chWorksheet:range("e" + STRING(i-linha)):VALUE = c-desc-item .
        chWorksheet:range("f" + STRING(i-linha)):VALUE = "9999" /*tt-es-movto-ext-item-cfa.classe*/ .
        chWorksheet:range("g" + STRING(i-linha)):VALUE = entry(1,tt-es-movto-ext-item-cfa.resposta-literal,"|").
         

        i-coluna = i-coluna-ini.
        i-coluna = i-coluna + 1.
    END.
    ELSE DO:
        chWorksheet:cells(i-linha,i-coluna):VALUE = entry(1,tt-es-movto-ext-item-cfa.resposta-literal,"|").
        ASSIGN i-coluna = i-coluna + 1.
    END.
    IF FIRST-OF(tt-es-movto-ext-item-cfa.it-codigo) THEN DO:

        IF LAST-OF(tt-es-movto-ext-item-cfa.it-codigo) THEN LEAVE.

       chWorksheet:cells(i-linha,i-coluna):VALUE = entry(1,tt-es-movto-ext-item-cfa.resposta-literal,"|").
    END.
     
    IF LAST-OF(tt-es-movto-ext-item-cfa.it-codigo) THEN     
        i-linha = i-linha + 1.
END.

ASSIGN i-nr-randon = RANDOM(1,30000000). 
 
chWorksheet = chExcelapp:Sheets:ITEM(4).
ASSIGN chWorksheet:range("HH1"):VALUE = i-nr-randon.
/*  chWorksheet:protect(i-nr-randon). */
chWorksheet = chExcelapp:Sheets:ITEM(1).
chWorksheet:protect(i-nr-randon).
chWorksheet = chExcelapp:Sheets:ITEM(2).  
chWorksheet:protect(i-nr-randon).
chWorksheet = chExcelapp:Sheets:ITEM(3).  
chWorksheet:protect(i-nr-randon).

 


                          
                

 











END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-le-param-planilha w-livre 
PROCEDURE pi-le-param-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-plan AS CHARACTER FORMAT "x(50)"   NO-UNDO.
                                          
                                          
FIND FIRST es-param-tax NO-LOCK  NO-ERROR.

c-plan = es-param-tax.arq-plan-tax.

CREATE "Excel.Application" chExcelapp.
chExcelapp:VISIBLE = TRUE.
chWorkbook         = chExcelapp:Workbooks:OPEN(c-plan) .
chWorksheet        = chExcelapp:Sheets:ITEM(4).

i-linha-ini    = chWorksheet:range("e3"):VALUE.
i-coluna-ini   = chWorksheet:range("e4"):VALUE.
i-qt-perguntas = chWorksheet:range("b4"):VALUE.


chExcelapp:workbooks:APPLICATION:QUIT.

RELEASE OBJECT  chExcelapp  NO-ERROR. 
RELEASE OBJECT  chWorkbook  NO-ERROR. 
RELEASE OBJECT  chWorksheet NO-ERROR.





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-le-plan-entrada w-livre 
PROCEDURE pi-le-plan-entrada :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE c-coluna1   AS CHARACTER INIT "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj"  NO-UNDO.
DEFINE VARIABLE c-coluna    AS CHAR  NO-UNDO.

DEFINE VARIABLE i-coluna    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-linha     AS INTEGER     NO-UNDO.

DEFINE VARIABLE c-item      AS CHARACTER FORMAT "x(20)"  NO-UNDO.
DEFINE VARIABLE c-estab     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-empresa2  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE i-conta     AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-seq       AS INTEGER     NO-UNDO.

DEFINE VARIABLE i-contaq     AS INTEGER     NO-UNDO.

DEFINE VARIABLE c-formato    AS CHARACTER  FORMAT "x(400)"  NO-UNDO.

DEFINE VARIABLE i-formato    AS INTEGER     NO-UNDO.

DEFINE VARIABLE i-identificador AS INTEGER     NO-UNDO.

DEFINE VARIABLE c-chancela      AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cfile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-conta2 AS INTEGER     NO-UNDO.

DEFINE VARIABLE C-ERRO AS CHARACTER  FORMAT "X(50)"  NO-UNDO.
DEFINE VARIABLE i-teste AS INTEGER     NO-UNDO.

DEFINE VARIABLE l-expan AS LOGICAL  INIT NO   NO-UNDO.

DEFINE VARIABLE c-senha AS CHARACTER   NO-UNDO.




cfile = "c:\temp\logymof0107-" + c-seg-usuario +  STRING(TODAY,"99999999") + ".txt".

OUTPUT TO  VALUE(cfile) NO-CONVERT APPEND.

IF SEARCH(c-arquivo-entrada) = ? THEN DO:

    run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Planilha de entrada n∆o encontrada!!!" ).
    RETURN.
    
END.

CREATE "Excel.Application" chExcelapp.
chExcelapp:VISIBLE = FALSE.
chWorkbook         = chExcelapp:Workbooks:ADD(c-arquivo-entrada) .
chWorksheet        = chExcelapp:Sheets:ITEM(4).
i-linha-ini        = chWorksheet:range("e3"):VALUE.
i-coluna-ini       = chWorksheet:range("e4"):VALUE.
i-qt-perguntas     = chWorksheet:range("b4"):VALUE.

chWorksheet        = chExcelapp:Sheets:ITEM(4).
c-senha = ENTRY(1,chWorksheet:range("HH1"):VALUE,",").


chWorksheet        = chExcelapp:Sheets:ITEM(1).

chWorksheet:unprotect(c-senha). 

ASSIGN i-linha  = i-linha-ini
       i-coluna = i-coluna-ini.
c-coluna = ENTRY(i-coluna,c-coluna1,",").

ASSIGN c-data = STRING(TODAY,"99/99/9999").


i-contaq = 1.


EMPTY TEMP-TABLE tt-erros.
EMPTY TEMP-TABLE tt-es-mov-ext-item-cfa.


DO i-contaq = 1 TO 200.


  
    c-chancela = chWorksheet:range("a" + STRING(i-linha)):VALUE.
    IF c-chancela <> "SIM" THEN DO:
        IF c-chancela = "" OR  c-chancela = ? THEN LEAVE.
        i-linha = i-linha + 1 .
        NEXT.
    END.

    i-seq = 0.

    i-qt-perguntas = i-coluna + i-qt-perguntas - 1.

    DO i-conta = i-coluna TO i-qt-perguntas:
         i-seq = i-seq + 1.
         c-hora   = STRING(TIME,"hh:mm").
/*          c-coluna = ENTRY(i-conta,c-coluna1,","). */

         c-formato = "".

        CREATE tt-es-mov-ext-item-cfa.
        ASSIGN tt-es-mov-ext-item-cfa.ep-codigo        = entry(1,chWorksheet:range("c" + STRING(i-linha)):VALUE,",").
               tt-es-mov-ext-item-cfa.classe           = entry(1,chWorksheet:range("f" + STRING(i-linha)):VALUE,",").
/*                tt-es-mov-ext-item-cfa.cod-estabel      = entry(1,chWorksheet:range("d" + STRING(i-linha)):VALUE,","). */
               c-formato = chWorksheet:cells(4,i-conta):VALUE.
               tt-es-mov-ext-item-cfa.pergunta-literal =  c-formato.
               tt-es-mov-ext-item-cfa.dt-movto         = date(c-data).
               tt-es-mov-ext-item-cfa.hr-movto         = c-hora.
               tt-es-mov-ext-item-cfa.identificador    = 78.
               tt-es-mov-ext-item-cfa.it-codigo        = entry(1,chWorksheet:range("d" + STRING(i-linha)):VALUE,",").
               tt-es-mov-ext-item-cfa.nr-seq           = i-seq.
               chWorksheet:cells(i-linha,i-conta):NumberFormat = "@".
               i-formato = INT(chWorksheet:cells(i-linha,i-conta):VALUE) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                   c-formato = chWorksheet:cells(i-linha,i-conta):VALUE.
               END.
               ELSE DO:
                    c-formato = STRING(i-formato).
               END.
               tt-es-mov-ext-item-cfa.resposta-literal = c-formato.
               tt-es-mov-ext-item-cfa.usuario          = c-seg-usuario.

    END.
    i-linha = i-linha + 1 . 
 
END.

FIND FIRST tt-1 NO-LOCK NO-ERROR.
IF NOT AVAIL tt-1 THEN DO:
   CREATE tt-erros.                                                                      
   tt-erros.erro =  "Descriá∆o das resposta n∆o carregado!!".      
   tt-erros.it-codigo = tt-es-mov-ext-item-cfa.it-codigo.                                
   RETURN.
END.



FOR EACH tt-es-mov-ext-item-cfa:
    
    FIND tt-1 WHERE tt-1.TAB = string(tt-es-mov-ext-item-cfa.nr-seq)
              AND   tt-1.cod = int(tt-es-mov-ext-item-cfa.resposta) NO-LOCK NO-ERROR.

    IF AVAIL tt-1 THEN DO:
          ASSIGN tt-es-mov-ext-item-cfa.resposta = tt-es-mov-ext-item-cfa.resposta + "|" + tt-1.DESCr.
    END.
END.



FOR EACH tt-es-mov-ext-item-cfa BREAK BY tt-es-mov-ext-item-cfa.it-codigo
                                      BY tt-es-mov-ext-item-cfa.ep-codigo:

    FIND es-cfa WHERE es-cfa.classe = tt-es-mov-ext-item-cfa.classe NO-LOCK NO-ERROR.
    IF NOT AVAIL es-cfa THEN DO:
        CREATE tt-erros.
        tt-erros.erro =  "CFA => "  + tt-es-mov-ext-item-cfa.classe + " N«O CADASTRADA".
        tt-erros.it-codigo = tt-es-mov-ext-item-cfa.it-codigo.
        NEXT.
    END.

    
/*     FIND estabelec WHERE estabelec.cod-estabel = trim(tt-es-mov-ext-item-cfa.cod-estabel) NO-LOCK NO-ERROR.        */
/*     IF NOT AVAIL estabelec THEN DO:                                                                                */
/*         CREATE tt-erros.                                                                                           */
/*         tt-erros.erro = "Estabelecimento =>   "  + STRING(tt-es-mov-ext-item-cfa.cod-estabel) + " N«O CADASTRADO". */
/*         tt-erros.it-codigo = tt-es-mov-ext-item-cfa.it-codigo.                                                     */
/*         NEXT.                                                                                                      */
/*     END.                                                                                                           */

    FIND ems2cadme.empresa WHERE ems2cadme.empresa.ep-codigo = tt-es-mov-ext-item-cfa.ep-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL ems2cadme.empresa THEN DO:
         CREATE tt-erros. 
         tt-erros.erro = "Empresa => "  + tt-es-mov-ext-item-cfa.ep-codigo + " N«O CADASTRADA".
         tt-erros.it-codigo = tt-es-mov-ext-item-cfa.it-codigo. 
         NEXT.
    END.




    FIND es-cfa-ge WHERE es-cfa-ge.classe    = tt-es-mov-ext-item-cfa.classe 
                   AND   es-cfa-ge.ep-codigo = i-ep-codigo-usuario NO-LOCK  NO-ERROR.
    IF NOT AVAIL es-cfa-ge THEN DO:
          CREATE tt-erros.                                                                        
          tt-erros.erro = "Grupo de Estoque n∆o relacionado para a CFA =>"  + tt-es-mov-ext-item-cfa.classe.  
          tt-erros.it-codigo = tt-es-mov-ext-item-cfa.it-codigo.                                  
          NEXT.
    END.
    

    RUN pi-verifica-exp-recla (INPUT tt-es-mov-ext-item-cfa.it-codigo,
                               INPUT tt-es-mov-ext-item-cfa.ep-codigo,
                              OUTPUT l-expan) .
   
    IF SUBSTR(tt-es-mov-ext-item-cfa.it-codigo,1,2) <> "XX"  AND l-expan = NO THEN DO:
        FIND ITEM WHERE ITEM.IT-CODIGO = tt-es-mov-ext-item-cfa.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN DO:
           
           FIND es-cfa-ge WHERE es-cfa-ge.classe    = tt-es-mov-ext-item-cfa.classe 
                          AND   es-cfa-ge.ep-codigo = i-ep-codigo-usuario NO-LOCK  NO-ERROR.
           IF AVAIL es-cfa-ge THEN DO:
               IF ITEM.GE-CODIGO <> es-cfa-ge.ge-codigo THEN DO:
                    CREATE tt-erros.                                                                                  
                    tt-erros.erro = "Grupo de Estoq da CFA " + "(" + tt-es-mov-ext-item-cfa.classe + ")" + " " + "Ç difrente do grp de estoq do item" +
                         "(" + string(item.it-codigo) + ")".
                    tt-erros.it-codigo = tt-es-mov-ext-item-cfa.it-codigo.                                            
                    NEXT.                                                                                             
               END.
           END.
        END.
    END.







    IF FIRST-OF(tt-es-mov-ext-item-cfa.it-codigo) THEN DO:
                                                                                                                                              
         FOR EACH es-movto-ext-item-cfa  WHERE es-movto-ext-item-cfa.it-codigo = tt-es-mov-ext-item-cfa.it-codigo   BY  es-movto-ext-item-cfa.identificador DESC  BY es-movto-ext-item-cfa.nr-seq :                          
                                                                                                                                                                                                           
                  ASSIGN i-conta2 = i-conta2 + 1.                                                                                                                                                          
                  IF i-conta2 = 1 THEN DO:                                                                       
                        ASSIGN i-identificador = es-movto-ext-item-cfa.identificador.                                                                                                                      
                       LEAVE.                                                                                                                                                                              
                  END.                                                                                                                                                                                     
         END.                                                                                                                                                                                          

    END.
    
     
    CREATE es-movto-ext-item-cfa.
    BUFFER-COPY tt-es-mov-ext-item-cfa EXCEPT tt-es-mov-ext-item-cfa.identificador TO es-movto-ext-item-cfa.
    ASSIGN es-movto-ext-item-cfa.identificador = i-identificador + 1.

   
END.

FIND FIRST tt-erros NO-LOCK NO-ERROR.
IF AVAIL tt-erros THEN DO:
  
    FOR EACH tt-erros BREAK BY TT-ERROS.ERRO:
        IF FIRST-OF(TT-ERROS.ERRO) THEN  PUT UNFORMAT tt-erros.ERRO SKIP.
    END.

    MESSAGE "Existem erros!!! " "verifique o arquivo" SKIP
         cfile
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.


FOR EACH tt-es-mov-ext-item-cfa BREAK BY tt-es-mov-ext-item-cfa.it-codigo:

    IF FIRST-OF(tt-es-mov-ext-item-cfa.it-codigo) THEN DO:

        FIND first tt-erros WHERE tt-erros.it-codigo = tt-es-mov-ext-item-cfa.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL tt-erros THEN NEXT.

        run pi-acompanhar in h-acomp (input "Planilha Entrada - Item: " + tt-es-mov-ext-item-cfa.it-codigo).
        RUN pi-verifica-item (INPUT tt-es-mov-ext-item-cfa.it-codigo,
                              INPUT tt-es-mov-ext-item-cfa.ep-codigo,
                              INPUT tt-es-mov-ext-item-cfa.classe   ).
    END.        
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-obsoleta-item w-livre 
PROCEDURE pi-obsoleta-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE i-ident1         AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-conta2         AS INTEGER INIT 0     NO-UNDO.
DEFINE VARIABLE i-identificador1 AS INTEGER     NO-UNDO.



ASSIGN c-hora = STRING(TIME,"hh:mm").
       c-data = STRING(TODAY,"99/99/9999").



 /*  c-item-reclass                */
 /* = c-empresa                    */
 /* = c-estab-reclass              */


FIND ext-item-cfa WHERE ext-item-cfa.it-codigo   = c-item-reclass
                  AND   ext-item-cfa.ep-codigo   = c-empresa NO-LOCK NO-ERROR.

IF AVAIL ext-item-cfa THEN DO:

    IF ext-item-cfa.classe = "9999" THEN RETURN.




END.









FOR EACH es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.ep-codigo   = c-empresa
                               AND   es-movto-ext-item-cfa.it-codigo   = c-item-reclass
                               BY es-movto-ext-item-cfa.identificador DESC:


   ASSIGN i-conta2 = i-conta2 + 1.
   IF i-conta2 = 1  THEN  DO:
         CREATE b-ES-MOVTO-EXT-ITEM-CFA.                                                              
          ASSIGN b-es-movto-ext-item-cfa.classe            = es-movto-ext-item-cfa.classe.             
                 b-es-movto-ext-item-cfa.dt-movto          = TODAY.                                    
                 b-es-movto-ext-item-cfa.ep-codigo         = es-movto-ext-item-cfa.ep-codigo.          
                 b-es-movto-ext-item-cfa.hr-movto          = c-hora.                                   
                 b-es-movto-ext-item-cfa.identificador     = es-movto-ext-item-cfa.identificador + 1.                         
                 b-es-movto-ext-item-cfa.it-codigo         = es-movto-ext-item-cfa.it-codigo.          
                 b-es-movto-ext-item-cfa.pergunta-literal  = "RECLASSIFICAÄAO".                        
                 b-es-movto-ext-item-cfa.resposta-literal  = "RECLASSIFICAÄAO".                        
                 b-es-movto-ext-item-cfa.usuario           =  C-SEG-USUARIO.

          LEAVE.

   END.







END.








   
         
         
         
         






             
             
             
             
             
             
             
             
             
             
             


      
FIND ext-item-cfa WHERE ext-item-cfa.it-codigo   = c-item-reclass
                  AND   ext-item-cfa.ep-codigo   = c-empresa EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL ext-item-cfa  THEN DO:
   
    ASSIGN ext-item-cfa.classe = "9999".

    IF i-ep-codigo-usuario = c-empresa THEN DO:

       
        FIND ITEM WHERE ITEM.it-codigo = c-item-reclass EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL ITEM THEN DO:
            ITEM.cod-obsoleto = 4.
        END.

        FIND ITEM-uni-estab WHERE ITEM-uni-estab.it-codigo   = c-item-reclass
                            AND   ITEM-uni-estab.cod-estabel = c-estab-reclass exclusive-lock NO-ERROR.

        IF AVAIL ITEM-uni-estab THEN DO:
            ASSIGN ITEM-uni-estab.cod-obsoleto = 4.
        END.

 
    END.


END.
     
     
     
     
     
 {&OPEN-QUERY-{&BROWSE-NAME}}    
     
     
     
     





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-replica-item w-livre 
PROCEDURE pi-replica-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-item  AS CHARACTER   NO-UNDO.


FIND ITEM WHERE ITEM.it-codigo = p-item NO-LOCK NO-ERROR.
IF AVAIL ITEM THEN DO:
    CREATE tt-item1.
    BUFFER-COPY ITEM TO tt-item1.
END.


FIND FIRST  es-it-depto WHERE es-it-depto.it-codigo = p-item   NO-LOCK NO-ERROR.


FOR EACH ems2cadme.empresa FIELDS(ep-codigo data-inicio data-fim)NO-LOCK
/*    WHERE empresa.data-inicio <= TODAY */
/*      AND empresa.data-fim    >= TODAY */
    ,
    EACH bco_empres WHERE bco_empres.cod_bco_logic = "ems2cademp"
                    AND   bco_empres.cod_empresa   = empresa.ep-codigo NO-LOCK:

    FIND FIRST trad_org_ext WHERE trad_org_ext.cod_tip_unid_organ      = "998"                  
                            AND   trad_org_ext.cod_matriz_trad_org_ext = "ems2"            
                            AND   trad_org_ext.cod_unid_organ_ext      = bco_empres.cod_empresa NO-LOCK NO-ERROR. 

    FIND FIRST es-emp-desativada 
         WHERE es-emp-desativada.ep-codigo = bco_empres.cod_empresa NO-ERROR.
   IF AVAIL(es-emp-desativada) THEN NEXT.

    IF AVAIL(trad_org_ext) THEN DO:
        FIND FIRST ems5.unid_organ NO-LOCK
             WHERE ems5.unid_organ.cod_unid_organ = trad_org_ext.cod_unid_organ_ext NO-ERROR.
        IF AVAIL(ems5.unid_organ) THEN DO:
    
            IF ems5.unid_organ.dat_fim_valid < TODAY THEN NEXT.
    
        END.
    END.


    IF bco_empres.cod_empresa = i-ep-codigo-usuario  THEN NEXT.

    FIND FIRST es-fila-rep-item WHERE es-fila-rep-item.ep-codigo  = bco_empres.cod_empresa
                                AND   es-fila-rep-item.it-codigo  = ITEM.it-codigo NO-ERROR.

    IF NOT AVAIL es-fila-rep-item  THEN DO:

        CREATE  es-fila-rep-item.
            es-fila-rep-item.un               = tt-item1.un            .
            es-fila-rep-item.tipo-contr       = tt-item1.tipo-contr    .
            es-fila-rep-item.quant-segur      = tt-item1.quant-segur   .
            es-fila-rep-item.narrativa        = tt-item1.narrativa     .
            es-fila-rep-item.it-codigo        = tt-item1.it-codigo     .
            es-fila-rep-item.char-2           = tt-item1.char-2        .
            es-fila-rep-item.class-fiscal     = tt-item1.class-fiscal  .
            es-fila-rep-item.cod-depto        = IF AVAIL es-it-depto THEN es-it-depto.cod-depto ELSE 0 . 
            es-fila-rep-item.cod-servico      = tt-item1.cod-servico   .   
            es-fila-rep-item.codigo-orig      = tt-item1.codigo-orig   .   
            es-fila-rep-item.codigo-refer     = tt-item1.codigo-refer  .   
            es-fila-rep-item.data-implant     = tt-item1.data-implant  .   
            es-fila-rep-item.dec-1            = tt-item1.dec-1         .   
            es-fila-rep-item.desc-item        = tt-item1.desc-item     .   
            es-fila-rep-item.dt-criacao       = TODAY    .   
            es-fila-rep-item.ep-codigo        = bco_empres.cod_empresa  .
            es-fila-rep-item.fm-cod-com       = tt-item1.fm-cod-com       .
            es-fila-rep-item.fm-codigo        = tt-item1.fm-codigo        .
            es-fila-rep-item.ge-codigo        = 99 /*tt-item1.ge-codigo*/        .

    END.
    ELSE DO:

        ASSIGN es-fila-rep-item.un               = tt-item1.un            .
               es-fila-rep-item.tipo-contr       = tt-item1.tipo-contr    .
               es-fila-rep-item.quant-segur      = tt-item1.quant-segur   .
               es-fila-rep-item.narrativa        = tt-item1.narrativa     .
               es-fila-rep-item.char-2           = tt-item1.char-2        .
               es-fila-rep-item.class-fiscal     = tt-item1.class-fiscal  .
               es-fila-rep-item.cod-depto        = IF AVAIL es-it-depto THEN es-it-depto.cod-depto ELSE 0 . 
               es-fila-rep-item.cod-servico      = tt-item1.cod-servico   .   
               es-fila-rep-item.codigo-orig      = tt-item1.codigo-orig   .   
               es-fila-rep-item.codigo-refer     = tt-item1.codigo-refer  .   
               es-fila-rep-item.data-implant     = tt-item1.data-implant  .   
               es-fila-rep-item.dec-1            = tt-item1.dec-1         .   
               es-fila-rep-item.ep-codigo        = bco_empres.cod_empresa  .
               es-fila-rep-item.fm-cod-com       = tt-item1.fm-cod-com       .
               es-fila-rep-item.fm-codigo        = tt-item1.fm-codigo        .
               es-fila-rep-item.ge-codigo        = 99 /*tt-item1.ge-codigo*/        .
    END.

   /* Begins Jan/2018 - Willians Ambrosio DKP */
   FIND FIRST es-klassmatt-integr WHERE
              es-klassmatt-integr.idklassmatt = 999999999                                   AND
              es-klassmatt-integr.dt-trans    = TODAY                                       AND
              es-klassmatt-integr.hr-trans    = REPLACE(STRING(TIME,"HH:MM:SS"),":","")     EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL es-klassmatt-integr THEN
   DO:
      CREATE es-klassmatt-integr.
      BUFFER-COPY es-fila-rep-item     TO es-klassmatt-integr
                                   ASSIGN es-klassmatt-integr.idklassmatt = 999999999                               
                                          es-klassmatt-integr.dt-trans    = TODAY                                   
                                          es-klassmatt-integr.hr-trans    = REPLACE(STRING(TIME,"HH:MM:SS"),":","") 
                                          es-klassmatt-integr.log-retorno   = "YMOF0107.W - TIPO DE CONTROLE ATUALIZADO Tipo-Contr = "  + STRING(es-fila-rep-item.tipo-contr)      
                                          es-klassmatt-integr.statusRetorno = "N".                                              
   END.
   /* End Jan/2018 - Willians Ambrosio DKP */


    DISP bco_empres.cod_empresa.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-retorno-integra w-livre 
PROCEDURE pi-retorno-integra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN esp\retornows.p.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-verifica-exp-recla w-livre 
PROCEDURE pi-verifica-exp-recla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-item     AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER p-empresa  AS CHARACTER   NO-UNDO.
/* DEFINE INPUT PARAMETER p-estab    AS CHARACTER   NO-UNDO.  */
DEFINE OUTPUT PARAMETER p-l-exp   AS LOGICAL INIT TRUE    NO-UNDO.

DEFINE VARIABLE i-ident2          AS INTEGER     NO-UNDO.


/* anteriormente era pesquisado com TRIM causando lentid∆o - Maurilio 29-03-2016 */ 
FIND FIRST es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.it-codigo   = p-item                                                
                                 AND   es-movto-ext-item-cfa.ep-codigo   = p-empresa                                          
                                 USE-INDEX pk-ident  NO-LOCK NO-ERROR. 
IF NOT AVAIL es-movto-ext-item-cfa THEN
    FIND FIRST es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.it-codigo   = chr(9) + p-item                                                
                                     AND   es-movto-ext-item-cfa.ep-codigo   = chr(9) + p-empresa                                          
                                     USE-INDEX pk-ident  NO-LOCK NO-ERROR.                                                                         
IF NOT AVAIL es-movto-ext-item-cfa THEN
    FIND FIRST es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.it-codigo   = " " + p-item                                                
                                     AND   es-movto-ext-item-cfa.ep-codigo   = " " + p-empresa                                          
                                     USE-INDEX pk-ident  NO-LOCK NO-ERROR.                                                                         

IF AVAIL es-movto-ext-item-cfa THEN DO:                                                                                                        
                                                                                                                                               
    ASSIGN i-ident2 = es-movto-ext-item-cfa.identificador.                                                                                      
                                                                                                                                               
    FOR EACH  es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.it-codigo     = ITEM.it-codigo                                                 
                                    AND   es-movto-ext-item-cfa.ep-codigo     = i-ep-codigo-usuario                                            
                                    AND   es-movto-ext-item-cfa.identificador = i-ident2  BY es-movto-ext-item-cfa.nr-seq:            
                                                                                                                                               
                                                                                                                                               
                                                                                                                                               
        IF es-movto-ext-item-cfa.pergunta-literal = "RECLASSIFICAÄAO" THEN DO:                                     
                                                                                                                                               
            ASSIGN p-l-exp = NO.                                                                                                           
                                                                                                                                               
        END.                                                                                                                                   
        ELSE DO:                                                                                                                               
            ASSIGN p-l-exp = TRUE.                                                                                                         
        END.                                                                                                                                   
    END.                                                                                                                                       
END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-verifica-item w-livre 
PROCEDURE pi-verifica-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER  p-item    AS CHARACTER FORMAT "x(16)"   NO-UNDO.
DEFINE INPUT PARAMETER  p-empresa AS CHARACTER                  NO-UNDO.
DEFINE INPUT PARAMETER  p-classe  AS CHARACTER                  NO-UNDO.
/* DEFINE INPUT PARAMETER  p-estabel AS CHARACTER                  NO-UNDO. */

DEFINE VARIABLE l-conectado       AS LOGICAL  INIT NO           NO-UNDO.
DEFINE VARIABLE l-envia-ret       AS LOGICAL  INIT NO           NO-UNDO.
DEFINE VARIABLE i-ident           AS INTEGER     NO-UNDO.

DEFINE BUFFER buf-item FOR ITEM.
DEFINE BUFFER b-item-refer FOR ITEM.

CREATE tt-versao-integr. 
ASSIGN tt-versao-integr.cod-versao-integracao = 001     
       tt-versao-integr.ind-origem-msg        = 01.  
ASSIGN i-cod-depto = 0.

EMPTY TEMP-TABLE tt-item-codigo.
EMPTY TEMP-TABLE tt-erros-geral.
EMPTY TEMP-TABLE tt-item.
EMPTY TEMP-TABLE tt-erros-geral.    
EMPTY TEMP-TABLE tt-altera.
EMPTY TEMP-TABLE tt-elimina.
EMPTY TEMP-TABLE tt-item-de.

FIND ITEM WHERE ITEM.it-codigo = p-item NO-LOCK NO-ERROR.
CREATE tt-item-de.
BUFFER-COPY item TO tt-item-de.

FIND FIRST tt-item-de NO-LOCK NO-ERROR.

IF i-ep-codigo-usuario <> p-empresa THEN DO:

END.
ELSE 
DO:
   FIND ITEM WHERE ITEM.it-codigo = p-item NO-LOCK NO-ERROR.
   IF NOT AVAIL ITEM THEN DO:
      CREATE tt-erros.                                                                        
      tt-erros.erro      =  "Item  " + p-item + " n∆o cadastrado na empresa =" + p-empresa .  
      tt-erros.it-codigo = p-item .                                                           
      NEXT.
   END.
   ELSE 
   DO:
      IF SUBSTR(ITEM.it-codigo,1,2) = "XX" THEN 
      DO:               
         FIND FIRST tt-item-de NO-LOCK NO-ERROR.
         IF tt-item-de.cod-obsoleto = 4 THEN 
         DO:

            
            FIND FIRST b-item-para NO-LOCK
                 WHERE b-item-para.it-codigo = tt-item-de.it-codigo NO-ERROR. /*SUBSTR(p-classe,1,2) + SUBSTR(tt-item-de.it-codigo,3,6) NO-ERROR.*/
            IF AVAIL(b-item-para) THEN 
            DO:

                                       
                CREATE tt-elimina.
                ASSIGN tt-elimina.it-codigo  = tt-item-de.it-codigo
                       tt-elimina.un         = tt-item-de.un
                       tt-elimina.descricao  = tt-item-de.Desc-item.
                RUN pi-cd0205rp.
            END.
         END.

         FIND es-cfa-ge WHERE es-cfa-ge.classe = tt-es-mov-ext-item-cfa.classe NO-LOCK  NO-ERROR.
         FIND FIRST es-de-para-it-padr WHERE es-de-para-it-padr.it-codigo-temp = tt-item-de.it-codigo NO-LOCK NO-ERROR.
         
         CREATE tt-altera.
         ASSIGN tt-altera.it-codigo     = tt-item-de.it-codigo
                tt-altera.descricao     = tt-item-de.desc-item
                tt-altera.un            = tt-item-de.un
                tt-altera.new-it-codigo = IF avail(es-de-para-it-padr) THEN es-de-para-it-padr.it-codigo-padr ELSE SUBSTR(p-classe,1,2) + SUBSTR(tt-item-de.it-codigo,3,6)
                tt-altera.new-un        = tt-item-de.un
                tt-altera.fator-conv    = tt-item-de.fator-conv
                tt-altera.ge-codigo     = IF AVAIL es-cfa-ge THEN es-cfa-ge.ge-codigo ELSE 0.
                tt-altera.old-ge-codigo = tt-item-de.ge-codigo.

         /* Verificar se item para existe e esta ativo no ERP */
         FOR FIRST b-item-para
             WHERE b-item-para.it-codigo = tt-item-de.it-codigo NO-LOCK: END.
         IF AVAIL b-item-para THEN 
         DO:

             RUN pi-altera-item.
             

             FOR FIRST buf-item
                 WHERE buf-item.it-codigo = tt-item-de.it-codigo NO-LOCK: END.
             IF AVAIL buf-item THEN DO: 
               CREATE tt-erros.                                                                              
               ASSIGN tt-erros.erro      = "N∆o foi poss°vel mudar o c¢digo (Erro Interno)." + "-" + p-item      
                        tt-erros.it-codigo = tt-altera.it-codigo .
                        /*UNDO altera, RETURN "NOK".*/
               NEXT.
             END.

             FIND FIRST tt-erros-geral NO-LOCK NO-ERROR.
             IF AVAIL tt-erros-geral THEN NEXT.


             FIND FIRST es-de-para-it-padr WHERE es-de-para-it-padr.it-codigo-temp = tt-altera.it-codigo NO-LOCK NO-ERROR.
                IF NOT AVAIL es-de-para-it-padr THEN DO:
                   CREATE es-de-para-it-padr.                                              
                   ASSIGN es-de-para-it-padr.it-codigo-temp = tt-altera.it-codigo     
                          es-de-para-it-padr.it-codigo-padr = tt-altera.new-it-codigo.
                END.
         
               
         
                FIND ext-item-cfa   WHERE ext-item-cfa.it-codigo   = tt-altera.it-codigo                    
                                    AND   ext-item-cfa.ep-codigo   = i-ep-codigo-usuario                               
                                    EXCLUSIVE-LOCK NO-ERROR. 
                IF AVAIL ext-item-cfa THEN DO:                                                               
                    ASSIGN ext-item-cfa.classe    = p-classe. 
                           ext-item-cfa.it-codigo = tt-altera.new-it-codigo.
                END.
         
         
                FOR EACH es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.it-codigo = tt-altera.it-codigo:
                   ASSIGN es-movto-ext-item-cfa.it-codigo = tt-altera.new-it-codigo.
                END.
         
                FIND FIRST  es-it-depto WHERE es-it-depto.it-codigo =  tt-altera.it-codigo EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL es-it-depto THEN DO:
                    ASSIGN es-it-depto.it-codigo = tt-altera.new-it-codigo. 
                END.
                
                RUN pi-altera-tabelas-internas.
         
                FIND es-cfa-ge WHERE es-cfa-ge.classe    = tt-es-mov-ext-item-cfa.classe            
                               AND   es-cfa-ge.ep-codigo = i-ep-codigo-usuario NO-LOCK  NO-ERROR.   
         
                FIND ITEM WHERE ITEM.it-codigo = tt-altera.new-it-codigo EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL ITEM THEN DO:
                    ASSIGN ITEM.cod-obsoleto = 1
                           ITEM.GE-CODIGO = IF AVAIL es-cfa-ge THEN es-cfa-ge.ge-codigo ELSE 99. 
                           
                END.
         
                FOR EACH item-uni-estab WHERE item-uni-estab.it-codigo = tt-altera.new-it-codigo:
                     ASSIGN item-uni-estab.cod-obsoleto = 1.
                END.
         
                
                RUN pi-atualiza-tipo-item(INPUT p-empresa,
                                          INPUT tt-altera.new-it-codigo ).
         
         
                 FIND ext-item-cfa   WHERE ext-item-cfa.it-codigo   = tt-altera.new-it-codigo                 
                                     AND   ext-item-cfa.ep-codigo   = i-ep-codigo-usuario                       
                                     NO-LOCK NO-ERROR.  
         
                CREATE es-integra-retorno.
                       es-integra-retorno.dt-carga      = TODAY.
                       es-integra-retorno.ep-codigo     = IF AVAIL ext-item-cfa THEN ext-item-cfa.EP-CODIGO ELSE "0". 
                       es-integra-retorno.idsin         = IF AVAIL ext-item-cfa THEN ext-item-cfa.idsin ELSE 0. 
                       es-integra-retorno.dt-int-erp    = TODAY.
                       es-integra-retorno.IdKlassmatt   = IF AVAIL ext-item-cfa THEN ext-item-cfa.IdKlassmatt ELSE 0.
                       es-integra-retorno.codigo        = IF AVAIL ITEM THEN ITEM.it-codigo ELSE tt-altera.new-it-codigo. 
                       es-integra-retorno.StatusRetorno = "S".
                       es-integra-retorno.log-retorno   = "Troca de codigo do item  de " +  tt-altera.it-codigo + " para " + tt-altera.new-it-codigo.
         
                RUN pi-grava-log-monitor.
                RUN pi-replica-item (INPUT ITEM.it-codigo).    
         END.
         ELSE DO:

             /* Atualizar dados enviados */
             CREATE tt-item.            
             BUFFER-COPY tt-item-de TO tt-item
             ASSIGN tt-item.ind-tipo-movto = 1. /*1 inclusao / 2 alteracao*/ 
             ASSIGN tt-item.it-codigo    = tt-altera.new-it-codigo
                    tt-item.desc-item    = tt-item-de.desc-item
                    tt-item.narrativa    = tt-item-de.narrativa
                    tt-item.fm-codigo    = tt-item-de.fm-codigo
                    tt-item.fm-cod-com   = tt-item-de.fm-cod-com
                    tt-item.codigo-refer = tt-item-de.codigo-refer
                    tt-item.class-fiscal = tt-item-de.class-fiscal
                    tt-item.dec-1        = DEC(tt-item-de.dec-1).
         


             
             RUN cdp/cdapi344.p ( INPUT        TABLE tt-versao-integr,          
                                  OUTPUT       TABLE tt-erros-geral,           
                                  INPUT-OUTPUT TABLE tt-item).


             FIND FIRST  tt-erros-geral NO-LOCK  NO-ERROR.
             IF AVAIL tt-erros-geral THEN DO:
                 FOR EACH tt-erros-geral:
                     CREATE tt-erros.                                                                              
                     ASSIGN tt-erros.erro      = STRING(tt-erros-geral.cod-erro) + "-" + tt-erros-geral.des-erro
                            tt-erros.it-codigo = tt-altera.new-it-codigo .

                     
                 END.
                 /*UNDO altera, RETURN "NOK".*/
                 NEXT.
             END.
             ELSE DO:
                 
                 /*Classificaá∆o*/
                 FIND FIRST b-item-class EXCLUSIVE-LOCK
                     WHERE b-item-class.it-codigo = tt-altera.new-it-codigo NO-ERROR.
                     
                 IF AVAIL(b-item-class) THEN DO:
             
                     
                     ASSIGN b-item-class.class-fiscal = tt-item-de.class-fiscal
                            b-item-class.aliquota-ipi = tt-item-de.aliquota-ipi. 
                     
                     FIND CURRENT b-item-class NO-LOCK NO-ERROR.                  
                 END.                
             END.                

             FIND FIRST es-de-para-it-padr WHERE es-de-para-it-padr.it-codigo-temp = tt-altera.it-codigo NO-LOCK NO-ERROR.
             IF NOT AVAIL es-de-para-it-padr THEN DO:
                CREATE es-de-para-it-padr.                                              
                ASSIGN es-de-para-it-padr.it-codigo-temp = tt-altera.it-codigo     
                       es-de-para-it-padr.it-codigo-padr = tt-altera.new-it-codigo.
             END.
             
             
              
             FIND ext-item-cfa   WHERE ext-item-cfa.it-codigo   = tt-altera.it-codigo                    
                                 AND   ext-item-cfa.ep-codigo   = i-ep-codigo-usuario                               
                                 EXCLUSIVE-LOCK NO-ERROR. 
             IF AVAIL ext-item-cfa THEN DO:                                                               
                 ASSIGN ext-item-cfa.classe    = p-classe. 
                        ext-item-cfa.it-codigo = tt-altera.new-it-codigo.
             END.
              
       .     
             FOR EACH es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.it-codigo = tt-altera.it-codigo:
                ASSIGN es-movto-ext-item-cfa.it-codigo = tt-altera.new-it-codigo.
             END.
             
             FIND FIRST  es-it-depto WHERE es-it-depto.it-codigo =  tt-altera.it-codigo EXCLUSIVE-LOCK NO-ERROR.
             IF AVAIL es-it-depto THEN DO:
                 ASSIGN es-it-depto.it-codigo = tt-altera.new-it-codigo. 
             END.
             
             
             RUN pi-altera-tabelas-internas.
             
             FIND es-cfa-ge WHERE es-cfa-ge.classe    = tt-es-mov-ext-item-cfa.classe            
                            AND   es-cfa-ge.ep-codigo = i-ep-codigo-usuario NO-LOCK  NO-ERROR.   
             
             FIND ITEM WHERE ITEM.it-codigo = tt-altera.new-it-codigo EXCLUSIVE-LOCK NO-ERROR. /* Ponto Ö verificar */
             IF AVAIL ITEM THEN DO:
                 ASSIGN ITEM.cod-obsoleto = 1
                        ITEM.GE-CODIGO    = IF AVAIL es-cfa-ge THEN es-cfa-ge.ge-codigo ELSE 99
                        ITEM.codigo-refer = tt-item-de.codigo-refer.
                        
             END.
             
             FOR EACH item-uni-estab WHERE item-uni-estab.it-codigo = tt-altera.new-it-codigo:
                  ASSIGN item-uni-estab.cod-obsoleto = 1.
             END.
             
             RUN pi-atualiza-tipo-item(INPUT p-empresa,
                                       INPUT tt-altera.new-it-codigo ).
             
             
             FIND ext-item-cfa   WHERE ext-item-cfa.it-codigo   = tt-altera.new-it-codigo                 
                                 AND   ext-item-cfa.ep-codigo   = i-ep-codigo-usuario                       
                                 NO-LOCK NO-ERROR.  
       
             CREATE es-integra-retorno.
                    es-integra-retorno.dt-carga      = TODAY.
                    es-integra-retorno.ep-codigo     = IF AVAIL ext-item-cfa THEN ext-item-cfa.EP-CODIGO ELSE "0". 
                    es-integra-retorno.idsin         = IF AVAIL ext-item-cfa THEN ext-item-cfa.idsin ELSE 0. 
                    es-integra-retorno.dt-int-erp    = TODAY.
                    es-integra-retorno.IdKlassmatt   = IF AVAIL ext-item-cfa THEN ext-item-cfa.IdKlassmatt ELSE 0.
                    es-integra-retorno.codigo        = IF AVAIL ITEM THEN ITEM.it-codigo ELSE tt-altera.new-it-codigo. 
                    es-integra-retorno.StatusRetorno = "S".
                    es-integra-retorno.log-retorno   = "Troca de codigo do item  de " +  tt-altera.it-codigo + " para " + tt-altera.new-it-codigo.
       
             RUN pi-grava-log-monitor.
             RUN pi-replica-item (INPUT ITEM.it-codigo).
            
         END.

      END.
      ELSE 
      DO:
         /* Este ponto precisa ser tratado para filtrar itens oriundos do JDE via klamatt */
         FIND FIRST es-integra-retorno WHERE 
                    es-integra-retorno.codigo                        = p-item         AND 
                    es-integra-retorno.ep-codigo                     = p-empresa      EXCLUSIVE-LOCK NO-ERROR.

         FIND es-cfa-ge WHERE es-cfa-ge.classe    = tt-es-mov-ext-item-cfa.classe 
                        AND   es-cfa-ge.ep-codigo = i-ep-codigo-usuario NO-LOCK  NO-ERROR.

         FIND ITEM WHERE ITEM.it-codigo = p-item EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
             ASSIGN ITEM.cod-obsoleto = 1.
                    ITEM.GE-CODIGO = IF AVAIL es-cfa-ge THEN es-cfa-ge.ge-codigo ELSE 99.          

         FOR EACH item-uni-estab WHERE item-uni-estab.it-codigo = p-item:
             ASSIGN item-uni-estab.cod-obsoleto = 1.
         END.

         RUN pi-atualiza-tipo-item(INPUT p-empresa,
                                   INPUT p-item ).    

         FIND ext-item-cfa   WHERE ext-item-cfa.it-codigo   = ITEM.it-codigo               
                             AND   ext-item-cfa.ep-codigo   = i-ep-codigo-usuario                    
                             EXCLUSIVE-LOCK NO-ERROR.    
         IF AVAIL ext-item-cfa THEN DO:                                                              
              ASSIGN ext-item-cfa.classe    = p-classe.
         END.
         
         /* anteriormente era pesquisado com TRIM causando lentid∆o - Maurilio 29-03-2016 */ 
         FIND FIRST es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.it-codigo   = ITEM.it-codigo 
                                          AND   es-movto-ext-item-cfa.ep-codigo   = p-empresa
                                          USE-INDEX pk-ident  NO-LOCK NO-ERROR.
         IF NOT AVAIL es-movto-ext-item-cfa THEN
             FIND FIRST es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.it-codigo   = chr(9) + ITEM.it-codigo 
                                              AND   es-movto-ext-item-cfa.ep-codigo   = chr(9) + p-empresa
                                              USE-INDEX pk-ident  NO-LOCK NO-ERROR.
         IF NOT AVAIL es-movto-ext-item-cfa THEN
             FIND FIRST es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.it-codigo   = " " + ITEM.it-codigo 
                                              AND   es-movto-ext-item-cfa.ep-codigo   = " " + p-empresa
                                              USE-INDEX pk-ident  NO-LOCK NO-ERROR.

         IF AVAIL es-movto-ext-item-cfa THEN DO:

             ASSIGN i-ident = es-movto-ext-item-cfa.identificador.

             FOR EACH  es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.it-codigo     = ITEM.it-codigo 
                                             AND   es-movto-ext-item-cfa.ep-codigo     = i-ep-codigo-usuario 
                                             AND   es-movto-ext-item-cfa.identificador = i-ident  BY es-movto-ext-item-cfa.nr-seq:

                 IF es-movto-ext-item-cfa.pergunta-literal = "RECLASSIFICAÄAO" THEN DO:
                     ASSIGN l-envia-ret = NO.
                     LEAVE.
                 END.
                 ELSE DO:
                     ASSIGN l-envia-ret = TRUE.
                     LEAVE.
                 END.
             END.
         END.

         IF l-envia-ret THEN 
         DO:             
            ASSIGN l-oriundo-jde = NO.

            IF INDEX(ITEM.narrativa,"ORIGEM JDE") <> 0 THEN
               ASSIGN l-oriundo-jde = YES
                      ITEM.narrativa  = REPLACE(ITEM.narrativa,"ORIGEM JDE","").

            IF AVAIL es-integra-retorno AND
               INDEX(es-integra-retorno.log-retorno,"ORIGEM JDE") <> 0 THEN               
            DO:             
               RELEASE es-integra-retorno.
               ASSIGN l-oriundo-jde = YES.
            END.

            CREATE es-integra-retorno.
                   es-integra-retorno.dt-carga      = TODAY.
                   es-integra-retorno.EP-CODIGO     = IF AVAIL ext-item-cfa THEN ext-item-cfa.EP-CODIGO ELSE "0".
                   es-integra-retorno.idsin         = IF AVAIL ext-item-cfa THEN ext-item-cfa.idsin  ELSE 0.
                   es-integra-retorno.dt-int-erp    = TODAY.                                                                                                 
                   es-integra-retorno.IdKlassmatt   = IF AVAIL ext-item-cfa THEN ext-item-cfa.IdKlassmatt ELSE 0.                                            
                   es-integra-retorno.codigo        = ITEM.it-codigo.                                
                   es-integra-retorno.StatusRetorno = "S".                                                                                                   
                   es-integra-retorno.log-retorno   = IF ITEM.it-codigo MATCHES "*r"  THEN "Item recuperado " + ITEM.it-codigo + "Chancelado com sucesso" ELSE "Expans∆o do item " + ITEM.it-codigo + " chancelado com sucesso".

            RUN pi-grava-log-monitor.

            IF l-oriundo-jde THEN
            DO:            
               ASSIGN es-integra-retorno.log-retorno = es-integra-retorno.log-retorno + ". ORIGEM JDE".
               RUN pi-replica-item (INPUT ITEM.it-codigo).
            END.
         END.


         IF ITEM.it-codigo MATCHES "*r" THEN
            RUN pi-replica-item (INPUT ITEM.it-codigo).         
      END.
   END.
END.

FIND FIRST es-integra-retorno NO-LOCK NO-ERROR.

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
  {src/adm/template/snd-list.i "tt-chancelador"}

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

&IF DEFINED(EXCLUDE-pi-grava-log-monitor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-log-monitor Procedure 
PROCEDURE pi-grava-log-monitor:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* ------------------------------------------------------------------------------ */
/* Begins Jan/2019 - Willians Ambrosio DKP                                        */             
/* ------------------------------------------------------------------------------ */
FIND FIRST es-klassmatt-integr WHERE
           es-klassmatt-integr.idklassmatt = es-integra-retorno.idklassmatt          AND
           es-klassmatt-integr.dt-trans    = TODAY                                   AND
           es-klassmatt-integr.hr-trans    = REPLACE(STRING(TIME,"HH:MM:SS"),":","") EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL es-klassmatt-integr THEN
DO:
   CREATE es-klassmatt-integr.
   BUFFER-COPY es-integra-retorno   TO es-klassmatt-integr
                                ASSIGN es-klassmatt-integr.dt-trans    = TODAY                                   
                                       es-klassmatt-integr.hr-trans    = STRING(TIME,"HH:MM:SS"). 
END.   
/* ------------------------------------------------------------------------------ */
/* End Jan/2019 - Willians Ambrosio DKP                                           */
/* ------------------------------------------------------------------------------ */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
