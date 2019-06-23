&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer portador    for ems2cadme.portador.

{include/i-prgvrs.i ESFT0515 2.00.00.025}  /*** 010025 ***/
 
 
/*------------------------------------------------------------------------
 
  File: 
 
  Description: 
 
  Input Parameters:
      <none>
 
  Output Parameters:
      <none>
 
  Author: 
 
  Created: 
 
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
 
/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */
 
&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp

 
/* Parameters Definitions ---                                           */
 
/* Temporary Table Definitions ---                                      */
 
define temp-table tt-param
    field destino          as integer
    field arquivo          as char
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field identific        as char
    field ini-cod-estabel  as char
    field fim-cod-estabel  as char
    field ini-serie        as char
    field fim-serie        as char
    field ini-cdd-embarq   as dec
    field fim-cdd-embarq   as dec
    field ini-nr-nota-fis  as char
    field fim-nr-nota-fis  as char
    field rs-imprime       as integer
    field banco            as integer
    field cod-febraban     as integer
    field cod-portador     as integer
    field prox-bloq        as char
    field c-instrucao      as char extent 5
    field imprime-bloq     as logical.
    
define new shared temp-table tt-notas-impressas
    field r-nota as rowid.
 
define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem.
 
define buffer b-tt-digita for tt-digita.
 
/* Transfer Definitions */
 
def var raw-param        as raw no-undo.

def var i-empresa like param-global.empresa-prin no-undo.
{cdp/cdcfgdis.i}

def temp-table tt-raw-digita
   field raw-digita      as raw.

 
/* Local Variable Definitions ---                                       */
 
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def new shared var i-tipo  as integer.
def new shared var r-nota  as rowid.
def var i-sit-nota         as integer no-undo.
def var i-cod-febraban     as integer no-undo.
def var l-main             as logical.
def var c-resp             as char init "nao".
def var i-cod-portador     as integer no-undo.
def var c-prox-bloq        as char no-undo.
def var i-tamanho-bloq     as integer no-undo.
def var i-tamanho          as integer no-undo.
def var c-formato          as char.
def var c-bloq-digito      as char.
def var c-numero           as char.
def var c-bloq             as char.
def var c-oper             as char.
def var c-arquivo-salvo    as char    no-undo.
DEF VAR l-embarque         AS LOGICAL NO-UNDO.

def new global shared var i-ep-codigo-usuario  like ems2cadme.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var h-rsocial             as Handle                           no-undo.
def new global shared var l-achou-prog          as Logical                          no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-imp

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES nota-fiscal

/* Definitions for FRAME f-pg-par                                       */
&Scoped-define QUERY-STRING-f-pg-par FOR EACH nota-fiscal SHARE-LOCK
&Scoped-define OPEN-QUERY-f-pg-par OPEN QUERY f-pg-par FOR EACH nota-fiscal SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-f-pg-par nota-fiscal
&Scoped-define FIRST-TABLE-IN-QUERY-f-pg-par nota-fiscal


/* Definitions for FRAME f-pg-sel                                       */
&Scoped-define QUERY-STRING-f-pg-sel FOR EACH nota-fiscal SHARE-LOCK
&Scoped-define OPEN-QUERY-f-pg-sel OPEN QUERY f-pg-sel FOR EACH nota-fiscal SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-f-pg-sel nota-fiscal
&Scoped-define FIRST-TABLE-IN-QUERY-f-pg-sel nota-fiscal


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-destino bt-arquivo c-arquivo ~
bt-config-impr rs-execucao rs-destino-bloq bt-arquivo-bloq ~
bt-config-impr-bloq RECT-11 RECT-7 RECT-9 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao ~
rs-destino-bloq c-arquivo-bloq text-modo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-arquivo-bloq 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr-bloq 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-arquivo-bloq AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-destino-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Destino Bloqueto" 
      VIEW-AS TEXT 
     SIZE 16.29 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 9.43 BY .67 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-destino-bloq AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-line", 1,
"Batch", 2
     SIZE 25.72 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.43 BY 1.83.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE BUTTON bt-next 
     IMAGE-UP FILE "image\im-nex":U
     LABEL "Button 2" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-prev 
     IMAGE-UP FILE "image\im-pre":U
     LABEL "Button 1" 
     SIZE 4 BY 1.25.

DEFINE VARIABLE rs-banco AS CHARACTER FORMAT "X(256)":U INITIAL "Bradesco" 
     VIEW-AS COMBO-BOX INNER-LINES 9
     LIST-ITEMS "Bradesco","Banco do Brasil","Banco Ita£","Banco Francàs e Brasileiro","Banco Bamerindus","Banco Boa Vista","Banco Real","Bradesco 80 colunas","Banco Safra" 
     DROP-DOWN-LIST
     SIZE 28.86 BY 1 NO-UNDO.

DEFINE VARIABLE c-num-bloq AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26.14 BY .88 NO-UNDO.

DEFINE VARIABLE text-3 AS CHARACTER FORMAT "X(256)":U INITIAL "N£mero Bloqueto" 
      VIEW-AS TEXT 
     SIZE 17.14 BY .67 NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 32 BY 3.63.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 32.57 BY 3.63.

DEFINE VARIABLE i-bloq AS LOGICAL INITIAL no 
     LABEL "Emitir Bloqueto" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE c-cod-estabel AS CHARACTER FORMAT "X(3)" 
     LABEL "c-cod-estabel":R18 
     VIEW-AS FILL-IN 
     SIZE 7.43 BY .88.

DEFINE VARIABLE c-dt-saida AS DATE FORMAT "99/99/9999" 
     LABEL "c-dt-saida":R18 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .88.

DEFINE VARIABLE c-hr-saida AS CHARACTER FORMAT "xx:xx:xx":U INITIAL "000000" 
     LABEL "c-hr-saida" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE c-nr-nota-fis-fim AS CHARACTER FORMAT "x(16)" INITIAL "999.999" 
     VIEW-AS FILL-IN 
     SIZE 17.43 BY .88.

DEFINE VARIABLE c-nr-nota-fis-ini AS CHARACTER FORMAT "x(16)" INITIAL "0" 
     LABEL "c-nr-nota-fis-ini":R25 
     VIEW-AS FILL-IN 
     SIZE 17.43 BY .88.

DEFINE VARIABLE c-serie AS CHARACTER FORMAT "x(5)" 
     LABEL "c-serie":R7 
     VIEW-AS FILL-IN 
     SIZE 9.43 BY .88.

DEFINE VARIABLE de-cdd-embarq-fim AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9" INITIAL 9999999999999999 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88.

DEFINE VARIABLE de-cdd-embarq-ini AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9" INITIAL 0 
     LABEL "de-cdd-embarq-ini":R10 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .92.

DEFINE VARIABLE i-ind-sit-nota AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impress∆o", 1,
"Reimpress∆o", 2
     SIZE 31.43 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 34.86 BY 1.71.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0  
     SIZE 78.72 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0  
     SIZE .43 BY 11.21
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0  
     SIZE .43 BY 11.17
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0  
     SIZE 78.72 BY .13
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY f-pg-par FOR 
      nota-fiscal SCROLLING.

DEFINE QUERY f-pg-sel FOR 
      nota-fiscal SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2 COL 3.14 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.21 COL 43.14 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 3.25 COL 3.14 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.25 COL 43 HELP
          "Configuraá∆o da impressora"
     rs-execucao AT ROW 5.63 COL 5.29 HELP
          "Modo de Execuá∆o" NO-LABEL
     rs-destino-bloq AT ROW 9 COL 3.14 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-arquivo-bloq AT ROW 10.21 COL 43.14 HELP
          "Escolha do nome do arquivo"
     c-arquivo-bloq AT ROW 10.25 COL 3.14 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-config-impr-bloq AT ROW 10.25 COL 43.43 HELP
          "Configuraá∆o da impressora"
     text-destino AT ROW 1.25 COL 3.72 NO-LABEL
     text-modo AT ROW 4.75 COL 1.72 COLON-ALIGNED NO-LABEL
     text-destino-2 AT ROW 8.25 COL 3.72 NO-LABEL
     RECT-11 AT ROW 4.96 COL 2
     RECT-7 AT ROW 1.54 COL 2
     RECT-9 AT ROW 8.54 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.63
         SIZE 73.72 BY 11.

DEFINE FRAME f-pg-sel
     c-cod-estabel AT ROW 2.17 COL 23.14 COLON-ALIGNED
     c-serie AT ROW 3.17 COL 23.14 COLON-ALIGNED
     c-nr-nota-fis-ini AT ROW 4.17 COL 23.14 COLON-ALIGNED
     c-nr-nota-fis-fim AT ROW 4.17 COL 49.72 COLON-ALIGNED NO-LABEL
     de-cdd-embarq-ini AT ROW 5.17 COL 23.14 COLON-ALIGNED
     de-cdd-embarq-fim AT ROW 5.17 COL 49.86 COLON-ALIGNED NO-LABEL
     c-dt-saida AT ROW 6.17 COL 23.14 COLON-ALIGNED
     c-hr-saida AT ROW 7.17 COL 23.29 COLON-ALIGNED
     i-ind-sit-nota AT ROW 9.17 COL 25.72 NO-LABEL
     IMAGE-2 AT ROW 5.17 COL 48
     IMAGE-3 AT ROW 4.17 COL 43.14
     IMAGE-5 AT ROW 5.17 COL 43.14
     IMAGE-6 AT ROW 4.17 COL 48
     RECT-8 AT ROW 8.75 COL 23
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-imp AT ROW 1.5 COL 33.57
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-par
     i-bloq AT ROW 1.5 COL 2
     rs-banco AT ROW 3.83 COL 1.57 COLON-ALIGNED HELP
          "Banco para emiss∆o de bloqueto" NO-LABEL
     bt-prev AT ROW 4 COL 38.29
     bt-next AT ROW 4 COL 42.14
     c-num-bloq AT ROW 5.5 COL 36.29 COLON-ALIGNED NO-LABEL
     text-3 AT ROW 3 COL 35.86 COLON-ALIGNED NO-LABEL
     RECT-12 AT ROW 3.25 COL 2
     RECT-13 AT ROW 3.25 COL 35.29
     "Lay-out" VIEW-AS TEXT
          SIZE 7.86 BY .75 AT ROW 2.75 COL 3.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 76.86 BY 10.92.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 15.17
         WIDTH              = 81.14
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.33
         VIRTUAL-WIDTH      = 114.29
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */
 
{src/adm/method/containr.i}
{include/w-relat.i}
 
/* Include Com as Vari†veis Globais */
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME f-pg-par:FRAME = FRAME f-relat:HANDLE.

/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR EDITOR c-arquivo-bloq IN FRAME f-pg-imp
   NO-ENABLE                                                            */
ASSIGN 
       rs-execucao:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execuá∆o".

/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-destino-2 IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino-2:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino Bloqueto".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-ENABLE                                                            */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execuá∆o".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN c-num-bloq IN FRAME f-pg-par
   NO-ENABLE                                                            */
ASSIGN 
       RECT-13:HIDDEN IN FRAME f-pg-par           = TRUE.

/* SETTINGS FOR COMBO-BOX rs-banco IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN text-3 IN FRAME f-pg-par
   NO-ENABLE                                                            */
ASSIGN 
       text-3:PRIVATE-DATA IN FRAME f-pg-par     = 
                "N£mero Bloqueto".

/* SETTINGS FOR FRAME f-pg-sel
   L-To-R                                                               */
/* SETTINGS FOR FRAME f-relat
   L-To-R                                                               */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-imp
/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-par
/* Query rebuild information for FRAME f-pg-par
     _TblList          = "nota-fiscal"
     _Query            is NOT OPENED
*/  /* FRAME f-pg-par */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _TblList          = "nota-fiscal"
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo C-Win
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-arquivo-bloq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-bloq C-Win
ON CHOOSE OF bt-arquivo-bloq IN FRAME f-pg-imp
DO:
    def var c-arq-conv  as char no-undo.
 
    assign c-arq-conv = replace(input frame {&frame-name} c-arquivo-bloq, "/", "\").
    SYSTEM-DIALOG GET-FILE c-arq-conv
       FILTERS "*.lst" "*.lst",
               "*.*" "*.*"
       ASK-OVERWRITE 
       DEFAULT-EXTENSION "lst"
       INITIAL-DIR "spool" 
       SAVE-AS
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then do:
        assign c-arquivo-bloq = replace(c-arq-conv, "\", "/"). 
        display c-arquivo-bloq with frame {&frame-name}.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar C-Win
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Cancelar */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr C-Win
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-config-impr-bloq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr-bloq C-Win
ON CHOOSE OF bt-config-impr-bloq IN FRAME f-pg-imp
DO:
 
def var c-arquivo-temp as char no-undo.
def var c-impressora as char no-undo.
def var c-arq as char no-undo.
def var c-layout as char no-undo.
def var c-ant as char no-undo.
 
assign c-ant = c-arquivo-bloq:screen-value in frame f-pg-imp
       c-arquivo-temp =  replace(c-arquivo-bloq:screen-value in frame f-pg-imp,":",",").
if c-arquivo-bloq:screen-value in frame f-pg-imp <> "" then do:
  if num-entries(c-arquivo-temp) = 4 then
    assign c-impressora = entry(1,c-arquivo-temp)
           c-layout     = entry(2,c-arquivo-temp)
           c-arq        = entry(3,c-arquivo-temp) + ":" + entry(4,c-arquivo-temp).
  if num-entries(c-arquivo-temp) = 3 then
    assign c-impressora = entry(1,c-arquivo-temp)
           c-layout     = entry(2,c-arquivo-temp)
           c-arq        = entry(3,c-arquivo-temp).
  if num-entries(c-arquivo-temp) = 2 then
    assign c-impressora = entry(1,c-arquivo-temp)
           c-layout     = entry(2,c-arquivo-temp)
           c-arq        = "".
end.         
 
run utp/ut-impr.w (input-output c-impressora, input-output c-layout, input-output c-arq).
 
if c-arq = "" then
  assign c-arquivo-bloq = c-impressora + ":" + c-layout.
else
  assign c-arquivo-bloq = c-impressora + ":" + c-layout + ":" + c-arq.  
 
 
 
if c-arquivo = ":" then
  assign c-arquivo-bloq = c-ant.
 
assign c-imp-old = c-arquivo-bloq.
 
disp c-arquivo-bloq with frame f-pg-imp.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar C-Win
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME bt-next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-next C-Win
ON CHOOSE OF bt-next IN FRAME f-pg-par /* Button 2 */
DO:
 
   if avail portador then do:
      assign c-oper = "+"
             c-bloq = c-num-bloq:screen-value in frame {&frame-name}.
 
      run ftp/ft0503c.p (input  rowid(portador),
                         input  c-bloq,
                         input  c-oper,
                         output c-bloq-digito).
      run pi-mostra-numero.                             
   end.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-prev C-Win
ON CHOOSE OF bt-prev IN FRAME f-pg-par /* Button 1 */
DO:
   if avail portador then do:
      assign c-oper = "-"
             c-bloq = c-num-bloq:screen-value in frame {&frame-name}.
 
      run ftp/ft0503c.p (input  rowid(portador),
                         input  c-bloq,
                         input  c-oper,
                         output c-bloq-digito).
      run pi-mostra-numero.                             
   end.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME c-dt-saida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-dt-saida C-Win
ON LEAVE OF c-dt-saida IN FRAME f-pg-sel /* c-dt-saida */
DO:
  {cdp/cd9998.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-nr-nota-fis-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-nota-fis-fim C-Win
ON F5 OF c-nr-nota-fis-fim IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom = dizoom/z03di135.w
                     &campo=c-cod-estabel 
                     &campozoom=cod-estabel 
                     &campo2=c-serie 
                     &campozoom2=serie
                     &campo3=c-nr-nota-fis-fim
                     &campozoom3=nr-nota-fis
                     &parametros="run pi-seta-inicial in wh-pesquisa 
                                      (input frame {&frame-name} c-cod-estabel,
                                       input frame {&frame-name} c-serie,
                                       input frame {&frame-name} c-nr-nota-fis-ini)."} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-nota-fis-fim C-Win
ON MOUSE-SELECT-DBLCLICK OF c-nr-nota-fis-fim IN FRAME f-pg-sel
DO:
  apply "f5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-nr-nota-fis-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-nota-fis-ini C-Win
ON F5 OF c-nr-nota-fis-ini IN FRAME f-pg-sel /* c-nr-nota-fis-ini */
DO:
    {include/zoomvar.i &prog-zoom = dizoom/z03di135.w
                     &campo=c-cod-estabel 
                     &campozoom=cod-estabel 
                     &campo2=c-serie 
                     &campozoom2=serie
                     &campo3=c-nr-nota-fis-ini
                     &campozoom3=nr-nota-fis
                     &parametros="run pi-seta-inicial in wh-pesquisa 
                                      (input frame {&frame-name} c-cod-estabel,
                                       input frame {&frame-name} c-serie,
                                       input frame {&frame-name} c-nr-nota-fis-ini)."}

 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-nota-fis-ini C-Win
ON MOUSE-SELECT-DBLCLICK OF c-nr-nota-fis-ini IN FRAME f-pg-sel /* c-nr-nota-fis-ini */
DO:
  apply "f5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME i-bloq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-bloq C-Win
ON VALUE-CHANGED OF i-bloq IN FRAME f-pg-par /* Emitir Bloqueto */
DO:
   if i-bloq:checked in frame f-pg-par = yes then do:
 
      {utp/ut-liter.i N£mero_Bloqueto * L}
      assign text-3:screen-value in frame f-pg-par = return-value.
 
      assign c-arquivo-bloq:sensitive in frame f-pg-imp = yes
             bt-arquivo-bloq:visible   = yes
             bt-config-impr-bloq:visible  = yes.
      enable rs-destino-bloq with frame f-pg-imp. 
      enable rs-banco        with frame f-pg-par.
      apply "mouse-select-click" to im-pg-imp in frame f-relat.
      apply 'value-changed' to rs-destino-bloq in frame f-pg-imp.
      apply "mouse-select-click" to im-pg-par in frame f-relat.   

      find first param-global no-lock no-error.

      find nota-fiscal use-index ch-nota no-lock
           where nota-fiscal.cod-estabel = input frame f-pg-sel c-cod-estabel
           and   nota-fiscal.serie       = input frame f-pg-sel c-serie
           and   nota-fiscal.nr-nota-fis = input frame f-pg-sel c-nr-nota-fis-ini no-error.

      IF  NOT AVAIL nota-fiscal
      AND INPUT FRAME f-pg-sel de-cdd-embarq-ini <> 0 THEN
          FIND FIRST nota-fiscal WHERE
            nota-fiscal.cdd-embarq = INPUT FRAME f-pg-sel de-cdd-embarq-ini NO-LOCK NO-ERROR.

      if avail nota-fiscal then do:

         assign i-empresa = param-global.empresa-prin.

         &if defined (bf_dis_consiste_conta) &then
          
             find estabelec where
                  estabelec.cod-estabel = nota-fiscal.cod-estabel no-lock no-error.
          
             run cdp/cd9970.p (input rowid(estabelec),
                               output i-empresa).
         &endif
      
         find portador use-index codigo no-lock
              where portador.ep-codigo    = i-empresa
              and   portador.cod-portador = nota-fiscal.cod-portador
              and   portador.modalidade   = nota-fiscal.modalidade no-error.
         if avail portador then
            case portador.cod-febraban:
              when 237 then 
                   assign rs-banco:screen-value in frame f-pg-par = "Bradesco".
              when 001 then     
                   assign rs-banco:screen-value in frame f-pg-par = "Banco do Brasil".
              when 341 then     
                   assign rs-banco:screen-value in frame f-pg-par = "Banco Ita£".
              when 346 then     
                   assign rs-banco:screen-value in frame f-pg-par = "Banco Francàs e Brasileiro".
              when 399 then      
                   assign rs-banco:screen-value in frame f-pg-par = "Banco Bamerindus".
              when 231 then 
                   assign rs-banco:screen-value in frame f-pg-par = "Banco Boa Vista".
              when 275 then 
                   assign rs-banco:screen-value in frame f-pg-par = "Banco Real".
              when 237 then
                   assign rs-banco:screen-value in frame f-pg-par = "Bradesco 80 colunas".     
              when 422 then
                   assign rs-banco:screen-value in frame f-pg-par = "Banco Safra".     
            end.
      end.
      
      apply 'value-changed' to rs-banco in frame f-pg-par.    
      assign text-3:visible     in frame f-pg-par = yes.
 
      assign rect-13:visible    in frame f-pg-par = yes
             bt-prev:visible    in frame f-pg-par = yes
             bt-next:visible    in frame f-pg-par = yes
             c-num-bloq:visible in frame f-pg-par = yes.
   end.
   else do:
      assign c-arquivo-bloq:sensitive in frame f-pg-imp  = no
             bt-arquivo-bloq:visible   = no
             bt-config-impr-bloq:visible  = no.
      disable rs-destino-bloq 
              bt-arquivo-bloq with frame f-pg-imp. 
      disable rs-banco with frame f-pg-par.
      CLEAR  frame f-pg-par NO-PAUSE.
      assign c-arquivo-bloq:visible in frame f-pg-imp = no.        
 
      assign text-3:visible     in frame f-pg-par = no
             rect-13:visible    in frame f-pg-par = no
             bt-prev:visible    in frame f-pg-par = no
             bt-next:visible    in frame f-pg-par = no
             c-num-bloq:visible in frame f-pg-par = no.
   end.    
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME i-ind-sit-nota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-ind-sit-nota C-Win
ON VALUE-CHANGED OF i-ind-sit-nota IN FRAME f-pg-sel
DO:
   if input frame f-pg-sel i-ind-sit-nota = 1 then
      assign i-bloq:sensitive in frame f-pg-par = yes.
   else do with frame f-pg-par:
      CLEAR frame f-pg-par NO-PAUSE.
      assign i-bloq:checked     = no
/*             i-bloq:sensitive   = no*/
             rs-banco:sensitive = no
             text-3:visible     = no
             rect-13:visible    = no
             bt-prev:visible    = no
             bt-next:visible    = no
             c-num-bloq:visible = no.
   end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME de-cdd-embarq-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL de-cdd-embarq-fim C-Win
ON F5 OF de-cdd-embarq-fim IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = "dizoom/z01di041.w"
                     &campo = de-cdd-embarq-fim
                     &campozoom = cdd-embarq}
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL de-cdd-embarq-fim C-Win
ON MOUSE-SELECT-DBLCLICK OF de-cdd-embarq-fim IN FRAME f-pg-sel
DO:
  apply "f5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME de-cdd-embarq-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL de-cdd-embarq-ini C-Win
ON F5 OF de-cdd-embarq-ini IN FRAME f-pg-sel /* de-cdd-embarq-ini */
DO:
  {include/zoomvar.i &prog-zoom = "dizoom/z01di041.w"
                     &campo = de-cdd-embarq-ini
                     &campozoom = cdd-embarq}
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL de-cdd-embarq-ini C-Win
ON MOUSE-SELECT-DBLCLICK OF de-cdd-embarq-ini IN FRAME f-pg-sel /* de-cdd-embarq-ini */
DO:
  apply "f5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp C-Win
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par C-Win
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel C-Win
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME rs-banco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-banco C-Win
ON VALUE-CHANGED OF rs-banco IN FRAME f-pg-par
DO:

      case input frame {&frame-name} rs-banco: 
         when "Bradesco" then 
              assign i-cod-febraban = 237.
         when "Banco do Brasil" then     
              assign i-cod-febraban = 001.
         when "Banco Ita£" then     
              assign i-cod-febraban = 341.
         when "Banco Francàs e Brasileiro" then     
              assign i-cod-febraban = 346.
         when "Banco Bamerindus" then      
              assign i-cod-febraban = 399.
         when "Banco Boa Vista" then 
              assign i-cod-febraban = 231.
         when "Banco Real" then 
              assign i-cod-febraban = 275.
         when "Bradesco 80 colunas" then
              assign i-cod-febraban = 237.     
         when "Banco Safra" then
              assign i-cod-febraban = 422.
    end.           

    if rs-banco:sensitive in frame f-pg-par = yes then do:   
       assign c-num-bloq:screen-value in frame {&frame-name} = "".

       find first param-global no-lock no-error.

       assign i-empresa = param-global.empresa-prin.

       &if defined (bf_dis_consiste_conta) &then

           find estabelec where
                estabelec.cod-estabel = INPUT FRAME f-pg-sel c-cod-estabel no-lock no-error.

           IF AVAIL estabelec THEN
               run cdp/cd9970.p (input rowid(estabelec),
                                 output i-empresa).
       &endif

        ASSIGN l-embarque = NO.

        /* busca por embarque */
        IF  int(INPUT FRAME f-pg-sel c-nr-nota-fis-ini) = 0
        AND INPUT FRAME f-pg-sel de-cdd-embarq-ini <> 0 THEN
            FOR EACH nota-fiscal FIELDS (cod-estabel serie cdd-embarq cod-portador modalidade) NO-LOCK WHERE
                nota-fiscal.cdd-embarq >= INPUT FRAME f-pg-sel de-cdd-embarq-ini AND
                nota-fiscal.cdd-embarq <= INPUT FRAME f-pg-sel de-cdd-embarq-fim:

                IF nota-fiscal.cod-estabel <> INPUT FRAME f-pg-sel c-cod-estabel
                OR nota-fiscal.serie <> INPUT FRAME f-pg-sel c-serie THEN
                    NEXT.

                RUN pi-busca-numero (INPUT nota-fiscal.cod-portador,
                                     INPUT nota-fiscal.modalidade).

                ASSIGN l-embarque = YES.
            END.

        IF NOT l-embarque THEN
            FOR EACH nota-fiscal FIELDS (cod-estabel serie nr-nota-fis cod-portador modalidade) NO-LOCK WHERE
                nota-fiscal.cod-estabel  = INPUT FRAME f-pg-sel c-cod-estabel     AND
                nota-fiscal.serie        = INPUT FRAME f-pg-sel c-serie           AND
                nota-fiscal.nr-nota-fis >= INPUT FRAME f-pg-sel c-nr-nota-fis-ini AND
                nota-fiscal.nr-nota-fis <= INPUT FRAME f-pg-sel c-nr-nota-fis-fim:
    
                RUN pi-busca-numero (INPUT nota-fiscal.cod-portador,
                                     INPUT nota-fiscal.modalidade).
            END.
    end.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino C-Win
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = yes.
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   c-arquivo               = session:temp-directory + "esft0515" + ".tmp"
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-destino-bloq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino-bloq C-Win
ON VALUE-CHANGED OF rs-destino-bloq IN FRAME f-pg-imp
DO:
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo-bloq:visible  = yes
                   c-arquivo-bloq:screen-value in frame f-pg-imp = ""
                   bt-arquivo-bloq:visible   = no
                   bt-config-impr-bloq:visible  = yes.
            disable bt-arquivo-bloq with frame f-pg-imp.
            enable  bt-config-impr-bloq
                    c-arquivo-bloq with frame f-pg-imp.         
        end.
        when "2" then do:
            if rs-destino-bloq:sensitive in frame f-pg-imp = yes then do:
               assign c-arquivo-bloq:visible = yes.
               find usuar_mestre where usuar_mestre.cod_usuario = c-seg-usuario no-lock no-error.
               if  avail usuar_mestre
                  then do:
                   assign c-arquivo-bloq:screen-value = if length(usuar_mestre.nom_subdir_spool) <> 0
                                                        then caps(replace(usuar_mestre.nom_dir_spool, "~\", "~/") + "~/" + replace(usuar_mestre.nom_subdir_spool, "~\", "~/") + "~/esft0515b~.lst")
                                                        else caps(replace(usuar_mestre.nom_dir_spool, "~\", "~/") + "~/esft0515b~.lst").
                                               
               end.  
               else do:
                 assign c-arquivo-bloq:screen-value = caps("spool~/esft0515b~.lst").
               end.   
               
               assign bt-arquivo-bloq:visible      = yes
                      c-arquivo-bloq:sensitive     = yes
                      bt-config-impr-bloq:visible  = no.
               enable bt-arquivo-bloq with frame f-pg-imp.
               disable bt-config-impr-bloq with frame f-pg-imp.       
            end.
            else do:
               assign c-arquivo-bloq:sensitive  = no
                      bt-arquivo-bloq:visible   = no
                      bt-config-impr-bloq:visible  = no.
               disable bt-arquivo-bloq
                       bt-config-impr-bloq with frame f-pg-imp.      
            end.          
        end.
        when "3" then do:
             assign c-arquivo-bloq:visible  = no
                    bt-arquivo-bloq:visible   = no
                    bt-config-impr-bloq:visible  = no.
             disable bt-arquivo-bloq
                     bt-config-impr-bloq with frame f-pg-imp.      
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


{utp/ut9000.i "ESFT0515" "2.00.00.025"}
 
/* ***************************  Main Block  *************************** */
 
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
 
 
/* inicializaá‰es do template de relat¢rio */
{include/i-rpini.i}
 
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.
 
{include/i-rplbl.i}
 
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
 
 
 
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 
 
    RUN enable_UI.
 
assign rs-destino-bloq:screen-value in frame f-pg-imp = "3".
assign l-main = yes.
 
apply 'value-changed' to rs-banco in frame f-pg-par.
apply 'value-changed' to rs-destino-bloq in frame f-pg-imp.
apply "mouse-select-click" to im-pg-sel in frame f-relat.
 
 
{utp/ut-field.i mgdis nota-fiscal cod-estabel 1}
assign c-cod-estabel:label in frame f-pg-sel = return-value.
 
{utp/ut-field.i mgdis nota-fiscal serie 1}
assign c-serie:label in frame f-pg-sel = return-value.
 
{utp/ut-field.i mgdis nota-fiscal nr-nota-fis 1}
assign c-nr-nota-fis-ini:label in frame f-pg-sel = return-value.
 
{utp/ut-field.i mgdis nota-fiscal cdd-embarq 1}
assign de-cdd-embarq-ini:label in frame f-pg-sel = return-value.
 
{utp/ut-field.i mgdis nota-fiscal dt-saida 1}
assign c-dt-saida:label in frame f-pg-sel = return-value.
 
{utp/ut-liter.i Hora_de_Sa°da * L}
assign c-hr-saida:label in frame f-pg-sel = return-value.
 
{utp/ut-liter.i N£mero_Bloqueto * L}
assign text-3:screen-value in frame f-pg-par = return-value.
 
assign c-hr-saida:screen-value in frame f-pg-sel = "".
i-pais-impto-usuario = 1.
 
 
if c-nr-nota-fis-ini:load-mouse-pointer ("image/lupa.cur") in frame f-pg-sel then.
if c-nr-nota-fis-fim:load-mouse-pointer ("image/lupa.cur") in frame f-pg-sel then.
if de-cdd-embarq-ini:load-mouse-pointer ("image/lupa.cur") in frame f-pg-sel then.
if de-cdd-embarq-fim:load-mouse-pointer ("image/lupa.cur") in frame f-pg-sel then.
 
assign c-hr-saida:screen-value in frame f-pg-sel = "".
 
 
 
    {include/i-rpmbl.i}
 
    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
assign c-arquivo-bloq:visible in frame f-pg-imp = no.
 
assign text-3:visible     in frame f-pg-par = no
       rect-13:visible    in frame f-pg-par = no
       bt-prev:visible    in frame f-pg-par = no
       bt-next:visible    in frame f-pg-par = no
       c-num-bloq:visible in frame f-pg-par = no.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects C-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available C-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE bt-executar bt-cancelar bt-ajuda im-pg-imp im-pg-par im-pg-sel 
      WITH FRAME f-relat IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-destino c-arquivo rs-execucao rs-destino-bloq c-arquivo-bloq 
          text-modo 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  ENABLE rs-destino bt-arquivo c-arquivo bt-config-impr rs-execucao 
         rs-destino-bloq bt-arquivo-bloq bt-config-impr-bloq RECT-11 RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY i-bloq rs-banco c-num-bloq text-3 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE i-bloq bt-prev bt-next RECT-12 RECT-13 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY c-cod-estabel c-serie c-nr-nota-fis-ini c-nr-nota-fis-fim 
          de-cdd-embarq-ini de-cdd-embarq-fim c-dt-saida c-hr-saida 
          i-ind-sit-nota 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE c-cod-estabel c-serie c-nr-nota-fis-ini c-nr-nota-fis-fim 
         de-cdd-embarq-ini de-cdd-embarq-fim c-dt-saida c-hr-saida 
         i-ind-sit-nota IMAGE-2 IMAGE-3 IMAGE-5 IMAGE-6 RECT-8 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-numero C-Win 
PROCEDURE pi-busca-numero :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAM p-cod-portador LIKE nota-fiscal.cod-portador.
    DEF INPUT PARAM p-modalidade   LIKE nota-fiscal.modalidade.

    find first portador
         where portador.ep-codigo    = i-empresa
         and   portador.cod-portador = p-cod-portador
         and   portador.modalidade   = p-modalidade 
         and   portador.cod-febraban = i-cod-febraban no-lock no-error.

    if avail portador
    and portador.emite-bloq = 1 then do:
        assign i-cod-portador = portador.cod-portador.
        if portador.char-1 <> "" then do:
            assign i-tamanho-bloq = length(portador.char-1)
                   c-bloq = portador.char-1
                   c-oper = "". 

            run ftp/ft0503c.p (input  rowid(portador),
                               input  c-bloq,
                               input  c-oper,
                               output c-bloq-digito).
            run pi-mostra-numero.
            leave.
        end.       
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar C-Win 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
do  on error undo, return error
    on stop  undo, return error:     
 
    {include/i-rpexa.i}
 
    if  input frame f-pg-imp rs-destino = 2 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        if  return-value = "nok" then do:
            run utp/ut-msgs.p (input "show",
                               input 73,
                               input "").
            apply 'mouse-select-click' to im-pg-imp in frame f-relat.
            apply 'entry' to c-arquivo in frame f-pg-imp.                   
            return error.
        end.
    end.
 
    /* Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p†gina 
       com problemas e colocar o focus no campo com problemas             */    
 
    find estabelec
         where estabelec.cod-estabel = input frame f-pg-sel c-cod-estabel
         no-lock no-error.
    if not avail estabelec then do:
      {utp/ut-table.i mgadm estabelec 1}
       run utp/ut-msgs.p (input "show",
                          input 47,
                          input return-value).
       apply 'mouse-select-click' to im-pg-sel in frame f-relat.
       apply 'entry' to c-cod-estabel in frame f-pg-sel.
       return 'adm-error'.
    end.
 
    find serie 
         where serie.serie = input frame f-pg-sel c-serie 
         no-lock no-error.
    if not avail serie then do:
      {utp/ut-table.i mgind serie 1}
       run utp/ut-msgs.p (input "show",
                          input 47,
                          input return-value).
       apply 'mouse-select-click' to im-pg-sel in frame f-relat.
       apply 'entry' to c-serie in frame f-pg-sel.
       return 'adm-error'.
    end.
 
    if  input frame f-pg-sel c-nr-nota-fis-fim <
        input frame f-pg-sel c-nr-nota-fis-ini then do:
        run utp/ut-msgs.p (input "show",
                           input 252,
                           input "").
        apply 'mouse-select-click' to im-pg-sel in frame f-relat.
        apply 'entry' to c-nr-nota-fis-fim in frame f-pg-sel.
        return 'adm-error'.
    end.
 
    if  input frame f-pg-sel de-cdd-embarq-fim <
        input frame f-pg-sel de-cdd-embarq-ini then do:
        run utp/ut-msgs.p (input "show",
                           input 252,
                           input "").
        apply 'mouse-select-click' to im-pg-sel in frame f-relat.
        apply 'entry' to de-cdd-embarq-fim in frame f-pg-sel.
        return 'adm-error'.
    end.
 
    if input frame f-pg-sel c-dt-saida <> ?  and
       input frame f-pg-sel c-dt-saida <> "" then do:
        run utp/ut-msgs.p (input "show",
                           input 364,
                           input "").
        if return-value = "no" then do:
           apply 'mouse-select-click' to im-pg-sel in frame f-relat.
           apply 'entry' to c-dt-saida in frame f-pg-sel.
           return 'adm-error'.
        end.
    end.
 
    if  input frame f-pg-imp rs-destino = 1 then do:
        run utp/ut-msgs.p (input "show",
                           input 302,
                           input "").
        if  return-value = "no" then do:
            apply 'mouse-select-click' to im-pg-imp in frame f-relat.
            apply 'entry' to rs-destino in frame f-pg-imp.                   
            return error.
        end.
    end. 
 
    create tt-param.
    assign tt-param.usuario           = c-seg-usuario
           tt-param.destino           = input frame f-pg-imp rs-destino
           tt-param.data-exec         = today
           tt-param.hora-exec         = time
           tt-param.ini-cod-estabel   = input frame f-pg-sel c-cod-estabel
           tt-param.ini-serie         = input frame f-pg-sel c-serie
           tt-param.ini-nr-nota-fis   = input frame f-pg-sel c-nr-nota-fis-ini
           tt-param.fim-nr-nota-fis   = input frame f-pg-sel c-nr-nota-fis-fim
           tt-param.ini-cdd-embarq   = input frame f-pg-sel de-cdd-embarq-ini
           tt-param.fim-cdd-embarq   = input frame f-pg-sel de-cdd-embarq-fim
           tt-param.data-exec         = input frame f-pg-sel c-dt-saida
           tt-param.hora-exec         = integer(input frame f-pg-sel c-hr-saida)
           tt-param.rs-imprime        = input frame f-pg-sel i-ind-sit-nota
           tt-param.prox-bloq         = input frame f-pg-par c-num-bloq
           tt-param.cod-febraban      = i-cod-febraban
           tt-param.cod-portador      = i-cod-portador.        

    case input frame f-pg-par rs-banco: 
         when "Bradesco" then 
              assign tt-param.banco = 1.
         when "Banco do Brasil" then     
              assign tt-param.banco = 2.
         when "Banco Ita£" then     
              assign tt-param.banco = 3.
         when "Banco Francàs e Brasileiro" then     
              assign tt-param.banco = 4.
         when "Banco Bamerindus" then      
              assign tt-param.banco = 5.
         when "Banco Boa Vista" then 
              assign tt-param.banco = 6.
         when "Banco Real" then 
              assign tt-param.banco = 7.
         when "Bradesco 80 colunas" then
              assign tt-param.banco = 8.     
         when "Banco Safra" then
              assign tt-param.banco = 9.
    end.           

    if  tt-param.destino = 1 then
        assign tt-param.arquivo = "".
    else
    if  tt-param.destino = 2 then 
        assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    else
        assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
 
    /* Coloque aqui a l¢gica de gravaá∆o dos parÉmtros e seleá∆o na temp-table
       tt-param */ 
 
    {include/i-rpexb.i}
 
    if  session:set-wait-state("general") then.
 
    {include/i-rprun.i ftp/esft0515rp.p "input table tt-param, input table tt-digita"}
 
    {include/i-rpexc.i}
 
    if  session:set-wait-state("") then.
 
    {include/i-rptrm.i}
 
end.
 
if  i-bloq:checked in frame f-pg-par = yes then do:
   run pi-executar-bloq.
   if avail portador then do:
      assign c-oper = " "
             c-bloq = portador.char-1.
 
      run ftp/ft0503c.p (input  rowid(portador),
                         input  c-bloq,
                         input  c-oper,
                         output c-bloq-digito).
      run pi-mostra-numero.                             
   end.
end.
   
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar-bloq C-Win 
PROCEDURE pi-executar-bloq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
do  on error undo, return error
    on stop  undo, return error:     
 
    {include/i-rpexa.i}
 
    if  input frame f-pg-imp rs-destino-bloq = 2 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo-bloq).
        if  return-value = "nok" then do:
            run utp/ut-msgs.p (input "show",
                               input 73,
                               input "").
            apply 'mouse-select-click' to im-pg-imp in frame f-relat.
            apply 'entry' to c-arquivo-bloq in frame f-pg-imp.                   
            return error.
        end.
    end.
 
    if  input frame f-pg-imp rs-destino-bloq = 1 then do:
        run utp/ut-msgs.p (input "show",
                           input 302,
                           input "").
        if  return-value = "no" then do:
            apply 'mouse-select-click' to im-pg-imp in frame f-relat.
            apply 'entry' to rs-destino-bloq in frame f-pg-imp.                   
            return error.
        end.
    end. 
 
    create tt-param.
    assign tt-param.usuario           = c-seg-usuario
           tt-param.destino           = input frame f-pg-imp rs-destino-bloq
           tt-param.data-exec         = today
           tt-param.hora-exec         = time
           tt-param.identific         = ""
           tt-param.ini-cod-estabel   = input frame f-pg-sel c-cod-estabel
           tt-param.fim-cod-estabel   = input frame f-pg-sel c-cod-estabel
           tt-param.ini-serie         = input frame f-pg-sel c-serie
           tt-param.fim-serie         = input frame f-pg-sel c-serie
           tt-param.ini-nr-nota-fis   = input frame f-pg-sel c-nr-nota-fis-ini
           tt-param.fim-nr-nota-fis   = input frame f-pg-sel c-nr-nota-fis-fim
           tt-param.ini-cdd-embarq   = input frame f-pg-sel de-cdd-embarq-ini
           tt-param.fim-cdd-embarq   = input frame f-pg-sel de-cdd-embarq-fim
           tt-param.data-exec         = input frame f-pg-sel c-dt-saida
           tt-param.hora-exec         = integer(input frame f-pg-sel c-hr-saida)
           tt-param.rs-imprime        = input frame f-pg-sel i-ind-sit-nota
           tt-param.prox-bloq         = input frame f-pg-par c-num-bloq
           tt-param.cod-febraban      = i-cod-febraban
           tt-param.cod-portador      = i-cod-portador.
           
    case input frame f-pg-par rs-banco: 
         when "Bradesco" then 
              assign tt-param.banco = 1.
         when "Banco do Brasil" then     
              assign tt-param.banco = 2.
         when "Banco Ita£" then     
              assign tt-param.banco = 3.
         when "Banco Francàs e Brasileiro" then     
              assign tt-param.banco = 4.
         when "Banco Bamerindus" then      
              assign tt-param.banco = 5.
         when "Banco Boa Vista" then 
              assign tt-param.banco = 6.
         when "Banco Real" then 
              assign tt-param.banco = 7.
         when "Bradesco 80 colunas" then
              assign tt-param.banco = 8.     
         when "Banco Safra" then
              assign tt-param.banco = 9.
    end.           

    if  tt-param.destino = 1 then
        assign tt-param.arquivo = ""
               c-arquivo-salvo  = c-arquivo:screen-value in frame f-pg-imp
               c-arquivo:screen-value in frame f-pg-imp = input frame f-pg-imp c-arquivo-bloq.
    else
    if  tt-param.destino = 2 then 
        assign tt-param.arquivo = input frame f-pg-imp c-arquivo-bloq.
    else
        assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
 
    /* Coloque aqui a l¢gica de gravaá∆o dos parÉmtros e seleá∆o na temp-table
       tt-param */ 
 
    {include/i-rpexb.i}
 
    if  tt-param.destino = 1 then
        assign c-arquivo:screen-value in frame f-pg-imp = c-arquivo-salvo.

    if  session:set-wait-state("general") then.
 
    {include/i-rprun.i ftp/ft0503b.p "input table tt-param"}
 
    {include/i-rpexc.i}
 
    if  session:set-wait-state("") then.
 
    {include/i-rptrm.i}
 
end.
 
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mostra-numero C-Win 
PROCEDURE pi-mostra-numero :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign i-tamanho-bloq = length(c-bloq-digito).
case i-cod-febraban:
     when 237 then do:
          assign c-prox-bloq    = string(decimal(substring(c-bloq-digito, 1, i-tamanho-bloq)), "99999999999")
                 c-formato      = "99999999999".
                 assign i-tamanho-bloq = length(c-bloq-digito).
     end.               
 
     when 001 then do:
          assign i-tamanho-bloq = i-tamanho-bloq - 0
                 c-formato      = "999999999999"
                 c-prox-bloq    = string(decimal(substring(c-bloq-digito, 1, i-tamanho-bloq)),c-formato).
          assign i-tamanho-bloq = length(c-bloq-digito).
     end.
 
     when 399 then
          assign i-tamanho-bloq = i-tamanho-bloq - 0
                 c-prox-bloq    = string(decimal(substring(c-bloq-digito, 1, i-tamanho-bloq)),"99999999999")
                 c-formato      = "99999999999".
 
     when 231 then 
        assign i-tamanho-bloq = i-tamanho-bloq - 0
               c-prox-bloq    = string(decimal(substring(c-bloq-digito, 1, i-tamanho-bloq)),"999999999999")
               c-formato      = "999999999999".
 
     when 341 then
        if i-tamanho-bloq > 12 then
           assign i-tamanho-bloq = i-tamanho-bloq - 12
                  c-prox-bloq    = string(decimal(substring(c-bloq-digito, 13, i-tamanho-bloq)),"99999999")
                  c-formato      = "99999999".
        else
           assign i-tamanho-bloq = i-tamanho-bloq /* - 4 */
                  c-prox-bloq    = c-bloq-digito
                  c-formato      = "99999999".

     when 346 then 
        if i-tamanho-bloq > 12 then
           assign i-tamanho-bloq = i-tamanho-bloq - 12
                  c-prox-bloq    = string(decimal(substring(c-bloq-digito, 13, i-tamanho-bloq)),"99999999")
                  c-formato      = "99999999".
        else
            assign i-tamanho-bloq = i-tamanho-bloq - 4
                   c-prox-bloq    = string(decimal(substring(c-bloq-digito, 5, i-tamanho-bloq)),"99999999")
                   c-formato      = "99999999".
 
     when 275 then 
        if i-tamanho-bloq > 7 then
           assign c-prox-bloq = string(decimal(substring(c-bloq-digito, 1, 15)), "999999999999999")
                  c-formato   = "999999999999999".
        else 
           assign c-prox-bloq = string(decimal(substring(c-bloq-digito, 1, 7)), "9999999")
                  c-formato   = "9999999".
     when 422 then do:
          assign c-prox-bloq    = string(decimal(substring(c-bloq-digito, 1, i-tamanho-bloq)), "99999999")
                 c-formato      = "99999999".
          assign i-tamanho-bloq = length(c-bloq-digito).
     end.               
end.
assign c-num-bloq:screen-value in frame f-pg-par = string(c-prox-bloq, c-formato).
 
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina C-Win 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P†gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
{include/i-rptrp.i}
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records C-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "nota-fiscal"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed C-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
 
  run pi-trata-state (p-issuer-hdl, p-state).
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

