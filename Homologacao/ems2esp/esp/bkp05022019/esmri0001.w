&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*******************************************************************************
**
** Programa  : ESMRI0001.w
**
** Objetivo  : Relat�rio de BENS (RI)
**
** Autor     : Renato Oliveira
**
** Data      : Agosto/2018
**
** Versao    : 2.12.00.000 - Desenvolvimento Inicial
**
******************************************************************************/
{include/i-prgvrs.i ESMRI0001 2.12.00.000}

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat�rio                            */
/* Obs: Retirar o valor do preprocessador para as p�ginas que n�o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR 
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp


/* Include Com as Vari�veis Globais */
{utp/ut-glob.i}                              

define temp-table tt-param no-undo
    field destino           as integer
    field arquivo           as char
    field usuario           as char
    field data-exec         as date
    field hora-exec         as INTEGER
    field cod-estabel-ini  like ri-bem.cod-estabel 
    field cod-estabel-fim  like ri-bem.cod-estabel  
    field serie-ini        like ri-bem.serie        
    field serie-fim        like ri-bem.serie        
    field nr-nota-fis-ini  like ri-bem.nr-nota-fis  
    field nr-nota-fis-fim  like ri-bem.nr-nota-fis  
    field cod-emitente-ini like ri-bem.cod-emitente
    field cod-emitente-fim like ri-bem.cod-emitente      
    field dt-emissao-ini   like ri-bem.dat-entrada
    field dt-emissao-fim   like ri-bem.dat-entrada
    field arquivo-excel    AS CHAR.

define temp-table tt-digita no-undo
    field it-codigo like ITEM.it-codigo
    index id it-codigo.
        
def temp-table tt-raw-digita
   field raw-digita      as raw.

/* Parameters Definitions --- */

/* Temporary Table Definitions --- */

define buffer b-tt-digita for tt-digita.
/* Transfer Definitions */

def var raw-param        as raw no-undo.

            
/* Local Variable Definitions ---                                       */

def    var      l-ok         as logical                  no-undo.
def    var      c-arq-digita as char                     no-undo.
def    var      c-terminal   as char                     no-undo.
def    var      i-ind        as integer format "99":U    no-undo.
def    var      i-pag        as integer                  no-undo.
def    var      c-ano        as char                     no-undo.
DEFINE VARIABLE c-desc-item  AS CHARACTER format "x(60)" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 RECT-22 rs-destino ~
bt-config-impr bt-arquivo c-arquivo rs-execucao tb-exporta c-arquivo-2 ~
bt-arquivo-2 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao ~
tb-exporta c-arquivo-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-retornaDescItem C-Win 
FUNCTION fn-retornaDescItem RETURNS CHARACTER
  (INPUT p-c-it-codigo AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE BUTTON bt-arquivo-2 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-arquivo-2 AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu��o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.43 BY 3.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE tb-exporta AS LOGICAL INITIAL yes 
     LABEL "Exportar XLS em Arquivo" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .83 NO-UNDO.

DEFINE VARIABLE cod-emitente-fim AS INTEGER FORMAT ">>>>>>>9" INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE cod-emitente-ini AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Emitente":R15 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE cod-estabel-fim AS CHARACTER FORMAT "X(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE cod-estabel-ini AS CHARACTER FORMAT "X(5)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE dt-emissao-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE dt-emissao-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Emiss�o" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE nr-nota-fis-fim AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE nr-nota-fis-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "N�mero Documento" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE serie-fim AS CHARACTER FORMAT "X(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE serie-ini AS CHARACTER FORMAT "X(5)" 
     LABEL "S�rie":R7 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

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

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 11.42
     FGCOLOR 0 .

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu��o do relat�rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     im-pg-sel AT ROW 1.5 COL 2.14
     im-pg-imp AT ROW 1.5 COL 17.86
     rt-folder AT ROW 2.5 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.86 BY 14.75
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-sel
     cod-estabel-ini AT ROW 1.25 COL 17 COLON-ALIGNED WIDGET-ID 70
     cod-estabel-fim AT ROW 1.25 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     serie-ini AT ROW 2.25 COL 17 COLON-ALIGNED WIDGET-ID 24
     serie-fim AT ROW 2.25 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     nr-nota-fis-ini AT ROW 3.25 COL 17 COLON-ALIGNED WIDGET-ID 68
     nr-nota-fis-fim AT ROW 3.25 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     cod-emitente-ini AT ROW 4.25 COL 17 COLON-ALIGNED WIDGET-ID 50
     cod-emitente-fim AT ROW 4.25 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     dt-emissao-ini AT ROW 5.25 COL 17 COLON-ALIGNED WIDGET-ID 72
     dt-emissao-fim AT ROW 5.25 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     IMAGE-1 AT ROW 1.25 COL 36.57
     IMAGE-2 AT ROW 1.25 COL 41.57
     IMAGE-9 AT ROW 5.25 COL 36.57 WIDGET-ID 6
     IMAGE-10 AT ROW 5.25 COL 41.57 WIDGET-ID 8
     IMAGE-3 AT ROW 2.25 COL 36.57 WIDGET-ID 34
     IMAGE-4 AT ROW 2.25 COL 41.57 WIDGET-ID 42
     IMAGE-5 AT ROW 3.25 COL 36.57 WIDGET-ID 44
     IMAGE-6 AT ROW 3.25 COL 41.57 WIDGET-ID 46
     IMAGE-7 AT ROW 4.25 COL 36.57 WIDGET-ID 52
     IMAGE-8 AT ROW 4.25 COL 41.57 WIDGET-ID 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 73.29 BY 10.63
         FONT 1.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.5 COL 5.14 HELP
          "Destino de Impress�o do Relat�rio" NO-LABEL
     bt-config-impr AT ROW 3.71 COL 45.14 HELP
          "Configura��o da impressora"
     bt-arquivo AT ROW 3.71 COL 45.14 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 3.75 COL 5.14 HELP
          "Nome do arquivo de destino do relat�rio" NO-LABEL
     rs-execucao AT ROW 5.88 COL 4.86 HELP
          "Modo de Execu��o" NO-LABEL
     tb-exporta AT ROW 7.75 COL 5 WIDGET-ID 4
     c-arquivo-2 AT ROW 9 COL 5 HELP
          "Nome do arquivo de destino do relat�rio" NO-LABEL WIDGET-ID 10
     bt-arquivo-2 AT ROW 9 COL 45 HELP
          "Escolha do nome do arquivo" WIDGET-ID 8
     text-destino AT ROW 1.75 COL 5.72 NO-LABEL
     text-modo AT ROW 5.13 COL 3.14 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 2.04 COL 4
     RECT-9 AT ROW 5.42 COL 4
     RECT-22 AT ROW 7.5 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 68.29 BY 10.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Relat�rio Suprimentos"
         HEIGHT             = 14.92
         WIDTH              = 81.57
         MAX-HEIGHT         = 30.13
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 30.13
         VIRTUAL-WIDTH      = 182.86
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
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-imp
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu��o".

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Relat�rio Suprimentos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Relat�rio Suprimentos */
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


&Scoped-define SELF-NAME bt-arquivo-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-2 C-Win
ON CHOOSE OF bt-arquivo-2 IN FRAME f-pg-imp
DO:
    /*****************************************************************
    **
    ** I-RPARQ - Choose of bt-Arquivo no template de relat�rio
    **
    *****************************************************************/
    
        def var c-arq-conv2  as char no-undo.
    
        /* tech1139 - FO 1223.694  - 02/11/2005 */
        assign c-arq-conv2 = replace(input frame f-pg-imp c-arquivo-2, "/", "\").
        /* tech1139 - FO 1223.694  - 02/11/2005 */
    
        
    /*tech14178 modificado para apresentar dialog com extens�o PDF quando o mesmo estiver sendo usado */
    &IF "{&PDF}" = "YES" &THEN /*tech868*/
        
        IF NOT usePDF() THEN
    
    &ENDIF
        
            SYSTEM-DIALOG GET-FILE c-arq-conv2
               FILTERS "*.xls" "*.xls",
                       "*.xlsx" "*.xlsx",
                       "*.*" "*.*"
               ASK-OVERWRITE 
               DEFAULT-EXTENSION "xls,xlsx"
               INITIAL-DIR "c:\temp" 
               SAVE-AS
               USE-FILENAME
               UPDATE l-ok.
    
    &IF "{&PDF}" = "YES" &THEN /*tech868*/
       ELSE
           SYSTEM-DIALOG GET-FILE c-arq-conv2
              FILTERS "*.pdf" "*.pdf",
                      "*.*" "*.*"
              ASK-OVERWRITE 
              DEFAULT-EXTENSION "pdf"
              INITIAL-DIR "spool" 
              SAVE-AS
              USE-FILENAME
              UPDATE l-ok.
    
    &endif
    
    
        if  l-ok = yes then do:
            /* tech1139 - FO 1223.694  - 02/11/2005 */
            assign c-arquivo-2 = replace(c-arq-conv2, CHR(92), "/"). 
            /* tech1139 - FO 1223.694  - 02/11/2005 */
            display c-arquivo-2 with frame f-pg-imp.
        end.
    
    /* i-rparq */
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


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME cod-estabel-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cod-estabel-fim C-Win
ON LEAVE OF cod-estabel-fim IN FRAME f-pg-sel /* Estabelecimento */
DO:
/*   {cdp\cd9998.i}  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dt-emissao-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dt-emissao-fim C-Win
ON LEAVE OF dt-emissao-fim IN FRAME f-pg-sel /* Data Emiss�o */
DO:
/*   {cdp\cd9998.i}  */
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


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel C-Win
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
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


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao C-Win
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   {include/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb-exporta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb-exporta C-Win
ON VALUE-CHANGED OF tb-exporta IN FRAME f-pg-imp /* Exportar XLS em Arquivo */
DO:
   IF INPUT tb-exporta THEN
       ASSIGN c-arquivo-2:SENSITIVE  = YES
              bt-arquivo-2:SENSITIVE = YES
              c-arquivo-2:SCREEN-VALUE = SESSION:TEMP-DIRECTORY + "ESMRI0001.XLS".
   ELSE
       ASSIGN c-arquivo-2:SENSITIVE  = NO
              bt-arquivo-2:SENSITIVE = NO
              c-arquivo-2:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESMRI0001" "12.6.00.000"}

/* inicializa��es do template de relat�rio */
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

c-arquivo-2 = SESSION:TEMP-DIRECTORY + "ESMRI0001.xls".

MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.

    {include/i-rpmbl.i im-pg-sel}    
  
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
       WAIT-FOR CLOSE OF THIS-PROCEDURE.

END.

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
  ENABLE im-pg-sel im-pg-imp bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY cod-estabel-ini cod-estabel-fim serie-ini serie-fim nr-nota-fis-ini 
          nr-nota-fis-fim cod-emitente-ini cod-emitente-fim dt-emissao-ini 
          dt-emissao-fim 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-9 IMAGE-10 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 
         IMAGE-7 IMAGE-8 cod-estabel-ini cod-estabel-fim serie-ini serie-fim 
         nr-nota-fis-ini nr-nota-fis-fim cod-emitente-ini cod-emitente-fim 
         dt-emissao-ini dt-emissao-fim 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao tb-exporta c-arquivo-2 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  ENABLE RECT-7 RECT-9 RECT-22 rs-destino bt-config-impr bt-arquivo c-arquivo 
         rs-execucao tb-exporta c-arquivo-2 bt-arquivo-2 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar C-Win 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

do on error undo, return error
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
          
   create tt-param.
   assign tt-param.usuario   = c-seg-usuario
          tt-param.destino   = input frame f-pg-imp rs-destino
          tt-param.data-exec = today
          tt-param.hora-exec = time.
              
   if  tt-param.destino = 1 then
       assign tt-param.arquivo = "".
   else
   if  tt-param.destino = 2 then 
       assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
   else
       assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

   ASSIGN tt-param.cod-estabel-ini   = input frame f-pg-sel cod-estabel-ini
          tt-param.cod-estabel-fim   = input frame f-pg-sel cod-estabel-fim
          tt-param.serie-ini         = input frame f-pg-sel serie-ini
          tt-param.serie-fim         = input frame f-pg-sel serie-fim
          tt-param.nr-nota-fis-ini   = input frame f-pg-sel nr-nota-fis-ini
          tt-param.nr-nota-fis-fim   = input frame f-pg-sel nr-nota-fis-fim
          tt-param.cod-emitente-ini  = input frame f-pg-sel cod-emitente-ini 
          tt-param.cod-emitente-fim  = input frame f-pg-sel cod-emitente-fim
          tt-param.dt-emissao-ini    = input frame f-pg-sel dt-emissao-ini
          tt-param.dt-emissao-fim    = input frame f-pg-sel dt-emissao-fim
          tt-param.arquivo-excel     = input frame f-pg-imp c-arquivo-2. 
                                           
   {include/i-rpexb.i}

   if  session:set-wait-state("general") then.

   {include/i-rprun.i esp/ESMRI0001rp.p}

   {include/i-rpexc.i}

   if  session:set-wait-state("") then.

   IF tt-param.arquivo-excel = "" THEN DO:
       {include/i-rptrm.i} 
   END.
   
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina C-Win 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P�gina (folder)   
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this Window, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-retornaDescItem C-Win 
FUNCTION fn-retornaDescItem RETURNS CHARACTER
  (INPUT p-c-it-codigo AS CHAR):

  DEFINE VARIABLE c-fn-desc-item AS CHAR format "x(60)" NO-UNDO.

  ASSIGN c-fn-desc-item = "".
  FOR FIRST ITEM NO-LOCK
      WHERE item.it-codigo = p-c-it-codigo:         

      ASSIGN c-fn-desc-item = ITEM.desc-item.

  END. /* FOR FIRST item USE-INDEX it-codigo NO-LOCK */

  RETURN c-fn-desc-item.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

