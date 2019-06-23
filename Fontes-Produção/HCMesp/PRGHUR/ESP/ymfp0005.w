&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YMFP0005 1.02.00.003 } /*** 010003 ***/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp

/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */
{prghur/esp/ymfp0005tt.i}

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-rtf              as char    no-undo.

def var v_des_arq          as char    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-classif 
&Scoped-Define DISPLAYED-OBJECTS rs-classif 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Estabelecimento\Matr°cula", 1,
"Por Estabelecimento\Nome", 2,
"Por Estabelecimento\Conta Corrente", 3
     SIZE 32 BY 3.5 NO-UNDO.

DEFINE BUTTON bt-arquivo 
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

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE text-parametro AS CHARACTER FORMAT "X(256)":U INITIAL "ParÉmetros de Impress∆o" 
      VIEW-AS TEXT 
     SIZE 27 BY .63 NO-UNDO.

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

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.79.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE RECTANGLE RECT-98
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE tb-parametro AS LOGICAL INITIAL no 
     LABEL "Imprimir P†gina de ParÉmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .83 NO-UNDO.

DEFINE BUTTON bt-arquivo-entrada 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-tipo-folha AS CHARACTER FORMAT "X(256)":U INITIAL "Tipo C†lculo" 
      VIEW-AS TEXT 
     SIZE 11.43 BY .5 NO-UNDO.

DEFINE VARIABLE text-entrada AS CHARACTER FORMAT "X(256)":U INITIAL "Arquivo L°quidos" 
      VIEW-AS TEXT 
     SIZE 17.29 BY .58 NO-UNDO.

DEFINE VARIABLE v_cdn_agenc_emp AS INTEGER FORMAT "zzzz9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 6.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_banco_emp AS INTEGER FORMAT "zz9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_central AS INTEGER FORMAT "zz9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE v_cod_convenio AS CHARACTER FORMAT "X(20)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 20.14 BY .88 NO-UNDO.

DEFINE VARIABLE v_cod_docto AS CHARACTER FORMAT "X(20)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE v_dat_lancto AS DATE FORMAT "99/99/9999":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_ano_refer AS INTEGER FORMAT "9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.57 BY .88.

DEFINE VARIABLE v_num_conta_emp AS INTEGER FORMAT "9999999":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_dig_agenc AS CHARACTER FORMAT "xx":U 
     VIEW-AS FILL-IN 
     SIZE 2.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_dig_conta AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 2.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_mes_refer AS INTEGER FORMAT "99" INITIAL 0 
     LABEL "":R21 
     VIEW-AS FILL-IN 
     SIZE 3.57 BY .88.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 5.25.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.38.

DEFINE VARIABLE t-13 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .83 NO-UNDO.

DEFINE VARIABLE t-adiant-13 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE t-adiant-normal AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .83 NO-UNDO.

DEFINE VARIABLE t-ferias AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.86 BY .83 NO-UNDO.

DEFINE VARIABLE t-normal AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE t-rescisao AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE v_cdn_banco_fim AS INTEGER FORMAT "zz9":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_banco_ini AS INTEGER FORMAT "zz9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_fim AS INTEGER FORMAT "zz9":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_ini AS INTEGER FORMAT "zz9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE v_dat_outros_pagtos AS DATE FORMAT "99/99/9999":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE v_dat_pagto_fer_rescis_fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE v_dat_pagto_fer_rescis_ini AS DATE FORMAT "99/99/9999":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
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

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-layout 
     LABEL "Layout" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-cla
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

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

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 11.38
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
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-layout AT ROW 14.54 COL 25
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     im-pg-cla AT ROW 1.5 COL 17.86
     im-pg-imp AT ROW 1.5 COL 49.29
     im-pg-par AT ROW 1.5 COL 33.57
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 3.75 COL 20 HELP
          "Classificaá∆o para emiss∆o do relat¢rio" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.58 COL 19.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.67 COL 59.29 HELP
          "Configuraá∆o da impressora"
     c-arquivo AT ROW 3.71 COL 19.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.75 COL 59 HELP
          "Escolha do nome do arquivo"
     rs-execucao AT ROW 6.04 COL 18.86 HELP
          "Modo de Execuá∆o" NO-LABEL
     tb-parametro AT ROW 8.38 COL 30
     text-destino AT ROW 2 COL 19.86 NO-LABEL
     text-modo AT ROW 5.29 COL 17.14 COLON-ALIGNED NO-LABEL
     text-parametro AT ROW 7.58 COL 20 NO-LABEL
     RECT-7 AT ROW 2.29 COL 18.14
     RECT-9 AT ROW 5.5 COL 18
     RECT-98 AT ROW 7.88 COL 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.5.

DEFINE FRAME f-pg-sel
     v_cdn_estab_ini AT ROW 4.5 COL 22 COLON-ALIGNED
     v_cdn_estab_fim AT ROW 4.5 COL 42.86 COLON-ALIGNED NO-LABEL
     v_cdn_banco_ini AT ROW 5.5 COL 22 COLON-ALIGNED
     v_cdn_banco_fim AT ROW 5.5 COL 42.86 COLON-ALIGNED NO-LABEL
     v_dat_pagto_fer_rescis_ini AT ROW 6.5 COL 22 COLON-ALIGNED
     v_dat_pagto_fer_rescis_fim AT ROW 6.5 COL 42.86 COLON-ALIGNED NO-LABEL
     v_dat_outros_pagtos AT ROW 7.46 COL 22 COLON-ALIGNED
     IMAGE-1 AT ROW 4.5 COL 37.86
     IMAGE-2 AT ROW 4.5 COL 42
     IMAGE-3 AT ROW 5.5 COL 37.86
     IMAGE-4 AT ROW 5.5 COL 42
     IMAGE-5 AT ROW 6.5 COL 37.86
     IMAGE-6 AT ROW 6.5 COL 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 75.43 BY 10.71.

DEFINE FRAME f-pg-par
     c-arquivo-entrada AT ROW 1.75 COL 21 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-arquivo-entrada AT ROW 1.67 COL 61.14 HELP
          "Escolha do nome do arquivo"
     v_cdn_estab_central AT ROW 3.5 COL 19 COLON-ALIGNED
     v_dat_lancto AT ROW 4.5 COL 19 COLON-ALIGNED
     v_num_mes_refer AT ROW 5.5 COL 19 COLON-ALIGNED
     v_num_ano_refer AT ROW 5.5 COL 23 COLON-ALIGNED NO-LABEL
     v_cod_convenio AT ROW 6.5 COL 19 COLON-ALIGNED
     v_cdn_banco_emp AT ROW 7.5 COL 19 COLON-ALIGNED
     v_cdn_agenc_emp AT ROW 8.5 COL 19 COLON-ALIGNED
     v_num_dig_agenc AT ROW 8.5 COL 25.86 COLON-ALIGNED NO-LABEL
     v_num_conta_emp AT ROW 9.5 COL 19 COLON-ALIGNED
     v_num_dig_conta AT ROW 9.5 COL 28.86 COLON-ALIGNED NO-LABEL
     v_cod_docto AT ROW 10.5 COL 19 COLON-ALIGNED
     text-entrada AT ROW 1.13 COL 21.72 NO-LABEL
     t-normal AT ROW 4.25 COL 46 WIDGET-ID 14
     t-adiant-normal AT ROW 5 COL 46 WIDGET-ID 10
     t-13 AT ROW 5.75 COL 46 WIDGET-ID 6
     t-adiant-13 AT ROW 6.5 COL 46 WIDGET-ID 8
     t-rescisao AT ROW 7.25 COL 46 WIDGET-ID 18
     t-ferias AT ROW 8 COL 46 WIDGET-ID 12
     fi-tipo-folha AT ROW 3.5 COL 42.86 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     RECT-8 AT ROW 1.46 COL 19.43
     RECT-11 AT ROW 3.75 COL 44 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75.72 BY 10.58.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "<Title>"
         HEIGHT             = 15
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-relat 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-relat
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-cla
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execuá∆o".

/* SETTINGS FOR FILL-IN text-parametro IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-parametro:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "ParÉmetros de Impress∆o".

/* SETTINGS FOR FRAME f-pg-par
   Custom                                                               */
/* SETTINGS FOR FILL-IN fi-tipo-folha IN FRAME f-pg-par
   NO-ENABLE                                                            */
ASSIGN 
       fi-tipo-folha:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Tipo C†lculo".

/* SETTINGS FOR FILL-IN text-entrada IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-entrada:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Arquivo L°quidos".

/* SETTINGS FOR FILL-IN v_cod_convenio IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_cod_docto IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_num_ano_refer IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_num_dig_agenc IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_num_mes_refer IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
   Custom                                                               */
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
THEN w-relat:HIDDEN = no.

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
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON END-ERROR OF w-relat /* <Title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* <Title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-relat
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo w-relat
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME bt-arquivo-entrada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-entrada w-relat
ON CHOOSE OF bt-arquivo-entrada IN FRAME f-pg-par
DO:
    {include/i-imarq.i c-arquivo-entrada f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr w-relat
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-layout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-layout w-relat
ON CHOOSE OF bt-layout IN FRAME f-relat /* Layout */
DO:
  run prghur/esp/ymfp0005a.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-cla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-cla w-relat
ON MOUSE-SELECT-CLICK OF im-pg-cla IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp w-relat
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par w-relat
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-relat
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao w-relat
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   {include/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME t-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-13 w-relat
ON VALUE-CHANGED OF t-13 IN FRAME f-pg-par
DO:
  run pi-habilita-parc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-adiant-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-adiant-13 w-relat
ON VALUE-CHANGED OF t-adiant-13 IN FRAME f-pg-par
DO:
  run pi-habilita-parc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-adiant-normal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-adiant-normal w-relat
ON VALUE-CHANGED OF t-adiant-normal IN FRAME f-pg-par
DO:
  run pi-habilita-parc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-ferias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-ferias w-relat
ON VALUE-CHANGED OF t-ferias IN FRAME f-pg-par
DO:
  run pi-habilita-parc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-normal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-normal w-relat
ON VALUE-CHANGED OF t-normal IN FRAME f-pg-par
DO:
  run pi-habilita-parc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-rescisao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-rescisao w-relat
ON VALUE-CHANGED OF t-rescisao IN FRAME f-pg-par
DO:
  run pi-habilita-parc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_cdn_agenc_emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_agenc_emp w-relat
ON F5 OF v_cdn_agenc_emp IN FRAME f-pg-par
DO:
   assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="object/sopy/zoom/z02py002.w"
                     &campo=v_cdn_agenc_emp
                     &campozoom=cdn_agenc_bcia
                     &campo2=v_num_dig_agenc
                     &campozoom2=cod_digito_verfdor_agenc_bcia
                     &campo3=v_cdn_banco_emp
                     &campozoom3=cdn_banco
                     &parametros="run pi-seta-inicial in wh-pesquisa (input v_cdn_empres_usuar,
                                                                      input frame f-pg-par v_cdn_banco_emp )".
                     &frame=f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_agenc_emp w-relat
ON LEAVE OF v_cdn_agenc_emp IN FRAME f-pg-par
DO:
  find first rh_agenc_bcia no-lock where
             rh_agenc_bcia.cdn_banco      = input frame f-pg-par v_cdn_banco_emp and
             rh_agenc_bcia.cdn_agenc_bcia = input frame f-pg-par v_cdn_agenc_emp no-error.
  if avail rh_agenc_bcia then
     assign v_num_dig_agenc:screen-value in frame f-pg-par = rh_agenc_bcia.cod_digito_verfdor_agenc_bcia.
  else
     assign v_num_dig_agenc:screen-value in frame f-pg-par = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_agenc_emp w-relat
ON MOUSE-SELECT-DBLCLICK OF v_cdn_agenc_emp IN FRAME f-pg-par
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_cdn_banco_emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_banco_emp w-relat
ON F5 OF v_cdn_banco_emp IN FRAME f-pg-par
DO:
  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="object/sopy/zoom/z01py019.w"
                     &campo=v_cdn_banco_emp
                     &campozoom=cdn_banco
                     &frame=f-pg-par} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_banco_emp w-relat
ON MOUSE-SELECT-DBLCLICK OF v_cdn_banco_emp IN FRAME f-pg-par
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_cdn_estab_central
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_estab_central w-relat
ON F5 OF v_cdn_estab_central IN FRAME f-pg-par
DO:
   assign l-implanta = yes.
   {include/zoomvar.i &prog-zoom="object/sopy/zoom/z01py060.w"
                      &campo=v_cdn_estab_central
                      &campozoom=cdn_estab
                      &frame=f-pg-par}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_estab_central w-relat
ON MOUSE-SELECT-DBLCLICK OF v_cdn_estab_central IN FRAME f-pg-par
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_dat_lancto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_dat_lancto w-relat
ON LEAVE OF v_dat_lancto IN FRAME f-pg-par
DO:
   if t-normal:checked in frame f-pg-par or t-adiant-normal:checked in frame f-pg-par or
      t-13:checked in frame f-pg-par     or t-adiant-13:checked in frame f-pg-par      then
      FIND FIRST param_empres_rh WHERE param_empres_rh.cdn_empresa = v_cdn_empres_usuar NO-LOCK NO-ERROR.
      IF AVAIL param_empres_rh THEN 
          assign v_num_mes_refer:screen-value in frame f-pg-par = string(param_empres_rh.num_mes_refer_calc_efetd)
                 v_num_ano_refer:screen-value in frame f-pg-par = string(param_empres_rh.num_ano_refer_calc_efetd).
      ELSE 
          assign v_num_mes_refer:screen-value in frame f-pg-par = "0"
                 v_num_ano_refer:screen-value in frame f-pg-par = "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "YMFP0005" "1.00.00.000"}

/*:T inicializaá‰es do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

v_cdn_estab_central:load-mouse-pointer("image/lupa.cur") in frame f-pg-par.
v_cdn_banco_emp:load-mouse-pointer("image/lupa.cur") in frame f-pg-par.
v_cdn_agenc_emp:load-mouse-pointer("image/lupa.cur") in frame f-pg-par.

{utp/ut-liter.i Estabelecimento MFP R}
assign v_cdn_estab_ini:label in frame f-pg-sel = trim(return-value).

{utp/ut-liter.i Banco MFP R}
assign v_cdn_banco_ini:label in frame f-pg-sel = trim(return-value).

{utp/ut-liter.i Data_Pagto_FÇrias\Rescis MFP R}
assign v_dat_pagto_fer_rescis_ini:label in frame f-pg-sel = trim(return-value).

{utp/ut-liter.i Data_Outros_Pagtos}
assign v_dat_outros_pagtos:label in frame f-pg-sel = trim(return-value).

{utp/ut-liter.i Adiantamento_Normal MFP R}
assign t-adiant-normal:label in frame f-pg-par = trim(return-value).

{utp/ut-liter.i Normal MFP R}
assign t-normal:label in frame f-pg-par = trim(return-value).

{utp/ut-liter.i Adiantamento_13_Sal†rio MFP R}
assign t-adiant-13:label in frame f-pg-par = trim(return-value).

{utp/ut-liter.i 13_Sal†rio MFP R}
assign t-13:label in frame f-pg-par = trim(return-value).

/*{utp/ut-liter.i Pens∆o MFP R}
assign t-pensao:label in frame f-pg-par = trim(return-value).*/

{utp/ut-liter.i FÇrias MFP R}
assign t-ferias:label in frame f-pg-par = trim(return-value).

{utp/ut-liter.i Rescis∆o MFP R}
assign t-rescisao:label in frame f-pg-par = trim(return-value).

{utp/ut-liter.i Banco_Empresa MFP R}
assign v_cdn_banco_emp:label in frame f-pg-par = trim(return-value).

{utp/ut-liter.i Data_Lanáamento MFP R}
assign v_dat_lancto:label in frame f-pg-par = trim(return-value).

{utp/ut-liter.i Màs/Ano_Referància MFP R}
assign v_num_mes_refer:label in frame f-pg-par = trim(return-value).

{utp/ut-liter.i Convànio MFP R}
assign v_cod_convenio:label in frame f-pg-par = trim(return-value).

{utp/ut-liter.i Agància_Empresa MFP R}
assign v_cdn_agenc_emp:label in frame f-pg-par = trim(return-value).

{utp/ut-liter.i Conta_Corrente_Empresa MFP R}
assign v_num_conta_emp:label in frame f-pg-par = trim(return-value).

{utp/ut-liter.i Documento MFP R}
assign v_cod_docto:label in frame f-pg-par = trim(return-value).

{utp/ut-liter.i Estabelecimento_Matriz MFP R}
assign v_cdn_estab_central:label in frame f-pg-par = trim(return-value).

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.
    
    find first param_empres_rh no-lock where
               param_empres_rh.cdn_empresa = v_cdn_empres_usuar no-error.
    if avail param_empres_rh then do:

       assign v_dat_pagto_fer_rescis_ini:screen-value in frame f-pg-sel = string(date(param_empres_rh.num_mes_refer_calc_efetd,01,param_empres_rh.num_ano_refer_calc_efetd)).
       run prghur/fpp/fpapi002.p (input param_empres_rh.num_mes_refer_calc_efetd, input param_empres_rh.num_ano_refer_calc_efetd, input-output v_dat_pagto_fer_rescis_fim).
       assign v_dat_pagto_fer_rescis_fim:screen-value in frame f-pg-sel = string(v_dat_pagto_fer_rescis_fim).

       ASSIGN v_dat_outros_pagtos = ?
              v_dat_lancto        = ?.
       FOR FIRST habilit_calc_fp NO-LOCK WHERE 
           habilit_calc_fp.cdn_empresa              = param_empres_rh.cdn_empresa              and
           habilit_calc_fp.num_mes_refer_fp_calcula = param_empres_rh.num_mes_refer_calc_efetd AND 
           habilit_calc_fp.num_ano_refer_fp_calcula = param_empres_rh.num_ano_refer_calc_efetd
           BY habilit_calc_fp.cdn_empresa
           BY habilit_calc_fp.cdn_estab
           BY habilit_calc_fp.cdn_categ_sal
           BY habilit_calc_fp.num_ano_refer_fp_calcula DESCENDING
           BY habilit_calc_fp.num_mes_refer_fp_calcula DESCENDING
           BY habilit_calc_fp.num_seq_calc_fp DESCENDING:
           assign v_dat_outros_pagtos:screen-value in frame f-pg-sel = string(habilit_calc_fp.dat_pagto_salario)
                  v_dat_outros_pagtos = habilit_calc_fp.dat_pagto_salario
                  v_dat_lancto:screen-value in frame f-pg-par = string(habilit_calc_fp.dat_pagto_salario)
                  v_dat_lancto = habilit_calc_fp.dat_pagto_salario.
       END.
       IF v_dat_outros_pagtos = ? THEN DO:
           run prghur/fpp/fpapi002.p (input param_empres_rh.num_mes_refer_calc_efetd, input param_empres_rh.num_ano_refer_calc_efetd, input-output v_dat_outros_pagtos). 
           run prghur/fpp/fpapi002.p (input param_empres_rh.num_mes_refer_calc_efetd, input param_empres_rh.num_ano_refer_calc_efetd, input-output v_dat_lancto). 
           assign v_dat_outros_pagtos:screen-value in frame f-pg-sel = string(v_dat_outros_pagtos)
                  v_dat_lancto:screen-value in frame f-pg-par = string(v_dat_lancto).
       END.

       /*assign v_num_mes_refer:screen-value in frame f-pg-par = string(month(v_dat_lancto))
              v_num_ano_refer:screen-value in frame f-pg-par = string(year(v_dat_lancto)).*/
       
       assign v_des_arq = "liq" + string(param_empres_rh.num_mes_refer_calc_efetd,"99") +
                                  substring(string(param_empres_rh.num_ano_refer_calc_efetd,"9999"),03,02) + ".lst".
    end.
    
    {prghur/fpp/fp9200.i16 c-arquivo-entrada f-pg-par v_des_arq}
    
    {include/i-rpmbl.i}
  
    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-relat  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-relat  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-relat  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
  THEN DELETE WIDGET w-relat.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-relat  _DEFAULT-ENABLE
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
  ENABLE im-pg-cla im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar 
         bt-layout bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY v_cdn_estab_ini v_cdn_estab_fim v_cdn_banco_ini v_cdn_banco_fim 
          v_dat_pagto_fer_rescis_ini v_dat_pagto_fer_rescis_fim 
          v_dat_outros_pagtos 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE v_cdn_estab_ini v_cdn_estab_fim v_cdn_banco_ini v_cdn_banco_fim 
         v_dat_pagto_fer_rescis_ini v_dat_pagto_fer_rescis_fim 
         v_dat_outros_pagtos IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao tb-parametro 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 RECT-98 rs-destino bt-config-impr c-arquivo bt-arquivo 
         rs-execucao tb-parametro 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY c-arquivo-entrada v_cdn_estab_central v_dat_lancto v_num_mes_refer 
          v_num_ano_refer v_cod_convenio v_cdn_banco_emp v_cdn_agenc_emp 
          v_num_dig_agenc v_num_conta_emp v_num_dig_conta v_cod_docto t-normal 
          t-adiant-normal t-13 t-adiant-13 t-rescisao t-ferias fi-tipo-folha 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE c-arquivo-entrada bt-arquivo-entrada v_cdn_estab_central v_dat_lancto 
         v_cdn_banco_emp v_cdn_agenc_emp v_num_conta_emp v_num_dig_conta 
         t-normal t-adiant-normal t-13 t-adiant-13 t-rescisao t-ferias RECT-8 
         RECT-11 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  VIEW w-relat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-relat 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-relat 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var r-tt-digita as rowid no-undo.

do on error undo, return error on stop  undo, return error:
    {include/i-rpexa.i}

    if input frame f-pg-imp rs-destino = 2 and input frame f-pg-imp rs-execucao = 1 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show":U, input 73, input "").
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.

    /*:T Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p†gina com 
       problemas e colocar o focus no campo com problemas */
    
    {prghur/fpp/fp9200.i17 c-arquivo-entrada f-pg-par im-pg-par f-relat}

    if input frame f-pg-par v_cdn_estab_central < 1 then do:
       {utp/ut-liter.i Estabelecimento_Matriz MFP C}
        run utp/ut-msgs.p (input "show", input 54, input return-value).                               
        apply 'mouse-select-click' to im-pg-par in frame f-relat.
        apply 'entry' to v_cdn_estab_central in frame f-pg-par.                
        return error.
    end.

    find rh_estab where 
         rh_estab.cdn_empresa = v_cdn_empres_usuar and 
         rh_estab.cdn_estab = input frame f-pg-par v_cdn_estab_central no-lock no-error.
    if not available rh_estab then do:   
       {utp/ut-liter.i Estabelecimento_Matriz MFP C}
        run utp/ut-msgs.p (input "show", input 47, input return-value).                               
        apply 'mouse-select-click' to im-pg-par in frame f-relat.
        apply 'entry' to v_cdn_estab_central in frame f-pg-par.                
        return error.
    end.

    if input frame f-pg-par v_dat_lancto = ? then do:
       {utp/ut-liter.i Data_do_CrÇdito MFP C}
        run utp/ut-msgs.p (input "show", input 54, input return-value).                               
        apply 'mouse-select-click' to im-pg-par in frame f-relat.
        apply 'entry' to v_dat_lancto in frame f-pg-par.                
        return error.
    end.

    if weekday(input frame f-pg-par v_dat_lancto) = 7 then do:
       {utp/ut-liter.i S†bado * C}
       run utp/ut-msgs.p (input "show", input 3141, input return-value).                               
       apply 'mouse-select-click' to im-pg-par in frame f-relat.
       apply 'entry' to v_dat_lancto in frame f-pg-par.                
       return error.
    end.

    if weekday(input frame f-pg-par v_dat_lancto) = 1 then do:
       {utp/ut-liter.i Domingo * C}
       run utp/ut-msgs.p (input "show", input 3141, input return-value).                               
       apply 'mouse-select-click' to im-pg-par in frame f-relat.
       apply 'entry' to v_dat_lancto in frame f-pg-par.
       return error.
    end.

    if input frame f-pg-par v_cdn_banco_emp = 0 then do:
       {utp/ut-liter.i Banco_da_Empresa MFP C}
       run utp/ut-msgs.p (input "show", input 54, input return-value).                               
       apply 'mouse-select-click' to im-pg-par in frame f-relat.
       apply 'entry' to v_cdn_banco_emp in frame f-pg-par.                
       return error.
    end.

    find first rh_bco no-lock where
               rh_bco.cdn_banco = input frame f-pg-par v_cdn_banco_emp no-error.
    if not avail rh_bco then do:
       {utp/ut-liter.i Banco_da_Empresa MFP C}
       run utp/ut-msgs.p (input "show", input 47, input return-value).                               
       apply 'mouse-select-click' to im-pg-par in frame f-relat.
       apply 'entry' to v_cdn_banco_emp in frame f-pg-par.                
       return error.
    end.

    if input frame f-pg-par v_cdn_agenc_emp = 0 then do:
       {utp/ut-liter.i Agància_da_Empresa MFP C}
       run utp/ut-msgs.p (input "show", input 54, input return-value).                               
       apply 'mouse-select-click' to im-pg-par in frame f-relat.
       apply 'entry' to v_cdn_agenc_emp in frame f-pg-par.                
       return error.
    end.

    find first rh_agenc_bcia no-lock where
               rh_agenc_bcia.cdn_banco = input frame f-pg-par v_cdn_banco_emp and
               rh_agenc_bcia.cdn_agenc_bcia = input frame f-pg-par v_cdn_agenc_emp no-error.
    if not avail rh_agenc_bcia then do:
       {utp/ut-liter.i Agància_da_Empresa MFP C}
       run utp/ut-msgs.p (input "show", input 47, input return-value).                               
       apply 'mouse-select-click' to im-pg-par in frame f-relat.
       apply 'entry' to v_cdn_agenc_emp in frame f-pg-par.                
       return error.
    end.

    if input frame f-pg-par v_num_conta_emp = 0 then do:
       {utp/ut-liter.i Conta_da_Empresa MFP C}
        run utp/ut-msgs.p (input "show", input 54, input return-value).                               
        apply 'mouse-select-click' to im-pg-par in frame f-relat.
        apply 'entry' to v_num_conta_emp in frame f-pg-par.                
        return error.
    end.

    /*:T Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */
    
    assign v_des_arq = c-arquivo-entrada:screen-value in frame f-pg-par.

    create tt-param.
    assign tt-param.parametro          = if input frame f-pg-imp tb-parametro = "yes" then yes else no
           tt-param.usuario            = c-seg-usuario
           tt-param.destino            = input frame f-pg-imp rs-destino
           tt-param.data-exec          = today
           tt-param.hora-exec          = time
           tt-param.classifica         = input frame f-pg-cla rs-classif
           tt-param.desc-classifica    = entry((tt-param.classifica - 1) * 2 + 1, 
                                                rs-classif:radio-buttons in frame f-pg-cla)
           tt-param.v_cdn_empres_usuar = v_cdn_empres_usuar
           tt-param.cdn_estab_ini      = input frame f-pg-sel v_cdn_estab_ini
           tt-param.cdn_estab_fim      = input frame f-pg-sel v_cdn_estab_fim
           tt-param.cdn_banco_ini      = input frame f-pg-sel v_cdn_banco_ini
           tt-param.cdn_banco_fim      = input frame f-pg-sel v_cdn_banco_fim
           tt-param.dat_pagto_ini      = input frame f-pg-sel v_dat_pagto_fer_rescis_ini
           tt-param.dat_pagto_fim      = input frame f-pg-sel v_dat_pagto_fer_rescis_fim
           tt-param.dat_outros_pagtos  = input frame f-pg-sel v_dat_outros_pagtos
           tt-param.log_normal         = input frame f-pg-par t-normal
           tt-param.log_adiant_normal  = input frame f-pg-par t-adiant-normal
           tt-param.log_13             = input frame f-pg-par t-13
           tt-param.log_adiant_13      = input frame f-pg-par t-adiant-13
           tt-param.log_ferias         = input frame f-pg-par t-ferias
           /*tt-param.log_pensao         = input frame f-pg-par t-pensao*/
           tt-param.log_rescisao       = input frame f-pg-par t-rescisao
           tt-param.c-arquivo-pensao   = v_des_arq
           tt-param.cdn_banco_emp      = input frame f-pg-par v_cdn_banco_emp
           tt-param.num_mes_refer      = input frame f-pg-par v_num_mes_refer
           tt-param.num_ano_refer      = input frame f-pg-par v_num_ano_refer
           tt-param.cdn_agenc_emp      = input frame f-pg-par v_cdn_agenc_emp
           tt-param.num_dig_agenc      = v_num_dig_agenc:screen-value in frame f-pg-par
           tt-param.num_conta_emp      = input frame f-pg-par v_num_conta_emp
           tt-param.num_dig_conta      = v_num_dig_conta:screen-value in frame f-pg-par
           tt-param.dat_lancto         = input frame f-pg-par v_dat_lancto
           tt-param.cod_convenio       = input frame f-pg-par v_cod_convenio
           tt-param.cod_docto          = v_cod_docto:screen-value in frame f-pg-par
           tt-param.cdn_estab_central  = input frame f-pg-par v_cdn_estab_central.
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 OR tt-param.destino = 4 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l¢gica de gravaá∆o dos demais campos que devem ser passados
       como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */
    
    
    
    /*:T Executar do programa RP.P que ir† criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i prghur/esp/ymfp0005rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita-parc w-relat 
PROCEDURE pi-habilita-parc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   if t-normal:checked in frame f-pg-par or t-adiant-normal:checked in frame f-pg-par or
      t-13:checked in frame f-pg-par     or t-adiant-13:checked in frame f-pg-par     /*or
      t-pensao:checked in frame f-pg-par*/ THEN DO:
      assign v_num_mes_refer:sensitive in frame f-pg-par = yes
             v_num_ano_refer:sensitive in frame f-pg-par = yes
             /*v_num_mes_refer:screen-value in frame f-pg-par = string(month(input frame f-pg-par v_dat_lancto))
             v_num_ano_refer:screen-value in frame f-pg-par = string(year(input frame f-pg-par v_dat_lancto))*/ .
      FIND FIRST param_empres_rh WHERE param_empres_rh.cdn_empresa = v_cdn_empres_usuar NO-LOCK NO-ERROR.
      IF AVAIL param_empres_rh THEN 
          assign v_num_mes_refer:screen-value in frame f-pg-par = string(param_empres_rh.num_mes_refer_calc_efetd)
                 v_num_ano_refer:screen-value in frame f-pg-par = string(param_empres_rh.num_ano_refer_calc_efetd).
      ELSE 
          assign v_num_mes_refer:screen-value in frame f-pg-par = "0"
                 v_num_ano_refer:screen-value in frame f-pg-par = "0".
   END.
   else do:
      assign v_num_mes_refer:screen-value in frame f-pg-par = "0"
             v_num_ano_refer:screen-value in frame f-pg-par = "0"
             v_num_mes_refer:sensitive in frame f-pg-par = no
             v_num_ano_refer:sensitive in frame f-pg-par = no.
   end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P†gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-rptrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-relat  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-relat, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-relat 
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

