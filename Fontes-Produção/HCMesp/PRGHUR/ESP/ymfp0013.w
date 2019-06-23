&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YMFP0013 1.02.00.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
/*     {include/i-license-manager.i ymfp0013 mfp} */
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp

&GLOBAL-DEFINE RTF   false
&undefine PGDIG
&undefine PGCLA
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

{prghur/esp/ymfp0013tt.i}

/* Transfer Definitions */

def var raw-param        as raw no-undo.

/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-rtf              as char    no-undo.
def var c-modelo-default   as char    no-undo.

def var v_diretorio_usuario as char no-undo.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est  rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

PROCEDURE ShellExecute{&A} EXTERNAL "shell32" :
     DEFINE INPUT PARAMETER HWND AS LONG.
     DEFINE INPUT PARAMETER lpOperation AS CHARACTER.
     DEFINE INPUT PARAMETER lpFile AS CHARACTER.
     DEFINE INPUT PARAMETER lpParameters AS CHARACTER.
     DEFINE INPUT PARAMETER lpDirectory AS CHARACTER.
     DEFINE INPUT PARAMETER nShowCmd AS LONG.
     DEFINE RETURN PARAMETER hInstance AS LONG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 RECT-98 RECT-7 rs-destino bt-arquivo ~
bt-config-impr c-arquivo rs-execucao tb-parametro text-destino 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao ~
tb-parametro text-destino 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 v_cdn_param_calc_ppr v_num_mes_refer_calc_efetd ~
v_num_ano_refer_calc_efetd v_cdn_individual_factor 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn_nome_arquivo w-relat 
FUNCTION fn_nome_arquivo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE text-parametro AS CHARACTER FORMAT "X(256)":U INITIAL "Parƒmetros de ImpressÆo" 
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
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE RECTANGLE RECT-98
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE tb-parametro AS LOGICAL INITIAL yes 
     LABEL "Imprimir P gina de Parƒmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .83 NO-UNDO.

DEFINE BUTTON bt-arquivo-entrada 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE v_arquivo_exportacao AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 44.72 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-entrada AS CHARACTER FORMAT "X(256)":U INITIAL "Arquivo Exporta‡Æo" 
      VIEW-AS TEXT 
     SIZE 19 BY .63 NO-UNDO.

DEFINE VARIABLE v_cdn_individual_factor AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "Individual Factor" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_param_calc_ppr AS INTEGER FORMAT "zzz9" INITIAL 0 
     LABEL "Parƒmetro Calculo PLR" 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .88 NO-UNDO.

DEFINE VARIABLE v_des_param AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_ano_refer_calc_efetd AS INTEGER FORMAT "9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_mes_refer_calc_efetd AS INTEGER FORMAT "99" INITIAL 0 
     LABEL "Mˆs Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .88 NO-UNDO.

DEFINE VARIABLE v_exporta AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "ProvisÆo PLR", 1,
"Pagamento PLR", 2
     SIZE 21 BY 2.25 NO-UNDO.

DEFINE VARIABLE v_tp_func AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ativos", 1,
"Desligados", 2,
"Ambos", 3
     SIZE 47 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 1.63.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 2.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 3.

DEFINE VARIABLE v_individual_factor AS LOGICAL INITIAL yes 
     LABEL "Individual Factor" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .83 NO-UNDO.

DEFINE VARIABLE v_cod_id_feder_fim AS CHARACTER FORMAT "x(20)" INITIAL "99999999999" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE v_cod_id_feder_ini AS CHARACTER FORMAT "x(20)" 
     LABEL "ID Federal" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
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
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     im-pg-imp AT ROW 1.5 COL 33.57
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar WIDGET-ID 100.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 3.17 COL 17 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL WIDGET-ID 10
     bt-arquivo AT ROW 4.33 COL 57.14 HELP
          "Escolha do Nome do arquivo" WIDGET-ID 26
     bt-config-impr AT ROW 4.33 COL 57.14 HELP
          "Configura‡Æo da impressora" WIDGET-ID 2
     c-arquivo AT ROW 4.38 COL 17 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL WIDGET-ID 4
     rs-execucao AT ROW 6.54 COL 17 HELP
          "Modo de Execu‡Æo" NO-LABEL WIDGET-ID 14
     tb-parametro AT ROW 9 COL 28 WIDGET-ID 18
     text-destino AT ROW 2.42 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     text-modo AT ROW 5.75 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     text-parametro AT ROW 8.21 COL 18 NO-LABEL WIDGET-ID 24
     RECT-9 AT ROW 6.08 COL 16 WIDGET-ID 6
     RECT-98 AT ROW 8.5 COL 16 WIDGET-ID 8
     RECT-7 AT ROW 2.71 COL 16 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.5 WIDGET-ID 100.

DEFINE FRAME f-pg-sel
     v_cod_id_feder_ini AT ROW 4.75 COL 11 COLON-ALIGNED HELP
          "C¢digo ID Federal" WIDGET-ID 2
     v_cod_id_feder_fim AT ROW 4.75 COL 51 HELP
          "C¢digo ID Federal" NO-LABEL WIDGET-ID 10
     IMAGE-1 AT ROW 4.75 COL 31.43
     IMAGE-2 AT ROW 4.75 COL 47.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62 WIDGET-ID 100.

DEFINE FRAME f-pg-par
     v_cdn_param_calc_ppr AT ROW 1.17 COL 24 COLON-ALIGNED WIDGET-ID 2
     v_des_param AT ROW 1.17 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 4 NO-TAB-STOP 
     v_num_mes_refer_calc_efetd AT ROW 2.17 COL 24 COLON-ALIGNED WIDGET-ID 8
     v_num_ano_refer_calc_efetd AT ROW 2.17 COL 27.72 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     v_exporta AT ROW 4 COL 18 NO-LABEL WIDGET-ID 14
     v_individual_factor AT ROW 4 COL 42 WIDGET-ID 18
     v_cdn_individual_factor AT ROW 5.25 COL 56 COLON-ALIGNED HELP
          "C¢digo" WIDGET-ID 48
     v_tp_func AT ROW 7.33 COL 16 NO-LABEL WIDGET-ID 40
     v_arquivo_exportacao AT ROW 9.79 COL 15.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL WIDGET-ID 34
     bt-arquivo-entrada AT ROW 9.79 COL 59.86 HELP
          "Escolha do nome do arquivo" WIDGET-ID 32
     text-entrada AT ROW 8.75 COL 16.14 NO-LABEL WIDGET-ID 38
     "Funcion rio" VIEW-AS TEXT
          SIZE 14 BY .67 AT ROW 6.54 COL 18 WIDGET-ID 46
     "Exportar" VIEW-AS TEXT
          SIZE 9 BY .67 AT ROW 3.17 COL 18 WIDGET-ID 12
     RECT-8 AT ROW 3.5 COL 14 WIDGET-ID 10
     RECT-11 AT ROW 9.08 COL 14 WIDGET-ID 36
     RECT-10 AT ROW 7 COL 14 WIDGET-ID 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.25 WIDGET-ID 100.


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
/* SETTINGS FOR FRAME f-pg-imp
   FRAME-NAME                                                           */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

/* SETTINGS FOR FILL-IN text-parametro IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-parametro:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Parƒmetros de ImpressÆo".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN text-entrada IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-entrada:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Arquivo Exporta‡Æo".

/* SETTINGS FOR FILL-IN v_cdn_individual_factor IN FRAME f-pg-par
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN v_cdn_param_calc_ppr IN FRAME f-pg-par
   1                                                                    */
/* SETTINGS FOR FILL-IN v_des_param IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_num_ano_refer_calc_efetd IN FRAME f-pg-par
   1                                                                    */
/* SETTINGS FOR FILL-IN v_num_mes_refer_calc_efetd IN FRAME f-pg-par
   1                                                                    */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN v_cod_id_feder_fim IN FRAME f-pg-sel
   ALIGN-L                                                              */
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
    def var c-arq-conv  as char no-undo.
    def var l-ok as logical no-undo.

    assign c-arq-conv = replace(input frame {&frame-name} v_arquivo_exportacao, "/", "~\").

    SYSTEM-DIALOG GET-FILE c-arq-conv
       FILTERS "Planilhas Excel" + chr(169) + " (*.xlsx,*.xls)" "*.xlsx,*.xls",
               "Todos os arquivos (*.*)" "*.*"         
       DEFAULT-EXTENSION "xlsx"
       USE-FILENAME
       ASK-OVERWRITE 
       save-as
       UPDATE l-ok.
    if  l-ok = yes then do:
        assign v_arquivo_exportacao = replace(c-arq-conv, "~\", "/").
        display v_arquivo_exportacao with frame {&frame-name}.
    end.  
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
&Scoped-define SELF-NAME v_cdn_individual_factor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_individual_factor w-relat
ON F5 OF v_cdn_individual_factor IN FRAME f-pg-par /* Individual Factor */
DO:
  &scoped-define frame-name f-pg-par
  {include/zoomvar.i &prog-zoom="object/sopy/zoom/z01py417.w"
                     &campo=v_cdn_param_calc_ppr
                     &campozoom=cdn_param_calc_pp
                     &campo2=v_des_param
                     &campozoom2=des_param_calc_pp}
  &undefine frame-name

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_individual_factor w-relat
ON MOUSE-SELECT-DBLCLICK OF v_cdn_individual_factor IN FRAME f-pg-par /* Individual Factor */
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_cdn_param_calc_ppr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_param_calc_ppr w-relat
ON F5 OF v_cdn_param_calc_ppr IN FRAME f-pg-par /* Parƒmetro Calculo PLR */
DO:
  &scoped-define frame-name f-pg-par
  {include/zoomvar.i &prog-zoom="object/sopy/zoom/z01py417.w"
                     &campo=v_cdn_param_calc_ppr
                     &campozoom=cdn_param_calc_pp
                     &campo2=v_des_param
                     &campozoom2=des_param_calc_pp}
  &undefine frame-name

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_param_calc_ppr w-relat
ON LEAVE OF v_cdn_param_calc_ppr IN FRAME f-pg-par /* Parƒmetro Calculo PLR */
DO:
&scoped-define frame-name f-pg-par
  find param_calc_ppr where 
       param_calc_ppr.cdn_param_calc_ppr = input frame {&frame-name} v_cdn_param_calc_ppr no-lock no-error.
  if avail param_calc_ppr then
      assign v_des_param:screen-value in frame {&frame-name} = param_calc_ppr.des_param_calc_ppr.
  else 
      assign v_des_param:screen-value in frame {&frame-name} = "".
&undefine frame-name
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_param_calc_ppr w-relat
ON MOUSE-SELECT-DBLCLICK OF v_cdn_param_calc_ppr IN FRAME f-pg-par /* Parƒmetro Calculo PLR */
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_exporta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_exporta w-relat
ON VALUE-CHANGED OF v_exporta IN FRAME f-pg-par
DO:
  
    assign v_arquivo_exportacao = fn_nome_arquivo().

    disp v_arquivo_exportacao with frame f-pg-par.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_individual_factor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_individual_factor w-relat
ON VALUE-CHANGED OF v_individual_factor IN FRAME f-pg-par /* Individual Factor */
DO:
  IF v_individual_factor:CHECKED THEN
      ASSIGN v_cdn_individual_factor = 0
             v_cdn_individual_factor:SENSITIVE IN FRAME f-pg-par = NO.
  ELSE
      ASSIGN v_cdn_individual_factor:SENSITIVE IN FRAME f-pg-par = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_num_ano_refer_calc_efetd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_num_ano_refer_calc_efetd w-relat
ON LEAVE OF v_num_ano_refer_calc_efetd IN FRAME f-pg-par
DO:
  apply "VALUE-CHANGED":U to v_exporta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_num_mes_refer_calc_efetd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_num_mes_refer_calc_efetd w-relat
ON LEAVE OF v_num_mes_refer_calc_efetd IN FRAME f-pg-par /* Mˆs Referˆncia */
DO:
  apply "VALUE-CHANGED":U to v_exporta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "YMFP0013" "1.02.00.000"}

/*:T inicializa‡äes do template de relat¢rio */
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

    find first param_empres_rh no-lock
        where param_empres_rh.cdn_empresa = v_cdn_empres_usuar no-error.

    assign v_num_ano_refer_calc_efetd = param_empres_rh.num_ano_refer_calc_efetd
           v_num_mes_refer_calc_efetd = param_empres_rh.num_mes_refer_calc_efetd.

    assign v_diretorio_usuario = replace(c-arquivo, "~/" + c-programa-mg97 + ".LST":U, "~/").

    assign rs-destino = 2
           v_arquivo_exportacao = fn_nome_arquivo().
  
    RUN enable_UI.
    
    {include/i-rpmbl.i}

    &scoped-define frame-name f-pg-par
    hide frame {&frame-name}.
    v_cdn_param_calc_ppr:load-mouse-pointer("image/lupa.cur") in frame {&frame-name}.    
    &undefine frame-name
    
    apply "MOUSE-SELECT-CLICK":U to im-pg-par in frame f-relat.    
    
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
  ENABLE im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY v_cod_id_feder_ini v_cod_id_feder_fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-2 v_cod_id_feder_ini v_cod_id_feder_fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao tb-parametro text-destino 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-9 RECT-98 RECT-7 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao tb-parametro text-destino 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY v_cdn_param_calc_ppr v_des_param v_num_mes_refer_calc_efetd 
          v_num_ano_refer_calc_efetd v_exporta v_individual_factor 
          v_cdn_individual_factor v_tp_func v_arquivo_exportacao 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-8 RECT-11 RECT-10 v_cdn_param_calc_ppr v_num_mes_refer_calc_efetd 
         v_num_ano_refer_calc_efetd v_exporta v_individual_factor v_tp_func 
         v_arquivo_exportacao bt-arquivo-entrada 
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
def var hInstance as int no-undo.

do on error undo, return error on stop  undo, return error:
    {include/i-rpexa.i}
    /*14/02/2005 - tech1007 - Alterada condicao para nÆo considerar mai o RTF como destino*/
    if input frame f-pg-imp rs-destino = 2 and
       input frame f-pg-imp rs-execucao = 1 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show":U, input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.

    /*14/02/2005 - tech1007 - Teste efetuado para nao permitir modelo em branco*/
    &IF "{&RTF}":U = "YES":U &THEN
    IF ( INPUT FRAME f-pg-imp c-modelo-rtf = "" AND
         INPUT FRAME f-pg-imp l-habilitaRtf = "Yes" ) OR
       ( SEARCH(INPUT FRAME f-pg-imp c-modelo-rtf) = ? AND
         input frame f-pg-imp rs-execucao = 1 AND
         INPUT FRAME f-pg-imp l-habilitaRtf = "Yes" )
         THEN DO:
        run utp/ut-msgs.p (input "show":U, input 73, input "").        
        apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
        /*30/12/2004 - tech1007 - Evento removido pois causa problemas no WebEnabler*/
        /*apply "CHOOSE":U to bt-modelo-rtf in frame f-pg-imp.*/
        return error.
    END.
    &endif
    /*Fim teste Modelo*/
    
    /*:T Coloque aqui as valida‡äes da p gina de Digita‡Æo, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/

    &IF defined(PGDIG) > 0 &THEN
    for each tt-digita no-lock:
        assign r-tt-digita = rowid(tt-digita).
        
        /*:T Valida‡Æo de duplicidade de registro na temp-table tt-digita */
        find first b-tt-digita where b-tt-digita.ordem = tt-digita.ordem and 
                                     rowid(b-tt-digita) <> rowid(tt-digita) no-lock no-error.
        if avail b-tt-digita then do:
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid rowid(b-tt-digita).
            
            run utp/ut-msgs.p (input "show":U, input 108, input "").
            apply "ENTRY":U to tt-digita.ordem in browse br-digita.
            
            return error.
        end.
        
        /*:T As demais valida‡äes devem ser feitas aqui */
        if tt-digita.ordem <= 0 then do:
            assign browse br-digita:CURRENT-COLUMN = tt-digita.ordem:HANDLE in browse br-digita.
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid r-tt-digita.
            
            run utp/ut-msgs.p (input "show":U, input 99999, input "").
            apply "ENTRY":U to tt-digita.ordem in browse br-digita.
            
            return error.
        end.
        
    end.
    &ENDIF

    
    
    /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */

    &scoped-define frame-name f-pg-par
    
    if not can-find(first param_calc_ppr no-lock
                    where param_calc_ppr.cdn_param_calc_ppr = 
                    input frame {&frame-name} v_cdn_param_calc_ppr) then do:

        run utp/ut-msgs.p (input "show":U, input 56, input "Parƒmetro C lculo PLR").

        apply "MOUSE-SELECT-CLICK":U to im-pg-par in frame f-relat.
        apply "ENTRY":U to v_cdn_param_calc_ppr in frame {&frame-name}.
        return error.

    end.

    if input frame {&frame-name} v_num_mes_refer_calc_efetd < 1 
    or input frame {&frame-name} v_num_mes_refer_calc_efetd > 12 
    then do:

        run utp/ut-msgs.p (input "show", input 54, input "Mˆs Referˆncia").      
        apply "MOUSE-SELECT-CLICK":U to im-pg-par in frame f-relat.
        apply 'entry' to v_num_mes_refer_calc_efetd in frame f-pg-par.
        return error.

    end.

    if input frame {&frame-name} v_num_ano_refer_calc_efetd = 0
    then do:

        run utp/ut-msgs.p (input "show", input 54, input "Ano Referˆncia").      
        apply "MOUSE-SELECT-CLICK":U to im-pg-par in frame f-relat.
        apply 'entry' to v_num_ano_refer_calc_efetd in frame f-pg-par.
        return error.

    end.

    IF input frame {&frame-name} v_individual_factor = NO THEN DO:

        IF input frame {&frame-name} v_cdn_individual_factor = 0 THEN DO:
            run utp/ut-msgs.p (input "show", input 54, input "Individual Factor").      
            apply "MOUSE-SELECT-CLICK":U to im-pg-par in frame f-relat.
            apply 'entry' to v_cdn_individual_factor in frame f-pg-par.
            return error.

        END.

    END.


    if input frame {&frame-name} v_num_mes_refer_calc_efetd > param_empres_rh.num_mes_refer_calc_efetd 
    and input frame {&frame-name} v_num_ano_refer_calc_efetd = param_empres_rh.num_ano_refer_calc_efetd 
    or input frame {&frame-name} v_num_ano_refer_calc_efetd > param_empres_rh.num_ano_refer_calc_efetd then do: 
        run utp/ut-msgs.p (input "show", input 17006, 
                           input "Mˆs ou Ano Referˆncia maior que Per¡odo Atual~~" +
                                 "Informe um per¡odo no m ximo igual ao per¡odo em aberto da Folha").      
        apply "MOUSE-SELECT-CLICK":U to im-pg-par in frame f-relat.
        apply 'entry' to v_num_mes_refer_calc_efetd in frame f-pg-par.
        return error.
    end.

    if input frame {&frame-name} v_arquivo_exportacao = "" 
    or input frame {&frame-name} v_arquivo_exportacao = ? then do:

        run utp/ut-msgs.p (input "show":U, input 265, 
                           input text-entrada:input-value in frame {&frame-name}).
        
        apply "MOUSE-SELECT-CLICK":U to im-pg-par in frame f-relat.
        apply "ENTRY":U to v_arquivo_exportacao in frame {&frame-name}.
        return error.

    end.

    &undefine frame-name
    
    
    /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.v_cdn_empres_usuar = v_cdn_empres_usuar
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.imprime_parametros = input frame f-pg-imp tb-parametro
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           &IF defined(PGCLA) > 0 &THEN
           tt-param.classifica      = input frame f-pg-cla rs-classif
           tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1, 
                                            rs-classif:radio-buttons in frame f-pg-cla)
           &ENDIF
           &IF "{&RTF}":U = "YES":U &THEN
           tt-param.modelo-rtf      = INPUT FRAME f-pg-imp c-modelo-rtf
           /*Alterado 14/02/2005 - tech1007 - Armazena a informa‡Æo se o RTF est  habilitado ou nÆo*/
           tt-param.l-habilitaRtf     = INPUT FRAME f-pg-imp l-habilitaRtf
           /*Fim alteracao 14/02/2005*/ 
           &endif
           .
    
    /*Alterado 14/02/2005 - tech1007 - Alterado o teste para verificar se a op‡Æo de RTF est  selecionada*/
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    /*Fim alteracao 14/02/2005*/

    /*:T Coloque aqui a/l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
    &scoped-define frame-name f-pg-sel
    
         /*
    assign tt-param.tt_ini_cdn_estab       = input frame {&frame-name} v_ini_cdn_estab      
           tt-param.tt_fim_cdn_estab       = input frame {&frame-name} v_fim_cdn_estab      
           tt-param.tt_ini_cdn_funcionario = input frame {&frame-name} v_ini_cdn_funcionario
           tt-param.tt_fim_cdn_funcionario = input frame {&frame-name} v_fim_cdn_funcionario.*/

    assign tt-param.v_cod_id_feder_ini       = input frame {&frame-name} v_cod_id_feder_ini
           tt-param.v_cod_id_feder_fim       = input frame {&frame-name} v_cod_id_feder_fim.
    
    &undefine frame-name
    
    &scoped-define frame-name f-pg-par
    
    assign tt-param.tt_cdn_param_calc_ppr       = input frame {&frame-name} v_cdn_param_calc_ppr      
           tt-param.tt_num_ano_refer_calc_efetd = input frame {&frame-name} v_num_ano_refer_calc_efetd
           tt-param.tt_num_mes_refer_calc_efetd = input frame {&frame-name} v_num_mes_refer_calc_efetd
           tt-param.tt_ind_tip_exporta          = input frame {&frame-name} v_exporta         
           tt-param.tt_des_ind_exporta          = entry((tt-param.tt_ind_tip_exporta - 1) * 2 + 1, 
                                                        v_exporta:radio-buttons in frame {&frame-name})                            
           tt-param.tt_individual_factor        = input frame {&frame-name} v_individual_factor       
           tt-param.tt_arquivo_exporta          = input frame {&frame-name} v_arquivo_exportacao
           tt-param.tp_func                = input frame {&frame-name} v_tp_func
           tt-param.v_cdn_individual_factor =  input frame {&frame-name} v_cdn_individual_factor .         

    if frame {&frame-name} v_arquivo_exportacao NOT ENTERED then
        apply "VALUE-CHANGED":U to v_exporta.

    if r-index(tt-param.tt_arquivo_exporta, ".xlsx") = 0 then tt-param.tt_arquivo_exporta = tt-param.tt_arquivo_exporta + ".xlsx".
    
    &undefine frame-name


    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i prghur/esp/ymfp0013rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}

    if input frame f-pg-imp rs-execucao = 1 then do:
        file-info:filename = tt-param.tt_arquivo_exporta.

        if file-info:pathname ne ? then do:
            RUN ShellExecute{&A} IN hpApi(0,
                                          "open",
                                          file-info:pathname,
                                          "",
                                          "",
                                          1,
                                          OUTPUT hInstance).
        end.


    end.


end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P gina (folder)   
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn_nome_arquivo w-relat 
FUNCTION fn_nome_arquivo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    do with frame f-pg-par :

        assign input v_num_ano_refer_calc_efetd
               input v_num_mes_refer_calc_efetd
               input v_exporta.

        if v_num_mes_refer_calc_efetd > 0
        and v_num_mes_refer_calc_efetd < 13 
        and v_num_ano_refer_calc_efetd > 0 then do:
            case v_exporta :
                when 1 then return v_diretorio_usuario +
                                   substitute("F&1_TIP_Cash_&2",
                                              v_num_ano_refer_calc_efetd,
                                              fn_nome_mes(v_num_mes_refer_calc_efetd)) +
                                              "_ProvisÆo.xlsx".

                when 2 then return v_diretorio_usuario +
                                   substitute("F&1_TIP_Cash_&2",
                                              v_num_ano_refer_calc_efetd,
                                              "Pagamento.xlsx").
            end case.
        end.

    end.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

