&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/* --------------------------------------------------------------------------------------- 
                                                                                        
   Sistema................: TOTVS                                                       
   Modulo.................: Manuten‡Æo Industrial                                                   
                                                                                           
   Programa...............: Ficha de Manuten‡Æo                                              
   Sub Programa...........:                                                                
                                                                                            
   Descricao..............: 
   programa para consultar: 
                             
   Entidade Desenvolvedora: DSC PRAXIS
   
   tabelas usadas.........: 
   importante.............:  
                                                                                           
   Historico Programa -------------------------------------------------------------------+ 
   | Data       | Autor               | Descricao                                        | 
   +----------- +---------------------+--------------------------------------------------+ 
   | 01/07/2015 | Marcos A.Souza      | Desenvolvimento do Programa                      | 
   +------------+---------------------+--------------------------------------------------+ */
{include/i-prgvrs.i esmi0304 2.08.10.001}  /*** 010019 ***/
{cdp/cdcfgmnt.i}
{cdp/cd9911.i} /* Verifica Unidade Neg½cio */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat½rio                            */
/* Obs: Retirar o valor do preprocessador para as pÿginas que n’o existirem  */

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
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field i-ordem-ini      like ord-manut.nr-ord-produ
    field i-ordem-fim      like ord-manut.nr-ord-produ
    field d-dt-man-ini     as date format 99/99/9999
    field d-dt-man-fim     as date format 99/99/9999
    field c-custo-ini      as char format "x(8)"   
    field c-custo-fim      as char format "x(8)"
    field c-equipto-ini    as char format "x(16)"
    field c-equipto-fim    as char format "x(16)"
    field c-equipe-ini     as char format "x(08)" 
    field c-equipe-fim     as char format "x(08)"
    field i-tipo-manut-ini like manut.cd-tipo
    field i-tipo-manut-fim like manut.cd-tipo
    field c-planejado-ini  as char format "x(12)"
    field c-planejado-fim  as char format "x(12)"
    field c-plano-parada-ini as char format "x(8)"
    field c-plano-parada-fim as char format "x(8)"
    FIELD imprimir         AS INTEGER
    field l-item           as log
    field l-ferr           as log
    field l-epi            as log
    field l-espec          as log
 &if defined (bf_mnt_ems204) &then
    field l-turno          as log
    field l-ficha          as log
 &endif   
    field l-imp            as log
    field l-met            as log
    field l-tarefa         as log
    field l-nar-ord        as log
    field l-nar-tar        as log
    field rs-ordem-plano   as integer
    field c-estabel-ini    as char format "x(03)"
    field c-estabel-fim    as char format "x(03)"
    FIELD l-equipto        AS LOG
    FIELD c-cod-unid-negoc-ini AS CHAR FORMAT "x(03)"
    FIELD c-cod-unid-negoc-fim AS CHAR FORMAT "x(03)".

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rect-rtf RECT-7 RECT-9 rs-destino ~
bt-config-impr bt-arquivo c-arquivo l-habilitaRtf bt-modelo-rtf rs-execucao ~
text-rtf text-modelo-rtf 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo l-habilitaRtf ~
c-modelo-rtf rs-execucao text-rtf text-modelo-rtf 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE BUTTON bt-modelo-rtf 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-modelo-rtf AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modelo-rtf AS CHARACTER FORMAT "X(256)":U INITIAL "Modelo:" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE text-rtf AS CHARACTER FORMAT "X(256)":U INITIAL "Rich Text Format(RTF)" 
      VIEW-AS TEXT 
     SIZE 20.86 BY .63 NO-UNDO.

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

DEFINE RECTANGLE rect-rtf
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 3.54.

DEFINE VARIABLE l-habilitaRtf AS LOGICAL INITIAL no 
     LABEL "RTF" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE c-retangulo AS CHARACTER FORMAT "X(25)":U INITIAL "Informa»„es da Narrativa" 
      VIEW-AS TEXT 
     SIZE 18.86 BY .67 NO-UNDO.

DEFINE VARIABLE rs-ordem-plano AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ordem", 1,
"Plano", 2
     SIZE 26 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 1.75.

DEFINE VARIABLE l-epi AS LOGICAL INITIAL yes 
     LABEL "Equipamento de Prote»‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE l-equipto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE .14 BY .04 NO-UNDO.

DEFINE VARIABLE l-espec AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE .14 BY .04 NO-UNDO.

DEFINE VARIABLE l-ferr AS LOGICAL INITIAL yes 
     LABEL "Ferramenta" 
     VIEW-AS TOGGLE-BOX
     SIZE .14 BY .04 NO-UNDO.

DEFINE VARIABLE l-ficha AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE .14 BY .04 NO-UNDO.

DEFINE VARIABLE l-imp AS LOGICAL INITIAL no 
     LABEL "Fichas j  Impressas" 
     VIEW-AS TOGGLE-BOX
     SIZE .14 BY .08 NO-UNDO.

DEFINE VARIABLE l-item AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE .14 BY .04 NO-UNDO.

DEFINE VARIABLE l-met AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE .14 BY .04 NO-UNDO.

DEFINE VARIABLE l-nar-ord AS LOGICAL INITIAL yes 
     LABEL "Narrativa da Ordem" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.86 BY .88 NO-UNDO.

DEFINE VARIABLE l-nar-tar AS LOGICAL INITIAL yes 
     LABEL "Narrativa da Tarefa" 
     VIEW-AS TOGGLE-BOX
     SIZE .14 BY .04 NO-UNDO.

DEFINE VARIABLE l-tarefa AS LOGICAL INITIAL no 
     LABEL "Uma Tarefa por P gina" 
     VIEW-AS TOGGLE-BOX
     SIZE .14 BY .04 NO-UNDO.

DEFINE VARIABLE l-turno AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE .14 BY .04 NO-UNDO.

DEFINE VARIABLE c-cod-unid-negoc-fim AS CHARACTER FORMAT "X(03)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-unid-negoc-ini AS CHARACTER FORMAT "X(03)":U 
     LABEL "Unidade Neg¢cio" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE c-custo-fim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 1 BY .88 NO-UNDO.

DEFINE VARIABLE c-custo-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Centro Custo":R15 
     VIEW-AS FILL-IN 
     SIZE 1 BY .88 NO-UNDO.

DEFINE VARIABLE c-equipe-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-equipe-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Equipe Manuten»‡Æo":R21 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-equipto-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-equipto-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Equipamento":R14 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-estabel-fim AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 1 BY .88 NO-UNDO.

DEFINE VARIABLE c-estabel-ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento":R18 
     VIEW-AS FILL-IN 
     SIZE 1 BY .88 NO-UNDO.

DEFINE VARIABLE c-planejado-fim AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 1 BY .88 NO-UNDO.

DEFINE VARIABLE c-planejado-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Planejador":R12 
     VIEW-AS FILL-IN 
     SIZE 1 BY .88 NO-UNDO.

DEFINE VARIABLE d-dt-man-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE d-dt-man-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Programada":R18 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-plano-parada-fim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-plano-parada-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Plano Parada" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE i-cd-tipo-fim AS INTEGER FORMAT ">>,>>9" INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE i-cd-tipo-ini AS INTEGER FORMAT ">>,>>9" INITIAL 0 
     LABEL "Tipo Manuten»‡Æo":R21 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE i-ordem-fim AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE i-ordem-ini AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     LABEL "Ordem":R7 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-43
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-44
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-45
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-46
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-47
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-48
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-49
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-50
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-51
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-las":U
     SIZE 2 BY .88.

DEFINE VARIABLE rs-imprimir AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Visualizar", 1,
"Imprimir", 2
     SIZE 29 BY 1.25 NO-UNDO.

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
     rs-destino AT ROW 1.63 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 2.71 COL 43.29 HELP
          "Configura‡Æo da impressora"
     bt-arquivo AT ROW 2.71 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 2.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     l-habilitaRtf AT ROW 4.83 COL 3.29
     c-modelo-rtf AT ROW 6.63 COL 3 HELP
          "Nome do arquivo de modelo do relat¢rio" NO-LABEL
     bt-modelo-rtf AT ROW 6.63 COL 43 HELP
          "Escolha do nome do arquivo"
     rs-execucao AT ROW 8.88 COL 2.86 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.04 COL 3.86 NO-LABEL
     text-rtf AT ROW 4.17 COL 1.14 COLON-ALIGNED NO-LABEL
     text-modelo-rtf AT ROW 5.96 COL 1.14 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 8.13 COL 1.14 COLON-ALIGNED NO-LABEL
     rect-rtf AT ROW 4.46 COL 2
     RECT-7 AT ROW 1.33 COL 2.14
     RECT-9 AT ROW 8.33 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.5 WIDGET-ID 100.

DEFINE FRAME f-pg-par
     l-item AT ROW 1.25 COL 5.72 WIDGET-ID 46
     l-imp AT ROW 1.25 COL 37 WIDGET-ID 44
     l-ferr AT ROW 2.25 COL 5.72 WIDGET-ID 40
     l-tarefa AT ROW 2.25 COL 37 WIDGET-ID 54
     l-epi AT ROW 3.25 COL 5.72 WIDGET-ID 34
     l-nar-ord AT ROW 3.25 COL 37 WIDGET-ID 50
     l-espec AT ROW 4.25 COL 5.72 WIDGET-ID 38
     l-nar-tar AT ROW 4.25 COL 37 WIDGET-ID 52
     l-ficha AT ROW 5.25 COL 5.72 WIDGET-ID 42
     l-met AT ROW 5.25 COL 37 WIDGET-ID 48
     l-turno AT ROW 6.25 COL 5.72 WIDGET-ID 56
     l-equipto AT ROW 6.25 COL 37 WIDGET-ID 36
     rs-ordem-plano AT ROW 8.25 COL 40 NO-LABEL WIDGET-ID 60
     c-retangulo AT ROW 7.5 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     RECT-10 AT ROW 7.75 COL 37 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10 WIDGET-ID 100.

DEFINE FRAME f-pg-sel
     c-cod-unid-negoc-ini AT ROW 1.46 COL 27.43 COLON-ALIGNED WIDGET-ID 16
     c-cod-unid-negoc-fim AT ROW 1.46 COL 51 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     i-ordem-ini AT ROW 2.5 COL 27.43 COLON-ALIGNED HELP
          "Nœmero da Ordem de Manuten»’o" WIDGET-ID 28
     i-ordem-fim AT ROW 2.5 COL 51 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     d-dt-man-ini AT ROW 3.58 COL 27.43 COLON-ALIGNED WIDGET-ID 20
     d-dt-man-fim AT ROW 3.58 COL 51 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     i-cd-tipo-ini AT ROW 4.67 COL 27.43 COLON-ALIGNED WIDGET-ID 24
     i-cd-tipo-fim AT ROW 4.67 COL 51 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     fi-plano-parada-ini AT ROW 5.71 COL 27.43 COLON-ALIGNED WIDGET-ID 2
     fi-plano-parada-fim AT ROW 5.71 COL 51 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     c-equipto-ini AT ROW 6.71 COL 27.43 COLON-ALIGNED WIDGET-ID 56
     c-equipto-fim AT ROW 6.71 COL 51 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     c-equipe-ini AT ROW 7.71 COL 27.43 COLON-ALIGNED WIDGET-ID 52
     c-equipe-fim AT ROW 7.71 COL 51 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     rs-imprimir AT ROW 9.25 COL 29 NO-LABEL WIDGET-ID 10
     c-planejado-fim AT ROW 9.75 COL 12.57 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     c-planejado-ini AT ROW 9.75 COL 13.14 COLON-ALIGNED WIDGET-ID 64
     c-estabel-fim AT ROW 10 COL 12.43 COLON-ALIGNED HELP
          "C½digo do estabelecimento de destino do item" NO-LABEL WIDGET-ID 58
     c-estabel-ini AT ROW 10 COL 17.29 COLON-ALIGNED HELP
          "C½digo do estabelecimento de destino do item" WIDGET-ID 60
     c-custo-fim AT ROW 10.5 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     c-custo-ini AT ROW 10.5 COL 13.72 COLON-ALIGNED WIDGET-ID 48
     IMAGE-1 AT ROW 2.5 COL 43 WIDGET-ID 30
     IMAGE-2 AT ROW 2.5 COL 48 WIDGET-ID 34
     IMAGE-6 AT ROW 3.58 COL 43 WIDGET-ID 44
     IMAGE-10 AT ROW 3.58 COL 48 WIDGET-ID 32
     IMAGE-44 AT ROW 4.67 COL 43 WIDGET-ID 36
     IMAGE-45 AT ROW 4.67 COL 48 WIDGET-ID 38
     IMAGE-46 AT ROW 1.46 COL 43 WIDGET-ID 40
     IMAGE-47 AT ROW 1.46 COL 48 WIDGET-ID 42
     IMAGE-48 AT ROW 5.71 COL 48 WIDGET-ID 8
     IMAGE-49 AT ROW 5.71 COL 43 WIDGET-ID 6
     IMAGE-11 AT ROW 9.75 COL 6.57 WIDGET-ID 66
     IMAGE-43 AT ROW 10 COL 11.43 WIDGET-ID 78
     IMAGE-3 AT ROW 10 COL 6.43 WIDGET-ID 74
     IMAGE-5 AT ROW 10.5 COL 3 WIDGET-ID 80
     IMAGE-9 AT ROW 10.5 COL 9 WIDGET-ID 84
     IMAGE-4 AT ROW 7.75 COL 43 WIDGET-ID 76
     IMAGE-8 AT ROW 7.75 COL 48 WIDGET-ID 82
     IMAGE-12 AT ROW 9.75 COL 11.57 WIDGET-ID 68
     IMAGE-50 AT ROW 6.75 COL 48 WIDGET-ID 86
     IMAGE-51 AT ROW 6.75 COL 43 WIDGET-ID 88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "Ficha de Manuten»‡Æo"
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
/* SETTINGS FOR EDITOR c-modelo-rtf IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

ASSIGN 
       text-modelo-rtf:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Modelo:".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

ASSIGN 
       text-rtf:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Rich Text Format(RTF)".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN c-retangulo IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       c-retangulo:HIDDEN IN FRAME f-pg-par           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-10 IN FRAME f-pg-par
   NO-ENABLE                                                            */
ASSIGN 
       RECT-10:HIDDEN IN FRAME f-pg-par           = TRUE.

/* SETTINGS FOR RADIO-SET rs-ordem-plano IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rs-ordem-plano:HIDDEN IN FRAME f-pg-par           = TRUE.

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
ASSIGN 
       c-cod-unid-negoc-fim:READ-ONLY IN FRAME f-pg-sel        = TRUE.

ASSIGN 
       c-cod-unid-negoc-ini:READ-ONLY IN FRAME f-pg-sel        = TRUE.

/* SETTINGS FOR FILL-IN c-custo-fim IN FRAME f-pg-sel
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       c-custo-fim:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR FILL-IN c-custo-ini IN FRAME f-pg-sel
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       c-custo-ini:HIDDEN IN FRAME f-pg-sel           = TRUE.

ASSIGN 
       c-equipto-fim:READ-ONLY IN FRAME f-pg-sel        = TRUE.

ASSIGN 
       c-equipto-ini:READ-ONLY IN FRAME f-pg-sel        = TRUE.

/* SETTINGS FOR FILL-IN c-estabel-fim IN FRAME f-pg-sel
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       c-estabel-fim:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR FILL-IN c-estabel-ini IN FRAME f-pg-sel
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       c-estabel-ini:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR FILL-IN c-planejado-fim IN FRAME f-pg-sel
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       c-planejado-fim:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR FILL-IN c-planejado-ini IN FRAME f-pg-sel
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       c-planejado-ini:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-10 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-11 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       IMAGE-11:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR IMAGE IMAGE-12 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       IMAGE-12:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR IMAGE IMAGE-2 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-3 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       IMAGE-3:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR IMAGE IMAGE-4 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       IMAGE-4:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR IMAGE IMAGE-43 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       IMAGE-43:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR IMAGE IMAGE-44 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-45 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-46 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-47 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-5 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       IMAGE-5:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR IMAGE IMAGE-6 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-8 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       IMAGE-8:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR IMAGE IMAGE-9 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       IMAGE-9:HIDDEN IN FRAME f-pg-sel           = TRUE.

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
ON END-ERROR OF w-relat /* Ficha de Manuten»‡Æo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Ficha de Manuten»‡Æo */
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


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-modelo-rtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modelo-rtf w-relat
ON CHOOSE OF bt-modelo-rtf IN FRAME f-pg-imp
DO:
    def var c-arq-conv  as char no-undo.
    def var l-ok as logical no-undo.

    assign c-modelo-rtf = replace(input frame {&frame-name} c-modelo-rtf, "/", "~\").
    SYSTEM-DIALOG GET-FILE c-arq-conv
       FILTERS "*.rtf" "*.rtf",
               "*.*" "*.*"
       DEFAULT-EXTENSION "rtf"
       INITIAL-DIR "modelos" 
       MUST-EXIST
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then
        assign c-modelo-rtf:screen-value in frame {&frame-name}  = replace(c-arq-conv, "~\", "/"). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME l-ficha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-ficha w-relat
ON VALUE-CHANGED OF l-ficha IN FRAME f-pg-par
DO:
  if l-ficha:checked in frame f-pg-par = no then
     assign l-met:checked in frame f-pg-par = no.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME l-habilitaRtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-habilitaRtf w-relat
ON VALUE-CHANGED OF l-habilitaRtf IN FRAME f-pg-imp /* RTF */
DO:
    &IF "{&RTF}":U = "YES":U &THEN
    RUN pi-habilitaRtf.
    &endif
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME l-met
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-met w-relat
ON VALUE-CHANGED OF l-met IN FRAME f-pg-par
DO:
  if l-met:checked in frame f-pg-par = yes then
     assign l-ficha:checked in frame f-pg-par = yes.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME l-nar-tar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-nar-tar w-relat
ON VALUE-CHANGED OF l-nar-tar IN FRAME f-pg-par /* Narrativa da Tarefa */
DO:
  rs-ordem-plano:sensitive = l-nar-tar:checked.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
/*Alterado 15/02/2005 - tech1007 - Evento alterado para correto funcionamento dos novos widgets
  utilizados para a funcionalidade de RTF*/
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = YES
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF est  ativo*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = NO
                   l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No"
                   l-habilitaRtf = NO
                   &endif
                   /*Fim alteracao 17/02/2005*/
                   .
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = NO
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF est  ativo*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   /*Fim alteracao 17/02/2005*/
                   .
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF est  ativo*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   /*Fim alteracao 17/02/2005*/
                   .
            /*Alterado 15/02/2005 - tech1007 - Teste para funcionar corretamente no WebEnabler*/
            &IF "{&RTF}":U = "YES":U &THEN
            IF VALID-HANDLE(hWenController) THEN DO:
                ASSIGN l-habilitaRtf:sensitive  = NO
                       l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No"
                       l-habilitaRtf = NO.
            END.
            &endif
            /*Fim alteracao 15/02/2005*/
        end.
    end case.
end.
&IF "{&RTF}":U = "YES":U &THEN
RUN pi-habilitaRtf.
&endif
/*Fim alteracao 15/02/2005*/
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.



{utp/ut9000.i "esmi0304" "2.08.10.001"}

/* inicializa»„es do template de relat½rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

assign d-dt-man-fim = today.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    &if defined (bf_mnt_ems204) &then
     {utp/ut-liter.i Narrativa_Ficha_de_M²todo}
     assign l-met:label in frame f-pg-par = trim(return-value).
     assign l-ficha:hidden       in frame f-pg-par = no
            l-ficha:sensitive    in frame f-pg-par = yes
            l-ficha:checked      in frame f-pg-par = yes
            l-turno:hidden       in frame f-pg-par = no
            l-turno:sensitive    in frame f-pg-par = yes
            l-turno:checked      in frame f-pg-par = yes.
     &endif

    RUN enable_UI.

    ASSIGN c-cod-unid-negoc-ini:VISIBLE IN FRAME f-pg-sel = l-usa-unid-negoc
           c-cod-unid-negoc-fim:VISIBLE IN FRAME f-pg-sel = l-usa-unid-negoc
           image-46:VISIBLE             IN FRAME f-pg-sel = l-usa-unid-negoc
           image-47:VISIBLE             IN FRAME f-pg-sel = l-usa-unid-negoc.

    {include/i-rpmbl.i}
/*      {mip/mi9999.i} */
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
  DISPLAY c-cod-unid-negoc-ini c-cod-unid-negoc-fim i-ordem-ini i-ordem-fim 
          d-dt-man-ini d-dt-man-fim i-cd-tipo-ini i-cd-tipo-fim 
          fi-plano-parada-ini fi-plano-parada-fim c-equipto-ini c-equipto-fim 
          c-equipe-ini c-equipe-fim rs-imprimir 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-48 IMAGE-49 IMAGE-50 IMAGE-51 c-cod-unid-negoc-ini 
         c-cod-unid-negoc-fim i-ordem-ini i-ordem-fim d-dt-man-ini d-dt-man-fim 
         i-cd-tipo-ini i-cd-tipo-fim fi-plano-parada-ini fi-plano-parada-fim 
         c-equipto-ini c-equipto-fim c-equipe-ini c-equipe-fim rs-imprimir 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo l-habilitaRtf c-modelo-rtf rs-execucao text-rtf 
          text-modelo-rtf 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rect-rtf RECT-7 RECT-9 rs-destino bt-config-impr bt-arquivo c-arquivo 
         l-habilitaRtf bt-modelo-rtf rs-execucao text-rtf text-modelo-rtf 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY l-item l-imp l-ferr l-tarefa l-epi l-nar-ord l-espec l-nar-tar l-ficha 
          l-met l-turno l-equipto 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE l-item l-imp l-ferr l-tarefa l-epi l-nar-ord l-espec l-nar-tar l-ficha 
         l-met l-turno l-equipto 
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

    /* Coloque aqui as valida»„es das outras pÿginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na pÿgina 
       com problemas e colocar o focus no campo com problemas             */    

    create tt-param.
    assign tt-param.usuario          = c-seg-usuario
           tt-param.destino          = input frame f-pg-imp rs-destino
           tt-param.data-exec        = today
           tt-param.hora-exec        = time
           tt-param.i-ordem-ini      = input frame f-pg-sel i-ordem-ini
           tt-param.i-ordem-fim      = input frame f-pg-sel i-ordem-fim
           tt-param.d-dt-man-ini     = input frame f-pg-sel d-dt-man-ini
           tt-param.d-dt-man-fim     = input frame f-pg-sel d-dt-man-fim
           tt-param.c-custo-ini      = input frame f-pg-sel c-custo-ini
           tt-param.c-custo-fim      = input frame f-pg-sel c-custo-fim
           tt-param.c-equipto-ini    = input frame f-pg-sel c-equipto-ini
           tt-param.c-equipto-fim    = input frame f-pg-sel c-equipto-fim
           tt-param.c-equipe-ini     = input frame f-pg-sel c-equipe-ini
           tt-param.c-equipe-fim     = input frame f-pg-sel c-equipe-fim
           tt-param.i-tipo-manut-ini = input frame f-pg-sel i-cd-tipo-ini
           tt-param.i-tipo-manut-fim = input frame f-pg-sel i-cd-tipo-fim
           tt-param.c-planejado-ini  = input frame f-pg-sel c-planejado-ini
           tt-param.c-planejado-fim  = input frame f-pg-sel c-planejado-fim
           tt-param.c-plano-parada-ini  = input frame f-pg-sel fi-plano-parada-ini
           tt-param.c-plano-parada-fim  = input frame f-pg-sel fi-plano-parada-fim
           tt-param.imprimir         = input frame f-pg-sel rs-imprimir
           tt-param.l-item           = input frame f-pg-par l-item
           tt-param.l-ferr           = input frame f-pg-par l-ferr
           tt-param.l-epi            = input frame f-pg-par l-epi
           tt-param.l-espec          = input frame f-pg-par l-espec
           tt-param.l-imp            = input frame f-pg-par l-imp
           tt-param.l-met            = input frame f-pg-par l-met
        &if defined (bf_mnt_ems204) &then
           tt-param.l-ficha          = input frame f-pg-par l-ficha
           tt-param.l-turno          = input frame f-pg-par l-turno 
        &endif    
           tt-param.l-tarefa         = input frame f-pg-par l-tarefa
           tt-param.l-nar-ord        = input frame f-pg-par l-nar-ord
           tt-param.l-nar-tar        = input frame f-pg-par l-nar-tar
           tt-param.rs-ordem-plano   = input frame f-pg-par rs-ordem-plano
           tt-param.c-estabel-ini    = input frame f-pg-sel c-estabel-ini
           tt-param.c-estabel-fim    = input frame f-pg-sel c-estabel-fim
           tt-param.l-equipto        = input frame f-pg-par l-equipto
           tt-param.c-cod-unid-negoc-ini = INPUT FRAME f-pg-sel c-cod-unid-negoc-ini
           tt-param.c-cod-unid-negoc-fim = INPUT FRAME f-pg-sel c-cod-unid-negoc-fim.

    if  tt-param.destino = 1 then           
        assign tt-param.arquivo = "".
    else
    if  tt-param.destino = 2 then 
        assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    else
        assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

    /* Coloque aqui a l½gica de grava»’o dos par³mtros e sele»’o na temp-table
       tt-param */

    {include/i-rpexb.i}

    if  session:set-wait-state("general") then.

    {include/i-rprun.i mip/esmi0304rp.p}

    {include/i-rpexc.i}

    if  session:set-wait-state("") then.

/*     {include/i-rptrm.i}  */

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

