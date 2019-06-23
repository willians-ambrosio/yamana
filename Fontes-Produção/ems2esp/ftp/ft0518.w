&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wReport 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa         for ems2cadme.empresa.
def buffer param_seg_estab for ems2cadme.param_seg_estab.
def buffer portador        for ems2cadme.portador.
def buffer seg_usuar_estab for ems2cadme.seg_usuar_estab.

{include/i-prgvrs.i ESFT0518 2.00.00.999 } /*** 010001 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ft0516aa MFT}
&ENDIF

/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        FT0518
&GLOBAL-DEFINE Version        2.00.00.999
&GLOBAL-DEFINE VersionLayout

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Seleá∆o,ParÉmetro,Impress∆o

&GLOBAL-DEFINE PGLAY          NO
&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGCLA          NO
&GLOBAL-DEFINE PGPAR          YES
&GLOBAL-DEFINE PGDIG          NO
&GLOBAL-DEFINE PGIMP          YES
&GLOBAL-DEFINE PGLOG          NO

&GLOBAL-DEFINE RTF            NO

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btHelp2
&GLOBAL-DEFINE page1Widgets
&GLOBAL-DEFINE page2Widgets   rs-imprime
&GLOBAL-DEFINE page3Widgets
&GLOBAL-DEFINE page4Widgets   i-bloq rs-banco ~
                              bt-prev bt-next
&GLOBAL-DEFINE page5Widgets
&GLOBAL-DEFINE page6Widgets   rs-impressora rsDestiny ~
                              btConfigImpr btFile ~
                              rsExecution bt-arquivo-bloq ~
                              bt-config-impr-bloq
&GLOBAL-DEFINE page7Widgets
&GLOBAL-DEFINE page8Widgets

&GLOBAL-DEFINE page0Text
&GLOBAL-DEFINE page1Text
&GLOBAL-DEFINE page2Text
&GLOBAL-DEFINE page3Text
&GLOBAL-DEFINE page4Text      text-3 text-4
&GLOBAL-DEFINE page5Text
&GLOBAL-DEFINE page6Text      text-destino text-modo ~
                              text-destino-bloq
&GLOBAL-DEFINE page7Text
&GLOBAL-DEFINE page8Text

&GLOBAL-DEFINE page1Fields
&GLOBAL-DEFINE page2Fields    c-cod-estabel c-serie ~
                              c-nr-nota-fis-ini c-nr-nota-fis-fim ~
                              de-cdd-embarq-ini de-cdd-embarq-fim ~
                              da-dt-saida c-hr-saida i-nr-copias
&GLOBAL-DEFINE page3Fields
&GLOBAL-DEFINE page4Fields    c-num-bloq
&GLOBAL-DEFINE page5Fields
&GLOBAL-DEFINE page6Fields    cFile c-arquivo-bloq
&GLOBAL-DEFINE page7Fields
&GLOBAL-DEFINE page8Fields

/* Parameters Definitions ---                                           */

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field identific        as char
    field ini-cod-estabel  as char
    field fim-cod-estabel  as char
    field ini-serie        as char
    field fim-serie        as char
    field ini-cdd-embarq   as dec format ">>>>>>>>>>>>>>>9":U
    field fim-cdd-embarq   as dec format ">>>>>>>>>>>>>>>9":U
    field ini-nr-nota-fis  as char
    field fim-nr-nota-fis  as char
    field rs-imprime       as integer
    field banco            as integer
    field cod-febraban     as integer
    field cod-portador     as integer
    field prox-bloq        as char
    field c-instrucao      as char extent 5
    field imprime-bloq     as logical.

def temp-table tt-param-aux
    field destino              as integer
    field destino-bloq         as integer
    field arquivo              as char
    field arquivo-bloq         as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field cod-layout           as character
    field des-layout           as character
    field log-impr-dados       as logical  
    field v_num_tip_aces_usuar as integer
&IF "{&mguni_version}" >= "2.071" &THEN
    field ep-codigo            LIKE ems2cadme.empresa.ep-codigo
&ELSE
    field ep-codigo            LIKE ems2cadme.empresa.ep-codigo
&ENDIF
    field c-cod-estabel        like movdis.nota-fiscal.cod-estabel
    field c-serie              like movdis.nota-fiscal.serie
    field c-nr-nota-fis-ini    like movdis.nota-fiscal.nr-nota-fis
    field c-nr-nota-fis-fim    like movdis.nota-fiscal.nr-nota-fis
    field de-cdd-embarq-ini    like movdis.nota-fiscal.cdd-embarq
    field de-cdd-embarq-fim    like movdis.nota-fiscal.cdd-embarq
    field da-dt-saida          like movdis.nota-fiscal.dt-saida
    field c-hr-saida           AS CHAR FORMAT "xx:xx:xx":U INITIAL "000000"
    field banco                as integer
    field cod-febraban         as integer      
    field cod-portador         as integer      
    field prox-bloq            as char         
    field c-instrucao          as char extent 5
    field imprime-bloq         as logical
    field rs-imprime           as integer
    FIELD impressora-so        AS CHAR
    FIELD impressora-so-bloq   AS CHAR
    FIELD nr-copias            AS INTEGER.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id ordem.

define buffer b-tt-digita for tt-digita.

{cdp/cdcfgdis.i}
{cdp/cd0019.i MFT YES} /* inicializaá∆o da seguranáa por estabelecimento */
{utp/ut-glob.i}

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

define new shared temp-table tt-notas-impressas
    field r-nota as rowid.

def new shared var i-tipo    as integer.
def new shared var r-nota    as rowid.
def var l-ok                 as logical no-undo.
def var c-arq-digita         as char    no-undo.
def var c-terminal           as char    no-undo.
def var v-cod-pg-mouse-selec as char    no-undo.
def var v-cod-prog-i-rprun   as char    no-undo.
def var c-impressora-old     as char    no-undo.
def var c-arquivo-old        as char    no-undo.
def var c-destino-old        as char    no-undo.
def var i-cont               as int     no-undo.
def var v-cod-prog-gerado    as char    no-undo.
def var v-cod-extens-arq     as char    no-undo initial "doc".
def var c-oper               as char.
def var c-prox-bloq          as char    no-undo.
def var c-bloq               as char    no-undo.
def var c-bloq-digito        as char    no-undo.
def var i-empresa            like param-global.empresa-prin no-undo.
def var i-tamanho-bloq       as integer no-undo.
def var i-tamanho            as integer no-undo.
def var i-cod-febraban       as integer no-undo.
def var l-embarque           as logical no-undo.
def var c-formato            as char    no-undo.
def var i-cod-portador       as integer no-undo.
def var c-arquivo-salvo      as char    no-undo.
DEFINE VARIABLE l-not-valid-estab AS LOGICAL     NO-UNDO.

define variable l-monitorNF-e        as logical  init no          no-undo.
define variable c-cod-estabel-ft0909 like nota-fiscal.cod-estabel no-undo.
define variable c-serie-ft0909       like nota-fiscal.serie       no-undo.
define variable c-nr-nota-fis-ft0909 like nota-fiscal.nr-nota-fis no-undo.
def stream s-imp.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est† rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE  NO-UNDO.
def new Global shared var c-seg-usuario         as Character        format "x(12)"  no-undo.



{cdp/cdapi3001.i MFT YES} /* inicializaá∆o da seguranáa por estabelecimento */
ON FIND OF estabelec do:
    FIND param_seg_estab 
        WHERE param_seg_estab.cdn_param = 1004  /* Modulo de Faturamento */
        NO-LOCK NO-ERROR.

    IF AVAIL param_seg_estab
    AND param_seg_estab.des_valor = "SIM":U then do:
        IF  NOT CAN-FIND(tt_estab_ems2 WHERE
                tt_estab_ems2.tta_cod_estab = estabelec.cod-estabel) THEN return error.		        
    END.
END.    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar btOK btCancel btHelp2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wReport AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Fechar":U 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "&Ajuda":U 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&Executar":U 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE c-cod-estabel LIKE estabelec.cod-estabel
     LABEL "Estabelecimento":U 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-hr-saida AS CHARACTER FORMAT "xx:xx:xx":U INITIAL "000000" 
     LABEL "Hr Sa°da":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-nr-nota-fis-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-nr-nota-fis-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Nr Nota Fiscal":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-serie AS CHARACTER FORMAT "X(5)":U 
     LABEL "SÇrie":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE da-dt-saida AS DATE FORMAT "99/99/9999" 
     LABEL "Dt Sa°da":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE de-cdd-embarq-fim AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9" INITIAL 9999999999999999
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE de-cdd-embarq-ini AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9" INITIAL 0
     LABEL "Embarque":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE i-nr-copias AS INTEGER FORMAT ">>9" INITIAL 1
     LABEL "Nr C¢pias":U
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-imprime AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impress∆o":U, 1,
"Reimpress∆o":U, 2
     SIZE 30.86 BY 1.25 NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81.86 BY 4.92.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81.86 BY 3.92.

DEFINE RECTANGLE RECT-99
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81.86 BY 1.92.

DEFINE BUTTON bt-next 
     IMAGE-UP FILE "image~\im-nex":U
     LABEL "Button 2":U 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-prev 
     IMAGE-UP FILE "image~\im-pre":U
     LABEL "Button 1" 
     SIZE 4 BY 1.25.

DEFINE VARIABLE rs-banco AS CHARACTER FORMAT "X(256)":U INITIAL "Bradesco":U 
     VIEW-AS COMBO-BOX INNER-LINES 9
     LIST-ITEMS "Bradesco":U,"Banco do Brasil":U,"Banco Ita£":U,"Banco Francàs e Brasileiro":U,"Banco Bamerindus":U,"Banco Boa Vista":U,"Banco Real":U,"Bradesco 80 colunas":U,"Banco Safra":U 
     DROP-DOWN-LIST
     SIZE 28.14 BY 1 NO-UNDO.

DEFINE VARIABLE c-num-bloq AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26.14 BY .88 NO-UNDO.

DEFINE VARIABLE text-3 AS CHARACTER FORMAT "X(256)":U INITIAL "N£mero Bloqueto":U 
      VIEW-AS TEXT 
     SIZE 17.14 BY .67 NO-UNDO.

DEFINE VARIABLE text-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Lay-out":U 
      VIEW-AS TEXT 
     SIZE 8 BY .67 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 3.63.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32.57 BY 3.63.

DEFINE VARIABLE i-bloq AS LOGICAL INITIAL no 
     LABEL "Emitir Bloqueto":U 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

DEFINE BUTTON bt-arquivo-bloq 
     IMAGE-UP FILE "image~\im-sea":U
     IMAGE-INSENSITIVE FILE "image~\ii-sea":U
     LABEL " " 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr-bloq 
     IMAGE-UP FILE "image~\im-cfprt":U
     LABEL " " 
     SIZE 4 BY 1.

DEFINE BUTTON btConfigImpr 
     IMAGE-UP FILE "image~\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btFile 
     IMAGE-UP FILE "image~\im-sea":U
     IMAGE-INSENSITIVE FILE "image~\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE rs-impressora AS CHARACTER FORMAT "X(256)":U INITIAL "Sem Impressoras Cadastradas no Sistema Operacional":U 
     VIEW-AS COMBO-BOX INNER-LINES 9
     LIST-ITEMS "Sem Impressoras Cadastradas no Sistema Operacional":U 
     DROP-DOWN-LIST
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE c-arquivo-bloq AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cFile AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino":U 
      VIEW-AS TEXT 
     SIZE 8.14 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE text-destino-bloq AS CHARACTER FORMAT "X(256)" INITIAL "Destino Bloqueto":U 
      VIEW-AS TEXT 
     SIZE 16 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o":U 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE rs-destino-bloq AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora":U, 1,
"Arquivo":U, 2,
"Terminal":U, 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rsDestiny AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora":U, 1,
"Arquivo":U, 2,
"Terminal":U, 3
     SIZE 44 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsExecution AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line":U, 1,
"Batch":U, 2
     SIZE 27.86 BY .92
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 2.92.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 1.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 16.75 COL 2
     btCancel AT ROW 16.75 COL 13
     btHelp2 AT ROW 16.75 COL 80
     rtToolBar AT ROW 16.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17
         FONT 1.

DEFINE FRAME fPage6
     rsDestiny AT ROW 2.38 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio":U NO-LABEL
     rs-impressora AT ROW 3.63 COL 1.29 COLON-ALIGNED HELP
          "Nome do arquivo de destino do relat¢rio":U NO-LABEL
     cFile AT ROW 3.63 COL 3.14 HELP
          "Nome do arquivo de destino do relat¢rio":U NO-LABEL
     c-arquivo-bloq AT ROW 11.17 COL 3.29 HELP
          "Nome do arquivo de destino do bloqueto":U NO-LABEL
     btFile AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo":U
     btConfigImpr AT ROW 3.58 COL 43.29 HELP
          "Configuraá∆o da impressora":U
     bt-arquivo-bloq AT ROW 11.29 COL 43.29 HELP
          "Escolha do nome do bloqueto":U
     bt-config-impr-bloq AT ROW 11.21 COL 43.29 HELP
          "Configuraá∆o da impressora":U
     rs-destino-bloq AT ROW 10 COL 3.29 HELP
          "Destino de Impress∆o do Bloqueto":U NO-LABEL
     rsExecution AT ROW 7.38 COL 3 HELP
          "Modo de Execuá∆o":U NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 COLON-ALIGNED NO-LABEL
     text-destino-bloq AT ROW 9.25 COL 3.86 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 6.63 COL 1.29 COLON-ALIGNED NO-LABEL
     "OBS: Quando estiver parametrizado para n∆o utilizar o Microsoft":U VIEW-AS TEXT
          SIZE 44.14 BY .54 AT ROW 4.75 COL 3.29
     "Word, o arquivo n∆o ser† enviado diretamente para a impressora,":U VIEW-AS TEXT
          SIZE 44.72 BY .54 AT ROW 5.25 COL 3.29
     "ele ser† aberto em tela. (v†lido para opá∆o 'Impressora')":U VIEW-AS TEXT
          SIZE 44.72 BY .54 AT ROW 5.75 COL 3.29
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 6.92 COL 2.14
     RECT-10 AT ROW 9.54 COL 2.14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.79
         SIZE 84.43 BY 11.96
         FONT 1.

DEFINE FRAME fPage4
     i-bloq AT ROW 2 COL 2
     rs-banco AT ROW 3.75 COL 2.29 COLON-ALIGNED HELP
          "Banco para emiss∆o de bloqueto":U NO-LABEL
     bt-prev AT ROW 4 COL 38.29
     bt-next AT ROW 4 COL 42.14
     c-num-bloq AT ROW 5.5 COL 36.29 COLON-ALIGNED NO-LABEL
     text-4 AT ROW 3 COL 2.86 COLON-ALIGNED NO-LABEL
     text-3 AT ROW 3 COL 35.86 COLON-ALIGNED NO-LABEL
     RECT-11 AT ROW 3.25 COL 2
     RECT-8 AT ROW 3.25 COL 35.29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.79
         SIZE 84.43 BY 11.96
         FONT 1.

DEFINE FRAME fPage2
     c-cod-estabel AT ROW 2 COL 24.14 COLON-ALIGNED HELP
          ""
          LABEL "Estabelecimento":U
          FONT 1
     c-serie AT ROW 3 COL 24.14 COLON-ALIGNED
     c-nr-nota-fis-ini AT ROW 4 COL 24.14 COLON-ALIGNED
     c-nr-nota-fis-fim AT ROW 4 COL 51.43 COLON-ALIGNED NO-LABEL
     de-cdd-embarq-ini AT ROW 5 COL 24.14 COLON-ALIGNED
     de-cdd-embarq-fim AT ROW 5 COL 51.43 COLON-ALIGNED NO-LABEL
     da-dt-saida AT ROW 7 COL 24.14 COLON-ALIGNED
     c-hr-saida AT ROW 8 COL 24.14 COLON-ALIGNED
     i-nr-copias AT ROW 9 COL 24.14 COLON-ALIGNED
     rs-imprime AT ROW 10.79 COL 26.14 NO-LABEL
     IMAGE-1 AT ROW 4 COL 44.43
     IMAGE-2 AT ROW 4 COL 49.43
     IMAGE-3 AT ROW 5 COL 44.43
     IMAGE-4 AT ROW 5 COL 49.43
     RECT-12 AT ROW 1.5 COL 2.14
     RECT-13 AT ROW 6.5 COL 2.14
     RECT-99 AT ROW 10.5 COL 2.14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.79
         SIZE 84.43 BY 11.96
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wReport ASSIGN
         HIDDEN             = YES
         TITLE              = "Emissor DANFE - NF-e - ESFT0518":U
         HEIGHT             = 17
         WIDTH              = 90
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.14
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.14
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wReport 
/* ************************* Included-Libraries *********************** */

{report/report.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wReport
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage2:FRAME = FRAME fpage0:HANDLE
       FRAME fPage4:FRAME = FRAME fpage0:HANDLE
       FRAME fPage6:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR FILL-IN c-cod-estabel IN FRAME fPage2
   LIKE = mgadm.estabelec.cod-estabel EXP-LABEL EXP-SIZE                */
/* SETTINGS FOR FRAME fPage4
                                                                        */
/* SETTINGS FOR FRAME fPage6
   Custom                                                               */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME fPage6     = 
                "Destino":U.

ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME fPage6     = 
                "Execuá∆o":U.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wReport)
THEN wReport:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage6
/* Query rebuild information for FRAME fPage6
     _Query            is NOT OPENED
*/  /* FRAME fPage6 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON END-ERROR OF wReport /* Emissor DANFE - NF-e - FT0518 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON WINDOW-CLOSE OF wReport /* Emissor DANFE - NF-e - FT0518 */
DO:
  /* This event will close the window and terminate the procedure.  */
  {report/logfin.i}
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME bt-arquivo-bloq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-bloq wReport
ON CHOOSE OF bt-arquivo-bloq IN FRAME fPage6 /*   */
DO:
    def var c-arq-conv  as char no-undo.

    assign c-arq-conv = replace(input frame fPage6 c-arquivo-bloq, "/", CHR(92)).

    &IF "{&PDF}" = "YES" &THEN 

    IF NOT usePDF() THEN

    &ENDIF
     SYSTEM-DIALOG GET-FILE c-arq-conv
        FILTERS "*.lst" "*.lst",
                "*.*" "*.*"
        ASK-OVERWRITE
        DEFAULT-EXTENSION "lst"
        INITIAL-DIR "spool"
        SAVE-AS
        USE-FILENAME
        UPDATE l-ok.

    &IF "{&PDF}" = "YES" &THEN
    ELSE
       SYSTEM-DIALOG GET-FILE c-arq-conv
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
        assign c-arquivo-bloq = replace(c-arq-conv, CHR(92), "/").
        display c-arquivo-bloq with frame fPage6.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-config-impr-bloq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr-bloq wReport
ON CHOOSE OF bt-config-impr-bloq IN FRAME fPage6 /*   */
DO:
    def var cTempFile as char no-undo.
    def var cPrinter  as char no-undo.
    def var cAuxFile  as char no-undo.
    def var cLayout   as char no-undo.
    def var cPrev     as char no-undo.

    assign cPrev     = c-arquivo-bloq:screen-value in frame fPage6
           cTempFile = replace(c-arquivo-bloq:screen-value in frame fPage6,":":U,",":U).
    if c-arquivo-bloq:screen-value in frame fPage6 <> "" then do:
      if num-entries(cTempFile) = 4 then
        assign cPrinter = entry(1,cTempFile)
               cLayout  = entry(2,cTempFile)
               cAuxFile = entry(3,cTempFile) + ":":U + entry(4,cTempFile).
      if num-entries(cTempFile) = 3 then
        assign cPrinter = entry(1,cTempFile)
               cLayout  = entry(2,cTempFile)
               cAuxFile = entry(3,cTempFile).
      if num-entries(cTempFile) = 2 then
        assign cPrinter = entry(1,cTempFile)
               cLayout  = entry(2,cTempFile)
               cAuxFile = "".
    end.

    run utp/ut-impr.w (input-output cPrinter, input-output cLayout, input-output cAuxFile).

    if cAuxFile = "" then
      assign c-arquivo-bloq = cPrinter + ":":U + cLayout.
    else
      assign c-arquivo-bloq = cPrinter + ":":U + cLayout + ":":U + cAuxFile.

    if c-arquivo-bloq = ":":U then
      assign c-arquivo-bloq = cPrev.

    assign c-imp-old = c-arquivo-bloq.

    disp c-arquivo-bloq with frame fPage6.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage4
&Scoped-define SELF-NAME bt-next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-next wReport
ON CHOOSE OF bt-next IN FRAME fPage4 /* Button 2 */
DO:
    if avail portador then do:
       assign c-oper = "+"
              c-bloq = c-num-bloq:screen-value in frame fPage4.

       run ftp/ft0503c.p (input  rowid(portador),
                                c-bloq,
                                c-oper,
                         output c-bloq-digito).
       run pi-mostra-numero.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-setaNf-eMonitor wReport 
PROCEDURE pi-monitor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM p-cod-estabel LIKE nota-fiscal.cod-estabel NO-UNDO.
DEF INPUT PARAM p-serie       LIKE nota-fiscal.serie       NO-UNDO.
DEF INPUT PARAM p-nr-nota-fis LIKE nota-fiscal.nr-nota-fis NO-UNDO.
DEF INPUT PARAM pl-monitor    AS LOG                       NO-UNDO.


ASSIGN l-monitorNf-e = pl-monitor
       c-cod-estabel-ft0909 = p-cod-estabel  
       c-serie-ft0909       = p-serie        
       c-nr-nota-fis-ft0909 = p-nr-nota-fis. 

RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME bt-prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-prev wReport
ON CHOOSE OF bt-prev IN FRAME fPage4 /* Button 1 */
DO:
    if avail portador then do:
       assign c-oper = "-"
              c-bloq = c-num-bloq:screen-value in frame fPage4.

       run ftp/ft0503c.p (input  rowid(portador),
                                c-bloq,
                                c-oper,
                         output c-bloq-digito).
       run pi-mostra-numero.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wReport
ON CHOOSE OF btCancel IN FRAME fpage0 /* Fechar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME btConfigImpr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConfigImpr wReport
ON CHOOSE OF btConfigImpr IN FRAME fPage6
DO:
   {report/rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFile wReport
ON CHOOSE OF btFile IN FRAME fPage6
DO:
/*     {report/rparq.i} */
    def var c-arq-conv  as char no-undo.

    assign c-arq-conv = replace(input frame fPage6 cFile, "/", CHR(92))
           c-arq-conv = SUBSTRING(c-arq-conv,1,1) + REPLACE(SUBSTRING(c-arq-conv,2),"~\~\","~\").
          
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
        assign cFile = replace(c-arq-conv, CHR(92), "/").
        display cFile with frame fPage6.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wReport
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wReport
ON CHOOSE OF btOK IN FRAME fpage0 /* Executar */
DO:
   do  on error undo, return no-apply:
       run piExecute.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME c-nr-nota-fis-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-nota-fis-fim wReport
ON F5 OF c-nr-nota-fis-fim IN FRAME fPage2
DO:
    {include/zoomvar.i &prog-zoom = dizoom/z03di135.w
                       &campo=c-cod-estabel
                       &campozoom=cod-estabel
                       &FRAME=fPage2
                       &campo2=c-serie
                       &campozoom2=serie
                       &frame2=fPage2
                       &campo3=c-nr-nota-fis-fim
                       &campozoom3=nr-nota-fis
                       &frame3=fPage2
                       &parametros="run pi-seta-inicial in wh-pesquisa
                                      (input frame fPage2 c-cod-estabel,
                                       input frame fPage2 c-serie,
                                       input frame fPage2 c-nr-nota-fis-ini)."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-nota-fis-fim wReport
ON MOUSE-SELECT-DBLCLICK OF c-nr-nota-fis-fim IN FRAME fPage2
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-nr-nota-fis-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-nota-fis-ini wReport
ON F5 OF c-nr-nota-fis-ini IN FRAME fPage2 /* Nr Nota Fiscal */
DO:
    {include/zoomvar.i &prog-zoom = dizoom/z03di135.w
                       &campo=c-cod-estabel
                       &campozoom=cod-estabel
                       &frame=fPage2
                       &campo2=c-serie
                       &campozoom2=serie
                       &frame2=fPage2
                       &campo3=c-nr-nota-fis-ini
                       &campozoom3=nr-nota-fis
                       &frame3=fPage2
                       &parametros="run pi-seta-inicial in wh-pesquisa
                                      (input frame fPage2 c-cod-estabel,
                                       input frame fPage2 c-serie,
                                       input frame fPage2 c-nr-nota-fis-ini)." }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-serie wReport
ON LEAVE OF c-serie IN FRAME fPage2 /* Serie */
DO:

    IF  CAN-FIND (FIRST ser-estab                                 
                  WHERE ser-estab.cod-estabel = INPUT FRAME fPage2 c-cod-estabel      
                    AND ser-estab.serie       = INPUT FRAME fPage2 c-serie
                    AND &IF "{&bf_dis_versao_ems}":U >= "2.08":U &THEN    
                            ser-estab.log-word-danfe                      
                        &ElSE                                             
                            substring(ser-estab.char-1,70,1) = "S":U   
                        &ENDIF) THEN 
        DISABLE i-nr-copias WITH FRAME fPage2.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-nota-fis-ini wReport
ON LEAVE OF c-nr-nota-fis-ini IN FRAME fPage2 /* Nr Nota Fiscal */
DO:

    IF  c-cod-estabel:SCREEN-VALUE IN FRAME fPage2 = "" AND
        c-serie:SCREEN-VALUE       IN FRAME fPage2 = "" THEN DO:

        FIND FIRST nota-fiscal NO-LOCK
             WHERE nota-fiscal.nr-nota-fis >= INPUT FRAME fPage2 c-nr-nota-fis-ini
               AND nota-fiscal.nr-nota-fis <= INPUT FRAME fPage2 c-nr-nota-fis-fim NO-ERROR.
        IF AVAIL nota-fiscal THEN
            ASSIGN c-cod-estabel:SCREEN-VALUE IN FRAME fPage2 = nota-fiscal.cod-estabel
                   c-serie:SCREEN-VALUE       IN FRAME fPage2 = nota-fiscal.serie.
    END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-nr-copias wReport
ON LEAVE OF i-nr-copias IN FRAME fPage2 /* Nr Copias */
DO:

    IF  i-nr-copias:SCREEN-VALUE IN FRAME fPage2 = "0" OR 
        i-nr-copias:SCREEN-VALUE IN FRAME fPage2 = "?" THEN
        ASSIGN i-nr-copias:SCREEN-VALUE IN FRAME fPage2 = "1".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-nota-fis-ini wReport
ON MOUSE-SELECT-DBLCLICK OF c-nr-nota-fis-ini IN FRAME fPage2 /* Nr Nota Fiscal */
DO:
    apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME cFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFile wReport
ON LEAVE OF cFile IN FRAME fPage6
DO:
    IF rsExecution = 1 THEN
        ASSIGN c-arq-old = cFile:SCREEN-VALUE.
    ELSE
        ASSIGN c-arq-old-batch = cFile:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME da-dt-saida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL da-dt-saida wReport
ON LEAVE OF da-dt-saida IN FRAME fPage2 /* Dt Sa°da */
DO:
  {cdp/cd9998.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage4
&Scoped-define SELF-NAME i-bloq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-bloq wReport
ON VALUE-CHANGED OF i-bloq IN FRAME fPage4 /* Emitir Bloqueto */
DO:
    {utp/ut-liter.i Lay-out * L}
   if i-bloq:checked in frame fPage4 = yes then do:
      
      {utp/ut-liter.i N£mero_Bloqueto * L}
      assign text-3:screen-value in frame fPage4 = return-value.

      assign c-arquivo-bloq:sensitive    in frame fPage6  = yes
             bt-arquivo-bloq:visible     in frame fPage6  = yes
             bt-arquivo-bloq:SENSITIVE   in frame fPage6  = YES
             bt-config-impr-bloq:visible in frame fPage6  = YES.

      enable rs-destino-bloq with frame fPage6.
      enable rs-banco        with frame fPage4.
      apply 'value-changed' to rs-destino-bloq in frame fPage6.

      FRAME fPage4:MOVE-TO-TOP().

      IF  NOT AVAIL param-global THEN
          find first param-global no-lock no-error.

      FIND FIRST nota-fiscal NO-LOCK
           WHERE nota-fiscal.cod-estabel = INPUT FRAME fPage2 c-cod-estabel
             AND nota-fiscal.serie       = INPUT FRAME fPage2 c-serie
             AND nota-fiscal.nr-nota-fis = INPUT FRAME fPage2 c-nr-nota-fis-ini NO-ERROR.

      IF  NOT AVAIL nota-fiscal
      AND INPUT FRAME fPage2 de-cdd-embarq-ini <> 0 THEN
          FIND FIRST nota-fiscal WHERE
            nota-fiscal.cdd-embarq = INPUT FRAME fPage2 de-cdd-embarq-ini NO-LOCK NO-ERROR.

      IF AVAIL nota-fiscal THEN DO:
         assign i-empresa = param-global.empresa-prin.

         &if defined (bf_dis_consiste_conta) &then

             FIND FIRST estabelec
                  WHERE estabelec.cod-estabel = nota-fiscal.cod-estabel NO-LOCK NO-ERROR.

             run cdp/cd9970.p (input rowid(estabelec),
                               output i-empresa).
         &endif

         FIND FIRST portador use-index codigo no-lock
              WHERE portador.ep-codigo    = i-empresa
                AND portador.cod-portador = nota-fiscal.cod-portador
                AND portador.modalidade   = nota-fiscal.modalidade no-error.
         if avail portador THEN
            case portador.cod-febraban:
              when 237 THEN
                   assign rs-banco:screen-value in frame fPage4 = "Bradesco".
              when 001 THEN
                   assign rs-banco:screen-value in frame fPage4 = "Banco do Brasil".
              when 341 THEN
                   assign rs-banco:screen-value in frame fPage4 = "Banco Ita£".
              when 346 THEN
                   assign rs-banco:screen-value in frame fPage4 = "Banco Francàs e Brasileiro".
              when 399 THEN
                   assign rs-banco:screen-value in frame fPage4 = "Banco Bamerindus".
              when 231 THEN
                   assign rs-banco:screen-value in frame fPage4 = "Banco Boa Vista".
              when 275 THEN
                   assign rs-banco:screen-value in frame fPage4 = "Banco Real".
              when 237 then
                   assign rs-banco:screen-value in frame fPage4 = "Bradesco 80 colunas".
              when 422 then
                   assign rs-banco:screen-value in frame fPage4 = "Banco Safra".
            end.
      end.
      apply 'value-changed' to rs-banco in frame fPage4.
      assign text-3:visible     in frame fPage4 = yes.

      ASSIGN rect-8:VISIBLE     IN FRAME fPage4 = YES
             bt-prev:VISIBLE    IN FRAME fPage4 = YES
             bt-next:VISIBLE    IN FRAME fPage4 = YES
             c-num-bloq:VISIBLE IN FRAME fPage4 = YES
             c-num-bloq:SENSITIVE IN FRAME fPage4 = NO.
   end.
   else do:
      assign c-arquivo-bloq:sensitive in frame fPage6  = no
             bt-arquivo-bloq:visible                   = no
             bt-config-impr-bloq:visible               = NO.
      disable rs-destino-bloq
              bt-arquivo-bloq with frame fPage6.
      disable rs-banco with frame fPage4.

      assign c-arquivo-bloq:visible in frame fPage6 = no.

      assign text-3:visible     in frame fPage4 = no
             rect-8:visible     in frame fPage4 = no
             bt-prev:visible    in frame fPage4 = no
             bt-next:visible    in frame fPage4 = no
             c-num-bloq:visible in frame fPage4 = no.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME de-cdd-embarq-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL de-cdd-embarq-fim wReport
ON F5 OF de-cdd-embarq-fim IN FRAME fPage2
DO:
    {include/zoomvar.i &prog-zoom = "dizoom/z01di041.w"
                       &campo = de-cdd-embarq-fim
                       &campozoom = cdd-embarq
                       &FRAME = fPage2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL de-cdd-embarq-fim wReport
ON MOUSE-SELECT-DBLCLICK OF de-cdd-embarq-fim IN FRAME fPage2
DO:
    apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME de-cdd-embarq-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL de-cdd-embarq-ini wReport
ON F5 OF de-cdd-embarq-ini IN FRAME fPage2 /* Embarque */
DO:
    {include/zoomvar.i &prog-zoom = "dizoom/z01di041.w"
                     &campo = de-cdd-embarq-ini
                     &campozoom = cdd-embarq
                     &frame=fPage2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL de-cdd-embarq-ini wReport
ON MOUSE-SELECT-DBLCLICK OF de-cdd-embarq-ini IN FRAME fPage2 /* Embarque */
DO:
    apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage4
&Scoped-define SELF-NAME rs-banco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-banco wReport
ON VALUE-CHANGED OF rs-banco IN FRAME fPage4
DO:
    case input frame fPage4 rs-banco:
         when "Bradesco" THEN
              assign i-cod-febraban = 237.
         when "Banco do Brasil" THEN
              assign i-cod-febraban = 001.
         when "Banco Ita£" THEN
              assign i-cod-febraban = 341.
         when "Banco Francàs e Brasileiro" THEN
              assign i-cod-febraban = 346.
         when "Banco Bamerindus" THEN
              assign i-cod-febraban = 399.
         when "Banco Boa Vista" THEN
              assign i-cod-febraban = 231.
         when "Banco Real" THEN
              assign i-cod-febraban = 275.
         when "Bradesco 80 colunas" then
              assign i-cod-febraban = 237.
         when "Banco Safra" then
              assign i-cod-febraban = 422.
    end.

    if rs-banco:sensitive in frame fPage4 = yes then do:
       assign c-num-bloq:screen-value in frame fPage4 = "".

       find first param-global no-lock no-error.

       assign i-empresa = param-global.empresa-prin.

       &if defined (bf_dis_consiste_conta) &then

           FIND FIRST estabelec
                WHERE estabelec.cod-estabel = INPUT FRAME fPage2 c-cod-estabel NO-LOCK NO-ERROR.

           IF AVAIL estabelec THEN
               run cdp/cd9970.p (input rowid(estabelec),
                                 output i-empresa).
       &endif

        ASSIGN l-embarque = NO.

        /* busca por embarque */
        IF  INT(INPUT FRAME fPage2 c-nr-nota-fis-ini) = 0
        AND INPUT FRAME fPage2 de-cdd-embarq-ini <> 0 THEN
            FOR EACH nota-fiscal FIELDS (cod-estabel serie cdd-embarq cod-portador modalidade) NO-LOCK
               WHERE nota-fiscal.cdd-embarq >= INPUT FRAME fPage2 de-cdd-embarq-ini
                 AND nota-fiscal.cdd-embarq <= INPUT FRAME fPage2 de-cdd-embarq-fim:

                IF nota-fiscal.cod-estabel <> INPUT FRAME fPage2 c-cod-estabel
                OR nota-fiscal.serie <> INPUT FRAME fPage2 c-serie THEN
                    NEXT.

                RUN pi-busca-numero (INPUT nota-fiscal.cod-portador,
                                     INPUT nota-fiscal.modalidade).

                ASSIGN l-embarque = YES.
            END.

        IF NOT l-embarque THEN
            FOR EACH nota-fiscal FIELDS (cod-estabel serie nr-nota-fis cod-portador modalidade) NO-LOCK
               WHERE nota-fiscal.cod-estabel  = INPUT FRAME fPage2 c-cod-estabel
                 AND nota-fiscal.serie        = INPUT FRAME fPage2 c-serie
                 AND nota-fiscal.nr-nota-fis >= INPUT FRAME fPage2 c-nr-nota-fis-ini
                 AND nota-fiscal.nr-nota-fis <= INPUT FRAME fPage2 c-nr-nota-fis-fim:

                RUN pi-busca-numero (INPUT nota-fiscal.cod-portador,
                                     INPUT nota-fiscal.modalidade).
            END.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME rs-destino-bloq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino-bloq wReport
ON ANY-KEY OF rs-destino-bloq IN FRAME fPage6
DO:
    BELL.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino-bloq wReport
ON MOUSE-SELECT-CLICK OF rs-destino-bloq IN FRAME fPage6
DO:
    DO WITH FRAME fPage6:
        APPLY 'VALUE-CHANGED' TO rs-destino-bloq.
        DO  WITH FRAME fPage6:
            IF  SELF:SCREEN-VALUE = "1":U THEN
                ASSIGN c-arquivo-bloq:VISIBLE       = YES
                       bt-config-impr-bloq:VISIBLE  = YES.
            ELSE
                ASSIGN bt-config-impr-bloq:VISIBLE  = NO.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino-bloq wReport
ON VALUE-CHANGED OF rs-destino-bloq IN FRAME fPage6
DO:
    do  with frame fPage6:
       case self:screen-value:
          when "1" then do:
              if c-destino-old = "2" then assign c-impressora-old = c-arquivo-bloq:screen-value.
              assign c-arquivo-bloq:sensitive    = YES
                     c-destino-old               = "1"
                     c-arquivo-bloq:visible      = YES
                     c-arquivo-bloq:screen-value = c-arquivo-old
                     bt-arquivo-bloq:visible     = no
                     bt-config-impr-bloq:visible = YES.
            end.

            when "2" then do:
               if c-destino-old = "1" then assign c-arquivo-old = c-arquivo-bloq:screen-value.
               assign c-arquivo-bloq:sensitive     = yes
                      c-destino-old                = "2"
                      c-arquivo-bloq:visible       = yes
                      c-arquivo-bloq:screen-value  = c-impressora-old
                      bt-arquivo-bloq:visible      = yes
                      bt-config-impr-bloq:visible  = no.
            end.

            when "3" then do:
               if c-destino-old = "2" then assign c-impressora-old = c-arquivo-bloq:screen-value.
               if c-destino-old = "1" then assign c-arquivo-old = c-arquivo-bloq:screen-value.
               assign c-arquivo-bloq:sensitive     = no
                      c-destino-old                = "3"
                      c-arquivo-bloq:visible       = no
                      bt-arquivo-bloq:visible      = no
                      bt-config-impr-bloq:visible  = no.
            end.
       end case.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsDestiny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDestiny wReport
ON ANY-KEY OF rsDestiny IN FRAME fPage6
DO:
    BELL.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDestiny wReport
ON MOUSE-SELECT-CLICK OF rsDestiny IN FRAME fPage6
DO:
    DO WITH FRAME fPage6:
        APPLY 'VALUE-CHANGED' TO rsDestiny.
        DO  WITH FRAME fPage6:
            IF  SELF:SCREEN-VALUE = "1":U THEN
                ASSIGN rs-impressora:VISIBLE   = YES
                       cFile:VISIBLE           = NO
                       btConfigImpr:VISIBLE    = NO.
            ELSE
                ASSIGN rs-impressora:VISIBLE   = NO.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDestiny wReport
ON VALUE-CHANGED OF rsDestiny IN FRAME fPage6
DO:
    do  with frame fPage6:
        case self:screen-value:
            when "1":U then do:
                assign cFile:sensitive         = NO 
                       cFile:visible           = YES
                       btFile:visible          = no
                       btConfigImpr:visible    = YES.
            end.
            when "2":U then do:
                assign cFile:sensitive       = yes
                       cFile:visible         = yes
                       btFile:visible        = yes
                       btConfigImpr:visible  = no.
            end.
            when "3":U then do:
                assign cFile:visible         = no
                       cFile:sensitive       = no
                       btFile:visible        = no
                       btConfigImpr:visible  = no.
            END.
        end case.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsExecution
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsExecution wReport
ON VALUE-CHANGED OF rsExecution IN FRAME fPage6
DO:
    {report/rprse.i} 
    
    IF rsExecution = 2 THEN
       rs-destino-bloq:DISABLE(c-terminal) IN FRAME fPage6.
    ELSE
       rs-destino-bloq:ENABLE(c-terminal) IN FRAME fPage6.

    IF rsDestiny:SCREEN-VALUE = '1' THEN
       ASSIGN cFile:VISIBLE      IN FRAME fPage6 = NO
               btConfigImpr:VISIBLE IN FRAME fPage6 = NO.

    IF rsExecution = 2 AND i-bloq:CHECKED IN FRAME fPage4 = YES THEN
    DO:
        IF rs-destino-bloq:SCREEN-VALUE IN FRAME fPage6 = '1' THEN
            ASSIGN c-arquivo-bloq:VISIBLE      IN FRAME fPage6 = YES
                   bt-config-impr-bloq:VISIBLE IN FRAME fPage6 = YES.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wReport 

{report/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDestroyInterface wReport 
PROCEDURE afterDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {cdp/cd0019.i1} /* finaliza??o da seguran?a por estabelecimento */

END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wReport 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF l-monitorNf-e THEN
        RUN pi-setaNf-eMonitor.
    APPLY "VALUE-CHANGED"      TO i-bloq    IN FRAME fPage4.
    APPLY "MOUSE-SELECT-CLICK" TO rsDestiny IN FRAME fPage6.
    APPLY "MOUSE-SELECT-CLICK" TO rs-destino-bloq IN FRAME fPage6.

    ASSIGN rs-destino-bloq:screen-value  in frame fPage6 = "3":U.

    ASSIGN c-terminal = " Terminal".

    if  c-nr-nota-fis-ini:load-mouse-pointer   ("image/lupa.cur") in frame fPage2 then.
    if  c-nr-nota-fis-fim:load-mouse-pointer   ("image/lupa.cur") in frame fPage2 then.
    if  de-cdd-embarq-ini:load-mouse-pointer ("image/lupa.cur") in frame fPage2 then.
    if  de-cdd-embarq-fim:load-mouse-pointer ("image/lupa.cur") in frame fPage2 then.

    ASSIGN rs-impressora:LIST-ITEMS   IN FRAME fPage6 = SESSION:GET-PRINTERS() NO-ERROR. /* Localiza Impressoras do Windows */
    ASSIGN rs-impressora:SCREEN-VALUE IN FRAME fPage6 = SESSION:PRINTER-NAME NO-ERROR. /* Impressora default como padr∆o  */
    IF  rs-impressora:SCREEN-VALUE IN FRAME fPage6 = ""
    OR  rs-impressora:SCREEN-VALUE IN FRAME fPage6= ? THEN
        ASSIGN rs-impressora:SCREEN-VALUE IN FRAME fPage6 = ENTRY(1,rs-impressora:LIST-ITEMS IN FRAME fPage6) NO-ERROR.

    assign v-cod-prog-gerado = "FT0518":U.

    find usuar_mestre where usuar_mestre.cod_usuario = c-seg-usuario no-lock no-error.
    IF NOT CAN-FIND(FIRST funcao NO-LOCK
                    WHERE funcao.cd-funcao = "spp-danfe"
                    AND   funcao.ativo) THEN DO:
        if avail usuar_mestre then
            assign cFile = if length(usuar_mestre.nom_subdir_spool) <> 0
                then caps(replace(usuar_mestre.nom_dir_spool, "~\", "~/") + "~/" + replace(usuar_mestre.nom_subdir_spool, "~\", "~/") + "~/" + v-cod-prog-gerado + "~." + v-cod-extens-arq)
                else caps(replace(usuar_mestre.nom_dir_spool, "~\", "~/") + "~/" + v-cod-prog-gerado + "~." + v-cod-extens-arq).
        else
            assign cFile = caps("spool~/" + v-cod-prog-gerado + "~." + v-cod-extens-arq).
    END.
    ELSE DO:
        if avail usuar_mestre then
            assign cFile = if length(usuar_mestre.nom_subdir_spool) <> 0
                then caps(replace(usuar_mestre.nom_dir_spool, "~\", "~/") + "~/" + replace(usuar_mestre.nom_subdir_spool, "~\", "~/") + "~/" + v-cod-prog-gerado + string(RANDOM(1,9999999)) + "~." + v-cod-extens-arq)
                else caps(replace(usuar_mestre.nom_dir_spool, "~\", "~/") + "~/" + v-cod-prog-gerado + string(RANDOM(1,9999999)) + "~." + v-cod-extens-arq).
        else
            assign cFile = caps("spool~/" + v-cod-prog-gerado + string(RANDOM(1,9999999)) + "~." + v-cod-extens-arq).
    END.

    c-arq-old = cFile.

    APPLY "LEAVE":U TO c-serie IN FRAME fPage2.
    
    /* Para desabilitar as opá‰es de impress∆o do DANFE */
    disable rsDestiny rsExecution with frame fPage6.
    assign cFile:hidden           in frame fPage6 = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-setaNf-eMonitor wReport 
PROCEDURE pi-setaNf-eMonitor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN c-cod-estabel    :SCREEN-VALUE IN FRAME fPage2 = c-cod-estabel-ft0909 
           c-serie          :SCREEN-VALUE IN FRAME fPage2 = c-serie-ft0909       
           c-nr-nota-fis-ini:SCREEN-VALUE IN FRAME fPage2 = c-nr-nota-fis-ft0909 
           c-nr-nota-fis-fim:SCREEN-VALUE IN FRAME fPage2 = c-nr-nota-fis-ft0909.
                                                                                 
RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-numero wReport 
PROCEDURE pi-busca-numero :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM p-cod-portador LIKE nota-fiscal.cod-portador.
    DEF INPUT PARAM p-modalidade   LIKE nota-fiscal.modalidade.

    FIND FIRST portador
         WHERE portador.ep-codigo    = i-empresa
           AND portador.cod-portador = p-cod-portador
           AND portador.modalidade   = p-modalidade 
           AND portador.cod-febraban = i-cod-febraban NO-LOCK NO-ERROR.
    
    IF AVAIL portador
    AND portador.emite-bloq = 1 THEN DO:
        ASSIGN i-cod-portador = portador.cod-portador.
        IF portador.char-1 <> "" THEN DO:

            ASSIGN i-tamanho-bloq = LENGTH(portador.char-1)
                   c-bloq = portador.char-1
                   c-oper = "". 

            RUN ftp/ft0503c.p (INPUT  ROWID(portador),
                               INPUT  c-bloq,
                               INPUT  c-oper,
                               OUTPUT c-bloq-digito).
            RUN pi-mostra-numero.
            LEAVE.
        END.       
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar-bloq wReport 
PROCEDURE pi-executar-bloq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

&IF DEFINED(PGIMP) <> 0 AND "{&PGIMP}":U = "YES":U &THEN
    
DO ON ERROR UNDO, RETURN ERROR ON STOP  UNDO, RETURN ERROR:     
   
    {report/rpexa.i}

    if  input frame fPage6 rs-destino-bloq = 2 then do:
        run utp/ut-vlarq.p (INPUT INPUT frame fPage6 c-arquivo-bloq).
        if  return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show",
                               input 73,
                               input "").
            apply 'entry':U to c-arquivo-bloq in frame fPage6.
            return error.
        end.
    end.

    if  input frame fPage6 rs-destino-bloq = 1 then do:
        run utp/ut-msgs.p (input "show",
                           input 302,
                           input "").
        if  return-value = "no":U then do:
            apply 'entry':U to rs-destino-bloq in frame fPage6.
            return error.
        end.
    end.

	empty temp-table tt-param-aux.

    create tt-param-aux.
    assign tt-param-aux.usuario              = c-seg-usuario
           tt-param-aux.destino              = input frame fPage6 rs-destino-bloq
           tt-param-aux.data-exec            = today
           tt-param-aux.hora-exec            = time
           tt-param-aux.v_num_tip_aces_usuar = v_num_tip_aces_usuar
           tt-param-aux.ep-codigo            = i-ep-codigo-usuario
           tt-param-aux.c-cod-estabel        = input frame fPage2 c-cod-estabel
           tt-param-aux.c-serie              = input frame fPage2 c-serie
           tt-param-aux.c-nr-nota-fis-ini    = input frame fPage2 c-nr-nota-fis-ini
           tt-param-aux.c-nr-nota-fis-fim    = input frame fPage2 c-nr-nota-fis-fim
           tt-param-aux.de-cdd-embarq-ini    = input frame fPage2 de-cdd-embarq-ini
           tt-param-aux.de-cdd-embarq-fim    = input frame fPage2 de-cdd-embarq-fim
           tt-param-aux.da-dt-saida          = input frame fPage2 da-dt-saida
           tt-param-aux.c-hr-saida           = input frame fPage2 c-hr-saida
           tt-param-aux.nr-copias            = input frame fPage2 i-nr-copias
           tt-param-aux.prox-bloq            = input frame fPage4 c-num-bloq
           tt-param-aux.cod-febraban         = i-cod-febraban
           tt-param-aux.cod-portador         = i-cod-portador
           tt-param-aux.imprime-bloq         = i-bloq:checked in frame fPage4
           tt-param-aux.rs-imprime           = input frame fPage2 rs-imprime.

    FIND FIRST ser-estab NO-LOCK
         WHERE ser-estab.cod-estabel  = tt-param-aux.c-cod-estabel
           AND ser-estab.serie        = tt-param-aux.c-serie NO-ERROR.
    
      IF  AVAIL ser-estab THEN DO:

          &if "{&bf_dis_versao_ems}"  >=  "2.07":U &then
             IF ser-estab.idi-format-emis-danfe = 1 THEN
                  ASSIGN tt-param-aux.cod-layout = "DANFE-Mod.1":U.
             ELSE
                 IF ser-estab.idi-format-emis-danfe = 2 THEN
                      ASSIGN tt-param-aux.cod-layout = "DANFE-Mod.2":U.
          &else
             IF INT(SUBSTRING(ser-estab.char-1,4,01)) = 1 THEN
                 ASSIGN tt-param-aux.cod-layout = "DANFE-Mod.1":U.
             ELSE
                 IF INT(SUBSTRING(ser-estab.char-1,4,01)) = 2 THEN
                     ASSIGN tt-param-aux.cod-layout = "DANFE-Mod.2":U.
          &endif
      END.
   
    IF tt-param-aux.cod-layout = "" THEN
       ASSIGN tt-param-aux.cod-layout = "DANFE-Mod.1":U.
    
    case input frame fPage4 rs-banco: 
         when "Bradesco" then 
              assign tt-param-aux.banco = 1.
         when "Banco do Brasil" then     
              assign tt-param-aux.banco = 2.
         when "Banco Ita£" then     
              assign tt-param-aux.banco = 3.
         when "Banco Francàs e Brasileiro" then     
              assign tt-param-aux.banco = 4.
         when "Banco Bamerindus" then      
              assign tt-param-aux.banco = 5.
         when "Banco Boa Vista" then 
              assign tt-param-aux.banco = 6.
         when "Banco Real" then 
              assign tt-param-aux.banco = 7.
         when "Bradesco 80 colunas" then
              assign tt-param-aux.banco = 8.     
         when "Banco Safra" then
              assign tt-param-aux.banco = 9.
    end.           

    if  tt-param-aux.destino = 1 then
        assign tt-param-aux.arquivo = ""
               c-arquivo-salvo  = cFile:screen-value in frame fPage6
               cFile:screen-value in frame fPage6 = input frame fPage6 c-arquivo-bloq.
    else
    if  tt-param-aux.destino = 2 then 
        assign tt-param-aux.arquivo = input frame fPage6 c-arquivo-bloq.
    else
        assign tt-param-aux.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

    /* Coloque aqui a l¢gica de gravaá∆o dos parÉmtros e seleá∆o na temp-table
       tt-param */ 

    CREATE tt-param.
    ASSIGN tt-param.destino          = tt-param-aux.destino                          
           tt-param.arquivo          = tt-param-aux.arquivo
           tt-param.usuario          = tt-param-aux.usuario
           tt-param.data-exec        = tt-param-aux.data-exec           
           tt-param.hora-exec        = tt-param-aux.hora-exec           
           tt-param.ini-cod-estabel  = tt-param-aux.c-cod-estabel        
           tt-param.fim-cod-estabel  = tt-param-aux.c-cod-estabel       
           tt-param.ini-serie        = tt-param-aux.c-serie               
           tt-param.fim-serie        = tt-param-aux.c-serie 
           tt-param.ini-cdd-embarq   = tt-param-aux.de-cdd-embarq-ini            
           tt-param.fim-cdd-embarq   = tt-param-aux.de-cdd-embarq-fim            
           tt-param.ini-nr-nota-fis  = tt-param-aux.c-nr-nota-fis-ini              
           tt-param.fim-nr-nota-fis  = tt-param-aux.c-nr-nota-fis-fim 
           tt-param.banco            = tt-param-aux.banco                   
           tt-param.cod-febraban     = tt-param-aux.cod-febraban                     
           tt-param.cod-portador     = tt-param-aux.cod-portador                  
           tt-param.prox-bloq        = tt-param-aux.prox-bloq
           tt-param.imprime-bloq     = tt-param-aux.imprime-bloq.  

    {report/rpexb.i}

    if  tt-param-aux.destino = 1 then
        assign cFile:screen-value in frame fPage6 = c-arquivo-salvo.

    if  session:set-wait-state("general":U) then.

    {report/rprun.i ftp/ft0503b.p "input table tt-param"}

    {report/rpexc.i}

    if  session:set-wait-state("") then.
  
    {report/rptrm.i}
       
end.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mostra-numero wReport 
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
                 c-formato      = if i-tamanho-bloq > 15 then "99999999999999999"
                                     else "99999999999".
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
     
     when 341 then do:
        if i-tamanho-bloq > 12 then do:
           assign i-tamanho-bloq = i-tamanho-bloq - 12
                  c-prox-bloq    = string(decimal(substring(c-bloq-digito, 13, i-tamanho-bloq)),"99999999")
                  c-formato      = "99999999".
        end.
        else do:
           assign i-tamanho-bloq = i-tamanho-bloq /* - 4 */
                  c-prox-bloq    = c-bloq-digito
                  c-formato      = "99999999".
       end.
     end.   
     when 346 then 
        if i-tamanho-bloq > 12 then
           assign i-tamanho-bloq = i-tamanho-bloq - 12
                  c-prox-bloq    = c-bloq-digito
                  c-formato      = "99999999".
        else
            assign i-tamanho-bloq = i-tamanho-bloq - 4
                   c-prox-bloq    = c-bloq-digito
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

assign c-num-bloq:screen-value in frame fPage4 = string(c-prox-bloq, c-formato).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExecute wReport 
PROCEDURE piExecute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define var r-tt-digita as rowid no-undo.

&IF DEFINED(PGIMP) <> 0 AND "{&PGIMP}":U = "YES":U &THEN
/*:T** Relatorio ***/
do on error undo, return error on stop  undo, return error:
    {report/rpexa.i}

    IF (SEARCH("layout/danfev2mod1.rtf":U) = ? 
        OR SEARCH("layout/danfev2mod1.rtf":U) = "")
    AND (SEARCH("layout/danfev2mod2.rtf":U) = ?
         OR SEARCH("layout/danfev2mod2.rtf":U) = "") THEN DO: 
        run utp/ut-msgs.p (input "show":U,
                           input  31653,
                           input "Layout n∆o encontrado.":U + "~~" + "Layout para a impress∆o do Danfe n∆o consta na pasta Layout.":U).
    END.
    ELSE DO:        
            if input frame fPage6 rsDestiny = 2 then
            do:
                run utp/ut-vlarq.p (input input frame fPage6 cFile).
                if return-value = "NOK":U then
                do:
                    run utp/ut-msgs.p (input "show":U,
                                       input 73,
                                       input "":U).
                    apply "ENTRY":U to cFile in frame fPage6.
                    return error.
                end.
            end.
        
            /*:T Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem
               apresentar uma mensagem de erro cadastrada, posicionar na p†gina com
               problemas e colocar o focus no campo com problemas */
        
            IF input frame fPage2 da-dt-saida <> ? THEN
                 FIND FIRST nota-fiscal
                            WHERE nota-fiscal.cod-estabel  = input frame fPage2 c-cod-estabel
                              AND nota-fiscal.serie        = input frame fPage2 c-serie
                              AND nota-fiscal.nr-nota-fis >= input frame fPage2 c-nr-nota-fis-ini
                              AND nota-fiscal.nr-nota-fis <= input frame fPage2 c-nr-nota-fis-fim
                              AND nota-fiscal.dt-emis-nota > input frame fPage2 da-dt-saida NO-LOCK NO-ERROR.
                IF AVAIL nota-fiscal THEN DO:
                    run utp/ut-msgs.p (input "show",
                                        input 7722,  /* Dt Emiss∆o Ç maior que a dt de sa°da */
                                        input "").
                    apply 'entry' to da-dt-saida in frame fPage2.
                    return error.
                END.
        
            if input frame fPage2 c-nr-nota-fis-fim < input frame fPage2 c-nr-nota-fis-ini
            then do:
               run utp/ut-msgs.p (input "show",
                                   input 38,
                                   input "").
               apply 'entry' to c-nr-nota-fis-fim in frame fPage2.
               return error.
            end.
        
            if input frame fPage2 de-cdd-embarq-fim < input frame fPage2 de-cdd-embarq-ini
            then do:
               run utp/ut-msgs.p (input "show",
                                   input 38,
                                   input "").
               apply 'entry' to de-cdd-embarq-fim in frame fPage2.
               return error.
            end.
        
            if input frame fPage2 da-dt-saida <> "" and input frame fPage2 da-dt-saida
            <> ? then do:
               run utp/ut-msgs.p (input "show",
                                  input 364,
                                  input "").
               if return-value = "no" then do:
                  apply 'entry' to da-dt-saida in frame fPage2.
                  return error.
               end.
            end.
    
            if  v_cdn_empres_usuar <> ?
            then
                assign i-ep-codigo-usuario = v_cdn_empres_usuar.
        
        
            /*:T Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
               para o programa RP.P */
        
            create tt-param-aux.
            assign tt-param-aux.usuario              = c-seg-usuario
                   tt-param-aux.destino              = input frame fPage6 rsDestiny
                   tt-param-aux.data-exec            = today
                   tt-param-aux.hora-exec            = time
                   tt-param-aux.v_num_tip_aces_usuar = v_num_tip_aces_usuar
                   tt-param-aux.ep-codigo            = i-ep-codigo-usuario
                   tt-param-aux.c-cod-estabel        = input frame fPage2 c-cod-estabel
                   tt-param-aux.c-serie              = input frame fPage2 c-serie
                   tt-param-aux.c-nr-nota-fis-ini    = input frame fPage2 c-nr-nota-fis-ini
                   tt-param-aux.c-nr-nota-fis-fim    = input frame fPage2 c-nr-nota-fis-fim
                   tt-param-aux.de-cdd-embarq-ini    = input frame fPage2 de-cdd-embarq-ini
                   tt-param-aux.de-cdd-embarq-fim    = input frame fPage2 de-cdd-embarq-fim
                   tt-param-aux.da-dt-saida          = input frame fPage2 da-dt-saida
                   tt-param-aux.c-hr-saida           = input frame fPage2 c-hr-saida
                   tt-param-aux.nr-copias            = input frame fPage2 i-nr-copias
                   tt-param-aux.prox-bloq            = input frame fPage4 c-num-bloq
                   tt-param-aux.cod-febraban         = i-cod-febraban
                   tt-param-aux.cod-portador         = i-cod-portador
                   tt-param-aux.imprime-bloq         = i-bloq:checked in frame fPage4
                   tt-param-aux.rs-imprime           = input frame fPage2 rs-imprime
                   tt-param-aux.impressora-so        = rs-impressora:SCREEN-VALUE IN FRAME fPage6
                   tt-param-aux.impressora-so-bloq   = c-arquivo-bloq:SCREEN-VALUE IN FRAME fPage6.
        
            FIND FIRST ser-estab NO-LOCK
                 WHERE ser-estab.cod-estabel  = tt-param-aux.c-cod-estabel
                   AND ser-estab.serie        = tt-param-aux.c-serie NO-ERROR.
            
            IF  AVAIL ser-estab THEN DO:
    
                IF &if "{&bf_dis_versao_ems}"  >=  "2.07":U &then
                       ser-estab.idi-format-emis-danfe = 1 OR 
                       ser-estab.idi-format-emis-danfe = 2 
                   &else
                       INT(SUBSTRING(ser-estab.char-1,4,01)) = 1 OR
                       INT(SUBSTRING(ser-estab.char-1,4,01)) = 2  
                   &endif
                THEN DO:
                   run utp/ut-msgs.p (input "show",
                                       INPUT 52042,
                                       input "").
                   apply 'entry' to c-nr-nota-fis-fim in frame fPage2.
                   return error.
                end.
    
                &if "{&bf_dis_versao_ems}"  >=  "2.07":U &then
                   IF ser-estab.idi-format-emis-danfe = 1 THEN
                        ASSIGN tt-param-aux.cod-layout = "DANFE-Mod.1":U.
                   ELSE
                       IF ser-estab.idi-format-emis-danfe = 2 THEN
                            ASSIGN tt-param-aux.cod-layout = "DANFE-Mod.2":U.
                &else
                   IF INT(SUBSTRING(ser-estab.char-1,4,01)) = 1 THEN
                       ASSIGN tt-param-aux.cod-layout = "DANFE-Mod.1":U.
                   ELSE
                       IF INT(SUBSTRING(ser-estab.char-1,4,01)) = 2 THEN
                           ASSIGN tt-param-aux.cod-layout = "DANFE-Mod.2":U.
               &endif
            END.
           
            IF tt-param-aux.cod-layout = "" THEN
               ASSIGN tt-param-aux.cod-layout = "DANFE-Mod.1":U.
        
            case input frame fPage4 rs-banco:
                    when "Bradesco":U then
                         assign tt-param-aux.banco = 1.
                    when "Banco do Brasil":U then
                         assign tt-param-aux.banco = 2.
                    when "Banco Ita£":U then
                         assign tt-param-aux.banco = 3.
                    when "Banco Francàs e Brasileiro":U then
                         assign tt-param-aux.banco = 4.
                    when "Banco Bamerindus":U then
                         assign tt-param-aux.banco = 5.
                    when "Banco Boa Vista":U then
                         assign tt-param-aux.banco = 6.
                    when "Banco Real":U then
                         assign tt-param-aux.banco = 7.
                    when "Bradesco 80 colunas":U then
                         assign tt-param-aux.banco = 8.
                    when "Banco Safra":U then
                         assign tt-param-aux.banco = 9.
               end.
        
            if  tt-param-aux.destino = 2 then
                assign tt-param-aux.arquivo = input frame fPage6 cFile.
            else
                IF NOT CAN-FIND(FIRST funcao NO-LOCK
                    WHERE funcao.cd-funcao = "spp-danfe":U
                    AND   funcao.ativo) THEN
                    assign tt-param-aux.arquivo = session:temp-directory + "FT0518":U + REPLACE(STRING(TODAY,"99/99/99"),"/","") + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + "." + v-cod-extens-arq.
                ELSE
                    assign tt-param-aux.arquivo = session:temp-directory + "FT0518":U + REPLACE(STRING(TODAY,"99/99/99"),"/","") + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + string(RANDOM(1,9999999)) + "." + v-cod-extens-arq.
        
            if  session:set-wait-state("general":U) then.
                assign v-cod-prog-i-rprun = "ftp/ft0518rp.p":U.
        
            IF INPUT FRAME fPage6 rsExecution = 2 AND tt-param-aux.destino = 1 THEN
                ASSIGN tt-param-aux.destino = 4.
        
            raw-transfer tt-param-aux to raw-param.
        
            if input frame fPage6 rsExecution = 2 then do:
              run btb/btb911zb.p (input c-programa-mg97,
                                  input v-cod-prog-i-rprun,
                                  input c-versao-mg97,
                                  input 97,
                                  input input frame fPage6 cFile,
                                  input tt-param-aux.destino,
                                  input raw-param,
                                  input table tt-raw-digita,
                                  output i-num-ped-exec-rpw).
              if i-num-ped-exec-rpw <> 0 THEN
                run utp/ut-msgs.p (input "show", input 4169, input string(i-num-ped-exec-rpw)).
            end.
            else do:
              run value(v-cod-prog-i-rprun) (input raw-param, input table tt-raw-digita).
            end.
        
        
            if session:set-wait-state("") then.
               def var c-key-value as char no-undo.
        
            if  tt-param-aux.destino = 3 AND RETURN-VALUE <> "NOK" then do:
            
                IF v-cod-extens-arq = "lst":U THEN DO:
                   get-key-value section "Datasul_EMS2":U key    "Show-Report-Program":U value c-key-value.
                   if c-key-value = "":U or c-key-value = ?  then do:
                      assign c-key-value = "Notepad.exe":U.
                      put-key-value section "Datasul_EMS2":U key    "Show-Report-Program":U value c-key-value no-error.
                   end.
                   run winexec (input c-key-value + chr(32) + tt-param-aux.arquivo, input 1).
                END.
                IF v-cod-extens-arq = "pdf" THEN DO:
                   RUN OpenDocument(tt-param-aux.arquivo).
                END.
            
             end.
            
             if  i-bloq:checked in frame fPage4 = yes then do:
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
            
            /*:T Coloque aqui a l¢gica de gravaá∆o dos demais campos que devem ser passados
               como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */
        
            /*:T Executar do programa RP.P que ir† criar o relat¢rio */
        END.
end.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


