&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ra               PROGRESS
*/
&Scoped-define WINDOW-NAME w-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
  
  /************************Modificaá‰es*************/
  V07_R00_01 04/09/2014 = Foi colocado tratamento Pi-inicializar para controlar trazer default 
                          o recebimento Fisico ou fiscal dependendo da parametrizaá∆o

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
     
CREATE WIDGET-POOL.



     
def var c-programa-mg97 as char no-undo.
assign c-programa-mg97 = "esra100".

 {include/i-prgvrs.i ESRA100 2.06.00.000} 
 {utp/ut-glob.i} 
 {include/i-epc200.i} 
{utp/ut-vfsec.i}


/* ***************************  Definitions  ************************** */

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE c-status-sefaz  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-status-erp    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-erro-ret      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE c-sit-sefaz     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-sit-erp       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-arq-conv      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-ok            AS LOGICAL     NO-UNDO.
DEFINE VARIABLE i-cont          AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-diretorio     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-danfe-cte     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE h_bonfe001      AS HANDLE      NO-UNDO.
DEFINE VARIABLE h_esnfe208      AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-mensagem-erro AS CHARACTER   NO-UNDO.
DEFINE VARIABLE r-busca-dfe     AS ROWID       NO-UNDO.
DEFINE VARIABLE i-cor           AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-cor-bg        AS INTEGER     NO-UNDO.

DEFINE VARIABLE c-chave-busca-ini AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-chave-busca-fim AS CHARACTER   NO-UNDO.

DEFINE BUFFER bf-nfe-dfe FOR nfe-dfe.

DEFINE VARIABLE i-tot-docs AS INTEGER     NO-UNDO.

DEFINE BUFFER bf-nfe-it-param-rec FOR nfe-it-param-rec.

DEFINE VARIABLE l-danfe-ok AS LOGICAL     NO-UNDO.

DEFINE VARIABLE c-sit-triagem AS CHARACTER   NO-UNDO.

DEFINE VARIABLE h-esra015 AS HANDLE      NO-UNDO.
DEFINE VARIABLE i-cor-triagem AS INTEGER     NO-UNDO.

DEFINE VARIABLE r-nfe-dfe-aux AS ROWID       NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-doctos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES nfe-dfe

/* Definitions for BROWSE br-doctos                                     */
&Scoped-define FIELDS-IN-QUERY-br-doctos nfe-dfe.cod-estabel nfe-dfe.tipo nfe-dfe.serie nfe-dfe.nr-docto nfe-dfe.nome-abrev nfe-dfe.dt-emissao fc-sit-sefaz() @ c-sit-sefaz fc-sit-erp() @ c-sit-erp fc-sit-triagem() @ c-sit-triagem nfe-dfe.chave-acesso   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-doctos   
&Scoped-define SELF-NAME br-doctos
&Scoped-define QUERY-STRING-br-doctos FOR EACH nfe-dfe NO-LOCK          WHERE nfe-dfe.chave-acesso >= c-chave-busca-ini                    AND                nfe-dfe.chave-acesso <= c-chave-busca-fim                    AND              ((nfe-dfe.tipo = "NFE" AND INPUT tg-nfe = YES)                  OR               (nfe-dfe.tipo = "CTE" AND INPUT tg-cte = YES))                AND                nfe-dfe.dt-emissao               >= INPUT fi-dt-emissao-ini  AND                nfe-dfe.dt-emissao               <= INPUT fi-dt-emissao-fim  AND                nfe-dfe.cod-estabel              >= INPUT fi-cod-estabel-ini AND                nfe-dfe.cod-estabel              <= INPUT fi-cod-estabel-fim AND                nfe-dfe.serie                    >= INPUT fi-serie-ini       AND                nfe-dfe.serie                    <= INPUT fi-serie-fim       AND                nfe-dfe.nr-docto                 >= INPUT fi-nr-docto-ini    AND                nfe-dfe.nr-docto                 <= INPUT fi-nr-docto-fim    AND                nfe-dfe.nome-abrev               >= INPUT fi-nome-abrev-ini  AND                nfe-dfe.nome-abrev               <= INPUT fi-nome-abrev-fim  AND                CAN-DO(c-status-sefaz, ~
       STRING(nfe-dfe.sit-sefaz))            AND                CAN-DO(c-status-erp, ~
         STRING(nfe-dfe.sit-erp))               BY nfe-dfe.dt-emissao               BY nfe-dfe.nr-docto     INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-doctos OPEN QUERY br-doctos     FOR EACH nfe-dfe NO-LOCK          WHERE nfe-dfe.chave-acesso >= c-chave-busca-ini                    AND                nfe-dfe.chave-acesso <= c-chave-busca-fim                    AND              ((nfe-dfe.tipo = "NFE" AND INPUT tg-nfe = YES)                  OR               (nfe-dfe.tipo = "CTE" AND INPUT tg-cte = YES))                AND                nfe-dfe.dt-emissao               >= INPUT fi-dt-emissao-ini  AND                nfe-dfe.dt-emissao               <= INPUT fi-dt-emissao-fim  AND                nfe-dfe.cod-estabel              >= INPUT fi-cod-estabel-ini AND                nfe-dfe.cod-estabel              <= INPUT fi-cod-estabel-fim AND                nfe-dfe.serie                    >= INPUT fi-serie-ini       AND                nfe-dfe.serie                    <= INPUT fi-serie-fim       AND                nfe-dfe.nr-docto                 >= INPUT fi-nr-docto-ini    AND                nfe-dfe.nr-docto                 <= INPUT fi-nr-docto-fim    AND                nfe-dfe.nome-abrev               >= INPUT fi-nome-abrev-ini  AND                nfe-dfe.nome-abrev               <= INPUT fi-nome-abrev-fim  AND                CAN-DO(c-status-sefaz, ~
       STRING(nfe-dfe.sit-sefaz))            AND                CAN-DO(c-status-erp, ~
         STRING(nfe-dfe.sit-erp))               BY nfe-dfe.dt-emissao               BY nfe-dfe.nr-docto     INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-doctos nfe-dfe
&Scoped-define FIRST-TABLE-IN-QUERY-br-doctos nfe-dfe


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-doctos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-sair RECT-2 RECT-3 IMAGE-18 IMAGE-25 ~
IMAGE-26 IMAGE-27 IMAGE-29 RECT-9 RECT-10 RECT-11 RECT-5 RECT-6 rt-button ~
bt-limpa fi-cod-estabel-ini fi-cod-estabel-fim bt-chave tg-sefaz-pendente ~
tg-erp-pendente tg-nfe fi-serie-ini fi-serie-fim tg-sefaz-nao-aut ~
tg-erp-implantada bt-filtrar tg-cte fi-nr-docto-ini bt-implantar ~
fi-nr-docto-fim tg-sefaz-cancelado tg-erp-imp-manual fi-nome-abrev-ini ~
fi-nome-abrev-fim tg-sefaz-autorizado fi-dt-emissao-ini fi-dt-emissao-fim ~
br-doctos bt-Danfe bt-salva-xml bt-consulta bt-historico bt-altera ~
rs-recebimento 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estabel-ini fi-cod-estabel-fim ~
tg-sefaz-pendente tg-erp-pendente tg-nfe fi-serie-ini fi-serie-fim ~
tg-sefaz-nao-aut tg-erp-implantada tg-cte fi-nr-docto-ini fi-nr-docto-fim ~
tg-sefaz-cancelado tg-erp-imp-manual fi-nome-abrev-ini fi-nome-abrev-fim ~
tg-sefaz-autorizado fi-dt-emissao-ini fi-dt-emissao-fim rs-recebimento 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fc-sit-erp w-Win 
FUNCTION fc-sit-erp RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fc-sit-sefaz w-Win 
FUNCTION fc-sit-sefaz RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fc-sit-triagem w-Win 
FUNCTION fc-sit-triagem RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-br-doctos TITLE "Opá‰es"
       MENU-ITEM m_salvar_danfe LABEL "Salvar Danfe"  
              DISABLED
       MENU-ITEM m_salvar_xml   LABEL "Salvar XML"    
       MENU-ITEM m_Consulta_Sefaz LABEL "Consulta_Sefaz"
       MENU-ITEM m_Historico    LABEL "Historico"     
       MENU-ITEM m_Excluir      LABEL "Excluir"       .


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-altera 
     LABEL "Alterar" 
     SIZE 15 BY 1.13 TOOLTIP "Alterar dados do documento.".

DEFINE BUTTON bt-chave 
     LABEL "Chave" 
     SIZE 13 BY 1.13 TOOLTIP "Abre a tela para inserá∆o da Chave Acesso a ser procurada".

DEFINE BUTTON bt-consulta 
     LABEL "Cons.Sefaz" 
     SIZE 15 BY 1.13 TOOLTIP "Consultar Status do documento no Sefaz".

DEFINE BUTTON bt-Danfe 
     LABEL "Gerar Danfe" 
     SIZE 15 BY 1.13 TOOLTIP "Salva a Danfe selecionada num diret¢rio escolhido pelo usu†rio".

DEFINE BUTTON bt-filtrar 
     LABEL "Filtrar" 
     SIZE 13 BY 1.13.

DEFINE BUTTON bt-historico 
     LABEL "Historico" 
     SIZE 15 BY 1.13 TOOLTIP "Consultar Historico do Documento".

DEFINE BUTTON bt-implantar  NO-FOCUS
     LABEL "Integrar ERP" 
     SIZE 15 BY 1.13 TOOLTIP "Implantar Documento no recebimento do ERP".

DEFINE BUTTON bt-limpa 
     LABEL "Limpar" 
     SIZE 13 BY 1.13 TOOLTIP "Retorna para default todos os campos da Seleá∆o, Sefaz, ERP e tipo".

DEFINE BUTTON bt-sair 
     IMAGE-UP FILE "dsc/ra/img/logout.bmp":U NO-FOCUS
     LABEL "Sair" 
     SIZE 8.57 BY 1.21 TOOLTIP "SAIR".

DEFINE BUTTON bt-salva-xml 
     LABEL "Salvar XML" 
     SIZE 15 BY 1.13 TOOLTIP "Salva XML selecionada num diret¢rio escolhido pelo usu†rio".

DEFINE VARIABLE fi-cod-estabel-fim AS CHARACTER FORMAT "X(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-ini AS CHARACTER FORMAT "X(3)" 
     LABEL "Estabelecimento":R7 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-emissao-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-emissao-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/1900 
     LABEL "Dt Emissao" 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-abrev-fim AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 TOOLTIP "Digite Nome Abrev. ou Codigo do Fornecedor" NO-UNDO.

DEFINE VARIABLE fi-nome-abrev-ini AS CHARACTER FORMAT "X(12)":U 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 TOOLTIP "Digite Nome Abrev. ou Codigo do Fornecedor" NO-UNDO.

DEFINE VARIABLE fi-nr-docto-fim AS CHARACTER FORMAT "X(9)":U INITIAL "ZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-docto-ini AS CHARACTER FORMAT "X(9)":U 
     LABEL "Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "SÇrie" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-18
     FILENAME "dsc\ra\img\im-faixa.bmp":U
     SIZE 5.86 BY .79.

DEFINE IMAGE IMAGE-25
     FILENAME "dsc\ra\img\im-faixa.bmp":U
     SIZE 5.86 BY .79.

DEFINE IMAGE IMAGE-26
     FILENAME "dsc\ra\img\im-faixa.bmp":U
     SIZE 5.86 BY .79.

DEFINE IMAGE IMAGE-27
     FILENAME "dsc\ra\img\im-faixa.bmp":U
     SIZE 5.86 BY .79.

DEFINE IMAGE IMAGE-29
     FILENAME "dsc\ra\img\im-faixa.bmp":U
     SIZE 5.86 BY .79.

DEFINE VARIABLE rs-recebimento AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Fisico", 1,
"Fiscal", 2
     SIZE 19.57 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.29 BY 5.5.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.29 BY 5.5.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 5.5.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20.72 BY 5.5.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 1.71.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80.43 BY 1.71.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.57 BY 5.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 125.86 BY 1.46
     BGCOLOR 8 .

DEFINE VARIABLE tg-cte AS LOGICAL INITIAL yes 
     LABEL "CTE" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .83 NO-UNDO.

DEFINE VARIABLE tg-erp-imp-manual AS LOGICAL INITIAL yes 
     LABEL "Imp. Manual" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tg-erp-implantada AS LOGICAL INITIAL yes 
     LABEL "Implantadas" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .83 NO-UNDO.

DEFINE VARIABLE tg-erp-pendente AS LOGICAL INITIAL yes 
     LABEL "Pendentes" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tg-nfe AS LOGICAL INITIAL yes 
     LABEL "NFE" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sefaz-autorizado AS LOGICAL INITIAL yes 
     LABEL "Autorizadas" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.29 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sefaz-cancelado AS LOGICAL INITIAL yes 
     LABEL "Canceladas" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.29 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sefaz-nao-aut AS LOGICAL INITIAL yes 
     LABEL "Falha" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.29 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sefaz-pendente AS LOGICAL INITIAL yes 
     LABEL "Pendentes" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-doctos FOR 
      nfe-dfe SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-doctos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-doctos w-Win _FREEFORM
  QUERY br-doctos NO-LOCK DISPLAY
      nfe-dfe.cod-estabel                COLUMN-LABEL "Estab"        FORMAT "X(3)"   
nfe-dfe.tipo                       COLUMN-LABEL "Tipo"         FORMAT "X(3)"      
nfe-dfe.serie                      COLUMN-LABEL "Ser"          FORMAT "X(3)"            
nfe-dfe.nr-docto                   COLUMN-LABEL "Nr Nota"      FORMAT "X(18)"
nfe-dfe.nome-abrev                 COLUMN-LABEL "Emitente"     FORMAT "X(13)"
nfe-dfe.dt-emissao                 COLUMN-LABEL "Dt Emiss"     
fc-sit-sefaz()   @ c-sit-sefaz     COLUMN-LABEL "Sefaz"        FORMAT "x(14)" 
fc-sit-erp()     @ c-sit-erp       COLUMN-LABEL "ERP"          FORMAT "x(20)"
fc-sit-triagem() @ c-sit-triagem   COLUMN-LABEL "TRIAGEM"      FORMAT "x(20)"
nfe-dfe.chave-acesso               COLUMN-LABEL "Chave Acesso" FORMAT "X(48)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 125.43 BY 12.25
         TITLE "Documentos Eletronicos" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     bt-sair AT ROW 1.17 COL 117.29 HELP
          "Fecha o Monitor" WIDGET-ID 126
     bt-limpa AT ROW 1.21 COL 2.86 WIDGET-ID 40
     fi-cod-estabel-ini AT ROW 3.13 COL 14.86 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" WIDGET-ID 92
     fi-cod-estabel-fim AT ROW 3.13 COL 39.29 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" NO-LABEL WIDGET-ID 94
     bt-chave AT ROW 3.75 COL 112.43 WIDGET-ID 202
     tg-sefaz-pendente AT ROW 3.79 COL 61.72 WIDGET-ID 24
     tg-erp-pendente AT ROW 3.79 COL 82.86 WIDGET-ID 32
     tg-nfe AT ROW 3.92 COL 102.14 WIDGET-ID 86
     fi-serie-ini AT ROW 4.13 COL 14.86 COLON-ALIGNED WIDGET-ID 96
     fi-serie-fim AT ROW 4.13 COL 39.29 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     tg-sefaz-nao-aut AT ROW 4.79 COL 61.72 WIDGET-ID 26
     tg-erp-implantada AT ROW 4.79 COL 82.86 WIDGET-ID 34
     bt-filtrar AT ROW 4.88 COL 112.43 WIDGET-ID 42
     tg-cte AT ROW 4.92 COL 102.14 WIDGET-ID 88
     fi-nr-docto-ini AT ROW 5.13 COL 14.86 COLON-ALIGNED WIDGET-ID 122
     bt-implantar AT ROW 21.13 COL 109.57 WIDGET-ID 70
     fi-nr-docto-fim AT ROW 5.13 COL 39.29 COLON-ALIGNED NO-LABEL WIDGET-ID 124
     tg-sefaz-cancelado AT ROW 5.79 COL 61.72 WIDGET-ID 28
     tg-erp-imp-manual AT ROW 5.79 COL 82.86 WIDGET-ID 36
     fi-nome-abrev-ini AT ROW 6.13 COL 14.86 COLON-ALIGNED WIDGET-ID 154
     fi-nome-abrev-fim AT ROW 6.13 COL 39.29 COLON-ALIGNED NO-LABEL WIDGET-ID 152
     tg-sefaz-autorizado AT ROW 6.79 COL 61.72 WIDGET-ID 30
     fi-dt-emissao-ini AT ROW 7.13 COL 14.86 COLON-ALIGNED WIDGET-ID 90
     fi-dt-emissao-fim AT ROW 7.13 COL 39.29 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     br-doctos AT ROW 8.38 COL 1.57 WIDGET-ID 200
     bt-Danfe AT ROW 21.08 COL 2.29 WIDGET-ID 56
     bt-salva-xml AT ROW 21.08 COL 18.14 WIDGET-ID 54
     bt-consulta AT ROW 21.08 COL 33.86 WIDGET-ID 58
     bt-historico AT ROW 21.08 COL 49.43 WIDGET-ID 62
     bt-altera AT ROW 21.08 COL 65.14 WIDGET-ID 60
     rs-recebimento AT ROW 21.29 COL 86.29 NO-LABEL WIDGET-ID 66
     "Tipo" VIEW-AS TEXT
          SIZE 6.43 BY .42 AT ROW 2.58 COL 101.86 WIDGET-ID 186
     "Sefaz" VIEW-AS TEXT
          SIZE 6.43 BY .42 AT ROW 2.58 COL 61.43 WIDGET-ID 166
     "ERP" VIEW-AS TEXT
          SIZE 6.43 BY .42 AT ROW 2.58 COL 83.14 WIDGET-ID 184
     "Filtros" VIEW-AS TEXT
          SIZE 6.43 BY .42 AT ROW 2.58 COL 111.72 WIDGET-ID 188
     "Seleá∆o" VIEW-AS TEXT
          SIZE 8.43 BY .42 AT ROW 2.58 COL 2.57 WIDGET-ID 204
     RECT-2 AT ROW 2.79 COL 1.57 WIDGET-ID 48
     RECT-3 AT ROW 2.79 COL 60.29 WIDGET-ID 50
     IMAGE-18 AT ROW 3.13 COL 35.43 WIDGET-ID 118
     IMAGE-25 AT ROW 4.13 COL 35.43 WIDGET-ID 120
     IMAGE-26 AT ROW 6.13 COL 35.43 WIDGET-ID 160
     IMAGE-27 AT ROW 5.13 COL 35.43 WIDGET-ID 162
     IMAGE-29 AT ROW 7.17 COL 35.43 WIDGET-ID 170
     RECT-9 AT ROW 2.79 COL 81.86 WIDGET-ID 172
     RECT-10 AT ROW 2.79 COL 101 WIDGET-ID 174
     RECT-11 AT ROW 2.79 COL 110.86 WIDGET-ID 178
     RECT-5 AT ROW 20.88 COL 82.86 WIDGET-ID 72
     RECT-6 AT ROW 20.88 COL 1.57 WIDGET-ID 74
     rt-button AT ROW 1 COL 1 WIDGET-ID 206
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 129.72 BY 21.71 WIDGET-ID 100.


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
  CREATE WINDOW w-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Monitor de Documentos Fiscais Eletronicos"
         HEIGHT             = 22.71
         WIDTH              = 126.86
         MAX-HEIGHT         = 26.71
         MAX-WIDTH          = 144.43
         VIRTUAL-HEIGHT     = 26.71
         VIRTUAL-WIDTH      = 144.43
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT w-Win:LOAD-ICON("dsc/ra/img/dsc.ico":U) THEN
    MESSAGE "Unable to load icon: dsc/ra/img/dsc.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB br-doctos fi-dt-emissao-fim DEFAULT-FRAME */
ASSIGN 
       br-doctos:POPUP-MENU IN FRAME DEFAULT-FRAME             = MENU POPUP-MENU-br-doctos:HANDLE
       br-doctos:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-Win)
THEN w-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-doctos
/* Query rebuild information for BROWSE br-doctos
     _START_FREEFORM
OPEN QUERY br-doctos
    FOR EACH nfe-dfe NO-LOCK
         WHERE nfe-dfe.chave-acesso >= c-chave-busca-ini                    AND
               nfe-dfe.chave-acesso <= c-chave-busca-fim                    AND
             ((nfe-dfe.tipo = "NFE" AND INPUT tg-nfe = YES)                  OR
              (nfe-dfe.tipo = "CTE" AND INPUT tg-cte = YES))                AND
               nfe-dfe.dt-emissao               >= INPUT fi-dt-emissao-ini  AND
               nfe-dfe.dt-emissao               <= INPUT fi-dt-emissao-fim  AND
               nfe-dfe.cod-estabel              >= INPUT fi-cod-estabel-ini AND
               nfe-dfe.cod-estabel              <= INPUT fi-cod-estabel-fim AND
               nfe-dfe.serie                    >= INPUT fi-serie-ini       AND
               nfe-dfe.serie                    <= INPUT fi-serie-fim       AND
               nfe-dfe.nr-docto                 >= INPUT fi-nr-docto-ini    AND
               nfe-dfe.nr-docto                 <= INPUT fi-nr-docto-fim    AND
               nfe-dfe.nome-abrev               >= INPUT fi-nome-abrev-ini  AND
               nfe-dfe.nome-abrev               <= INPUT fi-nome-abrev-fim  AND
               CAN-DO(c-status-sefaz, STRING(nfe-dfe.sit-sefaz))            AND
               CAN-DO(c-status-erp,   STRING(nfe-dfe.sit-erp))
              BY nfe-dfe.dt-emissao
              BY nfe-dfe.nr-docto
    INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-doctos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-Win w-Win
ON END-ERROR OF w-Win /* Monitor de Documentos Fiscais Eletronicos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-Win w-Win
ON WINDOW-CLOSE OF w-Win /* Monitor de Documentos Fiscais Eletronicos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-doctos
&Scoped-define SELF-NAME br-doctos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-doctos w-Win
ON CTRL-C OF br-doctos IN FRAME DEFAULT-FRAME /* Documentos Eletronicos */
DO:
    FIND FIRST nfe-it-param-rec NO-LOCK
        WHERE nfe-it-param-rec.cod-parametro          = "param_global"
        AND   nfe-it-param-rec.cod-item-parametro     = "monitor_valida_danfe"
        AND   nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR.
    
    IF NOT AVAIL nfe-it-param-rec THEN DO:  
    
        IF AVAIL nfe-dfe THEN
            ASSIGN CLIPBOARD:VALUE =   nfe-dfe.chave-acesso.
        ELSE
            MESSAGE "Nenhum Documento Selecionado!"
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-doctos w-Win
ON MOUSE-SELECT-DBLCLICK OF br-doctos IN FRAME DEFAULT-FRAME /* Documentos Eletronicos */
DO:
  APPLY "Choose" TO bt-altera.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-doctos w-Win
ON ROW-DISPLAY OF br-doctos IN FRAME DEFAULT-FRAME /* Documentos Eletronicos */
DO:
  CASE nfe-dfe.sit-sefaz :
        WHEN 1 THEN ASSIGN i-cor = 00. /* --- Pendente              --- */
        WHEN 2 THEN ASSIGN i-cor = 12. /* --- N∆o Autorizada        --- */
        WHEN 3 THEN ASSIGN i-cor = 12. /* --- Cancelada             --- */
        WHEN 4 THEN ASSIGN i-cor = 02. /* --- Autorizada            --- */
  END CASE.

  IF i-cor <> 12 THEN DO:
      CASE nfe-dfe.sit-erp :
            WHEN 2 THEN ASSIGN i-cor = 09. /* --- Implantada            --- */
            WHEN 3 THEN ASSIGN i-cor = 03. /* --- Implantada Manual     --- */
            
      END CASE.
      
  END.

  IF CAN-FIND(FIRST nfe-his-nota-fis-rec
              WHERE nfe-his-nota-fis-rec.chave-acesso-nfe = nfe-dfe.chave-acesso
              AND   nfe-his-nota-fis-rec.dt-retorno  = TODAY
              AND   nfe-his-nota-fis-rec.hr-retorno >= STRING(TIME - 300,"HH:MM")) THEN DO:


      ASSIGN i-cor-bg = 14.


  END.
  ELSE 
      ASSIGN i-cor-bg = ?.


  CASE nfe-dfe.sit-triagem:
      WHEN 1 THEN i-cor-triagem = 12.
      WHEN 2 THEN i-cor-triagem = 02.
      WHEN 3 THEN i-cor-triagem = 14.
      WHEN 9 THEN i-cor-triagem = 09.
  END CASE.



  ASSIGN nfe-dfe.cod-estabel :FGCOLOR IN BROWSE br-doctos = i-cor         
         nfe-dfe.tipo        :FGCOLOR IN BROWSE br-doctos = i-cor         
         nfe-dfe.serie       :FGCOLOR IN BROWSE br-doctos = i-cor         
         nfe-dfe.nr-docto    :FGCOLOR IN BROWSE br-doctos = i-cor         
         nfe-dfe.nome-abrev  :FGCOLOR IN BROWSE br-doctos = i-cor           
         nfe-dfe.chave-acesso:FGCOLOR IN BROWSE br-doctos = i-cor         
         nfe-dfe.dt-emissao  :FGCOLOR IN BROWSE br-doctos = i-cor        
         c-sit-sefaz         :FGCOLOR IN BROWSE br-doctos = i-cor        
         c-sit-erp           :FGCOLOR IN BROWSE br-doctos = i-cor.  

  ASSIGN c-sit-triagem       :FGCOLOR IN BROWSE br-doctos = i-cor-triagem.  

  ASSIGN nfe-dfe.cod-estabel :bGCOLOR IN BROWSE br-doctos = i-cor-bg         
         nfe-dfe.tipo        :bGCOLOR IN BROWSE br-doctos = i-cor-bg         
         nfe-dfe.serie       :bGCOLOR IN BROWSE br-doctos = i-cor-bg         
         nfe-dfe.nr-docto    :bGCOLOR IN BROWSE br-doctos = i-cor-bg         
         nfe-dfe.nome-abrev  :bGCOLOR IN BROWSE br-doctos = i-cor-bg           
         nfe-dfe.chave-acesso:bGCOLOR IN BROWSE br-doctos = i-cor-bg         
         nfe-dfe.dt-emissao  :bGCOLOR IN BROWSE br-doctos = i-cor-bg        
         c-sit-sefaz         :bGCOLOR IN BROWSE br-doctos = i-cor-bg        
         c-sit-erp           :bGCOLOR IN BROWSE br-doctos = i-cor-bg
         c-sit-triagem       :bGCOLOR IN BROWSE br-doctos = i-cor-bg.  


                                    
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-doctos w-Win
ON VALUE-CHANGED OF br-doctos IN FRAME DEFAULT-FRAME /* Documentos Eletronicos */
DO:
  RUN pi-controle-bts.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-altera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-altera w-Win
ON CHOOSE OF bt-altera IN FRAME DEFAULT-FRAME /* Alterar */
DO:
  ASSIGN r-nfe-dfe-aux = ROWID(nfe-dfe).
    
  RUN pi-alterar.

  RUN pi-carrega.
  REPOSITION br-doctos TO ROWID(r-nfe-dfe-aux).
  br-doctos:SELECT-FOCUSED-ROW( ).            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-chave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-chave w-Win
ON CHOOSE OF bt-chave IN FRAME DEFAULT-FRAME /* Chave */
DO:
/*   br-doctos:DESELECT-ROWS( ). */
/*                               */
  RUN dsc/ra/esp/esra100b.w(OUTPUT r-busca-dfe).


  FIND FIRST nfe-dfe NO-LOCK
      WHERE rowid(nfe-dfe) = r-busca-dfe NO-ERROR.
  IF AVAIL nfe-dfe THEN DO:
      
      RUN pi-limpar.

      ASSIGN c-chave-busca-ini = nfe-dfe.chave-acesso
             c-chave-busca-fim = nfe-dfe.chave-acesso
             fi-dt-emissao-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "01/01/1900"
             fi-dt-emissao-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(TODAY).

      {&open-query-br-doctos}

      RUN pi-desabilita-filtro.


/*        REPOSITION br-doctos TO ROWID(ROWID(nfe-dfe)). */
/*        br-doctos:SELECT-FOCUSED-ROW( ).               */

  END.
  ELSE DO:

      MESSAGE "Documento n∆o existe!"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.

  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta w-Win
ON CHOOSE OF bt-consulta IN FRAME DEFAULT-FRAME /* Cons.Sefaz */
DO:
  
    IF br-doctos:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 1 THEN DO:
        
        IF AVAIL nfe-dfe THEN DO:
            
            RUN dsc/ra/esp/esra008.p (INPUT nfe-dfe.chave).
                
            MESSAGE "Consulta Solicitada com Sucesso"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

        END.
        ELSE DO:
            MESSAGE "Selecione um documento para Consuta!"
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                
        END.


    END.
    ELSE DO:

        DO i-cont = 1 TO br-doctos:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
            br-doctos:FETCH-SELECTED-ROW(i-cont). 

                RUN dsc/ra/esp/esra008.p (INPUT nfe-dfe.chave).

        END.

         MESSAGE "Consulta Solicitada com Sucesso"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.


    END.
    
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-Danfe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-Danfe w-Win
ON CHOOSE OF bt-Danfe IN FRAME DEFAULT-FRAME /* Gerar Danfe */
DO:
  
    IF br-doctos:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 1 THEN DO:
        
        ASSIGN c-arq-conv = nfe-dfe.chave-acesso + ".doc".
            
        SYSTEM-DIALOG GET-FILE c-arq-conv
        FILTERS "*.doc" "*.doc",
                "*.*" "*.*"
        ASK-OVERWRITE 
        DEFAULT-EXTENSION "lst"
        INITIAL-DIR "spool" 
        SAVE-AS
        USE-FILENAME
        UPDATE l-ok.

        IF l-ok THEN DO:
        
            RUN dsc/ra/esp/esra006.p(INPUT ROWID(nfe-dfe),
                                     INPUT c-arq-conv).

            MESSAGE "Danfe Salva com Sucesso! em :" SKIP
                    c-arq-conv
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE
            MESSAGE "Download Abortado!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.


    END.
    ELSE DO:

        
        SYSTEM-DIALOG GET-DIR c-diretorio
            INITIAL-DIR "spool" .

        IF c-diretorio <> "" THEN DO:

            DO i-cont = 1 TO br-doctos:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
                br-doctos:FETCH-SELECTED-ROW(i-cont). 

                IF nfe-dfe.tipo = "CTE" THEN DO:

                    ASSIGN l-danfe-cte = YES.
                    NEXT.

                END.
                
                ASSIGN c-arq-conv = c-diretorio + "\" + nfe-dfe.chave-acesso + ".doc".
                    
                RUN dsc/ra/esp/esra006.p(INPUT ROWID(nfe-dfe),
                                         INPUT c-arq-conv).
        
        

            END.   
            IF l-danfe-cte = YES THEN
                MESSAGE "Foram Selecionados documentos de CTE para impress∆o de DANFE, os mesmos foram descartados!"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

            MESSAGE "Arquivos salvos com Sucesso em:" SKIP
                c-diretorio  
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

        END.
        ELSE
           MESSAGE "Download Abortado!"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtrar w-Win
ON CHOOSE OF bt-filtrar IN FRAME DEFAULT-FRAME /* Filtrar */
DO:
  
    
    RUN pi-carrega.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-historico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-historico w-Win
ON CHOOSE OF bt-historico IN FRAME DEFAULT-FRAME /* Historico */
DO:
    
    IF br-doctos:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 1 THEN DO:
        IF AVAIL nfe-dfe THEN
            RUN dsc/ra/esp/esra100a.w(INPUT ROWID(nfe-dfe)).
        ELSE 
            MESSAGE "Nenhum Documento Selecionado!"
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END.
    ELSE IF br-doctos:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} > 1 THEN DO:

        MESSAGE "Selecione Apenas um Documento!"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END.
    ELSE DO:

        MESSAGE "Selecione um documento!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-implantar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-implantar w-Win
ON CHOOSE OF bt-implantar IN FRAME DEFAULT-FRAME /* Integrar ERP */
DO:
    IF AVAIL nfe-dfe THEN DO:
    
        DEFINE VARIABLE i-acao AS INTEGER     NO-UNDO.
        
        ASSIGN l-danfe-ok = NO.
    
        FIND FIRST nfe-it-param-rec NO-LOCK
            WHERE nfe-it-param-rec.cod-parametro          = "param_global"
            AND   nfe-it-param-rec.cod-item-parametro     = "monitor_valida_danfe"
            AND   nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR.
        
        IF AVAIL nfe-it-param-rec THEN DO:
        
            RUN dsc\ra\esp\esra100c.w(INPUT  ROWID(nfe-dfe),
                                      OUTPUT l-danfe-ok).
        END.
        ELSE 
            ASSIGN l-danfe-ok = YES.
        
        IF l-danfe-ok = YES THEN DO:
    
          DO WITH FRAME {&FRAME-NAME}:
    
    
                FIND CURRENT nfe-dfe NO-LOCK.
                IF nfe-dfe.tipo = "NFE" THEN DO:
    
                     FIND FIRST nfe-it-param-rec NO-LOCK
                         WHERE  nfe-it-param-rec.cod-parametro      = "param_global"             
                         AND    nfe-it-param-rec.cod-item-parametro = "triagem"  NO-ERROR.
                         
                     IF AVAIL nfe-it-param-rec AND nfe-it-param-rec.valor-1-item-parametro = "SIM" THEN  DO:
        
                         IF nfe-dfe.sit-triagem <> 9 THEN DO:
                             MESSAGE "Nota N∆o Liberada Pela Triagem !"
                                 VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                                 RETURN NO-APPLY.
                         END.
                     END.
                
                    FIND FIRST nfe-nota-fiscal-rec NO-LOCK
                        WHERE nfe-nota-fiscal-rec.chave-acesso-nfe = nfe-dfe.chave-acesso NO-ERROR.
        
                    IF NOT AVAIL nfe-nota-fiscal-rec THEN 
                    DO:
                        ASSIGN c-mensagem-erro = "2,"                                       +
                                                 "Nota Fiscal Eletronica invalida!,"        +
                                                 "Selecione uma Nota Fiscal Eletronica para Efetivar.".
            
                        RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001.
            
                        RUN pi_mensagem_erro IN h_bonfe001 (INPUT c-mensagem-erro,
                                                            OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
            
                        DELETE PROCEDURE h_bonfe001.
            
                        ASSIGN c-mensagem-erro = "".
                    END.
                    ELSE 
                    DO:
    
                        FIND FIRST nfe-it-param-rec NO-LOCK
                            WHERE  nfe-it-param-rec.cod-parametro      = "param_global"             
                            AND    nfe-it-param-rec.cod-item-parametro = "triagem"  NO-ERROR.
                            
                        IF AVAIL nfe-it-param-rec AND nfe-it-param-rec.valor-1-item-parametro = "SIM" THEN  DO:
    
                            IF nfe-dfe.sit-triagem <> 9 THEN DO:
    
                                MESSAGE "Nota N∆o Liberada Pela Triagem !"
                                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    
                                RETURN NO-APPLY.
                            END.
    
                        END.
    
                        /* --- Efetiva Nf-e no Recebimento --- */
            
            /*             DEBUGGER:INITIATE().  */
            /*             DEBUGGER:SET-BREAK(). */
             
                        RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001.
            
                        RUN pi_busca_danfe IN h_bonfe001 (INPUT  (INPUT rs-recebimento), /* --- 1 - Painel Receb Fisico / 2 - Painel Receb Fiscal --- */
                                                          INPUT  nfe-nota-fiscal-rec.chave-acesso-nfe, /* --- Chave NF-e --- */
                                                          OUTPUT c-mensagem-erro).
            
            
                        DELETE PROCEDURE h_bonfe001.
                              
                                 
                        /* --- Verifica ERRO --- */
            
                        IF c-mensagem-erro <> "" THEN 
                        DO:
                            
                            RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001.
            
                            RUN pi_mensagem_erro IN h_bonfe001 (INPUT c-mensagem-erro,
                                                                OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
            
                            DELETE PROCEDURE h_bonfe001.
                                     
                            /* --- Se for Mensagem de Erro nao Prossegue --- */
            
                            IF i-acao = 1 THEN 
                            DO:
                                ASSIGN c-mensagem-erro = "".
            
                                APPLY "ENTRY" TO br-doctos.
            
                                RETURN NO-APPLY.
                            END.
                        END.
            
                        APPLY "CHOOSE" TO bt-filtrar.
                        
                    END.
                END.
                ELSE IF nfe-dfe.tipo = "CTE" THEN DO:
        
                    FIND FIRST nfe-cte-inf NO-LOCK
                        WHERE nfe-cte-inf.chave-acesso = nfe-dfe.chave-acesso NO-ERROR.
        
                    Run dsc\ra\esp\bonfe001.p Persistent Set h_bonfe001.
        
                    If Not Avail nfe-cte-inf Then Do:
                        
                        Assign c-mensagem-erro = "2,"                   +
                                                 "Documento inv†lido!," +
                                                 "Selecione um Documento para Efetivar.".
                        
                        Run pi_mensagem_erro In h_bonfe001 (Input c-mensagem-erro,
                                                            Output i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
                        
                        Assign c-mensagem-erro = "".
        
                        Return No-apply.
        
                    End.
        
                    If Input Frame {&FRAME-NAME} rs-recebimento = 1 Then Do:
        
                        Assign c-mensagem-erro = "2,"                   +
                                                 "Opá∆o inv†lida!," +
                                                 "Documento do tipo CTe deve ser efetivado apenas no Recebimento Fiscal.".
                        
                        Run pi_mensagem_erro In h_bonfe001 (Input c-mensagem-erro,
                                                            Output i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
                        
                        Assign c-mensagem-erro = "".
        
                        Return No-apply.
        
                    End.
        
                    Run dsc\ra\esp\esnfe208.p Persistent Set h_esnfe208.
        
                    Run efetivar-cte In h_esnfe208 (Input Rowid(nfe-cte-inf)).
        
                    Delete Procedure h_esnfe208.
                    
                    Apply 'Choose' To bt-filtrar.
                    
                END.
                                                  
           END.
        END.
        ELSE DO:
            MESSAGE "Chave de Acesso nao confere com Documento selecionado!"
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        END.
    END.
    ELSE /*if not avail nfe-dfe*/
        MESSAGE "Nenhum Registro selecionado!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-limpa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-limpa w-Win
ON CHOOSE OF bt-limpa IN FRAME DEFAULT-FRAME /* Limpar */
DO: 
    RUN pi-limpar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sair w-Win
ON CHOOSE OF bt-sair IN FRAME DEFAULT-FRAME /* Sair */
DO:
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salva-xml
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salva-xml w-Win
ON CHOOSE OF bt-salva-xml IN FRAME DEFAULT-FRAME /* Salvar XML */
DO:
    
    IF br-doctos:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 1 THEN DO:
        
        ASSIGN c-arq-conv = nfe-dfe.chave-acesso + ".xml".
            
        SYSTEM-DIALOG GET-FILE c-arq-conv
        FILTERS "*.xml" "*.xml",
                "*.*" "*.*"
        ASK-OVERWRITE 
        DEFAULT-EXTENSION "lst"
        INITIAL-DIR "spool" 
        SAVE-AS
        USE-FILENAME
        UPDATE l-ok.

        IF l-ok THEN DO:
        
            RUN dsc/ra/esp/esra005.p(INPUT ROWID(nfe-dfe),
                                     INPUT c-arq-conv).

            MESSAGE "Arquivo salvo com Sucesso em:" SKIP
                c-arq-conv  
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE
            MESSAGE "Download Abortado!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.


    END.
    ELSE DO:
        SYSTEM-DIALOG GET-DIR c-diretorio
            INITIAL-DIR "spool" .

        IF c-diretorio <> "" THEN DO:

            DO i-cont = 1 TO br-doctos:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
                br-doctos:FETCH-SELECTED-ROW(i-cont). 
                
                ASSIGN c-arq-conv = c-diretorio + "\" + nfe-dfe.chave-acesso + ".xml".
                    
                RUN dsc/ra/esp/esra005.p(INPUT ROWID(nfe-dfe),
                                         INPUT c-arq-conv).
        
        

            END.                      
            MESSAGE "Arquivos salvos com Sucesso em:" SKIP
                c-diretorio  
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

        END.
        ELSE
           MESSAGE "Download Abortado!"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-abrev-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-fim w-Win
ON LEAVE OF fi-nome-abrev-fim IN FRAME DEFAULT-FRAME
DO:
  FIND FIRST emitente NO-LOCK
      WHERE string(emitente.cod-emitente) = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fim NO-ERROR.
  IF AVAIL emitente THEN DO:
      ASSIGN fi-nome-abrev-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-abrev-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-ini w-Win
ON LEAVE OF fi-nome-abrev-ini IN FRAME DEFAULT-FRAME /* Fornecedor */
DO:
  FIND FIRST emitente NO-LOCK
      WHERE string(emitente.cod-emitente) = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini NO-ERROR.
  IF AVAIL emitente THEN DO:
      ASSIGN fi-nome-abrev-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Consulta_Sefaz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Consulta_Sefaz w-Win
ON CHOOSE OF MENU-ITEM m_Consulta_Sefaz /* Consulta_Sefaz */
DO:
  APPLY "choose" TO bt-consulta IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Excluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Excluir w-Win
ON CHOOSE OF MENU-ITEM m_Excluir /* Excluir */
DO:
  RUN pi-excluir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Historico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Historico w-Win
ON CHOOSE OF MENU-ITEM m_Historico /* Historico */
DO:
  APPLY "choose" TO bt-historico IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_salvar_danfe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_salvar_danfe w-Win
ON CHOOSE OF MENU-ITEM m_salvar_danfe /* Salvar Danfe */
DO:
  
    IF AVAIL nfe-dfe THEN DO:
        IF nfe-dfe.tipo = "CTE" THEN
            MESSAGE "N∆o Ç Possivel Emissao de Danfe de CTE!"
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.

        ELSE
            APPLY "choose" TO bt-danfe IN FRAME {&FRAME-NAME}.
        
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_salvar_xml
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_salvar_xml w-Win
ON CHOOSE OF MENU-ITEM m_salvar_xml /* Salvar XML */
DO:
  APPLY "choose" TO bt-salva-xml IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
{dsc\ra\include\i-versao-ra.i 0}
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/*DEFINE NEW GLOBAL SHARED VAR v_cod_grp_usuar_lst AS CHAR NO-UNDO.

FIND FIRST grp_usuar
    WHERE grp_usuar.cod_grp_usuar = "RA" NO-LOCK NO-ERROR.

IF AVAIL grp_usuar THEN DO:
    IF LOOKUP ("RA", v_cod_grp_usuar_lst) = 0 THEN DO: /*n∆o encontrei o grupo RA ou n∆o pertenáo a ele*/
        MESSAGE "Usuario nao tem permissao para acessar esse programa." SKIP
                "Favor cadastrar o usuario no grupo RA (BAS_GRP_USUAR)."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "CLOSE" TO THIS-PROCEDURE.
    END.
END.
ELSE DO:*/
    RUN pi-limpar.
    RUN pi-carrega.
    RUN pi-inicializar.
/*END.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-Win)
  THEN DELETE WIDGET w-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-Win  _DEFAULT-ENABLE
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
  DISPLAY fi-cod-estabel-ini fi-cod-estabel-fim tg-sefaz-pendente 
          tg-erp-pendente tg-nfe fi-serie-ini fi-serie-fim tg-sefaz-nao-aut 
          tg-erp-implantada tg-cte fi-nr-docto-ini fi-nr-docto-fim 
          tg-sefaz-cancelado tg-erp-imp-manual fi-nome-abrev-ini 
          fi-nome-abrev-fim tg-sefaz-autorizado fi-dt-emissao-ini 
          fi-dt-emissao-fim rs-recebimento 
      WITH FRAME DEFAULT-FRAME IN WINDOW w-Win.
  ENABLE bt-sair RECT-2 RECT-3 IMAGE-18 IMAGE-25 IMAGE-26 IMAGE-27 IMAGE-29 
         RECT-9 RECT-10 RECT-11 RECT-5 RECT-6 rt-button bt-limpa 
         fi-cod-estabel-ini fi-cod-estabel-fim bt-chave tg-sefaz-pendente 
         tg-erp-pendente tg-nfe fi-serie-ini fi-serie-fim tg-sefaz-nao-aut 
         tg-erp-implantada bt-filtrar tg-cte fi-nr-docto-ini bt-implantar 
         fi-nr-docto-fim tg-sefaz-cancelado tg-erp-imp-manual fi-nome-abrev-ini 
         fi-nome-abrev-fim tg-sefaz-autorizado fi-dt-emissao-ini 
         fi-dt-emissao-fim br-doctos bt-Danfe bt-salva-xml bt-consulta 
         bt-historico bt-altera rs-recebimento 
      WITH FRAME DEFAULT-FRAME IN WINDOW w-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW w-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.
   

  {include/win-size.i}

  /*{utp/ut9000.i "ESRA100" "2.06.00.000"}*/
  


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-alterar w-Win 
PROCEDURE pi-alterar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/



IF NOT AVAIL nfe-dfe THEN  RETURN NO-APPLY.

ASSIGN l-danfe-ok = NO.

/* FIND FIRST nfe-it-param-rec NO-LOCK                                        */
/*     WHERE nfe-it-param-rec.cod-parametro          = "param_global"         */
/*     AND   nfe-it-param-rec.cod-item-parametro     = "monitor_valida_danfe" */
/*     AND   nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR.        */
/*                                                                            */
/* IF AVAIL nfe-it-param-rec THEN DO:                                         */
/*     RUN dsc\ra\esp\esra100c.w(INPUT  ROWID(nfe-dfe),                       */
/*                               OUTPUT l-danfe-ok).                          */
/* END.                                                                       */
/* ELSE                                                                       */
    ASSIGN l-danfe-ok = YES.

IF l-danfe-ok = YES THEN DO:
    
    IF nfe-dfe.tipo = 'NFE' THEN DO:
                      
        FIND FIRST nfe-nota-fiscal-rec NO-LOCK
            WHERE nfe-nota-fiscal-rec.chave-acesso-nfe = nfe-dfe.chave-acesso NO-ERROR.
            
        /*w-win:SENSITIVE = NO.*/      /* --- Executa tela de Itens da Nota por Natureza --- */
        
        
        RUN dsc\ra\esp\esnfe200d.w (INPUT  ROWID(nfe-nota-fiscal-rec),
                                    INPUT  INPUT FRAME {&FRAME-NAME} rs-recebimento,
                                    OUTPUT l-erro-ret).
        
                /*(w-Win:MOVE-TO-TOP().*/
        
                /*w-win:SENSITIVE = YES.*/
    END.
        
    IF nfe-dfe.tipo = 'CTE' THEN DO:
        FIND FIRST nfe-cte-inf NO-LOCK
            WHERE nfe-cte-inf.chave-acesso = nfe-dfe.chave-acesso NO-ERROR.
                
    /*             If Not Avail nfe-cte-inf Then Return No-apply. */
                    
                /*Assign w-win:Sensitive = No.*/
                    
                Run dsc\ra\esp\esnfe200f.w (Input Rowid(nfe-cte-inf),
                                            Output l-erro-ret).
    
                /*Assign w-win:Sensitive = Yes.*/
    
    END.
   
END.
ELSE DO:
    MESSAGE "Chave de Acesso nao confere com Documento selecionado!"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega w-Win 
PROCEDURE pi-carrega :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST prog_dtsul NO-LOCK
         WHERE prog_dtsul.cod_prog_dtsul = "esra100"
           AND prog_dtsul.nom_prog_upc  <> "" NO-ERROR.
    IF AVAIL prog_dtsul THEN DO:

        ASSIGN c-nom-prog-upc-mg97 = prog_dtsul.nom_prog_upc.

        run value(c-nom-prog-upc-mg97) (input "INITIALIZE":U, 
                                        input "CONTAINER":U,
                                        input this-procedure,
                                        input frame default-frame:handle,
                                        input "nfe-dfe",
                                        input ?). 
        IF RETURN-VALUE = "NOK" THEN 
            return 'ADM-ERROR':U.

    END.

    ASSIGN c-status-sefaz = ""
           c-status-erp   = "".

    IF INPUT frame {&frame-name} tg-sefaz-pendente   = YES THEN c-status-sefaz = "1,".
    IF INPUT frame {&frame-name} tg-sefaz-nao-aut    = YES THEN c-status-sefaz = c-status-sefaz + "2,".
    IF INPUT frame {&frame-name} tg-sefaz-cancelado  = YES THEN c-status-sefaz = c-status-sefaz + "3,".
    IF INPUT frame {&frame-name} tg-sefaz-autorizado = YES THEN c-status-sefaz = c-status-sefaz + "4".

    IF INPUT frame {&frame-name} tg-erp-pendente   = YES THEN c-status-erp = "1,".
    IF INPUT frame {&frame-name} tg-erp-implantada = YES THEN c-status-erp = c-status-erp + "2,".
    IF INPUT frame {&frame-name} tg-erp-imp-manual = YES THEN c-status-erp = c-status-erp + "3".
    

    {&open-query-br-doctos}
/*      ASSIGN fi-tot-dfe:SCREEN-VALUE IN FRAME {&FRAME-NAME} =  STRING(num-results("br-doctos")). */

    ASSIGN i-tot-docs = 0.

    FOR EACH nfe-dfe FIELDS(nfe-dfe.chave-acesso) NO-LOCK
        WHERE nfe-dfe.chave-acesso >= c-chave-busca-ini AND
        nfe-dfe.chave-acesso <= c-chave-busca-fim AND
        ((nfe-dfe.tipo = "NFE" AND INPUT tg-nfe = YES) OR
        (nfe-dfe.tipo = "CTE" AND INPUT tg-cte = YES)) AND
        nfe-dfe.dt-emissao               >= INPUT fi-dt-emissao-ini  AND
        nfe-dfe.dt-emissao               <= INPUT fi-dt-emissao-fim  AND
        nfe-dfe.cod-estabel              >= INPUT fi-cod-estabel-ini AND
        nfe-dfe.cod-estabel              <= INPUT fi-cod-estabel-fim AND
        nfe-dfe.serie                    >= INPUT fi-serie-ini       AND
        nfe-dfe.serie                    <= INPUT fi-serie-fim       AND
        nfe-dfe.nr-docto                 >= INPUT fi-nr-docto-ini    AND
        nfe-dfe.nr-docto                 <= INPUT fi-nr-docto-fim    AND
        nfe-dfe.nome-abrev               >= INPUT fi-nome-abrev-ini  AND
        nfe-dfe.nome-abrev               <= INPUT fi-nome-abrev-fim  AND
        CAN-DO(c-status-sefaz, STRING(nfe-dfe.sit-sefaz)) AND
        CAN-DO(c-status-erp,   STRING(nfe-dfe.sit-erp)):
        
        ASSIGN i-tot-docs = i-tot-docs + 1.


    END.






        ASSIGN br-doctos:TITLE = "Documentos Eletronicos Total: " + STRING (i-tot-docs) /*STRING(num-results("br-doctos"))*/.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-controle-bts w-Win 
PROCEDURE pi-controle-bts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


IF AVAIL nfe-dfe THEN DO:
    IF nfe-dfe.tipo = "CTE" THEN DO:       
        ASSIGN bt-Danfe:SENSITIVE IN FRAME {&FRAME-NAME} = NO
               MENU-ITEM m_salvar_danfe:SENSITIVE IN MENU POPUP-MENU-br-doctos = NO.


        
    END.                                     
    ELSE
        ASSIGN bt-Danfe:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               MENU-ITEM m_salvar_danfe:SENSITIVE IN MENU POPUP-MENU-br-doctos = YES.


END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desabilita-filtro w-Win 
PROCEDURE pi-desabilita-filtro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN fi-cod-estabel-ini   :sensitive in frame {&frame-name} = no
       fi-cod-estabel-fim   :sensitive in frame {&frame-name} = no
       fi-serie-ini         :sensitive in frame {&frame-name} = no
       fi-serie-fim         :sensitive in frame {&frame-name} = no
       fi-nr-docto-ini      :sensitive in frame {&frame-name} = no
       fi-nr-docto-fim      :sensitive in frame {&frame-name} = no
       fi-nome-abrev-ini    :sensitive in frame {&frame-name} = no
       fi-nome-abrev-fim    :sensitive in frame {&frame-name} = no
       fi-dt-emissao-ini    :sensitive in frame {&frame-name} = no
       fi-dt-emissao-fim    :sensitive in frame {&frame-name} = no
       tg-sefaz-pendente    :sensitive in frame {&frame-name} = no
       tg-sefaz-nao-aut     :sensitive in frame {&frame-name} = no
       tg-sefaz-cancelado   :sensitive in frame {&frame-name} = no
       tg-sefaz-autorizado  :sensitive in frame {&frame-name} = no
       tg-erp-pendente      :sensitive in frame {&frame-name} = no
       tg-erp-implantada    :sensitive in frame {&frame-name} = no
       tg-erp-imp-manual    :sensitive in frame {&frame-name} = no
       tg-nfe               :sensitive in frame {&frame-name} = no
       tg-cte               :sensitive in frame {&frame-name} = no.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excluir w-Win 
PROCEDURE pi-excluir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/



    
    DEFINE VARIABLE c-mensagem-erro AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE h_bonfe001      AS HANDLE      NO-UNDO.
    DEFINE VARIABLE i-acao          AS INTEGER     NO-UNDO.
    DEFINE VARIABLE Qh-br-doctos       AS HANDLE      NO-UNDO.
    


    FIND FIRST nfe-nota-fiscal-rec NO-LOCK
        WHERE nfe-nota-fiscal-rec.chave-acesso-nfe = nfe-dfe.chave-acesso NO-ERROR.

    FIND FIRST bf-nfe-dfe OF nfe-dfe EXCLUSIVE-LOCK NO-ERROR.
    DO WITH FRAME {&FRAME-NAME} :
        IF  AVAIL nfe-nota-fiscal-rec THEN DO:
            
            IF nfe-dfe.sit-erp = 2 OR nfe-dfe.sit-erp = 3 THEN DO: /* --- Implantada ou implntada manual--- */
                ASSIGN c-mensagem-erro = "2,"                                       +
                                         "Nota Fiscal Eletronica bloqueada!,"       +
                                         "O documento fiscal selecionado j† foi efetivado no Sistema e nao pode ser eliminado.".
                RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001.
                RUN pi_mensagem_erro IN h_bonfe001 (INPUT c-mensagem-erro,
                                                    OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
                DELETE PROCEDURE h_bonfe001.
                ASSIGN c-mensagem-erro = "".
                RETURN NO-APPLY.
            END.

            MESSAGE "Confirma a Exclus∆o do Registro ?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-del-reg AS LOG.

            IF l-del-reg THEN DO:
                RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001.
                RUN pi_exclui_registro IN h_bonfe001 (INPUT ROWID(nfe-nota-fiscal-rec)).


                DELETE bf-nfe-dfe.
                DELETE PROCEDURE h_bonfe001.
                
                    
                ASSIGN Qh-br-doctos = br-doctos:QUERY.
                Qh-br-doctos:REPOSITION-FORWARD(0) NO-ERROR.
            END.
        END.
        ELSE DO:
            FIND FIRST nfe-cte-inf NO-LOCK
                WHERE nfe-cte-inf.chave-acesso = nfe-dfe.chave-acesso  NO-ERROR.
            
            IF AVAIL nfe-cte-inf THEN DO:
                
                IF nfe-dfe.sit-erp = 2 OR nfe-dfe.sit-erp = 3 THEN DO: /* --- Implantada ou implntada manual--- */
                    ASSIGN c-mensagem-erro = "2,"                                       +
                                             "CTE bloqueada!,"       +
                                             "O documento fiscal selecionado j† foi efetivado no Sistema e nao pode ser eliminado.".
                    RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001.
                    RUN pi_mensagem_erro IN h_bonfe001 (INPUT c-mensagem-erro,
                                                        OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
                    DELETE PROCEDURE h_bonfe001.
                    ASSIGN c-mensagem-erro = "".
                    RETURN NO-APPLY.

                END.
                MESSAGE "Confirma a Exclus∆o do Registro ?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-deleta AS LOG.
    
                IF l-deleta THEN DO:
                    RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001.
                    RUN pi_exclui_registro IN h_bonfe001 (INPUT ROWID(nfe-cte-inf)).
                    DELETE PROCEDURE h_bonfe001.
                    DELETE bf-nfe-dfe.
                    ASSIGN Qh-br-doctos = br-doctos:QUERY.
                    Qh-br-doctos:REPOSITION-FORWARD(0) NO-ERROR.
                END.



            END.
            ELSE DO:
                ASSIGN c-mensagem-erro = "2,"                                       +
                                         "Nota Fiscal Eletronica invalida!,"        +
                                         "Selecione uma Nota Fiscal Eletronica para Eliminar.".
                RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001.
                RUN pi_mensagem_erro IN h_bonfe001 (INPUT c-mensagem-erro,
                                                    OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
                DELETE PROCEDURE h_bonfe001.
                ASSIGN c-mensagem-erro = "".

            END.


            APPLY "choose" TO bt-filtrar IN FRAME {&FRAME-NAME}. 
        END.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-inicializar w-Win 
PROCEDURE pi-inicializar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN dsc/ra/esp/esra015.p PERSISTENT SET h-esra015.

FIND FIRST nfe-param-rec no-lock
    WHERE nfe-param-rec.cod-parametro = "param_global" NO-ERROR.
    
IF AVAIL nfe-param-rec THEN DO:
    /* --- Consulta Sefaz --- */
    
/*     FIND FIRST nfe-it-param-rec WHERE                                                              */
/*                nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND               */
/*                nfe-it-param-rec.cod-item-parametro = "consulta_sefaz"            NO-LOCK NO-ERROR. */
/*                                                                                                    */
/*     IF AVAIL nfe-it-param-rec                          AND                                         */
/*        nfe-it-param-rec.valor-1-item-parametro = "SIM" THEN                                        */
/*         ASSIGN bt-sefaz:SENSITIVE = YES.                                                           */
    
    ASSIGN rs-recebimento:SENSITIVE in frame {&frame-name} = NO.
            
    /* --- Recebimento Fisico --- */
    
    FIND FIRST nfe-it-param-rec no-lock
        WHERE  nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
               nfe-it-param-rec.cod-item-parametro = "recebimento_fisico"         NO-ERROR.
    
    /* --- Recebimento Fiscal --- */
    FIND FIRST bf-nfe-it-param-rec no-lock
        WHERE  bf-nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro 
        AND    bf-nfe-it-param-rec.cod-item-parametro = "recebimento_fiscal"         NO-ERROR.
    
    /* --- Verifica se Sistema Utiliza Recebimento Fisico --- */
    
    FIND FIRST param-estoq NO-LOCK NO-ERROR.
    
    /* --- Habilita Recebimento Fisico e Fiscal --- */
    
    IF AVAIL nfe-it-param-rec                               AND
       AVAIL bf-nfe-it-param-rec                            AND
       (nfe-it-param-rec.valor-1-item-parametro     = "SIM" AND
       AVAIL param-estoq                                    AND
       param-estoq.rec-fisico                       = YES)  AND
       bf-nfe-it-param-rec.valor-1-item-parametro   = "SIM" THEN 
        
        ASSIGN rs-recebimento:SENSITIVE in frame {&frame-name} = YES.
    
    /* --- Habilita Recebimento Fisico --- */
    IF AVAIL nfe-it-param-rec                               AND
       AVAIL bf-nfe-it-param-rec                            AND
       (nfe-it-param-rec.valor-1-item-parametro     = "SIM" AND
       AVAIL param-estoq                                    AND
       param-estoq.rec-fisico                       = YES)  AND
       bf-nfe-it-param-rec.valor-1-item-parametro  <> "SIM" THEN 
        
        ASSIGN rs-recebimento:SCREEN-VALUE in frame {&frame-name}  = "1".

    /* --- Habilita Recebimento Fiscal --- */

    IF AVAIL nfe-it-param-rec                               AND
       AVAIL bf-nfe-it-param-rec                            AND
       (nfe-it-param-rec.valor-1-item-parametro   <> "SIM"  OR
       (AVAIL param-estoq                                   AND
       param-estoq.rec-fisico                       = NO))  AND
       bf-nfe-it-param-rec.valor-1-item-parametro  = "SIM" THEN 
        ASSIGN rs-recebimento:SCREEN-VALUE in frame {&frame-name} = "2".

END.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-limpar w-Win 
PROCEDURE pi-limpar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


ASSIGN fi-cod-estabel-ini   :sensitive in frame {&frame-name} = yes
       fi-cod-estabel-fim   :sensitive in frame {&frame-name} = yes
       fi-serie-ini         :sensitive in frame {&frame-name} = yes
       fi-serie-fim         :sensitive in frame {&frame-name} = yes
       fi-nr-docto-ini      :sensitive in frame {&frame-name} = yes
       fi-nr-docto-fim      :sensitive in frame {&frame-name} = yes
       fi-nome-abrev-ini    :sensitive in frame {&frame-name} = yes
       fi-nome-abrev-fim    :sensitive in frame {&frame-name} = yes
       fi-dt-emissao-ini    :sensitive in frame {&frame-name} = yes
       fi-dt-emissao-fim    :sensitive in frame {&frame-name} = yes
       tg-sefaz-pendente    :sensitive in frame {&frame-name} = yes
       tg-sefaz-nao-aut     :sensitive in frame {&frame-name} = yes
       tg-sefaz-cancelado   :sensitive in frame {&frame-name} = yes
       tg-sefaz-autorizado  :sensitive in frame {&frame-name} = yes
       tg-erp-pendente      :sensitive in frame {&frame-name} = yes
       tg-erp-implantada    :sensitive in frame {&frame-name} = yes
       tg-erp-imp-manual    :sensitive in frame {&frame-name} = yes
       tg-nfe               :sensitive in frame {&frame-name} = yes
       tg-cte               :sensitive in frame {&frame-name} = yes.

ASSIGN
fi-cod-estabel-ini = ""
fi-cod-estabel-fim = "ZZZ"
fi-serie-ini       = ""
fi-serie-fim       = "ZZZ"
fi-nr-docto-ini    = ""
fi-nr-docto-fim    = "ZZZZZZZZZ"
fi-nome-abrev-ini  = ""
fi-nome-abrev-fim  = "ZZZZZZZZZZZZ"
fi-dt-emissao-ini  = TODAY - 7
fi-dt-emissao-fim  = TODAY
tg-sefaz-pendente  = yes
tg-sefaz-nao-aut   = yes
tg-sefaz-cancelado = yes
tg-sefaz-autorizado = yes
tg-erp-pendente    = yes
tg-erp-implantada  = yes
tg-erp-imp-manual  = yes
tg-nfe             = yes
tg-cte             = YES
c-chave-busca-ini  = ""
c-chave-busca-fim  = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz".
                      
    DISPLAY
fi-cod-estabel-ini 
fi-cod-estabel-fim 
fi-serie-ini      
fi-serie-fim       
fi-nr-docto-ini    
fi-nr-docto-fim    
fi-nome-abrev-ini  
fi-nome-abrev-fim  
fi-dt-emissao-ini  
fi-dt-emissao-fim  
tg-sefaz-pendente  
tg-sefaz-nao-aut   
tg-sefaz-cancelado 
tg-sefaz-autorizado 
tg-erp-pendente    
tg-erp-implantada  
tg-erp-imp-manual  
tg-nfe             
tg-cte        

WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fc-sit-erp w-Win 
FUNCTION fc-sit-erp RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    CASE nfe-dfe.sit-erp :
        WHEN 1 THEN RETURN "Pendente".
        WHEN 2 THEN RETURN "Implantada".
        WHEN 3 THEN RETURN "Implant Manual".
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fc-sit-sefaz w-Win 
FUNCTION fc-sit-sefaz RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    CASE nfe-dfe.sit-sefaz :
        WHEN 1 THEN RETURN "Pendente".
        WHEN 2 THEN RETURN "Falha".
        WHEN 3 THEN RETURN "Cancelada".
        WHEN 4 THEN RETURN "Autorizada".
        
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fc-sit-triagem w-Win 
FUNCTION fc-sit-triagem RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST nfe-it-param-rec NO-LOCK
      WHERE  nfe-it-param-rec.cod-parametro      = "param_global"             
      AND    nfe-it-param-rec.cod-item-parametro = "triagem"  NO-ERROR.

  IF AVAIL nfe-it-param-rec AND nfe-it-param-rec.valor-1-item-parametro = "SIM" THEN  DO:
      
      IF NOT valid-handle(h-esra015) THEN DO:
          RUN dsc/ra/esp/esra015.p PERSISTENT SET h-esra015.
      END.
      
      RUN pi-documento IN h-esra015 (INPUT nfe-dfe.chave-acesso )  .

      CASE nfe-dfe.sit-triagem :
            WHEN 1 THEN RETURN "Pendente".
            WHEN 2 THEN RETURN "Traduzido".
            WHEN 3 THEN RETURN "Em Analise".
            WHEN 4 THEN RETURN "Bloqueado".
            WHEN 9 THEN RETURN "Liberado".
            OTHERWISE RETURN "Pendente" .
      END CASE.
  END.
  ELSE 
      RETURN "" .

  /*RETURN "".   /* Function return value. */*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

