&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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
{include/i-prgvrs.i ESCD0422 12.1.17.000}  /*** 010001 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i escd0422 MCD}
&ENDIF

/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESCD0422
&GLOBAL-DEFINE Version        12.1.17.000
&GLOBAL-DEFINE VersionLayout  

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Seleá∆o,Classificaá∆o,ParÉmetros,Impress∆o

&GLOBAL-DEFINE PGLAY          no
&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGCLA          YES
&GLOBAL-DEFINE PGPAR          YES
&GLOBAL-DEFINE PGDIG          no
&GLOBAL-DEFINE PGIMP          YES
&GLOBAL-DEFINE PGLOG          no

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btHelp2
&GLOBAL-DEFINE page1Widgets   
&GLOBAL-DEFINE page2Widgets   
&GLOBAL-DEFINE page3Widgets   iClassificacao
&GLOBAL-DEFINE page4Widgets   lParamAtivos ~
                              lParamRestrCC ~
                              lParamRestrCCRE ~
                              lParamInativos ~
                              dtAvalSituacao

&GLOBAL-DEFINE page5Widgets   
&GLOBAL-DEFINE page6Widgets   rsDestiny ~
                              btConfigImpr ~
                              btFile ~
                              rsExecution
&GLOBAL-DEFINE page7Widgets   
&GLOBAL-DEFINE page8Widgets   

&GLOBAL-DEFINE page0Text      
&GLOBAL-DEFINE page1Text      
&GLOBAL-DEFINE page2Text      
&GLOBAL-DEFINE page3Text      
&GLOBAL-DEFINE page4Text      
&GLOBAL-DEFINE page5Text      
&GLOBAL-DEFINE page6Text      text-destino text-modo /*text-impressao*/
&GLOBAL-DEFINE page7Text      
&GLOBAL-DEFINE page8Text   

&GLOBAL-DEFINE page1Fields    
&GLOBAL-DEFINE page2Fields    iCodEmitenteIni ~
                              iCodEmitenteFim ~
                              cNomeAbrevIni ~
                              cNomeAbrevfim ~
                              cCgcIni ~
                              cCgcFim ~
                              iCodGrFornIni ~
                              iCodGrFornFim
&GLOBAL-DEFINE page3Fields    
&GLOBAL-DEFINE page4Fields    dtAvalSituacao
&GLOBAL-DEFINE page5Fields    
&GLOBAL-DEFINE page6Fields    cFile 
                              /*lImprimeParam*/
&GLOBAL-DEFINE page7Fields    
&GLOBAL-DEFINE page8Fields    

/* Parameters Definitions ---*/

define temp-table tt-param no-undo
    field destino             as integer
    field arquivo             as char format "x(35)"
    field usuario             as char format "x(12)"
    field data-exec           as date
    field hora-exec           as integer
    &IF "{&mguni_version}" >= "2.071" &THEN
    field ep-codigo           LIKE ems2cadme.empresa.ep-codigo
    &ELSE
    field ep-codigo           as integer
    &ENDIF
    field classifica          as integer
    field desc-classifica     as char
    field i-cod-emitente-ini  as integer format ">>>>>>>>9" 
    field i-cod-emitente-fim  as integer format ">>>>>>>>9"
    field c-nome-abrev-ini    as char
    field c-nome-abrev-fim    as char
    field c-cgc-ini           as char
    field c-cgc-fim           as char
    field i-cod-gr-forn-ini   as integer
    field i-cod-gr-forn-fim   as integer
    field l-param-ativos      as logical
    field l-param-restr-cc    as logical
    field l-param-restr-cc-re as logical
    field l-param-inativos    as logical
    field dt-aval-situacao    as date
    field parametro           as logical.
   
define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def temp-table tt-raw-digita
   field raw-digita      as raw.

define buffer b-tt-digita for tt-digita.

/*Definiá∆o das vari†veis*/
def var raw-param          as raw no-undo.
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-arq-layout       as char    no-undo.      
def var c-arq-temp         as char    no-undo.

def stream s-imp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btOK btCancel btHelp2 rtToolBar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wReport AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE cCgcFim AS CHARACTER FORMAT "X(19)":U INITIAL "ZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cCgcIni AS CHARACTER FORMAT "X(19)":U 
     LABEL "CGC/CPF" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cNomeAbrevFim AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cNomeAbrevIni AS CHARACTER FORMAT "X(12)":U 
     LABEL "Nome Abreviado" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iCodEmitenteFim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iCodEmitenteIni AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "C¢digo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iCodGrFornFim AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iCodGrFornIni AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Grupo" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88
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

DEFINE IMAGE IMAGE-5
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE cLabelClassif AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 12 BY .67 NO-UNDO.

DEFINE VARIABLE iClassificacao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "C¢digo", 1,
"Nome Abreviado", 2,
"CGC", 3,
"Grupo", 4,
"Raz∆o Social", 5
     SIZE 20 BY 5.75
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY 7.

DEFINE VARIABLE cLabelParam AS CHARACTER FORMAT "X(256)":U INITIAL "Considera Fornecedores" 
      VIEW-AS TEXT 
     SIZE 20 BY .67 NO-UNDO.

DEFINE VARIABLE dtAvalSituacao AS DATE FORMAT "99/99/9999":U 
     LABEL "Situaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .88 TOOLTIP "Data para Avaliaá∆o da Situaá∆o do Fornecedor" NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 8.5.

DEFINE VARIABLE lParamAtivos AS LOGICAL INITIAL yes 
     LABEL "Ativos" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.72 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE lParamInativos AS LOGICAL INITIAL yes 
     LABEL "Inativos" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.72 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE lParamRestrCC AS LOGICAL INITIAL yes 
     LABEL "Restriá∆o Compras" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.72 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE lParamRestrCCRE AS LOGICAL INITIAL yes 
     LABEL "Restriá∆o Compras Recebimento" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.72 BY 1.08
     FONT 1 NO-UNDO.

DEFINE BUTTON btConfigImpr 
     IMAGE-UP FILE "image~\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btFile 
     IMAGE-UP FILE "image~\im-sea":U
     IMAGE-INSENSITIVE FILE "image~\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE cFile AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.14 BY .63
     FONT 1 NO-UNDO.

/* DEFINE VARIABLE text-impressao AS CHARACTER FORMAT "X(256)":U INITIAL "ParÉmetros de Impress∆o" */
/*       VIEW-AS TEXT                                                                              */
/*      SIZE 18 BY .63                                                                             */
/*      FONT 1 NO-UNDO.                                                                            */

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsDestiny AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsExecution AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

/* DEFINE VARIABLE lImprimeParam AS LOGICAL INITIAL yes */
/*      LABEL "Imprimir P†gina ParÉmetros"              */
/*      VIEW-AS TOGGLE-BOX                              */
/*      SIZE 24 BY .83 NO-UNDO.                         */


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 14.5 COL 2
     btCancel AT ROW 14.5 COL 13
     btHelp2 AT ROW 14.5 COL 80
     rtToolBar AT ROW 14.29 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.75
         FONT 1.

DEFINE FRAME fPage2
     iCodEmitenteIni AT ROW 2 COL 14 COLON-ALIGNED
     iCodEmitenteFim AT ROW 2 COL 47 COLON-ALIGNED NO-LABEL
     cNomeAbrevIni AT ROW 3 COL 14 COLON-ALIGNED
     cNomeAbrevFim AT ROW 3 COL 47 COLON-ALIGNED NO-LABEL
     cCgcIni AT ROW 4 COL 14 COLON-ALIGNED
     cCgcFim AT ROW 4 COL 47 COLON-ALIGNED NO-LABEL
     iCodGrFornIni AT ROW 5 COL 14 COLON-ALIGNED
     iCodGrFornFim AT ROW 5 COL 47 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 2 COL 41
     IMAGE-2 AT ROW 2 COL 45
     IMAGE-3 AT ROW 3 COL 41
     IMAGE-4 AT ROW 3 COL 45
     IMAGE-5 AT ROW 4 COL 41
     IMAGE-6 AT ROW 4 COL 45
     IMAGE-7 AT ROW 5 COL 41
     IMAGE-8 AT ROW 5 COL 45
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.81
         SIZE 76.86 BY 10.15
         FONT 1.

DEFINE FRAME fPage6
     rsDestiny AT ROW 2.38 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     btConfigImpr AT ROW 3.58 COL 43.29 HELP
          "Configuraá∆o da impressora"
     btFile AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     cFile AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rsExecution AT ROW 5.75 COL 3 HELP
          "Modo de Execuá∆o" NO-LABEL
/*      lImprimeParam AT ROW 8.21 COL 4 */
     text-destino AT ROW 1.63 COL 1.86 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
/*      text-impressao AT ROW 7.25 COL 1 COLON-ALIGNED NO-LABEL */
     RECT-7 AT ROW 1.92 COL 2.14
/*      RECT-8 AT ROW 7.5 COL 2 */
     RECT-9 AT ROW 5.29 COL 2.14 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.81
         SIZE 76.86 BY 10.15
         FONT 1.

DEFINE FRAME fPage3
     iClassificacao AT ROW 2.71 COL 6.57 HELP
          "Classificaá∆o para emiss∆o do relat¢rio" NO-LABEL
     cLabelClassif AT ROW 1.5 COL 2.86 COLON-ALIGNED NO-LABEL
     RECT-17 AT ROW 1.96 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.81
         SIZE 76.86 BY 10.15
         FONT 1.

DEFINE FRAME fPage4
     lParamAtivos AT ROW 2.79 COL 6.29
     lParamRestrCC AT ROW 3.79 COL 6.29
     lParamRestrCCRE AT ROW 4.79 COL 6.29
     lParamInativos AT ROW 5.79 COL 6.29
     dtAvalSituacao AT ROW 7.71 COL 14 COLON-ALIGNED
     cLabelParam AT ROW 1.67 COL 2 COLON-ALIGNED NO-LABEL
     RECT-16 AT ROW 2 COL 2.14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.81
         SIZE 76.86 BY 10.15
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wReport ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 14.75
         WIDTH              = 90
         MAX-HEIGHT         = 27.83
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.83
         VIRTUAL-WIDTH      = 146.29
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
       FRAME fPage3:FRAME = FRAME fpage0:HANDLE
       FRAME fPage4:FRAME = FRAME fpage0:HANDLE
       FRAME fPage6:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR FRAME fPage3
                                                                        */
/* SETTINGS FOR FRAME fPage4
                                                                        */
ASSIGN 
       cLabelParam:PRIVATE-DATA IN FRAME fPage4     = 
                "Considera Fornecedores".

/* SETTINGS FOR FRAME fPage6
                                                                        */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME fPage6     = 
                "Destino".

/* ASSIGN                                                   */
/*        text-impressao:PRIVATE-DATA IN FRAME fPage6     = */
/*                 "ParÉmetros de Impress∆o".               */

ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME fPage6     = 
                "Execuá∆o".

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
ON END-ERROR OF wReport
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON WINDOW-CLOSE OF wReport
DO:
  /* This event will close the window and terminate the procedure.  */
  {report/logfin.i}  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
    {report/rparq.i}
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


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME rsDestiny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDestiny wReport
ON VALUE-CHANGED OF rsDestiny IN FRAME fPage6
DO:
do  with frame fPage6:
    case self:screen-value:
        when "1" then do:
            assign cFile:sensitive       = no
                   cFile:visible         = yes
                   btFile:visible        = no
                   btConfigImpr:visible  = yes.
        end.
        when "2" then do:
            assign cFile:sensitive       = yes
                   cFile:visible         = yes
                   btFile:visible        = yes
                   btConfigImpr:visible  = no.
        end.
        when "3" then do:
            assign cFile:visible         = no
                   cFile:sensitive       = no
                   btFile:visible        = no
                   btConfigImpr:visible  = no.
        end.
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wReport 


/*--- L¢gica para inicializaá∆o do programam ---*/

{report/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AfterInitializeInterface wReport 
PROCEDURE AfterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign dtAvalSituacao:screen-value in frame fPage4 = string(today)
       cLabelParam:screen-value in frame fPage4    = "Considera Fornecedores"
       cLabelClassif:SCREEN-VALUE IN FRAME fPage3  = "Classifica Por:".

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
/*** Relatorio ***/
do on error undo, return error on stop  undo, return error:
    {report/rpexa.i}
    
    if input frame fPage6 rsDestiny = 2 and
       input frame fPage6 rsExecution = 1 then do:
        run utp/ut-vlarq.p (input input frame fPage6 cFile).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show", input 73, input "").
            apply "ENTRY":U to cFile in frame fPage6.
            return error.
        end.
    end.
    /* Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p†gina com 
       problemas e colocar o focus no campo com problemas */
    
    /* Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */

    if  v_cdn_empres_usuar <> ? then
       assign i-ep-codigo-usuario = v_cdn_empres_usuar.
   
    create tt-param.
    assign tt-param.usuario             = c-seg-usuario
           tt-param.destino             = input frame fPage6 rsDestiny
           tt-param.data-exec           = today
           tt-param.hora-exec           = time
           tt-param.classifica          = input frame fPage3 iClassificacao
           tt-param.desc-classifica     = entry((tt-param.classifica - 1) * 2 + 1, iClassificacao:radio-buttons in frame fPage3)
           tt-param.ep-codigo           = i-ep-codigo-usuario
           tt-param.i-cod-emitente-ini  = input frame fPage2 iCodEmitenteIni 
           tt-param.i-cod-emitente-fim  = input frame fPage2 iCodEmitenteFim 
           tt-param.i-cod-gr-forn-ini   = input frame fPage2 iCodGrFornIni
           tt-param.i-cod-gr-forn-fim   = input frame fPage2 iCodGrFornFim
           tt-param.c-cgc-ini           = input frame fPage2 cCgcIni
           tt-param.c-cgc-fim           = input frame fPage2 cCgcFim
           tt-param.c-nome-abrev-ini    = input frame fPage2 cNomeAbrevIni
           tt-param.c-nome-abrev-fim    = input frame fPage2 cNomeAbrevFim
           tt-param.l-param-ativos      = input frame fPage4 lParamAtivos
           tt-param.l-param-restr-cc    = input frame fPage4 lParamRestrCC
           tt-param.l-param-restr-cc-re = input frame fPage4 lParamRestrCCRE
           tt-param.l-param-inativos    = input frame fPage4 lParamInativos
           tt-param.dt-aval-situacao    = input frame fPage4 dtAvalSituacao
           tt-param.parametro           = NO /*input frame fPage6 lImprimeParam*/ .
           
           
    if tt-param.destino = 1 then 
        assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /* Executar do programa RP.P que ir† criar o relat¢rio */
    {report/rpexb.i}
    
    SESSION:SET-WAIT-STATE("GENERAL":U).

    {report/rprun.i esp/escd0422rp.p}
    
    {report/rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
/*     {report/rptrm.i} */
end.

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

