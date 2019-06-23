&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wReport 
/********************************************************************************
** Copyright DATASUL S.A. (2008)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esmv0701 2.06.00.000}  /*** 010005 ***/
/********************************************************************************
** Copyright DATASUL S.A. (2008)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{cdp/cdcfgmnt.i}
CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program       esmv0701
&GLOBAL-DEFINE Version        2.06.00.000
&GLOBAL-DEFINE VersionLayout  

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Sele��o,Impress�o

&GLOBAL-DEFINE PGLAY          NO
&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGCLA          NO
&GLOBAL-DEFINE PGPAR          NO
&GLOBAL-DEFINE PGDIG          NO
&GLOBAL-DEFINE PGIMP          YES
&GLOBAL-DEFINE PGLOG          NO

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btHelp2
&GLOBAL-DEFINE page1Widgets                                 
&GLOBAL-DEFINE page2Widgets  fi-dt-programacao-ini ~ fi-dt-programacao-fim ~ fi-empresa-ini ~ fi-empresa-fim ~ fi-estabelecimento-ini ~ fi-estabelecimento-fim ~ fi-equipamento-ini ~ fi-equipamento-fim ~ fi-oficina-ini ~ fi-oficina-fim ~         


&GLOBAL-DEFINE page3Widgets  
&GLOBAL-DEFINE page4Widgets   
                              
                              
&GLOBAL-DEFINE page5Widgets   
&GLOBAL-DEFINE page6Widgets   rsDestiny ~
                              btConfigImpr ~
                              btFile ~
                              rsExecution ~
                              
&GLOBAL-DEFINE page7Widgets   
&GLOBAL-DEFINE page8Widgets   

&GLOBAL-DEFINE page0Text      
&GLOBAL-DEFINE page1Text      
&GLOBAL-DEFINE page2Text      
&GLOBAL-DEFINE page3Text      
&GLOBAL-DEFINE page4Text      
&GLOBAL-DEFINE page5Text      
&GLOBAL-DEFINE page6Text      text-destino text-modo  
&GLOBAL-DEFINE page7Text      
&GLOBAL-DEFINE page8Text   

&GLOBAL-DEFINE page1Fields    
&GLOBAL-DEFINE page2Fields  fi-dt-programacao-ini ~ fi-dt-programacao-fim ~ fi-empresa-ini ~ fi-empresa-fim ~ fi-estabelecimento-ini ~ fi-estabelecimento-fim ~ fi-equipamento-ini ~ fi-equipamento-fim ~ fi-oficina-ini ~ fi-oficina-fim ~          

&GLOBAL-DEFINE page3Fields    
&GLOBAL-DEFINE page4Fields    
&GLOBAL-DEFINE page5Fields    
&GLOBAL-DEFINE page6Fields    cFile
&GLOBAL-DEFINE page7Fields    
&GLOBAL-DEFINE page8Fields    

/* Parameters Definitions ---                                           */
      DEF temp-table tt-param
        field destino              as integer
        field arquivo              as char
        field usuario              as char
        field data-exec            as date
        field hora-exec            as integer
        field parametro            as logical
        FIELD formato              as integer
        field v_num_tip_aces_usuar as integer
        field i-ep-codigo          as INTEGER
        field naoIniciado          as LOGICAL
        FIELD Iniciado             AS LOGICAL
        FIELD Terminado            AS LOGICAL
        field estado               as integer
        field tipo                 as integer
        field desc-classifica      as char format "x(40)"
        field rs-considera         as integer
        field desc-considera       as char format "x(40)"
        field l-motorizados        as logical  
        field l-n-motorizados      as logical
        field l-ativos             as logical
        field l-Inativos           as logical 
        field l-proprios           as logical  
        field l-terceiros          as logical
        field l-combust            as logical
        field l-caract             as logical
        field l-comptes            as logical
        field l-hist               as logical  
        field l-eventos            as logical
        field l-planos             as logical
        field l-config             as logical 
        field l-param              as logical
        field l-Insere             as LOGICAL
        field dt-programacao-ini   like mmv-eqpto-calend-plano.dat-movto
        field dt-programacao-fim   like mmv-eqpto-calend-plano.dat-movto
        field i-empresa-ini        like mmv-eqpto-calend-plano.ep-codigo
        field i-empresa-fim        like mmv-eqpto-calend-plano.ep-codigo
        field c-estabelec-ini      like mmv-ord-manut.cod-estabel
        field c-estabelec-fim      like mmv-ord-manut.cod-estabel
        field c-equipamento-ini    like mmv-eqpto-calend-plano.cod-eqpto
        field c-equipamento-fim    like mmv-eqpto-calend-plano.cod-eqpto
        field c-oficina-ini        like mmv-eqpto-calend-plano.cod-ofici
        field c-oficina-fim        like mmv-eqpto-calend-plano.cod-ofici.
    
        

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

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
     LABEL "&Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&Executar" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE fi-dt-programacao-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/08 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-programacao-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/08 
     LABEL "Data Programa��o" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-fim AS INTEGER FORMAT ">>9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 6.43 BY .88.

DEFINE VARIABLE fi-empresa-ini AS INTEGER FORMAT "->>9" INITIAL 0 
     LABEL "Empresa":R14 
     VIEW-AS FILL-IN 
     SIZE 6.43 BY .88.

DEFINE VARIABLE fi-equipamento-fim AS CHARACTER FORMAT "X(256)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE fi-equipamento-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Equipamento" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE fi-estabelecimento-fim AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6.43 BY .88.

DEFINE VARIABLE fi-estabelecimento-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Estabelecimento":R21 
     VIEW-AS FILL-IN 
     SIZE 6.43 BY .88.

DEFINE VARIABLE fi-oficina-fim AS CHARACTER FORMAT "X(40)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-oficina-ini AS CHARACTER FORMAT "X(40)":U 
     LABEL "Oficina" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE BUTTON btConfigImpr 
     IMAGE-UP FILE "image/im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btFile 
     IMAGE-UP FILE "image/im-sea":U
     IMAGE-INSENSITIVE FILE "image/ii-sea":U
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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu��o" 
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

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 14.17 COL 2
     btCancel AT ROW 14.17 COL 13
     btHelp2 AT ROW 14.17 COL 80
     rtToolBar AT ROW 13.92 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.67
         FONT 1.

DEFINE FRAME fPage2
     fi-dt-programacao-ini AT ROW 1.83 COL 21.57 COLON-ALIGNED
     fi-dt-programacao-fim AT ROW 1.75 COL 43 COLON-ALIGNED NO-LABEL
     fi-empresa-ini AT ROW 2.83 COL 27.57 COLON-ALIGNED HELP
          "C�digo do Equipamento"
     fi-empresa-fim AT ROW 2.83 COL 43 COLON-ALIGNED HELP
          "C�digo do Equipamento" NO-LABEL
     fi-estabelecimento-ini AT ROW 3.83 COL 27.57 COLON-ALIGNED HELP
          "C�digo Grupo Equipamento"
     fi-estabelecimento-fim AT ROW 3.83 COL 43 COLON-ALIGNED HELP
          "C�digo Grupo Equipamento" NO-LABEL
     fi-equipamento-ini AT ROW 4.83 COL 13 COLON-ALIGNED
     fi-equipamento-fim AT ROW 4.83 COL 43 COLON-ALIGNED NO-LABEL
     fi-oficina-ini AT ROW 5.83 COL 21.57 COLON-ALIGNED
     fi-oficina-fim AT ROW 5.83 COL 43 COLON-ALIGNED NO-LABEL
     IMAGE-10 AT ROW 4.83 COL 41.14
     IMAGE-26 AT ROW 1.83 COL 36.72
     IMAGE-27 AT ROW 2.83 COL 36.72
     IMAGE-28 AT ROW 2.83 COL 41.14
     IMAGE-29 AT ROW 1.83 COL 41.14
     IMAGE-3 AT ROW 5.83 COL 36.72
     IMAGE-4 AT ROW 5.83 COL 41.14
     IMAGE-7 AT ROW 3.83 COL 36.72
     IMAGE-8 AT ROW 3.83 COL 41.14
     IMAGE-9 AT ROW 4.83 COL 36.72
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.79
         SIZE 76.86 BY 7.21
         FONT 1.

DEFINE FRAME fPage6
     rsDestiny AT ROW 2.38 COL 3.29 HELP
          "Destino de Impress�o do Relat�rio" NO-LABEL
     btFile AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     btConfigImpr AT ROW 3.58 COL 43.29 HELP
          "Configura��o da impressora"
     cFile AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat�rio" NO-LABEL
     rsExecution AT ROW 5.75 COL 3 HELP
          "Modo de Execu��o" NO-LABEL
     text-destino AT ROW 1.63 COL 1.86 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-10 AT ROW 5.29 COL 2.14
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.79
         SIZE 76.86 BY 7.21
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wReport ASSIGN
         HIDDEN             = YES
         TITLE              = "Listagem de Equipamentos"
         HEIGHT             = 14.67
         WIDTH              = 90
         MAX-HEIGHT         = 29
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29
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
       FRAME fPage6:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME fPage2
   Custom                                                               */
/* SETTINGS FOR FRAME fPage6
                                                                        */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME fPage6     = 
                "Destino".

ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME fPage6     = 
                "Execu��o".

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
ON END-ERROR OF wReport /* Listagem de Equipamentos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON WINDOW-CLOSE OF wReport /* Listagem de Equipamentos */
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


/*--- L�gica para inicializa��o do programam ---*/
{report/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wReport 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {utp/ut-liter.i "Data Programa��o"}
    assign fi-dt-programacao-ini:label in frame fPage2 = trim(return-value).
    {utp/ut-liter.i "Empresa"}
    assign fi-empresa-ini:label in frame fPage2 = trim(return-value).
    {utp/ut-liter.i "Estabelecimento"}
    assign fi-estabelecimento-ini:label in frame fPage2 = trim(return-value).
    {utp/ut-liter.i "Equipamento"}
    assign fi-equipamento-ini:label in frame fPage2 = trim(return-value).
    {utp/ut-liter.i "Oficina"}
    assign fi-oficina-ini:label in frame fPage2 = trim(return-value).
    
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
     
    /* Aqui s�o gravados os campos da temp-table que ser� passada como par�metro
       para o programa RP.P */
    
    create tt-param.
    ASSIGN tt-param.dt-programacao-ini    = input frame fpage2  fi-dt-programacao-ini       
           tt-param.dt-programacao-fim    = input frame fpage2  fi-dt-programacao-fim       
           tt-param.i-empresa-ini         = input frame fpage2  fi-empresa-ini       
           tt-param.i-empresa-fim         = input frame fpage2  fi-empresa-fim       
           tt-param.c-estabelec-ini       = input frame fpage2  fi-estabelecimento-ini   
           tt-param.c-estabelec-fim       = input frame fpage2  fi-estabelecimento-fim   
           tt-param.c-equipamento-ini     = input frame fpage2  fi-equipamento-ini      
           tt-param.c-equipamento-fim     = input frame fpage2  fi-equipamento-fim       
           tt-param.c-oficina-ini         = input frame fpage2  fi-oficina-ini     
           tt-param.c-oficina-fim         = input frame fpage2  fi-oficina-fim.

    if tt-param.destino = 1 
    then 
        assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /* Executar do programa RP.P que ir� criar o relat�rio */
    {report/rpexb.i}

    SESSION:SET-WAIT-STATE("GENERAL":U).
    {report/rprun.i mvp/esmv0701rp.p}
    
    {report/rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {report/rptrm.i}
end.
&ELSE
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

