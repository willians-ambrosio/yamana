&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wReport 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCE0402 2.00.00.001}

def buffer empresa     for ems2cadme.empresa.


CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESCE0402
&GLOBAL-DEFINE Version        2.00.00.001
&GLOBAL-DEFINE VersionLayout  

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Sele‡Æo,ImpressÆo

&GLOBAL-DEFINE PGLAY          
&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGCLA          
&GLOBAL-DEFINE PGPAR          
&GLOBAL-DEFINE PGDIG          
&GLOBAL-DEFINE PGIMP          YES
&GLOBAL-DEFINE PGLOG          

&GLOBAL-DEFINE RTF            

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btHelp2
&GLOBAL-DEFINE page1Widgets   
&GLOBAL-DEFINE page2Widgets   
&GLOBAL-DEFINE page3Widgets   
&GLOBAL-DEFINE page4Widgets   
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
&GLOBAL-DEFINE page6Text      text-destino text-modo
&GLOBAL-DEFINE page7Text      
&GLOBAL-DEFINE page8Text   

&GLOBAL-DEFINE page1Fields    
&GLOBAL-DEFINE page2Fields    cCodEstab cDescEstab cItCodigo cDescItem cData
&GLOBAL-DEFINE page3Fields    
&GLOBAL-DEFINE page4Fields    
&GLOBAL-DEFINE page5Fields    
&GLOBAL-DEFINE page6Fields    cFile
&GLOBAL-DEFINE page7Fields    
&GLOBAL-DEFINE page8Fields    

/* Parameters Definitions ---                                           */

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field it-codigo        like item.it-codigo
    field data-ini         like param-estoq.mensal-ate 
    field data-fim         like param-estoq.mensal-ate 
    field mes-nome         as character
    field nome-empresa     like estabelec.nome.

define temp-table tt-estrutura no-undo
    field it-codigo    like item.it-codigo
    field inform-compl like item.inform-compl
    field codigo-refer like item.codigo-refer
    field cod-estabel  like item.cod-estabel
    field niv-mais-bai like item.niv-mais-bai
    field un           like item.un
    field num-sequen   as integer column-label 'Seq'
    
    field qtidade-ajust     like saldo-estoq.qtidade-ini column-label 'Qtde Ajust'

    field qtidade-ini       like saldo-estoq.qtidade-ini column-label 'Qtde Inic'
    field qtidade-from      like saldo-estoq.qtidade-ini column-label 'Qtde From'
    field qtidade-movto     like saldo-estoq.qtidade-ini column-label 'Qtde Movto'
    field qtidade-to        like saldo-estoq.qtidade-ini column-label 'Qtde To'
    field qtidade-fin       like saldo-estoq.qtidade-fin column-label 'Qtde Fin'
                           
    field sald-cash-ini     like movto-estoq.valor-mat-m column-label 'Vl Cash Inic' 
    field sald-cash-from    like movto-estoq.valor-mat-m column-label 'Vl Cash From' 
    field sald-cash-movto   like movto-estoq.valor-mat-m column-label 'Vl Cash Movto'
    field sald-cash-to      like movto-estoq.valor-mat-m column-label 'Vl Cash To'   
    field sald-cash-fin     like movto-estoq.valor-mat-m column-label 'Vl Cash Fin'  

    field sald-nocash-ini   like movto-estoq.valor-mat-m column-label 'Vl No Cash Inic' 
    field sald-nocash-from  like movto-estoq.valor-mat-m column-label 'Vl No Cash From' 
    field sald-nocash-movto like movto-estoq.valor-mat-m column-label 'Vl No Cash Movto'
    field sald-nocash-to    like movto-estoq.valor-mat-m column-label 'Vl No Cash To'   
    field sald-nocash-fin   like movto-estoq.valor-mat-m column-label 'Vl No Cash Fin'  

    index id is primary unique num-sequen it-codigo ascending.
 

/* Variable Definitions */
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-rtf              as char    no-undo.
def var c-arq-layout       as char    no-undo.      
def var c-arq-temp         as char    no-undo.
DEF VAR c-modelo-default   AS CHAR    NO-UNDO.

def stream s-imp.

define variable data-medio  like param-estoq.mensal-ate no-undo.
define variable c-lista     as character no-undo.
define variable c-lista-aux as character no-undo.
              
function fnMonthName returns character (input Data as date) forward.

{cdp/cdcfgmat.i}
{utp/ut-glob.i}


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar btOK btCancel btHelp2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */


FUNCTION fnMonthName returns character
  ( input Data as date)  FORWARD.

/* _UIB-CODE-BLOCK-END */



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

DEFINE VARIABLE cCodEstab AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE cData AS DATE FORMAT "99/99/9999" 
     LABEL "Calcula at‚":R21 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cDescEstab AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 37.72 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cDescItem AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cItCodigo AS CHARACTER FORMAT "x(16)" 
     LABEL "Item Final":R10 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 82.14 BY 2.75.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 82.14 BY 1.75.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 82.14 BY 1.75.

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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Exec" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsDestiny AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3,
"Excel", 4
     SIZE 44 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsExecution AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.86 BY .92
     FONT 1 NO-UNDO.

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

DEFINE FRAME fPage2
     cCodEstab AT ROW 3.83 COL 23.86 COLON-ALIGNED HELP
          "C¢digo do Estabelecimento"
     cDescEstab AT ROW 3.83 COL 35 NO-LABEL
     cItCodigo AT ROW 6.54 COL 18.43 HELP
          "C¢digo do Item"
     cDescItem AT ROW 6.54 COL 41.72 NO-LABEL
     cData AT ROW 8.25 COL 13 HELP
          "Data do Stock Control"
     RECT-10 AT ROW 2.96 COL 2.14
     RECT-12 AT ROW 6.13 COL 2.14
     RECT-13 AT ROW 7.79 COL 2.14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
         FONT 1.

DEFINE FRAME fPage6
     rsDestiny AT ROW 2.38 COL 3.14 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     cFile AT ROW 3.63 COL 3.14 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     btFile AT ROW 3.5 COL 43 HELP
          "Escolha do nome do arquivo"
     btConfigImpr AT ROW 3.5 COL 43 HELP
          "Configura‡Æo da impressora"
     rsExecution AT ROW 9.5 COL 2.86 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.63 COL 1.86 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 8.75 COL 1.14 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 9 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
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
       FRAME fPage6:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR FILL-IN cData IN FRAME fPage2
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN cDescEstab IN FRAME fPage2
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN cDescItem IN FRAME fPage2
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN cItCodigo IN FRAME fPage2
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME fPage6
   Custom                                                               */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME fPage6     = 
                "Destino".

ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME fPage6     = 
                "Execu‡Æo".

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


&Scoped-define FRAME-NAME fPage6
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


&Scoped-define FRAME-NAME fpage0
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
&Scoped-define SELF-NAME cCodEstab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCodEstab wReport
ON VALUE-CHANGED OF cCodEstab IN FRAME fPage2 /* Estabelecimento */
DO:
    
        if param-estoq.tp-fech = 2 then do :
           find first estab-mat 
               where estab-mat.cod-estabel = input frame fPage2 cCodEstab
               no-lock no-error. 
            if avail estab-mat then
                assign data-medio = estab-mat.mensal-ate.
        end.
        else 
            assign data-medio = param-estoq.mensal-ate.
    
    
    find first estabelec 
        where estabelec.cod-estabel = input frame fPage2 cCodEstab
        no-lock no-error.
    
    assign cDescEstab:screen-value in frame fPage2 = if avail estabelec then estabelec.nome else ''
           cData:screen-value in frame fPage2      = string(data-medio).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cItCodigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cItCodigo wReport
ON LEAVE OF cItCodigo IN FRAME fPage2 /* Item Final */
DO:
  find first item 
      where item.it-codigo = input frame fPage2 cItCodigo
      no-lock no-error.
  if avail item then
      cDescItem:screen-value in frame fPage2 = item.desc-item. /*inform-compl*/
  else
      cDescItem:screen-value in frame fPage2 = '.'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cItCodigo wReport
ON MOUSE-SELECT-DBLCLICK OF cItCodigo IN FRAME fPage2 /* Item Final */
or F5 of cItCodigo in frame fPage2
DO:
/*
    {method/zoomfields.i &ProgramZoom="inzoom/z01in172.w"
                         &FieldZoom1="it-codigo"
                         &FieldScreen1="cItCodigo"
                         &Frame1="fPage2"}
*/
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
        when "1":U then do:
            assign cFile:sensitive       = no
                   cFile:visible         = yes
                   btFile:visible        = no
                   btConfigImpr:visible  = yes
                   .
        end.
        when "2":U then do:
            assign cFile:sensitive       = yes
                   cFile:visible         = yes
                   btFile:visible        = yes
                   btConfigImpr:visible  = no
                   .
        end.
        when "3":U then do:
            assign cFile:visible         = no
                   cFile:sensitive       = no
                   btFile:visible        = no
                   btConfigImpr:visible  = no
                   .
        END.
        when "4":U then do:
            assign cFile:sensitive       = no
                   cFile:visible         = no
                   btFile:visible        = no
                   btConfigImpr:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
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


/*:T--- L¢gica para inicializa‡Æo do programam ---*/
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
    find first param-global no-lock no-error.
    find first param-estoq  no-lock no-error.
    find first param-cp     no-lock no-error.
    
    for each estabelec 
        where estabelec.ep-codigo = i-ep-codigo-usuario
        no-lock :
    
        if c-lista = "" then 
            assign c-lista     = string(string(estabelec.cod-estabel),"x(3)")
                   c-lista-aux = c-lista.     
        else
            assign c-lista = c-lista + ',' + string(string(estabelec.cod-estabel),"x(3)").
    end.
        
    assign cCodEstab:list-items   in frame fPage2 = ?
           cCodEstab:list-items   in frame fPage2 = c-lista
           cCodEstab:screen-value in frame fPage2 = c-lista-aux
           c-lista = "".

    apply 'value-changed':U to cCodEstab in frame fPage2.
    apply 'leave':U         to cItCodigo in frame fPage2.
    
    assign cDescEstab:sensitive   in frame fPage2  = no
           cDescItem:sensitive    in frame fPage2  = no
           cData:sensitive        in frame fPage2  = no
           
           rsExecution:sensitive  in frame fPage6  = no
           cFile:visible          in frame fPage6  = no
           cFile:sensitive        in frame fPage6  = no
           btFile:visible         in frame fPage6  = no
           btConfigImpr:visible   in frame fPage6  = no.
    
    /* Logica Para Desabilitar Radio-Buttons ---                               */
    if rsDestiny:disable(entry(1,(rsDestiny:radio-buttons in frame fPage6))) then.
    if rsDestiny:disable(entry(3,(rsDestiny:radio-buttons in frame fPage6))) then.
    if rsDestiny:disable(entry(5,(rsDestiny:radio-buttons in frame fPage6))) then.
       
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
do on error undo, return error on stop  undo, return error:
    /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */
    
    
    
    /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame fPage6 rsDestiny
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.it-codigo = input frame fPage2 cItCodigo
           tt-param.data-ini  = date(month(input frame fPage2 cData),01,year(input frame fPage2 cData))
           tt-param.data-fim  = input frame fPage2 cData
           tt-param.mes-nome  = fnMonthName(input frame fPage2 cData).

        if not avail param-cp then 
            find first param-cp no-lock no-error.

        find first estabelec 
            where estabelec.cod-estabel = param-cp.cod-estab
            exclusive-lock no-error.
        if avail estabelec then
            assign tt-param.nome-empresa = estabelec.nome.

    if tt-param.destino = 1 
    then 
        assign tt-param.arquivo = "":U.
    else if  tt-param.destino = 2 
        then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
    
    
    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
        IF param-cp.cod-estab = "301" OR
           param-cp.cod-estab = "302" THEN 
            run esp/esce0402b.p (input table tt-param).
        ELSE
            run esp/esce0402rp.p (input table tt-param).
    
    {report/rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    delete tt-param.
    
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */


FUNCTION fnMonthName returns character
  ( input Data as date) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  define variable month-number       as integer                  no-undo.
  define variable month-name         as character                no-undo.

  assign month-number = month(Data)
         month-name   = "Stock Schedule " + entry(month-number,"January,February,March,April,May,June,July,August,September,October,November,December") +
                        ", " + string(year(Data)).

  return month-name.   

end function.

/* _UIB-CODE-BLOCK-END */


