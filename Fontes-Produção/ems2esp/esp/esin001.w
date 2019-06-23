

/* Connected Databases 
          mgesp            PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-controle-inv-esp NO-UNDO LIKE controle-inv-esp
       field r-rowid as rowid.




/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/************************************************************************
**
**  i-prgvrs.i - Programa para criacao do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

/*Altera‡Æo - 08/09/2006 - tech1007 - Alterado para possuir a defini‡Æo dos pr‚processadores logo no in¡cio do programa*/
/**** Altera‡Æo efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
/*************************************************
* i_dbvers.i - Include de versÆo de banco de dados   
**************************************************/

/* Preprocessadores que identificam os bancos do Produto EMS 5 */

/* Preprocessadores que identificam os bancos do Produto EMS 2 */
/*RAC Incorporado na 2.04*/

/* Preprocessadores que identificam os bancos do Produto HR 2 */

/* Fim */

 
/*Fim altera‡Æo 08/09/2006*/

def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.000[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.000"
       c-prg-obj = "ESIN001".

/* Alteracao - 02/10/2006 - Nakamura - Incluida chamada a include que verifica a integridade dos programas do registro de produto */

 

/*Altera‡Æo - 08/09/2006 - tech1007 - Altera‡Æo para exibir o nome do programa que executou o programa que ser  exibido no extrato de versÆo
                                      Solicita‡Æo realizada na FO 1239827*/

/*Fim altera‡Æo 08/09/2006*/


if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "ESIN001"
        no-lock no-error.
        
   if not avail prog_dtsul then do:
          if  c-prg-obj begins "btb":U then
              assign c-prg-obj = "btb~/":U + c-prg-obj.
          else if c-prg-obj begins "men":U then
                  assign c-prg-obj = "men~/":U + c-prg-obj.
          else if c-prg-obj begins "sec":U then
                  assign c-prg-obj = "sec~/":U + c-prg-obj.
          else if c-prg-obj begins "utb":U then
                  assign c-prg-obj = "utb~/":U + c-prg-obj.
          find prog_dtsul where
               prog_dtsul.nom_prog_ext begins c-prg-obj no-lock no-error.
   end .            /*if*/
    
    output to value(c-arquivo-log) append.

    /*Altera‡Æo - 08/09/2006 - tech1007 - Altera‡Æo para exibir o nome do programa que executou o programa que ser  exibido no extrato de versÆo
                                      Solicita‡Æo realizada na FO 1239827*/
    
        /*FO 1329.898 - tech1139 - 01/08/2006 */
        PUT "ESIN001" AT 1 "2.00.00.000" AT 69 STRING(TODAY,'99/99/99') AT 84 STRING(TIME,'HH:MM:SS':U) AT 94 SKIP.
        /*FO 1329.898 - tech1139 - 01/08/2006 */
    
    /*Fim altera‡Æo 08/09/2006*/
                                                  
    if  avail prog_dtsul then do:
        if  prog_dtsul.nom_prog_dpc <> "" then
            put "DPC : ":U at 5 prog_dtsul.nom_prog_dpc  at 12 skip.
        if  prog_dtsul.nom_prog_appc <> "" then
            put "APPC: ":U at 5 prog_dtsul.nom_prog_appc at 12 skip.
        if  prog_dtsul.nom_prog_upc <> "" then
            put "UPC : ":U at 5 prog_dtsul.nom_prog_upc  at 12 skip.
    end.
    output close.        
end.  
error-status:error = no.
/***************************************************
** i_dbtype.i - Tipo de Gerenciadores utilizados
***************************************************/


            
    /* Preprocessadores que identificam os bancos do Produto EMS 5 */
                    
    /* Preprocessadores que identificam os bancos do Produto EMS 2 */
                                                                                                                                        
    
    /* Preprocessadores que identificam os bancos do Produto HR 2 */
            

/* Fim */

 

/*alteracao Anderson(tech540) em 04/02/2003 Include com a definicao 
da temp table utilizada nas includes btb008za.i1 e btb008za.i2 para 
execucao de programas via rpc*/
def temp-table tt-control-prog NO-UNDO
    field cod-versao-integracao as integer       format '999'
    field cod-erro              as integer       format '99999'
    field desc-erro             as character     format 'x(60)'
    field wgh-servid-rpc        as widget-handle format '>>>>>>9'.
 
 
/*fim alteracao Anderson 04/02/2003*/


/* altera‡Æo feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */

DEFINE NEW GLOBAL SHARED VARIABLE hWenController AS HANDLE     NO-UNDO.
/*Constante utilizada apenas para verificar se a vari vel acima foi definida */ 

/* fim da alatera‡Æo */

/* Altera‡Æo realizada por tech38629 - 19/07/2006 - Defini‡Æo do pre-processador para o facelift */
/****************************************************************/
/* i_fclpreproc.i                                               */
/* Criado por: tech38629                                        */
/* Data de cria‡Æo: 19/07/2006                                  */
/* Descri‡Æo: Define o pr‚-processador que indica a utiliza‡Æo  */
/*            do facelift                                       */
/****************************************************************/

 
/* Fim da altera‡Æo */

 

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */







/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/*******************************************************************
**
**  CD0666.I - Definicao temp-table de erros
**
*******************************************************************/
def new global shared var l-multi as logical initial yes.

def var l-del-erros as logical init yes.
def var v-nom-arquivo-cb as char format "x(50)" no-undo.
def var c-mensagem-cb    as char format "x(132)" no-undo.

def  temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)"
    field c-param  as char.

form
    space(04)
    tt-erro.cd-erro 
    space (02)
    c-mensagem-cb
    with width 132 no-box down stream-io frame f-consiste.

/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Mensagem",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tt-erro.cd-erro:label in frame f-consiste = trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Descri‡Æo",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-mensagem-cb:label in frame f-consiste = trim(return-value).
 
define variable wh-pesquisa         as handle     no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.
/* Local Variable Definitions (DBOs Handles) --- */
DEFINE VARIABLE h-dboes001 AS HANDLE NO-UNDO.
DEFINE VARIABLE h-esapi001   AS HANDLE NO-UNDO.
DEFINE VARIABLE rRowid       AS ROWID  NO-UNDO.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */


/* Name of designated FRAME-NAME and/or first browse and/or first query */

/* Standard List Definitions                                            */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */




/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wMaintenance AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU smFile 
       MENU-ITEM miFirst        LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM miPrev         LABEL "&Anterior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM miNext         LABEL "&Pr¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM miLast         LABEL "&éltimo"        ACCELERATOR "CTRL-END"
       RULE
       MENU-ITEM miGoTo         LABEL "&V  Para"       ACCELERATOR "CTRL-T"
       MENU-ITEM miSearch       LABEL "&Pesquisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM miAdd          LABEL "&Incluir"       ACCELERATOR "CTRL-INS"
       MENU-ITEM miCopy         LABEL "&Copiar"        ACCELERATOR "CTRL-C"
       MENU-ITEM miUpdate       LABEL "&Alterar"       ACCELERATOR "CTRL-A"
       MENU-ITEM miDelete       LABEL "&Eliminar"      ACCELERATOR "CTRL-DEL"
       RULE
       MENU-ITEM miUndo         LABEL "&Desfazer"      ACCELERATOR "CTRL-U"
       MENU-ITEM miCancel       LABEL "&Cancelar"      ACCELERATOR "CTRL-F4"
       RULE
       MENU-ITEM miSave         LABEL "&Salvar"        ACCELERATOR "CTRL-S"
       RULE
       MENU-ITEM miQueryJoins   LABEL "&Consultas"    
       MENU-ITEM miReportsJoins LABEL "&Relat¢rios"   
       RULE
       MENU-ITEM miExit         LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU smHelp 
       MENU-ITEM miContents     LABEL "&Conte£do"     
       RULE
       MENU-ITEM miAbout        LABEL "&Sobre..."     .

DEFINE MENU mbMain MENUBAR
       SUB-MENU  smFile         LABEL "&Arquivo"      
       SUB-MENU  smHelp         LABEL "&Ajuda"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btAdd 
     IMAGE-UP FILE "image\im-add":U
     IMAGE-INSENSITIVE FILE "image\ii-add":U
     LABEL "Add" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btCancel 
     IMAGE-UP FILE "image\im-can":U
     IMAGE-INSENSITIVE FILE "image\im-can":U
     LABEL "Cancel" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btCopy 
     IMAGE-UP FILE "image\im-copy":U
     IMAGE-INSENSITIVE FILE "image\ii-copy":U
     LABEL "Copy" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btDelete 
     IMAGE-UP FILE "image\im-era":U
     IMAGE-INSENSITIVE FILE "image\ii-era":U
     LABEL "Delete" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btExit 
     IMAGE-UP FILE "image\im-exi":U
     IMAGE-INSENSITIVE FILE "image\ii-exi":U
     LABEL "Exit" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btFirst 
     IMAGE-UP FILE "image\im-fir":U
     IMAGE-INSENSITIVE FILE "image\ii-fir":U
     LABEL "First":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btGoTo 
     IMAGE-UP FILE "image\im-enter":U
     IMAGE-INSENSITIVE FILE "image\ii-enter":U
     LABEL "Go To" 
     SIZE 4 BY 1.25.

DEFINE BUTTON btHelp 
     IMAGE-UP FILE "image\im-hel":U
     IMAGE-INSENSITIVE FILE "image\ii-hel":U
     LABEL "Help" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btLast 
     IMAGE-UP FILE "image\im-las":U
     IMAGE-INSENSITIVE FILE "image\ii-las":U
     LABEL "Last":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btNext 
     IMAGE-UP FILE "image\im-nex":U
     IMAGE-INSENSITIVE FILE "image\ii-nex":U
     LABEL "Next":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btPrev 
     IMAGE-UP FILE "image\im-pre":U
     IMAGE-INSENSITIVE FILE "image\ii-pre":U
     LABEL "Prev":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btQueryJoins 
     IMAGE-UP FILE "image\im-joi":U
     IMAGE-INSENSITIVE FILE "image\ii-joi":U
     LABEL "Query Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btReportsJoins 
     IMAGE-UP FILE "image\im-pri":U
     IMAGE-INSENSITIVE FILE "image\ii-pri":U
     LABEL "Reports Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btSave 
     IMAGE-UP FILE "image\im-sav":U
     IMAGE-INSENSITIVE FILE "image\ii-sav":U
     LABEL "Save" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btSearch 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "Search" 
     SIZE 4 BY 1.25.

DEFINE BUTTON btUndo 
     IMAGE-UP FILE "image\im-undo":U
     IMAGE-INSENSITIVE FILE "image\ii-undo":U
     LABEL "Undo" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btUpdate 
     IMAGE-UP FILE "image\im-mod":U
     IMAGE-INSENSITIVE FILE "image\ii-mod":U
     LABEL "Update" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE VARIABLE c-label-narrativa AS CHARACTER FORMAT "X(256)":U INITIAL "Narrativa:" 
      VIEW-AS TEXT 
     SIZE 6.86 BY .67 NO-UNDO.

DEFINE VARIABLE fi-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ordem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE fi-proj AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 6.5.

DEFINE RECTANGLE rtKeys
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 5.83.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.5
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     fi-estab AT ROW 5.25 COL 35.29 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     btFirst AT ROW 1.13 COL 1.57 HELP
          "Primeira ocorrˆncia"
     btPrev AT ROW 1.13 COL 5.57 HELP
          "Ocorrˆncia anterior"
     btNext AT ROW 1.13 COL 9.57 HELP
          "Pr¢xima ocorrˆncia"
     btLast AT ROW 1.13 COL 13.57 HELP
          "éltima ocorrˆncia"
     btGoTo AT ROW 1.13 COL 17.57 HELP
          "V  Para"
     btSearch AT ROW 1.13 COL 21.57 HELP
          "Pesquisa"
     btAdd AT ROW 1.13 COL 31 HELP
          "Inclui nova ocorrˆncia"
     btCopy AT ROW 1.13 COL 35 HELP
          "Cria uma c¢pia da ocorrˆncia corrente"
     btUpdate AT ROW 1.13 COL 39 HELP
          "Altera ocorrˆncia corrente"
     btDelete AT ROW 1.13 COL 43 HELP
          "Elimina ocorrˆncia corrente"
     btUndo AT ROW 1.13 COL 47 HELP
          "Desfaz altera‡äes"
     btCancel AT ROW 1.13 COL 51 HELP
          "Cancela altera‡äes"
     btSave AT ROW 1.13 COL 55 HELP
          "Confirma altera‡äes"
     btQueryJoins AT ROW 1.13 COL 74.72 HELP
          "Consultas relacionadas"
     btReportsJoins AT ROW 1.13 COL 78.72 HELP
          "Relat¢rios relacionados"
     btExit AT ROW 1.13 COL 82.72 HELP
          "Sair"
     btHelp AT ROW 1.13 COL 86.72 HELP
          "Ajuda"
     tt-controle-inv-esp.num-ord-inv AT ROW 4.25 COL 25 COLON-ALIGNED
          LABEL "Num. Ord. EMS"
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     tt-controle-inv-esp.cod-est-exec AT ROW 5.25 COL 25 COLON-ALIGNED
          LABEL "Estab Exec"
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     tt-controle-inv-esp.num-projeto AT ROW 6.25 COL 25 COLON-ALIGNED
          LABEL "Num Projeto"
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     tt-controle-inv-esp.num-ordem AT ROW 7.25 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     tt-controle-inv-esp.dt-trans AT ROW 9.04 COL 25 COLON-ALIGNED
          LABEL "Data Movimenta‡Æo"
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     tt-controle-inv-esp.valor-origem AT ROW 10.04 COL 25 COLON-ALIGNED
          LABEL "Valor MOB"
          VIEW-AS FILL-IN 
          SIZE 18 BY .88
     tt-controle-inv-esp.narrativa AT ROW 11.04 COL 27 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP MAX-CHARS 100 SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 62 BY 3.75
     tt-controle-inv-esp.sequencia AT ROW 3.25 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     c-label-narrativa AT ROW 11.04 COL 17.86 COLON-ALIGNED NO-LABEL
     fi-ordem AT ROW 7.25 COL 35.29 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-proj AT ROW 6.25 COL 35.29 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     RECT-1 AT ROW 8.67 COL 1
     rtKeys AT ROW 2.67 COL 1
     rtToolBar AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.29
         FONT 1.


/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Maintenance
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-controle-inv-esp T "?" NO-UNDO mgesp controle-inv-esp
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
   END-TABLES.
 */


/* *************************  Create Window  ************************** */


IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wMaintenance ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 14.21
         WIDTH              = 90
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE wMaintenance = CURRENT-WINDOW.

ASSIGN wMaintenance:MENUBAR    = MENU mbMain:HANDLE.
/* END WINDOW DEFINITION                                                */



/* ************************* Included-Libraries *********************** */


/* Procedure Description
"Method Library principal para Maintenance Template, que cont‚m defini‡äes e chamadas a outras Method Libraries."
*/


/*--------------------------------------------------------------------------
    Library    : maintenance/Maintenance.i
    Purpose    : Method Library principal para Maintenance Template, que 
                 cont‚m defini‡äes e chamadas a outras Method Libraries 

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* Global Variable Definitions ---                                        */
DEFINE NEW GLOBAL SHARED VARIABLE hWindowStyles AS HANDLE NO-UNDO.


        
    DEFINE NEW GLOBAL SHARED VARIABLE gr-controle-inv-esp AS ROWID NO-UNDO.


/* Local Temp-Table Definitions ---                                       */
/*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/

    
/* Procedure Description
"Include com defini‡Æo da temptable RowErrors."
*/


/*--------------------------------------------------------------------------
    Library    : method/dbotterr.i
    Purpose    : Include com defini‡Æo da temptable RowErrors

    Author     : John Cleber Jaraceski

    Notes      :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  **************************** */

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */



 


/*Alterado 08/11/2006 - tech1007 - FO 1301831 - Altera‡Æo para exibir o botÆo de LAST em programas Thin
/** Farley - Projeto SQL - 12/08/2003 **/
&IF "{&EMS_DBTYPE}":U = "MSS":U &THEN
    define variable iBotaoLast as integer no-undo.
    define variable hBotaoLast as handle  no-undo.
    define variable hFieldGroup as handle no-undo.
    define variable iAchaBtLast as integer no-undo.
    &GLOBAL-DEFINE LAST NO
    assign hBotaoLast = frame {&frame-name}:first-child.
    Repeat While Valid-handle(hBotaoLast):
        If iAchaBtLast = 10 Then leave. /* O objeto do botÆo Last sempre estar  presente nos 10 primeiros Handles da Frames fPage0 */
        If hBotaoLast:type = "field-group" Then Assign hFieldGroup = hBotaoLast.
        If hBotaoLast:type <> "Expression" Then Do:
            If hBotaoLast:type = "button" Then do:
                If hBotaoLast:name = "btLast" Then do:
                    assign hBotaoLast:sensitive = no
                           hBotaoLast:visible   = no
                           hBotaoLast:hidden    = yes.
                    leave.
                End.
            End.
        End.
        
        If Can-query(hBotaoLast,"first-child") Then Assign hBotaoLast = hBotaoLast:First-child.
        Else Assign hBotaoLast = hBotaoLast:next-sibling No-error.
        If Valid-handle(hBotaoLast) = No Then do:
            Assign hBotaoLast = hFieldGroup:next-sibling No-error.
        End.
        assign iAchaBtLast = iAchaBtLast + 1.
    End.
               
    assign hBotaoLast = menu mbMain:first-child
           iAchaBtLast = 0.
    Repeat while valid-handle(hBotaoLast):
        If iAchaBtLast = 10 Then leave.
        If can-query(hBotaoLast,"name") Then do:
            If hBotaoLast:name = "miLast" Then do:
                assign hBotaoLast:sensitive = no.
                leave.
            End.
        End.
        If can-query(hBotaoLast,"first-child") Then
            assign hBotaoLast = hBotaoLast:first-child.
        else assign hBotaoLast = hBotaoLast:next-sibling.
        assign iAchaBtLast = iAchaBtLast + 1.
    End.
&ENDIF
/** FIM - Farley **/
Fim altera‡Æo 08/11/2006*/

/* Local Variable Definitions ---                                         */
DEFINE VARIABLE cAction           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButtonsState     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButtonsStateAux  AS CHARACTER NO-UNDO.
DEFINE VARIABLE deColPanel        AS DECIMAL   NO-UNDO
    DECIMALS 2 INITIAL 31.
DEFINE VARIABLE deColPanelNav     AS DECIMAL   NO-UNDO
    DECIMALS 2 INITIAL 1.60.
DEFINE VARIABLE hFolder           AS HANDLE    NO-UNDO.
DEFINE VARIABLE hProgramZoom      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQueryJoins       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hReportsJoins     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hShowMsg          AS HANDLE    NO-UNDO.
DEFINE VARIABLE lMultipleAdd      AS LOGICAL   NO-UNDO
    INITIAL YES.
DEFINE VARIABLE lCustomExecuted   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lOverrideExecuted AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rCurrent          AS ROWID     NO-UNDO.

DEFINE VARIABLE c-nom-prog-dpc-mg97  AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nom-prog-appc-mg97 AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nom-prog-upc-mg97  AS CHARACTER NO-UNDO.

DEFINE VARIABLE epc-rowid            AS ROWID     NO-UNDO.


/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    define new global shared variable h-facelift as handle no-undo.
    if not valid-handle(h-facelift) then run btb/btb901zo.p persistent set h-facelift.


/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 10.17
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */



/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}

/********************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

/********************************************************************************
** Programa : include/i-sysvar.i
**
** Data : 02/06/1999
**
** Cria‡Æo : John Cleber Jaraceski
**
** Objetivo : Definicao das System Variables
**
** Ultima Alt : ?
*******************************************************************************/


        
    DEFINE VARIABLE c-programa-mg97       AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-versao-mg97         AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-modulo-mg97         AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-titulo-prog-mg97    AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-nom-manual-hlp-mg97 AS CHARACTER FORMAT "x(06)":U NO-UNDO.
    DEFINE VARIABLE c-cod-mod-mg97        AS CHARACTER                  NO-UNDO.
    DEFINE VARIABLE d-data-contrato       AS DATE                       NO-UNDO.
    DEFINE VARIABLE i-num-topico-hlp-mg97 AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE i-user-conectados     AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE i-licenca-usuar       AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE l-acesso-livre        AS LOGICAL                    NO-UNDO.


/* include/i-sysvar.i ---                                                     */

 
/***************************************************************************
**
**   btb008za.i0  -  Include para defini‡Æo de fun‡äes do RPC.
**
***************************************************************************/

FUNCTION rpc_exec         RETURNS logical   (input p_cod_program as character) in h-servid-rpc.
FUNCTION rpc_server       RETURNS handle    (input p_cod_program as character) in h-servid-rpc.
FUNCTION rpc_program      RETURNS character (input p_cod_program as character) in h-servid-rpc.
FUNCTION rpc_tip_exec     RETURNS logical   (input p_cod_program as character) in h-servid-rpc.
FUNCTION rpc_exec_set     RETURNS logical   (input p_cod_program as character, 
                                             input p_log_value as logical)     in h-servid-rpc.
                                             
DEFINE VARIABLE c-lst-prg-rpc as char    no-undo.
DEFINE VARIABLE l-inf-prg     as logical no-undo.
DEFINE VARIABLE i-prg-rpc     as integer no-undo.
 

/* Procedure Description
"Method Library que cont‚m as l¢gicas dos botäes do ToolBar."
*/


/*--------------------------------------------------------------------------
    Library    : maintenance/Buttons.i
    Purpose    : Method Library que cont‚m as l¢gicas dos botäes do ToolBar

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

DEFINE VARIABLE offQuery AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */



/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */



 





/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */




PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Inclui novo registro
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-ADD":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-ADD":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-ADD":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Seta vari vel cAction ---*/
    ASSIGN cAction = "ADD":U.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Cria novo registro para temp-table {&ttTable} no DBO ---*/
        RUN newRecord IN h-dboes001.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN h-dboes001 (OUTPUT TABLE tt-controle-inv-esp).
    
    
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.

    DO WITH FRAME fPage0:
        /*--- Seta vari vel cButtonsState com o estado atual dos botäes 
              de navega‡Æo ---*/
        ASSIGN
            
            cButtonsState = "":U
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btFirst:SENSITIVE)
            

            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btPrev:SENSITIVE)
            

            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btNext:SENSITIVE)
            

            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btLast:SENSITIVE)
            

            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btgoTo:SENSITIVE)
            

            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btSearch:SENSITIVE)
            

            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btAdd:SENSITIVE)
            

            

            

            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btDelete:SENSITIVE)
            
            
            /* tech14187 - 10/04/2007 - 1468007 - nÆo ‚ necess rio guardar o estado anterior destes botäes. */
/*             /*tech1139 - 05/12/2006 - FO 1387.046 */                                                         */
/*             &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN                                                  */
/*                 cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btQueryJoins:SENSITIVE)   */
/*             &ENDIF                                                                                           */
/*                                                                                                              */
/*             &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN                                                */
/*                 cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btReportsJoins:SENSITIVE) */
/*             &ENDIF                                                                                           */
/*             /*tech14207*/                                                                                    */
/*                                                                                                              */
/*             /*tech1139 - botÆo Exit */                                                                       */
/*             cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btExit:SENSITIVE)             */
/*                                                                                                              */
/*             /*tech1139 - botÆo Help */                                                                       */
/*             cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btHelp:SENSITIVE)             */
/*                                                                                                              */
/*             /*tech1139 - 05/12/2006 - FO 1387.046 */                                                         */
            .
        
        /*--- Seta vari vel cButtonsStateAux para ser passada como parƒmetro para o 
              m‚todo controlToolBar ---*/
        ASSIGN
            
            cButtonsStateAux = "":U
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U                        
            
            
            /*tech1139 - botÆo Exit */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U                        

           /*tech1139 - botÆo Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U                                     

            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .

    END.
    

    /*--- Seta estado dos botäes/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta estado das frames ---*/
    RUN enableFields IN THIS-PROCEDURE.

    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.    

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-ADD":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-ADD":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-ADD":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Cancela altera‡äes feitas para o registro corrente
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-CANCEL":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-CANCEL":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-CANCEL":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Elimina janela de mensagens de erro ---*/
    
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz somente a elimina‡Æo da instƒncia do utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i3
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz somente a elimina‡Æo da instƒncia do 
                 utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Destr¢i tela de mensagens de erros ---*/
IF VALID-HANDLE(hShowMsg) and
  hShowMsg:TYPE = "PROCEDURE":U and
  hShowMsg:FILE-NAME = "utp/ShowMessage.w":U THEN DO:
       RUN destroyInterface IN hShowMsg.
end.
/* _UIB-CODE-BLOCK-END */



 
    
    DO WITH FRAME fPage0:
        /*--- Seta vari vel cButtonsStateAux para ser passada como parƒmetro para o 
              m‚todo controlToolBar ---*/
        ASSIGN
            
            cButtonsStateAux = "":U
            
            
                
                                    
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY(1, cButtonsState)
            
            
            
                
                                    
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY(1 + 1, cButtonsState)
            
            
            
                
                                    
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY(1 + 1 + 1, cButtonsState)
            
            
            
                
                                    
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY(1 + 1 + 1 + 1, cButtonsState)
            
            
            
                
                                    
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY(1 + 1 + 1 + 1 + 1, cButtonsState)
            
            
            
                
                                    
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY(1 + 1 + 1 + 1 + 1 + 1, cButtonsState)
            
            
            
                
                                    
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1, cButtonsState)
            
            
            
            
            
            
            
                
                                    
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, cButtonsState)
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */ 
            /* tech14187 - 10/04/2007 - 1468007*/
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
                        
            /*botÆo Exit */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

            /*botÆo Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .
    END.
    
    /*--- Seta estado dos botäes/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).

    /*--- Seta estado das frames ---*/
    RUN disableFields IN THIS-PROCEDURE.
    
    /*--- Limpa temp-table {&ttTable} ---*/
    FOR EACH tt-controle-inv-esp:
        DELETE tt-controle-inv-esp.
    END.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Atualiza temp-table {&ttTable} com base no registro corrento do DBO ---*/
        
        /*--- Limpa temp-table RowErrors do DBO ---*/
        RUN emptyRowErrors IN h-dboes001.
        
        /*--- Posiciona DBO no registro corrente ---*/
        RUN bringCurrent IN h-dboes001.
        
        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN h-dboes001 (OUTPUT TABLE tt-controle-inv-esp).
    
    
    ASSIGN cAction = "":U.
    
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-CANCEL":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-CANCEL":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-CANCEL":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE copyRecord :
/*------------------------------------------------------------------------------
  Purpose:     Copia registro atual
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-COPY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-COPY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-COPY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Seta vari vel cAction ---*/
    ASSIGN cAction = "COPY":U.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
    
    DO WITH FRAME fPage0:
        /*--- Seta vari vel cButtonsState com o estado atual dos botäes 
              de navega‡Æo ---*/
        ASSIGN
            
            cButtonsState = "":U
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btFirst:SENSITIVE)
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btPrev:SENSITIVE)
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btNext:SENSITIVE)
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btLast:SENSITIVE)
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btgoTo:SENSITIVE)
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btSearch:SENSITIVE)
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btAdd:SENSITIVE)
            
            
            
            
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btDelete:SENSITIVE)
            
            

            /* tech14187 - 10/04/2007 - 1468007 - nÆo ‚ necess rio guardar o estado anterior destes botäes. */
/*             /*tech1139 - 05/12/2006 - FO 1387.046 */                                                         */
/*             &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/                                     */
/*                 cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btQueryJoins:SENSITIVE)   */
/*             &ENDIF                                                                                           */
/*                                                                                                              */
/*             &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN                                                */
/*                 cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btReportsJoins:SENSITIVE) */
/*             &ENDIF                                                                                           */
/*                                                                                                              */
/*             /*tech1139 - botÆo Exit */                                                                       */
/*             cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btExit:SENSITIVE)             */
/*                                                                                                              */
/*             /*tech1139 - botÆo Help */                                                                       */
/*             cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btHelp:SENSITIVE)             */
/*                                                                                                              */
/*             /*tech1139 - 05/12/2006 - FO 1387.046 */                                                         */
            .
        
        /*--- Seta vari vel cButtonsStateAux para ser passada como parƒmetro para o 
              m‚todo controlToolBar ---*/
        ASSIGN
            
            cButtonsStateAux = "":U
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            /*alterado*/
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            /*tech1139 - botÆo Exit */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                
            /*tech1139 - botÆo Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U

            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .
    END.
    
    /*--- Seta estado dos botäes/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta estado das frames ---*/
    RUN enableFields IN THIS-PROCEDURE.
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-COPY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-COPY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-COPY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     Elimina registro corrente
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnAux       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rCurrentAux      AS ROWID     NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-DELETE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-DELETE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-DELETE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Limpa temp-table RowErrors no DBO ---*/
        RUN emptyRowErrors IN h-dboes001.
    
    
    /*--- Elimina janela de mensagens de erro ---*/
    
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz somente a elimina‡Æo da instƒncia do utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i3
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz somente a elimina‡Æo da instƒncia do 
                 utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Destr¢i tela de mensagens de erros ---*/
IF VALID-HANDLE(hShowMsg) and
  hShowMsg:TYPE = "PROCEDURE":U and
  hShowMsg:FILE-NAME = "utp/ShowMessage.w":U THEN DO:
       RUN destroyInterface IN hShowMsg.
end.
/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    /*--- Exibe mensagem de confirma‡Æo de elimina‡Æo ---*/
    RUN utp/ut-msgs.p (INPUT "SHOW":U, 
                       INPUT 550, 
                       INPUT "":U).
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF RETURN-VALUE = "YES":U THEN DO:
        /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
        
            /*--- Elimina registro corrento do DBO ---*/
            RUN deleteRecord IN h-dboes001.
            
            /*--- Seta vari vel cReturnAux com o conte£do do RETURN-VALUE ---*/
            ASSIGN cReturnAux = RETURN-VALUE.
        
        
        /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
        
            /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
            RUN getRecord IN h-dboes001 (OUTPUT TABLE tt-controle-inv-esp).
        
        
        /*--- Atualize dados em tela ---*/
        RUN displayFields IN THIS-PROCEDURE.
        
        /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
        /*alterado*/
            IF cReturnAux = "NOK":U THEN DO:
                /*--- Retorna temp-table RowErrors do DBO ---*/
                RUN getRowErrors IN h-dboes001 (OUTPUT TABLE RowErrors).               
                
                /*--- Descri‡Æo dos erros:
                      3: Tabela {&TableLabel} nÆo dispon¡vel
                      6: Query est  fechada
                      8: Query est  vazia 
                      10: Query est  posicionada no £ltimo registro ---*/
                
                IF CAN-FIND(FIRST RowErrors 
                            WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                                  (RowErrors.ErrorNumber = 8)) THEN DO:
                    /*--- Seta estado dos botäes/menus ---*/
                    DO WITH FRAME fPage0:
                        ASSIGN
                            cButtonsStateAux = "":U
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            
                            
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            /*tech1139 - 05/12/2006 - FO 1387.046 */
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            /* tech14187 - 10/04/2007 - 1468007 */
                            /*tech1139 - botÆo Exit */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                            /*tech1139 - botÆo Help */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            /*tech1139 - 05/12/2006 - FO 1387.046 */
                            .
                        
                    END.
                    
                    /*--- Seta estado dos botäes/menu ---*/
                    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                    
                    /*--- Seta cursor do mouse para normal ---*/
                    SESSION:SET-WAIT-STATE("":U).
                    
                    RETURN "OK":U.
                END.
                
                IF CAN-FIND(FIRST RowErrors 
                            WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                                  (RowErrors.ErrorNumber = 10)) THEN DO:
                    /*--- Seta estado dos botäes/menus ---*/
                    DO WITH FRAME fPage0:
                        ASSIGN
                            cButtonsStateAux = "":U
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            
                            
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            /*tech1139 - 05/12/2006 - FO 1387.046 */
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            /*tech1139 - botÆo Exit */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                            /*tech1139 - botÆo Help */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            /*tech1139 - 05/12/2006 - FO 1387.046 */                
                            .
                    END.
                    
                    /*--- Seta estado dos botäes/menu ---*/
                    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                    
                    /*--- Seta cursor do mouse para normal ---*/
                    SESSION:SET-WAIT-STATE("":U).
                    
                    RETURN "OK":U.
                END.
                
                IF CAN-FIND(FIRST RowErrors 
                            WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                                  (RowErrors.ErrorNumber = 3 OR
                                   RowErrors.ErrorNumber = 6)) THEN DO:
                    /*--- Seta estado dos botäes/menus ---*/
                    DO WITH FRAME fPage0:
                        ASSIGN
                            cButtonsStateAux = "":U
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            
                            
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            
                            
                            /*tech1139 - 05/12/2006 - FO 1387.046 */
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            
                            /* tech14187 - 10/04/2007 - 1468007*/
                            /*tech1139 - botÆo Exit */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                                                      
                            /*tech1139 - botÆo Help */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            /*tech1139 - 05/12/2006 - FO 1387.046 */
                            .
                    END.
                    
                    /*--- Seta estado dos botäes/menu ---*/
                    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                    
                    /*--- Seta cursor do mouse para normal ---*/
                    SESSION:SET-WAIT-STATE("":U).
                    
                    RETURN "OK":U.
                END.
                
                /*--- Inicializa janela de mensagens de erro ---*/
                
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz somente a instƒncia do utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i1
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz somente a instƒncia do utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 1
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Inicializa tela de mensagens de erros ---*/
IF NOT VALID-HANDLE(hShowMsg) or
   hShowMsg:TYPE <> "PROCEDURE":U or
   hShowMsg:FILE-NAME <> "utp/ShowMessage.w":U THEN
        RUN utp/showmessage.w PERSISTENT SET hShowMsg.

/* _UIB-CODE-BLOCK-END */



 

                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).                

                /*--- Transfere temp-table RowErrors para a janela de mensagens de erro ---*/
                
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz a transferˆncia dos erros da temp-table RowErrors para o utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i2
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz a transferˆncia dos erros da temp-table 
                 RowErrors para o utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Transferˆncia dos erros da temp-table RowErrors para o utilit rio ---*/
IF VALID-HANDLE(hShowMsg) and
   hShowMsg:TYPE = "PROCEDURE":U and
   hShowMsg:FILE-NAME = "utp/ShowMessage.w":U THEN DO:
    
        RUN setModal IN hShowMsg (INPUT NO) NO-ERROR.
    

    RUN showMessages IN hShowMsg (INPUT TABLE RowErrors).
END.

/* _UIB-CODE-BLOCK-END */



 
                /****
                 TECH14187 - 1538059
                 Problema da ShowMessage Modal
                 ****/
                
                    WAIT-FOR CLOSE OF hShowMsg. /*Alterado por tech30713 - 07/09/2006 - Ao eliminar um regostro nÆo estava 
                                                 aparecendo a mensagem de erro. FO:1375930 */
                
                
                
                /*FO 1336930 - tech1007 - Alteracao para eliminar a janela da ut-msgs - 15/08/2006 */
                /*C¢digo destinado a tirar a ShowMessage da mem¢ria */
                
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz somente a elimina‡Æo da instƒncia do utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i3
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz somente a elimina‡Æo da instƒncia do 
                 utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Destr¢i tela de mensagens de erros ---*/
IF VALID-HANDLE(hShowMsg) and
  hShowMsg:TYPE = "PROCEDURE":U and
  hShowMsg:FILE-NAME = "utp/ShowMessage.w":U THEN DO:
       RUN destroyInterface IN hShowMsg.
end.
/* _UIB-CODE-BLOCK-END */



 
                /*FO 1336930 - tech1007 - Alteracao para eliminar a janela da ut-msgs - 15/08/2006 */

                RETURN "NOK":U.
            END.
        
        
    END.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-DELETE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-DELETE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-DELETE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE getFirst :
/*------------------------------------------------------------------------------
  Purpose:     Posiciona no primeiro registro
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnAux       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rCurrentAux      AS ROWID     NO-UNDO.
   

    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Limpa temp-table RowErrors no DBO ---*/
        RUN emptyRowErrors IN h-dboes001.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Posiciona DBO no primeiro registro ---*/
        RUN getFirst IN h-dboes001.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Seta vari vel cReturnAux com o conte£do do RETURN-VALUE ---*/
        ASSIGN cReturnAux = RETURN-VALUE.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN h-dboes001 (OUTPUT TABLE tt-controle-inv-esp).
    
    
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    /*alterado*/
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN h-dboes001 (OUTPUT TABLE RowErrors).
            
            /*--- Descri‡Æo dos erros:
                  3: Tabela {&TableLabel} nÆo dispon¡vel
                  6: Query est  fechada
                  8: Query est  vazia ---*/
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 3 OR
                               RowErrors.ErrorNumber = 6 OR
                               RowErrors.ErrorNumber = 8 )) THEN DO:
                /*--- Seta estado dos botäes/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        

                        /* tech14187 - 10/04/2007 - 1468007*/
                        /*tech1139 - botÆo Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                        /*tech1139 - botÆo Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */          
                        .

                END.
                
                /*--- Seta estado dos botäes/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
                
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.

                RETURN "NOK":U.
            END.
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            
            RETURN "NOK":U.
        END.
    
    
    /*--- Seta estado dos botäes/menus ---*/
    DO WITH FRAME fPage0:

        ASSIGN
            cButtonsStateAux = "":U
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            

            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            /*tech1139 - botÆo Exit */                                      
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            /*tech1139 - botÆo Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .
    END.
    
    /*--- Seta estado dos botäes/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
    /*--- Transfere o Focus para a Frame ---*/
    
    /*alteracao feita por anderson em 09/06/2001 para verificar
     se o botoes de navegacao estao definidos no programa*/
    
        IF btFirst:SENSITIVE = NO THEN
            RUN transfereFocoFrame IN THIS-PROCEDURE.
    
         
   RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE getLast :
/*------------------------------------------------------------------------------
  Purpose:     Posiciona no £ltimo registro
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnAux       AS CHARACTER NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Limpa temp-table RowErrors no DBO ---*/
        RUN emptyRowErrors IN h-dboes001.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Posiciona DBO no £ltimo registro ---*/
        RUN getLast IN h-dboes001.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Seta vari vel cReturnAux com o conte£do do RETURN-VALUE ---*/
        ASSIGN cReturnAux = RETURN-VALUE.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN h-dboes001 (OUTPUT TABLE tt-controle-inv-esp).
    
    
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN h-dboes001 (OUTPUT TABLE RowErrors).
            
            /*--- Descri‡Æo dos erros:
                  3: Tabela {&TableLabel} nÆo dispon¡vel
                  6: Query est  fechada
                  8: Query est  vazia ---*/
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 3 OR
                               RowErrors.ErrorNumber = 6 OR
                               RowErrors.ErrorNumber = 8 )) THEN DO:
                /*--- Seta estado dos botäes/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        /* tech14187 - 10/04/2007 - 1468007 */
                        /*tech1139 - botÆo Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - botÆo Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                END.
                
                /*--- Seta estado dos botäes/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
              
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.
              
                RETURN "NOK":U.
            END.
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            
            RETURN "NOK":U.
        END.
    
    
    /*--- Seta estado dos botäes/menus ---*/
    DO WITH FRAME fPage0:
        ASSIGN
            cButtonsStateAux = "":U
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            /*alterado*/
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            

            /*tech1139 - botÆo Exit */            
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

            /*tech1139 - botÆo Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
           /*tech1139 - 05/12/2006 - FO 1387.046 */
           .
    END.
    
    /*--- Seta estado dos botäes/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).

    /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
    /*--- Transfere o Focus para a Frame ---*/
    
    /*alteracao feita por anderson em 09/06/2001 para verificar
     se o botoes de navegacao estao definidos no programa*/
    
        IF btLast:SENSITIVE = NO THEN
            RUN transfereFocoFrame IN THIS-PROCEDURE.
    
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE getNext :
/*------------------------------------------------------------------------------
  Purpose:     Posiciona no pr¢ximo registro
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnAux       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rCurrentAux      AS ROWID     NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Limpa temp-table RowErrors no DBO ---*/
        RUN emptyRowErrors IN h-dboes001.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Posiciona DBO no pr¢ximo registro ---*/
        RUN getNext IN h-dboes001.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Seta vari vel cReturnAux com o conte£do do RETURN-VALUE ---*/
        ASSIGN cReturnAux = RETURN-VALUE.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN h-dboes001 (OUTPUT TABLE tt-controle-inv-esp).
    
    
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    /*alterado*/
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN h-dboes001 (OUTPUT TABLE RowErrors).
            
            /*--- Descri‡Æo dos erros:
                  3: Tabela {&TableLabel} nÆo dispon¡vel
                  6: Query est  fechada
                  8: Query est  vazia 
                  10: Query est  posicionada no £ltimo registro ---*/
            
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 10)) THEN DO:
                /*--- Seta estado dos botäes/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        /*tech1139 - botÆo Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                        /*tech1139 - botÆo Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                                                  
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                END.
                
                /*--- Seta estado dos botäes/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
                
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.

                RETURN "NOK":U.
            END.
            
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 3 OR
                               RowErrors.ErrorNumber = 8 OR
                               RowErrors.ErrorNumber = 6)) THEN DO:
                /*--- Seta estado dos botäes/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        /* tech14187 - 10/04/2007 - 1468007 */
                        /*tech1139 - botÆo Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                        /*tech1139 - botÆo Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                END.
                
                /*--- Seta estado dos botäes/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
                
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.

                RETURN "NOK":U.
            END.
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            
            RETURN "NOK":U.
        END.
    
    
    /*--- Seta estado dos botäes/menus ---*/
    DO WITH FRAME fPage0:
        ASSIGN
            cButtonsStateAux = "":U
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            /*alterado*/
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                 cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U 
            
            
            /*tech1139 - botÆo Exit */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U 

            /*tech1139 - botÆo Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U 

            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .
    END.
    
    /*--- Seta estado dos botäes/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).

    /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
    /*--- Transfere o Focus para a Frame ---*/
    
    /*alteracao feita por anderson em 09/06/2001 para verificar
     se o botoes de navegacao estao definidos no programa*/
    
        IF btNext:SENSITIVE = NO THEN
            RUN transfereFocoFrame IN THIS-PROCEDURE.
    
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE getPrev :
/*------------------------------------------------------------------------------
  Purpose:     Posiciona no registro anterior
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnAux       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rCurrentAux      AS ROWID     NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Limpa temp-table RowErrors no DBO ---*/
        RUN emptyRowErrors IN h-dboes001.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Posiciona DBO no registro anterior ---*/
        RUN getPrev IN h-dboes001.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Seta vari vel cReturnAux com o conte£do do RETURN-VALUE ---*/
        ASSIGN cReturnAux = RETURN-VALUE.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN h-dboes001 (OUTPUT TABLE tt-controle-inv-esp).
    
    
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    /*alterado*/
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN h-dboes001 (OUTPUT TABLE RowErrors).
            
            /*--- Descri‡Æo dos erros:
                  3: Tabela {&TableLabel} nÆo dispon¡vel
                  5: Funcao nao disponivel para banco de dados diferente de Progress
                  6: Query est  fechada
                  8: Query est  vazia 
                  9: Query est  posicionada no primeiro registro ---*/
            
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 9)) THEN DO:
                /*--- Seta estado dos botäes/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        /*tech1139 - botÆo Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - botÆo Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                END.
                
                /*--- Seta estado dos botäes/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
                
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.

                RETURN "NOK":U.
            END.
            
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 3 OR
                               RowErrors.ErrorNumber = 8 OR
                               RowErrors.ErrorNumber = 6)) THEN DO:
                /*--- Seta estado dos botäes/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        /* tech14187 - 10/04/2007 - 1468007 */
                        /*tech1139 - botÆo Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                                                   
                        /*tech1139 - botÆo Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .

                END.
                
                /*--- Seta estado dos botäes/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
                
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.

                RETURN "NOK":U.
            END.

            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.ErrorType = "INTERNAL":U AND
                              RowErrors.ErrorNumber = 5) THEN DO:
                /*--- Seta estado dos botäes/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /* Desabilitar botao de Prev em banco diferente de Progress */
                        
                            
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        /*tech1139 - botÆo Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                        /*tech1139 - botÆo Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                END.
                
                /*--- Seta estado dos botäes/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
                
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.

                RETURN "NOK":U.
            END.
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            
            RETURN "NOK":U.
        END.
    
    
    /*--- Seta estado dos botäes/menus ---*/
    DO WITH FRAME fPage0:
        ASSIGN
            cButtonsStateAux = "":U
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            /*ALTERADO*/
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            /*tech1139 - botÆo Exit */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
            /*tech1139 - botÆo Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                                      
            /*tech1139 - 05/12/2006 - FO 1387.046 */

        .
    END.
    
    /*--- Seta estado dos botäes/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
    /*--- Transfere o Focus para a Frame ---*/
    
    /*alteracao feita por anderson em 09/06/2001 para verificar
     se o botoes de navegacao estao definidos no programa*/
    
        IF btPrev:SENSITIVE = NO THEN
            RUN transfereFocoFrame IN THIS-PROCEDURE.
    

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE saveRecord :
/*------------------------------------------------------------------------------
  Purpose:     Salva modifica‡äes efetuadas
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnAux       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rCurrentAux      AS ROWID     NO-UNDO.

    /* Adicionado por Anderson(tech540) para valida‡Æo de campos date e char devido limita‡Æo banco SQL 
    em 29/01/2003*/
    
    /*fim altera‡Æo Anderson 29/01/2003*/

    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-ASSIGN":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-ASSIGN":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-ASSIGN":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Limpa temp-table RowErrors no DBO ---*/
        RUN emptyRowErrors IN h-dboes001.
    
    
    /*--- Salva os campos da temp-table {&ttTable} ---*/
    RUN saveFields IN THIS-PROCEDURE.
    IF RETURN-VALUE = "NOK":U THEN
       RETURN "NOK":U.

    
    /*--- Executar m‚todo validateRecord, caso exista ---*/
    IF THIS-PROCEDURE:GET-SIGNATURE("validateRecord":U) <> "":U THEN DO:
        RUN validateRecord IN THIS-PROCEDURE NO-ERROR.
        IF RETURN-VALUE = "NOK":U THEN
            RETURN "NOK":U.
    END.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Transfere temp-table {&ttTable} para o DBO ---*/
        RUN setRecord IN h-dboes001 (INPUT TABLE tt-controle-inv-esp).
    
    
    CASE cAction:
        /*--- Verifica se est  sendo feita uma inclusÆo/c¢pia ---*/
        WHEN "ADD":U OR WHEN "COPY":U THEN
            /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
            
                /*--- Cria um novo registro ---*/
                RUN createRecord IN h-dboes001.
            
        
        /*--- Verifica se est  sendo feita uma altera‡Æo ---*/
        WHEN "UPDATE":U THEN
            /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
            
                /*--- Altera o registro corrente do DBO ---*/
                RUN updateRecord IN h-dboes001.
            
    END CASE.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Atualiza vari vel cReturnAux com o conte£do do RETURN-VALUE ---*/
        ASSIGN cReturnAux = RETURN-VALUE.
    
    
    /*--- Verifica ocorrˆncia de erros durante a grava‡Æo ---*/
    IF cReturnAux = "NOK":U THEN DO:
        /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
        
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN h-dboes001 (OUTPUT TABLE RowErrors).
        
        
        /*--- Inicializa tela de mensagens de erros ---*/
        
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz somente a instƒncia do utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i1
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz somente a instƒncia do utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 1
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Inicializa tela de mensagens de erros ---*/
IF NOT VALID-HANDLE(hShowMsg) or
   hShowMsg:TYPE <> "PROCEDURE":U or
   hShowMsg:FILE-NAME <> "utp/ShowMessage.w":U THEN
        RUN utp/showmessage.w PERSISTENT SET hShowMsg.

/* _UIB-CODE-BLOCK-END */



 
        
        /*--- Seta cursor do mouse para normal ---*/
        SESSION:SET-WAIT-STATE("":U).
        
        /*Logica inserida por Anderson para quando houver erro 3 ou 34
        nÆo se mostrar nenhum outro erro de valida‡Æo dos boïs*/
        IF CAN-FIND(RowErrors WHERE RowErrors.ErrorNumber = 3 AND
                                     RowErrors.Errortype = "INTERNAL":U AND
                                     RowErrors.ErrorSubType = "ERROR":U) OR
            CAN-FIND(RowErrors WHERE RowErrors.ErrorNumber = 34 AND
                                     RowErrors.Errortype = "INTERNAL":U AND
                                     RowErrors.ErrorSubType = "ERROR":U) THEN DO:

            FOR EACH RowErrors WHERE RowErrors.ErrorNumber <> 3 AND 
                                     RowErrors.ErrorNumber <> 34 NO-LOCK:
                DELETE rowErrors.                                
            END.

        END.
        /*fim da altera‡Æo Anderson*/

        
        /*--- Transfere temp-table RowErrors para a tela de mensagens de erros ---*/
        
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz a transferˆncia dos erros da temp-table RowErrors para o utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i2
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz a transferˆncia dos erros da temp-table 
                 RowErrors para o utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Transferˆncia dos erros da temp-table RowErrors para o utilit rio ---*/
IF VALID-HANDLE(hShowMsg) and
   hShowMsg:TYPE = "PROCEDURE":U and
   hShowMsg:FILE-NAME = "utp/ShowMessage.w":U THEN DO:
    
        RUN setModal IN hShowMsg (INPUT NO) NO-ERROR.
    

    RUN showMessages IN hShowMsg (INPUT TABLE RowErrors).
END.

/* _UIB-CODE-BLOCK-END */



 

        /****
         TECH14187 - 1538059
         Problema da ShowMessage Modal
         ****/
        
            WAIT-FOR CLOSE OF hShowMsg. 
        

         /*Alterado por Anderson para tratar o erro de registro eliminado por outro
         usu rio*/
         IF CAN-FIND(RowErrors WHERE RowErrors.ErrorNumber = 3 AND
                                     RowErrors.Errortype = "INTERNAL":U AND
                                     RowErrors.ErrorSubType = "ERROR":U) OR
            CAN-FIND(RowErrors WHERE RowErrors.ErrorNumber = 34 AND
                                     RowErrors.Errortype = "INTERNAL":U AND
                                     RowErrors.ErrorSubType = "ERROR":U) THEN DO:
                /*Cancela a altera‡Æo do registro pois o mesmo nÆo esta disponivel*/
                RUN cancelRecord IN THIS-PROCEDURE.
                
                /*---tente reposicionar o registro pai se nao consguir reabre a query ---*/
                RUN initializeDBOs IN THIS-PROCEDURE.
                
                /*--- Posiciona no primeiro registro da query ---*/
                RUN getFirst IN THIS-PROCEDURE.
         END.
         /*Fim da altera‡Æo Anderson*/
    END.
    ELSE
        /*--- Destr¢i tela de mensagens de erros ---*/
        
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz somente a elimina‡Æo da instƒncia do utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i3
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz somente a elimina‡Æo da instƒncia do 
                 utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Destr¢i tela de mensagens de erros ---*/
IF VALID-HANDLE(hShowMsg) and
  hShowMsg:TYPE = "PROCEDURE":U and
  hShowMsg:FILE-NAME = "utp/ShowMessage.w":U THEN DO:
       RUN destroyInterface IN hShowMsg.
end.
/* _UIB-CODE-BLOCK-END */



 

    IF cReturnAux = "OK":U THEN DO:
        /*--- Retorna rowid do registro corrente do DBO ---*/
        RUN getRowid IN h-dboes001 (OUTPUT rCurrentAux).
        
        /*--- Atualiza vari vel rCurrent, campo {&ttTable}.r-Rowid e vari vel
              global {&rTable} com o valor do rowid do registro rec‚m criado ---*/
        ASSIGN rCurrent          = rCurrentAux
               tt-controle-inv-esp.r-Rowid = rCurrentAux
               gr-controle-inv-esp         = rCurrentAux.
        
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

        /*--- Executa programas de customiza‡Æo (before/after) ---*/
        
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-ASSIGN":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-ASSIGN":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-ASSIGN":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
        
        /*--- Continua em modo de inclusÆo no caso da vari vel cAction = "ADD" ---*/
        IF cAction = "ADD":U THEN DO:
            ASSIGN cAction = "":U.
            
            /*--- Seleciona a primeira p gina caso ocorra sucesso na grava‡Æo ---*/
            
            
            /*--- Verifica se est  sendo utilizada inclusÆo M£ltipla ou Simples ---*/
            IF lMultipleAdd THEN DO:
                /*--- Executa evento de inclusÆo ---*/
                
                    APPLY "CHOOSE":U TO btAdd IN FRAME fPage0.
                
                
                /*--- Seta vari vel cButtonsState com o estado atual dos botäes 
                      de navega‡Æo ---*/
                ASSIGN
                    
                    cButtonsState = "":U
                    
                    
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    
                    /* Desabilitar botao de Prev em banco diferente de Progress */
                    
                        
                            cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                        
                    
                    
                    
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    
                    
                    
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    
                    
                    
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    
                    
                    
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    
                    
                    
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    
                    
                    
                    
                    
                    
                    
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    
                    
                       /* tech14187 - 10/04/2007 - 1468007 - nÆo ‚ necess rio guardar o estado anterior destes botäes. */
/*                     /*tech14207 - 20/12/2003 - FO 1387.046*/                                */
/*                     &IF "{&ExcludeBtQueryJoins}":U = "YES":U &THEN                          */
/*                         cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U */
/*                     &ENDIF                                                                  */
/*                                                                                             */
/*                     &IF "{&ExcludeBtReportsJoins}":U = "YES":U &THEN                        */
/*                         cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U */
/*                     &ENDIF                                                                  */
/*                                                                                             */
/*                     cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U     */
/*                                                                                             */
/*                     cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U     */
/*                                                                                             */
/*                     /*tech14207 - 20/12/2003 - FO 1387.046*/                                */
                    .
            END.
            ELSE DO:
                /*--- Seta vari vel cButtonsStateAux para ser passada como parƒmetro para o 
                      m‚todo controlToolBar ---*/
                ASSIGN
                    
                    cButtonsStateAux = cButtonsState
                    
                    
                    
                    
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    
                    
                    
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    
                    
                    /*tech14207 - 20/12/2003 - FO 1387.046*/                    
                     
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    
                    
                    
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    
                    
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                    /*tech14207 - 20/12/2003 - FO 1387.046*/                    
                    .
                
                /*--- Seta estado dos botäes/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta estado das frames ---*/
                RUN disableFields IN THIS-PROCEDURE.
            END.
        END.
        ELSE DO:
            ASSIGN cAction = "":U.
            
            /*--- Seta vari vel cButtonsStateAux para ser passada como parƒmetro para o 
                  m‚todo controlToolBar ---*/
            ASSIGN
                
                cButtonsStateAux = cButtonsState
                
                
                
                
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                
                
                
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                
                
                /*tech14207 - 20/12/2003 - FO 1387.046*/
                
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
                
                
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
                
                /*tech1139 - botÆo Exit */
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    
                /*tech1139 - botÆo Help */
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
                /*tech14207 - 20/12/2003 - FO 1387.046*/
                .
            
            /*--- Seta estado dos botäes/menu ---*/
            RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
            
            /*--- Seta estado das frames ---*/
            RUN disableFields IN THIS-PROCEDURE.
        END.
        
        /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
        
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN h-dboes001 (OUTPUT TABLE RowErrors).
        

        /* Caso esteja definido o preprocessor newRecordOffQuery no DBO,
           então não mostra erros de reposicionamento */ 
        RUN NewRecordOffQuery IN h-dboes001 (OUTPUT offQuery) NO-ERROR.
        IF ERROR-STATUS:ERROR = NO AND offQuery = YES THEN DO:
            IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorNumber <> 3 AND
                                              RowErrors.ErrorNumber <> 8) and
                                              RowErrors.ErrorSubType = "ERROR":U THEN DO: 
            /*--- Inicializa tela de mensagens de erros ---*/
                
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz somente a instƒncia do utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i1
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz somente a instƒncia do utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 1
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Inicializa tela de mensagens de erros ---*/
IF NOT VALID-HANDLE(hShowMsg) or
   hShowMsg:TYPE <> "PROCEDURE":U or
   hShowMsg:FILE-NAME <> "utp/ShowMessage.w":U THEN
        RUN utp/showmessage.w PERSISTENT SET hShowMsg.

/* _UIB-CODE-BLOCK-END */



 
               
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
            
            /*--- Transfere temp-table RowErrors para a tela de mensagens de erros ---*/
                
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz a transferˆncia dos erros da temp-table RowErrors para o utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i2
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz a transferˆncia dos erros da temp-table 
                 RowErrors para o utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Transferˆncia dos erros da temp-table RowErrors para o utilit rio ---*/
IF VALID-HANDLE(hShowMsg) and
   hShowMsg:TYPE = "PROCEDURE":U and
   hShowMsg:FILE-NAME = "utp/ShowMessage.w":U THEN DO:
    
        RUN setModal IN hShowMsg (INPUT YES) NO-ERROR.
    

    RUN showMessages IN hShowMsg (INPUT TABLE RowErrors).
END.

/* _UIB-CODE-BLOCK-END */



 
            END.
        END.
        ELSE DO:
            IF CAN-FIND(FIRST RowErrors) THEN DO: 
            /*--- Inicializa tela de mensagens de erros ---*/
                
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz somente a instƒncia do utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i1
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz somente a instƒncia do utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 1
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Inicializa tela de mensagens de erros ---*/
IF NOT VALID-HANDLE(hShowMsg) or
   hShowMsg:TYPE <> "PROCEDURE":U or
   hShowMsg:FILE-NAME <> "utp/ShowMessage.w":U THEN
        RUN utp/showmessage.w PERSISTENT SET hShowMsg.

/* _UIB-CODE-BLOCK-END */



 
               
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
            
            /*--- Transfere temp-table RowErrors para a tela de mensagens de erros ---*/
                
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz a transferˆncia dos erros da temp-table RowErrors para o utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i2
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz a transferˆncia dos erros da temp-table 
                 RowErrors para o utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Transferˆncia dos erros da temp-table RowErrors para o utilit rio ---*/
IF VALID-HANDLE(hShowMsg) and
   hShowMsg:TYPE = "PROCEDURE":U and
   hShowMsg:FILE-NAME = "utp/ShowMessage.w":U THEN DO:
    
        RUN setModal IN hShowMsg (INPUT YES) NO-ERROR.
    

    RUN showMessages IN hShowMsg (INPUT TABLE RowErrors).
END.

/* _UIB-CODE-BLOCK-END */



 
            END.
        END.
    END.
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
    /*--- Transfere o Focus para a Frame ---*/
    /*alteracao feita por anderson em 09/06/2001 para verificar
     se o botoes de navegacao estao definidos no programa*/
    
        IF btSave:SENSITIVE = NO THEN
            RUN transfereFocoFrame IN THIS-PROCEDURE.
    

    IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U 
                OR RowErrors.ErrorSubType = "":U) THEN
        RETURN "NOK":U.
    ELSE
        RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE showQueryJoins :
/*------------------------------------------------------------------------------
  Purpose:     Executa janela de Consultas Relacionadas
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
/* alterado por Valdir (tech264) novo m‚todo de teste do valid-handle */    
    /*--- Inicializa janela de Consultas Relacionadas ---*/
    IF NOT VALID-HANDLE(hQueryJoins) OR
       hQueryJoins:TYPE <> "PROCEDURE":U OR
       hQueryJoins:FILE-NAME <> "utp/ut-cons.w" THEN
        RUN utp/ut-cons.w PERSISTENT SET hQueryJoins (INPUT c-programa-mg97).
    
    IF VALID-HANDLE(hQueryJoins) AND
       hQueryJoins:TYPE = "PROCEDURE":U AND
       hQUeryJoins:FILE-NAME = "utp/ut-cons.w" THEN
        RUN dispatch IN hQueryJoins (INPUT "INITIALIZE":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE showReportsJoins :
/*------------------------------------------------------------------------------
  Purpose:     Executa janela de Relat¢rios Relacionados
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
/* alterado por Valdir (tech264) novo m‚todo de teste do valid-handle */    
    /*--- Inicializa janela de Relat¢rios Relacionados ---*/
    IF NOT VALID-HANDLE(hReportsJoins) OR
       hReportsJoins:TYPE <> "PROCEDURE":U OR
       hReportsJoins:FILE-NAME <> "utp/ut-relat.w":U THEN
        RUN utp/ut-relat.w PERSISTENT SET hReportsJoins (INPUT c-programa-mg97).
    
    IF VALID-HANDLE(hReportsJoins) AND
       hReportsJoins:TYPE = "PROCEDURE":U AND
       hReportsJoins:FILE-NAME = "utp/ut-relat.w":U THEN
        RUN dispatch IN hReportsJoins (INPUT "INITIALIZE":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE transfereFocoFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    APPLY "entry":U TO FRAME fPage0.
    
    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE undoRecord :
/*------------------------------------------------------------------------------
  Purpose:     Desfaz altera‡äes feitas para o registro corrente
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-UNDO":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-UNDO":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-UNDO":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Limpa temp-table {&ttTable} ---*/
    FOR EACH tt-controle-inv-esp:
        DELETE tt-controle-inv-esp.
    END.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Atualiza temp-table {&ttTable} com base no registro corrento do DBO ---*/
        
        /*--- Limpa temp-table RowErrors do DBO ---*/
        RUN emptyRowErrors IN h-dboes001.
        
        /*Correcao do comportamento do evento do botao undorecord no thim maintenence 
          para que o mesmo, na inclusao limpe os campos 
          e na altercao ou copia traga os valores do registro corrente do BO */
        /* Alteracao feita por Anderson (tech485) em 19/04/2001
           Alterado por Paulo H. Lazzarotti em 08/05/2002 */
        IF cAction = "COPY":U OR cAction = "UPDATE":U THEN
            /*--- Posiciona DBO no registro corrente ---*/
            RUN bringCurrent IN h-dboes001.

        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN h-dboes001 (OUTPUT TABLE tt-controle-inv-esp).
    
    
        
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-UNDO":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-UNDO":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-UNDO":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     Inclui novo registro
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    /*****************
      In¡cio modifica‡äes referentes a FO 1189107, sobre a cria‡Æo de eventos de EPC para interface de 
      ThinTemplates para o evento de UpdateRecord, similar aos que j  existem para AddRecord. 
      Respons vel: tech14187 - Data: 09/08/2005 - Task: 2526
      OS: 6722 - Ativid: 139698 - Tarefa: 167095
     *****************/

    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.    

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-UPDATE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-UPDATE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-UPDATE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*****************
      Fim das modifica‡äes referentes a FO 1189107, sobre a cria‡Æo de eventos de EPC para interface de 
      ThinTemplates para o evento de UpdateRecord, similar aos que j  existem para AddRecord. 
      Respons vel: tech14187 - Data: 09/08/2005 - Task: 2526
      OS: 6722 - Ativid: 139698 - Tarefa: 167095
     *****************/


    /*--- Seta vari vel cAction ---*/
    ASSIGN cAction = "UPDATE":U.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
    
    DO WITH FRAME fPage0:
        /*--- Seta vari vel cButtonsState com o estado atual dos botäes 
              de navega‡Æo ---*/
        ASSIGN
            
            cButtonsState = "":U
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btFirst:SENSITIVE)
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btPrev:SENSITIVE)
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btNext:SENSITIVE)
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btLast:SENSITIVE)
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btgoTo:SENSITIVE)
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btSearch:SENSITIVE)
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btAdd:SENSITIVE)
            
            
            
            
            
            
            
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btDelete:SENSITIVE)
            
            
            .
        
        /*--- Seta vari vel cButtonsStateAux para ser passada como parƒmetro para o 
              m‚todo controlToolBar ---*/
        ASSIGN
            
            cButtonsStateAux = "":U
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            /*alterado*/
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            
            /*tech1139 - botÆo Exit */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U            

            /*tech1139 - botÆo Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .
    END.
    
    /*--- Seta estado dos botäes/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta estado das frames ---*/
    RUN enableFields IN THIS-PROCEDURE.
    
    /*****************
      In¡cio modifica‡äes referentes a FO 1189107, sobre a cria‡Æo de eventos de EPC para interface de 
      ThinTemplates para o evento de UpdateRecord, similar aos que j  existem para AddRecord. 
      Respons vel: tech14187 - Data: 09/08/2005 - Task: 2526
      OS: 6722 - Ativid: 139698 - Tarefa: 167095
     *****************/

    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.    

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-UPDATE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-UPDATE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-UPDATE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*****************
      Fim das modifica‡äes referentes a FO 1189107, sobre a cria‡Æo de eventos de EPC para interface de 
      ThinTemplates para o evento de UpdateRecord, similar aos que j  existem para AddRecord. 
      Respons vel: tech14187 - Data: 09/08/2005 - Task: 2526
      OS: 6722 - Ativid: 139698 - Tarefa: 167095
     *****************/

    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




 
/*** Alterado por Farley - em 23/07/2003 ***/
 

/* _UIB-CODE-BLOCK-END */



 





/* ***************************  Main Block  *************************** */
    
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    run pi_aplica_facelift in this-procedure.
    

/*******
    Altera‡Æo 07/12/2005 - tech14187 - Altera‡Æo para evitar estouro de segmento
******/
    RUN piTraduz IN THIS-PROCEDURE.


/*Alterado por Valdir (tech264) para fazer tratamento da trigger de help*/
ON HELP OF wMaintenance DO:
    apply "choose":U to btHelp in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */




PROCEDURE applyReturn :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de RETURN
  Parameters:  
  Notes:       Torna-se necess rio o comando CASE pois a trigger de RETURN,
               aparentemente, executa autom ticamente a op‡Æo NO-APPLY
------------------------------------------------------------------------------*/
    CASE SELF:TYPE:
        /*--- Evento RETURN padrÆo para widgets do tipo editor ---*/
        WHEN "EDITOR":U THEN
            SELF:INSERT-STRING(CHR(10)).
        
        /*--- Evento RETURN padrÆo para widgets do tipo button ---*/
        WHEN "BUTTON":U THEN
            APPLY "CHOOSE":U TO SELF.
        
        /*--- Evento RETURN padrÆo para outros widgets ---*/
        OTHERWISE
            
                APPLY "CHOOSE":U TO btSave IN FRAME fPage0.
            
    END CASE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE changePage :
/*------------------------------------------------------------------------------
  Purpose:     M‚todo executado pelo programa de Folder, quando h  troca de 
               p gina
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforechangePage":U) <> "":U THEN DO:
    
        RUN BeforechangePage IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.

    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.    

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AfterchangePage":U) <> "":U THEN DO:
    
        RUN AfterchangePage IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE controlToolBar :
/*------------------------------------------------------------------------------
  Purpose:     Controla estado dos botäes do retƒngulo rtToolBar
  Parameters:  recebe estado dos botäes
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pButtonsState AS CHARACTER NO-UNDO.
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforecontrolToolBar":U) <> "":U THEN DO:
    
        RUN BeforecontrolToolBar IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-CONTROL-TOOL-BAR":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-CONTROL-TOOL-BAR":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-CONTROL-TOOL-BAR":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 

    DO WITH FRAME fPage0:

        ASSIGN
            
            
                
                                    
                
                btFirst:SENSITIVE                          = ENTRY(1, pButtonsState) = "YES":U
                MENU-ITEM miFirst:SENSITIVE IN MENU mbMain = ENTRY(1, pButtonsState) = "YES":U
            
            
            
                
                                    
                
                btPrev:SENSITIVE                          = ENTRY(1 + 1, pButtonsState) = "YES":U
                MENU-ITEM miPrev:SENSITIVE IN MENU mbMain = ENTRY(1 + 1, pButtonsState) = "YES":U
            
            
            
                
                                    
                
                btNext:SENSITIVE                          = ENTRY(1 + 1 + 1, pButtonsState) = "YES":U
                MENU-ITEM miNext:SENSITIVE IN MENU mbMain = ENTRY(1 + 1 + 1, pButtonsState) = "YES":U
            
            
            
                
                                    
                
                btLast:SENSITIVE                          = ENTRY(1 + 1 + 1 + 1, pButtonsState) = "YES":U
                MENU-ITEM miLast:SENSITIVE IN MENU mbMain = ENTRY(1 + 1 + 1 + 1, pButtonsState) = "YES":U
            
            
            
                
                                    
                
                btGoTo:SENSITIVE                          = ENTRY(1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
                MENU-ITEM miGoTo:SENSITIVE IN MENU mbMain = ENTRY(1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
            
            
            
                
                                    
                
                btSearch:SENSITIVE                          = ENTRY(1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
                MENU-ITEM miSearch:SENSITIVE IN MENU mbMain = ENTRY(1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
            
            
            
                
                                    
                
                btAdd:SENSITIVE                          = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
                MENU-ITEM miAdd:SENSITIVE IN MENU mbMain = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
            
            
            
            
            
            
            
                
                                    
                
                btDelete:SENSITIVE                          = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
                MENU-ITEM miDelete:SENSITIVE IN MENU mbMain = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
            
            
            
            
            
                
                                    
                
                btCancel:SENSITIVE                          = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
                MENU-ITEM miCancel:SENSITIVE IN MENU mbMain = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
            
            
            
                
                                    
                
                btSave:SENSITIVE                          = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
                MENU-ITEM miSave:SENSITIVE IN MENU mbMain = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
            

            /*tech1139 - 05/12/2006 - FO 1387.046 */
            
                
                                    
                
                btQueryJoins:SENSITIVE                          = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
                MENU-ITEM miQueryJoins:SENSITIVE IN MENU mbMain = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U            
            
            
            
                
                                    
                
                btReportsJoins:SENSITIVE                          = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
                MENU-ITEM miReportsJoins:SENSITIVE IN MENU mbMain = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U                        
            

            
                            
                
            /*Erro de entry*/
            btExit:SENSITIVE                          = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
            MENU-ITEM miExit:SENSITIVE IN MENU mbMain = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U      
            
                    
            
                            
                
            btHelp:SENSITIVE                              = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U
            MENU-ITEM miContents:SENSITIVE IN MENU mbMain = ENTRY(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1, pButtonsState) = "YES":U                        
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .
    END.
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-CONTROL-TOOL-BAR":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-CONTROL-TOOL-BAR":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-CONTROL-TOOL-BAR":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AftercontrolToolBar":U) <> "":U THEN DO:
    
        RUN AftercontrolToolBar IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE destroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     Destr¢i programa
  Parameters:  
  Notes:       Destr¢i programas de: Folder, Consultas e Relat¢rios Relacionados
------------------------------------------------------------------------------*/
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforedestroyInterface":U) <> "":U THEN DO:
    
        RUN BeforedestroyInterface IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*Inserida a chamada da include i-logfin1.i
    devido aos thintemplates nÆo gerarem log de 
    execucao de programas*/
    /*************************************************************************
**
** I-LOGFIN1.I - Encerra o Log de Execucao
**
**************************************************************************/

run btb/btb918zb.p (input c-programa-mg97,
                    input-output rw-log-exec,
                    input no).
 

    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Destr¢i DBO ---*/
        
            IF VALID-HANDLE(h-dboes001) THEN
                RUN destroy IN h-dboes001.
        
    
    
    /*--- Destr¢i programa de folder ---*/
    
    
    /*--- Destr¢i programa de Consultas Relacionadas ---*/
    IF VALID-HANDLE(hQueryJoins) THEN
        DELETE PROCEDURE hQueryJoins.
    
    /*--- Destr¢i programa de Relat¢rios Relacionados ---*/
    IF VALID-HANDLE(hReportsJoins) THEN
        DELETE PROCEDURE hReportsJoins.
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AfterdestroyInterface":U) <> "":U THEN DO:
    
        RUN AfterdestroyInterface IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    /*--- Destr¢i os Servidores RPC inicializados pelos DBOs ---*/
    /***************************************************************************
**
**   btb008za.i3  -  Include para finaliza‡Æo RPC.
**
***************************************************************************/


        assign l-inf-prg = no.    



    



if l-inf-prg = yes then do:
    run pi_destroy_rpc in h-servid-rpc ("").
end.
else do i-prg-rpc = 1 to num-entries(c-lst-prg-rpc):
    run pi_destroy_rpc in h-servid-rpc (entry(i-prg-rpc,c-lst-prg-rpc)).
end.
 
        
    /*Alteracao para deletar da mem¢ria o WindowStyles e o btb008za.p*/
    IF VALID-HANDLE(h-servid-rpc) THEN
    DO:
       DELETE PROCEDURE h-servid-rpc.
       ASSIGN h-servid-rpc = ?. /*Garantir que a vari vel nÆo vai mais apontar para nenhum handle de outro objeto - este problema apareceu na v9.1B com Windows2000*/
    END.

    /*--- Destr¢i janela associada ao programa ---*/
    IF VALID-HANDLE(wMaintenance) THEN
        DELETE WIDGET wMaintenance.
    
    /*--- Destr¢i programa ---*/
    IF THIS-PROCEDURE:PERSISTENT THEN
        DELETE PROCEDURE THIS-PROCEDURE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE disableFields :
/*------------------------------------------------------------------------------
  Purpose:     Desabilita os campos da temp-table {&tt-table} 
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforedisableFields":U) <> "":U THEN DO:
    
        RUN BeforedisableFields IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-DISABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-DISABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-DISABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Desabilita campos contidos no {&page0KeyFields} ---*/
    
        DO WITH FRAME fPage0:
            DISABLE tt-controle-inv-esp.sequencia                                  tt-controle-inv-esp.num-ord-inv                                tt-controle-inv-esp.cod-est-exec                               tt-controle-inv-esp.num-projeto                                tt-controle-inv-esp.num-ordem.
        END.
    
    
    /*--- Desabilita campos contidos no {&page0Fields} ---*/
    
        DO WITH FRAME fPage0:
            DISABLE tt-controle-inv-esp.dt-trans                                   tt-controle-inv-esp.valor-origem                               tt-controle-inv-esp.narrativa                               fi-estab fi-proj fi-ordem.
        END.
    
    
    /*--- Desabilita campos contidos no {&page1Fields} ---*/
    
    
    /*--- Desabilita campos contidos no {&page2Fields} ---*/
    
    
    /*--- Desabilita campos contidos no {&page3Fields} ---*/
    
    
    /*--- Desabilita campos contidos no {&page4Fields} ---*/
    
    
    /*--- Desabilita campos contidos no {&page5Fields} ---*/
    
    
    /*--- Desabilita campos contidos no {&page6Fields} ---*/
    
    
    /*--- Desabilita campos contidos no {&page7Fields} ---*/
    
    
    /*--- Desabilita campos contidos no {&page8Fields} ---*/
    
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-DISABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-DISABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-DISABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AfterdisableFields":U) <> "":U THEN DO:
    
        RUN AfterdisableFields IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    IF VALID-HANDLE(h-faceLift) THEN DO:
        
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage0:HANDLE, NO).
        
        /*--- Desabilita campos contidos no {&page1Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page2Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page3Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page4Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page5Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page6Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page7Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page8Fields} ---*/
        
    END.
    
    /*********fim altera‡Æo*************/

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE dispatch :
/*------------------------------------------------------------------------------
  Purpose:     Manter compatibilidade com SmartObjects
  Parameters:  recebe m‚todo a ser executado
  Notes:       Somente haver  tratamento para o m‚todo initialize, quando a 
               execu‡Æo deste m‚todo for solicitada ser  executado o m‚todo
               initializeInterface
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pMethod AS CHARACTER NO-UNDO.
    
    IF pMethod = "INITIALIZE":U THEN
        RUN initializeInterface IN THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     Exibe os campos da temp-table {&tt-table} em tela
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Posiciona temp-table {&ttTable} no primeiro registro ---*/
    FIND FIRST tt-controle-inv-esp NO-ERROR.
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforedisplayFields":U) <> "":U THEN DO:
    
        RUN BeforedisplayFields IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-DISPLAY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-DISPLAY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-DISPLAY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    IF AVAILABLE tt-controle-inv-esp THEN DO:
        /*--- Atualiza vari veis rCurrent e {&rTable} com o valor do 
              rowid corrente ---*/
        ASSIGN rCurrent  = tt-controle-inv-esp.r-Rowid
               gr-controle-inv-esp = rCurrent.
        
        /*--- Exibe campos contidos no {&page0KeyFields} em tela ---*/
        
            DO WITH FRAME fPage0:
                DISPLAY tt-controle-inv-esp.sequencia                                  tt-controle-inv-esp.num-ord-inv                                tt-controle-inv-esp.cod-est-exec                               tt-controle-inv-esp.num-projeto                                tt-controle-inv-esp.num-ordem.
            END.
        
        
        /*--- Exibe campos contidos no {&page0Fields} em tela ---*/
        
            DO WITH FRAME fPage0:
                DISPLAY tt-controle-inv-esp.dt-trans                                   tt-controle-inv-esp.valor-origem                               tt-controle-inv-esp.narrativa                               fi-estab fi-proj fi-ordem.
            END.
        
        
        /*--- Exibe campos contidos no {&page1Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page2Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page3Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page4Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page5Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page6Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page7Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page8Fields} em tela ---*/
        
    END.
    ELSE DO:
        /*--- Atualiza vari veis rCurrent e {&rTable} com o valor do 
              rowid corrente ---*/
        ASSIGN rCurrent    = ?
               gr-controle-inv-esp = rCurrent.
        
        CREATE tt-controle-inv-esp.
        
        /*--- Exibe campos contidos no {&page0KeyFields} em tela ---*/
        
            DO WITH FRAME fPage0:
                DISPLAY tt-controle-inv-esp.sequencia                                  tt-controle-inv-esp.num-ord-inv                                tt-controle-inv-esp.cod-est-exec                               tt-controle-inv-esp.num-projeto                                tt-controle-inv-esp.num-ordem.
            END.
        
        
        /*--- Exibe campos contidos no {&page0Fields} em tela ---*/
        
            DO WITH FRAME fPage0:
                DISPLAY tt-controle-inv-esp.dt-trans                                   tt-controle-inv-esp.valor-origem                               tt-controle-inv-esp.narrativa                               fi-estab fi-proj fi-ordem.
            END.
        
        
        /*--- Exibe campos contidos no {&page1Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page2Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page3Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page4Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page5Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page6Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page7Fields} em tela ---*/
        
        
        /*--- Exibe campos contidos no {&page8Fields} em tela ---*/
        
        
        DELETE tt-controle-inv-esp.
    END.
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-DISPLAY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-DISPLAY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-DISPLAY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AfterdisplayFields":U) <> "":U THEN DO:
    
        RUN AfterdisplayFields IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     Habilita os campos da temp-table {&tt-table} 
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE hFieldEntryAux    AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lExecutedEntryAux AS LOGICAL NO-UNDO.
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforeenableFields":U) <> "":U THEN DO:
    
        RUN BeforeenableFields IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Habilita campos contidos no {&page0KeyFields} ---*/
    
        IF cAction = "ADD":U OR cAction = "COPY":U THEN DO WITH FRAME fPage0:
            ENABLE tt-controle-inv-esp.sequencia                                  tt-controle-inv-esp.num-ord-inv                                tt-controle-inv-esp.cod-est-exec                               tt-controle-inv-esp.num-projeto                                tt-controle-inv-esp.num-ordem.
        END.
    
    
    /*--- Habilita campos contidos no {&page0Fields} ---*/
    
        DO WITH FRAME fPage0:
            ENABLE tt-controle-inv-esp.dt-trans                                   tt-controle-inv-esp.valor-origem                               tt-controle-inv-esp.narrativa                               fi-estab fi-proj fi-ordem.
        END.
    
    
    /*--- Habilita campos contidos no {&page1Fields} ---*/
    
    
    /*--- Habilita campos contidos no {&page2Fields} ---*/
    
    
    /*--- Habilita campos contidos no {&page3Fields} ---*/
    
    
    /*--- Habilita campos contidos no {&page4Fields} ---*/
    
    
    /*--- Habilita campos contidos no {&page5Fields} ---*/
    
    
    /*--- Habilita campos contidos no {&page6Fields} ---*/
    
    
    /*--- Habilita campos contidos no {&page7Fields} ---*/
    
    
    /*--- Habilita campos contidos no {&page8Fields} ---*/
    
    
    
        /*--- Executar o ENTRY para o primeiro campo ---*/
        ASSIGN hFieldEntryAux = FRAME fPage0:FIRST-CHILD.
        DO WHILE VALID-HANDLE(hFieldEntryAux):
            IF (hFieldEntryAux:TYPE = "FILL-IN":U OR
                hFieldEntryAux:TYPE = "COMBO-BOX":U OR
                hFieldEntryAux:TYPE = "TOGGLE-BOX":U OR
                hFieldEntryAux:TYPE = "RADIO-SET":U) AND
               (hFieldEntryAux:FRAME = FRAME fPage0:HANDLE) THEN
                IF hFieldEntryAux:SENSITIVE = YES THEN DO:
                    /*--- Corrigir BUG PROGRESS, pois o ENTRY nÆo funciona enquanto 
                          o SET-WAIT-STATE est  em espera ---*/
                    SESSION:SET-WAIT-STATE("":U).
                    
                    APPLY "ENTRY":U TO hFieldEntryAux.
                    
                    ASSIGN lExecutedEntryAux = YES.
                    
                    /*--- Corrigir BUG PROGRESS, pois o ENTRY nÆo funciona enquanto 
                          o SET-WAIT-STATE est  em espera ---*/
                    SESSION:SET-WAIT-STATE("GENERAL":U).
                    
                    LEAVE.
                END.
            
            IF hFieldEntryAux:TYPE = "FIELD-GROUP":U THEN
                ASSIGN hFieldEntryAux = hFieldEntryAux:FIRST-CHILD.
            ELSE
                ASSIGN hFieldEntryAux = hFieldEntryAux:NEXT-SIBLING.
        END.
    
    
    
        /*--- Executar o ENTRY para o primeiro campo ---*/
        IF NOT lExecutedEntryAux THEN DO:
            ASSIGN hFieldEntryAux = FRAME fPage0:FIRST-CHILD.
            
            DO WHILE VALID-HANDLE(hFieldEntryAux):
                IF (hFieldEntryAux:TYPE = "FILL-IN":U OR
                    hFieldEntryAux:TYPE = "COMBO-BOX":U OR
                    hFieldEntryAux:TYPE = "TOGGLE-BOX":U OR
                    hFieldEntryAux:TYPE = "RADIO-SET":U) AND
                   (hFieldEntryAux:FRAME = FRAME fPage0:HANDLE) THEN
                    IF hFieldEntryAux:SENSITIVE = YES THEN DO:
                        /*--- Corrigir BUG PROGRESS, pois o ENTRY nÆo funciona 
                              enquanto o SET-WAIT-STATE est  em espera ---*/
                        SESSION:SET-WAIT-STATE("":U).
                        APPLY "ENTRY":U TO hFieldEntryAux.
                        
                        ASSIGN lExecutedEntryAux = YES.
                        
                        /*--- Corrigir BUG PROGRESS, pois o ENTRY nÆo funciona enquanto 
                              o SET-WAIT-STATE est  em espera ---*/
                        SESSION:SET-WAIT-STATE("GENERAL":U).
                        
                        LEAVE.
                    END.
                
                IF hFieldEntryAux:TYPE = "FIELD-GROUP":U THEN
                    ASSIGN hFieldEntryAux = hFieldEntryAux:FIRST-CHILD.
                ELSE
                    ASSIGN hFieldEntryAux = hFieldEntryAux:NEXT-SIBLING.
            END.
        END.
    
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AfterenableFields":U) <> "":U THEN DO:
    
        RUN AfterenableFields IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    IF VALID-HANDLE(h-faceLift) THEN DO:
        
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage0:HANDLE, YES).
        
        /*--- Desabilita campos contidos no {&page1Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page2Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page3Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page4Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page5Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page6Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page7Fields} ---*/
        
        
        /*--- Desabilita campos contidos no {&page8Fields} ---*/
        
    END.
    
    /*********fim altera‡Æo*************/

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE initializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     Inicialize programa
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    /*Alterado por Anderson (tech485) para o template mostrar o nome da empresa*/

    def var c_cod_empres_usuar as char no-undo.
    def var c_nom_razao_social as char no-undo.

    /*fim alteracao Anderson*/

    /*--- Inicializa‡Æo de OCXs ---*/
    IF THIS-PROCEDURE:GET-SIGNATURE("control_load":U) <> "":U THEN DO:
        RUN control_load IN THIS-PROCEDURE NO-ERROR.
        VIEW FRAME fPage0 IN WINDOW wMaintenance.
    END.
    
    /*--- Executa valida‡äes de inicializa‡Æo ---*/
    ASSIGN c-programa-mg97 = CAPS("ESIN001":U)
           c-versao-mg97   = "2.00.00.000":U.

    /*--------------------------------------------------------------------------
    File        : UTP/UT-VFSEC.I
    Purpose     : Verifica‡Æo da Seguran‡a

    Syntax      :

    Description : Verificar a seguran‡a

    Author(s)   : Fabiano
    Created     : 31/12/1997
    Notes       :
------------------------------------------------------------------------*/
/* NÆo faz a valida‡Æo para programas do tipo V  Para */
if index(replace(program-name(1),"~\","~/"),"go/g") = 0 then do:
  run men/men901za.p (input c-programa-mg97).
  if  return-value = "2012" then do:
      run utp/ut-msgs.p (input "show",
                         input 2858,
                         input c-programa-mg97).
      if "Maintenance" = "SmartDialog" or this-procedure:persistent = no then
        return "adm-error".
      else do:     
        delete procedure this-procedure.
        return.
      end.  
  end.                       

  if  return-value = "2014" then do:
      run utp/ut-msgs.p (input "show",
                         input 3045,
                         input c-programa-mg97).
      if "Maintenance" = "SmartDialog" then
        return "adm-error".
      else do:
        delete procedure this-procedure.
        return.
      end.  
  end.
end.  

/* ut-vfsec.i */
 

    /*Inserida a chamada da include i-logini.i
    devido aos thintemplates nÆo gerarem log de 
    execucao de programas*/
    /*************************************************************************
**                                                                        
**  I-LOGINI.I - Inicia Log de Execu‡Æo
**
*************************************************************************/

run btb/btb918zb.p (input c-programa-mg97,
                    input-output rw-log-exec,
                    input yes).

/* i-logini */

 
    
    
    /*Alterado por Anderson (tech485) para o template mostrar o nome da empresa*/
    /*{utp/ut9000.i "{&Program}" "{&Version}"} *//*esta foi comentada por estar causando erro nos thintemplates*/
    /*rodar pi-rsocial persistent para verifica»’o empresa usuario*/
    /* alterado por Valdir (tech264) novo m‚todo de teste do valid-handle */
   
    if not valid-handle(h-rsocial) or
       h-rsocial:TYPE <> "PROCEDURE":U or
       h-rsocial:FILE-NAME <> "utp/ut-rsocial.p":U then do:
        if l-achou-prog then
            run utp/ut-rsocial.p persistent set h-rsocial.
    end.
    if l-achou-prog then
        run pi-rsocial in h-rsocial (output c_cod_empres_usuar, output c_nom_razao_social).
    
    find prog_dtsul no-lock
        where prog_dtsul.cod_prog_dtsul = c-programa-mg97 no-error.
    if  avail prog_dtsul then do:
        assign c-titulo-prog-mg97    = prog_dtsul.des_prog_dtsul
               c-nom-prog-upc-mg97   = prog_dtsul.nom_prog_upc
               c-nom-prog-appc-mg97  = prog_dtsul.nom_prog_appc
               c-nom-prog-dpc-mg97   = prog_dtsul.nom_prog_dpc
               i-num-topico-hlp-mg97 = prog_dtsul.num_topico.
       
          if session:window-system <> "TTY":U then 
          
             assign i-template          = prog_dtsul.idi_template.
          
       

        
        find procedimento no-lock
            where procedimento.cod_proced = prog_dtsul.cod_proced no-error.
        if  avail procedimento then do:
            find modul_dtsul no-lock
                where modul_dtsul.cod_modul_dtsul = procedimento.cod_modul_dtsul no-error. 
            if  avail modul_dtsul then do:
                assign c-modulo-mg97         = caps(modul_dtsul.nom_modul_dtsul_menu)
                       c-cod-mod-mg97        = caps(modul_dtsul.cod_modul_dtsul)
                       c-nom-manual-hlp-mg97 = "dochlp~/":U + string(modul_dtsul.num_manual_documen, "999999":U) + ".hlp":U.
            end.
        end.
    end.                                                      
    else do:
        assign c-titulo-prog-mg97    = caps(c-programa-mg97)
               c-nom-prog-upc-mg97   = ""
               c-nom-prog-appc-mg97  = ""
               i-num-topico-hlp-mg97 = 0
               c-nom-manual-hlp-mg97 = "dochlp~/000000.hlp":U.
    end.                 
    /* Tradu‡Æo T¡tulo dos Programas */

    /* FO 1354855  - 11/08/2006 - tech1139 */
    run utp/ut-liter.p (input replace(c-titulo-prog-mg97, " ", "_") ,
                        input "*":U,
                        input "":U). 
    /* FO 1354855  - 11/08/2006 - tech1139 */

    Assign c-titulo-prog-mg97 = Return-value.

    
        
             assign wMaintenance:title = if l-achou-prog then
                                           c-titulo-prog-mg97
                                         + " - ":U 
                                         + c-programa-mg97 
                                         + " - ":U 
                                         + c-versao-mg97  
                                         + " - ":U 
                                         + c_cod_empres_usuar
                                         + " - ":U 
                                         + c_nom_razao_social
                                         else 
                                           c-titulo-prog-mg97
                                         + " - ":U 
                                         + c-programa-mg97 
                                         + " - ":U 
                                         + c-versao-mg97.
        
     
    

    /*fim alteracao Anderson(tech485)*/
    
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforeinitializeInterface":U) <> "":U THEN DO:
    
        RUN BeforeinitializeInterface IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN DO:
        /*--- Destr¢i programa ---*/
        RUN destroyInterface IN THIS-PROCEDURE.
        
        RETURN "NOK":U.
    END.
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    DO WITH FRAME fPage0:
        
        /*--- Habilitar/Desabilitar botäes/menus ---*/ 
        ASSIGN
            
                btFirst:SENSITIVE                          = NO
                MENU-ITEM miFirst:SENSITIVE IN MENU mbMain = NO
            
            
            
                btPrev:SENSITIVE                          = NO
                MENU-ITEM miPrev:SENSITIVE IN MENU mbMain = NO
            
            
            
                btNext:SENSITIVE                          = NO
                MENU-ITEM miNext:SENSITIVE IN MENU mbMain = NO
            
            
            
                btLast:SENSITIVE                          = NO
                MENU-ITEM miLast:SENSITIVE IN MENU mbMain = NO
            
            
            
                btGoTo:SENSITIVE                          = NO
                MENU-ITEM miGoTo:SENSITIVE IN MENU mbMain = NO
            
            
            
                btSearch:SENSITIVE                          = NO
                MENU-ITEM miSearch:SENSITIVE IN MENU mbMain = NO
            
            
            
                btAdd:SENSITIVE                          = NO
                MENU-ITEM miAdd:SENSITIVE IN MENU mbMain = NO
            
            
            
            
            
            
            
                btDelete:SENSITIVE                          = NO
                MENU-ITEM miDelete:SENSITIVE IN MENU mbMain = NO
            
            
            
            
            
                btCancel:SENSITIVE                          = NO
                MENU-ITEM miCancel:SENSITIVE IN MENU mbMain = NO
            
            
            
                btSave:SENSITIVE                          = NO
                MENU-ITEM miSave:SENSITIVE IN MENU mbMain = NO
            
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            /*tech1139 - 09/02/2007 - FO 1453.143 
            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN
                btQueryJoins:SENSITIVE                            = NO
                MENU-ITEM miQueryJoins:SENSITIVE IN MENU mbMain   = NO
            &ENDIF
            
            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                btReportsJoins:SENSITIVE                          = NO
                MENU-ITEM miReportsJoins:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            FIM - tech1139 - 09/02/2007 - FO 1453.143 */
            
            btExit:SENSITIVE                                  = YES
            MENU-ITEM miExit:SENSITIVE IN MENU mbMain         = YES
            
            
            btHelp:SENSITIVE                                  = YES
            MENU-ITEM miContents:SENSITIVE IN MENU mbMain     = YES
            /*tech1139 - 05/12/2006 - FO 1387.046 */

            .
        /*--- Posicionar widgets ---*/
        ASSIGN 
            rtToolBar:WIDTH  = FRAME fPage0:WIDTH
            rtToolBar:HEIGHT = 1.46
            rtToolBar:COL    = 1
            rtToolBar:ROW    = 1
            
            
                btFirst:ROW    = 1.12
                btFirst:COL    = deColPanelNav
                
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btFirst:HEIGHT = 1.13
                
                btFirst:WIDTH  = 4
                deColPanelNav  = deColPanelNav + 4
            
            
            
                btPrev:ROW    = 1.12
                btPrev:COL    = deColPanelNav
                
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btPrev:HEIGHT = 1.13
                
                btPrev:WIDTH  = 4
                deColPanelNav = deColPanelNav + 4
            
            
            
                btNext:ROW    = 1.12
                btNext:COL    = deColPanelNav
                
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btNext:HEIGHT = 1.13
                
                btNext:WIDTH  = 4
                deColPanelNav = deColPanelNav + 4 
            
            
            
                btLast:ROW    = 1.12
                btLast:COL    = deColPanelNav
                
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btLast:HEIGHT = 1.13
                
                btLast:WIDTH  = 4
                deColPanelNav = deColPanelNav + 4 
            
            
            
                btGoTo:ROW    = 1.12
                btGoTo:COL    = deColPanelNav
                
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btGoTo:HEIGHT = 1.13
                
                btGoTo:WIDTH  = 4
                deColPanelNav = deColPanelNav + 4 
            
            
            
                btSearch:ROW    = 1.12
                btSearch:COL    = deColPanelNav
                
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btSearch:HEIGHT = 1.13
                
                btSearch:WIDTH  = 4
                deColPanelNav   = deColPanelNav + 4
            
            
            
                btAdd:ROW    = 1.12
                btAdd:COL    = deColPanel
                
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btAdd:HEIGHT = 1.13
                
                btAdd:WIDTH  = 4
                deColPanel   = deColPanel + 4
            
            
            
            
            
            
            
                btDelete:ROW    = 1.12
                btDelete:COL    = deColPanel
                
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btDelete:HEIGHT = 1.13
                
                btDelete:WIDTH  = 4
                deColPanel      = deColPanel + 4
            
            
            
            
            
                btCancel:ROW    = 1.12
                btCancel:COL    = deColPanel
                
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btCancel:HEIGHT = 1.13
                
                btCancel:WIDTH  = 4
                deColPanel      = deColPanel + 4
            
            
            
                btSave:ROW    = 1.12
                btSave:COL    = deColPanel
                
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btSave:HEIGHT = 1.13
                
                btSave:WIDTH  = 4
                deColPanel    = deColPanel + 4
            
            

            /*tech30713 - 08/12/2006 - FO 1387046 */
            /*tech30713 - 08/12/2006 - FO 1387046 */
            
            /*tech30713 - 08/12/2006 - FO 1387046 */
            /*tech1139 - 09/02/2007 - FO 1453.143 
            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                btReportsJoins:ROW    = 1.12 
                btReportsJoins:COL    = deColPanel
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btReportsJoins:HEIGHT = 1.13
                &ELSE
                btReportsJoins:HEIGHT = 1.25
                &ENDIF
                btReportsJoins:WIDTH  = 4
                deColPanel            = deColPanel - 4
            &ENDIF

            
            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN
                btQueryJoins:ROW      = 1.12 
                btQueryJoins:COL      = deColPanel
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btQueryJoins:HEIGHT   = 1.13
                &ELSE
                btQueryJoins:HEIGHT   = 1.25
                &ENDIF
                btQueryJoins:WIDTH    = 4
            &ENDIF
            FIM - tech1139 - 09/02/2007 - FO 1453.143 */

            .
        /*tech30713 - 08/12/2006 - FO 1387046 */
    END.
    
    /*--- Executa programa de folder ---*/
    
    
    /*--- Executa programa de estilo de janelas ---*/
    IF  VALID-HANDLE(hWindowStyles) = NO OR
        hWindowStyles:TYPE <> "PROCEDURE":U OR
        (hWindowStyles:FILE-NAME <> "utp/WindowStyles.p":U AND
        hWindowStyles:FILE-NAME <> "utp/WindowStyles.r":U) THEN
        RUN utp/windowstyles.p PERSISTENT SET hWindowStyles.
    
    /*TECH14187 - FO1468831 - Permite maximizar se o pr‚-processador estiver definido como YES*/
    
        /*--- Seta estilo de janela para thinMaintenance ---*/
        RUN disableMax IN hWindowStyles (INPUT wMaintenance:hWnd).
    
    /*FIM TECH14187 FO1468831*/
    
    /*Colocado esta l¢gica para caso o desenvolvedor passar o handle da bo j  
     estartado o mesmo nÆo mostrar erros de execu‡äes antigas sa inicializa‡Æo 
     do programa Feita por Anderson tech485 28/08/2202*/
    
        
            IF VALID-HANDLE(h-dboes001) THEN
                RUN emptyrowerrors IN h-dboes001 NO-ERROR.
        
    
    /*fim altera‡Æo Anderson tech485 28/08/2202*/

    /*--- Inicializa DBOs ---*/
    RUN initializeDBOs IN THIS-PROCEDURE.
    
    /*--- Verifica se a inicializa‡Æo do DBO foi feita com sucesso ---*/
    
        IF NOT VALID-HANDLE(h-dboes001) THEN DO:
            /*--- Exibir mensagem de finaliza‡Æo do programa ---*/
            RUN utp/ut-msgs.p (INPUT "SHOW":U, 
                               INPUT 18881, 
                               INPUT CAPS("ESIN001":U) + "~~":U + CAPS("2.00.00.000":U)).
            
            /*--- Destr¢i programa ---*/
            RUN destroyInterface IN THIS-PROCEDURE.
            
            RETURN "NOK":U.
        END.
        
        ELSE DO:
            /*Foi inclu¡da esta l¢gica para verifica‡Æo de erros relacionados com 
            a permissÆo de execu‡Æo do programa de dbo Altera‡Æo em 17/06/2002 por Anderson(tech540)*/
            RUN getrowerrors IN h-dboes001 (OUTPUT TABLE rowerrors).
            IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorNumber <> 3 AND
                                              RowErrors.ErrorNumber <> 8 AND
                                              RowErrors.ErrorNumber <> 10 AND
                                              RowErrors.ErrorSubType = "ERROR":U) THEN DO:
               
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz somente a instƒncia do utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i1
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz somente a instƒncia do utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 1
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Inicializa tela de mensagens de erros ---*/
IF NOT VALID-HANDLE(hShowMsg) or
   hShowMsg:TYPE <> "PROCEDURE":U or
   hShowMsg:FILE-NAME <> "utp/ShowMessage.w":U THEN
        RUN utp/showmessage.w PERSISTENT SET hShowMsg.

/* _UIB-CODE-BLOCK-END */



 
               
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz a transferˆncia dos erros da temp-table RowErrors para o utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i2
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz a transferˆncia dos erros da temp-table 
                 RowErrors para o utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Transferˆncia dos erros da temp-table RowErrors para o utilit rio ---*/
IF VALID-HANDLE(hShowMsg) and
   hShowMsg:TYPE = "PROCEDURE":U and
   hShowMsg:FILE-NAME = "utp/ShowMessage.w":U THEN DO:
    
        RUN setModal IN hShowMsg (INPUT NO) NO-ERROR.
    

    RUN showMessages IN hShowMsg (INPUT TABLE RowErrors).
END.

/* _UIB-CODE-BLOCK-END */



 

               /****
                TECH14187 - 1538059
                Problema da ShowMessage Modal
                ****/
               
                   WAIT-FOR CLOSE OF hShowMsg.
               

               
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz somente a elimina‡Æo da instƒncia do utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i3
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz somente a elimina‡Æo da instƒncia do 
                 utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Destr¢i tela de mensagens de erros ---*/
IF VALID-HANDLE(hShowMsg) and
  hShowMsg:TYPE = "PROCEDURE":U and
  hShowMsg:FILE-NAME = "utp/ShowMessage.w":U THEN DO:
       RUN destroyInterface IN hShowMsg.
end.
/* _UIB-CODE-BLOCK-END */



 
                RUN destroyInterface IN THIS-PROCEDURE.
                RETURN "NOK":U.
            END.
        END.
        
    
    
    /*--- Reposicionamento autom tico atrav‚s de vari vel global ---*/
    IF gr-controle-inv-esp <> ? THEN DO:
        /*--- Reposiciona DBO atrav‚s de vari vel global ---*/
        RUN repositionRecord IN THIS-PROCEDURE (INPUT gr-controle-inv-esp).
        
        
            IF RETURN-VALUE = "NOK":U THEN
                APPLY "CHOOSE":U TO btFirst IN FRAME fPage0.
        
    END.
    
    
        ELSE DO:
            APPLY "CHOOSE":U TO btFirst IN FRAME fPage0.
        END.
    
    
    
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AfterinitializeInterface":U) <> "":U THEN DO:
    
        RUN AfterinitializeInterface IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN DO:
        /*--- Destr¢i programa ---*/
        RUN destroyInterface IN THIS-PROCEDURE.
        
        RETURN "NOK":U.
    END.

    /*** Alterado por Farley - em 23/07/2003 ***/
    
    
    /*--- Visualiza janela ---*/
    VIEW wMaintenance.
    APPLY "ENTRY":U TO FRAME fPage0.
    APPLY "ENTRY":U TO wMaintenance.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE piTraduz :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*******
    Altera‡Æo 07/12/2005 - tech14187 - Altera‡Æo para evitar estouro de segmento
******/

/*Alteracao 27/07/2005 - tech1007 - Alteracao para fazer a traducao dos templates*/

    RUN utp/ut-liter.p (INPUT "Primeira ocorrˆncia", "*", "R").
    btFirst:TOOLTIP in frame fpage0 = RETURN-VALUE.
    btFirst:HELP IN FRAME fpage0 = RETURN-VALUE.


    RUN utp/ut-liter.p (INPUT "Ocorrˆncia anterior", "*", "R").    
    btPrev:TOOLTIP in frame fpage0 = RETURN-VALUE.
    btPrev:HELP IN FRAME fpage0 = RETURN-VALUE.


    RUN utp/ut-liter.p (INPUT "Pr¢xima ocorrˆncia", "*", "R").    
    btNext:TOOLTIP in frame fpage0 = RETURN-VALUE.
    btNext:HELP IN FRAME fpage0 = RETURN-VALUE.


    RUN utp/ut-liter.p (INPUT "éltima ocorrˆncia", "*", "R").    
    btLast:TOOLTIP in frame fpage0 = RETURN-VALUE.
    btLast:HELP IN FRAME fpage0 = RETURN-VALUE.


    RUN utp/ut-liter.p (INPUT "V  para", "*", "R").    
    btGoTo:TOOLTIP in frame fpage0 = RETURN-VALUE.
    btGoTo:HELP IN FRAME fpage0 = RETURN-VALUE.


    RUN utp/ut-liter.p (INPUT "Pesquisa", "*", "R").    
    btSearch:TOOLTIP in frame fpage0 = RETURN-VALUE.
    btSearch:HELP IN FRAME fpage0 = RETURN-VALUE.


    RUN utp/ut-liter.p (INPUT "Inclui nova ocorrˆncia", "*", "R").    
    btAdd:TOOLTIP in frame fpage0 = RETURN-VALUE.
    btAdd:HELP IN FRAME fpage0 = RETURN-VALUE.




    RUN utp/ut-liter.p (INPUT "Elimina ocorrˆncia corrente", "*", "R").    
    btDelete:TOOLTIP in frame fpage0 = RETURN-VALUE.
    btDelete:HELP IN FRAME fpage0 = RETURN-VALUE.



    RUN utp/ut-liter.p (INPUT "Cancela altera‡äes", "*", "R").    
    btCancel:TOOLTIP in frame fpage0 = RETURN-VALUE.
    btCancel:HELP IN FRAME fpage0 = RETURN-VALUE.


    RUN utp/ut-liter.p (INPUT "Confirma altera‡äes", "*", "R").    
    btSave:TOOLTIP in frame fpage0 = RETURN-VALUE.
    btSave:HELP IN FRAME fpage0 = RETURN-VALUE.


/*tech1139 - 09/02/2007 - FO 1453.143 */

    RUN utp/ut-liter.p (INPUT "Consultas relacionadas", "*", "R").    
    btQueryJoins:TOOLTIP in frame fpage0 = RETURN-VALUE.
    btQueryJoins:HELP IN FRAME fpage0 = RETURN-VALUE.


    RUN utp/ut-liter.p (INPUT "Relat¢rios relacionados", "*", "R").    
    btReportsJoins:TOOLTIP in frame fpage0 = RETURN-VALUE.
    btReportsJoins:HELP IN FRAME fpage0 = RETURN-VALUE.

/*tech1139 - 09/02/2007 - FO 1453.143 */


RUN utp/ut-liter.p (INPUT "Sair", "*", "R").    
btExit:TOOLTIP in frame fpage0 = RETURN-VALUE.
btExit:HELP IN FRAME fpage0 = RETURN-VALUE.
    
RUN utp/ut-liter.p (INPUT "Ajuda", "*", "R").    
btHelp:TOOLTIP in frame fpage0 = RETURN-VALUE.
btHelp:HELP IN FRAME fpage0 = RETURN-VALUE.
/*Fim alteracao 27/07/2005*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi_aplica_facelift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    define variable wg     as widget-handle no-undo.
       

    
        run pi_aplica_facelift_thin in h-facelift ( input frame fPage0:handle ).
    
    
    
    
    
    
    
    
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE repositionRecord :
/*------------------------------------------------------------------------------
  Purpose:     Reposiciona DBO atrav‚s de um rowid
  Parameters:  recebe rowid
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pRowid AS ROWID NO-UNDO.
    
    DEFINE VARIABLE cReturnAux  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rCurrentAux AS ROWID     NO-UNDO.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Reposiciona query do DBO atrav‚s de rowid passado como parƒmetro ---*/
        RUN repositionRecord IN h-dboes001 (INPUT pRowid).
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Seta vari vel cReturnAux com o conte£do do RETURN-VALUE ---*/
        ASSIGN cReturnAux = RETURN-VALUE.
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN h-dboes001 (OUTPUT TABLE tt-controle-inv-esp).
    
    
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    /*alterado*//*alterado*/
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN h-dboes001 (OUTPUT TABLE RowErrors).
            
            /*--- Descri‡Æo dos erros:
                   6: Query est  fechada 
                  11: NÆo foi poss¡vel reposicionar a query ---*/
            
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 6)) THEN DO:

                /*--- Alterado por Valdir (tech264) para permitir customiza‡äes nos botäes ---*/
                /*--- Seta estado dos botäes/menus ---*/
                DO WITH FRAME fPage0:

                    ASSIGN
                        cButtonsStateAux = "":U
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        

                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U            
                        
                        
                            
                        /* tech14187 - 10/04/2007 - 1468007 */
                        /*tech1139 - botÆo Exit */ 
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - botÆo Help */ 
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                    
                    /*--- Seta estado dos botäes/menu ---*/
                    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                    
                    /*--- Seta cursor do mouse para normal ---*/
                    SESSION:SET-WAIT-STATE("":U).
                    
                    RETURN "NOK":U.
                END.
            END.
            
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 11)) THEN DO:

                /*--- Alterado por Valdir (tech264) para permitir customiza‡äes nos botäes ---*/
                /*--- Seta estado dos botäes/menus ---*/
                DO WITH FRAME fPage0:

                    ASSIGN
                        cButtonsStateAux = "":U
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        
                        /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U            
                        
                                                
                        /* tech14187 - 10/04/2007 - 1468007 */
                        /*tech1139 - botÆo Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U            
                        
                        /*tech1139 - botÆo Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                    
                    /*--- Seta estado dos botäes/menu ---*/
                    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                                        
                    /*--- Seta cursor do mouse para normal ---*/
                    SESSION:SET-WAIT-STATE("":U).
                    
                    RETURN "NOK":U.
                END.
            END.
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            
            RETURN "NOK":U.
        END.
    
    
    /*--- Alterado por Valdir (tech264) para permitir customiza‡äes nos botäes ---*/
    /*--- Seta estado dos botäes/menus ---*/
    DO WITH FRAME fPage0:

          ASSIGN
                cButtonsStateAux = "":U
                
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
                /* Desabilitar botÆo de Prev em banco diferente de Progress */
                
                    /*Alteracao 03/10/2006 - tech1007 - Alteracao para desabilitar os botäes de forma correta para bancos Oracle*/
                    /*O teste foi feito desta forma para desabilitar o botÆo prev apenas para versäes >= 9.1B. A partir da 9.1C
                      este botÆo nÆo precisa ser desabilitado */
                    
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    
                
                
                
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
                
                
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
                
                
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
                
                
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
                
                
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
                
                
                
                
                
                
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
                
                
                
                
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                
                
                
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                
                                                    
                /*tech1139 - 05/12/2006 - FO 1387.046 */
                /*alterado*/
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
                
                /*alterado*/
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U            
                
                
                /*tech1139 - botÆo Exit */
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U            

                /*tech1139 - botÆo Help */
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                /*tech1139 - 05/12/2006 - FO 1387.046 */
                .

    END.

    /*--- Seta estado dos botäes/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE saveFields :
/*------------------------------------------------------------------------------
  Purpose:     Salva campos da temp-table {&ttTable}
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
   
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforesaveFields":U) <> "":U THEN DO:
    
        RUN BeforesaveFields IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-SAVE-FIELDS":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-SAVE-FIELDS":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-SAVE-FIELDS":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Salva campos contidos no {&page0KeyFields} ---*/
    
        DO WITH FRAME fPage0:
            ASSIGN tt-controle-inv-esp.sequencia                                  tt-controle-inv-esp.num-ord-inv                                tt-controle-inv-esp.cod-est-exec                               tt-controle-inv-esp.num-projeto                                tt-controle-inv-esp.num-ordem.
        END.
    
    
    /*--- Salva campos contidos no {&page0Fields} ---*/
    
        DO WITH FRAME fPage0:
            ASSIGN tt-controle-inv-esp.dt-trans                                   tt-controle-inv-esp.valor-origem                               tt-controle-inv-esp.narrativa                               fi-estab fi-proj fi-ordem.
        END.
    
    
    /*--- Salva campos contidos no {&page1Fields} ---*/
    
    
    /*--- Salva campos contidos no {&page2Fields} ---*/
    
    
    /*--- Salva campos contidos no {&page3Fields} ---*/
    
    
    /*--- Salva campos contidos no {&page4Fields} ---*/
    
    
    /*--- Salva campos contidos no {&page5Fields} ---*/
    
    
    /*--- Salva campos contidos no {&page6Fields} ---*/
    
    
    /*--- Salva campos contidos no {&page7Fields} ---*/
    
    
    /*--- Salva campos contidos no {&page8Fields} ---*/
    
    
    IF AVAILABLE tt-controle-inv-esp THEN 
         assign epc-rowid = tt-controle-inv-esp.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-SAVE-FIELDS":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-SAVE-FIELDS":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-SAVE-FIELDS":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "controle-inv-esp":U,
                                    INPUT epc-rowid).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AftersaveFields":U) <> "":U THEN DO:
    
        RUN AftersaveFields IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE translate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
         RUN translateMenu IN THIS-PROCEDURE (INPUT wMaintenance:MENU-BAR).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE translateMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/****************************************************************
**
**  I-TRDMN.I - Tradu‡Æo dos Menus das Janelas
**              Conte£do da pi-trad-menu nas Method Library
**  20/03/1997 - Gilsinei
**  01/07/1998 - John C. Jaraceski
****************************************************************/

define input param p-wh-menu as widget-handle no-undo.

define var wh-menu-child      as widget-handle no-undo.
define var wh-menu-grandchild as widget-handle no-undo.

assign p-wh-menu = p-wh-menu:FIRST-CHILD.

do while valid-handle(p-wh-menu):
    if p-wh-menu:LABEL <> ? then do:
        if p-wh-menu:LABEL = "A&juda" or 
           p-wh-menu:LABEL = "&Ajuda" then
            run utp/ut-liter.p (input "Aj&uda", input "*", input "R").
        else
            run utp/ut-liter.p (input replace(p-wh-menu:LABEL, chr(32), "_"),
                                input "*",
                                input "R").
        
        assign p-wh-menu:LABEL = trim(RETURN-VALUE).
    end.
    
    if can-query(p-wh-menu, "FIRST-CHILD") then do:
        assign wh-menu-child = p-wh-menu:FIRST-CHILD.
        
        do while valid-handle(wh-menu-child):
              /* TECH14187 - FO 1494479 - testa se existe accelerator antes de usar */
              IF CAN-QUERY(wh-menu-child,'ACCELERATOR') THEN
                /*Alterado por tech14207 - 24/10/06 - FO:1315708  - Tratamento para acelerar o sair dos programas, passa a ser ctrl-r*/
                IF wh-menu-child:ACCELERATOR = 'CTRL-X' THEN
                    ASSIGN wh-menu-child:ACCELERATOR = 'CTRL-R'.
                /*Fim tech14207*/
              /*FIM TECH14187 - FO 1494479*/

            if  wh-menu-child:LABEL <> ? then do:
                if wh-menu-child:LABEL = "A&juda" or 
                   wh-menu-child:LABEL = "&Ajuda" then
                    run utp/ut-liter.p (input "Aj&uda", input "*", input "R").
                else
                    run utp/ut-liter.p (input replace(wh-menu-child:LABEL, chr(32), "_"),
                                        input "*",
                                        input "R").
                
                assign wh-menu-child:LABEL = trim(RETURN-VALUE).
            end.
            
            if can-query(wh-menu-child, "FIRST-CHILD") then do:
                assign wh-menu-grandchild = wh-menu-child:FIRST-CHILD.
                
                do while valid-handle(wh-menu-grandchild):
                    if wh-menu-grandchild:LABEL <> ? then do:
                        if wh-menu-grandchild:LABEL = "A&juda" or
                           wh-menu-grandchild:LABEL = "&Ajuda" then
                            run utp/ut-liter.p (input "Aj&uda", input "*", input "R").
                        else
                            run utp/ut-liter.p (input replace(wh-menu-grandchild:LABEL, chr(32), "_"),
                                                input "*",
                                                input "R").
                        
                        assign wh-menu-grandchild:LABEL = trim(RETURN-VALUE).
                    end.
                    
                    assign wh-menu-grandchild = wh-menu-grandchild:NEXT-SIBLING.
                end.
            end.
            
            assign wh-menu-child = wh-menu-child:NEXT-SIBLING.
        end.
    end.
    
    assign p-wh-menu = p-wh-menu:NEXT-SIBLING.
end.

/* I-TRDMN.I */
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




 

/* _UIB-CODE-BLOCK-END */





/* ***********  Runtime Attributes and AppBuilder Settings  *********** */


/* SETTINGS FOR WINDOW wMaintenance
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN tt-controle-inv-esp.cod-est-exec IN FRAME fpage0
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN tt-controle-inv-esp.dt-trans IN FRAME fpage0
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN tt-controle-inv-esp.num-ord-inv IN FRAME fpage0
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN tt-controle-inv-esp.num-projeto IN FRAME fpage0
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN tt-controle-inv-esp.valor-origem IN FRAME fpage0
   EXP-LABEL                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wMaintenance)
THEN wMaintenance:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */



/* Setting information for Queries and Browse Widgets fields            */


/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */


 



/* ************************  Control Triggers  ************************ */


ON END-ERROR OF wMaintenance
OR ENDKEY OF wMaintenance ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON WINDOW-CLOSE OF wMaintenance
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btAdd IN FRAME fpage0 /* Add */
OR CHOOSE OF MENU-ITEM miAdd in MENU mbMain DO:
    RUN addRecord IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancel */
OR CHOOSE OF MENU-ITEM miCancel IN MENU mbMain DO:
    RUN cancelRecord IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btCopy IN FRAME fpage0 /* Copy */
OR CHOOSE OF MENU-ITEM miCopy IN MENU mbMain DO:
    /*RUN copyRecord IN THIS-PROCEDURE.*/
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btDelete IN FRAME fpage0 /* Delete */
OR CHOOSE OF MENU-ITEM miDelete IN MENU mbMain DO:
    for each tt-erro:
        delete tt-erro.
    end.
    run pi-remove-movimento in this-procedure.
    if return-value = "OK" then
        RUN deleteRecord IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btExit IN FRAME fpage0 /* Exit */
OR CHOOSE OF MENU-ITEM miExit IN MENU mbMain DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btFirst IN FRAME fpage0 /* First */
OR CHOOSE OF MENU-ITEM miFirst IN MENU mbMain DO:
    RUN getFirst IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btGoTo IN FRAME fpage0 /* Go To */
OR CHOOSE OF MENU-ITEM miGoTo IN MENU mbMain DO:
    RUN goToRecord IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btHelp IN FRAME fpage0 /* Help */
OR CHOOSE OF MENU-ITEM miContents IN MENU mbMain DO:
    /*************************************************************************
**
** AJUDA.I - Include padrÆo para chamada do Help
**
**************************************************************************/


    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).


RETURN NO-APPLY.

/* include/ajuda.i */
 
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btLast IN FRAME fpage0 /* Last */
OR CHOOSE OF MENU-ITEM miLast IN MENU mbMain DO:
    RUN getLast IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btNext IN FRAME fpage0 /* Next */
OR CHOOSE OF MENU-ITEM miNext IN MENU mbMain DO:
    RUN getNext IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btPrev IN FRAME fpage0 /* Prev */
OR CHOOSE OF MENU-ITEM miPrev IN MENU mbMain DO:
    RUN getPrev IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btQueryJoins IN FRAME fpage0 /* Query Joins */
OR CHOOSE OF MENU-ITEM miQueryJoins IN MENU mbMain DO:
    RUN showQueryJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btReportsJoins IN FRAME fpage0 /* Reports Joins */
OR CHOOSE OF MENU-ITEM miReportsJoins IN MENU mbMain DO:
    RUN showReportsJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btSave IN FRAME fpage0 /* Save */
OR CHOOSE OF MENU-ITEM miSave IN MENU mbMain DO:

    if avail tt-controle-inv-esp then
        assign tt-controle-inv-esp.ep-codigo = param-global.empresa-prin
               tt-controle-inv-esp.tipo-doc  = "Apontamento MOB".
    for each tt-erro:
        delete tt-erro.
    end.
    run pi-valida-movimento in this-procedure.
    if return-value = "OK" then do:
        RUN saveRecord IN THIS-PROCEDURE.
        if return-value = "OK" then do:
            for each tt-erro:
                delete tt-erro.
            end.
            run pi-cria-movimento in this-procedure.
        end.
    end.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btSearch IN FRAME fpage0 /* Search */
OR CHOOSE OF MENU-ITEM miSearch IN MENU mbMain DO:
    
/* Procedure Description
"Executa programa de pesquisa, utilizado para fazer o reposicionamento de registro - atrav‚s do m‚todo repositionRecord."
*/


/*--------------------------------------------------------------------------
    File       : method/ZoomReposition.i
    Purpose    : Executa programa de pesquisa, utilizado para fazer o 
                 reposicionamento de registro - atrav‚s do m‚todo 
                 repositionRecord

    Parameters : 
        &ProgramZoom : nome do programa de pesquisa

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa programa de pesquisa ---*/
RUN eszoom\z01es001.w PERSISTENT SET hProgramZoom.

IF VALID-HANDLE(hProgramZoom) THEN DO:
    /**
        TECH14187 - FO 1588390 - Criar op‡Æo de RunMethod.
    **/
    /*--- Executa m‚todo definido pelo desenvolvedor atrav‚s do preprocessador 
          {&RunMethod} ---*/
    
    /*--- Seta, no programa de pesquisa, os campos que devem ser retornados
          Neste caso ser  retornado o ROWID do registro selecionado ---*/
    RUN setFieldNamesHandles IN hProgramZoom (INPUT "ROWID":U, INPUT STRING(THIS-PROCEDURE)).

    /*--- Inicializa programa de pesquisa ---*/
    RUN initializeInterface IN hProgramZoom.
END.

/* _UIB-CODE-BLOCK-END */



 
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btUndo IN FRAME fpage0 /* Undo */
OR CHOOSE OF MENU-ITEM miUndo IN MENU mbMain DO:
    /*RUN undoRecord IN THIS-PROCEDURE.*/
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btUpdate IN FRAME fpage0 /* Update */
OR CHOOSE OF MENU-ITEM miUpdate IN MENU mbMain DO:
    /*RUN updateRecord IN THIS-PROCEDURE.*/
END.

/* _UIB-CODE-BLOCK-END */




ON F5 OF tt-controle-inv-esp.num-ord-inv IN FRAME fpage0 /* Num. Ord. EMS */
DO:
    


/*--------------------------------------------------------------------------
    File        : ZoomVar.i
    Purpose     : Chamar o programa de zoom para as chaves estrangeiras
                  para vari veis e atributos que receberÆo seus valores
                  de atributos com nome diferente.

    Syntax      : {include/zoomvar.i}

    Description : Chama o programa de zoom para os atributos que representam
                  chaves estrangeiras

    Author(s)   : Vanei
    Created     : 16/01/1997
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
  
  if  valid-handle(wh-pesquisa) and
      wh-pesquisa:TYPE = "PROCEDURE":U and
      wh-pesquisa:FILE-NAME = "ivzoom/z05iv043.w":U then
        return.
      
  RUN ivzoom/z05iv043.w persistent set wh-pesquisa.

  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "ivzoom/z05iv043.w":U then
      return.
      
  
  

  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "ivzoom/z05iv043.w":U then do:
    
        RUN pi-entry IN wh-pesquisa.
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(tt-controle-inv-esp.num-ord-inv:handle in frame fpage0) + '|':U + 'num-ord-magnus'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    

    
  end.
                    
assign l-implanta = no.



/* _UIB-CODE-BLOCK-END */



 
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF tt-controle-inv-esp.num-ord-inv IN FRAME fpage0 /* Num. Ord. EMS */
DO:
    run pi-busca-dados in this-procedure.
    RUN afterdisplayfields  in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-DBLCLICK OF tt-controle-inv-esp.num-ord-inv IN FRAME fpage0 /* Num. Ord. EMS */
DO:
    apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */







/*:T--- L¢gica para inicializa‡Æo do programam ---*/

/* Procedure Description
"Method Library que cont‚m a l¢gica da Main Block."
*/


/*--------------------------------------------------------------------------
    Library    : maintenance/MainBlock.i
    Purpose    : Method Library que cont‚m a l¢gica da Main Block 

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Seta cursor do mouse para espera ---*/
SESSION:SET-WAIT-STATE("GENERAL":U).

/*--- Evento de CLOSE padrÆo para THIS-PROCEDURE ---*/
ON CLOSE OF THIS-PROCEDURE 
   RUN destroyInterface IN THIS-PROCEDURE.

/*--- Evento de RETURN padrÆo para FRAME fPage0 ---*/
ON RETURN OF FRAME fPage0 ANYWHERE
    RUN applyReturn IN THIS-PROCEDURE.

/*--- Evento de CTRL-TAB padrÆo para THIS-PROCEDURE ---*/


/*--- Evento de SHIFT-CTRL-TAB padrÆo para THIS-PROCEDURE ---*/


/*--- Seta CURRENT-WINDOW como sendo a window atual ---*/
ASSIGN CURRENT-WINDOW                = wMaintenance
       THIS-PROCEDURE:CURRENT-WINDOW = wMaintenance.

/*--- PadrÆo para janelas GUI ---*/
PAUSE 0 BEFORE-HIDE.


    IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
        /*--- Inicializa programa ---*/
        RUN initializeInterface IN THIS-PROCEDURE.
        IF RETURN-VALUE = "NOK":U THEN
            RETURN "NOK":U.
    END.


/*Alteracao 27/07/2005 - tech1007 - procedure para traduzir tooltips de botoes e tamb‚m de menu*/
RUN translate IN THIS-PROCEDURE.
/*Fim Alteracao 27/07/2005*/

/*--- Block principal do programa ---*/
DO ON ERROR   UNDO, LEAVE
   ON END-KEY UNDO, LEAVE:
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */



 
if tt-controle-inv-esp.num-ord-inv:LOAD-MOUSE-POINTER("image\lupa.cur") then.

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */


PROCEDURE afterDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    if valid-handle(h-esapi001) then do:
        delete procedure h-esapi001.
        assign h-esapi001 = ?.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE AfterDisplayFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN 
        fi-estab:sensitive in frame fPage0 = no
        fi-proj :sensitive in frame fPage0 = no
        fi-ordem:sensitive in frame fPage0 = no.


    FIND FIRST estabelec
        WHERE estabelec.cod-estabel = tt-controle-inv-esp.cod-est-exec:SCREEN-VALUE IN FRAME fPage0 NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN
        ASSIGN fi-estab:SCREEN-VALUE IN FRAME fPage0 = estabelec.nome.
    ELSE 
        ASSIGN fi-estab:SCREEN-VALUE IN FRAME fPage0 = "".

    FIND FIRST proj-inv
        WHERE proj-inv.num-projeto = int(tt-controle-inv-esp.num-projeto:SCREEN-VALUE IN FRAME fPage0) NO-LOCK NO-ERROR.
    IF AVAIL proj-inv THEN
        ASSIGN fi-proj:SCREEN-VALUE IN FRAME fPage0 = proj-inv.descricao.
    ELSE 
        ASSIGN fi-proj:SCREEN-VALUE IN FRAME fPage0 = "".

    FIND FIRST ordem-inv
        WHERE ordem-inv.num-ordem = int(tt-controle-inv-esp.num-ordem:SCREEN-VALUE IN FRAME fPage0) NO-LOCK NO-ERROR.
    IF AVAIL ordem-inv THEN
        ASSIGN fi-ordem:SCREEN-VALUE IN FRAME fPage0 = ordem-inv.descricao.
    ELSE 
        ASSIGN fi-ordem:SCREEN-VALUE IN FRAME fPage0 = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE afterEnableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    disable tt-controle-inv-esp.sequencia with frame fPage0.
    apply "entry":U to tt-controle-inv-esp.num-ord-inv in frame fPage0.

    ASSIGN 
        fi-estab:sensitive in frame fPage0 = no
        fi-proj :sensitive in frame fPage0 = no
        fi-ordem:sensitive in frame fPage0 = no.

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    run esp\esapi001.p persistent set h-esapi001.

    for each tt-erro:
        delete tt-erro.
    end.

    find first param-global no-lock no-error.
    find first param-inv    no-lock 
         where param-inv.ep-codigo = param-global.empresa-prin no-error.

    ASSIGN 
        fi-estab:sensitive in frame fPage0 = no
        fi-proj :sensitive in frame fPage0 = no
        fi-ordem:sensitive in frame fPage0 = no.

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE goToRecord :
/*:T------------------------------------------------------------------------------
  Purpose:     Exibe dialog de V  Para
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUTTON btGoToCancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON btGoToOK AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE RECTANGLE rtGoToButton
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 55 BY 1.42
         BGCOLOR 7.
    
    DEFINE VARIABLE rGoTo AS ROWID NO-UNDO.
    
    DEFINE VARIABLE i-sequencia    LIKE tt-controle-inv-esp.sequencia    NO-UNDO.
    DEFINE VARIABLE i-num-ord-inv  LIKE tt-controle-inv-esp.num-ord-inv  NO-UNDO.
    DEFINE VARIABLE c-cod-est-exec LIKE tt-controle-inv-esp.cod-est-exec NO-UNDO.
    DEFINE VARIABLE i-num-projeto  LIKE tt-controle-inv-esp.num-projeto  NO-UNDO.
    DEFINE VARIABLE i-num-ordem    LIKE tt-controle-inv-esp.num-ordem    NO-UNDO.
    
    DEFINE FRAME fGoToRecord
        i-sequencia       AT ROW 1.21 COL 20.72 colon-aligned view-as fill-in size 6  by 0.88
        i-num-ord-inv     AT ROW 2.21 COL 20.72 colon-aligned view-as fill-in size 10 by 0.88
        c-cod-est-exec    AT ROW 3.21 COL 20.72 colon-aligned view-as fill-in size 4  by 0.88
        i-num-projeto     AT ROW 4.21 COL 20.72 colon-aligned view-as fill-in size 6  by 0.88
        i-num-ordem       AT ROW 5.21 COL 20.72 colon-aligned view-as fill-in size 4  by 0.88
        btGoToOK          AT ROW 6.63 COL 2.14
        btGoToCancel      AT ROW 6.63 COL 13
        rtGoToButton      AT ROW 6.38 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "V  Para Controle Investimento Espec¡fica" FONT 1
             DEFAULT-BUTTON btGoToOK CANCEL-BUTTON btGoToCancel.
    
/*tech1139 - FO 1338.917 - 10/07/2006  */
    RUN utp/ut-trfrrp.p (input Frame fGoToRecord:Handle).
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "V _Para_Controle_Investimento_Espec¡fica",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    ASSIGN FRAME fGoToRecord:TITLE = RETURN-VALUE.
/*tech1139 - FO 1338.917 - 10/07/2006  */

    ON "CHOOSE":U OF btGoToOK IN FRAME fGoToRecord DO:
        ASSIGN i-sequencia i-num-ord-inv c-cod-est-exec i-num-projeto i-num-ordem.

        
        RUN goToKey IN h-dboes001 (input c-cod-est-exec,
                                     input i-num-projeto,
                                     input i-num-ordem,
                                     input i-num-ord-inv,
                                     input i-sequencia).
        IF RETURN-VALUE = "NOK":U THEN DO:
            RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 2, INPUT "Controle Investimento Espec¡fica":U).
            
            RETURN NO-APPLY.
        END.
        
        /*:T Retorna rowid do registro corrente do DBO */
        RUN getRowid IN h-dboes001 (OUTPUT rGoTo).
        
        /*:T Reposiciona registro com base em um rowid */
        RUN repositionRecord IN THIS-PROCEDURE (INPUT rGoTo).

        APPLY "GO":U TO FRAME fGoToRecord.
    END.
    
    ENABLE i-sequencia i-num-ord-inv c-cod-est-exec i-num-projeto i-num-ordem btGoToOK btGoToCancel 
        WITH FRAME fGoToRecord. 
    
    WAIT-FOR "GO":U OF FRAME fGoToRecord.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE initializeDBOs :
/*:T------------------------------------------------------------------------------
  Purpose:     Inicializa DBOs
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*:T--- Verifica se o DBO j  est  inicializado ---*/
    IF NOT VALID-HANDLE(h-dboes001) OR
       h-dboes001:TYPE <> "PROCEDURE":U OR
       h-dboes001:FILE-NAME <> "esbo\boes001.p":U THEN DO:
        /***************************************************************************
**
**   btb008za.i1  -  Include para inicializa‡Æo RPC.
**
***************************************************************************/


        assign c-lst-prg-rpc = c-lst-prg-rpc + min(c-lst-prg-rpc, ",") + "esbo\boes001.p" /* + ","*/ .



    


    

if  not valid-handle(h-servid-rpc)
or h-servid-rpc:type <> "procedure":U
or h-servid-rpc:file-name <> "btb/btapi008.p":U then do:
    run btb/btapi008.p persistent set h-servid-rpc.
end /* if */.

run pi_connect in h-servid-rpc ("esbo\boes001.p", no, "").

if rpc_tip_exec("esbo\boes001.p") = yes then do:

    for each tt-control-prog:
        delete tt-control-prog.
    end.

    create tt-control-prog.
    assign tt-control-prog.cod-versao-integracao = 1
           tt-control-prog.wgh-servid-rpc        = rpc_server("esbo\boes001.p").

    run btb/btb923za.p(input-output table tt-control-prog).
end.
 
        /***************************************************************************
**
**   btb008za.i2  -  Include para execu‡Æo RPC.
**
***************************************************************************/


    


    


    

rpc_exec_set("esbo\boes001.p",yes).
rpc_block:
repeat while rpc_exec("esbo\boes001.p") on stop undo rpc_block, retry rpc_block:
    if rpc_program("esbo\boes001.p") = ? then
       leave rpc_block.

    if  retry
    then do:
        run pi_status_error in h-servid-rpc.
        next rpc_block.
    end /* if */.
    if  rpc_tip_exec("esbo\boes001.p")
    
    
    
    
    
    
    
    
    then do:
    
        run pi_check_server in h-servid-rpc ("esbo\boes001.p").
        if  return-value = 'yes'
        then do:
            if rpc_program("esbo\boes001.p") <> ? then do:
                if  "''" = "''"
                then do:
                    
                        run value(rpc_program("esbo\boes001.p")) persistent set h-dboes001 on rpc_server("esbo\boes001.p") transaction distinct no-error.
                    
                end /* if */.
                else do:
                    
                        run value(rpc_program("esbo\boes001.p")) persistent set h-dboes001 on rpc_server("esbo\boes001.p") transaction distinct ('') no-error.
                    
                end /* else */.
            end.     
        end /* if */.
        else do:
            next rpc_block.
        end /* else */.
    end /* if */.
    else do:
        if rpc_program("esbo\boes001.p") <> ? then do:
            if  "''" = "''"
            then do:
                
                    run value(rpc_program("esbo\boes001.p")) persistent set h-dboes001 no-error.
                
            end /* if */.
            else do:
                
                    run value(rpc_program("esbo\boes001.p")) persistent set h-dboes001 ('') no-error.
                
            end /* else */.
        end.    
    end /* else */.
    
    run pi_status_error in h-servid-rpc.
end /* repeat rpc_block */.
 
    END.
    
    RUN openQueryStatic IN h-dboes001 (INPUT "Main":U) NO-ERROR.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-busca-dados :
/*------------------------------------------------------------------------------
  Purpose:     pi-busca-dados
  Parameters:  
  Notes:       Ao usu rio preencher o n£mero da ordem do EMS, o sistema ir  
               preencher automaticamente as informa‡äes "Estab Exec", 
               "Num Projeto" e "Numero Ordem". Sendo que esses campos serÆo 
               desabilitados, nÆo permitindo a altera‡Æo uma vez que essas 
               informa‡äes se relacionam com a ordem do EMS
------------------------------------------------------------------------------*/

    find first param-global no-lock no-error.

    assign input frame fPage0 tt-controle-inv-esp.num-ord-inv.

    find first sub-div-ordem no-lock
         where sub-div-ordem.ep-codigo      = param-global.empresa-prin
         and   sub-div-ordem.num-ord-magnus = tt-controle-inv-esp.num-ord-inv no-error.

    if avail sub-div-ordem then do:

        find first secao-inv no-lock
             where secao-inv.cod-est-exec = sub-div-ordem.cod-est-exec
             and   secao-inv.ep-codigo    = sub-div-ordem.ep-codigo
             and   secao-inv.num-ordem    = sub-div-ordem.num-ordem
             and   secao-inv.num-projeto  = sub-div-ordem.num-projeto no-error.

        if avail secao-inv then do:

            assign tt-controle-inv-esp.cod-est-exec = secao-inv.cod-est-exec
                   tt-controle-inv-esp.num-projeto  = secao-inv.num-projeto
                   tt-controle-inv-esp.num-ordem    = secao-inv.num-ordem.
            display tt-controle-inv-esp.cod-est-exec
                    tt-controle-inv-esp.num-projeto 
                    tt-controle-inv-esp.num-ordem
                with frame fPage0.
            disable tt-controle-inv-esp.cod-est-exec
                    tt-controle-inv-esp.num-projeto 
                    tt-controle-inv-esp.num-ordem
                with frame fPage0.
        end.
    end.
    else
        enable tt-controle-inv-esp.cod-est-exec
               tt-controle-inv-esp.num-projeto 
               tt-controle-inv-esp.num-ordem
            with frame fPage0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-cria-movimento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    if valid-handle(h-esapi001) then do: 
        /*Cria‡Æo*/
        RUN getRowid     IN h-dboes001 (OUTPUT rRowid).
        run pi-principal in h-esapi001 (input 3,
                                        input rRowid,
                                        input 1,
                                        output table tt-erro).
        if can-find (first tt-erro) then do:
            run cdp/cd0669.w (input table tt-erro).
            return "NOK":U.
        end.
    end.

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-remove-movimento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    if valid-handle(h-esapi001) then do: 
        /*Cria‡Æo*/
        RUN getRowid     IN h-dboes001 (OUTPUT rRowid).
        run pi-principal in h-esapi001 (input 3,
                                        input rRowid,
                                        input 3,
                                        output table tt-erro).
        if can-find (first tt-erro) then do:
            run cdp/cd0669.w (input table tt-erro).
            return "NOK":U.
        end.
    end.

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-valida-movimento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    if valid-handle(h-esapi001) then do: 
        /*Valida‡Æo*/
        run pi-atualiza-verba in h-esapi001 (input  1                               , /* Valida */
                                             input  tt-controle-inv-esp.ep-codigo   ,
                                             input  tt-controle-inv-esp.num-ord-inv ,
                                             input  tt-controle-inv-esp.dt-trans    ,
                                             input  param-inv.moeda-inv             ,
                                             input  0                               , /* Compromissado */
                                             input  tt-controle-inv-esp.valor-origem, /* Realizado */
                                             output table tt-erro).
        if can-find (first tt-erro) then do:
            run cdp/cd0669.w (input table tt-erro).
            return "NOK":U.
        end.
    end.

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




