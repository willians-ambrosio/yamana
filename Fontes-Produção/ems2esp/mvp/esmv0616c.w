


/********************************************************************************
** Copyright DATASUL S.A. (1997)
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

/* Preprocessadores que identificam os bancos do Produto HR */
/*Esta include est  sendo liberada vazia para o EMS 2
 para nÆo ocorrer erros de compila‡Æo*/
 


/* Fim */


/* Fim */
    

/* Fim */
    
/*&if "{&cd_rel_hr}" = "2.10" &then
/*2.10*/
/***************************************************
** i_dbvers.i - Versao dos Bancos Utilizados
***************************************************/
    
    &GLOBAL-DEFINE dthrpmg_version 2.10
    &GLOBAL-DEFINE dthrpyc_version 2.10
    &GLOBAL-DEFINE dthrtma_version 2.10
    &GLOBAL-DEFINE dthrgst_version 2.10
    &GLOBAL-DEFINE mguni_version 2.06B 
    
/* Fim */
&endif.*/    

.    

/* Fim */

.    

.    

/* Fim */


 
/*Fim altera‡Æo 08/09/2006*/

def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.002[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.002"
       c-prg-obj = "ESMV0616C".

/* Alteracao - 02/10/2006 - Nakamura - Incluida chamada a include que verifica a integridade dos programas do registro de produto */
/***
{include/i-ctrlrp.i {1}}
***/

/*Altera‡Æo - 08/09/2006 - tech1007 - Altera‡Æo para exibir o nome do programa que executou o programa que ser  exibido no extrato de versÆo
                                      Solicita‡Æo realizada na FO 1239827*/

/*Fim altera‡Æo 08/09/2006*/


if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "ESMV0616C"
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
        PUT "ESMV0616C" AT 1 "2.00.00.002" AT 69 STRING(TODAY,'99/99/99') AT 84 STRING(TIME,'HH:MM:SS':U) AT 94 SKIP.
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

   /*** 2.00.00.002 ***/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */




/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

/** Sele‡Æo **/
define temp-table ttSelecao no-undo
    field empresa-ini    like mab-eqpto.ep-codigo
    field empresa-fim    like mab-eqpto.ep-codigo
    field equipto-ini    like mab-eqpto.cod-eqpto
    field equipto-fim    like mab-eqpto.cod-eqpto
    field grupo-ini      like mab-eqpto.cod-grp-eqpto
    field grupo-fim      like mab-eqpto.cod-grp-eqpto
    field modelo-ini     like mab-eqpto.cod-model
    field modelo-fim     like mab-eqpto.cod-model
    field estab-ini      like mab-eqpto.cod-estabel
    field estab-fim      like mab-eqpto.cod-estabel
    field ano-fabric-ini like mab-eqpto.vli-ano-fabric
    field ano-fabric-fim like mab-eqpto.vli-ano-fabric
    field estrut-ini     like mab-eqpto.cod-estrut-mec
    field estrut-fim     like mab-eqpto.cod-estrut-mec
    field tag-ini        like mab-eqpto.cd-tag
    field tag-fim        like mab-eqpto.cd-tag
    field cc-ini         like mab-histor-ativid.cc-codigo
    field cc-fim         like mab-histor-ativid.cc-codigo
    field periodo-ini    as date
    field periodo-fim    as date
    FIELD dt-trans-ini   AS DATE
    FIELD dt-trans-fim   AS DATE
    field lMtbf          as logical
    field lMttr          as logical
    field lDispo         as logical
    field lPmpl          as logical
    field iIndicador     as integer
    FIELD iTipoDispo     AS INTEGER
    field lMat           as logical
    field lGGF           as logical
    field lServ          as logical
    field lContratos     as logical
    field lCusto         as logical
    field lTotal         as logical
    field lMatMesAnt     as logical
    field iNivTag        as integer.

/**Temp-table para dados do Excell**/
define temp-table tt-dados-ex no-undo
    field arquivo-num                   as integer format ">9"     initial 1
    field planilha-num                  as integer format ">9"
    field celula-coluna                 as integer format ">>>>9"
    field celula-linha                  as integer format ">>>>9"
    field celula-cor-interior           as integer format ">9"     initial 58 /* None */
    field celula-formato                as char    format "x(255)"
    field celula-formula                as char    format "x(255)"
    field celula-alinhamento-horizontal as integer format "9"      initial 4 /* Left */
    field celula-alinhamento-vertical   as integer format "9"      initial 1 /* Bottom */
    field celula-valor                  as char    format "x(255)"
    field celula-fonte-nome             as char    format "x(255)" initial "Times New Roman"
    field celula-fonte-tamanho          as integer format ">9"     initial 10
    field celula-fonte-negrito          as logical                 initial no
    field celula-fonte-italico          as logical                 initial no
    field celula-fonte-sublinhado       as integer format "9"      initial 3 /* None */
    field celula-fonte-cor              as integer format ">9"     initial 57 /* Automatic */
    field celula-tipo-borda-sup         as integer format "9"      initial 7 /* None */
    field celula-tipo-borda-inf         as integer format "9"      initial 7 /* None */
    field celula-tipo-borda-esq         as integer format "9"      initial 7 /* None */
    field celula-tipo-borda-dir         as integer format "9"      initial 7 /* None */
    index tt-dados-pri is unique primary
        arquivo-num
        planilha-num
        celula-coluna
        celula-linha.
  /** Defini‡Æo da ttSelecao **/

/* Parameters Definitions ---                                           */
define input-output parameter table for ttSelecao.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */


/* Name of designated FRAME-NAME and/or first browse and/or first query */

/* Standard List Definitions                                            */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */




/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE fiNivTag AS INTEGER FORMAT ">>9":U INITIAL 999 
     LABEL "N¡vel Tag" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fiTexto1 AS CHARACTER FORMAT "X(256)":U INITIAL "Indicador de" 
      VIEW-AS TEXT 
     SIZE 9 BY .67 NO-UNDO.

DEFINE VARIABLE fiTexto2 AS CHARACTER FORMAT "X(256)":U INITIAL "Indicadores" 
      VIEW-AS TEXT 
     SIZE 9 BY .67 NO-UNDO.

DEFINE VARIABLE fiTexto3 AS CHARACTER FORMAT "X(256)":U INITIAL "Material" 
      VIEW-AS TEXT 
     SIZE 6 BY .67 NO-UNDO.

DEFINE VARIABLE fiTexto4 AS CHARACTER FORMAT "X(256)":U INITIAL "Disponibilidade" 
      VIEW-AS TEXT 
     SIZE 11 BY .67 NO-UNDO.

DEFINE VARIABLE rsIndicador AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Manuten‡Æo de Frotas", 1,
"Manuten‡Æo Industrial", 2
     SIZE 50 BY 1.13 NO-UNDO.

DEFINE VARIABLE rsTipoDispo AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Encerramento O.M.", 1,
"Horas Apropriadas", 2
     SIZE 21 BY 2 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54.43 BY 2.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54.43 BY 2.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54.43 BY 4.5.

DEFINE RECTANGLE RECT-77
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54.43 BY 2.29.

DEFINE RECTANGLE RECT-78
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54.29 BY 2.5.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 55 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tgContratos AS LOGICAL INITIAL yes 
     LABEL "Contratos" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE tgCusto AS LOGICAL INITIAL yes 
     LABEL "Custo" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE tgDispo AS LOGICAL INITIAL yes 
     LABEL "Disponibilidade" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tgGGF AS LOGICAL INITIAL yes 
     LABEL "Pessoal e Indiretos" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .83 NO-UNDO.

DEFINE VARIABLE tgMat AS LOGICAL INITIAL yes 
     LABEL "Material" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.72 BY .88 NO-UNDO.

DEFINE VARIABLE tgMedio AS LOGICAL INITIAL yes 
     LABEL "M‚dio do mˆs anterior" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .88 NO-UNDO.

DEFINE VARIABLE tgMtbf AS LOGICAL INITIAL yes 
     LABEL "MTBF" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgMttr AS LOGICAL INITIAL yes 
     LABEL "MTTR" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgPmpl AS LOGICAL INITIAL yes 
     LABEL "PMPL" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgServ AS LOGICAL INITIAL yes 
     LABEL "Servi‡os" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE tgTotal AS LOGICAL INITIAL yes 
     LABEL "Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     rsIndicador AT ROW 2.04 COL 5 NO-LABEL
     tgDispo AT ROW 4.25 COL 4
     tgPmpl AT ROW 4.25 COL 22
     tgServ AT ROW 4.25 COL 40
     tgMtbf AT ROW 5.25 COL 4
     tgMat AT ROW 5.25 COL 22
     tgTotal AT ROW 5.25 COL 40
     tgMttr AT ROW 6.25 COL 4
     tgGGF AT ROW 6.25 COL 22
     tgContratos AT ROW 6.25 COL 40
     tgCusto AT ROW 7.25 COL 40
     rsTipoDispo AT ROW 8.75 COL 18 NO-LABEL
     fiNivTag AT ROW 12 COL 10 COLON-ALIGNED
     tgMedio AT ROW 14.46 COL 3.43
     btOK AT ROW 16.21 COL 2
     btCancel AT ROW 16.21 COL 13
     btHelp2 AT ROW 16.21 COL 45
     fiTexto1 AT ROW 1.25 COL 2 COLON-ALIGNED NO-LABEL
     fiTexto2 AT ROW 3.5 COL 2 COLON-ALIGNED NO-LABEL
     fiTexto4 AT ROW 9.25 COL 3 COLON-ALIGNED NO-LABEL
     fiTexto3 AT ROW 13.54 COL 1.43 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.5 COL 1.57
     RECT-2 AT ROW 13.79 COL 1.57
     RECT-3 AT ROW 3.75 COL 1.57
     rtToolBar AT ROW 16 COL 1
     RECT-77 AT ROW 11.25 COL 1.57
     RECT-78 AT ROW 8.5 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 55 BY 16.42
         FONT 1.


/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */


/* *************************  Create Window  ************************** */


IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 16.42
         WIDTH              = 55
         MAX-HEIGHT         = 30.33
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 30.33
         VIRTUAL-WIDTH      = 182.86
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE wWindow = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */



/* ************************* Included-Libraries *********************** */


/* Procedure Description
"Method Library principal para Maintenance Template, que cont‚m defini‡äes e chamadas a outras Method Libraries."
*/


/*--------------------------------------------------------------------------
    Library    : window/Window.i
    Purpose    : Method Library principal para Window Template, que 
                 cont‚m defini‡äes e chamadas a outras Method Libraries 

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* Global Variable Definitions ---                                        */
DEFINE NEW GLOBAL SHARED VARIABLE hWindowStyles AS HANDLE NO-UNDO.


    /**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    define new global shared variable h-facelift as handle no-undo.
    if not valid-handle(h-facelift) then run btb/btb901zo.p persistent set h-facelift.


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



 


/* Local Variable Definitions ---                                         */
DEFINE VARIABLE hFolder           AS HANDLE  NO-UNDO.
DEFINE VARIABLE hProgramZoom      AS HANDLE  NO-UNDO.
DEFINE VARIABLE hQueryJoins       AS HANDLE  NO-UNDO.
DEFINE VARIABLE hReportsJoins     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hShowMsg          AS HANDLE  NO-UNDO.
DEFINE VARIABLE hWindowParent     AS HANDLE  NO-UNDO.
DEFINE VARIABLE lCustomExecuted   AS LOGICAL NO-UNDO.
DEFINE VARIABLE lOverrideExecuted AS LOGICAL NO-UNDO.

DEFINE VARIABLE c-nom-prog-dpc-mg97  AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nom-prog-appc-mg97 AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nom-prog-upc-mg97  AS CHARACTER NO-UNDO.

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

 
/*** Alterado por Farley - em 23/07/2003 ***/
 

/* _UIB-CODE-BLOCK-END */



 





/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */




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
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
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
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
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







PROCEDURE destroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     Destr¢i programa
  Parameters:  
  Notes:       Destr¢i programa de Folder
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
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Destr¢i programa de folder ---*/
    
    
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
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
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
    
    
        /*--- Retira estilo de janela (modal) para thinMaintenace ---*/
        IF VALID-HANDLE(hWindowParent) THEN DO:
            ASSIGN hWindowParent:SENSITIVE = YES.
        END.
    
    
    /*--- Destr¢i janela associada ao programa ---*/
    IF VALID-HANDLE(wWindow) THEN
        DELETE WIDGET wWindow.
    
    /*--- Destr¢i programa ---*/
    IF THIS-PROCEDURE:PERSISTENT THEN
        DELETE PROCEDURE THIS-PROCEDURE.
    
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







PROCEDURE displayWidgets :
/*------------------------------------------------------------------------------
  Purpose:     Exibe os widgets em tela
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
IF THIS-PROCEDURE:GET-SIGNATURE("BeforedisplayWidgets":U) <> "":U THEN DO:
    
        RUN BeforedisplayWidgets IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
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
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-DISPLAY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-DISPLAY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Exibe widgets contidos em {page0Widgets} ---*/
    
        DISPLAY btOK btCancel btHelp2                               rsIndicador tgMtbf tgMttr tgDispo                               tgPmpl tgMat tgServ tgContratos tgMedio tgGGF                               tgTotal fiNivTag rsTipoDispo WITH FRAME fPage0.
    
    
    /*--- Exibe widgets contidos em {page1Widgets} ---*/
    
    
    /*--- Exibe widgets contidos em {page2Widgets} ---*/
    
    
    /*--- Exibe widgets contidos em {page3Widgets} ---*/
    
    
    /*--- Exibe widgets contidos em {page4Widgets} ---*/
    
    
    /*--- Exibe widgets contidos em {page5Widgets} ---*/
    
    
    /*--- Exibe widgets contidos em {page6Widgets} ---*/
    
    
    /*--- Exibe widgets contidos em {page7Widgets} ---*/
    
    
    /*--- Exibe widgets contidos em {page8Widgets} ---*/
    
    
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
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-DISPLAY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-DISPLAY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
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
IF THIS-PROCEDURE:GET-SIGNATURE("AfterdisplayWidgets":U) <> "":U THEN DO:
    
        RUN AfterdisplayWidgets IN THIS-PROCEDURE NO-ERROR.
    

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







PROCEDURE enableWidgets :
/*------------------------------------------------------------------------------
  Purpose:     Habilita os widgets em tela
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
IF THIS-PROCEDURE:GET-SIGNATURE("BeforeenableWidgets":U) <> "":U THEN DO:
    
        RUN BeforeenableWidgets IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
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
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Habilita widgets contidos em {page0Widgets} ---*/
    
        ENABLE btOK btCancel btHelp2                               rsIndicador tgMtbf tgMttr tgDispo                               tgPmpl tgMat tgServ tgContratos tgMedio tgGGF                               tgTotal fiNivTag rsTipoDispo WITH FRAME fPage0.
    
    
    /*--- Habilita widgets contidos em {page1Widgets} ---*/
    
    
    /*--- Habilita widgets contidos em {page2Widgets} ---*/
    
    
    /*--- Habilita widgets contidos em {page3Widgets} ---*/
    
    
    /*--- Habilita widgets contidos em {page4Widgets} ---*/
    
    
    /*--- Habilita widgets contidos em {page5Widgets} ---*/
    
    
    /*--- Habilita widgets contidos em {page6Widgets} ---*/
    
    
    /*--- Habilita widgets contidos em {page7Widgets} ---*/
    
    
    /*--- Habilita widgets contidos em {page8Widgets} ---*/
    
    
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
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
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
IF THIS-PROCEDURE:GET-SIGNATURE("AfterenableWidgets":U) <> "":U THEN DO:
    
        RUN AfterenableWidgets IN THIS-PROCEDURE NO-ERROR.
    

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
        VIEW FRAME fPage0 IN WINDOW wWindow.
    END.
    
    /*--- Executa valida‡äes de inicializa‡Æo ---*/
    ASSIGN c-programa-mg97 = CAPS("ESMV0616C":U)
           c-versao-mg97   = "2.00.00.002":U.
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
      if "Window" = "SmartDialog" or this-procedure:persistent = no then
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
      if "Window" = "SmartDialog" then
        return "adm-error".
      else do:
        delete procedure this-procedure.
        return.
      end.  
  end.
end.  

/* ut-vfsec.i */
 
    

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
    /* TECH14187 - FO 1514824 - Erro na tradu‡Æo do titulo das ThinWindow */
    run utp/ut-liter.p (input replace(c-titulo-prog-mg97, " ", "_") ,
                        input "*":U,
                        input "":U). 

    Assign c-titulo-prog-mg97 = Return-value.

    
        
             assign wWindow:title = if l-achou-prog then
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
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa programa de folder ---*/
    
    
    /*--- Executa programa de estilo de janelas ---*/
    IF  VALID-HANDLE(hWindowStyles) = NO OR
        hWindowStyles:TYPE <> "PROCEDURE":U OR
        (hWindowStyles:FILE-NAME <> "utp/WindowStyles.p":U AND
        hWindowStyles:FILE-NAME <> "utp/WindowStyles.r":U) THEN
        RUN utp/windowstyles.p PERSISTENT SET hWindowStyles.
    
    /*--- Exibe Widgets em tela ---*/
    RUN displayWidgets IN THIS-PROCEDURE.
    
    /*--- Habilita Widgets em tela ---*/
    RUN enableWidgets IN THIS-PROCEDURE.

    /*--- Paulo - FO 679.985 
          Este tratamento deve ser feito depois de inicializar algum 
          objeto na window, para que o handle da window seja v lido ---*/
    
        /*--- Seta estilo de janela para thinWindow ---*/
        RUN deleteMinMax IN hWindowStyles (INPUT wWindow:hWnd).
    
    
    
    
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
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
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
    VIEW wWindow.
    
    
        /*--- Seta estilo de janela modal para thinMaintenance ---*/
        /* tech1139 - 02/11/2005 - FO 1214.774 */

/*         ASSIGN hWindowParent           = SESSION:FIRST-CHILD        */
/*                hWindowParent           = hWindowParent:NEXT-SIBLING */
/*                hWindowParent:SENSITIVE = NO.                        */    
/*
        ASSIGN i = LENGTH(PROGRAM-NAME(2)) - 23.
        ASSIGN c-programa = SUBSTRING(PROGRAM-NAME(2),24,i).
        /* tech1139 - para separar o nome do programa da string USER-INTERFACE-TRIGGER no program-name(2) */
*/    

        /*Alteracao 24/01/2006 - tech14207 - Alterado para desabilitar o programa pai*/
        /*O c¢digo foi alterado para permitir que o mesmo programa fosse executado mais de uma vez corretamente*/
        
        
            DEFINE VAR h-source AS HANDLE     NO-UNDO.
            DEFINE VARIABLE h-this AS HANDLE     NO-UNDO. /*26/09/2006 - tech30713 - FO: 1309021*/
    
            ASSIGN h-source = SOURCE-PROCEDURE.
            ASSIGN h-this = THIS-PROCEDURE.
    
            IF (THIS-PROCEDURE <> SOURCE-PROCEDURE) THEN DO:
                ASSIGN h-source = SOURCE-PROCEDURE.
                IF VALID-HANDLE(h-source) AND  VALID-HANDLE(h-source:CURRENT-WINDOW) THEN DO:
                    ASSIGN hWindowParent           = h-source:CURRENT-WINDOW
                           hWindowParent:SENSITIVE = NO.  /* FO 1367.797 - tech1139 - 08/09/2006 */
                END.
            END.
        
        /*Fim da altera‡Æo 24/01/2006 - tech14207*/
    
    
    APPLY "ENTRY":U TO FRAME fPage0.
    APPLY "ENTRY":U TO wWindow.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE showQueryJoins :
/*------------------------------------------------------------------------------
  Purpose:     Executa janela de Consultas Relacionadas
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Inicializa janela de Consultas Relacionadas ---*/
    IF NOT VALID-HANDLE(hQueryJoins) THEN
        RUN utp/ut-cons.w PERSISTENT SET hQueryJoins (INPUT c-programa-mg97).
    
    IF VALID-HANDLE(hQueryJoins) THEN
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
    
    /*--- Inicializa janela de Relat¢rios Relacionados ---*/
    IF NOT VALID-HANDLE(hReportsJoins) THEN
        RUN utp/ut-relat.w PERSISTENT SET hReportsJoins (INPUT c-programa-mg97).
    
    IF VALID-HANDLE(hReportsJoins) THEN
        RUN dispatch IN hReportsJoins (INPUT "INITIALIZE":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




/*Alterado 07/11/2006 - tech1007 - FO 1410116 - Cria‡Æo das procedures respons veis pela tradu‡Æo do template*/



PROCEDURE translate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE translateMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



/*Fim altera‡Æo 07/11/2006*/
 

/* _UIB-CODE-BLOCK-END */





/* ***********  Runtime Attributes and AppBuilder Settings  *********** */


/* SETTINGS FOR WINDOW wWindow
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */



/* Setting information for Queries and Browse Widgets fields            */


/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */


 



/* ************************  Control Triggers  ************************ */


ON END-ERROR OF wWindow
OR ENDKEY OF wWindow ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON WINDOW-CLOSE OF wWindow
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
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




ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
    run piGrava in this-procedure.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON VALUE-CHANGED OF rsIndicador IN FRAME fpage0
DO:
    if rsIndicador:screen-value in frame fPage0 = "1" then
        assign tgContratos:sensitive in frame fPage0 = yes
               tgContratos:checked   in frame fPage0 = yes
               fiNivTag:sensitive    in frame fPage0 = no.
    else
        assign tgContratos:sensitive in frame fPage0 = no
               tgContratos:checked   in frame fPage0 = no
               fiNivTag:sensitive    in frame fPage0 = yes.
END.

/* _UIB-CODE-BLOCK-END */







/*--- L¢gica para inicializa‡Æo do programam ---*/

/* Procedure Description
"Method Library que cont‚m a l¢gica da Main Block."
*/


/*--------------------------------------------------------------------------
    Library    : window/MainBlock.i
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

/*--- Evento de CTRL-TAB padrÆo para THIS-PROCEDURE ---*/


/*--- Evento de SHIFT-CTRL-TAB padrÆo para THIS-PROCEDURE ---*/


/*--- Seta CURRENT-WINDOW como sendo a window atual ---*/
ASSIGN CURRENT-WINDOW                = wWindow
       THIS-PROCEDURE:CURRENT-WINDOW = wWindow.


/**** Alteracao efetuada por tech38629 para o projeto Facelift ****/
    
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage0:handle ).
    
    
    
    
    
    
    
    
    


/*--- PadrÆo para janelas GUI ---*/
PAUSE 0 BEFORE-HIDE.


    IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
        /*--- Inicializa programa ---*/
        RUN initializeInterface IN THIS-PROCEDURE.
        IF RETURN-VALUE = "NOK":U THEN
            RETURN "NOK":U.
    END.


/*Alterado por tech14207 - 24/10/06 - FO:1315708  - Tratamento para acelerar o sair dos programas, passa a ser ctrl-r*/
RUN translate IN THIS-PROCEDURE.
/*FIM tech 14207*/

/*--- Block principal do programa ---*/
DO ON ERROR   UNDO, LEAVE
   ON END-KEY UNDO, LEAVE:
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */


    
 

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */


PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     afterInitializeInterface
  Parameters:  <none>
  Notes:       Override ap¢s inicializa‡Æo da tela.
------------------------------------------------------------------------------*/

/** Labels dos campos **/
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Indicador_de",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign fiTexto1:screen-value in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Indicadores",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign fiTexto2:screen-value in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Material",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign fiTexto3:screen-value in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "MTBF",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tgMtbf:label in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "MTTR",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tgMttr:label in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Disponibilidade",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tgDispo:label in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "PMPL",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tgPmpl:label in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Material",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tgMat:label in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "GGF",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tgGGF:label in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Servi‡os",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tgServ:label in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Contratos",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tgContratos:label in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Custo",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tgCusto:label in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Total",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tgTotal:label in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "M‚dio_do_mˆs_anterior",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tgMedio:label in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "N¡vel_Tag",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign fiNivTag:label in frame fPage0 = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Disponibilidade",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign fiTexto4:screen-value in frame fPage0 = return-value.

/** Busca Parƒmtros **/
find first ttSelecao no-lock no-error.
if avail ttSelecao then do:
    assign tgMtbf:checked       in frame fPage0 = ttSelecao.lMtbf
           tgMttr:checked       in frame fPage0 = ttSelecao.lMttr
           tgDispo:checked      in frame fPage0 = ttSelecao.lDispo
           tgPmpl:checked       in frame fPage0 = ttSelecao.lPmpl
           rsIndicador                          = ttSelecao.iIndicador
           tgMat:checked        in frame fPage0 = ttSelecao.lMat
           tgGGF:checked        in frame fPage0 = ttSelecao.lGGF
           tgServ:checked       in frame fPage0 = ttSelecao.lServ
           tgContratos:checked  in frame fPage0 = ttSelecao.lContratos
           tgCusto:checked      in frame fPage0 = ttSelecao.lCusto
           tgTotal:checked      in frame fPage0 = ttSelecao.lTotal
           tgMedio:checked      in frame fPage0 = ttSelecao.lMatMesAnt
           fiNivTag:screen-value in frame fPage0 = string(ttSelecao.iNivTag)
           rsTipoDispo                           = ttSelecao.iTipoDispo.
end.

if ttSelecao.iIndicador = 1 then
    assign tgContratos:sensitive in frame fPage0 = yes
           fiNivTag:sensitive in frame fPage0    = no.
else
    assign tgContratos:sensitive in frame fPage0 = no
           fiNivTag:sensitive in frame fPage0    = yes.

/** Assign temporario **/
ASSIGN tgContratos:VISIBLE IN FRAME fPage0 = NO
       tgCusto:VISIBLE     IN FRAME fPage0 = NO.

DISP rsIndicador 
     rsTipoDispo WITH FRAME fPage0.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piGrava :
/*------------------------------------------------------------------------------
  Purpose:     piGrava
  Parameters:  <none>
  Notes:       Grava os parƒmetros selecionados na temp-table
------------------------------------------------------------------------------*/

find first ttSelecao exclusive-lock no-error.
if avail ttSelecao then do:
    assign ttSelecao.lMtbf         = tgMtbf:checked      in frame fPage0    
           ttSelecao.lMttr         = tgMttr:checked      in frame fPage0    
           ttSelecao.lDispo        = tgDispo:checked     in frame fPage0    
           ttSelecao.lPmpl         = tgPmpl:checked      in frame fPage0
           ttSelecao.iIndicador    = input frame fPage0 rsIndicador
           ttSelecao.lMat          = tgMat:checked       in frame fPage0        
           ttSelecao.lGGF          = tgGGF:checked       in frame fPage0
           ttSelecao.lServ         = tgServ:checked      in frame fPage0
           ttSelecao.lContratos    = tgContratos:checked in frame fPage0
           ttSelecao.lCusto        = tgCusto:checked     in frame fPage0
           ttSelecao.lTotal        = tgTotal:checked     in frame fPage0
           ttSelecao.lMatMesAnt    = tgMedio:checked     in frame fPage0
           ttSelecao.iNivTag       = int(fiNivTag:screen-value in frame fPage0)
           ttSelecao.iTipoDispo    = input frame fPage0 rsTipoDispo.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */


