/* Connected Databases 
*/

/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa     for ems2cadme.empresa.

{include/i-prgvrs.i ESMV0616 2.00.00.016}
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




DEFINE VARIABLE hDBOTag AS HANDLE     NO-UNDO.

/* Local Variable Definitions ---                                       */
define variable c-arquivo     as character                no-undo.
DEFINE VARIABLE i-cont               AS INTEGER                            NO-UNDO.
DEFINE VARIABLE iVisao               AS INTEGER                            NO-UNDO.
DEFINE VARIABLE lReal                AS LOGICAL                            NO-UNDO.
DEFINE VARIABLE d-hr-hora-ini        AS DECIMAL FORMAT ">9.99"             NO-UNDO.
DEFINE VARIABLE d-hr-hora-fim        AS DECIMAL FORMAT ">9.99"             NO-UNDO.
DEFINE VARIABLE da-data-retorno      AS DATE    FORMAT "99/99/9999"        NO-UNDO.
DEFINE VARIABLE da-data-parada       AS DATE    FORMAT "99/99/9999"        NO-UNDO.
DEFINE VARIABLE d-hr-manut-aux       AS DECIMAL FORMAT ">>9.99"            NO-UNDO.
DEFINE VARIABLE d-val-mat            AS DECIMAL FORMAT "->>>,>>>,>>9"      NO-UNDO.
DEFINE VARIABLE d-val-servico        AS DECIMAL FORMAT "->>>,>>>,>>9"      NO-UNDO.
DEFINE VARIABLE d-val-proces         AS DECIMAL FORMAT "->>>,>>>,>>9"      NO-UNDO. 
DEFINE VARIABLE i-nr-inter-corre     AS INTEGER                            NO-UNDO.
DEFINE VARIABLE c-hr-hora-ini        AS CHAR    FORMAT "99:99:99"          NO-UNDO.
DEFINE VARIABLE c-hr-hora-fim        AS CHAR    FORMAT "99:99:99"          NO-UNDO.
DEFINE VARIABLE i-nr-corre-fin       AS INTEGER                            NO-UNDO.
DEFINE VARIABLE d-tot-corre-fin      AS DECIMAL INITIAL 0                  NO-UNDO. 
DEFINE VARIABLE d-tot-prev           AS DECIMAL INITIAL 0                  NO-UNDO. 
DEFINE VARIABLE d-tot-corre          AS DECIMAL INITIAL 0                  NO-UNDO. 
DEFINE VARIABLE d-tot-pred           AS DECIMAL INITIAL 0                  NO-UNDO. 
DEFINE VARIABLE d-tot-corre-plan     AS DECIMAL INITIAL 0                  NO-UNDO.
DEFINE VARIABLE d-tot-corre-abert    AS DECIMAL INITIAL 0                  NO-UNDO.
DEFINE VARIABLE dataAux              AS DATE                               NO-UNDO.
DEFINE VARIABLE i-nr-dias            AS INTEGER                            NO-UNDO.
DEFINE VARIABLE dCalc                AS DECIMAL FORMAT "->>>,>>>,>>9.99"   NO-UNDO.
DEFINE VARIABLE dManutCorre          AS DECIMAL FORMAT "->>>,>>>,>>9.99"   NO-UNDO.
DEFINE VARIABLE dGGF                 AS DECIMAL FORMAT ">>>>,>>>,>>9.9999" NO-UNDO.
DEFINE VARIABLE hAcomp               as handle                             NO-UNDO.
DEFINE VARIABLE cEstado              AS CHARACTER FORMAT "x(20)"           NO-UNDO.
DEFINE VARIABLE deHoras              AS DECIMAL FORMAT "->>,>>9.99"        NO-UNDO.
DEFINE VARIABLE deHorasAux           AS DECIMAL FORMAT "->>,>>9.99"        NO-UNDO.
DEFINE VARIABLE deHorasTag           AS DECIMAL FORMAT "->>,>>9.99"        NO-UNDO.
DEFINE VARIABLE cTime                AS CHARACTER                          NO-UNDO.
DEFINE VARIABLE dtData               LIKE mmv-ord-manut.dat-prev           NO-UNDO.
DEFINE VARIABLE dHorasDia            AS DECIMAL                            NO-UNDO.
DEFINE VARIABLE iCont                AS INTEGER                            NO-UNDO.
DEFINE VARIABLE cAux                 AS CHARACTER                          NO-UNDO.
DEFINE VARIABLE deKM                 AS DECIMAL FORMAT "->>>,>>>,>>9"      NO-UNDO.
DEFINE VARIABLE deKM2                AS DECIMAL FORMAT "->>>,>>>,>>9"      NO-UNDO.
DEFINE VARIABLE dtInicioOrdem        LIKE mmv-ord-manut.dat-prev           NO-UNDO.
DEFINE VARIABLE dtFimOrdem           LIKE mmv-ord-manut.dat-prev           NO-UNDO. 
DEFINE VARIABLE cHoraInicioOrdem     AS CHARACTER                          NO-UNDO.
DEFINE VARIABLE cHoraFimOrdem        AS CHARACTER                          NO-UNDO.
DEFINE VARIABLE i-qt-eqpto           AS INTEGER INITIAL 0                  NO-UNDO.

DEFINE VARIABLE iTipo                AS INTEGER                            NO-UNDO.
DEFINE VARIABLE dData                AS DATE                               NO-UNDO.
DEFINE VARIABLE iEmp                 AS CHARACTER                          NO-UNDO.
DEFINE VARIABLE cEqpto               AS CHARACTER                          NO-UNDO.
DEFINE VARIABLE cModel               AS CHARACTER   NO-UNDO.

/** Busca Valor **/
define variable rRowid        as rowid                    no-undo.
define variable cCodPai       as character format "x(20)" no-undo.
define variable vCodigo       as character format "x(20)" no-undo.
define variable vDescricao    as character format "x(32)" no-undo.
define variable iImage        as integer                  no-undo.
define variable cTag          as character                no-undo.
/** OCX **/
DEFINE VARIABLE chTreeList    AS COM-HANDLE               NO-UNDO.
define variable chImageList   as com-handle               no-undo.
/** Controles **/
define variable l-expande     as logical init no          no-undo.
define variable l-ok          as logical                  no-undo.
define variable i-result      as integer                  no-undo.
define variable i-button-tree as integer  initial 1       no-undo.
/** Pop-Menu **/
define variable pop-menu      as widget-handle            no-undo.
define variable exp-con       as widget-handle            no-undo.
define variable detalhe       as widget-handle            no-undo.
define variable regua         as widget-handle            no-undo.
/*** Window ***/
define variable dWinXC        as decimal                  no-undo.
define variable dWinYC        as decimal                  no-undo.
/*** EXCEL ***/
DEFINE VARIABLE chExcel       AS COM-HANDLE.
/** defini‡äes variaveis operacionais **/
DEFINE VARIABLE ch-ComponenteTemp  AS COM-HANDLE    NO-UNDO.
DEFINE VARIABLE n-colunas          AS INTEGER       NO-UNDO.
DEFINE VARIABLE i-QtyPai           AS INTEGER       NO-UNDO.
DEFINE VARIABLE cIdent             AS CHARACTER     NO-UNDO.
DEFINE VARIABLE l-expand           AS LOG           NO-UNDO.
DEFINE VARIABLE h-acomp            AS HANDLE        NO-UNDO.
DEFINE VARIABLE i-coluna           AS INTEGER       NO-UNDO.
/** variaveis tipo contadoras **/
DEFINE VARIABLE i-linha          AS INTEGER INIT 5  NO-UNDO.
DEFINE VARIABLE i-countCol       AS INTEGER         NO-UNDO.
DEFINE VARIABLE i-countLinhaCor  AS INTEGER         NO-UNDO. 
DEFINE VARIABLE i-loopComponente AS INTEGER         NO-UNDO.
DEFINE VARIABLE i-seq            AS INTEGER         NO-UNDO.
/** componentes Excel **/
DEFINE VARIABLE ch-Excel    AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE ch-Planilha AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE ch-Arquivo  AS COM-HANDLE  NO-UNDO.

DEFINE TEMP-TABLE ttComponente NO-UNDO
    FIELD posicao        AS INTEGER
    FIELD cod-componente AS CHAR
    FIELD sequencia      AS INT
    FIELD descricao      AS CHAR
    FIELD cor-fundo      AS INT
    FIELD cor-fonte      AS INT
    FIELD l-negrito      AS LOG
    INDEX ch-posicao     AS PRIMARY UNIQUE posicao
    INDEX ch-dimensao    sequencia.

DEFINE TEMP-TABLE ttColunaComponente NO-UNDO
    FIELD cod-componente AS CHAR
    FIELD valor          AS CHAR
    FIELD posicao        AS INT
    FIELD titulo-coluna  AS CHAR
    FIELD tipo           AS CHAR
    FIELD cor-fundo      AS INT
    FIELD cor-fonte      AS INT
    FIELD l-negrito      AS LOG
    INDEX ch-auxiliar cod-componente posicao.

DEFINE TEMP-TABLE tt-estrut-tag NO-UNDO
    FIELD cod-tipo       LIKE tag.cod-tipo
    FIELD cd-tag         LIKE tag.cd-tag
    FIELD descricao      LIKE tag.descricao
    FIELD sequencia      AS INTEGER
    FIELD ordem          AS INTEGER 
    INDEX ch-principal ordem
    INDEX ch-sequencia sequencia.

DEF TEMP-TABLE tt-tag-equipto NO-UNDO
    FIELD id            AS INT
    FIELD cd-codigo     AS CHAR
    FIELD descricao     AS CHAR
    FIELD r-rowid       AS ROWID /* rowid do Tag ou Equipamento para detalhe*/
    FIELD i-tipo        AS INT   /* 1 - Tag segundo nivel , 2 - Tag terceiro nivel ou +, 3 - Equipamento */
    FIELD id-pai        AS INT 
    FIELD cod-imagem    AS CHAR
    INDEX cd-codigo cd-codigo.

DEF BUFFER bf-tt-estrut-tag FOR tt-estrut-tag.
DEF BUFFER bf-tag           FOR tag.
DEF BUFFER bf-estr-tag      FOR estr-tag.
DEF BUFFER bfttComponente   FOR ttComponente.

DEFINE VARIABLE v-arqv-destino   AS CHARACTER         NO-UNDO.
DEFINE VARIABLE c-alfabeto       AS CHAR init "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,as" NO-UNDO.
define variable i-lin          as integer                  no-undo.
define variable i-col          as integer                  no-undo.
DEFINE VARIABLE h-utils          as handle            NO-UNDO.
DEFINE VARIABLE chWorkbook       AS COM-HANDLE.
DEFINE VARIABLE chWorksheet      AS COM-HANDLE.

/*** Esta include no ems 2.01 nÆo dever  possuir nenhum c¢digo, serve 
     apenas para desenvolvimento tratar o conceito de miniflexibiliza‡Æo.
     Utilizado apenas para MANUTEN€ÇO INDUSTRIAL. ***/

/* Fun‡äes EMS 2.03 - Manuten‡Æo Industrial        *//* Fun‡äes EMS 2.04 - Manuten‡Æo Industrial        *//* Modulo de Calibra‡Æo                    */
/*** EMS 2.05 ***/
/* Planejamento da MÆo-de-obra (Agenda do T‚cnico) *//* Altera‡äes Gerais                               */
/*** RELEASE 2.06 ***/
/* Altera‡äes Gerais EMS 2.06                      */
/*** RELEASE 2.06B ***/
/* Altera‡äes Gerais EMS 2.06B                     */ 
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
   /** Defini‡Æo da temp-table de parƒmetros **/

/** Classifica‡Æo **/
DEF TEMP-TABLE ttVisao NO-UNDO
    FIELD dimensao  AS CHARACTER
    FIELD sequencia AS INTEGER
    INDEX codigo IS PRIMARY UNIQUE sequencia.

/** Tabelas auxiliares **/
DEF TEMP-TABLE ttAux
    FIELD ep-codigo           LIKE mab-eqpto.ep-codigo
    FIELD cod-eqpto           LIKE mab-eqpto.cod-eqpto
    FIELD cod-modelo          LIKE mab-eqpto.cod-model
    FIELD descricao           LIKE mab-model.des-model
    FIELD cod-estabel         LIKE mab-eqpto.cod-estabel
    FIELD cd-tag              LIKE mab-eqpto.cd-tag
    FIELD nr-inter-corre      AS DECIMAL
    FIELD nr-corre-fin        AS DECIMAL
    FIELD hr-manut-corre-fin  AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD hr-manut-prev       AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD hr-manut-corre      AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD hr-manut-pred       AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD hr-manut-corre-plan AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD nr-dias             AS INTEGER 
    FIELD mat                 AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD ggf                 AS DECIMAL FORMAT ">>>>,>>>,>>9.9999"
    FIELD serv                AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD contratos           AS DECIMAL
    FIELD hr-op-eqpto         LIKE mab-movto-km-eqpto.val-hodom-horim
    FIELD hr-op-tag           LIKE mab-movto-km-eqpto.val-hodom-horim
    FIELD fator-conver        AS DECIMAL FORMAT "->>,>>9.99".

/** Dados **/
DEF TEMP-TABLE ttDados NO-UNDO
    FIELD sequencia           AS INTEGER
    FIELD cod-dimensao        AS CHARACTER FORMAT "x(300)"
    FIELD cod-oficial         AS CHARACTER FORMAT "X(16)"
    FIELD desc-dimensao       AS CHARACTER FORMAT "x(32)"
    FIELD cod-dimens-pai      AS CHARACTER FORMAT "x(300)"
    FIELD mtbf                AS DECIMAL   FORMAT "->>>,>>>,>>9.99"   label "MTBF"
    FIELD mttr                AS DECIMAL   FORMAT "->>>,>>>,>>9.99"   label "MTTR"
    FIELD dispo               AS DECIMAL   FORMAT ">>9.99"   label "Disponibilidade"
    FIELD pmpl                AS DECIMAL   FORMAT "->>>,>>>,>>9.99"   label "PMPL"
    FIELD mat                 AS DECIMAL   FORMAT "->>>,>>>,>>9.99"   label "Material"
    FIELD ggf                 AS DECIMAL   FORMAT ">>>>,>>>,>>9.9999" label "MÆo de Obra"
    FIELD serv                AS DECIMAL   FORMAT "->>>,>>>,>>9.99"   label "Servi‡os"
    FIELD contratos           AS DECIMAL   FORMAT "->>>,>>>,>>9.99"   label "Contratos"
    FIELD custo               AS DECIMAL   FORMAT "->>>,>>>,>>9.99"   label "Custo"
    FIELD tot                 AS DECIMAL   FORMAT "->>>,>>>,>>9.99"   label "Total"
    FIELD r-rowid             AS ROWID
    FIELD p-image             AS INTEGER
    FIELD seq-tree            AS INTEGER
    FIELD nr-inter-corre      AS DECIMAL                                                  
    FIELD nr-corre-fin        AS DECIMAL                         
    FIELD hr-manut-corre-fin  AS DECIMAL FORMAT "->>>,>>>,>>9.99"                         
    FIELD hr-manut-prev       AS DECIMAL FORMAT "->>>,>>>,>>9.99" 
    FIELD hr-manut-corre      AS DECIMAL FORMAT "->>>,>>>,>>9.99" 
    FIELD hr-manut-pred       AS DECIMAL FORMAT "->>>,>>>,>>9.99" 
    FIELD hr-manut-corre-plan AS DECIMAL FORMAT "->>>,>>>,>>9.99" 
    FIELD hr-manut            AS DECIMAL
    FIELD hr-op-eqpto         AS DECIMAL FORMAT "->>>,>>>,>>9.99" 
    FIELD cod-eqpto           LIKE mab-eqpto.cod-eqpto
    FIELD cd-tag              LIKE equipto.cd-tag
    FIELD nr-ord-produ        LIKE ord-manut.nr-ord-produ
    FIELD des-man-corr        LIKE ord-manut.des-man-corr
    FIELD estado              LIKE ord-manut.estado-om       
    FIELD dt-manut            LIKE ord-manut.dt-manut    
    FIELD dt-fecham           LIKE ord-manut.dt-fecham
    FIELD indicador           AS LOGICAL INITIAL YES /** para definir a cor no browse **/
    INDEX id IS PRIMARY UNIQUE sequencia    ASCENDING
                               cod-dimensao ASCENDING.

DEF TEMP-TABLE tt-ord-manut NO-UNDO
    FIELD nr-ord-produ LIKE ord-manut.nr-ord-produ
    FIELD des-man-corr LIKE ord-manut.des-man-corr
    FIELD estado       LIKE ord-manut.estado-om
    FIELD cod-eqpto    LIKE ord-manut.cd-equipto
    FIELD dt-manut     LIKE ord-manut.dt-manut
    FIELD dt-fecham    LIKE ord-manut.dt-fecha
    FIELD indicador    AS LOGICAL
    INDEX codigo IS PRIMARY UNIQUE cod-eqpto nr-ord-produ.

DEF TEMP-TABLE tt-disp NO-UNDO
    FIELD ep-codigo       LIKE mab-eqpto.ep-codigo 
    FIELD cod-eqpto       LIKE mab-eqpto.cod-eqpto
    FIELD descricao       LIKE mab-model.des-model
    FIELD dt-trans        LIKE movto-ggf.dt-trans
    FIELD hr-manut        AS DECIMAL FORMAT ">>9.99"
    FIELD disponibilidade AS DECIMAL FORMAT ">>9.99"
    FIELD cd-tag          LIKE mab-eqpto.cd-tag.

DEF TEMP-TABLE tt-tag NO-UNDO
    FIELD ep-codigo       LIKE mab-eqpto.ep-codigo 
    FIELD cod-eqpto       LIKE mab-eqpto.cod-eqpto
    FIELD descricao       LIKE mab-model.des-model
    FIELD cd-tag          LIKE equipto.cd-tag
    FIELD dt-trans        LIKE movto-ggf.dt-trans
    FIELD hr-manut        AS DECIMAL FORMAT ">>9.99"
    FIELD disponibilidade AS DECIMAL FORMAT ">>9.99".

DEF TEMP-TABLE tt-eqpto NO-UNDO
    FIELD cod-eqpto  LIKE mab-eqpto.cod-eqpto
    FIELD cd-tag     LIKE mab-eqpto.cd-tag
    FIELD descricao  LIKE mab-model.des-model
    FIELD dispo-acum AS DECIMAL FORMAT ">>9.99"
    INDEX id IS PRIMARY UNIQUE cod-eqpto.

DEF TEMP-TABLE tt-pai NO-UNDO
    FIELD codigo     LIKE mab-eqpto.cd-tag
    FIELD descricao  LIKE mab-model.des-model
    FIELD dispo-acum AS DECIMAL FORMAT ">>9.99"
    INDEX id IS PRIMARY UNIQUE codigo.

DEFINE TEMP-TABLE tt-horas NO-UNDO
    FIELD hra-inicial LIKE mmv-movto-mdo.hra-inicial
    FIELD hra-final   LIKE mmv-movto-mdo.hra-final
    FIELD cod-model   LIKE mab-eqpto.cod-model
    FIELD dat-movto   LIKE mmv-movto-mdo.dat-movto
    FIELD primeiro    AS LOGICAL
    FIELD ep-codigo   LIKE mab-eqpto.ep-codigo
    FIELD cod-eqpto   LIKE mab-eqpto.cod-eqpto.

DEFINE TEMP-TABLE tt-apont NO-UNDO
    FIELD hra-inicial LIKE mmv-movto-mdo.hra-inicial
    FIELD hra-final   LIKE mmv-movto-mdo.hra-final
    FIELD cod-model   LIKE mab-eqpto.cod-model
    FIELD dat-ini     LIKE mmv-movto-mdo.dat-movto
    FIELD dat-fim     LIKE mmv-movto-mdo.dat-movto
    FIELD ep-codigo   LIKE mab-eqpto.ep-codigo
    FIELD cod-eqpto   LIKE mab-eqpto.cod-eqpto
    FIELD parada      AS LOGICAL.

DEFINE TEMP-TABLE ttModelo NO-UNDO
    FIELD cod-model             LIKE mab-eqpto.cod-model
    FIELD cod-tag-nivel-usuar   LIKE mab-eqpto.cod-model
    FIELD descricao             LIKE mab-model.des-model
    FIELD quant                 AS INTEGER
    .

DEFINE TEMP-TABLE tt-horas-aux NO-UNDO LIKE tt-horas.

DEFINE BUFFER bfttDados  FOR ttDados.
DEFINE BUFFER bfttDados2 FOR ttDados.
DEFINE BUFFER bfttAux    FOR ttAux.
DEFINE BUFFER bf-tt-disp FOR tt-disp.
DEFINE BUFFER bf-tt-tag  FOR tt-tag.
DEFINE BUFFER bf-ttHoras FOR tt-horas-aux.
DEFINE BUFFER bf-mab-movto-km-eqpto FOR mab-movto-km-eqpto.

{utp/utapi013.i}

/** Vari veis aux¡liares para armazenar o TAG inicial e final da estrutura **/
DEFINE VARIABLE cTagIni LIKE tag.cd-tag  NO-UNDO.
DEFINE VARIABLE cTagFim LIKE tag.cd-tag  NO-UNDO.
DEFINE VARIABLE lPrimeiroValorTag AS LOGICAL NO-UNDO.
DEFINE VARIABLE lMediaTagModelo   AS LOGICAL NO-UNDO. /** Para quando criar a ttDados, verificar a primeira vez que ela est  sendo criada, calculando assim a m‚dia certa **/
                                                      /** para o modelo e o tag **/

DEFINE TEMP-TABLE tt-om-leitura NO-UNDO
    FIELD nr-ord-produ LIKE ord-manut.nr-ord-produ.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */


/* Name of designated FRAME-NAME and/or first browse and/or first query */

/* Internal Tables (found by Frame, Query & Browse Queries)             */

/* Definitions for BROWSE brDetalhe                                     */


/* Definitions for FRAME fpage0                                         */

/* Standard List Definitions                                            */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */



/* ************************  Function Prototypes ********************** */


FUNCTION fnBrowse RETURNS CHARACTER
  ( pCampo as character,
    iTipo  as integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */



FUNCTION fnEstado RETURNS CHARACTER
  ( pEstado as integer /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */



FUNCTION fnLabels RETURNS CHARACTER
  ( pImage as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */



FUNCTION FormataDados RETURNS LOGICAL
  (pRange as char,pCor as int,pCorFundo as int,pFonte as int,pValue as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU miArquivo 
       MENU-ITEM miAbrir        LABEL "&Abrir"         ACCELERATOR "CTRL-A"
       MENU-ITEM miSalvar       LABEL "&Salvar"        ACCELERATOR "CTRL-S"
       MENU-ITEM miAtualizar    LABEL "At&ualizar"     ACCELERATOR "CTRL-R"
       MENU-ITEM miExpande      LABEL "&Expande/Contrai" ACCELERATOR "CTRL-E"
       RULE
       MENU-ITEM miQtdInterv    LABEL "&Qtd Interven‡äes" ACCELERATOR "CTRL-Q"
       MENU-ITEM miSelecao      LABEL "Se&le‡Æo"       ACCELERATOR "CTRL-P"
       MENU-ITEM miFiltro       LABEL "&Filtro"        ACCELERATOR "CTRL-F"
       MENU-ITEM miClassifica   LABEL "&Classifica"    ACCELERATOR "CTRL-C"
       RULE
       MENU-ITEM miGrafico      LABEL "&Gr fico"       ACCELERATOR "CTRL-G"
       MENU-ITEM miImprimir     LABEL "&Imprimir"      ACCELERATOR "CTRL-J"
       MENU-ITEM miExcel        LABEL "E&xcel"         ACCELERATOR "CTRL-L"
       MENU-ITEM miTxt          LABEL "&Texto"         ACCELERATOR "CTRL-T"
       RULE
       MENU-ITEM miQueryJoins   LABEL "C&onsultas"    
       MENU-ITEM miReportsJoins LABEL "Rela&t¢rios"   
       RULE
       MENU-ITEM miExit         LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU smHelp 
       MENU-ITEM miContents     LABEL "&Conte£do"     
       MENU-ITEM miAbout        LABEL "&Sobre..."     .

DEFINE MENU mbMain MENUBAR
       SUB-MENU  miArquivo      LABEL "&Arquivo"      
       SUB-MENU  smHelp         LABEL "Aj&uda"        .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt0603 
     IMAGE-UP FILE "adeicon/browse-u.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.25.

DEFINE BUTTON btAbrir 
     IMAGE-UP FILE "image\im-open":U
     IMAGE-INSENSITIVE FILE "image\ii-open":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Abrir"
     FONT 4.

DEFINE BUTTON btEx 
     IMAGE-UP FILE "image\excel":U
     LABEL "teste" 
     SIZE 4 BY 1.25.

DEFINE BUTTON btExit 
     IMAGE-UP FILE "image/im-exi":U
     IMAGE-INSENSITIVE FILE "image/ii-exi":U
     LABEL "Exit" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btExpande 
     IMAGE-UP FILE "image/im-crit":U
     IMAGE-INSENSITIVE FILE "image/ii-crit":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Expande / Contrai"
     FONT 4.

DEFINE BUTTON btFiltro 
     IMAGE-UP FILE "image/im-fil":U
     IMAGE-INSENSITIVE FILE "image/ii-fil":U
     LABEL "Filtro" 
     SIZE 4 BY 1.25 TOOLTIP "Filtro"
     FONT 4.

DEFINE BUTTON btHelp 
     IMAGE-UP FILE "image/im-hel":U
     IMAGE-INSENSITIVE FILE "image/ii-hel":U
     LABEL "Help" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btParam 
     IMAGE-UP FILE "image/im-param":U
     IMAGE-INSENSITIVE FILE "image/ii-param":U
     LABEL "Parƒmetros" 
     SIZE 4 BY 1.25 TOOLTIP "Parƒmetros"
     FONT 4.

DEFINE BUTTON btQueryJoins 
     IMAGE-UP FILE "image/im-joi":U
     IMAGE-INSENSITIVE FILE "image/ii-joi":U
     LABEL "Query Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btRedimensiona 
     LABEL "" 
     SIZE 90 BY .29.

DEFINE BUTTON btRefresh 
     IMAGE-UP FILE "image/im-sav":U
     IMAGE-INSENSITIVE FILE "image/ii-sav":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Atualizar"
     FONT 4.

DEFINE BUTTON btReportsJoins 
     IMAGE-UP FILE "image/im-pri":U
     IMAGE-INSENSITIVE FILE "image/ii-pri":U
     LABEL "Reports Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btSalvar 
     IMAGE-UP FILE "image/im-grava":U
     IMAGE-INSENSITIVE FILE "image/ii-grava":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Salvar"
     FONT 4.

DEFINE BUTTON btVisao 
     IMAGE-UP FILE "image/im-aloca":U
     IMAGE-INSENSITIVE FILE "image/im-aloci":U
     LABEL "Classificar" 
     SIZE 4 BY 1.25 TOOLTIP "Classifica"
     FONT 4.

DEFINE RECTANGLE rtToolBar-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 90 BY 1.5
     BGCOLOR 7 .

/* Query definitions                                                    */

DEFINE QUERY brDetalhe FOR 
      ttDados SCROLLING.


/* Browse definitions                                                   */
DEFINE BROWSE brDetalhe

  QUERY brDetalhe NO-LOCK DISPLAY
      ttDados.cod-oficial      width 14
ttDados.desc-dimensao    width 30
ttDados.nr-ord-produ     width 15
ttDados.des-man-corr     width 25
fnEstado(ttDados.estado) @ cEstado width 10
ttDados.cod-eqpto        width 20
ttDados.dt-manut         width 15
ttDados.dt-fecham        width 13
/* _UIB-CODE-BLOCK-END */

    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 8.25
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btAbrir AT ROW 1.13 COL 2
     btSalvar AT ROW 1.13 COL 6
     btRefresh AT ROW 1.13 COL 10
     btExpande AT ROW 1.13 COL 16
     btVisao AT ROW 1.13 COL 34 HELP
          "Classifica"
     btParam AT ROW 1.13 COL 43 HELP
          "Parƒmetros"
     btFiltro AT ROW 1.13 COL 47 HELP
          "Filtro"
     btEx AT ROW 1.13 COL 51
     bt0603 AT ROW 1.13 COL 55
     btQueryJoins AT ROW 1.13 COL 74.86 HELP
          "Consultas relacionadas"
     btReportsJoins AT ROW 1.13 COL 78.86 HELP
          "Relat¢rios relacionados"
     btExit AT ROW 1.13 COL 82.86 HELP
          "Sair"
     btHelp AT ROW 1.13 COL 86.86 HELP
          "Ajuda"
     btRedimensiona AT ROW 11 COL 1
     brDetalhe AT ROW 11.25 COL 1
     rtToolBar-2 AT ROW 1 COL 1
     SPACE(0.00) SKIP(8.46)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
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
         HEIGHT             = 18.13
         WIDTH              = 90.14
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
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

ASSIGN wWindow:MENUBAR    = MENU mbMain:HANDLE.
/* END WINDOW DEFINITION                                                */



/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}
{btb/btb008za.i0}
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
    
        DISPLAY btQueryJoins btReportsJoins btExit btHelp                               btAbrir btSalvar btRefresh btFiltro btParam btVisao btExpande                               btRedimensiona brDetalhe btEx bt0603 WITH FRAME fPage0.
    
    
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
    
        ENABLE btQueryJoins btReportsJoins btExit btHelp                               btAbrir btSalvar btRefresh btFiltro btParam btVisao btExpande                               btRedimensiona brDetalhe btEx bt0603 WITH FRAME fPage0.
    
    
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
    ASSIGN c-programa-mg97 = CAPS("ESMV0616":U)
           c-versao-mg97   = "2.00.00.016":U.
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
   FRAME-NAME Size-to-Fit                                               */
/* BROWSE-TAB brDetalhe btRedimensiona fpage0 */
ASSIGN 
       FRAME fpage0:SCROLLABLE       = FALSE.

ASSIGN 
       brDetalhe:COLUMN-RESIZABLE IN FRAME fpage0       = TRUE
       brDetalhe:COLUMN-MOVABLE IN FRAME fpage0         = TRUE.

ASSIGN 
       btRedimensiona:MOVABLE IN FRAME fpage0          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */



/* Setting information for Queries and Browse Widgets fields            */


/* Query rebuild information for BROWSE brDetalhe
     _START_FREEFORM
open query {&SELF-NAME} for each ttDados no-lock.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE brDetalhe */



/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */


 


/* **********************  Create OCX Containers  ********************** */





CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fpage0:HANDLE
       ROW             = 2.5
       COLUMN          = 1
       HEIGHT          = 8.46
       WIDTH           = 90
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME fpage0:HANDLE
       ROW             = 8.5
       COLUMN          = 76
       HEIGHT          = 1.58
       WIDTH           = 5.43
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {11334785-F3D7-4E44-8007-DFD19CB08003} type: TreeList */
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {2C247F23-8591-11D1-B16A-00C0F0283628} type: ImageList */
      CtrlFrame:MOVE-AFTER(btHelp:HANDLE IN FRAME fpage0).
      CtrlFrame-2:MOVE-AFTER(CtrlFrame).



/* End of _CREATE-DYNAMIC */


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




ON WINDOW-MAXIMIZED OF wWindow
DO:
    def var dXC    as dec    no-undo.
    def var dYC    as dec    no-undo.
    DEF VAR hlabel AS HANDLE NO-UNDO.
     
  
    assign /**--------------------------------- Tamanho da tela ----------------------------------------------**/
           dXC                                     = wWindow:width-chars  - dWinXC
           dYC                                     = wWindow:height-chars - dWinYC
           dWinXC                                  = wWindow:width-chars
           dWinYC                                  = wWindow:HEIGHT-CHARS
           /**-------------------------------- Tamanho dos frames --------------------------------------------**/
           frame fPage0:width-chars                = wWindow:width-chars
           frame fPage0:height-chars               = wWindow:height-chars
           /**--------------------------------- Objetos em tela ----------------------------------------------**/
           rtToolBar-2:width-chars in frame fPage0    = (rtToolBar-2:width-chars in frame fPage0 + dXC)
           btHelp:column in frame fPage0              = (btHelp:column in frame fPage0 + dXC)
           btExit:column in frame fPage0              = (btExit:column in frame fPage0 + dXC)
           btReportsJoins:column in frame fPage0      = (btReportsJoins:column in frame fPage0 + dXC)
           btQueryJoins:column in frame fPage0        = (btQueryJoins:column in frame fPage0 + dXC)
           btParam:column in frame fPage0             = (btParam:column in frame fPage0 + dXC)
           btFiltro:column in frame fPage0            = (btFiltro:column in frame fPage0 + dXC)
           btVisao:column in frame fPage0             = (btVisao:column in frame fPage0 + dXC)
           btEx:column in frame fPage0                = (btEx:column in frame fPage0 + dXC)
           bt0603:column in frame fPage0              = (bt0603:column in frame fPage0 + dXC)
           brDetalhe:width-chars in frame fPage0      = (brDetalhe:width-chars in frame fPage0 + dXC)
           btRedimensiona:width-chars in frame fPage0 = (btRedimensiona:width-chars in frame fPage0 + dXC)
           btRedimensiona:row in frame fPage0         = (btRedimensiona:row in frame fPage0 + (dYC / 2))
           /**---------------------------------- OCXs --------------------------------------------------------**/
           CtrlFrame:width                         = CtrlFrame:width   + dXC 
           CtrlFrame:HEIGHT                        = CtrlFrame:HEIGHT  + dYC NO-ERROR.

    apply "END-MOVE":U to btRedimensiona in frame fPage0.
          
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON WINDOW-RESTORED OF wWindow
DO:
    def var dXC    as dec    no-undo.
    def var dYC    as dec    no-undo.
    DEF VAR hlabel AS HANDLE NO-UNDO.
   

    assign /**--------------------------------- Tamanho da tela ----------------------------------------------**/
           dXC                                     = dWinXC - wWindow:width-chars
           dYC                                     = dWinYC - wWindow:height-chars
           dWinXC                                  = wWindow:width-chars
           dWinYC                                  = wWindow:HEIGHT-CHARS
           /**--------------------------------- Objetos em tela ----------------------------------------------**/
           rtToolBar-2:width-chars in frame fPage0    = (rtToolBar-2:width-chars in frame fPage0 - dXC)
           btHelp:column in frame fPage0              = (btHelp:column in frame fPage0 - dXC)
           btExit:column in frame fPage0              = (btExit:column in frame fPage0 - dXC)
           btReportsJoins:column in frame fPage0      = (btReportsJoins:column in frame fPage0 - dXC)
           btQueryJoins:column in frame fPage0        = (btQueryJoins:column in frame fPage0 - dXC)
           btParam:column in frame fPage0             = (btParam:column in frame fPage0 - dXC)
           btFiltro:column in frame fPage0            = (btFiltro:column in frame fPage0 - dXC)
           btVisao:column in frame fPage0             = (btVisao:column in frame fPage0 - dXC)
           btEx:column in frame fPage0                = (btEx:column in frame fPage0 - dXC)
           bt0603:column in frame fPage0              = (bt0603:column in frame fPage0 - dXC)
           brDetalhe:width-chars in frame fPage0      = (brDetalhe:width-chars in frame fPage0 - dXC)
           btRedimensiona:width-chars in frame fPage0 = (btRedimensiona:width-chars in frame fPage0 - dXC)
           btRedimensiona:row in frame fPage0         = 11
           /**---------------------------------- OCXs --------------------------------------------------------**/
           CtrlFrame:width                         = CtrlFrame:width  - dXC
           /**-------------------------------- Tamanho dos frames --------------------------------------------**/
           frame fPage0:width-chars                = wWindow:width-chars.
           frame fPage0:height-chars               = wWindow:HEIGHT-CHARS   NO-ERROR.

    apply "END-MOVE":U to btRedimensiona in frame fPage0.

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON ROW-DISPLAY OF brDetalhe IN FRAME fpage0
DO:
  IF ttDados.sequencia = 3 AND ttDados.indicador = NO THEN DO:
      IF ttDados.indicador = NO THEN
          ASSIGN ttDados.cod-oficial:FGCOLOR    IN BROWSE brDetalhe = 12     
                 ttDados.desc-dimensao:FGCOLOR  IN BROWSE brDetalhe = 12   
                 ttDados.nr-ord-produ:FGCOLOR   IN BROWSE brDetalhe = 12    
                 ttDados.des-man-corr:FGCOLOR   IN BROWSE brDetalhe = 12    
                 cEstado:FGCOLOR                IN BROWSE brDetalhe = 12
                 ttDados.cod-eqpto:FGCOLOR      IN BROWSE brDetalhe = 12       
                 ttDados.dt-manut:FGCOLOR       IN BROWSE brDetalhe = 12        
                 ttDados.dt-fecham:FGCOLOR      IN BROWSE brDetalhe = 12.
  END.
END.

/* _UIB-CODE-BLOCK-END */




ON ROW-ENTRY OF brDetalhe IN FRAME fpage0
DO:
  APPLY "value-changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt0603 IN FRAME fpage0 /* Button 1 */
DO:
    DEFINE VARIABLE hProgram AS HANDLE NO-UNDO.
    DEFINE VARIABLE cdTag AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lPrimeiroTag AS LOGICAL    NO-UNDO.


    if num-results("brDetalhe") > 0 then do:
        IF ttSelecao.iIndicador = 1 THEN DO:
            run mvp/esmv0603.w persistent set hProgram.
            if valid-handle(hProgram) and
                    hProgram:type = "PROCEDURE":U and
                    hProgram:file-name = "mvp/esmv0603.w":U then do:

                run initializeInterface in hProgram.
                
                IF ttDados.sequencia = 2 THEN DO:
                   RUN piOverSelecaoModelo IN hProgram (INPUT SUBSTRING(ttDados.cod-dimens-pai,2,8),
                                                        INPUT ttSelecao.periodo-ini,
                                                        INPUT ttSelecao.periodo-fim, 
                                                        INPUT ttSelecao.iTipoDispo).
                END.
                ELSE DO:
                    run piOverSelecao       in hProgram (INPUT ttDados.cod-eqpto,
                                                         INPUT ttSelecao.periodo-ini,
                                                         INPUT ttSelecao.periodo-fim,
                                                         INPUT ttSelecao.iTipoDispo).
                END.
            END.
        END.
        ELSE DO:
            IF bfttDados.sequencia = 1 THEN DO:
                EMPTY TEMP-TABLE tt-tag-equipto.
                run mip/esmi0603.w persistent set hProgram.
                if valid-handle(hProgram) and
                        hProgram:type = "PROCEDURE":U and
                        hProgram:file-name = "mip/esmi0603.w":U then do:

                    IF ttSelecao.iNivTag = 999 THEN DO:
                        ASSIGN cTagIni = bfttDados.cd-tag
                               cTagFim = bfttDados.cd-tag.
                    END.
                    ELSE DO:
                        ASSIGN lPrimeiroTag = YES.
                        run retornatt-tag-equipto in hDBOTag (input bfttDados.cd-tag,
                                                              input NO,
                                                              output table tt-tag-equipto).

                        FOR EACH tt-tag-equipto
                            WHERE tt-tag-equipto.i-tipo = 1:

                            IF lPrimeiroTag = NO THEN DO:
                                IF cTagIni > tt-tag-equipto.cd-codigo THEN
                                    ASSIGN cTagIni = tt-tag-equipto.cd-codigo.
                                IF cTagFim < tt-tag-equipto.cd-codigo THEN
                                    ASSIGN cTagfim = tt-tag-equipto.cd-codigo.
                            END.
                            ELSE DO:
                                ASSIGN cTagIni = tt-tag-equipto.cd-codigo
                                       cTagFim = tt-tag-equipto.cd-codigo
                                       lPrimeiroTag = NO.
                            END.
                        END.
                    END.

                    run initializeInterface in hProgram.
                    run piOverSelecao       in hProgram (INPUT cTagIni,
                                                         INPUT cTagFim,
                                                         INPUT ttSelecao.periodo-ini,
                                                         INPUT ttSelecao.periodo-fim,
                                                         INPUT ttSelecao.iNivTag,       
                                                         INPUT ttSelecao.iTipoDispo).
                end.
            END.
            
        END.
        
    end.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btAbrir IN FRAME fpage0
OR CHOOSE OF MENU-ITEM miAbrir  IN MENU mbMain DO:

   /** Troca as barras **/
   assign c-arquivo = replace(c-arquivo, "/", "~\").
   /** Abre caixa para abrir arquivo **/
   SYSTEM-DIALOG GET-FILE c-arquivo
       FILTERS "*.dat" "*.dat",
               "*.*" "*.*"
       ASK-OVERWRITE
       DEFAULT-EXTENSION "dat"
       INITIAL-DIR entry (1, propath)
       USE-FILENAME
       UPDATE l-ok.
   /** Retorno de OK, abre o arquivo **/
   if l-ok then do:
      if search (c-arquivo) <> ? then
         run carregaFiltro (input c-arquivo).
   end.

   apply "ENTRY":U to btAbrir in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btEx IN FRAME fpage0 /* teste */
DO:
    if num-results("brDetalhe") > 0 then
        run piExcel in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btExit IN FRAME fpage0 /* Exit */
OR CHOOSE OF MENU-ITEM miExit IN MENU mbMain DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btExpande IN FRAME fpage0
OR CHOOSE OF MENU-ITEM miExpande  IN MENU mbMain DO:
    /** Verifica se existem dados no tree-view **/
    if chTreeList:nodes:Count > 0 then do:
        assign chTreeList:visible = no.
        /** Controla expansÆo **/
        assign l-expande = not l-expande.
        /** Expande os n¡veis abaixo **/
        do i-cont = 1 to chTreeList:Nodes:Count:
           assign chTreeList:Nodes(i-cont):Expanded = l-expande.
        end.
        assign chTreeList:visible = yes.
    end.
  
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btFiltro IN FRAME fpage0 /* Filtro */
OR CHOOSE OF MENU-ITEM miFiltro  IN MENU mbMain DO:

  assign wWindow:sensitive = no.
  run mvp/ESMV0616a.w (input-output  table ttSelecao).
  assign wWindow:sensitive = yes.
  apply "ENTRY":U to btFiltro in frame fPage0.
  find first ttSelecao no-lock no-error.
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




ON CHOOSE OF btParam IN FRAME fpage0 /* Parƒmetros */
OR CHOOSE OF MENU-ITEM miSelecao  IN MENU mbMain DO:
    
  assign wWindow:sensitive = no.
  run mvp/ESMV0616c.w (input-output  table ttSelecao).
  assign wWindow:sensitive = yes.
  apply "ENTRY":U to btParam in frame fPage0.
  find first ttSelecao no-lock no-error.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btQueryJoins IN FRAME fpage0 /* Query Joins */
OR CHOOSE OF MENU-ITEM miQueryJoins IN MENU mbMain DO:
    RUN showQueryJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON END-MOVE OF btRedimensiona IN FRAME fpage0
DO:
  
    if btRedimensiona:row < 3.5 then
     assign btRedimensiona:row = 3.5.
     
    if btRedimensiona:row > frame fPage0:height then
       assign btRedimensiona:row = frame fPage0:height - 1.
    
    assign CtrlFrame:height = btRedimensiona:row - 2.5 no-error.         
    chTreelist:refresh().

    assign brDetalhe:row = btRedimensiona:row + btRedimensiona:height no-error.
    assign brDetalhe:height  = frame fPage0:height + 0.5
                                 - (btRedimensiona:row) no-error.
  
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btRefresh IN FRAME fpage0
OR CHOOSE OF MENU-ITEM miAtualizar IN MENU mbMain DO:

/** Controla expansÆo **/
if l-expande then assign l-expande = no.

session:set-wait-state ("GENERAL").
/** Frotas **/
if ttSelecao.iIndicador = 1 then   
    run piAtualizar in this-procedure.   
else
    /** MI **/
    run piAtualizarMI in this-procedure.
session:set-wait-state ("").

APPLY "ENTRY":U TO CtrlFrame.

END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btReportsJoins IN FRAME fpage0 /* Reports Joins */
OR CHOOSE OF MENU-ITEM miReportsJoins IN MENU mbMain DO:
    RUN showReportsJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btSalvar IN FRAME fpage0
OR CHOOSE OF MENU-ITEM miSalvar  IN MENU mbMain DO:
   /** Troca os tipos de barras **/ 
   assign c-arquivo = replace(c-arquivo, "/", "~\").

   /** Abre caixa de escolha de arquivos **/
   SYSTEM-DIALOG GET-FILE c-arquivo
       FILTERS "*.dat" "*.dat",
               "*.*" "*.*"
       ASK-OVERWRITE
       DEFAULT-EXTENSION "dat"
       INITIAL-DIR entry (1, propath)
       SAVE-AS
       USE-FILENAME
       UPDATE l-ok.
   /** Retorno de OK **/
   if l-ok then do:
      output to value (c-arquivo).
      /** Salva Sele‡Æo **/
      for first ttSelecao:
          export delimiter ";"
                 ttSelecao.periodo-ini
                 ttSelecao.periodo-fim
                 ttSelecao.empresa-ini
                 ttSelecao.empresa-fim
                 ttSelecao.equipto-ini
                 ttSelecao.equipto-fim
                 ttSelecao.estab-ini
                 ttSelecao.estab-fim
                 ttSelecao.grupo-ini
                 ttSelecao.grupo-fim
                 ttSelecao.modelo-ini
                 ttSelecao.modelo-fim
                 ttSelecao.estrut-ini
                 ttSelecao.estrut-fim
                 ttSelecao.tag-ini
                 ttSelecao.tag-fim
                 ttSelecao.cc-ini
                 ttSelecao.cc-fim
                 ttSelecao.ano-fabric-ini
                 ttSelecao.ano-fabric-fim
                 ttSelecao.lMtbf
                 ttSelecao.lMttr
                 ttSelecao.lDispo
                 ttSelecao.lPmpl
                 ttSelecao.iIndicador
                 ttSelecao.lMat
                 ttSelecao.lGGF
                 ttSelecao.lServ
                 ttSelecao.lContratos
                 ttSelecao.lCusto
                 ttSelecao.lTotal
                 ttSelecao.lMatMesAnt
                 ttSelecao.iNivTag
                 ttSelecao.dt-trans-ini 
                 ttSelecao.dt-trans-fim
              .
      end.
      /** Salva Visäes **/
      for each ttVisao:
          export delimiter ";"
                 ttVisao.dimensao
                 ttVisao.sequencia.
      end.
      output close.
   end.
  
   apply "ENTRY":U to btSalvar in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btVisao IN FRAME fpage0 /* Classificar */
OR CHOOSE OF MENU-ITEM miClassifica  IN MENU mbMain DO:
 
  assign wWindow:sensitive = no.
  run mvp/ESMV0616b.w (input-output table ttVisao).
  assign wWindow:sensitive = yes.
  apply "ENTRY":U to btVisao in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */




PROCEDURE CtrlFrame.TreeList.MouseDown .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    X
    Y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-X      AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-Y      AS DECIMAL NO-UNDO.

assign i-button-tree = p-Button.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




PROCEDURE CtrlFrame.TreeList.NodeClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Node
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Node AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE cExpande AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cContrai AS CHARACTER  NO-UNDO.
DEFINE VARIABLE rRow     AS ROWID      NO-UNDO.

/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Expande",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign cExpande = return-value.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Contrai",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign cContrai = return-value.

if chTreeList:Nodes:Count > 0 then do:
    /** Sele‡Æo de linha **/
    case i-button-tree:
        /** BotÆo esquerdo do mouse **/
        when 1 then do:
            /** Guarda rowid do tree-view **/
            assign rRow = to-rowid(entry(1,chTreeList:SelectedItem:Tag)).
            /** Busca Registro com rowid **/
            for first bfttDados 
                where rowid(bfttDados) = rRow no-lock:
            end.
            /** Verifica se ele cont‚m filhos **/
            for first ttDados
                where ttDados.cod-dimens-pai = bfttDados.cod-dimensao no-lock:
            end.
            /** Mostra registros no browse **/
            run atualizaBrowse in this-procedure.
        end.
        /** BotÆo direito do mouse **/
        when 2 then do:
           /** Pop Menu Expande-Contrai **/
           case int(chTreeList:SelectedItem:image):
              when 1  then assign exp-con:sensitive = TRUE.  
              when 2  then assign exp-con:sensitive = TRUE.  
              when 3  then assign exp-con:sensitive = TRUE.  
              when 4  then assign exp-con:sensitive = TRUE.  
              when 5  then assign exp-con:sensitive = TRUE.  
              when 6  then assign exp-con:sensitive = TRUE.  
              when 7  then assign exp-con:sensitive = TRUE.  
              when 8  then assign exp-con:sensitive = TRUE.  
              when 9  then assign exp-con:sensitive = TRUE.  
              when 10 then assign exp-con:sensitive = TRUE.  
              when 11 then assign exp-con:sensitive = TRUE.
              when 12 then assign exp-con:sensitive = TRUE.
              otherwise    assign exp-con:sensitive = true.
           end case.
           assign exp-con:label = if chTreeList:SelectedItem:Expanded = false then cExpande
                                                                              else cContrai.
           run SendMessageA (self:hwnd, 517, 0, 0).            
        end.
    end case.
end.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







/*--- L¢gica para inicializa‡Æo do programam ---*/
/** Cria‡Æo do pop menu do tree-view **/
create menu pop-menu
    assign popup-only = true
           title = "POPUP":u.
create menu-item exp-con
    assign parent = pop-menu.
create menu-item regua
    assign parent  = pop-menu
           subtype = "RULE":u.

/** Include padrÆo do template **/

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


    
 

/** Procedure que manda mensagens para o sistema operacional **/
PROCEDURE SendMessageA EXTERNAL "user32":U:
    DEFINE INPUT PARAMETER hwnd   AS long.
    DEFINE INPUT PARAMETER umsg   AS long.
    DEFINE INPUT PARAMETER wparam AS long.
    DEFINE INPUT PARAMETER lparam AS LONG. 
END.
/** define procedure externa para execucao do programa de visualizacao do txt **/
PROCEDURE WinExec EXTERNAL "kernel32.dll":
  DEF INPUT  PARAM prg_name                          AS CHARACTER.
  DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE.

/** Trigger de choose do pop menu "Expande" **/
on choose of exp-con do:
    if chTreeList:nodes:Count > 0 then do:
        run expandeItem (input chTreeList:SelectedItem,
                         input not chTreeList:SelectedItem:Expanded).
    end.
end.

btRedimensiona:load-mouse-pointer ("image~\size2.cur") in frame fPage0.




/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*****************************************************************************
**
**       PROGRAMA: ABAPI001.i2
**
**       DATA....: Julho de 2003
**
**       AUTOR...: Marcio Willwock - Manufatura - DATASUL S.A.
**
**       OBJETIVO: API para Atualiza‡Æo de Apontamentos de Abastecimento e
**                 Lubrifica‡Æo
**
*****************************************************************************/

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
         HEIGHT             = 6.04
         WIDTH              = 26.43.
/* END WINDOW DEFINITION */
                                                                        */


 





/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */


PROCEDURE piCriaErro :
/*------------------------------------------------------------------------------
  Purpose:     piCriaErro
  Parameters:  entrada pErrorNumber      = N£mero do erro
               entrada pErrorType        = Tipo do erro (EMS)
               entrada pErrorParameters  = Parƒmetros necess rios para erros
  Notes:       Insere registro na temptable RowErrors manualmente
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pErrorNumber      AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER pErrorType        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pErrorParameters  AS CHARACTER NO-UNDO.

DEFINE VARIABLE iErrorSequence    AS INTEGER    NO-UNDO.
DEFINE VARIABLE cErrorSubType     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cErrorDescription AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cErrorHelp        AS CHARACTER  NO-UNDO.

/** Busca o Tipo de Mensagem do EMS **/
RUN utp/ut-msgs.p (INPUT "TYPE":U,
                   INPUT pErrorNumber,
                   INPUT pErrorParameters).
CASE /***************************
* Include: i01un001.i
* Campo:   tipo-cod-situacao
***************************/

 






/***************************************************************************
**  ind01-10.i - define as funcoes de um indicador
**  Para indicadores de 1 a 10 items
**
**  Funcoes disponiveis
**  01: view-as Combo-box
**  02: view-as radio-set
**  03: lista com os itens separados por virgula
**  04 n: retorna o item n da lista
**  05: retorna o numero de items da lista
**  06: retorna a posicao do item (numero)
**  07: valores para a propriedade Radio-Buttons de um Radio-Set
***************************************************************************/

/* verifica parametros ****************************************************/




/* &if lookup("{1}", "01,02,03,04,05,06,07") = 0 &then
    &message *** ({&file-name}): Parametro incorreto: {1} !
    &message *** Deveria ser: 01, 02, 03, 04, 05, 06, 07 
&endif  */
    

  


/* monta lista de items para LISTA (03), NUM (04), ITEM(05), IND(06) ************************/

          
               
     
               
     
               
     
     
     
     
     
     


/* funcao Combo-box (01) *************************************************************/


/* funcao Radio-set (02) *************************************************************/


/* funcao Lista (03) **********************************************************/


/* funcao NUM (05) ************************************************************/


/* funcao Item n (04) *********************************************************/    


/* funcao IND string (06) ****************************************************/



     lookup(RETURN-VALUE, "Erro,Advertˆncia,Pergunta,Informa‡Æo")
     


/* valores para a propriedade Radio-Buttons de um Radio-Set *******************/




    

    

    

    

    







/* fim */
 
/* Fim */

 :
    WHEN 1 THEN
        ASSIGN cErrorSubType = "ERROR":U.
    WHEN 2 THEN
        ASSIGN cErrorSubType = "WARNING":U.
    WHEN 4 THEN
        ASSIGN cErrorSubType = "INFORMATION":U.
END CASE.

/** Busca Texto da Mensagem **/
RUN utp/ut-msgs.p (INPUT "MSG":U,
                   INPUT pErrorNumber,
                   INPUT pErrorParameters).
ASSIGN cErrorDescription = RETURN-VALUE.

/** Busca Help da Mensagem **/
RUN utp/ut-msgs.p (INPUT "HELP":U,
                   INPUT pErrorNumber,
                   INPUT pErrorParameters).
ASSIGN cErrorHelp = RETURN-VALUE.


/*--- Atualiza vari vel de sequˆncia de erros ---*/
IF CAN-FIND(LAST RowErrors) THEN DO:
    FIND LAST RowErrors.
    ASSIGN iErrorSequence = RowErrors.ErrorSequence + 1.
END.
ELSE
    ASSIGN iErrorSequence = iErrorSequence + 1.

CREATE RowErrors.
ASSIGN RowErrors.ErrorSequence    = iErrorSequence
       RowErrors.ErrorNumber      = pErrorNumber
       RowErrors.ErrorType        = pErrorType
       RowErrors.ErrorSubType     = cErrorSubType
       RowErrors.ErrorDescription = cErrorDescription
       RowErrors.ErrorHelp        = cErrorHelp
       RowErrors.ErrorParameters  = pErrorParameters.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */


  /** Cria‡Æo de Erros **/

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */


PROCEDURE afterDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     afterDestroyInterface
  Parameters:  <none>
  Notes:       Override ap¢s fechar a tela
------------------------------------------------------------------------------*/
/** Caixa de mensagens **/

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



 
chTreeList:terminate.
release object chTreeList  no-error.
release object chImageList no-error.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     afterInitializeInterface
  Parameters:  <none>
  Notes:       Override ap¢s inicializa‡Æo da tela
------------------------------------------------------------------------------*/
/** Oculta botao de Classificacao **/
assign btVisao:hidden in frame fPage0 = yes.

run piFormataTreeList in this-procedure.

/*:T--- Verifica se o DBO jÿ estÿ inicializado ---*/
IF NOT VALID-HANDLE(hDBOTag) OR
    hDBOTag:TYPE <> "PROCEDURE":U OR
    hDBOTag:FILE-NAME <> "mnbo/bomn152.p":U THEN DO:
    RUN mnbo/bomn152.p PERSISTENT SET hDBOTag.
END.
RUN openQueryStatic IN hDBOTag (INPUT "Default":U) NO-ERROR.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE atualizaBrowse :
/*------------------------------------------------------------------------------
  Purpose:     atualizaBrowse
  Parameters:  <none>
  Notes:       Mostra registros no browse
------------------------------------------------------------------------------*/
/** Se tiver filhos, mostra no browse, os filhos **/
if avail ttDados then do:
    open query brDetalhe for each  ttDados
                             where ttDados.cod-dimens-pai = bfttDados.cod-dimensao.
end.
/** SenÆo, mostra ele mesmo **/
else do:
    open query brDetalhe for each  ttDados
                             where ttDados.cod-dimensao = bfttDados.cod-dimensao.
end.

assign ttDados.nr-ord-produ:visible in browse brDetalhe = yes
       ttDados.des-man-corr:visible in browse brDetalhe = yes
       cEstado:visible              in browse brDetalhe = yes
       ttDados.cod-eqpto:visible    in browse brDetalhe = yes
       ttDados.dt-manut:visible     in browse brDetalhe = yes
       ttDados.dt-fecham:visible    in browse brDetalhe = yes.

if avail ttDados and ttDados.sequencia > iVisao then assign lReal = yes.
                                                else assign lReal = no.

if not(lReal) then
    assign ttDados.nr-ord-produ:visible in browse brDetalhe = NO
           ttDados.des-man-corr:visible in browse brDetalhe = NO
           cEstado:visible              in browse brDetalhe = NO
           ttDados.cod-eqpto:visible    in browse brDetalhe = NO
           ttDados.dt-manut:visible     in browse brDetalhe = NO
           ttDados.dt-fecham:visible    in browse brDetalhe = NO.

if ttSelecao.iIndicador = 1 then
    assign ttDados.des-man-corr:visible in browse brDetalhe = no.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE beforeInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     beforeInitializeInterface
  Parameters:  <none>
  Notes:       Override antes de iniciar a tela
------------------------------------------------------------------------------*/
/*- Manut. Mecanica -*/
run cdp/cd9902.p (input 3).
if return-value = "NOK":U then do:
   /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Manuten‡Æo Mecƒnica",
                    input "*",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
   run piCriaErro in this-procedure (input 2082, 
                                     input "EMS":U,
                                     input return-value).
   run piMostraErros.
   return "NOK":U.
end.

assign chTreeList               = chCtrlFrame:TreeList
       chImageList              = chCtrlFrame-2:ImageList
       chTreeList:ImageList     = chImageList
       CtrlFrame:popup-menu     = pop-menu
       chTreeList:HideSelection = FALSE
       dWinXC                   = wWindow:width-chars
       dWinYC                   = wWindow:height-chars.

ChTreeList:Nodes:Clear().
chTreelist:FullRowSelect = true.

if ChTreeList:Nodes:Count > 0 then
   assign ChTreeList:SelectedItem = ChTreeList:Nodes(1). 

run criattSelecao in this-procedure.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE buscaValor :
/*------------------------------------------------------------------------------
  Purpose:     buscaValor
  Parameters:  entrada pValorDimensao = N£mero da dimensÆo escolhida 
  Notes:       Busca os c¢digos e descri‡Æo das visäes escolhidas
------------------------------------------------------------------------------*/
define input parameter pValorDimensao as character format "x(40)" no-undo.

/** Zera vari veis no in¡cio **/
assign vCodigo    = ""
       vDescricao = ""
       iImage     = 0
       rRowid     = ?.

/** Verifica tipo de visÆo (classifica‡Æo) **/
case substring(trim(pValorDimensao),1,2):
    /** Empresa **/
    when "01":U then do:
        if not avail empresa or empresa.ep-codigo <> ttAux.ep-codigo then do:
            for first empresa fields(nome ep-codigo)  
                where empresa.ep-codigo = ttAux.ep-codigo no-lock:
            end.
        end.
        if avail empresa then do:
            assign vCodigo    = string(empresa.ep-codigo, "999")
                   vDescricao = empresa.nome
                   iImage     = 1
                   rRowid     = rowid(empresa).
        end.      
        else do:
           /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Empresa_nÆo _cadastrada",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
           assign vCodigo    = string(ttAux.ep-codigo, "999")
                  vDescricao = return-value
                  iImage     = 1
                  rRowid     = rowid(ttAux).
        end.
    end.
    /** Estabelecimento **/
    when "02":U then do:
        if not avail estabelec or estabelec.cod-estabel <> ttAux.cod-estabel then do:
            for first estabelec fields(nome cod-estabel)  
                where estabelec.cod-estabel = ttAux.cod-estabel no-lock:
            end.
        end.
        if avail estabelec then do:
            assign vCodigo    = estabelec.cod-estabel
                   vDescricao = estabelec.nome
                   iImage     = 2
                   rRowid     = rowid(estabelec).
        end.   
        else do:
           /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Estabelecimento_nÆo_cadastrado",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
               
           assign vCodigo     = ttAux.cod-estabel
                   vDescricao = return-value                     
                   iImage     = 2                                
                   rRowid     = rowid(ttAux).                   
        end. 
    end.
    /** Modelo/Familia Equipamento **/
    when "03":U then do:
        if ttSelecao.iIndicador = 1 then do:
            if not avail mab-model or mab-model.cod-model <> ttAux.cod-modelo then do:
                for first mab-model fields(cod-model des-model)  
                    where mab-model.cod-model = ttAux.cod-modelo no-lock:
                end.
            end.
            if avail mab-model then do:
                assign vCodigo    = mab-model.cod-model
                       vDescricao = mab-model.des-model
                       iImage     = 3
                       rRowid     = rowid(mab-model).
            end.
            else do:
               /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Grupo_de_Equipamento_nÆo_cadastrado",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
               
               assign vCodigo     = ttAux.cod-modelo 
                       vDescricao = return-value                     
                       iImage     = 3                                
                       rRowid     = rowid(ttAux).                   
            end. 
        end.
        else do:
            if not avail fam-equipto or fam-equipto.fm-equipto <> ttAux.cod-modelo then do:
                for first fam-equipto fields(fm-equipto descricao)
                    where fam-equipto.fm-equipto = ttAux.cod-modelo no-lock:
                end.
            end.
            if avail fam-equipto then do:
                assign vCodigo    = fam-equipto.fm-equipto
                       vDescricao = fam-equipto.descricao
                       iImage     = 3
                       rRowid     = rowid(fam-equipto).
            end.
            else do:
               /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Familia_do_Equipamento_nÆo_cadastrado",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
               
               assign vCodigo     = ttAux.cod-modelo 
                       vDescricao = return-value                     
                       iImage     = 3                                
                       rRowid     = rowid(ttAux).                   
            end.
        end.
    end.
    /** Equipamento **/
    when "04":U then do:
        if ttSelecao.iIndicador = 1 then do:
            if not avail mab-eqpto or mab-eqpto.ep-codigo <> ttAux.ep-codigo
                                   or mab-eqpto.cod-eqpto <> ttAux.cod-eqpto then do:
                for first mab-eqpto fields(ep-codigo cod-eqpto cod-model vli-ano-fabric)  
                    where mab-eqpto.ep-codigo = ttAux.ep-codigo
                    and   mab-eqpto.cod-eqpto = ttAux.cod-eqpto no-lock:
                end.
            end.
            if avail mab-eqpto then do:
                assign vCodigo    = string(mab-eqpto.ep-codigo) + "-" + mab-eqpto.cod-eqpto
                       iImage     = 4
                       rRowid     = rowid(mab-eqpto).
                if not avail mab-model or mab-model.cod-model <> mab-eqpto.cod-model then do:
                    for first mab-model fields(des-model cod-model un)
                        where mab-model.cod-model = mab-eqpto.cod-model no-lock:
                    end.
                end.
                if avail mab-model then do:
                    assign vDescricao = mab-model.des-model.
                end.
            end.
            else do:
               /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Equipamento_nÆo_cadastrado",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
               
               assign vCodigo     = string(ttAux.ep-codigo) + "-" + ttAux.cod-eqpto   
                       vDescricao = return-value                     
                       iImage     = 4                                
                       rRowid     = rowid(ttAux).                   
            end.
        end.
        else do:
            if not avail equipto or equipto.cd-equipto <> ttAux.cod-eqpto then do:
                for first equipto fields(ep-codigo cd-equipto descricao)  
                    where equipto.cd-equipto = ttAux.cod-eqpto no-lock:
                end.
            end.
            if avail equipto then do:
                assign vCodigo    = equipto.cd-equipto
                       iImage     = 4
                       rRowid     = rowid(equipto)
                       vDescricao = equipto.descricao.
            end.
            else do:
               /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Equipamento_nÆo_cadastrado",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
               
               assign vCodigo     = ttAux.cod-eqpto   
                       vDescricao = return-value                     
                       iImage     = 4                                
                       rRowid     = rowid(ttAux).                   
            end.
        end.
    end.
    /** TAG **/
    when "05":U then do:
        if not avail tag or tag.cd-tag <> ttAux.cd-tag then do:
            for first tag 
                where tag.cd-tag = ttAux.cd-tag no-lock:
            end.
        end.
        if avail tag then do:
            assign vCodigo    = if ttAux.cd-tag = "" then "_" else ttAux.cd-tag
                   iImage     = 5
                   rRowid     = rowid(tag)
                   vDescricao = tag.descricao.

        END.
        else do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Tag_nÆo_dispon¡vel",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
             assign vCodigo    = if ttAux.cd-tag = "" then "_" else mab-eqpto.cd-tag
                    vDescricao = return-value
                    iImage     = 5
                    rRowid     = rowid(ttAux).
        end.
    end.
END CASE.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE calcHoras :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input  param p-hra-entr like mmv-ord-manut.hra-entr.
define input  param p-hra-prev like mmv-ord-manut.hra-entr.
define input  param p-dat-entr like mmv-ord-manut.dat-entr. 
define input  param p-dat-prev like mmv-ord-manut.dat-prev.
define output param p-total    as decimal no-undo.

IF VALID-HANDLE(hAcomp) THEN DO:
    RUN pi-Acompanhar IN hAcomp ("Calculando Horas").
END.

if  substring(p-hra-entr,1,2) <> "":U and 
    substring(p-hra-entr,3,2) <> "":U then
    assign d-hr-hora-ini = ((int(substring(p-hra-entr,1,2)) * 60) + (int(substring(p-hra-entr,3,2)))) / 60.

if  substring(p-hra-prev,1,2) <> "":U and 
    substring(p-hra-prev,3,2) <> "":U then
    assign d-hr-hora-fim = ((int(substring(p-hra-prev,1,2)) * 60) + (int(substring(p-hra-prev,3,2)))) / 60.

if  (p-dat-entr <> ?) OR 
    (p-dat-prev <> ?) then do:
    assign p-total = dec(p-dat-prev  - p-dat-entr).

    if  p-total = 0 then do:
        assign p-total = d-hr-hora-fim - d-hr-hora-ini.
    end.
    else do:
        assign p-total = p-total * 24.
        if  d-hr-hora-fim < d-hr-hora-ini then
            assign p-total = p-total - (d-hr-hora-ini - d-hr-hora-fim).
        else assign p-total = p-total + (d-hr-hora-fim - d-hr-hora-ini).
    end.
end.
else do:
    assign p-total = d-hr-hora-fim - d-hr-hora-ini.
end.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE calcHorasDia :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters: <none>
  Notes: pTipo = 1 - Equipamento; (Dia a Dia *Excel)
                 2 - TAG; (Dia a Dia *Excel)
                 888 - Equipamento; (M‚dia)
                 999 - TAG; (M‚dia)
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pEmp   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pEqpto AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pTag   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pTipo  AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER pData  AS DATE      NO-UNDO.

DEFINE VARIABLE dIni     AS DECIMAL              NO-UNDO.
DEFINE VARIABLE dFim     AS DECIMAL              NO-UNDO.
DEFINE VARIABLE controle AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE dataCont AS DATE                 NO-UNDO.
DEFINE VARIABLE dHoras   AS DECIMAL              NO-UNDO.
DEFINE VARIABLE dMinutos AS DECIMAL              NO-UNDO.

IF VALID-HANDLE(hAcomp) THEN DO:
    RUN pi-Acompanhar IN hAcomp ("Calculando Horas/Dias").
END.

/* Limpa a tabela tempor ria de informa‡äes das horas */
EMPTY TEMP-TABLE tt-horas.
/** Zera a vari vel **/
ASSIGN dHorasDia = 0.

IF (ttSelecao.iIndicador = 1 AND (pTipo = 2 OR pTipo = 999)) THEN DO:

    FOR EACH tt-horas-aux
        WHERE tt-horas-aux.cod-model = pTag NO-LOCK:

        IF pTipo = 2 THEN
            IF tt-horas-aux.dat-movto <> pData THEN NEXT.

        ASSIGN dFim = ((INT(SUBSTRING(tt-horas-aux.hra-final,1,2)) * 60)   + (INT(SUBSTRING(tt-horas-aux.hra-final,3,2)))) / 60.
        ASSIGN dIni = ((INT(SUBSTRING(tt-horas-aux.hra-inicial,1,2)) * 60) + (INT(SUBSTRING(tt-horas-aux.hra-inicial,3,2)))) / 60.

        IF (dFim - dIni) >= 23.93 THEN DO:
            ASSIGN dHorasDia = dHorasDia + 24.
        END.
        ELSE DO:
            ASSIGN dHorasDia = dHorasDia + (dFim - dIni).
        END.
    END.
END.
ELSE DO:
    IF ttSelecao.iIndicador = 2 THEN DO:

        FOR EACH  tt-apont
            WHERE tt-apont.cod-model = pTag:

            /** Se tiver calculando TAG, considerando somenta OMïs 'Area Parada' **/
            IF pTipo = 999 OR pTipo = 2 THEN DO:
                IF NOT(tt-apont.parada) THEN NEXT.
            END.

            /** Se tiver calculando horas para o Equipamento, filtra pelo mesmo **/
            IF pTipo = 1 OR pTipo = 888 THEN DO:
                IF NOT(tt-apont.cod-eqpto = pEqpto) THEN NEXT.
            END.

            IF tt-apont.dat-ini < tt-apont.dat-fim THEN DO:
                DO dataCont = tt-apont.dat-ini TO tt-apont.dat-fim:
                    CREATE tt-horas.
                    ASSIGN tt-horas.dat-movto = dataCont
                           tt-horas.cod-model = tt-apont.cod-model.

                    IF tt-apont.dat-fim = dataCont THEN DO:
                        ASSIGN tt-horas.hra-final = tt-apont.hra-final.
                    END.
                    ELSE DO:
                        ASSIGN tt-horas.hra-final = "235959":U.
                    END.

                    IF tt-apont.dat-ini = dataCont THEN DO:
                        ASSIGN tt-horas.hra-inicial = tt-apont.hra-inicial.
                    END.
                    ELSE DO:
                        ASSIGN tt-horas.hra-inicial = "000000":U.
                    END.
                END.
            END.
            ELSE DO:
                CREATE tt-horas.
                ASSIGN tt-horas.ep-codigo   = tt-apont.ep-codigo 
                       tt-horas.cod-eqpto   = tt-apont.cod-eqpto
                       tt-horas.cod-model   = tt-apont.cod-model
                       tt-horas.hra-inicial = tt-apont.hra-inicial
                       tt-horas.hra-final   = tt-apont.hra-final
                       tt-horas.dat-movto   = tt-apont.dat-ini
                       tt-horas.cod-model   = tt-apont.cod-model.
            END.
        END.

        EMPTY TEMP-TABLE tt-horas-aux.

        FOR EACH  tt-horas
            WHERE tt-horas.cod-model = pTag:

            /** Valida‡Æo para o calculo dia a dia, considera apenas data informada **/
            IF pTipo = 1 OR pTipo = 2 THEN DO:
                IF tt-horas.dat-movto <> pData THEN NEXT.
            END.

            /** Valida‡Æo para Media da Disponibilidade **/
            IF pTipo = 999 OR pTipo = 888 THEN DO:
                IF NOT(tt-horas.dat-movto >= ttSelecao.periodo-ini  AND
                       tt-horas.dat-movto <= ttSelecao.periodo-fim) THEN NEXT.
            END.

            IF NOT CAN-FIND(FIRST tt-horas-aux
                            WHERE tt-horas-aux.dat-movto = tt-horas.dat-movto) THEN DO:
                CREATE tt-horas-aux.
                ASSIGN tt-horas-aux.hra-inicial = tt-horas.hra-inicial
                       tt-horas-aux.hra-final   = tt-horas.hra-final
                       tt-horas-aux.dat-movto   = tt-horas.dat-movto
                       tt-horas-aux.primeiro    = YES.
                ASSIGN controle = "":U.
            END.
            ELSE DO:
                FOR EACH  tt-horas-aux
                    WHERE tt-horas-aux.dat-movto = tt-horas.dat-movto:

                    IF  (tt-horas-aux.hra-inicial > tt-horas.hra-inicial  AND
                         tt-horas-aux.hra-inicial > tt-horas.hra-final)   OR 
                        (tt-horas-aux.hra-inicial < tt-horas.hra-inicial  AND
                         tt-horas-aux.hra-final   < tt-horas.hra-inicial) THEN DO:
                        ASSIGN controle = "ADD":U.
                    END.
                    ELSE IF tt-horas-aux.hra-inicial >= tt-horas.hra-inicial AND
                            tt-horas-aux.hra-inicial <= tt-horas.hra-final   THEN DO:
                        ASSIGN controle = "UPDATE":U.
                        ASSIGN tt-horas-aux.hra-inicial = tt-horas.hra-inicial.
                        IF tt-horas-aux.hra-final <= tt-horas.hra-final THEN
                            ASSIGN tt-horas-aux.hra-final = tt-horas.hra-final.
                    END.
                    ELSE IF tt-horas-aux.hra-inicial <= tt-horas.hra-inicial AND
                            tt-horas-aux.hra-final   >= tt-horas.hra-inicial THEN DO:
                        ASSIGN controle = "UPDATE":U.
                        IF tt-horas-aux.hra-final <= tt-horas.hra-final THEN
                            ASSIGN tt-horas-aux.hra-final = tt-horas.hra-final.
                    END.
                    ELSE DO: 
                        ASSIGN controle = "":U.
                    END.
                END.
            END.

            IF controle = "ADD":U THEN DO:
                CREATE tt-horas-aux.
                ASSIGN tt-horas-aux.hra-inicial = tt-horas.hra-inicial
                       tt-horas-aux.hra-final   = tt-horas.hra-final
                       tt-horas-aux.dat-movto   = tt-horas.dat-movto
                       tt-horas-aux.primeiro    = NO.
            END.
        END.

        FOR EACH  bf-ttHoras
            WHERE bf-ttHoras.primeiro = YES:
            FOR FIRST tt-horas-aux
                WHERE tt-horas-aux.dat-movto     = bf-ttHoras.dat-movto
                AND   tt-horas-aux.primeiro      = NO
                AND ((tt-horas-aux.hra-inicial = bf-ttHoras.hra-inicial AND tt-horas-aux.hra-final  = bf-ttHoras.hra-final) 
                OR   (tt-horas-aux.hra-inicial  = bf-ttHoras.hra-inicial AND tt-horas-aux.hra-final  < bf-ttHoras.hra-final)  
                OR   (tt-horas-aux.hra-inicial  > bf-ttHoras.hra-inicial AND tt-horas-aux.hra-final  < bf-ttHoras.hra-final)  
                OR   (tt-horas-aux.hra-inicial  > bf-ttHoras.hra-inicial AND tt-horas-aux.hra-final  = bf-ttHoras.hra-final)) NO-LOCK:
                DELETE tt-horas-aux.
            END.
        END.

        ASSIGN dHorasDia = 0
               dFim      = 0
               dIni      = 0.

        FOR EACH tt-horas-aux:
            ASSIGN dFim = ((INT(SUBSTRING(tt-horas-aux.hra-final,1,2)) * 60)   + (INT(SUBSTRING(tt-horas-aux.hra-final,3,2)))) / 60.
            ASSIGN dIni = ((INT(SUBSTRING(tt-horas-aux.hra-inicial,1,2)) * 60) + (INT(SUBSTRING(tt-horas-aux.hra-inicial,3,2)))) / 60.
            IF (dFim - dIni) >= 23.93 THEN DO:
                ASSIGN dHorasDia = dHorasDia + 24.
            END.
            ELSE DO:
                ASSIGN dHorasDia = dHorasDia + (dFim - dIni).
            END.
        END.
    END.
    ELSE DO:
        ASSIGN iTipo  = pTipo
               cTag   = pTag
               dData  = pData
               iEmp   = pEmp
               cEqpto = pEqpto. /** Foram definidas como vari veis do programa para nÆo ter que passar como parƒmetros**/
        RUN simultaneidadeFrotas IN THIS-PROCEDURE.
    END.
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE carregaFiltro :
/*------------------------------------------------------------------------------
  Purpose:     carregaFiltro
  Parameters:  entrada cNome = Nome do arquivo 
  Notes:       Importa arquivo de configura‡äes da consulta 
------------------------------------------------------------------------------*/
define input parameter cNome  as character no-undo.

define variable c-linha as char no-undo.

/** Limpa as visäes **/
EMPTY TEMP-TABLE ttVisao.

input from value (cNome).
/** Busca os parƒmetros e sele‡äes **/
for first ttSelecao exclusive-lock:
    import unformatted c-linha.
    assign c-linha                  = replace (c-linha, chr(34), "")
           ttSelecao.periodo-ini    = date(entry(1,c-linha,";")) 
           ttSelecao.periodo-fim    = date(entry(2,c-linha,";")) 
           ttSelecao.empresa-ini    = entry(3,c-linha,";") 
           ttSelecao.empresa-fim    = entry(4,c-linha,";") 
           ttSelecao.equipto-ini    = entry(5,c-linha,";") 
           ttSelecao.equipto-fim    = entry(6,c-linha,";") 
           ttSelecao.estab-ini      = entry(7,c-linha,";") 
           ttSelecao.estab-fim      = entry(8,c-linha,";") 
           ttSelecao.grupo-ini      = entry(9,c-linha,";") 
           ttSelecao.grupo-fim      = entry(10,c-linha,";")
           ttSelecao.modelo-ini     = entry(11,c-linha,";")
           ttSelecao.modelo-fim     = entry(12,c-linha,";")
           ttSelecao.estrut-ini     = entry(13,c-linha,";")
           ttSelecao.estrut-fim     = entry(14,c-linha,";")
           ttSelecao.tag-ini        = entry(15,c-linha,";")
           ttSelecao.tag-fim        = entry(16,c-linha,";")
           ttSelecao.cc-ini         = entry(17,c-linha,";")
           ttSelecao.cc-fim         = entry(18,c-linha,";")
           ttSelecao.ano-fabric-ini = int(entry(19,c-linha,";"))
           ttSelecao.ano-fabric-fim = int(entry(20,c-linha,";"))
           ttSelecao.lMtbf          = (entry(21,c-linha,";") = "yes")
           ttSelecao.lMttr          = (entry(22,c-linha,";") = "yes")
           ttSelecao.lDispo         = (entry(23,c-linha,";") = "yes")
           ttSelecao.lPmpl          = (entry(24,c-linha,";") = "yes")
           ttSelecao.iIndicador     = int(entry(25,c-linha,";"))
           ttSelecao.lMat           = (entry(26,c-linha,";") = "yes") 
           ttSelecao.lGGF           = (entry(27,c-linha,";") = "yes")  
           ttSelecao.lServ          = (entry(28,c-linha,";") = "yes")  
           ttSelecao.lContratos     = (entry(29,c-linha,";") = "yes")
           ttSelecao.lCusto         = (entry(30,c-linha,";") = "yes")  
           ttSelecao.lTotal         = (entry(31,c-linha,";") = "yes")  
           ttSelecao.lMatMesAnt     = (entry(32,c-linha,";") = "yes")  
           ttSelecao.iNivTag        = int(entry(33,c-linha,";"))
           ttSelecao.dt-trans-ini   = date(entry(34,c-linha,";"))
           ttSelecao.dt-trans-fim   = date(entry(35,c-linha,";"))
        .
end.

repeat:
    /** Busca as visäes **/
    import unformatted c-linha.
    assign c-linha = replace (c-linha, chr(34), "").
    if num-entries (c-linha, ";") = 2 then do:
        create ttVisao.
        assign ttVisao.dimensao  = entry (1, c-linha, ";")
               ttVisao.sequencia = int (entry (2, c-linha, ";")).
    end.
end.

input close.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/


DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "esmv0616.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    CtrlFrame-2:NAME = "CtrlFrame-2":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "esmv0616.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE criaOrdemForaDisponibilidade :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR FIRST mmv-ord-manut
    WHERE mmv-ord-manut.nr-ord-produ = mmv-movto-ord.nr-ord-produ NO-LOCK:
    /** Cria temp-table com todas as Ordens que entraram no filtro **/                                                                                         
    CREATE tt-ord-manut.                                                                                                                                       
    ASSIGN tt-ord-manut.nr-ord-produ = mmv-ord-manut.nr-ord-produ                                                                                              
           tt-ord-manut.estado       = mmv-ord-manut.estado                                                                                                    
           tt-ord-manut.cod-eqpto    = mmv-ord-manut.cod-eqpto                                                                                                 
           tt-ord-manut.dt-manut     = mmv-ord-manut.dat-entr                                                                                                  
           tt-ord-manut.dt-fecham    = mmv-ord-manut.dat-term
           tt-ord-manut.indicador    = NO. 
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE criaOrdemForaMI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE tt-ord-manut.                                      
ASSIGN tt-ord-manut.nr-ord-produ = ord-manut.nr-ord-produ 
       tt-ord-manut.des-man-corr = ord-manut.des-man-corr 
       tt-ord-manut.estado       = ord-manut.estado       
       tt-ord-manut.cod-eqpto    = ord-manut.cd-equipto   
       tt-ord-manut.dt-manut     = ord-manut.dt-manut     
       tt-ord-manut.dt-fecham    = ord-manut.dt-fecham    
       tt-ord-manut.indicador    = NO.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE criaTreeList :
/*------------------------------------------------------------------------------
  Purpose:     criaTreeList
  Parameters:  <none>
  Notes:       Cria os n¢s do tree-list
------------------------------------------------------------------------------*/
define variable iQtSeq  as integer initial 0 no-undo.
define variable i       as integer           no-undo.
define variable c-texto as character         no-undo.
define buffer bfttDados for ttDados.

assign i-cont = 0.
/** Verifica se existem visäes escolhidas **/
for last ttVisao fields(sequencia) no-lock:
    assign iQtSeq = ttVisao.sequencia.
end.
    if iQtSeq = 0 then return.

assign chTreeList:visible = no.

/** Busca os dados criados **/
for each  ttDados
    where ttDados.sequencia  <= iQtSeq 
    by ttDados.cod-dimensao:

    if ttDados.sequencia > iVisao then assign lReal = no.
                                  else assign lReal = yes.

    /** Conta sequˆncias de linhas **/ 
    assign i-cont = i-cont + 1.
    assign ttDados.seq-tree = i-cont.
    assign c-texto = ttDados.cod-oficial + " - " + ttDados.desc-dimensao.

    /** Inclui primeira dimensÆo **/
    if ttDados.sequencia = 1 then do:
        if lReal then do:
            chTreeList:Nodes:Add (,, "i" + string(i-cont), c-texto, ttDados.p-image).
            if ttSelecao.lDispo     then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.dispo,"->,>>>,>>9.99")).
          /*if ttSelecao.lCusto     then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.custo,"->>>,>>>,>>9.99")).*/
            if ttSelecao.lMtbf      then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.mtbf,"->>>,>>>,>>9.99")).
            if ttSelecao.lMttr      then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.mttr,"->>>,>>>,>>9.99")).            
            if ttSelecao.lPmpl      then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.pmpl,"->>>,>>>,>>9.99")).
            if ttSelecao.lMat       then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.mat,"->>>,>>>,>>9.99")).
            if ttSelecao.lGGF       then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.ggf,">>>>,>>>,>>9.9999")).
            if ttSelecao.lServ      then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.serv,"->>>,>>>,>>9.99")).
          /*if ttSelecao.lContratos then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.contratos,"->>>,>>>,>>9.99")).*/
            if ttSelecao.lTotal     then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.tot,"->>>,>>>,>>9.99")).
        end.
        else do:
            chTreeList:Nodes:Add (,, "i" + string(i-cont), c-texto, ttDados.p-image).
        end.
        /** Guarda chave (rowid) da temp-table no tree-view **/
        assign chTreeList:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1,".
        if ttDados.r-rowid <> ? then
            assign chTreeList:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1," + string(ttDados.r-rowid).
    end.
    else do:
      /** Busca filhos da visÆo escolhida **/
      for first bfttDados fields(seq-tree)
          where bfttDados.cod-dimensao = ttDados.cod-dimens-pai no-lock:
          /** Inclui linhas das visäes filhas **/
          if lReal then do:
              chTreeList:Nodes:Add ("i" + string(bfttDados.seq-tree), 4, "i" + string(i-cont), c-texto, ttDados.p-image).
              if ttSelecao.lDispo     then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.dispo,"->,>>>,>>9.99")).
            /*if ttSelecao.lCusto     then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.custo,"->>>,>>>,>>9.99")).*/
              if ttSelecao.lMtbf      then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.mtbf,"->>>,>>>,>>9.99")).
              if ttSelecao.lMttr      then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.mttr,"->>>,>>>,>>9.99")).              
              if ttSelecao.lPmpl      then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.pmpl,"->>>,>>>,>>9.99")).
              if ttSelecao.lMat       then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.mat,"->>>,>>>,>>9.99")).
              if ttSelecao.lGGF       then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.ggf,">>>>,>>>,>>9.9999")).
              if ttSelecao.lServ      then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.serv,"->>>,>>>,>>9.99")).
            /*if ttSelecao.lContratos then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.contratos,"->>>,>>>,>>9.99")).              */
              if ttSelecao.lTotal     then chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.tot,"->>>,>>>,>>9.99")).
          end.
          else do:
              chTreeList:Nodes:Add ("i" + string(bfttDados.seq-tree), 4, "i" + string(i-cont), c-texto, ttDados.p-image).
          end. 
         /** Guarda chave (rowid) da temp-table no tree-view **/
         assign chTreeList:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1,". 
         if  ttDados.r-rowid <> ? then
             assign chTreeList:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",2," + string(ttDados.r-rowid).
      end.
    end.
end.

assign chTreeList:visible = yes.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE criaTTSelecao :
/*------------------------------------------------------------------------------
  Purpose:     criaTTSelecao
  Parameters:  <none>
  Notes:       Cria a temp-table de sele‡Æo (parƒmetros) do programa
------------------------------------------------------------------------------*/
create ttSelecao.
assign ttSelecao.periodo-ini    = today - 30
       ttSelecao.periodo-fim    = today
       ttSelecao.empresa-ini    = ""
       ttSelecao.empresa-fim    = "ZZZ"
       ttSelecao.equipto-ini    = ""
       ttSelecao.equipto-fim    = "ZZZZZZZZZZZZZZZZ"
       ttSelecao.estab-ini      = ""
       ttSelecao.estab-fim      = "ZZZ"
       ttSelecao.grupo-ini      = ""
       ttSelecao.grupo-fim      = "ZZZZZZZZ"
       ttSelecao.modelo-ini     = ""
       ttSelecao.modelo-fim     = "ZZZZZZZZ"
       ttSelecao.estrut-ini     = ""
       ttSelecao.estrut-fim     = "ZZZZZZZZ"
       ttSelecao.tag-ini        = ""
       ttSelecao.tag-fim        = "ZZZZZZZZZZZZZZZZ"
       ttSelecao.cc-ini         = ""
       ttSelecao.cc-fim         = "ZZZZZZZZ"
       ttSelecao.ano-fabric-ini = 0
       ttSelecao.ano-fabric-fim = 9999
       ttSelecao.lMtbf          = yes
       ttSelecao.lMttr          = yes
       ttSelecao.lDispo         = yes
       ttSelecao.lPmpl          = yes
       ttSelecao.iIndicador     = 1
       ttSelecao.iTipoDispo     = 1
       ttSelecao.lMat           = yes
       ttSelecao.lGGF           = yes
       ttSelecao.lServ          = yes
       ttSelecao.lContratos     = yes   
       ttSelecao.lCusto         = yes
       ttSelecao.lTotal         = yes
       ttSelecao.lMatMesAnt     = no
       ttSelecao.iNivTag        = 999
       ttSelecao.dt-trans-ini   = TODAY - 30
       ttSelecao.dt-trans-fim   = TODAY.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE expandeItem :
/*------------------------------------------------------------------------------
  Purpose:     expandeItem
  Parameters:  <none>
  Notes:       Expande o tree-list
------------------------------------------------------------------------------*/
def input param p-node   as com-handle no-undo.
def input param p-expcon as log        no-undo.

def var h-child as com-handle no-undo.
def var i-aux   as int        no-undo.

assign p-node:Expanded = p-expcon
       h-child         = p-node:Child.

do i-aux = 1 to p-node:Children:
    assign h-child = h-child:next no-error.
end.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE extraiDadosDaTreeList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var iTemp as integer no-undo.

assign n-colunas = chTreeList:nodes:item("i1"):listSubItems:count() + 2
       iTemp     = 0.

empty temp-table ttComponente.
empty temp-table ttColunaComponente.

assign i-loopComponente = 1.
repeat:        
    if i-loopComponente > chTreeList:nodes:count() then leave.
    
    if i-loopComponente modulo 200 = 0 then do:
        /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Lendo_Componente",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        run pi-acompanhar in h-acomp (input return-value + " - " + string(i-loopComponente) + " / " +  string(chTreeList:nodes:count())).
    end.

    if chTreeList:nodes:item("i" + string(i-loopComponente)):parent <> 0 then do:
        assign l-expand = yes.

        run piVerificaNo(input chTreeList:nodes:item("i" + string(i-loopComponente)):parent,
                         input-output l-expand).
        
        if l-expand = no then do:
            assign i-loopComponente = i-loopComponente + 1.
            next.
        end.            
    end.

    assign i-QtyPai = 0.
    assign ch-ComponenteTemp = chTreeList:nodes:item("i" + string(i-loopComponente)).
    
   repeat:
        assign i-QtyPai = i-QtyPai + 1.
            
        if ch-ComponenteTemp:parent = 0 then leave.
        ch-ComponenteTemp = ch-ComponenteTemp:parent.
    end.

    /*resgatando dados do componente*/
    assign iTemp = iTemp + 1.

    create ttComponente.
    assign ttComponente.posicao          = iTemp
           ttComponente.cod-componente   = chTreeList:nodes:item ("i" + string(i-loopComponente)):key
           ttComponente.descricao        = chTreeList:nodes:item ("i" + string(i-loopComponente)):text
           ttComponente.sequencia        = i-QtyPai.

    /*resgatando dados das colunas*/
    do i-countCol = 1 to n-colunas - 2:
        create ttColunaComponente.
        assign ttColunaComponente.cod-componente = ttComponente.cod-componente
               ttColunaComponente.posicao        = i-countCol + 2.
               ttColunaComponente.valor          = chTreeList:nodes:item ("i" + string(i-loopComponente)):listSubItems:item (i-countCol):text.
               ttColunaComponente.tipo           = chTreeList:ColumnHeaders(i-countCol + 1):tag.
    end.
    assign i-loopComponente = i-loopComponente + 1.
 end.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE movimentosMiForaIndicador :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN piCriaTTLeitura IN THIS-PROCEDURE.

/** Zera as vari veis **/
ASSIGN d-val-mat     = 0
       dGGF          = 0
       d-val-servico = 0.

FOR EACH tt-om-leitura NO-LOCK:

    /* Acompanhamento */
    IF VALID-HANDLE(hAcomp) THEN DO:
        RUN pi-Acompanhar IN hAcomp ("Validando Movimentos MI - Ordem: " + STRING(tt-om-leitura.nr-ord-produ)).
    END.

    /** Busca os valores de Material da Ordem **/
    IF ttSelecao.lMat THEN DO:
        blk_mat:
        FOR EACH  movto-mat FIELDS(nr-ord-produ dt-trans esp-docto tipo-trans valor-mat-m it-codigo) USE-INDEX ordem
            WHERE movto-mat.nr-ord-produ = tt-om-leitura.nr-ord-produ
            AND   movto-mat.dt-trans    >= ttSelecao.dt-trans-ini 
            AND   movto-mat.dt-trans    <= ttSelecao.dt-trans-fim NO-LOCK:

            IF  movto-mat.esp-docto <> 28 AND movto-mat.esp-docto <> 30 AND movto-mat.esp-docto <> 20 AND movto-mat.esp-docto <> 21 THEN NEXT blk_mat.

            FIND FIRST ord-manut WHERE ord-manut.nr-ord-produ = movto-mat.nr-ord-produ
                                 AND   ord-manut.cd-equipto   = equipto.cd-equipto NO-LOCK NO-ERROR.

            IF NOT AVAIL ord-manut THEN NEXT blk_mat.

            FIND FIRST tt-ord-manut WHERE tt-ord-manut.nr-ord-produ = movto-mat.nr-ord-produ
                                    AND   tt-ord-manut.indicador    = YES NO-LOCK NO-ERROR.

            IF NOT AVAIL tt-ord-manut THEN DO:
                FIND FIRST tt-ord-manut WHERE tt-ord-manut.nr-ord-produ = movto-mat.nr-ord-produ
                                        AND   tt-ord-manut.indicador    = NO NO-LOCK NO-ERROR.

                IF NOT AVAIL tt-ord-manut THEN DO:
                    RUN criaOrdemForaMI IN THIS-PROCEDURE.
                END.

                FOR FIRST item                                                                                               
                    WHERE item.it-codigo = movto-mat.it-codigo NO-LOCK:
                    /** 2 - item.ind-serv-mat = Material **/                                                                        
                    IF item.ind-serv-mat <> 2 THEN NEXT blk_mat.
                    ASSIGN d-val-mat = d-val-mat + (movto-mat.valor-mat-m[1] * (IF movto-mat.tipo-trans = 1 THEN -1 ELSE 1)).
                END.
            END.
        END.
        IF d-val-mat = 0 AND ttSelecao.lMatMesAnt THEN DO:
            RUN piMesAnterior IN THIS-PROCEDURE (INPUT ord-manut.nr-ord-produ).
        END.
    END.
    
    /** Busca os valores de Servi‡o da Ordem **/
    IF ttSelecao.lServ THEN DO:
        blk_estoq:
        FOR EACH  movto-estoq FIELDS(esp-docto dt-trans cod-emitente nr-ord-produ it-codigo valor-mat-m tipo-trans) USE-INDEX esp-data
            WHERE movto-estoq.nr-ord-produ = tt-om-leitura.nr-ord-produ
            AND   movto-estoq.esp-docto    = 28
            AND   movto-estoq.dt-trans    >= ttSelecao.dt-trans-ini 
            AND   movto-estoq.dt-trans    <= ttSelecao.dt-trans-fim
            AND   movto-estoq.cod-emitente > 0 NO-LOCK:

            FIND FIRST ord-manut WHERE ord-manut.nr-ord-produ = movto-estoq.nr-ord-produ
                                 AND   ord-manut.cd-equipto   = equipto.cd-equipto NO-LOCK NO-ERROR.

            IF NOT AVAIL ord-manut THEN NEXT blk_estoq.

            FIND FIRST tt-ord-manut WHERE tt-ord-manut.nr-ord-produ = movto-estoq.nr-ord-produ
                                    AND   tt-ord-manut.indicador    = YES NO-LOCK NO-ERROR.

            IF NOT AVAIL tt-ord-manut THEN DO:
                FIND FIRST tt-ord-manut WHERE tt-ord-manut.nr-ord-produ = movto-estoq.nr-ord-produ
                                        AND   tt-ord-manut.indicador    = NO NO-LOCK NO-ERROR.

                IF NOT AVAIL tt-ord-manut THEN DO:
                    RUN criaOrdemForaMI IN THIS-PROCEDURE.
                END.

                FOR FIRST item
                    WHERE item.it-codigo = movto-estoq.it-codigo NO-LOCK:
                    /** 1 - item.ind-serv-mat = Servico **/
                    IF item.ind-serv-mat <> 1 THEN NEXT blk_estoq.
                    ASSIGN d-val-servico = d-val-servico + (movto-estoq.valor-mat-m[1] * (IF movto-estoq.tipo-trans = 1 THEN -1 ELSE 1)).
                END.
            END.
        END.
    END.

    /** GGF **/
    IF ttSelecao.lGGF THEN DO:
        ASSIGN dGGF = 0.

        blk_ggf:
        FOR EACH  movto-ggf FIELDS(valor-ggf-1-m valor-ggf-2-m valor-ggf-3-m nr-ord-produ valor-ggf-4-m valor-ggf-5-m valor-ggf-6-m horas-report tipo-trans dt-trans)
            WHERE movto-ggf.nr-ord-produ = tt-om-leitura.nr-ord-produ
            AND   movto-ggf.dt-trans    >= ttSelecao.dt-trans-ini 
            AND   movto-ggf.dt-trans    <= ttSelecao.dt-trans-fim NO-LOCK:

            FIND FIRST ord-manut WHERE ord-manut.nr-ord-produ = movto-ggf.nr-ord-produ
                                 AND   ord-manut.cd-equipto   = equipto.cd-equipto NO-LOCK NO-ERROR.

            IF NOT AVAIL ord-manut THEN NEXT blk_ggf.

            FIND FIRST tt-ord-manut WHERE tt-ord-manut.nr-ord-produ = movto-ggf.nr-ord-produ
                                    AND   tt-ord-manut.indicador    = YES NO-LOCK NO-ERROR.

            IF NOT AVAIL tt-ord-manut THEN DO:
                FIND FIRST tt-ord-manut WHERE tt-ord-manut.nr-ord-produ = movto-ggf.nr-ord-produ
                                        AND   tt-ord-manut.indicador    = NO NO-LOCK NO-ERROR.

                IF NOT AVAIL tt-ord-manut THEN DO:
                    RUN criaOrdemForaMI IN THIS-PROCEDURE.
                END.

                ASSIGN dGGF = dGGF + ((movto-ggf.valor-ggf-1-m[1] + movto-ggf.valor-ggf-2-m[1] + movto-ggf.valor-ggf-3-m[1] +                                                
                                       movto-ggf.valor-ggf-4-m[1] + movto-ggf.valor-ggf-5-m[1] + movto-ggf.valor-ggf-6-m[1]) * (IF movto-ggf.tipo-trans = 1 THEN 1 ELSE -1)).
            END.
        END.
    END.
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piAcertaTamanho :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do i-coluna = 1 to n-colunas - 1:
    chWorksheet:columns( entry(i-coluna,c-alfabeto,",") + ":" + entry(i-coluna,c-alfabeto,",") ):EntireColumn:AutoFit.
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piAgrupaDados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var l-ok             as log      no-undo.
def var iUltPosicao      as integer  no-undo.
def var iPosicao1        as integer  no-undo.
def var iEstado          as integer  no-undo.
def input param iInicial as integer  no-undo.

for each ttComponente use-index ch-posicao:
    assign iEstado = 0.

    for first bfttComponente use-index ch-posicao
        where bfttComponente.posicao   > ttComponente.posicao
          and bfttComponente.sequencia = ttComponente.sequencia:
    end.
    if avail bfttComponente then do:
        assign iPosicao1 = bfttComponente.posicao - 1.

        if iPosicao1 = ttComponente.posicao then next.

        if can-find(first bfttComponente use-index ch-posicao
                    where bfttComponente.posicao   > ttComponente.posicao 
                      and bfttComponente.posicao  <= iPosicao1
                      and bfttComponente.sequencia < ttComponente.sequencia) then assign iEstado = 1.

        if iEstado = 1 then do:
            for first bfttComponente no-lock use-index ch-posicao
                where bfttComponente.posicao   > ttComponente.posicao
                  and bfttComponente.sequencia < ttComponente.sequencia:
            end.
            if avail bfttComponente then
                assign iPosicao1 = bfttComponente.posicao - 1.
            else do:
                for last bfttComponente use-index ch-posicao: end.

                if avail bfttComponente then
                    assign iPosicao1 = bfttComponente.posicao.
            end.
            if iPosicao1 = ttComponente.posicao then next.
            assign iEstado = 0.
        end.
        if iEstado = 1 then next.
    end.
    else do:
        for first bfttComponente no-lock use-index ch-posicao
            where bfttComponente.posicao   > ttComponente.posicao
              and bfttComponente.sequencia < ttComponente.sequencia:
        end.

        if avail bfttComponente then
            assign iPosicao1 = bfttComponente.posicao - 1.
        else do:
            for last bfttComponente use-index ch-posicao: end.

            if avail bfttComponente then
                assign iPosicao1 = bfttComponente.posicao.
        end.
        if iPosicao1 = ttComponente.posicao then next.
    end.

    assign l-ok = chWorksheet:Range("A" + string(ttComponente.posicao + 1 + iInicial) + ":" + entry(n-colunas,c-alfabeto,",") + string(iPosicao1 + iInicial)):group(1) no-error.
end.
assign l-ok = chWorksheet:OutLine:ShowLevels(1,0).

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piAtualizar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /** Busca Parƒmetros **/
    FIND FIRST ttSelecao NO-LOCK NO-ERROR.
    IF NOT AVAIL ttSelecao THEN RETURN "NOK":U.

    EMPTY TEMP-TABLE ttVisao.
    /** Cria Visäes 1 - Modelo e 2 - Equipamento **/
    RUN piCriaVisao IN THIS-PROCEDURE.

    /** Guarda a £ltima visÆo **/
    ASSIGN iVisao = 0.
    FOR LAST ttVisao NO-LOCK:
        ASSIGN iVisao = ttVisao.sequencia.
    END.

    /** N£mero de dias do per¡odo selecionado **/
    ASSIGN i-nr-dias = ttSelecao.periodo-fim - ttSelecao.periodo-ini.
    IF i-nr-dias = 0 THEN
        ASSIGN i-nr-dias = 1.
    ELSE
        ASSIGN i-nr-dias = i-nr-dias + 1.

    EMPTY TEMP-TABLE ttAux.
    EMPTY TEMP-TABLE ttDados.
    EMPTY TEMP-TABLE tt-disp.
    EMPTY TEMP-TABLE tt-tag.
    EMPTY TEMP-TABLE tt-ord-manut.
    EMPTY TEMP-TABLE tt-apont.
    EMPTY TEMP-TABLE tt-horas-aux.

    open query brDetalhe for each ttDados no-lock.
    /** Limpa o tree-view **/
    chTreeList:Nodes:CLEAR().

blk-principal:
DO ON STOP UNDO, RETURN "NOK":U:
    /** Acompanhamento **/
    RUN utp~/ut-acomp.p PERSISTENT SET hAcomp.
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Buscando",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    RUN pi-inicializar IN hAcomp (TRIM(RETURN-VALUE)).

    FOR EACH  mab-eqpto FIELDS (ep-codigo cod-eqpto cod-model cd-tag cod-estabel cod-sit-eqpto)
        WHERE mab-eqpto.ep-codigo      >= ttSelecao.empresa-ini 
        AND   mab-eqpto.ep-codigo      <= ttSelecao.empresa-fim 
        AND   mab-eqpto.cod-eqpto      >= ttSelecao.equipto-ini 
        AND   mab-eqpto.cod-eqpto      <= ttSelecao.equipto-fim 
        AND   mab-eqpto.cod-model      >= ttSelecao.modelo-ini
        AND   mab-eqpto.cod-model      <= ttSelecao.modelo-fim
        AND   mab-eqpto.cod-estrut-mec >= ttSelecao.estrut-ini
        AND   mab-eqpto.cod-estrut-mec <= ttSelecao.estrut-fim
        AND   mab-eqpto.cod-grp-eqpto  >= ttSelecao.grupo-ini
        AND   mab-eqpto.cod-grp-eqpto  <= ttSelecao.grupo-fim
        AND   mab-eqpto.vli-ano-fabric >= ttSelecao.ano-fabric-ini 
        AND   mab-eqpto.vli-ano-fabric <= ttSelecao.ano-fabric-fim 
        AND   mab-eqpto.cd-tag         >= ttSelecao.tag-ini
        AND   mab-eqpto.cd-tag         <= ttSelecao.tag-fim  NO-LOCK:

        /** Considera somente Eqptos 'Ativos' (idi-tip = 1) **/
        FOR FIRST mab-sit-eqpto
            WHERE mab-sit-eqpto.cod-sit-eqpto = mab-eqpto.cod-sit-eqpto NO-LOCK:
        END.        
        IF AVAIL mab-sit-eqpto AND mab-sit-eqpto.idi-tip = 2 THEN NEXT.

        /** Acompanhamento **/
        /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Equipamento",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        RUN pi-Acompanhar IN hAcomp (RETURN-VALUE + " : " + STRING(mab-eqpto.ep-codigo) + "-" + mab-eqpto.cod-eqpto).

        FOR FIRST mab-model-esp FIELDS (fator-conver)
            WHERE mab-model-esp.cod-model = mab-eqpto.cod-model NO-LOCK:
        END.
        FOR FIRST mab-model FIELD (cod-model des-model)
            WHERE mab-model.cod-model = mab-eqpto.cod-model NO-LOCK:
        END.
        
        /** Calcula Horas de Operacao do Eqpto **/
        RUN piHorasOperacao IN THIS-PROCEDURE.

        CREATE ttAux.
        ASSIGN ttAux.ep-codigo           = mab-eqpto.ep-codigo      
               ttAux.cod-eqpto           = mab-eqpto.cod-eqpto      
               ttAux.cod-modelo          = mab-eqpto.cod-model      
               ttAux.descricao           = mab-model.des-model      
               ttAux.cod-estabel         = mab-eqpto.cod-estabel
               ttAux.cd-tag              = mab-eqpto.cd-tag
               ttAux.hr-manut-corre-fin  = 0
               ttAux.hr-manut-prev       = 0
               ttAux.hr-manut-corre      = 0
               ttAux.hr-manut-pred       = 0
               ttAux.hr-manut-corre-plan = 0
               ttAux.mat                 = 0
               ttAux.ggf                 = 0
               ttAux.serv                = 0
               ttAux.nr-dias             = i-nr-dias
               ttAux.mat                 = 0
               ttAux.ggf                 = 0 
               ttAux.serv                = 0 
               ttAux.contratos           = 0 
               ttAux.hr-op-eqpto         = 0               
               ttAux.contratos           = 0
               ttAux.hr-op-eqpto         = deKM2 - deKM
               ttAux.fator-conver        = IF AVAIL mab-model-esp THEN mab-model-esp.fator-conver ELSE 1.

        FOR EACH mmv-ord-manut FIELDS (dat-abert nr-ord-produ estado hra-entr hra-prev-term hra-term dat-entr cd-tipo cod-estabel cod-eqpto dat-prev-term dat-term ep-codigo cod-eqpto)
            WHERE mmv-ord-manut.ep-codigo    = mab-eqpto.ep-codigo
            AND   mmv-ord-manut.cod-eqpto    = mab-eqpto.cod-eqpto
            AND   mmv-ord-manut.cod-estabel >= ttSelecao.estab-ini
            AND   mmv-ord-manut.cod-estabel <= ttSelecao.estab-fim
            AND   mmv-ord-manut.cc-codigo   >= ttSelecao.cc-ini
            AND   mmv-ord-manut.cc-codigo   <= ttSelecao.cc-fim NO-LOCK:

            /** Verifica se estado da ordem ‚ finalizada ou terminda**/
            IF mmv-ord-manut.estado > 6 THEN DO:
                ASSIGN dtFimOrdem    = mmv-ord-manut.dat-term
                       cHoraFimOrdem = mmv-ord-manut.hra-term.
            END.
            ELSE DO:
                ASSIGN dtFimOrdem    = mmv-ord-manut.dat-prev-term
                       cHoraFimOrdem = mmv-ord-manut.hra-prev-term.
            END.

            /** Valida‡Æo para saber se a ordem estava em aberto no per¡odo selecionado **/
            IF mmv-ord-manut.dat-abert < ttSelecao.periodo-ini  and
               dtFimOrdem              < ttSelecao.periodo-ini  THEN NEXT.

            IF mmv-ord-manut.dat-abert > ttSelecao.periodo-fim AND 
               dtFimOrdem              > ttSelecao.periodo-fim THEN NEXT.

            IF mmv-ord-manut.dat-entr < ttSelecao.periodo-ini THEN DO:
                ASSIGN dtInicioOrdem    = ttSelecao.periodo-ini
                       cHoraInicioOrdem = "000000".
            END.
            ELSE DO:
                ASSIGN dtInicioOrdem    = mmv-ord-manut.dat-entr
                       cHoraInicioOrdem = mmv-ord-manut.hra-entr.
            END.
            
            /** Se a ordem estiver em andamento ou estiverem al‚m da faixa de selecÆo os campos finais assumem outros valores **/
/*             IF mmv-ord-manut.estado < 7 THEN DO: */
               IF dtFimOrdem > ttSelecao.periodo-fim THEN 
                    ASSIGN dtFimOrdem    = IF dtFimOrdem > TODAY THEN TODAY ELSE  ttSelecao.periodo-fim
                           cHoraFimOrdem = IF dtFimOrdem > TODAY THEN SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) + SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2) + SUBSTRING(STRING(TIME,"HH:MM:SS"),7,2) ELSE  "235959".

                IF dtFimOrdem >= TODAY THEN
                    ASSIGN dtFimOrdem    = TODAY 
                           cHoraFimOrdem = SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) + SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2) + SUBSTRING(STRING(TIME,"HH:MM:SS"),7,2).

            /** Limpa variaveis **/
            RUN piZeraVar IN THIS-PROCEDURE.

            /** Valores para Ordem **/
            IF ttSelecao.lGGF OR ttSelecao.lMat OR ttSelecao.lServ THEN
                
               /*  /* Somente entrar  as Ordens que estiverem dentro da faixa de sele‡Æo para valoriza‡Æo dos Custos  */            */
               /*  IF mmv-ord-manut.dat-abert >= ttSelecao.periodo-ini AND                                                          */
               /*     mmv-ord-manut.dat-abert <= ttSelecao.periodo-fim THEN DO:                                                     */
                
                    FOR EACH mmv-movto-ord FIELDS(val-nota-corren val-serv-corren val-pneu-corren val-item-corren nr-ord-produ
                                                  mmv-movto-ord.val-ggf-1-corren mmv-movto-ord.val-ggf-2-corren mmv-movto-ord.val-ggf-3-corren
                                                  mmv-movto-ord.val-ggf-4-corren mmv-movto-ord.val-ggf-5-corren mmv-movto-ord.val-ggf-6-corren)
                        WHERE mmv-movto-ord.nr-ord-produ = mmv-ord-manut.nr-ord-produ  
                        AND   mmv-movto-ord.dat-movto   >= ttSelecao.dt-trans-ini
                        AND   mmv-movto-ord.dat-movto   <= ttSelecao.dt-trans-fim NO-LOCK:
                        
                        IF ttSelecao.lMat THEN 
                            ASSIGN d-val-mat = d-val-mat + mmv-movto-ord.val-nota-corren + mmv-movto-ord.val-pneu-corren + mmv-movto-ord.val-item-corren.
                        IF ttSelecao.lServ THEN
                            ASSIGN d-val-servico = d-val-servico + mmv-movto-ord.val-serv-corren.
                        IF ttSelecao.lGGF THEN
                            ASSIGN dGGF = dGGF + mmv-movto-ord.val-ggf-1-corren + mmv-movto-ord.val-ggf-2-corren + mmv-movto-ord.val-ggf-3-corren
                                               + mmv-movto-ord.val-ggf-4-corren + mmv-movto-ord.val-ggf-5-corren + mmv-movto-ord.val-ggf-6-corren.
                    END.
               /*  END.  */
                IF d-val-mat = 0 AND ttSelecao.lMatMesAnt THEN
                    RUN piMesAnterior IN THIS-PROCEDURE (INPUT mmv-ord-manut.nr-ord-produ).
                
            /** Horas de Manutencao - Encerramento da O.M **/
            IF ttSelecao.iTipoDispo = 1 THEN DO:
                
                /** Tabela de controle de Horas para os Apontamentos Simultaneos **/
                CREATE tt-apont.
                ASSIGN tt-apont.hra-inicial = cHoraInicioOrdem
                       tt-apont.hra-final   = cHoraFimOrdem
                       tt-apont.cod-model   = mab-eqpto.cod-model
                       tt-apont.dat-ini     = dtInicioOrdem
                       tt-apont.dat-fim     = dtFimOrdem
                       tt-apont.ep-codigo   = mab-eqpto.ep-codigo
                       tt-apont.cod-eqpto   = mab-eqpto.cod-eqpto.
            END.
            ELSE DO:
                FOR EACH mmv-movto-mdo
                    WHERE mmv-movto-mdo.nr-ord-produ = mmv-ord-manut.nr-ord-produ NO-LOCK:

                    /** Tabela de controle de Horas para os Apontamentos Simultaneos **/
                    CREATE tt-apont.
                    ASSIGN tt-apont.hra-inicial = mmv-movto-mdo.hra-inicial
                           tt-apont.hra-final   = mmv-movto-mdo.hra-final
                           tt-apont.cod-model   = mab-eqpto.cod-model
                           tt-apont.dat-ini     = mmv-movto-mdo.dat-movto
                           tt-apont.dat-fim     = mmv-movto-mdo.dat-movto
                           tt-apont.ep-codigo   = mab-eqpto.ep-codigo
                           tt-apont.cod-eqpto   = mab-eqpto.cod-eqpto.
                END.
            END.            

            /** Acompanhamento **/
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Equipamento",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
            RUN pi-Acompanhar IN hAcomp (RETURN-VALUE + " : " + STRING(mab-eqpto.ep-codigo) + "-" + mab-eqpto.cod-eqpto).

            /** Busca tipo da manutencao **/
            FOR FIRST tipo-manut FIELDS (cd-tipo tipo)
                WHERE tipo-manut.cd-tipo = mmv-ord-manut.cd-tipo NO-LOCK:
            END.
            FOR FIRST mmv-tipo-manut FIELDS (corret-plan)
                WHERE mmv-tipo-manut.tipo-manut = tipo-manut.cd-tipo NO-LOCK:
            END.

            /** Calcula as Horas de Manuten‡Æo **/
            RUN piCalculaHorasManut-FROTAS IN THIS-PROCEDURE.

            /** Contratos **/
            /*RUN piContratos IN THIS-PROCEDURE.*/

            FOR FIRST ttAux
                WHERE ttAux.ep-codigo = mab-eqpto.ep-codigo
                AND   ttAux.cod-eqpto = mab-eqpto.cod-eqpto NO-LOCK:
                ASSIGN ttAux.nr-inter-corre      = ttAux.nr-inter-corre      + IF tipo-manut.tipo = 2 THEN 1 ELSE 0
                       ttAux.nr-corre-fin        = ttAux.nr-corre-fin        + IF tipo-manut.tipo = 2 AND mmv-ord-manut.estado > 6 THEN 1 ELSE 0
                       ttAux.hr-manut-corre-fin  = ttAux.hr-manut-corre-fin  + d-tot-corre-fin  /** Horas de Manuten‡Æo Corretiva do Equipamento - OM Finalizadas **/
                       ttAux.hr-manut-prev       = ttAux.hr-manut-prev       + d-tot-prev       /** Horas de Manuten‡Æo Preventiva do Equipamento                 **/
                       ttAux.hr-manut-corre      = ttAux.hr-manut-corre      + d-tot-corre      /** Horas de Manuten‡Æo Corretiva do Equipamento                  **/
                       ttAux.hr-manut-pred       = ttAux.hr-manut-pred       + d-tot-pred       /** Horas de Manuten‡Æo Preditiva do Equipamento                  **/
                       ttAux.hr-manut-corre-plan = ttAux.hr-manut-corre-plan + d-tot-corre-plan /** Horas de Manuten‡Æo Corretiva Planejadas do Equipamento       **/                           
                       ttAux.mat                 = ttAux.mat                 + d-val-mat
                       ttAux.ggf                 = ttAux.ggf                 + dGGF
                       ttAux.serv                = ttAux.serv                + d-val-servico.
            END.

            /** Cria temp-table com todas as Ordens que entraram no filtro **/
            CREATE tt-ord-manut.
            ASSIGN tt-ord-manut.nr-ord-produ = mmv-ord-manut.nr-ord-produ
                   tt-ord-manut.estado       = mmv-ord-manut.estado      
                   tt-ord-manut.cod-eqpto    = mmv-ord-manut.cod-eqpto   
                   tt-ord-manut.dt-manut     = mmv-ord-manut.dat-entr   
                   tt-ord-manut.dt-fecham    = mmv-ord-manut.dat-term
                   tt-ord-manut.indicador    = YES.
            
        END.

        /** Zera as vari veis **/
        ASSIGN d-val-mat     = 0
               dGGF          = 0
               d-val-servico = 0.
        
        FOR EACH mmv-movto-ord FIELDS(val-nota-corren val-serv-corren val-pneu-corren val-item-corren nr-ord-produ    dat-movto                        
                                      mmv-movto-ord.val-ggf-1-corren mmv-movto-ord.val-ggf-2-corren mmv-movto-ord.val-ggf-3-corren           
                                      mmv-movto-ord.val-ggf-4-corren mmv-movto-ord.val-ggf-5-corren mmv-movto-ord.val-ggf-6-corren)          
            WHERE mmv-movto-ord.ep-codigo    = mab-eqpto.ep-codigo
            AND   mmv-movto-ord.cod-eqpto    = mab-eqpto.cod-eqpto
            AND   mmv-movto-ord.dat-movto   >= ttSelecao.dt-trans-ini                                                                        
            AND   mmv-movto-ord.dat-movto   <= ttSelecao.dt-trans-fim NO-LOCK: 

            FOR FIRST tt-ord-manut
                WHERE tt-ord-manut.nr-ord-produ = mmv-movto-ord.nr-ord-produ 
                AND   tt-ord-manut.indicador    = YES NO-LOCK:
            END.

            IF NOT AVAIL tt-ord-manut THEN DO:
                
                FOR FIRST tt-ord-manut
                    WHERE tt-ord-manut.nr-ord-produ = mmv-movto-ord.nr-ord-produ 
                    AND   tt-ord-manut.indicador    = NO                         NO-LOCK:
                END.

                IF NOT AVAIL tt-ord-manut THEN DO:
                    RUN criaOrdemForaDisponibilidade.
                END.
            
                IF ttSelecao.lMat THEN                                                                                                           
                    ASSIGN d-val-mat = d-val-mat + mmv-movto-ord.val-nota-corren + mmv-movto-ord.val-pneu-corren + mmv-movto-ord.val-item-corren.
                IF ttSelecao.lServ THEN                                                                                                          
                    ASSIGN d-val-servico = d-val-servico + mmv-movto-ord.val-serv-corren.                                                        
                IF ttSelecao.lGGF THEN                                                                                                           
                    ASSIGN dGGF = dGGF + mmv-movto-ord.val-ggf-1-corren + mmv-movto-ord.val-ggf-2-corren + mmv-movto-ord.val-ggf-3-corren        
                                       + mmv-movto-ord.val-ggf-4-corren + mmv-movto-ord.val-ggf-5-corren + mmv-movto-ord.val-ggf-6-corren.       
                
            END.
        END.
        IF d-val-mat = 0 AND ttSelecao.lMatMesAnt THEN                              
            RUN piMesAnterior IN THIS-PROCEDURE (INPUT mmv-ord-manut.nr-ord-produ). 
        
        /** pega a soma das vari veis para esse equipamento dos movimentos no periodo, por‚m as ordens estavam fora **/
        FOR FIRST ttAux                                                                  
            WHERE ttAux.ep-codigo = mab-eqpto.ep-codigo                                  
            AND   ttAux.cod-eqpto = mab-eqpto.cod-eqpto NO-LOCK:                         
            ASSIGN ttAux.mat                 = ttAux.mat                 + d-val-mat     
                   ttAux.ggf                 = ttAux.ggf                 + dGGF          
                   ttAux.serv                = ttAux.serv                + d-val-servico.
        END.                                                                             
    END.

    /** Terceira Fase da Gera‡Æo **/
    RUN piCriaTTDados IN THIS-PROCEDURE.

    IF VALID-HANDLE(hAcomp) THEN
        RUN pi-finalizar IN hAcomp.

    /** Atualiza o Display do TreeList **/
    RUN piFormataTreeList IN THIS-PROCEDURE.

    /** Mostra o tree-list em tela **/
    RUN criaTreeList IN THIS-PROCEDURE.
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piAtualizarMI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/** Busca Parƒmetros **/
FIND FIRST ttSelecao NO-LOCK NO-ERROR.
IF NOT AVAIL ttSelecao THEN RETURN "NOK":U.

EMPTY TEMP-TABLE ttVisao.
/** Cria Visäes 1 - TAG e 2 - Equipamento **/
RUN piCriaVisao IN THIS-PROCEDURE.

/** Guarda a £ltima visÆo **/
ASSIGN iVisao = 0.
FOR LAST ttVisao NO-LOCK:
    ASSIGN iVisao = ttVisao.sequencia.
END.
/** N£mero de dias do per¡odo selecionado **/
ASSIGN i-nr-dias = ttSelecao.periodo-fim - ttSelecao.periodo-ini.
IF i-nr-dias = 0 THEN ASSIGN i-nr-dias = 1.
ELSE ASSIGN i-nr-dias = i-nr-dias + 1.

EMPTY TEMP-TABLE ttAux.
EMPTY TEMP-TABLE ttDados.
EMPTY TEMP-TABLE tt-disp.
EMPTY TEMP-TABLE tt-tag.
EMPTY TEMP-TABLE tt-ord-manut.
EMPTY TEMP-TABLE tt-apont.
EMPTY TEMP-TABLE tt-horas-aux.

open query brDetalhe for each ttDados no-lock.
/** Limpa o tree-view **/
chTreeList:Nodes:CLEAR().

EMPTY TEMP-TABLE ttModelo.

blk-principal:
DO ON STOP UNDO, RETURN "NOK":U:
    /** Acompanhamento **/
    RUN utp\ut-acomp.p PERSISTENT SET hAcomp.
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Buscando",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    RUN pi-inicializar IN hAcomp (TRIM(RETURN-VALUE)).     

    /** Avisa que ‚ o primeiro TAG e o valor do TAG inicial e final passado **/
    /** para o ESMI0603 assumir  o primeiro valor de TAG encontrado         **/
    ASSIGN lPrimeiroValorTag = YES.

    FOR EACH  equipto FIELDS (cd-tag ep-codigo cd-equipto descricao fm-equipto cod-estabel)
        WHERE equipto.ep-codigo  >= ttSelecao.empresa-ini 
        AND   equipto.ep-codigo  <= ttSelecao.empresa-fim 
        AND   equipto.cd-equipto >= ttSelecao.equipto-ini 
        AND   equipto.cd-equipto <= ttSelecao.equipto-fim 
        AND   equipto.fm-equipto >= ttSelecao.modelo-ini
        AND   equipto.fm-equipto <= ttSelecao.modelo-fim
        AND   equipto.cd-tag     >= ttSelecao.tag-ini
        AND   equipto.cd-tag     <= ttSelecao.tag-fim 
        AND   equipto.cc-codigo  >= ttSelecao.cc-ini
        AND   equipto.cc-codigo  <= ttSelecao.cc-fim 
        AND   equipto.situacao    = 1 NO-LOCK:

        RUN piTag (INPUT equipto.cd-tag, INPUT ttSelecao.iNivTag, INPUT 999, OUTPUT TABLE tt-estrut-tag).

        FOR FIRST tt-estrut-tag
            WHERE tt-estrut-tag.cod-tipo = ttSelecao.iNivTag:
        END.
        IF NOT AVAIL tt-estrut-tag THEN NEXT.

        /** Acompanhamento **/
        RUN pi-Acompanhar IN hAcomp (STRING(equipto.ep-codigo) + "-" + equipto.cd-equipto).

        /** Calcula Horas de Operacao do Eqpto **/
        RUN piHorasOperacao IN THIS-PROCEDURE.

        CREATE ttAux.
        ASSIGN ttAux.ep-codigo           = equipto.ep-codigo
               ttAux.cod-eqpto           = equipto.cd-equipto
               ttAux.cod-modelo          = equipto.fm-equipto
               ttAux.descricao           = equipto.descricao
               ttAux.cod-estabel         = equipto.cod-estabel
               ttAux.cd-tag              = tt-estrut-tag.cd-tag
               ttAux.nr-inter-corre      = 0
               ttAux.nr-corre-fin        = 0
               ttAux.hr-manut-corre-fin  = 0
               ttAux.hr-manut-prev       = 0
               ttAux.hr-manut-corre      = 0
               ttAux.hr-manut-pred       = 0
               ttAux.hr-manut-corre-plan = 0
               ttAux.nr-dias             = i-nr-dias
               ttAux.mat                 = 0
               ttAux.ggf                 = 0
               ttAux.serv                = 0
               ttAux.contratos           = 0
               ttAux.hr-op-eqpto         = deHoras
               ttAux.hr-op-tag           = deHorasTag.

        
        FOR EACH ord-manut FIELDS(dt-manut nr-ord-produ estado dt-parada dt-retorno hr-parada hr-retorno data-1 data-2 INT-1 INT-2 
                                  ep-codigo dt-manut dt-prev dt-prev-manut cd-tipo cod-estabel cd-equipto des-man-corr dt-fecham)
        
            WHERE ord-manut.cd-equipto   = equipto.cd-equipto
            /* AND   ord-manut.dt-manut    >= ttSelecao.periodo-ini   */
            /* AND   ord-manut.dt-manut    <= ttSelecao.periodo-fim   */
            AND   ord-manut.cod-estabel >= ttSelecao.estab-ini
            AND   ord-manut.cod-estabel <= ttSelecao.estab-fim NO-LOCK:
            
            /** Valida o estado da ordem e conforme valiza‡Æo carrega campos considerados como fim da ordem **/
            IF ord-manut.estado > 6 THEN DO:
                
                /** LOGICA PARA A RELEASE 206 **/                            
                ASSIGN dtInicioOrdem     = ord-manut.dt-parada                 
                       dtFimOrdem        = ord-manut.dt-retorno                
                       cHoraInicioOrdem  = STRING(ord-manut.hr-parada,"9999")  
                       cHoraFimOrdem     = STRING(ord-manut.hr-retorno,"9999").
                
                IF dtInicioOrdem = ? OR dtFimOrdem = ? THEN            
                    ASSIGN dtInicioOrdem = ord-manut.dt-manut              
                           dtFimOrdem    = ord-manut.dt-prev.              
                                                                             
                IF INT(cHoraFimOrdem) = 0 THEN                               
                    ASSIGN cHoraFimOrdem = "2359":U .                        
            END.
            ELSE DO:
                ASSIGN cHoraFimOrdem = SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) + SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2) + SUBSTRING(STRING(TIME,"HH:MM:SS"),7,2).
                ASSIGN dtFimOrdem     = IF TODAY < ord-manut.dt-prev THEN TODAY ELSE ord-manut.dt-prev
                       cHoraFimOrdem  = IF TODAY < ord-manut.dt-prev THEN cHoraFimOrdem ELSE "2359":U .

                ASSIGN dtInicioOrdem    = ord-manut.dt-manut
                       cHoraInicioOrdem = "000000".         
                
                /** Valida Periodo **/
                IF ttSelecao.periodo-fim < dtFimOrdem THEN DO:
                    ASSIGN dtFimOrdem    = ttSelecao.periodo-fim
                           cHoraFimOrdem = "2359":U.
                END.
            END.
            
            /** Filtro para manter apenas ordens que estÆo ou estiveram aberta no periodo selecionado **/
            IF dtInicioOrdem < ttSelecao.periodo-ini  and
               dtFimOrdem    < ttSelecao.periodo-ini  THEN NEXT.

            IF dtInicioOrdem > ttSelecao.periodo-fim THEN NEXT.

            IF dtInicioOrdem < ttSelecao.periodo-ini THEN DO:
                ASSIGN dtInicioOrdem    = ttSelecao.periodo-ini
                       cHoraInicioOrdem = "000000".
            END.
                      

            /** Tabela especifica para verificar se a OM ‚ de "Area Parada" **/
            FOR FIRST ext-ord-manut FIELDS (parada)
                WHERE ext-ord-manut.nr-ord-produ = ord-manut.nr-ord-produ NO-LOCK:
            END.
            /** Limpa variaveis **/
            RUN piZeraVar IN THIS-PROCEDURE.
            
            /** Busca os valores de Material da Ordem **/
            IF ttSelecao.lMat THEN DO:                
                FOR EACH movto-mat FIELDS(nr-ord-produ dt-trans esp-docto tipo-trans valor-mat-m it-codigo) USE-INDEX ordem
                    WHERE movto-mat.nr-ord-produ = ord-manut.nr-ord-produ
                    AND   movto-mat.dt-trans    >= ttSelecao.dt-trans-ini 
                    AND   movto-mat.dt-trans    <= ttSelecao.dt-trans-fim NO-LOCK:
                    IF  movto-mat.esp-docto <> 28 AND movto-mat.esp-docto <> 30 AND 
                        movto-mat.esp-docto <> 20 AND movto-mat.esp-docto <> 21 THEN NEXT.
                    FOR FIRST item
                        WHERE item.it-codigo = movto-mat.it-codigo NO-LOCK:
                        /** 2 - item.ind-serv-mat = Material **/
                        IF item.ind-serv-mat <> 2 THEN NEXT.
                        ASSIGN d-val-mat = d-val-mat + (movto-mat.valor-mat-m[1] * (IF movto-mat.tipo-trans = 1 THEN -1 ELSE 1)).
                    END.
                END.
                IF d-val-mat = 0 AND ttSelecao.lMatMesAnt THEN
                    RUN piMesAnterior IN THIS-PROCEDURE (INPUT ord-manut.nr-ord-produ).
            END.
            /** Busca os valores de Servi‡o da Ordem **/
            IF ttSelecao.lServ THEN DO:                
                FOR EACH movto-estoq FIELDS(esp-docto dt-trans cod-emitente nr-ord-produ it-codigo valor-mat-m tipo-trans) USE-INDEX esp-data 
                    WHERE movto-estoq.esp-docto     = 28
                    AND   movto-estoq.dt-trans     >= ttSelecao.dt-trans-ini 
                    AND   movto-estoq.dt-trans     <= ttSelecao.dt-trans-fim
                    AND   movto-estoq.cod-emitente  > 0
                    AND   movto-estoq.nr-ord-produ  = ord-manut.nr-ord-produ NO-LOCK:
                    FOR FIRST item
                        WHERE item.it-codigo = movto-estoq.it-codigo NO-LOCK:
                        /** 1 - item.ind-serv-mat = Servico **/
                        IF item.ind-serv-mat <> 1 THEN NEXT.
                        ASSIGN d-val-servico = d-val-servico + (movto-estoq.valor-mat-m[1] * (IF movto-estoq.tipo-trans = 1 THEN -1 ELSE 1)).
                    END.
                END.
            END.
            /** GGF **/
            IF ttSelecao.lGGF THEN DO:               
                ASSIGN dGGF = 0.
                FOR EACH movto-ggf FIELDS(valor-ggf-1-m valor-ggf-2-m valor-ggf-3-m nr-ord-produ valor-ggf-4-m valor-ggf-5-m valor-ggf-6-m horas-report tipo-trans dt-trans)
                    WHERE movto-ggf.nr-ord-produ = ord-manut.nr-ord-produ
                    AND   movto-ggf.dt-trans    >= ttSelecao.dt-trans-ini 
                    AND   movto-ggf.dt-trans    <= ttSelecao.dt-trans-fim NO-LOCK:
                    ASSIGN dGGF = dGGF + ((movto-ggf.valor-ggf-1-m[1] + movto-ggf.valor-ggf-2-m[1] + movto-ggf.valor-ggf-3-m[1] +
                                           movto-ggf.valor-ggf-4-m[1] + movto-ggf.valor-ggf-5-m[1] + movto-ggf.valor-ggf-6-m[1]) * (IF movto-ggf.tipo-trans = 1 THEN 1 ELSE -1)).
                END.
            END.
            /** Acumula o total de Horas de Manutencao da Ordem **/
            IF ttSelecao.iTipoDispo = 2 THEN DO:
                FOR EACH  ord-mob FIELDS(nr-ord-produ horas-report tipo-trans dt-trans hora-inicio hora-termino)
                    WHERE ord-mob.nr-ord-produ = ord-manut.nr-ord-produ
                    AND   ord-mob.dt-trans    >= ttSelecao.dt-trans-ini 
                    AND   ord-mob.dt-trans    <= ttSelecao.dt-trans-fim NO-LOCK:

                    ASSIGN c-hr-hora-ini = SUBSTRING(ord-mob.hora-inicio,1,2)  + SUBSTRING(ord-mob.hora-inicio,4,2)  + SUBSTRING(ord-mob.hora-inicio,7,2)
                           c-hr-hora-fim = SUBSTRING(ord-mob.hora-termino,1,2) + SUBSTRING(ord-mob.hora-termino,4,2) + SUBSTRING(ord-mob.hora-termino,7,2).

                    /** Tabela de controle de Horas para os Apontamentos Simultaneos **/
                    CREATE tt-apont.
                    ASSIGN tt-apont.hra-inicial = c-hr-hora-ini
                           tt-apont.hra-final   = c-hr-hora-fim
                           tt-apont.cod-model   = tt-estrut-tag.cd-tag
                           tt-apont.dat-ini     = ord-mob.dt-trans
                           tt-apont.dat-fim     = ord-mob.dt-trans
                           tt-apont.ep-codigo   = ord-manut.ep-codigo
                           tt-apont.cod-eqpto   = ord-manut.cd-equipto
                           tt-apont.parada      = IF AVAIL ext-ord-manut THEN ext-ord-manut.parada ELSE NO. /** [X] Area Parada - MI0307 **/                
                END.
            END.
            ELSE DO:
                IF ord-manut.estado > 6 THEN DO:
                    
                    /** Tabela de controle de Horas para os Apontamentos Simultaneos **/
                    CREATE tt-apont.
                    ASSIGN tt-apont.hra-inicial = cHoraInicioOrdem
                           tt-apont.hra-final   = cHoraFimOrdem
                           tt-apont.cod-model   = tt-estrut-tag.cd-tag
                           tt-apont.dat-ini     = dtInicioOrdem
                           tt-apont.dat-fim     = dtFimOrdem
                           tt-apont.ep-codigo   = ord-manut.ep-codigo
                           tt-apont.cod-eqpto   = ord-manut.cd-equipto
                           tt-apont.parada      = IF AVAIL ext-ord-manut THEN ext-ord-manut.parada ELSE NO. /** [X] Area Parada - MI0307 **/  
                END.
                ELSE DO:
                    /** Tabela de controle de Horas para os Apontamentos Simultaneos **/
                    CREATE tt-apont.
                    ASSIGN tt-apont.hra-inicial = "0000":U
                           tt-apont.hra-final   = cHoraFimOrdem
                           tt-apont.cod-model   = tt-estrut-tag.cd-tag
                           tt-apont.dat-ini     = ord-manut.dt-manut
                           tt-apont.dat-fim     = dtFimOrdem
                           tt-apont.ep-codigo   = ord-manut.ep-codigo
                           tt-apont.cod-eqpto   = ord-manut.cd-equipto
                           tt-apont.parada      = IF AVAIL ext-ord-manut THEN ext-ord-manut.parada ELSE NO. /** [X] Area Parada - MI0307 **/  
                     
                END.
            END.           

            /** Acompanhamento **/
            RUN pi-Acompanhar IN hAcomp (STRING(equipto.ep-codigo) + "-" + equipto.cd-equipto).

            /** Busca tipo da manutencao **/
            FOR FIRST tipo-manut FIELDS(cd-tipo tipo)
                WHERE tipo-manut.cd-tipo = ord-manut.cd-tipo NO-LOCK:
            END.
            FOR FIRST mmv-tipo-manut FIELDS(corret-plan)
                WHERE mmv-tipo-manut.tipo-manut = tipo-manut.cd-tipo NO-LOCK:
            END.

            /** Calcula as Horas de Manuten‡Æo **/
            RUN piCalculaHorasManut-MI IN THIS-PROCEDURE.

            /** Acumula os Indicadores por Equipamento **/
            FOR FIRST ttAux                                         
                WHERE ttAux.cod-eqpto = ord-manut.cd-equipto NO-LOCK:
                ASSIGN ttAux.nr-inter-corre      = ttAux.nr-inter-corre      + IF tipo-manut.tipo = 2 THEN 1 ELSE 0
                       ttAux.nr-corre-fin        = ttAux.nr-corre-fin        + IF tipo-manut.tipo = 2 AND ord-manut.estado > 6 THEN 1 ELSE 0
                       ttAux.hr-manut-corre-fin  = ttAux.hr-manut-corre-fin  + d-tot-corre-fin    /** Horas de Manuten‡Æo Corretiva do Equipamento - OM Finalizadas **/
                       ttAux.hr-manut-prev       = ttAux.hr-manut-prev       + d-tot-prev         /** Horas de Manuten‡Æo Preventiva do Equipamento                 **/
                       ttAux.hr-manut-corre      = ttAux.hr-manut-corre      + d-tot-corre        /** Horas de Manuten‡Æo Corretiva do Equipamento                  **/
                       ttAux.hr-manut-pred       = ttAux.hr-manut-pred       + d-tot-pred         /** Horas de Manuten‡Æo Preditiva do Equipamento                  **/
                       ttAux.hr-manut-corre-plan = ttAux.hr-manut-corre-plan + d-tot-corre-plan   /** Horas de Manuten‡Æo Corretiva Planejadas do Equipamento       **/
                       ttAux.mat                 = ttAux.mat                 + d-val-mat
                       ttAux.ggf                 = ttAux.ggf                 + dGGF
                       ttAux.serv                = ttAux.serv                + d-val-servico
                       ttAux.contratos           = ttAux.contratos           + d-val-proces.
            END.

            /** Cria temp-TABLE com todas as Ordens que entraram no filtro **/
            RUN piCriaTTOrd-manut IN THIS-PROCEDURE.  
        END.

        /** Teste **/                 
        RUN movimentosMiForaIndicador IN THIS-PROCEDURE.

        FOR FIRST ttAux                                                                  
            WHERE ttAux.cod-eqpto = equipto.cd-equipto NO-LOCK:                         
            ASSIGN ttAux.mat                 = ttAux.mat                 + d-val-mat     
                   ttAux.ggf                 = ttAux.ggf                 + dGGF          
                   ttAux.serv                = ttAux.serv                + d-val-servico.
        END.                                                                     
    END.

    /** Terceira Fase da Gera‡Æo **/
    RUN piCriaTTDados IN THIS-PROCEDURE.

    IF VALID-HANDLE (hAcomp) THEN
        DELETE PROCEDURE hAcomp.
        
    /** Atualiza o Display do TreeList **/
    RUN piFormataTreeList IN THIS-PROCEDURE.
    /** Mostra o tree-list em tela **/
    RUN criaTreeList IN THIS-PROCEDURE.
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piCalculaHorasManut-FROTAS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CASE tipo-manut.tipo:
        /** Preventiva **/
        WHEN 1 THEN DO:
            RUN calcHoras IN THIS-PROCEDURE (INPUT cHoraInicioOrdem, 
                                             INPUT cHoraFimOrdem   ,
                                             INPUT dtInicioOrdem   ,
                                             INPUT dtFimOrdem      ,
                                             OUTPUT d-tot-prev).
        END.
        /** Corretiva **/
        WHEN 2 THEN DO:
            /** Corretiva / Corretiva Planejada **/
            IF AVAIL mmv-tipo-manut AND mmv-tipo-manut.corret-plan THEN DO:
                RUN calcHoras IN THIS-PROCEDURE (INPUT cHoraInicioOrdem,
                                                 INPUT cHoraFimOrdem   ,
                                                 INPUT dtInicioOrdem   ,
                                                 INPUT dtFimOrdem      ,
                                                 OUTPUT d-tot-corre-plan).
            END.
            ELSE
               RUN calcHoras IN THIS-PROCEDURE (INPUT cHoraInicioOrdem,
                                                INPUT cHoraFimOrdem   ,
                                                INPUT dtInicioOrdem   ,
                                                INPUT dtFimOrdem      ,
                                                OUTPUT d-tot-corre).
              
          RUN calcHoras IN THIS-PROCEDURE (INPUT cHoraInicioOrdem,
                                           INPUT cHoraFimOrdem   ,
                                           INPUT dtInicioOrdem   ,
                                           INPUT dtFimOrdem      ,
                                           OUTPUT d-tot-corre-fin).
        END.
        /** Preditiva **/
        WHEN 3 THEN DO:
            RUN calcHoras IN THIS-PROCEDURE (INPUT cHoraInicioOrdem,
                                             INPUT cHoraFimOrdem   ,
                                             INPUT dtInicioOrdem   ,
                                             INPUT dtFimOrdem      ,
                                             OUTPUT d-tot-pred).
        END.
    END CASE.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piCalculaHorasManut-MI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hAcomp) THEN DO:
    RUN pi-Acompanhar IN hAcomp ("Calculando Horas Manuten‡Æo MI").
END.

CASE tipo-manut.tipo:
    /** Preventiva **/
    WHEN 1 THEN DO:
        RUN calcHoras IN THIS-PROCEDURE (INPUT "",
                                         INPUT "",
                                         INPUT dtInicioOrdem,
                                         INPUT dtFimOrdem,
                                         OUTPUT d-tot-prev).
    END.
    /** Corretiva **/
    WHEN 2 THEN DO:
        /** Corretiva / Corretiva Planejada **/
        IF AVAIL mmv-tipo-manut AND mmv-tipo-manut.corret-plan THEN DO:
            RUN calcHoras IN THIS-PROCEDURE (INPUT "",
                                             INPUT "",
                                             INPUT dtInicioOrdem,
                                             INPUT dtFimOrdem,
                                             OUTPUT d-tot-corre-plan).
        END.
        ELSE DO:
            RUN calcHoras IN THIS-PROCEDURE (INPUT "",
                                             INPUT "",
                                             INPUT dtInicioOrdem,
                                             INPUT dtFimOrdem,
                                             OUTPUT d-tot-corre).
        END.

        IF ord-manut.estado > 6 THEN DO:
            RUN calcHoras IN THIS-PROCEDURE (INPUT cHoraInicioOrdem,
                                             INPUT cHoraFimOrdem,
                                             INPUT dtInicioOrdem,
                                             INPUT dtFimOrdem,
                                             OUTPUT d-tot-corre-fin).
        END.
        ELSE DO:
            RUN calcHoras IN THIS-PROCEDURE (INPUT "",
                                             INPUT cHoraFimOrdem,
                                             INPUT dtInicioOrdem,
                                             INPUT dtFimOrdem,
                                             OUTPUT d-tot-corre-fin).
        END.
    END.
    /** Preditiva **/
    WHEN 3 THEN DO:
        RUN calcHoras IN THIS-PROCEDURE (INPUT "",
                                         INPUT "",
                                         INPUT dtInicioOrdem,
                                         INPUT dtFimOrdem,
                                         OUTPUT d-tot-pred).
    END.
END CASE.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piCarregaSequencias :
/*------------------------------------------------------------------------------
  Purpose:     piCarregaSequencias
  Parameters:  <none>
  notes:       Carrega a temp-table que ser  mostrada no tree-view e no browse
------------------------------------------------------------------------------*/
DEFINE VARIABLE validaMenorZero AS DECIMAL INITIAL 0  NO-UNDO.

    ASSIGN i-qt-eqpto = 0.

    /** Busca registro **/
    FIND FIRST ttDados
         WHERE ttDados.cod-dimensao  = (cCodPai + "#" + vCodigo)
         AND   ttDados.sequencia     = ttVisao.sequencia EXCLUSIVE-LOCK NO-ERROR.

    ASSIGN lMediaTagModelo = NO.
    /** Se nÆo estiver criado, cria registro **/
    IF NOT AVAIL ttDados THEN DO:
       ASSIGN lMediaTagModelo = YES.
       CREATE ttDados.
       ASSIGN ttDados.cod-dimensao   = (cCodPai + "#" + vCodigo)  /** C¢digo encadeado para tree-view **/
              ttDados.cod-oficial    = vCodigo                    /** C¢digo Original do registro **/
              ttDados.desc-dimensao  = vDescricao                 /** Descri‡Æo do c¢digo **/
              ttDados.sequencia      = ttVisao.sequencia          /** Sequˆncia dos dados **/
              ttDados.p-image        = iImage                     /** N£mero da imagem do registro **/
              ttDados.r-rowid        = rRowid                     /** Rowid do registro para consultas futuras **/
              ttDados.cod-dimens-pai = cCodPai                    /** C¢digo do pai do registro **/
              ttDados.cod-eqpto      = ttAux.cod-eqpto
              ttDados.cd-tag         = IF ttSelecao.iIndicador = 1 THEN ttAux.cod-modelo ELSE ttAux.cd-tag. /** Se for frotas guarda o modelo **/
    END.
    ASSIGN cCodPai = ttDados.cod-dimensao.

    ASSIGN dCalc       = 0
           dManutCorre = 0.

    /** Acumula Valores **/
    ASSIGN ttDados.nr-inter-corre       = ttDados.nr-inter-corre      + ttAux.nr-inter-corre
           ttDados.hr-manut-corre-fin   = ttDados.hr-manut-corre-fin  + ttAux.hr-manut-corre-fin
           ttDados.nr-corre-fin         = ttDados.nr-corre-fin        + ttAux.nr-corre-fin
           ttDados.hr-manut-prev        = ttDados.hr-manut-prev       + ttAux.hr-manut-prev      
           ttDados.hr-manut-pred        = ttDados.hr-manut-pred       + ttAux.hr-manut-pred      
           ttDados.hr-manut-corre-plan  = ttDados.hr-manut-corre-plan + ttAux.hr-manut-corre-plan
           ttDados.hr-manut-corre       = ttDados.hr-manut-corre      + ttAux.hr-manut-corre
           ttDados.hr-op-eqpto          = ttDados.hr-op-eqpto         + ttAux.hr-op-eqpto.

    ASSIGN dCalc       = ttDados.hr-manut-prev + ttDados.hr-manut-pred + ttDados.hr-manut-corre-plan
           dManutCorre = ttDados.hr-manut-corre.

    ASSIGN deHorasAux = ttAux.hr-op-eqpto.

    /** VisÆo Pai **/
    IF ttVisao.sequencia = 1 THEN DO:
        IF ttSelecao.iIndicador = 2 THEN DO:
            /** Calcula total de horas de manutencao para o Tag/Modelo considerando sobreposicao de horarios **/
            RUN calcHorasDia (INPUT 0, INPUT "", INPUT ttDados.cd-tag, INPUT 999, INPUT 01/01/1800). /* piCarregaSequencias */
            ASSIGN ttDados.hr-manut = dHorasDia.
            ASSIGN validaMenorZero = (24 * ttAux.nr-dias).
        END.
        ELSE DO:
            ASSIGN validaMenorZero = (24 * ttAux.nr-dias * i-qt-eqpto).
        END.
        
        /** MI **/
        IF ttSelecao.iIndicador = 2 THEN DO:
            /** Pega as horas de operacao referentes ao tag **/
            ASSIGN deHorasAux = ttAux.hr-op-tag.
            /* Defido com o cliente que apenas quando for frotas e horas encerradas   */
            /* ser  considerado a quantidade de equipamentos                          */
           
        END.
        /** Frotas **/
        ELSE DO:
            /** Pega o somatorio das horas de operacao dos equipamentos referentes ao modelo **/
            ASSIGN deHorasAux = ttDados.hr-op-eqpto.
        END.

        /** A visÆo sendo a do Modelo/Tag o sistema calcula a m‚dia multiplicando pelo n£mero de Equipamentos **/
        /** Valida‡Æo conforme o cliente deseja realizar a valida‡Æo do horas por encerramento e para frotas  **/
/*         IF ttSelecao.iIndicador = 1 AND ttSelecao.iTipoDispo = 1 THEN */
            
        /* ELSE                                                      */
        /*     ASSIGN validaMenorZero = (24 * ttAux.nr-dias).        */

        IF ttDados.hr-manut > validaMenorZero THEN
            ASSIGN ttDados.hr-manut = validaMenorZero.
        
        IF ttSelecao.iIndicador = 2 THEN DO:
            ASSIGN ttDados.dispo = (((24 * ttAux.nr-dias) - ttDados.hr-manut) * 100) / (24 * ttAux.nr-dias).
        END. 
    END.
    ELSE DO:
        /** Calcula total de horas de manutencao para o Equipamento considerando sobreposicao de horarios **/
        RUN calcHorasDia (INPUT ttAux.ep-codigo, INPUT ttAux.cod-eqpto, INPUT ttDados.cd-tag, INPUT 888, INPUT 01/01/1800). /* piCarregaSequencias */
        ASSIGN ttDados.hr-manut = dHorasDia.
    END.

    /** Frotas **/
    IF ttSelecao.iIndicador = 1 THEN
        ASSIGN ttDados.mtbf = IF ttDados.nr-inter-corre <> 0 THEN ((deHorasAux * ttAux.fator-conver) /  ttDados.nr-inter-corre) ELSE 0.
    ELSE
        ASSIGN ttDados.mtbf = IF ttDados.nr-inter-corre <> 0 THEN ((deHorasAux * ttAux.nr-dias) /  ttDados.nr-inter-corre) ELSE 0.

    /** Caso a visÆo for equipamento, a disponibilidade ‚ calculada normalmente **/
    IF ttDados.sequencia = 2 THEN DO:
        ASSIGN validaMenorZero = (24 * ttAux.nr-dias).

        IF ttDados.hr-manut > validaMenorZero THEN                          
            ASSIGN ttDados.hr-manut = validaMenorZero.

        ASSIGN ttDados.dispo = (((24 * ttAux.nr-dias) - ttDados.hr-manut) * 100) / (24 * ttAux.nr-dias).
    END.

    ASSIGN ttDados.mttr      = IF ttDados.nr-inter-corre <> 0    THEN (ttDados.hr-manut-corre-fin / ttDados.nr-inter-corre) ELSE 0           
           ttDados.pmpl      = IF dManutCorre <> 0 OR dCalc <> 0 THEN (dCalc / (dCalc + dManutCorre)) * 100 ELSE 0
           ttDados.mat       = ttDados.mat  + ttAux.mat
           ttDados.ggf       = ttDados.ggf  + ttAux.ggf
           ttDados.serv      = ttDados.serv + ttAux.serv
           ttDados.contratos = ttDados.contratos + ttAux.contratos
           /*ttDados.custo   = (ttDados.mat + ttDados.serv + ttDados.ggf) / ttDados.dispo*/
           ttDados.tot       = ttDados.mat + ttDados.serv + ttDados.ggf.

    /** ttDados.custo = (ttDados.mat + ttDados.serv + ttDados.ggf + ttDados.contratos) / ttDados.dispo **/
    /** ttDados.tot   = ttDados.tot + ttDados.mat + ttDados.serv + ttDados.ggf + ttDados.contratos     **/

    /* Cria um nivel a mais quando for a £ltima dimensÆo  */
    IF ttVisao.sequencia = iVisao THEN DO:
        RUN piCriaVisoes IN THIS-PROCEDURE.
    END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piContratos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH  mnd-movto-proces FIELDS(val-proces)
    WHERE mnd-movto-proces.ep-codigo    = mmv-ord-manut.ep-codigo
    AND   mnd-movto-proces.cod-eqpto    = mmv-ord-manut.cod-eqpto
    AND  (mnd-movto-proces.dat-inicial >= ttSelecao.dt-trans-ini
    AND   mnd-movto-proces.dat-inicial <= ttSelecao.dt-trans-fim)
    OR   (mnd-movto-proces.dat-vencto  <= ttSelecao.dt-trans-ini
    AND   mnd-movto-proces.dat-vencto  >= ttSelecao.dt-trans-fim) NO-LOCK:
    ASSIGN d-val-proces =  d-val-proces + mnd-movto-proces.val-proce.
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piCriaTTDados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /** Terceira Fase da Gera‡Æo **/
    FOR EACH ttAux:        
        /** Primeiro da lista ‚ o Pai, entÆo nÆo cont‚m pai **/       
        ASSIGN cCodPai = "".                                          
        /** Busca visäes escolhidas **/                               
        FOR EACH ttVisao NO-LOCK:                                     
            /** Busca c¢digo e descri‡Æo dos registros da visÆo **/   
            RUN buscaValor IN THIS-PROCEDURE (INPUT ttVisao.dimensao).
            /** Verifica se encontrou c¢digo, senÆo busca pr¢ximo **/ 
            IF vCodigo = "" THEN NEXT.                                
            /** Carrega os dados  **/                                 
            RUN piCarregaSequencias IN THIS-PROCEDURE.                
        END.                                                          

         /** Se o periodo for maior que 30 dias, calcula disponibilidade dia a dia apenas para 30 dias,
             caso contrario calcula os dias informado no periodo. **/                                  
         IF i-nr-dias >= 30 THEN                                                                       
             ASSIGN dtData = ttSelecao.periodo-ini + 30.                                               
         ELSE                                                                                          
             ASSIGN dtData = ttSelecao.periodo-fim.                                                    
                                                                                                       
                                                                                                       
        IF ttSelecao.iIndicador = 1 THEN                                                               
            ASSIGN cAux = ttAux.cod-model.                                                             
        ELSE                                                                                           
            ASSIGN cAux = ttAux.cd-tag.                                                                

        /** Percorre dia a dia no periodo selecionado para calcular a Disponibilidade **/
        DO dataAux = ttSelecao.periodo-ini TO dtData:
            IF NOT CAN-FIND(FIRST tt-disp
                            WHERE tt-disp.ep-codigo = ttAux.ep-codigo
                            AND   tt-disp.cod-eqpto = ttAux.cod-eqpto
                            AND   tt-disp.dt-trans  = dataAux NO-LOCK) THEN DO:
                CREATE tt-disp.
                ASSIGN tt-disp.ep-codigo  = ttAux.ep-codigo
                       tt-disp.cod-eqpto  = ttAux.cod-eqpto
                       tt-disp.descricao  = ttAux.descricao
                       tt-disp.cd-tag     = cAux
                       tt-disp.dt-trans   = dataAux.
            END.
            ELSE DO:
                FOR FIRST tt-disp
                    WHERE tt-disp.ep-codigo = ttAux.ep-codigo
                    AND   tt-disp.cod-eqpto = ttAux.cod-eqpto
                    AND   tt-disp.dt-trans  = dataAux:
                END.
            END.
        
            /** Horas de Manutencao **/
            ASSIGN d-hr-manut-aux = 0.
            /** Calcula as Horas para o Equipamento **/
            RUN calcHorasDia (INPUT ttAux.ep-codigo, INPUT ttAux.cod-eqpto, INPUT cAux, INPUT 1, INPUT dataAux). /* piCriaTTDados */
            ASSIGN tt-disp.hr-manut = tt-disp.hr-manut + dHorasDia.

            /** Como trata n¡vel de equipamento realiza indepedente do indicador **/
            IF tt-disp.hr-manut > 24 OR ((24 - tt-disp.hr-manut) <= 0.07) THEN     
                ASSIGN tt-disp.hr-manut = 24.                                      
                                                                                   
            ASSIGN tt-disp.disponibilidade = ((24 - tt-disp.hr-manut) / 24) * 100. 

            /** Calcula as Horas para o Tag se for MI**/
            IF ttSelecao.iIndicador = 2 THEN DO:
                 
                RUN calcHorasDia (INPUT 0, INPUT "", INPUT cAux, INPUT 2, INPUT dataAux). /* piCriaTTDados */
                ASSIGN d-hr-manut-aux   = dHorasDia.
                
                IF d-hr-manut-aux > 24 OR ((24 - d-hr-manut-aux) <= 0.07) THEN
                    ASSIGN d-hr-manut-aux = 24.                                 
    
                /** Disponibilidade do Modelo **/
                IF NOT CAN-FIND(FIRST tt-tag
                                WHERE tt-tag.cd-tag   = cAux
                                AND   tt-tag.dt-trans = dataAux) THEN DO:
                    CREATE tt-tag.
                    ASSIGN tt-tag.cd-tag          = cAux
                           tt-tag.descricao       = ttAux.descricao
                           tt-tag.dt-trans        = dataAux
                           tt-tag.hr-manut        = d-hr-manut-aux
                           tt-tag.disponibilidade = ((24 - d-hr-manut-aux) / 24) * 100.
                END.

            END.
            /* Alterada a l¢gica para atender a necessidade do cliente                                                   */
            /* ELSE DO:                                                                                                  */
            /*     ASSIGN i-qt-eqpto = 0.                                                                                */
            /*     FOR EACH  bfttAux                                                                                     */
            /*         WHERE bfttAux.cod-modelo = cAux NO-LOCK:                                                          */
            /*         ASSIGN i-qt-eqpto = i-qt-eqpto + 1.                                                               */
            /*     END.                                                                                                  */
            /*                                                                                                           */
            /*     IF d-hr-manut-aux > (24 * i-qt-eqpto) OR (((24 * i-qt-eqpto) - d-hr-manut-aux) <= 0.07) THEN          */
            /*         ASSIGN d-hr-manut-aux = (24 * i-qt-eqpto).                                                        */
            /*                                                                                                           */
            /*     IF tt-disp.hr-manut > 24 OR ((24 - tt-disp.hr-manut) <= 0.07) THEN                                    */
            /*         ASSIGN tt-disp.hr-manut = 24.                                                                     */
            /*                                                                                                           */
            /*     ASSIGN tt-disp.disponibilidade = ((24 - tt-disp.hr-manut) / 24 ) * 100.                               */
            /*                                                                                                           */
            /*     /** Disponibilidade do Modelo **/                                                                     */
            /*     IF NOT CAN-FIND(FIRST tt-tag                                                                          */
            /*                     WHERE tt-tag.cd-tag   = cAux                                                          */
            /*                     AND   tt-tag.dt-trans = dataAux) THEN DO:                                             */
            /*         CREATE tt-tag.                                                                                    */
            /*         ASSIGN tt-tag.cd-tag          = cAux                                                              */
            /*                tt-tag.descricao       = ttAux.descricao                                                   */
            /*                tt-tag.dt-trans        = dataAux                                                           */
            /*                tt-tag.hr-manut        = d-hr-manut-aux                                                    */
            /*                tt-tag.disponibilidade = (((24 * i-qt-eqpto) - d-hr-manut-aux) / (24 * i-qt-eqpto)) * 100. */
            /*     END.                                                                                                  */
            /* END.                                                                                                      */
        END.
    END.
    /** L¢gica para calcular o dia-a-dia para o modelo de frotas **/
    IF ttSelecao.iIndicador = 1 THEN DO:
       
        FOR EACH ttDados
            WHERE ttDados.sequencia = 1 NO-LOCK:

            ASSIGN i-qt-eqpto = 0.                                     
            FOR EACH  bfttAux                                          
                WHERE bfttAux.cod-modelo = ttDados.cod-oficial NO-LOCK:
                ASSIGN i-qt-eqpto = i-qt-eqpto + 1.                    
            END.                                                       
            
            /** Percorre dia-a-dia para calcular o modelo **/
            DO dataAux = ttSelecao.periodo-ini TO dtData:
                /** Campo gen‚rico: modelo**/
                RUN calcHorasDia (INPUT 0, INPUT "", INPUT ttDados.cod-oficial, INPUT 2, INPUT dataAux). /* piCriaTTDados */
                ASSIGN d-hr-manut-aux   = dHorasDia.

                IF d-hr-manut-aux > (24 * i-qt-eqpto) OR (((24 * i-qt-eqpto) - d-hr-manut-aux) <= (0.07 * i-qt-eqpto)) THEN         
                    ASSIGN d-hr-manut-aux = (24 * i-qt-eqpto). 

                /** Disponibilidade do Modelo **/                                                                    
                IF NOT CAN-FIND(FIRST tt-tag                                                                         
                                WHERE tt-tag.cd-tag   = ttDados.cod-oficial                                                         
                                AND   tt-tag.dt-trans = dataAux) THEN DO:                                            
                    CREATE tt-tag.                                                                                   
                    ASSIGN tt-tag.cd-tag          = ttDados.cod-oficial                                                             
                           tt-tag.descricao       = ttDados.desc-dimensao                                                  
                           tt-tag.dt-trans        = dataAux                                                          
                           tt-tag.hr-manut        = d-hr-manut-aux                                                   
                           tt-tag.disponibilidade = (((24 * i-qt-eqpto) - d-hr-manut-aux) / (24 * i-qt-eqpto)) * 100.  
                END.
            END.

            FOR FIRST ttAux 
                WHERE ttAux.cod-modelo = ttDados.cod-oficial NO-LOCK:
            END.
            
            RUN calcHorasDia (INPUT 0, INPUT "", INPUT ttDados.cd-tag, INPUT 999, INPUT 01/01/1800). /* piCriaTTDados */
            ASSIGN ttDados.hr-manut = dHorasDia.                                                    
            ASSIGN ttDados.dispo = (((24 * ttAux.nr-dias * i-qt-eqpto) - ttDados.hr-manut) * 100) / (24 * ttAux.nr-dias * i-qt-eqpto).
            
        END.
    END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piCriaTTLeitura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ordemIni LIKE ord-manut.nr-ord-produ NO-UNDO.
DEFINE VARIABLE ordemFim LIKE ord-manut.nr-ord-produ NO-UNDO.

FOR FIRST ord-manut NO-LOCK:
    ASSIGN ordemIni = ord-manut.nr-ord-produ.
END.

FOR LAST ord-manut NO-LOCK:
    ASSIGN ordemFim = ord-manut.nr-ord-produ.
END.

EMPTY TEMP-TABLE tt-om-leitura.

FOR EACH  movto-mat FIELDS(nr-ord-produ)
    WHERE movto-mat.nr-ord-produ >= ordemIni
    AND   movto-mat.nr-ord-produ <= ordemFim
    AND   movto-mat.dt-trans     >= ttSelecao.dt-trans-ini 
    AND   movto-mat.dt-trans     <= ttSelecao.dt-trans-fim NO-LOCK:
    IF NOT CAN-FIND(FIRST tt-om-leitura
                    WHERE tt-om-leitura.nr-ord-produ = movto-mat.nr-ord-produ NO-LOCK) THEN DO:
        CREATE tt-om-leitura.
        ASSIGN tt-om-leitura.nr-ord-produ = movto-mat.nr-ord-produ.
    END.
END.

FOR EACH  movto-estoq FIELDS(nr-ord-produ)
    WHERE movto-estoq.nr-ord-produ >= ordemIni
    AND   movto-estoq.nr-ord-produ <= ordemFim
    AND   movto-estoq.esp-docto     = 28
    AND   movto-estoq.dt-trans     >= ttSelecao.dt-trans-ini 
    AND   movto-estoq.dt-trans     <= ttSelecao.dt-trans-fim
    AND   movto-estoq.cod-emitente  > 0 NO-LOCK:
    IF NOT CAN-FIND(FIRST tt-om-leitura
                    WHERE tt-om-leitura.nr-ord-produ = movto-estoq.nr-ord-produ NO-LOCK) THEN DO:
        CREATE tt-om-leitura.
        ASSIGN tt-om-leitura.nr-ord-produ = movto-estoq.nr-ord-produ.
    END.
END.

FOR EACH  movto-ggf FIELDS(nr-ord-produ)
    WHERE movto-ggf.nr-ord-produ >= ordemIni
    AND   movto-ggf.nr-ord-produ <= ordemFim
    AND   movto-ggf.dt-trans     >= ttSelecao.dt-trans-ini 
    AND   movto-ggf.dt-trans     <= ttSelecao.dt-trans-fim NO-LOCK:
    IF NOT CAN-FIND(FIRST tt-om-leitura
                    WHERE tt-om-leitura.nr-ord-produ = movto-ggf.nr-ord-produ NO-LOCK) THEN DO:
        CREATE tt-om-leitura.
        ASSIGN tt-om-leitura.nr-ord-produ = movto-ggf.nr-ord-produ.
    END.
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piCriaTTOrd-manut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE tt-ord-manut.
ASSIGN tt-ord-manut.nr-ord-produ = ord-manut.nr-ord-produ
       tt-ord-manut.des-man-corr = ord-manut.des-man-corr
       tt-ord-manut.estado       = ord-manut.estado      
       tt-ord-manut.cod-eqpto    = ord-manut.cd-equipto   
       tt-ord-manut.dt-manut     = ord-manut.dt-manut    
       tt-ord-manut.dt-fecham    = ord-manut.dt-fecham
       tt-ord-manut.indicador    = YES.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piCriaTTTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER c-tag AS CHAR NO-UNDO.

IF VALID-HANDLE(hAcomp) THEN DO:
    RUN pi-Acompanhar IN hAcomp ("Efetuando Leitura de TAGs").
END.

FIND bf-tag NO-LOCK WHERE bf-tag.cd-tag = c-tag NO-ERROR.
IF NOT AVAIL bf-tag THEN NEXT.

FIND LAST bf-tt-estrut-tag USE-INDEX ch-sequencia NO-ERROR.

CREATE tt-estrut-tag.
ASSIGN tt-estrut-tag.cod-tipo  = bf-tag.cod-tipo
       tt-estrut-tag.cd-tag    = bf-tag.cd-tag
       tt-estrut-tag.descricao = bf-tag.descricao
       tt-estrut-tag.sequencia = IF AVAIL bf-tt-estrut-tag THEN bf-tt-estrut-tag.sequencia + 1 ELSE 1.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piCriaVisao :
/*------------------------------------------------------------------------------
  Purpose:     piCriaVisao
  Parameters:  <none>
  Notes:       Cria as visäes se nÆo foram selecionadas 
------------------------------------------------------------------------------*/
if ttSelecao.iIndicador = 1 then do:
    create ttVisao.
    assign ttVisao.dimensao  = '03 ' + fnLabels(3)
           ttVisao.sequencia = 1.
    create ttVisao.
    assign ttVisao.dimensao  = '04 ' + fnLabels(4)
           ttVisao.sequencia = 2.
end.
else do:
    create ttVisao.
    assign ttVisao.dimensao  = '05 ' + fnLabels(5)
           ttVisao.sequencia = 1.
    create ttVisao.
    assign ttVisao.dimensao  = '04 ' + fnLabels(4)
           ttVisao.sequencia = 2.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piCriaVisoes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT CAN-FIND(FIRST tt-ord-manut
                    WHERE tt-ord-manut.cod-eqpto = ttAux.cod-eqpto) THEN DO:

        CREATE ttDados.                                                                                                              
        ASSIGN ttDados.cod-dimensao   = (cCodPai + "#")             /** C¢digo encadeado para tree-view **/
               ttDados.cod-oficial    = ""                          /** C¢digo Original do registro **/
               ttDados.sequencia      = ttVisao.sequencia + 1      /** Sequˆncia dos dados **/ 
               ttDados.cod-dimens-pai = cCodPai                    /** C¢digo do pai do registro **/
               ttDados.nr-ord-produ   = ?
               ttDados.p-image        = iImage
               ttDados.estado         = ?
               ttDados.dt-manut       = ?
               ttDados.dt-fecham      = ?
               .
    END.

    FOR EACH tt-ord-manut
        WHERE tt-ord-manut.cod-eqpto = ttAux.cod-eqpto:

        CREATE ttDados.
        ASSIGN ttDados.cod-dimensao  = (cCodPai + "#" + STRING(tt-ord-manut.nr-ord-produ))  /** C¢digo encadeado para tree-view **/
               ttDados.cod-oficial    = vCodigo                                             /** C¢digo Original do registro **/
               ttDados.desc-dimensao  = vDescricao                                          /** Descri‡Æo do c¢digo **/
               ttDados.sequencia      = ttVisao.sequencia + 1                               /** Sequˆncia dos dados **/
               ttDados.p-image        = iImage                                              /** N£mero da imagem do registro **/
               ttDados.r-rowid        = rRowid                                              /** Rowid do registro para consultas futuras **/
               ttDados.cod-dimens-pai = cCodPai                                             /** C¢digo do pai do registro **/
               ttDados.cod-eqpto      = ttAux.cod-eqpto
               ttDados.cd-tag         = IF ttSelecao.iIndicador = 1 THEN ttAux.cod-modelo ELSE ttAux.cd-tag.

        ASSIGN ttDados.nr-ord-produ  = tt-ord-manut.nr-ord-produ
               ttDados.des-man-corr  = tt-ord-manut.des-man-corr
               ttDados.estado        = tt-ord-manut.estado       
               ttDados.dt-manut      = tt-ord-manut.dt-manut    
               ttDados.dt-fecham     = tt-ord-manut.dt-fecham
               ttDados.indicador     = tt-ord-manut.indicador.
    END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piExcel :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEF VAR c-range     AS CHAR       NO-UNDO.
    DEF VAR i-alf       AS INTEGER    NO-UNDO.
    DEF VAR i-alf2      AS INTEGER    NO-UNDO.
    DEF VAR i-colu      AS INTEGER    NO-UNDO.
    DEF VAR lin-Inicial AS INTEGER    NO-UNDO.
    DEF VAR lin-Final   AS INTEGER    NO-UNDO.
    DEF VAR cDescEqpto  AS CHARACTER  NO-UNDO.
    DEF VAR cDescTagMod AS CHARACTER  NO-UNDO.

    run utp/ut-utils.p persistent set h-utils.
    /** inicia tela de acompanhamento **/
    run utp/ut-acomp.p persistent set h-acomp.
    run pi-inicializar in h-acomp (input "Exporta‡Æo").
    run pi-acompanhar  in h-acomp (input "Carregando..").

    empty temp-table tt-eqpto.
    empty temp-table tt-pai.

    repeat i-alf = 1 TO 26:
        repeat i-alf2 = 1 TO 26:
            assign c-alfabeto = c-alfabeto + "," + entry(i-alf,c-alfabeto,",") + entry(i-alf2,c-alfabeto,",").
        end.
    end.

    assign chExcel     = ?
           chWorkbook  = ?
           chWorksheet = ?.

    create "Excel.Application" chExcel.

    chWorkbook  = chExcel:Workbooks:open(search("mvp/esmv0616.xls")).
    chWorksheet = chWorkbook:Sheets:item(1).
    chWorksheet:Activate().

    /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Dia",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    chWorkSheet:Cells(1,3):value  = return-value.
    chWorkSheet:Cells(41,3):value = return-value.
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Disponibilidade_Di ria",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    chWorkSheet:Cells(2,3):value  = return-value.
    chWorkSheet:Cells(42,3):value = return-value.
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "ACUM.",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
  
    chWorkSheet:Cells(1,4):value  = return-value.
    chWorkSheet:Cells(41,4):value = return-value.

    /** Cria temp-table de Equipamentos **/
    for each bf-tt-disp:
        if not can-find(first tt-eqpto
                        where tt-eqpto.cod-eqpto = bf-tt-disp.cod-eqpto) then do:
            /** Disponibilidade Acumuluda **/
            for first bfttDados
                where bfttDados.cod-eqpto = bf-tt-disp.cod-eqpto 
                and   bfttDados.sequencia = 2 no-lock:                
            end.
            create tt-eqpto.
            assign tt-eqpto.cod-eqpto  = bf-tt-disp.cod-eqpto
                   tt-eqpto.cd-tag     = bf-tt-disp.cd-tag
                   tt-eqpto.descricao  = bf-tt-disp.descricao
                   tt-eqpto.dispo-acum = bfttDados.dispo.
        end.
    end.
    /** Cria temp-table de Tag/Modelo **/
    for each bf-tt-tag:
        if not can-find(first tt-pai
                        where tt-pai.codigo = bf-tt-tag.cd-tag) then do:
                for first bfttDados
                    where bfttDados.cd-tag    = bf-tt-tag.cd-tag 
                    and   bfttDados.sequencia = 1 no-lock:                  
                end.
                create tt-pai.
                assign tt-pai.codigo     = bf-tt-tag.cd-tag
                       tt-pai.descricao  = bf-tt-tag.descricao
                       tt-pai.dispo-acum = bfttDados.dispo.
        end.
    end.

    /** Disponibilidade Acumuluda **/
    for first bfttDados
        where bfttDados.cd-tag    = ttDados.cd-tag 
        and   bfttDados.sequencia = 1 no-lock:
        assign chWorkSheet:Cells(2,4):value = bfttDados.dispo
               cDescTagMod = bfttDados.desc-dimensao.
        run piFormato in this-procedure (input 2, input 4, input bfttDados.dispo).
    end.
    for first bfttDados
        where bfttDados.cod-eqpto = ttDados.cod-eqpto
        and   bfttDados.sequencia = 2 no-lock:
        assign chWorkSheet:Cells(3,4):value = bfttDados.dispo
               cDescEqpto = bfttDados.desc-dimensao.
        run piFormato in this-procedure (input 3, input 4, input bfttDados.dispo).
    end.

    /** Imprimi cabe‡alho das Data **/
    assign i-colu = 4.
    repeat dataAux = ttSelecao.periodo-ini to ttSelecao.periodo-fim:
        assign i-colu = i-colu + 1
               chWorkSheet:Cells(1,i-colu):value  = dataAux
               chWorkSheet:Cells(41,i-colu):value = dataAux.
    end.

    /** Pinta Linhas das Datas **/
    assign chWorksheet:Range(entry(3,c-alfabeto,",") + "1" + ":" + entry(i-colu,c-alfabeto,",") + "1"):Interior:ColorIndex = 48
           chWorksheet:Range(entry(3,c-alfabeto,",") + "1" + ":" + entry(i-colu,c-alfabeto,",") + "1"):Font:ColorIndex = 1.

    assign chWorksheet:Range(entry(3,c-alfabeto,",") + "41" + ":" + entry(i-colu,c-alfabeto,",") + "41"):Interior:ColorIndex = 48
           chWorksheet:Range(entry(3,c-alfabeto,",") + "41" + ":" + entry(i-colu,c-alfabeto,",") + "41"):Font:ColorIndex = 1.

    /** Imprimi TAG que ‚ informado no Grafico **/
    assign chWorkSheet:Cells(2,1):value = 1 /** Dimensao 1 **/
           chWorkSheet:Cells(2,2):value = ttDados.cd-tag + " - " + cDescTagMod.
           i-colu = 4.
    for each  tt-tag
        where tt-tag.cd-tag = ttDados.cd-tag:
        assign i-colu = i-colu + 1
               chWorkSheet:Cells(2,i-colu):value = tt-tag.disponibilidade.
        run piFormato in this-procedure (input 2, input i-colu, input tt-tag.disponibilidade).
    end.
    /** Imprimi Eqpto que ‚ informado no Grafico **/
    assign chWorkSheet:Cells(3,1):value = 2 /** Dimensao 2 **/
           chWorkSheet:Cells(3,2):value = ttDados.cod-eqpto + " - " + cDescEqpto.
           i-colu = 4.
    for each  tt-disp
        where tt-disp.cod-eqpto = ttDados.cod-eqpto:
        assign i-colu = i-colu + 1
               chWorkSheet:Cells(3,i-colu):value = tt-disp.disponibilidade.
        run piFormato in this-procedure (input 3, input i-colu, input tt-disp.disponibilidade).
    end.

   /** Imprimi todas as visoes mostrandos suas disponibilidades diaria **/
    assign i-lin = 41
           i-col = 4.
    for each tt-pai 
        by tt-pai.codigo:
        assign i-lin       = i-lin + 1
               i-col       = 4
               lin-Inicial = i-lin + 1
               chWorkSheet:Cells(i-lin,1):value = 1
               chWorkSheet:Cells(i-lin,2):value = tt-pai.codigo + " - " + tt-pai.descricao
               chWorkSheet:Cells(i-lin,4):value = tt-pai.dispo-acum.
        run piFormato in this-procedure (input i-lin, input 4, input tt-pai.dispo-acum).
        /** Imprimi Disponibilidade do Tag dia a dia **/
        for each tt-tag
            where tt-tag.cd-tag = tt-pai.codigo:
            assign i-col = i-col + 1
                   chWorkSheet:Cells(i-lin,i-col):value = tt-tag.disponibilidade.
            run piFormato in this-procedure (input i-lin, input i-col, input tt-tag.disponibilidade).
        end.
        /** Imprimi os Eqptos do TAG **/
        for each tt-eqpto
            where tt-eqpto.cd-tag = tt-pai.codigo:
            assign i-lin = i-lin + 1
                   i-col = 4
                   chWorkSheet:Cells(i-lin,1):value = 2
                   chWorkSheet:Cells(i-lin,2):value = tt-eqpto.cod-eqpto + " - " + tt-eqpto.descricao
                   chWorkSheet:Cells(i-lin,4):value = tt-eqpto.dispo-acum.
            run piFormato in this-procedure (input i-lin, input 4, input tt-eqpto.dispo-acum).
            /** Imprimi Disponibilidade do Eqpto dia a dia **/
            for each tt-disp
                where tt-disp.cod-eqpto = tt-eqpto.cod-eqpto:
                assign i-col = i-col + 1
                       chWorkSheet:Cells(i-lin,i-col):value = tt-disp.disponibilidade.
                run piFormato in this-procedure (input i-lin, input i-col, input tt-disp.disponibilidade).
            end.
        end.
        assign lin-Final = i-lin.
        /** Agrupa Visoes **/
        chWorkSheet:Range("A" + string(lin-Inicial) + ":" + "A" + string(lin-Final)):group(1) no-error.
    end.

    /** Extrai dados do TreeList **/
    run extraiDadosDaTreeList in this-procedure.
    /** Gera/Pinta cabelho das visoes **/
    assign i-lin = i-lin + 5.
    run piGeraCabecalho in this-procedure (input i-lin).
    /** Exporta os dados das visoes **/
    assign i-lin = i-lin + 1.
    run piExportaDetalhe in this-procedure (input i-lin).
    /** Agrupa os dados **/
    assign i-lin = i-lin - 1.
    run piAgrupaDados in this-procedure (input i-lin).

    /** Acerta tamanho das colunas **/
    if c-range = "" then
        assign c-range = entry(1,c-alfabeto,",") + ":" + entry(256,c-alfabeto,",").
    chWorksheet:columns(c-range):EntireColumn:AutoFit no-error.

    /** Ocultando linhas de grade **/
    chExcel:ActiveWindow:DisplayGridlines = False.

    /** Posiciona na primeira celula **/
    chWorksheet:Range("A1"):Select().

    /*chWorksheet:Save.        */
    chExcel:VISIBLE = TRUE.  
    /*chExcel:WindowState = 3. */

    /** encerra tela de acompanhamento **/
    run pi-finalizar in h-acomp. 

    /* --- Release all the com handles --- */
    if chWorksheet <> ? then
        release object chWorksheet.

    if chWorkbook <> ? then
        release object chWorkbook.

    if chExcel <> ? then
        release object chExcel.

    session:set-wait-state ("").

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piExplodeTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER c-tag AS CHARACTER NO-UNDO.

IF VALID-HANDLE(hAcomp) THEN DO:
    RUN pi-Acompanhar IN hAcomp ("Efetuando Leitura de TAGs").
END.

FOR FIRST bf-estr-tag
    WHERE bf-estr-tag.tag-filho = c-tag NO-LOCK:

    RUN piCriaTTtag(INPUT bf-estr-tag.tag-pai).

    RUN piExplodeTag(INPUT bf-estr-tag.tag-pai).
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piExportaDetalhe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var c-linha       as char  no-undo.
    def var c-delimitador as char  no-undo.
    def var i-cont        as int   no-undo.
    def input param i-lin as int   no-undo.

    assign c-delimitador = chr(9)
           i-cont        = ?.
         /*i-lin         = 43.*/

    for each ttComponente use-index ch-posicao:
        if i-linha modulo 120 = 0 then
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Exportando_Excel",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
            run pi-acompanhar in h-acomp (input return-value + " - " + string(i-linha)).

        if ttComponente.sequencia = 1 then
            assign cIdent = "".
        else
            assign cIdent = fill("  ", ttComponente.sequencia * 2).

        assign c-linha = "".
        assign c-linha = string(ttComponente.sequencia)  + c-delimitador +
                         cIdent + ttComponente.descricao + c-delimitador.

        for each ttColunaComponente use-index ch-auxiliar 
            where ttColunaComponente.cod-componente = ttComponente.cod-componente:
            assign c-linha = c-linha + ttColunaComponente.valor + c-delimitador.
        end.

        if i-cont = ? then do:
            output to "CLIPBOARD" no-convert.
            assign i-cont = 0.
        end.

        put c-linha format "X(" + string(length(c-linha)) + ")" skip.
        assign i-linha = i-linha + 1
               i-cont  = i-cont  + 1.

        if i-cont = 120 then do:
            output close.
            chWorksheet:Range("A" + string(i-lin)):select().
            chWorksheet:paste().
            assign i-lin = i-lin + i-cont.
            assign i-cont  = ?.
        end.
    end.
    if i-cont <> ? then do:
        output close.

        chWorksheet:Range("A" + string(i-lin)):select().
        chWorksheet:paste().
        chWorksheet = chWorkbook:Sheets:item(1).
        chWorksheet:Activate().
      /*chWorksheet:Range("A1"):Select().               */
      /*chWorksheet:COLUMNS("B:B":U):NumberFormat = "@".*/
    end.

/*     if p-arvore = YES THEN */
/*         RUN piAgrupaDados. */

    chExcel:Calculate().
    chExcel:Calculation = -4105. /* xlAutomatic */
    chExcel:ScreenUpdating = true.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piFormataTreeList :
/*------------------------------------------------------------------------------
  Purpose:     piformatTreeList
  Parameters:  <none>
  Notes:       Formata o TreeList e o Browser (visualiza‡Æo de colunas) conforme parametriza‡Æo do usu rio  
------------------------------------------------------------------------------*/
    /** Labels do browse **/
    assign ttDados.cod-oficial:label   in browse brDetalhe = fnBrowse("cod-oficial":U,1)  
           ttDados.desc-dimensao:label in browse brDetalhe = fnBrowse("desc-dimensao":U,1). 
           ttDados.nr-ord-produ:label  in browse brDetalhe = fnBrowse("nr-ord-produ":U,1). 
           ttDados.des-man-corr:label  in browse brDetalhe = fnBrowse("des-man-corr":U,1). 
           cEstado:label               in browse brDetalhe = fnBrowse("estado":U,1).       
           ttDados.cod-eqpto:label     in browse brDetalhe = fnBrowse("cod-eqpto":U,1).    
           ttDados.dt-manut:label      in browse brDetalhe = fnBrowse("dt-manut":U,1).     
           ttDados.dt-fecham:label     in browse brDetalhe = fnBrowse("dt-fecham":U,1).    

           if ttSelecao.iIndicador = 1 then
               assign ttDados.des-man-corr:visible in browse brDetalhe = no.

    /** Labels das Colunas do TreeList **/
    chTreeList:ColumnHeaders:clear().
    chTreeList:ColumnHeaders:add (, , "", 3500, 0).
    if ttSelecao.lDispo     then chTreeList:ColumnHeaders:Add (, , fnBrowse("dispo":U,1), 1300, 1).
  /*if ttSelecao.lCusto     then chTreeList:ColumnHeaders:Add (, , fnBrowse("custo":U,1), 1300, 1).*/
    if ttSelecao.lMtbf      then chTreeList:ColumnHeaders:Add (, , fnBrowse("mtbf":U,1), 1200, 1).
    if ttSelecao.lMttr      then chTreeList:ColumnHeaders:Add (, , fnBrowse("mttr":U,1), 1200, 1).
    if ttSelecao.lPmpl      then chTreeList:ColumnHeaders:Add (, , fnBrowse("pmpl":U,1), 1300, 1).
    if ttSelecao.lMat       then chTreeList:ColumnHeaders:Add (, , fnBrowse("mat":U,1), 1300, 1).
    if ttSelecao.lGGF       then chTreeList:ColumnHeaders:Add (, , fnBrowse("ggf":U,1), 1600, 1).
    if ttSelecao.lServ      then chTreeList:ColumnHeaders:Add (, , fnBrowse("serv":U,1), 1300, 1).
  /*if ttSelecao.lContratos then chTreeList:ColumnHeaders:Add (, , fnBrowse("contratos":U,1), 1300, 1).*/
    if ttSelecao.lTotal     then chTreeList:ColumnHeaders:Add (, , fnBrowse("tot":U,1), 1300, 1).

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piFormato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input param pLin  as integer no-undo.
define input param pCol  as integer no-undo.
define input param pDisp as decimal no-undo.

if pDisp <> 100 then do:
    chWorkSheet:Range(entry(pCol,c-alfabeto) + string(pLin)):select.
    chExcel:Selection:NumberFormat = "#,##0.00".
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piGeraCabecalho :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input param ilinha as integer no-undo.

    /** Area do cabe‡alho da colunas **/                                                                                             
    /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "DimensÆo",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    FormataDados(entry(1,c-alfabeto,",") + "41",2,55,11,return-value).
    FormataDados(entry(1,c-alfabeto,",") + "1",2,55,11,return-value).
    FormataDados(entry(1,c-alfabeto,",") + string(ilinha),2,55,11,return-value).

    do i-coluna = 1 to n-colunas - 1:
        FormataDados(entry(i-coluna + 1,c-alfabeto,",") + string(ilinha),2,55,11,chTreeList:ColumnHeaders(i-coluna):text).
    end.

    /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "VisÆo",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    FormataDados(entry(2,c-alfabeto,",") + "41",2,55,11,return-value).
    FormataDados(entry(2,c-alfabeto,",") + "1",2,55,11,return-value).
    FormataDados(entry(2,c-alfabeto,",") + string(ilinha),2,55,11,return-value).

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piHorasOperacao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hAcomp) THEN DO:
    RUN pi-Acompanhar IN hAcomp ("Calculando Horas de Opera‡Æo").
END.

ASSIGN deHoras    = 0
       deHorasTag = 0
       deKM       = 0
       deKM2      = 0.

/**  ttSelecao.iIndicador = 1 -> FROTAS **/
IF ttSelecao.iIndicador = 1 THEN DO:
    FOR LAST  mab-movto-km-eqpto 
        WHERE mab-movto-km-eqpto.ep-codigo   = mab-eqpto.ep-codigo
        AND   mab-movto-km-eqpto.cod-eqpto   = mab-eqpto.cod-eqpto
        AND   mab-movto-km-eqpto.dat-movto  <= ttSelecao.periodo-fim
        AND   mab-movto-km-eqpto.dat-movto  >= ttSelecao.periodo-ini NO-LOCK:
        ASSIGN deKM2 = mab-movto-km-eqpto.val-hodom-horim.
    END.

    IF i-nr-dias = 1 THEN DO:
        FOR LAST mab-movto-km-eqpto
            WHERE mab-movto-km-eqpto.ep-codigo  = mab-eqpto.ep-codigo
            AND   mab-movto-km-eqpto.cod-eqpto  = mab-eqpto.cod-eqpto
            AND   mab-movto-km-eqpto.dat-movto  < ttSelecao.periodo-ini NO-LOCK:            
            ASSIGN deKM = mab-movto-km-eqpto.val-hodom-horim.
        END.
        IF NOT AVAIL mab-movto-km-eqpto THEN DO:
            FOR LAST mab-movto-km-eqpto
                WHERE mab-movto-km-eqpto.ep-codigo  = mab-eqpto.ep-codigo
                AND   mab-movto-km-eqpto.cod-eqpto  = mab-eqpto.cod-eqpto
                AND   mab-movto-km-eqpto.dat-movto  = ttSelecao.periodo-ini NO-LOCK:
                FOR LAST bf-mab-movto-km-eqpto
                    WHERE bf-mab-movto-km-eqpto.ep-codigo   = mab-eqpto.ep-codigo
                    AND   bf-mab-movto-km-eqpto.cod-eqpto   = mab-eqpto.cod-eqpto
                    AND   bf-mab-movto-km-eqpto.dat-movto   = ttSelecao.periodo-ini
                    AND   bf-mab-movto-km-eqpto.hra-inicial < mab-movto-km-eqpto.hra-inicial NO-LOCK:
                    ASSIGN deKM = bf-mab-movto-km-eqpto.val-hodom-horim.
                END.
            END.
        END.
    END.
    ELSE
        FOR FIRST mab-movto-km-eqpto 
            WHERE mab-movto-km-eqpto.ep-codigo  = mab-eqpto.ep-codigo
            AND   mab-movto-km-eqpto.cod-eqpto  = mab-eqpto.cod-eqpto
            AND   mab-movto-km-eqpto.dat-movto >= ttSelecao.periodo-ini 
            AND   mab-movto-km-eqpto.dat-movto <= ttSelecao.periodo-fim NO-LOCK:
            ASSIGN deKM = mab-movto-km-eqpto.val-hodom-horim.
        END.
END.
ELSE DO:
    /** Tipo 1 -> Eqpto 
     - Guarda Horas Opera‡Æo do Equipamento **/        
    FOR FIRST mmi-horas-oper
        WHERE mmi-horas-oper.cd-equipto = equipto.cd-equipto
        AND   mmi-horas-oper.cd-tag     = equipto.cd-tag 
        AND   mmi-horas-oper.tipo       = 1 NO-LOCK:
        ASSIGN deHoras = mmi-horas-oper.horas-oper-eqpto.
    END.
    IF NOT AVAIL mmi-horas-oper OR deHoras = 0 THEN DO:
        /** Tipo 2 -> Tag **/
        FOR FIRST mmi-horas-oper
            WHERE mmi-horas-oper.cd-tag = tt-estrut-tag.cd-tag 
            AND   mmi-horas-oper.tipo   = 2 NO-LOCK:
            ASSIGN deHoras = mmi-horas-oper.horas-oper-tag.
        END.
        IF NOT AVAIL mmi-horas-oper THEN
            ASSIGN deHoras = 24.
    END.

    /** - Guarda Horas Opera‡Æo do TAG **/
    FOR FIRST mmi-horas-oper
        WHERE mmi-horas-oper.cd-tag = tt-estrut-tag.cd-tag 
        AND   mmi-horas-oper.tipo   = 2 NO-LOCK:
        ASSIGN deHorasTag = mmi-horas-oper.horas-oper-tag.
    END.
    IF NOT AVAIL mmi-horas-oper THEN
        ASSIGN deHorasTag = 24.
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piMesAnterior :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter iOrdem like ord-manut.nr-ord-produ no-undo.

IF VALID-HANDLE(hAcomp) THEN DO:
    RUN pi-Acompanhar IN hAcomp ("Validando Movimentos Material - Ordem: " + STRING(iOrdem)).
END.

/** Busca os valores de Material da Ordem **/
for each  movto-mat fields(nr-ord-produ dt-trans esp-docto tipo-trans valor-mat-m it-codigo quantidade qt-reporte) use-index ordem
    where movto-mat.nr-ord-produ  = iOrdem
    and   movto-mat.dt-trans     >= ttSelecao.dt-trans-ini 
    and   movto-mat.dt-trans     <= ttSelecao.dt-trans-fim no-lock:

    if movto-mat.esp-docto <> 28 and movto-mat.esp-docto <> 30 then next.

    for first item
        where item.it-codigo = movto-mat.it-codigo no-lock:
        /** 2 - item.ind-serv-mat = Material **/
        if item.ind-serv-mat <> 2 then next.

        FIND LAST  pr-it-per WHERE pr-it-per.it-codigo   = item.it-codigo
                             AND   pr-it-per.cod-estabel = item.cod-estabel NO-LOCK NO-ERROR.

        if not avail pr-it-per then next.

        assign d-val-mat = d-val-mat + (movto-mat.quantidade * pr-it-per.val-unit-mat-m[1]) * (IF movto-mat.tipo-trans = 1 THEN -1 ELSE 1).
    end.
end.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piMostraErros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/** Instancia aplicativo **/

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



 
/** Mostra caixa com erros **/

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



 

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piPintaLinhas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     /** setar cor de fundo das celulas **/
     assign i-countLinhaCor = 5.
     for each ttComponente:
         assign i-countLinhaCor = i-countLinhaCor + 1.
                 chWorksheet:Range(chWorksheet:cells(i-countLinhaCor,1),chWorksheet:cells(i-countLinhaCor,n-colunas)):Interior:color = ttComponente.cor-fundo.
                 chWorksheet:Range(chWorksheet:cells(i-countLinhaCor,1),chWorksheet:cells(i-countLinhaCor,n-colunas)):font:color     = ttComponente.cor-fonte.

          for each ttColunaComponente where ttColunaComponente.cod-componente = ttComponente.cod-componente:
                chWorksheet:Range(chWorksheet:cells(i-countLinhaCor,1),chWorksheet:cells(i-countLinhaCor,n-colunas)):Interior:color  = ttColunaComponente.cor-fundo.
                chWorksheet:Range(chWorksheet:cells(i-countLinhaCor,1),chWorksheet:cells(i-countLinhaCor,n-colunas)):font:color      = ttColunaComponente.cor-fonte.
          end.
     end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER c-tag     AS CHAR    NO-UNDO.
DEFINE INPUT  PARAMETER i-nivel   AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER i-num-niv AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tt-estrut-tag.

DEFINE VARIABLE l-considera AS LOGICAL INIT NO NO-UNDO.
DEFINE VARIABLE i-seq       AS INTEGER INIT 0  NO-UNDO.

EMPTY TEMP-TABLE tt-estrut-tag.

IF VALID-HANDLE(hAcomp) THEN DO:
    RUN pi-Acompanhar IN hAcomp ("Efetuando Leitura de TAGs").
END.

RUN piCriaTTtag(INPUT c-tag).

RUN piExplodeTag(INPUT c-tag).

FOR EACH tt-estrut-tag BREAK BY tt-estrut-tag.sequencia DESC:
    IF tt-estrut-tag.cod-tipo = i-nivel THEN
        ASSIGN l-considera = YES.

    IF l-considera = NO THEN DO:
        DELETE tt-estrut-tag.
        NEXT.
    END.

    ASSIGN i-seq = i-seq + 1.
    ASSIGN tt-estrut-tag.ordem = i-seq.
    ASSIGN i-num-niv = i-num-niv - 1.

    IF i-num-niv = 0 THEN ASSIGN l-considera = NO.
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piVerificaNo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input        parameter pNo        as com-handle.
def input-output parameter pExpandido as log.

if pNo:Expanded = no then
    assign pExpandido = no.

if pExpandido = yes and pNo:parent <> 0 then
    run piVerificaNo(input pNo:parent,
                     input-output pExpandido).

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piZeraVar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN d-hr-hora-ini     = 0
       d-hr-hora-fim     = 0
       d-tot-corre-abert = 0
       d-tot-corre-fin   = 0
       d-tot-prev        = 0
       d-tot-corre       = 0
       d-tot-pred        = 0
       d-tot-corre-plan  = 0
       i-nr-inter-corre  = 0
       i-nr-corre-fin    = 0           
       d-val-mat         = 0
       d-val-servico     = 0
       d-val-proces      = 0.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE simultaneidadeFrotas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dIni     AS DECIMAL              NO-UNDO. 
DEFINE VARIABLE dFim     AS DECIMAL              NO-UNDO. 
DEFINE VARIABLE controle AS CHARACTER INITIAL "" NO-UNDO. 
DEFINE VARIABLE dataCont AS DATE                 NO-UNDO. 
DEFINE VARIABLE dHoras   AS DECIMAL              NO-UNDO. 
DEFINE VARIABLE dMinutos AS DECIMAL              NO-UNDO. 

IF VALID-HANDLE(hAcomp) THEN DO:
    RUN pi-Acompanhar IN hAcomp ("Calculando Simultaneidade Frotas").
END.

ASSIGN dHorasDia = 0.

FOR EACH  tt-apont
    WHERE tt-apont.ep-codigo = iEmp
    AND   tt-apont.cod-eqpto = cEqpto
    AND   tt-apont.cod-model = cTag:

    /** Se tiver calculando horas para o Equipamento, filtra pelo mesmo **/
    IF iTipo = 1 OR iTipo = 888 THEN
        IF NOT(tt-apont.ep-codigo = iEmp AND tt-apont.cod-eqpto = cEqpto) THEN NEXT.

    IF tt-apont.dat-ini < tt-apont.dat-fim THEN DO:
        /** tt-apont.dat-ini TO tt-apont.dat-fim **/
        DO dataCont = tt-apont.dat-ini TO tt-apont.dat-fim:
            CREATE tt-horas.
            ASSIGN tt-horas.dat-movto = dataCont
                   tt-horas.cod-model = tt-apont.cod-model
                   tt-horas.ep-codigo = tt-apont.ep-codigo
                   tt-horas.cod-eqpto = tt-apont.cod-eqpto.

            IF tt-apont.dat-fim = dataCont THEN DO:
                ASSIGN tt-horas.hra-final = tt-apont.hra-final.
            END.
            ELSE DO:
                ASSIGN tt-horas.hra-final = "235959":U.
            END.

            IF tt-apont.dat-ini = dataCont THEN DO:
                ASSIGN tt-horas.hra-inicial = tt-apont.hra-inicial.
            END.
            ELSE DO:
                ASSIGN tt-horas.hra-inicial = "000000":U.
            END.
        END.
    END.
    ELSE DO:
        CREATE tt-horas.
        ASSIGN tt-horas.ep-codigo   = tt-apont.ep-codigo
               tt-horas.cod-eqpto   = tt-apont.cod-eqpto
               tt-horas.cod-model   = tt-apont.cod-model
               tt-horas.hra-inicial = tt-apont.hra-inicial
               tt-horas.hra-final   = tt-apont.hra-final
               tt-horas.dat-movto   = tt-apont.dat-ini
               tt-horas.cod-model   = tt-apont.cod-model.
    END.
END.
/** EMPTY TEMP-TABLE tt-horas-aux.  **/
FOR EACH  tt-horas
    WHERE tt-horas.cod-model = cTag
    AND   tt-horas.ep-codigo = iEmp
    AND   tt-horas.cod-eqpto = cEqpto:

    /** Valida‡Æo para o calculo dia a dia, considera apenas data informada **/
    IF iTipo = 1 OR iTipo = 2 THEN
        IF tt-horas.dat-movto <> dData THEN NEXT.

    /** Valida‡Æo para Media da Disponibilidade **/
    IF iTipo = 999 OR iTipo = 888 THEN
        IF NOT(tt-horas.dat-movto >= ttSelecao.periodo-ini  AND tt-horas.dat-movto <= ttSelecao.periodo-fim) THEN NEXT.

    IF NOT CAN-FIND(FIRST tt-horas-aux
                    WHERE tt-horas-aux.dat-movto = tt-horas.dat-movto
                    AND   tt-horas-aux.ep-codigo = tt-horas.ep-codigo
                    AND   tt-horas-aux.cod-eqpto = tt-horas.cod-eqpto
                    AND   tt-horas-aux.cod-model = tt-horas.cod-model) THEN DO:

        CREATE tt-horas-aux.
        ASSIGN tt-horas-aux.ep-codigo   = tt-horas.ep-codigo
               tt-horas-aux.cod-eqpto   = tt-horas.cod-eqpto
               tt-horas-aux.cod-model   = tt-horas.cod-model
               tt-horas-aux.hra-inicial = tt-horas.hra-inicial
               tt-horas-aux.hra-final   = tt-horas.hra-final
               tt-horas-aux.dat-movto   = tt-horas.dat-movto
               tt-horas-aux.primeiro    = YES.
        ASSIGN controle = "":U.
    END.
    ELSE DO:
        FOR EACH  tt-horas-aux
            WHERE tt-horas-aux.dat-movto = tt-horas.dat-movto
            AND   tt-horas-aux.ep-codigo = tt-horas.ep-codigo
            AND   tt-horas-aux.cod-eqpto = tt-horas.cod-eqpto
            AND   tt-horas-aux.cod-model = tt-horas.cod-model:

            IF (tt-horas-aux.hra-inicial > tt-horas.hra-inicial  AND
                tt-horas-aux.hra-inicial > tt-horas.hra-final)   OR 
               (tt-horas-aux.hra-inicial < tt-horas.hra-inicial  AND
                tt-horas-aux.hra-final   < tt-horas.hra-inicial) THEN DO:
                ASSIGN controle = "ADD":U.
            END.
            ELSE IF tt-horas-aux.hra-inicial >= tt-horas.hra-inicial AND
                    tt-horas-aux.hra-inicial <= tt-horas.hra-final   THEN DO:
                ASSIGN controle = "UPDATE":U.
                ASSIGN tt-horas-aux.hra-inicial = tt-horas.hra-inicial.

                IF tt-horas-aux.hra-final <= tt-horas.hra-final THEN ASSIGN tt-horas-aux.hra-final = tt-horas.hra-final.
            END.
            ELSE IF tt-horas-aux.hra-inicial <= tt-horas.hra-inicial AND
                    tt-horas-aux.hra-final   >= tt-horas.hra-inicial   THEN DO:
                ASSIGN controle = "UPDATE":U.
                IF tt-horas-aux.hra-final <= tt-horas.hra-final THEN ASSIGN tt-horas-aux.hra-final = tt-horas.hra-final.
            END.
            ELSE DO:
                ASSIGN controle = "":U.
            END.
        END.
    END.

    IF controle = "ADD":U THEN DO:
        FOR FIRST bf-ttHoras
            WHERE bf-ttHoras.dat-movto   = tt-horas.dat-movto
            AND   bf-ttHoras.ep-codigo   = tt-horas.ep-codigo
            AND   bf-ttHoras.cod-eqpto   = tt-horas.cod-eqpto
            AND   bf-ttHoras.cod-model   = tt-horas.cod-model
            AND   bf-ttHoras.hra-final   = tt-horas.hra-final
            AND   bf-ttHoras.hra-inicial = tt-horas.hra-inicial:
        END.
        IF NOT AVAIL bf-ttHoras THEN DO:
            CREATE tt-horas-aux.
            ASSIGN tt-horas-aux.ep-codigo   = tt-horas.ep-codigo
                   tt-horas-aux.cod-eqpto   = tt-horas.cod-eqpto
                   tt-horas-aux.cod-model   = tt-horas.cod-model
                   tt-horas-aux.hra-inicial = tt-horas.hra-inicial
                   tt-horas-aux.hra-final   = tt-horas.hra-final
                   tt-horas-aux.dat-movto   = tt-horas.dat-movto
                   tt-horas-aux.primeiro    = NO.
        END.
    END.
END.

FOR EACH  bf-ttHoras
    WHERE bf-ttHoras.primeiro  = YES
    AND   bf-ttHoras.ep-codigo = iEmp
    AND   bf-ttHoras.cod-eqpto = cEqpto
    AND   bf-ttHoras.cod-model = cTag:

    FOR FIRST tt-horas-aux
        WHERE tt-horas-aux.dat-movto     = bf-ttHoras.dat-movto
        AND   tt-horas-aux.ep-codigo     = bf-ttHoras.ep-codigo     /** Diferen‡a para o MI, aqui se valida o equipamento **/
        AND   tt-horas-aux.cod-eqpto     = bf-ttHoras.cod-eqpto     /** Diferen‡a para o MI, aqui se valida o equipamento **/
        AND   tt-horas-aux.cod-model     = bf-ttHoras.cod-model     /** Diferen‡a para o MI, aqui se valida o equipamento **/
        AND   tt-horas-aux.primeiro      = NO
        AND   ((tt-horas-aux.hra-inicial = bf-ttHoras.hra-inicial AND tt-horas-aux.hra-final  = bf-ttHoras.hra-final)
           OR  (tt-horas-aux.hra-inicial = bf-ttHoras.hra-inicial AND tt-horas-aux.hra-final  < bf-ttHoras.hra-final)
           OR  (tt-horas-aux.hra-inicial  > bf-ttHoras.hra-inicial AND tt-horas-aux.hra-final  < bf-ttHoras.hra-final)
           OR  (tt-horas-aux.hra-inicial  > bf-ttHoras.hra-inicial AND tt-horas-aux.hra-final  = bf-ttHoras.hra-final)) :
        DELETE tt-horas-aux.
    END.
END.

ASSIGN dHorasDia = 0
       dFim      = 0
       dIni      = 0.

IF iTipo = 1 THEN DO:
    FOR EACH  tt-horas-aux
        WHERE tt-horas-aux.ep-codigo = iEmp
        AND   tt-horas-aux.cod-eqpto = cEqpto
        AND   tt-horas-aux.cod-model = cTag
        AND   tt-horas-aux.dat-movto = dData :

        ASSIGN dFim = ((INT(SUBSTRING(tt-horas-aux.hra-final,1,2)) * 60)   + (INT(SUBSTRING(tt-horas-aux.hra-final,3,2)))) / 60.
        ASSIGN dIni = ((INT(SUBSTRING(tt-horas-aux.hra-inicial,1,2)) * 60) + (INT(SUBSTRING(tt-horas-aux.hra-inicial,3,2)))) / 60.

        IF (dFim - dIni) >= 23.93 THEN DO:
            ASSIGN dHorasDia = dHorasDia + 24.
        END.
        ELSE DO:
            ASSIGN dHorasDia = dHorasDia + (dFim - dIni).
        END.
    END.
END.
ELSE DO:
    FOR EACH  tt-horas-aux
        WHERE tt-horas-aux.ep-codigo = iEmp
        AND   tt-horas-aux.cod-eqpto = cEqpto
        AND   tt-horas-aux.cod-model = cTag:

        ASSIGN dFim = ((INT(SUBSTRING(tt-horas-aux.hra-final,1,2)) * 60)   + (INT(SUBSTRING(tt-horas-aux.hra-final,3,2)))) / 60.
        ASSIGN dIni = ((INT(SUBSTRING(tt-horas-aux.hra-inicial,1,2)) * 60) + (INT(SUBSTRING(tt-horas-aux.hra-inicial,3,2)))) / 60.

        IF (dFim - dIni) >= 23.93 THEN DO:
            ASSIGN dHorasDia = dHorasDia + 24.
        END.
        ELSE DO:
            ASSIGN dHorasDia = dHorasDia + (dFim - dIni). 
        END.
    END.
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */


/* ************************  Function Implementations ***************** */


FUNCTION fnBrowse RETURNS CHARACTER
  ( pCampo as character,
    iTipo  as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  fnBrowse
    Notes:  Busca os t¡tulos dos campos do browse
------------------------------------------------------------------------------*/
DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

if iTipo = 1 then do:
    case pCampo:
        when "cod-oficial":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Equipamento",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "desc-dimensao":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Descri‡Æo",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.                                                 
        when "mtbf":U then do:                       
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "MTBF",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
                     
        end.                                                   
        when "mttr":U then do:                                           
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "MTTR",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "dispo":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Disponibilidade",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "pmpl":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "PMPL",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "mat":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Material",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "ggf":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Pessoal_e_Indiretos",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "serv":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Servi‡os",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "contratos":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Contratos",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "custo":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Custo_Hora",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "tot":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Total",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "nr-ord-produ":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Ordem",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "des-man-corr":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Descri‡Æo",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "estado":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Estado_OM",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "cod-eqpto":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Equipamento",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "dt-manut":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Data Manuten‡Æo",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
        when "dt-fecham":U then do:
            /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Data Termino",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
        end.
    end case.
    assign cRetorno = trim(return-value).
end.
else do:
    case pCampo:
        when "cod-oficial":U then do:
            assign cRetorno = bfttDados2.cod-oficial.
        end.
        when "desc-dimensao":U then do:
            assign cRetorno = bfttDados2.desc-dimensao.
        end.
        when "mtbf":U then do:
            assign cRetorno = string(bfttDados2.mtbf,"->>>,>>>,>>9.99").
        end.
        when "mttr":U then do:                        
            assign cRetorno = string(bfttDados2.mttr,"->>>,>>>,>>99").
        end.
        when "dispo":U then do:
            assign cRetorno = string(bfttDados2.dispo,"->,>>>,>>9.99").
        end.
        when "pmpl":U then do:
            assign cRetorno = string(bfttDados2.pmpl,"->>>,>>>,>>9.99").
        end.
        when "mat":U then do:
            assign cRetorno = string(bfttDados2.mat,"->>>,>>>,>>9.99").
        end.
        when "ggf":U then do:
            assign cRetorno = string(bfttDados2.ggf,">>>>,>>>,>>9.9999").
        end.
        when "serv":U then do:
            assign cRetorno = string(bfttDados2.serv,"->>>,>>>,>>9.99").
        end.
        when "contratos":U then do:
            assign cRetorno = string(bfttDados2.contratos,"->>>,>>>,>>9.99").
        end.
        when "custo":U then do:
            assign cRetorno = string(bfttDados2.custo,"->>>,>>>,>>9.99").
        end.
        when "tot":U then do:
            assign cRetorno = string(bfttDados2.tot,"->>>,>>>,>>9.99").
        end.
    end case.
end.

RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */



FUNCTION fnEstado RETURNS CHARACTER
  ( pEstado as integer /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN /**************************************************************************
** i01in271.i  - campo: estado   ( tabela: ord-prod )
**************************************************************************/

 






/***************************************************************************
**  ind01-10.i - define as funcoes de um indicador
**  Para indicadores de 1 a 10 items
**
**  Funcoes disponiveis
**  01: view-as Combo-box
**  02: view-as radio-set
**  03: lista com os itens separados por virgula
**  04 n: retorna o item n da lista
**  05: retorna o numero de items da lista
**  06: retorna a posicao do item (numero)
**  07: valores para a propriedade Radio-Buttons de um Radio-Set
***************************************************************************/

/* verifica parametros ****************************************************/




/* &if lookup("{1}", "01,02,03,04,05,06,07") = 0 &then
    &message *** ({&file-name}): Parametro incorreto: {1} !
    &message *** Deveria ser: 01, 02, 03, 04, 05, 06, 07 
&endif  */
    

  


/* monta lista de items para LISTA (03), NUM (04), ITEM(05), IND(06) ************************/

          
               
     
               
     
               
     
               
     
               
     
               
     
               
     
     


/* funcao Combo-box (01) *************************************************************/


/* funcao Radio-set (02) *************************************************************/


/* funcao Lista (03) **********************************************************/


/* funcao NUM (05) ************************************************************/


/* funcao Item n (04) *********************************************************/    


     entry(pEstado, "NÆo Iniciada,Liberada,Alocada,Separada,Requisitada,Iniciada,Finalizada,Terminada")



/* funcao IND string (06) ****************************************************/



/* valores para a propriedade Radio-Buttons de um Radio-Set *******************/




    

    

    

    

    

    

    

    

    



/* fim */
 
/* Fim */

 .   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */



FUNCTION fnLabels RETURNS CHARACTER
  ( pImage as int ) :
/*------------------------------------------------------------------------------
  Purpose:  fnLabels
    Notes:  Busca a label da imagem pasada
------------------------------------------------------------------------------*/

DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

    /** T¡tulo do browse **/
    /********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/*******************************************************************************
*   {1}  N£mero da imagem
*******************************************************************************/

case pImage:
    when 1 then do:
        /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Empresa",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    end.
    when 2 then do:
        /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Estabelecimento",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    end.
    when 3 then do:
        /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Modelo_/_Fam¡lia_Equipamento",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    end.
    when 4 then do:
        /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Equipamento",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    end.
    when 5 then do:
        /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "TAG",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    end.
    when 6 then do:
        /**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Dia_a_Dia",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
    end.
end case.
 

    assign cRetorno = trim(return-value).

  RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */



FUNCTION FormataDados RETURNS LOGICAL
  (pRange as char,pCor as int,pCorFundo as int,pFonte as int,pValue as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    assign chWorksheet:Range(pRange):MergeCells               = true
           chWorksheet:Range(pRange):HorizontalAlignment      = -4108
           chWorksheet:Range(pRange):VerticalAlignment        = -4108
           chWorksheet:Range(pRange):Font:Size                = pFonte
           chWorksheet:Range(pRange):Interior:ColorIndex      = pCorFundo
           chWorksheet:Range(pRange):Font:Bold                = true.

    if pValue <> "" then
        assign chWorksheet:Range(pRange):Value = pValue.

    if pCor <> ? then
        assign chWorksheet:Range(pRange):Font:ColorIndex = pCor.

    return true.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */


