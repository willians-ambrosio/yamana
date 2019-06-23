/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*****************************************************************************
**
**       PROGRAMA: esmv0501.i
**
**       DATA....: Mar‡o de 208
**
**       AUTOR...:Leonardo Correia Santos de Oliveira - Manufatura - DATASUL S.A.
**
**       OBJETIVO: Include de defini‡Æo de forms e temp-tables
**
*****************************************************************************/


/************************Defini‡Æo de Vari veis *************/

DEFINE VARIABLE c-liter-cla    AS CHARACTER format "x(14)" NO-UNDO.
DEFINE VARIABLE c-liter-par    AS CHARACTER format "x(14)" NO-UNDO.
DEFINE VARIABLE c-liter-imp    AS CHARACTER format "x(14)" NO-UNDO.
DEFINE VARIABLE c-destino      AS CHARACTER format "x(09)" NO-UNDO.
DEFINE VARIABLE lTarefas       AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lMateriais     AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lEpi           AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lFerramentas   AS LOGICAL    NO-UNDO.  
DEFINE VARIABLE lFicha         AS LOGICAL    NO-UNDO.  
DEFINE VARIABLE lCompartimento AS LOGICAL    NO-UNDO.  
DEFINE VARIABLE lEventos       AS LOGICAL    NO-UNDO.  
DEFINE VARIABLE lAnexos        AS LOGICAL    NO-UNDO.  
DEFINE VARIABLE lUltima        AS LOGICAL    NO-UNDO.  
DEFINE VARIABLE lObs           AS LOGICAL    NO-UNDO. 
DEFINE VARIABLE i-num-ficha    AS INTEGER    NO-UNDO.
DEFINE VARIABLE cSusSist       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE l-param        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lItens         AS LOGICAL    NO-UNDO.

/************************Defini‡Æo de Vari veis de Layout*************/
DEFINE VARIABLE cl-data               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-pagina              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-empresa            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-pagina             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-titulo             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-documento          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-abertura           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-entrada            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-tp-om              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-prev-term          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-termino            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-estado             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-conta              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-eqpto              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-oficina            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-contador           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-subsist            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-compon             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-tp-manut           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-planej             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-tarefas            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-itens              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-aceite             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-numTarefa          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-sistema            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-descricao1          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-descricao1         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-plano              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-durabilidade       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-compte             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-reformado          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-retirado           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-colocada           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-localizacao        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-garantia           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-estimativa         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-dias               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-dias2              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-matl               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-item               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-quantde            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-quantde1           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-epi                AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-equipamento        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-ferramenta_title   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-ferramenta         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-ferramenta          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-ficha-met          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-fichas             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-comptos            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-compartimentos      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-eventos            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-anexo              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-descricao          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-docto2             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-ult-manut          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-subsist3           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-event3             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-data2              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-tp-manut2          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-observacao         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-index               AS INTEGER    NO-UNDO.
DEFINE VARIABLE cl-hr-movto           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-abertuta           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-sub-sist           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-event1             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-event2             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-subsist2           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-docto              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-anexos             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-linha1              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-linha2              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-linha3              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-num-ord             AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-data-abert          AS DATE       NO-UNDO.
DEFINE VARIABLE c-hora-abert          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-data-entrada        AS DATE       NO-UNDO.
DEFINE VARIABLE c-hora-entrada        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-tp-om               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-data-prev           AS DATE       NO-UNDO.
DEFINE VARIABLE c-hora-prev           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-estado              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-data-term           AS DATE       NO-UNDO.
DEFINE VARIABLE c-hora-term           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-conta               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cc-custo              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-cod-emp             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cod-eqpto           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-model               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-val-hod             AS DEC        NO-UNDO.
DEFINE VARIABLE c-un                  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cod-sub-sit         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-sub-sist        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cod-comp            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-model           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-tp-manut            AS INT        NO-UNDO.
DEFINE VARIABLE c-descricao           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-plane               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE  c-nome               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-num-eq              AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-cod-evento1         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-event1          AS CHARACTER  format "x(20)" NO-UNDO.
DEFINE VARIABLE c-sub-sit1            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-sub-sist1       AS CHARACTER  format "x(21)" NO-UNDO.
DEFINE VARIABLE c-plano               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE d-durabilidade        AS decimal    format ">>,>>>,>>9.9" NO-UNDO.
DEFINE VARIABLE c-reformado           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-model2          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-sit-1               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-retirado            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-sit-2               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-model3          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-colocada            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-model4          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-sit-3               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-localizacao         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-num-dias-1          AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-val-garantia        AS DEC        NO-UNDO.
DEFINE VARIABLE c-un-garantia         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-num-dias-2          AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-val-estimativa      AS DEC        NO-UNDO.
DEFINE VARIABLE c-un-estimativa       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-item                AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-item            AS CHARACTER  format "x(40)" NO-UNDO.
DEFINE VARIABLE c-qtde-1              AS DEC        NO-UNDO.
DEFINE VARIABLE c-epi                 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-epi             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-qtde-2              AS DEC        NO-UNDO.
DEFINE VARIABLE c-cod-ferr            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-desc-ferr           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-ficha               AS INT        NO-UNDO.
DEFINE VARIABLE c-des-ficha           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cod-compto          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-compto          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-evento-1            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-evento-1        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-evento-2            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-evento-2        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-sub-sist-2          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-sub-sist-2      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-num-docto-2         AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-num-docto-3         AS CHAR       NO-UNDO.
DEFINE VARIABLE c-descricao-2         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-sub-sist-3      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-event-3         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-des-tp-manut-3      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-data-movto          AS DATE       NO-UNDO.
DEFINE VARIABLE c-obs-1               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-obs-2               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-obs-3               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-obs-4               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-obs-5               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-model-eqpto         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-model-compn         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-cont                AS INT        NO-UNDO.
DEFINE VARIABLE iep-cod-ordem         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-criacao            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-dat-criacao         AS DATE       NO-UNDO.
DEFINE VARIABLE c-hr-cricao           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-conta2             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iep-conta-desp        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cont-desp           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cc-cond-desp          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cod-usuar           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-usuar              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-estabel1           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cod-estabel         AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cl-Causa              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cod-Causa           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-Sintoma            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cod-Sintoma            AS CHARACTER  NO-UNDO.

DEFINE VARIABLE iTpOrdManut           AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCompon               AS INTEGER    NO-UNDO.
DEFINE VARIABLE iComSubSist           AS INTEGER    NO-UNDO.
 DEFINE VARIABLE inum-seq             AS INTEGER    NO-UNDO.
DEFINE VARIABLE cCodPlano             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCodSubSist           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCodEvento            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dValDtHr              AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iTpManut              AS INTEGER    NO-UNDO.
DEFINE VARIABLE cDescSitEqpto         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDescSitEqpto1        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-conteudo            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCod-Ofici            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDes-Ofici            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-Itens               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE clItens               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE l-durabilidade        AS LOGICAL    NO-UNDO.
/** Form Requisi‡Æo **/
DEFINE VARIABLE i-nr-requisicao       AS INT format ">>>,>>>,>>9":U    NO-UNDO.
DEFINE VARIABLE i-sequencia           AS INT format ">>>>>":U          NO-UNDO.
DEFINE VARIABLE c-it-codigo           AS CHARACTER format "x(16)":U    NO-UNDO.
DEFINE VARIABLE d-dat-trans           AS DATE   format "99/99/9999":U  NO-UNDO.
DEFINE VARIABLE c-tp-requis           AS CHARACTER format "x(8)":U     NO-UNDO.
DEFINE VARIABLE c-situacao            AS CHARACTER format "x(11)":U     NO-UNDO.
DEFINE VARIABLE c-estado-req          AS CHARACTER format "x(11)":U    NO-UNDO.

DEFINE VARIABLE c-dsl-obs             AS CHARACTER FORMAT "x(36)" NO-UNDO.

/* Planos vencidos e a vencer */
def var l-planovenc    as log  no-undo.
def var cl-planovencav as char no-undo.
def var cl-planovenc   as char no-undo.
def var cl-tarefa      as char no-undo.
def var cl-vencto      as char no-undo.
def var cl-diferenca   as char no-undo.
def var c-planovenc    as char no-undo.
def var c-tarefa       as char no-undo.
DEFINE VARIABLE i-num-tarefa AS INTEGER NO-UNDO.
def var c-vencto       as char no-undo.
def var d-diferenca    as dec  no-undo.


DEFINE VARIABLE c-executante      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-data-tarefa     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-data-inicial    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-data-final      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-tarefas         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-assinatura      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-supervisor      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE C-data-assinatura AS CHARACTER  NO-UNDO.

DEFINE VARIABLE c-observacao      AS CHARACTER  NO-UNDO.

DEFINE VARIABLE c-causa        AS CHARACTER FORMAT "x(28)" NO-UNDO.
DEFINE VARIABLE c-causa-desc   AS CHARACTER FORMAT "x(27)" NO-UNDO.
DEFINE VARIABLE c-sintoma      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-sintoma-desc AS CHARACTER FORMAT "x(25)" NO-UNDO.



/****Temp table do editor**/
def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(84)"
    index editor-id is primary unique linha.

/** Defini‡Æo da Temp-table de parƒmetros**/
DEF temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    FIELD formato              as integer
    field v_num_tip_aces_usuar as integer
    field i-ep-codigo          as integer
    field l-tarefas            as logical  
    field l-materiais          as logical  
    field l-epi                as logical  
    field l-ferramentas        as logical  
    field l-fichas             as logical  
    field l-compartimentos     as logical  
    field l-eventos            as logical  
    field l-anexos             as logical  
    field l-ultima             as logical  
    field l-obs                as logical  
    field c-ord-ini            like mmv-ord-manut.nr-ord-produ
    field c-ord-fim            like mmv-ord-manut.nr-ord-produ
    field c-oficina-ini        like mmv-ord-manut.cod-ofici
    field c-oficina-fim        like mmv-ord-manut.cod-ofici
    field c-data-fim           like mmv-ord-manut.dat-abert
    field c-data-ini           like mmv-ord-manut.dat-abert
    field c-empresa-fim        like mmv-ord-manut.ep-codigo
    field c-empresa-ini        like mmv-ord-manut.ep-codigo
    field c-eqpto-fim          like mmv-ord-manut.cod-eqpto
    field c-eqpto-ini          like mmv-ord-manut.cod-eqpto
    field c-componente-ini     like mmv-ord-manut.cod-compon
    field c-componente-fim     like mmv-ord-manut.cod-compon
    field tag-ini              like mab-eqpto.cd-tag
    field tag-fim              like mab-eqpto.cd-tag
    field cc-ini               like mab-histor-ativid.cc-codigo
    field cc-fim               like mab-histor-ativid.cc-codigo
    field i-num-ficha          as integer  format ">>9" INITIAL 0
    field c-sub-sist           like mmv-ord-manut.cod-sub-sist
    field l-param              as logical
    field l-QuebraOM           as logical
    field l-Eqpto              as logical
    field l-Comp               as logical
    field l-Custo              as logical
    field l-NIniciada          as logical
    field l-Iniciada           as logical
    field l-Itens              as logical
    field l-Terminada          as logical
    FIELD l-estoque            AS LOGICAL
    FIELD l-compras            AS LOGICAL
    FIELD l-aprovado           AS LOGICAL
    FIELD l-reprovado          AS LOGICAL
    FIELD l-aberta             AS LOGICAL
    FIELD l-fechada            AS LOGICAL
    FIELD l-pendente           AS LOGICAL
    FIELD l-ComOrdem           AS LOGICAL
    field l-linhas             as logical
    field nr-linhas            as integer format ">9"
    FIELD l-PlanoVenc          AS LOGICAL
    FIELD l-durabilidade       AS LOGICAL.

define temp-table tt-materiais no-undo
    field nr-ord-produ         as integer
    field it-codigo            as character
    field qt-item              as decimal
    field qt-atend             as decimal
    FIELD tarefa               AS INTEGER
    index id is primary unique nr-ord-produ it-codigo.
/*****************************************************************************/
                                                     /** Form de Parƒmetros **/
DEFINE VARIABLE c-liter-sel  AS CHARACTER format "x(09)" NO-UNDO.

form   /*SELE€ÇO*/
       skip(1)
       c-liter-sel                AT 09    no-label
       skip(1)
       tt-param.c-ord-ini         colon 30  "|<  >|" at 50 
       tt-param.c-ord-fim         no-label 
       tt-param.c-oficina-ini     colon 30  "|<  >|" at 50 
       tt-param.c-oficina-fim     NO-LABEL 
       tt-param.c-data-ini        format "99/99/9999" colon 30  "|<  >|" at 50
       tt-param.c-data-fim        format "99/99/9999" no-label
       tt-param.c-empresa-ini     colon 30  "|<  >|" at 50
       tt-param.c-empresa-fim     no-label
       tt-param.c-eqpto-ini       colon 30  "|<  >|" at 50
       tt-param.c-eqpto-fim       no-label
       tt-param.c-componente-ini  colon 30  "|<  >|" at 50
       tt-param.c-componente-fim  no-label
       tt-param.tag-ini           colon 30  "|<  >|" at 50
       tt-param.tag-fim           no-label
       tt-param.cc-ini            colon 30  "|<  >|" at 50
       tt-param.cc-fim            no-label
       /*PAR¶METRO*/
       skip(1)
       c-liter-par                AT 09    NO-LABEL
       skip(1)
       tt-param.l-tarefas         COLON 30
       tt-param.l-materiais       COLON 30
       tt-param.l-epi             COLON 30
       tt-param.l-ferramentas     COLON 30
       tt-param.l-fichas          COLON 30
       tt-param.l-compartimentos  COLON 30
       tt-param.l-eventos         COLON 30
       tt-param.l-anexos          COLON 30
       tt-param.l-ultima          COLON 30
       tt-param.l-obs             COLON 30
       tt-param.i-num-ficha       COLON 30
       tt-param.c-sub-sist        COLON 30
       tt-param.l-Eqpto           COLON 30
       tt-param.l-Comp            COLON 30
       tt-param.l-Custo           COLON 30
       tt-param.l-NIniciada       COLON 30
       tt-param.l-Iniciada        COLON 30
       tt-param.l-Itens           COLON 30
       tt-param.l-Terminada       COLON 30
       SKIP(1)
       tt-param.l-estoque         COLON 30
       tt-param.l-compras         COLON 30
       tt-param.l-aprovado        COLON 30
       tt-param.l-reprovado       COLON 30
       tt-param.l-aberta          COLON 30
       tt-param.l-fechada         COLON 30
       tt-param.l-pendente        COLON 30
       tt-param.l-comordem        COLON 30
       skip(1)
       tt-param.l-linhas          colon 30
       tt-param.nr-linhas         colon 30
       SKIP(1)
       tt-param.l-PlanoVenc       COLON 30
       /*IMPRESSÇO*/
       skip(1)
       c-liter-imp              AT 09    no-label
       skip(1)
       c-destino                COLON 30 " - " tt-param.arquivo format "X(30)" NO-LABEL
       tt-param.usuario         COLON 30
       tt-param.l-param         COLON 30
       tt-param.l-QuebraOM      COLON 30
       skip(1)
       with width 132 side-labels frame f-param-definidos stream-io.
/*****************************************************************************/
                                               /** Literais Form Parƒmetros **/
{utp/ut-liter.i Usu rio * l}
assign tt-param.usuario:label in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i Destino * l}
assign c-destino:label in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i SELE€ÇO * R}
assign c-liter-sel = trim(return-value).
{utp/ut-liter.i PAR¶METROS * R}         
assign c-liter-par = trim(return-value).
{utp/ut-liter.i IMPRESSÇO * R}          
assign c-liter-imp = trim(return-value).
{utp/ut-liter.i "Ordem" * R}
ASSIGN tt-param.c-ord-ini:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Oficina" * R}
ASSIGN tt-param.c-oficina-ini:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Data Abertura" * R}
ASSIGN tt-param.c-data-ini :LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Empresa" * R}
ASSIGN tt-param.c-empresa-ini:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Equipamento" * R}
ASSIGN tt-param.c-eqpto-ini:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Componente" * R}
ASSIGN tt-param.c-componente-ini:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Tarefas" * R}
ASSIGN tt-param.l-tarefas:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Materiais" * R}
ASSIGN tt-param.l-materiais:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "EPI" * R}
ASSIGN tt-param.l-epi:LABEL in frame f-param-definidos = trim(return-value).
ASSIGN cl-epi = trim(return-value).
{utp/ut-liter.i "Ferramentas" * R}
ASSIGN tt-param.l-ferramentas:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Ficha de M‚todos" * R}
ASSIGN tt-param.l-fichas:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Eventos vencidos" * R}
ASSIGN tt-param.l-compartimentos:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Eventos nÆo disparados" * R}
ASSIGN tt-param.l-eventos:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Anexos" * R}
ASSIGN tt-param.l-anexos:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "éltimas Manuten‡äes" * R}
ASSIGN tt-param.l-ultima:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Observa‡äes" * R}
ASSIGN tt-param.l-obs:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Imprime_Parƒmetros" * R}
ASSIGN tt-param.l-param:LABEL in frame f-param-definidos = trim(return-value).
/* {utp/ut-liter.i "Quebra_P gina_por_OM" * R}                                       */
/* ASSIGN tt-param.l-QuebraOM:LABEL in frame f-param-definidos = trim(return-value). */
{utp/ut-liter.i "Quantidade de Fichas" * R}
ASSIGN tt-param.i-num-ficha:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i Requisi‡Æo_Estoque * }
ASSIGN tt-param.l-estoque:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i Solicita‡Æo_Compras * }
ASSIGN tt-param.l-compras:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i Aprovado * }
ASSIGN tt-param.l-aprovado:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i Reprovado * }
ASSIGN tt-param.l-reprovado:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i Aberta * }
ASSIGN tt-param.l-aberta:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i Fechada * }
ASSIGN tt-param.l-fechada:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i Pendente * }
ASSIGN tt-param.l-pendente:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i Com_Ordem * }
ASSIGN tt-param.l-comordem:LABEL in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Linhas Adicionais Tarefas"}
assign tt-param.l-linhas:label in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Linhas"}
assign tt-param.nr-linhas:label in frame f-param-definidos = trim(return-value).
{utp/ut-liter.i "Planos vencidos e a vencer"}
assign tt-param.l-PlanoVenc:label in frame f-param-definidos = trim(return-value).

{utp/ut-liter.i "Sub-Sistema" * R}
ASSIGN tt-param.c-sub-sist:LABEL in frame f-param-definidos = trim(return-value).
assign tt-param.l-Eqpto:label in frame f-param-definidos     = {frinc/i00fr072.i 04 01}
       tt-param.l-Comp:label in frame f-param-definidos      = {frinc/i00fr072.i 04 02}
       tt-param.l-Custo:label in frame f-param-definidos     = {frinc/i00fr072.i 04 03}
       tt-param.l-NIniciada:label in frame f-param-definidos = {ininc/i01in271.i 04 01}
       tt-param.l-Iniciada:label in frame f-param-definidos  = {ininc/i01in271.i 04 06}
       tt-param.l-Itens:label in frame f-param-definidos     = {ininc/i01in271.i 04 06}
       tt-param.l-Terminada:label in frame f-param-definidos = {ininc/i01in271.i 04 08}.
{utp/ut-liter.i Sim/NÆo}
assign  tt-param.l-tarefas:format        in frame f-param-definidos = return-value
        tt-param.l-materiais:format      in frame f-param-definidos = return-value
        tt-param.l-epi:format            in frame f-param-definidos = return-value       
        tt-param.l-ferramentas:format    in frame f-param-definidos = return-value     
        tt-param.l-fichas:format         in frame f-param-definidos = return-value     
        tt-param.l-compartimentos:format in frame f-param-definidos = return-value    
        tt-param.l-eventos:format        in frame f-param-definidos = return-value      
        tt-param.l-anexos:format         in frame f-param-definidos = return-value      
        tt-param.l-ultima:format         in frame f-param-definidos = return-value         
        tt-param.l-obs:format            in frame f-param-definidos = return-value
        tt-param.l-param:format          in frame f-param-definidos = return-value
        tt-param.l-QuebraOM:format       in frame f-param-definidos = return-value
        tt-param.l-Eqpto:format          in frame f-param-definidos = return-value
        tt-param.l-Comp:format           in frame f-param-definidos = return-value
        tt-param.l-Custo:format          in frame f-param-definidos = return-value
        tt-param.l-NIniciada:format      in frame f-param-definidos = return-value
        tt-param.l-Iniciada:format       in frame f-param-definidos = return-value
        tt-param.l-Itens:format          in frame f-param-definidos = return-value
        tt-param.l-Terminada:format      in frame f-param-definidos = return-value
        tt-param.l-estoque:format        in frame f-param-definidos = return-value
        tt-param.l-compras:format        in frame f-param-definidos = return-value
        tt-param.l-aprovado:format       in frame f-param-definidos = return-value
        tt-param.l-reprovado:format      in frame f-param-definidos = return-value
        tt-param.l-aberta:format         in frame f-param-definidos = return-value
        tt-param.l-fechada:format        in frame f-param-definidos = return-value
        tt-param.l-pendente:format       in frame f-param-definidos = return-value
        tt-param.l-comordem:format       in frame f-param-definidos = return-value
        tt-param.l-linhas:format         in frame f-param-definidos = return-value
        tt-param.l-PlanoVenc:format      in frame f-param-definidos = return-value.
  
/*****************************************************************************/
                                                       /** Forms do Usu rio **/
FORM HEADER 
     "+"                                        AT  1 
     FILL("-":U, 82)    format "x(82)":U        AT  2  
     "+"                                        AT 84 SKIP 
     "|"                                        AT  1 
     cl-data            format "x(04)"          AT  2
     ":"                                        AT  6  
     today format "99/99/9999":U                AT  8 
     cl-empresa         format "x(41)"          AT 24
     cl-pagina          format "x(06)"          AT 65 
     ":"                                        AT 71 
     PAGE-NUMBER format ">>>>>9":U              AT 73   
     "|"                                        AT 84 skip
     "|"                                        AT  1
     cl-hr-movto        format "x(04)"          AT  2 
     ":"                                        AT  6 
     string(TIME,"HH:MM")                       AT  8
     "ESMV0501"                                 AT 18
     "-"                                        AT 27
     cl-titulo          format "x(40)"          AT 29 
     "|"                                        AT 84 skip
     "|"                                        AT  1 
     cl-documento       format "x(19)"          AT  2     
     ":"                                        AT 21 
     i-num-ord        format "->,>>>,>>>,>>9"   AT 22
     cl-abertura        format "x(08)"          AT 51
     ":"                                        AT 59 
     c-data-abert      format "99/99/9999":U    AT 61
     c-hora-abert      format "99:99":u         AT 74 
     "|"                                        AT 84 SKIP
     "|"                                        AT 1
     cl-entrada     format "x(07)"              AT 51
     ":"                                        AT 59 
     c-data-entrada format "99/99/9999":U       AT 61
     c-hora-entrada format "99:99":u            AT 74
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-tp-om     format "x(07)"                AT 10   
     cl-prev-term format "x(12)"                AT 47  
     ":"                                        AT 59  
     c-data-prev  format "99/99/9999":U         AT 61
     c-hora-prev  format "99:99":u              AT 74
     "|"                                        AT 84 SKIP
     "|"                                        AT  1 
     cl-oficina       format "x(7)"             AT  5
     ":"                                        AT 12
     ccod-ofici      format "x(03)"             at 14 no-label
     Cdes-ofici      format "x(30)"             at 19
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-eqpto       format "x(11)"              AT  5
     ":"                                        AT 16
     i-cod-emp      format ">>9"                AT 18
     "-"                                        AT 22
     c-cod-eqpto                                AT 24
     cl-termino     format "x(07)"              AT 51
     ":"                                        AT 59
     c-data-term    format "99/99/9999":U       AT 61
     c-hora-term    format "99:99":u            AT 74
     "|"                                        AT 84 SKIP
     "|"                                        AT 1
     cl-tp-manut format "x(15)"                 AT 5 
     ":"                                        AT 20
     c-tp-manut   format ">>9"                  AT 21
     "-"                                        AT 25
     c-descricao   format "x(22)"               AT 27
     cl-criacao     format "x(07)"              AT 51
     ":"                                        AT 59
     c-dat-criacao  format "99/99/9999":U       AT 61
     c-hr-cricao    format "99:99":u            AT 74
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-planej format "x(10)"                   AT  5
     ":"                                        AT 15
     c-plane   format "x(5)"                    AT 17
     "-"                                        AT 22
     c-nome         format "x(20)"              AT 24
     cl-usuar       format "x(07)"              AT 51
     ":"                                        AT 59
     c-cod-usuar    format "x(12)"              AT 61 
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-estabel1    format "x(7)"               AT 51
     ":"                                        AT 59
     c-cod-estabel  format "x(3)"               AT 61
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-Sintoma + ":"                           AT  5
     c-sintoma-desc                             AT 15
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-Causa + ":"                             AT  7
     c-causa-desc                               AT 15
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     "|"                                        AT 84 SKIP

    with no-box width 132 NO-LABELS frame fFrameA stream-io.
    
FORM HEADER 
     "+"                                        AT  1 
     FILL("-":U, 82)    format "x(82)":U        AT  2  
     "+"                                        AT 84 SKIP 
     "|"                                        AT  1 
     cl-data            format "x(04)"          AT  2
     ":"                                        AT  6  
     today format "99/99/9999":U                AT  8 
     cl-empresa         format "x(41)"          AT 24
     cl-pagina          format "x(06)"          AT 65 
     ":"                                        AT 71 
     PAGE-NUMBER format ">>>>>9":U              AT 73   
     "|"                                        AT 84 skip
     "|"                                        AT  1
     cl-hr-movto        format "x(04)"          AT  2 
     ":"                                        AT  6 
     string(TIME,"HH:MM")                       AT  8
     "ESMV0501"                                 AT 18
     "-"                                        AT 27
     cl-titulo          format "x(40)"          AT 29 
     "|"                                        AT 84 skip
     "|"                                        AT  1 
     cl-documento       format "x(19)"          AT  2     
     ":"                                        AT 21 
     i-num-ord        format "->,>>>,>>>,>>9"   AT 22
     cl-abertura        format "x(08)"          AT 51
     ":"                                        AT 59 
     c-data-abert      format "99/99/9999":U    AT 61
     c-hora-abert      format "99:99":u         AT 74 
     "|"                                        AT 84 SKIP
     "|"                                        AT 1
     cl-entrada     format "x(07)"              AT 51
     ":"                                        AT 59 
     c-data-entrada format "99/99/9999":U       AT 61
     c-hora-entrada format "99:99":u            AT 74
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-tp-om     format "x(07)"                AT 10   
     cl-prev-term format "x(12)"                AT 47  
     ":"                                        AT 59  
     c-data-prev  format "99/99/9999":U         AT 61
     c-hora-prev  format "99:99":u              AT 74
     "|"                                        AT 84 SKIP
     "|"                                        AT  1 
     cl-oficina       format "x(7)"             AT  5
     ":"                                        AT 12
     ccod-ofici      format "x(03)"             at 14 no-label
     Cdes-ofici      format "x(30)"             at 19
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-eqpto       format "x(11)"              AT  5
     ":"                                        AT 16
     i-cod-emp      format ">>9"                AT 18
     "-"                                        AT 22
     c-cod-eqpto                                AT 24
     cl-termino     format "x(07)"              AT 51
     ":"                                        AT 59
     c-data-term    format "99/99/9999":U       AT 61
     c-hora-term    format "99:99":u            AT 74
     "|"                                        AT 84 SKIP
     "|"                                        AT 1
     cl-tp-manut format "x(15)"                 AT 5 
     ":"                                        AT 20
     c-tp-manut   format ">>9"                  AT 21
     "-"                                        AT 25
     c-descricao   format "x(22)"               AT 27
     cl-criacao     format "x(07)"              AT 51
     ":"                                        AT 59
     c-dat-criacao  format "99/99/9999":U       AT 61
     c-hr-cricao    format "99:99":u            AT 74
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-planej format "x(10)"                   AT  5
     ":"                                        AT 15
     c-plane   format "x(5)"                    AT 17
     "-"                                        AT 22
     c-nome         format "x(20)"              AT 24
     cl-usuar       format "x(07)"              AT 51
     ":"                                        AT 59
     c-cod-usuar    format "x(12)"              AT 61 
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-estabel1    format "x(7)"               AT 51
     ":"                                        AT 59
     c-cod-estabel  format "x(3)"               AT 61
    "|"                                        AT 84 SKIP
    "|"                                        AT  1
    "|"                                        AT 84 SKIP
    "|"                                        AT  1
    cl-Sintoma + ":"                           AT  5
    c-sintoma-desc                             AT 15
    "|"                                        AT 84 SKIP
    "|"                                        AT  1
    cl-Causa + ":"                             AT  7
    c-causa-desc                               AT 15
    "|"                                        AT 84 SKIP
    "|"                                        AT  1
    "|"                                        AT 84 SKIP


    with no-box width 132 NO-LABELS frame fFrameB stream-io.


FORM HEADER 
     "+"                                        AT  1 
     FILL("-":U, 82)    format "x(82)":U        AT  2  
     "+"                                        AT 84 SKIP 
     "|"                                        AT  1 
     cl-data            format "x(04)"          AT  2
     ":"                                        AT  6  
     today format "99/99/9999":U                AT  8 
     cl-empresa         format "x(41)"          AT 24
     cl-pagina          format "x(06)"          AT 65 
     ":"                                        AT 71 
     PAGE-NUMBER format ">>>>>9":U              AT 73   
     "|"                                        AT 84 skip
     "|"                                        AT  1
     cl-hr-movto        format "x(04)"          AT  2 
     ":"                                        AT  6 
     string(TIME,"HH:MM")                       AT  8
     "ESMV0501"                                 AT 18
     "-"                                        AT 27
     cl-titulo          format "x(40)"          AT 29 
     "|"                                        AT 84 skip
     "|"                                        AT  1 
     cl-documento       format "x(19)"          AT  2     
     ":"                                        AT 21 
     i-num-ord        format "->,>>>,>>>,>>9"   AT 22
     cl-abertura        format "x(08)"          AT 51
     ":"                                        AT 59 
     c-data-abert      format "99/99/9999":U    AT 61
     c-hora-abert      format "99:99":u         AT 74 
     "|"                                        AT 84 SKIP
     "|"                                        AT 1
     cl-entrada     format "x(07)"              AT 51
     ":"                                        AT 59 
     c-data-entrada format "99/99/9999":U       AT 61
     c-hora-entrada format "99:99":u            AT 74
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-tp-om     format "x(07)"                AT 10   
     cl-prev-term format "x(12)"                AT 47  
     ":"                                        AT 59  
     c-data-prev  format "99/99/9999":U         AT 61
     c-hora-prev  format "99:99":u              AT 74
     "|"                                        AT 84 SKIP
     "|"                                        AT  1 
     cl-oficina       format "x(7)"             AT  5
     ":"                                        AT 12
     ccod-ofici      format "x(03)"             at 14 no-label
     Cdes-ofici      format "x(30)"             at 19
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-eqpto       format "x(11)"              AT  5
     ":"                                        AT 16
     i-cod-emp      format ">>9"                AT 18
     "-"                                        AT 22
     c-cod-eqpto                                AT 24
     cl-termino     format "x(07)"              AT 51
     ":"                                        AT 59
     c-data-term    format "99/99/9999":U       AT 61
     c-hora-term    format "99:99":u            AT 74
     "|"                                        AT 84 SKIP
     "|"                                        AT 1
     cl-tp-manut format "x(15)"                 AT 5 
     ":"                                        AT 20
     c-tp-manut   format ">>9"                  AT 21
     "-"                                        AT 25
     c-descricao   format "x(22)"               AT 27
     cl-criacao     format "x(07)"              AT 51
     ":"                                        AT 59
     c-dat-criacao  format "99/99/9999":U       AT 61
     c-hr-cricao    format "99:99":u            AT 74
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-planej format "x(10)"                   AT  5
     ":"                                        AT 15
     c-plane   format "x(5)"                    AT 17
     "-"                                        AT 22
     c-nome         format "x(20)"              AT 24
     cl-usuar       format "x(07)"              AT 51
     ":"                                        AT 59
     c-cod-usuar    format "x(12)"              AT 61 
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-estabel1    format "x(7)"               AT 51
     ":"                                        AT 59
     c-cod-estabel  format "x(3)"               AT 61
    "|"                                        AT 84 SKIP
    "|"                                        AT  1
    "|"                                        AT 84 SKIP
    "|"                                        AT  1
    cl-Sintoma + ":"                           AT  5
    c-sintoma-desc                             AT 15
    "|"                                        AT 84 SKIP
    "|"                                        AT  1
    cl-Causa + ":"                             AT  7
    c-causa-desc                               AT 15
    "|"                                        AT 84 SKIP
    "|"                                        AT  1
    "|"                                        AT 84 SKIP

    with no-box width 132 NO-LABELS frame fFrameC stream-io.


{utp/ut-liter.i Usu rio * R }
assign cl-usuar = trim(return-value).
{utp/ut-liter.i Estabel * R }
ASSIGN cl-estabel1 = TRIM(RETURN-VALUE).
{utp/ut-liter.i Causa * R }
ASSIGN cl-Causa = TRIM(RETURN-VALUE).
{utp/ut-liter.i Sintoma * R }
ASSIGN cl-Sintoma = TRIM(RETURN-VALUE).
{utp/ut-liter.i Conta_Despesa * R }
assign  cl-conta2 = trim(return-value).   
{utp/ut-liter.i Cria‡Æo * R }
assign  cl-criacao = trim(return-value).   
{utp/ut-liter.i DATA * R }
assign  cl-data = trim(return-value).   
{utp/ut-liter.i PµGINA * R }
assign  cl-pagina = trim(return-value).
{utp/ut-liter.i EMISSÇO_DA_FICHA_DA_ORDEM_DE_MANUTEN€ÇO * R }
assign  cl-titulo = trim(return-value).
{utp/ut-liter.i Ordem_de_Manuten‡Æo * R }
assign  cl-documento = trim(return-value).
{utp/ut-liter.i Abertura * R }
assign  cl-abertura = trim(return-value).
{utp/ut-liter.i Entrada * R }
assign  cl-entrada = trim(return-value).

{utp/ut-liter.i Prev.T‚rmino * R }
assign  cl-prev-term = trim(return-value).
{utp/ut-liter.i T‚rmino * R }
assign  cl-termino = trim(return-value).
{utp/ut-liter.i Estado * R }
assign  cl-estado = trim(return-value).

{utp/ut-liter.i Equipamento * R }
assign cl-eqpto = trim(return-value).
{utp/ut-liter.i Oficina * R }
assign cl-oficina = trim(return-value).

{utp/ut-liter.i SubSistema * R }
assign cl-subsist = trim(return-value).
{utp/ut-liter.i Componente * R }
assign cl-compon  = trim(return-value).
{utp/ut-liter.i Tipo_Manuten‡Æo * R }
assign cl-tp-manut  = trim(return-value).
{utp/ut-liter.i Planejador * R }
assign cl-planej  = trim(return-value).
{utp/ut-liter.i HORA * R }
ASSIGN cl-hr-movto   = trim(return-value).
{utp/ut-liter.i DURABILIDADE * R }
ASSIGN cl-durabilidade   = trim(return-value).
{utp/ut-liter.i DURABILIDADE * R }
ASSIGN cl-durabilidade   = trim(return-value).

/*------------------------- segundo Painel ---------------------*/

FORM HEADER
     "+"                                        AT  1 
     FILL("-":U, 30) format "x(30)":U           AT  2
     cl-tarefas      format "x(13)"             AT 33
     FILL("-":U, 36) format "x(36)":U           AT 50
     "+"                                        AT 84 skip
     "|"                                        AT  1 
     c-executante  FORMAT "x(10)"               AT 47
     cl-descricao1   FORMAT "(20)"              AT 55
     FILL("-":U, 6) format  "x(06)":U           AT  2
     "|"                                        AT  8 
     FILL("-":U, 06) format "x(06)":U           AT  9
     FILL("-":U, 29) format "x(29)":U           AT 16
     "|"                                        AT 45
     FILL("-":U, 34) FORMAT "x(34)":U           AT 46
     "|"                                        AT 84 SKIP
     "|"                                        AT  1 
     "|"                                        AT 84 SKIP

     
     with no-box width 132 NO-LABELS frame fFrameD1 stream-io.

/*--------------   Mostra as tarefas no relat¢rio   -------------------*/

FORM HEADER
     "|"                                         AT  1
     "|"                                         AT  8
     i-num-eq format ">>9"                       AT  9
     c-descricao1    FORMAT "x(29)"              AT 16
     "|"                                         AT 45
      c-executante    FORMAT "x(10)"             AT 47
     "|"                                         AT 58
     c-data-tarefa   FORMAT "x(05)"              AT 61
     "|"                                         AT 67
     c-data-inicial  FORMAT "x(07)"              AT 68
     "|"                                         AT 75
     c-data-final    FORMAT "x(05)"              AT 76
     "|"                                         AT 84 SKIP
     "|      |"                                  AT 01
/*      "Sintoma:" c-sintoma-desc                         */
     "|"                                         AT 45
     "|"                                         AT 58
     "|"                                         AT 67
     "|"                                         AT 75
     "|"                                         AT 84 SKIP
     "|      |"                                  AT  1 
     "|"                                         AT 45
     "|"                                         AT 58
     "|"                                         AT 67
     "|"                                         AT 75
     "|"                                         AT 84 SKIP
/*      "|"                                         AT  1  */
/*      "|"                                         AT  8  */
/*      "Causa: "                                   AT 10       */
/*       c-causa-desc                                     AT 17 */
/*      "|            |        |       |        |"  AT 45 SKIP */

    with no-box width 132 NO-LABELS frame fFrameDsDurabilidade stream-io.

   
/************************ Mostra os dados padräes das tarefas *********/        
FORM HEADER

     "+"                                           AT  1
     FILL("-":U, 30) FORMAT "x(30)":U              AT  2
     c-tarefas       FORMAT "x(13)"                AT 33
     FILL("-":U, 37) FORMAT "x(37)":U              AT 47
     "+"                                           AT 84 skip
     "|"                                           AT  1
     cl-aceite       FORMAT "x(06)"                AT  2
     "|"                                           AT  8
     cl-numTarefa    FORMAT "x(06)"                AT  9
     cl-sistema      FORMAT "x(22)"                AT 16
     "|"                                           AT 45
     cl-descricao1   FORMAT "x(20)"                AT 55
     "|"                                           AT 84 SKIP
     
    with no-box width 132 NO-LABELS frame fFrameD1sDurabilidade stream-io.

{utp/ut-liter.i T_A_R_E_F_A_S * R }
assign  c-tarefas = trim(return-value).
{utp/ut-liter.i ACEITE * R }
assign  cl-aceite = trim(return-value).
{utp/ut-liter.i TAREFA * R }
assign  cl-numTarefa = trim(return-value).
{utp/ut-liter.i DESCRICAO_DA_ATIVIDADE * R }
assign  cl-sistema = trim(return-value).
{utp/ut-liter.i REPORTE_DE_HORAS * R }
ASSIGN  cl-descricao1 = TRIM(RETURN-VALUE).
{utp/ut-liter.i DATA * R }
ASSIGN  c-data-tarefa = TRIM(RETURN-VALUE).
{utp/ut-liter.i INICIAL * R }
ASSIGN  c-data-inicial = TRIM(RETURN-VALUE).
{utp/ut-liter.i FINAL * R }
ASSIGN  c-data-final = TRIM(RETURN-VALUE).
{utp/ut-liter.i EXECUTANTES * R }
ASSIGN  c-executante = TRIM(RETURN-VALUE).
{utp/ut-liter.i EXECUTANTES * R }
ASSIGN  c-executante = TRIM(RETURN-VALUE).

{utp/ut-liter.i PLANO * R }
ASSIGN  cl-planovenc = TRIM(RETURN-VALUE).
{utp/ut-liter.i DESCRICAO_TAREFA * R }
ASSIGN  cl-tarefa = TRIM(RETURN-VALUE).
{utp/ut-liter.i VENCTO * R }
ASSIGN  cl-vencto = TRIM(RETURN-VALUE).
{utp/ut-liter.i DIFERENCA * R }
ASSIGN  cl-diferenca = TRIM(RETURN-VALUE).

FORM HEADER
    "|"                                         AT  1
     c-cod-compto    format "x(08)"             AT  2
     c-des-compto    format "x(29)"             AT 11
     c-evento-1      format "x(08)"             AT 42
     c-des-evento-1  format "x(29)"             AT 51
     "|"                                        AT 84 SKIP
    
    with no-box width 132 NO-LABELS frame fFrameK stream-io.


{utp/ut-liter.i C_O_M_P_A_R_T_I_M_E_N_T_O_S * R }
assign  cl-comptos = trim(return-value).
{utp/ut-liter.i COMPARTIMENTO * R }
assign  c-compartimentos = trim(return-value).
{utp/ut-liter.i EVENTO * R }
assign  cl-eventos = trim(return-value).


FORM HEADER
     "|"                                        AT  1
     c-evento-2   format "x(08)"                AT  2
     c-des-evento-2  format "x(24)"             AT 11
     c-sub-sist-2      format "x(08)"           AT 36
     c-des-sub-sist-2   format "x(24)"          AT 45
     i-num-docto-2 format ">,>>>,>>9"           AT 71
     "|"                                        AT 84 SKIP

    with no-box width 132 NO-LABELS frame fFrameL stream-io.

FORM HEADER
     "+"                                        AT  1
     FILL("-":U, 32)    format "x(32)":U        AT  2
     cl-event1          format "x(13)"          AT 34
     FILL("-":U, 31)    format "x(31)":U        AT 48
     "+"                                        AT 84 SKIP
     "|"                                        AT  1
     cl-event2           format "x(06)"         AT  2
     cl-subsist2         format "x(10)"         AT 36
     cl-docto            format "x(09)"         AT 71
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     FILL("-":U, 08) format "x(08)":U           AT  2
     FILL("-":U, 23) format "x(23)":U           AT 11
     FILL("-":U, 08) format "x(08)":U           AT 36
     FILL("-":U, 25) format "x(25)":U           AT 45
     FILL("-":U, 09) format "x(09)":U           AT 71
     "|"                                        AT 84 SKIP
     
    with no-box width 132 NO-LABELS frame fFrameL1 stream-io.

{utp/ut-liter.i E_V_E_N_T_O_S * R }
assign  cl-event1 = trim(return-value).
{utp/ut-liter.i EVENTO * R }
assign  cl-event2 = trim(return-value).
{utp/ut-liter.i SUBSISTEMA * R }
assign  cl-subsist2 = trim(return-value).
{utp/ut-liter.i DOCUMENTO * R }
assign   cl-docto = trim(return-value).


FORM HEADER
     "|"                                        AT  1 
     c-descricao-2   format "x(48)"             AT  2
     c-num-docto-3   format "x(30)"             AT 50 
     "|"                                        AT 84 SKIP
    
   with no-box width 132 NO-LABELS frame fFrameM stream-io.

FORM HEADER
     "+"                                        AT  1 
     FILL("-":U, 31)    format "x(31)":U        AT  2 
     cl-anexos          format "x(11)"          AT 34    
     FILL("-":U, 33)    format "x(33)":U        AT 46
     "+"                                        AT 84 SKIP
     "|"                                        AT  1 
     cl-descricao       format "x(09)":U        AT  2
     cl-docto2          format "x(09)":U        AT 50
     "|"                                        AT 84 SKIP
     "|"                                        AT  1
     FILL("-":U, 47) format "x(47)":U           AT  2
     FILL("-":U, 29) format "x(29)":U           AT 50
     "|"                                        AT 84 SKIP
    
    with no-box width 132 NO-LABELS frame fFrameM1 stream-io.

{utp/ut-liter.i A_N_E_X_O_S * R }
assign  cl-anexos = trim(return-value).
{utp/ut-liter.i DESCRI€ÇO * R }
assign  cl-descricao  = trim(return-value).
{utp/ut-liter.i DOCUMENTO * R }
assign   cl-docto2 = trim(return-value).

FORM HEADER
     "|"                                        AT  1
     c-des-sub-sist-3  format "x(21)":U         AT  2        
     c-des-event-3     format "x(21)":U         AT 24
     c-des-tp-manut-3  format "x(21)":U         AT 47
     c-data-movto     format "99/99/9999":U     AT 70
     "|"                                        AT 84 SKIP
    
    with no-box width 132 NO-LABELS frame fFrameN stream-io.


{utp/ut-liter.i é_L_T_I_M_A_S__M_A_N_U_T_E_N_€_å_E_S * R }
assign  cl-ult-manut = trim(return-value).
{utp/ut-liter.i SUBSISTEMA * R }
assign  cl-subsist3 = trim(return-value).
{utp/ut-liter.i EVENTO * R }
assign  cl-event3 = trim(return-value).
{utp/ut-liter.i DATA * R }
assign  cl-data2 = trim(return-value).
{utp/ut-liter.i TIPO MANUTEN€ÇO * R }
assign  cl-tp-manut2 = trim(return-value).


FORM HEADER
     "|"                                        AT  1
     c-conteudo format "x(70)"                  AT  8
     "|"                                        AT 84 skip
     "|"                                        AT 1
     "|"                                        AT 84 skip
    with no-box width 132 NO-LABELS frame fFrameO stream-io.

FORM HEADER

     "+"                                        AT  1 
     FILL("-":U, 27)    format "x(27)":U        AT  2 
     cl-observacao      format "x(19)"          AT 30    
     FILL("-":U, 35)    format "x(34)":U        AT 49
     "+"                                        AT 84 SKIP
    with no-box width 132 NO-LABELS frame fFrameO1 stream-io.

{utp/ut-liter.i NARRATIVA_DA_ORDEM * R }
assign  cl-observacao = trim(return-value).

DEFINE VARIABLE c-liter-req-solicit AS CHARACTER format "x(47)":U NO-UNDO.
{utp/ut-liter.i R_E_Q_U_I_S_I_€_å_E_S_/_S_O_L_I_C_I_T_A_€_å_E_S *}
ASSIGN c-liter-req-solicit = RETURN-VALUE.

DEFINE VARIABLE c-liter-nr AS CHARACTER format "x(11)":U NO-UNDO.
{utp/ut-liter.i NR *}
ASSIGN c-liter-nr = RETURN-VALUE.

DEFINE VARIABLE c-liter-aceite AS CHARACTER format "x(5)":U NO-UNDO.
{utp/ut-liter.i ACEITE *}
ASSIGN c-liter-aceite = RETURN-VALUE.

DEFINE VARIABLE c-liter-item AS CHARACTER format "x(16)":U NO-UNDO.
{utp/ut-liter.i ITEM *}
ASSIGN c-liter-item = RETURN-VALUE.

DEFINE VARIABLE c-liter-data AS CHARACTER format "x(10)":U NO-UNDO.
{utp/ut-liter.i DATA *}
ASSIGN c-liter-data = RETURN-VALUE.

DEFINE VARIABLE c-liter-tipo AS CHARACTER format "x(8)":U NO-UNDO.
{utp/ut-liter.i TIPO *}
ASSIGN c-liter-tipo = RETURN-VALUE.

DEFINE VARIABLE c-liter-situacao AS CHARACTER format "x(11)":U NO-UNDO.
{utp/ut-liter.i SITUA€ÇO *}
ASSIGN c-liter-situacao = RETURN-VALUE.

DEFINE VARIABLE c-liter-estado AS CHARACTER format "x(11)":U NO-UNDO.
{utp/ut-liter.i ESTADO *}
ASSIGN c-liter-estado = RETURN-VALUE.

/**************************cabe‡alho REQUISICOES/SOLICITACOES******************/
FORM HEADER
     "+"                                     AT  1
     FILL("-":U, 12) format "x(12)":U        AT  2
     c-liter-req-solicit   format "x(47)":U  AT 15
     FILL("-":U, 17)  format "x(17)":U       AT 63
     "+"                                     AT 84
     "|"                                     AT 1
     c-liter-nr   format "X(11)":U           AT 2                          
     c-liter-aceite  format "x(5)":U         AT 14
     c-liter-item format "x(16)":U           AT 20
     c-liter-data format "x(10)":U           AT 37
     c-liter-tipo format "x(8)":U            AT 48
     c-liter-situacao format "x(11)":U       AT 57
     c-liter-estado format "x(11)":U         AT 69
/*      "|"                                     AT 84 SKIP */
/*      "|"                                     AT 1       */
     FILL("-", 11) format "X(11)":U          AT 2
     FILL("-", 5)  format "x(5)":U           AT 14
     FILL("-", 16)  format "x(16)":U         AT 20
     FILL("-", 10)  format "x(10)":U         AT 37
     FILL("-", 8)  format "x(8)":U           AT 48
     FILL("-", 11)  format "x(11)":U         AT 57
     FILL("-", 11)  format "x(11)":U         AT 69
     "|"                                     AT 84 SKIP
     
        with width 132 no-box NO-LABELS NO-ATTR-SPACE frame fCabecRecSolicit stream-io. 

FORM
     "|"                                        AT 1
     i-nr-requisicao format ">>>,>>>,>>9":U     AT 2
     i-sequencia     format ">>>>>":U           AT 14
     c-it-codigo     format "x(16)":U           AT 20
     d-dat-trans                                AT 37
     c-tp-requis     format "x(8)":U            AT 48
     c-situacao      format "x(11)":U           AT 57
     c-estado-req    format "x(11)":U           AT 69
     "|"                                        AT 84 SKIP
    
    with no-box width 132 NO-LABELS NO-ATTR-SPACE frame fRecSolicit stream-io.




/**************************Form linha final********************************************/
FORM HEADER

    "+"                                                   AT  1
    FILL("-":U, 30) format "x(30)":U                      AT  2
    c-observacao    FORMAT "x(11)":U                      AT 33
    FILL("-":U, 39) format "x(39)":U                      AT 45
                           
    "+"                                                   AT 84 SKIP
    "|"                                                   AT  1
    "|"                                                   AT 84
    "|"                                                   AT  1
    "|"                                                   AT 84
    "|"                                                   AT  1
    "|"                                                   AT 84 SKIP
    "|"                                                   AT  1
    "|"                                                   AT 84 SKIP

    
    "+"                                                   AT  1 
     FILL("-":U, 30) format "x(30)":U                     AT  2
     c-assinatura FORMAT "X(11)":U                        AT 33
     FILL("-":U, 39) format "x(39)":U                     AT 45
     "+"                                                  AT 84 skip
     "|"                                                  AT  1
     "|"                                                  AT 84 SKIP
     "|"                                                  AT  1
      "_____________________________________"             AT 20
     "|"                                                  AT 84 SKIP
     "|"                                                  AT  1
     c-supervisor FORMAT "x(10)":U                        AT 33
     "|"                                                  AT 84
     "|"                                                  AT  1
     "|"                                                  AT 84 SKIP
     "|"                                                  AT  1
     "|"                                                  AT 84 SKIP
     "|"                                                  AT  1
     C-data-assinatura FORMAT "x(4)":U                    AT 28
     ": ___ / ___ / ___"                                  AT 32
     "|"                                                  AT 84 SKIP
     "+"                                                  AT  1 
     FILL("-":U, 82) format "x(82)":U                     AT  2
     "+"                                                  AT 84 SKIP
     
     " "                                                  AT  1 
     " "                                                  AT 84 SKIP 
     
    with NO-BOX width 132 NO-LABELS frame fFrameP stream-io.


{utp/ut-liter.i OBSERVA€åES  * R }
assign  c-observacao = trim(return-value).

{utp/ut-liter.i RECEBIMENTO * R }
assign  c-assinatura = trim(return-value).
{utp/ut-liter.i SUPERVISOR * R }
assign  c-supervisor = trim(return-value).
{utp/ut-liter.i DATA * R }
assign  c-data-assinatura = trim(return-value).
{utp/ut-liter.i QUANTIDADE * R }
assign  cl-quantde = trim(return-value).

form header
    "|"                            at  1
    c-planovenc     format "x(12)" at  2
    c-tarefa        format "x(38)" at 15
    c-vencto        format "x(08)" at 57
    d-diferenca     format "->>>,>>9.99" to 79
    "|"                            at 84 skip
    with no-box width 132 no-labels frame fFrameQ stream-io.

form header
    "+"                            at  1
    fill("-":U, 16) format "x(16)" at  2
    /*cl-planovencav  format "x(45)" at 18*/
    "P L A N O S   V E N C I D O S / ·   V E N C E R" 
    fill("-":U, 17) format "x(17)" at 67
    "+"                            at 84 skip
    "|"                            at  1
    cl-planovenc    format "x(05)" at  2
    cl-tarefa       format "x(16)" at 15
    cl-vencto       format "x(06)" at 57
    cl-diferenca    format "x(09)" to 76
    "|"                            at 84 skip
    "|"                            at  1
    fill("-":U, 12) format "x(12)" at  2
    fill("-":U, 41) format "x(41)" at 15
    fill("-":U, 10) format "x(10)" at 57
    fill("-":U, 12) format "x(12)" to 79
    "|"                            at 84 skip
    with no-box width 132 no-labels frame fFrameQ1 stream-io.

{utp/ut-liter.i M_A_T_E_R_I_A_I_S * R }
ASSIGN  cl-matl = TRIM(RETURN-VALUE).
{utp/ut-liter.i ITEM * R }
ASSIGN  cl-item = TRIM(RETURN-VALUE).
{utp/ut-liter.i ATENDIDA * R }
ASSIGN  cl-quantde1 = TRIM(RETURN-VALUE).



    
FORM HEADER
    "+------------------------------- "
    cl-matl FORMAT "x(18)"
    "------------------------------+" SKIP
    "|"
    cl-item AT 2
    cl-numTarefa AT 55
    cl-quantde AT 63
    cl-quantde1 AT 74
    "|" AT 84 SKIP
    "|---------------------------------------------------- ------- ---------- ----------|" SKIP
    WITH NO-BOX WIDTH 132 NO-LABELS FRAME fFrameG1 STREAM-IO.
    
form 
    "|"
    c-item      AT 2
    i-num-tarefa    FORMAT ">>>>>>9" AT 55
    c-qtde-1    AT 63
    c-qtde-2    AT 74
    "|"         AT 84
    WITH NO-BOX WIDTH 132 NO-LABELS FRAME fFrameG STREAM-IO.
    
    
    

{utp/ut-liter.i E_P_I * R } 
ASSIGN cl-epi = trim(return-value).

{utp/ut-liter.i EQUIPAMENTO_DE_PROTECAO_INDIVIDUAL * R }
assign  cl-equipamento = trim(return-value).

FORM HEADER
    "+--------------------------------------"
    cl-epi FORMAT "x(5)"
    " -------------------------------------+" AT 46 SKIP
    "|"
    cl-equipamento FORMAT "x(34)" AT 2
    cl-quantde FORMAT "x(10)" AT 73 
    "|" AT 84
    
    SKIP
    "|-------- ------------------------------------------------------------- ---------- |" SKIP
    WITH NO-BOX WIDTH 132 NO-LABELS FRAME fFrameH1 STREAM-IO.
    
form 
    "|"
    c-epi       AT 2
    c-qtde-2    AT 73
    "|"         AT 84
    WITH NO-BOX WIDTH 132 NO-LABELS FRAME fFrameH STREAM-IO.
    

{utp/ut-liter.i F_E_R_R_A_M_E_N_T_A_S * R }
assign  cl-ferramenta_title = trim(return-value).

{utp/ut-liter.i FERRAMENTAS * R }
assign  cl-ferramenta = trim(return-value).    


    
FORM header
    "+------------------------------"
    cl-ferramenta_title FORMAT "x(21)"
    " -----------------------------+" AT 54 SKIP
    "|"
    cl-ferramenta FORMAT "x(11)" AT 2
    "|" AT 84
    
    SKIP
    "|-------- -----------------------------------------------------------              |" SKIP
    WITH NO-BOX WIDTH 132 NO-LABELS FRAME fFrameI1 STREAM-IO.
    
    
form 
    "|"
    c-cod-ferr  AT 2
    c-desc-ferr FORMAT "x(60)" AT 11 
    "|"         AT 84
    WITH NO-BOX WIDTH 132 NO-LABELS FRAME fFrameI STREAM-IO.
    
{utp/ut-liter.i F_I_C_H_A_S__M__T_O_D_O_S * R }
assign  cl-ficha-met = trim(return-value).
{utp/ut-liter.i FICHA_DE_MTODOS * R }
assign  cl-fichas = trim(return-value).
{utp/ut-liter.i PLANO * R }
assign  cl-plano = trim(return-value).


FORM 
     "|"    AT  1
     c-ficha     format ">>>>9"                 AT 2
     c-des-ficha   FORMAT "x(30)"               AT 11
     c-plano       format "x(08)"               AT 72
     "|"                                        AT 84 SKIP
    with no-box width 132 NO-LABELS frame fFrameJ stream-io.

FORM HEADER
     "|"                                        AT  1 
     FILL("-":U, 27)    format "x(27)":U        AT  2 
     cl-ficha-met       format "x(26)"          AT 30   
     FILL("-":U, 26)    format "x(26)":U        AT 58
     "|"                                        AT 84 SKIP
     "|"                                        AT  1 
     cl-fichas         format "x(15)"           AT  2
     cl-plano          format "x(08)"           AT 72
     "|"                                        AT  84 SKIP
     "|"                                        AT  1 
      FILL("-":U, 08) format "x(08)":U          AT  2
      FILL("-":U, 58) format "x(58)":U          AT 11
      FILL("-":U,  10) format "x(10)":U         AT 72
     "|"                                        AT 84 SKIP
    with no-box width 132 NO-LABELS frame fFrameJ1 stream-io.

FORM HEADER
     "|"                                        AT  1 
     c-conteudo format "x(70)"                  AT  9
     "|"                                        AT 84 skip
    with no-box width 132 NO-LABELS frame fFrameJ2 stream-io.
    
    



/*****************************************************************************/
                                                         /** Fim da Include **/
