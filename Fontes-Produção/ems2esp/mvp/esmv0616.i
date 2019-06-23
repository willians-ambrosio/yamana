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
