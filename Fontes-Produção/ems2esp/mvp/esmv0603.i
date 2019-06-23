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
     field periodo-ini          as date format "99/99/9999"
     field periodo-fim          as date format "99/99/9999"
     field empresa-ini          like mab-eqpto.ep-codigo
     field empresa-fim          like mab-eqpto.ep-codigo
     field equipto-ini          like mab-eqpto.cod-eqpto
     field equipto-fim          like mab-eqpto.cod-eqpto
     field grupo-ini            like mab-eqpto.cod-grp-eqpto
     field grupo-fim            like mab-eqpto.cod-grp-eqpto
     field modelo-ini           like mab-eqpto.cod-model
     field modelo-fim           like mab-eqpto.cod-model
     field estab-ini            like mab-eqpto.cod-estabel
     field estab-fim            like mab-eqpto.cod-estabel
     field grp-evento-ini       like mab-event.cod-grp-event
     field grp-evento-fim       like mab-event.cod-grp-event
     field evento-ini           like mmv-tar-ord-manut.cod-evento
     field evento-fim           like mmv-tar-ord-manut.cod-evento
     field ccusto-ini           like mab-eqpto.cc-codigo
     field ccusto-fim           like mab-eqpto.cc-codigo
     field sistema-ini          like mab-sub-sist.cod-sistema
     field sistema-fim          like mab-sub-sist.cod-sistema
     field sub-sist-ini         like mmv-tar-ord-manut.cod-sub-sist 
     field sub-sist-fim         like mmv-tar-ord-manut.cod-sub-sist
     field tag-ini              like mab-eqpto.cd-tag
     field tag-fim              like mab-eqpto.cd-tag
     field sintoma-ini          as char format "x(08)"
     field sintoma-fim          as char format "x(08)"
     field causa-ini            as char format "x(08)"
     field causa-fim            as char format "x(08)"
     field lAtivos              as logical     
     field lProprios            as logical
     field lInativos            as logical
     field lTerceiros           as logical
    /*  field iConsReinc           AS integer */
    /*  field lCompart             as logical      */
    /*  field lFiltro              as logical      */
    /*  field lMecanica            as logical      */
    /*  field lOutros              as logical      */
     field lSomenteCorretivas   as logical
     field iAvalia              as integer
     FIELD iTipoDispo           AS INTEGER
     FIELD iPareto              AS INTEGER 
     .

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
