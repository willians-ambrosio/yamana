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
     /** Faixas **/
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
     field centro-ini     like mab-eqpto.cc-codigo
     field centro-fim     like mab-eqpto.cc-codigo
     field planej-ini     like mmv-ord-manut.cod-plandor
     field planej-fim     like mmv-ord-manut.cod-plandor
     field setor-ini      like mmv-ord-manut.cod-setor-ofici
     field setor-fim      like mmv-ord-manut.cod-setor-ofici
     field oficina-ini    like mmv-ord-manut.cod-ofici  
     field oficina-fim    like mmv-ord-manut.cod-ofici
     field evento-ini     like mab-event.cod-evento
     field evento-fim     like mab-event.cod-evento
     field tag-ini        like mab-eqpto.cd-tag
     field tag-fim        like mab-eqpto.cd-tag
     field sub-sist-ini   like mab-sub-sist.cod-sub-sist
     field sub-sist-fim   like mab-sub-sist.cod-sub-sist
     /** Parƒmetros **/
     field dt-corte       as date   format "99/99/9999"
     field dt-perc-ini    as date   format "99/99/9999"
     field dt-perc-fim    as date   format "99/99/9999"
     field lAtivos        as logical
     field lInativos      as logical
     field lProprios      as logical
     field lTerceiros     as logical
     field lMotor         as logical
     field lNMotor        as logical
     field lVencido       as logical
     field lAVencer       as logical
     field lOutrosEv      as logical
     field lServico       as logical
     field lNTerminada    as logical
     field lComp          as logical
     field lPlanoComp     as logical
     field lOleo          as logical
     field lMecanica      as logical
     field lTpEventos     as logical
     field lCusto         as logical
     field lOutros        as logical
     field lPlanoFuturo   as logical
     field iPlano         as integer
     field cod-calend     like mmv-plano-prevent.cod-calend-padr
     field iMinutos       as integer format ">>9"
     field deAntecipa     as decimal format ">>9.99"
     FIELD idi-durabilidade AS INTEGER INIT 1.  /*1 = Vencidos 2 = Todos 3 = Garantia 4 = NÆo verifica*/ 
