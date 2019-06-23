/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i FR0220R1 1.02.05.051}  /*** 010551 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i fr0220r1 MFR}
&ENDIF

/*******************************************************************************
**
**        Programa: prghur/frp/FR0220-1.P
**
**        Data....: Setembro/1991
**
**        Autor...: ZENITE Desenvolvimento de Software Ltda.
**
**        Objetivo: Emissao do Recibo de Ferias.
**                  Incluido da Pagamento e retorno ao trabalho - FC  
*******************************************************************************/
{prghur/frp/fr0220tt.i shared}  /* Parametro */  /* Parametro */
/*{include/i-rpvar.i &shared="yes"}*/
{include/i-rpvar.i}
{include/i_dbvers.i}

&GLOBAL-DEFINE RTF YES

/*{include/i_dbvers.i}*/
/* chamada epc */             
{include/i-epc200.i fr0220r1} 

define NEW shared buffer bfrctrcal                 for habilit_ferias.
define            buffer bhabilit                  for habilit_ferias.
define NEW shared buffer bfunciona                 for funcionario.
define NEW shared variable v_log_folha_educnal     as log                      initial no           no-undo.
define new shared variable d-valor                 as decimal                                       no-undo.
define new shared variable i-nr-linhas             as integer                  initial 3            no-undo.
define new shared variable c-extenso               as character format "x(189)"                     no-undo.
define new shared variable i-nr-bytes              as integer   extent 20                           no-undo
initial [55,76,58].
define            variable i-vias-aux              as integer   format "z9"    initial 0            no-undo.
define            variable i-ind                   as integer                                       no-undo.
define            variable c-estab                 like rh_estab.nom_pessoa_jurid                   no-undo.
define            variable v_des_nome              as char      format "x(40)"                      no-undo.
define            variable v_num_cod               like rh_estab.cdn_estab                          no-undo.
define            variable c-funciona              like funcionario.nom_pessoa_fisic                no-undo.
define            variable d-dt-ini-fer            as date      format "99/99/9999" 
initial &IF "{&ems_dbtype}":U = "MSS":U &THEN "01/01/1800":U &ELSE "01/01/0001":U &ENDIF           no-undo.
define            variable d-dt-fim-fer            as date      format "99/99/9999" 
initial "12/31/9999"                                                                                no-undo.
define            variable i-inx                   as integer                                       no-undo.
define            variable c-titulo-n              as character format "x(40)"                      no-undo.
define            variable d-vlr-total             as decimal   format "****,***,**9.99"            no-undo.
define            variable d-tot-ident             as decimal   format ">>>,>>>,>>9.99"             no-undo.
define            variable c-extenso-1             as character format "x(55)"                      no-undo.
define            variable c-extenso-2             as character format "x(76)"                      no-undo.
define            variable c-extenso-3             as character format "x(58)"                      no-undo.
define            variable d-dt-ini-lic            as date      format "99/99/9999"                 no-undo.
define            variable d-dt-fim-lic            as date      format "99/99/9999"                 no-undo.
define            variable d-liq-pagar             as decimal   format ">>>,>>>,>>9.99"             no-undo.
define            variable c-categoria             as character format "x(07)"                      no-undo.
define            variable c-tab-ident             as character                initial "V,D,O"      no-undo.
define            variable c-lit-a1                as character format "x(01)" initial "a"          no-undo.
define            variable c-lit-a2                as character format "x(01)" initial "a"          no-undo.
define            variable c-lit-a3                as character format "x(01)" initial "a"          no-undo.
define            variable c-lit-a4                as character format "x(01)" initial "a"          no-undo.
define            variable c-lit-dd-1              as character format "x(13)" initial "Dias Gozados:" no-undo.
define            variable c-lit-dd-1p1            as character format "x(13)" initial "Dias Gozados:" no-undo.
define            variable c-lit-dd-1p2            as character format "x(13)" initial "Dias Gozados:" no-undo.
define            variable c-lit-dd-2              as character format "x(13)" initial "Dias Abono..:" no-undo.
define            variable c-lit-dd-4              as character format "x(13)" initial "Dias Abono..:" no-undo.
define            variable c-lit-dd-3              as character format "x(13)" initial "Dias Licen‡a:" no-undo.
define            variable c-local                 as character format "x(70)"                      no-undo.
define            variable c-zeros                 as char      format "x(03)"                      no-undo.
define            variable c-mes                   as character format "x(09)"                      no-undo.
define            variable c-lit-mes               as character format "x(09)"                      no-undo
initial  ["Janeiro,Fevereiro,Marco,Abril,Maio,Junho,
           Julho,Agosto,Setembro,Outubro,Novembro,Dezembro"].
define            variable i-matr-func             as char      format "99999999999"                no-undo.
define            variable c-hifen                 as char      format "x"     initial "-"          no-undo.
define            variable c-barra                 as char      format "x"     initial "/"          no-undo.
define            variable v_cdn_bco_liq           like funcionario.cdn_bco_liq                     no-undo.
define            variable v_cdn_agenc_bcia_liq    like funcionario.cdn_agenc_bcia_liq              no-undo.
define            variable v_cdn_cta_corren        like funcionario.cdn_cta_corren                  no-undo.
define            variable v_cod_digito_cta_corren like funcionario.cod_digito_cta_corren           no-undo.
define            variable v_cod_dig_agenc         like rh_agenc_bcia.cod_digito_verfdor_agenc_bcia no-undo.
define            variable duas-ferias             as logical                   initial no          no-undo.
define            variable d-dt-inic-per-1         like bfrctrcal.dat_inic_period_aqst_ferias       no-undo.
define            variable d-dt-term-per-1         like bfrctrcal.dat_term_period_aqst_ferias       no-undo.
define            variable d-dt-inic-per-2         like bfrctrcal.dat_inic_period_aqst_ferias       no-undo.
define            variable d-dt-term-per-2         like bfrctrcal.dat_term_period_aqst_ferias       no-undo.
define            variable d-dt-ini-fer-tot        as date format "99/99/9999" initial &IF "{&ems_dbtype}":U = "MSS":U &THEN "01/01/1800":U &ELSE "01/01/0001":U &ENDIF no-undo.
define            variable c-lit-a1-tot            as character format "x(01)" initial "a"          no-undo.
define            variable d-dt-fim-fer-tot        as date format "99/99/9999" initial "12/31/9999" no-undo.
define            variable c-lit-dd-tot            as char format "x(14)" initial "Total Gozados:"  no-undo.
define            variable total_dias_gozar        like bfrctrcal.qtd_dias_ferias_gozar             no-undo.
define            variable dias_gozar_per_1        like bfrctrcal.qtd_dias_ferias_gozar             no-undo.
define            variable dias_gozar_per_2        like bfrctrcal.qtd_dias_ferias_gozar             no-undo.
define            variable v_dat_ini_abono         as date format "99/99/9999"                      no-undo.
define            variable v_dat_term_abono        as date format "99/99/9999"                      no-undo.
define            variable v_qtd_dias_abono        as int                                           no-undo.
define            variable v_val_base_inss         as dec                                           no-undo.
DEFINE            VARIABLE v_label_id_feder_jurid  AS CHAR FORMAT "x(6)"                            NO-UNDO.

def var c-formulario           as char format "x(08)"         no-undo.
def var c-titulo-s             as char format "x(20)"         no-undo.
def var c-titulo-p             as char format "x(20)"         no-undo.
def var c-titulo-c             as char format "x(20)"         no-undo.
def var c-titulo-i             as char format "x(20)"         no-undo.
define var c-destino-impressao as char format "x(10)"         no-undo.
def var i-contador             as int  initial 0              no-undo.

def var v_log_trad              as log initial no no-undo.
Def Var qtd-dias-falta          As Int Form "ZZ9"  No-undo.
Def Var dt-retorno-tr           As Date Form "99/99/9999" No-undo.

define temp-table tt-recibo
    field ev-codigo   like event_fp.cdn_event_fp
    field descricao   like event_fp.des_event_fp
    field ident       like event_fp.idi_ident_efp
    field base          as decimal                      format "zzz,zzz,zz9.99"
    field valor         as decimal                      format "zzz,zzz,zz9.99"
    field inc-liquido like event_fp.idi_tip_inciden_liq format "9"
    field c-inc-liq     as char                         format "x(01)".

form
    "|"                                         at  01
    sit_afast.des_sit_afast_func format "x(24)" at  05
    sit_afast_func.dat_inic_sit_afast           at  31
    "a"                                         at  42
    sit_afast_func.dat_term_sit_afast           at  44
    "|"                                         at  80 skip
    with stream-io no-labels no-attr-space no-box width 80 frame f-sit.    

form
    "+---------------------------------------"  at 01
    "---------------------------------------+"  at 41
    "| Empresa:"                                at 01
    v_num_cod                                   at 12 format "999"
    "-"                                         at 15
    v_des_nome                                  at 16 
    v_label_id_feder_jurid                      at 56 SPACE(0)
    rh_estab.cod_id_feder  format "99.999.999/9999-99" space(0) 
    "|"                                         at 80 skip
    "|"                                         at 01
    "|"                                         at 80 skip
    "|---------------------------------------"  at 01
    "---------------------------------------|"  at 41
    "|"                                         at 01
    "|"                                         at 80
    "|"                                         at 01
    c-titulo-n                                  at 21
    "|"                                         at 80
    "|"                                         at 01
    "|"                                         at 80
    "|---------------------------------------"  at 01
    "---------------------------------------|"  at 41
    "|"                                         at 01
    "|"                                         at 80
    "| Funcion rio..........:"                  at 01  
    i-matr-func                                 at 26 space(0)
    bfunciona.nom_pessoa_fisic                  at 38
    "|"                                         at 80 skip
    "| Carteira Profissional:"                  at 01
    bfunciona.cod_cart_trab                     at 26
    "Serie:"                                    at 38
    bfunciona.cod_ser_cart_trab                 at 45
    "|"                                         at 80 skip
    "| Unidade Lota‡Æo......:"                  at 01
    bfunciona.cod_unid_lotac                    at 26 space(1)
    "-"                                         space(1)
    unid_lotac.des_unid_lotac                   space(0)
    "|"                                               skip
    "| Centro de Custo......:"                  at 01
    rh_ccusto.cod_rh_ccusto                     at 26
    "-"                                         at 35
    rh_ccusto.des_rh_ccusto                     at 38
    "|"                                         at 80 skip
    "| Categoria Salarial...:"                  at 01
    c-categoria                                 at 26
    "Sal rio:"                                  at 36
    movto_ferias_calcul.val_salario_atual       at 44 format "zzz,zzz,zz9.99"
    "Dep IR..:"                                 at 62
    movto_ferias_calcul.qti_depend_irf          at 73
    "|"                                         at 80 skip
    "| Cargo................:"                  at 01
    cargo.des_cargo                             at 26
    "|"                                         at 80 skip
    "| Banco................:"                  at 01
    v_cdn_bco_liq                               at 26 space(2)
    "Agˆncia:"                                        space(1)
    v_cdn_agenc_bcia_liq                              space(0)
    "-"                                               space(0)
    v_cod_dig_agenc                                   space(2)
    "Conta Corrente:"                                 space(1)   
    v_cdn_cta_corren                                  space(1)
    "/"                                               space(0)
    v_cod_digito_cta_corren             
    "|"                                         at 80 skip
    "|"                                         at 01
    "|"                                         at 80
    "|------------------------- Demonstrativo"  at 01
    " da ConcessÆo -------------------------|"  at 41
    "|"                                         at 01
    "|"                                         at 80 skip
    with stream-io no-labels no-attr-space no-box width 80 frame f-recibo-1.

form
    "+---------------------------------------"  at 01
    "---------------------------------------+"  at 41
    "| Empresa:"                                at 01
    v_num_cod                                   at 12 format "999"
    "-"                                         at 15
    v_des_nome                                  at 16 
    v_label_id_feder_jurid                      at 56 SPACE(0)
    rh_estab.cod_id_feder  format "99.999.999/9999-99" space(0)
    "|"                                         at 80 skip
    "|"                                         at 01
    "|"                                         at 80 skip
    "|---------------------------------------"  at 01
    "---------------------------------------|"  at 41
    "|"                                         at 01
    "|"                                         at 80
    "|"                                         at 01
    c-titulo-n                                  at 21
    "|"                                         at 80
    "|"                                         at 01
    "|"                                         at 80
    "|---------------------------------------"  at 01
    "---------------------------------------|"  at 41
    "|"                                         at 01
    "|"                                         at 80
    "| Funcion rio..........:"                  at 01  
    i-matr-func                                 at 26 space(0)
    bfunciona.nom_pessoa_fisic                  at 38
    "|"                                         at 80 skip
    "| Carteira Profissional:"                  at 01
    bfunciona.cod_cart_trab                     at 26
    "Serie:"                                    at 38
    bfunciona.cod_ser_cart_trab                 at 45
    "|"                                         at 80 skip
    "| Unidade Lota‡Æo......:"                  at 01
    bfunciona.cod_unid_lotac                    at 26 space(1)
    "-"                                         space(1)
    unid_lotac.des_unid_lotac                   space(0)
    "|"                                               skip
    "| Centro de Custo......:"                  at 01
    rh_ccusto.cod_rh_ccusto                     at 26
    "-"                                         at 35
    rh_ccusto.des_rh_ccusto                     at 38
    "|"                                         at 80 skip
    "| Categoria Salarial...:"                  at 01
    c-categoria                                 at 26
    "Sal rio:"                                  at 36
    movto_ferias_calcul.val_salario_atual       at 44 format "zzz,zzz,zz9.999"
    "Dep IR..:"                                 at 62
    movto_ferias_calcul.qti_depend_irf          at 73
    "|"                                         at 80 skip
    "| Cargo................:"                  at 01
    cargo.des_cargo                             at 26
    "|"                                         at 80 skip
    "| Banco................:"                  at 01
    v_cdn_bco_liq                               at 26 space(2)
    "Agˆncia:"                                        space(1)
    v_cdn_agenc_bcia_liq                              space(0)
    "-"                                               space(0)
    v_cod_dig_agenc                                   space(2)
    "Conta Corrente:"                                 space(1)   
    v_cdn_cta_corren                                  space(1)
    "/"                                               space(0)
    v_cod_digito_cta_corren             
    "|"                                         at 80 skip
    "|"                                         at 80 skip
    "|"                                         at 01
    "|"                                         at 80
    "|------------------------- Demonstrativo"  at 01
    " da ConcessÆo -------------------------|"  at 41
    "|"                                         at 01
    "|"                                         at 80 skip
    with stream-io no-labels no-attr-space no-box width 80 frame 8b.

form
    "+---------------------------------------"  at 01
    "---------------------------------------+"  at 41
    "| Empresa:"                                at 01
    v_num_cod                                   at 12 format "999"
    "-"                                         at 15
    v_des_nome                                  at 16 
    v_label_id_feder_jurid                      at 56 SPACE(0)
    rh_estab.cod_id_feder  format "99.999.999/9999-99" space(0)
    "|"                                         at 80 skip
    "|"                                         at 01
    "|"                                         at 80 skip
    "|---------------------------------------"  at 01
    "---------------------------------------|"  at 41
    "|"                                         at 01
    "|"                                         at 80
    "|"                                         at 01
    c-titulo-n                                  at 21
    "|"                                         at 80
    "|"                                         at 01
    "|"                                         at 80
    "|---------------------------------------"  at 01
    "---------------------------------------|"  at 41
    "|"                                         at 01
    "|"                                         at 80
    "| Funcion rio..........:"                  at 01  
    i-matr-func                                 at 26 space(0)
    bfunciona.nom_pessoa_fisic                  at 38
    "|"                                         at 80 skip
    "| Carteira Profissional:"                  at 01
    bfunciona.cod_cart_trab                     at 26
    "Serie:"                                    at 38
    bfunciona.cod_ser_cart_trab                 at 45
    "|"                                         at 80 skip
    "| Unidade Lota‡Æo......:"                  at 01
    bfunciona.cod_unid_lotac                    at 26 space(1)
    "-"                                         space(1)
    unid_lotac.des_unid_lotac                   space(0)
    "|"                                               skip
    "| Centro de Custo......:"                  at 01
    rh_ccusto.cod_rh_ccusto                     at 26
    "-"                                         at 35
    rh_ccusto.des_rh_ccusto                     at 38
    "|"                                         at 80 skip
    "| Categoria Salarial...:"                  at 01
    c-categoria                                 at 26
    "Sal rio:"                                  at 36
    movto_ferias_calcul.val_salario_atual       at 44 format "zz,zzz,zz9.9999"
    "Dep IR..:"                                 at 62
    movto_ferias_calcul.qti_depend_irf          at 73
    "|"                                         at 80 skip
    "| Cargo................:"                  at 01
    cargo.des_cargo                             at 26
    "|"                                         at 80 skip
    "| Banco................:"                  at 01
    v_cdn_bco_liq                               at 26 space(2)
    "Agˆncia:"                                        space(1)
    v_cdn_agenc_bcia_liq                              space(0)
    "-"                                               space(0)
    v_cod_dig_agenc                                   space(2)
    "Conta Corrente:"                                 space(1)   
    v_cdn_cta_corren                                  space(1)
    "/"                                               space(0)
    v_cod_digito_cta_corren             
    "|"                                         at 80 skip
    "|"                                         at 80 skip
    "|"                                         at 01
    "|"                                         at 80
    "|------------------------- Demonstrativo"  at 01
    " da ConcessÆo -------------------------|"  at 41
    "|"                                         at 01
    "|"                                         at 80 skip
    with stream-io no-labels no-attr-space no-box width 80 frame f-recibo-1c.


form
    "| Per¡odo Aquisitivo........:"             at 01
    d-dt-inic-per-1                             at 31
    "a"                                         at 42
    d-dt-term-per-1                             at 44
    c-lit-dd-1p1                                at 61
    dias_gozar_per_1                            at 75
    "|"                                         at 80 skip
    "| Per¡odo Aquisitivo........:"             at 01
    d-dt-inic-per-2                             at 31
    "a"                                         at 42
    d-dt-term-per-2                             at 44
    c-lit-dd-1p2                                at 61
    dias_gozar_per_2                            at 75
    "|"                                         at 80 skip
    "| Per¡odo de Gozo...........:"             at 01
    d-dt-ini-fer-tot                            at 31
    c-lit-a1-tot                                at 42
    d-dt-fim-fer-tot                            at 44
    c-lit-dd-tot                                at 61
    total_dias_gozar                            at 75
    "|"                                         at 80 skip
    with stream-io no-labels no-attr-space no-box width 80 frame f-recibo-2.

form
    "| Per¡odo Aquisitivo........:"             at 01
    bfrctrcal.dat_inic_period_aqst_ferias       at 31
    "a"                                         at 42
    bfrctrcal.dat_term_period_aqst_ferias       at 44
    "|"                                         at 80 skip
    "| Per¡odo de Gozo...........:"             at 01
    d-dt-ini-fer                                at 31
    c-lit-a1                                    at 42
    d-dt-fim-fer                                at 44
    c-lit-dd-1                                  at 61
    bfrctrcal.qtd_dias_ferias_gozar             at 75
    "|"                                         at 80 skip
    with stream-io no-labels no-attr-space no-box width 80 frame f-recibo-3.

form
    "| Per¡odo de Abono..........:"             at 01
    bfrctrcal.dat_inic_abono_ferias             at 31
    c-lit-a2                                    at 42
    bfrctrcal.dat_term_abono_ferias             at 44
    c-lit-dd-2                                  at 61
    bfrctrcal.qtd_dias_abono_ferias             at 75
    "|"                                         at 80 skip
    "| Total de Faltas Per¡odo Aquisitivo: "    At 01
    qtd-dias-falta                              At 39
    "|"                                         At 80 Skip
    "| Data de Retorno ao Trabalho: "           At 01
    dt-retorno-tr                               At 32
    "|"                                         At 80 Skip
    "| Per¡odo Licen‡a Remunerada:"             at 01
    d-dt-ini-lic                                at 31
    c-lit-a3                                    at 42
    d-dt-fim-lic                                at 44
    c-lit-dd-3                                  at 61
    bfrctrcal.qtd_dias_licenc                   at 75
    "|"                                         at 80 skip
    "|"                                         at 01
    "|"                                         at 80 skip
    "|-------------------------- Demonstrativ"  at 01
    "o do C lculo --------------------------|"  at 41
    "|    Evt   Descri‡Æo                    "  at 01
    "      Base C lculo            Valor    |"  at 41
    "|    ---   -----------------------------"  at 01
    "-   --------------   --------------    |"  at 41
    with stream-io no-labels no-attr-space no-box width 80 frame f-recibo-4.

form
    "| Per¡odo de Abono..........:"             at 01
    bfrctrcal.dat_inic_abono_ferias             at 31
    c-lit-a2                                    at 42
    bfrctrcal.dat_term_abono_ferias             at 44
    c-lit-dd-2                                  at 61
    bfrctrcal.qtd_dias_abono_ferias             at 75
    "|"                                         at 80 
    "| Per¡odo de Abono..........:"             at 01
    bhabilit.dat_inic_abono_ferias              at 31
    c-lit-a4                                    at 42
    bhabilit.dat_term_abono_ferias              at 44
    c-lit-dd-4                                  at 61
    bhabilit.qtd_dias_abono_ferias              at 75
    "|"                                         at 80 skip
    "| Total de Faltas Per¡odo Aquisitivo: "    At 01
    qtd-dias-falta                              At 39
    "|"                                         At 80 Skip
    "| Data de Retorno ao Trabalho: "           At 01
    dt-retorno-tr                               At 32
    "|"                                         at 80 skip
    "| Per¡odo Licen‡a Remunerada:"             at 01
    d-dt-ini-lic                                at 31
    c-lit-a3                                    at 42
    d-dt-fim-lic                                at 44
    c-lit-dd-3                                  at 61
    bfrctrcal.qtd_dias_licenc                   at 75
    "|"                                         at 80 skip
    "|"                                         at 01
    "|"                                         at 80 skip
    "|-------------------------- Demonstrativ"  at 01
    "o do C lculo --------------------------|"  at 41
    "|    Evt   Descri‡Æo                    "  at 01
    "      Base C lculo            Valor    |"  at 41
    "|    ---   -----------------------------"  at 01
    "-   --------------   --------------    |"  at 41
    with stream-io no-labels no-attr-space no-box width 80 frame f-recibo-8.

form
    "|"                                         at  01
    tt-recibo.ev-codigo                           at  06
    tt-recibo.descricao format "x(30)"            at  12
    tt-recibo.base                                at  45
    tt-recibo.valor                               at  62
    tt-recibo.c-inc-liq                           to  77 /*at  76*/
    "|"                                         at  80 skip
    with stream-io no-labels no-attr-space no-box width 80 frame f-recibo-5.

form
    "|"                                         at  01
    "L¡quido a Receber............:"            at  12
    d-liq-pagar                                 at  62
    "|"                                         at  80 skip
    with stream-io no-labels no-attr-space no-box width 80 frame f-recibo-6.

form
    "|---------------------------------------"  at  01
    "---------------------------------------|"  at  41 skip
    "|"                                         at  01
    "|"                                         at  80 skip
    "|          Recebi de"                      at  01
    c-estab                                     at  22
    "a importƒncia de |"                        at  63 skip
    "| R$"                                      at  01
    d-vlr-total                                 at  07
    "("                                         at  23
    c-extenso-1                                 at  24
    "|"                                         at  80 skip
    "|"                                         at  01
    c-extenso-2                                 at  03
    "|"                                         at  80 skip
    "|"                                         at  01
    c-extenso-3                                 at  03 space(0)        
    ") conforme demons- |"                             skip
    "| trativo, relativos ao per¡odo aquisiti"  at  01
    "vo de f‚rias acima especificado.       |"  at  41 skip
    "|          Pela clareza, firmo o present"  at  01
    "e dando plena e geral quita‡Æo.        |"  at  41 skip
    "|"                                         at  01
    "|"                                         at  80 skip
    "|---------------------------------------"  at  01
    "---------------------------------------|"  at  41 skip
    "|"                                         at  01
    c-local                                     at  03
    "|"                                         at  80 skip
    "|---------------------------------------"  at  01
    "---------------------------------------|"  at  41 skip
    "|"                                         at  01
    "|"                                         at  80 skip
    "|"                                         at  01
    "|"                                         at  80 skip
    "|"                                         at  01
    " ________________________________________" at  10 space(1)  
    "  __/__/___"                               AT  57  
    "|"                                         at  80 skip
    "|"                                         at  01
    c-funciona                                  at  11
    "Data"                                      at  61      
    "|"                                         at  80 skip
    "+---------------------------------------"  at  01
    "---------------------------------------+"  at  41 skip
    with stream-io no-labels no-attr-space no-box width 80 frame f-recibo-7.

IF  not v_log_trad THEN DO:
    FORM
         "---"
        with stream-io no-labels no-attr-space no-box width 80 frame f-teste-trad.
    run utp/ut-trfrrp.p(input frame f-teste-trad:handle).       
    ASSIGN v_log_trad = YES.
END.

def shared var v_han_acomp as handle no-undo.
find first tt-param.

{prghur/fpp/fp9200.i11}
{prghur/fpp/fp9200.i10 new shared}
{prghur/fpp/fp9200.i8}

run utp/ut-acomp.p persistent set v_han_acomp.
{utp/ut-liter.i Imprimindo}
run pi-inicializar in v_han_acomp (input return-value).

{utp/ut-liter.i EmissÆo_do_Recibo_de_F‚rias *}
assign c-titulo-relat = return-value.

{utp/ut-liter.i F‚rias_e_Rescisäes *}
assign c-sistema = return-value.

{utp/ut-liter.i Empresa *}

/* chamada epc VAB FO 1873.260*/
empty temp-table tt-epc no-error.

create tt-epc.
assign tt-epc.cod-event      = "imprimir_word"
       tt-epc.val-parameter  = "". 

{include/i-epc201.i "imprimir_word"}

if return-value = "word" then do:
    return "OK":U.
end.

find empresa no-lock where empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.
    assign c-empresa  = return-value + " " + empresa.razao-social.

find param_folha_educnal no-lock where
     param_folha_educnal.cdn_empresa = v_cdn_empres_usuar no-error.
   if avail param_folha_educnal then
      assign v_log_folha_educnal = yes.


if tt-param.classifica = 1
   or tt-param.classifica = 2 then do:

      for each bfunciona no-lock where
               bfunciona.cdn_empresa           = tt-param.v_cdn_empres_usuar and
               bfunciona.cod_rh_ccusto        >= tt-param.c-cc-ini           and
               bfunciona.cod_rh_ccusto        <= tt-param.c-cc-fim           and
               bfunciona.cdn_estab            >= tt-param.i-es-ini           and
               bfunciona.cdn_estab            <= tt-param.i-es-fim           and
               bfunciona.cdn_funcionario      >= tt-param.i-fc-ini           and
               bfunciona.cdn_funcionario      <= tt-param.i-fc-fim           and
               bfunciona.cdn_tip_contrat_func >= tt-param.i-contr-ini        and
               bfunciona.cdn_tip_contrat_func <= tt-param.i-contr-fim
          break by bfunciona.cdn_estab  
             /*   by bfunciona.cdn_func_centrdor*/
                by if tt-param.classifica = 1 
                   then string(bfunciona.cdn_funcionario,"99999999")
                   else bfunciona.nom_pessoa_fisic:
          find rh_estab of bfunciona no-lock.
          assign i-contador = i-contador + 1.
          run pi-acompanhar in v_han_acomp (input i-contador).
          if tt-param.i-tp-form = 1 then do:
             run pi-impressao.
          end.
          else do:
          for each bfrctrcal NO-LOCK of rh_estab where                                         
                      bfrctrcal.cdn_empresa              = bfunciona.cdn_empresa     and
                      bfrctrcal.cdn_estab                = bfunciona.cdn_estab       and
                      bfrctrcal.cdn_funcionario          = bfunciona.cdn_funcionario and
                      bfrctrcal.dat_inic_ferias >= tt-param.d-dt-ini                 and
                      bfrctrcal.dat_inic_ferias <= tt-param.d-dt-fim:
              /*****************************************************************************/
               /************************* Verifica s data de retorno ao trabalho ************/
              Assign dt-retorno-tr = bfrctrcal.dat_term_concess_ferias + 1 .
            Find funcionario Of movto_ferias_calcul No-lock No-error.
            Find LAST turno_trab Of funcionario No-lock No-error.
            Find FIRST det_calend_turma_localid NO-LOCK WHERE                                                
                          det_calend_turma_localid.cdn_turno_trab   = funcionario.cdn_turno_trab          and
                          det_calend_turma_localid.cdn_turma_trab   = funcionario.cdn_turma_trab          and
                          det_calend_turma_localid.cod_pais         = "BRA"                               and
                          det_calend_turma_localid.cdn_localidade   = funcionario.cdn_localid             and
                          det_calend_turma_localid.dat_refer_calend >= dt-retorno-tr    And 
                          det_calend_turma_localid.idi_sit_dia_trab = 1 No-error.

            If Avail det_calend_turma_localid Then
               Assign dt-retorno-tr = det_calend_turma_localid.dat_refer_calend.  
            Else
                DO: 
                   ASSIGN dt-retorno-tr = dt-retorno-tr + 1.
                   Find First det_calend_turma_localid no-lock where
                              det_calend_turma_localid.cdn_turno_trab    = funcionario.cdn_turno_trab   and
                              det_calend_turma_localid.cdn_turma_trab    = funcionario.cdn_turma_trab   and
                              det_calend_turma_localid.cod_pais          = "BRA"                        and
                              det_calend_turma_localid.cdn_localidade    = funcionario.cdn_localid      and
                              det_calend_turma_localid.dat_refer_calend >=  dt-retorno-tr   And
                              det_calend_turma_localid.idi_sit_dia_trab = 1  No-error.
                     IF AVAIL det_calend_turma_localid THEN
                        ASSIGN dt-retorno-tr = det_calend_turma_localid.dat_refer_calend.
                
                END.
                
               /***************************************************/
                if tt-param.i-tp-ferias = 1
                and bfrctrcal.idi_tip_ferias <> 1 then
                   next.
                if tt-param.i-tp-ferias = 2
                and bfrctrcal.idi_tip_ferias <> 2 then
                   next.
                find first movto_ferias_calcul of bfunciona no-lock where
                           movto_ferias_calcul.cdn_empresa                  = bfunciona.cdn_empresa     and
                           movto_ferias_calcul.cdn_estab                    = bfunciona.cdn_estab       and
                           movto_ferias_calcul.cdn_funcionario              = bfrctrcal.cdn_funcionario and
                           movto_ferias_calcul.dat_inic_ferias              = bfrctrcal.dat_inic_ferias and
                           movto_ferias_calcul.cdn_tip_calc_ferias          = tt-param.i-tp-calc        and
                           movto_ferias_calcul.log_calc_efetd_movto_ferias  = yes  no-error.
                if not available movto_ferias_calcul then
                   next.
                if movto_ferias_calcul.log_recibo_impresso = yes
                and tt-param.l-imp-reci = no then
                   next.
                run prghur/frp/fr0220r2.p.


             end.
          end.
      end.
   end.

   if tt-param.classifica = 3
   or tt-param.classifica = 4 then do:
      {prghur/fpp/fp9200.i6}
      for each tt_lotac_funcionario no-lock,
          each bfunciona of tt_lotac_funcionario where
               bfunciona.cdn_empresa           = tt-param.v_cdn_empres_usuar and
               bfunciona.cdn_tip_contrat_func >= tt-param.i-contr-ini        and
               bfunciona.cdn_tip_contrat_func <= tt-param.i-contr-fim 
          break by tt_lotac_funcionario.num_seq_unid_lotac
                by tt_lotac_funcionario.num_niv_unid_lotac
                by tt_lotac_funcionario.cod_unid_lotac
                by bfunciona.cdn_estab
                by if tt-param.classifica = 3 
                   then string(bfunciona.cdn_funcionario,"99999999")
                   else bfunciona.nom_pessoa_fisic:
                               
          find rh_estab of tt_lotac_funcionario no-lock.
          assign i-contador = i-contador + 1.
          run pi-acompanhar in v_han_acomp (input i-contador).
          if tt-param.i-tp-form = 1 then do:
             run pi-impressao.
          end.
          else do: 
          for each bfrctrcal NO-LOCK of rh_estab where                                         
                      bfrctrcal.cdn_empresa              = bfunciona.cdn_empresa     and
                      bfrctrcal.cdn_estab                = bfunciona.cdn_estab       and
                      bfrctrcal.cdn_funcionario          = bfunciona.cdn_funcionario and
                      bfrctrcal.dat_inic_ferias >= tt-param.d-dt-ini                  and
                      bfrctrcal.dat_inic_ferias <= tt-param.d-dt-fim:
                /*****************************************************************************/
               /************************* Verifica s data de retorno ao trabalho ************/
              Assign dt-retorno-tr = bfrctrcal.dat_term_concess_ferias + 1. 
              Find funcionario Of movto_ferias_calcul No-lock No-error.
              Find LAST turno_trab Of funcionario No-lock No-error.
              Find FIRST det_calend_turma_localid NO-LOCK WHERE
                              det_calend_turma_localid.cdn_turno_trab   = funcionario.cdn_turno_trab          and
                              det_calend_turma_localid.cdn_turma_trab   = funcionario.cdn_turma_trab          and
                              det_calend_turma_localid.cod_pais         = "BRA"                               and
                              det_calend_turma_localid.cdn_localidade   = funcionario.cdn_localid             and
                              det_calend_turma_localid.dat_refer_calend >= dt-retorno-tr And 
                              det_calend_turma_localid.idi_sit_dia_trab = 1 No-error.
                  If Avail det_calend_turma_localid Then 
                   Assign dt-retorno-tr = det_calend_turma_localid.dat_refer_calend. 
                Else
                DO: 
                   ASSIGN dt-retorno-tr = dt-retorno-tr + 1.
                   Find First det_calend_turma_localid no-lock where
                              det_calend_turma_localid.cdn_turno_trab    = funcionario.cdn_turno_trab   and
                              det_calend_turma_localid.cdn_turma_trab    = funcionario.cdn_turma_trab   and
                              det_calend_turma_localid.cod_pais          = "BRA"                        and
                              det_calend_turma_localid.cdn_localidade    = funcionario.cdn_localid      and
                              det_calend_turma_localid.dat_refer_calend >=  dt-retorno-tr /* bfrctrcal.dat_term_concess_ferias */  And
                              det_calend_turma_localid.idi_sit_dia_trab = 1  No-error.
                     IF AVAIL det_calend_turma_localid THEN
                        ASSIGN dt-retorno-tr = det_calend_turma_localid.dat_refer_calend.
                             
                END.
                 
               /***************************************************/                                                                                       
                if tt-param.i-tp-ferias = 1
                and bfrctrcal.idi_tip_ferias <> 1 then
                   next.
                if tt-param.i-tp-ferias = 2
                and bfrctrcal.idi_tip_ferias <> 2 then
                   next.     
                find first movto_ferias_calcul of bfunciona no-lock where
                           movto_ferias_calcul.cdn_empresa                 = bfunciona.cdn_empresa     and
                           movto_ferias_calcul.cdn_estab                   = bfunciona.cdn_estab       and
                           movto_ferias_calcul.cdn_funcionario             = bfrctrcal.cdn_funcionario and
                           movto_ferias_calcul.dat_inic_ferias             = bfrctrcal.dat_inic_ferias and
                           movto_ferias_calcul.cdn_tip_calc_ferias         = tt-param.i-tp-calc        and
                           movto_ferias_calcul.log_calc_efetd_movto_ferias = yes no-error.
                if not available movto_ferias_calcul then
                   next.
                if movto_ferias_calcul.log_recibo_impresso = yes
                and tt-param.l-imp-reci = no then
                   next.
               run prghur/frp/fr0220r2.p.


             end.
          end.
      end.
   end.

if tt-param.classifica =  9                                                                                    
or tt-param.classifica = 10 then do:                                                                           
      for each bfunciona use-index fncnr_py08507 NO-LOCK where
               bfunciona.cdn_empresa           = tt-param.v_cdn_empres_usuar and
               bfunciona.cod_rh_ccusto        >= tt-param.c-cc-ini           and
               bfunciona.cod_rh_ccusto        <= tt-param.c-cc-fim           and
               bfunciona.cdn_estab            >= tt-param.i-es-ini           and
               bfunciona.cdn_estab            <= tt-param.i-es-fim           and
               bfunciona.cdn_funcionario      >= tt-param.i-fc-ini           and
               bfunciona.cdn_funcionario      <= tt-param.i-fc-fim           and
               bfunciona.cdn_tip_contrat_func >= tt-param.i-contr-ini        and
               bfunciona.cdn_tip_contrat_func <= tt-param.i-contr-fim
          break by bfunciona.cod_rh_ccusto
                by bfunciona.cdn_estab
            /*    by bfunciona.cdn_func_centrdor*/
                by if tt-param.classifica = 9 
                   then string(bfunciona.cdn_funcionario,"99999999")
                   else bfunciona.nom_pessoa_fisic:
         assign i-contador = i-contador + 1.                 
         run pi-acompanhar in v_han_acomp (input i-contador).
         if tt-param.i-tp-form = 1 then do:
            run pi-impressao.
         end.
         else do:
         for each bfrctrcal NO-LOCK of bfunciona where                                         
                     bfrctrcal.cdn_funcionario          = bfunciona.cdn_funcionario and
                     bfrctrcal.dat_inic_ferias >= tt-param.d-dt-ini                 and
                     bfrctrcal.dat_inic_ferias <= tt-param.d-dt-fim:
             /*****************************************************************************/
           /************************* Verifica s data de retorno ao trabalho ************/
           Assign dt-retorno-tr = bfrctrcal.dat_term_concess_ferias + 1 .
            Find funcionario Of movto_ferias_calcul No-lock No-error.
            Find LAST turno_trab Of funcionario No-lock No-error.
            FIND FIRST det_calend_turma_localid NO-LOCK WHERE
                          det_calend_turma_localid.cdn_turno_trab   = funcionario.cdn_turno_trab          and
                          det_calend_turma_localid.cdn_turma_trab   = funcionario.cdn_turma_trab          and
                          det_calend_turma_localid.cod_pais         = "BRA"                               and
                          det_calend_turma_localid.cdn_localidade   = funcionario.cdn_localid             and
                          det_calend_turma_localid.dat_refer_calend >= dt-retorno-tr  And 
                          det_calend_turma_localid.idi_sit_dia_trab = 1 No-error.

            If Avail det_calend_turma_localid THEN 
               Assign dt-retorno-tr = det_calend_turma_localid.dat_refer_calend. 
            Else
            DO: 
               ASSIGN dt-retorno-tr = dt-retorno-tr + 1.
               Find First det_calend_turma_localid no-lock where
                          det_calend_turma_localid.cdn_turno_trab    = funcionario.cdn_turno_trab   and
                          det_calend_turma_localid.cdn_turma_trab    = funcionario.cdn_turma_trab   and
                          det_calend_turma_localid.cod_pais          = "BRA"                        and
                          det_calend_turma_localid.cdn_localidade    = funcionario.cdn_localid      and
                          det_calend_turma_localid.dat_refer_calend >=  dt-retorno-tr And
                          det_calend_turma_localid.idi_sit_dia_trab = 1  No-error.
                 IF AVAIL det_calend_turma_localid THEN
                    ASSIGN dt-retorno-tr = det_calend_turma_localid.dat_refer_calend.
            
            END.
                
           /***************************************************/                                                                                          
               if tt-param.i-tp-ferias = 1
               and bfrctrcal.idi_tip_ferias <> 1 then
                  next.

               if tt-param.i-tp-ferias = 2
               and bfrctrcal.idi_tip_ferias <> 2 then
                  next.     

               find first movto_ferias_calcul of bfunciona no-lock where
                          movto_ferias_calcul.cdn_funcionario             = bfrctrcal.cdn_funcionario and
                          movto_ferias_calcul.dat_inic_ferias             = bfrctrcal.dat_inic_ferias and
                          movto_ferias_calcul.cdn_tip_calc_ferias         = tt-param.i-tp-calc        and
                          movto_ferias_calcul.log_calc_efetd_movto_ferias = yes  no-error.
               if not available movto_ferias_calcul then
                  next.

               if movto_ferias_calcul.log_recibo_impresso = yes
               and tt-param.l-imp-reci = no then
                  next.

               run prghur/frp/fr0220r2.p.


            end.
         end.
      end.
   END.
  
/* run pi-finalizar in v_han_acomp. */
/* {include/i-rpclo.i}              */

PROCEDURE pi-impressao.

assign v_val_base_inss = 0.

for each bfrctrcal NO-LOCK where
         bfrctrcal.cdn_empresa      = bfunciona.cdn_empresa     and
         bfrctrcal.cdn_estab        = bfunciona.cdn_estab       and
         bfrctrcal.cdn_funcionario  = bfunciona.cdn_funcionario and
         bfrctrcal.dat_inic_ferias >= tt-param.d-dt-ini         and
         bfrctrcal.dat_inic_ferias          <= tt-param.d-dt-fim:
   if tt-param.i-tp-ferias = 1
   and bfrctrcal.idi_tip_ferias <> 1 then
      next.
   if tt-param.i-tp-ferias = 2
   and bfrctrcal.idi_tip_ferias <> 2 then
      next.     

   find first movto_ferias_calcul exclusive-lock where
              movto_ferias_calcul.cdn_empresa                 = bfunciona.cdn_empresa     and
              movto_ferias_calcul.cdn_estab                   = bfunciona.cdn_estab       and
              movto_ferias_calcul.cdn_funcionario             = bfrctrcal.cdn_funcionario and
              movto_ferias_calcul.dat_inic_ferias             = bfrctrcal.dat_inic_ferias and
              movto_ferias_calcul.cdn_tip_calc_ferias         = tt-param.i-tp-calc        and
              movto_ferias_calcul.log_calc_efetd_movto_ferias = yes  no-error.
   if duas-ferias then do:
          assign movto_ferias_calcul.log_recibo_impresso = yes.
          next.
   end.


   if not available movto_ferias_calcul then
      next.
   if movto_ferias_calcul.log_recibo_impresso = yes
   and tt-param.l-imp-reci = no then
      next.
   assign d-liq-pagar = 0
          qtd-dias-falta = 0.
   /****** Calcula dias de afastamento ****************/
   For Each sit_afast_func Where 
            sit_afast_func.cdn_empresa = movto_ferias_calcul.cdn_empresa And
            sit_afast_func.cdn_estab   = movto_ferias_calcul.cdn_estab   And
            sit_afast_func.cdn_funcionario   = movto_ferias_calcul.cdn_funcionario   And 
            sit_afast_func.qti_dias_sit_func > 0 And
            sit_afast_func.dat_inic_sit_afast >= bfrctrcal.dat_inic_period_aqst_ferias And
            sit_afast_func.dat_term_sit_afast <= bfrctrcal.dat_term_period_aqst_ferias:
       Find sit_afast Of sit_afast_func Where sit_afast.log_influi_ferias No-lock No-error.
       If Not Avail sit_afast Then Next.
       If sit_afast_func.dat_term_sit_afast > bfrctrcal.dat_term_period_aqst_ferias Then
          Assign qtd-dias-falta = qtd-dias-falta + (bfrctrcal.dat_term_period_aqst_ferias - sit_afast_func.dat_inic_sit_afast).
       Else
          Assign qtd-dias-falta = qtd-dias-falta + sit_afast_func.qti_dias_sit_func.
   End.
   /*****************************************************************************/
   /************************* Verifica s data de retorno ao trabalho ************/
   Assign dt-retorno-tr = bfrctrcal.dat_term_concess_ferias + 1.
    Find funcionario Of movto_ferias_calcul No-lock No-error.
    Find LAST turno_trab Of funcionario No-lock No-error.
    FIND FIRST det_calend_turma_localid NO-LOCK WHERE
                  det_calend_turma_localid.cdn_turno_trab   = funcionario.cdn_turno_trab          and
                  det_calend_turma_localid.cdn_turma_trab   = funcionario.cdn_turma_trab          and
                  det_calend_turma_localid.cod_pais         = "BRA"                               and
                  det_calend_turma_localid.cdn_localidade   = funcionario.cdn_localid             and
                  det_calend_turma_localid.dat_refer_calend >= dt-retorno-tr   And 
                  det_calend_turma_localid.idi_sit_dia_trab = 1 No-error.

    If Avail det_calend_turma_localid Then 
       Assign dt-retorno-tr = det_calend_turma_localid.dat_refer_calend. 
    Else
    DO: 
        ASSIGN dt-retorno-tr = dt-retorno-tr + 1.
       Find First det_calend_turma_localid no-lock where
                  det_calend_turma_localid.cdn_turno_trab    = funcionario.cdn_turno_trab   and
                  det_calend_turma_localid.cdn_turma_trab    = funcionario.cdn_turma_trab   and
                  det_calend_turma_localid.cod_pais          = "BRA"                        and
                  det_calend_turma_localid.cdn_localidade    = funcionario.cdn_localid      and
                  det_calend_turma_localid.dat_refer_calend >=  dt-retorno-tr  And
                  det_calend_turma_localid.idi_sit_dia_trab = 1  No-error.
         IF AVAIL det_calend_turma_localid THEN
            ASSIGN dt-retorno-tr = det_calend_turma_localid.dat_refer_calend.
    
    END.
     
   /***************************************************/
   if tt-param.log_agrupa then do:
      find first bhabilit NO-LOCK where
                 bhabilit.cdn_empresa                   = bfunciona.cdn_empresa                       and
                 bhabilit.cdn_estab                     = bfunciona.cdn_estab                         and
                 bhabilit.cdn_funcionario               = bfunciona.cdn_funcionario                   and
               ((bhabilit.dat_inic_ferias      >= tt-param.d-dt-ini                                   and
                 bhabilit.dat_inic_ferias      <= tt-param.d-dt-fim                                   and 
                 not tt-param.log_fr0080) or
                 tt-param.log_fr0080) and
                /* bhabilit.dat_inic_period_aqst_ferias   = (bfrctrcal.dat_term_period_aqst_ferias + 1) and */
                 bhabilit.dat_pagto_ferias              =  bfrctrcal.dat_pagto_ferias and
                 rowid(bhabilit)                        <> rowid(bfrctrcal) no-error. 
      if avail bhabilit then do:
         assign duas-ferias = yes.
         if tt-param.i-tp-ferias = 1
         and bhabilit.idi_tip_ferias <> 1 then
            assign duas-ferias = no.
         if tt-param.i-tp-ferias = 2
         and bhabilit .idi_tip_ferias <> 2 then
            assign duas-ferias = no .     
         find first movto_ferias_calcul no-lock where
                    movto_ferias_calcul.cdn_empresa                 = bfunciona.cdn_empresa    and
                    movto_ferias_calcul.cdn_estab                   = bfunciona.cdn_estab      and
                    movto_ferias_calcul.cdn_funcionario             = bhabilit.cdn_funcionario and
                    movto_ferias_calcul.dat_inic_ferias             = bhabilit.dat_inic_ferias and
                    movto_ferias_calcul.cdn_tip_calc_ferias         = tt-param.i-tp-calc       and
                    movto_ferias_calcul.log_calc_efetd_movto_ferias = yes  no-error.
         if not available movto_ferias_calcul then
            assign duas-ferias = no.
         if movto_ferias_calcul.log_recibo_impresso = yes
         and l-imp-reci = no then
            assign duas-ferias = no.
         if duas-ferias then do:
            for each reg_det_ferias no-lock where
                     reg_det_ferias.cdn_empresa         = movto_ferias_calcul.cdn_empresa     and   
                     reg_det_ferias.cdn_estab           = movto_ferias_calcul.cdn_estab       and   
                     reg_det_ferias.cdn_funcionario     = movto_ferias_calcul.cdn_funcionario and   
                     reg_det_ferias.dat_inic_ferias     = movto_ferias_calcul.dat_inic_ferias and   
                     reg_det_ferias.cdn_tip_calc_ferias = movto_ferias_calcul.cdn_tip_calc_ferias:
                do i-inx = 1 to reg_det_ferias.qti_efp:
                   if reg_det_ferias.cdn_idx_efp_espcif_ferias[i-inx] = 8
                   or reg_det_ferias.cdn_idx_efp_espcif_ferias[i-inx] = 17 then
                      next.
                   if reg_det_ferias.cdn_idx_efp_espcif_ferias[i-inx] = 1 then do:
                      assign d-liq-pagar = if reg_det_ferias.log_livre_1 
                                           then reg_det_ferias.val_efp_mes_inic_ferias[i-inx] +
                                                reg_det_ferias.val_efp_seguinte_ferias[i-inx] + 
                                                reg_det_ferias.val_tot_efp_ferias[i-inx]
                                           else reg_det_ferias.val_tot_efp_ferias[i-inx].
                     next.
                   end.
                   if reg_det_ferias.cdn_event_fp[i-inx] = "000" then
                      next.
                   find event_fp no-lock where
                        event_fp.cdn_event_fp = reg_det_ferias.cdn_event_fp[i-inx] no-error.
                   if event_fp.log_impr_envel_fp = no then
                      next.

                   if event_fp.idi_tip_inciden_inss = 1 then do:
                      assign v_val_base_inss = if reg_det_ferias.log_livre_1
                                                then  v_val_base_inss +
                                                    (reg_det_ferias.val_efp_mes_inic_ferias[i-inx] +
                                                     reg_det_ferias.val_efp_seguinte_ferias[i-inx] +
                                                     reg_det_ferias.val_tot_efp_ferias[i-inx])
                                                else v_val_base_inss + 
                                                     reg_det_ferias.val_tot_efp_ferias[i-inx].
                   end.
                   else do:
                      if event_fp.idi_tip_inciden_inss = 2 then do:
                         assign v_val_base_inss = if reg_det_ferias.log_livre_1
                                                   then v_val_base_inss - 
                                                      (reg_det_ferias.val_efp_mes_inic_ferias[i-inx] +
                                                        reg_det_ferias.val_efp_seguinte_ferias[i-inx] +
                                                        reg_det_ferias.val_tot_efp_ferias[i-inx])
                                                   else v_val_base_inss - 
                                                       reg_det_ferias.val_tot_efp_ferias[i-inx].
                      end.
                   end.   
                   create tt-recibo.
                   assign tt-recibo.ev-codigo   = reg_det_ferias.cdn_event_fp[i-inx]
                          tt-recibo.descricao   = event_fp.des_event_fp  
                                                tt-recibo.ident       = event_fp.idi_ident_efp
                          tt-recibo.base        = if event_fp.cdn_idx_efp_funcao_espcif = 18
                                                then v_val_base_inss
                                                else 
                                                   if reg_det_ferias.log_livre_1
                                                   then reg_det_ferias.val_base_mes_inic_ferias[i-inx] +
                                                        reg_det_ferias.val_base_seguinte_ferias[i-inx] +
                                                        reg_det_ferias.val_base_calc_ferias[i-inx]
                                                   else reg_det_ferias.val_base_calc_ferias[i-inx]
                          tt-recibo.valor       = if reg_det_ferias.log_livre_1
                                                then reg_det_ferias.val_efp_mes_inic_ferias[i-inx] +
                                                     reg_det_ferias.val_efp_seguinte_ferias[i-inx] +
                                                     reg_det_ferias.val_tot_efp_ferias[i-inx]
                                                else reg_det_ferias.val_tot_efp_ferias[i-inx]
                          tt-recibo.inc-liquido = event_fp.idi_tip_inciden_liq
                          tt-recibo.c-inc-liq   = if tt-recibo.inc-liquido = 1
                                                then "+"
                                                else if tt-recibo.inc-liquido = 2
                                                     then "-"
                                                     else " ".
                end. 
            end.
         end.
      end.
      else 
         assign duas-ferias = no.
   end.   
   else
      assign duas-ferias = no.

   do i-vias-aux = 1 to i-vias:

      find rh_estab no-lock where
           rh_estab.cdn_empresa = bfunciona.cdn_empresa and
           rh_estab.cdn_estab   = bfunciona.cdn_estab.

      find rh_pessoa_jurid no-lock where
           rh_pessoa_jurid.num_pessoa_jurid = rh_estab.num_pessoa_jurid.

      if bfunciona.cdn_bco_liq         <> 0
      and bfunciona.cdn_agenc_bcia_liq <> 0 then do:

         assign v_cdn_bco_liq           = bfunciona.cdn_bco_liq
                v_cdn_agenc_bcia_liq    = bfunciona.cdn_agenc_bcia_liq
                v_cdn_cta_corren        = bfunciona.cdn_cta_corren
                v_cod_digito_cta_corren = bfunciona.cod_digito_cta_corren.

         find rh_agenc_bcia no-lock where
              rh_agenc_bcia.cdn_banco      = bfunciona.cdn_bco_liq and
              rh_agenc_bcia.cdn_agenc_bcia = bfunciona.cdn_agenc_bcia_liq no-error.
         if avail rh_agenc_bcia
         then assign v_cod_dig_agenc = rh_agenc_bcia.cod_digito_verfdor_agenc_bcia.
         else assign v_cod_dig_agenc = "0".
      end.
      else assign v_cod_digito_cta_corren = "0"
                  v_cod_dig_agenc         = "0".

      find first movto_ferias_calcul exclusive-lock where
                 movto_ferias_calcul.cdn_empresa                 = rh_estab.cdn_empresa      and
                 movto_ferias_calcul.cdn_estab                   = rh_estab.cdn_estab        and
                 movto_ferias_calcul.cdn_funcionario             = bfrctrcal.cdn_funcionario and
                 movto_ferias_calcul.dat_inic_ferias             = bfrctrcal.dat_inic_ferias and
                 movto_ferias_calcul.cdn_tip_calc_ferias         = tt-param.i-tp-calc        and
                 movto_ferias_calcul.log_calc_efetd_movto_ferias = yes no-error.
      assign i-inx        = (40 - length(bfunciona.nom_pessoa_fisic)) / 2
             c-funciona   = fill(" ",i-inx) +
                            substr(bfunciona.nom_pessoa_fisic,1,length(bfunciona.nom_pessoa_fisic))
             c-mes        = entry(month(bfrctrcal.dat_pagto_ferias),c-lit-mes)
             c-local      = TRIM(substr(rh_pessoa_jurid.nom_cidad_rh,1,length(rh_pessoa_jurid.nom_cidad_rh))   +
                            ", "   + string(day(bfrctrcal.dat_pagto_ferias),"99") +
                            " de " + c-mes +
                            " de " + string(year(bfrctrcal.dat_pagto_ferias),"9999") +
                            ".")
             c-titulo-n     = if bfrctrcal.idi_tip_ferias = 1
                            then if movto_ferias_calcul.cdn_tip_calc_ferias = 0
                                 then "            RECIBO DE FRIAS           "
                                 else "     RECIBO COMPLEMENTAR DE FRIAS     "
                            else if movto_ferias_calcul.cdn_tip_calc_ferias = 0
                                 then "      RECIBO DE FRIAS COLETIVAS       "
                                 else "RECIBO COMPLEMENTAR DE FRIAS COLETIVAS"
             d-dt-ini-fer = bfrctrcal.dat_inic_concess_ferias
             d-dt-fim-fer = bfrctrcal.dat_term_concess_ferias + i-dias-nao-cons
             d-dt-ini-lic = ?
             d-dt-fim-lic = ?
             i-matr-func = if v_log_folha_educnal then
                              string(bfunciona.cdn_func_centrdor) + c-barra + 
                              string(bfunciona.cdn_tip_contrat_func) + c-hifen + 
                              string(bfunciona.num_digito_verfdor_func)
                           else string(bfunciona.cdn_funcionario) + c-hifen + 
                                string(bfunciona.num_digito_verfdor_func).
      if bfrctrcal.qtd_dias_licenc > 0 then do:
         assign d-dt-fim-lic = d-dt-fim-fer
                d-dt-fim-fer = bfrctrcal.dat_inic_concess_ferias +
                               bfrctrcal.qtd_dias_ferias_gozar - 1
                d-dt-ini-lic = if trunc(bfrctrcal.qtd_dias_ferias_gozar,0) = 
                                        bfrctrcal.qtd_dias_ferias_gozar
                               then d-dt-fim-fer + 1
                               else d-dt-fim-fer.
      end.          
      if bfrctrcal.qtd_dias_ferias_gozar = 0 then
         assign d-dt-ini-fer = ?
                d-dt-fim-fer = ?.
      assign i-inx       = bfunciona.cdn_categ_sal 
             c-categoria = {database/inpy/i03py029.i 04 bfunciona.cdn_categ_sal}.
      find rh_ccusto of bfunciona no-lock.
      find cargo of bfunciona no-lock.
      find unid_lotac no-lock where
           unid_lotac.cod_unid_lotac = bfunciona.cod_unid_lotac no-error.

      if tt-param.rs_nome = 1 then do:
         find empresa no-lock where
              empresa.ep-codigo = rh_estab.cdn_empresa no-error.
         assign v_des_nome = empresa.razao-social
                v_num_cod  = rh_estab.cdn_empresa
                c-estab    = empresa.razao-social.
        if length(c-estab) < 40 then
           substr(c-estab,length(c-estab) + 1,40 - length(c-estab)) = fill (".",40 - length(c-estab)).
      end.
      else do:
         assign v_des_nome = rh_estab.nom_pessoa_jurid
                v_num_cod  = rh_estab.cdn_estab
                c-estab    = rh_estab.nom_pessoa_jurid.
        if length(c-estab) < 40 then
           substr(c-estab,length(c-estab) + 1,40 - length(c-estab)) = fill (".",40 - length(c-estab)).
      end. 

      FIND rh_pais NO-LOCK WHERE
          rh_pais.cod_pais = rh_estab.cod_pais NO-ERROR.
      IF AVAIL rh_pais THEN
          ASSIGN v_label_id_feder_jurid = REPLACE(rh_pais.nom_label_id_feder_jurid,".","") + ": ".

      if tt-param.num_dec_sal = 2 then do:   
         display v_num_cod
                 v_des_nome
                 v_label_id_feder_jurid
                 rh_estab.cod_id_feder
                 c-titulo-n
                 i-matr-func
                 bfunciona.nom_pessoa_fisic
                 bfunciona.cod_cart_trab
                 bfunciona.cod_ser_cart_trab
                 bfunciona.cod_unid_lotac
                 unid_lotac.des_unid_lotac
                 v_cdn_bco_liq 
                 v_cdn_agenc_bcia_liq
                 v_cod_dig_agenc
                 v_cdn_cta_corren
                 v_cod_digito_cta_corren
                 rh_ccusto.cod_rh_ccusto
                 rh_ccusto.des_rh_ccusto
                 c-categoria
                 movto_ferias_calcul.val_salario_atual
                 movto_ferias_calcul.qti_depend_irf
                 cargo.des_cargo
            with frame f-recibo-1.
         down with frame f-recibo-1.
      end.
      else do:
         if tt-param.num_dec_sal = 3 then do:
            display v_num_cod
                    v_des_nome
                    v_label_id_feder_jurid
                    rh_estab.cod_id_feder
                    c-titulo-n
                    i-matr-func
                    bfunciona.nom_pessoa_fisic
                    bfunciona.cod_cart_trab
                    bfunciona.cod_ser_cart_trab
                    bfunciona.cod_unid_lotac
                    unid_lotac.des_unid_lotac
                    v_cdn_bco_liq 
                    v_cdn_agenc_bcia_liq
                    v_cod_dig_agenc
                    v_cdn_cta_corren
                    v_cod_digito_cta_corren
                    rh_ccusto.cod_rh_ccusto
                    rh_ccusto.des_rh_ccusto
                    c-categoria
                    movto_ferias_calcul.val_salario_atual
                    movto_ferias_calcul.qti_depend_irf
                    cargo.des_cargo
                    with frame f-recibo-1b.
            down with frame f-recibo-1b.
         end.
         else do:
            display v_num_cod
                    v_des_nome
                    v_label_id_feder_jurid
                    rh_estab.cod_id_feder
                    c-titulo-n
                    i-matr-func
                    bfunciona.nom_pessoa_fisic
                    bfunciona.cod_cart_trab
                    bfunciona.cod_ser_cart_trab
                    bfunciona.cod_unid_lotac
                    unid_lotac.des_unid_lotac
                    v_cdn_bco_liq 
                    v_cdn_agenc_bcia_liq
                    v_cod_dig_agenc
                    v_cdn_cta_corren
                    v_cod_digito_cta_corren
                    rh_ccusto.cod_rh_ccusto
                    rh_ccusto.des_rh_ccusto
                    c-categoria
                    movto_ferias_calcul.val_salario_atual
                    movto_ferias_calcul.qti_depend_irf
                    cargo.des_cargo
                    with frame f-recibo-1c.
            down with frame f-recibo-1c.
         end.
      end.
      if duas-ferias then do:
         assign d-dt-inic-per-1  = bfrctrcal.dat_inic_period_aqst_ferias
                d-dt-term-per-1  = bfrctrcal.dat_term_period_aqst_ferias
                dias_gozar_per_1 = bfrctrcal.qtd_dias_ferias_gozar
                d-dt-inic-per-2  = bhabilit.dat_inic_period_aqst_ferias
                d-dt-term-per-2  = bhabilit.dat_term_period_aqst_ferias
                dias_gozar_per_2 = bhabilit.qtd_dias_ferias_gozar
                d-dt-ini-fer-tot = bfrctrcal.dat_inic_concess_ferias
                d-dt-fim-fer-tot = bhabilit.dat_term_concess_ferias + i-dias-nao-cons
                total_dias_gozar = dias_gozar_per_1 + dias_gozar_per_2.
         display d-dt-inic-per-1
                 d-dt-term-per-1
                 c-lit-dd-1p1
                 dias_gozar_per_1
                 d-dt-inic-per-2
                 d-dt-term-per-2
                 c-lit-dd-1p2
                 dias_gozar_per_2
                 d-dt-ini-fer-tot when total_dias_gozar > 0
                 c-lit-a1-tot     when total_dias_gozar > 0
                 d-dt-fim-fer-tot when total_dias_gozar > 0
                 c-lit-dd-tot     when total_dias_gozar > 0
                 total_dias_gozar when total_dias_gozar > 0
            with frame f-recibo-2.
         down with frame f-recibo-2.
         &if "{&dthrpyc_version}" >= "2.06" &then 
            if tt-param.v_log_situacao = yes then do:
                  for each sit_afast_func no-lock where 
                           sit_afast_func.cdn_empresa        = bfrctrcal.cdn_empresa                      and
                           sit_afast_func.cdn_estab          = bfrctrcal.cdn_estab                        and
                           sit_afast_func.cdn_funcionario    = bfrctrcal.cdn_funcionario                  and
                           ((sit_afast_func.dat_inic_sit_afast >= bfrctrcal.dat_inic_period_aqst_ferias   and
                            sit_afast_func.dat_inic_sit_afast  <= bhabilit.dat_term_period_aqst_ferias)  or
                           (sit_afast_func.dat_term_sit_afast  <= bhabilit.dat_term_period_aqst_ferias   and
                            sit_afast_func.dat_term_sit_afast  >= bfrctrcal.dat_inic_period_aqst_ferias)) :                             
                           find first sit_afast no-lock
                                where sit_afast.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func no-error.
                           if sit_afast.log_influi_ferias = yes then do:  
                                   disp sit_afast.des_sit_afast_func
                                        sit_afast_func.dat_inic_sit_afast                                           
                                        sit_afast_func.dat_term_sit_afast                                           
                                        with frame f-sit.
                                   down with frame f-sit.                                                                       
                           end.
                  end.
            end.                                      
         &endif                                  
      end.
      else do:
         display bfrctrcal.dat_inic_period_aqst_ferias
                 bfrctrcal.dat_term_period_aqst_ferias
                 d-dt-ini-fer           when bfrctrcal.qtd_dias_ferias_gozar  > 0
                 c-lit-a1               when bfrctrcal.qtd_dias_ferias_gozar  > 0
                 d-dt-fim-fer           when bfrctrcal.qtd_dias_ferias_gozar  > 0
                 c-lit-dd-1             when bfrctrcal.qtd_dias_ferias_gozar  > 0
                 bfrctrcal.qtd_dias_ferias_gozar  when bfrctrcal.qtd_dias_ferias_gozar  > 0
            with frame f-recibo-3.
         down with frame f-recibo-3.
         &if "{&dthrpyc_version}" >= "2.06" &then 
            if tt-param.v_log_situacao = yes then do:
                  for each sit_afast_func no-lock where 
                           sit_afast_func.cdn_empresa        = bfrctrcal.cdn_empresa                      and
                           sit_afast_func.cdn_estab          = bfrctrcal.cdn_estab                        and
                           sit_afast_func.cdn_funcionario    = bfrctrcal.cdn_funcionario                  and
                           ((sit_afast_func.dat_inic_sit_afast >= bfrctrcal.dat_inic_period_aqst_ferias   and
                            sit_afast_func.dat_inic_sit_afast  <= bfrctrcal.dat_term_period_aqst_ferias)  or
                           (sit_afast_func.dat_term_sit_afast  <= bfrctrcal.dat_term_period_aqst_ferias   and
                            sit_afast_func.dat_term_sit_afast  >= bfrctrcal.dat_inic_period_aqst_ferias)) :                             
                           find first sit_afast no-lock
                                where sit_afast.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func no-error.
                           if sit_afast.log_influi_ferias = yes then do:  
                                   disp sit_afast.des_sit_afast_func
                                        sit_afast_func.dat_inic_sit_afast                                           
                                        sit_afast_func.dat_term_sit_afast                                           
                                        with frame f-sit.
                                   down with frame f-sit.                                                                       
                           end.
                  end.
            end.               
         &endif                     
      end.
      if duas-ferias then do:
         assign v_dat_ini_abono  = ?
                v_dat_term_abono = ?.

         if bfrctrcal.qtd_dias_abono_ferias > 0 and 
            bhabilit.qtd_dias_abono_ferias > 0 then do:
            display bfrctrcal.dat_inic_abono_ferias when bfrctrcal.dat_inic_abono_ferias <>
                                                         bfrctrcal.dat_inic_concess_ferias
                    c-lit-a2                        when bfrctrcal.dat_inic_abono_ferias <>
                                                         bfrctrcal.dat_inic_concess_ferias
                    bfrctrcal.dat_term_abono_ferias when bfrctrcal.dat_inic_abono_ferias <>
                                                         bfrctrcal.dat_inic_concess_ferias
                    c-lit-dd-2                      
                    bfrctrcal.qtd_dias_abono_ferias 
                    bhabilit.dat_inic_abono_ferias when bhabilit.dat_inic_abono_ferias <>
                                                        bhabilit.dat_inic_concess_ferias
                    c-lit-a4                        when bhabilit.dat_inic_abono_ferias <>
                                                         bhabilit.dat_inic_concess_ferias
                    bhabilit.dat_term_abono_ferias when bhabilit.dat_inic_abono_ferias <>
                                                        bhabilit.dat_inic_concess_ferias
                    c-lit-dd-4                      
                    bhabilit.qtd_dias_abono_ferias
                    d-dt-ini-lic           when bfrctrcal.qtd_dias_licenc > 0
                    c-lit-a3               when bfrctrcal.qtd_dias_licenc > 0
                    d-dt-fim-lic           when bfrctrcal.qtd_dias_licenc > 0
                    c-lit-dd-3             when bfrctrcal.qtd_dias_licenc > 0
                    qtd-dias-falta
                    dt-retorno-tr
                    bfrctrcal.qtd_dias_licenc when bfrctrcal.qtd_dias_licenc > 0
               with frame f-recibo-8.
            down with frame f-recibo-8.
         end.
         else do:
            if bfrctrcal.qtd_dias_abono_ferias > 0 then
               assign v_dat_ini_abono  = bfrctrcal.dat_inic_abono_ferias 
                                                     when bfrctrcal.dat_inic_abono_ferias <>
                                                          bfrctrcal.dat_inic_concess_ferias
                      v_dat_term_abono = bfrctrcal.dat_term_abono_ferias 
                                                     when bfrctrcal.dat_inic_abono_ferias <>
                                                          bfrctrcal.dat_inic_concess_ferias
                      v_qtd_dias_abono = bfrctrcal.qtd_dias_abono_ferias.
            if bhabilit.qtd_dias_abono_ferias > 0 then
               assign v_dat_ini_abono  = bhabilit.dat_inic_abono_ferias 
                                                     when bhabilit.dat_inic_abono_ferias <>
                                                          bhabilit.dat_inic_concess_ferias
                      v_dat_term_abono = bhabilit.dat_term_abono_ferias 
                                                     when bhabilit.dat_inic_abono_ferias <>
                                                          bhabilit.dat_inic_concess_ferias
                      v_qtd_dias_abono = bhabilit.qtd_dias_abono_ferias.

            display v_dat_ini_abono  when v_dat_ini_abono <> ?  @ bfrctrcal.dat_inic_abono_ferias 
                    c-lit-a2         when v_dat_ini_abono <> ?
                    v_dat_term_abono when v_dat_term_abono <> ? @ bfrctrcal.dat_term_abono_ferias 
                    c-lit-dd-2       
                    v_qtd_dias_abono when v_qtd_dias_abono > 0  @ bfrctrcal.qtd_dias_abono_ferias 
                    d-dt-ini-lic           when bfrctrcal.qtd_dias_licenc > 0
                    c-lit-a3               when bfrctrcal.qtd_dias_licenc > 0
                    d-dt-fim-lic           when bfrctrcal.qtd_dias_licenc > 0
                    c-lit-dd-3             when bfrctrcal.qtd_dias_licenc > 0
                    qtd-dias-falta       
                    dt-retorno-tr
                    bfrctrcal.qtd_dias_licenc when bfrctrcal.qtd_dias_licenc > 0
                    with frame f-recibo-4.
            down with frame f-recibo-4.
         end.
      end.
      else do:
         display bfrctrcal.dat_inic_abono_ferias when bfrctrcal.qtd_dias_abono_ferias   > 0 and
                                             bfrctrcal.dat_inic_abono_ferias <>
                                                                bfrctrcal.dat_inic_concess_ferias
                 c-lit-a2               when bfrctrcal.qtd_dias_abono_ferias   > 0 and
                                             bfrctrcal.dat_inic_abono_ferias <>
                                                                bfrctrcal.dat_inic_concess_ferias
                 bfrctrcal.dat_term_abono_ferias when bfrctrcal.qtd_dias_abono_ferias   > 0 and
                                             bfrctrcal.dat_inic_abono_ferias <>
                                                                bfrctrcal.dat_inic_concess_ferias
                 c-lit-dd-2             when bfrctrcal.qtd_dias_abono_ferias   > 0
                 bfrctrcal.qtd_dias_abono_ferias   when bfrctrcal.qtd_dias_abono_ferias   > 0
                 d-dt-ini-lic           when bfrctrcal.qtd_dias_licenc > 0
                 c-lit-a3               when bfrctrcal.qtd_dias_licenc > 0
                 d-dt-fim-lic           when bfrctrcal.qtd_dias_licenc > 0
                 c-lit-dd-3             when bfrctrcal.qtd_dias_licenc > 0
                 qtd-dias-falta
                 dt-retorno-tr
                 bfrctrcal.qtd_dias_licenc when bfrctrcal.qtd_dias_licenc > 0
            with frame f-recibo-4.
         down with frame f-recibo-4.
      end.  
      
      assign v_val_base_inss = 0.
      for each reg_det_ferias no-lock where
               reg_det_ferias.cdn_empresa         = movto_ferias_calcul.cdn_empresa     and   
               reg_det_ferias.cdn_estab           = movto_ferias_calcul.cdn_estab       and   
               reg_det_ferias.cdn_funcionario     = movto_ferias_calcul.cdn_funcionario and   
               reg_det_ferias.dat_inic_ferias     = movto_ferias_calcul.dat_inic_ferias and   
               reg_det_ferias.cdn_tip_calc_ferias = movto_ferias_calcul.cdn_tip_calc_ferias:
          /* chamada epc Eucatex*/
          empty temp-table tt-epc no-error.
          
          create tt-epc.
          assign tt-epc.cod-event      = "impr_comp_base_irf"
                 tt-epc.val-parameter  = string(reg_det_ferias.cdn_empresa) + ';' +
                                         string(reg_det_ferias.cdn_estab) + ';' +
                                         string(reg_det_ferias.cdn_funcionario) + ';' +
                                         string(reg_det_ferias.dat_inic_ferias) + ';' +
                                         STRING(reg_det_ferias.cdn_tip_calc_ferias) + ';' +
                                         STRING(movto_ferias_calcul.dat_pagto_salario).

          {include/i-epc201.i "impr_comp_base_irf"}

          /*-----*/
          do i-inx = 1 to reg_det_ferias.qti_efp:
             if reg_det_ferias.cdn_idx_efp_espcif_ferias[i-inx] = 8
             or reg_det_ferias.cdn_idx_efp_espcif_ferias[i-inx] = 17 then
                next.
             if reg_det_ferias.cdn_idx_efp_espcif_ferias[i-inx] = 1 then do:
                if i-vias-aux = 1 then
                   assign d-liq-pagar = d-liq-pagar +
                                        if reg_det_ferias.log_livre_1 
                                        then reg_det_ferias.val_efp_mes_inic_ferias[i-inx] +
                                             reg_det_ferias.val_efp_seguinte_ferias[i-inx] + 
                                             reg_det_ferias.val_tot_efp_ferias[i-inx]
                                        else reg_det_ferias.val_tot_efp_ferias[i-inx].
                next.
             end.
             if reg_det_ferias.cdn_event_fp[i-inx] = "000" then
                next.
             find event_fp no-lock where
                  event_fp.cdn_event_fp = reg_det_ferias.cdn_event_fp[i-inx] no-error.
             if event_fp.log_impr_envel_fp = no then
                next.

             if duas-ferias then do:   
                if event_fp.idi_tip_inciden_inss = 1 then do:
                   assign v_val_base_inss = if reg_det_ferias.log_livre_1
                                            then  v_val_base_inss +
                                                 (reg_det_ferias.val_efp_mes_inic_ferias[i-inx] +
                                                  reg_det_ferias.val_efp_seguinte_ferias[i-inx] +
                                                  reg_det_ferias.val_tot_efp_ferias[i-inx])
                                            else v_val_base_inss + 
                                                 reg_det_ferias.val_tot_efp_ferias[i-inx].
                end.
                else do:
                   if event_fp.idi_tip_inciden_inss = 2 then do:
                      assign v_val_base_inss = if reg_det_ferias.log_livre_1
                                               then v_val_base_inss - 
                                                   (reg_det_ferias.val_efp_mes_inic_ferias[i-inx] +
                                                    reg_det_ferias.val_efp_seguinte_ferias[i-inx] +
                                                    reg_det_ferias.val_tot_efp_ferias[i-inx])
                                               else v_val_base_inss - 
                                                    reg_det_ferias.val_tot_efp_ferias[i-inx].
                   end.
                end.
             end.

             find first tt-recibo where tt-recibo.ev-codigo = reg_det_ferias.cdn_event_fp[i-inx] no-error.
             if not avail tt-recibo then do:
                create tt-recibo.
                assign tt-recibo.ev-codigo   = reg_det_ferias.cdn_event_fp[i-inx]
                       tt-recibo.descricao   = event_fp.des_event_fp
                       tt-recibo.ident       = event_fp.idi_ident_efp                 
                       tt-recibo.inc-liquido = event_fp.idi_tip_inciden_liq
                       tt-recibo.c-inc-liq   = if tt-recibo.inc-liquido = 1
                                             then "+"
                                             else if tt-recibo.inc-liquido = 2
                                                  then "-"
                                                  else " ".
             end.

             /** somente calcular os valores para a primeira via **/          
             if i-vias-aux = 1 or
                not duas-ferias then do:
                assign tt-recibo.base        = if duas-ferias 
                                             then if event_fp.cdn_idx_efp_funcao_espcif = 18 
                                                  then tt-recibo.base + v_val_base_inss
                                                  else tt-recibo.base
                                             else if reg_det_ferias.log_livre_1
                                                  then reg_det_ferias.val_base_mes_inic_ferias[i-inx] +
                                                       reg_det_ferias.val_base_seguinte_ferias[i-inx] +
                                                       reg_det_ferias.val_base_calc_ferias[i-inx]
                                                  else reg_det_ferias.val_base_calc_ferias[i-inx]
                       tt-recibo.valor       = tt-recibo.valor +
                                             if reg_det_ferias.log_livre_1
                                             then reg_det_ferias.val_efp_mes_inic_ferias[i-inx] +
                                                  reg_det_ferias.val_efp_seguinte_ferias[i-inx] + 
                                                  reg_det_ferias.val_tot_efp_ferias[i-inx]
                                             else reg_det_ferias.val_tot_efp_ferias[i-inx].
             end.
          end.
      end.
      for each tt-recibo
               break by tt-recibo.ident
                     by tt-recibo.ev-codigo:
          display tt-recibo.ev-codigo
                  tt-recibo.descricao
                  tt-recibo.base    when tt-recibo.base  > 0
                  tt-recibo.valor
                  tt-recibo.c-inc-liq
               with frame f-recibo-5.
          down with frame f-recibo-5.
          assign d-tot-ident = d-tot-ident + tt-recibo.valor.
          if last-of(tt-recibo.ident) then do:
             if tt-recibo.ident = 1 then
                put "|"                              at 01
                    "Total Vencimentos............:" at 12
                    d-tot-ident                      at 62
                    "|"                              at 80 skip
                    "|"                              at 01
                    "|"                              at 80 skip.
             if tt-recibo.ident = 2 then
                put "|"                              at 01
                    "Total Descontos..............:" at 12
                    d-tot-ident                      at 62
                    "|"                              at 80 skip
                    "|"                              at 01
                    "|"                              at 80 skip.
             if tt-recibo.ident = 3 then
                put "|"                              at 01
                    "Total Outros.................:" at 12
                    d-tot-ident                      at 62
                    "|"                              at 80 skip
                    "|"                              at 01
                    "|"                              at 80 skip.
             assign d-tot-ident = 0.
          end.
          if not duas-ferias then
             delete tt-recibo.
      end.
      display d-liq-pagar     with frame f-recibo-6.
      down with frame f-recibo-6.
      if line-counter > 0 then do while line-counter < 44:
         put  "|" at 01
              "|" at 80 skip.
      end.
      assign d-valor     = d-liq-pagar.
             d-vlr-total = d-liq-pagar.
      run prghur/fpp/fp9900rp.p.
      assign c-extenso-1 = substring(c-extenso,1,55)
             c-extenso-2 = substring(c-extenso,56,131)
             c-extenso-3 = substring(c-extenso,132,189).

      display c-estab
              d-vlr-total
              c-extenso-1
              c-extenso-2
              c-extenso-3
              c-local
              c-funciona     with frame f-recibo-7.
      down with frame f-recibo-7.
      /* chamada epc Eucatex*/
      empty temp-table tt-epc no-error.
      
      create tt-epc.
      assign tt-epc.cod-event      = "impr_comp_base".
             /*tt-epc.val-parameter  = string(v_num_cod) + ';' +
                                     string(v_des_nome) + ';' +
                                     string(i-matr-func).*/

      {include/i-epc201.i "impr_comp_base"}
      
      /*-----*/

      assign movto_ferias_calcul.log_recibo_impresso = yes.
   end.


end.

for each tt-recibo:
    delete tt-recibo.
end.
return "ok".

END PROCEDURE.
