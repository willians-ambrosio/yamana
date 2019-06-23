/******************************************************************************
**       Programa: prghur/esp/YMFR0001rp.p
**       Data....: Setembro 2009
**       Autor...: Datasul HCM
**       Objetivo: Listagem Aviso Ferias em Dobro
******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i YMFR0001RP 1.00.00.000}

{include/i-rpvar.i}

&GLOBAL-DEFINE RTF YES
{prghur/fpp/fp9501.i}

/***************************** Definicao de Variaveis Locais ******************/
&GLOBAL-DEFINE emit_func  no
{esp/ymfr0001tt.i new shared}  /* Parametro */  /* Parametro */
{prghur/fpp/fp9200.i10 new shared}
{prghur/fpp/fp9200.i8}

def temp-table tt-raw-digita
    field raw-digita       as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.
{prghur/fpp/fp9200.i11}

def buffer bhabilit_ferias      for habilit_ferias.
def buffer bfperiod_aqst_ferias for period_aqst_ferias.

def var i-fc-codigo    like funcionario.cdn_funcionario initial "0" no-undo.
def var c-titulo-p       as char format "x(40)"                     no-undo.                           
def var c-titulo-c       as char format "x(40)"                     no-undo.
def var c-titulo-i       as char format "x(40)"                     no-undo.
def var c-titulo-s       as char format "x(40)"                     no-undo.
def var c--programa2     as char format "x(20)"                     no-undo.
def var l-imprime        as logical                                 no-undo. 
def var i-es-codigo    like rh_estab.cdn_estab                      no-undo.
def var c-es-nome      like rh_estab.nom_pessoa_jurid               no-undo.
def var i-matr-func      as char format "x(11)"                     no-undo.
def var c-barra          as char format "x"     initial "/"         no-undo.
def var c-hifem          as char format "x"     initial "-"         no-undo.
def var i-dias-dir       as dec  format ">9.9"                      no-undo.
def var i-dias-conc      as dec  format ">9.9"                      no-undo.
def var i-dias-saldo     as dec  format ">>9.9-"                    no-undo.
def var d-dt-lim         as date                                    no-undo.
def var l-primeiro       as log                                     no-undo.
def var l-imp-per        as log                                     no-undo.
def var c-tp-ferias      as char format "x(08)" initial ""          no-undo.
def var adto-13S         as log  format "S/N"                       no-undo.
def var v_log_individual as log                                     no-undo.
def var v_log_coletiva   as log                                     no-undo. 
def var i-contador       as int initial 0                           no-undo.
def var v_log_retorno    as logical init no                         no-undo.
def var v_num_dias       as decimal init 0                          no-undo.
def var v_log_prog_neg   as logical                                 no-undo.
def var c-detalhe        as char format "x(80)"      initial ""     no-undo.
def var v_val_aux      as int                                     no-undo.
def var i-ind          as int                                     no-undo.
def var i-matr-ini   like funcionario.cdn_funcionario             no-undo.
def var i-matr-fim   like funcionario.cdn_funcionario             no-undo.

/********************* Definicao de Variaveis New Shared **********************/
def new shared var i-ep-codigo like empresa.ep-codigo    no-undo.
def new shared var i-ordem2    as int                    no-undo.
def new shared var i-falta     as dec  initial 0         no-undo.
def new shared var v_log_folha_educnal as log initial no no-undo.
def new shared var v_han_acomp as handle                 no-undo.

def workfile wfrctrcal like habilit_ferias
    field wempresa      like param_empres_rh.cdn_empresa
    field westabel      like rh_estab.cdn_estab
    field wfunc         like funcionario.cdn_funcionario
    field wdt-ferias    like habilit_ferias.dat_inic_ferias
    field wdias-conced  like habilit_ferias.qtd_dias_ferias_gozar
    field wdt-inic-per  like habilit_ferias.dat_inic_period_aqst_ferias
    field wdt-term-per  like habilit_ferias.dat_term_period_aqst_ferias
    field wdias-abono   like habilit_ferias.qtd_dias_abono_ferias
    field wdias-licenca like habilit_ferias.qtd_dias_licenc.

run utp/ut-acomp.p persistent set v_han_acomp.

/************************* Definicao de Frames ********************************/

form header
    "Estabelecimento:" at 01
    i-es-codigo
    "-"
    c-es-nome                skip(1)
    with stream-io no-labels no-box no-attr-space width 132 page-top frame f-cab-estb.

form header
    "------ Per¡odo ------ - Dias --  ----------- Programa‡Æo -----------   Dias   Adt"  at 44
    "Matric Nome                                In¡cio    T‚rmino  Dir Conc      In¡cio Tipo      Gozo  Lic Abono  Saldo  13S Faltas" at 05
    "---------- ------------------------------- ---------- ---------- ---- ----  ---------- -------- ----- ---- -----  -----  --- ------" at 01
    with stream-io no-labels no-box no-attr-space width 132 page-top frame f-cab-dados.

form header
    "------ Per¡odo ------ - Dias --  ----------- Programa‡Æo -----------   Dias   Adt"  at 44
    "CS     Matric Nome                             In¡cio    T‚rmino  Dir Conc      In¡cio Tipo      Gozo  Lic Abono  Saldo  13S Faltas" at 01
    "-- ---------- ---------------------------- ---------- ---------- ---- ----  ---------- -------- ----- ---- -----  -----  --- ------" at 01
    with stream-io no-labels no-box no-attr-space width 132 page-top frame f-cab-categ.

form
    i-matr-func                                  at  01
    funcionario.nom_pessoa_fisic  format "x(31)" at  13
    habilit_ferias.dat_inic_period_aqst_ferias   at  44
    habilit_ferias.dat_term_period_aqst_ferias   at  55
    i-dias-dir                                   at  66
    i-dias-conc                                  at  71
    habilit_ferias.dat_inic_ferias               at  77
    c-tp-ferias                                  at  89
    habilit_ferias.qtd_dias_ferias_gozar         at  97
    habilit_ferias.qtd_dias_licenc               at 102
    habilit_ferias.qtd_dias_abono_ferias         at 108
    i-dias-saldo                                 at 114
    adto-13S                                     at 124 space(1)
    i-faltas-l502                 format "zzzzz9" 
    with stream-io no-labels no-box no-attr-space width 132 53 down frame f-dados.

form
    funciona.cdn_categ_sal                       AT  01
    i-matr-func                                  at  04
    funcionario.nom_pessoa_fisic  format "x(28)" at  15
    habilit_ferias.dat_inic_period_aqst_ferias   at  44
    habilit_ferias.dat_term_period_aqst_ferias   at  55
    i-dias-dir                                   at  66
    i-dias-conc                                  at  71
    habilit_ferias.dat_inic_ferias               at  77
    c-tp-ferias                                  at  88
    habilit_ferias.qtd_dias_ferias_gozar         at  97 
    habilit_ferias.qtd_dias_licenc               at 102 
    habilit_ferias.qtd_dias_abono_ferias         at 108
    i-dias-saldo                                 at 114
    adto-13S                                     at 124 space(1)
    i-faltas-l502                 format "zzzzz9" 
    with stream-io no-labels no-box no-attr-space width 132 53 down frame f-categ.

form 
   tt-param.d-dt-inicio         colon 40
   "|<  >|"                     colon 53
   tt-param.d-dt-final no-label colon 61 
   with stream-io side-labels attr-space  no-box width 132 frame f-selecao.

form 
   tt-param.d-dt-inicio          colon 40
   "|<  >|"                      colon 53
   tt-param.d-dt-final no-label  colon 61 
   with stream-io side-labels attr-space  no-box width 132 frame f-selecao-educ.

form
   c--programa2              colon 40 
   with stream-io side-labels attr-space  no-box width 132 frame f-param.

form
    skip(1)
    c-detalhe                                    at 45
    skip(1)
    with stream-io no-labels no-box no-attr-space width 132 53 down frame f-prog_neg.

{utp/ut-liter.i Data_F‚rias MFR}
assign tt-param.d-dt-inicio:label in frame f-selecao = return-value.
{utp/ut-liter.i Imprime_Programa‡Æo MFR}
assign c--programa2:label in frame f-param = return-value.

/**************************** Inicio do Programa ******************************/
{utp/ut-liter.i Listagem_de_F‚rias_Programadas *}
assign c-titulo-relat = return-value.

{utp/ut-liter.i F‚rias_e_Rescisäes *}
assign c-sistema = return-value.
run pi-inicializar in v_han_acomp (input c-titulo-relat).

find empresa no-lock where empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.
assign c-empresa  = "Empresa " + empresa.razao-social
       c-programa = "YMFR0001"
       c-versao   = "1.00.00"
       c-revisao  = "000"
       l-imprime  = no.

run utp/ut-trfrrp.p (input frame f-prog_neg:handle).
run utp/ut-trfrrp.p (input frame f-param:handle).
run utp/ut-trfrrp.p (input frame f-selecao-educ:handle).
run utp/ut-trfrrp.p (input frame f-selecao:handle).
run utp/ut-trfrrp.p (input frame f-categ:handle).
run utp/ut-trfrrp.p (input frame f-dados:handle).
run utp/ut-trfrrp.p (input frame f-cab-categ:handle).
run utp/ut-trfrrp.p (input frame f-cab-dados:handle).
run utp/ut-trfrrp.p (input frame f-cab-estb:handle).
{include/i-rpcab.i}
{include/i-rpout.i}

    MESSAGE '  tt-param.parametro             '  tt-param.parametro                   skip
            '  tt-param.v_cdn_empres_usuar    '  tt-param.v_cdn_empres_usuar          skip
            '  tt-param.v_cod_unid_lotac_ini  '  tt-param.v_cod_unid_lotac_ini        skip
            '  tt-param.v_cod_unid_lotac_fim  '  tt-param.v_cod_unid_lotac_fim        skip
            '  tt-param.i-es-ini              '  tt-param.i-es-ini                    skip
            '  tt-param.i-es-fim              '  tt-param.i-es-fim                    skip
            '  tt-param.i-fc-ini              '  tt-param.i-fc-ini                    skip
            '  tt-param.i-fc-fim              '  tt-param.i-fc-fim                    skip
            '  tt-param.v_num_tip_aces_usuar  '  tt-param.v_num_tip_aces_usuar        skip
            '  tt-param.v_cod_grp_usuar       '  tt-param.v_cod_grp_usuar             skip
            '  tt-param.v_num_opcao           '  tt-param.v_num_opcao                 skip
            '  tt-param.v_des_opcao           '  tt-param.v_des_opcao                 skip
            '  tt-param.v_dat_valid           '  tt-param.v_dat_valid                 skip
            '  tt-param.v_log_expande_estrut  '  tt-param.v_log_expande_estrut        skip
            '  tt-param.v_num_salta_pg        '  tt-param.v_num_salta_pg              skip
            '  tt-param.v_num_quebra          '  tt-param.v_num_quebra                skip
            '  tt-param.v_num_faixa           '  tt-param.v_num_faixa                 skip
            '  tt-param.destino               '  tt-param.destino                     skip
            '  tt-param.arquivo               '  tt-param.arquivo                     skip
            '  tt-param.modelo-rtf            '  tt-param.modelo-rtf                  skip
            '  tt-param.l-habilitaRtf         '  tt-param.l-habilitaRtf               skip
            '  tt-param.usuario               '  tt-param.usuario                     skip
            '  tt-param.data-exec             '  tt-param.data-exec                   skip
            '  tt-param.hora-exec             '  tt-param.hora-exec                   skip
            '  tt-param.classifica            '  tt-param.classifica                  skip
            '  tt-param.desc-classifica       '  tt-param.desc-classifica             skip
            '  tt-param.d-dt-inicio           '  tt-param.d-dt-inicio                 skip
            '  tt-param.d-dt-final            '  tt-param.d-dt-final                  skip
            '  i-ordem2                       '  i-ordem2                             skip
                                                                                      

        VIEW-AS ALERT-BOX INFO BUTTONS OK.

   IF NOT tt-param.l-habilitaRtf THEN
    view frame f-cabec.
   IF NOT tt-param.l-habilitaRtf THEN
    view frame f-rodape.

   if classifica = 3
   or classifica = 4 then do with frame f-dados:
      view frame f-cab-estb.
      view frame f-cab-dados.
      {prghur/fpp/fp9200.i6}
      assign i-fc-codigo = {prghur/dop/eng002.i}.
      assign i-dias-saldo = 0.
      for each tt_lotac_funcionario no-lock,
         each funcionario of tt_lotac_funcionario no-lock, 
         each habilit_ferias of funcionario       no-lock where
              habilit_ferias.idi_tip_ferias       = 1 /* Individual */
              break by funcionario.cdn_empresa
                    by funcionario.cdn_estab
                    by tt_lotac_funcionario.num_seq_unid_lotac
                    by tt_lotac_funcionario.num_niv_unid_lotac
                    by tt_lotac_funcionario.cod_unid_lotac
                    by if i-ordem2 = 3
                         then string(funcionario.cdn_funcionario,"99999999")
                         else funcionario.nom_pessoa_fisic with frame f-dados:

         FIND period_aqst_ferias OF habilit_ferias NO-LOCK NO-ERROR.
         IF NOT AVAIL period_aqst_ferias THEN
            NEXT.

         /** FO 1795.021 
             Verificar se o per¡odo aquisitivo possui concessäes hist¢ricas sem programa‡Æo de f‚rias **/
         assign v_val_aux = 0.
         do i-ind = 1 to 5 :
             if period_aqst_ferias.dat_concess_efetd[i-ind] <> ? then do:
                 if not can-find(bhabilit_ferias no-lock where
                                 bhabilit_ferias.cdn_empresa = period_aqst_ferias.cdn_empresa and
                                 bhabilit_ferias.cdn_estab = period_aqst_ferias.cdn_estab and
                                 bhabilit_ferias.cdn_funcionario = period_aqst_ferias.cdn_funcionario and
                                 bhabilit_ferias.dat_inic_period_aqst_ferias = period_aqst_ferias.dat_inic_period_aqst_ferias and
                                 bhabilit_ferias.dat_inic_ferias = period_aqst_ferias.dat_concess_efetd[i-ind]) then do:
                    assign v_val_aux = v_val_aux + period_aqst_ferias.qtd_dias_gozado[i-ind] + period_aqst_ferias.qtd_dias_abdo[i-ind].
                 end.
             end.
         end.
         /**********************************************/

         assign i-contador = i-contador + 1.
         run pi-acompanhar in v_han_acomp (input i-contador).
         assign i-dias-conc = 0.

         create wfrctrcal.
         assign wfrctrcal.wempresa      = habilit_ferias.cdn_empresa
                wfrctrcal.westabel      = habilit_ferias.cdn_estab
                wfrctrcal.wfunc         = habilit_ferias.cdn_funcionario
                wfrctrcal.wdt-ferias    = habilit_ferias.dat_inic_ferias
                wfrctrcal.wdias-conced  = habilit_ferias.qtd_dias_ferias_gozar
                wfrctrcal.wdt-inic-per  = habilit_ferias.dat_inic_period_aqst_ferias
                wfrctrcal.wdt-term-per  = habilit_ferias.dat_term_period_aqst_ferias
                wfrctrcal.wdias-abono   = habilit_ferias.qtd_dias_abono_ferias
                wfrctrcal.wdias-licenca = habilit_ferias.qtd_dias_licenc.

         if first-of(funcionario.cdn_estab) then do:
            find rh_estab of funcionario NO-LOCK NO-ERROR.
               assign c-es-nome   = rh_estab.nom_pessoa_jurid
                      i-es-codigo = rh_estab.cdn_estab.
         end.

         If first-of(tt_lotac_funcionario.cod_unid_lotac) then
            assign v_log = yes.

         /*if period_aqst_ferias.log_calcula_dias_direito_autom then*/ do:
            assign d-inic-per = period_aqst_ferias.dat_inic_period_aqst_ferias
                   d-term-per = period_aqst_ferias.dat_term_period_aqst_ferias.
            {prghur/fpp/fp9502.i}
            assign i-dias-dir  = i-dias-fer
                   i-dias-conc = period_aqst_ferias.qtd_dias_ferias_concedid.
            assign i-dias-saldo = i-dias-dir - v_val_aux.
         end.
         for each wfrctrcal no-lock where 
                  wfrctrcal.wempresa      = period_aqst_ferias.cdn_empresa and
                  wfrctrcal.westabel      = period_aqst_ferias.cdn_estab   and
                  wfrctrcal.wfunc         = period_aqst_ferias.cdn_funcionario and
                  wfrctrcal.wdt-inic-per  = d-inic-per         and
                  wfrctrcal.wdt-term-per  = d-term-per         and
                  wfrctrcal.wdt-ferias < habilit_ferias.dat_inic_ferias:

            assign i-dias-saldo = i-dias-saldo - (wfrctrcal.wdias-licenca +
                                                  wfrctrcal.wdias-conced +
                                                  wfrctrcal.wdias-abono).
         end.
         assign i-dias-saldo = i-dias-saldo - (habilit_ferias.qtd_dias_licenc +
                                               habilit_ferias.qtd_dias_ferias_gozar  +
                                               habilit_ferias.qtd_dias_abono_ferias).
         if line-counter > 63 then do:
            page.
            assign l-primeiro = yes
                   l-imp-per  = yes.
         end.

         find first movto_ferias_calcul of funcionario no-lock
             where movto_ferias_calcul.dat_inic_ferias = habilit_ferias.dat_inic_ferias and
                   movto_ferias_calcul.cdn_tip_calc_ferias = 0 no-error.
         
         assign l-primeiro  = no
                l-imp-per   = no.
         if i-fc-codigo <> funcionario.cdn_funcionario then do:
            if habilit_ferias.dat_inic_ferias >= tt-param.d-dt-inicio and
               habilit_ferias.dat_inic_ferias <= tt-param.d-dt-final then do:
               assign i-fc-codigo = funcionario.cdn_funcionario
                      l-primeiro  = yes
                      l-imp-per   = yes.
            end.
         end. 

         if v_log then do:
            if habilit_ferias.dat_inic_ferias >= tt-param.d-dt-inicio and
               habilit_ferias.dat_inic_ferias <= tt-param.d-dt-final then do  :
               Run pi_report_unid_lotac.
               assign v_log = no.
            end.   
         end.

         assign c-tp-ferias = if habilit_ferias.idi_tip_ferias = 1
                                 then "Normal"
                                 else "Coletiva".


         assign i-matr-func = string(funcionario.cdn_funcionario, "zzzzzzz9") + c-hifem +
                              string(funcionario.num_digito_verfdor_func).           

         if habilit_ferias.dat_inic_ferias >= tt-param.d-dt-inicio and
            habilit_ferias.dat_inic_ferias <= tt-param.d-dt-final then do:

            if l-primeiro then
               display i-matr-func
                       funciona.nom_pessoa_fisic format "x(31)".
            assign adto-13S = no.
            if habilit_ferias.vli_perc_adiant_13o > 0 then adto-13S = yes.
            if l-imp-per then do:
               display habilit_ferias.dat_inic_period_aqst_ferias
                       habilit_ferias.dat_term_period_aqst_ferias
                       i-dias-dir
                       i-dias-conc.
            end.           

            display habilit_ferias.dat_inic_ferias
                    c-tp-ferias
                    habilit_ferias.qtd_dias_ferias_gozar
                    habilit_ferias.qtd_dias_licenc
                    habilit_ferias.qtd_dias_abono_ferias
                    i-dias-saldo
                    adto-13S
                    i-faltas-l502.
            down.

            assign l-primeiro = no
                   l-imp-per  = no.

         end.
      end.
   end.

   assign i-matr-ini = tt-param.i-fc-ini  
          i-matr-fim = tt-param.i-fc-fim.  

   IF classifica = 5 THEN DO:
       view frame f-cab-estb.
       view frame f-cab-categ.
       for each funcionario no-lock
           where funcionario.cdn_empresa =  tt-param.v_cdn_empres_usuar and
                 funcionario.cdn_estab >= i-es-ini    and
                 funcionario.cdn_estab <= i-es-fim    and
                 funcionario.cdn_funcionario >= i-matr-ini and
                 funcionario.cdn_funcionario <= i-matr-fim and
                (funcionario.dat_desligto_func = ? or
                 funcionario.dat_desligto_func >= tt-param.d-dt-inicio) 
           break by funcionario.cdn_empresa
                 by funcionario.cdn_estab
                 BY funcionario.cdn_categ_sal
                 BY funcionario.cdn_funcionario:
           if first-of(funcionario.cdn_estab) then do:
              find rh_estab of funcionario NO-LOCK NO-ERROR.
              assign c-es-nome = rh_estab.nom_pessoa_jurid
                     i-es-codigo = rh_estab.cdn_estab.
              page.
           end.
           if not(can-find(first habilit_ferias of funcionario )) then
              next.

           assign i-contador = i-contador + 1.
           run pi-acompanhar in v_han_acomp (input i-contador).                     

           assign i-dias-saldo = 0.

           for each habilit_ferias of funcionario use-index hbltfrs_py07903 no-lock where
                    habilit_ferias.idi_tip_ferias = 1 with frame f-categ:

               find period_aqst_ferias of habilit_ferias no-lock no-error.
               IF NOT AVAIL period_aqst_ferias THEN NEXT.

               /** FO 1795.021 
                   Verificar se o per¡odo aquisitivo possui concessäes hist¢ricas sem programa‡Æo de f‚rias **/
               assign v_val_aux = 0.
               do i-ind = 1 to 5 :
                   if period_aqst_ferias.dat_concess_efetd[i-ind] <> ? then do:
                       if not can-find(bhabilit_ferias no-lock where
                                       bhabilit_ferias.cdn_empresa = period_aqst_ferias.cdn_empresa and
                                       bhabilit_ferias.cdn_estab = period_aqst_ferias.cdn_estab and
                                       bhabilit_ferias.cdn_funcionario = period_aqst_ferias.cdn_funcionario and
                                       bhabilit_ferias.dat_inic_period_aqst_ferias = period_aqst_ferias.dat_inic_period_aqst_ferias and
                                       bhabilit_ferias.dat_inic_ferias = period_aqst_ferias.dat_concess_efetd[i-ind]) then do:
                          assign v_val_aux = v_val_aux + period_aqst_ferias.qtd_dias_gozado[i-ind] + period_aqst_ferias.qtd_dias_abdo[i-ind].
                       end.
                   end.
               end.
               /**********************************************/

               assign i-dias-conc = 0.

               create wfrctrcal.
               assign wfrctrcal.wempresa      = habilit_ferias.cdn_empresa
                      wfrctrcal.westabel      = habilit_ferias.cdn_estab
                      wfrctrcal.wfunc         = habilit_ferias.cdn_funcionario
                      wfrctrcal.wdt-ferias    = habilit_ferias.dat_inic_ferias
                      wfrctrcal.wdias-conced  = habilit_ferias.qtd_dias_ferias_gozar
                      wfrctrcal.wdt-inic-per  = habilit_ferias.dat_inic_period_aqst_ferias
                      wfrctrcal.wdt-term-per  = habilit_ferias.dat_term_period_aqst_ferias
                      wfrctrcal.wdias-abono   = habilit_ferias.qtd_dias_abono_ferias
                      wfrctrcal.wdias-licenca = habilit_ferias.qtd_dias_licenc.

               /*if period_aqst_ferias.log_calcula_dias_direito_autom then*/  do:
                  assign d-inic-per = period_aqst_ferias.dat_inic_period_aqst_ferias
                         d-term-per = period_aqst_ferias.dat_term_period_aqst_ferias.
                  assign i-dias-dir = 0.
                  {prghur/fpp/fp9502.i}
                  assign i-dias-dir  = i-dias-fer.
                  assign i-dias-conc = period_aqst_ferias.qtd_dias_ferias_concedid.
                  assign i-dias-saldo = i-dias-dir - v_val_aux.
               end. 

               for each wfrctrcal no-lock where 
                        wfrctrcal.wempresa      = period_aqst_ferias.cdn_empresa and
                        wfrctrcal.westabel      = period_aqst_ferias.cdn_estab   and
                        wfrctrcal.wfunc         = period_aqst_ferias.cdn_funcionario and
                        wfrctrcal.wdt-inic-per  = d-inic-per         and
                        wfrctrcal.wdt-term-per  = d-term-per         and
                        wfrctrcal.wdt-ferias < habilit_ferias.dat_inic_ferias:
                   assign i-dias-saldo = i-dias-saldo - (wfrctrcal.wdias-licenca +
                                                         wfrctrcal.wdias-conced +
                                                         wfrctrcal.wdias-abono).
              end.

              assign i-dias-saldo = i-dias-saldo - (habilit_ferias.qtd_dias_licenc +
                                                    habilit_ferias.qtd_dias_ferias_gozar  +
                                                    habilit_ferias.qtd_dias_abono_ferias).

              if line-counter > 63 then do:
                  page.
                  assign l-primeiro = yes
                         l-imp-per  = yes.
               end.
               find first movto_ferias_calcul of funcionario no-lock where 
                          movto_ferias_calcul.dat_inic_ferias = habilit_ferias.dat_inic_ferias and
                          movto_ferias_calcul.cdn_tip_calc_ferias = 0 no-error.
               
               assign l-primeiro  = no.

               if i-fc-codigo <> funcionario.cdn_funcionario then do:
                  if habilit_ferias.dat_inic_ferias >= tt-param.d-dt-inicio and
                     habilit_ferias.dat_inic_ferias <= tt-param.d-dt-final then do:
                     assign i-fc-codigo = funcionario.cdn_funcionario
                            l-primeiro  = yes
                            l-imp-per   = yes.
                  end.
               end. 

               assign c-tp-ferias = if habilit_ferias.idi_tip_ferias = 1
                                    then "Normal"
                                    else "Coletiva".

               assign i-matr-func = string(funcionario.cdn_funcionario, "zzzzzzz9") + c-hifem +
                                    string(funcionario.num_digito_verfdor_func).                                     

               if habilit_ferias.dat_inic_ferias >= tt-param.d-dt-inicio and
                  habilit_ferias.dat_inic_ferias <= tt-param.d-dt-final then do:
                  if l-primeiro then
                     display funciona.cdn_categ_sal
                             i-matr-func
                             funciona.nom_pessoa_fisic format "x(20)".
                  assign adto-13S = no.
                  if habilit_ferias.vli_perc_adiant_13o > 0 then adto-13S = yes.
                  if l-imp-per then do:
                     display habilit_ferias.dat_inic_period_aqst_ferias
                             habilit_ferias.dat_term_period_aqst_ferias
                             i-dias-dir
                             i-dias-conc.
                  end.           

                  display habilit_ferias.dat_inic_ferias
                          c-tp-ferias
                          habilit_ferias.qtd_dias_ferias_gozar
                          habilit_ferias.qtd_dias_licenc
                          habilit_ferias.qtd_dias_abono_ferias
                          i-dias-saldo
                          adto-13S
                          i-faltas-l502.
                  down.

                  assign l-primeiro = no
                         i-dias-saldo = 0.
               end.
           end.    
       end.
   END.

   assign i-ordem2 = classifica.

   if classifica = 1 
   or classifica = 2 then do:

       view frame f-cab-estb.
       view frame f-cab-dados.
   
       for each funcionario no-lock
           where funcionario.cdn_empresa         =  tt-param.v_cdn_empres_usuar and
                 funcionario.cdn_estab          >= i-es-ini    and
                 funcionario.cdn_estab          <= i-es-fim    and
                 funcionario.cdn_funcionario    >= i-matr-ini  and
                 funcionario.cdn_funcionario    <= i-matr-fim  and
                (funcionario.dat_desligto_func  = ? or
                 funcionario.dat_desligto_func  >= tt-param.d-dt-inicio) 
           break by funcionario.cdn_empresa
                 by funcionario.cdn_estab
                 by if i-ordem2 = 1
                    then string(funcionario.cdn_funcionario,"99999999")
                    else funcionario.nom_pessoa_fisic with frame f-dados:
   
           if first-of(funcionario.cdn_estab) then do:
              find rh_estab of funcionario NO-LOCK NO-ERROR.
              assign c-es-nome = rh_estab.nom_pessoa_jurid
                     i-es-codigo = rh_estab.cdn_estab.
              page.
           end.
   
           if not(can-find(first habilit_ferias of funcionario )) then
              next.
   
           assign i-contador = i-contador + 1.
           run pi-acompanhar in v_han_acomp (input i-contador).                     
   
           assign i-dias-saldo = 0.
           
           for each habilit_ferias of funcionario use-index hbltfrs_py07903 no-lock where
                    habilit_ferias.idi_tip_ferias = 1 /*or
                    habilit_ferias.idi_tip_ferias = 2*/ with frame f-dados:
   
               find period_aqst_ferias of habilit_ferias no-lock no-error.
               IF NOT AVAIL period_aqst_ferias THEN NEXT.
   
               /** Verificar se o per¡odo aquisitivo possui concessäes hist¢ricas sem programa‡Æo de f‚rias **/
               assign v_val_aux = 0.
               do i-ind = 1 to 5 :
                   if period_aqst_ferias.dat_concess_efetd[i-ind] <> ? then do:
                       if not can-find(bhabilit_ferias no-lock where
                                       bhabilit_ferias.cdn_empresa = period_aqst_ferias.cdn_empresa and
                                       bhabilit_ferias.cdn_estab = period_aqst_ferias.cdn_estab and
                                       bhabilit_ferias.cdn_funcionario = period_aqst_ferias.cdn_funcionario and
                                       bhabilit_ferias.dat_inic_period_aqst_ferias = period_aqst_ferias.dat_inic_period_aqst_ferias and
                                       bhabilit_ferias.dat_inic_ferias = period_aqst_ferias.dat_concess_efetd[i-ind]) then do:
                          assign v_val_aux = v_val_aux + period_aqst_ferias.qtd_dias_gozado[i-ind] + period_aqst_ferias.qtd_dias_abdo[i-ind].
                       end.
                   end.
               end.
               /**********************************************/
   
               assign i-dias-conc = 0.
   
               create wfrctrcal.
               assign wfrctrcal.wempresa      = habilit_ferias.cdn_empresa
                      wfrctrcal.westabel      = habilit_ferias.cdn_estab
                      wfrctrcal.wfunc         = habilit_ferias.cdn_funcionario
                      wfrctrcal.wdt-ferias    = habilit_ferias.dat_inic_ferias
                      wfrctrcal.wdias-conced  = habilit_ferias.qtd_dias_ferias_gozar
                      wfrctrcal.wdt-inic-per  = habilit_ferias.dat_inic_period_aqst_ferias
                      wfrctrcal.wdt-term-per  = habilit_ferias.dat_term_period_aqst_ferias
                      wfrctrcal.wdias-abono   = habilit_ferias.qtd_dias_abono_ferias
                      wfrctrcal.wdias-licenca = habilit_ferias.qtd_dias_licenc.
   
               /*if period_aqst_ferias.log_calcula_dias_direito_autom then*/  do:
                  assign d-inic-per = period_aqst_ferias.dat_inic_period_aqst_ferias
                         d-term-per = period_aqst_ferias.dat_term_period_aqst_ferias.
                  assign i-dias-dir = 0.
                  {prghur/fpp/fp9502.i}
                  assign i-dias-dir  = i-dias-fer.
                  assign i-dias-conc = period_aqst_ferias.qtd_dias_ferias_concedid.
                  assign i-dias-saldo = i-dias-dir - v_val_aux.
               end.
   
               for each wfrctrcal no-lock where 
                        wfrctrcal.wempresa      = period_aqst_ferias.cdn_empresa and
                        wfrctrcal.westabel      = period_aqst_ferias.cdn_estab   and
                        wfrctrcal.wfunc         = period_aqst_ferias.cdn_funcionario and
                        wfrctrcal.wdt-inic-per  = d-inic-per         and
                        wfrctrcal.wdt-term-per  = d-term-per         and
                        wfrctrcal.wdt-ferias < habilit_ferias.dat_inic_ferias:
                   assign i-dias-saldo = i-dias-saldo - (wfrctrcal.wdias-licenca +
                                                         wfrctrcal.wdias-conced +
                                                         wfrctrcal.wdias-abono).
              end.
   
              assign i-dias-saldo = i-dias-saldo - (habilit_ferias.qtd_dias_licenc +
                                                    habilit_ferias.qtd_dias_ferias_gozar  +
                                                    habilit_ferias.qtd_dias_abono_ferias).
   
              if line-counter > 63 then do:
                  page.
                  assign l-primeiro = yes
                         l-imp-per  = yes.
               end.
               find first movto_ferias_calcul of funcionario no-lock where 
                          movto_ferias_calcul.dat_inic_ferias = habilit_ferias.dat_inic_ferias and
                          movto_ferias_calcul.cdn_tip_calc_ferias = 0 no-error.
               
               assign l-primeiro  = no
                      /*l-imp-per   = no */.
               if i-fc-codigo <> funcionario.cdn_funcionario then do:
                  if habilit_ferias.dat_inic_ferias >= tt-param.d-dt-inicio and
                     habilit_ferias.dat_inic_ferias <= tt-param.d-dt-final then do:
                     assign i-fc-codigo = funcionario.cdn_funcionario
                            l-primeiro  = yes
                            l-imp-per   = yes.
                  end.
               end. 
   
               assign c-tp-ferias = if habilit_ferias.idi_tip_ferias = 1
                                    then "Normal"
                                    else "Coletiva".
   
               assign i-matr-func = string(funcionario.cdn_funcionario, "zzzzzzz9") + c-hifem +
                                    string(funcionario.num_digito_verfdor_func).                                     
   
               if habilit_ferias.dat_inic_ferias >= tt-param.d-dt-inicio and
                  habilit_ferias.dat_inic_ferias <= tt-param.d-dt-final then do:
                  if l-primeiro then
                     display i-matr-func
                             funciona.nom_pessoa_fisic format "x(31)".
                  assign adto-13S = no.
                  if habilit_ferias.vli_perc_adiant_13o > 0 then adto-13S = yes.
                  if l-imp-per then do:
                     display habilit_ferias.dat_inic_period_aqst_ferias
                             habilit_ferias.dat_term_period_aqst_ferias
                             i-dias-dir
                             i-dias-conc.
                  end.           
   
                  display habilit_ferias.dat_inic_ferias
                          c-tp-ferias
                          habilit_ferias.qtd_dias_ferias_gozar
                          habilit_ferias.qtd_dias_licenc
                          habilit_ferias.qtd_dias_abono_ferias
                          i-dias-saldo
                          adto-13S
                          i-faltas-l502.
                  down.
               
                  assign l-primeiro = no
                         /*l-imp-per  = no*/
                         i-dias-saldo = 0.
               end.
           end.    
       end.
   end.
      
   assign l-imprime = yes.
   
   hide frame f-cab-estb.
   hide frame f-cab-dados.
   hide frame f-dados.        

   if tt-param.parametro = no then do:
      run pi-finalizar in v_han_acomp. 
      {include/i-rpclo.i}
      return "OK":U.
   end.

   page.       
   run pi_report_selec.  
   display tt-param.d-dt-inicio
           tt-param.d-dt-final  
           with frame f-selecao.

   run pi_report_param.   
      
   display c--programa2
           with frame f-param.

   run pi_report_impres.          

   {include/i-rpclo.i}
   run pi-finalizar in v_han_acomp.

return "OK":U.
