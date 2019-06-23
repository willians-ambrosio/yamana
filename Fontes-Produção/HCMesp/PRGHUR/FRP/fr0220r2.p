/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i FR0220R1 1.02.05.021}  /*** 010521 ***/
/*******************************************************************************
**
**        Programa: prghur/frp/FR0220-1.P
**
**        Data....: Dezembro/2004
**
**        Autor...: Raimundo C. Soares
**
**        Objetivo: Emissao do Recibo de Ferias.
**
*******************************************************************************/
DEFINE  VARIABLE  c-mes-ano AS CHAR FORMAT "x(15)". 
define  variable d-tot-ident-venc             as decimal   format ">>>,>>>,>>9.99"             no-undo.
define  variable d-tot-ident-desc             as decimal   format ">>>,>>>,>>9.99"             no-undo.
define  variable d-tot-ident-outros           as decimal   format ">>>,>>>,>>9.99"             no-undo.

/*define shared temp-table tt-param
    field v_cdn_empres_usuar      like param_empres_rh.cdn_empresa
    field v_cod_unid_lotac_ini    like unid_lotac.cod_unid_lotac
    field v_cod_unid_lotac_fim    like unid_lotac.cod_unid_lotac
    field i-es-ini                like rh_estab.cdn_estab
    field i-es-fim                like rh_estab.cdn_estab
    field i-fc-ini                like funcionario.cdn_funcionario
    field i-fc-fim                like funcionario.cdn_funcionario
    field i-contr-ini             like funcionario.cdn_tip_contrat_func
    field i-contr-fim             like funcionario.cdn_tip_contrat_func
    field v_num_tip_aces_usuar      as integer format "9" 
    field v_cod_grp_usuar           as char
    field v_num_opcao               as int  format "9"
    field v_des_opcao               as char format "x(10)"
    field v_dat_valid               as date format "99/99/9999"
    field v_log_expande_estrut      as log
    field v_num_salta_pg            as integer
    field v_num_quebra              as integer
    field v_num_faixa               as integer
    field destino                   as integer
    field arquivo                   as char
    field usuario                   as char
    field data-exec                 as date format "99/99/9999"
    field hora-exec                 as integer
    field classifica                as integer
    field desc-classifica           as char format "x(40)"
    field c-cc-ini                like rh_ccusto.cod_rh_ccusto
    field c-cc-fim                like rh_ccusto.cod_rh_ccusto
    field d-dt-ini                  as date format "99/99/9999"
    field d-dt-fim                  as date format "99/99/9999"
    field i-tp-calc                 as integer format "9"
    field i-dias-nao-cons           as integer format "99"
    field l-imp-reci                as logical
    field i-tp-form                 as integer
    field i-vias                    as integer format "99"
    field rowid-frctrcal            as rowid
    field rs_nome                   as integer
    field v_emis_colet              as log
    field i-tp-ferias               as integer format "9"
    field v_log_situacao            as log
    field log_agrupa                as log
    field log_fr0080                as log
    field num_dec_sal               as int format "9".

define shared temp-table tt-digita
    field v_cdn_empres_usuar      	  like param_empres_rh.cdn_empresa
    field v_cdn_estab       	  like rh_estab.cdn_estab
    field v_cdn_funcionario 	  like funcionario.cdn_funcionario
    field v_nom_pessoa      	  like funcionario.nom_pessoa_fisic
    field cdn_plano_lotac   	  like plano_lotac.cdn_plano_lotac
    field v_cod_unid_lotac  	  like unid_lotac.cod_unid_lotac
    index id is primary unique
          v_cdn_empres_usuar
          v_cdn_estab
          v_cdn_funcionario.*/

{prghur/frp/fr0220tt.i SHARED}

{include/i_dbvers.i}
find first tt-param.

define     shared buffer bfrctrcal                 for habilit_ferias.
define            buffer bhabilit                  for habilit_ferias.
define     shared buffer bfunciona                 for funcionario.
define     shared variable v_log_folha_educnal     as log                      initial no           no-undo.
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
initial &IF "{&ems_dbtype}":U = "MSS":U &THEN "01/01/1800":U &ELSE "01/01/0001":U &ENDIF                                                                                no-undo.
define            variable d-dt-fim-fer            as date      format "99/99/9999" 
initial "12/31/9999"                                                                                no-undo.
define            variable i-inx                   as integer                                       no-undo.
define            variable c-titulo                as character format "x(40)"                      no-undo.
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

def var v_log_trad  as log initial no no-undo.

define temp-table tt-recibo
    field ev-codigo   like event_fp.cdn_event_fp
    field descricao   like event_fp.des_event_fp
    field ident       like event_fp.idi_ident_efp
    field base          as decimal                      format "zzz,zzz,zz9.99"
    field valor         as decimal                      format "zzz,zzz,zz9.99"
    field inc-liquido like event_fp.idi_tip_inciden_liq format "9"
    field c-inc-liq     as char                         format "x(01)".


form
   /*        1         2         3         4         5         6         7         8*/
   /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
        
    "conforme demons trativo, relativos ao per¡odo aquisitivo de f‚rias  acima  "  at  01 SKIP
    "especificado. Pela clareza, firmo o present e dando plena e geral quita‡Æo "  at  01 skip
    with stream-io no-labels no-attr-space no-box width 80 frame f-recibo-7.

IF v_log_trad  = NO THEN DO:
   FORM
   "---"
   with stream-io no-labels no-attr-space no-box width 80 frame f-teste-trad.

   run utp/ut-trfrrp.p (input frame f-teste-trad:handle).       
   ASSIGN v_log_trad = YES.
END.

assign v_val_base_inss = 0.

for each bfrctrcal exclusive-lock where
         bfrctrcal.cdn_empresa      = bfunciona.cdn_empresa     and
         bfrctrcal.cdn_estab        = bfunciona.cdn_estab       and
         bfrctrcal.cdn_funcionario  = bfunciona.cdn_funcionario and
         bfrctrcal.dat_inic_ferias >= d-dt-ini                  and
         bfrctrcal.dat_inic_ferias <= d-dt-fim:
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
              movto_ferias_calcul.cdn_tip_calc_ferias         = i-tp-calc                 and
              movto_ferias_calcul.log_calc_efetd_movto_ferias = yes  no-error.
   if duas-ferias then do:
      assign movto_ferias_calcul.log_recibo_impresso = yes.
      next.
   end.
   if not available movto_ferias_calcul then
      next.
   if movto_ferias_calcul.log_recibo_impresso = yes
   and l-imp-reci = no then
      next.
   assign d-liq-pagar = 0.

   if tt-param.log_agrupa then do:
      find first bhabilit exclusive-lock where
                 bhabilit.cdn_empresa                   = bfunciona.cdn_empresa                       and
                 bhabilit.cdn_estab                     = bfunciona.cdn_estab                         and
                 bhabilit.cdn_funcionario               = bfunciona.cdn_funcionario                   and
               ((bhabilit.dat_inic_ferias              >= d-dt-ini                                    and
                 bhabilit.dat_inic_ferias              <= d-dt-fim                                    and 
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
                    movto_ferias_calcul.cdn_tip_calc_ferias         = i-tp-calc                and
                    movto_ferias_calcul.log_calc_efetd_movto_ferias = yes  no-error.
         if not available movto_ferias_calcul then
            assign duas-ferias = no.
         if movto_ferias_calcul.log_recibo_impresso = yes
         and l-imp-reci = no then
            assign duas-ferias = no.
         if duas-ferias then do:
/*             for each reg_det_ferias of movto_ferias_calcul no-lock: */
/* ** "reg_det_ferias of movto_ferias_calcul".  Index fields of table 1 must be fields in table 2. (230)
   **  Could not understand line 250. (196) */

            for each reg_det_ferias no-lock
               where reg_det_ferias.cdn_empresa         = movto_ferias_calcul.cdn_empresa
                 and reg_det_ferias.cdn_estab           = movto_ferias_calcul.cdn_estab
                 and reg_det_ferias.cdn_funcionario     = movto_ferias_calcul.cdn_funcionario
                 and reg_det_ferias.dat_inic_ferias     = movto_ferias_calcul.dat_inic_ferias
                 and reg_det_ferias.cdn_tip_calc_ferias = movto_ferias_calcul.cdn_tip_calc_ferias:
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
             c-titulo     = if bfrctrcal.idi_tip_ferias = 1
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

                           CASE MONTH(bfrctrcal.dat_pagto_ferias):
                                  WHEN 1  THEN  ASSIGN c-mes-ano = "Janeiro"  + "/" +  STRING(YEAR(bfrctrcal.dat_pagto_ferias), "9999").
                                  WHEN 2  THEN  ASSIGN c-mes-ano = "Feveriro" + "/" +  STRING(YEAR(bfrctrcal.dat_pagto_ferias), "9999").
                                  WHEN 3  THEN  ASSIGN c-mes-ano = "Mar‡o"    + "/" +  STRING(YEAR(bfrctrcal.dat_pagto_ferias), "9999").
                                  WHEN 4  THEN  ASSIGN c-mes-ano = "Abril"    + "/" +  STRING(YEAR(bfrctrcal.dat_pagto_ferias), "9999").
                                  WHEN 5  THEN  ASSIGN c-mes-ano = "Maio"     + "/" +  STRING(YEAR(bfrctrcal.dat_pagto_ferias), "9999").
                                  WHEN 6  THEN  ASSIGN c-mes-ano = "Junho"    + "/" +  STRING(YEAR(bfrctrcal.dat_pagto_ferias), "9999").
                                  WHEN 7  THEN  ASSIGN c-mes-ano = "Julho"    + "/" +  STRING(YEAR(bfrctrcal.dat_pagto_ferias), "9999").
                                  WHEN 8  THEN  ASSIGN c-mes-ano = "Agosto"   + "/" +  STRING(YEAR(bfrctrcal.dat_pagto_ferias), "9999").
                                  WHEN 9  THEN  ASSIGN c-mes-ano = "Setembro" + "/" +  STRING(YEAR(bfrctrcal.dat_pagto_ferias), "9999").
                                  WHEN 10 THEN  ASSIGN c-mes-ano = "Outubro"  + "/" +  STRING(YEAR(bfrctrcal.dat_pagto_ferias), "9999").
                                  WHEN 11 THEN  ASSIGN c-mes-ano = "Novembro" + "/" +  STRING(YEAR(bfrctrcal.dat_pagto_ferias), "9999").
                                  WHEN 12 THEN  ASSIGN c-mes-ano = "Dezembro" + "/" +  STRING(YEAR(bfrctrcal.dat_pagto_ferias), "9999").
                              END CASE.


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

      
/***************************************************************************************************************************************/

      assign v_val_base_inss = 0.
/*       for each reg_det_ferias of movto_ferias_calcul no-lock: */
/* ** "reg_det_ferias of movto_ferias_calcul".  Index fields of table 1 must be fields in table 2. (230)
   **  Could not understand line 250. (196) */

      for each reg_det_ferias no-lock
         where reg_det_ferias.cdn_empresa         = movto_ferias_calcul.cdn_empresa
           and reg_det_ferias.cdn_estab           = movto_ferias_calcul.cdn_estab
           and reg_det_ferias.cdn_funcionario     = movto_ferias_calcul.cdn_funcionario
           and reg_det_ferias.dat_inic_ferias     = movto_ferias_calcul.dat_inic_ferias
           and reg_det_ferias.cdn_tip_calc_ferias = movto_ferias_calcul.cdn_tip_calc_ferias:
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
                assign tt-recibo.base        = if duas-ferias then if event_fp.cdn_idx_efp_funcao_espcif = 18 
                                                                   then tt-recibo.base + v_val_base_inss
                                                                   else tt-recibo.base
                                               else if reg_det_ferias.log_livre_1
                                                    then reg_det_ferias.val_base_mes_inic_ferias[i-inx] +
                                                         reg_det_ferias.val_base_seguinte_ferias[i-inx] +
                                                         reg_det_ferias.val_base_calc_ferias[i-inx]
                                                    else reg_det_ferias.val_base_calc_ferias[i-inx]

                       tt-recibo.valor       = tt-recibo.valor + if reg_det_ferias.log_livre_1 
                                                                 then reg_det_ferias.val_efp_mes_inic_ferias[i-inx] +
                                                                      reg_det_ferias.val_efp_seguinte_ferias[i-inx] + 
                                                                      reg_det_ferias.val_tot_efp_ferias[i-inx]
                                                                 else reg_det_ferias.val_tot_efp_ferias[i-inx].
             end.
          end.
      end.
/****************************************************************************************************************************************/


 /** Totalizando valores  Raimundo 1**/

      for each tt-recibo:
          if tt-recibo.ident = 1 THEN ASSIGN d-tot-ident-venc   = d-tot-ident-venc + tt-recibo.valor.
          if tt-recibo.ident = 2 then ASSIGN d-tot-ident-desc   = d-tot-ident-desc + tt-recibo.valor.
          if tt-recibo.ident = 3 then ASSIGN d-tot-ident-outros = d-tot-ident-outros + tt-recibo.valor.
      END.


/****************************************************************************************************************************/


       do:   /** if tt-param.num_dec_sal = 2 then **/

        /* Raimundo */

         PUT  substring(bfunciona.nom_pessoa_fisic,1,30)  AT 01
              i-matr-func                                 AT 32
              bfunciona.cod_unid_lotac                    AT 45
              c-mes-ano                                   AT 60
              SKIP (1)
              cargo.des_cargo                             AT 01
              rh_ccusto.cod_rh_ccusto                     AT 40
              movto_ferias_calcul.val_salario_atual       AT 55
              SKIP (1).

        /*  unid_lotac.des_unid_lotac     */
        /*  rh_ccusto.des_rh_ccusto      */
        /*     with frame f-recibo-1.    */
        /*  down with frame f-recibo-1.  */
      end.
/*************************************************************************************************************************************/      

/*************************************************************************************************************************************/      

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

         /*raimundo*/
         PUT d-dt-inic-per-1  AT 01
             " a " d-dt-term-per-1 " e "     /*periodo aquisitivo 1*/  
             d-dt-inic-per-2  " a " d-dt-term-per-2    .  /*periodo aquisitivo 2*/

         IF total_dias_gozar  > 0 THEN
         PUT  d-dt-ini-fer-tot  AT 30 /*Perio de gozo ini*/
              " a "
              d-dt-fim-fer-tot  /*Perio de gozo fim*/
              SKIP (1). 

                 /**
                 total_dias_gozar when total_dias_gozar > 0  /*Dia Gozados*/
                 **/
 
                 /* c-lit-dd-1p1                                  */
                 /* dias_gozar_per_1                              */
                 /* c-lit-dd-1p2                                  */
                 /* dias_gozar_per_2                              */
                 /* c-lit-a1-tot     when total_dias_gozar > 0    */
                 /* c-lit-dd-tot     when total_dias_gozar > 0    */

         /*****
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
         *******/
      end.
      else do:

         ASSIGN  total_dias_gozar = bfrctrcal.qtd_dias_ferias_gozar.

         /* Raimundo*/

         /*Periodo aquisitivo*/
         PUT bfrctrcal.dat_inic_period_aqst_ferias AT 01
             " a "
             bfrctrcal.dat_term_period_aqst_ferias .
         
         IF bfrctrcal.qtd_dias_ferias_gozar  > 0 THEN
         /*Periodo de Gozo */
         PUT d-dt-ini-fer    AT 30
              " a "
             d-dt-fim-fer  
             SKIP (1).

         /**
         total_dias_gozar       when bfrctrcal.qtd_dias_ferias_gozar  > 0  
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
         ****/                   
      end.

/******************************************************************************************************************************************/

      if duas-ferias then do:
         assign v_dat_ini_abono  = ?
                v_dat_term_abono = ?.

         if bfrctrcal.qtd_dias_abono_ferias > 0 and 

            bhabilit.qtd_dias_abono_ferias > 0 then do:

             v_qtd_dias_abono = bfrctrcal.qtd_dias_abono_ferias .

             /**
            display v_qtd_dias_abono 
/*                     bhabilit.qtd_dias_abono_ferias */

                    with frame f-recibo-8.
            down with frame f-recibo-8.
            **/
         END.
         else do:
            if bfrctrcal.qtd_dias_abono_ferias > 0 then
               assign v_qtd_dias_abono = bfrctrcal.qtd_dias_abono_ferias.

            if bhabilit.qtd_dias_abono_ferias > 0 then
               ASSIGN v_qtd_dias_abono = bhabilit.qtd_dias_abono_ferias.

            /**
            display  v_qtd_dias_abono when v_qtd_dias_abono > 0  @ bfrctrcal.qtd_dias_abono_ferias 
                    
                    with frame f-recibo-4.
            down with frame f-recibo-4.
            **/
         end.
      end.

      else do:
         v_qtd_dias_abono = bfrctrcal.qtd_dias_abono_ferias.

         /**
         display v_qtd_dias_abono   when bfrctrcal.qtd_dias_abono_ferias   > 0
                 with frame f-recibo-4.
         down with frame f-recibo-4.
         **/
      end.  



/***************************************************************************************************************************************/
      /*raimndo*/
     PUT  total_dias_gozar   AT 01
          v_qtd_dias_abono   AT 10
          d-tot-ident-venc   AT 20
          d-tot-ident-desc   AT 35
          d-liq-pagar        AT 55
          SKIP (1).

      for each tt-recibo
               break by tt-recibo.ident
                     by tt-recibo.ev-codigo:
          display tt-recibo.ev-codigo
                  /** Frequencia **/
                  tt-recibo.descricao
                  /*tt-recibo.base    when tt-recibo.base  > 0**/
                  tt-recibo.valor
                  tt-recibo.c-inc-liq
               with frame f-recibo-5.
          down with frame f-recibo-5.

          if not duas-ferias then
             delete tt-recibo.
      end.

      /**
      display d-liq-pagar     with frame f-recibo-6.
      down with frame f-recibo-6.
      

      if line-counter > 0 then do while line-counter < 44:
         put  "|" at 01
              "|" at 80 skip.
      end.
      
      assign d-valor     = d-liq-pagar.
             d-vlr-total = d-liq-pagar.
      **/

      run prghur/fpp/fp9900rp.p.
      assign c-extenso-1 = substring(c-extenso,1,55)
             c-extenso-2 = substring(c-extenso,56,131)
             c-extenso-3 = substring(c-extenso,132,189).

      /**
      display c-estab
              d-vlr-total
              c-extenso-1
              c-extenso-2
              c-extenso-3
              c-local
              c-funciona     with frame f-recibo-7.
      down with frame f-recibo-7.
      ******/
      assign movto_ferias_calcul.log_recibo_impresso = yes.
   end.
end.

for each tt-recibo:
    delete tt-recibo.
end.
return "ok".
