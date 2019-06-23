/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i YMFP0005RP 1.02.00.003 } /*** 010003 ***/
/*******************************************************************************
**
**       Programa: prghur/esp/ymfp0005rp.P
**
**       Data....: Agosto/2012
**
**       Autor...: Lennon Climaco Ferreira
**
**       Objetivo: Geracao de Liquidos para Bancos - Padrao FEBRABAN 240 posi‡äes
**
*******************************************************************************/
{include/i-rpvar.i}
/** WBD: BEGIN_NULL_CODE **/
{include/i-freeac.i}
/** WBD: END_NULL_CODE **/

{prghur/esp/ymfp0005tt.i}

def var h-acomp as handle no-undo.
run utp/ut-acomp.p persistent set h-acomp.  

/***************************** Definicao de Variaveis ***********************/
def temp-table tt-raw-digita
    field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.   

/**************************** Var Definitions ****************************/
def stream ger-liq.

def var v_cont                as int                               no-undo.
def var v_val_pagto           as dec                               no-undo.
def var c-hora                as char format "x(08)"               no-undo.
def var v_log_folha_educnal   as log                               no-undo.
def var i-sequencia           as int                               no-undo.
def var v_num_lote            as int                               no-undo.
def var v_seq_lote            as int                               no-undo.
def var v_tot_reg_arq         as int                               no-undo.

def var v_qtd_reg_lote        as int                               no-undo.
def var v_val_tot_pagto       as dec                               no-undo.

def var c-destino             as char format "x(15)"               no-undo.
def var des_layout            as char format "x(25)"               no-undo.
def var v_cdn_cta_corren      as char format "x(10)"               no-undo.

/**************************** Forms ****************************/
form
    "SELE€ÇO"                                                          colon 20          skip(1)
    tt-param.cdn_estab_ini         label "Estabelecimento"             colon 50 space(6)
    "|<  >|"                                                                    space(1) 
    tt-param.cdn_estab_fim      no-label                                                 skip
    tt-param.cdn_banco_ini         label "Banco"                       colon 50 space(6)
    "|<  >|"                                                                    space(1) 
    tt-param.cdn_banco_fim      no-label                                                 skip
    tt-param.dat_pagto_ini         label "Data Pagamento"              colon 50 space(1)
    "|<  >|"                                                                    space(1) 
    tt-param.dat_pagto_fim      no-label                                                 skip
    tt-param.dat_outros_pagtos     label "Data Outros Pagamentos"      colon 50
    tt-param.log_normal            label "Normal"                      colon 50
    tt-param.log_adiant_normal     label "Adiantamento Normal"         colon 50
    tt-param.log_13                label "13§ Sal rio"                 colon 50
    tt-param.log_adiant_13         label "Adiantamento 13§ Sal rio"    colon 50
    tt-param.log_ferias            label "F‚rias"                      colon 50
    tt-param.log_pensao            label "PensÆo"                         colon 50
    tt-param.log_rescisao          label "RescisÆo"                    colon 50          skip(2)
    "CLASSIFICA€ÇO"                                                    colon 20          skip(1)
    tt-param.desc-classifica       label "Classifica‡Æo"               colon 50          skip(2)
    "PAR¶METROS"                                                       colon 20          skip(1)
    tt-param.c-arquivo-pensao      label "Arquivo L¡quidos" format "x(60)" colon 50
    tt-param.cdn_estab_central     label "Estabelecimento Central"     colon 50
    tt-param.dat_lancto            label "Data Lan‡amento"             colon 50
    tt-param.num_mes_refer         label "Mˆs Referˆncia"              colon 50
    tt-param.num_ano_refer         label "Ano Referˆncia"              colon 50
    tt-param.cod_convenio          label "Convˆnio"                    colon 50
    tt-param.cdn_banco_emp         label "Banco Empresa"               colon 50
    tt-param.cdn_agenc_emp         label "Agˆncia Empresa"             colon 50 space(1)
    " - "                                                                       space(1)
    tt-param.num_dig_agenc      no-label                                                 skip
    tt-param.num_conta_emp         label "Conta Corrente Empresa"      colon 50 space(1)
    " - "                                                                       space(1)
    tt-param.num_dig_conta      no-label
    tt-param.cod_docto             label "Documento"                   colon 50 
    tt-param.log_credito_conta     label "Cr‚dito Conta"               colon 50 
    tt-param.log_cartao_salario    label "CartÆo Sal rio"              colon 50 
    des_layout                     label "Layout"                      colon 50
    tt-param.cod_dac               label "D¡gito - DAC"                colon 50          skip(2)
    "IMPRESSÇO"                                                        colon 20          skip(1)
    c-destino                      label "Destino"                     colon 50 space(1)     
    tt-param.arquivo            no-label                                                 skip
    tt-param.usuario               label "Usu rio"                     colon 50              
    with side-labels no-attr-space no-box stream-io page-top width 132 frame f-param.

{utp/ut-liter.i Sim/NÆo MFP R}
assign tt-param.log_normal:format in frame f-param = trim(return-value).
       tt-param.log_adiant_normal:format in frame f-param = trim(return-value).
       tt-param.log_13:format in frame f-param = trim(return-value).
       tt-param.log_adiant_13:format in frame f-param = trim(return-value).
       tt-param.log_ferias:format in frame f-param = trim(return-value).
       tt-param.log_pensao:format in frame f-param = trim(return-value).
       tt-param.log_rescisao:format in frame f-param = trim(return-value).

find empresa where empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-lock no-error.
find param_empres_rh where param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-lock no-error.

find param_folha_educnal no-lock where
     param_folha_educnal.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.
assign v_log_folha_educnal = if avail param_folha_educnal then yes else no.

{utp/ut-liter.i FOLHA_DE_PAGAMENTO MFP C}
assign c-sistema = return-value.   
run pi-inicializar in h-acomp (input return-value).

{utp/ut-liter.i Banco_Bradesco MFP C}

/**************************** Inicio do Programa ******************************/
assign c-versao       = "C.01"
       c-programa     = "BWFP6124"
       c-empresa      = empresa.razao-social
       c-titulo-relat = return-value.

run utp/ut-trfrrp.p (input frame f-param:handle).
{include/i-rpcab.i}
{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.

assign c-hora = string(time,"HH:MM:SS")
       i-sequencia = 1.

output stream ger-liq to value(tt-param.c-arquivo-pensao).

run pi-header.

run pi-dados.

run pi-trailler.

assign i-sequencia = i-sequencia + 1.

if tt-param.parametro then do:
   assign c-destino = {varinc/var00002.i 04 tt-param.destino}. 

   assign des_layout = "Febraban 240 Posi‡äes".

   display tt-param.cdn_estab_ini      
           tt-param.cdn_estab_fim      
           tt-param.cdn_banco_ini      
           tt-param.cdn_banco_fim      
           tt-param.dat_pagto_ini      
           tt-param.dat_pagto_fim      
           tt-param.dat_outros_pagtos  
           tt-param.log_normal         
           tt-param.log_adiant_normal  
           tt-param.log_13             
           tt-param.log_adiant_13      
           tt-param.log_ferias         
           tt-param.log_pensao            
           tt-param.log_rescisao       
           tt-param.desc-classifica    
           tt-param.c-arquivo-pensao   
           tt-param.cdn_estab_central  
           tt-param.dat_lancto         
           tt-param.num_mes_refer      
           tt-param.num_ano_refer      
           tt-param.cod_convenio       
           tt-param.cdn_banco_emp      
           tt-param.cdn_agenc_emp      
           tt-param.num_dig_agenc      
           tt-param.num_conta_emp      
           tt-param.num_dig_conta      
           tt-param.cod_docto          
           tt-param.log_credito_conta  
           tt-param.log_cartao_salario 
           des_layout
           tt-param.cod_dac            
           c-destino                   
           tt-param.arquivo            
           tt-param.usuario with frame f-param.
   down with frame f-param.
end.

output stream ger-liq close.
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

procedure pi-dados:

   for each bnfciar_palim no-lock where
            bnfciar_palim.cdn_empresa                    = tt-param.v_cdn_empres_usuar and
            bnfciar_palim.cdn_estab                     >= tt-param.cdn_estab_ini      and
            bnfciar_palim.cdn_estab                     <= tt-param.cdn_estab_fim      and
            bnfciar_palim.cdn_banco                     >= tt-param.cdn_banco_ini      and
            bnfciar_palim.cdn_banco                     <= tt-param.cdn_banco_fim      and
            bnfciar_palim.idi_forma_pagto_bnfciar_palim  = 1 /*and
          ((int(substr(bnfciar_palim.cod_livre_1,1,2))   = 1 and
            tt-param.log_credito_conta)*/ or
           (int(substr(bnfciar_palim.cod_livre_1,1,2))   = 4 and
            tt-param.log_cartao),
       first funcionario no-lock of bnfciar_palim,
       first rh_estab no-lock of funcionario,
       first rh_pessoa_jurid no-lock of rh_estab
       break BY bnfciar_palim.cdn_empresa
             by bnfciar_palim.cdn_estab
             by if tt-param.classifica = 1 then
                      string(bnfciar_palim.cdn_funcionario,"99999999")
                   else if tt-param.classifica = 2 then
                           bnfciar_palim.nom_pessoa_fisic
                        else
                           string(bnfciar_palim.cdn_cta_corren,"999999999"):

      
      assign v_cont  = v_cont + 1
             v_val_pagto = 0.
   
      run pi-acompanhar in h-acomp (input string(v_cont)).

      for each histor_pagto_palim no-lock where
               histor_pagto_palim.cdn_empresa                = bnfciar_palim.cdn_empresa       and
               histor_pagto_palim.cdn_estab                  = bnfciar_palim.cdn_estab         and
               histor_pagto_palim.cdn_funcionario            = bnfciar_palim.cdn_funcionario   and
               histor_pagto_palim.cdn_bnfciar_palim          = bnfciar_palim.cdn_bnfciar_palim and
              (tt-param.log_normal    or tt-param.log_adiant_normal or tt-param.log_13         or 
               tt-param.log_adiant_13 or tt-param.log_pensao           or tt-param.log_ferias     or 
               tt-param.log_rescisao                                                           and
               histor_pagto_palim.num_mes_refer_histor_palim = tt-param.num_mes_refer          and
               histor_pagto_palim.num_ano_refer_histor_palim = tt-param.num_ano_refer)         and
             ((tt-param.log_rescisao                         = yes  and
               histor_pagto_palim.idi_tip_calc_palim         = 4    and
               histor_pagto_palim.dat_palim_normal          >= tt-param.dat_pagto_ini       and
               histor_pagto_palim.dat_palim_normal          <= tt-param.dat_pagto_fim)      or
              (tt-param.log_pensao                              = yes  and
               histor_pagto_palim.dat_palim_ppr              = tt-param.dat_outros_pagtos)  or
             ((tt-param.log_normal                           = yes  or
               tt-param.log_adiant_normal                    = yes) and
              (histor_pagto_palim.dat_palim_normal           = tt-param.dat_outros_pagtos   or 
               histor_pagto_palim.dat_palim_ppr              = tt-param.dat_outros_pagtos)) or
             ((tt-param.log_13                               = yes  or  
               tt-param.log_adiant_13                        = yes) and
               histor_pagto_palim.dat_palim_13               = tt-param.dat_outros_pagtos)  or
              (tt-param.log_ferias                           = yes  and
               histor_pagto_palim.idi_tip_calc_palim         = 1    and
               histor_pagto_palim.dat_palim_ferias          >= tt-param.dat_pagto_ini       and
               histor_pagto_palim.dat_palim_ferias          <= tt-param.dat_pagto_fim))
          break by histor_pagto_palim.cdn_empresa
                by histor_pagto_palim.cdn_estab
                by histor_pagto_palim.cdn_bnfciar_palim:


         if tt-param.log_rescisao = yes and histor_pagto_palim.idi_tip_calc_palim = 4 then
            if histor_pagto_palim.dat_palim_normal >= tt-param.dat_pagto_ini and 
               histor_pagto_palim.dat_palim_normal <= tt-param.dat_pagto_fim then
               assign v_val_pagto = v_val_pagto + (histor_pagto_palim.val_palim_normal * 100) + (histor_pagto_palim.val_palim_13o    * 100) + 
                                    (histor_pagto_palim.val_palim_ferias * 100).
   
         if (tt-param.log_normal        = yes and histor_pagto_palim.idi_tip_calc_palim = 5) or 
            (tt-param.log_adiant_normal = yes and histor_pagto_palim.idi_tip_calc_palim = 0) then
            if histor_pagto_palim.dat_palim_normal = tt-param.dat_outros_pagtos then
               assign v_val_pagto = v_val_pagto + (histor_pagto_palim.val_palim_normal * 100).
            
            /*
         /*-- Considera PLR para calculo Normal e Adiantamento ------*/
         if tt-param.log_pensao = yes and histor_pagto_palim.dat_palim_ppr = tt-param.dat_outros_pagtos and histor_pagto_palim.val_palim_ppr <> 0 then 
            assign v_val_pagto = v_val_pagto + (histor_pagto_palim.val_palim_ppr * 100).*/
   
         if (tt-param.log_13        = yes and (histor_pagto_palim.idi_tip_calc_palim = 3 OR histor_pagto_palim.idi_tip_calc_palim = 5))  or 
            (tt-param.log_adiant_13 = yes and  histor_pagto_palim.idi_tip_calc_palim = 2) then
            if histor_pagto_palim.dat_palim_13o    = tt-param.dat_outros_pagtos then
               assign v_val_pagto = v_val_pagto + (histor_pagto_palim.val_palim_13o * 100).
   
         if tt-param.log_ferias = yes and histor_pagto_palim.idi_tip_calc_palim = 1 then 
            if histor_pagto_palim.dat_palim_ferias  >= tt-param.dat_pagto_ini and
               histor_pagto_palim.dat_palim_ferias  <= tt-param.dat_pagto_fim then
               assign v_val_pagto = v_val_pagto + (histor_pagto_palim.val_palim_ferias * 100) + (histor_pagto_palim.val_palim_13o * 100).
      end.      
      if first-of(bnfciar_palim.cdn_empresa) then
            run pi-header-240.

      if v_val_pagto > 0 then do:

         /*if length(string(bnfciar_palim.cdn_cta_corren)) > 7 then
             assign v_cdn_cta_corren = "0" + string(bnfciar_palim.cdn_cta_corren, "999999999").
         else
             assign v_cdn_cta_corren = string(bnfciar_palim.cdn_cta_corren, "9999999") + "   ".*/

         assign v_cdn_cta_corren = "0" + string(bnfciar_palim.cdn_cta_corren, "999999999").
         
         assign v_qtd_reg_lote = v_qtd_reg_lote + 1
                v_seq_lote = v_seq_lote + 1.
   
   
         put stream ger-liq
             "237"                        format "999"
             v_num_lote                   format "9999"
             "3"                          format "9"
             v_seq_lote                   format "99999"
             "A"                          format "x(01)"
             "0"                          format "9"
             "00"                         format "99"
             "018"                        format "999"
             "356"                        format "999"
             bnfciar_palim.cdn_agenc_bcia format "99999".
   
         find first rh_agenc_bcia no-lock where
                    rh_agenc_bcia.cdn_banco      = bnfciar_palim.cdn_banco and
                    rh_agenc_bcia.cdn_agenc_bcia = bnfciar_palim.cdn_agenc_bcia no-error.
         if avail rh_agenc_bcia then
            put stream ger-liq
                rh_agenc_bcia.cod_digito_verfdor_agenc_bcia format "x(01)".
         else
            put stream ger-liq
                " ".
         
         put stream ger-liq
             bnfciar_palim.cdn_cta_corren                         format "999999999999"
             bnfciar_palim.cod_digito_cta_corren                  format "x(01)"
             substring(bnfciar_palim.cod_digito_cta_corren,02,01) format "x(01)"
             fn-free-accent(bnfciar_palim.nom_pessoa_fisic)       format "x(30)"
             "00000000000000000000"
             tt-param.dat_lancto                                  format "99999999"
             "BRL"                                                format "x(03)"
             "000000000000000"                                    format "999999999999999"
             v_val_pagto                                          format "999999999999999"
             fill(" ",20)                                         format "x(20)"
             tt-param.dat_lancto                                  format "99999999"
             v_val_pagto                                          format "999999999999999"
             fill(" ",40)                                         format "x(40)"
             fill(" ",02)                                         format "x(02)"
             fill(" ",10)                                         format "x(10)"
             "0"                                                  format "9"
             fill(" ",10)                                         format "x(10)" skip.
   
         assign v_tot_reg_arq   = v_tot_reg_arq + 1
                i-sequencia     = i-sequencia + 1
                v_val_tot_pagto = v_val_tot_pagto + v_val_pagto.
         
      end.
      if last-of(bnfciar_palim.cdn_empresa) then
            run pi-trailler-240.
      
   
   end.   
end procedure.

procedure pi-header:
   
   find first rh_estab no-lock where
              rh_estab.cdn_empresa = param_empres_rh.cdn_empresa and
              rh_estab.cdn_estab   = tt-param.cdn_estab_central no-error.
   if avail rh_estab then do:

      find first rh_bco no-lock where
                 rh_bco.cdn_banco = tt-param.cdn_banco_emp no-error.
      if avail rh_bco then do:
         find first rh_agenc_bcia no-lock of rh_bco where
                    rh_agenc_bcia.cdn_agenc_bcia = tt-param.cdn_agenc_emp no-error.
         
         if avail rh_agenc_bcia then
            put stream ger-liq
                "237"                                       format "999"
                "0000"                                      format "0000"
                "0"                                         format "9"
                fill(" ",09)                                format "x(09)"
                "2"                                         format "9"
                rh_estab.cod_id_feder                       format "99999999999999"
                fn-free-accent(tt-param.cod_convenio)       format "x(20)"
                rh_agenc_bcia.cdn_agenc_bcia                format "99999"
                rh_agenc_bcia.cod_digito_verfdor_agenc_bcia format "x(01)"
                tt-param.num_conta_emp                      format "999999999999"
                tt-param.num_dig_conta                      format "x(01)"
                fill(" ",01)                                format "x(01)"
                fn-free-accent(empresa.razao-social)        format "x(30)"
                "BRADESCO"                                  format "x(30)"
                fill(" ",10)                                format "x(10)"
                "1"                                         format "9"
                today                                       format "99999999"
                substr(c-hora,01,02)                        format "99"
                substr(c-hora,04,02)                        format "99"
                substr(c-hora,07,02)                        format "99"
                i-sequencia                                 format "999999"
                "080"                                       format "999"
                "01600"                                     format "99999"
                fill(" ",69)                                format "x(69)" skip.
         
      end.
   end.

   assign i-sequencia = i-sequencia + 1.

end procedure.

procedure pi-trailler:
   
   put stream ger-liq
       "237"         format "999"
       "9999"        format "9999"
       "9"           format "9"
       fill(" ",09)  format "x(09)"
       v_num_lote    format "999999"
       v_tot_reg_arq format "999999"
       "000000"      format "999999"
       "00000"       format "99999"
       fill(" ",205) format "x(200)" skip.

   assign i-sequencia = i-sequencia + 1.
       
end procedure.

procedure pi-header-240:

   find first rh_bco no-lock where
              rh_bco.cdn_banco = tt-param.cdn_banco_emp no-error.
   if avail rh_bco then do:
      find first rh_agenc_bcia no-lock of rh_bco where
                 rh_agenc_bcia.cdn_agenc_bcia = tt-param.cdn_agenc_emp no-error.
      if avail rh_agenc_bcia then do:

         assign v_num_lote = v_num_lote + 1
                v_qtd_reg_lote = 0
                v_seq_lote = 0
                v_val_tot_pagto = 0.
         
         put stream ger-liq
             "237"                                               format "999"
             v_num_lote                                          format "9999"
             "1"                                                 format "9"
             "C"                                                 format "x(01)"
             "30"                                                format "99"
             "25"                                                format "99" /*forma de lancto*/
             "040"                                               format "999"
             " "                                                 format "x(01)"
             "2"                                                 format "9"
             rh_pessoa_jurid.cod_id_feder                        format "99999999999999"
             tt-param.cod_convenio                               format "x(20)"
             rh_agenc_bcia.cdn_agenc_bcia                        format "99999"
             rh_agenc_bcia.cod_digito_verfdor_agenc_bcia         format "x(01)"
             tt-param.num_conta_emp                              format "999999999999"
             tt-param.num_dig_conta                              format "x(01)"
             fill(" ",01)                                        format "x(01)"
             fn-free-accent(empresa.razao-social)                format "x(30)"
             fill(" ",40)                                        format "x(40)"
             fn-free-accent(rh_pessoa_jurid.nom_ender)           format "x(30)"
             rh_pessoa_jurid.num_ender_rh                        format "99999"
             fn-free-accent(rh_pessoa_jurid.cod_compl_ender_rh)  format "x(15)"
             fn-free-accent(rh_pessoa_jurid.nom_cidad_rh)        format "x(20)"
             rh_pessoa_jurid.cod_cep_rh                          format "x(08)"
             fn-free-accent(rh_pessoa_jurid.cod_unid_federac_rh) format "x(02)"
             fill(" ",18)                                        format "x(18)" skip.
         
      end.
   end.

   assign i-sequencia = i-sequencia + 1.

end procedure.

procedure pi-trailler-240:

   put stream ger-liq
       "237"                 format "999"
       v_num_lote            format "9999"
       "5"                   format "9"
       fill(" ",09)          format "x(09)"
       v_qtd_reg_lote        format "999999"
       v_val_tot_pagto       format "999999999999999999"
       "000000000000000000"  format "999999999999999999"
       fill(" ",181)         format "x(181)" skip.

   assign i-sequencia = i-sequencia + 1.

end procedure.
