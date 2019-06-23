/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i YMFR0003RP 1.02.00.003 } /*** 010003 ***/
/*******************************************************************************
**
**       Programa: prghur/esp/ymfr0003rp.p
**
**       Data....: abril/2008.
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Liquido Banco Nossa Caixa Meio Magnetico - Rescis∆o
**
*******************************************************************************/

{prghur/esp/ymfr0003tt.i}
{include/i-rpvar.i}

/*************************** Definicao de Parametros ***************************/
def temp-table tt-raw-digita 
    field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.
{prghur/fpp/fp9200.i11}

/*************************** Definicao de Variaveis ***************************/

def var l-primeiro        as log                initial no     no-undo.
def var l-imprime         as log                initial no     no-undo.
def var i-inx             as int                               no-undo.
def var c-destino         as char                              no-undo.
def var l-mensal-2        as log                               no-undo.
def var l-horista-2       as log                               no-undo.
def var l-semanal-2       as log                               no-undo.
def var l-quinzenal-2     as log                               no-undo.
def var l-diarista-2      as log                               no-undo.
def var l-tarefa-2        as log                               no-undo.
def var i-numseq          as int  format 999999 initial 1      no-undo.
def var v_cod_dv          as char format "x"                   no-undo.
def var d-valor-lanc      as dec  format "9999999999999.99"    no-undo.
def var c-valor-lanc      as char format "x(15)"               no-undo.
def var d-valor-lote      as dec  format "9999999999999999.99" no-undo.
def var c-valor-lote      as char format "x(18)"               no-undo.
def var v_num_qtd_reg     as int                               no-undo.
def var h-acomp           as handle                            no-undo.
def var c-hora-grav       as char format "x(06)"               no-undo.
def var c-data-grav       as char format "x(08)"               no-undo.
def var v_num_seq_lot     as int                               no-undo.
def var v_dat_lancto      as char format "x(08)"               no-undo.
def var v_num_cont        as int                               no-undo.
def var v_cod_dv_agconta  as char format "x"                   no-undo.
def var v_num_qtd_reg_arq as int                               no-undo.
def var v_num_index       as int                               no-undo.
def var v_num_lote        as int                               no-undo.
def var v_cdn_forma_pagto as int                               no-undo.
def var v_num_conta       as dec  format "999999999999"        no-undo.
def var d-valor-moeda     as dec  format "9999999999.99999"    no-undo.
def var c-valor-moeda     as char format "x(15)"               no-undo.
def var d-tot-moeda-lote  as dec  format "9999999999999.99999" no-undo.
def var c-tot-moeda-lote  as char format "x(18)"               no-undo.
def var v_doc             as char format "x(3)"                no-undo.
def var v_tipo_pagto      as int  format "99"                  no-undo.

def stream ger-liq.

/***************************** Definicao de Frames ****************************/
form
    skip(3)
    "SELEÄ«O"                                                                 colon 20 skip(1)
    tt-param.i-es-ini        label "Estabelecimento" format "zz9"             colon 50 space(8)
    "|<    >|"                                                                         space(1)
    tt-param.i-es-fim        no-label     
    tt-param.i-bco-liq-ini   label "Banco"           format "zz9"             colon 50 space(8)
    "|<    >|"                                                                         space(1)    
    tt-param.i-bco-liq-fim   no-label                format "zz9"          
    tt-param.d-dt-ini        label "Data Pagamento"  format "99/99/9999"      colon 50 space(1)
    "|<    >|"                                                                         space(1)   
    tt-param.d-dt-fim        no-label                format "99/99/9999"               skip
    l-mensal-2               label "Mensal"                                   colon 50  
    l-horista-2              label "Horista"                                  colon 50  
    l-semanal-2              label "Semanal"                                  colon 50  
    l-quinzenal-2            label "Quinzenal"                                colon 50  
    l-tarefa-2               label "Tarefa"                                   colon 50  
    l-diarista-2             label "Diarista"                                 colon 50 skip(2)
    "PAR∂METROS"                                                              colon 20
    skip(1)
    tt-param.c-arq-lqd       label "Arquivo L°quidos"                         colon 50
    tt-param.i-tp-res        label "Tipo Rescis∆o"                            colon 50
    tt-param.dat_lancto      label "Data Lanáamento"                          colon 50 
    tt-param.cdn_banco       label "Banco Empresa"                            colon 50
    tt-param.cdn_agencia     label "Agància Empresa"                          colon 50 space(0)
    "-"                                                                                space(0)
    tt-param.cod_dv_agencia  no-label                                       
    tt-param.num_conta       label "Conta Corrente"                           colon 50 space(0)
    "-"                                                                                space(0)
    tt-param.cod_dv_conta    no-label                                       
    tt-param.cdn_estab_centr label "Estabelecimento Central"                  colon 50
    tt-param.num_convenio    label "Convànio"                                 colon 50
    tt-param.cod_docto       label "Documento"                                colon 50 
    tt-param.log_cred_conta  label "CrÇdito Conta Corrente"  format "Sim/N∆o" colon 50
    tt-param.log_doc         label "Documento CrÇdito"       format "Sim/N∆o" colon 50
    tt-param.log_cheque      label "Cheque Pagto/Admin"      format "Sim/N∆o" colon 50
    tt-param.log_cartao      label "Cart∆o Sal†rio"          format "Sim/N∆o" colon 50
    tt-param.log_conta_poup  label "CrÇdito Conta Poupanáa"  format "Sim/N∆o" colon 50
    i-numseq                 format ">>>>>>>>>"                               colon 50 skip(2)
    with stream-io side-labels no-attr-space no-box width 132 frame f-param.  

form 
    skip(2)
    "CLASSIFICAÄ«O"                      colon 20 skip(1)
    tt-param.desc-classifica no-label    colon 50 skip(2)
    with stream-io side-labels no-attr-space no-box width 132 frame f-classif.  

form                    
    "IMPRESS«O"                          colon 20   
    skip
    c-destino    label "Destino"         colon 50 
    tt-param.destino      no-label                colon 60 space(1)
    tt-param.arquivo      no-label format "x(30)" skip
    tt-param.usuario               format "x(30)" colon 50 skip
    with stream-io side-labels no-attr-space no-box width 132 frame f-impressao.


/******************************* Inicio do Programa ***************************/

{utp/ut-liter.i Quantidade_de_Registros MFP L}
assign i-numseq:label in frame f-param = return-value.     
{utp/ut-liter.i Liquido_Meio_MagnÇtico_Rescis∆o_Nossa_Caixa MFP C}
assign c-titulo-relat = return-value.
{utp/ut-liter.i FÇrias_e_Rescis‰es *}
assign c-sistema = return-value.

find empresa no-lock where empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.
if avail empresa then assign c-empresa = "Empresa "+ empresa.razao-social.       

find param_empres_rh no-lock where
     param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.

{include/i-freeac.i}
run utp/ut-trfrrp.p (input frame f-impressao:handle).
run utp/ut-trfrrp.p (input frame f-classif:handle).
run utp/ut-trfrrp.p (input frame f-param:handle).

{include/i-rpcab.i}

run utp/ut-acomp.p persistent set h-acomp.

{utp/ut-liter.i Imprimindo._Aguarde *}
run pi-inicializar in h-acomp (input return-value). 

{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.

if tt-param.classifica = 1 or
   tt-param.classifica = 2 or
   tt-param.classifica = 3 then do:
   assign l-primeiro        = yes
          l-imprime         = no
          d-valor-lote      = 0
          d-tot-moeda-lote  = 0
          v_num_seq_lot     = 0
          v_num_qtd_reg     = 0
          v_num_cont        = 0
          v_num_qtd_reg_arq = 0
          v_num_lote        = 0.

   find first rh_estab no-lock where 
              rh_estab.cdn_empresa = param_empres_rh.cdn_empresa and
              rh_estab.cdn_estab   = tt-param.cdn_estab_centr no-error.    
   find first rh_bco NO-LOCK where 
              rh_bco.cdn_banco = tt-param.cdn_banco no-error.
   if avail rh_bco then do:
      find first rh_agenc_bcia of rh_bco where 
                 rh_agenc_bcia.cdn_agenc_bcia = tt-param.cdn_agencia no-lock no-error.
   end.

   if avail rh_agenc_bcia and 
      avail rh_bco and 
      avail rh_estab then do:

      output stream ger-liq to value(tt-param.c-arq-lqd).

      find rh_pessoa_jurid no-lock where
           rh_pessoa_jurid.num_pessoa_jurid = rh_estab.num_pessoa_jurid no-error.

      assign c-hora-grav  = substr(string(time,"HH:MM:SS"),01,02) + 
                            substr(string(time,"HH:MM:SS"),04,02) +
                            substr(string(time,"HH:MM:SS"),07,02)
             c-data-grav  = string(today,"99999999")
             v_dat_lancto = string(tt-param.dat_lancto,"99999999").

      /*** registro header do arquivo ***/
      put stream ger-liq 
          "237"
          "0000"
          "0"
          " "                                         format "x(9)"
          "2"
          rh_pessoa_jurid.cod_id_feder                format "99999999999999"
          tt-param.num_convenio                       format "99999"
          " "                                         format "x(15)"
          tt-param.cdn_agencia                        format "99999"
          tt-param.cod_dv_agencia                     format "9"
          tt-param.num_conta                          format "999999999999"
          tt-param.cod_dv_conta                       format "9"
          " "
          caps(fn-free-accent(empresa.razao-social))  format "x(30)"
          "BRADESCO"                                  format "x(30)"
          " "                                         format "x(10)"
          "1"
          c-data-grav                                 format "99999999"
          c-hora-grav                                 format "999999"
          tt-param.num_seq                            format "999999"
          "080"
          "01600"
          " "                                         format "x(69)"
          skip. 

      do v_num_index = 1 to 6:
         if (v_num_index = 1 and
             tt-param.log_cred_conta) or 
            (v_num_index = 2 and
             tt-param.log_cheque) or
            (v_num_index = 3 and
             tt-param.log_doc) or
            (v_num_index = 4 and
             tt-param.log_cartao) or
            (v_num_index = 5 and
             tt-param.log_conta_poup) then do:

            if v_num_index = 1 then
               assign v_cdn_forma_pagto = 1.
            else
               if v_num_index = 2 then
                  assign v_cdn_forma_pagto = 2.
               else
                  if v_num_index = 3 then
                     assign v_cdn_forma_pagto = 3.
                  else 
                     if v_num_index = 4 then
                        assign v_cdn_forma_pagto = 4.
                     else 
                        if v_num_index = 5 then
                           assign v_cdn_forma_pagto = 5.

            for each funcionario no-lock where
                     funcionario.cdn_empresa    = tt-param.v_cdn_empres_usuar and
                     funcionario.cdn_estab     >= tt-param.i-es-ini and
                     funcionario.cdn_estab     <= tt-param.i-es-fim and
                     funcionario.cdn_bco_liq   >= tt-param.i-bco-liq-ini and
                     funcionario.cdn_bco_liq   <= tt-param.i-bco-liq-fim and
                   ((funcionario.cdn_categ_sal = 1 and tt-param.l-mensal) or
                    (funcionario.cdn_categ_sal = 2 and tt-param.l-horista) or
                    (funcionario.cdn_categ_sal = 3 and tt-param.l-semanal) or
                    (funcionario.cdn_categ_sal = 4 and tt-param.l-quinzenal) or
                    (funcionario.cdn_categ_sal = 5 and tt-param.l-tarefa) or
                    (funcionario.cdn_categ_sal = 6 and tt-param.l-diarista)) and
                     funcionario.idi_forma_pagto     = 1 and
                     funcionario.cdn_forma_pagto_bco = v_cdn_forma_pagto 
                break by funcionario.cdn_estab
                      by funcionario.cdn_agenc_bcia_liq
                      by if tt-param.classifica = 1
                         then string(funcionario.cdn_funcionario,"99999999")
                         else if tt-param.classifica = 2
                              then funcionario.nom_pessoa_fisic
                              else string(funcionario.cdn_cta_corren,"99999999")
                      by if tt-param.classifica = 3
                         then funcionario.nom_pessoa_fisic
                         else "":

                assign v_num_cont = v_num_cont + 1.
                run pi-acompanhar in h-acomp (input v_num_cont).

                assign d-valor-lanc  = 0.

                for each det_rescis no-lock where
                         det_rescis.cdn_empresa           = funcionario.cdn_empresa and
                         det_rescis.cdn_estab             = funcionario.cdn_estab and
                         det_rescis.cdn_funcionario       = funcionario.cdn_funcionario and
                         det_rescis.cdn_tip_calc_rescis   = tt-param.i-tp-res and
                         det_rescis.log_calc_efetd_rescis = yes and
                         det_rescis.dat_pagto_salario    >= d-dt-ini and
                         det_rescis.dat_pagto_salario    <= d-dt-fim 
                    break by det_rescis.cdn_funcionario:
                    do i-inx = 1 to det_rescis.qti_efp:
                       if det_rescis.cdn_idx_efp_funcao_espcif[i-inx] = 52 then do:
                          assign d-valor-lanc  = det_rescis.val_calcul_efp[i-inx].
                          leave.
                       end.
                    end.
                end.
                IF funcionario.cdn_forma_pagto_bco = 3 THEN
                    ASSIGN v_doc = if d-valor-lanc >= 5000
                                   then "018"
                                   else "700".
                ELSE 
                    ASSIGN v_doc = "000".

                if v_doc = "000" then
                   assign v_tipo_pagto = 30.
                else
                   assign v_tipo_pagto = 20.

                if d-valor-lanc > 0 then do:
                   if v_num_seq_lot = 0 then do:
                      assign v_num_lote = v_num_lote + 1.

                      /*** header do lote ***/ 
                      put stream ger-liq                                           
                          "237"                                                    
                          v_num_lote                                               format "9999" 
                          "1"                                                      
                          "C"                                                      
                          v_tipo_pagto                                             format "99"
                          v_cdn_forma_pagto                                        format "99"
                          "040" /*versao layout*/                                  
                          " "                                                      
                          "2"                                                      
                          rh_pessoa_jurid.cod_id_feder                             format "99999999999999"
                          tt-param.num_convenio                                    format "99999"
                          "   "                                                    
                          " "                                                      format "x(12)"
                          tt-param.cdn_agencia                                     format "99999"
                          tt-param.cod_dv_agencia                                  format "x"
                          tt-param.num_conta                                       format "999999999999"
                          tt-param.cod_dv_conta                                    format "x"
                          " "                                                      
                          caps(fn-free-accent(empresa.razao-social))               format "x(30)"
                          " "                                                      format "x(40)"
                          caps(fn-free-accent(rh_pessoa_jurid.nom_ender_rh))       format "x(30)"
                          rh_pessoa_jurid.num_livre_1                              format "99999"
                          caps(fn-free-accent(rh_pessoa_jurid.cod_compl_ender_rh)) format "x(15)"  
                          caps(fn-free-accent(rh_pessoa_jurid.nom_cidad_rh))       format "x(20)"
                          substr(rh_pessoa_jurid.cod_cep_rh,1,5)                   format "99999"
                          substr(rh_pessoa_jurid.cod_cep_rh,6,3)                   format "999"  
                          caps(trim(rh_pessoa_jurid.cod_unid_federac_rh))          format "x(2)"  
                          " "                                                      format "x(18)"
                          skip.
                   end.      

                   assign v_num_seq_lot    = v_num_seq_lot + 1
                          d-valor-lote     = d-valor-lote + d-valor-lanc
                          c-valor-lanc     = string((trunc((d-valor-lanc * 100),0)),"999999999999999")
                          d-tot-moeda-lote = d-tot-moeda-lote + d-valor-moeda
                          c-valor-moeda    = string((trunc((d-valor-moeda * 100000),0)),"999999999999999")
                          v_num_qtd_reg    = v_num_qtd_reg + 1
                          l-imprime        = if not l-imprime
                                             then yes
                                             else l-imprime.

                   /*** detalhe do lote ***/
                   find rh_bco no-lock where
                        rh_bco.cdn_banco = funcionario.cdn_bco_liq no-error.
                   find rh_agenc_bcia of rh_bco no-lock where
                        rh_agenc_bcia.cdn_agenc_bcia = funcionario.cdn_agenc_bcia_liq no-error.

                   assign v_cod_dv_agconta = " ".

                   if substring(funcionario.cod_digito_cta_corren,01,01) <> "" and
                      substring(funcionario.cod_digito_cta_corren,02,01) <> "" then 
                      assign v_cod_dv         = substring(funcionario.cod_digito_cta_corren,02,01)
                             v_cod_dv_agconta = substring(funcionario.cod_digito_cta_corren,02,01).
                   else do:
                      if substring(funcionario.cod_digito_cta_corren,01,01) =  "" and 
                         substring(funcionario.cod_digito_cta_corren,02,01) <> "" then 
                         assign v_cod_dv = substring(funcionario.cod_digito_cta_corren,02,01).

                      if substring(funcionario.cod_digito_cta_corren,02,01) =  "" and 
                         substring(funcionario.cod_digito_cta_corren,01,01) <> "" then 
                         assign v_cod_dv = substring(funcionario.cod_digito_cta_corren,01,01).

                      if substring(funcionario.cod_digito_cta_corren,01,01) = "" and 
                         substring(funcionario.cod_digito_cta_corren,02,01) = "" then
                         assign v_cod_dv = " ".
                   end.

                   if v_cdn_forma_pagto = 2 then
                      assign v_num_conta      = 0
                             v_cod_dv         = " "
                             v_cod_dv_agconta = " ".
                   else
                      assign v_num_conta = dec(funcionario.cdn_cta_corren).

                   put stream ger-liq 
                       "237" 
                       v_num_lote                                   format "9999"
                       "3"
                       v_num_seq_lot                                format "99999"
                       "A"
                       "0"
                       "00".
                   IF v_cdn_forma_pagto = 3 then
                       PUT STREAM ger-liq
                           "018".
                   ELSE
                       PUT STREAM ger-liq
                           "000".
                   PUT STREAM ger-liq
                       funcionario.cdn_bco_liq                              format "999"  
                       funcionario.cdn_agenc_bcia_liq                       format "99999"
                       rh_agenc_bcia.cod_digito_verfdor_agenc_bcia          format "x"
                       v_num_conta                                          format "999999999999"
                       v_cod_dv                                             format "x"
                       v_cod_dv_agconta                                     format "x"                                          
                       caps(fn-free-accent(funcionario.nom_pessoa_fisic))   format "x(30)"
                       "00000000000000000000"
                       v_dat_lancto                                         format "99999999"
                       "BRL"                                                
                       "000000000000000"
                       c-valor-lanc                                         format "xxxxxxxxxxxxxxx"
                       " "                                                  format "x(20)"
                       "00000000"                                           
                       "000000000000000"                                    
                       " "                                                  format "x(40)"
                       " "                                                  format "x(12)"
                       "0"                                                  
                       " "                                                  format "x(10)"
                       skip.

                    IF tt-param.LOG_gera_seg_b or
                       v_cdn_forma_pagto  = 3 THEN DO:
                       assign v_num_seq_lot = v_num_seq_lot + 1
                              v_num_qtd_reg = v_num_qtd_reg + 1.
                       put stream ger-liq
                           "237" 
                           v_num_lote          format "9999"
                           "3"
                           v_num_seq_lot       format "99999"
                           "B"
                           " "                 format "x(3)"
                           "1"
                           dec(funcionario.cod_id_feder) format "99999999999999".

                       find rh_pessoa_fisic no-lock where
                            rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic no-error.

                       put stream ger-liq
                           caps(fn-free-accent(rh_pessoa_fisic.nom_ender_rh))  format "x(30)"
                           "00000"
                           "               "
                           caps(fn-free-accent(rh_pessoa_jurid.cod_compl_ender_rh)) format "x(15)"  
                           caps(fn-free-accent(rh_pessoa_fisic.nom_bairro_rh))      format "x(15)"
                           caps(fn-free-accent(rh_pessoa_jurid.nom_cidad_rh))       format "x(20)"
                           substr(rh_pessoa_jurid.cod_cep_rh,1,5)                   format "99999"
                           substr(rh_pessoa_jurid.cod_cep_rh,6,3)                   format "999"  
                           caps(trim(rh_pessoa_jurid.cod_unid_federac_rh))          format "x(2)"  
                           v_dat_lancto                                             format "99999999"
                           c-valor-lanc                                             format "xxxxxxxxxxxxxxx"
/*                            "000000000000000" */
                           "000000000000000"
                           "000000000000000"
                           "000000000000000"
                           "000000000000000"
                           " "                                                      format "x(15)"
                           skip.
                    end.
                    assign d-valor-lanc  = 0
                           d-valor-moeda = 0.
                end.
            end.

            if l-imprime then do:
               /*** trailer do lote ***/
               if d-valor-lote > 0 then do:
                  assign c-valor-lote      = string((trunc((d-valor-lote * 100),0)),"999999999999999999")
                         c-tot-moeda-lote  = string((trunc((d-tot-moeda-lote * 100000),0)),"999999999999999999")
                         v_num_qtd_reg     = v_num_qtd_reg + 2
                         v_num_qtd_reg_arq = v_num_qtd_reg_arq + v_num_qtd_reg
                         v_num_seq_lot     = 0
                         d-valor-lote      = 0
                         d-tot-moeda-lote  = 0.

                  put stream ger-liq 
                      "237"
                      v_num_lote           format "9999"
                      "5"                 
                      " "                  format "x(9)"
                      v_num_qtd_reg        format "999999"
                      c-valor-lote         format "xxxxxxxxxxxxxxxxxx"
                      "00000000000000000000000"
                      " "                  format "x(166)"
                      " "                  format "x(10)"
                      skip.
                  assign v_num_qtd_reg = 0.
               end.
            end.                       
         end.   
      end.     

      /*** trailer do arquivo ***/      
      assign v_num_qtd_reg_arq = v_num_qtd_reg_arq + 2.

      put stream ger-liq 
          "237"
          "9999"
          "9"
          " "               format "x(9)"
          v_num_lote        format "999999"
          v_num_qtd_reg_arq format "999999"
          "000000"
          " "               format "x(205)"
          skip.
      output stream ger-liq close.
   end.       
end.

if tt-param.parametro then do:
    {utp/ut-liter.i Sim/N∆o MBS R}
    assign l-mensal-2   :format in frame f-param = trim(return-value)
           l-horista-2  :format in frame f-param = trim(return-value)
           l-semanal-2  :format in frame f-param = trim(return-value)
           l-quinzenal-2:format in frame f-param = trim(return-value)
           l-tarefa-2   :format in frame f-param = trim(return-value)
           l-diarista-2 :format in frame f-param = trim(return-value)
           l-mensal-2                            = tt-param.l-mensal
           l-horista-2                           = tt-param.l-horista
           l-semanal-2                           = tt-param.l-semanal       
           l-quinzenal-2                         = tt-param.l-quinzenal       
           l-tarefa-2                            = tt-param.l-tarefa       
           l-diarista-2                          = tt-param.l-diarista.

    disp tt-param.i-es-ini
         tt-param.i-es-fim
         tt-param.i-bco-liq-ini
         tt-param.i-bco-liq-fim
         tt-param.d-dt-ini   
         tt-param.d-dt-fim  
         l-mensal-2 
         l-horista-2
         l-semanal-2 
         l-quinzenal-2
         l-tarefa-2
         l-diarista-2 
         tt-param.c-arq-lqd  
         tt-param.i-tp-res 
         tt-param.dat_lancto 
         tt-param.cdn_banco  
         tt-param.cdn_agencia 
         tt-param.cod_dv_agencia 
         tt-param.num_conta  
         tt-param.cod_dv_conta
         tt-param.cdn_estab_centr 
         tt-param.num_convenio 
         tt-param.cod_docto
         tt-param.log_cred_conta 
         tt-param.log_doc        
         tt-param.log_cheque
         tt-param.log_cartao
         tt-param.log_conta_poup
         i-numseq
         with frame f-param.

    disp tt-param.desc-classifica with frame f-classif.

    assign c-destino = {varinc/var00002.i 04 tt-param.destino}. 

    disp c-destino
         tt-param.destino
         tt-param.arquivo    
         tt-param.usuario
         with frame f-impressao.
end.

{include/i-rpclo.i}

run pi-finalizar in h-acomp.

return "OK":U.

