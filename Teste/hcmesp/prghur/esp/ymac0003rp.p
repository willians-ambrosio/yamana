/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i YMAC0003RP 1.02.00.003 } /*** 010003 ***/
/*******************************************************************************
**
**       Programa: prghur/esp/YMFR0002RP.P
**
**       Data....: agosto/2012.
**
**       Autor...: Totvs S.A.
**
**       Objetivo: Liquido Banco Bradesco Meio Magnetico - F‚rias
**
*******************************************************************************/

/****************************************************************************************** 
** 	   Programa: ymac0003rp.p - copia do programa YMFR0002RP.P
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 05/12/2018
** Change/Chamado: 
**      Objetivo: Gerar o arquivo de remessa de pagamento de folha no padrÆo Febraban 240 posi‡äes para Accesstage
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A   
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{prghur/esp/ymac0003tt.i}
{include/i-rpvar.i}

define temp-table tt-raw-digita 
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.
{prghur/fpp/fp9200.i11}

/*************************** Definicao de Variaveis ***************************/
def var nm-arquivo        as char format "x(40)"                       no-undo.
def var l-primeiro        as log                     initial no        no-undo.
def var l-imprime         as log                     initial no        no-undo.
def var i-index           as int                                       no-undo.
def var c-destino         as char                                      no-undo.
def var l-mensal-2        as log                                       no-undo.
def var l-horista-2       as log                                       no-undo.
def var l-semanal-2       as log                                       no-undo.
def var l-quinzenal-2     as log                                       no-undo.
def var l-diarista-2      as log                                       no-undo.
def var l-tarefa-2        as log                                       no-undo.
def var i-numseq          as int  format 999999      initial 1         no-undo.
def var v_cod_dv          as char format "x"                           no-undo.
def var d-valor-lanc      as dec  format "9999999999999.99"            no-undo.
def var c-valor-lanc      as char format "x(15)"                       no-undo.
def var d-valor-lote      as dec  format "9999999999999999.99"         no-undo.
def var c-valor-lote      as char format "x(18)"                       no-undo.
def var v_num_qtd_reg     as int                                       no-undo.
def var h-acomp           as handle                                    no-undo.
def var c-hora-grav       as char format "x(06)"                       no-undo.
def var c-data-grav       as char format "x(08)"                       no-undo.
def var v_num_seq_lot     as int                                       no-undo.
def var v_dat_lancto      as char format "x(08)"                       no-undo.
def var v_num_cont        as int                                       no-undo.
def var v_cod_dv_agconta  as char format "x"                           no-undo.
def var v_num_qtd_reg_arq as int                                       no-undo.
def var v_num_index       as int                                       no-undo.
def var v_num_lote        as int                                       no-undo.
def var v_des_insc_favor  as char format "x(20)"                       no-undo.
def var v_cdn_forma_pagto as int                                       no-undo.
def var v_num_conta       as dec  format "999999999999"                no-undo.
def var d-valor-moeda     as dec  format "9999999999.99999"            no-undo.
def var c-valor-moeda     as char format "x(15)"                       no-undo.
def var d-tot-moeda-lote  as dec  format "9999999999999.99999"         no-undo.
def var c-tot-moeda-lote  as char format "x(18)"                       no-undo.
def var v_doc             as char format "x(3)"                        no-undo.
def var v_tipo_pagto      as int  format "99"                          no-undo.

def stream ger-liq.

/***************************** Definicao de Frames ****************************/

form
    skip(3)
    "SELE€ÇO"                                                            colon 20 skip(1)
    tt-param.i-es-ini        label "Estabelecimento" format "zz9"        colon 50 space(8)
    "|<    >|"                                                                    space(1)
    tt-param.i-es-fim        no-label     
    tt-param.i-ban-liq-ini   label "Banco"           format "zz9"        colon 50 space(8)
    "|<    >|"                                                                    space(1)    
    tt-param.i-ban-liq-fin   no-label                format "zz9"          
    tt-param.da-pgto-ini     label "Data Pagamento"  format "99/99/9999" colon 50 space(1)
    "|<    >|"                                                                    space(1)   
    tt-param.da-pgto-fin     no-label                format "99/99/9999"           
    l-mensal-2               label "Mensal"                              colon 50  
    l-horista-2              label "Horista"                             colon 50  
    l-semanal-2              label "Semanal"                             colon 50  
    l-quinzenal-2            label "Quinzenal"                           colon 50  
    l-tarefa-2               label "Tarefa"                              colon 50  
    l-diarista-2             label "Diarista"                            colon 50 skip(2)
    "PAR¶METROS"                                                         colon 20
    skip(1)
    tt-param.c-arq-lqd       label "Arquivo L¡quidos"                    colon 50
    tt-param.dat_lancto      label "Data Lan‡amento"                     colon 50 
    tt-param.cdn_banco       label "Banco Empresa"                       colon 50
    tt-param.cdn_agencia     label "Agˆncia Empresa"                     colon 50 space(0)
    "-"                                                                           space(0)
    tt-param.cod_dv_agencia  no-label                        
    tt-param.num_conta       label "Conta Corrente"                      colon 50 space(0)
    "-"                                                                           space(0)
    tt-param.cod_dv_conta    no-label                        
    tt-param.cdn_estab_centr label "Estabelecimento Central"             colon 50
    tt-param.num_convenio    label "Convˆnio"                            colon 50
    tt-param.cod_docto       label "Documento"                           colon 50 
    tt-param.log_cred_conta  label "Cr‚dito Conta Corrente"  format "Sim/NÆo" colon 50
    tt-param.log_doc         label "Documento Cr‚dito"       format "Sim/NÆo" colon 50
    tt-param.log_cheque      label "Cheque Pagto/Admin"      format "Sim/NÆo" colon 50
    tt-param.log_cartao      label "CartÆo Sal rio"          format "Sim/NÆo" colon 50
    tt-param.log_conta_poup  label "Cr‚dito Conta Poupan‡a"  format "Sim/NÆo" colon 50
    i-numseq                 format ">>>>>>>>>"              colon 50 skip(2)
    with stream-io side-labels no-attr-space no-box width 132 frame f-param.  

form 
    skip(2)
    "CLASSIFICA€ÇO"                      colon 20 skip(1)
    tt-param.desc-classifica no-label    colon 50 skip(2)
    with stream-io side-labels no-attr-space no-box width 132 frame f-classif.  

form                    
    "IMPRESSÇO"                          colon 20   
    skip
    c-destino    label "Destino"         colon 50 
    tt-param.destino      no-label                colon 60 space(1)
    tt-param.arquivo      no-label format "x(30)" skip
    tt-param.usuario               format "x(30)" colon 50 skip
    with stream-io side-labels no-attr-space no-box width 132 frame f-impressao.

/** tempor ria para relat¢rio final */ 
DEF TEMP-TABLE tt-dados
    FIELD cdn_estab         LIKE funcionario.cdn_estab              
    FIELD cdn_funcionario   LIKE funcionario.cdn_funcionario       
    FIELD cpf_func          LIKE funcionario.cod_id_feder
    FIELD valor             LIKE histor_pagto_palim.val_palim_normal  
    FIELD nome_func         LIKE funcionario.nom_pessoa_fisic
    FIELD cdn_banco         LIKE funcionario.cdn_bco_liq         
    FIELD cdn_agenc_bcia    LIKE funcionario.cdn_agenc_bcia_liq  
    FIELD cdn_cta_corren    LIKE funcionario.cdn_cta_corren                 
    FIELD cod_digito_cta_corren LIKE funcionario.cod_digito_cta_corren.

FORM
    tt-dados.cdn_estab            COLUMN-LABEL "Est"
    tt-dados.cdn_funcionario      COLUMN-LABEL "Matricula"
    tt-dados.nome_func            COLUMN-LABEL "Nome Funcionario"
    tt-dados.cpf_func             COLUMN-LABEL "CPF Funcionario" FORMAT "999.999.999-99"
    tt-dados.valor                COLUMN-LABEL "Valor L¡quido"
    tt-dados.cdn_banco            COLUMN-LABEL "Banco"
    tt-dados.cdn_agenc_bcia       COLUMN-LABEL "Agencia"
    tt-dados.cdn_cta_corren       COLUMN-LABEL "Conta Corrente"
    tt-dados.cod_digito_cta_corren COLUMN-LABEL "Dg.Cta"
        WITH FRAME f-arq DOWN WIDTH 180 STREAM-IO.


/******************************* Inicio do Programa ***************************/

{utp/ut-liter.i Liquido_F‚rias_Banco_Bradesco_VAN *}
assign c-titulo-relat = return-value.
{utp/ut-liter.i F‚rias_e_Rescisäes *}
assign c-sistema = return-value.
{utp/ut-liter.i Quantidade_de_Registros MFP L}
assign i-numseq:label in frame f-param = return-value.     
{utp/ut-liter.i Usu rio}
assign tt-param.usuario:label in frame f-impressao = return-value.

find param_empres_rh no-lock where
     param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.
find empresa no-lock where empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.
if avail empresa then assign c-empresa = empresa.razao-social.
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

assign nm-arquivo = tt-param.c-arq-lqd.

if tt-param.classifica = 1 or
   tt-param.classifica = 2 or
   tt-param.classifica = 3 then do:

   assign l-imprime         = no
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
          "237"                                                                     /* Banco 1 a 3 */                                           
          "0000"                                                                    /* Lote de Servi‡o 4 a 7*/                                  
          "0"                                                                       /* Tipo de Registro 8 */                                    
          " "                                         format "x(9)"                 /* Febraban - Brancos 9 a 17 */                             
          "2"                                                                       /* Tipo de Inscri‡Æo - 2-CNPJ */                            
          rh_pessoa_jurid.cod_id_feder                format "99999999999999"       /* CNPJ 19 a 32 */                                          
          STRING(tt-param.num_convenio)               format "x(20)"                /* Convenio - 20 33 a 52 */                                 
          tt-param.cdn_agencia                        format "99999"                /* Ag - 53 a 57 */                                          
          tt-param.cod_dv_agencia                     format "9"                    /* Dig Ag 58 */                                             
          tt-param.num_conta                          format "999999999999"         /* Conta 12 - 59 a 70 */                                    
          tt-param.cod_dv_conta                       format "9"                    /* Dig Conta 71 */                                          
          " "                                                                       /* Dig ver ag conta 72 */                                   
          caps(fn-free-accent(empresa.razao-social))  format "x(30)"                /* Razao - 73 a 102 */                                      
          "BRADESCO"                                  format "x(30)"                /* Bano 103 a 132 */                                        
          " "                                         format "x(10)"                /* 133 a 142 - brancos */                                   
          "1"                                                                       /* 143 a 143 - cod remessa */                               
          c-data-grav                                 format "99999999"             /* data geracao - 144 a 151 */                              
          c-hora-grav                                 format "999999"               /* 52 a 57 - hora */                                        
          tt-param.num_seq                            format "999999"               /* 158 a 163 - seq */                                       
          "089"                                                                     /* 164 a 166 - versao do layout - verificar se nÆo ‚ 103 */ 
          "01600"                                                                   /* 167 a 171 - Densidade - verificar se deixa esta */       
          " "                                         format "x(69)"                /* 172 a 191 - Branco 20 - 20 - 29 */                       
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
                     funcionario.cdn_estab   >= tt-param.i-es-ini      and
                     funcionario.cdn_estab   <= tt-param.i-es-fim      and
                     funcionario.cdn_funcionario >= tt-param.i-fc-ini  AND 
                     funcionario.cdn_funcionario <= tt-param.i-fc-fim  AND 
                     funcionario.cdn_bco_liq >= tt-param.i-ban-liq-ini AND 
                     funcionario.cdn_bco_liq <= tt-param.i-ban-liq-fin AND 
                   ((funcionario.cdn_categ_sal = 1 and tt-param.l-mensal    = yes) or
                    (funcionario.cdn_categ_sal = 2 and tt-param.l-horista   = yes) or
                    (funcionario.cdn_categ_sal = 3 and tt-param.l-semanal   = yes) or
                    (funcionario.cdn_categ_sal = 4 and tt-param.l-quinzenal = yes) or
                    (funcionario.cdn_categ_sal = 5 and tt-param.l-tarefa    = yes) or
                    (funcionario.cdn_categ_sal = 6 and tt-param.l-diarista  = yes)) and
                     funcionario.idi_forma_pagto     = 1 and
                     funcionario.cdn_forma_pagto_bco = v_cdn_forma_pagto on stop undo, leave
                break by funcionario.cdn_estab
                      by funcionario.cdn_agenc_bcia_liq
                      by if tt-param.classifica = 1
                         then string(funcionario.cdn_funcionario,"99999999")  /*alterei aqui*/
                         else if tt-param.classifica = 2
                              then funcionario.nom_pessoa_fisic
                              else string(funcionario.cdn_cta_corren,"99999999")
                      by if tt-param.classifica = 3
                         then funcionario.nom_pessoa_fisic
                         else "":

                assign v_num_cont = v_num_cont + 1.
                run pi-acompanhar in h-acomp (input STRING(v_num_cont) + " Matr.: " + STRING(funcionario.cdn_funcionario)).

                assign d-valor-lanc  = 0.

                for each habilit_ferias of funcionario no-lock where
                         habilit_ferias.dat_pagto_ferias   >= tt-param.da-pgto-ini and
                         habilit_ferias.dat_pagto_ferias   <= tt-param.da-pgto-fin:
                    find movto_ferias_calcul no-lock where
                         movto_ferias_calcul.cdn_empresa   = habilit_ferias.cdn_empresa and
                         movto_ferias_calcul.cdn_estab   = habilit_ferias.cdn_estab and
                         movto_ferias_calcul.cdn_funcionario   = habilit_ferias.cdn_funcionario and
                         movto_ferias_calcul.dat_inic_ferias   = habilit_ferias.dat_inic_ferias and
                         movto_ferias_calcul.cdn_tip_calc_ferias  = 0                  and
                         movto_ferias_calcul.log_calc_efetd_movto_ferias   = yes no-error.
                    if not available movto_ferias_calcul then
                       next.
                    for each reg_det_ferias no-lock where
                             reg_det_ferias.cdn_empresa         = movto_ferias_calcul.cdn_empresa     and   
                             reg_det_ferias.cdn_estab           = movto_ferias_calcul.cdn_estab       and   
                             reg_det_ferias.cdn_funcionario     = movto_ferias_calcul.cdn_funcionario and   
                             reg_det_ferias.dat_inic_ferias     = movto_ferias_calcul.dat_inic_ferias and   
                             reg_det_ferias.cdn_tip_calc_ferias = movto_ferias_calcul.cdn_tip_calc_ferias:
                        do i-index = 1 to reg_det_ferias.qti_efp:
                           if reg_det_ferias.cdn_idx_efp_espcif_ferias[i-index] = 1 then do:
                              if reg_det_ferias.log_livre_1 then
                                 assign d-valor-lanc = d-valor-lanc +
                                                  (reg_det_ferias.val_efp_mes_inic_ferias[i-index] +
                                                   reg_det_ferias.val_efp_seguinte_ferias[i-index] +
                                                   reg_det_ferias.val_tot_efp_ferias[i-index]).
                              else 
                                 assign d-valor-lanc = d-valor-lanc +
                                                       reg_det_ferias.val_tot_efp_ferias[i-index].
                              leave.
                           end.
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
                          "237"                                                                                      /* Banco 1 a 3 */                                                                                            
                          v_num_lote                                               format "9999"                     /* Lote de Servi‡o 4 a 7*/                                          
                          "1"                                                                                        /* Tipo de Registro = 1 8 a 8 */                                    
                          "C"                                                                                        /* Tipo operacao - 9 a 9  */                                        
                          v_tipo_pagto                                             format "99"                       /* Tipo do Servi‡o - 30  */                                         
                          v_cdn_forma_pagto                                        format "99"                       /* Forma de Lan‡amento - 12 a 13 */                                 
                          "045" /*versao layout*/                                                                    /* Versao do Layout */                                              
                          " "                                                                                        /* Branco - 17 a 17 */                                              
                          "2"                                                                                        /* 18 a 18 - tipo inscricao */                                      
                          rh_pessoa_jurid.cod_id_feder                             format "99999999999999"           /* 19 a 32 - cnpj */                                                
                          STRING(tt-param.num_convenio)                            format "x(20)"                    /* Convenio - 33 a 52 - 20 */                                       
                          tt-param.cdn_agencia                                     format "99999"                    /* Ag 53 a 57 - 5 */                                               
                          tt-param.cod_dv_agencia                                  format "x"                        /* DIg Ag 58  */                                                   
                          tt-param.num_conta                                       format "999999999999"              /* 59 a 70 - Conta */                                              
                          tt-param.cod_dv_conta                                    format "x(01)"                     /* 71 dig conta */                                                 
                          " "                                                                                         /* 72 dig ver ag conta */                                          
                          caps(fn-free-accent(empresa.razao-social))               format "x(30)"                     /* 73 a 102 - nome empresa */                                      
                          " "                                                      format "x(40)"                     /* 103 a 142 - mensagem  */                                        
                          caps(fn-free-accent(rh_pessoa_jurid.nom_ender_rh))       format "x(30)"                     /* 147 a 172 - endere‡o */                                         
                          rh_pessoa_jurid.num_livre_1                              format "99999"                     /* 173 a 177 - nr */                                               
                          caps(fn-free-accent(rh_pessoa_jurid.cod_compl_ender_rh)) format "x(15)"                     /* 178 a 192 - complemento */                                      
                          caps(fn-free-accent(rh_pessoa_jurid.nom_cidad_rh))       format "x(20)"                     /* 193 a 212 - cidade  */                                          
                          substr(rh_pessoa_jurid.cod_cep_rh,1,5)                   format "99999"                     /* 213 a 217 - cep */                                              
                          substr(rh_pessoa_jurid.cod_cep_rh,6,3)                   format "999"                       /* 218 a 220 - cep */                                              
                          caps(trim(rh_pessoa_jurid.cod_unid_federac_rh))          format "x(2)"                      /* 221 a 222 - uf */                                               
                          "01"                                                     FORMAT "99"                        /* 223 a 224 - Indificativo da forma de pagto = num‚rico 01 */
                          fill(" ",16)                                             FORMAT "X(16)" skip.               /* 225 a 230 - brancos / 231 a 240 - ocorrencias */                   
                          
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
                      assign v_cod_dv         = substring(funcionario.cod_digito_cta_corren,01,01)
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
                       "237"                                                                        /* Banco 1 a 3 */                        
                       v_num_lote                                   format "9999"                   /* Lote de Servi‡o - lote 4 a 7 */       
                       "3"                                                                          /* Tipo de registro = 3 8 */             
                       v_num_seq_lot                                format "99999"                  /* Seq Reg Lote - 9 a 13 */              
                       "A"                                                                          /* Segmento 14 a 14 = a */               
                       "0"                                                                          /* Tipo de Movimento = 0 */              
                       "00".                                                                        /* Instru‡Æo de movimento 16 a 17 */     
                   IF v_cdn_forma_pagto = 3 then
                       PUT STREAM ger-liq
                           "018".   /* Cod. Camara centralizadora 18 a 20 */ 
                   ELSE
                       PUT STREAM ger-liq
                           "000".           /* Cod. Camara centralizadora 18 a 20 */ 
                   PUT STREAM ger-liq
                       funcionario.cdn_bco_liq                              format "999"                    /* 21 a 23 - cod. banco favorecido */                                                              
                       funcionario.cdn_agenc_bcia_liq                       format "99999"                  /* 24 a 28 - ag favorecido */                                                                      
                       rh_agenc_bcia.cod_digito_verfdor_agenc_bcia          format "x(01)"                                                                                                                         
                       v_num_conta                                          format "999999999999"           /* Conta Corrente 30 a 41 */                                                                       
                       v_cod_dv                                             format "x(01)"                  /* Dig CC 42 */                                                                                    
                       v_cod_dv_agconta                                     format "x(01)"                  /* Dig ag conta 43 */                                                                                                  
                       caps(fn-free-accent(funcionario.nom_pessoa_fisic))   format "x(30)"                  /* Nome FAvorecediro - 44 a 73 */                                                                  
                       "00000000000000000000"                                                               /* Nr documento atribuido pela empresa - 74 a 93 */                                                
                       v_dat_lancto                                         format "99999999"               /* dt pagto 94 a 101 */                                                                            
                       "BRL"                                                                                /* tipo moeda 102 a 104 */                                                                         
                       FILL('0',15)                                         FORMAT "X(15)"                  /* 115 a 119 - 15 posi‡oes qtd moeda */                                                            
                       c-valor-lanc                                         format "xxxxxxxxxxxxxxx"        /* 120 a 134 - 15 posi‡äes valor pagto */                                                          
                       " "                                                  format "x(20)"                  /* NR documento atrib banco - 135 a 154 */                                                         
                       v_dat_lancto                                         format "99999999"               /* Dt. Real efetiva‡Æo do pagto - 155 a 162 */                                                     
                       c-valor-lanc                                         format "xxxxxxxxxxxxxxx"        /* 163 a 177 - valor real efetiva‡Æo pagto - 15 pos */                                             
                       fill(" ",40)                                         format "x(40)"                /* Outras infos - 178 a 217 */                                                                    
                       fill(" ",02)                                         format "x(02)"                /* 218 a 219 - cod finalidade DOC */                                                              
                       fill(" ",10)                                         format "x(10)"                /* 220 a 224 - cod finalidade TED / 225 a 226 - cod finalidade complementar / 227 a 229 - CNAB*/  
                       "0"                                                  format "9"                    /* 230 - aviso ao favorecido */                                                                   
                       fill(" ",10)                                         format "x(10)" skip.          /* 231 a 240 - ocorrencias de retorno */                                                          


                   IF tt-param.LOG_gera_seg_b or
                      v_cdn_forma_pagto  = 3 THEN DO:

                      find rh_pessoa_fisic no-lock where
                           rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic no-error.

                      assign v_num_seq_lot = v_num_seq_lot + 1
                             v_num_qtd_reg = v_num_qtd_reg + 1.
                      put stream ger-liq
                           "237"                                                                                    /* Banco 1 a 3 */                                
                           v_num_lote          format "9999"                                                        /* Lote de Servi‡o - lote 4 a 7 */               
                           "3"                                                                                      /* Tipo de registro = 3 8 */                     
                           v_num_seq_lot       format "99999"                                                       /* Seq Reg Lote - 9 a 13 */                      
                           "B"                                                                                      /* Segmento 14 a 14 = B */                       
                           " "                 format "x(3)"                                                        /* Febraban - 15 a 17 */                         
                           "1"                                                                                      /* 18 - tipo inscricao favorecido */             
                           dec(funcionario.cod_id_feder) format "99999999999999"                                    /* 19 a 32 - cpf */                              
                           caps(fn-free-accent(rh_pessoa_fisic.nom_ender_rh))  format "x(30)"                       /* endere‡o 33 a 62 */                           
                           "00000"                                                                                  /* nr endere‡o */                                
                           FILL(' ',15)                                             format "x(15)"                  /* complemento - 68 a 82 - 15 */                 
                                                                                                                                
                           caps(fn-free-accent(rh_pessoa_fisic.nom_bairro_rh))      format "x(15)"                  /* bairro - 83 a 97 */                           
                           caps(fn-free-accent(rh_pessoa_fisic.nom_cidad_rh))       format "x(20)"                  /* cidade - 20 - 98 a 117 */                                                          
                           substr(rh_pessoa_fisic.cod_cep_rh,1,5)                   format "99999"                                                                   
                           substr(rh_pessoa_fisic.cod_cep_rh,6,3)                   format "999"                    
                           caps(trim(rh_pessoa_fisic.cod_unid_federac_rh))          format "x(2)"                   /* UF - 126 a 127 */                               
                           v_dat_lancto                                             format "99999999"               /* Data Vencimento - 8 128 a 135 */                
                           c-valor-lanc                                             format "xxxxxxxxxxxxxxx"        /* 136 a 150 - 15 posiäes valor pagto */           
                           "000000000000000"                                                                        /* valor do abatimento */                          
                           "000000000000000"                                                                        /* valor de desconto */                            
                           "000000000000000"                                                                        /* valor da mora */                                
                           "000000000000000"                                                                        /* valor da multa */                               
                           FILL(" ",15)                                             FORMAT "X(15)"                     /* cod documento favorecido - 211 a 225 */               
                           "0"                                                      FORMAT "9"                         /* Aviso ao favorecido 226 a 226 */                      
                           "000000"                                                 FORMAT "999999"                    /* Cod. Ug Centralizadora - 227 a 232 */                 
                           FILL("0",8)                                              FORMAT "99999999"                  /* Cod identifica‡Æo banco no spb - 233 a 240 */         
                           SKIP.                                                                                                                                                
                                   
                   end.

                    /* Dados do Relat¢rio */ 
                    CREATE tt-dados.
                    ASSIGN  tt-dados.cdn_estab             = funcionario.cdn_estab                    
                            tt-dados.cdn_funcionario       = funcionario.cdn_funcionario              
                            tt-dados.cpf_func              = funcionario.cod_id_feder   
                            tt-dados.valor                 = d-valor-lanc
                            tt-dados.nome_func             = funcionario.nom_pessoa_fisic             
                            tt-dados.cdn_banco             = funcionario.cdn_bco_liq                  
                            tt-dados.cdn_agenc_bcia        = funcionario.cdn_agenc_bcia_liq           
                            tt-dados.cdn_cta_corren        = funcionario.cdn_cta_corren               
                            tt-dados.cod_digito_cta_corren = funcionario.cod_digito_cta_corren.

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
                       "000000000000000000"  format "999999999999999999"              
                       FILL('0',6)           FORMAT "999999"                          
                       fill(" ",165)         format "x(165)"                          
                       FILL(" ",10)          FORMAT "X(10)" /* correncias */          
                       SKIP.           
    
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
          fill(" ",09)           format "x(09)"      
          v_num_lote             format "999999"     
          v_num_qtd_reg_arq      format "999999"     
          "000000"                                   
          fill(" ",205)          format "x(205)"     
          skip.                                      


      output stream ger-liq close.
   end.       
end.

FOR EACH tt-dados BREAK BY tt-dados.nome_func:

    ACCUMULATE tt-dados.valor (TOTAL BY tt-dados.nome_func).

    DISP tt-dados.cdn_estab
         tt-dados.cdn_funcionario
         tt-dados.nome_func
         tt-dados.cpf_func 
         tt-dados.valor
         tt-dados.cdn_banco
         tt-dados.cdn_agenc_bcia
         tt-dados.cdn_cta_corren
         tt-dados.cod_digito_cta_corren
        WITH FRAME f-arq DOWN.
    DOWN WITH FRAME f-arq.

    IF LAST(tt-dados.nome_func)
         THEN PUT "TOTAL" AT 49
        (ACCUM TOTAL tt-dados.valor) FORMAT ">>>,>>>,>>>,>>9.99" AT 72.
END.

if tt-param.parametro = yes then do:      
    {utp/ut-liter.i Sim/NÆo MBS R}
    assign l-mensal-2:format in frame f-param = trim(return-value)
           l-horista-2:format in frame f-param = trim(return-value)
           l-semanal-2:format in frame f-param = trim(return-value)
           l-quinzenal-2:format in frame f-param = trim(return-value)
           l-tarefa-2:format in frame f-param = trim(return-value)
           l-diarista-2:format in frame f-param = trim(return-value)
           l-mensal-2    = tt-param.l-mensal       
           l-horista-2   = tt-param.l-horista
           l-semanal-2   = tt-param.l-semanal       
           l-quinzenal-2 = tt-param.l-quinzenal       
           l-tarefa-2    = tt-param.l-tarefa       
           l-diarista-2  = tt-param.l-diarista.

    disp tt-param.i-es-ini
         tt-param.i-es-fim  
         tt-param.i-ban-liq-ini
         tt-param.i-ban-liq-fin 
         tt-param.da-pgto-ini   
         tt-param.da-pgto-fin  
         l-mensal-2 
         l-horista-2
         l-semanal-2 
         l-quinzenal-2
         l-tarefa-2   
         l-diarista-2 
         tt-param.c-arq-lqd  
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
         i-numseq with frame f-param.

    disp tt-param.desc-classifica with frame f-classif.

    assign c-destino = {varinc/var00002.i 04 tt-param.destino}. 

    disp c-destino
         tt-param.destino   
         tt-param.arquivo    
         tt-param.usuario with frame f-impressao.
end.
run pi-finalizar in h-acomp.
{include/i-rpclo.i}

return "ok".

