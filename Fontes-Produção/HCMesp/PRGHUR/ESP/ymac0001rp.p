/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YMAC0001RP 1.02.00.050 } /*** 010050 ***/
{include/buffers_RH.i}

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i YMAC0001rp MFP}
&ENDIF

/******************************************************************************
***
**       Programa: prghur/fpp/fp6115.p
**
**       Data....: Fevereiro/1995.
**
**       Autor...: Datasul
**
**       Objetivo: Liquido Pensionista Banco do Brasil Meio Magnetico
**                 (Normal, 13o , FÇrias)
**
*******************************************************************************/
/****************************************************************************************** 
** 	   Programa: ymac0001rp.p - copia do programa ymfp0005rp.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 05/12/2018
** Change/Chamado: 
**      Objetivo: Gerar o arquivo de remessa de pagamento de folha no padr∆o Febraban 240 posiá‰es
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A   
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{include/i-rpvar.i}

def var h-acomp as handle no-undo.
run utp/ut-acomp.p persistent set h-acomp.  

/************************ Definicao de Variaveis ***************************/
def var i-cd-banco     as inte format "999"     initial 1   no-undo.

def var c-tab-trans    as char initial "ymac0001"           no-undo.
DEFINE VARIABLE c-hora AS CHARACTER   NO-UNDO.
def var x-valor        as char format "99999999999"         no-undo.
def var i-est-ini      like rh_estab.cdn_estab              no-undo.
def var i-est-fim      like rh_estab.cdn_estab              no-undo.
def var l-primeiro     as log                initial no     no-undo.
def var l-ultimo       as log                initial no     no-undo.
def var i-num-ins      as dec                               no-undo.
def var c-cod-ins      as char                              no-undo.
def var c-ind-oper     as char format "!!"                  no-undo.
def var c-brancos-90   as char format "x(090)"              no-undo.
def var l-imprime      as log                initial no     no-undo.
def var i-inx          as inte                              no-undo.
def var i-index        as inte                              no-undo.
def var i-ind          as inte               initial 1      no-undo.
def var i-dia          as inte                              no-undo.
def var i-lin-fra      as dec                               no-undo.
def var v_cont         as integer                           no-undo.
DEFINE VARIABLE c_docto AS CHARACTER   NO-UNDO.
/*************************** Variaveis do detalhe do lay-out *****************/
def var i-ordem-rel    as inte format ">>9"                 no-undo.
def var c-descricao    as char format "x(055)"              no-undo.
def var i-tamanho      as inte format ">>9"                 no-undo.
def var i-inicio       as inte format ">>9"                 no-undo.
def var i-termino      as inte format ">>9"                 no-undo.
def var c-formato      as char format "x(008)"              no-undo.
def var i-decimais     as inte format ">9"                  no-undo.
def var c-espaco       as char format "x(001)"              no-undo.
def var i-ano-arq      as inte format "99"   initial 0      no-undo.

/************************ Variaveis registro tipo 0 - header *****************/
def var c-tipo-reg     as char format "x(001)"    initial "0"       no-undo.
def var c-filler1      as char format "x(001)"    initial "1"       no-undo.
def var c-filler2      as char format "x(007)"                      no-undo.
def var c-tipo-serv    as char format "x(002)"    initial "03"      no-undo.
def var c-filler3      as char format "x(001)"    initial ""        no-undo.
def var i-vl-tarifa    as inte format "99999"     initial 0         no-undo.
def var c-filler4      as char format "x(009)"    initial ""        no-undo.
def var i-conta-s036   as inte format "999999999" initial 0         no-undo.
def var c-dvcc-s036    as char format "x(001)"    initial ""        no-undo.
def var c-filler5      as char format "x(005)"    initial ""        no-undo.
def var c-nome-emp     as char format "x(030)"                      no-undo.
def var c-codbanco     as char format "x(003)"    initial "001"     no-undo.
def var c-filler6      as char format "x(115)"    initial ""        no-undo.
def var c-filleri      as int  format "999999999999999"             no-undo.
def var i-numseq       as inte format 999999      initial 1         no-undo.

/************************ Variaveis registro tipo 1 - detalhe *****************/
def var c-filler7      as char format "x(014)"                      no-undo.
def var c-filler8      as char format "x(024)"    initial ""        no-undo.
def var c-filler9      as char format "x(003)"    initial ""        no-undo.
def var c-filler10     as char format "x(003)"    initial ""        no-undo.
def var i-age-func     as inte format "9999"                        no-undo.
def var c-dvage-func   as char format "x(001)"                      no-undo.
def var c-contacc-func as char format "999999999999"                no-undo.
def var c-dvcc-func    as char format "x(001)"                      no-undo.
def var c-filler11     as char format "x(002)"                      no-undo.
def var c-nome-func    as char format "x(040)"                      no-undo.
def var i-dt-processo  as inte format "999999"                      no-undo.
def var d-valor        as inte format "9999999999999"               no-undo.
def var i-cdserv       as inte format "999"       initial 0         no-undo.
def var c-filler12     as char format "x(050)"                      no-undo.
def var i-bco-dest     as int  format "999"                         no-undo.
def var i-cd-camara    as int  format "999"                         no-undo.
def var v_digito_verfdor like rh_agenc_bcia.cod_digito_verfdor_agenc_bcia no-undo.

def var v_log_folha_educnal as log                                       no-undo.
def var c-hora-grav         as char format "x(06)"                       no-undo.
def var c-data-grav         as char format "x(08)"                       no-undo.
def var v_dat_lancto        as char format "x(08)"                       no-undo.
def var v_num_index         as int                                       no-undo.
def var v_cdn_forma_pagto   as int                                       no-undo.
def var v_num_seq_lot       as integer                                   no-undo.
def var d-valor-lote        as dec  format "999999999999999999"          no-undo.
def var c-valor-lanc        as char format "x(15)"                       no-undo.
def var v_num_qtd_reg       as int                                       no-undo.
def var v_num_qtd_reg_arq   as int                                       no-undo.
def var v_num_cont          as int                                       no-undo.
def var v_num_lote          as int                                       no-undo.
def var v_cod_dv            as char format "x"                           no-undo.
def var v_cod_dv_agconta    as char format "x"                           no-undo.
def var v_num_conta         as dec  format "999999999999"                no-undo.
def var c-valor-lote        as char format "x(18)"                       no-undo.
DEF VAR v_outras_info       AS CHAR FORMAT "x(40)"                       NO-UNDO.

DEFINE VARIABLE v_tot_reg_arq   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE v_tot_pagto     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE v_val_tot_pagto AS DECIMAL     NO-UNDO.

DEF VAR v_doc                 AS CHAR FORMAT "x(3)"                        NO-UNDO.
/************************* Variaveis registro tipo 9 - trailler **************/
def var c-filler13      as char format "x(193)"                     no-undo.

def var c-destino     as char format "x(15)"                        no-undo.

def stream ger-liq.
{prghur/esp/ymac0001tt.i}

def temp-table tt-raw-digita
    field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

for each tt-raw-digita:
   create tt-digita.
   raw-transfer raw-digita to tt-digita.
end.
create tt-param.
raw-transfer raw-param to tt-param.

{prghur/fpp/fp9200.i11}

DEF BUFFER bhistor_pagto_palim for histor_pagto_palim.

/** tempor†ria para relat¢rio final */ 
DEF TEMP-TABLE tt-dados
    FIELD cdn_empreas       LIKE bnfciar_palim.cdn_empresa            
    FIELD cdn_estab         LIKE bnfciar_palim.cdn_estab              
    FIELD cdn_funcionario   LIKE bnfciar_palim.cdn_funcionario 
    FIELD cpf_func          LIKE funcionario.cod_id_feder
    FIELD cpf_bnfciar       LIKE bnfciar_palim.cod_id_feder
    FIELD cdn_bnfciar_palim LIKE bnfciar_palim.cdn_bnfciar_palim      
    FIELD valor             LIKE histor_pagto_palim.val_palim_normal  
    FIELD valor_rem         AS CHAR
    FIELD nome_bnfciar      LIKE bnfciar_palim.nom_pessoa_fisic       
    FIELD nome_func         LIKE funcionario.nom_pessoa_fisic
    FIELD cdn_banco         LIKE bnfciar_palim.cdn_banco     
    FIELD cdn_agenc_bcia    LIKE bnfciar_palim.cdn_agenc_bcia
    FIELD cdn_cta_corren    LIKE bnfciar_palim.cdn_cta_corren                 
    FIELD cod_digito_cta_corren LIKE bnfciar_palim.cod_digito_cta_corren
    FIELD documento         AS CHAR
    FIELD dig_agenc         AS CHAR 
    FIELD cod_dv            AS CHAR
    FIELD cod_dv_agconta    AS CHAR 
    FIELD doc               AS CHAR
    FIELD forma_pagto       AS INT
    FIELD cod_serv          AS INT
    FIELD cpf               AS DEC
    FIELD endereco          AS CHAR 
    FIELD bairro            AS CHAR
    FIELD cidade            AS CHAR
    FIELD cep               AS CHAR
    FIELD uf                AS CHAR.

FORM
    tt-dados.cdn_estab            COLUMN-LABEL "Est"
    tt-dados.cdn_funcionario      COLUMN-LABEL "Matricula"
    tt-dados.nome_func            COLUMN-LABEL "Nome Funcionario"
    tt-dados.cpf_func             COLUMN-LABEL "CPF Funcionario" FORMAT "999.999.999-99"
    tt-dados.cdn_bnfciar_palim    COLUMN-LABEL "Cod. Ben"
    tt-dados.nome_bnfciar         COLUMN-LABEL "Nome Beneficiario"
    tt-dados.cpf_bnfciar          COLUMN-LABEL "CPF Beneficiario" FORMAT "999.999.999-99"
    tt-dados.valor                COLUMN-LABEL "Valor L°quido"
    tt-dados.cdn_banco            COLUMN-LABEL "Banco"
    tt-dados.cdn_agenc_bcia       COLUMN-LABEL "Agencia"
    tt-dados.cdn_cta_corren       COLUMN-LABEL "Conta Corrente"
    tt-dados.cod_digito_cta_corren COLUMN-LABEL "Dg.Cta"
        WITH FRAME f-dados DOWN WIDTH 220 STREAM-IO.

FORM                     
    "IMPRESS«O"           COLON 20 SKIP(1)
    c-destino             COLON 50 SPACE(1)
    arquivo      NO-LABEL FORMAT "x(30)" SKIP 
    usuario               FORMAT "x(30)" COLON 50 SKIP 
    WITH STREAM-IO SIDE-LABELS NO-ATTR-SPACE NO-BOX WIDTH 132 FRAME f-impressao.

FORM  
    "SELEÄ«O"                                                colon 20 skip
    i-es-ini       label "Estabelecimento"                   colon 50 space(6) 
    "|<   >|"                                                         space(1)
    i-es-fim       no-label  skip 
    i-bco-liq-ini  label "Banco"                             colon 50 space(8) 
    "|<   >|"                                                         space(1)
    i-bco-liq-fim  no-label  
    d-dt-ini           label "Data Pagamento FÇrias"         colon 50 space(1) 
    "|<   >|"                                                         space(1)
    d-dt-fim           no-label  
    d-dt-sal           label "Pagamento Sal†rio"   format "99/99/9999" colon 50 
    v_log_adto_normal  label "Adiantamento Normal"           colon 50 
    v_log_ferias       label "FÇrias"                        colon 50 
    v_log_adto_13_sal  label "Adiantamento 13 Sal†rio"       colon 50 
    v_log_13_sal       label "13 Sal†rio"                    colon 50 
    v_log_rescisao     label "Rescis∆o"                      colon 50 
    v_log_normal       label "Normal"                        colon 50 
    v_log_plr          label "PLR"                           colon 50 skip(2)
    with stream-io side-labels no-attr-space no-box width 132 frame f-selec.

form 
    "PAR∂METROS"                                             colon 20 skip(1)
    c-arq-lqd            label "Arquivo Pens∆o Aliment°cia"  colon 50
    tt-param.cdn_banco       label "Banco Empresa"           colon 50
    tt-param.cdn_agencia     label "Agància Empresa"         colon 50 space(0)
    "-"                                                               space(0)
    tt-param.cod_dv_agencia  no-label                        
    tt-param.num_conta       label "Conta Corrente"          colon 50 space(0)
    "-"                                                               space(0)
    tt-param.cod_dv_conta    no-label                        
    tt-param.cod_docto       label "Documento"               colon 50
    tt-param.dat_lancto      label "Data Lanáamento"         colon 50 
    tt-param.cdn_estab_centr label "Estabelecimento Central" colon 50
    tt-param.num_convenio    label "Convànio"                colon 50
    tt-param.log_cred_conta  label "CrÇdito Conta Corrente"  format "Sim/N∆o" colon 50
    tt-param.log_doc         label "Documento CrÇdito"       format "Sim/N∆o" colon 50
    tt-param.log_cheque      label "Cheque Pagto/Admin"      format "Sim/N∆o" colon 50
    tt-param.log_cartao      label "Cart∆o Sal†rio"          format "Sim/N∆o" colon 50
    tt-param.log_conta_poup  label "CrÇdito Conta Poupanáa"  format "Sim/N∆o" colon 50
    i-numseq       format ">>>>>>>>>"                        colon 50 skip(2)
    with stream-io side-labels no-attr-space no-box width 132 frame f-param.     

{utp/ut-liter.i Destino}
assign c-destino:label in frame f-impressao = return-value.

{utp/ut-liter.i Usu†rio}
assign usuario:label in frame f-impressao = return-value.

{utp/ut-liter.i Quantidade_de_Registros MFP L}
assign i-numseq:label in frame f-param = return-value.     

find empresa where 
     empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-lock no-error.
find param_empres_rh where 
     param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-lock no-error.

find param_folha_educnal no-lock where
     param_folha_educnal.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.
assign v_log_folha_educnal = if avail param_folha_educnal then yes else no.

{utp/ut-liter.i FOLHA_DE_PAGAMENTO MFP C}
assign c-sistema = return-value.   
run pi-inicializar in h-acomp (input return-value).

{utp/ut-liter.i Pens∆o_Meio_MagnÇtico_Bradesco MFP C}

/***************************** Inicio do Programa ***************************/
assign c-programa     = "YMAC0001"
       c-empresa      = empresa.razao-social
       c-sistema      = "Folha de Pagamento"
       c-titulo-relat = return-value.
{include/i-freeac.i}
run utp/ut-trfrrp.p (input frame f-param:handle).
run utp/ut-trfrrp.p (input frame f-selec:handle).
run utp/ut-trfrrp.p (input frame f-impressao:handle).

DEFINE VARIABLE i-sequencia AS INTEGER     NO-UNDO.

ASSIGN l-primeiro        = yes
       c-hora            = string(time,"HH:MM:SS")
       i-sequencia       = 1
       d-valor-lote      = 0
       v_num_seq_lot     = 0
       v_num_qtd_reg     = 0
       v_num_qtd_reg_arq = 0
       l-imprime         = no
       v_num_cont        = 0
       v_num_lote        = 0.

find first rh_estab no-lock where 
           rh_estab.cdn_empresa = param_empres_rh.cdn_empresa and
           rh_estab.cdn_estab   = tt-param.cdn_estab_centr    no-error.    
find first rh_bco where 
           rh_bco.cdn_banco = tt-param.cdn_banco no-lock no-error.

find first rh_agenc_bcia of rh_bco where 
           rh_agenc_bcia.cdn_agenc_bcia = tt-param.cdn_agencia no-lock no-error.

if avail rh_agenc_bcia and 
   avail rh_bco and 
   avail rh_estab then do:
    
   find rh_pessoa_jurid no-lock where
        rh_pessoa_jurid.num_pessoa_jurid = rh_estab.num_pessoa_jurid no-error.
   assign c-hora-grav  = substr(string(time,"HH:MM:SS"),01,02) + 
                         substr(string(time,"HH:MM:SS"),04,02) +
                         substr(string(time,"HH:MM:SS"),07,02)
          c-data-grav  = string(today,"99999999")
          v_dat_lancto = string(tt-param.dat_lancto,"99999999").
  

   IF tt-param.i-ind-selec = 1 THEN DO:
    
      for each bnfciar_palim no-lock  where                                                       
               bnfciar_palim.cdn_empresa                    = tt-param.v_cdn_empres_usuar  and    
               bnfciar_palim.cdn_estab                     >= i-es-ini                     and    
               bnfciar_palim.cdn_estab                     <= i-es-fim                     and    
               bnfciar_palim.cdn_funcionario               >= v_cdn_funcionario_ini        and    
               bnfciar_palim.cdn_funcionario               <= v_cdn_funcionario_fim        and    
               bnfciar_palim.idi_forma_pagto_bnfciar_palim  = 1                            and    
               bnfciar_palim.cdn_banco                     >= i-bco-liq-ini                and    
               bnfciar_palim.cdn_banco                     <= i-bco-liq-fim                       
          break by bnfciar_palim.cdn_banco                                                        
                 by bnfciar_palim.cdn_agenc_bcia                                                  
                  by bnfciar_palim.nom_pessoa_fisic                                               
                   by bnfciar_palim.idi_forma_pagto_bnfciar_palim with frame f-param:             
         
          {prghur\esp\ymac0001rp.i}
    
      END.
   END.
   ELSE DO:
         for each tt-digita,
                   first bnfciar_palim no-lock  where
                            bnfciar_palim.cdn_empresa                    = tt-digita.v_cdn_empres_usuar and
                            bnfciar_palim.cdn_estab                      = tt-digita.es-codigo          and
                            bnfciar_palim.cdn_funcionario                = tt-digita.fc-codigo          and
                            bnfciar_palim.idi_forma_pagto_bnfciar_palim  = 1                            and
                            bnfciar_palim.cdn_banco                     >= i-bco-liq-ini                and
                            bnfciar_palim.cdn_banco                     <= i-bco-liq-fim               
                       break by bnfciar_palim.cdn_banco
                              by bnfciar_palim.cdn_agenc_bcia
                               by bnfciar_palim.nom_pessoa_fisic
                                by bnfciar_palim.idi_forma_pagto_bnfciar_palim with frame f-param:
    
            {prghur\esp\ymac0001rp.i}  
        
        END. 
   end.
end.   



/**** Gera arquivo de Remessa de Pens∆o ********/
IF CAN-FIND(FIRST tt-dados) THEN DO:

    OUTPUT STREAM ger-liq TO VALUE(c-arq-lqd).

    FOR EACH tt-dados BREAK BY tt-dados.forma_pagto:

        /*** Header do Arquivo */
        IF FIRST(tt-dados.forma_pagto) THEN
            RUN pi-header.

        /*** Header do Lote **/
        IF FIRST-OF(tt-dados.forma_pagto) THEN
            RUN pi-header-240.

        RUN pi-segmentoA.
        RUN pi-segmentoB.

        IF LAST-OF(tt-dados.forma_pagto) THEN
            RUN pi-trailler-240.

        IF LAST(tt-dados.forma_pagto) THEN 
            RUN pi-trailler.
    END.
    
    OUTPUT STREAM ger-liq CLOSE. 
END.

{include/i-rpcab.i}
{include/i-rpout.i}
view frame f-cabec.
view frame f-rodape.

{utp/ut-liter.i Sim/N∆o * R}
assign v_log_adto_normal:format in frame f-selec = trim(return-value)
       v_log_ferias:format in frame f-selec      = trim(return-value)
       v_log_adto_13_sal:format in frame f-selec = trim(return-value)
       v_log_13_sal:format in frame f-selec      = trim(return-value)
       v_log_rescisao:format in frame f-selec    = trim(return-value)
       v_log_normal:format in frame f-selec      = trim(return-value)
       v_log_plr:format in frame f-selec         = trim(return-value).

/* Gera Relat¢rio */ 
FOR EACH tt-dados BREAK BY tt-dados.nome_func:

    ACCUMULATE tt-dados.valor (TOTAL BY tt-dados.nome_func).

    DISP tt-dados.cdn_estab
         tt-dados.cdn_funcionario
         tt-dados.nome_func
         tt-dados.cpf_func     
         tt-dados.cdn_bnfciar_palim
         tt-dados.nome_bnfciar
         tt-dados.cpf_bnfciar  
         tt-dados.valor
         tt-dados.cdn_banco
         tt-dados.cdn_agenc_bcia
         tt-dados.cdn_cta_corren
         tt-dados.cod_digito_cta_corren
        WITH FRAME f-dados DOWN.
    DOWN WITH FRAME f-dados.

    IF LAST(tt-dados.nome_func)
         THEN PUT "TOTAL" AT 120
        (ACCUM TOTAL tt-dados.valor) FORMAT ">>>,>>>,>>>,>>9.99" AT 139.

END.

if tt-param.tb-parametro = no then do:
   {include/i-rpclo.i}
   run pi-finalizar in h-acomp.
   return "OK":U.
end.    

disp    
    i-es-ini  
    i-es-fim   
    i-bco-liq-ini  
    i-bco-liq-fim
    d-dt-ini
    d-dt-fim 
    d-dt-sal  
    v_log_adto_normal
    v_log_ferias
    v_log_adto_13_sal
    v_log_13_sal
    v_log_rescisao
    v_log_normal        
    v_log_plr
    with frame f-selec.

disp
    c-arq-lqd
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
assign c-destino = {varinc/var00002.i 04 tt-param.destino}. 
disp                   
    c-destino   
    arquivo    
    usuario      
    with frame f-impressao.


{include/i-rpclo.i}
RUN pi-finalizar in h-acomp.
RETURN.

PROCEDURE pi-header:

   ASSIGN v_tot_reg_arq = v_tot_reg_arq + 1
          v_num_qtd_reg = 0
          v_num_qtd_reg_arq = v_num_qtd_reg_arq + 1.
   
   find first rh_estab no-lock where
              rh_estab.cdn_empresa = param_empres_rh.cdn_empresa and
              rh_estab.cdn_estab   = tt-param.cdn_estab_centr no-error.
   if avail rh_estab then do:

      find first rh_bco no-lock where
                 rh_bco.cdn_banco = tt-param.cdn_banco no-error.
      if avail rh_bco then do:
         find first rh_agenc_bcia no-lock of rh_bco where
                    rh_agenc_bcia.cdn_agenc_bcia = tt-param.cdn_agencia  no-error.
         if avail rh_agenc_bcia then
            put stream ger-liq
                "237"                                       format "999"                /* Banco 1 a 3 */         
                "0000"                                      format "0000"               /* Lote de Serviáo 4 a 7*/
                "0"                                         format "9"                  /* Tipo de Registro 8 */
                fill(" ",09)                                format "x(09)"              /* Febraban - Brancos 9 a 17 */
                "2"                                         format "9"                  /* Tipo de Inscriá∆o - 2-CNPJ */
                rh_estab.cod_id_feder                       format "99999999999999"     /* CNPJ 19 a 32 */
                fn-free-accent(tt-param.num_convenio)       format "x(20)"              /* Convenio - 20 33 a 52 */
                rh_agenc_bcia.cdn_agenc_bcia                format "99999"              /* Ag - 53 a 57 */
                rh_agenc_bcia.cod_digito_verfdor_agenc_bcia format "x(01)"              /* Dig Ag 58 */
                tt-param.num_conta                          format "999999999999"       /* Conta 12 - 59 a 70 */
                tt-param.cod_dv_conta                       format "x(01)"              /* Dig Conta 71 */
                fill(" ",01)                                format "x(01)"              /* Dig ver ag conta 72 */
                fn-free-accent(empresa.razao-social)        format "x(30)"              /* Razao - 73 a 102 */
                "BRADESCO"                                  format "x(30)"              /* Bano 103 a 132 */
                fill(" ",10)                                format "x(10)"              /* 133 a 142 - brancos */
                "1"                                         format "9"                  /* 143 a 143 - cod remessa */
                today                                       format "99999999"           /* data geracao - 144 a 151 */
                substr(c-hora,01,02)                        format "99"                 /* 52 a 57 - hora */
                substr(c-hora,04,02)                        format "99"
                substr(c-hora,07,02)                        format "99"
                i-sequencia                                 format "999999"             /* 158 a 163 - seq */
                "089"                                       format "999"                /* 164 a 166 - versao do layout - verificar se n∆o Ç 103 */
                "01600"                                     format "99999"              /* 167 a 171 - Densidade - verificar se deixa esta */
                fill(" ",69)                                format "x(69)" skip.        /* 172 a 191 - Branco 20 - 20 - 29 */ 
         
      end.
   end.

   assign i-sequencia = i-sequencia + 1.

end procedure.

procedure pi-trailler:
   
   ASSIGN v_tot_reg_arq = v_tot_reg_arq + 1
          v_num_qtd_reg_arq = v_num_qtd_reg_arq + 1.

   put stream ger-liq
       "237"         format "999"
       "9999"        format "9999"
       "9"           format "9"
       fill(" ",09)  format "x(09)"
       v_num_lote    format "999999"
       v_num_qtd_reg_arq format "999999"
       "000000"      format "999999"
       fill(" ",205) format "x(205)" skip.

   assign i-sequencia = i-sequencia + 1.
       
end procedure.

PROCEDURE pi-header-240:

  DEFINE VARIABLE c-lancto AS CHARACTER   NO-UNDO.

   /* Header do lote */
   find first rh_bco no-lock where
              rh_bco.cdn_banco = tt-param.cdn_banco no-error.
   if avail rh_bco then do:
      find first rh_agenc_bcia no-lock of rh_bco where
                 rh_agenc_bcia.cdn_agenc_bcia = tt-param.cdn_agenc no-error.
      if avail rh_agenc_bcia then do:

         ASSIGN v_tot_pagto   = v_tot_pagto   + v_val_tot_pagto
                d-valor-lote  = 0
                v_num_lote    = v_num_lote    + 1
                v_tot_reg_arq = v_tot_reg_arq + 1
                v_num_qtd_reg_arq = v_num_qtd_reg_arq + 1
                v_num_qtd_reg = 1
                v_num_seq_lot = 0.
         
         put stream ger-liq
             "237"                                               format "999"                               /* Banco 1 a 3 */                                                   
             v_num_lote                                          format "9999"                              /* Lote de Serviáo 4 a 7*/                                          
             "1"                                                 format "9"                                 /* Tipo de Registro = 1 8 a 8 */                                            
             "C"                                                 format "x(01)"                             /* Tipo operacao - 9 a 9  */                                     
             "20"                                                format "99"                                /* Tipo do Serviáo - 01 */                                    
             tt-dados.forma_pagto                                format "99" /*forma de lancto*/            /* Forma de Lanáamento - 12 a 13 =01 - Mesmo Banco - 03-DOC/TED */ 
             "045"                                               format "999"                               /* Versao do Layout */                                         
             " "                                                 format "x(01)"                             /* Branco - 17 a 17 */                                                  
             "2"                                                 format "9"                                 /* 18 a 18 - tipo inscricao */                                                     
             rh_pessoa_jurid.cod_id_feder                        format "99999999999999"                    /* 19 a 32 - cnpj */                                            
             tt-param.num_convenio                               format "x(20)"                             /* Convenio - 33 a 52 - 20 */                                                  
             rh_agenc_bcia.cdn_agenc_bcia                        format "99999"                             /* Ag 53 a 57 - 5 */                                           
             rh_agenc_bcia.cod_digito_verfdor_agenc_bcia         format "x(01)"                             /* DIg Ag 58  */                                              
             tt-param.num_conta                                  format "999999999999"                      /* 59 a 70 - Conta */                                                
             tt-param.cod_dv_conta                               format "x(01)"                             /* 71 dig conta */                                           
             fill(" ",01)                                        format "x(01)"                             /* 72 dig ver ag conta */                                       
             fn-free-accent(empresa.razao-social)                format "x(30)"                             /* 73 a 102 - nome empresa */                                      
             "2"                                                 FORMAT "X(01)"                             /* 103 - mensagem 2-Pens∆o */
             fill(" ",39)                                        format "x(39)"                             /* 104 a 142 - mensagem  */                                                
             fn-free-accent(rh_pessoa_jurid.nom_ender)           format "x(30)"                             /* 147 a 172 - endereáo */                                                                    
             rh_pessoa_jurid.num_ender_rh                        format "99999"                             /* 173 a 177 - nr */                                                                    
             fn-free-accent(rh_pessoa_jurid.cod_compl_ender_rh)  format "x(15)"                             /* 178 a 192 - complemento */                                               
             fn-free-accent(rh_pessoa_jurid.nom_cidad_rh)        format "x(20)"                             /* 193 a 212 - cidade  */         
             rh_pessoa_jurid.cod_cep_rh                          format "x(08)"                             /* 213 a 220 - cep */               
             fn-free-accent(rh_pessoa_jurid.cod_unid_federac_rh) format "x(02)"                             /* 221 a 223 - uf */                               
             "01"                                                FORMAT "99"                                /* 223 a 224 - Indicativo da forma de pagto = numÇrico 01 */
             fill(" ",16)                                        FORMAT "X(16)" skip.                       /* 225 a 230 - brancos / 231 a 240 - ocorrencias */
         
      end.
   end.

   assign i-sequencia = i-sequencia + 1.

end procedure.

procedure pi-trailler-240:

   ASSIGN v_num_qtd_reg = v_num_qtd_reg + 1
          v_num_qtd_reg_arq = v_num_qtd_reg_arq + 1
          c-valor-lote  = string(trunc(d-valor-lote * 100,0),"999999999999999999").

   /* Trailler do lote */
   put stream ger-liq
       "237"                 format "999"
       v_num_lote            format "9999"
       "5"                   format "9"
       fill(" ",09)          format "x(09)"
       v_num_qtd_reg         format "999999"   
       c-valor-lote          format "xxxxxxxxxxxxxxxxxx"
       "000000000000000000"  format "999999999999999999"
       FILL('0',6)           FORMAT "999999" 
       fill(" ",165)         format "x(165)" 
       FILL(" ",10)          FORMAT "X(10)" /* correncias */
       skip.

   assign i-sequencia = i-sequencia + 1.

end procedure.

PROCEDURE pi-segmentoA:

    ASSIGN   v_num_qtd_reg = v_num_qtd_reg + 1
             v_num_qtd_reg_arq = v_num_qtd_reg_arq + 1
             d-valor-lote  = d-valor-lote + tt-dados.valor
             v_num_seq_lot = v_num_seq_lot + 1.

    put stream ger-liq
             "237"                        format "999"              /* Banco 1 a 3 */
             v_num_lote                   format "9999"             /* Lote de Serviáo - lote 4 a 7 */
             "3"                          format "9"                /* Tipo de registro = 3 8 */
             v_num_seq_lot                format "99999"            /* Seq Reg Lote - 9 a 13 */
             "A"                          format "x(01)"            /* Segmento 14 a 14 = a */
             "0"                          format "9"                /* Tipo de Movimento = 0 */
             "00"                         format "99"               /* Instruá∆o de movimento 16 a 17 */
             tt-dados.doc                 format "999"              /* Cod. Camara centralizadora 18 a 20 */
             tt-dados.cdn_banco           format "999"              /* 21 a 23 - cod. banco favorecido */
             tt-dados.cdn_agenc_bcia      format "99999"            /* 24 a 28 - ag favorecido */
             tt-dados.dig_agenc           format "x(01)"            /* Dig AG - 29 */
             tt-dados.cdn_cta_corren      format "999999999999"     /* Conta Corrente 30 a 41 */
             tt-dados.cod_digito_cta_corren format "x(01)"          /* Dig CC 42 */ 
             " " /*tt-dados.cod_dv */     format "x(01)"            /* Dig ag conta 43 */
             tt-dados.nome_bnfciar        format "x(30)"            /* Nome Favorecediro - 44 a 73 */  
             tt-dados.documento           FORMAT "X(20)"            /* Nr documento atribuido pela empresa - 74 a 93 */  
             v_dat_lancto                 format "99999999"                 /* dt pagto 94 a 101 */                         
             "BRL"                        format "x(03)"                    /* tipo moeda 102 a 104 */                 
             "000000000000000"            format "999999999999999"          /* 115 a 119 - 15 posiáoes qtd moeda */    
             tt-dados.valor_rem           FORMAT "xxxxxxxxxxxxxxx"          /* 120 a 134 - 15 posi‰es valor pagto */                                                                          
             fill(" ",20)                 FORMAT "X(20)"                    /* NR documento atrib banco - 135 a 154 */  
             v_dat_lancto                 FORMAT "99999999"                 /* Dt. Real efetivaá∆o do pagto - 155 a 162 */ 
             tt-dados.valor_rem           FORMAT "xxxxxxxxxxxxxxx"          /* 163 a 177 - valor real efetivaá∆o pagto - 15 pos */       
             fill(" ",38)                 FORMAT "X(38)"                    /* Outras infos - 178 a 215 */                                                                                    
             " "                          FORMAT "9"                        /* Outras infos - 216 - G031 - 2 = Pens∆o Aliment°cia Ativo */                                                    
             " "                          FORMAT "X(01)"                    /* Outras infos - 217 */                                    
             fill(" ",02)                 FORMAT "X(02)"                    /* 218 a 219 - cod finalidade DOC */                                                                              
             "00101CC"                    FORMAT "X(07)"                    /* 220 a 224 - cod finalidade TED - 00101 - Pens∆o / 225 a 226 - cod finalidade complementar - Conta Corrente */  
             fill(" ",03)                 FORMAT "X(03)"                    /* 227 a 229 - CNAB*/                                                                                             
             "0"                          FORMAT "9"                        /* 230 - aviso ao favorecido */                                                                                   
             fill(" ",10)                 FORMAT "X(10)" SKIP.              /* 231 a 240 - ocorrencias de retorno */


    ASSIGN v_val_tot_pagto = v_val_tot_pagto + d-valor / 100.

END.

PROCEDURE pi-segmentoB:

    ASSIGN v_num_qtd_reg = v_num_qtd_reg + 1
           v_num_qtd_reg_arq = v_num_qtd_reg_arq + 1
           v_num_seq_lot = v_num_seq_lot + 1
           c-valor-lanc  = string(tt-dados.valor,"999999999999999").

    put stream ger-liq
         "237"                          FORMAT "999"                                         /* Banco 1 a 3 */
         v_num_lote                     FORMAT "9999"                                        /* Lote de Serviáo - lote 4 a 7 */
         "3"                            FORMAT "9"                                           /* Tipo de registro = 3 8 */
         v_num_seq_lot                  FORMAT "99999"                                       /* Seq Reg Lote - 9 a 13 */
         "B"                            FORMAT "X(01)"                                       /* Segmento 14 a 14 = B */
         FILL(" ",3)                    FORMAT "X(03)"                                       /* Febraban - 15 a 17 */
         "1"                            FORMAT "9"                                           /* 18 - tipo inscricao favorecido */
         DEC(tt-dados.cpf_bnfciar)                FORMAT "99999999999999"                    /* 19 a 32 - cpf */
         CAPS(fn-free-accent(tt-dados.endereco))  FORMAT "x(30)"                             /* endereáo 33 a 62 */
         "00000"                                                                             /* nr endereáo */
         FILL(' ',15)                   FORMAT "X(15)"                                       /* complemento - 68 a 82 - 15 */
         caps(fn-free-accent(tt-dados.bairro )) FORMAT  "X(15)"             /* bairro - 83 a 97 */
         caps(fn-free-accent(tt-dados.cidade))  FORMAT  "X(20)"             /* cidade - 20 - 98 a 117 */
         substr(tt-dados.cep,1,5)               FORMAT  "99999"    
         substr(tt-dados.cep,6,3)               FORMAT  "999"  
         caps(TRIM(tt-dados.uf))                FORMAT  "X(2)"              /* UF - 126 a 127 */
         v_dat_lancto                           FORMAT "99999999"          /* Data Vencimento - 8 128 a 135 */
         tt-dados.valor_rem                     FORMAT "xxxxxxxxxxxxxxx"   /* 136 a 150 - 15 posi‰es valor pagto */
         "000000000000000"                                                 /* valor do abatimento */    
         "000000000000000"                                                 /* valor de desconto */
         "000000000000000"                                                 /* valor da mora */
         "000000000000000"                                                 /* valor da multa */
         FILL(" ",15)                           FORMAT "X(15)"             /* cod documento favorecido - 211 a 225 */
         "0"                                    FORMAT "9"                 /* Aviso ao favorecido 226 a 226 */
         "000000"                               FORMAT "999999"            /* Cod. Ug Centralizadora - 227 a 232 */
         FILL("0",8)                            FORMAT "99999999"          /* Cod identificaá∆o banco no spb - 233 a 240 */
         skip.                                                          

END.
