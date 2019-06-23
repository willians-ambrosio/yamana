/****************************************************************************************** 
** 	   Programa: ap001rp3.i
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 11/07/2018
** Change/Chamado: 
**      Objetivo: Importa‡Æo de t¡tulos de antecipaa‡Æo e reembolso de despesa com origem no Concur
**
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

/* Variaveis para excel */
DEFINE VARIABLE c-arq-excel      AS    CHARACTER             NO-UNDO.
DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.
DEF TEMP-TABLE tt-arq-anexo
    FIELD arquivo AS CHAR.

define temp-table tt-param no-undo
    field destino          as integer
    field c-destino        as char 
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as INTEGER
    FIELD tipo             AS INT.

DEF temp-table tt-raw-digita
field raw-digita as raw.    

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

DEFINE VARIABLE c-pendente AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-erro     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-sucesso  AS CHARACTER   NO-UNDO.

{include/i-rpvar.i}

find param-global no-lock no-error.     

create tt-param.
raw-transfer raw-param to tt-param.      


/* Fim defini‡äes template */
RUN utp/ut-acomp.p persistent set h-acomp.

RUN pi-inicializar in h-acomp (INPUT "Processando...").

/* Iniciio programa */
RUN pi-validacao-inicial.

/* IMPORTA DADOS */
IF tt-param.tipo = 1 /* Processamento */ THEN DO:

    /* Importa arquivo - tt-dados */
    RUN pi_upload.

    /* Processamento */
    /* Separa os t¡tulos do rateio cont bil */
    RUN pi_titulos. /* Cria as temp-tables tt_titulo e tt_rateio */

END.
ELSE RUN pi-reprocessa. /* Reprocessamento da tela de log */

/* Cria as tabelas para integra‡Æo */
RUN pi_integra_antecip. /* Cria as antecipa‡äes */

RUN pi_integra_apb. /* Cria os t¡tulos no APB */

RUN pi_integra_devolucao. /* Cria as baixa das devolu‡äes */

/* Gera a tabela de log de importacao */
RUN pi_tabela_log.

/* Para enviar email somente quando usar o ap001-w */
IF tt-param.tipo = 1 /* Processamento */ THEN DO:

    RUN pi-copia-arquivo.
    
    /* Gera Relatorio em excel */
    RUN pi-report-excel.
    
    /* Envia email do relatorio */
    RUN pi-email. 

END.

ASSIGN c-titulo-relat = "Consistˆncia da Atualiza‡Æo".
                                   
/* Gera relat¢rio de log na tela */
{include/i-rpcab.i}
{include/i-rpout.i}
RUN pi-relatorio.
  
RUN pi-finalizar IN h-acomp.

{include/i-rpclo.i}
RETURN "OK":U.


/**************** PROCEDURE ***************/
/***** PROCEDURES INTERNAS *****/
PROCEDURE pi-validacao-inicial:
    
   /* Verifica diretorios de arquivos */
    FIND FIRST concur_param NO-LOCK NO-ERROR.
    IF NOT AVAIL concur_param THEN 
        RUN pi_gera_log (INPUT INT(0),
                         INPUT "ERRO",
                         INPUT "",
                         INPUT 0,
                         INPUT "NÆo encontrado parƒmetro de diret¢rios dos arquivos do Concur.!",
                         INPUT "",
                         INPUT "",
                         INPUT "",
                         INPUT 0,
                         INPUT ?).

    ELSE ASSIGN c-sucesso  = concur_param.dir_sucesso
                c-erro     = concur_param.dir_erro  
/*                 c-pendente = "C:\Felipe\pendente". */
                c-pendente = concur_param.dir_pendente.
                

    FIND FIRST trad_finalid_econ_ext NO-LOCK 
         WHERE trad_finalid_econ_ext.cod_matriz_trad_finalid_ext = "MOEDAS"
           AND trad_finalid_econ_ext.cod_finalid_econ_ext        = "0" NO-ERROR.
    IF NOT AVAIL trad_finalid_econ_ext THEN 
        RUN pi_gera_log (INPUT INT(0),
                         INPUT "ERRO",
                         INPUT "",
                         INPUT 0,
                         INPUT "NÆo encontrada finalidade econ“mica!",
                         INPUT "",
                         INPUT "",
                         INPUT "",
                         INPUT 0,
                         INPUT ?).
    
    FIND FIRST histor_finalid_econ NO-LOCK 
         WHERE histor_finalid_econ.cod_finalid_econ        = trad_finalid_econ_ext.cod_finalid_econ
           AND histor_finalid_econ.dat_inic_valid_finalid <= TODAY 
           AND histor_finalid_econ.dat_fim_valid_finalid  >= TODAY NO-ERROR.
    IF NOT AVAIL histor_finalid_econ THEN 
       RUN pi_gera_log (INPUT INT(0),
                        INPUT "ERRO",
                        INPUT "",
                        INPUT 0,
                        INPUT "NÆo encontrado Hist¢rico Finalidade Econ“mica!",
                        INPUT "",
                        INPUT "",
                        INPUT "",
                        INPUT 0,
                        INPUT ?).

    ASSIGN c-carteira = "APB".
    IF AVAIL histor_finalid_econ and histor_finalid_econ.cod_finalid_econ <> "Corrente" THEN 
            ASSIGN c-portador = "80000" c-carteira = "APB".

    /* Verifica como fazer esta leitura */
    ASSIGN c_plano_conta = IF AVAIL plano_cta_ctbl THEN plano_cta_ctbl.cod_plano_cta_ctbl ELSE "CONTSOC".

    /* Busca Esp‚cie para Reembolso de Despesa */
    FIND FIRST concur_param_dados NO-LOCK 
        WHERE concur_param_dados.referencia = "Esp_Reembolso" 
        AND concur_param_dados.ativa NO-ERROR.
    IF NOT AVAIL concur_param_dados
         THEN RUN pi_gera_log (INPUT INT(0),
                               INPUT "ERRO",
                               INPUT "",
                               INPUT 0,
                               INPUT "NÆo encontrado parametro para a esp‚cie de reembolso, verifique o cadastro AP004-W",
                               INPUT "",
                               INPUT "",
                               INPUT "",
                               INPUT 0,
                               INPUT ?).
    ELSE ASSIGN c-esp-reemb = concur_param_dados.valor.  

    /* Busca Esp‚cie para Antecipa‡Æo de Despesa */
     FIND FIRST concur_param_dados NO-LOCK 
        WHERE concur_param_dados.referencia = "Esp_Antecipacao" 
        AND concur_param_dados.ativa NO-ERROR.
    IF NOT AVAIL concur_param_dados
         THEN RUN pi_gera_log (INPUT INT(0),
                               INPUT "ERRO",
                               INPUT "",
                               INPUT 0,
                               INPUT "NÆo encontrado parametro para a esp‚cie de antecipa‡Æo, verifique o cadastro AP004-W",
                               INPUT "",
                               INPUT "",
                               INPUT "",
                               INPUT 0,
                               INPUT ?).
    ELSE ASSIGN c-esp-antecip = concur_param_dados.valor.

    /* Busca Esp‚cie para Antecipa‡Æo de Despesa */
    FIND FIRST concur_param_dados NO-LOCK 
       WHERE concur_param_dados.referencia = "Esp_Cartao" 
        AND concur_param_dados.ativa NO-ERROR.
    IF NOT AVAIL concur_param_dados
         THEN RUN pi_gera_log (INPUT INT(0),
                               INPUT "ERRO",
                               INPUT "",
                               INPUT 0,
                               INPUT "NÆo encontrado parametro para a esp‚cie de cartÆo, verifique o cadastro AP004-W",
                               INPUT "",
                               INPUT "",
                               INPUT "",
                               INPUT 0,
                               INPUT ?).
    ELSE ASSIGN c-esp-cartao = concur_param_dados.valor.

    /* Busca Esp‚cie para Reembolso de Despesa */
     FIND FIRST concur_param_dados NO-LOCK 
        WHERE concur_param_dados.referencia = "Portador_Geral" 
        AND concur_param_dados.ativa NO-ERROR.
    IF NOT AVAIL concur_param_dados
         THEN RUN pi_gera_log (INPUT INT(0),
                               INPUT "ERRO",
                               INPUT "",
                               INPUT 0,
                               INPUT "NÆo encontrado parametro para portador, verifique o cadastro AP004-W",
                               INPUT "",
                               INPUT "",
                               INPUT "",
                               INPUT 0,
                               INPUT ?).
    ELSE ASSIGN c-portador = concur_param_dados.valor. 

END.

PROCEDURE pi_upload:

    DEFINE VARIABLE dc-data    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dt-emissao AS DATE      NO-UNDO.

    /* Lˆ os diret¢rios de dados */
    INPUT FROM OS-DIR (c-pendente).
    
    REPEAT: 
        IMPORT c-arquivo_imp.

        FILE-INFO:FILE-NAME = c-pendente + "\" + c-arquivo_imp.

            
        /* Despresa os diretor¢rios */
        IF  FILE-INFO:FILE-TYPE = "DRW" THEN NEXT.

        CREATE tt-arquivo.
        ASSIGN tt-arquivo.carq = FILE-INFO:FILE-NAME.
    
    END. 
    INPUT CLOSE.

    FOR EACH tt-arquivo:

        INPUT FROM VALUE(tt-arquivo.carq).
    
        /* O cabe‡alho do arquivo ‚ ignorado pois cont‚m os totais para todos os pa¡ses e nÆo apenas Brasil */
        REPEAT:
        
             IMPORT UNFORMATTED c-dados.
    
             IF TRIM(ENTRY(1,c-dados,"|")) = "EXTRACT" 
                 THEN ASSIGN c-batch = TRIM(ENTRY(5,c-dados,"|")).
        
             /* Importa os dados de detalhes */
             IF TRIM(ENTRY(1,c-dados,"|")) = "DETAIL" THEN DO:

                /* Esta valida‡Æo ‚ necess ria pois o arquivo vindo do Concur traz todos os pa¡ses */
                IF TRIM(ENTRY(23,c-dados,"|")) <> "BRAZIL" THEN NEXT.

                ASSIGN dc-data    = TRIM(ENTRY(64,c-dados,"|")).

                IF R-INDEX(TRIM(ENTRY(5,c-dados,"|")),"-") > 0 
                THEN ASSIGN c-fornec = SUBSTR(TRIM(ENTRY(5,c-dados,"|")),1,R-INDEX(TRIM(ENTRY(5,c-dados,"|")),"-") - 1)
                            c-hist   = " Movimento de CartÆo " + SUBSTR(TRIM(ENTRY(5,c-dados,"|")),R-INDEX(TRIM(ENTRY(5,c-dados,"|")),"-") + 1,LENGTH(TRIM(ENTRY(5,c-dados,"|")))).
                ELSE ASSIGN c-fornec = TRIM(ENTRY(5,c-dados,"|"))
                            c-hist = "".

                CREATE tt-dados.
                ASSIGN tt-dados.BATCH_id    = TRIM(ENTRY(2,c-dados,"|"))
                       tt-dados.sequence    = TRIM(ENTRY(4,c-dados,"|"))
                       tt-dados.cod_estab   = TRIM(ENTRY(193,c-dados,"|"))
                       tt-dados.cod_refer   = TRIM(ENTRY(20,c-dados,"|"))
                       tt-dados.dt_trans    = TRIM(ENTRY(64,c-dados,"|"))
                       tt-dados.valor_total = DECIMAL(REPLACE(TRIM(ENTRY(32,c-dados,"|")),".",","))
                       tt-dados.pessoa      = c-fornec
                       tt-dados.titulo      = TRIM(ENTRY(20,c-dados,"|"))
                       tt-dados.parcela     = "1"
                       tt-dados.dt_emissao  = TODAY
                       tt-dados.dt_vencto   = fncDiaVencto(TODAY)
                       tt-dados.dt_depos    = DATE(INT(SUBSTR(dc-data,6,2)),INT(SUBSTR(dc-data,9,2)),INT(SUBSTR(dc-data,1,4)))
                       tt-dados.ccontabil   = TRIM(ENTRY(167,c-dados,"|"))
                       tt-dados.ccusto      = TRIM(ENTRY(203,c-dados,"|"))
                       tt-dados.unid_neg    = TRIM(ENTRY(193,c-dados,"|"))
                       tt-dados.vl_rateio   = DECIMAL(REPLACE(TRIM(ENTRY(169,c-dados,"|")),".",","))
                       tt-dados.lancto      = SUBSTR(TRIM(ENTRY(169,c-dados,"|")),1,1)
                       tt-dados.pais        = TRIM(ENTRY(23,c-dados,"|")) 
                       tt-dados.historico   = TRIM(ENTRY(69,c-dados,"|"))  + " " +  TRIM(ENTRY(71,c-dados,"|")) + c-hist
                       tt-dados.portador    = c-portador
                       tt-dados.carteira    = c-carteira.

                  /*  Quando a data do dep¢sito for diferente da data do mˆs da importa‡Æo, ser  utilizado o primeiro dia do mˆs corrente(today)*/
                  IF MONTH(tt-dados.dt_depos) <> MONTH(TODAY)
                       THEN ASSIGN tt-dados.dt_depos = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
                  ASSIGN c-plano-ccusto = "".

                  /* Estabelecimento */
                  FIND FIRST estabelecimento NO-LOCK WHERE
                             estabelecimento.cod_estab = TRIM(tt-dados.cod_estab) NO-ERROR.
                  IF NOT AVAIL estabelecimento THEN DO:                                                   
                                                                                                          
                     RUN pi_gera_log (INPUT INT(trim(tt-dados.pessoa)),                                   
                                        INPUT "ERRO",                                                     
                                        INPUT tt-dados.cod_estab,                                         
                                        INPUT tt-dados.sequence,                                          
                                        INPUT "Estabelecimento" + tt-dados.cod_estab + " nÆo encontrado.",
                                        INPUT tt-dados.titulo,                                            
                                        INPUT tt-dados.especie,                                           
                                        INPUT "",                                                         
                                        INPUT tt-dados.valor_total,                                       
                                        INPUT tt-dados.dt_vencto).                                        
                      NEXT.                                                                               
                  END.                                                                                    
                  ELSE ASSIGN tt-dados.cod_empresa = estabelecimento.cod_empresa.

                  /* Verifica dep¢sito */

                  /* Verifica se ‚ um dep¢sito - caso seja, separa o movimento */
                  FIND cta_ctbl NO-LOCK
                        WHERE cta_ctbl.cod_plano_cta_ctbl = c_plano_conta
                        AND   cod_cta_ctbl = tt-dados.ccontabil NO-ERROR.
                  IF AVAIL cta_ctbl THEN DO: 
                      
                     IF cta_ctbl.cod_grp_cta_ctbl = "ATIVO" THEN DO:

                         IF tt-dados.valor_total > 0 AND tt-dados.vl_rateio > 0 THEN 
                            ASSIGN tt-dados.tipo = 1 /* Antecipa‡Æo */
                                   tt-dados.especie = c-esp-antecip.
    
                         IF tt-dados.valor_total = 0 AND tt-dados.vl_rateio > 0 THEN 
                            ASSIGN tt-dados.tipo    = 2 /* Dep¢sito */
                                   tt-dados.especie = "AD".
    
                         IF tt-dados.valor_total = 0 AND tt-dados.vl_rateio < 0 THEN 
                            ASSIGN tt-dados.tipo = 3 /* Presta‡Æo de contas de Antecipa‡Æo */
                                   tt-dados.especie = c-esp-reemb.

                         IF tt-dados.valor_total > 0 AND tt-dados.vl_rateio < 0 THEN 
                            ASSIGN tt-dados.tipo = 3 /* Presta‡Æo de contas de Antecipa‡Æo */
                                   tt-dados.especie = c-esp-reemb.

                         /* Esp‚cie para cartÆo de cr‚dito */
                         IF R-INDEX(TRIM(ENTRY(5,c-dados,"|")),"-") > 0 THEN 
                           ASSIGN tt-dados.tipo    = 3 /* T¡tulo de CartÆo de Cr‚dito para pagamento */
                                  tt-dados.especie = c-esp-cartao.
                                                                
                         FIND espec_docto NO-LOCK 
                             WHERE espec_docto.cod_espec_docto = c-esp-antecip NO-ERROR.
                         IF NOT AVAIL espec_docto OR espec_docto.ind_tip_espec_docto <> "antecipacao" THEN
                              RUN pi_gera_log (INPUT INT(TRIM(tt-dados.pessoa)),
                                               INPUT "ERRO",
                                               INPUT tt-dados.cod_estab, 
                                               INPUT tt-dados.sequence,
                                               INPUT "Conta cont bil " + tt-dados.ccontabil + " ‚ de ATIVO por‚m, a esp‚cie " + c-esp-antecip + " nÆo ‚ do tipo antecipa‡Æo. Verifique os parƒmetros da esp‚cie (bas_espec_docto)",
                                               INPUT tt-dados.titulo,
                                               INPUT c-esp-antecip,
                                               INPUT estabelecimento.cod_empresa,
                                               INPUT tt-dados.valor_total,
                                               INPUT tt-dados.dt_vencto).
                      
                         /* Verifica parƒmetro para saber se ‚ um dep¢sito */
                         FOR EACH concur_param_dados NO-LOCK 
                             WHERE concur_param_dados.referencia = "CC_Deposito_Portador"
                             AND   concur_param_dados.ativa = YES:
                      
                             IF concur_param_dados.valor     = tt-dados.ccontabil THEN DO:

                                 ASSIGN tt-dados.tipo        = 2 /* Qdo dep¢sito ser  efetuada a altera‡Æo da antecipa‡Æo */
                                        tt-dados.fora-rateio = YES
                                        tt-dados.cod_refer   = tt-dados.cod_refer + "D"
                                        tt-dados.especie     = "AD"
                                        tt-dados.portador    = concur_param_dados.valor_1
                                        c-portador           = concur_param_dados.valor_1.
                             
                      
                                 /* Valida o portador que ser  usado na altera‡Æo da Antecipa‡Æo */
                                 FIND portad_estab NO-LOCK
                                      WHERE portad_estab.cod_estab    = tt-dados.cod_estab
                                      AND   portad_estab.cod_portador = concur_param_dados.valor_1 NO-ERROR.
                                 IF AVAIL portad_estab THEN 
                                      ASSIGN tt-dados.portador = concur_param_dados.valor_1 /* Qdo dep¢sito ser  efetuada a altera‡Æo da antecipa‡Æo */
                                             c-portador        = concur_param_dados.valor_1.
                             END.
                         END.
                     END.
                     ELSE DO:

                         ASSIGN tt-dados.tipo = 3
                                tt-dados.especie = c-esp-reemb.

                         /* Esp‚cie para cartÆo de cr‚dito */
                         IF R-INDEX(TRIM(ENTRY(5,c-dados,"|")),"-") > 0 THEN 
                           ASSIGN tt-dados.tipo    = 3 /* T¡tulo de CartÆo de Cr‚dito para pagamento */
                                  tt-dados.especie = c-esp-cartao.

                         FIND espec_docto NO-LOCK 
                             WHERE espec_docto.cod_espec_docto = c-esp-reemb NO-ERROR.
                         IF NOT AVAIL espec_docto OR espec_docto.ind_tip_espec_docto = "antecipacao" THEN
                              RUN pi_gera_log (INPUT INT(TRIM(tt-dados.pessoa)),
                                               INPUT "ERRO",
                                               INPUT tt-dados.cod_estab, 
                                               INPUT tt-dados.sequence,
                                               INPUT "Conta cont bil " + tt-dados.ccontabil + " ‚ de DESPESA por‚m, a esp‚cie " + c-esp-reemb + " nÆo ‚ do tipo despesa. Verifique os parƒmetros da esp‚cie (bas_espec_docto)",
                                               INPUT tt-dados.titulo,
                                               INPUT c-esp-reemb,
                                               INPUT tt-dados.cod_empresa,
                                               INPUT tt-dados.valor_total,
                                               INPUT tt-dados.dt_vencto).

                     END.
                  END.
                  /* Final verifica dep¢sito */
                                  
                  /* Plano Centro de Custo */
                  FIND FIRST plano_ccusto NO-LOCK 
                     WHERE plano_ccusto.cod_empresa     = tt-dados.cod_empresa
                       AND plano_ccusto.dat_inic_valid <= TODAY 
                       AND plano_ccusto.dat_fim_valid  >= TODAY NO-ERROR.
                    IF NOT AVAIL plano_ccusto THEN 
                            RUN pi_gera_log (INPUT 0,
                                             INPUT "ERRO",
                                             INPUT "",
                                             INPUT 0,
                                             INPUT "Plano Centro de Custo nÆo encontrado!",
                                             INPUT "",
                                             INPUT "",
                                             INPUT estabelecimento.cod_empresa,
                                             INPUT 0,
                                             INPUT ?).
                   ELSE ASSIGN c-plano-ccusto = plano_ccusto.cod_Plano_ccusto.

                   /* Fornecedor */
                  FIND fornecedor NO-LOCK 
                      WHERE fornecedor.cod_empresa  = tt-dados.cod_empresa
                      AND fornecedor.cdn_fornecedor = INT(TRIM(tt-dados.pessoa)) NO-ERROR.
                  IF NOT AVAIL fornecedor THEN 
                      RUN pi_gera_log (INPUT INT(TRIM(tt-dados.pessoa)),
                                        INPUT "ERRO",
                                        INPUT tt-dados.cod_estab,
                                        INPUT tt-dados.sequence,
                                        INPUT "Fornecedor " + tt-dados.pessoa + " nÆo encontrado.",
                                        INPUT tt-dados.titulo,
                                        INPUT tt-dados.especie,
                                        INPUT tt-dados.cod_empresa,
                                        INPUT tt-dados.valor_total,
                                        INPUT tt-dados.dt_vencto). 
                                                           
                  /* Centro de Custo */
                  FIND ccusto NO-LOCK 
                      WHERE ccusto.cod_empresa      = tt-dados.cod_empresa
                      AND   ccusto.cod_plano_ccusto = c-plano-ccusto  /* Verificar o parƒmetro para identificar o plano ccusto */
                      AND   ccusto.cod_ccusto       = tt-dados.ccusto NO-ERROR.
                  IF NOT AVAIL ccusto THEN 
                      RUN pi_gera_log (INPUT INT(TRIM(tt-dados.pessoa)),
                                        INPUT "ERRO",
                                        INPUT tt-dados.cod_estab,
                                        INPUT tt-dados.sequence,
                                        INPUT "Centro Custo " + tt-dados.ccusto + " nÆo encontrado.",
                                        INPUT tt-dados.titulo,
                                        INPUT tt-dados.especie,
                                        INPUT tt-dados.cod_empresa,
                                        INPUT tt-dados.valor_total,
                                        INPUT tt-dados.dt_vencto).
    
                  FIND fornec_financ NO-LOCK 
                      WHERE fornec_financ.cod_empresa    = tt-dados.cod_empresa 
                      AND   fornec_financ.cdn_fornecedor = INT(trim(tt-dados.pessoa)) NO-ERROR.
                  IF NOT AVAIL fornec_financ THEN
                       RUN pi_gera_log (INPUT INT(trim(tt-dados.pessoa)),
                                        INPUT "ERRO",
                                        INPUT tt-dados.cod_estab, 
                                        INPUT tt-dados.sequence,
                                        INPUT "Fornecedor Financeiro " + tt-dados.pessoa + " nÆo encontrado.",
                                        INPUT tt-dados.titulo,
                                        INPUT tt-dados.especie,
                                        INPUT tt-dados.cod_empresa,
                                        INPUT tt-dados.valor_total,
                                        INPUT tt-dados.dt_vencto).

                  ELSE ASSIGN tt-dados.portador   = fornec_financ.cod_portador WHEN tt-dados.portador = ""
                              tt-dados.tipo-fluxo = fornec_financ.cod_tip_fluxo_financ WHEN AVAIL fornec_financ.
                  
            END. /* IF TRIM(EN*/
        END.
        
        INPUT CLOSE.
    END.
END.


PROCEDURE pi_titulos:

   /* Separa os t¡tulos do rateio e acerta a esp‚cie de cada um conforme o tipo de retorno - tt-dados.tipo */
   FOR EACH tt-dados BREAK BY tt-dados.pessoa
                           BY tt-dados.cod_estab
                           BY tt-dados.tipo
                           BY tt-dados.especie
                           BY tt-dados.titulo
                           BY tt-dados.valor:

        IF FIRST-OF(tt-dados.titulo) THEN DO:
            
            /* Para a transa‡Æo de dep¢sito verifica se j  existe um movimento identico efetuado no t¡tulo - para evitar duplicidades */
            IF NOT CAN-FIND(FIRST tt_log 
                         WHERE tt_log.cdn_fornecedor = INT(TRIM(tt-dados.pessoa))
                           AND tt_log.titulo         = tt-dados.titulo
                           AND tt_log.cod_esp        = tt-dados.especie) THEN DO:

                 /* Cria titulo */
                 FIND tt_titulo NO-LOCK 
                     WHERE tt_titulo.cod_empresa     = tt-dados.cod_empresa
                     AND   tt_titulo.cod_estab       = tt-dados.cod_estab
                     AND   tt_titulo.cdn_fornec      = INT(TRIM(tt-dados.pessoa))
                     AND   tt_titulo.cod_tit_ap      = tt-dados.titulo
                     AND   tt_titulo.cod_ser_docto   = "1"
                     AND   tt_titulo.cod_espec_docto = tt-dados.especie
                     AND   tt_titulo.cod_parcela     = tt-dados.parcela 
                     AND   tt_titulo.tipo            = tt-dados.tipo NO-ERROR.
                 IF NOT AVAIL tt_titulo THEN DO:
    
                     CREATE tt_titulo.
                     ASSIGN tt_titulo.cod_empresa     = tt-dados.cod_empresa
                            tt_titulo.cod_estab       = tt-dados.cod_estab              
                            tt_titulo.cdn_fornec      = INT(trim(tt-dados.pessoa))     
                            tt_titulo.cod_tit_ap      = tt-dados.titulo                 
                            tt_titulo.cod_ser_docto   = "1"                 
                            tt_titulo.cod_espec_docto = tt-dados.especie
                            tt_titulo.cod_parcela     = tt-dados.parcela
                            tt_titulo.cod_refer       = tt-dados.cod_refer
                            tt_titulo.cod_portador    = tt-dados.portador
                            tt_titulo.cod_carteira    = tt-dados.carteira
                            tt_titulo.dt_trans        = (IF tt-dados.especie = "AD" THEN tt-dados.dt_depos ELSE TODAY) 
                                                        /* Para efeitos da concilia‡Æo banc ria os dep¢sitos usam da data de transa‡Æo igual da data do dep¢sito */
                            tt_titulo.valor           = tt-dados.valor_total 
                            tt_titulo.dt_emissao      = tt-dados.dt_emissao  
                            tt_titulo.dt_vencto       = tt-dados.dt_vencto
                            tt_titulo.sequencia       = INT(tt-dados.sequence)
                            tt_titulo.cod_indic_econ  = histor_finalid_econ.cod_indic_econ WHEN AVAIL histor_finalid_econ
                            tt_titulo.cod_tip_fluxo   = fornec_financ.cod_tip_fluxo_financ WHEN AVAIL fornec_financ
                            tt_titulo.tipo            = tt-dados.tipo /* 1-Antecip, 2-devolucao $, 3-reembolso */
                            tt_titulo.historico       = tt-dados.historico.
                 END.

                 ASSIGN d-vl-tot = 0.
            /* felipe */
            END.
            /* felipe */
                 /* Cria rateios */
                 blk:
                 FOR EACH b-tt-dados 
                     WHERE b-tt-dados.cod_estab = tt-dados.cod_estab
                     AND   b-tt-dados.pessoa    = tt-dados.pessoa
                     AND   b-tt-dados.titulo    = tt-dados.titulo
                     AND   b-tt-dados.especie   = tt-dados.especie
                     AND   b-tt-dados.tipo      = tt-dados.tipo
                     AND   b-tt-dados.cod_refer = tt-dados.cod_refer:


                    /*** As linhas negativas sÆo as presta‡äes de contas das antecipa‡äes - elas servem para 
                         identificar a antecipa‡Æo e posteriormente serÆo descartadas */ 
                    IF b-tt-dados.vl_rateio < 0 OR b-tt-dados.tipo = 2 THEN DO:

                       /* Observa‡Æo - Para os dep¢sitos que geram o movimento "acerto de valor a menor" no t¡tulo, caso seja reprocessado mais de uma vez ser  duplicado o movimento*/
                       FIND FIRST tit_ap NO-LOCK 
                            WHERE tit_ap.cod_empresa       = b-tt-dados.cod_empresa
                              AND tit_ap.cod_estab         = b-tt-dados.cod_estab
                              AND tit_ap.cdn_fornec        = tt_titulo.cdn_fornec
                              AND tit_ap.cod_ser_docto     = tt_titulo.cod_ser_docto
                              AND tit_ap.cod_espec_docto   = c-esp-antecip
                              AND tit_ap.cod_parcela       = tt_titulo.cod_parcela
                              AND tit_ap.val_sdo_tit_ap    >= (IF b-tt-dados.tipo = 2 THEN b-tt-dados.vl_rateio ELSE (b-tt-dados.vl_rateio * -1)) 
                              AND tit_ap.log_tit_ap_estordo = NO 
                              AND tit_ap.log_sdo_tit_ap     = YES NO-ERROR.
                        IF NOT AVAIL tit_ap AND tt-dados.especie <> "DP" /* NÆo considera para cartÆo de cr‚dito */THEN 
                            RUN pi_gera_log (INPUT INT(trim(tt-dados.pessoa)),
                                            INPUT "ERRO",
                                            INPUT tt_titulo.cod_estab, 
                                            INPUT b-tt-dados.sequence,
                                            INPUT "NÆo foi encontrado o t¡tulo de antecipa‡Æo para a presta‡Æo de contas. T¡tulo: " + b-tt-dados.titulo + " Funcion rio: " + b-tt-dados.pessoa + " Esp: " + tt-dados.especie,
                                            INPUT tt-dados.titulo,
                                            INPUT tt-dados.especie,
                                            INPUT b-tt-dados.cod_empresa,
                                            INPUT tt-dados.valor_total,
                                            INPUT tt-dados.dt_vencto).
                        ELSE ASSIGN tt_titulo.cod_tit_antecip = tit_ap.cod_tit_ap WHEN AVAIL tit_ap
                                    tt_titulo.r_row_antecip   = ROWID(tit_ap) WHEN AVAIL tit_ap.
                             
                        /* Valida‡Æo necess ria para nÆo importar os valores negativos no rateio, por‚m se for conta de ativo e valor positivo ‚ um dep¢sito */
                        IF b-tt-dados.vl_rateio < 0 THEN NEXT blk.
                    END.

                    FIND tt_rateio 
                         WHERE tt_rateio.cod_estab  = tt-dados.cod_estab
                         AND   tt_rateio.cdn_fornec = int(tt-dados.pessoa)
                         AND   tt_rateio.titulo     = tt-dados.titulo
                         AND   tt_rateio.especie    = tt-dados.especie
                         AND   tt_rateio.ccontabil  = b-tt-dados.ccontabil
                         AND   tt_rateio.ccusto     = b-tt-dados.ccusto NO-ERROR.
                     IF NOT AVAIL tt_rateio THEN DO:
                           
                           CREATE tt_rateio.
                           ASSIGN tt_rateio.cod_estab           = tt-dados.cod_estab
                                  tt_rateio.cdn_fornec          = int(tt-dados.pessoa)
                                  tt_rateio.titulo              = tt-dados.titulo
                                  tt_rateio.especie             = tt-dados.especie
                                  tt_rateio.sequencia           = INT(b-tt-dados.sequence)
                                  tt_rateio.cod_plano_cta_ctbl  = c_plano_conta
                                  tt_rateio.ccontabil           = b-tt-dados.ccontabil 
                                  tt_rateio.cod_tip_fluxo       = tt_titulo.cod_tip_fluxo
                                  tt_rateio.ccusto              = b-tt-dados.ccusto
                                  tt_rateio.lancto              = b-tt-dados.lancto  
                                  tt_rateio.unid_neg            = SUBSTR(b-tt-dados.ccontabil,9,2).
                     END.
                     ASSIGN tt_rateio.valor = tt_rateio.valor + b-tt-dados.vl_rateio
                            d-vl-tot        = d-vl-tot + b-tt-dados.vl_rateio.
                               
                 END. /* for each tt-dados */
    
                 ASSIGN tt_titulo.valor = d-vl-tot. 

/*             END. /* Not can-find tt-erro */ */
        END.  /* last-of */
   END.  /* for each tt-dados */
END.


/*** Cria os t¡tulos de antecipa‡äes ***/
PROCEDURE pi_integra_antecip:
    
    DEFINE VARIABLE d-cotacao AS DECIMAL     NO-UNDO.
 
    FOR EACH tt_titulo WHERE
             tt_titulo.tipo = 1 
                             BY tt_titulo.cod_empresa
                             BY tt_titulo.cod_estab
                             BY tt_titulo.cdn_fornec
                             BY tt_titulo.cod_tit_ap:


      /* Se j  ocorreu um erro nÆo continua o processo */
        IF CAN-FIND(FIRST tt_log 
                         WHERE tt_log.cdn_fornecedor = tt_titulo.cdn_fornec
                           AND tt_log.titulo         = tt_titulo.cod_tit_ap
                           AND tt_log.cod_esp        = tt_titulo.cod_espec_docto) THEN NEXT.


        /* elimina as tabelas tempor rias */
        EMPTY TEMP-TABLE tt_log_erros_atualiz.
        empty TEMP-TABLE tt_integr_apb_antecip_pef_p1.
        empty TEMP-TABLE tt_integr_apb_antecip_pef_pend.
        EMPTY TEMP-TABLE tt_integr_apb_aprop_ctbl_pend.
        EMPTY TEMP-TABLE tt_integr_apb_abat_prev_provis.
        EMPTY TEMP-TABLE tt_integr_apb_impto_impl_pend.
        EMPTY TEMP-TABLE tt_integr_apb_aprop_ctbl_pend.
        EMPTY TEMP-TABLE tt_1099.
        EMPTY TEMP-TABLE tt_ord_compra_tit_ap_pend_1.
        EMPTY TEMP-TABLE tt_integr_apb_abat_prev_provis.

        IF VALID-HANDLE(v_hdl_aux) THEN
            DELETE PROCEDURE v_hdl_aux.

        IF VALID-HANDLE(v_hdl_aux) THEN
            DELETE OBJECT v_hdl_aux.

        /*---------- Cria Referˆncia ----------*/
        
        assign l_log_refer_unica = no
               c_cod_refer       = ''.
        repeat while NOT l_log_refer_unica:
            run pi_retorna_sugestao_referencia(input "M",
                                               input today,
                                               output c_cod_refer) /*pi_retorna_sugestao_referencia*/.
    
            ASSIGN l_log_refer_unica = fncLogReferUnicaAntecip(tt_titulo.cod_estab, c_cod_refer, "", ?).
        END.
        
        RUN pi-acompanhar in h-acomp(INPUT "Antecipa‡Æo Seq/Titulo " + STRING(tt_titulo.sequencia) + "/" + tt_titulo.cod_tit_ap).
        
        CREATE tt_integr_apb_antecip_pef_p1.
        ASSIGN tt_integr_apb_antecip_pef_p1.tta_cod_empresa                 = tt_titulo.cod_empresa
               tt_integr_apb_antecip_pef_p1.tta_cod_estab                   = tt_titulo.cod_estab
               tt_integr_apb_antecip_pef_p1.tta_cod_refer                   = c_cod_refer
               tt_integr_apb_antecip_pef_p1.tta_cod_espec_docto             = tt_titulo.cod_espec_docto
               tt_integr_apb_antecip_pef_p1.tta_cod_ser_docto               = tt_titulo.cod_ser_docto
               tt_integr_apb_antecip_pef_p1.tta_cdn_fornecedor              = tt_titulo.cdn_fornec
               tt_integr_apb_antecip_pef_p1.tta_cod_tit_ap                  = tt_titulo.cod_tit_ap
               tt_integr_apb_antecip_pef_p1.tta_cod_parcela                 = tt_titulo.cod_parcela
               tt_integr_apb_antecip_pef_p1.tta_cod_portador                = c-portador 
               tt_integr_apb_antecip_pef_p1.tta_cod_indic_econ              = tt_titulo.cod_indic_econ
               tt_integr_apb_antecip_pef_p1.tta_val_tit_ap                  = tt_titulo.valor
               tt_integr_apb_antecip_pef_p1.tta_val_cotac_indic_econ        = 1
               tt_integr_apb_antecip_pef_p1.tta_dat_emis_docto              = tt_titulo.dt_emiss
               tt_integr_apb_antecip_pef_p1.tta_dat_vencto_tit_ap           = tt_titulo.dt_vencto
               tt_integr_apb_antecip_pef_p1.tta_ind_tip_refer               = "Antecipa‡Æo"
               tt_integr_apb_antecip_pef_p1.tta_des_text_histor             = tt_titulo.historico
               tt_integr_apb_antecip_pef_p1.tta_ind_natur_cta_ctbl          = (IF tt_titulo.lancto = "+" THEN "DB" ELSE "CR")
               tt_integr_apb_antecip_pef_p1.tta_cod_usuar_gerac_movto       = v_cod_usuar_corren
               tt_integr_apb_antecip_pef_p1.ttv_rec_antecip_pef_pend        = RECID(tt_integr_apb_antecip_pef_p1)
               tt_integr_apb_antecip_pef_p1.tta_ind_origin_tit_ap           = "APB"
               tt_integr_apb_antecip_pef_p1.tta_cod_cart_bcia               = c-carteira
               tt_integr_apb_antecip_pef_p1.tta_num_talon_cheq              = 0
               tt_integr_apb_antecip_pef_p1.tta_num_cheque                  = 0
               tt_integr_apb_antecip_pef_p1.tta_ind_favorec_cheq            = ""
               tt_integr_apb_antecip_pef_p1.tta_nom_favorec_cheq            = "".

        create tt_integr_apb_aprop_ctbl_pend.
        assign tt_integr_apb_aprop_ctbl_pend.ttv_rec_integr_apb_item_lote   = ?
               tt_integr_apb_aprop_ctbl_pend.ttv_rec_antecip_pef_pend       = recid(tt_integr_apb_antecip_pef_p1)
               tt_integr_apb_aprop_ctbl_pend.ttv_rec_integr_apb_impto_pend  = recid(tt_integr_apb_impto_impl_pend)  WHEN AVAIL tt_integr_apb_impto_impl_pend
               tt_integr_apb_aprop_ctbl_pend.tta_cod_unid_negoc             = "00"
               tt_integr_apb_aprop_ctbl_pend.tta_cod_tip_fluxo_financ       = tt_titulo.cod_tip_fluxo
               tt_integr_apb_aprop_ctbl_pend.tta_val_aprop_ctbl             = tt_titulo.valor
               tt_integr_apb_aprop_ctbl_pend.tta_cod_pais                   = "BRA"
               tt_integr_apb_aprop_ctbl_pend.tta_cod_unid_federac           = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_imposto                = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_classif_impto          = ""
               tt_integr_apb_aprop_ctbl_pend.ttv_cod_tip_fluxo_financ_ext   = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_cta_ctbl_ext           = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_sub_cta_ctbl_ext       = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_ccusto_ext             = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_unid_negoc_ext         = "".


        /* Necess rio fazer este assing pois a API se perde quando no mesmo processamento h  v rias empresas */
        ASSIGN v_cod_empres_usuar = tt_titulo.cod_empresa.
        
        RUN prgfin/apb/apb905zd.py PERSISTENT SET v_hdl_aux.

        RUN pi_main_block_antecip_pef_pend_2 IN v_hdl_aux (input 3,
                                                           input "EMS2", /*p_cod_matriz_trad_org_ext*/
                                                           input-output table tt_integr_apb_antecip_pef_p1,
                                                           input table tt_integr_apb_aprop_ctbl_pend,
                                                           input table tt_integr_apb_impto_impl_pend,
                                                           input table tt_integr_apb_abat_prev_provis,
                                                           output table tt_log_erros_atualiz,
                                                           input table tt_1099,
                                                           input table tt_ord_compra_tit_ap_pend_1).
        IF VALID-HANDLE(v_hdl_aux) THEN
            DELETE OBJECT v_hdl_aux.

        IF VALID-HANDLE(v_hdl_aux) THEN
            DELETE PROCEDURE v_hdl_aux.

        /* Atualiza log de erros e tela de consulta de log */
        IF CAN-FIND(FIRST tt_log_erros_atualiz) THEN DO:

           FOR EACH tt_log_erros_atualiz NO-LOCK:

                 RUN pi_gera_log (INPUT tt_titulo.cdn_fornec,
                                  INPUT "ERRO",
                                  INPUT tt_titulo.cod_estab,
                                  INPUT tt_titulo.sequencia,
                                  INPUT tt_log_erros_atualiz.ttv_des_msg_erro + " " + tt_log_erros_atualiz.ttv_des_msg_ajuda,
                                  INPUT tt_titulo.cod_tit_ap,
                                  INPUT tt_titulo.cod_esp,
                                  INPUT tt_titulo.cod_empresa,
                                  INPUT tt_titulo.valor,
                                  INPUT tt_titulo.dt_vencto).
           END.
        END.
        ELSE DO:

             /* Criar tabela de log */
             RUN pi_gera_log (INPUT tt_titulo.cdn_fornec,
                              INPUT "OK",
                              INPUT tt_titulo.cod_estab,
                              INPUT tt_titulo.sequencia,
                              INPUT "T¡tulo implantado com sucesso",
                              INPUT tt_titulo.cod_tit_ap,
                              INPUT tt_titulo.cod_esp,
                              INPUT tt_titulo.cod_empresa,
                              INPUT tt_titulo.valor,
                              INPUT tt_titulo.dt_vencto).
        END.

        release tt_integr_apb_antecip_pef_p1.
        release tt_integr_apb_antecip_pef_pend.
        release tt_integr_apb_abat_prev_provis.
        release tt_integr_apb_impto_impl_pend.
        release tt_integr_apb_aprop_ctbl_pend.
        release tt_log_erros_atualiz.
        release tt_1099.
        release tt_ord_compra_tit_ap_pend_1.
        
    END.

    
END PROCEDURE.

/* Gera t¡tulos de Reembolso */
PROCEDURE pi_integra_apb:
    
    FOR EACH tt_titulo WHERE
             tt_titulo.tipo = 3
                    BY tt_titulo.cod_empresa
                    BY tt_titulo.cod_estab
                    BY tt_titulo.cdn_fornec
                    BY tt_titulo.cod_tit_ap:

        /* Se j  ocorreu um erro nÆo continua o processo */
        IF CAN-FIND(FIRST tt_log 
                         WHERE tt_log.cdn_fornecedor = tt_titulo.cdn_fornec
                           AND tt_log.titulo         = tt_titulo.cod_tit_ap
                           AND tt_log.cod_esp        = tt_titulo.cod_espec_docto) THEN NEXT.

        /* Cria a capa do lote */
        empty temp-table tt_integr_apb_abat_antecip_vouc.
        empty temp-table tt_integr_apb_abat_prev_provis.
        empty temp-table tt_integr_apb_aprop_ctbl_pend.
        empty temp-table tt_integr_apb_aprop_relacto.
        empty temp-table tt_integr_apb_impto_impl_pend.
        empty temp-table tt_integr_apb_item_lote_impl3v.
        empty temp-table tt_integr_apb_lote_impl.
        empty temp-table tt_integr_apb_relacto_pend.
        empty temp-table tt_log_erros_atualiz.
        empty temp-table tt_integr_apb_item_lote_impl.
        empty temp-table tt_params_generic_api.
        empty temp-table tt_integr_apb_relacto_pend_aux.
        empty temp-table tt_integr_apb_aprop_relacto_2.
        empty temp-table tt_docum_est_esoc_api.
        empty temp-table tt_item_doc_est_esoc_api.
        empty temp-table tt_integr_apb_nota_pend_cart.
        EMPTY TEMP-TABLE tt_vinc_an_x_tit.
        EMPTY TEMP-TABLE tt_erro_msg.
        
        /* Calcula c¢digo de referencia */
        ASSIGN l_log_refer_unica = no
               c_cod_refer       = ''
               d_vl_tot_lote     = 0.

        REPEAT WHILE NOT l_log_refer_unica:
            RUN pi_retorna_sugestao_referencia(INPUT "M",
                                               INPUT TODAY,
                                               OUTPUT c_cod_refer) /*pi_retorna_sugestao_referencia*/.
        
            ASSIGN l_log_refer_unica = fncLogReferUnicaNormal(tt_titulo.cod_estab, c_cod_refer, '', ?). /* Funcao na include ap001-rp1.i*/
        END.
        
        ASSIGN d_vl_tot_lote = tt_titulo.valor.

        RUN pi-acompanhar IN h-acomp(INPUT "Seq/T¡tulo " + STRING(tt_titulo.sequencia) + "/" + tt_titulo.cod_tit_ap).

        /* Capa do Lote */
        CREATE tt_integr_apb_lote_impl.
        ASSIGN tt_integr_apb_lote_impl.tta_cod_estab                = tt_titulo.cod_estab               
               tt_integr_apb_lote_impl.tta_cod_refer                = c_cod_refer
               tt_integr_apb_lote_impl.tta_dat_transacao            = tt_titulo.dt_trans
               tt_integr_apb_lote_impl.tta_ind_origin_tit_ap        = "APB"
               tt_integr_apb_lote_impl.tta_val_tot_lote_impl_tit_ap = d_vl_tot_lote
               tt_integr_apb_lote_impl.tta_cod_empresa              = tt_titulo.cod_empresa
               tt_integr_apb_lote_impl.tta_cod_indic_econ           = tt_titulo.cod_indic_econ
               tt_integr_apb_lote_impl.tta_val_tot_lote_impl_tit_ap = tt_titulo.valor.

        /* Itens do Lote */
        CREATE tt_integr_apb_item_lote_impl3v.
        ASSIGN tt_integr_apb_item_lote_impl3v.ttv_rec_integr_apb_lote_impl  = RECID(tt_integr_apb_lote_impl)
               tt_integr_apb_item_lote_impl3v.tta_num_seq_refer             = tt_titulo.sequencia
               tt_integr_apb_item_lote_impl3v.tta_cdn_fornecedor            = tt_titulo.cdn_fornec
               tt_integr_apb_item_lote_impl3v.tta_cod_espec_docto           = tt_titulo.cod_espec_docto
               tt_integr_apb_item_lote_impl3v.tta_cod_ser_docto             = tt_titulo.cod_ser_docto
               tt_integr_apb_item_lote_impl3v.tta_cod_tit_ap                = tt_titulo.cod_tit_ap
               tt_integr_apb_item_lote_impl3v.tta_cod_parcela               = tt_titulo.cod_parcela
               tt_integr_apb_item_lote_impl3v.tta_dat_emis_docto            = tt_titulo.dt_emissao
               tt_integr_apb_item_lote_impl3v.tta_dat_vencto_tit_ap         = tt_titulo.dt_vencto
               tt_integr_apb_item_lote_impl3v.tta_dat_prev_pagto            = tt_titulo.dt_vencto
               tt_integr_apb_item_lote_impl3v.tta_cod_indic_econ            = tt_titulo.cod_indic_econ
               tt_integr_apb_item_lote_impl3v.tta_val_tit_ap                = tt_titulo.valor
               tt_integr_apb_item_lote_impl3v.tta_cod_portador              = tt_titulo.cod_portador
               tt_integr_apb_item_lote_impl3v.tta_cod_cart_bcia             = tt_titulo.cod_carteira
               tt_integr_apb_item_lote_impl3v.tta_des_text_histor           = tt_titulo.historico
               tt_integr_apb_item_lote_impl3v.ttv_log_gerad                 = NO 
               tt_integr_apb_item_lote_impl3v.tta_cod_forma_pagto           = '001'. /* Validar a forma de pagamento */

        CREATE tt_integr_apb_item_lote_impl.
        BUFFER-COPY tt_integr_apb_item_lote_impl3v TO tt_integr_apb_item_lote_impl.
        ASSIGN  tt_integr_apb_item_lote_impl3v.ttv_rec_integr_apb_item_lote  = RECID( tt_integr_apb_item_lote_impl3v).

        /* Se for presta‡Æo de contas de antecipa‡Æo cria a tabela para descontar os valores do reembolso da antecip */
        IF tt_titulo.cod_tit_antecip <> "" THEN DO:

            FIND tit_ap NO-LOCK WHERE
                 ROWID(tit_ap) = tt_titulo.r_row_antecip NO-ERROR.
            IF NOT AVAIL tit_ap THEN NEXT.

            CREATE tt_integr_apb_abat_antecip_vouc.
            ASSIGN tt_integr_apb_abat_antecip_vouc.ttv_rec_integr_apb_item_lote = RECID(tt_integr_apb_lote_impl)
                   tt_integr_apb_abat_antecip_vouc.tta_cod_estab                = tit_ap.cod_estab
                   tt_integr_apb_abat_antecip_vouc.tta_cod_espec_docto          = tit_ap.cod_espec_docto
                   tt_integr_apb_abat_antecip_vouc.tta_cod_ser_docto            = tit_ap.cod_ser_docto
                   tt_integr_apb_abat_antecip_vouc.tta_cdn_fornecedor           = tit_ap.cdn_fornec
                   tt_integr_apb_abat_antecip_vouc.tta_cod_tit_ap               = tit_ap.cod_tit_ap
                   tt_integr_apb_abat_antecip_vouc.tta_cod_parcela              = tit_ap.cod_parcela
                   tt_integr_apb_abat_antecip_vouc.tta_val_abat_tit_ap          = tt_titulo.valor.
        END.

        /* Rateio Cont bil */
        FOR EACH tt_rateio
          WHERE tt_rateio.cdn_fornec = tt_titulo.cdn_fornec
          AND   tt_rateio.cod_estab  = tt_titulo.cod_estab
          AND   tt_rateio.titulo     = tt_titulo.cod_tit_ap
          AND   tt_rateio.especie    = tt_titulo.cod_espec_docto:

            CREATE tt_integr_apb_aprop_ctbl_pend.
            ASSIGN tt_integr_apb_aprop_ctbl_pend.ttv_rec_integr_apb_item_lote  = RECID(tt_integr_apb_item_lote_impl3V)
                   tt_integr_apb_aprop_ctbl_pend.ttv_rec_antecip_pef_pend      = ?
                   tt_integr_apb_aprop_ctbl_pend.ttv_rec_integr_apb_impto_pend = ?
                   tt_integr_apb_aprop_ctbl_pend.tta_cod_plano_cta_ctbl        = tt_rateio.cod_plano_cta_ctbl
                   tt_integr_apb_aprop_ctbl_pend.tta_cod_cta_ctbl              = tt_rateio.ccontabil
                   tt_integr_apb_aprop_ctbl_pend.tta_cod_unid_negoc            = "00"
                   tt_integr_apb_aprop_ctbl_pend.tta_cod_plano_ccusto          = c-plano-ccusto 
                   tt_integr_apb_aprop_ctbl_pend.tta_cod_ccusto                = tt_rateio.ccusto
                   tt_integr_apb_aprop_ctbl_pend.tta_cod_tip_fluxo_financ      = tt_rateio.cod_tip_fluxo
                   tt_integr_apb_aprop_ctbl_pend.tta_val_aprop_ctbl            = tt_rateio.valor.
        END.

        IF NOT VALID-HANDLE(v_hdl_aux) THEN
            RUN prgfin/apb/apb900zg.py PERSISTENT SET v_hdl_aux.

        IF VALID-HANDLE(v_hdl_aux) THEN
        RUN pi_main_block_api_tit_ap_cria_9 IN v_hdl_aux (INPUT 9,
                                                          input "GERAL", /*Cod. Matriz Tradu‡Æo Externa - Qdo Branco ser  usado do parƒmetro do APB */
                                                          input-output table tt_integr_apb_item_lote_impl3v,
                                                          input-output table tt_params_generic_api,
                                                          input table tt_integr_apb_relacto_pend_aux,
                                                          input table tt_integr_apb_aprop_relacto_2,
                                                          input table tt_docum_est_esoc_api,
                                                          input table tt_item_doc_est_esoc_api,
                                                          input table tt_integr_apb_nota_pend_cart).
        IF VALID-HANDLE(v_hdl_aux) THEN      
            DELETE OBJECT v_hdl_aux NO-ERROR.

        /* Atualiza log de erros e tela de consulta de log */
        IF CAN-FIND(FIRST tt_log_erros_atualiz) THEN DO:

            FOR EACH tt_log_erros_atualiz NO-LOCK:

                 RUN pi_gera_log (INPUT tt_titulo.cdn_fornec,
                                  INPUT "ERRO",
                                  INPUT tt_titulo.cod_estab,
                                  INPUT tt_titulo.sequencia,
                                  INPUT tt_log_erros_atualiz.ttv_des_msg_erro + " " + tt_log_erros_atualiz.ttv_des_msg_ajuda,
                                  INPUT tt_titulo.cod_tit_ap,
                                  INPUT tt_titulo.cod_esp,
                                  INPUT tt_titulo.cod_empresa,
                                  INPUT tt_titulo.valor,
                                  INPUT tt_titulo.dt_vencto).
            END.
        END.
        ELSE DO:

            FIND FIRST tt_integr_apb_item_lote_impl3v NO-ERROR.
            /* Criar tabela de log */
             RUN pi_gera_log (INPUT tt_titulo.cdn_fornec,
                              INPUT "OK",
                              INPUT tt_titulo.cod_estab,
                              INPUT tt_titulo.sequencia,
                              INPUT "T¡tulo implantado com sucesso",
                              INPUT tt_titulo.cod_tit_ap,
                              INPUT tt_titulo.cod_esp,
                              INPUT tt_titulo.cod_empresa,
                              INPUT tt_titulo.valor,
                              INPUT tt_titulo.dt_vencto).


             /* inicio Vincula tit x antecip */
             IF tt_titulo.cod_tit_antecip <> "" AND AVAIL tit_ap THEN DO:
                  
                   /* Cria tabelas para a vincula‡Æo do titulo a antecipa‡Æo */
                  CREATE tt_vinc_an_x_tit.
                  ASSIGN tt_vinc_an_x_tit.ttv_cod_estab_ant    = tit_ap.cod_estab
                         tt_vinc_an_x_tit.ttv_num_id_ant       = tit_ap.num_id_tit_ap
                         tt_vinc_an_x_tit.ttv_cod_estab_tit_ap = tt_titulo.cod_estab  
                         tt_vinc_an_x_tit.ttv_num_id_tit_ap    = tt_integr_apb_item_lote_impl3v.tta_num_id_tit_ap
                         tt_vinc_an_x_tit.ttv_val_vincul       = (IF tt_titulo.valor > tit_ap.val_sdo_tit_ap THEN tit_ap.val_sdo_tit_ap ELSE tt_titulo.valor)
                         tt_vinc_an_x_tit.ttv_val_cotac_indic_econ = 0
                         tt_vinc_an_x_tit.ttv_des_text_histor      = "".
    
                  IF VALID-HANDLE(v_hdl_aux) THEN
                      DELETE OBJECT v_hdl_aux NO-ERROR.
    
                  RUN prgfin/apb/apb532za.py persisten set v_hdl_aux.
    
                  RUN pi_main_code_api_vinc_an_x_tit_ap in v_hdl_aux (input 1,
                                                                      input tt_titulo.dt_trans,
                                                                      input table tt_vinc_an_x_tit,
                                                                      output table tt_erro_msg).
                  DELETE PROCEDURE v_hdl_aux.
    
                  IF CAN-FIND(FIRST tt_erro_msg) THEN DO:
    
                     RUN pi_gera_log (INPUT tt_titulo.cdn_fornec,
                                       INPUT "OK",
                                       INPUT tt_titulo.cod_estab,
                                       INPUT tt_titulo.sequencia,
                                       INPUT "NÆo foi poss¡vel vincular t¡tulo com a antecipa‡Æo. Vinculo dever  ser feito manualmente.",
                                       INPUT tt_titulo.cod_tit_ap,
                                       INPUT tt_titulo.cod_esp,
                                       INPUT tt_titulo.cod_empresa,
                                       INPUT tt_titulo.valor,
                                       INPUT tt_titulo.dt_vencto).

        
                     FOR EACH tt_erro_msg:
        
                         RUN pi_gera_log (INPUT tt_titulo.cdn_fornec,
                                          INPUT "OK",
                                          INPUT tt_titulo.cod_estab,
                                          INPUT tt_titulo.sequencia,
                                          INPUT tt_erro_msg.ttv_des_msg_erro,
                                          INPUT tt_titulo.cod_tit_ap,
                                          INPUT tt_titulo.cod_esp,
                                          INPUT tt_titulo.cod_empresa,
                                          INPUT tt_titulo.valor,
                                          INPUT tt_titulo.dt_vencto).

                     END.
                  END.
             END. 
             /* Fim Vincula Antecip x Titulo */
        END.
    END.
END.


/*** Cria os t¡tulos de antecipa‡äes ***/
PROCEDURE pi_integra_devolucao:
    
    DEFINE VARIABLE d-cotacao AS DECIMAL     NO-UNDO.

    FOR EACH tt_titulo WHERE
             tt_titulo.tipo = 2
                             BY tt_titulo.cod_empresa
                             BY tt_titulo.cod_estab
                             BY tt_titulo.cdn_fornec
                             BY tt_titulo.cod_tit_ap:

        
        /* Se j  ocorreu um erro nÆo continua o processo */
        IF CAN-FIND(FIRST tt_log 
                         WHERE tt_log.cdn_fornecedor = tt_titulo.cdn_fornec
                           AND tt_log.titulo         = tt_titulo.cod_tit_ap
                           AND tt_log.cod_esp        = tt_titulo.cod_espec_docto) THEN NEXT.

        /* elimina as tabelas tempor rias */
        EMPTY TEMP-TABLE tt_params_generic_api.
        EMPTY TEMP-TABLE tt_log_erros_tit_ap_alteracao.
        EMPTY TEMP-TABLE tt_tit_ap_alteracao_rateio.
        EMPTY TEMP-TABLE tt_tit_ap_alteracao_base_aux_3.

        RUN pi-acompanhar in h-acomp(INPUT "Dep¢sito Seq/Titulo " + STRING(tt_titulo.sequencia) + "/" + tt_titulo.cod_tit_ap).

        /* Localiza o t¡tulo de antecipa‡Æo que ser  alterado */
        /* Se for presta‡Æo de contas de antecipa‡Æo cria a tabela para descontar os valores do reembolso da antecip */
        IF tt_titulo.cod_tit_antecip = "" THEN NEXT. 

        FIND tit_ap NO-LOCK WHERE
             ROWID(tit_ap) = tt_titulo.r_row_antecip NO-ERROR.    
        IF NOT AVAIL tit_ap THEN NEXT.
        
        /* Dados do t¡tulo para altera‡Æo */
        CREATE tt_tit_ap_alteracao_base_aux_3.
        ASSIGN tt_tit_ap_alteracao_base_aux_3.ttv_cod_usuar_corren    = v_cod_usuar_corren
               tt_tit_ap_alteracao_base_aux_3.tta_cod_EMPRESA         = tit_ap.cod_empresa
               tt_tit_ap_alteracao_base_aux_3.tta_cod_estab           = tit_ap.cod_estab
               tt_tit_ap_alteracao_base_aux_3.tta_num_id_tit_ap       = tit_ap.num_id_tit_ap
               tt_tit_ap_alteracao_base_aux_3.ttv_rec_tit_ap          = RECID(tit_ap)
               tt_tit_ap_alteracao_base_aux_3.tta_cdn_fornecedor      = tit_ap.cdn_fornec
               tt_tit_ap_alteracao_base_aux_3.tta_cod_espec_docto     = tit_ap.cod_espec_docto
               tt_tit_ap_alteracao_base_aux_3.tta_cod_ser_docto       = tit_ap.cod_ser_docto
               tt_tit_ap_alteracao_base_aux_3.tta_cod_tit_ap          = tit_ap.cod_tit_ap
               tt_tit_ap_alteracao_base_aux_3.tta_cod_parcela         = tit_ap.cod_parcela
               tt_tit_ap_alteracao_base_aux_3.ttv_dat_transacao       = TODAY 
               tt_tit_ap_alteracao_base_aux_3.tta_val_sdo_tit_ap      = tit_ap.val_sdo_tit_ap - tt_titulo.valor 
               tt_tit_ap_alteracao_base_aux_3.tta_dat_emis_docto      = tit_ap.dat_emis_docto
               tt_tit_ap_alteracao_base_aux_3.tta_dat_prev_pagto      = tit_ap.dat_prev_pagto
               tt_tit_ap_alteracao_base_aux_3.tta_dat_vencto_tit_ap   = tit_ap.dat_vencto_tit_ap
               tt_tit_ap_alteracao_base_aux_3.tta_cod_indic_econ      = tit_ap.cod_indic_econ
               tt_tit_ap_alteracao_base_aux_3.ttv_ind_motiv_alter_val_tit_ap = "BAIXA"
               tt_tit_ap_alteracao_base_aux_3.ttv_cod_portador_mov    = tt_titulo.cod_portador 
               tt_tit_ap_alteracao_base_aux_3.tta_des_histor_padr     = tt_titulo.historico
               tt_tit_ap_alteracao_base_aux_3.tta_ind_tip_espec_docto = "ANTECIPACAO".

        RUN prgfin/apb/apb767zf.py persistent set v_hdl_aux.

        RUN pi_main_code_api_integr_ap_alter_tit_ap_6 in v_hdl_aux (Input 1,  
                                                                   Input "APB",
                                                                   Input "",
                                                                   input NO,        
                                                                   input-output table tt_tit_ap_alteracao_base_aux_3,
                                                                   input-output table tt_tit_ap_alteracao_rateio,    
                                                                   input-output table tt_params_generic_api,         
                                                                   output table tt_log_erros_tit_ap_alteracao).
        DELETE OBJECT v_hdl_aux.

        /* Atualiza log de erros e tela de consulta de log */
        IF CAN-FIND(FIRST tt_log_erros_tit_ap_alteracao) THEN DO:

            FOR EACH tt_log_erros_tit_ap_alteracao NO-LOCK:

                 RUN pi_gera_log (INPUT tt_titulo.cdn_fornec,
                                  INPUT "ERRO",
                                  INPUT tt_titulo.cod_estab,
                                  INPUT tt_titulo.sequencia,
                                  INPUT tt_log_erros_tit_ap_alteracao.ttv_des_msg_erro + " " + tt_log_erros_tit_ap_alteracao.ttv_des_msg_ajuda_1,
                                  INPUT tt_titulo.cod_tit_ap,
                                  INPUT tt_titulo.cod_esp,
                                  INPUT tt_titulo.cod_empresa,
                                  INPUT tt_titulo.valor,
                                  INPUT tt_titulo.dt_vencto).

            END.
        END.
        ELSE DO:

            FIND FIRST tt_tit_ap_alteracao_base_aux_3 NO-ERROR.
            /* Criar tabela de log */
             RUN pi_gera_log (INPUT tt_titulo.cdn_fornec,
                              INPUT "OK",
                              INPUT tt_titulo.cod_estab,
                              INPUT tt_titulo.sequencia,
                              INPUT "T¡tulo implantado com sucesso",
                              INPUT tt_titulo.cod_tit_ap,
                              INPUT tt_titulo.cod_esp,
                              INPUT tt_titulo.cod_empresa,
                              INPUT tt_titulo.valor,
                              INPUT tt_titulo.dt_vencto).

        END.
        
        RELEASE tt_params_generic_api.           
        RELEASE tt_log_erros_tit_ap_alteracao.   
        RELEASE tt_tit_ap_alteracao_rateio.      
        RELEASE tt_tit_ap_alteracao_base_aux_3.  
    END.
END PROCEDURE.

PROCEDURE pi_tabela_log:
    
    DEFINE VARIABLE i-seq-it AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-seq-tit AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c-id     AS CHARACTER   NO-UNDO.
    DEF BUFFER b-tt_titulo FOR tt_titulo.

    FOR EACH tt_log BREAK BY tt_log.cod_estab
                          BY tt_log.cdn_fornecedor
                          BY tt_log.cod_esp
                          BY tt_log.titulo:

        IF FIRST-OF(tt_log.titulo) THEN DO:

            ASSIGN i-seq-it = 0
                   i-seq-tit = i-seq-tit + 1.
            ASSIGN c-id = STRING(MD5-DIGEST(tt_log.cod_esp + tt_log.titulo + STRING(TODAY) + STRING(TIME) + STRING(i-seq-tit))).

            FIND log_concur_titulo 
                WHERE log_concur_titulo.cdn_fornec  = tt_log.cdn_fornecedor
                AND   log_concur_titulo.cod_esp     = tt_log.cod_esp
                AND   log_concur_titulo.cod_estab   = tt_log.cod_estab
                AND   log_concur_titulo.titulo      = tt_log.titulo
                AND   log_concur_titulo.cod_empresa = tt_log.cod_empresa NO-ERROR.
            IF NOT AVAIL log_concur_titulo THEN DO:

                FIND b-tt_titulo 
                    WHERE b-tt_titulo.cdn_fornec  = tt_log.cdn_fornecedor
                    AND   b-tt_titulo.cod_esp     = tt_log.cod_esp       
                    AND   b-tt_titulo.cod_estab   = tt_log.cod_estab     
                    AND   b-tt_titulo.cod_tit_ap  = tt_log.titulo        
                    AND   b-tt_titulo.cod_empresa = tt_log.cod_empresa NO-ERROR.


                CREATE log_concur_titulo.
                ASSIGN log_concur_titulo.id          = c-id
                       log_concur_titulo.arquivo     = c-arquivo_imp
                       log_concur_titulo.cdn_fornec  = tt_log.cdn_fornecedor
                       log_concur_titulo.cod_esp     = tt_log.cod_esp
                       log_concur_titulo.cod_estab   = tt_log.cod_estab
                       log_concur_titulo.datahora    = NOW
                       log_concur_titulo.titulo      = tt_log.titulo
                       log_concur_titulo.cod_empresa = tt_log.cod_empresa
                       log_concur_titulo.tipo        = (IF tt_log.tipo = "ERRO" THEN 1 ELSE 2)
                       log_concur_titulo.dt_vencto   = tt_log.dt_vencto
                       LOG_concur_titulo.valor       = tt_log.valor.

                IF AVAIL b-tt_titulo AND b-tt_titulo.cod_tit_antecip <> "" THEN DO: 
                 
                    FIND tit_ap NO-LOCK WHERE
                             ROWID(tit_ap) = tt_titulo.r_row_antecip NO-ERROR.
                    ASSIGN LOG_concur_titulo.tit_antecip = tit_ap.num_id_tit_ap WHEN AVAIL tit_ap.
                 
                END.    
            END.
            ELSE ASSIGN log_concur_titulo.dt_repros     = TODAY 
                        log_concur_titulo.log_reprocess = YES 
                        log_concur_titulo.usuar_reproc  = c-seg-usuario
                        c-id                            = log_concur_titulo.id.

            FOR EACH LOG_concur_rateio WHERE
                     LOG_concur_rateio.id = log_concur_titulo.id:
                DELETE LOG_concur_rateio.
            END.

               FOR EACH tt_rateio
                   WHERE tt_rateio.cdn_fornec = tt_log.cdn_fornecedor
                     AND tt_rateio.cod_estab  = tt_log.cod_estab
                     AND tt_rateio.titulo     = tt_log.titulo
                     AND tt_rateio.especie    = tt_log.cod_esp:
            
                   ASSIGN i-seq-it = i-seq-it + 1.
            
                   FIND FIRST LOG_concur_rateio
                       WHERE LOG_concur_rateio.id = c-id
                       AND log_concur_rateio.seq  = i-seq-it NO-ERROR.
                   IF NOT AVAIL LOG_concur_rateio THEN DO:
            
                       CREATE log_concur_rateio.
                       ASSIGN log_concur_rateio.id  = c-id
                              log_concur_rateio.seq = i-seq-it.
                   END.
                   ASSIGN log_concur_rateio.ccusto = tt_rateio.ccusto
                          log_concur_rateio.conta  = tt_rateio.cconta
                          log_concur_rateio.lancto = tt_rateio.lancto
                          log_concur_rateio.valor  = tt_rateio.valor.
               END.
       
            ASSIGN i-seq-it = 0.
        END.

        ASSIGN i-seq-it = i-seq-it + 1.

        IF i-seq-it = 1 THEN DO:
            FOR EACH LOG_concur_erros EXCLUSIVE-LOCK
                 WHERE log_concur_erros.id = c-id:
          
                DELETE LOG_concur_erros.
          
            END.
        END.

        FIND LOG_concur_erros NO-LOCK
            WHERE log_concur_erros.tipo      = (IF tt_log.tipo = "ERRO" THEN 1 ELSE 2)
              AND log_concur_erros.descricao = tt_log.msg                                                              
              AND log_concur_erros.id        = c-id NO-ERROR.                                   
        
        IF NOT AVAIL LOG_concur_erros THEN DO:
             CREATE log_concur_erros.
             ASSIGN log_concur_erros.tipo      = (IF tt_log.tipo = "ERRO" THEN 1 ELSE 2)
                    log_concur_erros.descricao = tt_log.msg
                    log_concur_erros.linha     = i-seq-it
                    log_concur_erros.id        = c-id.
        END.
    END.
END.

PROCEDURE pi_gera_log:

    DEFINE INPUT  PARAMETER ipi-fornec AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipc-tipo   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipc-estab  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipc-linha  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipc-msg    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipc-titulo AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipc-espec  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipc-empresa AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipd-valor   AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipd-dtvenc  AS DATE    NO-UNDO.

    DEFINE VARIABLE r-id AS RAW NO-UNDO.

    CREATE tt_log.
    ASSIGN tt_log.cdn_fornecedor = ipi-fornec
           tt_log.cod_estab      = ipc-estab
           tt_log.tipo           = ipc-tipo
           tt_log.linha          = INT(ipc-linha)
           tt_log.msg            = ipc-msg
           tt_log.nom_arquivo    = c-arquivo_imp
           tt_log.datahora       = NOW 
           tt_log.titulo         = ipc-titulo
           tt_log.batch_id       = c-batch
           tt_log.cod_esp        = ipc-espec
           tt_log.cod_empresa    = ipc-empresa
           tt_log.valor          = ipd-valor
           tt_log.dt_vencto      = ipd-dtvenc.
END.

PROCEDURE pi_retorna_sugestao_referencia:

    /************************ Parameter Definition Begin ************************/

    def Input param p_ind_tip_atualiz
        as character
        format "X(08)"
        no-undo.
    def Input param p_dat_refer
        as date
        format "99/99/9999"
        no-undo.
    def output param p_cod_refer
        as character
        format "x(10)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_des_dat                        as character       no-undo. /*local*/
    def var v_num_aux                        as integer         no-undo. /*local*/
    def var v_num_aux_2                      as integer         no-undo. /*local*/
    def var v_num_cont                       as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign v_des_dat   = string(p_dat_refer,"99999999")
           p_cod_refer = substring(v_des_dat,7,2)
                       + substring(v_des_dat,3,2)
                       + substring(v_des_dat,1,2)
                       + substring(p_ind_tip_atualiz,1,1)
           v_num_aux_2 = integer(this-procedure:handle).

    DO v_num_cont = 1 to 3:
        ASSIGN v_num_aux   = (random(0,v_num_aux_2) mod 26) + 97
               p_cod_refer = p_cod_refer + chr(v_num_aux).
    END.
END PROCEDURE. /* pi_retorna_sugestao_referencia */

PROCEDURE pi-relatorio:

    VIEW FRAME f-cabec.
    VIEW FRAME f-rodape.

    FOR EACH tt_log BREAK BY tt_log.nom_arquivo 
                          BY tt_log.tipo 
                          BY tt_log.datahora 
                          BY tt_log.cod_empresa 
                          BY tt_log.cod_estab 
                          BY tt_log.cdn_fornecedor:

        IF FIRST-OF(tt_log.tipo)
             THEN PUT UNFORMATTED 
                  (IF tt_log.tipo = "ERRO" THEN "Sequˆncias com erro nÆo importadas "
                        ELSE "Sequˆncias importadas com sucesso.")  " Data/Hora: " tt_log.datahora SKIP
                            " Arquivo: " tt_log.nom_arquivo FORMAT "x(100)" SKIP(01).

        FIND fornecedor NO-LOCK 
            WHERE fornecedor.cod_empresa = tt_log.cod_empresa 
             AND  fornecedor.cdn_fornec  = tt_log.cdn_fornecedor NO-ERROR.

        DISP tt_log.linha           COLUMN-LABEL "Seq"        FORMAT ">>9"
             tt_log.cod_empresa     COLUMN-LABEL "Emp"        FORMAT "X(03)"
             tt_log.cod_estab       COLUMN-LABEL "Est"        FORMAT "X(03)"
             tt_log.titulo          COLUMN-LABEL "T¡tulo"     FORMAT "X(08)"
             tt_log.cod_esp         COLUMN-LABEL "Esp"        FORMAT "X(03)"
             tt_log.cdn_fornecedor  COLUMN-LABEL "Fornecedor" FORMAT ">>>>>>>9"
             fornecedor.nom_pessoa  COLUMN-LABEL "Nome"       FORMAT "X(20)" WHEN AVAIL fornecedor
             tt_log.msg             COLUMN-LABEL "Mensagem"   FORMAT "x(100)" 
         WITH FRAME fdados DOWN WIDTH 200.

    END.
END.

/* Reprocessa os logs de Erro Selecionados pelo Concur */
PROCEDURE pi-reprocessa:
    DEFINE VARIABLE i-seq-it AS INTEGER NO-UNDO.

     FOR EACH tt-titulo WHERE
              tt-titulo.log_reprocess:

        /* Plano Centro de Custo */
        FIND FIRST plano_ccusto NO-LOCK 
           WHERE plano_ccusto.cod_empresa     = tt-titulo.cod_empresa
             AND plano_ccusto.dat_inic_valid <= TODAY 
             AND plano_ccusto.dat_fim_valid  >= TODAY NO-ERROR.
          IF NOT AVAIL plano_ccusto THEN 
                  RUN pi_gera_log (INPUT 0,
                                   INPUT "ERRO",
                                   INPUT "",
                                   INPUT 0,
                                   INPUT "Plano Centro de Custo nÆo encontrado!",
                                   INPUT "",
                                   INPUT "",
                                   INPUT estabelecimento.cod_empresa).
        ELSE ASSIGN c-plano-ccusto = plano_ccusto.cod_Plano_ccusto.
        
        FIND LOG_concur_titulo WHERE
             LOG_concur_titulo.id = tt-titulo.id NO-ERROR.
        IF AVAIL LOG_concur_titulo
             THEN ASSIGN log_concur_titulo.dt_repros = TODAY 
                         log_concur_titulo.log_reprocess = YES 
                         log_concur_titulo.usuar_reproc = c-seg-usuario.
        
        FOR EACH tt-rateio WHERE
                 tt-rateio.id = tt-titulo.id:

           CREATE tt_rateio.
           ASSIGN tt_rateio.cod_estab           = LOG_concur_titulo.cod_estab
                  tt_rateio.cdn_fornec          = LOG_concur_titulo.cdn_fornec
                  tt_rateio.titulo              = LOG_concur_titulo.titulo 
                  tt_rateio.especie             = LOG_concur_titulo.cod_esp
                  tt_rateio.sequencia           = tt-rateio.seq
                  tt_rateio.cod_plano_cta_ctbl  = c_plano_conta
                  tt_rateio.ccontabil           = tt-rateio.ccontabil
                  tt_rateio.cod_tip_fluxo       = fornec_financ.cod_tip_fluxo_financ WHEN AVAIL fornec_financ   
                  tt_rateio.cod_plano_ccusto    = c-plano-ccusto
                  tt_rateio.ccusto              = tt-rateio.ccusto
                  tt_rateio.lancto              = tt-rateio.lancto 
                  tt_rateio.unid_neg            = SUBSTR(tt-rateio.ccontabil,9,2)
                  tt_rateio.valor               = tt-rateio.valor
                  d-vl-tot                      = d-vl-tot + tt-rateio.valor.                
        END.

        ASSIGN i-seq-it = i-seq-it + 1.

        RUN pi-acompanhar in h-acomp(INPUT "Reprocessa Forn/Titulo " + STRING(tt-titulo.cdn_fornec) + "/" + tt-titulo.titulo).

        CREATE tt_titulo.                                                                                      
        ASSIGN tt_titulo.cod_empresa     = LOG_concur_titulo.cod_empresa 
               tt_titulo.cod_estab       = LOG_concur_titulo.cod_estab                                                  
               tt_titulo.cdn_fornec      = LOG_concur_titulo.cdn_fornec                                          
               tt_titulo.cod_tit_ap      = LOG_concur_titulo.titulo                                                     
               tt_titulo.cod_ser_docto   = "1"                                                                 
               tt_titulo.cod_espec_docto = LOG_concur_titulo.cod_esp                                                    
               tt_titulo.cod_parcela     = "1"                                                    
               tt_titulo.cod_refer       = LOG_concur_titulo.titulo
               tt_titulo.cod_portador    = c-portador
               tt_titulo.cod_carteira    = c-carteira
               tt_titulo.dt_trans        = TODAY 
               tt_titulo.valor           = d-vl-tot                                               
               tt_titulo.dt_emissao      = TODAY                                                 
               tt_titulo.dt_vencto       = fncDiaVencto(TODAY)
               tt_titulo.sequencia       = i-seq-it
               tt_titulo.cod_indic_econ  = histor_finalid_econ.cod_indic_econ WHEN AVAIL histor_finalid_econ   
               tt_titulo.cod_tip_fluxo   = fornec_financ.cod_tip_fluxo_financ WHEN AVAIL fornec_financ         
               tt_titulo.tipo            = (IF LOG_concur_titulo.cod_esp = c-esp-antecip THEN 1 ELSE (IF LOG_concur_titulo.cod_esp = c-esp-reemb THEN 3 ELSE 2)) /* 1-Antecip, 2-devolucao $, 3-reembolso */           
               tt_titulo.historico       = "Reprocessamento de Log do Concur".

        IF tt_titulo.tipo <> 1 AND LOG_concur_titulo.tit_antecip <> 0 THEN DO:
            FIND FIRST tit_ap NO-LOCK 
                WHERE tit_ap.cod_estab          = LOG_concur_titulo.cod_estab 
                  AND tit_ap.num_id_tit_ap      = LOG_concur_titulo.tit_antecip
                  AND tit_ap.val_sdo_tit_ap    >= d-vl-tot 
                  AND tit_ap.log_tit_ap_estordo = NO 
                  AND tit_ap.log_sdo_tit_ap     = YES NO-ERROR.
            IF NOT AVAIL tit_ap THEN                           
                RUN pi_gera_log (INPUT tt-titulo.cdn_fornec,
                                INPUT "ERRO",
                                INPUT tt_titulo.cod_estab, 
                                INPUT i-seq-it,
                                INPUT "NÆo foi encontrado o t¡tulo de antecipa‡Æo para a presta‡Æo de contas. T¡tulo: " + tt-titulo.titulo + " Funcion rio: " + STRING(tt-titulo.cdn_fornec) + " Esp: " + tt-titulo.cod_esp,
                                INPUT tt-titulo.titulo,
                                INPUT tt-titulo.cod_esp,
                                INPUT LOG_concur_titulo.cod_empresa,
                                INPUT LOG_concur_titulo.valor,
                                INPUT LOG_concur_titulo.dt_vencto).

            ELSE ASSIGN tt_titulo.cod_tit_antecip = tit_ap.cod_tit_ap
                        tt_titulo.r_row_antecip   = ROWID(tit_ap).
        END.
     END.
END.

PROCEDURE pi-report-excel:
   
    DEFINE VARIABLE c-usur-email AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c-arq-emp    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE d-vl-tot-est AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE d-vl-tot-emp AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE d-vl-tot-tipo AS DECIMAL  NO-UNDO.
    DEFINE VARIABLE d-tot-geral   AS DECIMAL  NO-UNDO.

    DEFINE VARIABLE l-cria-excel AS LOGICAL  INITIAL TRUE   NO-UNDO.
            
/*       ASSIGN d-vl-tot-emp  = 0 */
/*              d-vl-tot-est  = 0 */
/*              d-vl-tot-tipo = 0 */
             d-tot-geral   = 0.

    /* Monta os relat¢rios por empresa / estabelecimento */
    FOR EACH LOG_concur_titulo NO-LOCK WHERE
            (DATE(log_concur_titulo.datahora) = TODAY OR
             DATE(log_concur_titulo.dt_repros) = TODAY):

        IF l-cria-excel THEN DO: 
            RUN pi-abre-excel. /*testeando*/
            l-cria-excel = FALSE.
        END.


       ASSIGN d-tot-geral   = d-tot-geral   + LOG_concur_titulo.valor. 
     
       /*buscar o nome do fornecedor */
       FIND fornecedor NO-LOCK 
          WHERE fornecedor.cod_empresa = log_concur_titulo.cod_empresa 
           AND  fornecedor.cdn_fornec  = LOG_concur_titulo.cdn_fornec NO-ERROR.

        RUN pi-dados-excel.
        
    END.

    RUN pi-excel_total (INPUT "Total Geral ",
                        INPUT d-tot-geral).

    RUN pi-encerra-excel. /*teste*/
END.

PROCEDURE pi-excel_total:

    DEFINE INPUT  PARAMETER ipc-label AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipd-valor AS DECIMAL     NO-UNDO.

    DEFINE VARIABLE cRange AS CHARACTER   NO-UNDO.
 
    ASSIGN cRange                           = "A" + STRING(i-cont-linha) + ":L" + STRING(i-cont-linha).
           ch-excel:Range(cRange):FONT:NAME = "Tahoma".
           ch-excel:Range(cRange):FONT:SIZE = "8".

/*         ch-excel:SELECTION:MergeCells    = YES. */
/*         ch-excel:Range(cRange):Merge.           */
           ch-excel:Range("A" + STRING(i-cont-linha) + ":L" + STRING(i-cont-linha)):SELECT.    
           ch-excel:Range("A" + STRING(i-cont-linha) + ":L" + STRING(i-cont-linha)):Borders:LineStyle = 1.
           ch-excel:Range("A" + STRING(i-cont-linha) + ":L" + STRING(i-cont-linha)):Borders:Weight = 1.
           ch-excel:Range("A" + STRING(i-cont-linha) + ":L" + STRING(i-cont-linha)):Interior:ColorIndex = 34.
           ch-excel:Range(cRange):FONT:bold = YES.
           ch-excel:Range("A" + STRING(i-cont-linha)):VALUE  = ipc-label.
           ch-excel:Range( "I" + STRING(i-cont-linha)):VALUE = ipd-valor.
           cRange = "J" + STRING(i-cont-linha) + ":L" + STRING(i-cont-linha).
/*         ch-excel:SELECTION:MergeCells    = YES. */
/*         ch-excel:Range(cRange):Merge.           */

    ASSIGN i-cont-linha = i-cont-linha + 1.

END.

PROCEDURE pi-abre-excel:

   DEFINE VARIABLE cRange AS CHARACTER   NO-UNDO.
/*    ASSIGN c-arq-excel = SESSION:TEMP-DIRECTORY + "Concur_" + log_concur_titulo.cod_empresa + ".xlsx". */
   ASSIGN c-arq-excel = SESSION:TEMP-DIRECTORY + "Concur" + ".xlsx".
     
   CREATE tt-arq-anexo.
   ASSIGN tt-arq-anexo.arquivo = c-arq-excel.

   FILE-INFO:FILE-NAME = c-arq-excel.        
  
   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:ADD().
   ch-excel:ActiveSheet:NAME = "Concur".

   /* Cabe‡alho */
   cRange                           = "A1:L1".
   ch-excel:SELECTION:MergeCells    = YES. 
   ch-excel:Range(cRange):FONT:bold = YES.
   ch-excel:Range(cRange):Merge.  
   ch-excel:Range(cRange):VALUE     = "Log Processamento Concur".
   ch-excel:Range(cRange):FONT:NAME = "Tahoma".
   ch-excel:Range(cRange):FONT:SIZE = "14".
   ch-excel:Range(cRange):HorizontalAlignment = 3.
   
   ch-excel:Range("A4"):select.
   
   ASSIGN i-cont-linha = 4.

   ASSIGN ch-excel:columns( "A"):NumberFormat = "@"
          ch-excel:columns( "B"):NumberFormat = "@"
          ch-excel:columns( "C"):NumberFormat = "@"
          ch-excel:columns( "D"):NumberFormat = "@"
          ch-excel:columns( "E"):NumberFormat = "@"
          ch-excel:columns( "F"):NumberFormat = "@"
          ch-excel:columns( "G"):NumberFormat = "@"
          ch-excel:columns( "H"):NumberFormat = "@"
          ch-excel:columns( "I"):NumberFormat = "#.##0,00"
          ch-excel:columns( "J"):NumberFormat = "dd/mm/aaaa;@"
          ch-excel:columns( "K"):NumberFormat = "@"
          ch-excel:columns( "L"):NumberFormat = "@".
   
   /* inserindo data dos logs */

  cRange                           = "A2:D2".
  ch-excel:SELECTION:MergeCells    = YES. 
  ch-excel:Range(cRange):FONT:bold = YES.
  ch-excel:Range(cRange):Merge.
  ch-excel:Range(cRange):VALUE     = "Data do Processamento: " + STRING(LOG_concur_titulo.datahora,"99/99/9999").
  ch-excel:Range(cRange):FONT:NAME = "Tahoma".
  ch-excel:Range(cRange):FONT:SIZE = "9".
  ch-excel:Range(cRange):HorizontalAlignment = 3.
  

  cRange                           = "F2:J2".
  ch-excel:SELECTION:MergeCells    = YES. 
  ch-excel:Range(cRange):FONT:bold = YES.
  ch-excel:Range(cRange):Merge.
  ch-excel:Range(cRange):VALUE     =  IF LOG_concur_titulo.log_reprocess THEN "Data do Reprocessamento: " + STRING(LOG_concur_titulo.dt_repros,"99/99/9999") ELSE "".
  ch-excel:Range(cRange):FONT:NAME = "Tahoma".
  ch-excel:Range(cRange):FONT:SIZE = "9".
  ch-excel:Range(cRange):HorizontalAlignment = 3.
                                                           

   ASSIGN cRange = "A" + STRING(i-cont-linha) + ":L" + STRING(i-cont-linha)
          ch-excel:Range(cRange):FONT:NAME = "Tahoma"
          ch-excel:Range(cRange):FONT:SIZE = "8"
          ch-excel:Range(cRange):FONT:bold = YES.
  ASSIGN  ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Empresa"
          ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE = "Estab."
          ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE = "Fornecedor"
          ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE = "Nome Abrev"
          ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE = "Esp‚cie"
          ch-excel:Range( "F" + STRING(i-cont-linha,'99999')):VALUE = "S‚rie"
          ch-excel:Range( "G" + STRING(i-cont-linha,'99999')):VALUE = "T¡tulo"
          ch-excel:Range( "H" + STRING(i-cont-linha,'99999')):VALUE = "Parcela"
          ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = "Valor"
          ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = "Data Vencimento"
          ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = "Antecipa‡Æo"
          ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = "Tipo".

   ASSIGN i-cont-linha = i-cont-linha + 1.
END PROCEDURE.

PROCEDURE pi-dados-excel:

    DEFINE VARIABLE cRange AS CHARACTER   NO-UNDO.

    ASSIGN cRange = "A" + STRING(i-cont-linha) + ":L" + STRING(i-cont-linha).
           ch-excel:Range(cRange):FONT:NAME = "Tahoma".
           ch-excel:Range(cRange):FONT:SIZE = "8".

   ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = log_concur_titulo.cod_empresa
          ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE = LOG_concur_titulo.cod_estab 
          ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE = log_concur_titulo.cdn_fornec 
          ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE = fornecedor.nom_pessoa WHEN AVAIL fornecedor  
          ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE = log_concur_titulo.cod_esp
          ch-excel:Range( "F" + STRING(i-cont-linha,'99999')):VALUE = "1"
          ch-excel:Range( "G" + STRING(i-cont-linha,'99999')):VALUE = log_concur_titulo.titulo 
          ch-excel:Range( "H" + STRING(i-cont-linha,'99999')):VALUE = "1"
          ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = log_concur_titulo.valor
          ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = log_concur_titulo.dt_vencto      
          ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = log_concur_titulo.tit_antecip     
          ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = (IF log_concur_titulo.tipo = 1 THEN "ERRO" ELSE "Sucesso").
    ASSIGN i-cont-linha = i-cont-linha + 1.
END PROCEDURE.

PROCEDURE pi-encerra-excel:
   /* Encerra o excel */
   ch-excel:APPLICATION:DisplayAlerts = false.

   ch-excel:Cells:SELECT.
   ch-excel:Cells:EntireColumn:AutoFit.

   ch-excel:ActiveSheet:PageSetup:orientation = 2. 
  /*
   IF i-cont-linha <> 1 THEN DO:
      ch-excel:Range("A1:AB" + string(i-cont-linha - 1)):autofilter(,,,).
   END.
   */
   ch-excel:Range("A1:AB" + string(i-cont-linha - 1)):select.
   ch-excel:visible = false.
   ch-excel:ActiveWorkbook:close(yes,c-arq-excel).
   
   ch-excel:QUIT().
   RELEASE OBJECT ch-excel NO-ERROR.
  
END PROCEDURE.


PROCEDURE pi-email:

    DEFINE VARIABLE c-arq-anexo AS CHARACTER   NO-UNDO.
    FIND FIRST param_email NO-LOCK NO-ERROR.

    EMPTY TEMP-TABLE tt-envio2.

    FOR EACH es_parametros NO-LOCK 
        WHERE es_parametros.cod_prog_dtsul = "AP001-RP"
        AND   es_parametros.cod_referencia BEGINS "email":

         /*
         Find First usuar_mestre No-lock
            WHERE usuar_mestre.cod_usuario = es_parametros.cod_parametro No-error.
         IF NOT AVAIL usuar_mestre Then DO:
            RUN pi_gera_log (INPUT INT(0),                                                              /*ipi-fornec */ 
                             INPUT "ERRO",                                                              /*ipc-tipo   */
                             INPUT "",                                                                  /*ipc-estab  */
                             INPUT 0,                                                                   /*ipc-linha  */
                             INPUT "NÆo encontrado usuario mestre " + 
                                    es_parametros.cod_parametro + " para envio de e-mail",              /*ipc-msg    */
                             INPUT "",                                                                  /*ipc-titulo */
                             INPUT "",                                                                  /*ipc-espec  */
                             INPUT "",                                                                  /*ipc-empresa*/
                             INPUT 0,                                                                   /*ipd-valor  */
                             INPUT ?).                                                                  /*ipd-dtvenc */

            NEXT.
         END.
         */
         /* Verifica as empresas que o usu rio tem acesso */
         ASSIGN c-arq-excel = ""
                c-arq-anexo = "".

         FOR EACH tt-arq-anexo:
            
             ASSIGN c-arq-anexo = c-arq-anexo + (IF c-arq-anexo = "" THEN tt-arq-anexo.arquivo ELSE (',' + tt-arq-anexo.arquivo)).
         END.


         /* Verifica a existencia da planilha em excel gerada pela procedure pi-report-excel*/
         /* Ser  enviado pelo anexo para o usuario */
             
         EMPTY TEMP-TABLE tt-envio2.
         EMPTY TEMP-TABLE tt-mensagem.

         CREATE tt-envio2.
         ASSIGN tt-envio2.versao-integracao = 1
                tt-envio2.exchange          = param_email.log_servid_exchange
                tt-envio2.servidor          = param_email.cod_servid_e_mail
                tt-envio2.porta             = param_email.num_porta
                tt-envio2.destino           = es_parametros.cod_parametro
                tt-envio2.assunto           = "Relat¢rio Log de processamento Concur "
                tt-envio2.remetente         = "importacaoconcur@yamana.com"
                tt-envio2.copia             = ""
                tt-envio2.mensagem          = "Prezados(a/as)," +  CHR(13) + CHR(13) +
                                              "Segue em anexo o log de processamento do Concur de " + STRING(TODAY,"99/99/9999") + "." + CHR(13) + CHR(13) + 
                                              "Atenciosamente," + CHR(13) + CHR(13) + "Sustenta‡Æo Yamana"    
                tt-envio2.importancia       = 1
                tt-envio2.log-enviada       = NO 
                tt-envio2.log-lida          = NO 
                tt-envio2.acomp             = NO 
                tt-envio2.arq-anexo         = c-arq-anexo.

         CREATE tt-mensagem.
         ASSIGN tt-mensagem.seq-mensagem = 1
                tt-mensagem.mensagem     = "Prezados(a/as)," +  CHR(13) + CHR(13) +                                                      
                                           "Segue em anexo o log de processamento do Concur de " + STRING(TODAY,"99/99/9999") + "." + CHR(13) + CHR(13) +        
                                           "Atenciosamente," + CHR(13) + CHR(13) + "Sustenta‡Æo Yamana".

         RUN utp/utapi019.p PERSISTENT SET h-utapi019.

         RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                       INPUT  TABLE tt-mensagem,
                                       OUTPUT TABLE tt-erros).

         FOR EACH tt-erros:

             DISP tt-erros.cod-erro
                  tt-erros.desc-erro
                  tt-erros.desc-arq WITH WIDTH 500 DOWN FRAME f.
         END.
    
         DELETE PROCEDURE h-utapi019.     
    END.
END.


PROCEDURE pi-copia-arquivo:

    DEFINE VARIABLE c-arq-target AS CHARACTER   NO-UNDO.

    FOR EACH tt-arquivo:
        /* Copia arquivo processado para diret¢rio de log erro */
        IF CAN-FIND(FIRST tt_log WHERE tt_log.tipo = "ERRO")
                     THEN ASSIGN c-arq-target = REPLACE(tt-arquivo.carq,c-pendente,c-erro).

        ELSE ASSIGN c-arq-target = REPLACE(tt-arquivo.carq,c-pendente,c-sucesso).

        /* move arquivo processado para diretorio de erro/sucesso */
        OS-COMMAND SILENT COPY VALUE(tt-arquivo.carq) VALUE(c-arq-target).

    END.
END.

PROCEDURE pi-limpa-tt:

    EMPTY TEMP-TABLE tt_log_erros_atualiz.
    empty temp-table tt_integr_apb_antecip_pef_p1.
    empty temp-table tt_integr_apb_antecip_pef_pend.
    EMPTY TEMP-TABLE tt_integr_apb_aprop_ctbl_pend.
    empty temp-table tt_integr_apb_abat_prev_provis.
    empty temp-table tt_integr_apb_impto_impl_pend.
    empty temp-table tt_integr_apb_aprop_ctbl_pend.
    empty temp-table tt_1099.
    empty temp-table tt_ord_compra_tit_ap_pend_1.
    EMPTY TEMP-TABLE tt_integr_apb_abat_prev_provis.

    EMPTY TEMP-TABLE tt_params_generic_api.
    EMPTY TEMP-TABLE tt_log_erros_tit_ap_alteracao.
    EMPTY TEMP-TABLE tt_tit_ap_alteracao_rateio.
    EMPTY TEMP-TABLE tt_tit_ap_alteracao_base_aux_3.

    empty temp-table tt_integr_apb_abat_antecip_vouc.
    empty temp-table tt_integr_apb_abat_prev_provis.
    empty temp-table tt_integr_apb_aprop_ctbl_pend.
    empty temp-table tt_integr_apb_aprop_relacto.
    empty temp-table tt_integr_apb_impto_impl_pend.
    empty temp-table tt_integr_apb_item_lote_impl3v.
    empty temp-table tt_integr_apb_lote_impl.
    empty temp-table tt_integr_apb_relacto_pend.
    empty temp-table tt_log_erros_atualiz.
    empty temp-table tt_integr_apb_item_lote_impl.
    empty temp-table tt_params_generic_api.
    empty temp-table tt_integr_apb_relacto_pend_aux.
    empty temp-table tt_integr_apb_aprop_relacto_2.
    empty temp-table tt_docum_est_esoc_api.
    empty temp-table tt_item_doc_est_esoc_api.
    empty temp-table tt_integr_apb_nota_pend_cart.
    EMPTY TEMP-TABLE tt_vinc_an_x_tit.
    EMPTY TEMP-TABLE tt_erro_msg.

END.
