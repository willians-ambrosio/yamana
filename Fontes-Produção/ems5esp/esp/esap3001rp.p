/******************************************************************************************************************************************
** Programa: esp/esap3001rp.p
** Data    : 16-05-2015
** Autor   : Mauricio Cerqueira Miranda
** Objetivo: Relatorio Cria AVMN
********************************************************************************************************************************************/

/* include de controle de versío */
{include/i-prgvrs.i esap3001 "TOTVS-12"}


DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD c-arquivo-avm    AS CHAR.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

/* recebimento de par≥metros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

/*Carregando a tt-digita*/
create tt-param.
raw-transfer raw-param to tt-param.


/* carregando tt-digita */
For Each tt-raw-digita:
    Create tt-digita.
    Raw-transfer tt-raw-digita.raw-digita To tt-digita.
End.     



/************ API para executar baixar de t°tulos por AVMN *************/
def temp-table tt_tit_ap_alteracao_base no-undo
    field ttv_cod_usuar_corren             as character format "x(12)" label "Usuˇrio Corrente" column-label "Usuˇrio Corrente"
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_num_id_tit_ap                as integer format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field ttv_rec_tit_ap                   as recid format ">>>>>>9" initial ?
    field tta_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_espec_docto              as character format "x(3)" label "Esp≤cie Documento" column-label "Esp≤cie"
    field tta_cod_ser_docto                as character format "x(3)" label "S≤rie Documento" column-label "S≤rie"
    field tta_cod_tit_ap                   as character format "x(10)" label "T≠tulo" column-label "T≠tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field ttv_dat_transacao                as date format "99/99/9999" initial today label "Data Transaªío" column-label "Data Transaªío"
    field ttv_cod_refer                    as character format "x(10)" label "Referºncia" column-label "Referºncia"
    field tta_val_sdo_tit_ap               as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Saldo" column-label "Valor Saldo"
    field tta_dat_emis_docto               as date format "99/99/9999" initial today label "Data  Emissío" column-label "Dt Emissío"
    field tta_dat_vencto_tit_ap            as date format "99/99/9999" initial today label "Data Vencimento" column-label "Dt Vencto"
    field tta_dat_prev_pagto               as date format "99/99/9999" initial today label "Data Prevista Pgto" column-label "Dt Prev Pagto"
    field tta_dat_ult_pagto                as date format "99/99/9999" initial ? label "Data Çltimo Pagto" column-label "Data Çltimo Pagto"
    field tta_num_dias_atraso              as integer format ">9" initial 0 label "Dias Atraso" column-label "Dias Atr"
    field tta_val_perc_multa_atraso        as decimal format ">9.99" decimals 2 initial 00.00 label "Perc Multa Atraso" column-label "Multa Atr"
    field tta_val_juros_dia_atraso         as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Juro" column-label "Vl Juro"
    field tta_val_perc_juros_dia_atraso    as decimal format ">9.999999" decimals 6 initial 00.00 label "Perc Jur Dia Atraso" column-label "Perc Dia"
    field tta_dat_desconto                 as date format "99/99/9999" initial ? label "Data Desconto" column-label "Dt Descto"
    field tta_val_perc_desc                as decimal format ">9.9999" decimals 4 initial 0 label "Percentual Desconto" column-label "Perc Descto"
    field tta_val_desconto                 as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Desconto" column-label "Valor Desconto"
    field tta_cod_portador                 as character format "x(5)" label "Portador" column-label "Portador"
    field ttv_cod_portador_mov             as character format "x(5)" label "Portador Movto" column-label "Portador Movto"
    field tta_log_pagto_bloqdo             as logical format "Sim/Nío" initial no label "Bloqueia Pagamento" column-label "Pagto Bloqdo"
    field tta_cod_seguradora               as character format "x(8)" label "Seguradora" column-label "Seguradora"
    field tta_cod_apol_seguro              as character format "x(12)" label "ApΩlice Seguro" column-label "Apolice Seguro"
    field tta_cod_arrendador               as character format "x(6)" label "Arrendador" column-label "Arrendador"
    field tta_cod_contrat_leas             as character format "x(12)" label "Contrato Leasing" column-label "Contr Leas"
    field tta_ind_tip_espec_docto          as character format "X(17)" initial "Normal" label "Tipo Esp≤cie" column-label "Tipo Esp≤cie"
    field tta_cod_indic_econ               as character format "x(8)" label "Moeda" column-label "Moeda"
    field tta_num_seq_refer                as integer format ">>>9" initial 0 label "Sequºncia" column-label "Seq"
    field ttv_ind_motiv_alter_val_tit_ap   as character format "X(09)" initial "Alteraªío" label "Motivo Alteraªío" column-label "Motivo Alteraªío"
    field ttv_wgh_lista                    as widget-handle extent 15 format ">>>>>>9"
    field ttv_log_gera_ocor_alter_valores  as logical format "Sim/Nío" initial no
    field tta_cb4_tit_ap_bco_cobdor        as Character format "x(50)" label "Titulo Bco Cobrador" column-label "Titulo Bco Cobrador"
    field tta_cod_histor_padr              as character format "x(8)" label "HistΩrico Padrío" column-label "HistΩrico Padrío"
    field tta_des_histor_padr              as character format "x(40)" label "Descriªío" column-label "Descriªío HistΩrico Padrío"
    field tta_ind_sit_tit_ap               as character format "X(13)" label "Situaªío" column-label "Situaªío"
    field tta_cod_forma_pagto              as character format "x(3)" label "Forma Pagamento" column-label "F Pagto"
    index tt_titap_id                     
          tta_cod_estab                    ascending
          tta_cdn_fornecedor               ascending
          tta_cod_espec_docto              ascending
          tta_cod_ser_docto                ascending
          tta_cod_tit_ap                   ascending
          tta_cod_parcela                  ascending
    .

def temp-table tt_tit_ap_alteracao_rateio no-undo
    field ttv_rec_tit_ap                   as recid format ">>>>>>9" initial ?
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_refer                    as character format "x(10)" label "Referºncia" column-label "Referºncia"
    field tta_num_seq_refer                as integer format ">>>9" initial 0 label "Sequºncia" column-label "Seq"
    field tta_cod_tip_fluxo_financ         as character format "x(12)" label "Tipo Fluxo Financ" column-label "Tipo Fluxo Financ"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Contˇbil" column-label "Conta Contˇbil"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid NegΩcio" column-label "Un Neg"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field tta_val_aprop_ctbl               as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Aprop Ctbl" column-label "Vl Aprop Ctbl"
    field ttv_ind_tip_rat                  as character format "X(08)"
    field tta_num_id_tit_ap                as integer format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field tta_num_id_aprop_ctbl_ap         as integer format "9999999999" initial 0 label "Id Aprop Ctbl AP" column-label "Id Aprop Ctbl AP"
    index tt_aprpctba_id                   is primary unique
          tta_cod_estab                    ascending
          tta_cod_refer                    ascending
          tta_num_seq_refer                ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
          tta_cod_unid_negoc               ascending
          tta_cod_plano_ccusto             ascending
          tta_cod_ccusto                   ascending
          tta_cod_tip_fluxo_financ         ascending
          ttv_rec_tit_ap                   ascending
    .

def temp-table tt_compl_histor_padr_valor no-undo
    field tta_cod_compl_padr               as character format "x(8)" label "Complemento Padrío" column-label "Comp Pad"
    field tta_cod_format_compl_padr        as character format "x(20)" label "Formato Complemento" column-label "Formato Complemento"
    field ttv_des_val_compl_padr           as character format "x(20)"
    index tt_id                            is primary unique
          tta_cod_compl_padr               ascending
    .

def temp-table tt_log_erros_tit_ap_alteracao no-undo
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_espec_docto              as character format "x(3)" label "Esp≤cie Documento" column-label "Esp≤cie"
    field tta_cod_ser_docto                as character format "x(3)" label "S≤rie Documento" column-label "S≤rie"
    field tta_cod_tit_ap                   as character format "x(10)" label "T≠tulo" column-label "T≠tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field tta_num_id_tit_ap                as integer format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field ttv_num_mensagem                 as integer format ">>>>,>>9" label "Número" column-label "Número Mensagem"
    field ttv_cod_tip_msg_dwb              as character format "x(12)" label "Tipo Mensagem" column-label "Tipo Mensagem"
    field ttv_des_msg_erro                 as character format "x(60)" label "Mensagem Erro" column-label "Inconsistºncia"
    field ttv_des_msg_ajuda_1              as character format "x(170)"
    field ttv_wgh_focus                    as widget-handle format ">>>>>>9"
    .


DEF TEMP-TABLE tt-log-erro NO-UNDO
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_espec_docto              as character format "x(3)" label "Esp≤cie Documento" column-label "Esp≤cie"
    field tta_cod_ser_docto                as character format "x(3)" label "S≤rie Documento" column-label "S≤rie"
    field tta_cod_tit_ap                   as character format "x(10)" label "T≠tulo" column-label "T≠tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field tta_num_id_tit_ap                as integer format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field ttv_num_mensagem                 as integer format ">>>>,>>9" label "Número" column-label "Número Mensagem"
    field ttv_cod_tip_msg_dwb              as character format "x(12)" label "Tipo Mensagem" column-label "Tipo Mensagem"
    field ttv_des_msg_erro                 as character format "x(60)" label "Mensagem Erro" column-label "Inconsistºncia"
    field ttv_des_msg_ajuda_1              as character format "x(170)"
    .

DEF VAR w-cod-estab LIKE tit_ap.cod_estab.
DEF VAR w-fornecedor LIKE tit_ap.cdn_fornecedor.
DEF VAR w-ser-docto LIKE tit_ap.cod_ser_docto.
DEF VAR w-cod-tit-ap LIKE tit_ap.cod_tit_ap.
DEF VAR w-cod-parcela LIKE tit_ap.cod_parcela.
DEF VAR w-cod-especie LIKE tit_ap.cod_espec_docto.
 
  
 
DEFINE VARIABLE i-seq AS INTEGER     NO-UNDO.

def new global shared var v_rec_tit_ap
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.

def var v_cod_refer
    as character
    format "x(10)":U
    label "Referància"
    column-label "Referància"
    no-undo.


def var v_cod_histor_padr
    as character
    format "x(8)":U
    label "Hist¢rico Padr∆o"
    column-label "Hist¢rico Padr∆o"
    no-undo.


def var v_des_text_histor
    as character
    format "x(2000)":U
    view-as editor max-chars 2000 scrollbar-vertical
    size 50 by 4
    bgcolor 15 font 2
    label "Hist¢rico"
    column-label "Hist¢rico"
    no-undo.

DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.
    
DEF TEMP-TABLE tt-tit-ap 
    FIELD cod_estab       LIKE tit_ap.cod_estab      
    FIELD cdn_fornecedor  LIKE tit_ap.cdn_fornecedor 
    FIELD cod_espec_docto LIKE tit_ap.cod_espec_docto
    FIELD cod_ser_docto   LIKE tit_ap.cod_ser_docto  
    FIELD cod_tit_ap      LIKE tit_ap.cod_tit_ap     
    FIELD cod_parcela     LIKE tit_ap.cod_parcela .   

INPUT FROM VALUE(tt-param.c-arquivo-avm). 

    REPEAT:
       
        CREATE tt-tit-ap.
        IMPORT DELIMITER ';' tt-tit-ap. 
    
    END.

INPUT CLOSE.

ASSIGN i-cont = DAY(TODAY).

FOR EACH tt-tit-ap
    WHERE tt-tit-ap.cod_tit_ap <> "":


    FOR EACH tit_ap 
        WHERE tit_ap.cod_estab       = tt-tit-ap.cod_estab      
          AND tit_ap.cdn_fornecedor  = tt-tit-ap.cdn_fornecedor 
          AND tit_ap.cod_espec_docto = tt-tit-ap.cod_espec_docto
          AND tit_ap.cod_ser_docto   = tt-tit-ap.cod_ser_docto  
          AND tit_ap.cod_tit_ap      = tt-tit-ap.cod_tit_ap     
          AND tit_ap.cod_parcela     = tt-tit-ap.cod_parcela    :


        ASSIGN i-cont = i-cont + 1.

        ASSIGN v_cod_refer = "AVMN" + string(DAY(TODAY)) + string(RANDOM(1,261)) + STRING(i-cont).

        assign v_rec_tit_ap = recid(tit_ap).
        /*RUN pi-encontra-referencia.*/
        run pi_atualizar_tt_tit_ap_alteracao_base /*pi_atualizar_tt_tit_ap_alteracao_base*/.
        run pi_tit_ap_rateio /*pi_tit_ap_atualiza_focus*/.            
         
          
        run prgfin/apb/apb767za.r  (input 1,
                                    input-output table tt_tit_ap_alteracao_base  ,
                                    input-output table tt_tit_ap_alteracao_rateio,
                                    output table tt_log_erros_tit_ap_alteracao).

        FIND FIRST tt_log_erros_tit_ap_alteracao NO-LOCK NO-ERROR.
        IF AVAIL(tt_log_erros_tit_ap_alteracao) THEN DO:
            CREATE tt-log-erro.
            ASSIGN tt-log-erro.tta_cod_estab        = tt_log_erros_tit_ap_alteracao.tta_cod_estab           
                   tt-log-erro.tta_cdn_fornecedor   = tt_log_erros_tit_ap_alteracao.tta_cdn_fornecedor 
                   tt-log-erro.tta_cod_espec_docto  = tt_log_erros_tit_ap_alteracao.tta_cod_espec_docto
                   tt-log-erro.tta_cod_ser_docto    = tt_log_erros_tit_ap_alteracao.tta_cod_ser_docto  
                   tt-log-erro.tta_cod_tit_ap       = tt_log_erros_tit_ap_alteracao.tta_cod_tit_ap     
                   tt-log-erro.tta_cod_parcela      = tt_log_erros_tit_ap_alteracao.tta_cod_parcela    
                   tt-log-erro.tta_num_id_tit_ap    = tt_log_erros_tit_ap_alteracao.tta_num_id_tit_ap  
                   tt-log-erro.ttv_num_mensagem     = tt_log_erros_tit_ap_alteracao.ttv_num_mensagem   
                   tt-log-erro.ttv_cod_tip_msg_dwb  = tt_log_erros_tit_ap_alteracao.ttv_cod_tip_msg_dwb
                   tt-log-erro.ttv_des_msg_erro     = tt_log_erros_tit_ap_alteracao.ttv_des_msg_erro   
                   tt-log-erro.ttv_des_msg_ajuda_1  = tt_log_erros_tit_ap_alteracao.ttv_des_msg_ajuda_1.
        END.
        ELSE DO:
            CREATE tt-log-erro.
            ASSIGN tt-log-erro.tta_cod_estab        = tit_ap.cod_estab      
                   tt-log-erro.tta_cdn_fornecedor   = tit_ap.cdn_fornecedor 
                   tt-log-erro.tta_cod_espec_docto  = tit_ap.cod_espec_docto
                   tt-log-erro.tta_cod_ser_docto    = tit_ap.cod_ser_docto  
                   tt-log-erro.tta_cod_tit_ap       = tit_ap.cod_tit_ap     
                   tt-log-erro.tta_cod_parcela      = tit_ap.cod_parcela    
                   tt-log-erro.tta_num_id_tit_ap    = tit_ap.num_id_tit_ap  
                   tt-log-erro.ttv_des_msg_erro     = "OK".
        END.
    
    
    END.

END.


OUTPUT TO "D:\temp\LOG_AVMN.csv".

    FOR EACH tt-log-erro.
        EXPORT DELIMITER ';' tt-log-erro.
    END.

OUTPUT CLOSE. 

PROCEDURE pi_atualizar_tt_tit_ap_alteracao_base:
    EMPTY TEMP-TABLE tt_tit_ap_alteracao_base.


    FIND ems5.espec_docto
      WHERE espec_docto.cod_espec_docto =  tit_ap.cod_espec_docto NO-LOCK NO-ERROR.

        create tt_tit_ap_alteracao_base.
        assign tt_tit_ap_alteracao_base.tta_cod_empresa                      = tit_ap.cod_empresa
               tt_tit_ap_alteracao_base.tta_cod_estab                        = tit_ap.cod_estab 
               tt_tit_ap_alteracao_base.tta_cdn_fornecedor                   = tit_ap.cdn_fornecedor
               tt_tit_ap_alteracao_base.tta_cod_espec_docto                  = tit_ap.cod_espec_docto
               tt_tit_ap_alteracao_base.tta_cod_ser_docto                    = tit_ap.cod_ser_docto
               tt_tit_ap_alteracao_base.tta_cod_tit_ap                       = tit_ap.cod_tit_ap
               tt_tit_ap_alteracao_base.tta_cod_parcela                      = tit_ap.cod_parcela
               tt_tit_ap_alteracao_base.tta_num_id_tit_ap                    = tit_ap.num_id_tit_ap
               tt_tit_ap_alteracao_base.ttv_dat_transacao                    = 08/01/15
               tt_tit_ap_alteracao_base.ttv_ind_motiv_alter_val_tit_ap       = IF substr(espec_docto.ind_tip_espec_docto,1,3) = "Ant" THEN "BAIXA" ELSE "Alteraá∆o"
               tt_tit_ap_alteracao_base.tta_val_sdo_tit_ap                   = 0 /* tit_ap.val_sdo_tit_ap */
               tt_tit_ap_alteracao_base.tta_cod_portador                     = "" /*tit_ap.cod_portador*/
               tt_tit_ap_alteracao_base.ttv_cod_portador_mov                 = ""
               tt_tit_ap_alteracao_base.tta_dat_emis_docto                   = tit_ap.dat_emis_docto
               tt_tit_ap_alteracao_base.tta_dat_vencto_tit_ap                = tit_ap.dat_vencto_tit_ap
               tt_tit_ap_alteracao_base.tta_dat_prev_pagto                   = tit_ap.dat_prev_pagto      
               tt_tit_ap_alteracao_base.tta_cod_indic_econ                   = tit_ap.cod_indic_econ
               tt_tit_ap_alteracao_base.tta_num_dias_atraso                  = 0       
               tt_tit_ap_alteracao_base.tta_val_perc_multa_atraso            = 0
               tt_tit_ap_alteracao_base.tta_val_juros_dia_atraso             = 0
               tt_tit_ap_alteracao_base.tta_val_perc_juros_dia_atraso        = 0
               tt_tit_ap_alteracao_base.tta_dat_desconto                     = ?
               tt_tit_ap_alteracao_base.tta_val_perc_desc                    = 0
               tt_tit_ap_alteracao_base.tta_val_desconto                     = 0       
               tt_tit_ap_alteracao_base.tta_log_pagto_bloqdo                 = FALSE
               tt_tit_ap_alteracao_base.tta_ind_sit_tit_ap                   = "Liberado"
               tt_tit_ap_alteracao_base.tta_cod_seguradora                   = tit_ap.cod_seguradora
               tt_tit_ap_alteracao_base.tta_cod_apol_seguro                  = tit_ap.cod_apol_seguro
               tt_tit_ap_alteracao_base.tta_cod_arrendador                   = tit_ap.cod_arrendador
               tt_tit_ap_alteracao_base.tta_cod_contrat_leas                 = tit_ap.cod_contrat_leas
               tt_tit_ap_alteracao_base.tta_cb4_tit_ap_bco_cobdor            = tit_ap.cb4_tit_ap_bco_cobdor
               tt_tit_ap_alteracao_base.tta_cod_forma_pagto                  = "001"
               tt_tit_ap_alteracao_base.tta_cod_histor_padr                  = v_cod_histor_padr
               tt_tit_ap_alteracao_base.tta_des_histor_padr                  = "Venda CGO para BRIO GOLD"
               tt_tit_ap_alteracao_base.ttv_cod_refer                        = v_cod_refer .
 
  


END PROCEDURE. /* pi_atualizar_tt_tit_ap_alteracao_base */

PROCEDURE pi_tit_ap_rateio:

    EMPTY TEMP-TABLE tt_tit_ap_alteracao_rateio.

    ASSIGN i-seq = i-seq + 1.

    create tt_tit_ap_alteracao_rateio.
    assign tt_tit_ap_alteracao_rateio.ttv_rec_tit_ap              = RECID(tit_ap)      
           tt_tit_ap_alteracao_rateio.tta_cod_estab               = tt_tit_ap_alteracao_base.tta_cod_estab
           tt_tit_ap_alteracao_rateio.tta_cod_refer               = v_cod_refer 
           tt_tit_ap_alteracao_rateio.tta_num_seq_refer           = i-seq * 10 
           tt_tit_ap_alteracao_rateio.tta_cod_tip_fluxo_financ    = ""
           tt_tit_ap_alteracao_rateio.tta_cod_plano_cta_ctbl      = "CONTSOC"
           tt_tit_ap_alteracao_rateio.tta_cod_cta_ctbl            = "17202016"
           tt_tit_ap_alteracao_rateio.tta_cod_unid_negoc          = ""
           tt_tit_ap_alteracao_rateio.tta_cod_plano_ccusto        = ""
           tt_tit_ap_alteracao_rateio.tta_cod_ccusto              = ""
           tt_tit_ap_alteracao_rateio.tta_val_aprop_ctbl          = tit_ap.val_sdo_tit_ap 
           tt_tit_ap_alteracao_rateio.ttv_ind_tip_rat             = "Valor"
    /*       tt_tit_ap_alteracao_rateio.ttv_ind_tip_rat             = 50*/
           tt_tit_ap_alteracao_rateio.tta_num_id_tit_ap           = tit_ap.num_id_tit_ap. 

           /*tt_tit_ap_alteracao_rateio.tta_num_id_aprop_ctbl_ap    =*/ .

END PROCEDURE.