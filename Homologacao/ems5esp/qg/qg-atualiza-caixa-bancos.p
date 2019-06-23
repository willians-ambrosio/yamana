DEFINE VARIABLE c-mess-top         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mess             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mess-base        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mot-reprov       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-confirma-titulos AS LOGICAL     NO-UNDO.
{utp/ut-glob.i}
{utp/utapi019.i}
{include/i-prgvrs.i apb001-w01 1.00.00.000}

DEFINE VARIABLE c-assunto AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dt-aprov-atu AS DATETIME  NO-UNDO.
DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.


DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario          AS CHARACTER                          NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE v_rec_lote_impl_tit_ap AS RECID FORMAT ">>>>>>9":U INITIAL ? NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-prg-aprov-movto      AS LOGICAL NO-UNDO. 

def new shared temp-table tt_log_erros_atualiz no-undo
    field tta_cod_estab                         as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_refer                         as character format "x(10)" label "Referˆncia" column-label "Referˆncia"
    field tta_num_seq_refer                     as integer format ">>>9" initial 0 label "Sequˆncia" column-label "Seq"
    field ttv_num_mensagem                      as integer format ">>>>,>>9" label "Nœmero" column-label "Nœmero Mensagem"
    field ttv_des_msg_erro                      as character format "x(60)" label "Mensagem Erro" column-label "Inconsistˆncia"
    field ttv_des_msg_ajuda                     as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_ind_tip_relacto                   as character format "X(15)" label "Tipo Relacionamento" column-label "Tipo Relac"
    field ttv_num_relacto                       as integer format ">>>>,>>9" label "Relacionamento" column-label "Relacionamento".

DEFINE TEMP-TABLE tt_esp_pend_lote_ap           LIKE esp_pend_lote_ap
    FIELD cdn_fornecedor                        LIKE item_lote_impl_ap.cdn_fornecedor   
    FIELD cod_espec_docto                       LIKE item_lote_impl_ap.cod_espec_docto  
    FIELD cod_ser_docto                         LIKE item_lote_impl_ap.cod_ser_docto    
    FIELD cod_tit_ap                            LIKE item_lote_impl_ap.cod_tit_ap       
    FIELD cod_parcela                           LIKE item_lote_impl_ap.cod_parcela      
    FIELD dat_emis_docto                        LIKE item_lote_impl_ap.dat_emis_docto   
    FIELD dat_vencto_tit_ap                     LIKE item_lote_impl_ap.dat_vencto_tit_ap
    FIELD val_tit_ap                            LIKE item_lote_impl_ap.val_tit_ap
    FIELD des_text_histor                       LIKE item_lote_impl_ap.des_text_histor.

DEFINE TEMP-TABLE tt_esp_pend_movto_cta_corren  LIKE esp_pend_movto_cta_corren
    FIELD val_movto_cta_corren                  LIKE movto_cta_corren.val_movto_cta_corren
    FIELD dat_transacao                         LIKE movto_cta_corren.dat_transacao
    FIELD des_histor_movto_cta_corren           LIKE movto_cta_corren.des_histor_movto_cta_corren
    FIELD cod_usuar_ult_atualiz                 LIKE movto_cta_corren.cod_usuar_ult_atualiz
    FIELD ind_fluxo_movto_cta_corren            LIKE movto_cta_corren.ind_fluxo_movto_cta_corren.

DEFINE TEMP-TABLE tt-tit
    FIELD cod_estab                             LIKE esp_pend_lote_ap.cod_estab
    FIELD cod_refer                             LIKE esp_pend_lote_ap.cod_refer
    FIELD dt_aprovacao1                         LIKE esp_pend_lote.dt_aprovacao1
    FIELD dt_aprovacao2                         LIKE esp_pend_lote.dt_aprovacao2
    FIELD cod_usuario                           LIKE esp_pend_lote.cod_usuario.

DEFINE TEMP-TABLE tt_tit_ap_par
    FIELD cod_estab         LIKE tit_ap.cod_estab                
    FIELD cdn_fornecedor    LIKE tit_ap.cdn_fornecedor
    FIELD cod_espec_docto   LIKE tit_ap.cod_espec_docto          
    FIELD cod_ser_docto     LIKE tit_ap.cod_ser_docto            
    FIELD cod_tit_ap        LIKE tit_ap.cod_tit_ap               
    FIELD val_origin_tit_ap LIKE tit_ap.val_origin_tit_ap
    FIELD dat_emis_docto    LIKE tit_ap.dat_emis_docto.

DEFINE TEMP-TABLE tt_esp_hist_aprov_tit         LIKE esp_hist_aprov_tit.

DEFINE BUFFER   b_esp_aprovador FOR esp_aprovador.
DEFINE BUFFER   b-item_lote_impl_ap FOR item_lote_impl_ap.


FOR EACH esp_pend_movto_cta_corren EXCLUSIVE-LOCK:
    
    /*segunda aprova‡Æo*/
    {prgfin\apb\apb001-i06.i}

    FIND movto_cta_corren OF esp_pend_movto_cta_corren NO-LOCK NO-ERROR.
    FIND esp_hist_aprov_movto_cc OF esp_pend_movto_cta_corren EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL esp_hist_aprov_movto THEN
    DO:
        CREATE esp_hist_aprov_movto_cc.
        ASSIGN
            esp_hist_aprov_movto_cc.cod_cta_corren           = esp_pend_movto_cta_corren.cod_cta_corren          
            esp_hist_aprov_movto_cc.dat_movto_cta_corren     = esp_pend_movto_cta_corren.dat_movto_cta_corren    
            esp_hist_aprov_movto_cc.num_seq_movto_cta_corren = esp_pend_movto_cta_corren.num_seq_movto_cta_corren.
    END.
    ASSIGN
        esp_hist_aprov_movto_cc.dt_aprovacao1              = TODAY       
        esp_hist_aprov_movto_cc.cod_usuario_aprov1         = esp_pend_movto_cta_corren.cod_usuario  
        esp_hist_aprov_movto_cc.dt_aprovacao2              = ?       
        esp_hist_aprov_movto_cc.cod_usuario_aprov2         = ""  
        esp_hist_aprov_movto_cc.cod_usuario_reprov         = ""
        esp_hist_aprov_movto_cc.dt_reprovacao              = ?       
        esp_hist_aprov_movto_cc.mot-reprov                 = ""          
        esp_hist_aprov_movto_cc.cod_usuario                = esp_pend_movto_cta_corren.cod_usuario         
        esp_hist_aprov_movto_cc.dt_digitacao               = esp_pend_movto_cta_corren.dt_digitacao        
        esp_hist_aprov_movto_cc.des_text_histor            = movto_cta_corren.des_histor_movto_cta_corren
        esp_hist_aprov_movto_cc.dat_transacao              = movto_cta_corren.dat_transacao                
        esp_hist_aprov_movto_cc.val_movto_cta_corren       = movto_cta_corren.val_movto_cta_corren         
        esp_hist_aprov_movto_cc.ind_fluxo_movto_cta_corren = movto_cta_corren.ind_fluxo_movto_cta_corren.


    OUTPUT TO "c:\temp\caixa-banco-aprovados.txt" APPEND.
    PUT UNFORMATTED 
        esp_hist_aprov_movto_cc.cod_cta_corren                skip
        esp_hist_aprov_movto_cc.dat_movto_cta_corren          skip
        esp_hist_aprov_movto_cc.num_seq_movto_cta_corren      skip
        esp_hist_aprov_movto_cc.dt_aprovacao1                 skip
        esp_hist_aprov_movto_cc.cod_usuario_aprov1            skip
        esp_hist_aprov_movto_cc.dt_aprovacao2                 skip
        esp_hist_aprov_movto_cc.cod_usuario_aprov2            skip
        esp_hist_aprov_movto_cc.cod_usuario_reprov            skip
        esp_hist_aprov_movto_cc.dt_reprovacao                 skip
        esp_hist_aprov_movto_cc.mot-reprov                    skip
        esp_hist_aprov_movto_cc.cod_usuario                   skip
        esp_hist_aprov_movto_cc.dt_digitacao                  skip
        esp_hist_aprov_movto_cc.des_text_histor               skip
        esp_hist_aprov_movto_cc.dat_transacao                 skip
        esp_hist_aprov_movto_cc.val_movto_cta_corren          skip
        esp_hist_aprov_movto_cc.ind_fluxo_movto_cta_corren    SKIP.
    OUTPUT CLOSE.









    c-mess = "<tr>"
     + "<td>" + esp_hist_aprov_movto_cc.cod_cta_corren +                                    "</td>"
     + "<td>" + STRING(esp_hist_aprov_movto_cc.dat_movto_cta_corren, "99/99/9999") +        "</td>"
     + "<td>" + STRING(esp_hist_aprov_movto_cc.num_seq_movto_cta_corren, ">>>9") +          "</td>"
     + "<td>" + esp_hist_aprov_movto_cc.ind_fluxo_movto_cta_corren +                        "</td>"
     + "<td>" + STRING(esp_hist_aprov_movto_cc.val_movto_cta_corren, "->>>>,>>>,>>9.99") +  "</td>"
     + "<td>" + STRING(esp_hist_aprov_movto_cc.dat_transacao , "99/99/9999") +              "</td>"
     + "<td>" + esp_hist_aprov_movto_cc.des_text_histor +                                   "</td>"
     + "<td>" + esp_hist_aprov_movto_cc.cod_usuario +                                       "</td>"
     + "<td>" + STRING(esp_hist_aprov_movto_cc.dt_digitacao) +                              "</td>"
     + "</tr>".

    IF AVAIL esp_pend_movto_cta_corren THEN
        DELETE esp_pend_movto_cta_corren.

    ASSIGN c-assunto = "Aprova‡Æo de Movimento no Caixa e Bancos".
    /*manda e-mail para o usuario que cadastrou o movto no CMG*/
    RUN pi-envia-email (INPUT TRIM(esp_hist_aprov_movto_cc.cod_usuario) + "@yamana.com; V-rPaulucci@Yamana.com; aurelio.alves@Yamana.com; v-wambrosio@yamana.com",
                        INPUT c-assunto,
                        INPUT c-mess-top + c-mess + c-mess-base,
                        INPUT "CMG").
END.

PROCEDURE pi-envia-email:

    DEFINE INPUT PARAMETER p-dest      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p-assunto   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p-mess      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p-modulo    AS CHARACTER NO-UNDO.

    RUN utp/utapi019.p PERSISTENT SET h-utapi019.
    FIND FIRST param_email NO-LOCK NO-ERROR.
    
    EMPTY TEMP-TABLE tt-envio2.
    EMPTY TEMP-TABLE tt-mensagem.
    EMPTY TEMP-TABLE tt-erros.

    CREATE tt-envio2.
    ASSIGN tt-envio2.versao-integracao = 1
           tt-envio2.exchange          = param_email.log_servid_exchange
           tt-envio2.servidor          = param_email.cod_servid_e_mail
           tt-envio2.porta             = param_email.num_porta
           tt-envio2.destino           = p-dest
           tt-envio2.assunto           = p-assunto
           tt-envio2.remetente         = "SustencaoYamana@yamana.com"
           tt-envio2.copia             = ""
           tt-envio2.importancia       = 1
           tt-envio2.log-enviada       = NO
           tt-envio2.log-lida          = NO
           tt-envio2.acomp             = NO
           tt-envio2.formato           = "html".

    CREATE tt-mensagem.                                       
    ASSIGN tt-mensagem.seq-mensagem = 1                         
           tt-mensagem.mensagem     = p-mess. 

    RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                   INPUT  TABLE tt-mensagem,
                                   OUTPUT TABLE tt-erros).
    IF TEMP-TABLE tt-erros:HAS-RECORDS THEN
    DO:
        OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "erro-email-" + p-modulo + "-" + STRING(TODAY, "99999999") + "-" + STRING(ETIME, "99999999") + ".txt").
        FOR EACH tt-erros:
            DISP tt-erros WITH SCROLLABLE.
        END.
        OUTPUT CLOSE.
    END.

    DELETE PROCEDURE h-utapi019.

END PROCEDURE.


