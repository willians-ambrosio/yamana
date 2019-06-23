/****************************************************************************************** 
** 	   Programa: ap001-rp1.i
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 21/05/2018
** Change/Chamado: 
**      Objetivo: Defini‡Æo das tabelas tempor rias utilizadas pelo programa ap001rp.p
**
******************************** CONTROLE DE ALTERA€åES *********************************
** 
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

DEFINE VARIABLE h-acomp           AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-plano-ccusto    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-dados           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-portador        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-carteira        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_plano_conta     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_cod_refer       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l_log_refer_unica AS LOGICAL     NO-UNDO.
DEFINE VARIABLE d_vl_tot_lote     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE c-arquivo_imp     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-batch           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-esp-reemb       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-esp-antecip     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-esp-cartao      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-especie         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE d-vl-tot          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE i-seq-id          AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-tipo-movto      AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-fornec          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-hist            AS CHARACTER   NO-UNDO.

DEFINE BUFFER fornecedor  FOR ems5.fornecedor.
DEFINE BUFFER ccusto      FOR ems5.ccusto.
DEFINE BUFFER espec_docto FOR ems5.espec_docto.

DEF TEMP-TABLE tt_log
    FIELD cdn_fornecedor LIKE fornecedor.cdn_fornecedor
    FIELD cod_empresa AS CHAR
    FIELD id          AS CHAR
    FIELD tipo        AS CHAR
    FIELD linha       AS INTEGER
    FIELD esp         AS CHAR
    FIELD msg         AS CHAR
    FIELD nom_arquivo AS CHAR 
    FIELD datahora    AS DATETIME
    FIELD cod_estab   AS CHAR
    FIELD titulo      AS CHAR 
    FIELD batch_id    AS CHAR
    FIELD cod_esp     AS CHAR
    FIELD valor       AS DEC
    FIELD dt_vencto   AS DATE.

DEF TEMP-TABLE tt_log_item
    FIELD id        AS CHAR
    FIELD titulo    AS CHAR
    FIELD cod_estab AS CHAR
    FIELD cod_esp   AS CHAR
    FIELD conta     AS CHAR
    FIELD ccusto    AS CHAR 
    FIELD valor     AS DEC
    FIELD lancto    AS CHAR.

DEF TEMP-TABLE tt-header
    FIELD qtd_record    AS CHAR
    FIELD TOTAL_amount  AS CHAR
    FIELD BATCH_id      AS CHAR
    FIELD BATCH_date    AS CHAR.

DEF TEMP-TABLE tt-dados
    FIELD BATCH_id  AS CHAR 
    FIELD sequence  AS CHAR 
    FIELD cod_estab AS CHAR 
    FIELD cod_refer AS CHAR 
    FIELD especie   AS CHAR 
    FIELD dt_trans  AS CHAR 
    FIELD pessoa    AS CHAR
    FIELD titulo    AS CHAR
    FIELD valor_total AS DEC FORMAT "->>>,>>>,>>9.9999"
    FIELD parcela    AS CHAR
    FIELD dt_emissao AS DATE
    FIELD dt_vencto  AS DATE
    FIELD dt_depos   AS DATE
    FIELD ccontabil  AS CHAR
    FIELD ccusto     AS CHAR
    FIELD unid_neg   AS CHAR
    FIELD vl_rateio  AS DEC
    FIELD lancto     AS CHAR /* DB OU CR */
    FIELD pais       AS CHAR
    FIELD historico  AS CHAR
    FIELD tipo       AS INT
    FIELD cod_empresa AS CHAR
    FIELD portador    AS CHAR
    FIELD carteira    AS CHAR
    FIELD tipo-fluxo  AS CHAR
    FIELD fora-rateio AS LOG.

DEFINE BUFFER b-tt-dados FOR tt-dados.

DEF TEMP-TABLE tt_titulo
        FIELD cod_empresa     LIKE tit_ap.cod_empresa
        FIELD cod_estab       LIKE tit_ap.cod_estab
        FIELD cdn_fornec      LIKE tit_ap.cdn_fornec
        FIELD cod_tit_ap      LIKE tit_ap.cod_tit_ap
        FIELD cod_ser_docto   LIKE tit_ap.cod_ser_docto
        FIELD cod_espec_docto LIKE tit_ap.cod_espec_docto
        FIELD cod_parcela     LIKE tit_ap.cod_parcela    
        FIELD cod_refer       LIKE tit_ap.cod_refer      
        FIELD cod_portador    LIKE tit_ap.cod_portador
        FIELD cod_carteira    AS CHAR
        FIELD dt_trans        LIKE tit_ap.dat_transac
        FIELD valor           LIKE tit_ap.val_origin_tit_ap
        FIELD dt_emissao      LIKE tit_ap.dat_emis_docto
        FIELD dt_vencto       LIKE tit_ap.dat_vencto_tit_ap
        FIELD sequencia       AS INT 
        FIELD cod_indic_econ  LIKE histor_finalid_econ.cod_indic_econ
        FIELD cod_tip_fluxo   LIKE fornec_financ.cod_tip_fluxo_financ
        FIELD cod_tit_antecip LIKE tit_ap.cod_tit_ap
        FIELD lancto          AS CHAR /* + = DB / - = CR*/
        FIELD r_row_antecip   AS ROWID
        FIELD tipo            AS INT
        FIELD historico       AS CHAR.
                                                                        
DEF TEMP-TABLE tt_rateio
       FIELD cod_estab       LIKE tit_ap.cod_estab
       FIELD sequencia       AS INT
       FIELD cdn_fornec      AS INT
       FIELD titulo          AS CHAR
       FIELD especie         AS CHAR
       FIELD cod_plano_cta_ctbl LIKE plano_cta_ctbl.cod_plano_cta_ctbl
       FIELD ccontabil       LIKE cta_ctbl.cod_cta_ctbl 
       FIELD cod_tip_fluxo   LIKE fornec_financ.cod_tip_fluxo_financ
       FIELD cod_plano_ccusto LIKE plano_ccusto.cod_plano_ccusto
       FIELD ccusto          LIKE ccusto.cod_ccusto 
       FIELD valor           AS DECIMAL
       FIELD lancto          AS CHAR
       FIELD unid_neg        AS CHAR.

DEF {1} SHARED TEMP-TABLE tt-rateio 
       FIELD id              AS CHAR
       FIELD sequencia       AS INT
       FIELD ccontabil       LIKE cta_ctbl.cod_cta_ctbl 
       FIELD ccusto          LIKE ccusto.cod_ccusto 
       FIELD valor           AS DECIMAL
       FIELD lancto          AS CHAR.

DEF TEMP-TABLE tt-arquivo
    FIELD carq AS CHAR.

/* Para reprocessar arquivos */      
DEF {1} SHARED TEMP-TABLE tt-titulo
      FIELD dt_repros       LIKE log_concur_titulo.dt_repros
      FIELD valor           LIKE log_concur_titulo.valor  
      FIELD log_reprocess   AS LOG FORMAT "Sim/NÆo":U VIEW-AS TOGGLE-BOX
      FIELD id              AS CHAR
      FIELD cod_empresa     AS CHAR COLUMN-LABEL "Emp" FORMAT "x(3)":U
      FIELD cod_estab       AS CHAR FORMAT "x(05)":U
      FIELD cod_esp         AS CHAR FORMAT "x(4)":U
      FIELD cdn_fornec      AS INTEGER FORMAT ">>>,>>>,>>9":U
      FIELD titulo          AS CHAR FORMAT "x(16)":U
      FIELD datahora        AS DATETIME FORMAT "99/99/9999 HH:MM:SS.SSS":U
      FIELD arquivo         AS CHAR. 

DEF BUFFER b-tt-titulo FOR tt-titulo.

function fncLogReferUnicaNormal returns logical
  ( p_cod_estab  as char,
    p_cod_refer  as char,
    p_cod_table  as char,
    p_rec_tabela as recid):

    def buffer bf-antecip_pef_pend   for antecip_pef_pend.
    def buffer bf-impto_impl_pend_ap for impto_impl_pend_ap.
    def buffer bf-item_lote_impl_ap  for item_lote_impl_ap.
    def buffer bf-nota_pend_cartcred for nota_pend_cartcred.
    def buffer bf-lote_pagto         for lote_pagto.

    def buffer bf-lote_impl_tit_ap for lote_impl_tit_ap.
    def buffer bf-movto_tit_ap     for movto_tit_ap.

    if  p_cod_table <> "antecip_pef_pend" and
        can-find(first bf-antecip_pef_pend no-lock use-index antcppfp_id
                 where bf-antecip_pef_pend.cod_estab = p_cod_estab
                   and bf-antecip_pef_pend.cod_refer = p_cod_refer
                   and recid(bf-antecip_pef_pend)   <> p_rec_tabela) then
        return no.

    if  p_cod_table <> "impto_impl_pend_ap" and
        can-find(first bf-impto_impl_pend_ap no-lock use-index imptmplp_lote
                 where bf-impto_impl_pend_ap.cod_estab_refer = p_cod_estab
                   and bf-impto_impl_pend_ap.cod_refer       = p_cod_refer
                   and recid(bf-impto_impl_pend_ap)         <> p_rec_tabela) then
        return no.

    if  p_cod_table <> "item_lote_impl_ap" and
        can-find(first bf-item_lote_impl_ap no-lock use-index itmltmpl_id
                 where bf-item_lote_impl_ap.cod_estab = p_cod_estab
                   and bf-item_lote_impl_ap.cod_refer = p_cod_refer
                   and recid(bf-item_lote_impl_ap)   <> p_rec_tabela) then
        return no.

    if  p_cod_table <> "nota_pend_cartcred" and
        can-find(first bf-nota_pend_cartcred no-lock use-index ntpndcrt_id
                 where bf-nota_pend_cartcred.cod_estab = p_cod_estab
                   and bf-nota_pend_cartcred.cod_refer = p_cod_refer
                   and recid(bf-nota_pend_cartcred)   <> p_rec_tabela) then
        return no.

    if  p_cod_table <> "lote_pagto" and
        can-find(first bf-lote_pagto no-lock use-index ltpgt_id
                 where bf-lote_pagto.cod_estab_refer = p_cod_estab
                   and bf-lote_pagto.cod_refer       = p_cod_refer
                   and recid(bf-lote_pagto)         <> p_rec_tabela) then
        return no.

    if  p_cod_table <> "lote_impl_tit_ap" and
        can-find(first bf-lote_impl_tit_ap no-lock use-index ltmplttp_id
                 where bf-lote_impl_tit_ap.cod_estab = p_cod_estab
                   and bf-lote_impl_tit_ap.cod_refer = p_cod_refer
                   and recid(bf-lote_impl_tit_ap)   <> p_rec_tabela) then
        return no.

    if  can-find(first bf-movto_tit_ap no-lock use-index mvtttp_refer
                 where bf-movto_tit_ap.cod_estab = p_cod_estab
                   and bf-movto_tit_ap.cod_refer = p_cod_refer
                   and recid(bf-movto_tit_ap)   <> p_rec_tabela) then
        return no.

    if  can-find(first his_movto_tit_ap_histor no-lock use-index hsmvtttp_id
                 where his_movto_tit_ap_histor.cod_estab = p_cod_estab
                   and his_movto_tit_ap_histor.cod_refer = p_cod_refer) then
        return no.

    return yes.
end function.


function fncLogReferUnicaAntecip returns logical
  ( p_cod_estab  as char,
    p_cod_refer  as char,
    p_cod_table  as char,
    p_rec_tabela as recid):

    def buffer bf-antecip_pef_pend   for antecip_pef_pend.
    def buffer bf-aprop_ctbl_pend_ap for aprop_ctbl_pend_ap.
    def buffer bf-movto_tit_ap       for movto_tit_ap.

    if  p_cod_table <> "antecip_pef_pend" and
        can-find(first bf-antecip_pef_pend use-index antcppfp_id
                 where bf-antecip_pef_pend.cod_estab = p_cod_estab
                   and bf-antecip_pef_pend.cod_refer = p_cod_refer
                   and recid(bf-antecip_pef_pend)   <> p_rec_tabela) then
        return no.

    if  p_cod_table <> "aprop_ctbl_pend_ap" and
        can-find(first bf-aprop_ctbl_pend_ap use-index aprpctba_id
                 where bf-aprop_ctbl_pend_ap.cod_estab = p_cod_estab
                   and bf-aprop_ctbl_pend_ap.cod_refer = p_cod_refer
                   and recid(bf-aprop_ctbl_pend_ap)   <> p_rec_tabela) then
        return no.

    if  can-find(first bf-movto_tit_ap use-index mvtttp_refer
                 where bf-movto_tit_ap.cod_estab = p_cod_estab
                   and bf-movto_tit_ap.cod_refer = p_cod_refer
                   and recid(bf-movto_tit_ap)   <> p_rec_tabela) then
        return no.

    return yes.
end function.


function fncDiaVencto returns DATE
  (p_data AS DATE):

    DEFINE VARIABLE dt AS DATE NO-UNDO.
    DEFINE VARIABLE l-ok AS LOGICAL  NO-UNDO INITIAL NO.

    /* Pagamentos todos ocorrem as sextas-feiras*/
    ASSIGN dt = p_data.
    
    DO  WHILE WEEKDAY(dt) <> 6:
    
        ASSIGN dt = dt + 1.
    
    END.
    
    /* Se for o œltimo dia do m¼s postergar para o pr½ximo dia œtil */
    IF dt = (DATE(MONTH(dt + 28),1,YEAR(dt + 28)) - 1)  
        THEN ASSIGN dt = DATE(MONTH(dt + 28),1,YEAR(dt + 28)).

    /* Verifica se n’o ² sÿbado ou domingo */
    IF WEEKDAY(dt) = 7 THEN 
        ASSIGN dt = dt + 2.
    
    IF WEEKDAY(dt) = 1 THEN
        ASSIGN dt = dt + 1.

    /* Verifica calendÿrio */
    DO WHILE NOT l-ok:

        FIND dia_calend_glob NO-LOCK 
             WHERE dia_calend_glob.cod_calend = "FISCAL" 
             AND   dia_calend_glob.dat_calend = dt NO-ERROR.
        IF AVAIL dia_calend_glob THEN DO:

            IF NOT dia_calend_glob.LOG_dia_util
              THEN ASSIGN dt = dt + 1.
            ELSE ASSIGN l-ok = YES.
        END.
    END.

    RETURN dt.

end function.
