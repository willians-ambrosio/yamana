/******************************************************************************
**  Empresa: Yamana
**  Programa: 
**  Data....: Janeiro de 2016.
**  Autor...: Emerson Galdino - DSC
**  Objetivo: Relat¢rio de Gastos
*******************************************************************************/

{include/i-prgvrs.i espab008rp 2.06.00.000}
{utp/ut-glob.i}

define temp-table tt-param no-undo
    field destino               as integer
    field arquivo               as char format "x(35)"
    field usuario               as char format "x(12)"
    field data-exec             as date
    field hora-exec             as integer
    field classifica            as integer
    field desc-classifica       as char format "x(40)"
    field modelo-rtf            as char format "x(35)"
    field l-habilitaRtf         as LOG
    FIELD estab_selc            AS CHAR FORMAT "X(2000)"
    FIELD cdn_fornecedor_ini    LIKE es-relat-cat.cdn_fornecedor
    field cdn_fornecedor_fim    LIKE es-relat-cat.cdn_fornecedor
    FIELD cod_espec_docto_ini   AS CHAR FORMAT "X(200)"
    FIELD cod_espec_docto_fim   LIKE es-relat-cat.cod_espec_docto
    field dt_emiss_ini          AS DATE
    field dt_emiss_fim          AS DATE
    FIELD dt_vencimento_ini     AS DATE
    FIELD dt_vencimento_fim     AS DATE
    FIELD cod_empresa_ini       LIKE ems5.empresa.cod_empresa
    FIELD cod_empresa_fim       LIKE ems5.empresa.cod_empresa
    FIELD arq_de_para           AS CHAR FORMAT "X(2000)".

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def temp-table tt-raw-digita
   field raw-digita      as raw.

DEFINE TEMP-TABLE tt-titulos
    field cod-fornecedor        LIKE ems5.fornecedor.nom_pessoa
    field cod_espec_docto       like tit_ap.cod_espec_docto                 
    field cod_tit_ap            like tit_ap.cod_tit_ap                      
    field dat_emis_docto        like tit_ap.dat_emis_docto                  
    field cod_parcela           like tit_ap.cod_parcela                     
    field dat_transacao         like tit_ap.dat_transacao                   
    FIELD num-pagemtento        AS INT FORMAT ">>>>,>>>,>>9"
    FIELD dat_liquidac_tit_ap   LIKE tit_ap.dat_liquidac_tit_ap             
    FIELD cat-code              AS CHAR FORMAT "x(200)"
    FIELD categoria-cat-code    AS CHAR FORMAT "x(200)"
    FIELD histor-tit            AS CHAR                     
    FIELD val_origin_tit_ap     LIKE tit_ap.val_origin_tit_ap               
    FIELD val_dollar            LIKE tit_ap.val_origin_tit_ap       
    FIELD val_pagto_tit_ap      LIKE tit_ap.val_pagto_tit_ap                
    FIELD val_desc_tit_ap       LIKE tit_ap.val_desc_tit_ap                 
    FIELD cod_indic_econ        LIKE aprop_ctbl_ap.cod_indic_econ           
    FIELD COD_CTA_CTBL          LIKE cta_ctbl.COD_CTA_CTBL        
    FIELD conta_ccusto          AS CHAR
    FIELD DESC_conta            AS CHAR
    FIELD cod_unid_negoc        LIKE aprop_ctbl_ap.cod_unid_negoc           
    FIELD cod_Estab             LIKE tit_ap.cod_estab                       
    FIELD cod_ccusto            LIKE aprop_ctbl_ap.cod_ccusto       
    FIELD cod_plano_ccusto      LIKE aprop_ctbl_ap.cod_plano_ccusto 
    FIELD Object_Account        AS CHAR.

DEFINE TEMP-TABLE tt-de-para
    FIELD conta_ccusto          AS CHAR
    FIELD estma_gove            AS CHAR
    FIELD cod_empresa           AS CHAR
    FIELD Account_Description   AS CHAR
    INDEX idx01 cod_empresa conta_ccusto.

DEFINE TEMP-TABLE tt-linhas
    FIELD linha-negrito  AS INT.

DEFINE TEMP-TABLE tt-es-relat-cat LIKE es-relat-cat
    INDEX idx01
    cod_empresa       
    cdn_fornecedor    
    cod_cta_ctbl    
    cod_espec_docto   
    cod_plano_ccusto  
    cod_ccusto        
    cod_plano_cta_ctbl.

/* Vari veis do Relat¢rio */
DEFINE VARIABLE de-valor-dolar      LIKE tit_ap.val_origin_tit_ap.
DEFINE VARIABLE de-valor-nota       LIKE  movto-estoq.valor-nota.
DEFINE VARIABLE c-conta-contabil    LIKE cta_ctbl.cod_cta_ctbl. 
DEFINE VARIABLE c-ccusto            LIKE movto-estoq.sc-saldo.
DEFINE VARIABLE C-plano-ccusto      LIKE aprop_ctbl_ap.cod_plano_ccusto.
DEFINE VARIABLE c-desc-conta        LIKE cta_ctbl.des_tit_ctbl.

DEFINE VARIABLE c-histor-tit            AS CHARACTER                NO-UNDO.
DEFINE VARIABLE v_des_estab_select      AS CHARACTER                NO-UNDO.
DEFINE VARIABLE v_num_contador          AS INTEGER                  NO-UNDO.
DEFINE VARIABLE v_num_espec_ems2        AS INTEGER                  NO-UNDO. /*local*/
DEFINE VARIABLE i-num-pagamento         AS INTEGER FORMAT ">>>>,>>>,>>9".
DEFINE VARIABLE v_dat_transacao_ini     AS DATE                     NO-UNDO.
DEFINE VARIABLE v_dat_transacao_fim     AS DATE                     NO-UNDO.
DEFINE VARIABLE v_dat_aux               AS DATE                     NO-UNDO.
DEFINE VARIABLE v_row_nota              AS ROWID                    NO-UNDO. /*local*/
DEFINE VARIABLE c-matriz-org            AS CHARACTER   NO-UNDO.

/* Vari veis Excel */
DEFINE VARIABLE c-arq-csv               AS CHARACTER                NO-UNDO.
DEFINE VARIABLE c-arq-xlsx              AS CHARACTER                    NO-UNDO.
DEFINE VARIABLE cSaidaCodBarra          AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cNomeArquivo            AS CHARACTER                NO-UNDO.
DEFINE VARIABLE i-linha                 AS INTEGER      INITIAL 1   NO-UNDO.
DEFINE VARIABLE i-linha-de-para         AS INTEGER      INITIAL 1   NO-UNDO.
DEFINE VARIABLE chExcelApp              AS COM-HANDLE                   NO-UNDO.
DEFINE VARIABLE chworkbook              AS COM-HANDLE                   NO-UNDO.
DEFINE VARIABLE chworksheet             AS COM-HANDLE                   NO-UNDO.

/* Vari veis de totaliza‡Æo */
DEFINE VARIABLE de-tot-origin-tit-ap    like tt-titulos.val_origin_tit_ap   NO-UNDO.
DEFINE VARIABLE de-tot-dollar           like tt-titulos.val_origin_tit_ap   NO-UNDO.
DEFINE VARIABLE de-tot-pagto-tit-ap     like tt-titulos.val_origin_tit_ap   NO-UNDO.
DEFINE VARIABLE de-tot-desc-tit-ap      like tt-titulos.val_origin_tit_ap   NO-UNDO.
DEFINE VARIABLE c-banco                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i                       AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-categoria-cat-code    AS CHARACTER   FORMAT "x(200)" NO-UNDO.
/* Variavel de Acompanhamento */
DEFINE VARIABLE h-acomp                 AS HANDLE                   NO-UNDO.

/* Buffers */
DEF BUFFER b-docum-est FOR docum-est.
DEF BUFFER b-movto_tit_ap FOR movto_tit_ap.

/* Stream */
DEFINE STREAM s-csv.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* Include PadrÊo para Vari˜veis de Relat«rio  */
{include/i-rpvar.i}

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

{include/i-rpcab.i}
{include/i-rpout.i}

FIND FIRST param-global NO-LOCK NO-ERROR.

ASSIGN
    c-programa     = "ESAPB008rp"
    c-versao       = "5.06"
    c-revisao      = "00.001"
    c-empresa      = param-global.grupo
    c-sistema      = "CVP"
    c-titulo-relat = "Verbas".

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

ASSIGN c-arq-csv   = tt-param.arquivo
       c-arq-csv   = REPLACE(c-arq-csv,"~\","/")
       c-arq-csv   = REPLACE(c-arq-csv,ENTRY(NUM-ENTRIES(c-arq-csv,"/"),c-arq-csv,"/"),"ESAPB008-" + REPLACE(STRING(TIME,"hh:mm"),":","") + ".csv").

{include/i-rpout.i &STREAM="stream s-csv" &tofile=c-arq-csv &pagesize=0}

/* RUN pi-importa-de-para. */

    MESSAGE "dE pARA"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH es-relat-cat:
    CREATE tt-es-relat-cat.
    BUFFER-COPY es-relat-cat TO tt-es-relat-cat.
END.

RUN pi-cria-tt.

/* RUN pi-gera-csv. */

{include/i-rpclo.i &STREAM="stream s-csv"}

RUN pi-excel(INPUT c-arq-csv).

run pi-finalizar in h-acomp.

{include/i-rpclo.i}

PROCEDURE pi-cria-tt:

    ASSIGN v_des_estab_select   =   tt-param.estab_selc.

    FIND FIRST ems5.trad_org_ext WHERE
           ems5.trad_org_ext.cod_matriz_trad_org_ext    =   "ems2"  AND
           ems5.trad_org_ext.cod_tip_unid_organ         =   "998"     AND
           ems5.trad_org_ext.cod_unid_organ_ext         =   v_cdn_empres_usuar.
    IF AVAIL ems5.trad_org_ext THEN
        ASSIGN c-matriz-org =   ems5.trad_org_ext.cod_unid_organ.
    ELSE
        ASSIGN c-matriz-org =   "".

    block_estab:
    do v_num_contador = 1 to num-entries(v_des_estab_select):

        FOR EACH estabelecimento NO-LOCK WHERE
                 estabelecimento.cod_estab = entry(v_num_contador, v_des_estab_select):

            titulos:
            FOR EACH tit_ap NO-LOCK WHERE
                     tit_ap.cod_estab             = estabelecimento.cod_estab      and
                     tit_ap.cdn_fornecedor       >= tt-param.cdn_fornecedor_ini    and
                     tit_ap.cdn_fornecedor       <= tt-param.cdn_fornecedor_fim    and
                     tit_ap.cod_espec_docto      >= tt-param.cod_espec_docto_ini   and
                     tit_ap.cod_espec_docto      <= tt-param.cod_espec_docto_fim   and
                     tit_ap.dat_emis_docto       >= tt-param.dt_emiss_ini          and
                     tit_ap.dat_emis_docto       <= tt-param.dt_emiss_fim          and
                     tit_ap.dat_vencto_tit_ap    >= tt-param.dt_vencimento_ini     and
                     tit_ap.dat_vencto_tit_ap    <= tt-param.dt_vencimento_fim     AND
                     tit_ap.cod_empresa           = c-matriz-org
                BREAK BY tit_ap.cod_estab 
                      BY tit_ap.dat_emis_docto:

                FIND FIRST movto_tit_ap OF tit_ap where 
                           movto_tit_ap.ind_trans_ap = "Implanta‡Æo" NO-LOCK NO-ERROR. 
                IF NOT AVAIL movto_tit_ap THEN NEXT.

                FIND FIRST aprop_ctbl_ap OF movto_tit_ap where
                           aprop_ctbl_ap.ind_natur_lancto_ctbl = "DB" NO-LOCK NO-ERROR.
                IF NOT AVAIL aprop_ctbl_ap THEN NEXT.
                                                
                FIND FIRST tt-es-relat-cat WHERE
                           tt-es-relat-cat.cod_empresa        =   tit_ap.cod_empresa    AND
                           tt-es-relat-cat.cdn_fornecedor     =   tit_ap.cdn_fornecedor AND

                          ((tt-es-relat-cat.cod_cta_ctbl       =   aprop_ctbl_ap.cod_cta_ctbl OR
                            tt-es-relat-cat.cod_cta_ctbl       =   ?)  AND

                          (tt-es-relat-cat.cod_espec_docto    =   tit_ap.cod_espec_docto OR
                           tt-es-relat-cat.cod_espec_docto    =   ?)  AND

                          (tt-es-relat-cat.cod_plano_ccusto   =   aprop_ctbl_ap.cod_plano_ccusto OR
                           tt-es-relat-cat.cod_plano_ccusto   =   ?)  AND

                          (tt-es-relat-cat.cod_ccusto         =   aprop_ctbl_ap.cod_ccusto OR
                           tt-es-relat-cat.cod_ccusto         =   ?)  AND

                          (tt-es-relat-cat.cod_plano_cta_ctbl =   aprop_ctbl_ap.cod_plano_cta_ctbl OR
                           tt-es-relat-cat.cod_plano_cta_ctbl =   ?)) NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-es-relat-cat THEN
                    NEXT titulos.

                RUN pi-acompanhar in h-acomp (input "Processando Estab: " + tit_ap.cod_estab + " -  Data: " + STRING( tit_ap.dat_transacao )).
        
                FIND FIRST ems5.fornecedor NO-LOCK OF tit_ap.
        
                ASSIGN c-conta-contabil = ""
                       v_row_nota       = ?
                       c-ccusto         = ""
                       C-plano-ccusto   = "".
                
                IF aprop_ctbl_ap.cod_cta_ctbl = "17202002" THEN DO:
                    
                    find first ems5.espec_docto NO-LOCK where
                                    espec_docto.cod_espec_docto = tit_ap.cod_espec_docto no-error.
                    if  avail espec_docto then do:
                        case espec_docto.ind_tip_espec_docto:
                            when "PrevisÆo" /*l_previsao*/  then
                                assign v_num_espec_ems2 = 1.
                            when "Normal" /*l_normal*/  then
                                assign v_num_espec_ems2 = 2.
                            when "" /*l_null*/  then
                                assign v_num_espec_ems2 = 3.
                        END.
                    END.
        
                    IF aprop_ctbl_ap.cod_cta_ctbl = "17202002" THEN DO:
    
/*                         FIND FIRST val_tit_ap OF tit_ap NO-LOCK NO-ERROR.                                                                                                        */
/*                         IF AVAIL val_tit_ap THEN DO:                                                                                                                             */
/*                             FIND tip_fluxo_financ OF val_tit_ap WHERE                                                                                                            */
/*                                  tip_fluxo_financ.dat_inic_valid >= TODAY AND                                                                                                    */
/*                                  tip_fluxo_financ.dat_fim_valid  <= TODAY  NO-LOCK NO-ERROR.                                                                                     */
/*                             IF AVAIL tip_fluxo_financ THEN DO:                                                                                                                   */
/*                                 ASSIGN c-conta-contabil =   IF tip_fluxo_financ.cod_cta_ctbl <> "" THEN tip_fluxo_financ.cod_cta_ctbl ELSE tip_fluxo_financ.cod_tip_fluxo_financ */
/*                                        c-ccusto         =   ""                                                                                                                   */
/*                                        C-plano-ccusto   =   tip_fluxo_financ.cod_plano_cta_ctbl.                                                                                 */
/*                             END.                                                                                                                                                 */
/*                         END.                                                                                                                                                     */
                        run rep/reapi011.p (input 1,  /* 1- Verifica a exist¬ncia de NF no Recebimento */
                                            input tit_ap.cdn_fornecedor,
                                            input tit_ap.cod_tit_ap,
                                            input tit_ap.cod_ser_docto,
                                            input v_num_espec_ems2,
                                            input-output v_row_nota).

                        IF v_row_nota <> ? THEN DO:
    
                            FIND b-docum-est WHERE
                                 ROWID(b-docum-est) = v_row_nota NO-LOCK NO-ERROR.
                            IF AVAIL b-docum-est THEN DO:
    
                                ASSIGN de-valor-nota = 0.
    
                                FOR EACH movto-estoq OF b-docum-est where
                                         movto_tit_ap.ind_trans_ap = "Implanta‡Æo" AND
                                         movto-estoq.tipo-trans = 2 NO-LOCK:
    
                                    IF movto-estoq.valor-nota > de-valor-nota AND
                                       movto-estoq.ct-codigo <> "17202002" THEN DO:
                                        ASSIGN de-valor-nota       = movto-estoq.valor-nota
                                               c-conta-contabil    = movto-estoq.ct-codigo
                                               c-ccusto            = movto-estoq.sc-codigo
                                               C-plano-ccusto      = "PLCCUNI".
                                    END.
                                END.
                            END.
                        END.
                    END.
                END. 
        
                IF c-conta-contabil = "" THEN
                    ASSIGN c-conta-contabil = aprop_ctbl_ap.cod_cta_ctbl.

/*                 ASSIGN c-desc-conta = "".                                       */
/*                 FIND cta_ctbl WHERE                                             */
/*                      cta_ctbl.cod_cta_ctbl = c-conta-contabil NO-LOCK NO-ERROR. */
/*                 IF AVAIL cta_ctbl THEN                                          */
/*                     ASSIGN c-desc-conta =   cta_ctbl.des_tit_ctbl.              */
        
                IF c-ccusto = "" THEN
                    ASSIGN c-ccusto         = aprop_ctbl_ap.cod_ccusto
                           C-plano-ccusto   = "".
        
                FIND LAST histor_tit_movto_ap NO-LOCK where
                          histor_tit_movto_ap.cod_estab             = tit_ap.cod_estab                            and
                          histor_tit_movto_ap.num_id_tit_ap         = tit_ap.num_id_tit_ap                    and
                          histor_tit_movto_ap.num_id_movto_tit_ap   = movto_tit_ap.num_id_movto_tit_ap  and 
                          histor_tit_movto_ap.ind_orig_histor_ap    <> "Erro" NO-ERROR.
                IF AVAIL histor_tit_movto_ap THEN
                    ASSIGN c-histor-tit = histor_tit_movto_ap.des_text_histor.
                ELSE
                    ASSIGN c-histor-tit = "".
        
                FIND ems5.espec_docto OF tit_ap.
                IF espec_docto.ind_tip_espec_docto = "Imposto Retido" THEN
                    NEXT titulos.
        
                FIND FIRST cotac_parid WHERE
                           cotac_parid.cod_indic_econ_base  = "REAL"   AND
                           cotac_parid.cod_indic_econ_idx   = "DOLAR"       AND   
                           cotac_parid.ind_tip_cotac_parid  = "REAL"   AND   
                           cotac_parid.dat_cotac_indic_econ = tit_ap.dat_transacao NO-LOCK NO-ERROR.
                IF AVAIL cotac_parid THEN
                    ASSIGN de-valor-dolar = tit_ap.val_origin_tit_ap / cotac_parid.val_cotac_indic_econ  .
                ELSE
                    ASSIGN de-valor-dolar = 0.
        
                FOR EACH b-movto_tit_ap OF tit_ap WHERE
                         b-movto_tit_ap.ind_trans_ap        = "Baixa" AND 
                         b-movto_tit_ap.log_movto_estordo   = NO NO-LOCK. 
                    
                    FIND FIRST compl_movto_pagto OF b-movto_tit_ap WHERE
                              NO-LOCK NO-ERROR.    
                    IF AVAIL compl_movto_pagto THEN DO:
        
                        IF compl_movto_pagto.ind_modo_pagto = "Cheque"  THEN
                            ASSIGN i-num-pagamento  =   compl_movto_pagto.num_cheque.
                        ELSE IF compl_movto_pagto.ind_modo_pagto = "Border“" THEN
                            ASSIGN i-num-pagamento  =   compl_movto_pagto.num_bord_ap.
                        ELSE
                            ASSIGN i-num-pagamento  =   0.

                        FIND FIRST es-cat-code WHERE
                                   es-cat-code.cat-code = tt-es-relat-cat.cat-code NO-LOCK NO-ERROR.
                        IF AVAIL es-cat-code THEN DO:
                            IF es-cat-code.categoria-code = 1 THEN
                                ASSIGN c-categoria-cat-code = "801 - Federal Government (Foreign)".
                            ELSE IF es-cat-code.categoria-code = 2 THEN
                                ASSIGN c-categoria-cat-code = "802 - Provincial Government(Foreign)".
                            ELSE IF es-cat-code.categoria-code = 3 THEN
                                ASSIGN c-categoria-cat-code = "803 - Municipal Government(Foreign)".
                            ELSE
                                ASSIGN c-categoria-cat-code = "804 - Govt-assocatd Agency (Foreign)".
                        END.
                        ELSE
                            ASSIGN c-categoria-cat-code = "".
        
                        CREATE tt-titulos.
                        ASSIGN tt-titulos.cod-fornecedor        =   string(fornecedor.cdn_fornecedor) + "-" + fornecedor.nom_pessoa                         
                               tt-titulos.cod_espec_docto       =   CAPS(tit_ap.cod_espec_docto) + " - " + espec_docto.des_espec_docto                                                           
                               tt-titulos.cod_tit_ap            =   tit_ap.cod_tit_ap                                                               
                               tt-titulos.dat_emis_docto        =   tit_ap.dat_emis_docto
                               tt-titulos.cod_parcela           =   tit_ap.cod_parcela                                                        
                               tt-titulos.dat_transacao         =   tit_ap.dat_transacao 
                               tt-titulos.num-pagemtento        =   i-num-pagamento                                               
                               tt-titulos.dat_liquidac_tit_ap   =   tit_ap.dat_liquidac_tit_ap                                    
                               tt-titulos.categoria-cat-code    =   c-categoria-cat-code
                               tt-titulos.cat-code              =   IF AVAIL es-cat-code THEN es-cat-code.cat-code + " - " + es-cat-code.desc-cat-code ELSE "N/A"
                               tt-titulos.histor-tit            =   REPLACE(c-histor-tit,CHR(10),"")                              
                               tt-titulos.val_origin_tit_ap     =   tit_ap.val_origin_tit_ap                                      
                               tt-titulos.val_dollar            =   de-valor-dolar                                             
                               tt-titulos.val_pagto_tit_ap      =   tit_ap.val_pagto_tit_ap                                       
                               tt-titulos.val_desc_tit_ap       =   tit_ap.val_desc_tit_ap                                        
                               tt-titulos.cod_indic_econ        =   aprop_ctbl_ap.cod_indic_econ                                  
                               tt-titulos.COD_CTA_CTBL          =   c-conta-contabil       
/*                                tt-titulos.DESC_conta            =   c-desc-conta */
                               tt-titulos.conta_ccusto          =   c-conta-contabil + c-ccusto       
                               tt-titulos.cod_unid_negoc        =   aprop_ctbl_ap.cod_unid_negoc                               
                               tt-titulos.cod_Estab             =   tit_ap.cod_estab                                          
                               tt-titulos.cod_ccusto            =   c-ccusto /* aprop_ctbl_ap.cod_ccusto                                      */ .
/*                                tt-titulos.cod_plano_ccusto      =   C-plano-ccusto /*aprop_ctbl_ap.cod_plano_ccusto*/ . */

                        FIND FIRST tt-de-para WHERE
                                   tt-de-para.cod_emp      =   v_cdn_empres_usuar                 AND
                                   tt-de-para.conta_ccusto =   tt-titulos.conta_ccusto NO-LOCK NO-ERROR.
                        IF AVAIL tt-de-para THEN
                            ASSIGN tt-titulos.Object_Account    =   tt-de-para.estma_gove
                                   tt-titulos.DESC_conta        =   tt-de-para.Account_Description.
                        ELSE
                            ASSIGN tt-titulos.Object_Account    =   "N/A"
                                   tt-titulos.DESC_conta        =   "N/A".
        
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pi-gera-csv:

    EXPORT STREAM s-csv DELIMITER ";"
        "Supplier Number"
        "Document Type"
        "Document (Voucher, Invoice, etc.)"
        "Supplier Invoice Number"
        "Invoice Date"
        "Document Company" /*Isso ‚ o c¢digo da empresa no Datasul*/
        "Pay Item"
        "GL Date"
        "Document Type - Matching"
        "Payment Number"
        "Payment Date"
        "Vendor Category Code"
        "Remark"
        "Reais"
        "Dollares"
        "Payment Amount (AA)"
        "Discount Taken"
        "Currency Code - From"
        "Account ID"
        "Cost Centre from Datasul"
        "Account + Cost Center"
        "Account Description"
/*         "Object Account Cat code" */
        "Business Unit"
        "Object Account"
        "Legacy Entity Number"
        "Entity Name"
        "Payment to Government (Object Account Cat code)".
        
/*         "Subledger Type" */
/*         "Object Account". */
    
    FOR EACH tt-titulos
        BREAK BY tt-titulos.cod-fornecedor:


        FIND estabelecimento WHERE
             estabelecimento.cod_estab    =   tt-titulos.cod_estab NO-LOCK NO-ERROR.

        ASSIGN de-tot-origin-tit-ap = de-tot-origin-tit-ap  + tt-titulos.val_origin_tit_ap
               de-tot-dollar        = de-tot-dollar         + tt-titulos.val_dollar       
               de-tot-pagto-tit-ap  = de-tot-pagto-tit-ap   + tt-titulos.val_pagto_tit_ap 
               de-tot-desc-tit-ap   = de-tot-desc-tit-ap    + tt-titulos.val_desc_tit_ap  .

        ASSIGN i-linha = i-linha + 1.

        EXPORT STREAM s-csv DELIMITER ";"
            tt-titulos.cod-fornecedor                     
            tt-titulos.cod_espec_docto                 
            tt-titulos.cod_tit_ap  
            tt-titulos.cod_tit_ap  
            tt-titulos.dat_emis_docto                  
            v_cdn_empres_usuar
            tt-titulos.cod_parcela                     
            tt-titulos.dat_transacao                   
            ""                                         
            tt-titulos.num-pagemtento                  
            tt-titulos.dat_liquidac_tit_ap             
            tt-titulos.categoria-cat-code                        
            tt-titulos.histor-tit                      
            tt-titulos.val_origin_tit_ap               
            tt-titulos.val_dollar                      
            tt-titulos.val_pagto_tit_ap                
            tt-titulos.val_desc_tit_ap                 
            tt-titulos.cod_indic_econ                  
            tt-titulos.COD_CTA_CTBL    
            tt-titulos.cod_ccusto                      
            tt-titulos.conta_ccusto  + CHR(32)
            tt-titulos.DESC_conta
/*             tt-titulos.Object_Account */
            tt-titulos.cod_unid_negoc                  
            ""                                         
            tt-titulos.cod_Estab                       
            estabelecimento.nom_pessoa
            tt-titulos.cat-code                        
            
/*             tt-titulos.cod_plano_ccusto */
            .

        IF LAST-OF(tt-titulos.cod-fornecedor) THEN DO:

            ASSIGN i-linha = i-linha + 1.

            CREATE tt-linhas.
            ASSIGN tt-linhas.linha-negrito  =   i-linha.

            EXPORT STREAM s-csv DELIMITER ";"
                             "TOTAL " + tt-titulos.cod-fornecedor 
                             ""                                    
                             ""                                    
                             ""                                    
                             ""                                    
                             ""                                    
                             ""                                    
                             ""                                    
                             ""                                    
                             ""                                    
                             ""                                    
                             ""                                    
                             ""                                    
                            de-tot-origin-tit-ap                 
                            de-tot-dollar                        
                            de-tot-pagto-tit-ap                  
                            de-tot-desc-tit-ap                   .
                        
            ASSIGN de-tot-origin-tit-ap = 0
                   de-tot-dollar        = 0
                   de-tot-pagto-tit-ap  = 0
                   de-tot-desc-tit-ap   = 0.
        END.
    END.      

END PROCEDURE.

PROCEDURE pi-excel:
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-arq    AS CHARACTER   NO-UNDO.

    IF tt-param.destino <> 3 /* Terminal */ THEN
        RETURN.

    ASSIGN c-arq-xlsx = REPLACE(SEARCH(p-arq),".csv",".xlsx").

    CREATE "Excel.Application" chExcelApp.
    ASSIGN chExcelApp:APPLICATION:displayalerts = FALSE
           chworkbook  = chExcelApp:workbooks:ADD()
           chWorkSheet = chExcelApp:Sheets:Item(1).

    ASSIGN
        chWorkSheet:NAME                       = "Gastos"
        chWorkSheet:PageSetup:ORIENTATION      = 2
        chWorkSheet:PageSetup:PrintTitleRows   = "$1:$1" /* SELECINA LINHA INTEIRA */
        chWorkSheet:PageSetup:zoom             = 70
        chWorkSheet:PageSetup:LeftMargin       = 25
        chWorkSheet:PageSetup:RightMargin      = 25
        chWorkSheet:PageSetup:TopMargin        = 25
        chWorkSheet:PageSetup:BottomMargin     = 25
        chWorkSheet:PageSetup:HeaderMargin     = 25
        chWorkSheet:PageSetup:FooterMargin     = 25 NO-ERROR.

    /* Altera a largura da coluna de acordo com o tamanho do seu conteudo */
    chworksheet:Cells:Select.
    chworksheet:Cells:EntireColumn:AutoFit.

    chworksheet:Cells:FONT:SIZE = 8.
    chworksheet:Cells:FONT:NAME = "Microsoft Sans Serif".

    chworksheet:range("A1:AC1"):FONT:Bold = TRUE.
    chworksheet:range("A" + STRING(i-linha)):VALUE = "Supplier Number"                                             .                                     
    chworksheet:range("B" + STRING(i-linha)):VALUE = "Document Type"                                               .
    chworksheet:range("C" + STRING(i-linha)):VALUE = "Document (Voucher, Invoice, etc.)"                           .
    chworksheet:range("D" + STRING(i-linha)):VALUE = "Supplier Invoice Number"                                     .
    chworksheet:range("E" + STRING(i-linha)):VALUE = "Invoice Date"                                                .
    chworksheet:range("F" + STRING(i-linha)):VALUE = "Document Company" /*Isso ‚ o c¢digo da empresa no Datasul*/  .
    chworksheet:range("G" + STRING(i-linha)):VALUE = "Pay Item"                                                    .
    chworksheet:range("H" + STRING(i-linha)):VALUE = "GL Date"                                                     .
    chworksheet:range("I" + STRING(i-linha)):VALUE = "Document Type - Matching"                                    .
    chworksheet:range("J" + STRING(i-linha)):VALUE = "Payment Number"                                              .
    chworksheet:range("K" + STRING(i-linha)):VALUE = "Payment Date"                                                .
    chworksheet:range("L" + STRING(i-linha)):VALUE = "Vendor Category Code"                                        .
    chworksheet:range("M" + STRING(i-linha)):VALUE = "Remark"                                                      .
    chworksheet:range("N" + STRING(i-linha)):VALUE = "Reais"                                                       .
    chworksheet:range("O" + STRING(i-linha)):VALUE = "Dollares"                                                    .
    chworksheet:range("P" + STRING(i-linha)):VALUE = "Payment Amount (AA)"                                         .
    chworksheet:range("Q" + STRING(i-linha)):VALUE = "Discount Taken"                                              .
    chworksheet:range("R" + STRING(i-linha)):VALUE = "Currency Code - From"                                        .
    chworksheet:range("S" + STRING(i-linha)):VALUE = "Account ID"                                                  .
    chworksheet:range("T" + STRING(i-linha)):VALUE = "Cost Centre from Datasul"                                    .
    chworksheet:range("U" + STRING(i-linha)):VALUE = "Account + Cost Center"                                       .
    chworksheet:range("V" + STRING(i-linha)):VALUE = "Account Description"                                         .
    chworksheet:range("W" + STRING(i-linha)):VALUE = "Business Unit"                                               .
    chworksheet:range("X" + STRING(i-linha)):VALUE = "Object Account"                                              .
    chworksheet:range("Y" + STRING(i-linha)):VALUE = "Legacy Entity Number"                                        .
    chworksheet:range("Z" + STRING(i-linha)):VALUE = "Entity Name"                                                 .
    chworksheet:range("AA" + STRING(i-linha)):VALUE = "Payment to Government (Object Account Cat code)". 


    FOR EACH tt-titulos
        BREAK BY tt-titulos.cod-fornecedor:

        FIND estabelecimento WHERE
             estabelecimento.cod_estab    =   tt-titulos.cod_estab NO-LOCK NO-ERROR.
    
        ASSIGN de-tot-origin-tit-ap = de-tot-origin-tit-ap  + tt-titulos.val_origin_tit_ap
               de-tot-dollar        = de-tot-dollar         + tt-titulos.val_dollar       
               de-tot-pagto-tit-ap  = de-tot-pagto-tit-ap   + tt-titulos.val_pagto_tit_ap 
               de-tot-desc-tit-ap   = de-tot-desc-tit-ap    + tt-titulos.val_desc_tit_ap  .

        RUN pi-acompanhar h-acomp (INPUT "Gerando Excel, Linha: " + STRING(i-linha) ).
    
        ASSIGN i-linha = i-linha + 1.

    
/*         /* L¢gico para Negritar as linhas de total */                                                                                        */
/*         FOR EACH tt-linhas:                                                                                                                  */
/*             chworksheet:range("A"+ string(tt-linhas.linha-negrito) + ":AA" + string(tt-linhas.linha-negrito)):FONT:Bold = TRUE.              */
/*             chworksheet:range("A"+ string(tt-linhas.linha-negrito) + ":AA" + string(tt-linhas.linha-negrito)):borders:item(3):linestyle = 1. */
/*                                                                                                                                              */
/*         END.                                                                                                                                 */

        chworksheet:range("A" + STRING(i-linha)):VALUE = tt-titulos.cod-fornecedor         .    
        chworksheet:range("B" + STRING(i-linha)):VALUE = tt-titulos.cod_espec_docto        . 
        chworksheet:range("C" + STRING(i-linha)):VALUE = tt-titulos.cod_tit_ap             . 
        chworksheet:range("D" + STRING(i-linha)):VALUE = tt-titulos.cod_tit_ap             . 
        chworksheet:range("E" + STRING(i-linha)):VALUE = tt-titulos.dat_emis_docto         . 
        chworksheet:range("F" + STRING(i-linha)):VALUE = v_cdn_empres_usuar                . 
        chworksheet:range("G" + STRING(i-linha)):VALUE = tt-titulos.cod_parcela            . 
        chworksheet:range("H" + STRING(i-linha)):VALUE = tt-titulos.dat_transacao          . 
        chworksheet:range("I" + STRING(i-linha)):VALUE = ""                                . 
        chworksheet:range("J" + STRING(i-linha)):VALUE = tt-titulos.num-pagemtento         . 
        chworksheet:range("K" + STRING(i-linha)):VALUE = tt-titulos.dat_liquidac_tit_ap    . 
        chworksheet:range("L" + STRING(i-linha)):VALUE = tt-titulos.categoria-cat-code     . 
        chworksheet:range("M" + STRING(i-linha)):VALUE = tt-titulos.histor-tit             . 
        chworksheet:range("N" + STRING(i-linha)):VALUE = tt-titulos.val_origin_tit_ap      . 
        chworksheet:range("O" + STRING(i-linha)):VALUE = tt-titulos.val_dollar             . 
        chworksheet:range("P" + STRING(i-linha)):VALUE = tt-titulos.val_pagto_tit_ap       . 
        chworksheet:range("Q" + STRING(i-linha)):VALUE = tt-titulos.val_desc_tit_ap        . 
        chworksheet:range("R" + STRING(i-linha)):VALUE = tt-titulos.cod_indic_econ         . 
        chworksheet:range("S" + STRING(i-linha)):VALUE = tt-titulos.COD_CTA_CTBL           . 
        chworksheet:range("T" + STRING(i-linha)):VALUE = tt-titulos.cod_ccusto             . 
        chworksheet:range("U" + STRING(i-linha)):VALUE = tt-titulos.conta_ccusto           .
        chworksheet:range("V" + STRING(i-linha)):VALUE = tt-titulos.DESC_conta             . 
        chworksheet:range("W" + STRING(i-linha)):VALUE = tt-titulos.cod_unid_negoc         . 
        chworksheet:range("X" + STRING(i-linha)):VALUE = ""                                . 
        chworksheet:range("Y" + STRING(i-linha)):VALUE = tt-titulos.cod_Estab              . 
        chworksheet:range("Z" + STRING(i-linha)):VALUE = estabelecimento.nom_pessoa        . 
        chworksheet:range("AA" + STRING(i-linha)):VALUE = tt-titulos.cat-code              .


        IF LAST-OF(tt-titulos.cod-fornecedor) THEN DO:

            ASSIGN i-linha = i-linha + 1.

            chworksheet:range("A" + STRING(i-linha)):VALUE = "TOTAL " + tt-titulos.cod-fornecedor.
            chworksheet:range("N" + STRING(i-linha)):VALUE = de-tot-origin-tit-ap                . 
            chworksheet:range("O" + STRING(i-linha)):VALUE = de-tot-dollar                       . 
            chworksheet:range("P" + STRING(i-linha)):VALUE = de-tot-pagto-tit-ap                 . 
            chworksheet:range("Q" + STRING(i-linha)):VALUE = de-tot-desc-tit-ap                  .

            chworksheet:range("A"+ string(i-linha) + ":AA" + string(i-linha)):FONT:Bold = TRUE.
            chworksheet:range("A"+ string(i-linha) + ":AA" + string(i-linha)):borders:item(3):linestyle = 1.
           
            ASSIGN de-tot-origin-tit-ap = 0
                   de-tot-dollar        = 0
                   de-tot-pagto-tit-ap  = 0
                   de-tot-desc-tit-ap   = 0.
        END.
    END.

    chworksheet:range("N:Q"):NumberFormat = "#.###.###.##0,00".
    chworksheet:range("F:F"):NumberFormat = "@".
    chworksheet:range("G:G"):NumberFormat = "@".
    chworksheet:range("I:I"):NumberFormat = "@".
    chworksheet:range("L:M"):NumberFormat = "@".
    chworksheet:range("X:X"):NumberFormat = "@".
    chworksheet:range("U:U"):NumberFormat = "#".


    chworksheet:range("A1"):SELECT.

    chworkbook:SaveAs(c-arq-xlsx , 51 , "", "", false, false, false) /* NO-ERROR */ .
    
    chExcelApp:Workbooks:Close().
    chExcelApp:QUIT().
    RELEASE OBJECT chworksheet        NO-ERROR.
    RELEASE OBJECT chworkbook         NO-ERROR.
    RELEASE OBJECT chExcelApp NO-ERROR.

    PUT UNFORMAT
        "Arquivo gerado: " AT 1 c-arq-xlsx
        SKIP.

    os-command no-wait value(c-arq-xlsx) NO-ERROR.



END PROCEDURE.


PROCEDURE pi-conecta-banco :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p_cod_empresa    LIKE ems5.empresa.cod_empresa.

    DEFINE VARIABLE c-nome-fisico AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-nome-logico AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-portas AS CHARACTER   NO-UNDO.

    FIND bco_empres WHERE
         bco_empres.cod_empresa    =   p_cod_empresa AND
         bco_empres.cod_bco_dados  =   "ems2movemp" NO-LOCK NO-ERROR.
    IF AVAIL bco_empres THEN
        ASSIGN c-nome-fisico    =   bco_empres.cod_bco_fisic
               c-nome-logico    =   "ems2_" + p_cod_empresa
               c-portas         =   bco_empres.cod_param_conex.
    ELSE
        ASSIGN c-nome-fisico    =   ""
               c-nome-logico    =   ""
               c-portas         =   "".

/* Verifica se o banco est  conectado */
    IF NOT CONNECTED('ems2_' + STRING(p_cod_empresa)) THEN DO:

        CONNECT "-db " + c-nome-fisico  +  " -ld " + c-nome-logico + " " +   c-portas NO-ERROR.

        /* Retorna Erro Caso nÊo tenha Sucesso ao conectar o Banco */
        IF NOT CONNECTED('hcm') THEN DO:
            run utp/ut-msgs.p (input "show",
                               input 17006, 
                               input "Banco Movimento nÆo Conectado~~NÆo ser  poss¡vel mostrar as informa‡äes relativas a conta e centro de custo ").
            RETURN "NOK":U.
        END.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-importa-de-para:

    /*-------------- Variaveis Excel ----------------*/
    DEFINE VARIABLE chExcelApplication AS COM-HANDLE          NO-UNDO. 
/*     DEFINE VARIABLE chWorkBook2         AS COM-HANDLE          NO-UNDO. */
/*     DEFINE VARIABLE chWorkSheet2        AS COM-HANDLE          NO-UNDO. */

    CREATE "Excel.Application" chExcelApplication NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
       run utp/ut-msgs.p (input "show":U,
                          input 17006,
                          input "O Microsoft Excel n’o estÿ instalado.").
       RETURN "ADM-ERROR":U.
    END. 
    
    FILE-INFO:FILE-NAME = tt-param.arq_de_para.
    chExcelApplication:workbooks:OPEN(FILE-INFO:FULL-PATHNAME,TRUE).
    chExcelApplication:sheets:ITEM(1).
    chExcelApplication:sheets:ITEM(1):activate.
    chExcelApplication:VISIBLE                   = FALSE.
    chExcelApplication:APPLICATION:DisplayAlerts = FALSE.

    DO i-linha-de-para = 1 TO 9999:

        IF chExcelApplication:range("C" + STRING(i-linha-de-para)):VALUE = ? THEN
            LEAVE.

        RUN pi-acompanhar in h-acomp (input "Importando Arquivo DexPara. Linha: " + STRING(i-linha-de-para)).

        CREATE tt-de-para.
        ASSIGN tt-de-para.conta_ccusto          = trim(chExcelApplication:range("C" + STRING(i-linha-de-para)):VALUE) 
               tt-de-para.estma_gove            = trim(chExcelApplication:range("L" + STRING(i-linha-de-para)):VALUE) 
               tt-de-para.cod_empresa           = trim(chExcelApplication:range("D" + STRING(i-linha-de-para)):VALUE)
               tt-de-para.Account_Description   = trim(chExcelApplication:range("J" + STRING(i-linha-de-para)):VALUE).

    END.

    chExcelApplication:Workbooks:Close().
    chExcelApplication:QUIT().
/*     RELEASE OBJECT chworksheet        NO-ERROR. */
/*     RELEASE OBJECT chworkbook         NO-ERROR. */
    RELEASE OBJECT chExcelApplication NO-ERROR.


END PROCEDURE.
