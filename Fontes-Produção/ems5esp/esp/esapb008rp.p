/******************************************************************************
**  Empresa: Yamana
**  Programa: 
**  Data....: Janeiro de 2016.
**  Autor...: Emerson Galdino - DSC
**  Objetivo: Relat¢rio de Gastos
*******************************************************************************/
{esp/esapb009-bf.i}
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
    FIELD cdn_fornecedor_ini    LIKE tit_ap.cdn_fornecedor
    field cdn_fornecedor_fim    LIKE tit_ap.cdn_fornecedor
    FIELD cod_espec_docto_ini   AS CHAR FORMAT "X(200)"
    FIELD cod_espec_docto_fim   LIKE tit_ap.cod_espec_docto
    field dt_emiss_ini          AS DATE
    field dt_emiss_fim          AS DATE
    FIELD dt_vencimento_ini     AS DATE
    FIELD dt_vencimento_fim     AS DATE
    FIELD cod_empresa_ini       LIKE ems5.empresa.cod_empresa
    FIELD cod_empresa_fim       LIKE ems5.empresa.cod_empresa
    FIELD arq_de_para           AS CHAR FORMAT "X(2000)"
    FIELD dat_transacao_ini     AS DATE
    FIELD dat_transacao_fim     AS DATE
    FIELD tipo-clas             AS INT
    .

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def temp-table tt-raw-digita
   field raw-digita      as raw.

DEFINE TEMP-TABLE tt-titulos
    field cod-fornecedor        LIKE fornecedor.nom_pessoa
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
    FIELD DESC_conta_port       AS CHAR
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

DEFINE BUFFER bf-es-cat-code FOR es-cat-code.

/* DEFINE TEMP-TABLE tt-es-relat-cat LIKE es-relat-cat */
/*     INDEX idx01                                     */
/*     cod_empresa                                     */
/*     cdn_fornecedor                                  */
/*     cod_cta_ctbl                                    */
/*     cod_espec_docto                                 */
/*     cod_plano_ccusto                                */
/*     cod_ccusto                                      */
/*     cod_plano_cta_ctbl.                             */


/* Vari†veis do Relat¢rio */
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

/* Vari†veis Excel */
DEFINE VARIABLE c-arq-csv               AS CHARACTER                NO-UNDO.
DEFINE VARIABLE c-arq-xlsx              AS CHARACTER                    NO-UNDO.
DEFINE VARIABLE cSaidaCodBarra          AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cNomeArquivo            AS CHARACTER                NO-UNDO.
DEFINE VARIABLE i-linha                 AS INTEGER      INITIAL 1   NO-UNDO.
DEFINE VARIABLE i-linha-de-para         AS INTEGER      INITIAL 1   NO-UNDO.
DEFINE VARIABLE chExcelApp              AS COM-HANDLE                   NO-UNDO.
DEFINE VARIABLE chworkbook              AS COM-HANDLE                   NO-UNDO.
DEFINE VARIABLE chworksheet             AS COM-HANDLE                   NO-UNDO.

/* Vari†veis de totalizaá∆o */
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
DEF BUFFER bf-movto_tit_ap FOR movto_tit_ap.

/* Stream */
DEFINE STREAM s-csv.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* Include Padr o para Variòveis de Relat´rio  */
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

/* FOR EACH es-relat-cat:                           */
/*     CREATE tt-es-relat-cat.                      */
/*     BUFFER-COPY es-relat-cat TO tt-es-relat-cat. */
/* END.                                             */

RUN pi-cria-tt.

RUN pi-gera-csv.

{include/i-rpclo.i &STREAM="stream s-csv"}

RUN pi-excel(INPUT c-arq-csv).

run pi-finalizar in h-acomp.

{include/i-rpclo.i}

PROCEDURE pi-cria-tt:

    ASSIGN v_des_estab_select   =   tt-param.estab_selc.

    FIND FIRST trad_org_ext WHERE
           trad_org_ext.cod_matriz_trad_org_ext    =   "ems2"  AND
           trad_org_ext.cod_tip_unid_organ         =   "998"     AND
           trad_org_ext.cod_unid_organ_ext         =   string(v_cdn_empres_usuar).
    IF AVAIL trad_org_ext THEN
        ASSIGN c-matriz-org =   trad_org_ext.cod_unid_organ.
    ELSE
        ASSIGN c-matriz-org =   "".

    block_estab:
    do v_num_contador = 1 to num-entries(v_des_estab_select):

        FOR EACH estabelecimento NO-LOCK WHERE
                 estabelecimento.cod_estab = entry(v_num_contador, v_des_estab_select),
            EACH bf-movto_tit_ap NO-LOCK
            WHERE bf-movto_tit_ap.cod_estab = estabelecimento.cod_estab
              AND bf-movto_tit_ap.dat_transacao >= tt-param.dat_transacao_ini
              AND bf-movto_tit_ap.dat_transacao <= tt-param.dat_transacao_fim 
            BREAK BY bf-movto_tit_ap.num_id_tit_ap:

            RUN pi-acompanhar IN h-acomp ('T°tulo Verif: ' + STRING(bf-movto_tit_ap.num_id_tit_ap)).

/*             IF tt-param.tipo-clas = 1 AND                                                          */
/*                 NOT can-FIND(FIRST tt-es-relat-cat WHERE                                           */
/*                            tt-es-relat-cat.cod_empresa        =   bf-movto_tit_ap.cod_empresa  AND */
/*                            tt-es-relat-cat.cdn_fornecedor     =   bf-movto_tit_ap.cdn_fornecedor)  */
/*                 THEN NEXT.                                                                         */
/*             IF tt-param.tipo-clas = 2 AND                                                          */
/*                 can-FIND(FIRST tt-es-relat-cat WHERE                                               */
/*                        tt-es-relat-cat.cod_empresa        =   bf-movto_tit_ap.cod_empresa  AND     */
/*                        tt-es-relat-cat.cdn_fornecedor     =   bf-movto_tit_ap.cdn_fornecedor)      */
/*                 THEN NEXT.                                                                         */
/*                                                                                                    */
            IF NOT FIRST-OF(bf-movto_tit_ap.num_id_tit_ap) THEN NEXT.

            titulos:
            FOR EACH tit_ap NO-LOCK WHERE
                     tit_ap.cod_estab             = estabelecimento.cod_estab      and
                     tit_ap.num_id_tit_ap         = bf-movto_tit_ap.num_id_tit_ap:

                /*BREAK BY tit_ap.cod_estab 
                      BY tit_ap.dat_emis_docto:*/

                RUN pi-acompanhar in h-acomp (input "Processando Estab: " + tit_ap.cod_estab + " -  Data: " + STRING( tit_ap.dat_transacao )).

                IF NOT (tit_ap.cdn_fornecedor       >= tt-param.cdn_fornecedor_ini    and
                        tit_ap.cdn_fornecedor       <= tt-param.cdn_fornecedor_fim    and
                        tit_ap.cod_espec_docto      >= tt-param.cod_espec_docto_ini   and
                        tit_ap.cod_espec_docto      <= tt-param.cod_espec_docto_fim   and
                        tit_ap.dat_emis_docto       >= tt-param.dt_emiss_ini          and
                        tit_ap.dat_emis_docto       <= tt-param.dt_emiss_fim          and
                        tit_ap.dat_vencto_tit_ap    >= tt-param.dt_vencimento_ini     and
                        tit_ap.dat_vencto_tit_ap    <= tt-param.dt_vencimento_fim     AND
                        tit_ap.cod_empresa           = c-matriz-org) THEN NEXT.

                
                
                ASSIGN c-conta-contabil = ""
                       v_row_nota       = ?
                       c-ccusto         = ""
                       C-plano-ccusto   = "".
                

                FIND FIRST movto_tit_ap OF tit_ap where 
                           movto_tit_ap.ind_trans_ap = "Implantaá∆o" NO-LOCK NO-ERROR. 
                IF NOT AVAIL movto_tit_ap THEN NEXT.

                FIND FIRST aprop_ctbl_ap OF movto_tit_ap where
                           aprop_ctbl_ap.ind_natur_lancto_ctbl = "DB" NO-LOCK NO-ERROR.
                IF NOT AVAIL aprop_ctbl_ap THEN NEXT.

                /*Novos Cadastros de CAt-COde*/

/*                 DISP tit_ap.cdn_fornecedor               */
/*                      tit_ap.cod_espec_docto              */
/*                      tit_ap.dat_emis_docto               */
/*                      tit_ap.dat_vencto_tit_ap            */
/*                      tit_ap.cod_empresa       . PAUSE 0. */

/*                      MESSAGE "Passou !!!" SKIP         */
/*                           tit_ap.cod_empresa    SKIP   */
/*                           tit_ap.cdn_fornecedor        */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK. */


                IF tt-param.tipo-clas = 1 THEN DO:
                        FIND FIRST es-cat-code-fornec NO-LOCK
                            WHERE /*es-cat-code-fornec.cod_empresa    = tit_ap.cod_empresa
                            AND  */ es-cat-code-fornec.cdn_fornecedor = tit_ap.cdn_fornecedor 
                            AND   es-cat-code-fornec.cons-relatorio = YES NO-ERROR.
                        IF NOT AVAIL es-cat-code-fornec THEN NEXT.

                         FIND FIRST es-cat-code
                             WHERE es-cat-code.cat-code = es-cat-code-fornec.cat-code NO-LOCK NO-ERROR. 


                         

                         ASSIGN c-categoria-cat-code = "".
/*                         FIND FIRST es-cat-code-categoria NO-LOCK                                                    */
/*                                  WHERE es-cat-code-categoria.categoria-code = es-cat-code-fornec.cat-code NO-ERROR. */

                            IF AVAIL es-cat-code THEN DO:
                         

                                ASSIGN c-categoria-cat-code = string(es-cat-code.cat-code) + " - " +
                                                                     es-cat-code.desc-cat-code.

                            END.


                END.
                IF tt-param.tipo-clas = 2 THEN DO:
                    FIND FIRST es-cat-code-conta NO-LOCK
                            WHERE es-cat-code-conta.cod_cta_ctbl       = aprop_ctbl_ap.cod_cta_ctbl 
                            AND   es-cat-code-conta.cod_plano_cta_ctbl = aprop_ctbl_ap.cod_plano_cta_ctbl   
                            AND   es-cat-code-conta.cons-relatorio = YES NO-ERROR.
                        IF NOT AVAIL es-cat-code-conta THEN NEXT.


                        FIND FIRST es-cat-code-fornec NO-LOCK
                            WHERE es-cat-code-fornec.cdn_fornecedor = tit_ap.cdn_fornecedor 
                            AND   es-cat-code-fornec.cons-relatorio = YES NO-ERROR.
                        IF AVAIL es-cat-code-fornec THEN NEXT.

                        FIND FIRST es-cat-code
                             WHERE es-cat-code.cat-code = es-cat-code-conta.cat-code NO-LOCK NO-ERROR. 

                END.





                

/*                 FIND FIRST es-cat-code-categoria NO-LOCK                                                                 */
/*                                  WHERE es-cat-code-categoria.categoria-code = tt-es-relat-cat.categoria-code             */
/*                                 NO-ERROR.                                                                                */
/*                             IF AVAIL es-cat-code-categoria THEN                                                          */
/*                                 ASSIGN c-categoria-cat-code = string(es-cat-code-categoria.categoria-code) + " - " +     */
/*                                                               es-cat-code-categoria.des-categoria.                       */
/*                         END.                                                                                             */
/*                         ELSE DO:                                                                                         */
/*                             FIND FIRST es-relat-cat NO-LOCK                                                              */
/*                                 WHERE es-relat-cat.cod_empresa    = tit_ap.cod_empresa                                   */
/*                                   AND es-relat-cat.cdn_fornecedor = tit_ap.cdn_fornecedor                                */
/*                                 NO-ERROR.                                                                                */
/*                             IF AVAIL es-relat-cat THEN DO:                                                               */
/*                                 FIND FIRST es-cat-code-categoria NO-LOCK                                                 */
/*                                      WHERE es-cat-code-categoria.categoria-code = es-relat-cat.categoria-code            */
/*                                     NO-ERROR.                                                                            */
/*                                 IF AVAIL es-cat-code-categoria THEN                                                      */
/*                                     ASSIGN c-categoria-cat-code = string(es-cat-code-categoria.categoria-code) + " - " + */
/*                                                                   es-cat-code-categoria.des-categoria.                   */
/*                             END.                                                                                         */





                /* busca conta do recebimento para transit¢ria */
                RUN pi-verifica-conta-trans.
        
                IF c-conta-contabil = "" THEN
                    ASSIGN c-conta-contabil = aprop_ctbl_ap.cod_cta_ctbl.

                ASSIGN c-desc-conta = "".                                       
                FIND cta_ctbl WHERE
                     cta_ctbl.cod_plano_cta_ctbl = aprop_ctbl_ap.cod_plano_cta_ctbl AND
                     cta_ctbl.cod_cta_ctbl = c-conta-contabil NO-LOCK NO-ERROR.
                IF AVAIL cta_ctbl THEN
                    ASSIGN c-desc-conta =   cta_ctbl.des_tit_ctbl.
        
                IF c-ccusto = "" THEN
                    ASSIGN c-ccusto         = aprop_ctbl_ap.cod_ccusto
                           C-plano-ccusto   = "".

                /* fornecedor */
/*                 IF tt-param.tipo-clas = 1 THEN DO: /*Nova regra Cadastros novos Cat Code*/ */
/*                     FIND FIRST tt-es-relat-cat WHERE                                                                      */
/*                                tt-es-relat-cat.cod_empresa        =   tit_ap.cod_empresa    AND                           */
/*                                tt-es-relat-cat.cdn_fornecedor     =   tit_ap.cdn_fornecedor AND                           */
/*                                                                                                                           */
/*                               ((tt-es-relat-cat.cod_cta_ctbl       =   c-conta-contabil /*aprop_ctbl_ap.cod_cta_ctbl*/ OR */
/*                                 tt-es-relat-cat.cod_cta_ctbl       =   ?)  AND                                            */
/*                                                                                                                           */
/*                               (tt-es-relat-cat.cod_espec_docto    =   tit_ap.cod_espec_docto OR                           */
/*                                tt-es-relat-cat.cod_espec_docto    =   ?)  AND                                             */
/*                                                                                                                           */
/*                               (tt-es-relat-cat.cod_plano_ccusto   =   aprop_ctbl_ap.cod_plano_ccusto OR                   */
/*                                tt-es-relat-cat.cod_plano_ccusto   =   ?)  AND                                             */
/*                                                                                                                           */
/*                               (tt-es-relat-cat.cod_ccusto         =   aprop_ctbl_ap.cod_ccusto OR                         */
/*                                tt-es-relat-cat.cod_ccusto         =   ?)  AND                                             */
/*                                                                                                                           */
/*                               (tt-es-relat-cat.cod_plano_cta_ctbl =   aprop_ctbl_ap.cod_plano_cta_ctbl OR                 */
/*                                tt-es-relat-cat.cod_plano_cta_ctbl =   ?)) NO-LOCK NO-ERROR.                               */
/*                                                                                                                           */
/*                     IF NOT AVAIL tt-es-relat-cat THEN DO:                                                                 */
/*                         IF NOT CAN-FIND(FIRST tt-es-relat-cat WHERE                                                       */
/*                                    tt-es-relat-cat.cod_empresa        =   tit_ap.cod_empresa    AND                       */
/*                                    tt-es-relat-cat.cdn_fornecedor     =   tit_ap.cdn_fornecedor) THEN                     */
/*                         NEXT titulos.                                                                                     */
/*                     END.                                                                                                  */
/*                 END.                                                                                                      */
/*                                                                                                                           */
/*                 /* por conta */                                                                                           */
/*                 ELSE DO:                                                                                                  */
/*                                                                                                                           */
/*                     FIND FIRST tt-es-relat-cat WHERE                                                                      */
/*                                tt-es-relat-cat.cod_empresa        =   tit_ap.cod_empresa    AND                           */
/*                                tt-es-relat-cat.cdn_fornecedor     =   tit_ap.cdn_fornecedor                               */
/*                         NO-ERROR.                                                                                         */
/*                     IF AVAIL tt-es-relat-cat THEN NEXT titulos.                                                           */
/*                                                                                                                           */
/*                     FIND FIRST tt-de-para WHERE                                                                           */
/*                         tt-de-para.cod_emp      =   v_cdn_empres_usuar                 AND                                */
/*                         tt-de-para.conta_ccusto =   (c-conta-contabil + c-ccusto) NO-LOCK NO-ERROR.                       */
/*                     IF NOT AVAIL tt-de-para THEN NEXT titulos.                                                            */
/*                                                                                                                           */
/*                     FIND FIRST tt-es-relat-cat WHERE                                                                      */
/*                                tt-es-relat-cat.cod_empresa        =   ? AND                                               */
/*                                tt-es-relat-cat.cdn_fornecedor     =   ? AND                                               */
/*                                                                                                                           */
/*                               ((tt-es-relat-cat.cod_cta_ctbl       =   c-conta-contabil /*aprop_ctbl_ap.cod_cta_ctbl*/ OR */
/*                                 tt-es-relat-cat.cod_cta_ctbl       =   ?)  AND                                            */
/*                                                                                                                           */
/*                               (tt-es-relat-cat.cod_espec_docto    =   tit_ap.cod_espec_docto OR                           */
/*                                tt-es-relat-cat.cod_espec_docto    =   ?)  AND                                             */
/*                                                                                                                           */
/*                               (tt-es-relat-cat.cod_plano_ccusto   =   aprop_ctbl_ap.cod_plano_ccusto OR                   */
/*                                tt-es-relat-cat.cod_plano_ccusto   =   ?)  AND                                             */
/*                                                                                                                           */
/*                               (tt-es-relat-cat.cod_ccusto         =   aprop_ctbl_ap.cod_ccusto OR                         */
/*                                tt-es-relat-cat.cod_ccusto         =   ?)  AND                                             */
/*                                                                                                                           */
/*                               (tt-es-relat-cat.cod_plano_cta_ctbl =   aprop_ctbl_ap.cod_plano_cta_ctbl OR                 */
/*                                tt-es-relat-cat.cod_plano_cta_ctbl =   ?)) NO-LOCK NO-ERROR.                               */
/*                     IF NOT AVAIL tt-es-relat-cat THEN NEXT.                                                               */
/*                 END.                                                                                                      */
        
                FIND FIRST fornecedor NO-LOCK OF tit_ap.
        
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

                    /* periodo de data de paga,ento/transacao */
                    IF b-movto_tit_ap.dat_transacao < tt-param.dat_transacao_ini OR
                       b-movto_tit_ap.dat_transacao > tt-param.dat_transacao_fim THEN NEXT.
                    
                    FIND FIRST compl_movto_pagto OF b-movto_tit_ap WHERE
                              NO-LOCK NO-ERROR.    
                    IF AVAIL compl_movto_pagto THEN DO:
        
                        IF compl_movto_pagto.ind_modo_pagto = "Cheque"  THEN
                            ASSIGN i-num-pagamento  =   compl_movto_pagto.num_cheque.
                        ELSE IF compl_movto_pagto.ind_modo_pagto = "Borderì" THEN
                            ASSIGN i-num-pagamento  =   compl_movto_pagto.num_bord_ap.
                        ELSE
                            ASSIGN i-num-pagamento  =   0.

/*                         IF AVAIL tt-es-relat-cat THEN */
/*                          FIND FIRST es-cat-code WHERE                                                    */
/*                                        es-cat-code.cat-code = tt-es-relat-cat.cat-code NO-LOCK NO-ERROR. */
                        /*
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
                        */
                        /*Nova Regra Verificar Logica abaixo*/

/*                         c-categoria-cat-code = "". */
/*                         IF AVAIL tt-es-relat-cat THEN DO: */
/*                             FIND FIRST es-cat-code-categoria NO-LOCK                                                     */
/*                                  WHERE es-cat-code-categoria.categoria-code = tt-es-relat-cat.categoria-code             */
/*                                 NO-ERROR.                                                                                */
/*                             IF AVAIL es-cat-code-categoria THEN                                                          */
/*                                 ASSIGN c-categoria-cat-code = string(es-cat-code-categoria.categoria-code) + " - " +     */
/*                                                               es-cat-code-categoria.des-categoria.                       */
/*                         END.                                                                                             */
/*                         ELSE DO:                                                                                         */
/*                             FIND FIRST es-relat-cat NO-LOCK                                                              */
/*                                 WHERE es-relat-cat.cod_empresa    = tit_ap.cod_empresa                                   */
/*                                   AND es-relat-cat.cdn_fornecedor = tit_ap.cdn_fornecedor                                */
/*                                 NO-ERROR.                                                                                */
/*                             IF AVAIL es-relat-cat THEN DO:                                                               */
/*                                 FIND FIRST es-cat-code-categoria NO-LOCK                                                 */
/*                                      WHERE es-cat-code-categoria.categoria-code = es-relat-cat.categoria-code            */
/*                                     NO-ERROR.                                                                            */
/*                                 IF AVAIL es-cat-code-categoria THEN                                                      */
/*                                     ASSIGN c-categoria-cat-code = string(es-cat-code-categoria.categoria-code) + " - " + */
/*                                                                   es-cat-code-categoria.des-categoria.                   */
/*                             END.                                                                                         */
/*                         END. */

                            FIND FIRST es-cat-code-conta NO-LOCK
                                WHERE es-cat-code-conta.cod_cta_ctbl       = aprop_ctbl_ap.cod_cta_ctbl 
                                AND   es-cat-code-conta.cod_plano_cta_ctbl = aprop_ctbl_ap.cod_plano_cta_ctbl   NO-ERROR.
                           

/*                             MESSAGE 'aprop_ctbl_ap.cod_cta_ctbl       '  aprop_ctbl_ap.cod_cta_ctbl       SKIP   */
/*                                     'aprop_ctbl_ap.cod_plano_cta_ctbl '  aprop_ctbl_ap.cod_plano_cta_ctbl   SKIP */
/*                                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                                               */

/*                             IF  AVAIL es-cat-code-conta THEN                    */
/*                                 MESSAGE es-cat-code-conta.desc-conta-estrageiro */
/*                                     VIEW-AS ALERT-BOX INFO BUTTONS OK.          */

                        CREATE tt-titulos.
                        ASSIGN tt-titulos.cod-fornecedor        =   string(fornecedor.cdn_fornecedor) + "-" + fornecedor.nom_pessoa                         
                               tt-titulos.cod_espec_docto       =   CAPS(tit_ap.cod_espec_docto) + " - " + espec_docto.des_espec_docto                                                           
                               tt-titulos.cod_tit_ap            =   tit_ap.cod_tit_ap                                                               
                               tt-titulos.dat_emis_docto        =   tit_ap.dat_emis_docto
                               tt-titulos.cod_parcela           =   tit_ap.cod_parcela                                                        
                               tt-titulos.dat_transacao         =   tit_ap.dat_transacao 
                               tt-titulos.num-pagemtento        =   i-num-pagamento                                               
                               tt-titulos.dat_liquidac_tit_ap   =   b-movto_tit_ap.dat_transacao /*tit_ap.dat_liquidac_tit_ap */
                               tt-titulos.categoria-cat-code    =   c-categoria-cat-code
                               tt-titulos.cat-code              =   IF AVAIL es-cat-code THEN es-cat-code.cat-code + " - " + es-cat-code.desc-cat-code ELSE "N/A"
                               tt-titulos.histor-tit            =   REPLACE(c-histor-tit,CHR(10),"")                              
                               tt-titulos.val_origin_tit_ap     =   tit_ap.val_origin_tit_ap                                      
                               tt-titulos.val_dollar            =   de-valor-dolar                                             
                               tt-titulos.val_pagto_tit_ap      =   tit_ap.val_pagto_tit_ap                                       
                               tt-titulos.val_desc_tit_ap       =   tit_ap.val_desc_tit_ap                                        
                               tt-titulos.cod_indic_econ        =   aprop_ctbl_ap.cod_indic_econ                                  
                               tt-titulos.COD_CTA_CTBL          =   c-conta-contabil       
                               tt-titulos.DESC_conta            =  IF AVAIL es-cat-code-conta THEN es-cat-code-conta.desc-conta-estrageiro ELSE ""
                               tt-titulos.DESC_conta_port       =   c-desc-conta
                               tt-titulos.conta_ccusto          =   c-conta-contabil + c-ccusto       
                               tt-titulos.cod_unid_negoc        =   aprop_ctbl_ap.cod_unid_negoc                               
                               tt-titulos.cod_Estab             =   tit_ap.cod_estab                                          
                               tt-titulos.cod_ccusto            =   c-ccusto /* aprop_ctbl_ap.cod_ccusto                                      */ .

                       

                       FIND FIRST bf-es-cat-code NO-LOCK
                                   WHERE bf-es-cat-code.cat-code = es-cat-code-conta.cat-code NO-ERROR.


                       ASSIGN tt-titulos.Object_Account        = IF AVAIL bf-es-cat-code THEN bf-es-cat-code.cat-code + " - " + bf-es-cat-code.desc-cat-code ELSE "N/A"


                               
                                   .
/*                                tt-titulos.cod_plano_ccusto      =   C-plano-ccusto /*aprop_ctbl_ap.cod_plano_ccusto*/ . */

                        FIND FIRST tt-de-para WHERE
                                   tt-de-para.cod_emp      =   string(v_cdn_empres_usuar)                 AND
                                   tt-de-para.conta_ccusto =   tt-titulos.conta_ccusto NO-LOCK NO-ERROR.
/*                         IF AVAIL tt-de-para THEN                                                    */
/*                             ASSIGN tt-titulos.Object_Account    =   tt-de-para.estma_gove           */
/*                                    tt-titulos.DESC_conta        =   tt-de-para.Account_Description. */
/*                         ELSE                                                                        */
/*                             ASSIGN tt-titulos.Object_Account    =   "N/A"                           */
/*                                    tt-titulos.DESC_conta        =   "N/A".                          */
        
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pi-gera-csv:

    IF tt-param.tipo-clas = 1 THEN DO:
        EXPORT STREAM s-csv DELIMITER ";"
            "Supplier Number"
            "Document Type"
            "Document (Voucher, Invoice, etc.)"
            "Supplier Invoice Number"
            "Invoice Date"
            "Document Company" /*Isso Ç o c¢digo da empresa no Datasul*/
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
            "Account Description (Datasul)"
            "Business Unit"
            "Object Account"
            "Legacy Entity Number"
            "Entity Name"
            "Payment to Government (Object Account Cat code)".
    END.
    ELSE DO:
        EXPORT STREAM s-csv DELIMITER ";"
            "Account ID"
            "Cost Centre from Datasul"
            "Account + Cost Center"
            "Account Description"
            "Account Description (Datasul)"
            "Supplier Number"
            "Document Type"
            "Document (Voucher, Invoice, etc.)"
            "Supplier Invoice Number"
            "Invoice Date"
            "Document Company" /*Isso Ç o c¢digo da empresa no Datasul*/
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
            "Business Unit"
            "Object Account"
            "Legacy Entity Number"
            "Entity Name"
            "Payment to Government (Object Account Cat code)".



    END.
        


/*     MESSAGE "vai gerar skip"               */
/*         CAN-FIND(FIRST tt-titulos)         */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    
    FOR EACH tt-titulos
        BREAK BY tt-titulos.cod-fornecedor:


        FIND estabelecimento WHERE
             estabelecimento.cod_estab    =   tt-titulos.cod_estab NO-LOCK NO-ERROR.

        ASSIGN de-tot-origin-tit-ap = de-tot-origin-tit-ap  + tt-titulos.val_origin_tit_ap
               de-tot-dollar        = de-tot-dollar         + tt-titulos.val_dollar       
               de-tot-pagto-tit-ap  = de-tot-pagto-tit-ap   + tt-titulos.val_pagto_tit_ap 
               de-tot-desc-tit-ap   = de-tot-desc-tit-ap    + tt-titulos.val_desc_tit_ap  .

        ASSIGN i-linha = i-linha + 1.

        IF tt-param.tipo-clas = 1 THEN DO:

            EXPORT STREAM s-csv DELIMITER ";"
                tt-titulos.cod-fornecedor                     
                tt-titulos.cod_espec_docto                 
                "'" + tt-titulos.cod_tit_ap  + CHR(32)
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
                tt-titulos.DESC_conta_port
                "'" + tt-titulos.cod_unid_negoc                  
                ""                                         
                tt-titulos.cod_Estab                       
                estabelecimento.nom_pessoa
                tt-titulos.Object_Account /*tt-titulos.cat-code*/                        
                .
        END.
        ELSE DO:

            EXPORT STREAM s-csv DELIMITER ";"
                tt-titulos.COD_CTA_CTBL    
                tt-titulos.cod_ccusto                      
                tt-titulos.conta_ccusto  + CHR(32)
                tt-titulos.DESC_conta
                tt-titulos.DESC_conta_port
                tt-titulos.cod-fornecedor                     
                tt-titulos.cod_espec_docto                 
                "'" + tt-titulos.cod_tit_ap  + CHR(32)
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
                "'" + tt-titulos.cod_unid_negoc                  
                ""                                         
                tt-titulos.cod_Estab                       
                estabelecimento.nom_pessoa
                tt-titulos.Object_Account /*tt-titulos.cat-code*/                        
                .


        END.

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
    ASSIGN chExcelApp:APPLICATION:displayalerts = FALSE.

    ASSIGN chworkbook  = chExcelApp:workbooks:OPEN(p-arq)
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
        chWorkSheet:PageSetup:FooterMargin     = 25
        NO-ERROR.

    /* Altera a largura da coluna de acordo com o tamanho do seu conteudo */
    chworksheet:Cells:Select.
    chworksheet:Cells:EntireColumn:AutoFit.

    chworksheet:Cells:FONT:SIZE = 8.
    chworksheet:Cells:FONT:NAME = "Microsoft Sans Serif".
    chworksheet:range("N:Q"):NumberFormat = "#.###.###.##0,00".
    chworksheet:range("F:F"):NumberFormat = "@".
    chworksheet:range("G:G"):NumberFormat = "@".
    chworksheet:range("I:I"):NumberFormat = "@".
    chworksheet:range("L:M"):NumberFormat = "@".
    IF tt-param.tipo-clas = 1 THEN
        chworksheet:range("c:c"):NumberFormat = "@".
    ELSE 
        chworksheet:range("c:c"):NumberFormat = "#".
    chworksheet:range("b:b"):NumberFormat = "@".
    chworksheet:range("d:d"):NumberFormat = "@".
    chworksheet:range("U:U"):NumberFormat = "#".
    chworksheet:range("x:z"):NumberFormat = "@".

    chworksheet:range("A1:AC1"):FONT:Bold = TRUE.

    /* L¢gico para Negritar as linhas de total */
    FOR EACH tt-linhas:
        chworksheet:range("A"+ string(tt-linhas.linha-negrito) + ":AA" + string(tt-linhas.linha-negrito)):FONT:Bold = TRUE.
        chworksheet:range("A"+ string(tt-linhas.linha-negrito) + ":AA" + string(tt-linhas.linha-negrito)):borders:item(3):linestyle = 1.

    END.

    chworksheet:range("A1"):SELECT.

    chworkbook:SaveAs(c-arq-xlsx , 51 , "", "", false, false, false) /* NO-ERROR */ .
    
    chExcelApp:VISIBLE                   = TRUE.

    /*chExcelApp:Workbooks:Close().
    chExcelApp:QUIT().*/

    RELEASE OBJECT chworksheet        NO-ERROR.
    RELEASE OBJECT chworkbook         NO-ERROR.
    RELEASE OBJECT chExcelApp         NO-ERROR.

    

    PUT UNFORMAT
        "Arquivo gerado: " AT 1 c-arq-xlsx
        SKIP.

    /*os-command no-wait value(c-arq-xlsx) NO-ERROR.*/

END PROCEDURE.


PROCEDURE pi-conecta-banco :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p_cod_empresa    LIKE empresa.cod_empresa.

    DEFINE VARIABLE c-nome-fisico AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-nome-logico AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-portas AS CHARACTER   NO-UNDO.

/*     FIND bco_empres WHERE                                                                                                                             */
/*          bco_empres.cod_empresa    =   p_cod_empresa AND                                                                                              */
/*          bco_empres.cod_bco_dados  =   "ems2movemp" NO-LOCK NO-ERROR.                                                                                 */
/*     IF AVAIL bco_empres THEN                                                                                                                          */
/*         ASSIGN c-nome-fisico    =   bco_empres.cod_bco_fisic                                                                                          */
/*                c-nome-logico    =   "ems2_" + p_cod_empresa                                                                                           */
/*                c-portas         =   bco_empres.cod_param_conex.                                                                                       */
/*     ELSE                                                                                                                                              */
/*         ASSIGN c-nome-fisico    =   ""                                                                                                                */
/*                c-nome-logico    =   ""                                                                                                                */
/*                c-portas         =   "".                                                                                                               */
/*                                                                                                                                                       */
/* /* Verifica se o banco est† conectado */                                                                                                              */
/*     IF NOT CONNECTED('ems2_' + STRING(p_cod_empresa)) THEN DO:                                                                                        */
/*                                                                                                                                                       */
/*         CONNECT "-db " + c-nome-fisico  +  " -ld " + c-nome-logico + " " +   c-portas NO-ERROR.                                                       */
/*                                                                                                                                                       */
/*         /* Retorna Erro Caso n o tenha Sucesso ao conectar o Banco */                                                                                 */
/*         IF NOT CONNECTED('hcm') THEN DO:                                                                                                              */
/*             run utp/ut-msgs.p (input "show",                                                                                                          */
/*                                input 17006,                                                                                                           */
/*                                input "Banco Movimento n∆o Conectado~~N∆o ser† poss°vel mostrar as informaá‰es relativas a conta e centro de custo "). */
/*             RETURN "NOK":U.                                                                                                                           */
/*         END.                                                                                                                                          */
/*     END.                                                                                                                                              */
/*                                                                                                                                                       */
/*     RETURN "OK":U.                                                                                                                                    */

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
                          input "O Microsoft Excel nío estˇ instalado.").
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



PROCEDURE pi-verifica-conta-trans.
    IF aprop_ctbl_ap.cod_cta_ctbl = "17202002" THEN DO:

        find first ems5.espec_docto NO-LOCK where
                        espec_docto.cod_espec_docto = tit_ap.cod_espec_docto no-error.
        if  avail espec_docto then do:
            case espec_docto.ind_tip_espec_docto:
                when "Previs∆o" /*l_previsao*/  then
                    assign v_num_espec_ems2 = 1.
                when "Normal" /*l_normal*/  then
                    assign v_num_espec_ems2 = 2.
                when "" /*l_null*/  then
                    assign v_num_espec_ems2 = 3.
            END CASE.
        END.

        run rep/reapi011.p (input 1,  /* 1- Verifica a exist¨ncia de NF no Recebimento */
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
                         movto_tit_ap.ind_trans_ap = "Implantaá∆o" AND
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


END PROCEDURE.
