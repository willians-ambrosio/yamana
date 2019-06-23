/*###########################################################################################
**  Programa: escmg001rp.p
**  Finalidade: Extra‡Æodos dos dados do di rio auxiliar do Caixa e Bancos.
**  Autor: Leonardo J. Dalla Valle - Kraft 
**  Data: 20/01/11
**  Observa‡Æo: Foi desenvolvido com includes abertas, em virtude de EMS505 unificado em 1 banco.
#############################################################################################*/
/*###########################################################################################
**  Autera‡Æo: Thiago Coutinho - CSX
**  Data: 04/05/2012
**  Observa‡Æo: Atualizado para funcionar no EMS 5.06 - Havia sido desenvolvido para ems2.04.
#############################################################################################*/

/*---------DEFINI€ÇO DE BUFFERS----------------------------------*/
DEF BUFFER empresa                  FOR ems5.empresa.

/*-----include de controle de versÆo---- */
{include/i-prgvrs.i ESCMG001RP 5.06.00.000}

define temp-table tt-param no-undo
    field destino                       as integer
    field arquivo                       as char format "x(35)"
    field usuario                       as char format "x(12)"
    field data-exec                     as date
    field hora-exec                     as integer
    FIELD l-carga                       AS LOG
    FIELD l-contabilizado               LIKE aprop_ctbl_cmg.LOG_ctbz_movto_cta_corren
    FIELD cod_cta_corren_fim            LIKE aprop_ctbl_cmg.cod_cta_corren
    FIELD cod_cta_corren_ini            LIKE aprop_ctbl_cmg.cod_cta_corren
    FIELD cod_empresa_fim               LIKE aprop_ctbl_cmg.cod_empresa
    FIELD cod_empresa_ini               LIKE aprop_ctbl_cmg.cod_empresa
    FIELD cod_estab_fim                 LIKE aprop_ctbl_cmg.cod_estab
    FIELD cod_estab_ini                 LIKE aprop_ctbl_cmg.cod_estab
    FIELD cod_plano_ccusto_fim          LIKE aprop_ctbl_cmg.cod_plano_ccusto
    FIELD cod_plano_ccusto_ini          LIKE aprop_ctbl_cmg.cod_plano_ccusto
    FIELD dat_transacao_ini             LIKE aprop_ctbl_cmg.dat_transacao
    FIELD dat_transacao_fim             LIKE aprop_ctbl_cmg.dat_transacao
    .

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

/* recebimento de parƒmetros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

/* ---Atualiza dados na tt-param-- */
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* defini‡Æo de vari veis  */
DEFINE VARIABLE h-acomp   AS HANDLE     NO-UNDO.
DEF VAR w-id AS CHAR FORMAT "x(13)".

/* /* bloco principal do programa */                 */
/* ASSIGN c-programa       = "ESCMG001RP"            */
/*       c-versao          = "5.06"                  */
/*       c-revisao         = ".00.001"               */
/*       c-empresa         = c-empresa               */
/*       c-sistema         = "Caixa e Bancos"        */
/*       c-titulo-relat    = "Ilha- Caixa e Bancos". */

/*----Limpa tabela-----------*/
FIND FIRST es_diario5 NO-ERROR.
IF AVAIL es_diario5 THEN DO:
    FOR EACH es_diario5:
        DELETE es_diario5.
    END.
END.

/*----Inicio Acompanhamento---------------------*/
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

/*--------Encontra tt-param--------*/
FIND FIRST tt-param NO-LOCK NO-ERROR.
IF AVAIL tt-param THEN DO:
    MESSAGE
        'tt-param.usuario             : ' tt-param.usuario               skip
        'tt-param.destino             : ' tt-param.destino               skip
        'tt-param.data-exec           : ' tt-param.data-exec             skip
        'tt-param.hora-exec           : ' tt-param.hora-exec             skip
        'tt-param.l-carga             : ' tt-param.l-carga               skip
        'tt-param.l-contabilizado     : ' tt-param.l-contabilizado       skip
        'tt-param.cod_cta_corren_ini  : ' tt-param.cod_cta_corren_ini    skip
        'tt-param.cod_cta_corren_fim  : ' tt-param.cod_cta_corren_fim    skip
        'tt-param.cod_empresa_ini     : ' tt-param.cod_empresa_ini       skip
        'tt-param.cod_empresa_fim     : ' tt-param.cod_empresa_fim       skip
        'tt-param.cod_estab_ini       : ' tt-param.cod_estab_ini         skip
        'tt-param.cod_estab_fim       : ' tt-param.cod_estab_fim         skip
        'tt-param.cod_plano_ccusto_ini: ' tt-param.cod_plano_ccusto_ini  skip
        'tt-param.cod_plano_ccusto_fim: ' tt-param.cod_plano_ccusto_fim  skip
        'tt-param.dat_transacao_ini   : ' tt-param.dat_transacao_ini     skip
        'tt-param.dat_transacao_fim   : ' tt-param.dat_transacao_fim   
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    FOR EACH aprop_ctbl_cmg
       WHERE aprop_ctbl_cmg.cod_empresa      >= tt-param.cod_empresa_ini
         AND aprop_ctbl_cmg.cod_empresa      <= tt-param.cod_empresa_fim
         AND aprop_ctbl_cmg.cod_estab        >= tt-param.cod_estab_ini
         AND aprop_ctbl_cmg.cod_estab        <= tt-param.cod_estab_fim
         AND aprop_ctbl_cmg.dat_transacao    >= date(if NOT l-carga then today - 90 else tt-param.dat_transacao_ini)
         AND aprop_ctbl_cmg.dat_transacao    <= date(if NOT l-carga then today else tt-param.dat_transacao_fim)
         NO-LOCK :

        IF aprop_ctbl_cmg.LOG_ctbz_movto_cta_corren <> tt-param.l-contabilizado  THEN NEXT.

        IF aprop_ctbl_cmg.cod_plano_ccusto < tt-param.cod_plano_ccusto_ini
        OR aprop_ctbl_cmg.cod_plano_ccusto > tt-param.cod_plano_ccusto_fim THEN NEXT.

        IF aprop_ctbl_cmg.cod_cta_corren < tt-param.cod_cta_corren_ini
        OR aprop_ctbl_cmg.cod_cta_corren > tt-param.cod_cta_corren_fim THEN NEXT.

        /* leo-kraft-270111 */
        w-id = STRING(aprop_ctbl_cmg.num_id_movto_cta_corren,"9999999999") + string(num_seq_aprop_ctbl_cmg). 
               /*  + STRING(aprop_ctbl_cmg.num_seq_aprop_ctbl_cmg,"999"). */

        RUN pi-acompanhar IN h-acomp (INPUT "Conta : " + String(aprop_ctbl_cmg.cod_cta_ctbl)).
        
        /*FIND es_diario5 NO-LOCK WHERE es_diario5.id = w-id NO-ERROR.*/

        FIND FIRST movto_cta_corren
             where movto_cta_corren.num_id_movto_cta_corren = aprop_ctbl_cmg.num_id_movto_cta_corren
             no-lock no-error.
        IF AVAIL movto_cta_corren THEN DO:
                
            FIND FIRST cta_corren
                 WHERE cta_corren.cod_cta_corren =  movto_cta_corren.cod_cta_corren
                 USE-INDEX ctcrrn_id
                 NO-LOCK NO-ERROR.

            FIND FIRST cta_corren_cta_ctbl
                 WHERE cta_corren_cta_ctbl.cod_cta_corren = aprop_ctbl_cmg.cod_cta_corren 
                 NO-LOCK NO-ERROR.

            FIND FIRST es_diario5
                 WHERE es_diario5.id = w-id
                 NO-ERROR.

            IF NOT AVAIL es_diario5 THEN DO:

                CREATE  es_diario5.
                ASSIGN  es_diario5.cod_cta_ctbl             =   aprop_ctbl_cmg.cod_cta_ctbl
                        es_diario5.ep_codigo                =   aprop_ctbl_cmg.cod_empresa
                        es_diario5.cod_estab                =   aprop_ctbl_cmg.cod_estab    
                        es_diario5.dat_transacao            =   aprop_ctbl_cmg.dat_transacao
                        es_diario5.ind_natur_lancto_ctbl    =   aprop_ctbl_cmg.ind_natur_lancto_ctbl
                        es_diario5.val_movto_cta_corren     =   aprop_ctbl_cmg.val_movto_cta_corren
                        es_diario5.id                       =   w-id 
                        es_diario5.historico                =   movto_cta_corren.des_histor_movto_cta_corren
                        es_diario5.banco                    =   cta_corren.cod_banco 
                        es_diario5.agencia                  =   cta_corren.cod_agenc_bcia
                        es_diario5.conta_banco              =   cta_corren.cod_cta_corren_bco
                        es_diario5.cta_corren               =   aprop_ctbl_cmg.cod_cta_corren
                        es_diario5.cod_docto_movto          =   movto_cta_corren.cod_docto_movto_cta_bco
                        es_diario5.cod_ccusto               =   aprop_ctbl_cmg.cod_ccusto 
                        .
            END.

        END. /*  IF AVAIL movto_cta_corren THEN DO:  */

    END. /*  FOR EACH aprop_ctbl_cmg  */

END. /* IF AVAIL tt-param THEN DO: */

/*--Fim de Acompanhamento--*/
RUN pi-finalizar IN h-acomp.

RETURN "OK":U.
