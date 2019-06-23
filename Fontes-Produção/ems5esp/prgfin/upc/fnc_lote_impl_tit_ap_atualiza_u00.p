/****************************************************************************************** 
** 	   Programa: fnc_lote_impl_tit_ap_atualiza_u00.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 21/08/2018
** Change/Chamado: REQ04
**      Objetivo: Criar campo "imposto" e solicitar o c¢digo do tributo qdo for verdadeiro - Usado apenas qdo o 
                  bordorì n∆o for de DARF
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
** 13/11/2018   Vando Ribeiro   DKP         REQ04/Validar a atualizaá∆o de titulos para n∆o permitir
                                                  a atualizaá∆o sem a aprovaá∆o do financeiro.
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: p-ind-event
** PAR∂METROS DE SA÷DA: tt-epc
** CADASTRADO NO FONTE TOTVS: fnc_lote_impl_tit_ap_atualiza
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{include\i-epc200.i1}

DEFINE INPUT  PARAMETER p-ind-event AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-epc.

DEFINE VARIABLE l-erro AS LOGICAL     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-re1005-glosa    AS LOGICAL     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-re1005-retencao AS LOGICAL     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-nr-contrato-re1005 AS INTEGER  NO-UNDO.

/*
MESSAGE p-ind-event
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH tt-epc:

    MESSAGE tt-epc.cod-event SKIP
            tt-epc.cod-parameter SKIP
            tt-epc.val-parameter
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
  */
&SCOPED-DEFINE pesquisa FIND ITEM_lote_impl_ap NO-LOCK WHERE ~
                         RECID(ITEM_lote_impl_ap) = INT(tt-epc.val-parameter) NO-ERROR. ~
                            IF NOT AVAIL ITEM_lote_impl_ap THEN NEXT.  ~
                                                                       ~
                            FIND ext_espec_docto NO-LOCK WHERE         ~
                                 ext_espec_docto.cod_espec_docto = ITEM_lote_impl_ap.cod_espec_docto NO-ERROR. ~
                            IF NOT AVAIL ext_espec_docto THEN NEXT.                                            ~
                                                                                                               ~
                            IF NOT ext_espec_docto.log_fgts AND NOT ext_espec_docto.log_gps AND NOT ext_espec_docto.log_imposto THEN NEXT. ~
                                                                                                                                           ~
                            FIND ext_ITEM_lote_impl_ap                                                                                     ~
                                WHERE ext_ITEM_lote_impl_ap.cod_estab     = ITEM_lote_impl_ap.cod_estab                                    ~
                                AND   ext_ITEM_lote_impl_ap.cod_refer     = ITEM_lote_impl_ap.cod_refer                                    ~
                                AND   ext_ITEM_lote_impl_ap.num_seq_refer = ITEM_lote_impl_ap.num_seq_refer NO-ERROR.                      


/* Verifica a retená∆o de contrato e glosa */
IF p-ind-event = "Integraá∆o AED Pagamento" AND l-re1005-retencao THEN DO:
                                                                                           
    DEFINE VARIABLE c-especie2 AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-especie  AS CHARACTER   NO-UNDO.

    DEF BUFFER bf-tit_ap FOR tit_ap.
                                                                                                                                           
/*     FIND es_parametros NO-LOCK                                                */
/*           WHERE es_parametros.cod_prog_dtsul = "re1001"                       */
/*           AND   es_parametros.cod_referencia = "especie_glosa"                */
/*           AND   es_parametros.dat_valid_ini <= TODAY NO-ERROR.                */
/*     ASSIGN c-especie2 = es_parametros.cod_parametro WHEN AVAIL es_parametros. */

    FIND es_parametros NO-LOCK 
          WHERE es_parametros.cod_prog_dtsul = "re1001" 
          AND   es_parametros.cod_referencia = "especie_retencao" 
          AND   es_parametros.dat_valid_ini <= TODAY NO-ERROR.
    ASSIGN c-especie = es_parametros.cod_parametro WHEN AVAIL es_parametros.

    FOR EACH tt-epc WHERE
             tt-epc.cod-event = p-ind-event AND 
             tt-epc.cod-parameter = "Recid tit ap":
    
        FIND bf-tit_ap WHERE
             RECID(bf-tit_ap) = INT(tt-epc.val-parameter) NO-ERROR. 
        IF AVAIL bf-tit_ap THEN DO: 
            IF  bf-tit_ap.cod_espec_docto = c-especie THEN
            ASSIGN bf-tit_ap.log_pagto_bloqdo = YES.

            IF l-re1005-retencao THEN DO:
            
                FIND es_contrat_for_tit_ap NO-LOCK WHERE
                     es_contrat_for_tit_ap.cod_estab     = bf-tit_ap.cod_estab   AND 
                     es_contrat_for_tit_ap.num_id_tit_ap = bf-tit_ap.num_id_tit_ap AND 
                     es_contrat_for_tit_ap.nr-contrato   = i-nr-contrato-re1005 NO-ERROR.
                IF NOT AVAIL es_contrat_for_tit_ap THEN DO:
                    CREATE es_contrat_for_tit_ap.
                    ASSIGN es_contrat_for_tit_ap.cod_estab     = bf-tit_ap.cod_estab 
                           es_contrat_for_tit_ap.num_id_tit_ap = bf-tit_ap.num_id_tit_ap
                           es_contrat_for_tit_ap.nr-contrato   = i-nr-contrato-re1005.
                END.
            END.
        END.
    END.
END.

/* Validaá∆o da Atualizaá∆o */
IF p-ind-event = "ITEM_lote_impl_ap" THEN DO:

    FOR EACH tt-epc WHERE
             tt-epc.cod-event = p-ind-event:

        IF tt-epc.cod-parameter <> "Recid" THEN NEXT.

        {&pesquisa} 

        IF NOT AVAIL ext_item_lote_impl_ap THEN DO:
            
            MESSAGE "EspÇcie do t°tulo Ç de Tributo, porÇm " SKIP
                    "as informaá‰es n∆o foram digitadas!"  SKIP
                    "Atualizaá∆o do lote n∆o ser† permitida!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

           ASSIGN l-erro = YES.

        END.
    END.
END.


/* Validaá∆o da Exclus∆o */
IF p-ind-event = "BEFORE_DELETE" THEN DO:

    FOR EACH tt-epc WHERE
             tt-epc.cod-event = p-ind-event:

        IF tt-epc.cod-parameter <> "deletar" THEN NEXT.

        {&pesquisa}

        IF AVAIL ext_ITEM_lote_impl_ap THEN
            DELETE ext_ITEM_lote_impl_ap.
    END.
END.

IF l-erro THEN RETURN "NOK".


/* Vando Ribeiro*/
/**** RETIRADA ROTINA DE BLOQUEIO DE TÕTULOS MANUAIS - 
RUN prgfin\upc\fnc_lote_impl_tit_ap_atualiza_u01.p (INPUT p-ind-event,
                                                    OUTPUT l-erro,
                                                    INPUT-OUTPUT TABLE tt-epc).

IF l-erro THEN RETURN "NOK".
*******/

