/****************************************************************************************** 
** 	   Programa: apl007ca-u02.p
**   	  Autor: Unknow
** 	 Fornecedor: DKP
**         Data: ?/2017
** Change/Chamado: 
**      Objetivo: (Cadastro Operacao Financ - Inclus∆o) Validar a criaá∆o de campo para informar o contrato m∆e na tela da alteraá∆o/modificaá∆o da operaá∆o financeira
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: add_operac_financ
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

/*** Par≥metros de Recepªío da UPC ***/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID NO-UNDO.

DEFINE VARIABLE l-ok AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE tt-erro
       FIELDS cod-erro AS INTEGER
       FIELDS des-erro AS CHARACTER FORMAT "x(100)".

/*** definicao de variaveis         ***/
def var wh-frame       as widget-handle no-undo.
def var h-frame        as handle        no-undo.
def var h-objeto       as handle        no-undo.
def var c-objeto       as character     no-undo.

def new global shared var wgh-v_log_variac_cmcac as widget-handle no-undo.
def new global shared var wgh-txt_v_inst_banc    as widget-handle no-undo.
def new global shared var wgh-fil_v_inst_banc    as widget-handle no-undo.
def new global shared var wgh-cod_espec_docto    as widget-handle no-undo.
def new global shared var gr-es_operac_financ as rowid no-undo.

def new global shared var v_cod_empres_usuar as CHARACTER format "x(3)":U label "Empresa" column-label "Empresa" no-undo.

def new global shared var wgh-cod_banco              as widget-handle no-undo.
def new global shared var wgh-cod_produt_financ      as widget-handle no-undo.
def new global shared var wgh-cod_operac_financ      as widget-handle no-undo.
def new global shared var wgh-bt_ok                  as widget-handle no-undo.
def new global shared var wgh-bt_sav                 as widget-handle no-undo.
def new global shared var wgh-bt_ok_false            as widget-handle no-undo.
def new global SHARED var wgh-bt_sav_false           as widget-handle no-undo.
def new global shared var wgh-dat_operac_financ      as widget-handle no-undo.
def new global shared VAR wgh-dat_vencto_operac_financ as widget-handle no-undo.
DEF NEW GLOBAL SHARED VAR wgh-cod_cta_corren_pad       as widget-handle no-undo. 
def new global shared var wh-fill                   AS widget-handle no-undo.
def new global shared var wh-fill-2                 AS widget-handle no-undo.
DEF NEW GLOBAL SHARED VAR wh-apl007ca-bt-cad-fil    AS widget-handle no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-cod_indic_econ_orig_apl AS WIDGET-HANDLE NO-UNDO.

def new global shared var wh-cotacao AS WIDGET-HANDLE no-undo.
DEF BUFFER b_operac_financ FOR operac_financ.
DEF BUFFER cotacao FOR ems2cadme.cotacao.
DEF BUFFER bf-operac_financ FOR operac_financ.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:private-data,"~/"), p-wgh-object:private-data, "~/").

{include/i_fclpreproc.i}

&IF "{&aplica_facelift}" = "YES" &THEN
	{include/i_fcldef.i}
&endif

/* MESSAGE "p-ind-event " p-ind-event        SKIP */
/*         "p-ind-object" p-ind-object       SKIP */
/*         "p-wgh-object" p-wgh-object:NAME  SKIP */
/*         "p-wgh-frame " p-wgh-frame        SKIP */
/*         "p-cod-table " p-cod-table        SKIP */
/*         "c-objeto    " c-objeto                */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.    */

/* Funá∆o para c†lculo de moeda */
FUNCTION fncMoeda RETURNS CHAR (INPUT ipc-estab AS CHAR):

    DEF BUFFER pais FOR ems5.pais.

    FIND estabelecimento NO-LOCK WHERE
         estabelecimento.cod_estab = ipc-estab NO-ERROR.
    IF AVAIL estabelecimento THEN DO:

        FIND pessoa_jurid NO-LOCK WHERE
             pessoa_jurid.num_pessoa_jurid = estabelecimento.num_pessoa_jurid NO-ERROR.

        FIND pais NO-LOCK WHERE
             pais.cod_pais = pessoa_jurid.cod_pais NO-ERROR.

        FIND FIRST histor_finalid_econ NO-LOCK
            WHERE histor_finalid_econ.cod_finalid_econ = pais.cod_finalid_econ_pais
              AND histor_finalid_econ.dat_fim_valid_final     >= TODAY
              AND histor_finalid_econ.dat_inic_valid_finalid  <= TODAY NO-ERROR.
        IF AVAIL histor_finalid_econ THEN
            RETURN histor_finalid_econ.cod_indic_econ.
        ELSE RETURN "".

    END.
END.

IF c-objeto = "HLP=36" and p-ind-object = "VIEWER" then do:
    /* inicializa handles e cria campos de extenªío */
    IF p-ind-event = "INITIALIZE" THEN DO:
        RUN pi-inicializa NO-ERROR.
    END.
    IF p-ind-event = "DISPLAY" then do:
        RUN pi-display no-error.
    END.
    
    IF p-ind-event = "ENABLE" THEN 
        RUN pi-enable NO-ERROR.
END.


PROCEDURE pi-enable:

    /* DPC - Varifica a moeda do contrato m∆e informado x moeda da operaá∆o financeira - se for real Ç obrigat¢rio digitar o valor da cotaá∆o contratada, caso contr†rio ir† assumir a cotaá∆o 
    da operaá∆o */
    DEFINE VARIABLE cmoeda AS CHARACTER   NO-UNDO.

    FIND b_operac_financ NO-LOCK WHERE
            RECID(b_operac_financ) = p-row-table NO-ERROR.
    IF AVAIL b_operac_financ THEN DO:

        /* Busca Estabelecimento */
        FIND cta_corren NO-LOCK WHERE 
             cta_corren.cod_cta_corren = b_operac_financ.cod_cta_corren NO-ERROR.
        IF AVAIL cta_corren THEN DO:
                            
            ASSIGN cmoeda = fncMoeda(cta_corren.cod_estab).

            /* Qdo moeda n∆o Ç real assume cotaá∆o contratada */
            IF cmoeda <> b_operac_financ.cod_indic_econ_orig_apl THEN 
                ASSIGN wh-cotacao:SENSITIVE = NO
                       wh-cotacao:SCREEN-VALUE = STRING(b_operac_financ.val_cotac_contrat) WHEN AVAIL b_operac_financ.

            ELSE ASSIGN wh-cotacao:SENSITIVE = YES. /* Qdo real o usu†rio dever† informar uma cotaá∆o */
        END.
    END.
END.


PROCEDURE pi-display:
   
   IF VALID-HANDLE(wgh-cod_banco        ) AND
      VALID-HANDLE(wgh-cod_produt_financ) AND
      VALID-HANDLE(wgh-cod_operac_financ) THEN DO:
      FIND FIRST es_operac_financ
           WHERE es_operac_financ.cod_banco          = wgh-cod_banco:SCREEN-VALUE          AND
                 es_operac_financ.cod_produt_financ  = wgh-cod_produt_financ:SCREEN-VALUE  AND
                 es_operac_financ.cod_operac_financ  = wgh-cod_operac_financ:SCREEN-VALUE
           NO-LOCK NO-ERROR.
      IF AVAILABLE(es_operac_financ) THEN DO:
         ASSIGN wh-fill:SCREEN-VALUE    = es_operac_financ.cod_contrat_apf
                wh-fill-2:SCREEN-VALUE  = ""
                wh-cotacao:SCREEN-VALUE = STRING(es_operac_financ.vl-cotac-saldo).

         APPLY "leave":u TO wh-fill.
      END.
   END.

   /* Inativa o campo do contrato caso a operaá∆o esteja efetivada */
   FIND b_operac_financ NO-LOCK WHERE
        RECID(b_operac_financ) = p-row-table NO-ERROR.
   IF AVAIL b_operac_financ AND b_operac_financ.ind_sit_operac_financ_apl = "encerrada"
        THEN wh-fill:SENSITIVE = NO.

END PROCEDURE.

PROCEDURE pi-grava:
   DEFINE VARIABLE d-cotacao AS DECIMAL NO-UNDO.

   FIND FIRST es_operac_financ EXCLUSIVE-LOCK
           WHERE es_operac_financ.cod_banco          = wgh-cod_banco:SCREEN-VALUE          AND        
                 es_operac_financ.cod_produt_financ  = wgh-cod_produt_financ:SCREEN-VALUE  AND
                 es_operac_financ.cod_operac_financ  = wgh-cod_operac_financ:SCREEN-VALUE  NO-ERROR.
   IF NOT AVAIL es_operac_financ THEN DO:
       CREATE es_operac_financ.                                                         
       ASSIGN es_operac_financ.cod_banco          = wgh-cod_banco:SCREEN-VALUE          
              es_operac_financ.cod_produt_financ  = wgh-cod_produt_financ:SCREEN-VALUE  
              es_operac_financ.cod_operac_financ  = wgh-cod_operac_financ:SCREEN-VALUE.
   END.
   ASSIGN es_operac_financ.cod_contrat_apf = wh-fill:SCREEN-VALUE
          es_operac_financ.vl-cotac-saldo  = DEC(wh-cotacao:SCREEN-VALUE)
          es_operac_financ.log_atualizado  = NO. /* Log controla a utilizaá∆o da operaá∆o financeira no contrato m∆e. S¢ utiliza qdo operaá∆o est† ativa ou encerrada */

END PROCEDURE.

IF p-ind-event  = "CHOOSE" AND
   p-ind-object = "wgh-bt_ok_false" THEN DO:
    
   RUN pi-validate (OUTPUT l-ok). 
   
   IF l-ok THEN RUN pi-grava.
   ELSE DO:
       FOR EACH tt-erro:
           RUN utp/ut-msgs.p (INPUT "show",
                              INPUT 17006,
                              INPUT tt-erro.des-erro).
       END.

       RETURN "NOK".
   END.
      
   IF VALID-HANDLE(wgh-bt_ok) THEN
       APPLY "choose" TO wgh-bt_ok.
END.

IF p-ind-event  = "CHOOSE" AND
   p-ind-object = "wgh-bt_sav_false" THEN DO:

   RUN pi-validate (OUTPUT l-ok).

   IF l-ok THEN DO:

       IF VALID-HANDLE(wgh-bt_sav) THEN
             APPLY "choose" TO wgh-bt_sav.

       RUN pi-grava.
   END.             
   ELSE DO:
       FOR EACH tt-erro:
           RUN utp/ut-msgs.p (INPUT "show",
                              INPUT 17006,
                              INPUT tt-erro.des-erro).
       END.
       RETURN "NOK".
   END.
END.

PROCEDURE pi-validate:
   DEFINE OUTPUT PARAMETER ip-ok AS LOGICAL NO-UNDO.

   DEFINE VARIABLE d_data_vencimento AS DATE NO-UNDO.

   EMPTY TEMP-TABLE tt-erro.

   ASSIGN ip-ok = YES.

   FIND bf-operac_financ NO-LOCK WHERE
        RECID(bf-operac_financ) = p-row-table NO-ERROR.

   IF VALID-HANDLE(wgh-cod_produt_financ) THEN DO:
      /* Verifica se o produto financeiro tem que informar o contrato */
      FIND FIRST es_produt_financ
           WHERE es_produt_financ.cod_produt_financ = wgh-cod_produt_financ:SCREEN-VALUE
           NO-LOCK NO-ERROR.
/*       IF NOT AVAILABLE(es_produt_financ) OR es_produt_financ.log_contrato = NO AND wh-fill:SCREEN-VALUE <> "" THEN DO: */
/*          IF wh-fill:SCREEN-VALUE <> "" THEN DO:                                                                        */
/*             CREATE tt-erro.                                                                                            */
/*             ASSIGN tt-erro.cod-erro  = 1                                                                               */
/*                    tt-erro.des-erro = "Erro~~Produto Financeiro n∆o utilizado em Contratos Intercompany".              */
/*             ASSIGN ip-ok = NO.                                                                                         */
/*          END.                                                                                                          */
/*       END.                     
                                                                                        */

      IF AVAILABLE(es_produt_financ) AND es_produt_financ.log_contrato AND wh-fill:SCREEN-VALUE = ""  THEN DO:
         CREATE tt-erro.
         ASSIGN tt-erro.cod-erro  = 1
                tt-erro.des-erro = "Erro~~Favor informar o Contrato Intercompany".
         ASSIGN ip-ok = NO.                                        
      END.


      IF AVAILABLE(es_produt_financ) AND es_produt_financ.log_contrato THEN DO:

          FIND FIRST contrat_apf
               WHERE contrat_apf.cod_contrat_apf = wh-fill:SCREEN-VALUE
               NO-LOCK NO-ERROR.
          IF AVAILABLE contrat_apf  THEN DO:
             ASSIGN d_data_vencimento = contrat_apf.dat_fim_valid.
             FOR LAST aditivo_contrat_apf OF contrat_apf NO-LOCK USE-INDEX idx1:
                ASSIGN d_data_vencimento = aditivo_contrat_apf.dat_fim_valid.
             END.
             IF DATE(wgh-dat_vencto_operac_financ:SCREEN-VALUE) > d_data_vencimento THEN DO:
               CREATE tt-erro.
               ASSIGN tt-erro.cod-erro  = 2
                      tt-erro.des-erro = "Data de Vencimento inv†lida~~Data de vencimento deve ser igual a ultima data do contrato".
               ASSIGN ip-ok = NO.
             END.
    
             IF DATE(wgh-dat_operac_financ:SCREEN-VALUE) < contrat_apf.dat_inic_valid THEN DO:
                   CREATE tt-erro.
                   ASSIGN tt-erro.cod-erro  = 2
                          tt-erro.des-erro = "Data da Operaá∆o (" + wgh-dat_operac_financ:SCREEN-VALUE + ") fora da data de in°cio do contrato (" + STRING(contrat_apf.dat_inic_valid) + ".".
                   ASSIGN ip-ok = NO.
             END.                         
    
             /* Valida a empresa - deve ser a mesma do contrato m∆e e da operaá∆o financeira */
/*              IF contrat_apf.cod_empresa <> bf-operac_financ.cod_empresa THEN DO:                                                                                                      */
/*                  CREATE tt-erro.                                                                                                                                                      */
/*                  ASSIGN tt-erro.cod-erro  = 3                                                                                                                                         */
/*                         tt-erro.des-erro = "Empresa do contrato " + contrat_apf.cod_empresa + " Ç diferente da empresa da operaá∆o financeira " + bf-operac_financ.cod_empresa + ".". */
/*                  ASSIGN ip-ok = NO.                                                                                                                                                   */
/*              END.                                                                                                                                                                     */
    
             /* Verifica se a Administradora do Contrato pode usar o produto informado */
             IF NOT CAN-FIND(es_produt_fin_adm NO-LOCK 
                 WHERE es_produt_fin_adm.cod_admdra_apf    = contrat_apf.cod_admdra_apf
                   AND es_produt_fin_adm.cod_produt_financ = wgh-cod_produt_financ:SCREEN-VALUE) THEN DO:
    
                 CREATE tt-erro.
                 ASSIGN tt-erro.cod-erro  = 3
                        tt-erro.des-erro = "Administradora do contrato " + contrat_apf.cod_admdra_apf + " n∆o vinculada ao produto " + wgh-cod_produt_financ:SCREEN-VALUE + ".".
                 ASSIGN ip-ok = NO.
             END.
          END.
       END.
   END. /* IF VALID-HANDLE(wgh-cod_produt_financ) */

   /* DPC - Varifica a moeda do contrato m∆e informado x moeda da operaá∆o financeira - se for real Ç obrigat¢rio digitar o valor da cotaá∆o contratada, caso contr†rio ir† assumir a cotaá∆o 
    da operaá∆o */

    IF AVAILABLE(es_produt_financ) AND es_produt_financ.log_contrato THEN DO:
        DEFINE VARIABLE cmoeda AS CHARACTER   NO-UNDO.
    
        /* Valida a moeda do contrato, caso seja igual a moeda da empresa, ent∆o usar† a cotaá∆o informado, do contr†rio usuar† a cotaá∆o do contrato */
        IF VALID-HANDLE(wgh-cod_cta_corren_pad) AND VALID-HANDLE(wgh-cod_indic_econ_orig_apl) AND wgh-cod_cta_corren_pad:SCREEN-VALUE <> ""  THEN DO:
    
            FIND b_operac_financ NO-LOCK WHERE
                RECID(b_operac_financ) = p-row-table NO-ERROR.
    
            /* Busca Estabelecimento */
            FIND cta_corren NO-LOCK WHERE 
                 cta_corren.cod_cta_corren = wgh-cod_cta_corren_pad:SCREEN-VALUE NO-ERROR.
            IF AVAIL cta_corren THEN DO:
                                
                ASSIGN cmoeda = fncMoeda(cta_corren.cod_estab).
    
                /* Qdo moeda n∆o Ç real assume cotaá∆o contradata */
                IF cmoeda <> wgh-cod_indic_econ_orig_apl:SCREEN-VALUE THEN 
                    ASSIGN wh-cotacao:SENSITIVE = NO
                           wh-cotacao:SCREEN-VALUE = STRING(b_operac_financ.val_cotac_contrat) WHEN AVAIL b_operac_financ.
                ELSE DO:
                    /* Qdo real o usu†rio dever† informar uma cotaá∆o */
                    ASSIGN wh-cotacao:SENSITIVE = YES.
    
                    IF dec(wh-cotacao:SCREEN-VALUE) = 0 THEN DO:
    
                        CREATE tt-erro.
                        ASSIGN tt-erro.cod-erro  = 3
                               tt-erro.des-erro = "Para operaá∆o financeira em moeda " + wgh-cod_indic_econ_orig_apl:SCREEN-VALUE + " Ç necess†rio informar a cotaá∆o para o contrato m∆e.".
                        ASSIGN ip-ok = NO.
                    END.
                END.
            END.
        END.
    END.
END PROCEDURE.


/* localiza e ajusta posiªío dos campos na viewer e cria campos novos */
procedure pi-inicializa:
    ASSIGN h-frame = p-wgh-frame:FIRST-CHILD
           h-frame = h-frame:FIRST-CHILD.   
    DO  WHILE VALID-HANDLE(h-frame):
        IF  h-frame:TYPE <> "field-group" THEN DO:
            IF h-frame:TYPE = "FILL-IN" THEN DO:
                IF h-frame:NAME = "cod_banco" THEN DO:
                    assign wgh-cod_banco = h-frame:handle.
                END.
                IF h-frame:NAME = "cod_produt_financ" THEN DO:
                    assign wgh-cod_produt_financ  = h-frame:handle.
                END.
                IF h-frame:NAME = "cod_operac_financ" THEN DO:
                    assign wgh-cod_operac_financ  = h-frame:handle.
                END.
                IF h-frame:NAME = "dat_operac_financ" THEN DO:
                    assign wgh-dat_operac_financ  = h-frame:handle.
                END.
                IF h-frame:NAME = "dat_vencto_operac_financ" THEN DO:
                    assign wgh-dat_vencto_operac_financ  = h-frame:handle.
                END.

                IF h-frame:NAME = "cod_indic_econ_orig_apl" THEN DO: /* DPC */
                    assign wgh-cod_indic_econ_orig_apl  = h-frame:handle.
                END.

                IF h-frame:NAME = "cod_cta_corren_padr" THEN DO: /* DPC */
                    assign wgh-cod_cta_corren_pad  = h-frame:handle.
                END.                              

            END.
            IF h-frame:TYPE = "button" THEN DO:
                IF h-frame:NAME = "bt_ok" THEN DO:
                    assign wgh-bt_ok = h-frame:handle.
                END.
                IF h-frame:NAME = "bt_sav" THEN DO:
                    assign wgh-bt_sav  = h-frame:handle.
                END.
            END.
            assign h-frame = h-frame:NEXT-SIBLING. /* ver observaªío sobre o NEXT-SIBLING no fim deste exemplo */
        END.
        ELSE DO:
            LEAVE.
        END.
    END.

    IF VALID-HANDLE(wgh-bt_ok) THEN DO:
       IF NOT VALID-HANDLE(wgh-bt_ok_false) THEN DO:
          
          CREATE BUTTON wgh-bt_ok_false
          ASSIGN FRAME     = wgh-bt_ok:FRAME
                 WIDTH     = wgh-bt_ok:WIDTH
                 HEIGHT    = wgh-bt_ok:HEIGHT
                 TOOLTIP   = wgh-bt_ok:TOOLTIP
                 LABEL     = wgh-bt_ok:LABEL + "*"
                 HELP      = wgh-bt_ok:HELP
                 ROW       = wgh-bt_ok:ROW
                 COL       = wgh-bt_ok:COLUMN
                 FONT      = wgh-bt_ok:FONT
                 VISIBLE   = YES
                 SENSITIVE = YES.

           ON "CHOOSE" OF wgh-bt_ok_false PERSISTENT RUN prgfin\upc\apl007ca-u00.p (INPUT "CHOOSE",
                                                                                    INPUT "wgh-bt_ok_false",
                                                                                    INPUT  wgh-bt_ok_false,                  
                                                                                    INPUT p-wgh-frame,                  
                                                                                    INPUT "",                           
                                                                                    INPUT p-row-table).                 
           wgh-bt_ok_false:MOVE-AFTER-TAB-ITEM(wgh-bt_ok:HANDLE).
       END.
    END.

    IF VALID-HANDLE(wgh-bt_sav) THEN DO:
       IF NOT VALID-HANDLE(wgh-bt_sav_false) THEN DO:
          CREATE BUTTON wgh-bt_sav_false
          ASSIGN FRAME     = wgh-bt_sav:FRAME
                 WIDTH     = wgh-bt_sav:WIDTH
                 HEIGHT    = wgh-bt_sav:HEIGHT
                 TOOLTIP   = wgh-bt_sav:TOOLTIP
                 LABEL     = wgh-bt_sav:LABEL + "*"
                 HELP      = wgh-bt_sav:HELP
                 ROW       = wgh-bt_sav:ROW
                 COL       = wgh-bt_sav:COLUMN
                 FONT      = wgh-bt_sav:FONT
                 VISIBLE   = YES
                 SENSITIVE = YES.

           ON "CHOOSE" OF wgh-bt_sav_false PERSISTENT RUN prgfin\upc\apl007ca-u00.p (INPUT "CHOOSE",
                                                                                    INPUT "wgh-bt_sav_false",
                                                                                    INPUT  wgh-bt_sav_false,                  
                                                                                    INPUT p-wgh-frame,                  
                                                                                    INPUT "",                           
                                                                                    INPUT p-row-table).                 
           wgh-bt_sav_false:MOVE-AFTER-TAB-ITEM(wgh-bt_sav:HANDLE).
       END.
    END.
END.
