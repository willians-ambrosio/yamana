/****************************************************************************************** 
** 	   Programa: upc/upc_apl007aa-u02.p
**   	  Autor: Andr≤ Araújo - TOTVS DRG-SP - unidade Campinas
** 	 Fornecedor: DKP
**         Data: 29/09/2011
** Change/Chamado: 
**      Objetivo: Cria bot∆o para para sobrepor o bot∆o de atualizar e realizar as validaá‰es do contrato m∆e 
**
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

{tools/fc-handle-obj.i}
{tools/fc-falso.i}
/*** definicao de variaveis         ***/
def var wh-frame       as widget-handle no-undo.
def var h-frame        as handle        no-undo.
def var h-objeto       as handle        no-undo.
def var c-objeto       as character     no-undo.

def new global shared var wgh-v_log_variac_cmcac as widget-handle no-undo.
def new global shared var wgh-txt_v_inst_banc    as widget-handle no-undo.
def new global shared var wgh-fil_v_inst_banc    as widget-handle no-undo.
def new global shared var wgh-cod_espec_docto    as widget-handle no-undo.
def new global shared var wh-moeda               as widget-handle no-undo.


def new global shared var gr-es_operac_financ as rowid no-undo.
DEFINE new global shared var l-erro-adics AS LOGICAL   NO-UNDO.
DEF                   VAR c-handle-obj                AS CHARACTER      NO-UNDO.

def new global shared var wh-cod_banco-aa              as widget-handle no-undo.
def new global shared var wh-cod_produt_financ-aa      as widget-handle no-undo.
def new global shared var wh-cod_operac_financ-aa      as widget-handle no-undo.
def new global shared var wh-bt_atz                  as widget-handle no-undo.
def new global shared var wh-bt_atz_false            as widget-handle no-undo.
def new global shared var wgh-bt_sav_false           as widget-handle no-undo.
def new global shared var wgh-dat_operac_financ      as widget-handle no-undo.
def new global shared VAR wgh-dat_vencto_operac_financ as widget-handle no-undo.
def new global shared var wh-fill                      as widget-handle no-undo.
def new global shared var wh-fill-2                    as widget-handle no-undo.
DEF NEW GLOBAL SHARED VAR wh-apl007ca-bt-cad-fil       AS widget-handle no-undo.
def new global shared VAR wh-empresa-cc                as widget-handle no-undo.
def new global shared VAR wh-valor-oper                as widget-handle no-undo.


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



if c-objeto = "HLP=36" and p-ind-object = "VIEWER" then do:
    /* inicializa handles e cria campos de extenªío */
    if p-ind-event = "INITIALIZE" then do:
        run pi-inicializa no-error.

        ASSIGN c-handle-obj = fc-handle-obj("bt_atz", p-wgh-frame).
        ASSIGN wh-bt_atz    = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
            
        ASSIGN wh-bt_atz_false     = fc-falso(wh-bt_atz, wh-bt_atz:FRAME, "").
        
/*     ASSIGN wh-escd1508b-btOK:SENSITIVE = NO     */
/*            wh-escd1508b-btOK:VISIBLE   = NO     */
/*            wh-escd1508b-btOK:TAB-STOP  = NO     */
/*            wh-escd1508b-btOK-fs:ROW    = 14.27. */

           wh-bt_atz:SENSITIVE = NO. 
           wh-bt_atz:VISIBLE   = NO. 
           wh-bt_atz:TAB-STOP  = NO. 
           wh-bt_atz_false:LOAD-IMAGE-UP (wh-bt_atz:IMAGE).      
           wh-bt_atz_false:MOVE-TO-TOP().
               
        ON 'CHOOSE':U OF wh-bt_atz_false PERSISTENT RUN prgfin\upc\apl007aa-u02.p("CHOOSE",
                                                                                  "bt_atz",
                                                                                   p-wgh-object,
                                                                                   p-wgh-frame ,
                                                                                   p-cod-table ,
                                                                                   p-row-table ).
    END.
END.

IF p-ind-event  = "CHOOSE"                  AND
   p-ind-object = "bt_atz"                  THEN DO:

    FIND FIRST es_operac_financ NO-LOCK
                 WHERE es_operac_financ.cod_banco          = wh-cod_banco-aa:SCREEN-VALUE
                 AND   es_operac_financ.cod_produt_financ  = wh-cod_produt_financ-aa:SCREEN-VALUE
                 AND   es_operac_financ.cod_operac_financ  = wh-cod_operac_financ-aa:SCREEN-VALUE NO-ERROR.

    FIND es_produt_financ NO-LOCK WHERE
         es_produt_financ.cod_produt_financ = wh-cod_produt_financ-aa:SCREEN-VALUE NO-ERROR.
    IF NOT AVAIL es_produt_financ THEN 
        ASSIGN l-erro-adics = NO.
       
    IF NOT es_produt_financ.log_contrato AND NOT es_produt_financ.log_rof AND NOT es_produt_financ.log_drawdown AND NOT es_produt_financ.log_cambio
        THEN ASSIGN l-erro-adics = NO.
    ELSE DO:
        
        IF es_produt_financ.log_contrato THEN DO: 
           
            IF NOT AVAIL es_operac_financ 
               THEN ASSIGN l-erro-adics = YES.

            IF AVAIL es_operac_financ AND TRIM(es_operac_financ.cod_contrat_apf) = "" 
               THEN ASSIGN l-erro-adics = YES.
            /* felipe */
            ELSE IF AVAIL es_operac_financ AND TRIM(es_operac_financ.cod_contrat_apf) <> ""  
               THEN ASSIGN l-erro-adics = NO.
            /* felipe */
    
        END.
        
        /* Valida preenchimento contrato ROF */
        IF es_produt_financ.log_rof AND 
            NOT CAN-FIND(FIRST es_operac_financ_rof NO-LOCK
                  WHERE es_operac_financ_rof.cod_banco          = es_operac_financ.cod_banco        
                  AND   es_operac_financ_rof.cod_produt_financ  = es_operac_financ.cod_produt_financ
                  AND   es_operac_financ_rof.cod_operac_financ  = es_operac_financ.cod_operac_financ
                  AND   es_operac_financ_rof.cod_contrat_apf    = es_operac_financ.cod_contrat_apf)
            THEN ASSIGN l-erro-adics = YES.

        /* Contrato Drawdown */
        IF es_produt_financ.log_drawdown AND (NOT AVAIL es_operac_financ OR TRIM(es_operac_financ.arquivo) = "")
            THEN ASSIGN l-erro-adics = YES.
        
        /* Contrato de CÉmbio */
        /* Contrato CÉmbio */
        IF es_produt_financ.log_cambio AND 
            NOT CAN-FIND(FIRST es_operac_financ_cambio NO-LOCK                                                         
                WHERE es_operac_financ_cambio.cod_banco          = es_operac_financ.cod_banco                
                AND   es_operac_financ_cambio.cod_produt_financ  = es_operac_financ.cod_produt_financ        
                AND   es_operac_financ_cambio.cod_operac_financ  = es_operac_financ.cod_operac_financ        
                AND   es_operac_financ_cambio.cod_contrat_apf    = es_operac_financ.cod_contrat_apf)
            THEN ASSIGN l-erro-adics = YES.
    END.
    IF l-erro-adics THEN DO:
          MESSAGE "N∆o foram preenchidos os valores de Contrato m∆e ou ROF ou Drawdown ou CÉmbio!"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
    END.
    IF es_produt_financ.log_contrato THEN DO: /* Validaá‰es apenas qdo obrigat¢rio contrato m∆e */

      /*Verificar cotaá∆o*/
      FIND FIRST contrat_apf no-lock
          where contrat_apf.cod_contrat_apf = es_operac_financ.cod_contrat_apf NO-ERROR.
      IF AVAIL contrat_apf THEN DO:
      
          IF contrat_apf.cod_indic_econ = "Dolar" AND
               wh-moeda:SCREEN-VALUE      = "Real"  THEN DO:

              IF es_operac_financ.vl-cotac-saldo = 0 THEN DO:

                  MESSAGE "Nao foi digitada a cotaá∆o para efeito de saldo do contrato!" SKIP
                          "Por favor altere e preencha o campo"
                      VIEW-AS ALERT-BOX ERROR BUTTONS OK.

                  ASSIGN l-erro-adics = YES.

              END.
          END.
          
          /*Verifica se a empresa bate*/
          IF contrat_apf.cod_empresa <> wh-empresa-cc:SCREEN-VALUE THEN DO:

              MESSAGE "Empresa da Conta Corrente Diferente da Empresa do Contrato, Favor Ajustar!"
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              ASSIGN l-erro-adics = YES.
          END.

          /*Verifica se tem saldo...*/
          IF DEC(wh-valor-oper:SCREEN-VALUE) / es_operac_financ.vl-cotac-saldo > (contrat_apf.val_lim_cr_contrat_apf + contrat_apf.val_aditivo) - contrat_apf.val_operac_financ THEN DO:

              MESSAGE "N∆o Existe Saldo Suficiente no contrato!"
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              ASSIGN l-erro-adics = YES.
          END.
      END.
    END.

    IF l-erro-adics = NO THEN
       APPLY "choose" TO wh-bt_atz.
   
END.

IF p-ind-event  = "CHOOSE"                  AND
   p-ind-object = "wh-apl007aa-bt-cad-fil" THEN DO:

    FIND FIRST es_operac_financ NO-LOCK
        WHERE es_operac_financ.cod_banco          = wh-cod_banco-aa:SCREEN-VALUE
        AND   es_operac_financ.cod_produt_financ  = wh-cod_produt_financ-aa:SCREEN-VALUE
        AND   es_operac_financ.cod_operac_financ  = wh-cod_operac_financ-aa:SCREEN-VALUE      NO-ERROR.
    IF AVAILABLE es_operac_financ THEN DO:

      ASSIGN gr-es_operac_financ = ROWID(es_operac_financ).

      RUN prgfin/apl/apya501e.w.

      ASSIGN l-erro-adics = NO.

    END.
END.    

procedure pi-inicializa:
    ASSIGN h-frame = p-wgh-frame:FIRST-CHILD
           h-frame = h-frame:FIRST-CHILD.   
    DO  WHILE VALID-HANDLE(h-frame):
        IF  h-frame:TYPE <> "field-group" THEN DO:
            IF h-frame:TYPE = "FILL-IN" THEN DO:
                
                /*wh-moeda*/
                IF h-frame:NAME = "cod_banco" THEN DO:
                    assign wh-cod_banco-aa = h-frame:handle.
                END.
                IF h-frame:NAME = "cod_produt_financ" THEN DO:
                    assign wh-cod_produt_financ-aa  = h-frame:handle.
                END.
                IF h-frame:NAME = "cod_operac_financ" THEN DO:
                    assign wh-cod_operac_financ-aa  = h-frame:handle.
                END.
                IF h-frame:NAME = "dat_operac_financ" THEN DO:
                    assign wgh-dat_operac_financ  = h-frame:handle.
                END.
                IF h-frame:NAME = "dat_vencto_operac_financ" THEN DO:
                    assign wgh-dat_vencto_operac_financ  = h-frame:handle.
                END.
                IF h-frame:NAME = "cod_indic_econ_orig_apl" THEN DO:
                    
                    assign wh-moeda  = h-frame:handle.
                    
                END.
                IF h-frame:NAME = "cod_empresa" THEN DO:
                    
                    assign wh-empresa-cc  = h-frame:handle.
                    
                END.

                IF h-frame:NAME = "val_operac_financ" THEN DO:
                    
                    assign wh-valor-oper  = h-frame:handle.
                    
                END.
                /**/
            END.
            IF h-frame:TYPE = "button" THEN DO:
                
                IF h-frame:NAME = "bt_atz" THEN DO:
                    assign wh-bt_atz = h-frame:handle.
                END.
                
            END.
            assign h-frame = h-frame:NEXT-SIBLING. /* ver observaªío sobre o NEXT-SIBLING no fim deste exemplo */
        END.
        ELSE DO:
            LEAVE.
        END.
    END.
END.
