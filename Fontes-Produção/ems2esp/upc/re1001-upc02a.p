/*****************************************************************************
* Empresa  : DKP
* Cliente  : YAMANA
* Programa : upc/re1001-upc02a.p
* Descricao: Acionamento do Botao bt-geracao a partir do bt-geracao-esp  
* Autor    : Ramon - DKP
* Data     : 03/2018
*
******************************************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wh-object  AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wh-frame   AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.
     
DEF NEW GLOBAL SHARED VARIABLE wh-bt-geracao         AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE wh-bt-geracao-esp     AS WIDGET-HANDLE NO-UNDO.
 
DEF NEW GLOBAL SHARED VARIABLE wh-re1001-bt-ger-row  AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-brSon2          AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-handle-obj      AS WIDGET-HANDLE NO-UNDO. 
DEF NEW GLOBAL SHARED VARIABLE wh-container       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-nr-contrato-re1005 AS INTEGER NO-UNDO.

DEF BUFFER b-dupli-apagar FOR dupli-apagar.

DEF BUFFER b-ordem-compra FOR ordem-compra.
DEF BUFFER bf-docum-est FOR docum-est.
DEF BUFFER bf-item-doc-est FOR item-doc-est.
DEF BUFFER bf-dupli-apagar FOR dupli-apagar.

DEFINE VARIABLE c-especie  AS CHARACTER   NO-UNDO INITIAL "X".
DEFINE VARIABLE c-especie2 AS CHARACTER   NO-UNDO INITIAL "X".
DEFINE VARIABLE i-parcela  AS INTEGER     NO-UNDO.
DEF VAR d-pc-retencao      AS DECIMAL  FORMAT ">>9.99"           NO-UNDO.
DEFINE VARIABLE c-observ   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE d-totitem     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-totitem-me  AS DECIMAL     NO-UNDO.

/* para controlar o nr de parcelas j  que a pk da dupli-apagar ‚ uma merda!*/
DEF TEMP-TABLE tt-parcela
    FIELD row-dupli AS ROWID 
    INDEX idx0 IS PRIMARY UNIQUE row-dupli.
 
IF VALID-HANDLE(wh-bt-geracao) THEN DO:
       
  APPLY "choose" TO wh-bt-geracao.

/*   FIND es_parametros NO-LOCK                                                */
/*       WHERE es_parametros.cod_prog_dtsul = "re1001"                         */
/*       AND   es_parametros.cod_referencia = "especie_glosa"                  */
/*       AND   es_parametros.dat_valid_ini <= TODAY NO-ERROR.                  */
/*   ASSIGN c-especie2 = es_parametros.cod_parametro WHEN AVAIL es_parametros. */

  FIND es_parametros NO-LOCK 
      WHERE es_parametros.cod_prog_dtsul = "re1001" 
      AND   es_parametros.cod_referencia = "especie_retencao" 
      AND   es_parametros.dat_valid_ini <= TODAY NO-ERROR.
  ASSIGN c-especie = es_parametros.cod_parametro WHEN AVAIL es_parametros.
     
  FIND FIRST bf-docum-est 
         WHERE ROWID(bf-docum-est) = wh-re1001-bt-ger-row NO-LOCK NO-ERROR.
    IF AVAIL bf-docum-est THEN DO:

        ASSIGN d-totitem    = bf-docum-est.tot-valor
               d-totitem-me = bf-docum-est.tot-valor-me.    

         /* Verifica todas as parcelas para ADD - indice £nico da dupli-apagar nÆo cont‚m esp‚cie */
         FOR EACH bf-dupli-apagar NO-LOCK 
             WHERE bf-dupli-apagar.serie-docto  = bf-docum-est.serie-docto
               AND bf-dupli-apagar.nro-docto    = bf-docum-est.nro-docto   
               AND bf-dupli-apagar.cod-emitente = bf-docum-est.cod-emitente
               AND bf-dupli-apagar.nat-operacao = bf-docum-est.nat-operacao:

             IF bf-dupli-apagar.cod-esp = c-especie THEN NEXT.
    
             FIND tt-parcela WHERE
                  tt-parcela.row-dupli = ROWID(bf-dupli-apagar) NO-ERROR.
             IF NOT AVAIL tt-parcela THEN DO:

                 CREATE tt-parcela.
                 ASSIGN tt-parcela.row-dupli = ROWID(bf-dupli-apagar).
             END.
         END.

         /* POSICIONA REGISTRO DA £ltima DUPLICATA PARA POSTERIOR ACERTO DE VALORES */
         FIND LAST bf-dupli-apagar NO-LOCK 
                 WHERE bf-dupli-apagar.serie-docto  = bf-docum-est.serie-docto 
                   AND bf-dupli-apagar.nro-docto    = bf-docum-est.nro-docto   
                   AND bf-dupli-apagar.cod-emitente = bf-docum-est.cod-emitente
                   AND bf-dupli-apagar.nat-operacao = bf-docum-est.nat-operacao
                   AND bf-dupli-apagar.cod-esp      <> c-especie NO-ERROR.
          ASSIGN i-parcela = INT(bf-dupli-apagar.parcela) WHEN AVAIL bf-dupli-apagar.

        /* ROTINA PARA RETENCAO POR MEDICAO */
        FOR EACH bf-item-doc-est OF bf-docum-est NO-LOCK:
           
            FIND FIRST rat-ordem
                 WHERE rat-ordem.cod-emitente  = bf-item-doc-est.cod-emitente
                   AND rat-ordem.serie-docto   = bf-item-doc-est.serie-docto
                   AND rat-ordem.nro-docto     = bf-item-doc-est.nro-docto
                   AND rat-ordem.nat-operacao  = bf-item-doc-est.nat-operacao
                   AND rat-ordem.sequencia     = bf-item-doc-est.sequencia 
                       NO-LOCK NO-ERROR.
            IF AVAIL rat-ordem THEN DO:
                
                /* Verifica se nÆo houve a reten‡Æo do contrato - DPC */
                RUN pi-retencao-contrato (INPUT rat-ordem.numero-ordem).
               
                /* DPC 23/11/2018 - Ap¢s avalidar o processo de glosa em conjunto com Wanderson e Frederico, foi identificado que as glosas nÆo acontecem no recebimento fiscal e portanto, 
                    este processo nÆo ir  para a produ‡Æo, pois o mesmo poder  parar o recebimento fiscal*/

/*                 FIND FIRST medicao-contrat                                                                                      */
/*                      WHERE medicao-contrat.nr-contrato     = rat-ordem.nr-contrato                                              */
/*                        AND medicao-contrat.num-seq-item    = rat-ordem.num-seq-item                                             */
/*                        AND medicao-contrat.numero-ordem    = rat-ordem.numero-ordem                                             */
/*                        AND medicao-contrat.num-seq-event   = rat-ordem.num-seq-event                                            */
/*                        AND medicao-contrat.num-seq-medicao = rat-ordem.num-seq-medicao                                          */
/*                            NO-LOCK NO-ERROR.                                                                                    */
/*                                                                                                                                 */
/*                 IF AVAIL medicao-contrat THEN DO:                                                                               */
/*                                                                                                                                 */
/*                     FIND FIRST es-medicao-contrat OF medicao-contrat NO-LOCK NO-ERROR.                                          */
/*                     IF AVAIL es-medicao-contrat                                                                                 */
/*                          AND es-medicao-contrat.vl-glosa-desc > 0 THEN DO:                                                      */
/*                                                                                                                                 */
/*                           ASSIGN c-observ = "Glosa de contrato criada automaticamente. Contrato nr. "                           */
/*                                                                         + STRING(medicao-contrat.nr-contrato)                   */
/*                                                                         + " Seq. Medi‡Æo: "                                     */
/*                                                                         + STRING(medicao-contrat.num-seq-medicao)               */
/*                                                                         + " Vl.Medi‡Æo: "                                       */
/*                                                                         + STRING(medicao-contrat.val-medicao,">>>,>>>,>>9.99"). */
/*                                                                                                                                 */
/*                           IF NOT CAN-FIND(FIRST bf-dupli-apagar NO-LOCK                                                         */
/*                                 WHERE bf-dupli-apagar.serie-docto  = bf-docum-est.serie-docto                                   */
/*                                   AND bf-dupli-apagar.nro-docto    = bf-docum-est.nro-docto                                     */
/*                                   AND bf-dupli-apagar.cod-emitente = bf-docum-est.cod-emitente                                  */
/*                                   AND bf-dupli-apagar.nat-operacao = bf-docum-est.nat-operacao                                  */
/*                                   AND bf-dupli-apagar.cod-esp      = c-especie2)                                                */
/*                           THEN RUN pi-cria-duplicata (INPUT es-medicao-contrat.nr-contrato,                                     */
/*                                                       INPUT c-especie2,                                                         */
/*                                                       INPUT es-medicao-contrat.vl-glosa-desc,                                   */
/*                                                       INPUT c-observ).                                                          */
/*                     END.                                                                                                        */
/*                 END.                                                                                                            */
            END.
            ELSE RUN pi-retencao-contrato (INPUT bf-item-doc-est.numero-ordem).
        END.
    END.
    
    IF VALID-HANDLE(wh-container) 
        THEN RUN openQueriesSon IN wh-container.        

    RETURN "OK".
END.
 
/* fim - re1001-upc02a.p */

PROCEDURE pi-retencao-contrato:
    DEFINE INPUT  PARAMETER ipi-ordem-compra AS INTEGER     NO-UNDO.

    DEFINE VARIABLE l-prim-fat-contr AS LOGICAL     NO-UNDO.
    DEF VAR d-vl-retencao     AS DECIMAL  FORMAT ">>>,>>>,>>9.99"   NO-UNDO.
    DEFINE VARIABLE d-qtd     AS DECIMAL     NO-UNDO.

    FIND FIRST ordem-compra NO-LOCK 
         WHERE ordem-compra.numero-ordem = ipi-ordem-compra NO-ERROR.
    IF AVAIL ordem-compra THEN DO:

        /* Verifica se j  teve faturamento */
        FIND FIRST contrato-for NO-LOCK 
             WHERE contrato-for.nr-contrato = ordem-compra.nr-contrato NO-ERROR.
        IF AVAIL contrato-for THEN DO:

            FIND FIRST ext-contrato-for OF contrato-for NO-LOCK NO-ERROR.
            IF AVAIL ext-contrato-for AND ext-contrato-for.ind_tip_gar = 1 /* Reten‡Æo */ THEN DO:
            
                /*VERIFICAR SE  A PRIMEIRA FATURA DO CONTRATO*/
                ASSIGN d-qtd = 0.
                FOR EACH recebimento NO-LOCK OF ordem-compra:

                    ASSIGN d-qtd = d-qtd + recebimento.qtd-rec - recebimento.qtd-dev.
                END.
                    
                IF d-qtd > 0 THEN /* Houve 1 recebimento */
                    ASSIGN d-pc-retencao = ext-contrato-for.perc_2. /* NÆo ‚ o primeiro faturamento */
                ELSE ASSIGN d-pc-retencao = ext-contrato-for.perc_1. /* primeiro faturamento */

                ASSIGN d-vl-retencao = bf-docum-est.tot-valor * d-pc-retencao / 100
                       c-observ = "Reten‡Æo criada automaticamente. Contrato nr. "                                
                                                             + STRING(ext-contrato-for.nr-contrato)                        
                                                             + " Percentual Reten‡Æo: "                                          
                                                             + STRING(d-pc-retencao,">>9.99")
                                                             + "%"
                       i-nr-contrato-re1005  = ext-contrato-for.nr-contrato.


                RUN pi-cria-duplicata (INPUT ext-contrato-for.nr-contrato,
                                       INPUT c-especie, 
                                       INPUT d-vl-retencao, 
                                       INPUT c-observ).    
            END.    
        END.        
    END.  /* ordem-compra */
END. /* procedure */

PROCEDURE pi-cria-duplicata:
    DEFINE INPUT  PARAMETER ipi-nrContrat AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipc-esp     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipd-valor   AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER ipc-observ  AS CHARACTER   NO-UNDO.

    IF c-especie = "X" THEN DO:

      MESSAGE "NÆo encontrado parƒmetro para as esp‚cies de reten‡Æo!" SKIP 
              "NÆo ser  poss¡vel calcular as parcelas!" SKIP
              "Verifique o cadastro ESCD0007 para os parƒmetros:" SKIP
              "Esp‚cie Reten‡Æo: Programa re1001, Referˆncia 'especie_retencao' " SKIP
              "Esp‚cie Glosa: Programa re1001, Referˆncia 'especie_glosa' "
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

      RETURN "NOK".
    END.
   
    ASSIGN i-parcela = i-parcela + 1. 

    FOR EACH tt-parcela:

        FIND bf-dupli-apagar WHERE
             ROWID(bf-dupli-apagar) = tt-parcela.row-dupli NO-ERROR.

        ASSIGN i-parcela = i-parcela + 1.

        CREATE b-dupli-apagar.  
        BUFFER-COPY bf-dupli-apagar EXCEPT parcela cod-esp vl-a-pagar TO b-dupli-apagar
        ASSIGN b-dupli-apagar.parcela           = STRING(i-parcela,"99")
               b-dupli-apagar.dt-vencim         = 12/31/2500 /* A pedido do Cristiano a data de vencimento ser  uma data que nÆo vence */
               b-dupli-apagar.cod-esp           = ipc-esp
               b-dupli-apagar.vl-a-pagar        = bf-dupli-apagar.vl-a-pagar * d-pc-retencao / 100 /* Percentual de Reten‡Æo */
               b-dupli-apagar.Valor-a-pagar-me  = bf-dupli-apagar.Valor-a-pagar-me * d-pc-retencao / 100 /* Percentual de Reten‡Æo */
               b-dupli-apagar.observacao        = c-observ.

        ASSIGN bf-dupli-apagar.vl-a-pagar       = bf-dupli-apagar.vl-a-pagar - b-dupli-apagar.vl-a-pagar       
               bf-dupli-apagar.Valor-a-pagar-me = bf-dupli-apagar.Valor-a-pagar-me -     b-dupli-apagar.Valor-a-pagar-me.


        ASSIGN d-totitem    = d-totitem    - b-dupli-apagar.vl-a-pagar       
               d-totitem-me = d-totitem-me - b-dupli-apagar.Valor-a-pagar-me.    
                                                                              
        RELEASE bf-dupli-apagar.
        RELEASE b-dupli-apagar.

    END.



    /* L¢gica nÆo funciona qdo o pagamento ‚ parcelado 
    IF NOT CAN-FIND(FIRST dupli-apagar
                         WHERE dupli-apagar.serie-docto  = bf-docum-est.serie-docto
                           AND dupli-apagar.nro-docto    = bf-docum-est.nro-docto   
                           AND dupli-apagar.cod-emitente = bf-docum-est.cod-emitente
                           AND dupli-apagar.nat-operacao = bf-docum-est.nat-operacao
                           AND dupli-apagar.cod-esp      = ipc-esp) THEN DO:

        CREATE b-dupli-apagar.  
        BUFFER-COPY bf-dupli-apagar EXCEPT parcela cod-esp vl-a-pagar TO b-dupli-apagar
        ASSIGN b-dupli-apagar.parcela           = STRING(i-parcela,"99")
               b-dupli-apagar.dt-vencim         = 12/31/2500 /* A pedido do Cristiano a data de vencimento ser  uma data que nÆo vence */
               b-dupli-apagar.cod-esp           = ipc-esp
               b-dupli-apagar.vl-a-pagar        = ipd-valor
               b-dupli-apagar.Valor-a-pagar-me  = ipd-valor
               b-dupli-apagar.observacao        = c-observ.
    
        /*CORRIGIR VALOR DUPLICATA ORIGINAL*/
        FOR EACH b-dupli-apagar WHERE 
               ROWID(b-dupli-apagar) = ROWID(bf-dupli-apagar) NO-ERROR.
        IF AVAIL b-dupli-apagar THEN DO:
    
            ASSIGN b-dupli-apagar.vl-a-pagar       = d-totitem - ipd-valor
                   b-dupli-apagar.Valor-a-pagar-me = d-totitem-me - ipd-valor
                   d-totitem                       = d-totitem - ipd-valor
                   d-totitem-me                    = d-totitem-me - ipd-valor.
    
            RELEASE b-dupli-apagar.
    
        END.
    END.
    */

END.
