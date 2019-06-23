/****************************************************************************************** 
** 	   Programa: tw-dupli-apagar.p
**   	      Autor: 
**   	 Fornecedor:
**    	 Data:
** Change/Chamado:
**    Objetivo: 
**
******************************** CONTROLE DE ALTERA°†ES *********************************
** 
** Data         Autor   		Fornecedor  Change/Chamado Descri»’o da Altera»’o
** 19/11/2018   Vando Ribeiro   Grupo DKP   REQ02/Mudar o vencto da duplicata para a 5Ý 
                                                  seguinte ¹ data de vencto informada.
**
****************************** INFORMA°†ES ADICIONAIS ************************************
** PARôMETROS DE ENTRADA:new-dupli-apagar e old-dupli-apagar
** PARôMETROS DE SA™DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

/****************************************************************************
  Trigger de Write p/ a tabela dupli-apagar do EMS
*****************************************************************************/  
DEF PARAM BUFFER new-dupli-apagar FOR dupli-apagar.
DEF PARAM BUFFER old-dupli-apagar FOR dupli-apagar.

DEF BUFFER b-dupli-apagar FOR dupli-apagar.

DEFINE VARIABLE l-atu-esp        AS LOGICAL NO-UNDO.
DEFINE VARIABLE dt-vencto        AS DATE    NO-UNDO.
DEFINE VARIABLE dt-vencto-quinta AS DATE    NO-UNDO.

{utp/ut-glob.i}

/*---
- localizar docum-est of dulicatas
- localizar itens da duplicata
- localizar pedidos do item
- verificar cp do item
- alterar especie
---*/

ASSIGN l-atu-esp = FALSE.
FIND FIRST docum-est OF new-dupli-apagar NO-LOCK NO-ERROR.
IF AVAIL docum-est THEN DO:
   FOR EACH item-doc-est OF docum-est NO-LOCK:
       FIND FIRST pedido-compr 
           WHERE pedido-compr.num-pedido = item-doc-est.num-pedido NO-LOCK NO-ERROR.
       IF AVAIL pedido-compr THEN DO:
           IF pedido-compr.cod-cond-pag = 174 THEN
              ASSIGN l-atu-esp = TRUE.
       END.
       ELSE DO:
           FIND FIRST rat-ordem NO-LOCK
                WHERE rat-ordem.serie-docto  = item-doc-est.serie-docto
                  AND rat-ordem.nro-docto    = item-doc-est.nro-docto
                  AND rat-ordem.cod-emitente = item-doc-est.cod-emitente
                  AND rat-ordem.nat-operacao = item-doc-est.nat-operacao
                  AND rat-ordem.sequencia    = item-doc-est.sequencia NO-ERROR.

           FIND FIRST pedido-compr 
               WHERE pedido-compr.num-pedido = rat-ordem.num-pedido NO-LOCK NO-ERROR.
           IF AVAIL pedido-compr THEN DO:
               IF pedido-compr.cod-cond-pag = 174 THEN
                  ASSIGN l-atu-esp = TRUE.
           END.
       END.
   END.
END.

IF l-atu-esp THEN DO:
   FIND FIRST b-dupli-apagar WHERE
        ROWID(b-dupli-apagar) = ROWID(new-dupli-apagar) NO-ERROR.
   IF AVAIL b-dupli-apagar THEN
      ASSIGN b-dupli-apagar.cod-esp = 'DC'.
END.

/*Vando Ribeiro*/
FIND FIRST es-fornec-ap NO-LOCK /*n’o considerea fornec prioritÿrio*/
     WHERE es-fornec-ap.cod-emitente = new-dupli-apagar.cod-emitente NO-ERROR.
IF NOT AVAIL es-fornec-ap AND NEW new-dupli-apagar THEN
DO:
    FIND CURRENT new-dupli-apagar EXCLUSIVE-LOCK NO-ERROR.
    RUN pi-vencto (INPUT  new-dupli-apagar.dt-vencim,
                   OUTPUT dt-vencto-quinta).
    
    IF new-dupli-apagar.dt-vencim <> dt-vencto-quinta THEN
    
    RUN utp/ut-msgs.p (INPUT "show":U, input 27979, 
                       INPUT "Vencimento da Duplicata Alterado~~Vencimento da Duplicata Alterado de " + STRING(new-dupli-apagar.dt-vencim, "99/99/9999") + " para " + STRING(dt-vencto-quinta, "99/99/9999") + ".").
    
    ASSIGN new-dupli-apagar.dt-vencim = dt-vencto-quinta.
    FIND CURRENT new-dupli-apagar NO-LOCK NO-ERROR.
END.

PROCEDURE pi-vencto:
    DEFINE INPUT PARAMETER  p-dt-vencto         AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER p-dt-vencto-quinta  AS DATE NO-UNDO.

    DEFINE VARIABLE l-semana      AS LOGICAL NO-UNDO.
    DEFINE VARIABLE i-dia         AS INTEGER NO-UNDO.
    DEFINE VARIABLE dt-semana     AS DATE        NO-UNDO.

    ASSIGN
        l-semana  = YES
        dt-semana = p-dt-vencto
        i-dia     = WEEKDAY(p-dt-vencto).
    
    IF p-dt-vencto > TODAY THEN
    DO:
        IF i-dia = 5 THEN
        DO:
            IF p-dt-vencto - TODAY >= 7 THEN
            DO:
                p-dt-vencto-quinta = p-dt-vencto. /*quinta mantem*/
                RETURN "OK".
            END.
        END.
    
        IF i-dia = 6 THEN
        DO:
            IF (p-dt-vencto - 1) - TODAY >= 7 THEN
            DO:
                p-dt-vencto-quinta = p-dt-vencto - 1. /*sexta volta para quinta*/
                RETURN "OK".
            END.
        END.
    
        DO WHILE l-semana:
            ASSIGN
                dt-semana = dt-semana + 1
                i-dia     = WEEKDAY(dt-semana).
    
            IF i-dia = 5 THEN 
            DO:
                IF dt-semana - TODAY >= 7 THEN
                ASSIGN
                    l-semana           = NO
                    p-dt-vencto-quinta = dt-semana. /*qqer outro dia vai para quinta*/
            END.
        END.
    END.
    ELSE
    DO: /*vencidos*/
        dt-semana = TODAY.
        DO WHILE l-semana:
            ASSIGN
                dt-semana = dt-semana + 1
                i-dia     = WEEKDAY(dt-semana).
    
            IF dt-semana - TODAY >= 7 THEN
            DO:
                IF i-dia = 5 THEN 
                    ASSIGN
                        l-semana           = NO
                        p-dt-vencto-quinta = dt-semana. /*vai pra proxima quinta respeitando o minimo 7 dias*/
            END.
        END.
    END.

    RETURN "OK".
END.
