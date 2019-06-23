/****************************************************************************************** 
** 	   Programa: dolar-medio.p
**   	  Autor: 
** 	 Fornecedor: DKP
**         Data: 21/12/2017
** Change/Chamado: apya510rp.p
**      Objetivo: C lculo de dolar m‚dio para relat¢rio Intercompany
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/    
    
DEFINE INPUT PARAMETER dt-ini AS DATE NO-UNDO.
DEFINE INPUT PARAMETER dt-fim AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER p-dolar-med AS DEC.

DEFINE VARIABLE d-medio AS DECIMAL   NO-UNDO.
DEFINE VARIABLE d-medio-f AS DECIMAL NO-UNDO.

DEFINE VARIABLE i-mes  AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-cont-per AS INTEGER NO-UNDO.


DEF BUFFER cotacao FOR ems2cadme.cotacao. 

DO i-mes = 1 TO 12:

    IF YEAR(dt-ini) <> YEAR(dt-fim) THEN DO:
        
        IF i-mes >= MONTH(dt-ini) AND i-mes <= 12 THEN DO:

            RUN dolar-medio(INPUT string(YEAR(dt-ini)) + STRING(i-mes,"99"),
                           OUTPUT d-medio).
            ASSIGN d-medio-f = d-medio-f + d-medio
                   i-cont-per = i-cont-per + 1.

           
        END.
         IF i-mes >= 01 AND i-mes <= MONTH(dt-fim) THEN DO:
          
            RUN dolar-medio(INPUT string(YEAR(dt-fim)) + STRING(i-mes,"99"),
                           OUTPUT d-medio).
              ASSIGN d-medio-f = d-medio-f + d-medio
                   i-cont-per = i-cont-per + 1.
           
        END.

    END.
    ELSE DO:
       
         IF i-mes >= MONTH(dt-ini) AND i-mes <= MONTH(dt-fim) THEN DO:

            RUN dolar-medio(INPUT string(YEAR(dt-ini)) + STRING(i-mes,"99"),
                           OUTPUT d-medio).
              ASSIGN d-medio-f = d-medio-f + d-medio
                   i-cont-per = i-cont-per + 1.
        END.
    END.
END.

ASSIGN p-dolar-med = d-medio-f / i-cont-per.


PROCEDURE dolar-medio:
    DEF INPUT PARAMETER p-periodo AS CHAR.
    DEF OUTPUT PARAMETER p-medio AS DECIMAL    NO-UNDO.
    
    DEFINE VARIABLE i-dia AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.
    
    FOR EACH cotacao NO-LOCK WHERE cotacao.mo-codigo = 1
                              AND cotacao.ano-periodo = p-periodo:
    
        DO i-dia = 1 TO 31:
            
            IF cotacao.cotacao[i-dia] > 0 THEN DO:
                
                ASSIGN p-medio = p-medio + cotacao.cotacao[i-dia]
                        i-cont = i-cont + 1.
    
            END.
    
        END.
        ASSIGN p-medio = p-medio / i-cont.
    
    END.
END.

