/*******************************************************************************
** Definiá∆o das temp-tables de comunicaá∆o para integraá∆o de contratos
**
** Se esta include for definida dentro de um DBO ou programa thin-template,
** n∆o dever† declarar os mÇtodos e vari†veis padr‰es de tratamento de erro
** das DBOs. Neste caso, passar como parÉmetro "1" para esta include.
*******************************************************************************/

&IF '{&bf_mat_versao_ems}' >= '2.04' &THEN

    DEF TEMP-TABLE tt-integra-cn NO-UNDO
        FIELD cod-movto         AS INT    /* 1-Inclus∆o 2-Alteraá∆o 3-Eliminaá∆o  */
        FIELD prog-origem       AS CHAR   /* Programa Origem                      */
        FIELD des-ponto         AS CHAR   /* Ponto do programa, opcional          */
        FIELD cod-tabela        AS CHAR   /* Tabela                               */
        FIELD r-rowid           AS ROWID  /* Rowid da tabela                      */  
        FIELD conteudo-inicial  as RAW    /* Conte£do tabela antes das alteraá‰es */
        FIELD conteudo-final    AS RAW    /* Conte£do tabela ap¢s alteraá‰es      */
        FIELD seq               AS INT    /* Sequencia gerada automaticamente     */
        INDEX tt-integra-cn seq.

    DEF VAR i-seq-tt-integra-cn AS INT NO-UNDO.
    DEF VAR raw-tt-integra-cn   AS RAW NO-UNDO.

    DEF BUFFER b-tt-integra-cn FOR tt-integra-cn.

    DEF TEMP-TABLE tt-integra-cn-ordem-compra     NO-UNDO LIKE ordem-compra
        FIELD xcod-movto AS INT
        FIELD xseq       AS INT
    INDEX tt-integra-cn-ordem-compra xseq.

    DEF TEMP-TABLE tt-integra-cn-prazo-compra     NO-UNDO LIKE prazo-compra
        FIELD xcod-movto AS INT
        FIELD xseq       AS INT
    INDEX tt-integra-cn-ordem-compra xseq.

    DEF TEMP-TABLE tt-integra-cn-medicao-contrat  NO-UNDO LIKE medicao-contrat
        FIELD xcod-movto AS INT
        FIELD xseq       AS INT
    INDEX tt-integra-cn-medicao-contrat xseq.

    DEF TEMP-TABLE tt-integra-cn-matriz-rat-contr NO-UNDO LIKE matriz-rat-contr
        FIELD xcod-movto AS INT
        FIELD xseq       AS INT
    INDEX tt-integra-cn-matriz-rat-contr xseq.

    DEF TEMP-TABLE tt-integra-cn-matriz-rat-item  NO-UNDO LIKE matriz-rat-item
        FIELD xcod-movto AS INT
        FIELD xseq       AS INT
    INDEX tt-integra-cn-matriz-rat-item xseq.

    DEF TEMP-TABLE tt-integra-cn-matriz-rat-med   NO-UNDO LIKE matriz-rat-med
        FIELD xcod-movto AS INT
        FIELD xseq       AS INT
    INDEX tt-integra-cn-matriz-rat-med xseq.

    DEF VAR l-erro-integra-cn AS LOG NO-UNDO.

&ENDIF

