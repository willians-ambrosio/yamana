/*******************************************************************************
Extracao da tabela sl-it-per
Kraft Consulting
*******************************************************************************/
/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

DEFINE INPUT PARAMETER p-dt-inicial AS DATE NO-UNDO.
DEFINE INPUT PARAMETER p-dt-final   AS DATE NO-UNDO.
DEF INPUT PARAM h-acomp AS HANDLE NO-UNDO.

DEF VAR i AS i NO-UNDO.

/*
DEFINE VARIABLE p-dt-inicial AS DATE  NO-UNDO INIT TODAY.
DEFINE VARIABLE p-dt-final AS DATE    NO-UNDO INIT TODAY.
*/
/*
ASSIGN 
    p-dt-inicial = 07/04/2011 - 30
    p-dt-final = 07/04/2011.
*/


DEFINE VARIABLE dt-data AS DATE NO-UNDO.
DEFINE VARIABLE dt-periodo AS DATE NO-UNDO.

DEFINE TEMP-TABLE ttPeriodos 
    FIELD periodo AS DATE
    INDEX idx AS PRIMARY UNIQUE periodo .

DEFINE TEMP-TABLE ttItens
    FIELD cod-estabel AS CHAR
    FIELD cod-depos AS CHAR
    FIELD it-codigo AS CHAR
    INDEX idx AS PRIMARY UNIQUE cod-estabel cod-depos it-codigo.

/* acertando a data final */


FIND FIRST param-estoq NO-LOCK NO-ERROR.
/*
IF (MONTH(param-estoq.ult-fech-dia) = MONTH(p-dt-final)
   AND YEAR(param-estoq.ult-fech-dia) = YEAR(p-dt-final)) 
    OR (p-dt-final > param-estoq.ult-fech-dia) THEN DO:

    ASSIGN 
        p-dt-final = DATE(MONTH(param-estoq.ult-fech-dia), 1, YEAR(param-estoq.ult-fech-dia)) - 1.
END.

*/

/* preparar periodos a que devem ser processados */

RUN pi-inicializar IN h-acomp ("sl-it-per").
RUN pi-acompanhar  IN h-acomp ("1").

ASSIGN dt-data = p-dt-inicial.

DO WHILE dt-data <= p-dt-final:
    ASSIGN 
        dt-periodo = DATE(MONTH(dt-data), 1, YEAR(dt-data)) + 31 
        dt-periodo = DATE(MONTH(dt-periodo), 1, YEAR(dt-periodo)) - 1.

    FIND ttPeriodos
        WHERE ttPeriodos.periodo = dt-periodo NO-ERROR.

    IF NOT AVAIL ttPeriodos THEN DO:
        CREATE ttPeriodos.
        ASSIGN ttPeriodos.periodo = dt-periodo.
    END.

    ASSIGN dt-data = dt-data + 1.
END.

RUN pi-acompanhar IN h-acomp ("2").

/* preparando a lista de itens que devem ser atualizados */
FOR EACH sl-it-per:
    ASSIGN
       i = i + 1.
    IF i / 1000 = INT(i / 1000)
    THEN RUN pi-acompanhar IN h-acomp ("3 " 
                                     + STRING(i)
                                     + " " 
                                     + sl-it-per.it-codigo).
    FIND ttItens
        WHERE ttItens.cod-estabel = sl-it-per.cod-estabel
          AND ttItens.cod-depos = sl-it-per.cod-depos
          AND ttItens.it-codigo = sl-it-per.it-codigo NO-LOCK NO-ERROR.

    IF NOT AVAIL ttItens THEN DO:
        CREATE ttItens.
        ASSIGN 
            ttItens.cod-estabel = sl-it-per.cod-estabel
            ttItens.cod-depos = sl-it-per.cod-depos
            ttItens.it-codigo = sl-it-per.it-codigo.
    END.
END.

FOR EACH ttPeriodos:
    FOR EACH ttItens:
        ASSIGN
           i = i + 1.
        IF i / 1000 = INT(i / 1000)
        THEN RUN pi-acompanhar IN h-acomp ("4 "
                                         + STRING(i)
                                         + " "
                                         + STRING(ttPeriodos.periodo)
                                         + " "
                                         + ttItens.it-codigo).

        FIND sl-it-per  
            WHERE sl-it-per.it-codigo = ttItens.it-codigo
              AND sl-it-per.cod-estabel = ttItens.cod-estabel
              AND sl-it-per.cod-depos = ttItens.cod-depos
              AND sl-it-per.periodo = ttPeriodos.periodo NO-LOCK NO-ERROR.

        IF NOT AVAIL sl-it-per THEN
            FIND LAST sl-it-per  
                WHERE sl-it-per.it-codigo = ttItens.it-codigo
                  AND sl-it-per.cod-estabel = ttItens.cod-estabel
                  AND sl-it-per.cod-depos = ttItens.cod-depos
                  AND sl-it-per.periodo < ttPeriodos.periodo NO-LOCK NO-ERROR.

        IF NOT AVAIL sl-it-per THEN NEXT.

        FIND FIRST es_sl_it_per
             WHERE es_sl_it_per.cod_estabel = sl-it-per.cod-estabel
               AND es_sl_it_per.cod_depos   = sl-it-per.cod-depos
               AND es_sl_it_per.it_codigo   = sl-it-per.it-codigo
               AND es_sl_it_per.periodo     = ttPeriodos.periodo
             NO-ERROR.

        IF NOT AVAIL es_sl_it_per
        THEN DO:
           CREATE es_sl_it_per.
           ASSIGN
              es_sl_it_per.cod_estabel = sl-it-per.cod-estabel
              es_sl_it_per.cod_depos   = sl-it-per.cod-depos  
              es_sl_it_per.it_codigo   = sl-it-per.it-codigo  
              es_sl_it_per.periodo     = ttPeriodos.periodo    .
        END.

        ASSIGN
           es_sl_it_per.quantidade     = sl-it-per.quantidade
           es_sl_it_per.val_unit_mat_m = sl-it-per.val-unit-mat-m[1].
    END.
END.

RUN pi-inicializar IN h-acomp ("").

/*RUN D:\DadosBI\Extratores\disconnect.p.*/




/* backup clebertom 23/09/2011

DEFINE VARIABLE c-arquivo AS CHARACTER NO-UNDO.

FOR EACH sl-it-per
   WHERE sl-it-per.periodo >= p-dt-inicial
     AND sl-it-per.periodo <= p-dt-final:
   RUN pi-acompanhar IN h-acomp (STRING(sl-it-per.periodo)).
   FIND FIRST es_sl_it_per
        WHERE es_sl_it_per.cod_estabel = sl-it-per.cod-estabel
          AND es_sl_it_per.cod_depos   = sl-it-per.cod-depos
          AND es_sl_it_per.it_codigo   = sl-it-per.it-codigo
          AND es_sl_it_per.periodo     = sl-it-per.periodo
        NO-ERROR.
   IF NOT AVAIL es_sl_it_per
   THEN DO:
      CREATE es_sl_it_per.
      ASSIGN
         es_sl_it_per.cod_estabel = sl-it-per.cod-estabel
         es_sl_it_per.cod_depos   = sl-it-per.cod-depos  
         es_sl_it_per.it_codigo   = sl-it-per.it-codigo  
         es_sl_it_per.periodo     = sl-it-per.periodo    .
   END.
   ASSIGN
      es_sl_it_per.quantidade     = sl-it-per.quantidade
      es_sl_it_per.val_unit_mat_m = sl-it-per.val-unit-mat-m[1].
END.
*/
