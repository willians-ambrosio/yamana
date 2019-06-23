/**********************************************************************************
**    Programa....: epc-ce0416rp.p
**    Objetivo....: Transfere os Valores da Especie de GGF 'NO-CASH' (CS0101) para 
**                  para uma Tabela de MOB (CD0281) correspondente.
**    Data........: Novembro/2007
**    Responsavel.: Diogo Cezar Amaral
**    E-Mail......: diogo.amaral@datasul.com.br
**********************************************************************************/

/* Include i-epc200.i: Definicao Temp-Table tt-epc. */
/***************************************************************
**
** I-EPC200.I1 - Padroniza a temp-table usada para os epcs
**
***************************************************************/ 

/* begin_temp_table_definition */

define temp-table tt-epc no-undo
   field cod-event     as char format "x(12)"
   field cod-parameter as char format "x(32)"
   field val-parameter as char format "x(54)"
   index  id is primary cod-parameter cod-event ascending.    
   
/* end_temp_table_definition */   
 

/* Definicao de Variaveis. */
DEFINE INPUT        PARAMETER p-ind-event   AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE         FOR tt-epc.
DEFINE              VARIABLE  c-cc-codigo   AS CHARACTER NO-UNDO.
DEFINE              VARIABLE  i-moeda       AS INTEGER   NO-UNDO.
DEFINE              VARIABLE  i-moeda-estoq AS INTEGER   NO-UNDO.
DEFINE              VARIABLE  de-aux        AS DECIMAL   NO-UNDO.

/* Definicao de Temp-Table para busca dos valores do Centro de Custo. */
DEFINE TEMP-TABLE tt-centro-custo 
    FIELDS cc-codigo    LIKE centro-custo.cc-codigo
    FIELDS horas-report LIKE centro-custo.horas-report
    FIELDS periodo      LIKE ext-per-custo.periodo.

/* Rotina para buscar as informacoes retornadas pelo ponto EPC do programa. */
FOR EACH tt-epc NO-LOCK
    WHERE tt-epc.cod-event = p-ind-event :

    /* Rowid do Centro de Custo. */
    IF tt-epc.cod-parameter = "CCUSTO-ROWID":U THEN DO :
        /* Cria registro apenas se existir um Rowid de Centro de Custo. */
        CREATE tt-centro-custo.

        FIND FIRST centro-custo 
            WHERE ROWID(centro-custo) = TO-ROWID(tt-epc.val-parameter) 
            NO-LOCK NO-ERROR.
        IF AVAIL centro-custo THEN
            ASSIGN tt-centro-custo.cc-codigo = centro-custo.cc-codigo.
    END.                     

    /* Total de Horas Reportadas para o Centro de Custo atual. */
    IF tt-epc.cod-parameter = "HORAS-REP-CC":U THEN DO :
        IF AVAIL tt-centro-custo THEN
            ASSIGN tt-centro-custo.horas-report = DECIMAL(tt-epc.val-parameter).
    END.
    
    /* Periodo atual. */
    IF tt-epc.cod-parameter = "PERIODO":U THEN DO :
        IF AVAIL tt-centro-custo THEN
            ASSIGN tt-centro-custo.periodo = tt-epc.val-parameter.
    END.

END.

/* Busca os Parametro de Estoque e Custos. */
FIND FIRST param-estoq NO-LOCK NO-ERROR.
FIND FIRST param-cs    NO-LOCK NO-ERROR.

/* Rotina de Migracao de Valores de NO-CASH para a Tabela de MOB. */
FOR EACH tt-centro-custo.
    /* Encontra o Valor definido no Centro de Custo para o periodo atual - CS0101. */
    FIND FIRST ext-per-custo 
        WHERE ext-per-custo.cc-codigo = tt-centro-custo.cc-codigo
        AND   ext-per-custo.periodo   = tt-centro-custo.periodo
        EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL ext-per-custo THEN DO :

        ASSIGN c-cc-codigo = SUBSTRING(ext-per-custo.cc-codigo,3).
             /*
              MESSAGE "c-cc-codigo"  c-cc-codigo "-" "SUBSTRING(ext-per-custo.cc-codigo,3)"  SUBSTRING(ext-per-custo.cc-codigo,3) SKIP
                      "ext-per-custo.cc-codigo" ext-per-custo.cc-codigo
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
               */
        /* Busca Tabela de MOB correspondente ao Centro de Custo. */
        FIND FIRST tab-mob-dir 
            WHERE SUBSTRING(tab-mob-dir.descricao,1,LENGTH(c-cc-codigo)) = c-cc-codigo 
            EXCLUSIVE-LOCK NO-ERROR.
                 /*
            MESSAGE  "SUBSTRING(tab-mob-dir.descricao,1,LENGTH(c-cc-codigo)) - " SUBSTRING(tab-mob-dir.descricao,1,LENGTH(c-cc-codigo)) SKIP
                     " c-cc-codigo -"  c-cc-codigo  "tab-mob-dir.cd-mob-dir" tab-mob-dir.cd-mob-dir
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                */
        /* Caso existo Tabela de MOB e o valor de NO-CASH seja maior que 0 (zero) atualiza CD0281 
           Obs. Essa validacao e necessaria pois o usuario pode executar o CE0416 mais de uma vez , 
           onde os custo devem ser atualizados apenas na primeira execucao, pois nas proxima 
           o custo de NO-CASH ficara igual a 0 (zero). */
        IF  AVAIL tab-mob-dir                 AND 
           (ext-per-custo.custo-total[2] > 0) AND
           (tt-centro-custo.horas-report > 0) THEN DO :
            /* Transforma o Valor de NO-CASH do Centro de Custo em um valor Unitario depois zera o mesmo. */
            ASSIGN tab-mob-dir.vl-corrente[1]   = (ext-per-custo.custo-total[2] / tt-centro-custo.horas-report)
                   ext-per-custo.custo-total[2] = 0.
           /*
            MESSAGE "achou  "
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
             */
            /* Rotina para efetuar a Cotacao das Moedas Alternativas. */
            DO i-moeda = 2 TO 3 :
                IF i-moeda = 2 AND param-estoq.tem-moeda1 THEN 
                    ASSIGN i-moeda-estoq = param-estoq.moeda1.
                ELSE 
                    IF i-moeda = 3 AND param-estoq.tem-moeda2 THEN 
                        ASSIGN i-moeda-estoq = param-estoq.moeda2.
                    ELSE
                        NEXT.
                
                RUN cdp/cd0812.p (INPUT  0 ,
                                  INPUT  i-moeda-estoq ,
                                  INPUT  tab-mob-dir.vl-corrente[1] ,
                                  INPUT  tab-mob-dir.dt-atualiz-cor ,
                                  OUTPUT de-aux).

                CASE i-moeda :
                   WHEN 2 THEN 
                      IF de-aux <> ? THEN 
                          ASSIGN tab-mob-dir.vl-corrente[2] = de-aux.
                   WHEN 3 THEN
                      IF de-aux <> ? THEN 
                          ASSIGN tab-mob-dir.vl-corrente[3] = de-aux.
                END. 
            END.
        END.
    END.    
END.

    
