/*******************************************************************************
Extracao de Centro de Custos
Kraft Consulting
29/12/2010
*******************************************************************************/

{esp\KRAFT.I}

DEF INPUT PARAM p-dt-inicial AS DATE NO-UNDO.
DEF INPUT PARAM p-dt-final AS DATE NO-UNDO.
DEF INPUT PARAM p-handle AS HANDLE NO-UNDO.

DEF VAR dt-inicial AS DATE NO-UNDO.
DEF VAR dt-final AS DATE NO-UNDO.

/*DEFINE VARIABLE i-nr-dias AS INTEGER     NO-UNDO INIT 180*/.
DEF VAR c-arquivo AS CHAR NO-UNDO.
DEF VAR c-nome-emp AS CHAR NO-UNDO. 

FOR EACH contrato-for NO-LOCK 
   WHERE contrato-for.dt-contrato >= 01/01/2003 /*p-dt-inicial - Modificado em 14/08/12 por Daniel - Fiz esta modifica‡Æo pois os contratos devem sempre ser atualizados(todos)*/
     AND contrato-for.dt-contrato <= p-dt-final:
    RUN pi-acompanhar IN p-handle (INPUT "Contrato " + STRING(contrato-for.nr-contrato)).

    ASSIGN
        dt-inicial = contrato-for.dt-ini-validade
        dt-final = contrato-for.dt-ter-validade.

    IF dt-inicial = ? OR dt-inicial < 01/01/2003 THEN
        ASSIGN dt-inicial = 01/01/2003.

    IF dt-final = ? OR dt-final >= 12/31/2050 THEN
        ASSIGN dt-final = 12/31/2050.

    FIND FIRST contrato-for-ext NO-LOCK
         WHERE contrato-for-ext.nr-contrato = contrato-for.nr-contrato  NO-ERROR.

    FIND FIRST es_contrato_for
        WHERE es_contrato_for.codEstabelecimento = contrato-for.cod-estabel
          AND es_contrato_for.numeroContrato     = contrato-for.nr-contrato NO-ERROR.

    IF NOT AVAIL es_contrato_for THEN DO:
        CREATE es_contrato_for.
        ASSIGN es_contrato_for.NumeroContrato = contrato-for.nr-contrato.   /*chave primaria e unica*/ 
    END.
    
    FIND FIRST estabelec WHERE estabelec.cod-estabel = contrato-for.cod-estabel NO-LOCK NO-ERROR.

    IF estabelec.ep-codigo = "1"   THEN ASSIGN c-nome-emp = 'MFB'.
    IF estabelec.ep-codigo = "121" THEN ASSIGN c-nome-emp = 'MBC'.
    IF estabelec.ep-codigo = "201" THEN ASSIGN c-nome-emp = 'YDM'.
    IF estabelec.ep-codigo = "211" THEN ASSIGN c-nome-emp = 'STE'.
    IF estabelec.ep-codigo = "221" THEN ASSIGN c-nome-emp = 'CGO'.
    IF estabelec.ep-codigo = "291" THEN ASSIGN c-nome-emp = 'YDS'.
    IF estabelec.ep-codigo = "301" THEN ASSIGN c-nome-emp = 'MRC'.
    IF estabelec.ep-codigo = "800" THEN ASSIGN c-nome-emp = 'JMC'.

    ASSIGN es_contrato_for.codComprador         = contrato-for.cod-comprado   
           es_contrato_for.codEmitente          = contrato-for.cod-emitente    
           es_contrato_for.codEstabelecimento   = contrato-for.cod-estabel    
           es_contrato_for.DataInicio           = dt-inicial
           es_contrato_for.DataFim              = dt-final
           es_contrato_for.LimiterValor         = contrato-for.dec-2
           es_contrato_for.Natureza             = {ininc/i01in274.i 04 contrato-for.natureza}
           es_contrato_for.ativo                = IF contrato-for.log-libera = YES THEN 1 ELSE 2
           es_contrato_for.codUsuarioGestor     = contrato-for.gestor-tecnico
           es_contrato_for.Narrativa            = contrato-for.narrat-contrat
           es_contrato_for.codOrdemInvestimento = IF AVAIL contrato-for-ext THEN contrato-for-ext.num-ord-inv ELSE contrato-for.num-ord-inv
           es_contrato_for.Situacao             = {ininc/i05in065.i 04 contrato-for.ind-sit-contrat} 
           es_contrato_for.ValorMovimento       = contrato-for.sld-val-receb /*contrato-for.val-total modificado por:Daniel solicitante: Ronaldo Jubilato data:05/07/2012*/
           es_contrato_for.inv_cust             = STRING( CAN-FIND ( FIRST es-contrato-for NO-LOCK OF contrato-for
                                                                     WHERE es-contrato-for.inv-cust ),
                                                          "Investimento/Custeio" )
           es_contrato_for.id_erp               = c-nome-emp + contrato-for.cod-estabel + STRING(contrato-for.nr-contrato). /*modificado por:Daniel solicitante: Walisson data:09/04/2014*/
END. /* FOR EACH contrato-for */
