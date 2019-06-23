/*******************************************************************************
Extracao de Usuario Material
Kraft Consulting
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

DEFINE INPUT PARAMETER p-dt-inicial AS DATE NO-UNDO.
DEFINE INPUT PARAMETER p-dt-final AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pHandle AS HANDLE NO-UNDO.

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

DEFINE VARIABLE c-especie AS CHARACTER   NO-UNDO.

DEFINE VARIABLE dt-inicial AS DATE NO-UNDO.
DEFINE VARIABLE dt-final   AS DATE NO-UNDO.

DEF VAR dt-medio AS DATE NO-UNDO.

{utp/ut-glob.i}

def buffer ccusto for ems5.ccusto.

RUN pi-acompanhar IN pHandle(INPUT "Eliminando os registros de movimentos!") .

find first plano_ccusto no-lock
     where plano_ccusto.cod_empresa     = v_cod_empres_usuar
       and plano_ccusto.dat_inic_valid <= today
       and plano_ccusto.dat_fim_valid  >= today no-error.

FOR EACH estabelec NO-LOCK WHERE estabelec.ep-codigo = i-ep-codigo-usuario:
    DELETE FROM es_movimentacao_estoque
          WHERE es_movimentacao_estoque.codestabelecimento = estabelec.cod-estabel
            AND es_movimentacao_estoque.dt_trans          >= p-dt-inicial
            AND es_movimentacao_estoque.dt_trans          <= p-dt-final.
END.

ASSIGN c-especie = "ACA,ACT,NU1,DD,DEV,DIV,DRM,EAC,EGF,BEM,NU2,NU3,NU4,ICM,INV,IPL,MOB,NC,NF,NFD,NFE,NFS,NFT,NU5,REF,RCS,RDD,REQ,RFS,RM,RRQ,STR,TRA,ZZZ,SOB,EDD,VAR".

FOR EACH  movto-estoq NO-LOCK
    WHERE movto-estoq.dt-trans >= p-dt-inicial /*Lembrar de Colocar 30 dias*/
    AND   movto-estoq.dt-trans <= p-dt-final USE-INDEX data-conta:

    RUN pi-acompanhar IN pHandle(INPUT string(movto-estoq.nro-docto)) .

    FIND FIRST estabelec 
    WHERE estabelec.cod-estabel = movto-estoq.cod-estabel NO-LOCK NO-ERROR.

    IF NOT AVAIL estabelec OR estabelec.ep-codigo <> i-ep-codigo-usuario THEN NEXT.
    
    FIND FIRST item
    WHERE ITEM.it-codigo = movto-estoq.it-codigo USE-INDEX codigo NO-LOCK NO-ERROR.

    find first cta_ctbl no-lock
         where cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
           and cta_ctbl.cod_cta_ctbl       = movto-estoq.ct-codigo no-error.

    find first ccusto no-lock
         where ccusto.cod_empresa      = plano_ccusto.cod_empresa
           and ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
           and ccusto.cod_ccusto       = movto-estoq.sc-codigo no-error.

    FIND FIRST emitente
    WHERE emitente.cod-emitente = movto-estoq.cod-emitente NO-LOCK  NO-ERROR.
    
    FIND FIRST natur-oper 
    where natur-oper.nat-operacao = movto-estoq.nat-operacao NO-LOCK NO-ERROR.
    
    FIND FIRST grup-estoque WHERE 
    grup-estoque.ge-codigo = ITEM.ge-codigo NO-ERROR.

    FIND FIRST saldo-estoq 
        WHERE saldo-estoq.it-codigo   = movto-estoq.it-codigo
          AND saldo-estoq.cod-estabel = movto-estoq.cod-estabel NO-LOCK NO-ERROR.
    
    CREATE es_movimentacao_estoque.
    ASSIGN es_movimentacao_estoque.nr_trans              = movto-estoq.nr-trans
           es_movimentacao_estoque.dt_trans              = movto-estoq.dt-trans
           es_movimentacao_estoque.codITEMDeposito       = movto-estoq.it-codigo
           es_movimentacao_estoque.codESTABELECIMENTO    = movto-estoq.cod-estabel
           es_movimentacao_estoque.data                  = movto-estoq.dt-trans
           es_movimentacao_estoque.TIPOESPECIE           = entry(movto-estoq.esp-docto,c-especie).

    ASSIGN es_movimentacao_estoque.TIPOTRANSACAO         = {ininc/i01in218.i 04 movto-estoq.tipo-trans}
           es_movimentacao_estoque.nrDOCTO               = string(movto-estoq.nro-docto)
           es_movimentacao_estoque.LOTE                  = string(movto-estoq.lote)
           es_movimentacao_estoque.GRUPOESTOQ            = IF AVAIL grup-estoque THEN STRING(grup-estoque.ge-codigo) ELSE "N/INFORMADO" 
           es_movimentacao_estoque.DEPOSITO              = movto-estoq.cod-depos 
           es_movimentacao_estoque.CONTACONTABIL         = movto-estoq.conta-contabil 
           es_movimentacao_estoque.CONTA                 = IF AVAIL cta_ctbl THEN cta_ctbl.cod_cta_ctbl ELSE "N/INFORMADO" 
           es_movimentacao_estoque.SUBCONTA              = IF AVAIL ccusto THEN ccusto.cod_ccusto ELSE "N/INFORMADO" 
           es_movimentacao_estoque.ORDEMPRODUCAO         = (movto-estoq.numero-ordem)
           es_movimentacao_estoque.NATUROPER             = IF AVAIL natur-oper THEN STRING(movto-estoq.nat-operacao) ELSE "N/INFORMADO"
           es_movimentacao_estoque.codEmitente           = emitente.cod-emitente 
           es_movimentacao_estoque.QUANTIDADE            = movto-estoq.quantidade
           es_movimentacao_estoque.SaldoQuantidade       = saldo-estoq.qtidade-fin
           es_movimentacao_estoque.precoEntrada          = movto-estoq.valor-mat-m[1]
           es_movimentacao_estoque.cod_localiz           = movto-estoq.cod-localiz.
     
    ASSIGN dt-medio = DATE(MONTH(movto-estoq.dt-trans),1,YEAR(movto-estoq.dt-trans)) + 45
           dt-medio = DATE(MONTH(dt-medio),1,YEAR(dt-medio)) - 1.
    FOR LAST pr-it-per NO-LOCK
       WHERE pr-it-per.it-codigo = movto-estoq.it-codigo
         AND pr-it-per.cod-estab = movto-estoq.cod-estabel
         AND pr-it-per.periodo  <= dt-medio:
        ASSIGN es_movimentacao_estoque.precoMedio = pr-it-per.val-unit-mat-m[1] + pr-it-per.val-unit-ggf-m[1] + pr-it-per.val-unit-mob-m[1].
    END.
     
END. /*MOVTO-ESTOQ*/

/*codMoeda
tipoTransacao
entradaSaida
precoMedio
precoEntrada
saldoQuantidade*/                                                
