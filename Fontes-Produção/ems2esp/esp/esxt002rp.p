/*------------------------------------------------------------------------
    File        : 
    Purpose     :
    Syntax      :
    Description :
    Author(s)   :  
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{include/i-prgvrs.i esxt002rp 2.06.00.000} 


/***********************************************************************
                             TEMP-TABLES
 ***********************************************************************/
/* Temporary Table Definitions ---                                      */

/*MESSAGE "Programa em Teste 11.01.01" VIEW-AS ALERT-BOX.*/


define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD diretorio AS CHAR
    FIELD c-ini-empresa AS CHAR
    FIELD c-ini-estabelec AS CHAR
    FIELD c-fim-estabelec AS CHAR
    FIELD c-ini-periodo AS DATE 
    FIELD c-fim-periodo AS DATE 
    FIELD c-ini-item AS CHAR
    FIELD c-fim-item AS CHAR
    FIELD itemmaster AS LOG
    FIELD entrega AS LOG
    FIELD movto-ent-sai AS LOG
    FIELD prazo-compra AS LOG
    FIELD ordem AS LOG
    FIELD requisicao AS LOG
    FIELD donotexportitems AS LOG
    FIELD formato AS INT INITIAL 2
    FIELD cb-tp-item-ini AS CHAR
    FIELD cb-tp-item-fim AS CHAR
    FIELD c-ini-familia AS CHAR
    FIELD c-fim-familia AS CHAR
    FIELD l-aca         AS log
    field L-DIV         as log
    field L-NU2         as log
    field L-IPL         as log
    field L-NFE         as log
    field L-RCS         as log
    field L-RRQ         as log
    field l-act         as log
    field L-DRM         as log
    field L-NU3         as log
    field L-MOB         as log
    field L-NFS         as log
    field L-RDD         as log
    field L-STR         as log
    field L-NU1         as log
    field L-EAC         as log
    field L-NU4         as log
    field L-NC          as log
    field L-NFT         as log
    field L-REQ         as log
    field L-TRA         as log
    field L-DD          as log
    field L-EGF         as log
    field L-ICM         as log
    field L-NF          as log
    field L-NUS         as log
    field L-RFS         as log
    field L-ZZZ         as log
    field L-DEV         as log
    field L-BEM         as log
    field L-INV         as log
    field L-NFD         as log
    field L-REF         as log
    field L-RM          as log
    field L-SOB         as log.

  
DEFINE TEMP-TABLE tt-digita NO-UNDO 
    field cod-depos        LIKE deposito.cod-depos
    field nome             LIKE deposito.nome
    field selec            AS LOG 
    index id cod-depos.

DEFINE BUFFER b-tt-digita for tt-digita.

DEF TEMP-TABLE tt-esp-docto NO-UNDO
    FIELD esp-docto LIKE movto-estoq.esp-docto
    FIELD tipo-trans  LIKE movto-estoq.tipo-trans
    FIELD tipo AS CHAR
    INDEX chave IS UNIQUE PRIMARY
        esp-docto
        tipo-trans.


DEF TEMP-TABLE tt-item-estab NO-UNDO
    FIELD it-codigo         AS CHAR FORMAT "x(16)" /* itemnum */
    FIELD cod-estabel       AS CHAR /* storageloc */
    FIELD localizacao       AS CHAR /* brancos */
    FIELD custo-unit        AS CHAR /*DEC*/  /* AvgUnitCost */
    FIELD qt-estoque        AS CHAR /*DEC*/  /* qtyOH */
    FIELD qt-estoque1       AS DEC  /* qtyOH */
    FIELD Lead-time         AS CHAR /*DEC*/  /* LTDays */
    FIELD Sazonalidade      AS LOG  /* Seasonal */
    FIELD ponto-encomenda   AS CHAR /*DEC*/  /* OP */
    FIELD qt-ordem          AS CHAR /*DEC*/  /* OQ */
    FIELD qt-minimo         AS CHAR /*DEC*/  /* Min */
    FIELD qt-maximo         AS CHAR /*DEC*/  /* Max */
    FIELD donotexport       AS LOG  /* donotexport */
    FIELD descricao         AS CHAR FORMAT "x(60)" /* description */
    FIELD criticidade       AS CHAR /* CriticalityLevel */
    FIELD critic-inventario AS CHAR /*DEC*/  /*SetMinToOrAbove*/
    FIELD item-estoque      AS LOG  /* StockItem */
    FIELD fator-conversao   AS CHAR /*DEC*/  /* PurchaseToIssueConvFactor */
    FIELD data-criacao      AS CHAR FORMAT "x(10)" /*date*/ /* creationDate */
    FIELD indicar-estoque   AS LOG  /* StockIndicator */
    FIELD fornecedor        AS char /* PrimaryVendor */
    FIELD departamento      AS char /* campo novo */
    FIELD consumo-prev      AS CHAR /*DEC*/  /* PC */
    FIELD deposito          LIKE deposito.cod-depos /**** MOD.01: RODRIGO BAIONE ****/
    FIELD demanda           AS CHAR
    INDEX chave IS UNIQUE PRIMARY
        it-codigo
        cod-estabel.

DEF TEMP-TABLE tt-movto-ent-sai NO-UNDO
    FIELD it-codigo AS CHAR FORMAT "x(16)"
    FIELD cod-estabel LIKE estabelec.cod-estabel
    FIELD data-ult-ent AS CHAR FORMAT "X(10)"
    FIELD data-ult-sai AS CHAR FORMAT "X(10)"
    INDEX chave IS UNIQUE PRIMARY
          it-codigo
          cod-estabel.        


DEF TEMP-TABLE tt-entrega NO-UNDO 
    FIELD it-codigo         AS CHAR FORMAT "x(16)"
    FIELD cod-estabel       AS CHAR /* storageloc */
    FIELD data-entrada      AS CHAR FORMAT "x(10)" /*DATE*/ 
    FIELD qt-entrada        AS CHAR /*DEC*/ 
    FIELD tipo-entrada      AS CHAR
    FIELD tipo-trans        AS CHAR FORMAT "x(4)"
    INDEX chave IS PRIMARY
        it-codigo
        cod-estabel
        data-entrada.

DEF TEMP-TABLE tt-ordem NO-UNDO 
    FIELD origem            AS CHAR
    FIELD it-codigo         AS CHAR FORMAT "x(16)"
    FIELD cod-estabel       AS CHAR /* storageloc */
    FIELD numero-ordem      AS CHAR 
    FIELD tipo-movto        AS CHAR
    FIELD fornecedor        AS CHAR
    FIELD quantidade        AS CHAR /*DEC*/ /* qt recebida ou se PO aberto qt solicitada - recebida */
    FIELD un                AS CHAR /* unidade de medida */
    FIELD custo-unit        AS CHAR /*DEC*/ /* preco unitario */
    FIELD fator-conv        AS CHAR /*DEC*/ /* fator conversao */
    FIELD data-requisicao   AS CHAR FORMAT "x(10)" /*DATE*/ /* data de requisiªao */
    FIELD data-aprov-req    AS CHAR FORMAT "x(10)" /*DATE*/ /* data de aprovaªío da requisiªao */
    FIELD data-compra       AS CHAR FORMAT "x(10)" /*DATE*/ /* data de compra */
    FIELD data-atualizacao  AS CHAR FORMAT "x(10)" /*DATE*/ /* data de atualizacao */
    FIELD data-recebimento  AS CHAR FORMAT "x(10)" /*DATE*/ /* data de disponibilidade do produto */
    FIELD data-disponivel   AS CHAR FORMAT "x(10)" /*DATE*/ /* data de disponibilidade do produto */
    FIELD blanket           AS CHAR /*  */
    FIELD nr-nota-fis       AS CHAR
    FIELD tipo-trans        AS CHAR FORMAT "x(4)"
    FIELD pedido            AS INTEGER
    FIELD situacao          AS CHAR FORMAT "x(20)"
    FIELD contrato          AS CHAR FORMAT "x(20)"
    FIELD parcela           AS CHAR                                                                    
    FIELD dataEntrega       AS CHAR FORMAT "x(10)"                                                     
    FIELD qtd-sal-forn      AS CHAR FORMAT "x(30)"
    INDEX chave IS PRIMARY
        it-codigo
        numero-ordem.

DEF TEMP-TABLE tt-ordem-prazo NO-UNDO 
    FIELD origem            AS CHAR
    FIELD it-codigo         AS CHAR FORMAT "x(16)"
    FIELD cod-estabel       AS CHAR /* storageloc */
    FIELD numero-ordem      AS CHAR 
    FIELD dataOrdem         AS CHAR FORMAT "x(10)"
    FIELD situacao          AS CHAR FORMAT "x(20)"
    FIELD parcela           AS CHAR
    FIELD dataEntrega       AS CHAR FORMAT "x(10)"
    FIELD quantidade        AS CHAR /*DEC*/ /* qt recebida ou se PO aberto qt solicitada - recebida */
    INDEX chave IS PRIMARY
          it-codigo
          numero-ordem.

   
DEF TEMP-TABLE tt-transfer NO-UNDO 
    FIELD it-codigo         AS CHAR FORMAT "x(16)"
    FIELD data-transfer     AS DATE 
    FIELD qt-entrada        AS DEC 
    FIELD estab-origem      AS CHAR
    FIELD estab-destino     AS CHAR
    INDEX chave IS PRIMARY
          it-codigo
          data-transfer.

DEF TEMP-TABLE tt-item NO-UNDO
    FIELD it-codigo     AS CHAR FORMAT "x(16)"
    FIELD desc-item     AS CHAR
    FIELD data-implant  AS DATE 
    FIELD un            AS CHAR 
    FIELD movto         AS LOG
    FIELD departamento  AS CHAR 
    INDEX chave IS UNIQUE PRIMARY
          it-codigo.

DEF TEMP-TABLE tt-estab NO-UNDO
    FIELD cod-estabel AS CHAR
    INDEX chave IS UNIQUE PRIMARY
          cod-estabel.

DEF TEMP-TABLE tt-depos NO-UNDO
    FIELD cod-depos AS CHAR
    INDEX chave IS UNIQUE PRIMARY
          cod-depos.

define temp-table tt-raw-digita
    field raw-digita AS raw.


DEFINE VARIABLE d-qtd-areceber AS DECIMAL     NO-UNDO.

FUNCTION fc-mdyy RETURN CHAR (da-data AS DATE).
    DEF VAR c-data AS CHAR NO-UNDO.
    IF da-data <> ? THEN DO:
        IF tt-param.formato = 2 THEN
            ASSIGN c-data = STRING(month(da-data),"99") + "/"
                            + STRING(DAY(da-data),"99") + "/"
                            + STRING(YEAR(da-data),"9999").
        ELSE
            ASSIGN c-data = STRING(da-data,"99/99/9999").
    END.
    ELSE 
        ASSIGN c-data = "".
    RETURN c-data.
END FUNCTION.
FUNCTION fc-dtoc RETURN CHAR (de-dec AS DECIMAL).
    DEF VAR c-var AS CHAR NO-UNDO.
    ASSIGN c-var = STRING(de-dec).
/*    
    IF de-dec <> ? THEN DO:
        ASSIGN c-var = STRING(de-dec,">>>,>>>,>>>,>>9.99").
        IF tt-param.formato = 2 THEN
            ASSIGN de-dec = replace(STRING(de-dec),",","#")
                   c-var = replace(STRING(de-dec),".",",")
                   c-var = replace(STRING(de-dec),"#",".").
    END.
    ELSE 
        ASSIGN c-var = "0".
*/    
    RETURN c-var.
END FUNCTION.
FUNCTION fc-qtoc RETURN CHAR (de-dec AS DECIMAL).
    DEF VAR c-var AS CHAR NO-UNDO.
/*    
    IF de-dec <> ? THEN DO:
        ASSIGN c-var = STRING(de-dec).
        IF tt-param.formato = 2 THEN
            ASSIGN c-var = replace(STRING(de-dec),",",".").
    END.
    ELSE 
        ASSIGN c-var = "0".
*/
    ASSIGN c-var = STRING(de-dec).
    RETURN c-var.
END FUNCTION.


/***********************************************************************
                              PAR∂METROS
 ***********************************************************************/
DEFINE INPUT PARAMETER raw-param AS raw NO-UNDO.
DEFINE INPUT PARAMETER table FOR tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param NO-ERROR.

FOR EACH tt-raw-digita:
    CREATE tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita NO-ERROR.
END.


/***********************************************************************
                         VARIµVEIS INTERNAS
 ***********************************************************************/
{utp/ut-glob.i}
{include/i-rpvar.i}

    def new global shared var c-dir-spool-servid-exec as CHAR no-undo.

def var c-trans       as character forma "x(4)".
def var c-esp       as character forma "x(03)".
DEFINE VARIABLE l-delete AS LOGICAL     NO-UNDO.
DEFINE VARIABLE  teste AS DECIMAL  FORMAT "->>,>>>,>>>,>>>9.9999"    NO-UNDO.

DEF VAR c-diretorio    AS CHAR NO-UNDO.
DEF VAR c-arquivo-saida AS CHAR NO-UNDO.
DEF STREAM str.

IF i-num-ped-exec-rpw <> 0 Then 
   Assign c-diretorio = c-dir-spool-servid-exec.
ELSE
   Assign c-diretorio = tt-param.diretorio.

  

FOR EACH ITEM NO-LOCK
    WHERE ITEM.it-codigo  >= tt-param.c-ini-item
    AND ITEM.it-codigo    <= tt-param.c-fim-item
    AND ITEM.fm-codigo    >= tt-param.c-ini-familia
    AND ITEM.fm-codigo    <= tt-param.c-fim-familia
/*     AND substring(item.char-2,212,1) >=  tt-param.cb-tp-item-ini */
/*     AND substring(item.char-2,212,1) <=  tt-param.cb-tp-item-fim */
    AND ITEM.tipo-contr = 2 /* total */
    AND ITEM.cod-obsoleto <> 4: /* */

    /* Marcio - 07/06/2010 */
    IF CAN-FIND(FIRST ext-item-uni-estab NO-LOCK 
                WHERE ext-item-uni-estab.it-codigo      = item.it-codigo  
                  AND ext-item-uni-estab.l-considera-xt = YES) THEN NEXT.

    CREATE tt-item.
    ASSIGN tt-item.it-codigo    = ITEM.it-codigo
           tt-item.desc-item    = ITEM.desc-item
           tt-item.data-implant = ITEM.data-implant
           tt-item.un           = ITEM.un.
    ASSIGN tt-item.movto = NO. 
    FOR FIRST es-it-depto OF item NO-LOCK,
        FIRST es-depto WHERE es-depto.codigo = es-it-depto.cod-depto NO-LOCK:
        ASSIGN tt-item.departamento = es-depto.descricao.
    END.
END.

FOR EACH estabelec NO-LOCK
    WHERE estabelec.ep-codigo = tt-param.c-ini-empresa
    AND estabelec.cod-estabel >= tt-param.c-ini-estabelec
    AND estabelec.cod-estabel <= tt-param.c-fim-estabelec:
    CREATE tt-estab.
    ASSIGN tt-estab.cod-estabel = estabelec.cod-estabel.
END.
FOR EACH deposito NO-LOCK,
    FIRST tt-digita WHERE tt-digita.cod-depos = deposito.cod-depos AND selec = YES:
    CREATE tt-depos.
    ASSIGN tt-depos.cod-depo = deposito.cod-depos.
END.

DEFINE VAR h-acomp      AS HANDLE  NO-UNDO.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Pesquisando *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
RUN pi-pesquisar.
run pi-finalizar in h-acomp.

PROCEDURE pi-pesquisar.

    IF tt-param.itemmaster THEN DO:
        FOR EACH item-uni-estab NO-LOCK,
            FIRST tt-estab OF item-uni-estab,
            FIRST tt-item  OF item-uni-estab,
            /*FIRST tt-depos OF item-uni-estab,*/
            /**** MOD.01: RODRIGO BAIONE ****/
            EACH saldo-estoq NO-LOCK
                WHERE saldo-estoq.cod-estabel = item-uni-estab.cod-estabel
                AND   saldo-estoq.it-codigo   = tt-item.it-codigo,
                FIRST tt-depos OF saldo-estoq NO-LOCK:
            /**** FIM - MOD.01: RODRIGO BAIONE ****/

           /* Marcio - 07/06/2010 */
           IF CAN-FIND(FIRST ext-item-uni-estab  
                 WHERE ext-item-uni-estab.it-codigo      = item-uni-estab.it-codigo  
                   AND ext-item-uni-estab.cod-estabel    = item-uni-estab.cod-estabel
                   AND ext-item-uni-estab.l-considera-xt = YES) THEN NEXT.           

            RUN pi-acompanhar IN h-acomp(INPUT "Item x Estab " + tt-estab.cod-estabel + "/" + tt-item.it-codigo).
            
            /**** MOD.01: RODRIGO BAIONE ****/
            FIND FIRST tt-item-estab
                WHERE tt-item-estab.it-codigo    = tt-item.it-codigo
                AND   tt-item-estab.cod-estabel  = item-uni-estab.cod-estabel
                NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-item-estab THEN
            /* dados cadastrais */
            CREATE tt-item-estab.
            /**** FIM - MOD.01: RODRIGO BAIONE ****/
            ASSIGN tt-item-estab.it-codigo  = tt-item.it-codigo
                   tt-item-estab.descricao  = tt-item.desc-item
                   tt-item-estab.deposito   = tt-depos.cod-depos. /**** MOD.01: RODRIGO BAIONE ****/
            ASSIGN tt-item-estab.data-criacao = fc-mdyy(tt-item.data-implant).

            /* parametrizaácao */
            ASSIGN tt-item-estab.cod-estabel    = item-uni-estab.cod-estabel
                   tt-item-estab.localizacao    = "".


            ASSIGN tt-item-estab.Lead-time      = fc-qtoc(dec(item-uni-estab.res-cq-comp 
                                                    + item-uni-estab.res-cq-fabri
                                                    + item-uni-estab.res-for-comp 
                                                    + item-uni-estab.res-int-comp 
                                                    + item-uni-estab.res-min-fabri 
                                                    + item-uni-estab.ressup-fabri)).

           ASSIGN   tt-item-estab.sazonalidade  = NO.

           /*ASSIGN   tt-item-estab.ponto-encomenda = fc-qtoc(item-uni-estab.ponto-encomenda).*/

           /* SERGIO-DSC-INCIDENTE INC0038327- 19/07/2016 ********************************************/
           /*
           FIND FIRST ext-item-uni-estab01 OF item-uni-estab NO-LOCK NO-ERROR.
           IF AVAIL ext-item-uni-estab01 THEN
               ASSIGN   tt-item-estab.ponto-encomenda = fc-qtoc(ext-item-uni-estab01.ponto-encomenda).
           ELSE 
               ASSIGN   tt-item-estab.ponto-encomenda = fc-qtoc(0).
           */

           ASSIGN   tt-item-estab.ponto-encomenda = STRING(item-uni-estab.ponto-encomenda).
           /******************************************************************************************/





           ASSIGN   tt-item-estab.consumo-prev    = fc-qtoc(item-uni-estab.consumo-prev).
           ASSIGN   tt-item-estab.qt-minimo       = fc-qtoc(item-uni-estab.quant-segur).
           ASSIGN   tt-item-estab.qt-maximo       = fc-qtoc(0).


           ASSIGN   tt-item-estab.donotexport   = YES
                    tt-item-estab.criticidade   = IF item-uni-estab.criticidade = 1 THEN "X" 
                                                    ELSE IF item-uni-estab.criticidade = 2 THEN "Y"
                                                    ELSE IF item-uni-estab.criticidade = 1 THEN "Z"
                                                    ELSE ""
                    tt-item-estab.critic-inventario = tt-item-estab.qt-minimo
                    tt-item-estab.item-estoque  = YES
                    tt-item-estab.indicar-estoque = YES.

           ASSIGN   tt-item-estab.fator-conversao = fc-qtoc(1).
                    
            ASSIGN d-qtd-areceber = 0.
            /*Leo-Kraft-21/12/10
            /* quantidade da ultima ordem de compra */
           /* ====== Alterado para imprimir somente quantidade que falta receber ====== Felipe/Kraft */
            FOR LAST ordem-compra NO-LOCK USE-INDEX compra-item
                WHERE ordem-compra.it-codigo = tt-item.it-codigo
                AND ordem-compra.cod-estabel = tt-estab.cod-estabel
                AND (ordem-compra.situacao = 2 OR   /* confirmada */
                     ordem-compra.situacao = 6):    /* recebida */
            
                /* Felipe Silvestre - Regra para trazer o saldo que esta para receber */    
                FOR EACH prazo-compra 
                    WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem NO-LOCK:

                    ASSIGN d-qtd-areceber = d-qtd-areceber + prazo-compra.qtd-sal-forn.
                        
                END.                

                ASSIGN tt-item-estab.qt-ordem = fc-qtoc(d-qtd-areceber).
            END.
            */
            /*Leo-kraft-21/12/10*/
            FOR each ordem-compra NO-LOCK USE-INDEX compra-item
                WHERE ordem-compra.it-codigo = tt-item.it-codigo
                AND ordem-compra.cod-estabel = tt-estab.cod-estabel
                AND (ordem-compra.situacao = 2) /*OR   /* confirmada */
                     ordem-compra.situacao = 6) leo-Kraft-21/12/10 */:    /* recebida */
            
                /* Felipe Silvestre - Regra para trazer o saldo que esta para receber */    
                FOR EACH prazo-compra 
                    WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem NO-LOCK:

                    ASSIGN d-qtd-areceber = d-qtd-areceber + prazo-compra.qtd-sal-forn.
                        
                END.                

                ASSIGN tt-item-estab.qt-ordem = fc-qtoc(d-qtd-areceber).
            END.
            /********************/
            
            /* fornecedor padr∆o */
            FOR LAST item-fornec NO-LOCK
                WHERE item-fornec.it-codigo = tt-item.it-codigo
                BREAK BY item-fornec.perc-compra:
                FIND FIRST emitente NO-LOCK
                    WHERE emitente.cod-emitente = item-fornec.cod-emitente NO-ERROR.
                IF AVAIL emitente THEN
                    ASSIGN tt-item-estab.fornecedor = emitente.nome-abrev.
            END.

            /* custo medio */
            FIND LAST pr-it-per NO-LOCK
                WHERE pr-it-per.cod-estabel = tt-item-estab.cod-estabel
                AND   pr-it-per.it-codigo = tt-item-estab.it-codigo NO-ERROR.
            IF AVAIL pr-it-per THEN
                ASSIGN tt-item-estab.custo-unit = fc-dtoc(dec(pr-it-per.val-unit-mat-m[1] 
                                                + pr-it-per.val-unit-mob-m[1] 
                                                + pr-it-per.val-unit-ggf-m[1])).


            /**** MOD.01: RODRIGO BAIONE ****/
            IF saldo-estoq.qtidade-atu > 0 THEN
                ASSIGN tt-item-estab.qt-estoque1 = tt-item-estab.qt-estoque1 + saldo-estoq.qtidade-atu
                       tt-item.movto = YES.
            /**** FIM - MOD.01: RODRIGO BAIONE ****/

            ASSIGN tt-item-estab.qt-estoque = fc-qtoc(tt-item-estab.qt-estoque1).

            ASSIGN tt-item-estab.departamento = tt-item.departamento.
            ASSIGN tt-item-estab.demanda      = {ininc/i02in122.i 4 item-uni-estab.demanda}.
        END.

        ASSIGN c-arquivo-saida = c-diretorio + "\ItemMaster2.txt".
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"\","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        OUTPUT STREAM str TO VALUE(c-arquivo-saida).
        
        PUT STREAM str "ItemNum;StorageLoc;BinNum;AvgUnitCost;QtyOH;LTDays;Seasonal;OP;OQ;Min;Max;DoNotExport;Description;CriticalityLevel;SetMinToOrAbove;StockItem;PurchaseToIssueConvFactor;CreationDate;StockIndicator;PrimaryVendor;Departamento;PC;Demanda" SKIP.

                                                                    
        FOR EACH tt-item-estab NO-LOCK:
/*             EXPORT STREAM str DELIMITER ";" tt-item-estab EXCEPT tt-item-estab.qt-estoque1. */

              PUT STREAM str
                  tt-item-estab.it-codigo         ";" FORMAT "x(16)"/* itemnum */
                  /* SERGIO-DSC-INCIDENTE INC0038327- 19/07/2016 ********************************************/
                  /*tt-item-estab.cod-estabel       ";" /* storageloc */*/
                  tt-item-estab.deposito          ";" /* storageloc */

                  /* SERGIO-DSC-INCIDENTE INC0038327- 19/07/2016 ********************************************/
                  tt-item-estab.localizacao       ";" /* brancos */
                  dec(tt-item-estab.custo-unit)        FORMAT "->>,>>>,>>>,>>>9.9999" ";"  /*DEC*/  /* AvgUnitCost */
                  dec(tt-item-estab.qt-estoque)        FORMAT "->>,>>>,>>>,>>>9.9999" ";" /*DEC*/  /* qtyOH */
              /*     tt-item-estab.qt-estoque1    ";"    /* qtyOH */ */
                  dec(tt-item-estab.Lead-time)         FORMAT "->>,>>>,>>>,>>>9.9999" ";" /*DEC*/  /* LTDays */
                  tt-item-estab.Sazonalidade      ";" /* Seasonal */
                  dec(tt-item-estab.ponto-encomenda)   FORMAT "->>,>>>,>>>,>>>9.9999" ";" /*DEC*/  /* OP */
                  dec(tt-item-estab.qt-ordem)          FORMAT "->>,>>>,>>>,>>>9.9999" ";" /*DEC*/  /* OQ */
                  dec(tt-item-estab.qt-minimo)         FORMAT "->>,>>>,>>>,>>>9.9999" ";" /*DEC*/  /* Min */
                  dec(tt-item-estab.qt-maximo)         FORMAT "->>,>>>,>>>,>>>9.9999" ";" /*DEC*/  /* Max */
                  tt-item-estab.donotexport       ";" /* donotexport */
                  tt-item-estab.descricao         ";" /* description */
                  tt-item-estab.criticidade       ";" /* CriticalityLevel */
                  dec(tt-item-estab.critic-inventario) FORMAT "->>,>>>,>>>,>>>9.9999" ";" /*DEC*/  /*SetMinToOrAbove*/
                  tt-item-estab.item-estoque      ";" /* StockItem */
                  dec(tt-item-estab.fator-conversao)   FORMAT "->>,>>>,>>>,>>>9.9999" ";" /*DEC*/  /* PurchaseToIssueConvFactor */
                  tt-item-estab.data-criacao      ";" /*date*/ /* creationDate */
                  tt-item-estab.indicar-estoque   ";" /* StockIndicator */
                  tt-item-estab.fornecedor             FORMAT "x(20)" ";"  /* PrimaryVendor */
                  tt-item-estab.departamento           FORMAT "x(20)" ";" /* campo novo */
                  dec(tt-item-estab.consumo-prev)      FORMAT "->>,>>>,>>>,>>>9.9999" ";"   /*DEC*/  /* PC */ 
                  tt-item-estab.demanda                FORMAT "x(16)" SKIP.
        END.
        OUTPUT STREAM str CLOSE.
        EMPTY TEMP-TABLE tt-item-estab.
    END.

    IF tt-param.movto-ent-sai THEN DO:
        EMPTY TEMP-TABLE tt-esp-docto.

        /*Criar especies para que o movimento valide durante a entrada*/
        CREATE tt-esp-docto.
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 21
               tt-esp-docto.tipo-trans = 1
               tt-esp-docto.tipo = "NFE".
        /*Criar especies para que o movimento valide durante a saida*/
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 28
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "REQ".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 30
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "RM".
        

        FOR EACH movto-estoq NO-LOCK
            WHERE movto-estoq.dt-trans >= tt-param.c-ini-periodo
            AND movto-estoq.dt-trans <= tt-param.c-fim-periodo
            AND movto-estoq.quantidade <> 0,
            FIRST tt-esp-docto OF movto-estoq,
            FIRST tt-estab OF movto-estoq,
            FIRST tt-item OF movto-estoq,
            FIRST tt-depos OF movto-estoq:

			/* Marcio - 07/06/2010 */
            IF CAN-FIND(FIRST ext-item-uni-estab  
                        WHERE ext-item-uni-estab.it-codigo      = movto-estoq.it-codigo  
                          AND ext-item-uni-estab.cod-estabel    = movto-estoq.cod-estabel
                          AND ext-item-uni-estab.l-considera-xt = YES) THEN NEXT.           
            
            RUN pi-acompanhar IN h-acomp(INPUT "Movimentos " + string(movto-estoq.dt-tran,"99/99/9999")).

            FIND FIRST tt-movto-ent-sai
                WHERE tt-movto-ent-sai.cod-estabel  = movto-estoq.cod-estabel
                  AND tt-movto-ent-sai.it-codigo    = movto-estoq.it-codigo NO-LOCK NO-ERROR.

            IF NOT AVAIL tt-movto-ent-sai THEN DO:
                CREATE tt-movto-ent-sai.
                ASSIGN tt-movto-ent-sai.cod-estabel  = movto-estoq.cod-estabel
                       tt-movto-ent-sai.it-codigo    = movto-estoq.it-codigo.

            END.

            IF movto-estoq.tipo-trans = 1 THEN /*Ent*/
                ASSIGN tt-movto-ent-sai.data-ult-ent = fc-mdyy(movto-estoq.dt-trans).
            ELSE IF movto-estoq.tipo-trans = 2 THEN /*Sai*/
                ASSIGN tt-movto-ent-sai.data-ult-sai = fc-mdyy(movto-estoq.dt-trans).


        END.

        ASSIGN c-arquivo-saida = c-diretorio + "\MovtoEntSai.txt".
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"\","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        OUTPUT STREAM str TO VALUE(c-arquivo-saida).

        PUT STREAM str "ItemNum;StorageLoc;FirstInDate;LastOutDate" SKIP.
        FOR EACH tt-movto-ent-sai NO-LOCK:
/*             EXPORT STREAM str DELIMITER ";" tt-entrega. */
            PUT STREAM str
              tt-movto-ent-sai.it-codigo    ";" FORMAT "x(16)"
              tt-movto-ent-sai.cod-estabel  ";" 
              tt-movto-ent-sai.data-ult-ent ";"
              tt-movto-ent-sai.data-ult-sai SKIP.
                
        END.
        EMPTY TEMP-TABLE tt-movto-ent-sai.
        OUTPUT STREAM str CLOSE. 

    END.

    IF tt-param.entrega THEN DO:
        EMPTY TEMP-TABLE tt-esp-docto.

        /*Criar especies para que o movimento valide durante a entrada*/
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 2
               tt-esp-docto.tipo-trans = 1
               tt-esp-docto.tipo = "ACT".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 5
               tt-esp-docto.tipo-trans = 1
               tt-esp-docto.tipo = "DEV".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 6
               tt-esp-docto.tipo-trans = 1
               tt-esp-docto.tipo = "DIV".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 7
               tt-esp-docto.tipo-trans = 1
               tt-esp-docto.tipo = "DRM".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 15
               tt-esp-docto.tipo-trans = 1
               tt-esp-docto.tipo = "INV".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 21
               tt-esp-docto.tipo-trans = 1
               tt-esp-docto.tipo = "NFE".


        /*Criar especies para que o movimento valide durante a saida*/
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 2
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "ACT".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 6
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "DIV".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 15
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "INV".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 20
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "NFD".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 28
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "REQ".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 30
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "RM".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 33
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "TRA".


        /* movimento de entregas - especies */
        FOR EACH movto-estoq NO-LOCK
            WHERE movto-estoq.dt-trans >= tt-param.c-ini-periodo
            AND movto-estoq.dt-trans <= tt-param.c-fim-periodo
            AND movto-estoq.quantidade <> 0,
            FIRST tt-esp-docto OF movto-estoq ,
            FIRST tt-estab OF movto-estoq,
            FIRST tt-item OF movto-estoq,
            FIRST tt-depos OF movto-estoq:

			/* Marcio - 07/06/2010 */
            IF CAN-FIND(FIRST ext-item-uni-estab  
                        WHERE ext-item-uni-estab.it-codigo      = movto-estoq.it-codigo  
                          AND ext-item-uni-estab.cod-estabel    = movto-estoq.cod-estabel
                          AND ext-item-uni-estab.l-considera-xt = YES) THEN NEXT.           
            RUN pi-acompanhar IN h-acomp(INPUT "Entregas " + string(movto-estoq.dt-tran,"99/99/9999")).

            DEF VAR c-data-entrada AS CHAR NO-UNDO.
            CREATE tt-entrega.
            ASSIGN tt-entrega.it-codigo = tt-item.it-codigo
                   tt-entrega.cod-estabel = movto-estoq.cod-estabel.

            ASSIGN c-data-entrada = fc-mdyy(movto-estoq.dt-trans).
            ASSIGN tt-entrega.data-entrada = c-data-entrada.
            
            /* quantidade */
            IF tt-esp-docto.tipo-trans = 2 THEN /* nfd */
                ASSIGN tt-entrega.qt-entrada = fc-qtoc(movto-estoq.quantidade * -1).
            ELSE
                ASSIGN tt-entrega.qt-entrada = fc-qtoc(movto-estoq.quantidade).
            ASSIGN tt-entrega.tipo-entrada = tt-esp-docto.tipo.
            ASSIGN tt-item.movto = YES.
            
            /*Cleber*/
            assign c-trans = IF movto-estoq.tipo-trans = 1 THEN "Ent" ELSE "Sai".
            ASSIGN tt-entrega.tipo-trans = c-trans.

/*             IF tt-entrega.tipo-entrada <> tt-param.c-esp-ini AND tt-entrega.tipo-entrada <> tt-param.c-esp-fin THEN */
/*                 DELETE tt-entrega.                                                                                  */
            l-delete = NO.


            CASE string(tt-entrega.tipo-entrada):                                                                                            
                 WHEN  "ACA"  then  if tt-param.l-ACA   then  l-delete = no.   else l-delete = YES .  
                 WHEN  "DIV"  then  if tt-param.L-DIV   then  l-delete = no.   else l-delete = yes .
                 WHEN  "NU2"  then  if tt-param.L-NU2   then  l-delete = no.   else l-delete = yes .
                 WHEN  "IPL"  then  if tt-param.L-IPL   then  l-delete = no.   else l-delete = yes .
                 WHEN  "NFE"  then  if tt-param.L-NFE   then  l-delete = no.   else l-delete = yes .
                 WHEN  "RCS"  then  if tt-param.L-RCS   then  l-delete = no.   else l-delete = yes .
                 when  "RRQ"  then  if tt-param.L-RRQ   then  l-delete = no.   else l-delete = yes .
                 when  "act"  then  if tt-param.l-act   then  l-delete = no.   else l-delete = yes .
                 when  "DRM"  then  if tt-param.L-DRM   then  l-delete = no.   else l-delete = yes .
                 when  "NU3"  then  if tt-param.L-NU3   then  l-delete = no.   else l-delete = yes .
                 when  "MOB"  then  if tt-param.L-MOB   then  l-delete = no.   else l-delete = yes .
                 when  "NFS"  then  if tt-param.L-NFS   then  l-delete = no.   else l-delete = yes .
                 when  "RDD"  then  if tt-param.L-RDD   then  l-delete = no.   else l-delete = yes .
                 when  "STR"  then  if tt-param.L-STR   then  l-delete = no.   else l-delete = yes .
                 when  "NU1"  then  if tt-param.L-NU1   then  l-delete = no.   else l-delete = yes .
                 when  "EAC"  then  if tt-param.L-EAC   then  l-delete = no.   else l-delete = yes .
                 when  "NU4"  then  if tt-param.L-NU4   then  l-delete = no.   else l-delete = yes .
                 when  "NC"   then  if tt-param.L-NC    then  l-delete = no.   else l-delete = yes .
                 when  "NFT"  then  if tt-param.L-NFT   then  l-delete = no.   else l-delete = yes .
                 when  "REQ"  then  if tt-param.L-REQ   then  l-delete = no.   else l-delete = yes .
                 when  "TRA"  then  if tt-param.L-TRA   then  l-delete = no.   else l-delete = yes .
                 when  "DD"   then  if tt-param.L-DD    then  l-delete = no.   else l-delete = yes .
                 when  "EGF"  then  if tt-param.L-EGF   then  l-delete = no.   else l-delete = yes .
                 when  "ICM"  then  if tt-param.L-ICM   then  l-delete = no.   else l-delete = yes .
                 when  "NF"   then  if tt-param.L-NF    then  l-delete = no.   else l-delete = yes .
                 when  "NUS"  then  if tt-param.L-NUS   then  l-delete = no.   else l-delete = yes .
                 when  "RFS"  then  if tt-param.L-RFS   then  l-delete = no.   else l-delete = yes .
                 when  "ZZZ"  then  if tt-param.L-ZZZ   then  l-delete = no.   else l-delete = yes .
                 when  "DEV"  then  if tt-param.L-DEV   then  l-delete = no.   else l-delete = yes .
                 when  "BEM"  then  if tt-param.L-BEM   then  l-delete = no.   else l-delete = yes .
                 when  "INV"  then  if tt-param.L-INV   then  l-delete = no.   else l-delete = yes .
                 when  "NFD"  then  if tt-param.L-NFD   then  l-delete = no.   else l-delete = yes .
                 when  "REF"  then  if tt-param.L-REF   then  l-delete = no.   else l-delete = yes .
                 when  "RM"   then  if tt-param.L-RM    then  l-delete = no.   else l-delete = yes .
                 WHEN  "SOB"  then  if tt-param.L-SOB   then  l-delete = no.   else l-delete = yes .
                                                                         
             END CASE.
             IF l-delete THEN
               DELETE tt-entrega. 


        END.
        
        ASSIGN c-arquivo-saida = c-diretorio + "\Issue2.txt".
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"\","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        OUTPUT STREAM str TO VALUE(c-arquivo-saida).

        PUT STREAM str "ItemNum;StorageLoc;IssueDate;QtyIssued;Tp;TransactionType" SKIP.
        FOR EACH tt-entrega NO-LOCK:
/*             EXPORT STREAM str DELIMITER ";" tt-entrega. */
            PUT STREAM str
              
              tt-entrega.it-codigo         ";" FORMAT "x(16)"
              tt-entrega.cod-estabel       ";" /* storageloc */
              tt-entrega.data-entrada      ";" /*DATE*/ 
              dec(tt-entrega.qt-entrada)       FORMAT "->>,>>>,>>>,>>>9.9999" ";" /*DEC*/ 
              tt-entrega.tipo-trans        ";"
              tt-entrega.tipo-entrada      SKIP.

        END.
        EMPTY TEMP-TABLE tt-entrega.
        OUTPUT STREAM str CLOSE. 
    END.

    IF tt-param.ordem THEN DO:
        /* como consistir o periodo */
        FOR EACH ordem-compra NO-LOCK
            WHERE ordem-compra.data-emissao >= tt-param.c-ini-periodo
            AND ordem-compra.data-emissao <= tt-param.c-fim-periodo
            AND (ordem-compra.situacao = 2 OR   /* confirmada */
                 ordem-compra.situacao = 6),    /* recebida   */
            FIRST tt-estab OF ordem-compra NO-LOCK,
            FIRST tt-item OF ordem-compra NO-LOCK:

            /* Marcio - 07/06/2010 */
            IF CAN-FIND(FIRST ext-item-uni-estab  
                  WHERE ext-item-uni-estab.it-codigo      = ordem-compra.it-codigo   
                    AND ext-item-uni-estab.cod-estabel    = ordem-compra.cod-estabel 
                    AND ext-item-uni-estab.l-considera-xt = YES) THEN NEXT.           

            RUN pi-acompanhar IN h-acomp(INPUT "Ordens " + string(ordem-compra.numero-ordem)).

            FOR EACH prazo-compra 
               WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem NO-LOCK:

                CREATE tt-ordem.
                ASSIGN tt-ordem.origem = "PO".
                ASSIGN tt-ordem.it-codigo = tt-item.it-codigo
                       tt-ordem.cod-estabel = ordem-compra.cod-estabel
                       tt-ordem.numero-ordem = /*trim( tt-ordem.origem) +*/ string(ordem-compra.numero-ordem,"99999999"). 
    
                /* data do ultimo recebimento */
                ASSIGN tt-ordem.custo-unit = fc-dtoc(ordem-compra.preco-unit /* SERGIO-DSC-INCIDENTE INC0038327- 19/07/2016 ordem-compra.pre-unit-for*/).   
                ASSIGN tt-ordem.un = tt-item.un.
                ASSIGN tt-ordem.fator-conv = fc-qtoc(1).
                
                /* quantidade recebida */
                ASSIGN tt-ordem.quantidade    = fc-qtoc(prazo-compr.quantidade).
                ASSIGN tt-ordem.parcela       = STRING(prazo-compr.parcela).
                ASSIGN tt-ordem.dataEntrega   = fc-mdyy(prazo-compr.data-entrega).
                ASSIGN tt-ordem.qtd-sal-forn  = STRING(prazo-compr.qtd-sal-forn).
            
                FOR EACH  recebimento OF prazo-compra USE-INDEX ult-entrega NO-LOCK.

                    FIND docum-est NO-LOCK WHERE
                         docum-est.serie-docto  = recebimento.serie-nota   AND
                         docum-est.nro-docto    = recebimento.numero-nota  AND
                         docum-est.cod-emitente = recebimento.cod-emitente AND
                         docum-est.nat-operacao = recebimento.nat-operaá∆o 
                         NO-ERROR.
    
                    IF AVAIL docum-est THEN DO:
                        ASSIGN tt-ordem.nr-nota-fis      = recebimento.numero-nota.
                        ASSIGN tt-ordem.data-recebimento = fc-mdyy(recebimento.data-nota).
                        ASSIGN tt-ordem.data-disponivel  = fc-mdyy(docum-est.dt-trans).
                    END.
                END.

                /* data requisicao */
                ASSIGN tt-ordem.data-requisicao = ?.
                FOR EACH it-requisicao NO-LOCK
                    WHERE it-requisicao.numero-ordem = ordem-compra.numero-ordem,
                    FIRST requisicao OF it-requisicao NO-LOCK:
                    ASSIGN tt-ordem.data-requisicao = fc-mdyy(requisicao.dt-requisicao).
                END.
                IF tt-ordem.data-requisicao = ? THEN
                    ASSIGN tt-ordem.data-requisicao = fc-mdyy(ordem-compra.data-emissao).
    
                /* data aprovacao da requisicao */
                ASSIGN tt-ordem.data-aprov-req = fc-mdyy(ordem-compra.data-emissao).
    
                /* data da compra */
                ASSIGN tt-ordem.data-compra = fc-mdyy(ordem-compra.data-pedido). /*Leo-Kraft- 21/12/10 data-emissao */
                /*leo -  kraft - 04/01/11 
                ASSIGN tt-ordem.data-atualizacao = fc-mdyy(ordem-compra.data-emissao).
                */
                /*leo-kraft-04/01/11*/
                IF ordem-compra.num-pedido > 0 THEN DO:
                    FIND pedido-compr WHERE
                             pedido-compr.num-pedido = ordem-compra.num-pedido NO-LOCK NO-ERROR.
    
                    IF AVAIL pedido-compr THEN
                        ASSIGN tt-ordem.contrato = string(pedido-compr.nr-contrato).
    
                    find first doc-pend-aprov where
                        doc-pend-aprov.ind-tip-doc   = (IF pedido-compr.emergencial THEN 6 ELSE 4) and
                        doc-pend-aprov.numero-ordem  = ordem-compra.numero-ordem and
                        doc-pend-aprov.num-pedido    = ordem-compra.num-pedido   and
                        ((doc-pend-aprov.ind-situacao  = 1  or
                        doc-pend-aprov.ind-situacao    = 3) or
                        (doc-pend-aprov.ind-situacao   = 2  and
                        doc-pend-aprov.sit-aprov = no))
                    no-lock no-error.
    
                    if available doc-pend-aprov then
                        tt-ordem.data-atualizacao = "".
                    ELSE DO:
                        FIND LAST doc-pend-aprov where
                            doc-pend-aprov.ind-tip-doc =
                                 (IF pedido-compr.emergencial THEN 6 ELSE 4) and
                            doc-pend-aprov.num-pedido  = pedido-compr.num-pedido
                            AND doc-pend-aprov.sit-aprov = YES
                        NO-LOCK NO-ERROR.
    
                        IF AVAILABLE doc-pend-aprov THEN DO:
                            ASSIGN tt-ordem.data-atualizacao = fc-mdyy(doc-pend-aprov.dt-aprova).
                        END.
                        ELSE
                            ASSIGN tt-ordem.data-atualizacao = "".
                    END.
                END.
                ELSE ASSIGN tt-ordem.data-atualizacao = "". 
                tt-ordem.pedido = ordem-compra.num-pedido.
                /*leo-kraft-04/01/11 */
    
    
                /* fornecedor */
                FIND FIRST emitente NO-LOCK
                    WHERE emitente.cod-emitente = ordem-compra.cod-emitente NO-ERROR.
                IF AVAIL emitente THEN
                    ASSIGN tt-ordem.fornecedor = emitente.nome-abrev.
    
                /*Fixa a Situacao da Ordem de Compra*/
                ASSIGN tt-ordem.situacao = {ininc/i02in274.i 4 prazo-compr.situacao}.
            END.
        END.

        ASSIGN c-arquivo-saida = c-diretorio + "\ItemPurchase2.txt".
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"\","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        OUTPUT STREAM str TO VALUE(c-arquivo-saida).
                                                 
        PUT STREAM str "ItemNum;StorageLoc;PONUM;VendorName;Qty;OrderUnit;UnitCost;ConversionFactor;PurchaseRecDate;PurchaseRecApprovalDate;PurchaseDate;ReleaseDate;AvailableDate;ReceiptDate;BlanketPOIndcator;NumDocto;Pedido;POStatus;Contract;Sequence;DeliveryDate;Saldo" SKIP.
        FOR EACH tt-ordem NO-LOCK:
/*             EXPORT STREAM str DELIMITER ";" tt-ordem EXCEPT origem. */
            PUT STREAM str                                                                                           
            /*   tt-ordem.origem            ";" */                                                                   
              tt-ordem.it-codigo         ";" FORMAT "x(16)"                                                                        
              tt-ordem.cod-estabel       ";"   /* storageloc */                                                      
              /*tt-ordem.tipo-movto        ";"*/ /*MOD. 01: Retirado por solicitaá∆o do JosÇ Carlos - Rodrigo Baione*/
              tt-ordem.numero-ordem      ";"
              tt-ordem.fornecedor        FORMAT "x(60)" ";"                                                                         
              dec(tt-ordem.quantidade)        FORMAT "->>,>>>,>>>,>>>9.9999" ";"   /*DEC*/ /* qt recebida ou se PO aberto qt solicitada - recebida */    
              tt-ordem.un                ";"   /* unidade de medida */                                               
              dec(tt-ordem.custo-unit)        FORMAT "->>,>>>,>>>,>>>9.9999" ";"    /*DEC*/ /* preco unitario */                                          
              dec(tt-ordem.fator-conv)        FORMAT "->>,>>>,>>>,>>>9.9999" ";"    /*DEC*/ /* fator conversao */                                         
              tt-ordem.data-requisicao   ";"   /*DATE*/ /* data de requisiªao */                                     
              tt-ordem.data-aprov-req    ";"   /*DATE*/ /* data de aprovaªío da requisiªao */                        
              tt-ordem.data-compra       ";"   /*DATE*/ /* data de compra */                                         
              tt-ordem.data-atualizacao  ";"   /*DATE*/ /* data de atualizacao */                                    
              tt-ordem.data-recebimento  ";"   /*DATE*/ /* data de disponibilidade do produto */                     
              tt-ordem.data-disponivel   ";"   /*DATE*/ /* data de disponibilidade do produto */                     
              tt-ordem.blanket           ";"   /*  */                                                                
              tt-ordem.nr-nota-fis ";"       
              tt-ordem.pedido ";"
              tt-ordem.situacao ";" 
              tt-ordem.contrato ";"
              tt-ordem.parcela ";"
              tt-ordem.dataEntrega ";"
              tt-ordem.qtd-sal-forn
             SKIP.  

        END.
        OUTPUT STREAM str CLOSE.
        EMPTY TEMP-TABLE tt-ordem.

    END.

    DEF VAR c-data-ordem    AS CHAR.
    DEF VAR c-data-entrega  AS CHAR.

    IF tt-param.prazo-compra THEN DO:
        /* como consistir o periodo */
        FOR EACH ordem-compra NO-LOCK
            WHERE ordem-compra.data-emissao >= tt-param.c-ini-periodo
            AND ordem-compra.data-emissao <= tt-param.c-fim-periodo
            AND (ordem-compra.situacao <> 2 AND   /* confirmada */
                 ordem-compra.situacao <> 4 AND   /* elinada */
                 ordem-compra.situacao <> 6),    /* recebida */
            FIRST tt-estab OF ordem-compra NO-LOCK,
            FIRST tt-item OF ordem-compra NO-LOCK:
            
            /* Marcio - 07/06/2010 */
            IF CAN-FIND(FIRST ext-item-uni-estab  
                  WHERE ext-item-uni-estab.it-codigo      = ordem-compra.it-codigo   
                    AND ext-item-uni-estab.cod-estabel    = ordem-compra.cod-estabel 
                    AND ext-item-uni-estab.l-considera-xt = YES) THEN NEXT.           

            FOR EACH prazo-compra 
               WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem NO-LOCK:
                
                RUN pi-acompanhar IN h-acomp(INPUT "Ordens Prazo" + string(ordem-compra.numero-ordem)).

                ASSIGN c-data-ordem     = fc-mdyy(ordem-compra.data-emissao).
                ASSIGN c-data-entrega   = fc-mdyy(prazo-compr.data-entrega).

                CREATE tt-ordem-prazo.
                ASSIGN tt-ordem-prazo.origem        = "PR".
                ASSIGN tt-ordem-prazo.it-codigo     = tt-item.it-codigo
                       tt-ordem-prazo.cod-estabel   = ordem-compra.cod-estabel
                       tt-ordem-prazo.numero-ordem  = string(ordem-compra.numero-ordem,"99999999")
                       tt-ordem-prazo.dataOrdem     = c-data-ordem
                       tt-ordem-prazo.situacao      = {ininc/i02in274.i 4 ordem-compra.situacao}
                       tt-ordem-prazo.parcela       = STRING(prazo-compr.parcela)
                       tt-ordem-prazo.dataEntrega   = c-data-entrega
                       tt-ordem-prazo.quantidade    = STRING(prazo-compr.quantidade).
                
            END.                
        END.

        ASSIGN c-arquivo-saida = c-diretorio + "\PurchaseReq2.txt".
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"\","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        OUTPUT STREAM str TO VALUE(c-arquivo-saida).
                                                 
        PUT STREAM str "ItemNum;StorageLoc;PRNUM;date;Status;Sequence;DeliveryDate;Qty" SKIP.
            
        FOR EACH tt-ordem-prazo NO-LOCK:
            PUT STREAM str                  
                tt-ordem-prazo.it-codigo    ";" FORMAT "x(16)"
                tt-ordem-prazo.cod-estabel  ";" 
                tt-ordem-prazo.numero-ordem ";"
                tt-ordem-prazo.dataOrdem    ";"
                tt-ordem-prazo.situacao     ";"
                tt-ordem-prazo.parcela      ";"
                tt-ordem-prazo.dataEntrega  ";"
                tt-ordem-prazo.quantidade   SKIP.
        END.
        OUTPUT STREAM str CLOSE.
        EMPTY TEMP-TABLE tt-ordem-prazo.

    END.


    IF tt-param.requisicao THEN DO:
        /* movimento de saida por requisiá‰es  REQ e RRQ */
        /* como identificar requisiáoes */
        EMPTY TEMP-TABLE tt-esp-docto.
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 2
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "ACT".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 6
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "DIV".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 15
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "INV".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 28
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "REQ".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 30
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "RM".
        CREATE tt-esp-docto.
        ASSIGN tt-esp-docto.esp-docto = 33
               tt-esp-docto.tipo-trans = 2
               tt-esp-docto.tipo = "TRA".

        FOR EACH movto-estoq NO-LOCK  WHERE 
                movto-estoq.tipo-trans = 2
            AND movto-estoq.dt-trans >= tt-param.c-ini-periodo
            AND movto-estoq.dt-trans <= tt-param.c-fim-periodo
            AND movto-estoq.quantidade <> 0,
            FIRST tt-esp-docto OF movto-estoq,
            FIRST tt-estab OF movto-estoq,
            FIRST tt-item OF movto-estoq,
            FIRST tt-depos OF movto-estoq :

			/* Marcio - 07/06/2010 */
            IF CAN-FIND(FIRST ext-item-uni-estab  
                        WHERE ext-item-uni-estab.it-codigo      = movto-estoq.it-codigo      
                          AND ext-item-uni-estab.cod-estabel    = movto-estoq.cod-estabel    
                          AND ext-item-uni-estab.l-considera-xt = YES) THEN NEXT.    
            RUN pi-acompanhar IN h-acomp(INPUT "Requisicoes " + string(movto-estoq.dt-tran,"99/99/9999")).

            CREATE tt-ordem.
            ASSIGN tt-ordem.origem = "RQ".
            ASSIGN tt-ordem.it-codigo    = tt-item.it-codigo
                   tt-ordem.cod-estabel  = movto-estoq.cod-estabel
/*                   tt-ordem.numero-ordem = trim(tt-ordem.origem) + STRING(movto-estoq.nr-req-sum) */
                   tt-ordem.numero-ordem = trim(tt-ordem.origem)
                   tt-ordem.tipo-movto   = STRING(movto-estoq.nr-req-sum)


                   tt-ordem.un           = movto-estoq.un.

           ASSIGN  tt-ordem.custo-unit   = fc-dtoc(dec(movto-estoq.valor-mat-m[1] 
                                           + movto-estoq.valor-mob-m[1] 
                                           + movto-estoq.valor-ggf-m[1])).

             CASE movto-estoq.esp-docto:                                                                                            
                 WHEN 2 THEN ASSIGN tt-ordem.numero-ordem = trim(tt-esp-docto.tipo)
                                    tt-ordem.tipo-movto   = STRING(movto-estoq.nro-docto).                
                 WHEN 6 THEN ASSIGN tt-ordem.numero-ordem = trim(tt-esp-docto.tipo)
                                    tt-ordem.tipo-movto   =  STRING(movto-estoq.dt-trans,"99999999").      
                 WHEN 15 THEN ASSIGN tt-ordem.numero-ordem = trim(tt-esp-docto.tipo) 
                                     tt-ordem.tipo-movto   = STRING(movto-estoq.nro-docto).               
                 WHEN 28 THEN ASSIGN tt-ordem.numero-ordem = trim(tt-esp-docto.tipo)
                                     tt-ordem.tipo-movto   = STRING(movto-estoq.nro-docto).               
                 WHEN 30 THEN ASSIGN tt-ordem.numero-ordem = trim(tt-esp-docto.tipo)
                                     tt-ordem.tipo-movto   = STRING(movto-estoq.nro-docto).               
                 WHEN 33 THEN ASSIGN tt-ordem.numero-ordem = trim(tt-esp-docto.tipo) 
                                     tt-ordem.tipo-movto   = STRING(movto-estoq.nro-docto).               
             END CASE.                                                                                                              
             CASE movto-estoq.esp-docto:                                                                                            
                 WHEN 2 THEN ASSIGN tt-ordem.nr-nota-fis = STRING(movto-estoq.nro-docto).                                           
                 WHEN 6 THEN ASSIGN tt-ordem.nr-nota-fis = STRING(movto-estoq.nro-docto).                                           
                 WHEN 15 THEN ASSIGN tt-ordem.nr-nota-fis = STRING(movto-estoq.nro-docto).                                          
                 WHEN 28 THEN ASSIGN tt-ordem.nr-nota-fis = STRING(movto-estoq.nro-docto).                                          
                 WHEN 30 THEN ASSIGN tt-ordem.nr-nota-fis = STRING(movto-estoq.nro-docto).                                          
                 WHEN 33 THEN ASSIGN tt-ordem.nr-nota-fis = STRING(movto-estoq.nro-docto).                                          
             END CASE.                                                                                                              

            /* quantidade */
            ASSIGN tt-ordem.quantidade   = fc-qtoc(movto-estoq.quantidade).
            ASSIGN tt-ordem.fator-conv = fc-qtoc(1).

            /* data requisicao */
            ASSIGN tt-ordem.data-requisicao = "".

            IF tt-ordem.data-requisicao = "" THEN
                ASSIGN tt-ordem.data-requisicao = fc-mdyy(movto-estoq.dt-trans).
                ASSIGN tt-ordem.data-aprov-req  = fc-mdyy(movto-estoq.dt-trans).

            /* data da compra */
            ASSIGN tt-ordem.data-compra      = fc-mdyy(movto-estoq.dt-trans).
            ASSIGN tt-ordem.data-atualiz     = fc-mdyy(movto-estoq.dt-trans).
            ASSIGN tt-ordem.data-recebimento = fc-mdyy(movto-estoq.dt-trans).
            ASSIGN tt-ordem.data-disponivel  = fc-mdyy(movto-estoq.dt-trans).
                
            /* Para requisicao enviar Departamento padr∆o */
            ASSIGN tt-ordem.fornecedor = tt-item.departamento.
            
            /*Cleber*/
            assign c-trans = IF movto-estoq.tipo-trans = 1 THEN "Ent" ELSE "Sai".
            ASSIGN tt-ordem.tipo-trans = c-trans.

/*             IF tt-ordem.numero-ordem <> tt-param.c-esp-ini AND tt-ordem.numero-ordem <> tt-param.c-esp-fin THEN */
/*                 DELETE tt-ordem.                                                                                */
            l-delete = NO.

            CASE string(tt-ordem.numero-ordem):                                                                                            
                 WHEN  "ACA"  then  if tt-param.l-ACA   then  l-delete = no.   else l-delete = YES .                               
                 WHEN  "DIV"  then  if tt-param.L-DIV   then  l-delete = no.   else l-delete = yes .                               
                 WHEN  "NU2"  then  if tt-param.L-NU2   then  l-delete = no.   else l-delete = yes .                              
                 WHEN  "IPL"  then  if tt-param.L-IPL   then  l-delete = no.   else l-delete = yes .                              
                 WHEN  "NFE"  then  if tt-param.L-NFE   then  l-delete = no.   else l-delete = yes .                              
                 WHEN  "RCS"  then  if tt-param.L-RCS   then  l-delete = no.   else l-delete = yes .    
                 when  "RRQ"  then  if tt-param.L-RRQ   then  l-delete = no.   else l-delete = yes .    
                 when  "act"  then  if tt-param.l-act   then  l-delete = no.   else l-delete = yes .    
                 when  "DRM"  then  if tt-param.L-DRM   then  l-delete = no.   else l-delete = yes .    
                 when  "NU3"  then  if tt-param.L-NU3   then  l-delete = no.   else l-delete = yes .    
                 when  "MOB"  then  if tt-param.L-MOB   then  l-delete = no.   else l-delete = yes .    
                 when  "NFS"  then  if tt-param.L-NFS   then  l-delete = no.   else l-delete = yes .    
                 when  "RDD"  then  if tt-param.L-RDD   then  l-delete = no.   else l-delete = yes .    
                 when  "STR"  then  if tt-param.L-STR   then  l-delete = no.   else l-delete = yes .    
                 when  "NU1"  then  if tt-param.L-NU1   then  l-delete = no.   else l-delete = yes .    
                 when  "EAC"  then  if tt-param.L-EAC   then  l-delete = no.   else l-delete = yes .    
                 when  "NU4"  then  if tt-param.L-NU4   then  l-delete = no.   else l-delete = yes .    
                 when  "NC"   then  if tt-param.L-NC    then  l-delete = no.   else l-delete = yes .    
                 when  "NFT"  then  if tt-param.L-NFT   then  l-delete = no.   else l-delete = yes .    
                 when  "REQ"  then  if tt-param.L-REQ   then  l-delete = no.   else l-delete = yes .    
                 when  "TRA"  then  if tt-param.L-TRA   then  l-delete = no.   else l-delete = yes .    
                 when  "DD"   then  if tt-param.L-DD    then  l-delete = no.   else l-delete = yes .    
                 when  "EGF"  then  if tt-param.L-EGF   then  l-delete = no.   else l-delete = yes .    
                 when  "ICM"  then  if tt-param.L-ICM   then  l-delete = no.   else l-delete = yes .    
                 when  "NF"   then  if tt-param.L-NF    then  l-delete = no.   else l-delete = yes .    
                 when  "NUS"  then  if tt-param.L-NUS   then  l-delete = no.   else l-delete = yes .    
                 when  "RFS"  then  if tt-param.L-RFS   then  l-delete = no.   else l-delete = yes .    
                 when  "ZZZ"  then  if tt-param.L-ZZZ   then  l-delete = no.   else l-delete = yes .    
                 when  "DEV"  then  if tt-param.L-DEV   then  l-delete = no.   else l-delete = yes .    
                 when  "BEM"  then  if tt-param.L-BEM   then  l-delete = no.   else l-delete = yes .    
                 when  "INV"  then  if tt-param.L-INV   then  l-delete = no.   else l-delete = yes .    
                 when  "NFD"  then  if tt-param.L-NFD   then  l-delete = no.   else l-delete = yes .    
                 when  "REF"  then  if tt-param.L-REF   then  l-delete = no.   else l-delete = yes .    
                 when  "RM"   then  if tt-param.L-RM    then  l-delete = no.   else l-delete = yes .    
                 WHEN  "SOB"  then  if tt-param.L-SOB   then  l-delete = no.   else l-delete = yes .    

             END CASE.
             IF l-delete THEN
                DELETE tt-ordem. 

        END.

        ASSIGN c-arquivo-saida = c-diretorio + "\ItemRequisicao2.txt".
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"\","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        OUTPUT STREAM str TO VALUE(c-arquivo-saida).

        PUT STREAM str "ItemNum;StorageLoc;Tp;TypeMov;PONUM;VendorName;Qty;OrderUnit;UnitCost;ConversionFactor;PurchaseRecDate;PurchaseRecApprovalDate;PurchaseDate;ReleaseDate;ReceiptDate;AvailableDate;BlanketPOIndcator;NumDocto" SKIP.
        FOR EACH tt-ordem NO-LOCK:
            PUT STREAM str                                                                                                      
            /*   tt-ordem.origem            ";" */                                                                              
              tt-ordem.it-codigo         ";"   FORMAT "x(16)"                                                                                 
              tt-ordem.cod-estabel       ";"   /* storageloc */
              tt-ordem.tipo-trans        ";"
              tt-ordem.numero-ordem      ";"  
              tt-ordem.tipo-movto        FORMAT "x(12)"      ";" /* Leo - Kraft - 21/12/10 */
              tt-ordem.fornecedor        FORMAT "x(60)" ";"                                                                                    
              dec(tt-ordem.quantidade)   FORMAT "->>,>>>,>>>,>>>9.9999" ";"   /*DEC*/ /* qt recebida ou se PO aberto qt solicitada - recebida */               
              tt-ordem.un                ";"   /* unidade de medida */                                                          
              
              /*Leo - Kraft - 21/12/10 */
              round(dec(tt-ordem.custo-unit) / dec(tt-ordem.quantidade),4)
                        FORMAT "->>,>>>,>>>,>>>9.9999" ";"    /*DEC*/ /* preco unitario */                                                     
              
              dec(tt-ordem.fator-conv)   FORMAT "->>,>>>,>>>,>>>9.9999" ";"   /*DEC*/ /* fator conversao */                                                    
              tt-ordem.data-requisicao   ";"   /*DATE*/ /* data de requisiªao */                                                
              tt-ordem.data-aprov-req    ";"   /*DATE*/ /* data de aprovaªío da requisiªao */                                   
              tt-ordem.data-compra       ";"   /*DATE*/ /* data de compra */                                                    
              tt-ordem.data-atualizacao  ";"   /*DATE*/ /* data de atualizacao */                                               
              tt-ordem.data-recebimento  ";"   /*DATE*/ /* data de disponibilidade do produto */                                
              tt-ordem.data-disponivel   ";"   /*DATE*/ /* data de disponibilidade do produto */                                
              tt-ordem.blanket           ";"   /*  */                                                                           
              tt-ordem.nr-nota-fis  FORMAT "x(12)"     SKIP. /* Leo-Kraft- 21/12/10 */

        END.
        OUTPUT STREAM str CLOSE.
        EMPTY TEMP-TABLE tt-ordem.
    END.

    IF tt-param.donotexportitems THEN DO:
        ASSIGN c-arquivo-saida = c-diretorio + "\donotexportotems2.txt".
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"\","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        ASSIGN c-arquivo-saida = replace(c-arquivo-saida,"//","/").
        OUTPUT STREAM str TO VALUE(c-arquivo-saida).

        PUT STREAM str "ItemNum" SKIP.
        FOR EACH tt-item NO-LOCK
            WHERE tt-item.movto = NO:
            PUT STREAM str tt-item.it-codigo " ;" FORMAT "x(16)" SKIP.
        END.
        OUTPUT STREAM str CLOSE.
        EMPTY TEMP-TABLE tt-ordem.
    END.

END.

