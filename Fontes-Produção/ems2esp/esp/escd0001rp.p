&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description : Relat¢rio Analitico/SintÇtico - movimento de estoque

    Author(s)   : Wellington Rodrigo / Augusto guimar∆es
    Created     : 09/08/2010
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
def buffer empresa for ems2cadme.empresa.
def buffer ccusto  for ems5.ccusto.

/* ***************************  Definitions  ************************** */

{include/i-prgvrs.i escd0001rp 2.06.00.000} 
{utp/ut-glob.i} 

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD dt-trans-ini     LIKE movto-estoq.dt-trans
    FIELD dt-trans-fim     LIKE movto-estoq.dt-trans
    FIELD ct-codigo-ini    LIKE movto-estoq.ct-codigo
    FIELD ct-codigo-fim    LIKE movto-estoq.ct-codigo
    FIELD sc-codigo-ini    LIKE movto-estoq.sc-codigo
    FIELD sc-codigo-fim    LIKE movto-estoq.sc-codigo
    FIELD cd-equipto-ini   LIKE ord-manut.cd-equipto
    FIELD cd-equipto-fim   LIKE ord-manut.cd-equipto
    FIELD it-codigo-ini    LIKE movto-estoq.it-codigo
    FIELD it-codigo-fim    LIKE movto-estoq.it-codigo
    FIELD i-impressao      AS INTEGER
    FIELD tipo-trans       AS CHAR
    FIELD i-sql            AS INTEGER
    FIELD dir-sql          AS CHAR
    FIELD dir-excel        AS CHAR.

DEFINE TEMP-TABLE tt-relatorio NO-UNDO
    FIELD dt-trans         LIKE movto-estoq.dt-trans
    FIELD sc-codigo        LIKE movto-estoq.sc-codigo
    FIELD descricao        LIKE centro-custo.descricao
    FIELD ct-codigo        LIKE movto-estoq.ct-codigo
    FIELD titulo           LIKE cta_ctbl.des_tit_ctbl
    FIELD it-codigo        LIKE movto-estoq.it-codigo
    FIELD desc-item        LIKE ITEM.desc-item
    FIELD quantidade       LIKE movto-estoq.quantidade
    FIELD d-valor-tot      AS DECIMAL
    FIELD d-valor-uni      AS DECIMAL
    FIELD c-tipo-trans     AS CHAR
    FIELD tipo-trans       AS CHAR
    FIELD nro-docto        LIKE movto-estoq.nro-docto
    FIELD nome-emit        LIKE emitente.nome-emit
    FIELD dt-postagem      AS DATE
    FIELD dt-processamento AS DATE
    FIELD un               LIKE movto-estoq.un
    FIELD nr-ord-produ     LIKE movto-estoq.nr-ord-produ
    FIELD cd-equipto       LIKE ord-manut.cd-equipto
    FIELD narrativa        LIKE it-requisicao.narrativa
    INDEX conta-ctbl       sc-codigo ASC ct-codigo ASC it-codigo ASC d-valor-tot DESC.

DEF TEMP-TABLE tt-expedicao
    FIELD dt-emis-nota LIKE nota-fiscal.dt-emis-nota
    FIELD nr-nota-fis  LIKE nota-fiscal.nr-nota-fis
    FIELD placa        LIKE nota-fiscal.placa
    FIELD tara-camin   AS INTEGER
    FIELD nome-transp  LIKE nota-fiscal.nome-transp
    FIELD cod-des-merc LIKE nota-fiscal.cod-des-merc
    FIELD peso-bru-tot LIKE nota-fiscal.peso-bru-tot
    FIELD peso-liq-tot LIKE nota-fiscal.peso-liq-tot
    FIELD vl-tot-nota  LIKE nota-fiscal.vl-tot-nota
    INDEX i-nota-fis   nr-nota-fis.

define temp-table tt-digita no-undo
    FIELD nat-operacao     LIKE nota-fiscal.nat-operacao
    INDEX id nat-operacao.

DEFINE temp-table tt-raw-digita
    field raw-digita as raw.

DEF INPUT PARAM raw-param as raw no-undo.
DEF INPUT PARAM table for tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param to tt-param.

/* Ativar somente quando tiver (tt-digita)      */
FOR EACH tt-raw-digita:                     
    create tt-digita.                       
    RAW-TRANSFER tt-raw-digita.raw-digita to tt-digita.
END.


/*
DEF TEMP-TABLE tt-montagem NO-UNDO
    FIELD it-codigo LIKE ITEM.it-codigo
    FIELD desc-item LIKE ITEM.desc-item 
    INDEX codigo
          it-codigo.
*/
{include/i-rpvar.i} 

def new global shared var v_cdn_empres_usuar like mguni.empresa.ep-codigo no-undo.

DEF VAR h_acomp_rp         AS HANDLE     NO-UNDO.
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook         AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet        AS COM-HANDLE NO-UNDO.
DEF VAR i-linha            AS INT        NO-UNDO.
DEF VAR c-tipo-trans       AS char       NO-UNDO.
DEF VAR d-valor-tot        AS DECIMAL  FORMAT "->>,>>>,>>>,>>9.99"   NO-UNDO.
DEF VAR d-valor-uni        AS DECIMAL  FORMAT "->>,>>>,>>>,>>9.99"   NO-UNDO.

DEF VAR arq-log            AS CHAR       NO-UNDO.

DEF VAR arq-mmic-bal       AS CHAR       NO-UNDO.
DEF VAR arq-mmic-pre       AS CHAR       NO-UNDO.
DEF VAR arq-mmic-mov       AS CHAR       NO-UNDO.
DEF VAR arq-mmic-exp       AS CHAR       NO-UNDO.
DEF VAR arq-mmic-con       AS CHAR       NO-UNDO.
DEF VAR arq-mmic-om        AS CHAR       NO-UNDO.

DEF VAR conta-ctbl-aux     AS CHAR       NO-UNDO.
DEF VAR d-valor-tot-aux    AS DECIMAL  FORMAT "->>,>>>,>>>,>>9.99"   NO-UNDO.
DEF VAR c-quantidade       AS CHAR       NO-UNDO.
DEF VAR c-valor-tot        AS CHAR       NO-UNDO.
DEF VAR c-valor-uni        AS CHAR       NO-UNDO.
DEF VAR dt-it-per          AS DATE       NO-UNDO.
DEF VAR c-nat-operacao     AS CHAR       NO-UNDO.
DEF VAR arq-excel          AS CHAR       NO-UNDO.
DEF VAR i-arq              AS INT        NO-UNDO.

DEF BUFFER b-mab-item-abastec FOR mab-item-abastec.

DEF STREAM s-log.

DEF STREAM s-bal.
DEF STREAM s-pre.
DEF STREAM s-mov.
DEF STREAM s-exp.
DEF STREAM s-con.
DEF STREAM s-om.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15.04
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

find first empresa no-lock no-error.

ASSIGN c-programa     = "escd0001rp"
       c-versao       = "2.04"
       c-revisao      = "00.000"
       c-empresa      =  empresa.razao-social
       c-sistema      = "Especifico"
       c-titulo-relat = "".
/*    
form header
    "Codigo           Descricao"            AT   1
    FILL("-",132) FORMAT "x(132)"
    with stream-io width 132 no-labels no-box page-top frame f-cabec-1.
*/
                   
/*
{include\i-rpout.i &PAGESIZE=0}
{include\i-rpcab.i}
*/

RUN utp/ut-acomp.p PERSISTENT SET h_acomp_rp.
RUN pi-inicializar IN h_acomp_rp (INPUT "Criando Relat¢rio").


/* Se tiver uma montagem de temp-table */
/* RUN pi-monta. */

ASSIGN /*arq-log      = "C:\Temp\DATASUL-to-SQL.sql"*/
       arq-mmic-bal = dir-sql + "\SQL-mmic-bal.txt"
       arq-mmic-pre = dir-sql + "\SQL-mmic-pre.txt"
       arq-mmic-mov = dir-sql + "\SQL-mmic-mov.txt"
       arq-mmic-exp = dir-sql + "\SQL-mmic-exp.txt"
       arq-mmic-con = dir-sql + "\SQL-mmic-con.txt"
       arq-mmic-om  = dir-sql + "\SQL-mmic-om.txt".


IF tt-param.i-sql = 2 THEN /* EXPEDIÄ∂O */
DO:
    RUN pi-imprime.
END.
ELSE /* OUTROS */
DO:
    RUN pi-expedicao.
END.


run pi-finalizar in h_acomp_rp.

{include\i-rpclo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-analitico) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-analitico Procedure 
PROCEDURE pi-analitico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*L¢gica dos parametros de filtro do relat¢rio*/
FOR EACH tt-relatorio:
    DELETE tt-relatorio.
END.

find first plano_ccusto no-lock
     where plano_ccusto.cod_empresa     = v_cdn_empres_usuar
       and plano_ccusto.dat_inic_valid <= today
       and plano_ccusto.dat_fim_valid  >= today no-error.

FOR EACH movto-estoq  USE-INDEX data-item 
     WHERE LOOKUP({ininc/i03in218.i 4 movto-estoq.esp-docto}, tt-param.tipo-trans) <> 0 
       AND movto-estoq.dt-trans >= tt-param.dt-trans-ini  
       AND movto-estoq.dt-trans  <= tt-param.dt-trans-fim  
       AND movto-estoq.ct-codigo >= tt-param.ct-codigo-ini 
       AND movto-estoq.ct-codigo <= tt-param.ct-codigo-fim 
       AND movto-estoq.sc-codigo >= tt-param.sc-codigo-ini 
       AND movto-estoq.sc-codigo <= tt-param.sc-codigo-fim
       AND movto-estoq.it-codigo >= tt-param.it-codigo-ini 
       AND movto-estoq.it-codigo <= tt-param.it-codigo-fim NO-LOCK:

    /*L¢gica para pegar os campos que n∆o s∆o da tabela movto-estoq*/

    /*Centro Custo*/
    find first ccusto no-lock
         where ccusto.cod_empresa      = plano_ccusto.cod_empresa
           and ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
           and ccusto.cod_ccusto       = movto-estoq.sc-codigo no-error.

    /*Item*/
    FIND FIRST ITEM
         WHERE ITEM.it-codigo = movto-estoq.it-codigo  NO-LOCK NO-ERROR.

    /*Conta Contabil*/
    find first cta_ctbl no-lock
         where cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
           and cta_ctbl.cod_cta_ctbl       = movto-estoq.ct-codigo no-error.

    /*Emitente*/
    FIND FIRST emitente
         WHERE emitente.cod-emitente = movto-estoq.cod-emitente NO-LOCK NO-ERROR.

    /*ordem manutená∆o*/
    FIND FIRST ord-manut
         WHERE ord-manut.nr-ord-produ = movto-estoq.nr-ord-produ /*AND /*ANTERIORMENTE:  = INTEGER (movto-estoq.nro-docto)  */
               ord-manut.cd-equipto  >= tt-param.cd-equipto-ini  AND 
               ord-manut.cd-equipto  <= tt-param.cd-equipto-fim*/  NO-LOCK NO-ERROR.
    
    RUN pi-acompanhar IN h_acomp_rp (INPUT "Lendo Movto Estoque: " + STRING(movto-estoq.nr-trans)).


    /*preenchendo o relat¢rio com texto, pois no banco est∆o como numeros 1 e 2 respectivamente*/
    IF movto-estoq.tipo-trans = 1 THEN
        ASSIGN c-tipo-trans = "Entrada".
    ELSE IF movto-estoq.tipo-trans = 2 THEN
        ASSIGN c-tipo-trans = "Sa°da".

    /*preenchimento dos valores*/
    ASSIGN dt-it-per = DATE("01/" + STRING(MONTH(movto-estoq.dt-trans)) +
                              "/" + STRING(YEAR(movto-estoq.dt-trans))).
    ASSIGN dt-it-per = dt-it-per - 1.
    FIND FIRST pr-it-per WHERE pr-it-per.cod-estabel = movto-estoq.cod-estabel
                           AND pr-it-per.it-codigo   = movto-estoq.it-codigo
                           AND pr-it-per.periodo     = dt-it-per NO-LOCK NO-ERROR.
    IF AVAILABLE pr-it-per THEN
    DO:
        ASSIGN d-valor-uni = pr-it-per.val-unit-mat-m[1] +
                             pr-it-per.val-unit-mob-m[1] +
                             pr-it-per.val-unit-ggf-m[1].
    END.
    ELSE
    DO:
        ASSIGN d-valor-uni = movto-estoq.valor-mat-m[1] +
                             movto-estoq.valor-mob-m[1] +
                             movto-estoq.valor-ggf-m[1].
    END.
    IF movto-estoq.quantidade <> 0 THEN
    DO:
        ASSIGN d-valor-tot = (d-valor-uni * movto-estoq.quantidade).
    END.
    ELSE
    DO:
        ASSIGN d-valor-tot = 0.
    END.


    CREATE tt-relatorio.

    ASSIGN tt-relatorio.dt-trans         = movto-estoq.dt-trans        
           tt-relatorio.sc-codigo        = movto-estoq.sc-codigo 
           tt-relatorio.descricao        = IF AVAILABLE ccusto THEN ccusto.des_tit_ctbl ELSE "N/CADASTRADO" 
           tt-relatorio.ct-codigo        = movto-estoq.ct-codigo 
           tt-relatorio.titulo           = IF AVAILABLE cta_ctbl THEN cta_ctbl.des_tit_ctbl ELSE "N/CADASTRADO" 
           tt-relatorio.it-codigo        = movto-estoq.it-codigo 
           tt-relatorio.desc-item        = IF AVAILABLE ITEM THEN (ITEM.desc-item) ELSE "N/CADASTRADO" 
           tt-relatorio.quantidade       = movto-estoq.quantidade 
           tt-relatorio.d-valor-tot      = d-valor-tot 
           tt-relatorio.d-valor-uni      = d-valor-uni
           tt-relatorio.c-tipo-trans     = c-tipo-trans 
           tt-relatorio.tipo-trans       = {ininc/i03in218.i 4 movto-estoq.esp-docto}
           tt-relatorio.nro-docto        = movto-estoq.nro-docto 
           tt-relatorio.dt-postagem      = TODAY
           tt-relatorio.dt-processamento = ?
           tt-relatorio.un               = movto-estoq.un.


    FIND FIRST ordem-compra WHERE ordem-compra.numero-ordem = movto-estoq.numero-ordem NO-LOCK NO-ERROR.
    IF AVAILABLE ordem-compra THEN
    DO:
        FIND FIRST it-requisicao WHERE it-requisicao.nr-requisicao = ordem-compra.nr-requisicao AND
                                       it-requisicao.sequencia     = ordem-compra.sequencia     AND
                                       it-requisicao.it-codigo     = ordem-compra.it-codigo NO-LOCK NO-ERROR.
        IF AVAILABLE it-requisicao THEN
        DO:
            IF SUBSTRING(movto-estoq.it-codigo, 1,1) = "9" THEN
            DO:
                ASSIGN tt-relatorio.desc-item = it-requisicao.narrativa. 
            END.
        END.
    END.

    ASSIGN tt-relatorio.desc-item = REPLACE (tt-relatorio.desc-item, CHR(10), " ").


    /*Fornecedor (emitente) s¢ existe para notas fiscais*/
    IF movto-estoq.esp-docto = 18 OR       /* NC */
       movto-estoq.esp-docto = 19 OR       /* NF */  
       movto-estoq.esp-docto = 20 OR       /* NFD */        
       movto-estoq.esp-docto = 21 OR       /* NFE */        
       movto-estoq.esp-docto = 22 OR       /* NFS */        
       movto-estoq.esp-docto = 23 THEN     /* NFT */  
    DO:
        IF AVAILABLE emitente THEN
        DO:
            ASSIGN tt-relatorio.nome-emit = emitente.nome-emit.
        END.
    END.
    
    /*Ordem de Manutená∆o s¢ existe para DEV, DRM, RDD, REQ e RM*/
    IF movto-estoq.esp-docto = 5 OR        /* DEV */  
       movto-estoq.esp-docto = 7 OR        /* DRM */
       movto-estoq.esp-docto = 27 OR       /* RDD */
       movto-estoq.esp-docto = 28 OR       /* REQ */       
       movto-estoq.esp-docto = 30 THEN     /* RM */
    DO:
        
        ASSIGN tt-relatorio.nr-ord-produ = movto-estoq.nr-ord-produ.  /* ANTERIORMENTE: = INTEGER (movto-estoq.nro-docto) */
        IF AVAIL ord-manut THEN
        DO:
            ASSIGN tt-relatorio.cd-equipto = ord-manut.cd-equipto.
            
            RUN pi-om (INPUT 1).
        END.
        ELSE
        DO:
            ASSIGN tt-relatorio.cd-equipto = " ".
        END.
    END.
    ELSE
    DO:
        ASSIGN tt-relatorio.nr-ord-produ = ?.
    END.

    IF NOT AVAIL ord-manut THEN
    DO:
        FIND FIRST mmv-ord-manut WHERE mmv-ord-manut.nr-ord-produ = movto-estoq.nr-ord-produ NO-LOCK NO-ERROR.
        IF AVAILABLE mmv-ord-manut THEN
        DO:
            ASSIGN tt-relatorio.cd-equipto = mmv-ord-manut.cod-eqpto.

            RUN pi-om (INPUT 2).
        END.
    END.

    FIND FIRST mab-item-abastec WHERE mab-item-abastec.nr-trans = movto-estoq.nr-trans NO-LOCK NO-ERROR.
    IF AVAILABLE mab-item-abastec THEN
    DO:
        PUT STREAM s-con UNFORMATTED STRING(DAY(movto-estoq.dt-trans), "99") + "/" +
                                     STRING(MONTH(movto-estoq.dt-trans), "99") + "/" +
                                     STRING(YEAR(movto-estoq.dt-trans), "9999") + "|".
            
        FIND FIRST mab-eqpto WHERE mab-eqpto.ep-codigo = mab-item-abastec.ep-codigo AND
                                   mab-eqpto.cod-eqpto = mab-item-abastec.cod-eqpto NO-LOCK NO-ERROR.
        IF AVAILABLE mab-eqpto THEN
        DO:
            PUT STREAM s-con UNFORMATTED TRIM(STRING(mab-eqpto.cod-model)) + "|".
        END.
        ELSE
        DO:
            PUT STREAM s-con UNFORMATTED " " + "|".
        END.

        PUT STREAM s-con UNFORMATTED TRIM(STRING(mab-item-abastec.cod-eqpto)) + "|".
        PUT STREAM s-con UNFORMATTED " " + "|". /* OPERADOR (n∆o informado) */

        ASSIGN d-valor-tot-aux = 0.
        FOR EACH mab-abastec-lubrific WHERE mab-abastec-lubrific.cod-eqpto = mab-item-abastec.cod-eqpto AND
                                            mab-abastec-lubrific.dat-movto = movto-estoq.dt-trans NO-LOCK:
            
            FIND FIRST b-mab-item-abastec WHERE b-mab-item-abastec.num-docto = mab-abastec-lubrific.num-docto NO-LOCK NO-ERROR.
            IF AVAILABLE b-mab-item-abastec THEN
            DO:
                ASSIGN d-valor-tot-aux = d-valor-tot-aux + b-mab-item-abastec.val-quant.
            END.
        END.
        

        PUT STREAM s-con UNFORMATTED TRIM(STRING(d-valor-tot-aux), ">>>>>>>>9.9999") + "|".
        PUT STREAM s-con UNFORMATTED STRING(DAY(TODAY), "99") + "/" +
                                     STRING(MONTH(TODAY), "99") + "/" +
                                     STRING(YEAR(TODAY), "9999") + "|".
        PUT STREAM s-con UNFORMATTED " ". /* data_processamento */
        PUT STREAM s-con SKIP.
    END.

    
END.


/*preenchendo o Excel*/
ASSIGN i-linha = 2.

FOR EACH tt-relatorio USE-INDEX conta-ctbl:
    
    ASSIGN conta-ctbl-aux  = tt-relatorio.sc-codigo + tt-relatorio.ct-codigo.
    
    RUN pi-acompanhar IN h_acomp_rp (INPUT "Excel - Conta Contabil: " + conta-ctbl-aux).

    PUT STREAM s-mov UNFORMATTED STRING(DAY(tt-relatorio.dt-trans), "99") + "/" +
                                 STRING(MONTH(tt-relatorio.dt-trans), "99") + "/" +
                                 STRING(YEAR(tt-relatorio.dt-trans), "9999") + "|".
    
    chWorkSheet:Range("A" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("A" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.sc-codigo).
    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.sc-codigo)) + "|".

    chWorkSheet:Range("B" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("B" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.descricao).
    chWorkSheet:range("B" + STRING(i-linha)):WrapText = TRUE.
    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.descricao)) + "|".

    chWorkSheet:Range("C" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("C" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.ct-codigo).
    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.ct-codigo)) + "|".

    chWorkSheet:Range("D" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("D" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.titulo).
    chWorkSheet:range("D" + STRING(i-linha)):WrapText = TRUE.
    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.titulo)) + "|".
     
    chWorkSheet:Range("E" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("E" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.it-codigo).
    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.it-codigo)) + "|".

    chWorkSheet:Range("F" + STRING(i-linha) + ":G" + STRING(i-linha)):merge.             
    chWorkSheet:Range("F" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("F" + STRING(i-linha)):VALUE =  TRIM(tt-relatorio.desc-item).
    chWorkSheet:range("F" + STRING(i-linha)):WrapText = TRUE.
    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.desc-item)) + "|".

    chWorkSheet:Range("H" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.un).
    
    chWorkSheet:Range("I" + STRING(i-linha)):NumberFormat = "###.###.###.##0,0000".
    chWorkSheet:Range("I" + STRING(i-linha)):VALUE = tt-relatorio.quantidade.
    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.quantidade, ">>>>>>>>9.9999")) + "|".

    chWorkSheet:Range("J" + STRING(i-linha)):NumberFormat = "###.###.###.##0,00".
    chWorkSheet:Range("J" + STRING(i-linha)):VALUE = tt-relatorio.d-valor-tot.
    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.d-valor-tot, ">>>>>>>>9.99")) + "|".

    chWorkSheet:Range("K" + STRING(i-linha)):NumberFormat = "###.###.###.##0,00".
    chWorkSheet:Range("K" + STRING(i-linha)):VALUE = tt-relatorio.d-valor-uni.
    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.d-valor-uni, ">>>>>>>>9.99")) + "|".
    
    chWorkSheet:Range("M" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("M" + STRING(i-linha)):VALUE = tt-relatorio.c-tipo-trans.
    PUT STREAM s-mov UNFORMATTED TRIM(SUBSTRING(tt-relatorio.c-tipo-trans, 1, 1)) + "|".

    chWorkSheet:Range("L" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("L" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.tipo-trans).
    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.tipo-trans)) + "|".
     
    chWorkSheet:Range("N" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("N" + STRING(i-linha)):VALUE = tt-relatorio.nro-docto.
    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.nro-docto)) + "|".

    chWorkSheet:Range("O" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("O" + STRING(i-linha)):VALUE = tt-relatorio.nome-emit.
    chWorkSheet:range("O" + STRING(i-linha)):WrapText = TRUE.
    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.nome-emit)) + "|".

    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.un)) + "|".
    
    chWorkSheet:Range("P" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("P" + STRING(i-linha)):VALUE = tt-relatorio.nr-ord-produ.

    chWorkSheet:Range("Q" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("Q" + STRING(i-linha)):VALUE = tt-relatorio.cd-equipto.
    PUT STREAM s-mov UNFORMATTED TRIM(STRING(tt-relatorio.cd-equipto)) + "|".

    FIND FIRST equipto WHERE equipto.cd-equipto = tt-relatorio.cd-equipto NO-LOCK NO-ERROR.
    IF AVAILABLE equipto THEN
    DO:
        PUT STREAM s-mov UNFORMATTED TRIM(STRING(equipto.descricao)) + "|". 
    END.
    ELSE
    DO:
        PUT STREAM s-mov UNFORMATTED " " + "|".
    END.

    PUT STREAM s-mov UNFORMATTED STRING(DAY(TODAY), "99") + "/" +
                                 STRING(MONTH(TODAY), "99") + "/" +
                                 STRING(YEAR(TODAY), "9999") + "|".
    PUT STREAM s-mov UNFORMATTED " ". /* data_processamento */
    PUT STREAM s-mov SKIP.

    ASSIGN i-linha = i-linha + 1.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-arq-saida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-arq-saida Procedure 
PROCEDURE pi-arq-saida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF SEARCH(arq-mmic-bal) = ? THEN
DO:
    /* Arquivo n∆o encontrado */
    OUTPUT STREAM s-bal TO VALUE(arq-mmic-bal) APPEND NO-CONVERT .
    PUT STREAM s-bal UNFORMATTED "DataLancamento|NumCentroCusto|DesCentroCusto|" + 
                                 "NumContaContabil|DesContaContabil|ValorCusto|" + 
                                 "DataPostagem|DataProcessamento".
    PUT STREAM s-bal SKIP.
END.
ELSE
DO:
    /* Arquivo encontrado */
    OUTPUT STREAM s-bal TO VALUE(arq-mmic-bal) APPEND NO-CONVERT.
END.

IF SEARCH(arq-mmic-pre) = ? THEN
DO:
    /* Arquivo n∆o encontrado */
    OUTPUT STREAM s-pre TO VALUE(arq-mmic-pre) APPEND NO-CONVERT.
    
    PUT STREAM s-pre UNFORMATTED "DataLancamento|NumCentroCusto|DesCentroCusto|" + 
                                 "NumContaContabil|DesContaContabil|ValorCusto|" + 
                                 "DataPostagem|DataProcessamento".
    PUT STREAM s-pre SKIP.
END.
ELSE
DO:
    /* Arquivo encontrado */
    OUTPUT STREAM s-pre TO VALUE(arq-mmic-pre) APPEND NO-CONVERT.
END.

IF SEARCH(arq-mmic-mov) = ? THEN
DO:
    /* Arquivo n∆o encontrado */
    OUTPUT STREAM s-mov TO VALUE(arq-mmic-mov) APPEND NO-CONVERT.
    
    PUT STREAM s-mov UNFORMATTED "DataLancamento|NumCentroCusto|DesCentroCusto|" + 
                                 "NumContaContabil|DesContaContabil|NumItem|" +
                                 "DescItem|QuantidadeItem|ValorTotalItem|" +
                                 "ValorMedioItem|EntradaSaida|TipoRequisicao|" +
                                 "NumDocumento|DescFornecedor|UnidadeMedida|" +
                                 "CodEquipamento|DescEquipamento|DataPostagem|" +
                                 "DataProcessamento".
    PUT STREAM s-mov SKIP.
END.
ELSE
DO:
    /* Arquivo encontrado */
    OUTPUT STREAM s-mov TO VALUE(arq-mmic-mov) APPEND NO-CONVERT.
END.

IF SEARCH(arq-mmic-con) = ? THEN
DO:
    /* Arquivo n∆o encontrado */
    OUTPUT STREAM s-con TO VALUE(arq-mmic-con) APPEND NO-CONVERT.
    
    PUT STREAM s-con UNFORMATTED "Data|Frota|Equipamento|Operador|" +
                                 "ConsumoCombustivel|DataPostagem|Dataprocessamento".
    PUT STREAM s-con SKIP.
END.
ELSE
DO:
    /* Arquivo encontrado */
    OUTPUT STREAM s-con TO VALUE(arq-mmic-con) APPEND NO-CONVERT.
END.

IF SEARCH(arq-mmic-om) = ? THEN
DO:
    /* Arquivo n∆o encontrado */
    OUTPUT STREAM s-om TO VALUE(arq-mmic-om) APPEND NO-CONVERT.
    
    PUT STREAM s-om UNFORMATTED "DataLancamento2|NumCentroCusto2|DesCentroCusto2|" + 
                                "NumContaContabil2|DesContaContabil2|NumItemContaContabil2|" +
                                "Tarefa2|DescItemContaContabil2|QuantidadeItem2|ValorTotalItem2|" +
                                "ValorMedioItem2|EntradaSaida2|TipoRequisicao2|" +
                                "NumDocumento2|NumOM2|DescFornecedor2|UnidadeMedida2|" +
                                "CodEquipamento2|DescEquipamento2|DataPostagem2|" +
                                "DataProcessamento2".
    PUT STREAM s-om SKIP.
END.
ELSE
DO:
    /* Arquivo encontrado */
    OUTPUT STREAM s-om TO VALUE(arq-mmic-om) APPEND NO-CONVERT.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-arq-saida-exp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-arq-saida-exp Procedure 
PROCEDURE pi-arq-saida-exp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF SEARCH(arq-mmic-exp) = ? THEN
DO:
    /* Arquivo n∆o encontrado */
    OUTPUT STREAM s-exp TO VALUE(arq-mmic-exp) APPEND NO-CONVERT.
    
    PUT STREAM s-exp UNFORMATTED "DataNF|" +
                                 "NumeroNF|PlacaCaminhao|TaraCaminhao|" +
                                 "TransportadorProduto|DestinoProduto|PesoBruto|" +
                                 "PesoLiquido|ValorNR|DataPostagem|" +
                                 "DataProcessamento".
    PUT STREAM s-exp SKIP.
END.
ELSE
DO:
    /* Arquivo encontrado */
    OUTPUT STREAM s-exp TO VALUE(arq-mmic-exp) APPEND NO-CONVERT.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-expedicao) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-expedicao Procedure 
PROCEDURE pi-expedicao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN pi-arq-saida-exp.

RUN pi-monta.

FOR EACH tt-expedicao:
    DELETE tt-expedicao.
END.

FOR EACH movto-estoq  USE-INDEX data-item 
   WHERE LOOKUP({ininc/i03in218.i 4 movto-estoq.esp-docto}, tt-param.tipo-trans) <> 0 
     AND movto-estoq.dt-trans >= tt-param.dt-trans-ini  
     AND movto-estoq.dt-trans <= tt-param.dt-trans-fim NO-LOCK:

    FIND FIRST nota-fiscal WHERE nota-fiscal.cod-estabel = movto-estoq.cod-estabel AND
                                 nota-fiscal.serie       = movto-estoq.serie-docto AND
                                 nota-fiscal.nr-nota-fis = movto-estoq.nro-docto   AND
                                 LOOKUP(nota-fiscal.nat-operacao, c-nat-operacao) <> 0 NO-LOCK NO-ERROR.
    IF AVAILABLE nota-fiscal THEN
    DO:
        RUN pi-acompanhar IN h_acomp_rp (INPUT "Nota Fiscal Saida: " + STRING(nota-fiscal.nr-nota-fis)).
        
        FIND FIRST tt-expedicao WHERE tt-expedicao.nr-nota-fis = nota-fiscal.nr-nota-fis NO-ERROR.
        IF NOT AVAILABLE tt-expedicao THEN
        DO:
            CREATE tt-expedicao.
            
            ASSIGN tt-expedicao.dt-emis-nota = nota-fiscal.dt-emis-nota
                   tt-expedicao.nr-nota-fis  = nota-fiscal.nr-nota-fis 
                   tt-expedicao.placa        = nota-fiscal.placa       
                   tt-expedicao.tara-camin   = 0                  
                   tt-expedicao.nome-transp  = nota-fiscal.nome-transp 
                   tt-expedicao.cod-des-merc = nota-fiscal.cod-des-merc
                   tt-expedicao.peso-bru-tot = nota-fiscal.peso-bru-tot
                   tt-expedicao.peso-liq-tot = nota-fiscal.peso-liq-tot
                   tt-expedicao.vl-tot-nota  = nota-fiscal.vl-tot-nota.
        END.
        ELSE
        DO:
            ASSIGN tt-expedicao.peso-bru-tot = nota-fiscal.peso-bru-tot
                   tt-expedicao.peso-liq-tot = nota-fiscal.peso-liq-tot
                   tt-expedicao.vl-tot-nota  = nota-fiscal.vl-tot-nota.
        END.
          
    END.
END.


FOR EACH tt-expedicao:

    PUT STREAM s-exp UNFORMATTED STRING(DAY(tt-expedicao.dt-emis-nota), "99") + "/" +
                                 STRING(MONTH(tt-expedicao.dt-emis-nota), "99") + "/" +
                                 STRING(YEAR(tt-expedicao.dt-emis-nota), "9999") + "|".
    PUT STREAM s-exp UNFORMATTED TRIM(STRING(tt-expedicao.nr-nota-fis)) + "|".
    PUT STREAM s-exp UNFORMATTED TRIM(STRING(tt-expedicao.placa)) + "|".
    PUT STREAM s-exp UNFORMATTED TRIM(STRING(tt-expedicao.tara-camin)) + "|".
    PUT STREAM s-exp UNFORMATTED TRIM(STRING(tt-expedicao.nome-transp)) + "|".
    PUT STREAM s-exp UNFORMATTED TRIM(STRING(ENTRY(tt-expedicao.cod-des-merc, {diinc/i02di159.i 03}))) + "|".
    PUT STREAM s-exp UNFORMATTED TRIM(STRING(tt-expedicao.peso-bru-tot), ">>>>>>>>9.9999") + "|".
    PUT STREAM s-exp UNFORMATTED TRIM(STRING(tt-expedicao.peso-liq-tot), ">>>>>>>>9.9999") + "|".
    PUT STREAM s-exp UNFORMATTED TRIM(STRING(tt-expedicao.vl-tot-nota), ">>>>>>>>9.99") + "|".
    PUT STREAM s-exp UNFORMATTED STRING(DAY(TODAY), "99") + "/" +
                                 STRING(MONTH(TODAY), "99") + "/" +
                                 STRING(YEAR(TODAY), "9999") + "|".
    PUT STREAM s-exp UNFORMATTED " ". /* data_processamento */
    PUT STREAM s-exp SKIP.

END.


OUTPUT STREAM s-exp CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-imprime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime Procedure 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN i-arq = 1.

RUN pi-arq-saida.

CREATE "Excel.Application" chExcelApplication.
chworkbook  = chexcelapplication:workbooks:add.
chworksheet = chexcelapplication:sheets:item(1).
chExcelApplication:Visible = FALSE.
/*************** CabeØalho ***********************************/ 
chWorkSheet:Range("A1"):VALUE = "Centro de Custo".
chWorkSheet:Range("B1"):VALUE = "Nome Centro de Custo".
chWorkSheet:Range("C1"):VALUE = "Conta".
chWorkSheet:Range("D1"):VALUE = "Nome Conta".
chWorkSheet:Range("E1"):VALUE = "Item".
chWorkSheet:Range("F1"):VALUE = "Descriá∆o do Item".
/*chWorkSheet:Range("G1"):VALUE = "Narrativa".*/
chWorkSheet:Range("H1"):VALUE = "UN".
chWorkSheet:Range("I1"):VALUE = "Quantidade".
chWorkSheet:Range("J1"):VALUE = "Valor".
chWorkSheet:Range("K1"):VALUE = "Preáo".
chWorkSheet:Range("L1"):VALUE = "Tipo".

chWorkSheet:Range("F1:G1"):merge.  

chWorkSheet:Range("A1:L1"):FONT:Bold = TRUE.
chWorkSheet:Range("A1:L1"):HorizontalAlignment = 3.
chWorkSheet:Range("A:L"):VerticalAlignment = 2.
chWorkSheet:Range("A1:L1"):Interior:colorindex = 24.
chWorkSheet:Range("A1"):Borders(1):LineStyle = 1.
chWorkSheet:Range("A1:L1"):Borders(2):LineStyle = 1.
chWorkSheet:Range("A1:L1"):Borders(4):LineStyle = 1.
chWorkSheet:Range("A1:L1"):Borders(8):LineStyle = 1.

/*chWorkSheet:COLUMNS("G"):HorizontalAlignment = 3.*/
chWorkSheet:COLUMNS("H"):HorizontalAlignment = 3.
chWorkSheet:COLUMNS("I"):HorizontalAlignment = 3.
chWorkSheet:COLUMNS("J"):HorizontalAlignment = 3.
chWorkSheet:COLUMNS("K"):HorizontalAlignment = 3.
chWorkSheet:COLUMNS("A"):ColumnWidth = 25.
chWorkSheet:COLUMNS("B"):ColumnWidth = 40.
chWorkSheet:COLUMNS("C"):ColumnWidth = 20.
chWorkSheet:COLUMNS("D"):ColumnWidth = 40.
chWorkSheet:COLUMNS("E"):ColumnWidth = 25.
chWorkSheet:COLUMNS("F"):ColumnWidth = 60.
chWorkSheet:COLUMNS("G"):ColumnWidth = 40.
chWorkSheet:COLUMNS("H"):ColumnWidth = 6.
chWorkSheet:COLUMNS("I"):ColumnWidth = 25.
chWorkSheet:COLUMNS("J"):ColumnWidth = 25.
chWorkSheet:COLUMNS("K"):ColumnWidth = 20.
chWorkSheet:COLUMNS("L"):ColumnWidth = 10.



IF tt-param.i-impressao = 1 THEN
DO:
    chWorkSheet:Range("M1"):VALUE = "S/E".
    chWorkSheet:Range("N1"):VALUE = "N£mero Documento".
    chWorkSheet:Range("O1"):VALUE = "Fornecedor".
    chWorkSheet:Range("P1"):VALUE = "OM".
    chWorkSheet:Range("Q1"):VALUE = "Equipamento".
    
    chWorkSheet:Range("M1:Q1"):Borders(2):LineStyle = 1.
    chWorkSheet:Range("M1:Q1"):Borders(4):LineStyle = 1.
    chWorkSheet:Range("M1:Q1"):Borders(8):LineStyle = 1.
    chWorkSheet:Range("M1:Q1"):FONT:Bold = TRUE.
    chWorkSheet:Range("M1:Q1"):HorizontalAlignment = 3.
    chWorkSheet:Range("M:Q"):VerticalAlignment = 2.
    chWorkSheet:Range("M1:Q1"):Interior:colorindex = 24.

    chWorkSheet:COLUMNS("M"):HorizontalAlignment = 3.
    chWorkSheet:COLUMNS("M"):ColumnWidth = 20.
    chWorkSheet:COLUMNS("N"):ColumnWidth = 20.
    chWorkSheet:COLUMNS("O"):ColumnWidth = 45.
    chWorkSheet:COLUMNS("P"):ColumnWidth = 20.
    chWorkSheet:COLUMNS("Q"):ColumnWidth = 20.

    RUN pi-analitico.

    ASSIGN arq-excel = tt-param.dir-excel + "\RA " + 
                       STRING(TODAY, "99-99-99") +
                       ".xls".

    DO WHILE i-arq < 999:
        IF SEARCH(arq-excel) <> ? THEN
        DO:
            ASSIGN i-arq = i-arq + 1
                   arq-excel = tt-param.dir-excel + "\RA " + 
                       STRING(TODAY, "99-99-99") +
                       "(" + STRING(i-arq) + ")" +
                       ".xls".
        END.
        ELSE
        DO:
            ASSIGN i-arq = 1000.
        END.
    END.
END.
ELSE
DO:
    RUN pi-sintetico.

    ASSIGN arq-excel = tt-param.dir-excel + "\RS " + 
                       STRING(TODAY, "99-99-99") +
                       ".xls".

    DO WHILE i-arq < 999:
        IF SEARCH(arq-excel) <> ? THEN
        DO:
            ASSIGN i-arq = i-arq + 1
                   arq-excel = tt-param.dir-excel + "\RS " + 
                       STRING(TODAY, "99-99-99") +
                       "(" + STRING(i-arq) + ")" +
                       ".xls".
        END.
        ELSE
        DO:
            ASSIGN i-arq = 1000.
        END.
    END.
END.


/*chWorkSheet:COLUMNS("H"):NumberFormat = "###.###.###.##0,0000".
chWorkSheet:COLUMNS("I"):NumberFormat = "###.###.###.##0,00".
chWorkSheet:COLUMNS("J"):NumberFormat = "###,###,###,##0.00".*/


/*ASSIGN arq-excel = tt-param.dir-excel + "\dts-sql.xls".*/

chExcelApplication:Workbooks:Item(1):SaveAs(arq-excel,,,,,,).
chExcelApplication:QUIT(). 


/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

/*OUTPUT STREAM s-log CLOSE.*/
OUTPUT STREAM s-bal CLOSE.
OUTPUT STREAM s-pre CLOSE.
OUTPUT STREAM s-mov CLOSE.
OUTPUT STREAM s-con CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-monta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta Procedure 
PROCEDURE pi-monta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Montagem da temp-table      
------------------------------------------------------------------------------*/
DEF VAR c-nat-oper-aux AS CHAR NO-UNDO.

FOR EACH tt-digita:

    ASSIGN c-nat-oper-aux = TRIM(tt-digita.nat-operacao).

    IF LOOKUP(c-nat-oper-aux, c-nat-operacao) <> 0 THEN
    DO:
        NEXT.
    END.
    ELSE
    DO:
        ASSIGN c-nat-operacao = c-nat-operacao + c-nat-oper-aux + ",".
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-om) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-om Procedure 
PROCEDURE pi-om :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER i-chamada AS INTEGER NO-UNDO.

IF i-chamada = 1 THEN
DO:
    find first ccusto no-lock
         where ccusto.cod_empresa      = plano_ccusto.cod_empresa
           and ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
           and ccusto.cod_ccusto       = ord-manut.sc-desp no-error.

    find first cta_ctbl no-lock
         where cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
           and cta_ctbl.cod_cta_ctbl       = ord-manut.ct-desp no-error.

    FIND FIRST equipto WHERE equipto.cd-equipto = ord-manut.cd-equipto NO-LOCK NO-ERROR.
    FIND FIRST req-ord-produ WHERE req-ord-produ.nr-ord-produ = ord-manut.nr-ord-produ NO-LOCK NO-ERROR.
    
    FOR EACH it-requisicao WHERE it-requisicao.nr-requisicao = req-ord-produ.nr-requisicao AND
                                 it-requisicao.sequencia     = req-ord-produ.sequencia NO-LOCK:
        
        FIND FIRST ITEM WHERE ITEM.it-codigo = it-requisicao.it-codigo NO-LOCK NO-ERROR.
        
        PUT STREAM s-con UNFORMATTED STRING(DAY(ord-manut.dt-manut), "99") + "/" +
                                     STRING(MONTH(ord-manut.dt-manut), "99") + "/" +
                                     STRING(YEAR(ord-manut.dt-manut), "9999") + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(ord-manut.sc-desp)) + "|".
        PUT STREAM s-om UNFORMATTED IF AVAILABLE ccusto THEN TRIM(ccusto.des_tit_ctbl) + "|" ELSE "N/CADASTRADO" + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(ord-manut.ct-desp)) + "|".
        PUT STREAM s-om UNFORMATTED IF AVAILABLE cta_ctbl THEN TRIM(cta_ctbl.des_tit_ctbl) + "|" ELSE "N/CADASTRADO" + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(it-requisicao.it-codigo)) + "|".
        PUT STREAM s-om UNFORMATTED IF AVAILABLE req-ord-produ THEN TRIM(STRING(req-ord-produ.num-tarefa)) + "|" ELSE "" + "|".
        PUT STREAM s-om UNFORMATTED IF AVAILABLE ITEM THEN TRIM(STRING(ITEM.desc-item)) + "|" ELSE "N/CADASTRADO" + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(it-requisicao.qt-requisitada, ">>>>>>>>9.9999")) + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(it-requisicao.preco-unit * it-requisicao.qt-requisitada, ">>>>>>>>9.99")) + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(it-requisicao.preco-unit, ">>>>>>>>9.99")) + "|".
        PUT STREAM s-om UNFORMATTED "E" + "|".
        
        IF AVAILABLE req-ord-produ THEN
        DO:
            IF req-ord-produ.tp-requis = 1 THEN
            DO:
                PUT STREAM s-om UNFORMATTED "Requisiá∆o Estoque" + "|".
            END.

            IF req-ord-produ.tp-requis = 2 THEN
            DO:
                PUT STREAM s-om UNFORMATTED "Solicitaá∆o Compras" + "|".
            END.

            IF req-ord-produ.tp-requis = 3 THEN
            DO:
                PUT STREAM s-om UNFORMATTED "Solicitaá∆o Cotaá∆o" + "|".
            END.
        END.
        ELSE
        DO:
            PUT STREAM s-om UNFORMATTED "" + "|".
        END.

        PUT STREAM s-om UNFORMATTED IF AVAILABLE req-ord-produ THEN TRIM(STRING(req-ord-produ.nr-requisicao)) + "|" ELSE "" + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(ord-manut.nr-ord-produ)) + "|".
        PUT STREAM s-om UNFORMATTED "" + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(it-requisicao.un)) + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(ord-manut.cd-equipto)) + "|".
        PUT STREAM s-om UNFORMATTED IF AVAILABLE equipto THEN TRIM(STRING(equipto.descricao)) + "|" ELSE "" + "|".
        PUT STREAM s-om UNFORMATTED STRING(DAY(TODAY), "99") + "/" +
                                    STRING(MONTH(TODAY), "99") + "/" +
                                    STRING(YEAR(TODAY), "9999") + "|".
        PUT STREAM s-om UNFORMATTED " ". /* data_processamento */
    
        PUT STREAM s-om UNFORMATTED SKIP.
    END.
END.

IF i-chamada = 2 THEN
DO:
    find first ccusto no-lock
         where ccusto.cod_empresa      = plano_ccusto.cod_empresa
           and ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
           and ccusto.cod_ccusto       = mmv-ord-manut.cc-codigo no-error.

    find first cta_ctbl no-lock
         where cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
           and cta_ctbl.cod_cta_ctbl       = mmv-ord-manut.ct-codigo no-error.

    FIND FIRST equipto WHERE equipto.cd-equipto = mmv-ord-manut.cod-eqpto NO-LOCK NO-ERROR.
    FIND FIRST req-ord-produ WHERE req-ord-produ.nr-ord-produ = mmv-ord-manut.nr-ord-produ NO-LOCK NO-ERROR.
    
    FOR EACH it-requisicao WHERE it-requisicao.nr-requisicao = req-ord-produ.nr-requisicao AND
                                 it-requisicao.sequencia     = req-ord-produ.sequencia NO-LOCK:
        
        FIND FIRST ITEM WHERE ITEM.it-codigo = it-requisicao.it-codigo NO-LOCK NO-ERROR.
        
        PUT STREAM s-con UNFORMATTED STRING(DAY(mmv-ord-manut.dat-criacao), "99") + "/" +
                                     STRING(MONTH(mmv-ord-manut.dat-criacao), "99") + "/" +
                                     STRING(YEAR(mmv-ord-manut.dat-criacao), "9999") + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(mmv-ord-manut.cc-codigo)) + "|".
        PUT STREAM s-om UNFORMATTED IF AVAILABLE ccusto THEN TRIM(ccusto.des_tit_ctbl) + "|" ELSE "N/CADASTRADO" + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(mmv-ord-manut.ct-codigo)) + "|".
        PUT STREAM s-om UNFORMATTED IF AVAILABLE cta_ctbl THEN TRIM(cta_ctbl.des_tit_ctbl) + "|" ELSE "N/CADASTRADO" + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(it-requisicao.it-codigo)) + "|".
        PUT STREAM s-om UNFORMATTED IF AVAILABLE req-ord-produ THEN TRIM(STRING(req-ord-produ.num-tarefa)) + "|" ELSE "" + "|".
        PUT STREAM s-om UNFORMATTED IF AVAILABLE ITEM THEN TRIM(STRING(ITEM.desc-item)) + "|" ELSE "N/CADASTRADO" + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(it-requisicao.qt-requisitada, ">>>>>>>>9.9999")) + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(it-requisicao.preco-unit * it-requisicao.qt-requisitada, ">>>>>>>>9.99")) + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(it-requisicao.preco-unit, ">>>>>>>>9.99")) + "|".
        PUT STREAM s-om UNFORMATTED "E" + "|".

        IF AVAILABLE req-ord-produ THEN
        DO:
            IF req-ord-produ.tp-requis = 1 THEN
            DO:
                PUT STREAM s-om UNFORMATTED "Requisiá∆o Estoque" + "|".
            END.

            IF req-ord-produ.tp-requis = 2 THEN
            DO:
                PUT STREAM s-om UNFORMATTED "Solicitaá∆o Compras" + "|".
            END.

            IF req-ord-produ.tp-requis = 3 THEN
            DO:
                PUT STREAM s-om UNFORMATTED "Solicitaá∆o Cotaá∆o" + "|".
            END.
        END.
        ELSE
        DO:
            PUT STREAM s-om UNFORMATTED "" + "|".
        END.
        
        PUT STREAM s-om UNFORMATTED IF AVAILABLE req-ord-produ THEN TRIM(STRING(req-ord-produ.nr-requisicao)) + "|" ELSE "" + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(mmv-ord-manut.nr-ord-produ)) + "|".
        PUT STREAM s-om UNFORMATTED "" + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(it-requisicao.un)) + "|".
        PUT STREAM s-om UNFORMATTED TRIM(STRING(mmv-ord-manut.cod-eqpto)) + "|".
        PUT STREAM s-om UNFORMATTED IF AVAILABLE equipto THEN TRIM(STRING(equipto.descricao)) + "|" ELSE "" + "|".
        PUT STREAM s-om UNFORMATTED STRING(DAY(TODAY), "99") + "/" +
                                    STRING(MONTH(TODAY), "99") + "/" +
                                    STRING(YEAR(TODAY), "9999") + "|".
        PUT STREAM s-om UNFORMATTED " ". /* data_processamento */
    
        PUT STREAM s-om UNFORMATTED SKIP.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-sintetico) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-sintetico Procedure 
PROCEDURE pi-sintetico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH tt-relatorio:
    DELETE tt-relatorio.
END.

FOR EACH movto-estoq  USE-INDEX data-item
    WHERE movto-estoq.dt-trans  >= tt-param.dt-trans-ini  
      AND movto-estoq.dt-trans  <= tt-param.dt-trans-fim  
      AND movto-estoq.ct-codigo >= tt-param.ct-codigo-ini 
      AND movto-estoq.ct-codigo <= tt-param.ct-codigo-fim 
      AND movto-estoq.sc-codigo >= tt-param.sc-codigo-ini 
      AND movto-estoq.sc-codigo <= tt-param.sc-codigo-fim
      AND movto-estoq.it-codigo >= tt-param.it-codigo-ini 
      AND movto-estoq.it-codigo <= tt-param.it-codigo-fim NO-LOCK:


    /*L¢gica para pegar os campos que n∆o s∆o da tabela movto-estoq*/

    /*Centro Custo*/
    find first ccusto no-lock
         where ccusto.cod_empresa      = plano_ccusto.cod_empresa
           and ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
           and ccusto.cod_ccusto       = movto-estoq.sc-codigo no-error.

    /*Item*/
    FIND FIRST ITEM
         WHERE ITEM.it-codigo = movto-estoq.it-codigo  NO-LOCK NO-ERROR.

    /*Conta Contabil*/
    find first cta_ctbl no-lock
         where cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
           and cta_ctbl.cod_cta_ctbl       = movto-estoq.ct-codigo no-error.

    /*Emitente*/
    FIND FIRST emitente 
         WHERE emitente.cod-emitente = movto-estoq.cod-emitente NO-LOCK NO-ERROR.

    RUN pi-acompanhar IN h_acomp_rp (INPUT "Lendo Movto Estoque: " + STRING(movto-estoq.nr-trans)).


    /*preenchendo o relat¢rio com texto, pois no banco est∆o como numeros 1 e 2 respectivamente*/
    IF movto-estoq.tipo-trans = 1 THEN
        ASSIGN c-tipo-trans = "E".
    ELSE IF movto-estoq.tipo-trans = 2 THEN
        ASSIGN c-tipo-trans = "S".
    
    /*preenchimento dos valores*/
    ASSIGN dt-it-per = DATE("01/" + STRING(MONTH(movto-estoq.dt-trans)) +
                              "/" + STRING(YEAR(movto-estoq.dt-trans))).
    ASSIGN dt-it-per = dt-it-per - 1.
    FIND FIRST pr-it-per WHERE pr-it-per.cod-estabel = movto-estoq.cod-estabel
                           AND pr-it-per.it-codigo   = movto-estoq.it-codigo
                           AND pr-it-per.periodo     = dt-it-per NO-LOCK NO-ERROR.
    IF AVAILABLE pr-it-per THEN
    DO:
        ASSIGN d-valor-uni = pr-it-per.val-unit-mat-m[1] +
                             pr-it-per.val-unit-mob-m[1] +
                             pr-it-per.val-unit-ggf-m[1].
    END.
    ELSE
    DO:
        ASSIGN d-valor-uni = movto-estoq.valor-mat-m[1] +
                             movto-estoq.valor-mob-m[1] +
                             movto-estoq.valor-ggf-m[1].
    END.
    IF movto-estoq.quantidade <> 0 THEN
    DO:
        ASSIGN d-valor-tot = (d-valor-uni * movto-estoq.quantidade).
    END.
    ELSE
    DO:
        ASSIGN d-valor-tot = 0.
    END.
           
    
    /*Preenchimento da temp-table do relatorio*/
    CREATE tt-relatorio.

    ASSIGN tt-relatorio.dt-trans         = movto-estoq.dt-trans        
           tt-relatorio.sc-codigo        = movto-estoq.sc-codigo 
           tt-relatorio.descricao        = IF AVAILABLE ccusto THEN ccusto.des_tit_ctbl ELSE "N/CADASTRADO" 
           tt-relatorio.ct-codigo        = movto-estoq.ct-codigo 
           tt-relatorio.titulo           = IF AVAILABLE cta_ctbl THEN cta_ctbl.des_tit_ctbl ELSE "N/CADASTRADO" 
           tt-relatorio.it-codigo        = movto-estoq.it-codigo 
           tt-relatorio.desc-item        = IF AVAILABLE ITEM THEN (ITEM.desc-item) ELSE "N/CADASTRADO" 
           tt-relatorio.quantidade       = movto-estoq.quantidade 
           tt-relatorio.d-valor-tot      = d-valor-tot 
           tt-relatorio.d-valor-uni      = d-valor-uni
           tt-relatorio.c-tipo-trans     = c-tipo-trans 
           tt-relatorio.tipo-trans       = {ininc/i03in218.i 4 movto-estoq.esp-docto}
           tt-relatorio.nro-docto        = movto-estoq.nro-docto 
           tt-relatorio.dt-postagem      = TODAY
           tt-relatorio.dt-processamento = ?
           tt-relatorio.un               = movto-estoq.un.


    FIND FIRST ordem-compra WHERE ordem-compra.numero-ordem = movto-estoq.numero-ordem NO-LOCK NO-ERROR.
    IF AVAILABLE ordem-compra THEN
    DO:
        FIND FIRST it-requisicao WHERE it-requisicao.nr-requisicao = ordem-compra.nr-requisicao AND
                                       it-requisicao.sequencia     = ordem-compra.sequencia     AND
                                       it-requisicao.it-codigo     = ordem-compra.it-codigo NO-LOCK NO-ERROR.
        IF AVAILABLE it-requisicao THEN
        DO:
            /* 
            Verificar este IF abaixo. 
            Solicitaá∆o de JosÇ Carlos: item comeáar 99 -> descriá∆o da requisiá∆o, e n∆o do item!
            */
            IF SUBSTRING(movto-estoq.it-codigo, 1,1) = "9" THEN
            DO:
                ASSIGN tt-relatorio.desc-item = it-requisicao.narrativa. 
            END.
        END.
    END.

    ASSIGN tt-relatorio.desc-item = REPLACE (tt-relatorio.desc-item, CHR(10), " ").
    
    IF movto-estoq.esp-docto = 18 OR       /* NC */
       movto-estoq.esp-docto = 19 OR       /* NF */  
       movto-estoq.esp-docto = 20 OR       /* NFD */        
       movto-estoq.esp-docto = 21 OR       /* NFE */        
       movto-estoq.esp-docto = 22 OR       /* NFS */        
       movto-estoq.esp-docto = 23 THEN     /* NFT */  
    DO:
        IF AVAILABLE emitente THEN
        DO:
            ASSIGN tt-relatorio.nome-emit = emitente.nome-emit.
        END.

        /*IF AVAILABLE it-requisicao THEN
        DO:
            tt-relatorio.narrativa = it-requisicao.narrativa.
        END.*/
    END.
END.


/*preenchendo o Excel*/
ASSIGN i-linha = 2
       d-valor-tot-aux = 0.

FOR EACH tt-relatorio USE-INDEX conta-ctbl:
    
    IF i-linha = 2 THEN
    DO:
        ASSIGN conta-ctbl-aux  = tt-relatorio.sc-codigo + tt-relatorio.ct-codigo.
    END.

    IF tt-relatorio.sc-codigo + tt-relatorio.ct-codigo <> conta-ctbl-aux THEN
    DO:
        chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "TOTAL CONTA CONTABIL".
        chWorkSheet:Range("A" + STRING(i-linha)):FONT:Bold = TRUE.
        chWorkSheet:Range("J" + STRING(i-linha)):VALUE = TRIM(STRING(d-valor-tot-aux,"->>,>>>,>>>,>>9.99")).
        chWorkSheet:Range("J" + STRING(i-linha)):FONT:Bold = TRUE.

        /*Log para importaá∆o: mmic_balancete*/
        PUT STREAM s-bal UNFORMATTED STRING(DAY(tt-relatorio.dt-trans), "99") + "/" +
                                     STRING(MONTH(tt-relatorio.dt-trans), "99") + "/" +
                                     STRING(YEAR(tt-relatorio.dt-trans), "9999") + "|".
        PUT STREAM s-bal UNFORMATTED TRIM(STRING(tt-relatorio.sc-codigo)) + "|".
        PUT STREAM s-bal UNFORMATTED TRIM(STRING(tt-relatorio.descricao)) + "|".
        PUT STREAM s-bal UNFORMATTED TRIM(STRING(tt-relatorio.ct-codigo)) + "|".
        PUT STREAM s-bal UNFORMATTED TRIM(STRING(tt-relatorio.titulo)) + "|".
        PUT STREAM s-bal UNFORMATTED TRIM(STRING(d-valor-tot-aux, "->>>>>>>>>>9.99")) + "|".
        PUT STREAM s-bal UNFORMATTED STRING(DAY(TODAY), "99") + "/" +
                                     STRING(MONTH(TODAY), "99") + "/" +
                                     STRING(YEAR(TODAY), "9999") + "|".
        PUT STREAM s-bal UNFORMATTED " ". /* data_processamento */
        PUT STREAM s-bal SKIP.

        ASSIGN conta-ctbl-aux  = tt-relatorio.sc-codigo + tt-relatorio.ct-codigo
               d-valor-tot-aux = 0
               i-linha         = i-linha + 2.
    END.

    RUN pi-acompanhar IN h_acomp_rp (INPUT "Excel - Conta Contabil: " + conta-ctbl-aux).

    chWorkSheet:Range("A" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("A" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.sc-codigo).

    chWorkSheet:Range("B" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("B" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.descricao).
    chWorkSheet:range("B" + STRING(i-linha)):WrapText = TRUE.

    chWorkSheet:Range("C" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("C" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.ct-codigo).

    chWorkSheet:Range("D" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("D" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.titulo).
    chWorkSheet:range("D" + STRING(i-linha)):WrapText = TRUE. 
     
    chWorkSheet:Range("E" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("E" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.it-codigo).

    chWorkSheet:Range("F" + STRING(i-linha) + ":G" + STRING(i-linha)):merge.             
    chWorkSheet:Range("F" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("F" + STRING(i-linha)):VALUE =  TRIM(tt-relatorio.desc-item).
    chWorkSheet:range("F" + STRING(i-linha)):WrapText = TRUE.

    /*tt-relatorio.narrativa = REPLACE(tt-relatorio.narrativa, CHR(9), " ").
    tt-relatorio.narrativa = REPLACE(tt-relatorio.narrativa, CHR(13), " ").
    chWorkSheet:Range("G" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("G" + STRING(i-linha)):VALUE = tt-relatorio.narrativa.
    chWorkSheet:range("G" + STRING(i-linha)):WrapText = TRUE.*/

    chWorkSheet:Range("H" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.un).
    
    chWorkSheet:Range("I" + STRING(i-linha)):NumberFormat = "###.###.###.##0,0000".
    chWorkSheet:Range("J" + STRING(i-linha)):NumberFormat = "###.###.###.##0,00".
    chWorkSheet:Range("K" + STRING(i-linha)):NumberFormat = "###.###.###.##0,00".

    chWorkSheet:Range("I" + STRING(i-linha)):VALUE = tt-relatorio.quantidade.
    
    IF tt-relatorio.c-tipo-trans = "E" THEN
    DO:
        /*chWorkSheet:Range("I" + STRING(i-linha)):VALUE = "-" + TRIM(STRING(tt-relatorio.d-valor-tot,"->>,>>>,>>>,>>9.99")).*/
        chWorkSheet:Range("J" + STRING(i-linha)):VALUE = 0 - tt-relatorio.d-valor-tot.
        ASSIGN d-valor-tot-aux = d-valor-tot-aux - tt-relatorio.d-valor-tot.
    END.
    ELSE
    DO:
        /*chWorkSheet:Range("I" + STRING(i-linha)):VALUE = "+" + TRIM(STRING(tt-relatorio.d-valor-tot,"->>,>>>,>>>,>>9.99")).*/
        chWorkSheet:Range("J" + STRING(i-linha)):VALUE = tt-relatorio.d-valor-tot.
        ASSIGN d-valor-tot-aux = d-valor-tot-aux + tt-relatorio.d-valor-tot.
    END.


    /*IF tt-relatorio.tipo-trans = "DEV" OR
       tt-relatorio.tipo-trans = "DRM" OR
       tt-relatorio.tipo-trans = "NFD" THEN
    DO:
        /*chWorkSheet:Range("H" + STRING(i-linha)):VALUE = "-" + TRIM(STRING(tt-relatorio.quantidade,"->>,>>>,>>>,>>9.99")).

        chWorkSheet:Range("I" + STRING(i-linha)):VALUE = "-" + TRIM(STRING(tt-relatorio.d-valor-tot,"->>,>>>,>>>,>>9.99")).*/
        chWorkSheet:Range("I" + STRING(i-linha)):VALUE = 0 - tt-relatorio.quantidade.

        chWorkSheet:Range("J" + STRING(i-linha)):VALUE = 0 - tt-relatorio.d-valor-tot.

        ASSIGN d-valor-tot-aux = d-valor-tot-aux - tt-relatorio.d-valor-tot.
    END.
    ELSE
    DO:
        /*chWorkSheet:Range("H" + STRING(i-linha)):VALUE = TRIM(STRING(tt-relatorio.quantidade,"->>,>>>,>>>,>>9.99")).*/
        chWorkSheet:Range("I" + STRING(i-linha)):VALUE = 0 - tt-relatorio.quantidade.

        IF tt-relatorio.c-tipo-trans = "E" THEN
        DO:
            /*chWorkSheet:Range("I" + STRING(i-linha)):VALUE = "+" + TRIM(STRING(tt-relatorio.d-valor-tot,"->>,>>>,>>>,>>9.99")).*/
            chWorkSheet:Range("J" + STRING(i-linha)):VALUE = tt-relatorio.d-valor-tot.
            ASSIGN d-valor-tot-aux = d-valor-tot-aux + tt-relatorio.d-valor-tot.
        END.
        ELSE
        DO:
            /*chWorkSheet:Range("I" + STRING(i-linha)):VALUE = "-" + TRIM(STRING(tt-relatorio.d-valor-tot,"->>,>>>,>>>,>>9.99")).*/
            chWorkSheet:Range("J" + STRING(i-linha)):VALUE = 0 - tt-relatorio.d-valor-tot.
            ASSIGN d-valor-tot-aux = d-valor-tot-aux - tt-relatorio.d-valor-tot.
        END.
    END.*/
     
    chWorkSheet:Range("K" + STRING(i-linha)):VALUE = TRIM(STRING(tt-relatorio.d-valor-uni, "->>,>>>,>>>,>>9.99")).

    chWorkSheet:Range("L" + STRING(i-linha)):numberformat = "@".
    chWorkSheet:Range("L" + STRING(i-linha)):VALUE = TRIM(tt-relatorio.tipo-trans).
    
    /*Log para importaá∆o: mmic_previa_custos*/
    PUT STREAM s-pre UNFORMATTED STRING(DAY(tt-relatorio.dt-trans), "99") + "/" +
                                 STRING(MONTH(tt-relatorio.dt-trans), "99") + "/" +
                                 STRING(YEAR(tt-relatorio.dt-trans), "9999") + "|".
    PUT STREAM s-pre UNFORMATTED TRIM(STRING(tt-relatorio.sc-codigo)) + "|".
    PUT STREAM s-pre UNFORMATTED TRIM(STRING(tt-relatorio.descricao)) + "|".
    PUT STREAM s-pre UNFORMATTED TRIM(STRING(tt-relatorio.ct-codigo)) + "|".
    PUT STREAM s-pre UNFORMATTED TRIM(STRING(tt-relatorio.titulo)) + "|".
    PUT STREAM s-pre UNFORMATTED TRIM(STRING(tt-relatorio.d-valor-tot, "->>>>>>>>>>9.99")) + "|".
    PUT STREAM s-pre UNFORMATTED STRING(DAY(TODAY), "99") + "/" +
                                 STRING(MONTH(TODAY), "99") + "/" +
                                 STRING(YEAR(TODAY), "9999") + "|".
    PUT STREAM s-pre UNFORMATTED " ". /* data_processamento */
    PUT STREAM s-pre SKIP.
    
    ASSIGN i-linha = i-linha + 1.
END.


/* Èltimo item da tabela tt-relatorio: mmic_balancete */
FIND LAST tt-relatorio NO-ERROR.
IF AVAIL tt-relatorio THEN
DO:
    chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "TOTAL CONTA CONTABIL".
    chWorkSheet:Range("A" + STRING(i-linha)):FONT:Bold = TRUE.
    chWorkSheet:Range("J" + STRING(i-linha)):VALUE = TRIM(STRING(d-valor-tot-aux,"->>,>>>,>>>,>>9.99")).
    chWorkSheet:Range("J" + STRING(i-linha)):FONT:Bold = TRUE.

    PUT STREAM s-bal UNFORMATTED STRING(DAY(tt-relatorio.dt-trans), "99") + "/" +
                                 STRING(MONTH(tt-relatorio.dt-trans), "99") + "/" +
                                 STRING(YEAR(tt-relatorio.dt-trans), "9999") + "|".
    PUT STREAM s-bal UNFORMATTED TRIM(STRING(tt-relatorio.sc-codigo)) + "|".
    PUT STREAM s-bal UNFORMATTED TRIM(STRING(tt-relatorio.descricao)) + "|".
    PUT STREAM s-bal UNFORMATTED TRIM(STRING(tt-relatorio.ct-codigo)) + "|".
    PUT STREAM s-bal UNFORMATTED TRIM(STRING(tt-relatorio.titulo)) + "|".
    PUT STREAM s-bal UNFORMATTED TRIM(STRING(d-valor-tot-aux, "->>>>>>>>>>9.99")) + "|".
    PUT STREAM s-bal UNFORMATTED STRING(DAY(TODAY), "99") + "/" +
                                 STRING(MONTH(TODAY), "99") + "/" +
                                 STRING(YEAR(TODAY), "9999") + "|".
    PUT STREAM s-bal UNFORMATTED " ". /* data_processamento */
    PUT STREAM s-bal SKIP.
    
END.





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

