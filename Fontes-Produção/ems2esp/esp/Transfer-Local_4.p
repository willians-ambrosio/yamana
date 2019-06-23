/*--------------------------------------------------------------------------------------*/
DEF INPUT PARAM c-arquivo   AS CHARACTER  NO-UNDO.
/*--------------------------------------------------------------------------------------*/
def temp-table tt-erro-aux NO-UNDO
  field i-sequen as int
  field cd-erro  as INT
  field mensagem as char format "x(255)".
{cdp/cd0666.i}
DEF TEMP-TABLE tt-movto NO-UNDO
    FIELD cod-versao-integracao AS INTEGER FORMAT "999"
    FIELD cod-prog-orig         LIKE movto-estoq.cod-prog-orig
    FIELD l-mov-erro            AS LOGICAL INITIAL NO
    FIELD r-mov-inv             AS ROWID    
    FIELD r-mov-orig            AS ROWID /*registro original para valorizar o estorno, devolu»’o,retorno*/
    FIELD sequen-nf             LIKE movto-estoq.sequen-nf
    FIELD cod-depos             LIKE movto-estoq.cod-depos
    FIELD cod-emitente          LIKE movto-estoq.cod-emitente
    FIELD cod-estabel           LIKE movto-estoq.cod-estabel
    FIELD cod-refer             LIKE movto-estoq.cod-refer
    FIELD ct-codigo             LIKE movto-estoq.ct-codigo
    FIELD descricao-db          LIKE movto-estoq.descricao-db
    FIELD dt-nf-saida           LIKE movto-estoq.dt-nf-saida
    FIELD dt-trans              LIKE movto-estoq.dt-trans
    FIELD esp-docto             LIKE movto-estoq.esp-docto
    FIELD it-codigo             LIKE movto-estoq.it-codigo
    FIELD cod-localiz           LIKE movto-estoq.cod-localiz
    FIELD lote                  LIKE movto-estoq.lote
    FIELD nat-operacao          LIKE movto-estoq.nat-operacao
    FIELD nro-docto             LIKE movto-estoq.nro-docto
    FIELD num-sequen            LIKE movto-estoq.num-sequen
    FIELD numero-ordem          LIKE movto-estoq.numero-ordem
    FIELD nr-ord-produ          LIKE movto-estoq.nr-ord-produ
    FIELD peso-liquido          LIKE movto-estoq.peso-liquido
    FIELD quantidade            LIKE movto-estoq.quantidade
    FIELD referencia            LIKE movto-estoq.referencia
    FIELD sc-codigo             LIKE movto-estoq.sc-codigo
    FIELD serie-docto           LIKE movto-estoq.serie-docto
    FIELD tipo-preco            LIKE movto-estoq.tipo-preco
    FIELD tipo-trans            LIKE movto-estoq.tipo-trans
    FIELD tipo-valor            LIKE movto-estoq.tipo-valor
    FIELD un                    LIKE movto-estoq.un         
    FIELD valor-mat-m           LIKE movto-estoq.valor-mat-m
    FIELD valor-mat-o           LIKE movto-estoq.valor-mat-o
    FIELD valor-mat-p           LIKE movto-estoq.valor-mat-p
    FIELD valor-mob-m           LIKE movto-estoq.valor-mob-m
    FIELD valor-mob-o           LIKE movto-estoq.valor-mob-o
    FIELD valor-mob-p           LIKE movto-estoq.valor-mob-p
    FIELD valor-ggf-m           LIKE movto-estoq.valor-ggf-m
    FIELD valor-ggf-o           LIKE movto-estoq.valor-ggf-o
    FIELD valor-ggf-p           LIKE movto-estoq.valor-ggf-p
    FIELD valor-nota            LIKE movto-estoq.valor-nota
    FIELD vl-nota-fasb          LIKE movto-estoq.vl-nota-fasb
    FIELD nr-ord-refer          LIKE movto-estoq.nr-ord-refer
    FIELD nr-req-sum            LIKE movto-estoq.nr-req-sum
    FIELD cod-roteiro           LIKE movto-estoq.cod-roteiro
    FIELD nr-reporte            LIKE movto-estoq.nr-reporte
    FIELD item-pai              LIKE movto-estoq.item-pai
    FIELD op-codigo             LIKE movto-estoq.op-codigo
    FIELD cod-usu-ult-alter     LIKE movto-estoq.cod-usu-ult-alter
    FIELD ct-db                 LIKE movto-estoq.ct-codigo
    FIELD sc-db                 LIKE movto-estoq.sc-codigo
    FIELD dt-vali-lote          LIKE saldo-estoq.dt-vali-lote
    FIELD op-seq                LIKE movto-estoq.op-seq
    FIELD usuario               LIKE movto-estoq.usuario
    FIELD nr-trans              LIKE movto-estoq.nr-trans 
    FIELD cod-estabel-des       LIKE movto-estoq.cod-estabel-des
    FIELD origem-valor          LIKE movto-estoq.origem-valor
    FIELD num-ord-des           LIKE movto-estoq.num-ord-des
    FIELD num-seq-des           LIKE movto-estoq.num-seq-des
    FIELD num-ord-inv           LIKE movto-estoq.num-ord-inv
    FIELD valor-ipi             LIKE movto-estoq.valor-ipi
    FIELD valor-iss             LIKE movto-estoq.valor-iss
    FIELD valor-icm             LIKE movto-estoq.valor-icm
    FIELD vl-icm-fasb           LIKE movto-estoq.vl-icm-fasb
    FIELD vl-iss-fasb           LIKE movto-estoq.vl-iss-fasb
    FIELD vl-ipi-fasb           LIKE movto-estoq.vl-ipi-fasb 
    FIELD per-ppm               LIKE movto-estoq.per-ppm
    FIELD atualiza-ul-ent       AS LOGICAL
    FIELD i-sequen              AS INTEGER
    FIELD gera-saldo            AS LOGICAL INIT NO
    FIELD qt-alocada            AS DECIMAL
    FIELD cod-unid-negoc        LIKE movto-estoq.cod-unid-negoc
    FIELD cod-unid-negoc-db     LIKE movto-estoq.cod-unid-negoc
    FIELD cod-unid-negoc-sdo    LIKE movto-estoq.cod-unid-negoc
    field i-sequen-pai             as integer
    field dat-valid-lote-fabrican  like movto-estoq.dat-valid-lote-fabrican
    field dat-fabricc-lote         like movto-estoq.dat-fabricc-lote
    field nom-fabrican             like movto-estoq.nom-fabrican
    field cod-lote-fabrican        like movto-estoq.cod-lote-fabrican
    field log-ficha                as LOG.
/*--------------------------------------------------------------------------------------*/
DEF NEW GLOBAL SHARED VARIABLE c-seg-usuario AS CHAR FORMAT "X(12)" NO-UNDO.
/*--------------------------------------------------------------------------------------*/
def    var      i-empresa            like param-global.empresa-prin       no-undo.
DEF    VAR      h-cdapi024           AS HANDLE                            NO-UNDO.
DEF    VAR      c-estab              LIKE estab-mat.cod-estabel           NO-UNDO.
DEF    VAR      c-it-codigo          LIKE ITEM.it-codigo                  NO-UNDO.
def    VAR      c-cod-depos          like saldo-estoq.cod-depos           no-undo.
DEF    VAR      c-cod-localiz        like saldo-estoq.cod-localiz         no-undo.
DEF    VAR      c-lote               like saldo-estoq.lote                no-undo.
DEF    VAR      c-cod-refer          like saldo-estoq.cod-refer           no-undo.
DEF    VAR      h-ceapi001k          AS HANDLE                            NO-UNDO.
DEF    VAR      i-doc                AS INT                               NO-UNDO.
DEFINE VARIABLE h-acomp              AS HANDLE                            NO-UNDO.
DEFINE VARIABLE vchExcel             AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE vchWorkBook          AS COM-HANDLE                        NO-UNDO. 
DEFINE VARIABLE vchWorkSheet         AS COM-HANDLE                        NO-UNDO. 
DEFINE VARIABLE viLinha              AS INTEGER                           NO-UNDO.
/*--------------------------------------------------------------------------------------*/
DEF BUFFER b-saldo-estoq FOR saldo-estoq.
/*--------------------------------------------------------------------------------------*/
/*
LAYOUT DO EXCEL
---------------
A - Data de Transa‡Æo 
B - Documento
C - S‚rie
D - Conta Reduzida
E - Contro de Custo
F - Item
G - Estabelecimento PadrÆo
H - Dep¢sito Sa¡da
I - Localiza‡Æo Sa¡da
J - Lote Sa¡da
K - Referencia Sa¡da

L - Estabelecimento Entrada
M - Dep¢sito Entrada
N - Localizacao Entrada
O - Lote Entrada
P - Referˆncia de Entrada
*/
/*--------------------------------------------------------------------------------------*/
CREATE "Excel.Application":U vchExcel. 
ASSIGN 
  vchExcel:VISIBLE = FALSE /*TRUE*/ 
  vchWorkBook      = vchExcel:Workbooks:OPEN(c-arquivo) 
  vchWorkSheet     = vchExcel:Sheets:Item(1). 
/*--------------------------------------------------------------------------------------*/
find first param-global NO-LOCK.
FIND FIRST param-estoq NO-LOCK NO-ERROR.
ASSIGN i-doc = 0.
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Importanto").
run pi-acompanhar  in h-acomp (input "Carregando..").
/*--------------------------------------------------------------------------------------*/
blk-repeat:
REPEAT viLinha = 2 TO 5000 
  ON ERROR UNDO, LEAVE
  ON STOP  UNDO, RETURN 'NOK':
    /*--------------------------------------------------------------------------------------*/
    assign
      i-doc         = i-doc + 1
      i-empresa     = param-global.empresa-prin
      c-estab       = "931" /* STRING(INT(vchWorkSheet:Range("G" + TRIM(STRING(viLinha))):VALUE)) */
      c-it-codigo   = STRING(vchWorkSheet:Range("F" + TRIM(STRING(viLinha))):VALUE,"X(16)")
      c-cod-localiz = STRING(vchWorkSheet:Range("I" + TRIM(STRING(viLinha))):VALUE)
      c-lote        = "" /* STRING(vchWorkSheet:Range("J" + TRIM(STRING(viLinha))):VALUE) */
      c-cod-refer   = "" /* STRING(vchWorkSheet:Range("K" + TRIM(STRING(viLinha))):VALUE) */.
    IF c-it-codigo = "" OR c-it-codigo = ? THEN LEAVE blk-repeat. 
    run pi-acompanhar in h-acomp (input "Item: " + c-it-codigo).
    find estab-mat NO-LOCK where estab-mat.cod-estabel = c-estab NO-ERROR.
    find FIRST conta-contab
      no-lock
      where conta-contab.conta-contabil = param-estoq.conta-transf /* estab-mat.conta-transf */
      /* and   conta-contab.ep-codigo      = "930" */ /* i-empresa */
      no-error.
    IF NOT AVAIL conta-contab THEN NEXT blk-repeat.
    FIND ITEM NO-LOCK WHERE ITEM.it-codigo = c-it-codigo NO-ERROR.
    IF NOT AVAIL ITEM THEN NEXT blk-repeat.
    FOR EACH b-saldo-estoq NO-LOCK
      where  b-saldo-estoq.it-codigo   = c-it-codigo
      and    b-saldo-estoq.cod-estabel = c-estab
      /* and    b-saldo-estoq.cod-depos   = c-cod-depos */
      and    b-saldo-estoq.cod-localiz = c-cod-localiz 
      /*       and    b-saldo-estoq.lote        = c-lote      */
      /*       and    b-saldo-estoq.cod-refer   = c-cod-refer */
      :
        /* Depuracao
        MESSAGE b-saldo-estoq.qtidade-atu SKIP b-saldo-estoq.it-codigo SKIP b-saldo-estoq.cod-estabel SKIP b-saldo-estoq.cod-depos SKIP b-saldo-estoq.cod-localiz SKIP b-saldo-estoq.lote
            SKIP b-saldo-estoq.cod-refer 
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        */    
        /*------------------------------------------------------------------------------------*/
        IF b-saldo-estoq.qtidade-atu = 0 THEN NEXT blk-repeat.
        /*--------------------------------------------------------------------------------------*/
        ASSIGN c-cod-depos = b-saldo-estoq.cod-depos.
        /*----movimento de saida ----*/
        create tt-movto.
        assign 
          tt-movto.cod-versao-integracao  = 1
          tt-movto.ct-codigo              = conta-contab.ct-codigo
          tt-movto.sc-codigo              = conta-contab.sc-codigo
          tt-movto.cod-prog-orig          = "QBG-RUI-BIS"
          tt-movto.tipo-trans             = 2 /* saida */
          tt-movto.esp-docto              = 33
          tt-movto.ct-codigo              = conta-contab.ct-codigo
          /* tt-movto.conta-contabil         = conta-contab.ct-codigo /* conta-contab.conta-contabil */ /* vchWorkSheet:Range("D" + TRIM(STRING(viLinha))):VALUE */ */
          tt-movto.dt-trans               = TODAY /* DATE(vchWorkSheet:Range("A" + TRIM(STRING(viLinha))):VALUE) */
          tt-movto.dt-vali-lote           = ?
          tt-movto.nro-docto              = TRIM(STRING(i-doc,">>>>>>>>9")) /* STRING(vchWorkSheet:Range("B" + TRIM(STRING(viLinha))):VALUE) */
          tt-movto.sc-codigo              = conta-contab.sc-codigo
          tt-movto.serie-docto            = "" /* STRING(vchWorkSheet:Range("C" + TRIM(STRING(viLinha))):VALUE) */
          tt-movto.cod-depos              = c-cod-depos 
          tt-movto.cod-estabel            = c-estab
          tt-movto.it-codigo              = c-it-codigo
          tt-movto.cod-refer              = "" /* c-cod-refer */
          tt-movto.cod-localiz            = b-saldo-estoq.cod-localiz
          tt-movto.lote                   = "" /* c-lote  */
          tt-movto.quantidade             = b-saldo-estoq.qtidade-atu
          tt-movto.un                     = ITEM.un
          tt-movto.usuario                = c-seg-usuario.
        /*--------------------------------------------------------------------------------------*/
        assign 
          i-empresa     = param-global.empresa-prin
          c-estab       = "931" /* STRING(vchWorkSheet:Range("L" + TRIM(STRING(viLinha))):VALUE) */
          c-cod-localiz = STRING(vchWorkSheet:Range("N" + TRIM(STRING(viLinha))):VALUE)     
          c-lote        = "" /* STRING(vchWorkSheet:Range("O" + TRIM(STRING(viLinha))):VALUE)  */
          c-cod-refer   = "" /* STRING(vchWorkSheet:Range("P" + TRIM(STRING(viLinha))):VALUE) */.
        find estab-mat NO-LOCK where estab-mat.cod-estabel = c-estab NO-ERROR.
        find FIRST conta-contab
          no-lock
          where conta-contab.conta-contabil = param-estoq.conta-transf  /* estab-mat.conta-transf */
          /*           and   conta-contab.ep-codigo      = i-empresa */
          no-error.
        IF NOT AVAIL conta-contab THEN NEXT blk-repeat.
        /*--------------------------------------------------------------------------------------*/
        /*----movimento de saida ----*/
        create tt-movto.
        assign 
          tt-movto.cod-versao-integracao  = 1
          tt-movto.ct-codigo              = conta-contab.ct-codigo
          tt-movto.sc-codigo              = conta-contab.sc-codigo
          tt-movto.cod-prog-orig          = "QBG-RUI-BIS"
          tt-movto.tipo-trans             = 1 /* entrada */
          tt-movto.esp-docto              = 33
          tt-movto.ct-codigo              = conta-contab.ct-codigo
          /* tt-movto.conta-contabil         = vchWorkSheet:Range("D" + TRIM(STRING(viLinha))):VALUE */
          tt-movto.dt-trans               = TODAY /* DATE(vchWorkSheet:Range("A" + TRIM(STRING(viLinha))):VALUE) */
          tt-movto.dt-vali-lote           = ?
          tt-movto.nro-docto              = TRIM(STRING(i-doc,">>>>>>>>9"))  /* STRING(vchWorkSheet:Range("B" + TRIM(STRING(viLinha))):VALUE) */
          tt-movto.sc-codigo              = conta-contab.sc-codigo
          tt-movto.serie-docto            = "" /* STRING(vchWorkSheet:Range("C" + TRIM(STRING(viLinha))):VALUE) */
          tt-movto.cod-depos              = c-cod-depos 
          tt-movto.cod-estabel            = c-estab
          tt-movto.it-codigo              = c-it-codigo
          tt-movto.cod-refer              = "" /* c-cod-refer */ 
          tt-movto.cod-localiz            = c-cod-localiz
          tt-movto.lote                   = "" /* c-lote  */
          tt-movto.quantidade             = b-saldo-estoq.qtidade-atu
          tt-movto.un                     = ITEM.un
          tt-movto.usuario                = c-seg-usuario.
        /*--------------------------------------------------------------------------------------*/
        /* Efetua a atualizacao de Estoque ---------------------------------------*/
        find first saldo-estoq exclusive-lock
          where saldo-estoq.it-codigo   = c-it-codigo
            and saldo-estoq.cod-estabel = "931"
            and saldo-estoq.cod-depos   = c-cod-depos
            and saldo-estoq.cod-localiz = b-saldo-estoq.cod-localiz
            and saldo-estoq.lote        = b-saldo-estoq.lote
            and saldo-estoq.cod-refer   = b-saldo-estoq.cod-refer no-error.
        run cep/ceapi001k.p persistent set h-ceapi001k.
        if valid-handle (h-ceapi001k) then do:
          run pi-execute IN h-ceapi001k (input-output table tt-movto,
                                         input-output table tt-erro,
                                         input        YES).
          delete procedure h-ceapi001k.
          assign h-ceapi001k = ?.
        end.
        if avail saldo-estoq then
            release saldo-estoq.
        find first tt-erro no-lock no-error.
        if avail tt-erro then do:
          run cdp/cd0666.w (input table tt-erro).
          undo, return 'ADM-ERROR':U.
        end.
        FOR EACH tt-movto: DELETE tt-movto. END.
        FOR EACH tt-erro: DELETE tt-erro. END.
        /*------------------------------------------------------------------------------------*/
      END. /* FOR EACH b-saldo-estoq NO-LOCK */
    /*--------------------------------------------------------------------------------------*/
  END. /* REPEAT viLinha = 1 TO 10000 */
