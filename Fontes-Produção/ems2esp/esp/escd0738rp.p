/********************************************************************************
**   Autor: Joao B. C. Bisneto
** Empresa: DSC-PRAXIS
**    Data: Maio/2016
**Objetivo: Gera‡Æo de relat¢rio em Excel para levantamento de 
**          requisi‡äes por t‚cnico.
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.
def buffer bf-ord-manut FOR ord-manut. 
def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.
def var h-acomp       as handle  no-undo.
DEF VAR v-cd-equipto  LIKE equipto.cd-equipto          NO-UNDO.
DEF VAR v-DescEqpto   like equipto.descricao           no-undo.
DEF VAR v-cd-tecnico  LIKE mmi-tec-req-prod.cd-tecnico NO-UNDO.
DEF VAR c-arq-destino AS CHAR                          NO-UNDO.
DEF VAR r-equipto     AS ROWID                         NO-UNDO.
DEF VAR i-busca       AS INT                           NO-UNDO.
/* def stream str-rp. */
{include/i-prgvrs.i escd0738rp 0.12.00.001}  
{utp/ut-glob.i}
{include/i_fnctrad.i}

/* Miniflexibilizacao */
{cdp/cdcfgfin.i}
/* ------------------ */

/*--VARIAVIES--*/
{esp/escd0738.i}                         

/*** Verificar se a funcao especial esta habilitada ***/
{include/getdefinedfunction.i}

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

{include/i-rpvar.i}

find empresa no-lock where empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  not avail empresa then
    return.

assign c-programa = "Escd0738rp"
       c-empresa  = empresa.razao-social.

assign c-titulo-relat = "Relat¢rio de Requisi‡äes por T‚cnico".

{include/i-rpcab.i &WIDTH=256}
{include/i-rpout.i}

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Acompanhamento_Relat¢rio * R}
run pi-inicializar in h-acomp (input return-value).
/* run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)). */

/* Bloco Principal */
FIND FIRST tt-param NO-LOCK NO-ERROR.
IF NOT AVAIL tt-param THEN
  return "NOK":U.
FOR EACH tt-relat: DELETE tt-relat. END.
ASSIGN i-busca = 0.
IF 
 (tt-param.tt-ini-nr-requisicao > 0         OR
  tt-param.tt-fim-nr-requisicao < 99999999) AND 
  tt-param.tt-ini-cd-tecnico    = "000000"  AND
  tt-param.tt-fim-cd-tecnico    = "999999"  THEN
  ASSIGN i-busca = 1.
ELSE
IF 
  tt-param.tt-ini-cd-tecnico <> "000000" OR
  tt-param.tt-fim-cd-tecnico <> "999999" THEN
  ASSIGN i-busca = 2.   
IF i-busca = 1 OR i-busca = 0 THEN
  DO:
    blk-req-ord-prod:
    FOR EACH req-ord-produc
      NO-LOCK
      WHERE req-ord-produc.nr-requisicao >= tt-param.tt-ini-nr-requisicao
      AND   req-ord-produc.nr-requisicao <= tt-param.tt-fim-nr-requisicao:
        run pi-acompanhar in h-acomp(input "Nr Req: " + STRING(req-ord-produc.nr-requisicao) + " Seq:" + STRING(req-ord-produc.sequencia)).
        
        IF 
          req-ord-produc.nr-ord-produ < tt-param.tt-ini-nr-ord-produ THEN
          NEXT blk-req-ord-prod.
        IF
          req-ord-produc.nr-ord-produ > tt-param.tt-fim-nr-ord-produ THEN
          NEXT blk-req-ord-prod.
        
        IF 
          req-ord-produc.dat-trans    < tt-param.tt-ini-dat-trans    THEN
          NEXT blk-req-ord-prod.
        IF
          req-ord-produc.dat-trans    > tt-param.tt-fim-dat-trans    THEN
          NEXT blk-req-ord-prod.
        
        ASSIGN 
          v-cd-equipto = ""
          v-DescEqpto  = "".
        RUN buscaEqptoManutencao
         (INPUT  req-ord-produc.nr-ord-produ,
          OUTPUT v-cd-equipto,
          OUTPUT v-DescEqpto).
        IF 
          v-cd-equipto = "" THEN
          NEXT blk-req-ord-prod.
        
        IF 
          v-cd-equipto < tt-param.tt-ini-cd-equipto THEN
          NEXT blk-req-ord-prod.
        IF 
          v-cd-equipto > tt-param.tt-fim-cd-equipto THEN
          NEXT blk-req-ord-prod.
        
        ASSIGN v-cd-tecnico = "000000".
        FIND first mmi-tec-req-prod 
          NO-LOCK
          where mmi-tec-req-prod.nr-requisicao = req-ord-produc.nr-requisicao  
          and   mmi-tec-req-prod.sequencia     = req-ord-produc.sequencia
          NO-ERROR.
        IF AVAIL mmi-tec-req-prod THEN
          DO:
            ASSIGN v-cd-tecnico = mmi-tec-req-prod.cd-tecnico.
            IF v-cd-tecnico <> "0" AND v-cd-tecnico <> "000000" AND v-cd-tecnico <> ""  THEN
              DO:
                /*
                IF 
                  v-cd-tecnico < tt-param.tt-ini-cd-tecnico THEN
                  NEXT blk-req-ord-prod.  
                IF 
                  v-cd-tecnico > tt-param.tt-fim-cd-tecnico THEN
                  NEXT blk-req-ord-prod.  
                */  
              END.
          END.
        RUN CriaTTRelatorio.           
      END.
  END.
ELSE
IF i-busca = 2 OR i-busca = 0 THEN
  DO:
    blk-2-req-ord-prod:
    FOR EACH mmi-tec-req-prod 
      NO-LOCK
      where mmi-tec-req-prod.nr-requisicao >=  tt-param.tt-ini-nr-requisicao  
      AND   mmi-tec-req-prod.nr-requisicao <=  tt-param.tt-fim-nr-requisicao  
      and   mmi-tec-req-prod.cd-tecnico    >= tt-param.tt-ini-cd-tecnico
      AND   mmi-tec-req-prod.cd-tecnico    <= tt-param.tt-fim-cd-tecnico,
      EACH req-ord-produc
      NO-LOCK
      WHERE req-ord-produc.nr-requisicao = mmi-tec-req-prod.nr-requisicao
      AND   req-ord-produc.sequencia     = mmi-tec-req-prod.sequencia:
        /*--------------------------------------------------------------------------*/
        run pi-acompanhar in h-acomp(input "Nr Req: " + STRING(req-ord-produc.nr-requisicao) + " Seq:" + STRING(req-ord-produc.sequencia)).
        IF 
          req-ord-produc.nr-ord-produ < tt-param.tt-ini-nr-ord-produ THEN
          NEXT blk-2-req-ord-prod.
        IF
          req-ord-produc.nr-ord-produ > tt-param.tt-fim-nr-ord-produ THEN
          NEXT blk-2-req-ord-prod.
        IF 
          req-ord-produc.dat-trans    < tt-param.tt-ini-dat-trans    THEN
          NEXT blk-2-req-ord-prod.
        IF
          req-ord-produc.dat-trans    > tt-param.tt-fim-dat-trans    THEN
          NEXT blk-2-req-ord-prod.
        ASSIGN 
          v-cd-equipto = ""
          v-DescEqpto  = "".
        RUN buscaEqptoManutencao
         (INPUT  req-ord-produc.nr-ord-produ,
          OUTPUT v-cd-equipto,
          OUTPUT v-DescEqpto).
        IF 
          v-cd-equipto = "" THEN
          NEXT blk-2-req-ord-prod.
        IF 
          v-cd-equipto < tt-param.tt-ini-cd-equipto THEN
          NEXT blk-2-req-ord-prod.
        IF 
          v-cd-equipto > tt-param.tt-fim-cd-equipto THEN
          NEXT blk-2-req-ord-prod.
        RUN CriaTTRelatorio.
        /*--------------------------------------------------------------------------*/
      END.

  END.
run pi-excel-abrir.
assign 
  i-lin = 5.
  FOR EACH tt-relat:
    run pi-acompanhar in h-acomp(input " Imprimindo: Nr Req: " + TRIM(STRING(tt-relat.nr-requisicao)) + "-Seq:" + TRIM(STRING(tt-relat.sequencia))).
    ASSIGN 
      tt-relat.narrativa = REPLACE(tt-relat.narrativa,CHR(10),"")
      tt-relat.narrativa = REPLACE(tt-relat.narrativa,CHR(13),"")
      tt-relat.narrativa = REPLACE(tt-relat.narrativa,";",",").
    excelappl:range("B"  + STRING(i-lin)):value = STRING(tt-relat.cd-tecnico,"99999-9").
    excelappl:range("C"  + STRING(i-lin)):VALUE  = tt-relat.nome-compl.
    excelappl:range("D"  + STRING(i-lin)):value = tt-relat.nr-ord-produ.
    excelappl:range("E"  + STRING(i-lin)):value = tt-relat.nr-requisicao. 
    excelappl:range("F"  + STRING(i-lin)):value = tt-relat.sequencia.
    excelappl:range("G"  + STRING(i-lin)):value = tt-relat.cd-equipto.
    excelappl:range("H"  + STRING(i-lin)):value = tt-relat.dat-trans.
    excelappl:range("I"  + STRING(i-lin)):value = tt-relat.situacao.
    excelappl:range("J"  + STRING(i-lin)):value = tt-relat.estado.
    excelappl:range("K"  + STRING(i-lin)):value = tt-relat.it-codigo.
    excelappl:range("L"  + STRING(i-lin)):value = tt-relat.desc-item.
    excelappl:range("M"  + STRING(i-lin)):value = tt-relat.narrativa.
    excelappl:range("N"  + STRING(i-lin)):value = tt-relat.un.
    excelappl:range("O"  + STRING(i-lin)):value = tt-relat.qt-requisitada.
    excelappl:range("P"  + STRING(i-lin)):value = tt-relat.preco-unit.
    excelappl:range("Q"  + STRING(i-lin)):value = tt-relat.qt-requisitada * tt-relat.preco-unit.
    excelappl:range("R"  + STRING(i-lin)):value = tt-relat.dt-entrega.
    excelappl:range("S"  + STRING(i-lin)):value = tt-relat.tp-requis.
    excelappl:range("T"  + STRING(i-lin)):value = tt-relat.requisitante.
    excelappl:range("U"  + STRING(i-lin)):value = tt-relat.fm-codigo.
    excelappl:range("V"  + STRING(i-lin)):value = tt-relat.sc-desp.
    excelappl:range("W"  + STRING(i-lin)):value = tt-relat.ct-desp. 
    excelappl:range("X"  + STRING(i-lin)):value = tt-relat.numero-ordem.
    excelappl:range("Y"  + STRING(i-lin)):value = tt-relat.pedido-compr.
    excelappl:range("Z"  + STRING(i-lin)):value = tt-relat.nro-docto.
    excelappl:range("AA"  + STRING(i-lin)):value = tt-relat.des-man-ocor.
    excelappl:range("AB"  + STRING(i-lin)):value = tt-relat.parada-descricao .  



    ASSIGN i-lin = i-lin + 1.    
  END. /* FOR EACH tt-relat: */

run pi-finalizar in h-acomp.          
run pi-excel-fechar.
{include/i-rpclo.i}
return "OK":U.

PROCEDURE buscaEqptoManutencao :

  def input  param pNrOrdem   like req-ord-produc.nr-ord-produ no-undo.
  def output param pEqpto     like equipto.cd-equipto          no-undo.
  def output param pDescEqpto like equipto.descricao           no-undo.
  ASSIGN r-equipto = ? pEqpto = "".
  for first ord-manut 
       where ord-manut.nr-ord-produ = pNrOrdem no-lock:
       assign pEqpto = ord-manut.cd-equipto.
       FOR FIRST equipto FIELDS (cd-equipto descricao)
              WHERE equipto.cd-equipto = pEqpto NO-LOCK:
              IF AVAIL equipto THEN
                 ASSIGN pDescEqpto = equipto.descricao.
              ELSE 
                 ASSIGN pDescEqpto = "":U.
              ASSIGN r-equipto = ROWID(equipto).
       END.
  end.

END PROCEDURE.

PROCEDURE CriaTTRelatorio:
    
  DEF VAR c-tip-req AS CHAR FORMAT "X(08)"
    EXTENT 02
    INITIAL ["Estoque", "Compras"]
    NO-UNDO.
  DEF VAR c-estado AS CHAR FORMAT "X(08)"
    EXTENT 02
    INITIAL ["Aprovada","Nao Aprovada"]
    NO-UNDO.
  DEF VAR c-situacao AS CHAR FORMAT "X(08)"
    EXTENT 04
    INITIAL ["Aberta","Fechada","Pendente","Com OM"]
    NO-UNDO.
  FIND FIRST it-requisicao 
    use-index item-req
    NO-LOCK
    where it-requisicao.nr-requisicao = req-ord-produc.nr-requisicao
    and   it-requisicao.sequencia     = req-ord-produc.sequencia
    NO-ERROR.
  FIND ITEM 
    NO-LOCK
    WHERE ITEM.it-codigo = it-requisicao.it-codigo
    NO-ERROR.
  FIND equipto
    NO-LOCK
    WHERE ROWID(equipto) = r-equipto
    NO-ERROR.
  FIND first bf-ord-manut 
    no-lock
    where bf-ord-manut.nr-ord-produ = req-ord-produc.nr-ord-produ
    NO-ERROR.
  IF AVAIL bf-ord-manut THEN
    DO:
      IF req-ord-produc.tp-requis = 2 THEN
        DO:
          FIND ordem-compra 
            NO-LOCK
            WHERE ordem-compra.ordem-servic = bf-ord-manut.nr-ord-produ
            NO-ERROR.
          IF AVAIL ordem-compr THEN
            DO:
              FIND pedido-compr
                NO-LOCK
                WHERE pedido-compr.num-pedido = ordem-compr.num-pedido
                NO-ERROR.
              FIND FIRST item-doc-est
                NO-LOCK
                USE-INDEX itmdctst-09
                WHERE  item-doc-est.num-pedido   = pedido-compr.num-pedido 
                AND    item-doc-est.numero-ordem = ordem-compr.numero-ordem
                /* AND    item-doc-est.it-codigo    = it-requisicao.it-codigo */
                NO-ERROR.
            END.
        END.
    END.

    IF AVAIL(mmi-tec-req-prod) THEN DO:
        FIND FIRST tecn-mi NO-LOCK
             WHERE tecn-mi.cd-tecnico = mmi-tec-req-prod.cd-tecnico NO-ERROR.
    END.

    IF AVAIL(bf-ord-manut) THEN DO:
        FIND FIRST mi-parada NO-LOCK
             WHERE mi-parada.cd-parada = bf-ord-manut.cd-parada NO-ERROR.

        FIND FIRST tipo-manut OF bf-ord-manut NO-LOCK NO-ERROR.
    END.

  CREATE tt-relat.
  
  ASSIGN
    tt-relat.cd-tecnico      = IF AVAIL mmi-tec-req-prod THEN mmi-tec-req-prod.cd-tecnico ELSE "0"
    tt-relat.nome-compl      = IF AVAIL tecn-mi THEN tecn-mi.nome-compl ELSE ""
    tt-relat.nr-ord-produ    = req-ord-produc.nr-ord-produ  
    tt-relat.nr-requisicao   = req-ord-produc.nr-requisicao 
    tt-relat.sequencia       = req-ord-produc.sequencia     
    tt-relat.cd-equipto      = IF AVAIL equipto THEN equipto.cd-equipto ELSE "NOK"
    tt-relat.descricao       = v-DescEqpto 
    tt-relat.cod-estabel     = req-ord-produc.cod-estabel   
    tt-relat.dat-trans       = req-ord-produc.dat-trans     
    tt-relat.situacao        = c-situacao[it-requisicao.situacao]       
    tt-relat.estado          = c-estado[it-requisicao.estado]         
    tt-relat.it-codigo       = it-requisicao.it-codigo      
    tt-relat.desc-item       = IF AVAIL ITEM THEN item.desc-item ELSE "NOK"
    tt-relat.narrativa       = it-requisicao.narrativa
    tt-relat.un              = IF AVAIL ITEM THEN ITEM.un        ELSE "NOK"              
    tt-relat.qt-requisitada  = IF AVAIL it-requisicao THEN it-requisicao.qt-requisitada ELSE 0
    tt-relat.preco-unit      = IF AVAIL it-requisicao THEN it-requisicao.preco-unit     ELSE 0
    tt-relat.dt-entrega      = IF AVAIL it-requisicao THEN it-requisicao.dt-entrega     ELSE ?
    tt-relat.tp-requis       = c-tip-req[req-ord-produc.tp-requis]
    tt-relat.requisitante    = it-requisicao.nome-abrev
    tt-relat.fm-codigo       = item.fm-codigo
    tt-relat.sc-desp         = IF AVAIL bf-ord-manut THEN bf-ord-manut.sc-desp ELSE ""
    tt-relat.ct-desp         = IF AVAIL bf-ord-manut THEN bf-ord-manut.ct-desp ELSE "NOK"
    tt-relat.numero-ordem    = IF AVAIL ordem-compra AND req-ord-produc.tp-requis = 2 THEN ordem-compra.numero-ordem ELSE 0 
    tt-relat.pedido-compr    = IF AVAIL pedido-compr AND req-ord-produc.tp-requis = 2  AND tt-relat.numero-ordem > 0 THEN pedido-compr.num-pedido ELSE 0 
    tt-relat.nro-docto       = IF AVAIL item-doc-est AND req-ord-produc.tp-requis = 2  AND tt-relat.numero-ordem > 0 THEN item-doc-est.nro-docto ELSE "" /* item-doc-est.it-codigo */
    tt-relat.des-man-ocor    = IF AVAIL tipo-manut THEN tipo-manut.descricao ELSE ""
    tt-relat.parada-descricao = IF AVAIL(mi-parada)   THEN mi-parada.descricao ELSE ""
    .
       
END PROCEDURE.

PROCEDURE pi-excel-abrir :
                                 
  ASSIGN c-modelo = search ('esp\escd0738.xlt').
  CREATE "excel.application" excelappl.
  excelappl:VISIBLE = FALSE.
  excelappl:workbooks:ADD(c-modelo).
  excelappl:worksheets:ITEM(1):SELECT.

END PROCEDURE.
PROCEDURE pi-excel-fechar :
  excelappl:Rows(STRING(i-lin) + ":" + STRING(i-lin + 30000)):SELECT.
  excelappl:Selection:Delete().
  excelappl:Cells:SELECT(). /* precisa selecionar todas primeiro */
  excelappl:Cells:EntireColumn:AutoFit.
  excelappl:Cells:RowHeight = 19.
  excelappl:VISIBLE = TRUE.
  excelappl:worksheets:ITEM(1):SELECT.
  RELEASE OBJECT excelappl   NO-ERROR.
  RELEASE OBJECT workbooks    NO-ERROR.
  RELEASE OBJECT worksheets   NO-ERROR.

END PROCEDURE.
