/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
********************************************************************************/
{include/i-prgvrs.i ESCC0406EXCEL 2.00.00.048 } /*** 010048 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESCC0406a MCC}
&ENDIF

/*******************************************************************************
**
**   ESCC0406A.P - relatorio de pedidos emitidos por ordem de pedido
**
*******************************************************************************/
{cdp/cdcfgdis.i} /* include versao ems */
{cdp/cdcfgmat.i}

/* Variaveis e temp-tables comuns */
{ccp/ESCC0406.i5}

{cep/ce1234.i} /* Valida‡Æo Decimais */

def input param raw-param as raw no-undo.

def buffer b-emitente for emitente.

DEFINE VARIABLE de-vl-descto  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-total   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-preco-orig AS DECIMAL     NO-UNDO.

DEFINE VARIABLE c-arquivo        AS    CHARACTER             NO-UNDO.
DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.

{include/i-rpvar.i}

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Pedidos_Emitidos *}
run pi-inicializar in h-acomp (input  Return-value ).

create tt-param.
raw-transfer raw-param to tt-param.

find first param-global NO-LOCK no-error.
if  avail param-global then
    assign c-empresa = grupo.

assign c-programa  = "ESCC0406"
       c-versao    = "1.00"
       c-revisao   = "000"
       l-imprimiu  = no
       de-tot-ped  = 0
       de-tot-liq  = 0
       de-tot-ipi  = 0
       i-nr-pedido = 0
       i-narrativa = 0
       de-qt-tot-ped = 0.

{utp/ut-liter.i COMPRAS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Pedidos_Emitidos_de * r}
assign c-titulo-relat = trim(return-value) + " "
                      + string(tt-param.da-iniper, "99/99/9999") + " ".
{utp/ut-liter.i a * r}
assign c-titulo-relat = c-titulo-relat
                      + trim(return-value) + " "
                      + string(tt-param.da-fimper,"99/99/9999").

{include/i-rpcab.i}

RUN pi-abre-excel.

for each pedido-compr
    where pedido-compr.num-pedido   >= tt-param.i-pedido-i
    and   pedido-compr.num-pedido   <= tt-param.i-pedido-f
    and   pedido-compr.data-pedido  >= tt-param.da-iniper
    and   pedido-compr.data-pedido  <= tt-param.da-fimper
    and   pedido-compr.cod-emitente >= tt-param.i-fornec-i
    and   pedido-compr.cod-emitente <= tt-param.i-fornec-f
    and   pedido-compr.situacao     <> 3 no-lock:

    RUN pi-acompanhar IN h-acomp (INPUT pedido-compr.num-pedido).

    assign l-imp-ped     = yes.
    find emitente where 
         emitente.cod-emitente = pedido-compr.cod-emitente no-lock no-error.

    for each ordem-compra
        where ordem-compra.num-pedido    = pedido-compr.num-pedido
        and   ordem-compra.cod-estabel  >= tt-param.c-estabel-i
        and   ordem-compra.cod-estabel  <= tt-param.c-estabel-f
        and   ordem-compra.cod-comprado >= tt-param.c-comp-i        
        and   ordem-compra.cod-comprado <= tt-param.c-comp-f        
        and ((ordem-compra.situacao    <> 4 and tt-param.l-receb)
        or    ordem-compra.situacao     = 2) no-lock
        break by ordem-compra.cod-estabel
              by ordem-compra.num-pedido
              by ordem-compra.numero-ordem:

        if l-emergencial = yes and pedido-compr.emergencial = no then next.

        if  tt-param.l-alter
        and ordem-compra.nr-alt-preco = 0 then next.

        run pi-acompanhar in h-acomp (input ordem-compra.it-codigo).

        find item
            where item.it-codigo = ordem-compra.it-codigo no-lock no-error.

        &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
            DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
            ASSIGN cAuxTraducao001 = {ininc/i01in274.i 04 ordem-compra.natureza}.
            run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                                INPUT "",
                                INPUT "").
            ASSIGN  c-natureza = RETURN-VALUE.
        &else
            ASSIGN c-natureza = {ininc/i01in274.i 04 ordem-compra.natureza}.
        &endif

        if avail item then
            assign c-desc-item = item.desc-item.                

        find moeda where moeda.mo-codigo = tt-param.i-moeda  no-lock no-error .
        assign tt-param.c-moeda = moeda.descricao.

        run cdp/cd0812.p (input ordem-compra.mo-codigo,
                          input tt-param.i-moeda,
                          input ordem-compra.preco-unit,
                          input ordem-compra.data-cotacao,
                          output de-preco-conv).

        if  de-preco-conv = ? then do:
            assign de-preco-conv = ordem-compra.preco-unit.
            find moeda where moeda.mo-codigo = ordem-compra.mo-codigo NO-LOCK NO-ERROR.
            assign tt-param.c-moeda = moeda.descricao.
        end.

        l-imp-ord = yes.
        for each prazo-compra
            where prazo-compra.numero-ordem = ordem-compra.numero-ordem
            and ((tt-param.l-receb and prazo-compra.situacao <> 4)
            or    prazo-compra.situacao     = 2) no-lock:
            i-narrativa = 0.
            /*repeat:*/

                if  l-preco-bruto = no then
                    assign de-valor-ipi = round((de-preco-conv
                                        * ordem-compra.aliquota-ipi)
                                        / (100 + ordem-compra.aliquota-ipi),5).
                else do:
                    if  ordem-compra.perc-descto > 0 then
                        assign de-val-orig = (de-preco-conv * 100)
                                           / (100 - ordem-compra.perc-descto).
                    else
                        assign de-val-orig = de-preco-conv.

                    assign de-valor-ipi  = round((de-val-orig
                                         * ordem-compra.aliquota-ipi)
                                         / (100 + ordem-compra.aliquota-ipi),5).
                end.
                assign de-preco-unit   = de-preco-conv - de-valor-ipi.

                /*** fn_ajust_dec => fun‡Æo definida no include ce1234.i, 
                     utilizada para valida‡Æo de decimais ***/
                assign de-preco-merc   = fn_ajust_dec((prazo-compra.quantidade * de-preco-unit), 
                                         tt-param.i-moeda).

                assign de-valor-ipi    = fn_ajust_dec(round((prazo-compra.quantidade * de-valor-ipi),5), 
                                         tt-param.i-moeda). 

                assign de-preco-total  = de-preco-merc + de-valor-ipi. 

                run cdp/cd0812.p (input ordem-compra.mo-codigo,   
                                  input tt-param.i-moeda,         
                                  input ordem-compra.preco-fornec,  
                                  input ordem-compra.data-cotacao,
                                  output de-preco-orig). 

                ASSIGN de-vl-total = de-preco-orig * prazo-compra.quantidade.

                IF de-vl-total >= de-valor-ipi THEN

                    ASSIGN de-valor-descto = (de-vl-total * ordem-compra.perc-descto) / 100. 

                /*assign de-valor-descto = fn_ajust_dec((prazo-compra.quantidade * ordem-compra.valor-descto), 
                                         tt-param.i-moeda).         */                                         

                find first recebimento use-index ordem
                    where recebimento.numero-ordem = prazo-compra.numero-ordem
                    and   recebimento.parcela      = prazo-compra.parcela
                    no-lock no-error.
                if  avail recebimento
                and ordem-compra.situacao = 6 then
                    assign i-atraso = recebimento.data-movto
                                    - prazo-compra.data-entrega.
                else
                    assign i-atraso = today - prazo-compra.data-entrega.

                if  l-imp-ped or l-imp-ord then do:
                    if  tt-param.l-narrativa = yes then do:
                        /* Verifica o Nœmero de Linhas da Narrativa */
                        {ccp/ESCC0406.i6}
                    end.
                end.
                    /*******************************************/
                    &if defined (bf_mat_oper_triangular) &then                      
                        if pedido-compr.cod-emit-terc <> 0 then 
                            for first b-emitente fields(nome-abrev) 
                                where b-emitente.cod-emitente = pedido-compr.cod-emit-terc no-lock:
                                assign c-nome-abrev = b-emitente.nome-abrev.
                            end.                                                                                                                                                                                                                                                                                                                                                    
                        else assign c-nome-abrev = "".    
                    &endif
                    
            RUN pi-dados-excel.

            assign de-tot-ped    = de-tot-ped + de-preco-total
                   de-tot-liq    = de-tot-liq + de-preco-merc
                   de-tot-ipi    = de-tot-ipi + de-valor-ipi
                   de-tot-descto = de-tot-descto + de-valor-descto
                   de-qt-tot-ped = de-qt-tot-ped + prazo-compra.quantidade.
        end.
    end.

    /* Integracao Modulo Importacao */
    if param-global.modulo-07 then
        if tt-param.l-despesas then do:
            run imp/im9035.p (input pedido-compr.num-pedido,
                              input ?,
                              input ?,                              
                              input tt-param.i-moeda,
                              input tt-param.i-despesas-pag,
                              input tt-param.l-despesas-inc,
                              output de-val-desp).
            if tt-param.l-despesas-inc then
                assign de-tot-ped = de-tot-ped + de-val-desp.                              
        end.

    if  l-imprimiu then do:
        assign de-tot-ger    = de-tot-ger + de-tot-ped
               de-ger-liq    = de-ger-liq + de-tot-liq
               de-ger-ipi    = de-ger-ipi + de-tot-ipi
               de-ger-descto = de-ger-descto + de-tot-descto
               de-tot-ped    = 0
               de-tot-liq    = 0
               de-tot-ipi    = 0
               de-tot-descto = 0
               l-imprimiu    = no
               i-nr-pedido   = i-nr-pedido + 1.
    end.
end.

if  i-nr-pedido <> 0 then do:
    assign de-ger-liq = 0
           de-ger-ipi = 0
           de-tot-ger = 0.
end.

RUN pi-encerra-excel.

run pi-finalizar in h-acomp.

{include/pi-edit.i}

procedure pi-abre-excel:
   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + ".xls".
   
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:ADD().
   ch-excel:ActiveSheet:NAME = "ESCC0406-PEDIDOS-EMITIDOS".

   ch-excel:Range("A2"):select.

   ch-excel:ActiveWindow:FreezePanes = true.

   ASSIGN i-cont-linha = 1.

   ASSIGN ch-excel:columns( "A"):NumberFormat = "@"
          ch-excel:columns( "B"):NumberFormat = "dd/mm/aaaa;@"
          ch-excel:columns( "C"):NumberFormat = "@"
          ch-excel:columns( "D"):NumberFormat = "@"
          ch-excel:columns( "E"):NumberFormat = "#.##0"
          ch-excel:columns( "F"):NumberFormat = "@"           
          ch-excel:columns( "G"):NumberFormat = "@"           
          ch-excel:columns( "H"):NumberFormat = "@"    
          ch-excel:columns( "I"):NumberFormat = "@"    
          ch-excel:columns( "J"):NumberFormat = "@"    
          ch-excel:columns( "K"):NumberFormat = "#.##0,00"
          ch-excel:columns( "L"):NumberFormat = "@"
          ch-excel:columns( "M"):NumberFormat = "@"
          ch-excel:columns( "N"):NumberFormat = "@"
          ch-excel:columns( "O"):NumberFormat = "@"
          ch-excel:columns( "P"):NumberFormat = "@"           
          ch-excel:columns( "Q"):NumberFormat = "@"           
          ch-excel:columns( "R"):NumberFormat = "@"           
          ch-excel:columns( "S"):NumberFormat = "dd/mm/aaaa;@"
          ch-excel:columns( "T"):NumberFormat = "dd/mm/aaaa;@"           
          ch-excel:columns( "U"):NumberFormat = "#.##0,0000"           
          ch-excel:columns( "V"):NumberFormat = "#.##0,0000"    
          ch-excel:columns( "W"):NumberFormat = "#.##0,00"  
          ch-excel:columns( "X"):NumberFormat = "#.##0,00"  
          ch-excel:columns( "Y"):NumberFormat = "#.##0,00"  
          ch-excel:columns( "Z"):NumberFormat = "@"  
          ch-excel:columns("AA"):NumberFormat = "#.##0"  
          ch-excel:columns("AB"):NumberFormat = "#.##0". 

   ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Pedido"
          ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE = "Dt Ped"
          ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE = "Natureza"
          ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE = "Nome Abrev"
          ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE = "Cond Pagto"
          ch-excel:Range( "F" + STRING(i-cont-linha,'99999')):VALUE = "Numero Ordem"
          ch-excel:Range( "G" + STRING(i-cont-linha,'99999')):VALUE = "Estabelec"
          ch-excel:Range( "H" + STRING(i-cont-linha,'99999')):VALUE = "Item"
          ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = "Descri‡Æo"
          ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = "Un"
          ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = "Vl Unit"
          ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = "Moeda"
          ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = "Cod Alt"
          ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = "No Ab Tr"
          ch-excel:Range( "O" + STRING(i-cont-linha,'99999')):VALUE = "Unid Negoc"
          ch-excel:Range( "P" + STRING(i-cont-linha,'99999')):VALUE = "Narrativa"
          ch-excel:Range( "Q" + STRING(i-cont-linha,'99999')):VALUE = "Parc"
          ch-excel:Range( "R" + STRING(i-cont-linha,'99999')):VALUE = "Situa‡Æo"
          ch-excel:Range( "S" + STRING(i-cont-linha,'99999')):VALUE = "Dt EmissÆo"
          ch-excel:Range( "T" + STRING(i-cont-linha,'99999')):VALUE = "Dt Entrega"
          ch-excel:Range( "U" + STRING(i-cont-linha,'99999')):VALUE = "Quantidade"
          ch-excel:Range( "V" + STRING(i-cont-linha,'99999')):VALUE = "Saldo"
          ch-excel:Range( "W" + STRING(i-cont-linha,'99999')):VALUE = "Vl Merc"
          ch-excel:Range( "X" + STRING(i-cont-linha,'99999')):VALUE = "Vl IPI"
          ch-excel:Range( "Y" + STRING(i-cont-linha,'99999')):VALUE = "Vl Total"
          ch-excel:Range( "Z" + STRING(i-cont-linha,'99999')):VALUE = "Pedido Cliente"
          ch-excel:Range("AA" + STRING(i-cont-linha,'99999')):VALUE = "Seq"
          ch-excel:Range("AB" + STRING(i-cont-linha,'99999')):VALUE = "Atraso".

   ASSIGN i-cont-linha = i-cont-linha + 1.
END PROCEDURE.

PROCEDURE pi-dados-excel:
   ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = pedido-compr.num-pedido  
          ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE = pedido-compr.data-pedido 
          ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE = c-natureza               
          ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE = emitente.nome-abrev      
          ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE = pedido-compr.cod-cond-pag
          ch-excel:Range( "F" + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.numero-ordem
          ch-excel:Range( "G" + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.cod-estabel 
          ch-excel:Range( "H" + STRING(i-cont-linha,'99999')):VALUE = IF AVAILABLE(ITEM) THEN item.it-codigo ELSE ""
          ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = IF AVAILABLE(ITEM) THEN c-desc-item    ELSE ""
          ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = prazo-compra.un      
          ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = de-preco-unit        
          ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = tt-param.c-moeda     
          ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = IF prazo-compra.cod-alt THEN "Sim" ELSE "NÆo".

   &if defined (bf_mat_oper_triangular) &then
      ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = c-nome-abrev.
   &endif     
   
   /*Unidade de Neg¢cio*/
   &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
       ch-excel:Range( "O" + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.cod-unid-negoc.
   &ENDIF
   
   if  tt-param.l-narrativa = yes then do:
      if  avail item and avail narrativa then do:
          if  item.it-codigo = "" and  narrativa.descricao <> "" then do:
             ASSIGN ch-excel:Range( "P" + STRING(i-cont-linha,'99999')):VALUE = narrativa.descricao.
          end.
      end.
      if  ordem-compra.narrativa <> "" then do:
         ASSIGN ch-excel:Range( "P" + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.narrativa.
      end.
   end.

   ASSIGN ch-excel:Range( "Q" + STRING(i-cont-linha,'99999')):VALUE = prazo-compra.parcela     
          ch-excel:Range( "R" + STRING(i-cont-linha,'99999')):VALUE = c-sit                    
          ch-excel:Range( "S" + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.data-emissao
          ch-excel:Range( "T" + STRING(i-cont-linha,'99999')):VALUE = prazo-compra.data-entrega
          ch-excel:Range( "U" + STRING(i-cont-linha,'99999')):VALUE = prazo-compra.quantidade  
          ch-excel:Range( "V" + STRING(i-cont-linha,'99999')):VALUE = prazo-compra.quant-saldo 
          ch-excel:Range( "W" + STRING(i-cont-linha,'99999')):VALUE = de-preco-merc            
          ch-excel:Range( "X" + STRING(i-cont-linha,'99999')):VALUE = de-valor-ipi             
          ch-excel:Range( "Y" + STRING(i-cont-linha,'99999')):VALUE = de-preco-total           
          ch-excel:Range( "Z" + STRING(i-cont-linha,'99999')):VALUE = prazo-compra.pedido-clien
          ch-excel:Range("AA" + STRING(i-cont-linha,'99999')):VALUE = prazo-compra.nr-sequencia
          ch-excel:Range("AB" + STRING(i-cont-linha,'99999')):VALUE = i-atraso.

   ASSIGN i-cont-linha = i-cont-linha + 1.
END PROCEDURE.

PROCEDURE pi-encerra-excel:
   /* Encerra o excel */
   ch-excel:application:DisplayAlerts = false.

   ch-excel:Cells:select.
   ch-excel:Cells:EntireColumn:AutoFit.

   ch-excel:ActiveSheet:PageSetup:orientation = 2. 

   IF i-cont-linha <> 1 THEN DO:
      ch-excel:Range("A1:AB" + string(i-cont-linha - 1)):autofilter(,,,).
   END.

   ch-excel:Range("A1:AB" + string(i-cont-linha - 1)):select.

   case tt-param.destino:
       when 1 then do:
          ch-excel:worksheets:item(1):select.
          ch-excel:Sheets:PrintOut.
          ch-excel:application:DisplayAlerts = false.
          ch-excel:quit().
       end.
       when 2 then do:
          ch-excel:visible = false.
          ch-excel:ActiveWorkbook:close(yes,c-arquivo).
       end.
       when 3 then do:
          ch-excel:visible = true.
       end.
   end case.

   release object ch-excel no-error.
END PROCEDURE.
