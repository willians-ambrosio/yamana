/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer moeda for ems2cadme.moeda.

{include/i-prgvrs.i CC0302D 2.00.00.024}  /*** 010024 ***/
/********************************************************************************
**
**   Programa: CC0302d.p
**
********************************************************************************/

{utp/utapi009.i}

def input param raw-param           as raw     no-undo.
def input param  r-ordem-compra      as rowid   no-undo.
def input param r-item-fornec       as rowid   no-undo.
def input param l-primeiro-emitente as logical no-undo.
def input param l-ultimo-emitente   as logical no-undo.
def input param l-ultimo-cod-emit   as logical no-undo.


{ccp/cc0302.i3} /* Definicao da temp-table tt-param */
{cdp/cdcfgmat.i}
    
def var AppWord      as com-handle no-undo.
def var de-indice    as decimal    no-undo.
def var de-quant     as decimal    no-undo format "99,999,999,999.99" decimals 2.
def var c-natureza   as char       no-undo format "x(15)".
def var c-moeda      as char       no-undo format "x(10)".
def var i-count      as int        no-undo.
def var c-mod-cot    as char       no-undo.
def var c-mod-cot2   as char       no-undo.
def var c-arq1       as char       no-undo.
def var c-arq2       as char       no-undo.
def var c-prog       as char       no-undo.
def var h-acomp      as handle     no-undo.
def var r-registro   as rowid      no-undo.

def var c-preenche-via-web as char no-undo.
def var c-url-wcc0303a     as char no-undo.

def var c-assunto-e-mail  as char format "x(17)".
def var c-mensagem-e-mail as char format "x(42)" no-undo.
def var c-titulo-erro     as char format "x(56)" no-undo.
def var c-cod             as char format "x(8)"  no-undo.
def var c-desc            as char format "x(13)" no-undo.
def var c-trc-cod         as char format "x(10)" no-undo.
def var c-trc-desc        as char format "x(80)" no-undo.

def var c-contato   as char no-undo.
def var c-fone      as char no-undo.
def var c-item      as char no-undo.
def var c-it-forn   as char no-undo.
def var c-ordem     as char no-undo.
def var c-linha1    as char no-undo.
def var c-linha2    as char no-undo.
def var c-compra    as char no-undo.
def var c-tit-inic  as char no-undo.

form c-titulo-erro      at 01 skip(1)
     c-cod              at 01
     c-desc             at 13
     c-trc-cod          at 01
     c-trc-desc         at 13
     tt-erros.cod-erro  at 01 format ">>>>>>>>>9"
     tt-erros.desc-erro at 13 format "x(80)"
     with stream-io no-label width 132 frame f-erros-e-mail.

{utp/ut-liter.i Cotaá∆o_de_Item * L}
    assign c-assunto-e-mail = return-value.
{utp/ut-liter.i Contato * r}
    assign c-contato = trim(return-value) + ": ".
{utp/ut-liter.i Telefone * r}
    assign c-fone = trim(return-value) + ": ".
{utp/ut-liter.i Item * r}
    assign c-item = trim(return-value) + ": ".
{utp/ut-liter.i Item_Fornec * r}
    assign c-it-forn = "                      " + trim(return-value) + ": ".
{utp/ut-liter.i Ordem * r}
    assign c-ordem  = "     "  + trim(return-value) + "     ".
           c-linha1 = c-linha1 + trim(return-value) + "       ".
{utp/ut-liter.i Natureza * r}
    assign c-linha1 = c-linha1 + trim(return-value) + "          ".
{utp/ut-liter.i Qtdade_da_Ordem * r}
    assign c-linha1 = c-linha1 + trim(return-value) + "       ".
{utp/ut-liter.i Un * r}
    assign c-linha1 = c-linha1 + trim(return-value) + "   ".
{utp/ut-liter.i Emiss∆o * r}
    assign c-linha1 = c-linha1 + trim(return-value) + "       ".
{utp/ut-liter.i Requisitante * r}
    assign c-linha1 = c-linha1 + trim(return-value) + "     ".
{utp/ut-liter.i Comprador * r}
    assign c-linha1 = c-linha1 + trim(return-value).
{utp/ut-liter.i Èltima_Compra * r}
    assign c-compra = "      " + trim(return-value) + "      ".
    
{utp/ut-liter.i Fornec * r}
    assign c-linha2 = c-linha2 + trim(return-value) + "       ".
{utp/ut-liter.i Pedido * r}
    assign c-linha2 = c-linha2 + trim(return-value) + "     ".
{utp/ut-liter.i Data * r}
    assign c-linha2 = c-linha2 + trim(return-value) + "     ".
{utp/ut-liter.i Natureza * r}
    assign c-linha2 = c-linha2 + trim(return-value) + "        ".
{utp/ut-liter.i Un * r}
    assign c-linha2 = c-linha2 + trim(return-value) + " ".
{utp/ut-liter.i Pgto * r}
    assign c-linha2 = c-linha2 + trim(return-value) + " ".
{utp/ut-liter.i Moeda * r}
    assign c-linha2 = c-linha2 + trim(return-value) + " ".
{utp/ut-liter.i Preáo_Unit_For * r}
    assign c-linha2 = c-linha2 + trim(return-value) + "  ".
{utp/ut-liter.i Quantidade * r}
    assign c-linha2 = c-linha2 + trim(return-value) + "    ".
{utp/ut-liter.i IPI * r}
    assign c-linha2 = c-linha2 + trim(return-value).

{utp/ut-liter.i Emiss∆o_de_Ficha_para_Cotaá∆o * r}
assign c-tit-inic = trim(return-value).
{utp/ut-liter.i Arquivo_Anexo_com_dados_da_Solicitaá∆o_de_Cotaá∆o: * L}
assign c-mensagem-e-mail = trim(return-value).
{utp/ut-liter.i OCORRERAM_OS_ERROS_ABAIXO_DURANTE_O_ENVIO_DE_E-MAIL: * r }
assign c-titulo-erro = trim(return-value).
{utp/ut-liter.i C¢digo * r }
assign c-cod = trim(return-value).
{utp/ut-liter.i Descriá∆o * r }
assign c-desc = trim(return-value).
assign c-trc-cod  = fill("-",10)
       c-trc-desc = fill("-",80).

find first param-compra
    no-lock no-error.
if avail param-compra then do:
    assign c-arq1 = trim(substr(param-compra.char-1, 1, 50))
           c-arq2 = trim(substr(param-compra.char-1, 51, 100))
           c-arq1 = replace (c-arq1, "/","\").

    if  substring(trim(c-arq1),length(trim(c-arq1)),1) <> "\" then do:
        assign c-arq1 = c-arq1 + "\".
    end.
    if  substring(trim(c-arq2),length(trim(c-arq2)),1) <> "\" then do:
        assign c-arq2 = c-arq2 + "\".
    end.
end.

assign c-mod-cot  = trim(search("modelos\mod-cot.doc"))
       c-mod-cot2 = trim(search("modelos\mod-cot2.doc"))
       c-prog     = "".

/* Propriedade ProtectionType */
&global-define wdAllowOnlyComments      1
&global-define wdAllowOnlyFormFields    2
&global-define wdAllowOnlyRevisions     0
&global-define wdNoProtection           -1



def buffer b-ordem for ordem-compra.
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input c-tit-inic ).

/*create "Word.Application" AppWord.
AppWord:visible = FALSE.  /* Alteramos esta linha */*/


find ordem-compra where rowid(ordem-compra) = r-ordem-compra         no-wait no-error.
find item         where item.it-codigo      = ordem-compra.it-codigo no-lock no-error.
find item-fornec  where rowid(item-fornec)  = r-item-fornec          no-lock no-error.
find emitente
     where emitente.cod-emitente  = item-fornec.cod-emitente
     and   emitente.e-mail       <> "" no-lock no-error.

if  not avail ordem-compra
or  locked ordem-compra
or  not avail item
or  not avail item-fornec
or  not avail emitente then DO:
/*   Appword:Quit().              */
/*   release object appWord.      */

  run pi-finalizar in h-acomp. 
  return.
END. 

create "Word.Application" AppWord.
AppWord:visible = NO.  /* Alteramos esta linha */
run pi-acompanhar in h-acomp (input string(emitente.cod-emitente) + " - "
                                     +     ordem-compra.it-codigo).

AppWord:Documents:Open(c-mod-cot,False,False,False,"","",False).

if  l-primeiro-emitente = yes then do:
    find first cont-emit where cont-emit.cod-emitente = emitente.cod-emitente no-lock no-error.
    
    /*AppWord:Documents:Open(c-mod-cot,False,False,False,"","",False).*/

    Appword:Selection:TypeParagraph .
    Appword:Selection:TypeParagraph .

    Appword:Selection:Font:Name = "Arial Narrow" .
    Appword:Selection:Font:Size = 12.
    Appword:Selection:Font:Bold = true.

    Appword:Selection:TypeText(emitente.cod-emite).
    Appword:Selection:TypeText(" - ").
    Appword:Selection:TypeText(emitente.nome-abrev).
    Appword:Selection:TypeText(" - ").
    Appword:Selection:TypeText(emitente.nome-emit).
    Appword:Selection:TypeParagraph .
    Appword:Selection:TypeText(emitente.cidade).
    Appword:Selection:TypeText(" - ").
    Appword:Selection:TypeText(emitente.cep).
    Appword:Selection:TypeText(" - ").
    Appword:Selection:TypeText(emitente.estado).
    Appword:Selection:TypeText(" - ").
    Appword:Selection:TypeText(emitente.pais).
    Appword:Selection:TypeParagraph .
    Appword:Selection:TypeText(c-contato).

    if  avail cont-emit then
        Appword:Selection:TypeText(trim(cont-emit.nome)).
    else
        Appword:Selection:TypeText("            ").

    Appword:Selection:TypeText("    ").
    Appword:Selection:TypeText(c-fone).
    Appword:Selection:TypeText(" - ").

    if  avail cont-emit then
        Appword:Selection:TypeText(trim(cont-emit.telefone)).

end.

/**
***   Imprime dados da ordem
**/

assign de-quant = 0.
for each  prazo-compra no-lock
    where prazo-compra.numero-ordem = ordem-compra.numero-ordem:
    assign de-quant = de-quant + prazo-compra.quantidade.
end.
assign c-natureza = {ininc/i01in274.i 04 ordem-compra.natureza}
       de-indice  = item-fornec.fator-conver / if item-fornec.num-casa-dec = 0
                    then 1 else exp(10,item-fornec.num-casa-dec)
       de-quant   = de-quant * de-indice.

/**
*** OLE Automation com Word para geraá∆o do item da cotaá∆o
**/

Appword:Selection:TypeParagraph .
Appword:Selection:TypeParagraph .
Appword:Selection:Font:Name = "Arial" .
Appword:Selection:Font:Size = 12.
Appword:Selection:Font:Bold = true.

Appword:Selection:TypeText(c-item).
Appword:Selection:TypeText(Ordem-compra.it-codigo).
Appword:Selection:TypeText(" - ").
Appword:Selection:TypeText(item.desc-item).
Appword:Selection:TypeParagraph .
Appword:Selection:TypeText(c-it-forn).
Appword:Selection:TypeText(" - ").
if  avail item-fornec then
    if  item-fornec.item-do-forn <> " " then
        Appword:Selection:TypeText(item-fornec.item-do-forn).
    else
        Appword:Selection:TypeText("                ").
else
    Appword:Selection:TypeText("                ").

Appword:Selection:Font:Name = "Courier New" .
Appword:Selection:Font:Size = 9.
Appword:Selection:TypeParagraph .
Appword:Selection:TypeParagraph .
Appword:Selection:TypeText("-------------------------------------").
Appword:Selection:TypeText(c-ordem).
Appword:Selection:TypeText("-------------------------------------").
Appword:Selection:TypeParagraph .
Appword:Selection:Font:Size = 8.
Appword:Selection:Font:Bold = false.
Appword:Selection:TypeText(c-linha1).
Appword:Selection:TypeParagraph .
Appword:Selection:TypeText("---------   ---------------   ----------------      --   --------").
Appword:Selection:TypeText("      ------------     ----------").
Appword:Selection:TypeParagraph .
Appword:Selection:TypeText(string(Ordem-compra.numero-ordem,">>>>>>99")).
Appword:Selection:TypeText("    ").
Appword:Selection:TypeText(string(c-natureza,"x(15)")).
Appword:Selection:TypeText("  ").
Appword:Selection:TypeText(STRING(de-quant,">>,>>>,>>>,>>9.99")).
Appword:Selection:TypeText("      ").
Appword:Selection:TypeText(string(item-fornec.unid-med-for,"xx")).
Appword:Selection:TypeText("   ").
Appword:Selection:TypeText(ordem-compra.data-emissao).
Appword:Selection:TypeText("      ").
Appword:Selection:TypeText(string(ordem-compra.requisitante,"x(12)")).
Appword:Selection:TypeText("     ").
Appword:Selection:TypeText(string(ordem-compra.cod-comprado,"x(12)")).

/**
***  Imprime dados da £ltima compra
**/

assign de-quant = 0.
find last  b-ordem use-index compra-item
     where b-ordem.it-codigo = ordem-compra.it-codigo
     and   b-ordem.data-pedido <> ?
     no-lock no-error.

Appword:Selection:Font:Name = "Courier New" .
Appword:Selection:Font:Size = 9.
Appword:Selection:Font:Bold = true.
Appword:Selection:TypeParagraph .
Appword:Selection:TypeParagraph .
Appword:Selection:TypeParagraph .
Appword:Selection:TypeText("--------------------------------").
Appword:Selection:TypeText(c-compra).
Appword:Selection:TypeText("--------------------------------").
Appword:Selection:TypeParagraph .
Appword:Selection:Font:Size = 8.
Appword:Selection:Font:Bold = false.
Appword:Selection:TypeText(c-linha2).
Appword:Selection:TypeParagraph .
Appword:Selection:TypeText("------------ ---------- -------- --------------- -- ").
Appword:Selection:TypeText("---- ----- --------------  ----------    -------").
if  avail b-ordem then do:
    for each prazo-compra
        where prazo-compra.numero-ordem = b-ordem.numero-ordem no-lock:
        assign de-quant = de-quant + prazo-compra.qtd-do-forn.
    end.
    find moeda where moeda.mo-codigo = b-ordem.mo-codigo no-lock no-error.

    assign c-moeda    = (if avail moeda then moeda.descricao else "")
           c-natureza = {ininc/i01in274.i 04 b-ordem.natureza}.

    Appword:Selection:TypeParagraph .
    Appword:Selection:TypeText(string(emitente.nome-abrev,"x(12)")).
    Appword:Selection:TypeText(" ").
    Appword:Selection:TypeText(string(b-ordem.num-pedido,">>,>>>,>>9")).
    Appword:Selection:TypeText(" ").
    Appword:Selection:TypeText(b-ordem.data-pedido).
    Appword:Selection:TypeText(" ").
    Appword:Selection:TypeText(string(c-natureza,"x(15)")).
    Appword:Selection:TypeText(" ").
    Appword:Selection:TypeText(string(item.un,"xx")).
    Appword:Selection:TypeText("  ").
    Appword:Selection:TypeText(string(b-ordem.cod-cond-pag,">99")).
    Appword:Selection:TypeText("  ").
    Appword:Selection:TypeText(string(c-moeda,"x(10)")).
    Appword:Selection:TypeText("").
    Appword:Selection:TypeText(string(b-ordem.preco-unit,">>>,>>9.99")).
    Appword:Selection:TypeText("").
    Appword:Selection:TypeText(STRING(de-quant,">,>>>,>>9.99")).
    Appword:Selection:TypeText("      ").
    Appword:Selection:TypeText(string(b-ordem.aliquota-ipi,">9.99")).
end.

Appword:Selection:TypeParagraph .
Appword:Selection:TypeParagraph .
Appword:Selection:Insertfile(c-mod-cot2).

assign i-count = i-count + 1.
if  i-count = 2 then
    Appword:Selection:InsertBreak.

/*if  l-ultimo-emitente = yes then do:  /* Alteramos a c¢digo da linha abaixo...*/*/
    
    Appword:ActiveDocument:SaveAs(c-arq1 + string(emitente.cod-emite) + ".doc").
    Appword:ActiveDocument:Close.
    Appword:Quit().
    release object appWord.
    find first param-global no-lock no-error.
    
    if  emitente.e-mail <> "":U then do:
            
        /*&if  "{&bf_mat_versao_ems}" >= "2.04" &then*/
             {utp/ut-liter.i Preenchimento_da_Cotaá∆o_Via_Internet: * L}  
             assign c-preenche-via-web = return-value
                    c-url-wcc0303a = "http://" + param-global.nome-dominio-ext + param-global.des-app-url + "/web/men/wrun.w?program=web/ccp/wcc0303a.w&param=add&direto=yes&chavepai=" + string(rowid(ordem-compra)).
             assign c-mensagem-e-mail =  c-preenche-via-web + chr(13) + c-url-wcc0303a + chr(13) + c-mensagem-e-mail.
        /*&endif*/
        find usuar-mater where 
             usuar-mater.cod-usuario = ordem-compra.cod-comprado no-lock no-error.
        create tt-envio.
        assign tt-envio.versao-integracao = 1
               tt-envio.exchange    = param-global.log-1
               tt-envio.destino     =  emitente.e-mail
               tt-envio.assunto     = c-assunto-e-mail
               tt-envio.mensagem    = c-mensagem-e-mail
               tt-envio.importancia = 2
               tt-envio.log-enviada = yes
               tt-envio.log-lida    = yes
               tt-envio.acomp       = yes
               tt-envio.arq-anexo   = (c-arq1 + string(emitente.cod-emite) + ".doc")
               tt-envio.remetente   = if avail usuar-mater then usuar-mater.e-mail else "".
                 
                run utp/utapi009.p (input  table tt-envio,
                                   output table tt-erros).       
       
    end.                                                  
/*end.*/
assign r-registro = rowid(ordem-compra).
run ccp/cc0302b.p (input-output r-registro).

/*IF l-ultimo-emitente <> yes then do:
    Appword:ActiveDocument:SaveAs(c-arq1 + string(emitente.cod-emite) + ".doc").
    Appword:ActiveDocument:Close.
    Appword:Quit().
    release object appWord.
    run pi-finalizar in h-acomp. 
    return.
end.*/



/*find first tt-erros no-lock no-error.
if avail tt-erros then do:
   page.   
   disp c-titulo-erro
        c-cod
        c-desc 
        c-trc-cod
        c-trc-desc
        with frame f-erros-e-mail.
   for each tt-erros no-lock:
       disp tt-erros.cod-erro 
            tt-erros.desc-erro
            with frame f-erros-e-mail.
   end.
end.*/

run pi-finalizar in h-acomp.

/* Fim Programa */
