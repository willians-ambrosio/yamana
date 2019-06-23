/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCD0209RP 2.06.00.002}
{include/i_fnctrad.i}

/****************************************************************************
**  Objetivo: Importa‡Æo de Itens para bancos locais
*****************************************************************************/

{cdp/cdapi244.i "new shared"}   /*** Defini‡Æo temp-table tt-item ***/
{cdp/cdapi300.i1}               /*** Defini‡Æo de temp-tables e parƒmetros de integra‡Æo ***/
{include/i-rpvar.i}
{cdp/cdcfgdis.i}

/* Defini‡Æo e Prepara‡Æo dos Parƒmetros */
def temp-table tt-param
    field destino     as int
    field arq-destino as char
    field arq-entrada as char
    field todos       as int
    field usuario     as char
    field data-exec   as date
    field hora-exec   as int.

def temp-table tt-item-depto 
    field it-codigo like item.it-codigo
    field cod-depto like es-it-depto.cod-depto.

def temp-table tt-item-batch like tt-item.

def buffer b-tt-item for tt-item.
def buffer b2-tt-item for tt-item.

def temp-table tt-raw-digita
    field raw-digita as raw.

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

def var h-acomp    as handle no-undo.

def buffer b-item  for item.
def buffer b1-item for item.

def var x as int init 0.

def var c-reg         as char no-undo.
def var i-tipo-trans  as int  no-undo.
def var i-dia-implant as int  no-undo.
def var i-mes-implant as int  no-undo.
def var i-ano-implant as int  no-undo format 9999.
def var i-dia-liberac as int  no-undo.
def var i-mes-liberac as int  no-undo.
def var i-ano-liberac as int  no-undo.
def var i-cont        as int  no-undo.

def var i-erros       as int  extent 2 format ">>>>>>>9" init 0 no-undo.
def var c-msg         as char format "x(100)"  no-undo.
def var c-transacao   as char format "x(30)"   no-undo.
def var c-mensagem    as char format "x(50)"   no-undo.
def var c-item        as char                  no-undo.
def var c-narrativa   as char format "x(2000)" no-undo.
def var c-cod-depto   as int  format ">>9"     no-undo.
def var c-imp-erro    as char format "x(60)"   no-undo.

def temp-table tt-erros-valid
    field it-codigo like item.it-codigo
    field c-erro    as char format "x(90)"
    index codigo it-codigo.

def var i-cont-erros as int.

form c-transacao
     c-msg
    with width 132 down no-box frame f-importacao stream-io.

form c-imp-erro
     with width 132 down no-box frame f-erro stream-io.

form tt-erros-valid.it-codigo
     tt-erros-valid.c-erro
     with width 132 down no-box frame f-erro-1 stream-io.

{utp/ut-liter.i Observa‡Æo}
assign c-msg:label in frame f-importacao = trim(return-value).

{utp/ut-liter.i Transa‡Æo__Item}
assign c-transacao:label in frame f-importacao = trim(return-value).

{utp/ut-table.i mgind item 1}
assign c-item = return-value.

{utp/ut-liter.i ERROS_NO_ARQUIVO_DE_IMPORTA€ÇO}
assign c-imp-erro:label in frame f-erro = trim(return-value).

{utp/ut-liter.i Descri‡Æo_do_erro}
assign c-erro:label in frame f-erro-1 = trim(return-value).

def stream s-imp.
input stream s-imp from value(tt-param.arq-entrada).
/* {include/i-rpout.i &tofile = tt-param.arq-destino} */

/***  Inicio do programa  ***/

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Importa‡Æo_de_Itens *}
run pi-inicializar in h-acomp(input return-value).

find first param-global no-lock no-error.
find first param-estoq  no-lock no-error.
find first tt-param     no-lock no-error.

{utp/ut-liter.i ENGENHARIA * r}
assign c-sistema = trim(return-value).

{utp/ut-liter.i Importa‡Æo_de_Itens}
assign c-titulo-relat = trim(return-value).

assign c-programa = "ESCD0209RP"
       c-versao   = "2.04"
       c-revisao  = "00.002".

run utp/ut-trfrrp.p (input frame f-erro-1:handle).
run utp/ut-trfrrp.p (input frame f-erro:handle).
run utp/ut-trfrrp.p (input frame f-importacao:handle).
{include/i-rpcab.i}

/*** Opcao Importa‡Æo ***/
assign i-erros = 0.
if  search(tt-param.arq-entrada) = ? then
    run utp/ut-msgs.p(input "msg", input 326, input tt-param.arq-entrada).

input from value(tt-param.arq-entrada) convert target "ibm850" no-echo.
for each tt-erros-valid: delete tt-erros-valid. end.
run pi-leitura-registros.

{include/i-rpout.i &tofile = tt-param.arq-destino}

view frame f-cabec.
view frame f-rodape.

if  can-find(first tt-erros-valid) then do:
    disp c-imp-erro with frame f-erro.
    for each tt-erros-valid:
        disp tt-erros-valid.it-codigo
             tt-erros-valid.c-erro
             with frame f-erro-1 width 132 down stream-io.
        down with frame f-erro-1.
        delete tt-erros-valid.
    end.
    for each tt-item: delete tt-item. end.
    run pi-finalizar in h-acomp.
    {include/i-rpclo.i}.
    return.
end.
if  return-value = 'OK' then do:
    {utp/ut-liter.i Gravando_Registros * r}
    assign c-mensagem = trim(return-value).

    run pi-acompanhar in h-acomp(c-mensagem).
    create tt-versao-integr.
    assign tt-versao-integr.cod-versao-integracao = 1
           i-cont = 0.

    for each tt-item break by tt-item.it-codigo:

        run pi-acompanhar in h-acomp(c-mensagem).
        /* + ' ' + tt-item.it-codigo*/ /* Tirado esse trecho do codigo, pois no processo de gravacao dos registros
        aparentemente trava o registro, mas o processo ‚ encerrado com sucesso */ /* Chamado 1464.991 */

        if  i-cont <= 10000 then do:
            create tt-item-batch.
            buffer-copy tt-item to tt-item-batch.
            assign i-cont = i-cont + 1.
        end.

        if  i-cont = 10000 or last(tt-item.it-codigo) then do:
            run pi-acompanhar in h-acomp(c-mensagem).

            run cdp/cdapi344.p(input        table tt-versao-integr,
                               output       table tt-erros-geral,
                               input-output table tt-item-batch).

            for each tt-item-batch:
    
                if  tt-item-batch.cod-erro <> 0 then do:
                    find first b-tt-item exclusive-lock
                         where b-tt-item.it-codigo     = tt-item-batch.it-codigo
                           and b-tt-item.num-sequencia = tt-item-batch.num-sequencia no-error.
                    if  avail b-tt-item then
                        buffer-copy tt-item-batch to b-tt-item.
                end.
                else do:
                    create tt-erros-geral.
                    assign tt-erros-geral.identif-msg = tt-item-batch.it-codigo
                           tt-erros-geral.cod-erro    = 0.

                    find first tt-item-depto where tt-item-depto.it-codigo = tt-item-batch.it-codigo no-lock no-error.
                    if avail tt-item-depto then do:                        

                        if not can-find(first es-it-depto where es-it-depto.it-codigo = tt-item-depto.it-codigo) then do:
                            create es-it-depto.
                            assign es-it-depto.it-codigo = tt-item-depto.it-codigo
                                   es-it-depto.cod-depto = tt-item-depto.cod-depto
                                   es-it-depto.data      = today
                                   es-it-depto.hora      = string(time,"hh:mm:ss")
                                   es-it-depto.origem    = "ESCD0209"
                                   es-it-depto.usuario   = tt-param.usuario.
                        end.
                        else do:
                            assign es-it-depto.cod-depto    = tt-item-depto.cod-depto
                                   es-it-depto.usuar-altera = tt-param.usuario
                                   es-it-depto.data-altera  = today                   
                                   es-it-depto.hr-altera    = string(time,"hh:mm:ss").
                        end.
                    end.
                end.
                delete tt-item-batch.
            end.
            for each b2-tt-item:
                assign i-erros[1] = 0.

                case b2-tt-item.ind-tipo-movto:
                    when 1 then do:
                        {utp/ut-liter.i Inclui__}
                        assign c-item = return-value.
                    end.
                    when 2 then do:
                        {utp/ut-liter.i Altera__}
                        assign c-item = return-value.
                    end.
                    when 3 then do:
                        {utp/ut-liter.i Elimina_}
                        assign c-item = return-value.
                    end.
                end.

                for each tt-erros-geral
                   where tt-erros-geral.identif-msg = b2-tt-item.it-codigo:

                    if  tt-erros-geral.cod-erro <> 0 then do:
                        assign i-erros[1] = i-erros[1] + 1.
                        if  tt-erros-geral.cod-erro = 17 then do:
                            {utp/ut-table.i mgind item 1}
                            run utp/ut-msgs.p(input "msg", input 17, input return-value + "~~" + b2-tt-item.des-erro).
                            assign c-msg  = return-value + " : " + b2-tt-item.des-erro.
                        end.
                        else
                            assign c-msg = tt-erros-geral.des-erro.

                        assign c-transacao = c-item + ' ' + b2-tt-item.it-codigo.
                        disp c-transacao
                             c-msg
                            with frame f-importacao.
                        down with frame f-importacao.
                    end.
                    else do:
                        if  tt-param.todos = 1 then do:
                            if  b2-tt-item.ind-tipo-movto = 1 then
                                {utp/ut-liter.i Item_Importado_Com_Sucesso *}
                            if  b2-tt-item.ind-tipo-movto = 2 then
                                {utp/ut-liter.i Item_Alterado_Com_Sucesso *}
                            if  b2-tt-item.ind-tipo-movto = 3 then
                                {utp/ut-liter.i Item_Exclu¡do_Com_Sucesso *}

                            assign c-transacao = c-item + ' ' + b2-tt-item.it-codigo.
                                   c-msg       = return-value.

                            disp c-transacao
                                 c-msg
                                with frame f-importacao.
                            down with frame f-importacao.
                        end.
                    end.
                    delete tt-erros-geral.
                end.
            end.
            assign i-cont = 0.
        end.  /* = 10 */
    end.
end.

input stream s-imp close.
run pi-finalizar in h-acomp.

{include/i-rpclo.i}.

/*** Procedimentos Internos ***/

procedure pi-leitura-registros :
    def var i-seq as int no-undo.

    repeat on error undo, leave
           on stop  undo, return 'NOK':

        import unformatted c-reg.

        create tt-item.
        if  entry(1,c-reg,';') = "2" then do:
            find first item no-lock
                 where item.it-codigo = right-trim(entry(2,c-reg,';')) no-error.
            if  avail item then
                buffer-copy item to tt-item.
        end.

        assign tt-item.ind-tipo-movto =  int(right-trim(entry(1,c-reg,';')))
               tt-item.it-codigo      =                 entry(2,c-reg,';')
               tt-item.desc-item      =      right-trim(entry(3,c-reg,';'))
               tt-item.descricao-1    =      right-trim(substr(tt-item.desc-item,1,18))
               tt-item.descricao-2    =      right-trim(substr(tt-item.desc-item,19,18))
               tt-item.ge-codigo      =  int(right-trim(entry(4,c-reg,';')))
               tt-item.fm-codigo      =      right-trim(entry(5,c-reg,';'))
               tt-item.fm-cod-com     =      right-trim(entry(6,c-reg,';'))
               tt-item.un             =      right-trim(entry(7,c-reg,';'))
               tt-item.cod-estabel    =      right-trim(entry(8,c-reg,';'))
               tt-item.cod-obsoleto   =  int(entry(9,c-reg,';'))
               i-mes-implant          = month(today)
               i-dia-implant          =   day(today)
               i-ano-implant          =  year(today)
               tt-item.data-implant   = date(i-mes-implant,i-dia-implant,i-ano-implant)
               i-mes-liberac          = month(today)
               i-dia-liberac          =   day(today)
               i-ano-liberac          =  year(today)
               tt-item.data-liberac   = date(i-mes-liberac,i-dia-liberac,i-ano-liberac)
               tt-item.cd-folh-item   = right-trim(entry(12,c-reg,';'))
               tt-item.tipo-contr     =  int(entry(13,c-reg,';'))
               tt-item.ind-serv-mat   =  int(entry(14,c-reg,';'))/*Massimo - Kraft 26/09/2011*/ /*IF right-trim(entry(5,c-reg,';')) BEGINS '9' THEN 1 ELSE 2*/
               tt-item.lote-economi   =  dec(entry(15,c-reg,';')) / 10000
               tt-item.codigo-refer   = right-trim(entry(16,c-reg,';'))
               tt-item.inform-compl   = right-trim(entry(17,c-reg,';'))
               tt-item.cod-imagem     = right-trim(entry(18,c-reg,';'))
               c-narrativa            = entry(19,c-reg,';') + chr(10) + /* S */
                                        entry(20,c-reg,';') + chr(10) + /* T */
                                        entry(21,c-reg,';') + chr(10) + /* U */
                                        entry(22,c-reg,';') + chr(10) + /* V */
                                        entry(23,c-reg,';') + chr(10) + /* W */
                                        entry(24,c-reg,';') + chr(10) + /* X */
                                        entry(25,c-reg,';') + chr(10) + /* Y */
                                        entry(26,c-reg,';') + chr(10) + /* Z */
                                        entry(27,c-reg,';') + chr(10) + /* AA */
                                        entry(28,c-reg,';') + chr(10) + /* AB */
                                        entry(29,c-reg,';') + chr(10) + /* AC */
                                        entry(30,c-reg,';') + chr(10) + /* AD */
                                        entry(31,c-reg,';') + chr(10) + /* AE */
                                        entry(32,c-reg,';') + chr(10) + /* AF */
                                        entry(33,c-reg,';') + chr(10) + /* AG */
                                        entry(34,c-reg,';') + chr(10) + /* AH */
                                        entry(35,c-reg,';') + chr(10) + /* AI */
                                        entry(36,c-reg,';') + chr(10) + /* AJ */
                                        entry(37,c-reg,';') + chr(10) + /* AK */
                                        entry(38,c-reg,';') + chr(10) + /* AL */
                                        entry(39,c-reg,';') + chr(10) + /* AM */
                                        entry(40,c-reg,';') + chr(10) + /* AN */
                                        entry(41,c-reg,';')             /* AO */  
               tt-item.narrativa      = if  entry(1,c-reg,';') = "1" then
                                            tt-item.narrativa +
                                           (if c-narrativa <> "" then c-narrativa else "")
                                        else
                                           (if c-narrativa <> "" then c-narrativa else "")
               tt-item.quant-segur    = if  avail item then item.quant-segur else 0
               c-cod-depto            = int(entry(42,c-reg,';'))         /* AP */
               tt-item.num-sequencia  = i-seq               
               i-seq                  = i-seq + 1 
               no-error.        
            
        create tt-item-depto.
        assign tt-item-depto.it-codigo = right-trim(entry(2,c-reg,';')).
               tt-item-depto.cod-depto = c-cod-depto.        

        if  error-status:error then do:
            do i-cont-erros = 1 to error-status:num-messages:
                create tt-erros-valid.
                assign tt-erros-valid.it-codigo = right-trim(entry(2,c-reg,';'))
                       tt-erros-valid.c-erro = error-status:get-message(i-cont-erros).
            end.
        end.
        else do:
        &if defined(bf_dis_aloc_neg_pre) &then
            find first item-dist no-lock
                 where item-dist.it-codigo = tt-item.it-codigo no-error.
            if  not avail item-dist then do:
                create item-dist.
                assign item-dist.it-codigo = tt-item.it-codigo.

                find first fam-comerc no-lock
                     where fam-comerc.fm-cod-com = tt-item.fm-cod-com no-error.
                if  avail fam-comerc then
                    assign item-dist.log-aloc-neg = fam-comerc.log-aloc-neg.
            end.

        &endif
        end.

        run pi-acompanhar in h-acomp(c-item + ": " + entry(2,c-reg,';')).
    end.
    return 'OK':U.
END PROCEDURE.
