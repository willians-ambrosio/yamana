/***************************************************************************************
** Programa : escc0302rp.p
** Objetivo : Emissao de Pedido de Compra
** Autor    : Helder Prado Bustamante
** Empresa  : Datasul SÆo Paulo - GWA
** Data     : 02 de Fevereiro de 2006
***************************************************************************************/
{include/i-rpvar.i}
{include/tt-edit.i}
{include/pi-edit.i}

{utp/ut-glob.i}

{utp/utapi009.i}

def new shared var c-dest-e-mail as char format "x(400)" no-undo.
def var h-acomp                  as handle               no-undo.
def var c-det-cabec              as char format "x(23)"  no-undo.
def var c-prg-obj                as char                 no-undo.
def var c-prg-vrs                as char                 no-undo.
def var c-linha                  as char format "x(120)" no-undo.
def var c-remetente              as char format "x(80)"  no-undo.

DEF VAR c_nom_dir_spool LIKE usuar_mestre.nom_dir_spool NO-UNDO. /* Bisneto 12/01/2016 */

DEF VAR c-destino   AS CHAR NO-UNDO.
DEF VAR n-separador AS INT  NO-UNDO.
DEF VAR n-enviado   AS INT  NO-UNDO.
DEF VAR c-menserr AS   CHAR NO-UNDO.


def temp-table tt-param no-undo
    field destino                    as int
    field arquivo                    as char format "x(35)"
    field usuario                    as char format "x(12)"
    field data-exec                  as date
    field hora-exec                  as int
    field i-ini-numero-ordem         like ordem-compra.numero-ordem
    field i-fim-numero-ordem         like ordem-compra.numero-ordem
    field dt-ini-emissao             as date format "99/99/9999"
    field dt-fim-emissao             as date format "99/99/9999"
    field c-ini-requisitante         like ordem-compra.requisitante
    field c-fim-requisitante         like ordem-compra.requisitante
    field c-ini-cod-comprado         like ordem-compra.cod-comprado
    field c-fim-cod-comprado         like ordem-compra.cod-comprado
    field c-ini-it-codigo            like ordem-compra.it-codigo
    field c-fim-it-codigo            like ordem-compra.it-codigo
    field l-narrativa-ord-compr      as log
    field l-narrativa-item-ord-compr as log
    field l-descricao                as log
    field l-envia-email              as log
    FIELD narrativa                  LIKE ITEM.narrativa
    FIELD dt-limite                  as date.

DEF TEMP-TABLE tt-email NO-UNDO
    FIELD numero-ordem LIKE ordem-compra.numero-ordem
    FIELD destino      AS CHAR
    FIELD erro         AS CHAR
    INDEX xemail numero-ordem.

FOR EACH tt-email.
    DELETE tt-email.
END.

def temp-table tt-digita no-undo
    field flag         as char format "x"
    field numero-ordem like ordem-compra.numero-ordem
    field parcela      like prazo-compra.parcela
    field it-codigo    like ordem-compra.it-codigo
    field un           like item.un
    field quantidade   like prazo-compra.quantidade
    field data-entrega like prazo-compra.data-entrega
    index id numero-ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

for each tt-raw-digita no-lock:
    create tt-digita.
    raw-transfer raw-digita TO tt-digita.
end.

create tt-param.
raw-transfer raw-param to tt-param.

{include/i-rpcab.i}

{include/i-rpout.i}
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input string(c-titulo-relat)).

assign c-programa = "ESCC0302RP"
       c-versao   = "2.06"
       c-revisao  = "00.000"
       c-sistema  = "COMPRAS".

def var chExcelApplication  as com-handle no-undo.
def var chWorkbook          as com-handle no-undo.
def var chWorkSheet         as com-handle no-undo.
def var chActiveWorkbook    as com-handle no-undo.
def var c-planilha          as char       no-undo.
def var c-planilha-envio    as char       no-undo.
def var c-range             as char       no-undo.
def var i-celula            as int        no-undo.
def var i-linha             as int        no-undo.
def var i-conta-linha       as int        no-undo.
def var i-sequencia         as int        no-undo.

ASSIGN
    FILE-INFO:FILE-NAME = "esp\escc0302.xlt"
    c-planilha          = FILE-INFO:FULL-PATHNAME.

put "Planilha(s) Gerada(s):".

for each  tt-digita no-lock
    where tt-digita.flag <> "",
    first ordem-compra exclusive-lock
    where ordem-compra.numero-ordem = tt-digita.numero-ordem
   break
    by ordem-compra.nr-processo
    by tt-digita.numero-ordem:

    run pi-acompanhar in h-acomp(input "Ordem : " + string(tt-digita.numero-ordem)).

    IF FIRST-OF(ordem-compra.nr-processo) THEN
        RUN pi-inicia-planilha.

    for each  prazo-compra no-lock
        where prazo-compra.numero-ordem = ordem-compra.numero-ordem
        and   prazo-compra.parcela      = tt-digita.parcela:

        run pi-acompanhar in h-acomp(input "Ordem\Parcela : " + string(tt-digita.numero-ordem) +
                                           " \ " + string(prazo-compra.parcela)).

        find first item no-lock
             where item.it-codigo = ordem-compra.it-codigo no-error.

        assign i-sequencia = i-sequencia + 1
               c-range     = "B" + string(i-linha)
               chExcelApplication:Range(c-range):value = string(i-sequencia,"999")
               c-range     = "C" + string(i-linha)
               chExcelApplication:Range(c-range):value = string(ordem-compra.numero-ordem) + "/" +
                                                         string(prazo-compra.parcela)
               c-range     = "D" + string(i-linha)
               chExcelApplication:Range(c-range):value = ordem-compra.it-codigo
               c-range     = "F" + string(i-linha)
               chExcelApplication:Range(c-range):Numberformat = "#.##0,00"
               chExcelApplication:Range(c-range):value = string(prazo-compra.quantidade)
               c-range     = "G" + string(i-linha)
               chExcelApplication:Range(c-range):value = string(item.un).

        if  tt-param.l-descricao or
            tt-param.l-narrativa-item-ord-compr or
            tt-param.l-narrativa-ord-compr then do:
            if  tt-param.l-descricao then
                assign c-range = "E" + string(i-linha)
                       chWorkSheet:Range(c-range):font:size    = 8
                       chExcelApplication:Range(c-range):value = item.desc-item.

            if  tt-param.l-narrativa-item-ord-compr and
                avail item then do:
                if  tt-param.l-descricao then do:
                    assign i-linha = i-linha + 1
                           c-range = "A" + string(i-linha) + ":IA" + string(i-linha).
                    chExcelApplication:Range(c-range):select.
                    chExcelApplication:Range(c-range):insert.

                    assign i-linha = i-linha + 1
                           c-range = "A" + string(i-linha) + ":IA" + string(i-linha).
                    chExcelApplication:Range(c-range):select.
                    chExcelApplication:Range(c-range):insert.
                end.

                run pi-print-editor (item.narrativa, 55 /*60*/).
                for each tt-editor no-lock:
                    if  tt-editor.conteudo <> "" then do:
                        assign c-range = "E" + string(i-linha)
                               chWorkSheet:Range(c-range):font:size    = 6
                               chExcelApplication:Range(c-range):value = tt-editor.conteudo.

                        assign i-linha = i-linha + 1
                               c-range = "A" + string(i-linha) + ":IA" + string(i-linha).

                        chExcelApplication:Range(c-range):select.
                        chExcelApplication:Range(c-range):insert.
                    end.
                end.
            end.

            if  tt-param.l-narrativa-ord-compr then do:
                if  tt-param.l-descricao or
                    tt-param.l-narrativa-item-ord-compr then do:
                    assign i-linha = i-linha + 1
                           c-range = "A" + string(i-linha) + ":IA" + string(i-linha).
                    chExcelApplication:Range(c-range):select.
                    chExcelApplication:Range(c-range):insert.
                end.

                run pi-print-editor (ordem-compra.narrativa, 55 /*60*/).
                for each tt-editor no-lock:
                    if  tt-editor.conteudo <> "" then do:
                        assign c-range = "E" + string(i-linha)
                               chWorkSheet:Range(c-range):font:size    = 6
                               chExcelApplication:Range(c-range):value = tt-editor.conteudo.

                        assign i-linha = i-linha + 1
                               c-range = "A" + string(i-linha) + ":IA" + string(i-linha).
                        chExcelApplication:Range(c-range):select.
                        chExcelApplication:Range(c-range):insert.
                    end.
                end.
            end.
        end.

        if  not tt-param.l-narrativa-item-ord-compr and
            not tt-param.l-narrativa-ord-compr then do:
            assign i-linha = i-linha + 1
                   c-range = "A" + string(i-linha) + ":IA" + string(i-linha).

            chExcelApplication:Range(c-range):select.
            chExcelApplication:Range(c-range):insert.
        end.
    end.

    ASSIGN /**/
        ordem-compra.impr-ficha = FALSE.


    IF LAST-OF(ordem-compra.nr-processo) THEN
    DO:
        assign c-range = "A" + string(i-linha) + ":IA" + string(i-linha).

        chExcelApplication:Range(c-range):select.
        chExcelApplication:Range(c-range):delete.

        assign c-range = "B11".
        chExcelApplication:Range(c-range):select.

        /*chExcelApplication:workbooks:item(1):SaveAs(c-nome,,,,,,).*/

        chExcelApplication:workbooks:item(1):SaveAs(c-planilha-envio,,,,,,).
        chExcelApplication:range("A1"):select.

        chExcelApplication:ActiveSheet:Enableselection ='1'.
        chExcelApplication:DisplayAlerts = false.

        PUT c-planilha-envio format "x(60)" AT 5.

        if  tt-param.destino = 3 
        AND NOT tt-param.l-envia-email then
            assign chExcelApplication:visible = yes. /* Mostra a planilha Excel na tela */
        else
            chExcelApplication:quit.

        release object chWorkSheet.
        release object chWorkbook.
        release object chExcelApplication.

        if  tt-param.l-envia-email then
        do:

            FOR EACH tt-envio:
                DELETE tt-envio.
            END.

            FOR EACH tt-erros:
                DELETE tt-erros.
            END.

            {include/i-rpclo.i}
            run pi-finalizar in h-acomp.

            assign c-dest-e-mail = "".

            FOR EACH  item-fornec NO-LOCK
                WHERE item-fornec.it-codigo = ordem-compra.it-codigo:

                IF CAN-FIND(first cont-emit no-lock
                            where cont-emit.cod-emitente = item-fornec.cod-emitente
                            and   cont-emit.e-mail      <> "") = FALSE THEN
                DO:
                    find first emitente no-lock
                         where emitente.cod-emitente = item-fornec.cod-emitente no-error.

                    IF LOOKUP(emitente.e-mail,c-dest-e-mail,";") = 0 
                    AND emitente.e-mail <> ""    THEN
                        ASSIGN
                            c-dest-e-mail = c-dest-e-mail + emitente.e-mail + ";".
                END.
                ELSE
                DO:
                    FOR EACH  cont-emit NO-LOCK
                        WHERE cont-emit.cod-emitente = item-fornec.cod-emitente:

                        IF LOOKUP(cont-emit.e-mail,c-dest-e-mail,";") = 0 
                        AND cont-emit.e-mail <> ""    THEN
                            ASSIGN
                                c-dest-e-mail = c-dest-e-mail + cont-emit.e-mail + ";".
                    END.
                END.
            END.

            find first usuar-mater no-lock
                 where usuar-mater.cod-usuario = c-seg-usuario no-error.
            if  avail usuar-mater then
                assign c-remetente = usuar-mater.e-mail.

            run esp/escc0302a.w.

            ASSIGN c-destino = c-dest-e-mail.

            IF SUBSTR(trim(c-destino),LENGTH(TRIM(c-destino)),1) <> ";" THEN
                ASSIGN c-destino = TRIM(c-destino + ";").
            REPEAT:
                IF trim(c-destino) <> "" THEN DO:

                    FOR EACH tt-envio:
                        DELETE tt-envio.
                    END.
                    FOR EACH tt-erros:
                        DELETE tt-erros.
                    END.

                    ASSIGN n-separador = INDEX(c-destino,";").
    
                    RUN pi-cria-tt-envio.
                    
                    run utp/utapi009.p(input  table tt-envio,
                                       output table tt-erros).
        
                    if  return-value = "NOK" THEN do:
                        for each tt-erros:
                            CREATE tt-email.
                            ASSIGN tt-email.numero-ordem = ordem-compra.numero-ordem
                                   tt-email.erro         = tt-erros.desc-erro
                                   tt-email.destino      = SUBSTR(c-destino,1,n-separador).
                        END.
                    END.
                    ASSIGN c-destino = SUBSTR(c-destino,n-separador + 1,LENGTH(TRIM(c-destino))).
                END.
                ELSE LEAVE.
            END.

            FIND FIRST tt-email NO-LOCK NO-ERROR.

            IF NOT AVAIL tt-email THEN
                run utp/ut-msgs.p(input "show",
                                  input 17006,
                                  input "E-mail enviado." + "~~"  +
                                        "Enviado com Sucesso E-mail com o precesso " + STRING(ordem-compra.nr-processo) + ".").
            
            run utp/ut-acomp.p persistent set h-acomp.
            run pi-inicializar in h-acomp(input string(c-titulo-relat)).

            if  tt-param.destino = 3
            AND tt-param.l-envia-email then
            DO:
                create "Excel.Application" chExcelApplication.
                assign
                    chExcelApplication:visible  = yes /* Mostra a planilha Excel na tela */
                    chWorkbook                  = chExcelApplication:Workbooks:OPEN(c-planilha-envio). /* Cria uma nova planilha excel */
                release object chExcelApplication.
            END.
            
        END.
    END.
end.


FIND FIRST tt-email NO-LOCK NO-ERROR.

IF AVAIL tt-email THEN DO:
    ASSIGN c-menserr = "". 
    FOR EACH tt-email NO-LOCK
        BREAK BY tt-email.numero-ordem:
        ASSIGN c-menserr = c-menserr + tt-email.destino.
        IF LAST-OF(tt-email.numero-ordem) THEN DO:
            run utp/ut-msgs.p(input "show",
                              input 17006,
                              input "Ordem de Compra: " +  STRING(tt-email.numero-ordem) + "~~"  +
                                    "E-mail nÆo enviado - " + c-menserr).
            ASSIGN c-menserr = "".
        END.
    END.
END.




{include/i-rpclo.i}
run pi-finalizar in h-acomp.


return 'ok'.

procedure pi-planilha-envio:
    find first usuar_mestre no-lock
         where usuar_mestre.cod_usuario = c-seg-usuario no-error.

    IF tt-param.destino = 2 then
    do:
        assign c-planilha-envio = tt-param.arquivo                        +
                                  trim(string(ordem-compra.numero-ordem)) +
                                  '_Proc_'                                +
                                  trim(string(ordem-compra.nr-processo )) +
                                  '.xls'.
    END.
    else
    DO:
        if  avail usuar_mestre then do:
           ASSIGN 
             c_nom_dir_spool = usuar_mestre.nom_dir_spool
             c_nom_dir_spool = REPLACE(c_nom_dir_spool,'/','\'). 

            assign c-planilha-envio = c_nom_dir_spool + '\' +
                                      (if  usuar_mestre.nom_subdir_spool <> '' then
                                          usuar_mestre.nom_subdir_spool + '\'
                                      else '') + 'Ordem_' +
                                      trim(string(ordem-compra.numero-ordem)) +
                                      '_Proc_'                                +
                                      trim(string(ordem-compra.nr-processo )) +
                                      '.xls'.
            ASSIGN c-planilha-envio = REPLACE(c-planilha-envio,"\\","\").
        end.
        /* bisneto 11/01/2016
        if  avail usuar_mestre then do:
            assign c-planilha-envio = usuar_mestre.nom_dir_spool + '\' +
                                      (if  usuar_mestre.nom_subdir_spool <> '' then
                                          usuar_mestre.nom_subdir_spool + '\'
                                      else '') + 'Ordem_' +
                                      trim(string(ordem-compra.numero-ordem)) +
                                      '_Proc_'                                +
                                      trim(string(ordem-compra.nr-processo )) +
                                      '.xls'.
        end.
        */
        else
            assign c-planilha-envio = session:TEMP-DIRECTORY + "Ordem_" +
                                      trim(string(ordem-compra.numero-ordem)) +
                                      '_Proc_'                                +
                                      trim(string(ordem-compra.nr-processo )) +
                                      '.xls'.
    END.

    if  search(c-planilha-envio) <> "" and
        search(c-planilha-envio) <> ?  then
        dos silent del value(replace(c-planilha-envio,'/','\')).
end procedure.


PROCEDURE pi-inicia-planilha:
    run pi-planilha-envio.

    create "Excel.Application" chExcelApplication.
    assign chExcelApplication:visible = no /* Nao mostra a planilha Excel na tela enquanto esta sendo criada */
           chWorkbook = chExcelApplication:Workbooks:add(c-planilha) /* Cria uma nova planilha excel */
           chWorkSheet = chExcelApplication:Sheets:Item(1).
    if  avail ordem-compra then do:
        find first usuar-mater no-lock
             where usuar-mater.cod-usuario = ordem-compra.cod-comprado no-error.
        if  avail usuar-mater then
        do:
            FIND FIRST proc-compra NO-LOCK
                 WHERE proc-compra.nr-processo = ordem-compra.nr-processo NO-ERROR.
            assign c-range     = "D27" /* bisneto "D28" */
                   chExcelApplication:Range(c-range):value = usuar-mater.nome-usuar
                   c-range     = "G27" /* bisneto "G28" */
                   chExcelApplication:Range(c-range):value = usuar-mater.telefone[1]
                   c-range     = "K27" /* bisneto "K28" */
                   chExcelApplication:Range(c-range):value = usuar-mater.e-mail
                   c-range     = "M08"
                   chExcelApplication:Range(c-range):value = "'" + STRING(tt-param.dt-limite)
                   c-range     = "B08"
                   chExcelApplication:Range(c-range):value = "PROCESSO: " + STRING(ordem-compra.nr-processo) + IF AVAIL proc-compra THEN " - " + proc-compra.descricao ELSE "".
        END.
    end.

    if  tt-param.narrativa <> "" then do:
        run pi-print-editor (tt-param.narrativa, 200 /*800*/).

        assign i-linha = 24.
        for each tt-editor no-lock:
            /*if  tt-editor.conteudo <> "" then do:*/
               assign c-range = "B" + string(i-linha)
                      chWorkSheet:Range(c-range):font:size     = 7
                      chExcelApplication:Range(c-range):value  = tt-editor.conteudo.

               assign i-linha = i-linha + 1
                      c-range = "A" + string(i-linha) + ":IA" + string(i-linha).
               chExcelApplication:Range(c-range):select.
               chExcelApplication:Range(c-range):insert.

               assign c-range = "B" + string(i-linha) + ":M" + string(i-linha).
               chExcelApplication:Range(c-range):merge.
               /* MergeCells = True */
            /*end.*/
        end.
    end.

    assign i-linha     = 11
           i-sequencia = 0.
END PROCEDURE.


PROCEDURE pi-cria-tt-envio:

    FIND FIRST estabelec NO-LOCK
         WHERE estabelec.cod-estabel = ordem-compra.cod-estabel NO-ERROR.

    IF NOT AVAIL usuar-mater THEN
        find first usuar-mater no-lock
             where usuar-mater.cod-usuario = ordem-compra.cod-comprado no-error.

    create tt-envio.
    assign tt-envio.versao-integracao = 1
           /* tt-envio.servidor          = "172.24.52.29"
           tt-envio.porta             = 25 */
           tt-envio.destino           = SUBSTR(c-destino,1,n-separador)
           tt-envio.remetente         = c-remetente
           tt-envio.copia             = c-remetente
           tt-envio.assunto           = "Cota‡Æo de Pre‡os"
           tt-envio.arq-anexo         = c-planilha-envio
           tt-envio.exchange          = NO
           tt-envio.mensagem          =
        "COTA€ÇO DE PRE€OS"                         + CHR(10) +
        CHR(10) + CHR(10) +
        "Prezado(s) Senhor(es),"                    + CHR(10) +
        CHR(10) +
        "A µrea de Compras e Contratos convida V.Sas. a apresentarem propostas para fornecimento dos materiais/equipamentos especificados no(s) arquivos(s) em anexo." + CHR(10) +
        CHR(10) +
        "Condi‡äes do Fornecimento:"                 + CHR(10) +
        "Conforme especifica‡Æo t‚cnica em anexo. Favor preencher planilha anexa com pre‡os unit rios e todos os impostos inclusos." + CHR(10) +
        CHR(10) +
        "Apresentar pre‡os fixos e irreajust veis pelo per¡odo de vigˆncia do fornecimento para:" + CHR(10) +
        CHR(10) +
        estabelec.nome                                                      + CHR(10) +
        estabelec.endereco                                                  + CHR(10) +
        "Cep "                 + STRING(STRING(estabelec.cep),"XXXXX-XXX")          + CHR(10) +
        "CNPJ: "               + STRING(STRING(estabelec.cgc),"xx.xxx.xxx/xxxx-xx") + CHR(10) +
        "Inscri‡Æo Estadual: " + estabelec.ins-estadual                     + CHR(10) +
        CHR(10) +
        "As propostas deverÆo ser apresentadas de acordo com a presente requisi‡Æo de propostas, impreterivelmente at‚ o dia " +
        STRING(tt-param.dt-limite,"99/99/9999") +
        " para o e-mail ou fax abaixo indicados." + CHR(10) +
        CHR(10) +
        "At‚ a data de entrega das propostas, as d£vidas poderÆo ser encaminhadas por escrito … µrea de Compras e Contratos." + CHR(10) +
        CHR(10) +
        "Favor acusar o recebimento deste pedido."  + CHR(10) +
        CHR(10) +
        "Atenciosamente,"                           + CHR(10) +
        CHR(10) +
        usuar-mater.nome                            + CHR(10) +
        "E-mail: "  + usuar-mater.e-mail            + CHR(10) +
        "Tel.: "    + usuar-mater.telefone[1]       + IF usuar-mater.ramal[1] <> "" THEN "Ramal: "  + usuar-mater.ramal[1] ELSE "" + CHR(10) +
        "Fax: "     + usuar-mater.telefax           + CHR(10).
END PROCEDURE.
