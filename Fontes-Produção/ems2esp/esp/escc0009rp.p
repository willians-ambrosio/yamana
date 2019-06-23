/*************************************************************************
** Programa: esp/cqng3005rp.p
** Data    : 07-08-2014
** Autor   : Mauricio Cerqueira Miranda
** Objetivo: Extra‡Æo de Itens com movimento.
**************************************************************************/
def input param i-dias-pesquisa AS INT.
def input param l-executar      AS LOG.

DEFINE VARIABLE I-CONTA-ITEM AS INTEGER     NO-UNDO.

def temp-table tt-item-saida no-undo
    field it-codigo           as char
    field cod-empresa         as INT
    field cod-estabel         as char
    field un                  as char
    field cod-familia         as char
    field cod-fam-comerc      as char
    field desc-item           as char
    field desc-narrativa      as char
    field cod-depart          as CHAR FORMAT "x(30)"
    field cod-grp-estoque     as int
    field cod-complementear   as char
    field dt-extracao         as date
    field tp-transacao        as char
    field saldo-item          AS DEC
    field dt-saldo-estoq      AS DATE
    field tp-transacao2       as int
    field it-codigo-Klassmatt as char
    field NCM                 as char
    field codigoOrig          as INT
    field tipo                as CHAR
    FIELD c-log               AS CHAR FORMAT "x(20)".

def var da-ini-per     as date   no-undo.
def var da-fim-per     as date   no-undo.
def var i-per-corrente as int    no-undo.
def var i-ano-corrente as int    no-undo.
def var da-aux         as date   no-undo.
def var h-acomp        AS HANDLE NO-UNDO.
def var i-total-saldo  AS DEC    NO-UNDO.
DEFINE VARIABLE da-dt-ref   AS DATE        NO-UNDO.
DEFINE VARIABLE C-DEPTO     AS CHARACTER FORMAT "X(35)"   NO-UNDO.
DEFINE VARIABLE d-ini      AS DATE        NO-UNDO.
DEFINE VARIABLE d-fim      AS DATE        NO-UNDO.
DEFINE VARIABLE d-ref      AS DATE        NO-UNDO.
DEFINE VARIABLE l-tem-movimento        AS LOGICAL INIT FALSE    NO-UNDO.

/* ---------------------CONFIGURA EXCEL-------------------------- */
def var chExcelApp  AS COM-HANDLE no-undo.
def var chWorkbook  AS COM-HANDLE no-undo.
def var chWorksheet AS COM-HANDLE no-undo.
def var cRange      as char       no-undo.
def var i-linha     AS INT INIT 4 no-undo.
/* -------------------------------------------------------------- */

FIND FIRST param-global NO-LOCK NO-ERROR.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp(INPUT "Extra‡Æo Item").
/* run pi-dt-movto. */
RUN pi-processa.
/* RUN pi-acompanhar in h-acomp(INPUT "pi-saldo-estoq"). */
/* run pi-saldo-estoq. */
RUN pi-finalizar IN h-acomp.

I-CONTA-ITEM = 0.

FIND FIRST tt-item-saida NO-LOCK NO-ERROR.
IF NOT AVAIL tt-item-saida THEN DO:
    MESSAGE 'Registros nÆo encontrados'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.
ELSE DO:

    FOR EACH TT-ITEM-SAIDA:
        ASSIGN I-CONTA-ITEM = I-CONTA-ITEM + 1.
    END.
END.

/* if  l-executar then           */
/*     run pi-cria-tabela-banco. */

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp(INPUT "Gerando Excel").
RUN pi-cria-cabecalho.
RUN pi-cria-excel.
RUN pi-finaliza-handle.
RUN pi-finalizar IN h-acomp.


procedure pi-dt-movto :
    for each movto-estoq no-lock use-index data-item
       where movto-estoq.dt-trans  >= today - i-dias-pesquisa
         and movto-estoq.it-codigo >= ""
         and movto-estoq.it-codigo <= "ZZZZZZZZZZZZZZZZZ":
         find first item no-lock
              where item.it-codigo = movto-estoq.it-codigo no-error.
         if  not avail item or
             item.tipo-contr = 4 then next.

/*          FIND FIRST es_item_empresa NO-LOCK                                               */
/*               WHERE es_item_empresa.codItem    = item.it-codigo                           */
/*                 AND es_item_empresa.codEmpresa = int(param-global.empresa-prin) NO-ERROR. */
/*          IF AVAIL es_item_empresa THEN NEXT.                                              */

         RUN pi-acompanhar in h-acomp(INPUT "Item: " + item.it-codigo).

         FIND FIRST tt-item-saida
              WHERE tt-item-saida.it-codigo   = ITEM.it-codigo
                AND tt-item-saida.cod-empresa = int(param-global.empresa-prin) NO-ERROR.
         IF NOT AVAIL tt-item-saida THEN DO:
            FIND LAST es-it-depto where es-it-depto.it-codigo = item.it-codigo 
                                  and   es-it-depto.cod-depto <> 0 no-lock no-error.
          if avail es-it-depto then do:
                 FIND ES-DEPTO WHERE  es-depto.codigo = es-it-depto.cod-depto NO-LOCK NO-ERROR.
                 IF AVAIL ES-DEPTO THEN DO:
                     ASSIGN C-DEPTO = STRING(es-it-depto.cod-depto,"999") + "-" + ES-DEPTO.DESCRICAO.
                 END.
                     
          END.

          


             run pi-cria-tt-item-saida(INPUT item.it-codigo,
                                       INPUT param-global.empresa-prin,
                                       INPUT item.cod-estabel,
                                       INPUT item.un,
                                       INPUT item.fm-codigo,
                                       INPUT item.fm-cod-com,
                                       INPUT item.desc-item,
                                       INPUT item.narrativa,
                                       INPUT C-DEPTO,
                                       INPUT item.ge-codigo,
                                       INPUT item.cd-referencia,
                                       INPUT TODAY,
                                       INPUT movto-estoq.tipo-trans,
                                       INPUT 0,
                                       INPUT ?,
                                       input item.class-fiscal,
                                       input item.codigo-orig,
                                       input substr(item.char-2,212,1)).
         END.
    END.
end procedure.

procedure pi-saldo-estoq :
    def var l-entrou as log no-undo.

    for each item no-lock
       where item.it-codigo  >= ""
         and item.it-codigo  <= "ZZZZZZZZZZZZZZZZZ"
         and item.tipo-contr <> 4:

/*         FIND FIRST es_item_empresa NO-LOCK                                               */
/*              WHERE es_item_empresa.codItem    = item.it-codigo                           */
/*                AND es_item_empresa.codEmpresa = int(param-global.empresa-prin) NO-ERROR. */
/*         IF AVAIL es_item_empresa THEN NEXT.                                              */

        RUN pi-acompanhar in h-acomp(INPUT "Item (Saldo): " + item.it-codigo).

        assign i-total-saldo = 0
               l-entrou      = no.

        for each saldo-estoq of item no-lock
           where saldo-estoq.qtidade-atu <> 0:
            assign i-total-saldo = i-total-saldo + (saldo-estoq.qtidade-atu  -
                                                    saldo-estoq.qt-alocada   -
                                                    saldo-estoq.qt-aloc-prod -
                                                    saldo-estoq.qt-aloc-ped).
        end. /* each saldo-estoq */

        FIND FIRST param-estoq NO-LOCK NO-ERROR. /*param-estoq.ult-per-fech*/

        FIND FIRST estab-mat NO-LOCK
             WHERE estab-mat.cod-estabel = item.cod-estabel NO-ERROR.

        run cdp/cdapi005.p(input  estab-mat.ult-per-fech,
                           output da-ini-per,
                           output da-fim-per,
                           output i-per-corrente,
                           output i-ano-corrente,
                           output da-aux,
                           output da-aux).

        for each movto-estoq no-lock
           where movto-estoq.it-codigo   = item.it-codigo
             and movto-estoq.cod-estabel = item.cod-estabel
             and movto-estoq.dt-trans   >= da-aux:
            assign l-entrou = yes.
            if  movto-estoq.tipo-trans = 2 /* saida */ then
                assign i-total-saldo = i-total-saldo + movto-estoq.quantidade.
            else
                assign i-total-saldo = i-total-saldo - movto-estoq.quantidade.
       end. /*each  movto-estoq*/

        if  i-total-saldo > 0 or
            l-entrou then do:
            FIND FIRST tt-item-saida
                 WHERE tt-item-saida.it-codigo   = ITEM.it-codigo
                   AND tt-item-saida.cod-empresa = int(param-global.empresa-prin) NO-ERROR.
            IF NOT AVAIL tt-item-saida THEN DO:
                     FIND LAST es-it-depto where es-it-depto.it-codigo = item.it-codigo                            
                                           and   es-it-depto.cod-depto <> 0 no-lock no-error.                      
                   if avail es-it-depto then do:                                                                   
                          FIND ES-DEPTO WHERE  es-depto.codigo = es-it-depto.cod-depto NO-LOCK NO-ERROR.           
                          IF AVAIL ES-DEPTO THEN DO:                                                               
                              ASSIGN C-DEPTO = STRING(es-it-depto.cod-depto,"999") + "-" + ES-DEPTO.DESCRICAO.     
                          END.                                                                                     
                                                                                                                   
                   END.                                                                                            




                run pi-cria-tt-item-saida(INPUT item.it-codigo,
                                          INPUT param-global.empresa-prin,
                                          INPUT item.cod-estabel,
                                          INPUT item.un,
                                          INPUT item.fm-codigo,
                                          INPUT item.fm-cod-com,
                                          INPUT item.desc-item,
                                          INPUT item.narrativa,
                                          INPUT "",
                                          INPUT item.ge-codigo,
                                          INPUT item.cd-referencia,
                                          INPUT TODAY,
                                          INPUT 0,
                                          INPUT i-total-saldo,
                                          INPUT da-ini-per,
                                          input item.class-fiscal,
                                          input item.codigo-orig,
                                          input substr(item.char-2,212,1)).
            END.
            ELSE
                assign tt-item-saida.saldo-item     = i-total-saldo
                       tt-item-saida.dt-saldo-estoq = da-ini-per.
        end.
     end. /* each item */
end procedure.

procedure pi-cria-tt-item-saida :
    def input param it-codigo       LIKE item.it-codigo.
    def input param cod-empresa     LIKE param-global.empresa-prin.
    def input param cod-estabel     LIKE item.cod-estabel.
    def input param un              LIKE item.un.
    def input param fm-codigo       LIKE item.fm-codigo.
    def input param fm-cod-com      LIKE item.fm-cod-com.
    def input param desc-item       LIKE item.desc-item.
    def input param narrativa       LIKE item.narrativa.
    def input param cod-depart      AS CHAR FORMAT "x(35)".
    def input param ge-codigo       LIKE item.ge-codigo.
    def input param cd-referencia   LIKE item.cd-referencia.
    def input param dt-extracao     AS DATE.
    def input param tp-transacao    AS INT.
    def input param saldo-item      AS DEC.
    def input param dt-saldo-estoq  AS DATE.
    def input param class-fiscal    LIKE item.class-fiscal.
    def input param codigo-orig     LIKE item.codigo-orig.
    def input param tipo            AS CHAR.
    DEFINE INPUT PARAMETER p-log    AS CHARACTER FORMAT "X(20)"   NO-UNDO.

    create tt-item-saida.
    assign tt-item-saida.it-codigo         = it-codigo     .
           tt-item-saida.cod-empresa       = int(cod-empresa)   .
           tt-item-saida.cod-estabel       = cod-estabel   .
           tt-item-saida.un                = un            .
           tt-item-saida.cod-familia       = fm-codigo     .
           tt-item-saida.cod-fam-comerc    = fm-cod-com    .
           tt-item-saida.desc-item         = substr(desc-item,1,60)         .
           tt-item-saida.desc-narrativa    = substr(narrativa,1,2000)       .
           tt-item-saida.cod-depart        = cod-depart                     .
           tt-item-saida.cod-grp-estoque   = ge-codigo                      .
           tt-item-saida.cod-complementear = cd-referencia                  .
           tt-item-saida.dt-extracao       = dt-extracao                    .
           tt-item-saida.tp-transacao      = IF tp-transacao = 1 THEN "ENTRADA" ELSE IF tp-transacao = 2 THEN "SAIDA" ELSE "-" .
           tt-item-saida.saldo-item        = saldo-item             .
           tt-item-saida.dt-saldo-estoq    = dt-saldo-estoq         .
           tt-item-saida.NCM               = class-fiscal           .
           tt-item-saida.codigoOrig        = codigo-orig            .
           tt-item-saida.tipo              = tipo.                  .
           tt-item-saida.c-log             = p-log .
end procedure.



/* ************************ Rotina Excel ************************ */

PROCEDURE pi-cria-excel :
    FOR EACH tt-item-saida:
       RUN pi-acompanhar in h-acomp(INPUT "Item TT-SAIDA:" + STRING(I-CONTA-ITEM)  + "--" + STRING(tt-item-saida.it-codigo) ).

       chWorkSheet:Range("A" + string(i-linha)):VALUE = tt-item-saida.it-codigo.
       chWorkSheet:Range("B" + string(i-linha)):VALUE = tt-item-saida.cod-empresa.
       chWorkSheet:Range("C" + string(i-linha)):VALUE = tt-item-saida.cod-estabel.
       chWorkSheet:Range("D" + string(i-linha)):VALUE = tt-item-saida.un.
       chWorkSheet:Range("E" + string(i-linha)):VALUE = tt-item-saida.cod-familia.
       chWorkSheet:Range("F" + string(i-linha)):VALUE = tt-item-saida.cod-fam-comerc.
       chWorkSheet:Range("G" + string(i-linha)):VALUE = tt-item-saida.desc-item.
       chWorkSheet:Range("H" + string(i-linha)):VALUE = tt-item-saida.desc-narrativa.
       chWorkSheet:Range("I" + string(i-linha)):VALUE = tt-item-saida.cod-depart.
       chWorkSheet:Range("J" + string(i-linha)):VALUE = tt-item-saida.cod-grp-estoque.
       chWorkSheet:Range("K" + string(i-linha)):VALUE = tt-item-saida.cod-complementear.
       chWorkSheet:Range("L" + string(i-linha)):VALUE = tt-item-saida.dt-extracao.
       chWorkSheet:Range("M" + string(i-linha)):VALUE = tt-item-saida.tp-transacao.
       chWorkSheet:Range("N" + string(i-linha)):VALUE = tt-item-saida.saldo-item.
       chWorkSheet:Range("O" + string(i-linha)):VALUE = tt-item-saida.dt-saldo-estoq.
       chWorkSheet:Range("P" + string(i-linha)):VALUE = tt-item-saida.tp-transacao2.
       chWorkSheet:Range("Q" + string(i-linha)):VALUE = tt-item-saida.it-codigo-Klassmatt.
       chWorkSheet:Range("r" + string(i-linha)):VALUE = tt-item-saida.c-log.

/*        RUN pi-gera-borda(INPUT "A" + string(i-linha) + ":Q" + string(i-linha)). */

       ASSIGN i-linha = i-linha + 1.
       I-CONTA-ITEM = I-CONTA-ITEM - 1.
   END.
END PROCEDURE.

PROCEDURE pi-cria-cabecalho :

   
    CREATE "Excel.Application" chExcelApp.
    chExcelApp:VISIBLE = FALSE.
    chWorkbook = chExcelApp:Workbooks:Add().
    chExcelApp:AlertBeforeOverwriting = NO.
    chWorkSheet = chExcelApp:Sheets:Item(1).
    chWorkSheet:name = "Extra‡Æo Item".
    chExcelApp:ActiveWindow:Zoom = 90.

    cRange = "A1:Q1".
    chExcelApp:SELECTION:MergeCells = YES.
    chExcelApp:Range(cRange):FONT:bold = YES.
    chExcelApp:Range(cRange):Merge.
    chWorkSheet:Range(cRange):value = "Extra‡Æo de Itens".
    chExcelApp:Range(cRange):FONT:NAME = "Courier New".
    chExcelApp:Range(cRange):FONT:SIZE = "11".
    chExcelApp:Range(cRange):HorizontalAlignment = 3.

    /* ------------------------------------------------------------------------------------------------------------------------- */

    chWorkSheet:Range("A3"):value = "Item".
    chWorkSheet:Range("B3"):value = "Cod. Empresa".
    chWorkSheet:Range("C3"):value = "Cod. Estabel".
    chWorkSheet:Range("D3"):value = "UN".
    chWorkSheet:Range("E3"):value = "Cod. Familia".
    chWorkSheet:Range("F3"):value = "Cod. Familia Comerc.".
    chWorkSheet:Range("G3"):value = "Desc. Item".
    chWorkSheet:Range("H3"):value = "Narrativa".
    chWorkSheet:Range("I3"):value = "Cod. Depart.".
    chWorkSheet:Range("J3"):value = "Grp Estoque".
    chWorkSheet:Range("K3"):value = "Cod. Complementar".
    chWorkSheet:Range("L3"):value = "Dt. Extra‡Æo".
    chWorkSheet:Range("M3"):value = "Tipo Transa‡Æo".
    chWorkSheet:Range("N3"):value = "Saldo Estoque".
    chWorkSheet:Range("O3"):value = "Dt. Saldo Estoque".
    chWorkSheet:Range("P3"):value = "Tipo Transa‡Æo2".
    chWorkSheet:Range("Q3"):value = "Item Klassmatt".
    chWorkSheet:Range("r3"):value = "Log Extra‡Æo".

    chExcelApp:Range("A3:r3"):FONT:bold = YES.
    chExcelApp:Range("A3:r3"):FONT:NAME = "Arial".
    chExcelApp:Range("A3:r3"):FONT:SIZE = "9".
    chExcelApp:Range("A3:r3"):HorizontalAlignment = 3.
    chExcelApp:Range("A3:r3"):AutoFilter(,,,).

    RUN pi-gera-borda("A3:R3").

    chExcelApp:ActiveWindow:SplitColumn = 0.
    chExcelApp:ActiveWindow:SplitRow    = 3.
    chExcelApp:ActiveWindow:FreezePanes = YES.
END PROCEDURE.

PROCEDURE pi-gera-borda :
    def input param c-selecao as char NO-UNDO.

    chWorkSheet:Range(c-selecao):SELECT.
    ASSIGN chExcelApp:Selection:Borders(5):LineStyle   = -4142
           chExcelApp:Selection:Borders(6):LineStyle   = -4142
           chExcelApp:Selection:Borders(7):LineStyle   = 1
           chExcelApp:Selection:Borders(7):Weight      = 2
           chExcelApp:Selection:Borders(7):ColorIndex  = -4142
           chExcelApp:Selection:Borders(8):LineStyle   = 1
           chExcelApp:Selection:Borders(8):Weight      = 2
           chExcelApp:Selection:Borders(8):ColorIndex  = -4142
           chExcelApp:Selection:Borders(9):LineStyle   = 1
           chExcelApp:Selection:Borders(9):Weight      = 2
           chExcelApp:Selection:Borders(9):ColorIndex  = -4142
           chExcelApp:Selection:Borders(10):LineStyle  = 1
           chExcelApp:Selection:Borders(10):Weight     = 2
           chExcelApp:Selection:Borders(10):ColorIndex = -4142
           chExcelApp:Selection:Borders(11):LineStyle  = 1
           chExcelApp:Selection:Borders(12):LineStyle  = -4142.
END PROCEDURE.

PROCEDURE pi-finaliza-handle :
    chWorkSheet:Range("A:Q"):EntireColumn:AutoFit.
    chExcelApp:VISIBLE = YES.

    IF VALID-HANDLE(chExcelApp)  THEN RELEASE OBJECT chExcelApp.
    IF VALID-HANDLE(chWorkbook)  THEN RELEASE OBJECT chWorkbook.
    IF VALID-HANDLE(chWorkSheet) THEN RELEASE OBJECT chWorksheet.
END PROCEDURE.


PROCEDURE pi-processa:

DEFINE VARIABLE i-total-saldo AS INTEGER     NO-UNDO.
    
FOR EACH ITEM WHERE item.tipo-contr = 2   NO-LOCK:

 
    RUN pi-acompanhar in h-acomp(INPUT "Item EXTRACAO: " + item.it-codigo).

    i-total-saldo = 0.
    for each saldo-estoq of item NO-LOCK where saldo-estoq.qtidade-atu <> 0:
            assign i-total-saldo = i-total-saldo + (saldo-estoq.qtidade-atu  -
                                                    saldo-estoq.qt-alocada   -
                                                    saldo-estoq.qt-aloc-prod -
                                                    saldo-estoq.qt-aloc-ped).
    END.

    IF i-total-saldo > 0  THEN DO:
        

        FIND FIRST tt-item-saida WHERE tt-item-saida.it-codigo   = ITEM.it-codigo
                                 AND tt-item-saida.cod-empresa = int(param-global.empresa-prin) NO-ERROR.
        IF NOT AVAIL tt-item-saida THEN DO:

            C-DEPTO = "".

            FIND LAST es-it-depto where es-it-depto.it-codigo = item.it-codigo                            
                                  and   es-it-depto.cod-depto <> 0 no-lock no-error.                      
            if avail es-it-depto then do:                                                                   
                FIND ES-DEPTO WHERE  es-depto.codigo = es-it-depto.cod-depto NO-LOCK NO-ERROR.           
                IF AVAIL ES-DEPTO THEN DO:                                                               
                    ASSIGN C-DEPTO = STRING(es-it-depto.cod-depto,"999") + "-" + ES-DEPTO.DESCRICAO.     
                END.
            END.

          
            run pi-cria-tt-item-saida(INPUT item.it-codigo,
                                           INPUT param-global.empresa-prin,
                                          INPUT item.cod-estabel,
                                          INPUT item.un,
                                          INPUT item.fm-codigo,
                                          INPUT item.fm-cod-com,
                                          INPUT item.desc-item,
                                          INPUT item.narrativa,
                                          INPUT C-DEPTO,
                                          INPUT item.ge-codigo,
                                          INPUT item.codigo-refer,
                                          INPUT TODAY,
                                          INPUT 0,
                                          INPUT i-total-saldo,
                                          INPUT da-ini-per,
                                          input item.class-fiscal,
                                          input item.codigo-orig,
                                          input substr(item.char-2,212,1),
                                          INPUT "Com Saldo Estoque").
        END.
    END.
    ELSE DO:

        ASSIGN l-tem-movimento = FALSE.

        d-ini = today - i-dias-pesquisa.
        d-fim = TODAY.
        DO d-ref = TODAY  TO d-ini BY -1:
    
            FIND LAST movto-estoq no-lock use-index data-item where movto-estoq.dt-trans  = d-ref
                                                               and movto-estoq.it-codigo = ITEM.it-codigo NO-ERROR.
            IF AVAIL movto-estoq THEN DO:
                FIND FIRST tt-item-saida WHERE tt-item-saida.it-codigo   = ITEM.it-codigo
                                         AND tt-item-saida.cod-empresa   = int(param-global.empresa-prin) NO-ERROR.
                IF NOT AVAIL tt-item-saida THEN DO:
                    C-DEPTO = "".
                    FIND LAST es-it-depto where es-it-depto.it-codigo = item.it-codigo                            
                                          and   es-it-depto.cod-depto <> 0 no-lock no-error.                      
                    if avail es-it-depto then do:                                                                   
                        FIND ES-DEPTO WHERE  es-depto.codigo = es-it-depto.cod-depto NO-LOCK NO-ERROR.           
                        IF AVAIL ES-DEPTO THEN DO:                                                               
                            ASSIGN C-DEPTO = STRING(es-it-depto.cod-depto,"999") + "-" + ES-DEPTO.DESCRICAO.     
                        END.
                    END.


                    run pi-cria-tt-item-saida(INPUT item.it-codigo,                
                                  INPUT param-global.empresa-prin,     
                                  INPUT item.cod-estabel,              
                                  INPUT item.un,                       
                                  INPUT item.fm-codigo,                
                                  INPUT item.fm-cod-com,               
                                  INPUT item.desc-item,                
                                  INPUT item.narrativa,                
                                  INPUT C-DEPTO,                            
                                  INPUT item.ge-codigo,                
                                  INPUT item.codigo-refer,            
                                  INPUT TODAY,                         
                                  INPUT movto-estoq.tipo-trans,        
                                  INPUT 0,                             
                                  INPUT ?,                             
                                  input item.class-fiscal,             
                                  input item.codigo-orig,              
                                  input substr(item.char-2,212,1),
                                  INPUT "Ultimo" + "--" + string(movto-estoq.dt-trans,"99/99/9999") ).

                END.

                ASSIGN l-tem-movimento = TRUE.
                LEAVE.
 
            END.
            ELSE DO:
            END.
        END.
        IF NOT l-tem-movimento THEN RUN pi-procura-ordem.




    END.
END.





END PROCEDURE.




PROCEDURE pi-procura-ordem:


                



                FOR LAST ordem-compra WHERE ordem-compra.situacao <> 4
/*                                        AND   ordem-compra.situacao <> 6 */
                                       AND   ordem-compra.it-codigo = ITEM.it-codigo NO-LOCK:
                    ASSIGN da-dt-ref = TODAY - 90. 



                   

                    IF ordem-compra.data-emissao < da-dt-ref THEN NEXT.
                    ELSE DO:
                       FIND FIRST tt-item-saida WHERE tt-item-saida.it-codigo   = ITEM.it-codigo
                                                AND tt-item-saida.cod-empresa = int(param-global.empresa-prin) NO-ERROR.
                       IF NOT AVAIL tt-item-saida THEN DO:
                           C-DEPTO = "".
                           FIND LAST es-it-depto where es-it-depto.it-codigo = item.it-codigo                            
                                                 and   es-it-depto.cod-depto <> 0 no-lock no-error.                      
                           if avail es-it-depto then do:                                                                   
                               FIND ES-DEPTO WHERE  es-depto.codigo = es-it-depto.cod-depto NO-LOCK NO-ERROR.           
                               IF AVAIL ES-DEPTO THEN DO:                                                               
                                   ASSIGN C-DEPTO = STRING(es-it-depto.cod-depto,"999") + "-" + ES-DEPTO.DESCRICAO.     
                               END.
                           END.

                           run pi-cria-tt-item-saida(INPUT item.it-codigo,                            
                                                   INPUT param-global.empresa-prin,         
                                                   INPUT item.cod-estabel,                  
                                                   INPUT item.un,                           
                                                   INPUT item.fm-codigo,                    
                                                   INPUT item.fm-cod-com,                   
                                                   INPUT item.desc-item,                    
                                                   INPUT item.narrativa,                    
                                                   INPUT C-DEPTO,                                
                                                   INPUT item.ge-codigo,                    
                                                   INPUT item.codigo-refer,                
                                                   INPUT TODAY,                             
                                                   INPUT 0,                                 
                                                   INPUT i-total-saldo,                     
                                                   INPUT da-ini-per,                        
                                                   input item.class-fiscal,                 
                                                   input item.codigo-orig,                  
                                                   input substr(item.char-2,212,1),
                                                   INPUT "OC =>" + "--" + STRING(ordem-compra.data-emissao,"99/99/9999")).
                       END.
                    END.
                END.

END.  /*end Procedure*/












