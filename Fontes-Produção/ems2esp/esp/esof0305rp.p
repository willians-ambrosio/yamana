/************************************************************************
* Programa..: ESOF0305RP.P                                                *
* DescriØ o.: 
* AUTOR.....: DSC
* DATA......: 18/11/2014                                                *
************************************************************************/
{include/i-prgvrs.i ESOF0305RP 2.06.00.000}

/* ------------------------ Definiá‰es ------------------------- */
def temp-table tt-param
    field destino          as int
    field arquivo          as char
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as int
    field classifica       as int
    field desc-classifica  as char format "x(40)"
    field cod-estabel-ini  like doc-fiscal.cod-estabel
    field cod-estabel-fim  like doc-fiscal.cod-estabel
    field serie-ini        like doc-fiscal.serie
    field serie-fim        like doc-fiscal.serie
    field nr-doc-fis-ini   like doc-fiscal.nr-doc-fis
    field nr-doc-fis-fim   like doc-fiscal.nr-doc-fis
    field cod-emitente-ini like doc-fiscal.cod-emitente
    field cod-emitente-fim like doc-fiscal.cod-emitente
    field nat-operacao-ini like doc-fiscal.nat-operacao
    field nat-operacao-fim like doc-fiscal.nat-operacao
    field dt-docto-ini     like doc-fiscal.dt-docto
    field dt-docto-fim     like doc-fiscal.dt-docto
    field arq-import       as char.

def temp-table tt-digita
    field ordem   as int  format ">>>>9"
    field exemplo as char format "x(30)"
    index id is primary unique ordem.

def temp-table tt-it-doc-fisc no-undo
    field row-it-doc-fisc as rowid.

def temp-table tt-report no-undo
    field cod-erro     as int
    field des-erro     as char
    field num-linha    as int
    field cod-estabel  like it-doc-fisc.cod-estabel
    field serie        like it-doc-fisc.serie
    field nr-doc-fis   like it-doc-fisc.nr-doc-fis
    field cod-emitente like it-doc-fisc.cod-emitente
    field nat-operacao like it-doc-fisc.nat-operacao
    field nr-seq-doc   like it-doc-fisc.nr-seq-doc
    field it-codigo    like it-doc-fisc.it-codigo
    field dt-docto     like it-doc-fisc.dt-docto.

def buffer b-tt-digita for tt-digita.

def temp-table tt-raw-digita no-undo
    field raw-digita	   as raw.

def var v_han_acomp            as handle                     no-undo.

def var chExcel                as component-handle           no-undo.
def var chWBook                as component-handle           no-undo.
def var chWSheet               as component-handle           no-undo.

def var v_num_linha            as int                        no-undo.
def var c_num_linha            as char                       no-undo.

def var v-cod-estabel          like it-doc-fisc.cod-estabel  no-undo.
def var v-serie                like it-doc-fisc.serie        no-undo.
def var v-nr-doc-fis           like it-doc-fisc.nr-doc-fis   no-undo.
def var v-cod-emitente         like it-doc-fisc.cod-emitente no-undo.
def var v-nat-operacao         like it-doc-fisc.nat-operacao no-undo.
def var v-nr-seq-doc           like it-doc-fisc.nr-seq-doc   no-undo.
def var v-it-codigo            like it-doc-fisc.it-codigo    no-undo.
def var v-cod-cfop             like doc-fiscal.cod-cfop      no-undo.

def var v-log-desmembr         as log                        no-undo.

def var v-val-aliq-cofins-pago as dec                        no-undo.
def var v-val-cofins-pago      as dec                        no-undo.
def var v-val-aliq-pis-pago    as dec                        no-undo.
def var v-val-pis-pago         as dec                        no-undo.

def buffer b-doc-fiscal  for doc-fiscal.
def buffer b-it-doc-fisc for it-doc-fisc.

/* --------------------------- Inicializaá∆o -------------------------- */
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

run utp/ut-acomp.p persistent set v_han_acomp.
run pi-inicializar in v_han_acomp('Processando').

create 'Excel.application' chExcel.

chExcel:WorkBooks:open(tt-param.arq-import).
chWBook  = chExcel:ActiveWorkBook.
chWSheet = chWBook:ActiveSheet.

/* --------------------------- Processamento --------------------------- */
block1:
do v_num_linha = 2 to 400000:
    assign c_num_linha = string(v_num_linha).

    run pi-acompanhar in v_han_acomp('Linha: ' + c_num_linha).

    assign v-cod-estabel = entry(1,chWSheet:Range('A' + c_num_linha):value,",").


    
    if  v-cod-estabel = ? or
        v-cod-estabel = '' then
        leave block1.

    assign v-serie        = entry(1,chWSheet:Range('D' + c_num_linha):value,",")
           v-nr-doc-fis   = string(int(chWSheet:Range('B' + c_num_linha):value))
           v-cod-emitente = chWSheet:Range('J' + c_num_linha):value
           v-nat-operacao = entry(1,chWSheet:Range('G' + c_num_linha):value,",")
           v-nr-seq-doc   = chWSheet:Range('R' + c_num_linha):value
           v-it-codigo    = entry(1,chWSheet:Range('S' + c_num_linha):value,",")
           v-cod-cfop     = entry(1,chWSheet:Range('I' + c_num_linha):value,",")
           v-log-desmembr = no.

    if  length(v-nr-doc-fis) < 7 then
        assign v-nr-doc-fis = string(int(v-nr-doc-fis),"9999999").

    if  v-serie = ? then
        assign v-serie = ''.

    /****/

    IF v-it-codigo = ? THEN v-it-codigo = "".

   
    find first it-doc-fisc exclusive-lock where
               it-doc-fisc.cod-estabel  = v-cod-estabel  and
               it-doc-fisc.serie        = v-serie        and
               it-doc-fisc.nr-doc-fis   = v-nr-doc-fis   and
               it-doc-fisc.cod-emitente = v-cod-emitente and
               it-doc-fisc.nat-operacao = v-nat-operacao and
               it-doc-fisc.nr-seq-doc   = v-nr-seq-doc   and
               it-doc-fisc.it-codigo    = v-it-codigo no-error.
    if  not avail it-doc-fisc then do:
        create tt-report.
        assign tt-report.cod-erro     = 1
               tt-report.des-erro     = 'Item Documento Fiscal n∆o encontrado'
               tt-report.num-linha    = v_num_linha
               tt-report.cod-estabel  = v-cod-estabel
               tt-report.serie        = v-serie
               tt-report.nr-doc-fis   = v-nr-doc-fis
               tt-report.cod-emitente = v-cod-emitente
               tt-report.nat-operacao = v-nat-operacao
               tt-report.nr-seq-doc   = v-nr-seq-doc
               tt-report.it-codigo    = v-it-codigo.

        next block1.
    end.


    assign it-doc-fisc.val-base-calc-pis    = if chWSheet:Range('AC' + c_num_linha):value <> ? then dec(chWSheet:Range('AC' + c_num_linha):value) else 0
           it-doc-fisc.aliq-pis             = if chWSheet:Range('AD' + c_num_linha):value <> ? then dec(chWSheet:Range('AD' + c_num_linha):value) else 0
           it-doc-fisc.val-pis              = if chWSheet:Range('AE' + c_num_linha):value <> ? then dec(chWSheet:Range('AE' + c_num_linha):value) else 0
           it-doc-fisc.val-base-calc-cofins = if chWSheet:Range('AG' + c_num_linha):value <> ? then dec(chWSheet:Range('AG' + c_num_linha):value) else 0
           it-doc-fisc.aliq-cofins          = if chWSheet:Range('AH' + c_num_linha):value <> ? then dec(chWSheet:Range('AH' + c_num_linha):value) else 0
           it-doc-fisc.val-cofins           = if chWSheet:Range('AI' + c_num_linha):value <> ? then dec(chWSheet:Range('AI' + c_num_linha):value) else 0
           it-doc-fisc.vl-bicms-it          = if chWSheet:Range('AR' + c_num_linha):value <> ? then dec(chWSheet:Range('AR' + c_num_linha):value) else 0
           it-doc-fisc.aliquota-icm         = if chWSheet:Range('AS' + c_num_linha):value <> ? then dec(chWSheet:Range('AS' + c_num_linha):value) else 0
           it-doc-fisc.vl-icms-it           = if chWSheet:Range('AT' + c_num_linha):value <> ? then dec(chWSheet:Range('AT' + c_num_linha):value) else 0
           it-doc-fisc.vl-icmsou-it         = if chWSheet:Range('AU' + c_num_linha):value <> ? then dec(chWSheet:Range('AU' + c_num_linha):value) else 0
           it-doc-fisc.vl-icmsnt-it         = if chWSheet:Range('AV' + c_num_linha):value <> ? then dec(chWSheet:Range('AV' + c_num_linha):value) else 0
           it-doc-fisc.vl-bsubs-it          = if chWSheet:Range('AW' + c_num_linha):value <> ? then dec(chWSheet:Range('AW' + c_num_linha):value) else 0

           it-doc-fisc.vl-icmsub-it         = if chWSheet:Range('AY' + c_num_linha):value <> ? then chWSheet:Range('AY' + c_num_linha):value else 0
           it-doc-fisc.vl-icms-comp         = if chWSheet:Range('BA' + c_num_linha):value <> ? then chWSheet:Range('BA' + c_num_linha):value else 0
           v-val-aliq-cofins-pago           = if chWSheet:Range('BH' + c_num_linha):value <> ',0000000000' and chWSheet:Range('BH' + c_num_linha):value <> ? then dec(chWSheet:Range('BH' + c_num_linha):value) else 0
           v-val-cofins-pago                = if chWSheet:Range('BI' + c_num_linha):value <> ',0000000000' and chWSheet:Range('BI' + c_num_linha):value <> ? then dec(chWSheet:Range('BI' + c_num_linha):value) else 0
           v-val-aliq-pis-pago              = if chWSheet:Range('BJ' + c_num_linha):value <> ',0000000000' and chWSheet:Range('BJ' + c_num_linha):value <> ? then dec(chWSheet:Range('BJ' + c_num_linha):value) else 0
           v-val-pis-pago                   = if chWSheet:Range('BK' + c_num_linha):value <> ',0000000000' and chWSheet:Range('BK' + c_num_linha):value <> ? then dec(chWSheet:Range('BK' + c_num_linha):value) else 0.

    if it-doc-fisc.val-base-calc-pis    = ? then assign it-doc-fisc.val-base-calc-pis       = 0.
    if it-doc-fisc.aliq-pis             = ? then assign it-doc-fisc.aliq-pis                = 0.
    if it-doc-fisc.val-pis              = ? then assign it-doc-fisc.val-pis                 = 0.
    if it-doc-fisc.val-base-calc-cofins = ? then assign it-doc-fisc.val-base-calc-cofins    = 0.
    if it-doc-fisc.aliq-cofins          = ? then assign it-doc-fisc.aliq-cofins             = 0.
    if it-doc-fisc.val-cofins           = ? then assign it-doc-fisc.val-cofins              = 0.
    if it-doc-fisc.vl-bicms-it          = ? then assign it-doc-fisc.vl-bicms-it             = 0.
    if it-doc-fisc.aliquota-icm         = ? then assign it-doc-fisc.aliquota-icm            = 0.
    if it-doc-fisc.vl-icms-it           = ? then assign it-doc-fisc.vl-icms-it              = 0.
    if it-doc-fisc.vl-icmsou-it         = ? then assign it-doc-fisc.vl-icmsou-it            = 0.
    if it-doc-fisc.vl-icmsnt-it         = ? then assign it-doc-fisc.vl-icmsnt-it            = 0.
    if it-doc-fisc.vl-bsubs-it          = ? then assign it-doc-fisc.vl-bsubs-it             = 0.
    if it-doc-fisc.vl-icmsub-it         = ? then assign it-doc-fisc.vl-icmsub-it            = 0.
    if it-doc-fisc.vl-icms-comp         = ? then assign it-doc-fisc.vl-icms-comp            = 0.
    if v-val-aliq-cofins-pago           = 0 then assign substr(it-doc-fisc.char-2, 155, 9)  = '0,0000000'.    else assign substr(it-doc-fisc.char-2,155, 9) = string(v-val-aliq-cofins-pago,'999.99999').
    if v-val-cofins-pago                = 0 then assign substr(it-doc-fisc.char-2, 164, 12) = '0,0000000000'. else assign substr(it-doc-fisc.char-2,164,12) = string(v-val-cofins-pago,'999999999.99').
    if v-val-aliq-pis-pago              = 0 then assign substr(it-doc-fisc.char-2,235,9)    = '0,0000000'.    else assign substr(it-doc-fisc.char-2,235, 9) = string(v-val-aliq-pis-pago,'999.99999').
    if v-val-pis-pago                   = 0 then assign substr(it-doc-fisc.char-2,244,12)   = '0,0000000000'. else assign substr(it-doc-fisc.char-2,244,12) = string(v-val-pis-pago,'999999999.99').

    assign substr(it-doc-fisc.char-2, 22, 8) = string(it-doc-fisc.aliq-pis,'99999.99')
           substr(it-doc-fisc.char-2, 30, 8) = string(it-doc-fisc.aliq-cofins,'99999.99')
           substr(it-doc-fisc.char-2,138,17) = if chWSheet:Range('Y' + c_num_linha):value <> ',0000000000' and chWSheet:Range('Y' + c_num_linha):value <> ? then string(dec(chWSheet:Range('Y' + c_num_linha):value),'>>>>>>>>>>>>>9.99') else '                 '.

    /* Cleilton - 10/02/2015 */
    FIND FIRST item-doc-est 
        WHERE item-doc-est.serie-docto  = it-doc-fisc.serie
          AND item-doc-est.nro-docto    = it-doc-fisc.nr-doc-fis
          AND item-doc-est.cod-emitente = it-doc-fisc.cod-emitente
          AND item-doc-est.nat-operacao = it-doc-fisc.nat-operacao
          AND item-doc-est.sequencia    = it-doc-fisc.nr-seq-doc NO-ERROR.
    IF AVAIL item-doc-est THEN DO:
        IF chWSheet:Range('BB' + c_num_linha):value <> ? THEN DO:

                ASSIGN item-doc-est.num-sit-trib-icms     = INTEGER(SUBSTRING(chWSheet:Range('BB' + c_num_linha):VALUE,1,3))
                       OVERLAY(item-doc-est.char-2,502,3) = STRING(item-doc-est.num-sit-trib-icms,"999").
            /*  C‡DIGO POSIÄ«O 2 e 3	DESCRIÄ«O	Regra Campo cod-trib-icms
                00 	 Tributada Integralmente	ICMS = 1 TRIBUTADO
                10 	 Tributada e com cobranáa do ICMS por Substituiá∆o Tribut†ria	ICMS = 1 TRIBUTADO
                20 	 Com reduá∆o de base de c†lculo	ICMS = 4 REDUZIDO
                30 	 Isenta ou n∆o tributada e com cobranáa do ICMS por Substituiá∆o Tribut†ria	ICMS = 2 ISENTO
                40 	 Isenta	ICMS = 2 ISENTO
                41 	 N∆o tributada	ICMS = 2 ISENTO
                50 	 Suspens∆o	ICMS = 3 OUTROS
                51 	 Diferimento	ICMS = 3 OUTROS
                60	 ICMS cobrado anteriormente por Substituiá∆o Tribut†ria	ICMS = 3 OUTROS
                70	 Com reduá∆o de base de c†lculo e cobranáa do ICMS por Substituiá∆o tribut†ria	ICMS = 4 REDUZIDO
                90	 Outras	ICMS = 3 OUTROS
            
            */
            CASE SUBSTRING(chWSheet:Range('BB' + c_num_linha):VALUE,2,2):
                WHEN "00" OR 
                WHEN "10" THEN
                    ASSIGN it-doc-fisc.cd-trib-icm = 1.
                WHEN "30" OR 
                WHEN "40" OR 
                WHEN "41" THEN
                    ASSIGN it-doc-fisc.cd-trib-icm = 2.
                WHEN "50" OR 
                WHEN "51" OR 
                WHEN "60" OR 
                WHEN "90" THEN
                    ASSIGN it-doc-fisc.cd-trib-icm = 3.
                WHEN "20" OR 
                WHEN "70" THEN
                    ASSIGN it-doc-fisc.cd-trib-icm = 4.
            END CASE.
            
        END.
        /* 17/03/2015 - Cleilton - Atualizar classificaá∆o fiscal */
        IF chWSheet:Range('X' + c_num_linha):value <> "" AND chWSheet:Range('X' + c_num_linha):value <> ? THEN
            ASSIGN /*item-doc-est.class-fiscal = REPLACE(chWSheet:Range('X' + c_num_linha):value,".","") */
                   it-doc-fisc.class-fiscal  = REPLACE(chWSheet:Range('X' + c_num_linha):value,".","").
    END.
    ELSE DO:
        if it-doc-fisc.vl-bicms-it  <> 0 and
           it-doc-fisc.aliquota-icm <> 0 and
           it-doc-fisc.vl-icms-it   <> 0 then
            assign it-doc-fisc.cd-trib-icm = 1.
        else if it-doc-fisc.vl-icmsou-it <> 0 then
            assign it-doc-fisc.cd-trib-icm = 3.
        else
            assign it-doc-fisc.cd-trib-icm = 2.
    END.


    find first doc-fiscal exclusive-lock of it-doc-fisc no-error.
    if avail doc-fiscal then do:
        if chWSheet:Range('BL' + c_num_linha):value <> '' and
           chWSheet:Range('BL' + c_num_linha):value <>  ? and
           chWSheet:Range('BL' + c_num_linha):value <> ',0000000000' then
            assign substr(doc-fiscal.char-2,155,60) = chWSheet:Range('BL' + c_num_linha):value.

        if v-cod-cfop <> doc-fiscal.cod-cfop then do:
            find first b-doc-fiscal exclusive-lock where
                       b-doc-fiscal.cod-estabel  = v-cod-estabel  and
                       b-doc-fiscal.serie        = v-serie        and
                       b-doc-fiscal.nr-doc-fis   = v-nr-doc-fis   and
                       b-doc-fiscal.cod-emitente = v-cod-emitente and
                       b-doc-fiscal.nat-operacao = v-cod-cfop     no-error.
            if not avail b-doc-fiscal then do:
                create b-doc-fiscal.
                buffer-copy doc-fiscal except nat-operacao vl-cont-doc to b-doc-fiscal.
                assign b-doc-fiscal.nat-operacao = v-cod-cfop
                       b-doc-fiscal.cod-cfop     = v-cod-cfop.
            end.

            /* 24/03/2015 - Solicitado Manter sequància do item */
/*             find last b-it-doc-fisc no-lock of b-doc-fiscal no-error. */
/*             if avail b-it-doc-fisc then                                         */
/*                 assign it-doc-fisc.nr-seq-doc   = b-it-doc-fisc.nr-seq-doc + 10 */
/*                        it-doc-fisc.nat-operacao = v-cod-cfop.                   */
/*             else                                                                */
/*                 assign it-doc-fisc.nr-seq-doc   = 10                            */
/*                        it-doc-fisc.nat-operacao = v-cod-cfop.                   */
            ASSIGN it-doc-fisc.nat-operacao = v-cod-cfop.

            assign doc-fiscal.vl-cont-doc   = doc-fiscal.vl-cont-doc   - it-doc-fisc.vl-tot-item
                   b-doc-fiscal.vl-cont-doc = b-doc-fiscal.vl-cont-doc + it-doc-fisc.vl-tot-item.

            release b-doc-fiscal.
            release b-it-doc-fisc NO-ERROR.

            assign v-log-desmembr = true.

            create tt-report.
            assign tt-report.cod-erro     = 3
                   tt-report.des-erro     = 'Item/Documento desmembrado com sucesso'
                   tt-report.num-linha    = v_num_linha
                   tt-report.cod-estabel  = v-cod-estabel
                   tt-report.serie        = v-serie
                   tt-report.nr-doc-fis   = v-nr-doc-fis
                   tt-report.cod-emitente = v-cod-emitente
                   tt-report.nat-operacao = it-doc-fisc.nat-operacao
                   tt-report.nr-seq-doc   = v-nr-seq-doc
                   tt-report.it-codigo    = v-it-codigo
                   tt-report.dt-docto     = it-doc-fisc.dt-docto.
        end.

        find first b-it-doc-fisc no-lock
             where b-it-doc-fisc.cod-estabel  = v-cod-estabel
               and b-it-doc-fisc.serie        = v-serie
               and b-it-doc-fisc.nr-doc-fis   = v-nr-doc-fis
               and b-it-doc-fisc.cod-emitente = v-cod-emitente
               and b-it-doc-fisc.nat-operacao = v-nat-operacao no-error.
        if  not avail b-it-doc-fisc then
            delete doc-fiscal.

        release b-it-doc-fisc.
    end.

    create tt-it-doc-fisc.
    assign tt-it-doc-fisc.row-it-doc-fisc = rowid(it-doc-fisc).

    if  not v-log-desmembr then do:
        create tt-report.
        assign tt-report.cod-erro     = 2
               tt-report.des-erro     = 'Item/Documento atualizado com sucesso'
               tt-report.num-linha    = v_num_linha
               tt-report.cod-estabel  = v-cod-estabel
               tt-report.serie        = v-serie
               tt-report.nr-doc-fis   = v-nr-doc-fis
               tt-report.cod-emitente = v-cod-emitente
               tt-report.nat-operacao = v-nat-operacao
               tt-report.nr-seq-doc   = v-nr-seq-doc
               tt-report.it-codigo    = v-it-codigo
               tt-report.dt-docto     = it-doc-fisc.dt-docto.
    end.
end.

chWBook:close(no).

if  valid-handle(chWSheet) then release object chWSheet.
if  valid-handle(chWBook ) then release object chWBook.

/* ------------------------------------------- Impressao de Relatorio -------------------------------- */
chExcel:WorkBooks:add().
chWBook  = chExcel:ActiveWorkBook.
chWSheet = chWBook:ActiveSheet.

run pi-seta-titulo in v_han_acomp('Imprimindo Relat¢rio').

chWSheet:Range('A1:L3'):MergeCells = true.
chWSheet:Range('A1'):value = 'Relat¢rio de Importaá∆o de Itens/Documento Fiscal'.
/*
chWSheet:Range('A1'):HorizontalAlignment = -4103.
chWSheet:Range('A1'):VerticalAlignment   = -4103.
*/
assign v_num_linha = 5
       c_num_linha = string(v_num_linha).

chWSheet:Range('A' + c_num_linha):value = 'C¢d. Erro'.
chWSheet:Range('B' + c_num_linha):value = 'Desc. Erro'.
chWSheet:Range('C' + c_num_linha):value = 'Linha Arquivo'.
chWSheet:Range('D' + c_num_linha):value = 'Estab'.
chWSheet:Range('E' + c_num_linha):value = 'SÇrie'.
chWSheet:Range('F' + c_num_linha):value = 'Nr. Docto'.
chWSheet:Range('G' + c_num_linha):value = 'Emitente'.
chWSheet:Range('H' + c_num_linha):value = 'Nome Emit'.
chWSheet:Range('I' + c_num_linha):value = 'Nat. Operaá∆o'.
chWSheet:Range('J' + c_num_linha):value = 'Seq.'.
chWSheet:Range('K' + c_num_linha):value = 'Item'.
chWSheet:Range('L' + c_num_linha):value = 'Dt. Docto'.

assign v_num_linha = v_num_linha + 1
       c_num_linha = string(v_num_linha).

for each tt-report no-lock,
   first emitente fields(cod-emitente nome-abrev) no-lock where
         emitente.cod-emitente = tt-report.cod-emitente:

    run pi-acompanhar in v_han_acomp('Linha: ' + string(tt-report.num-linha)).

    chWSheet:Range('A' + c_num_linha):value = tt-report.cod-erro.
    chWSheet:Range('B' + c_num_linha):value = tt-report.des-erro.
    chWSheet:Range('C' + c_num_linha):value = tt-report.num-linha.
    chWSheet:Range('D' + c_num_linha):value = tt-report.cod-estabel.
    chWSheet:Range('E' + c_num_linha):value = tt-report.serie.
    chWSheet:Range('F' + c_num_linha):value = "'" + tt-report.nr-doc-fis.
    chWSheet:Range('G' + c_num_linha):value = tt-report.cod-emitente.
    chWSheet:Range('H' + c_num_linha):value = emitente.nome-abrev.
    chWSheet:Range('I' + c_num_linha):value = tt-report.nat-operacao.
    chWSheet:Range('J' + c_num_linha):value = tt-report.nr-seq-doc.
    chWSheet:Range('K' + c_num_linha):value = tt-report.it-codigo.
    chWSheet:Range('L' + c_num_linha):value = tt-report.dt-docto.

    assign v_num_linha = v_num_linha + 1
           c_num_linha = string(v_num_linha).
end.

for each it-doc-fisc no-lock where
         it-doc-fisc.cod-estabel  >= tt-param.cod-estabel-ini  and
         it-doc-fisc.cod-estabel  <= tt-param.cod-estabel-fim  and
         it-doc-fisc.serie        >= tt-param.serie-ini        and
         it-doc-fisc.serie        <= tt-param.serie-fim        and
         it-doc-fisc.nr-doc-fis   >= tt-param.nr-doc-fis-ini   and
         it-doc-fisc.nr-doc-fis   <= tt-param.nr-doc-fis-fim   and
         it-doc-fisc.cod-emitente >= tt-param.cod-emitente-ini and
         it-doc-fisc.cod-emitente <= tt-param.cod-emitente-fim and
         it-doc-fisc.nat-operacao >= tt-param.nat-operacao-ini and
         it-doc-fisc.nat-operacao <= tt-param.nat-operacao-fim and
         it-doc-fisc.dt-docto     >= tt-param.dt-docto-ini     and
         it-doc-fisc.dt-docto     <= tt-param.dt-docto-fim     and
    not can-find(first tt-it-doc-fisc no-lock where
                       tt-it-doc-fisc.row-it-doc-fisc = rowid(it-doc-fisc)),
    first emitente fields(cod-emitente nome-abrev) no-lock where
          emitente.cod-emitente = it-doc-fisc.cod-emitente:

    run pi-acompanhar in v_han_acomp('Linha: ' + c_num_linha).

    chWSheet:Range('A' + c_num_linha):value = 4.
    chWSheet:Range('B' + c_num_linha):value = 'Item/Documento n∆o informado para atualizaá∆o'.
    chWSheet:Range('C' + c_num_linha):value = 0.
    chWSheet:Range('D' + c_num_linha):value = it-doc-fisc.cod-estabel.
    chWSheet:Range('E' + c_num_linha):value = it-doc-fisc.serie.
    chWSheet:Range('F' + c_num_linha):value = it-doc-fisc.nr-doc-fis.
    chWSheet:Range('G' + c_num_linha):value = it-doc-fisc.cod-emitente.
    chWSheet:Range('H' + c_num_linha):value = emitente.nome-abrev.
    chWSheet:Range('I' + c_num_linha):value = it-doc-fisc.nat-operacao.
    chWSheet:Range('J' + c_num_linha):value = it-doc-fisc.nr-seq-doc.
    chWSheet:Range('K' + c_num_linha):value = it-doc-fisc.it-codigo.
    chWSheet:Range('L' + c_num_linha):value = it-doc-fisc.dt-docto.

    assign v_num_linha = v_num_linha + 1
           c_num_linha = string(v_num_linha).
end.

chWSheet:Range('A1:IV65535'):EntireColumn:Autofit.

/* ------------------------------------------ Finalizacao ------------------------------------ */
chExcel:visible = yes.

if  valid-handle(chWSheet) then release object chWSheet.
if  valid-handle(chWBook ) then release object chWBook.
if  valid-handle(chExcel ) then release object chExcel.

run pi-finalizar in v_han_acomp.

return 'OK':U.
