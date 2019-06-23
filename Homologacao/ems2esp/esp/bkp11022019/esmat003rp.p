/************************************************************************
**
**  Programa: ESMAT003RP
**  Objetivo: Relatorio de Gastos de Materiais
**     Autor: Roberto Leandro - Kraft
**      Data: 10/08/2011
**
************************************************************************/
DEFINE BUFFER pais FOR emsbas.pais.

define temp-table tt-param no-undo
    FIELD fi-est-ini   AS CHAR
    FIELD fi-est-fim   AS CHAR
    FIELD fi-item-ini  AS CHAR
    FIELD fi-item-fim  AS CHAR
    FIELD dt-base-ini  AS DATE
    FIELD dt-base-fim  AS DATE
    FIELD fi-ct-ini    AS CHAR
    FIELD fi-ct-fim    AS CHAR
    FIELD fi-cc-ini    AS CHAR
    FIELD fi-cc-fim    AS CHAR
    FIELD fi-ge-ini    AS INT
    FIELD fi-ge-fim    AS INT
    FIELD l-fisico     AS LOG
    FIELD l-total      AS LOG
    FIELD l-consig     AS LOG
    FIELD l-ddireto    AS LOG
    field l-cep    as log
    field l-app    as log.

define input param table for tt-param.
find first tt-param no-error.

DEFINE TEMP-TABLE tt-controle no-undo
    field dt-trans       as date
    FIELD origem         as char
    FIELD conta-contabil as char
    FIELD desc-conta     as char
    FIELD sc-codigo      as char
    FIELD desc-sc-codigo as char
    FIELD nr-docto       as char
    field emitente       as char
    FIELD valor          AS DEC
    field narrativa      as char
    index ch1 dt-trans origem conta-contabil sc-codigo.

FIND FIRST tt-param NO-ERROR.

{utp/ut-glob.i}

def buffer ccusto for ems5.ccusto.

/* Definicao de Variaveis */
DEF VAR h-acomp          AS HANDLE                    NO-UNDO.
DEF VAR i-linha          AS INT                       NO-UNDO.
DEF VAR i-regs           AS INT                       NO-UNDO.
DEF VAR x-regs           AS INT                       NO-UNDO.

DEF VAR i-signal         AS INT                       NO-UNDO EXTENT 2 INITIAL [+1,-1].
DEF VAR d-valor          AS DEC                       NO-UNDO.
DEF VAR d-qtde           AS DEC                       NO-UNDO.
DEF VAR dt-it-per        AS date                      NO-UNDO.

DEF VAR de-vl-tot   AS DECIMAL   NO-UNDO.
DEF VAR de-vl-pis   AS DECIMAL   NO-UNDO.
DEF VAR de-val-1    AS DEC.
DEF VAR de-val-2    AS DEC.

DEFINE VARIABLE v_cod_estab_aux    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE v_cod_finalid_econ AS CHARACTER   NO-UNDO.
/* Variaveis para Gerar em Excel */
DEF VAR chExcelApplication AS com-handle no-undo.
DEF VAR chworkbook         AS com-handle no-undo.
DEF VAR chworksheet        AS com-handle no-undo.
DEF VAR arquivo-at         AS char no-undo.
DEF VAR arquivo-cp         AS char no-undo.
DEF VAR arquivo-nv         AS char no-undo.

function descricoes returns logical ():
    find first plano_ccusto no-lock
         where plano_ccusto.cod_empresa     = v_cod_empres_usuar
           and plano_ccusto.dat_inic_valid <= today
           and plano_ccusto.dat_fim_valid  >= today no-error.

    for first cta_ctbl fields(des_tit_ctbl) no-lock
        where cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
          and cta_ctbl.cod_cta_ctbl       = tt-controle.conta-contabil:
        assign tt-controle.desc-conta = trim(cta_ctbl.des_tit_ctbl).
    end.

    for first centro-custo fields(descricao) no-lock where centro-custo.cc-codigo = tt-controle.sc-codigo:
        assign tt-controle.desc-sc-codigo = trim(centro-custo.descricao).
    end.
    if  not avail centro-custo or tt-controle.desc-sc-codigo = "" then do:
        for first ccusto no-lock
            where ccusto.cod_empresa      = plano_ccusto.cod_empresa
              and ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
              and ccusto.cod_ccusto       = tt-controle.sc-codigo:
            assign tt-controle.desc-sc-codigo = trim(ccusto.des_tit_ctbl).
        END.
    end.

    return false.
end.

/* Processamento das Informacoes */
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

DEF VAR c--acomp AS CHAR NO-UNDO INITIAL "\,|,/,-,\,|,/,-".
DEF VAR i--acomp AS INT  NO-UNDO.
PROCEDURE pi-acomp:
    DEF INPUT PARAM pMsg AS CHAR NO-UNDO.
    ASSIGN i--acomp = IF i--acomp = NUM-ENTRIES(c--acomp) THEN 1 ELSE i--acomp + 1.
    run pi-acompanhar IN h-acomp (subst("(&1) &2",ENTRY(i--acomp,c--acomp),pMsg)).
    RETURN.
END PROCEDURE.

if  l-cep then do:

    RUN pi-inicializar in h-acomp ("Aguarde, Selecionando CEP...").
    for each  item-estab no-lock use-index estabel
        where item-estab.cod-estabel >= fi-est-ini
        and   item-estab.cod-estabel <= fi-est-fim
        and   item-estab.it-codigo   >= fi-item-ini
        and   item-estab.it-codigo   <= fi-item-fim,
        first item no-lock
        where item.it-codigo = item-estab.it-codigo
          AND ITEM.ge-codigo >= fi-ge-ini
          AND ITEM.ge-codigo <= fi-ge-fim:

        RUN pi-acomp(subst("Estabelecimento: &1 / Item: &2",item-estab.cod-estabel,item-estab.it-codigo)).

        if item.tipo-contr = 1 and not tt-param.l-fisico  then next.
        if item.tipo-contr = 2 and not tt-param.l-total   then next.
        if item.tipo-contr = 3 and not tt-param.l-consig  then next.
        if item.tipo-contr = 4 and not tt-param.l-ddireto then next.

        for each  movto-estoq no-lock
            where movto-estoq.cod-estabel  = item-estab.cod-estabel
            and   movto-estoq.it-codigo    = item-estab.it-codigo  
            and   movto-estoq.dt-trans    >= dt-base-ini
            and   movto-estoq.dt-trans    <= dt-base-fim
            and   movto-estoq.ct-codigo   >= fi-ct-ini
            and   movto-estoq.ct-codigo   <= fi-ct-fim
            and   movto-estoq.sc-codigo   >= fi-cc-ini
            and   movto-estoq.sc-codigo   <= fi-cc-fim:

            RUN pi-acomp(subst("Estabelecimento: &1 / Item: &2",item-estab.cod-estabel,item-estab.it-codigo)).

/*            if  item.tipo-contr = 2 or item.tipo-contr = 3 then

                if  movto-estoq.esp-docto <> 05 and 
                    movto-estoq.esp-docto <> 07 and
                    movto-estoq.esp-docto <> 28 and
                    movto-estoq.esp-docto <> 30 then next.

            else 

                if  ( movto-estoq.esp-docto <> 21 and 
                      movto-estoq.esp-docto <> 18 ) or 
                      movto-estoq.tipo-trans = 1 or
                      movto-estoq.ct-codigo = "13601002" or
                      movto-estoq.ct-codigo = "17202002" or
                      movto-estoq.ct-codigo = "11511004" or
                      movto-estoq.ct-codigo = "17202014" or 
                      movto-estoq.ct-codigo = "41106002" or
                      movto-estoq.ct-codigo = "11508202" then next.*/

            ASSIGN de-val-1 = DEC(SUBSTRING(movto-estoq.char-1,29,14)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN de-val-1 = 0.
            ASSIGN de-val-2 = DEC(SUBSTRING(movto-estoq.char-1,43,14)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN de-val-2 = 0.

            ASSIGN de-vl-pis = de-val-1 + de-val-2
                   de-vl-tot = movto-estoq.valor-ggf-m[1]
                             + movto-estoq.valor-mat-m[1]
                             + movto-estoq.valor-mob-m[1]
                             + movto-estoq.valor-icm
                             + movto-estoq.valor-ipi
                             + movto-estoq.valor-iss
                             + de-vl-pis.

            IF de-vl-tot = 0 THEN NEXT.

            i-regs = i-regs + 1.
            create tt-controle.
            assign tt-controle.dt-trans       = movto-estoq.dt-trans
                   tt-controle.origem         = "CEP"
                   tt-controle.conta-contabil = movto-estoq.ct-codigo  
                   tt-controle.sc-codigo      = movto-estoq.sc-codigo  
                   tt-controle.nr-docto       = movto-estoq.nro-docto
/*                    tt-controle.valor          = de-vl-tot * if movto-estoq.tipo-trans = 1 then 1 else -1. */
                   tt-controle.valor          = de-vl-tot * if movto-estoq.tipo-trans = 2 then 1 else -1.

            descricoes().

            for first emitente fields(nome-emit) no-lock
                where emitente.cod-emitente = movto-estoq.cod-emitente:
                assign tt-controle.emitente = subst("&1 - &2",string(movto-estoq.cod-emitente),emitente.nome-emit).
            end.

            assign tt-controle.narrativa = "Est: " + movto-estoq.cod-estabel
                                         + " - Esp: " + {ininc/i03in218.i 04 movto-estoq.esp-docto}
                                         + " - Ser: " + movto-estoq.serie-docto
                                         + " - Ordem: " + ( IF movto-estoq.esp-docto = 19                        
                                                            OR movto-estoq.esp-docto = 20                        
                                                            OR movto-estoq.esp-docto = 21                        
                                                            OR movto-estoq.esp-docto = 22                        
                                                            OR movto-estoq.esp-docto = 23                        
                                                               THEN STRING(movto-estoq.numero-ordem,"zzzzz9,99") 
                                                               ELSE STRING(movto-estoq.nr-ord-prod,"zzzzzzzz9") )  
                                         + " - Item: " + movto-estoq.it-codigo + chr(10)
                                         + ( IF ITEM.tipo-contr = 4 AND movto-estoq.descricao-db <> ""
                                             THEN movto-estoq.descricao-db ELSE item.desc-item ).

        end.

    end.

end.

if  l-app then do:
    RUN pi-inicializar in h-acomp ("Aguarde, Selecionando APP...").

    FOR EACH aprop_ctbl_ap WHERE
             aprop_ctbl_ap.cod_estab           >= fi-est-ini  AND   
             aprop_ctbl_ap.cod_estab           <= fi-est-fim  AND   
             aprop_ctbl_ap.cod_cta_ctbl        >= fi-ct-ini   AND 
             aprop_ctbl_ap.cod_cta_ctbl        <= fi-ct-fim   AND 
             aprop_ctbl_ap.cod_ccusto          >= fi-cc-ini   AND 
             aprop_ctbl_ap.cod_ccusto          <= fi-cc-fim   AND 
             aprop_ctbl_ap.dat_transacao       >= dt-base-ini AND  
             aprop_ctbl_ap.dat_transacao       <= dt-base-fim AND 
             aprop_ctbl_ap.log_ctbz_aprop_ctbl  = YES         NO-LOCK:

        find movto_tit_ap no-lock
            where movto_tit_ap.cod_estab           = aprop_ctbl_ap.cod_estab
            and   movto_tit_ap.num_id_movto_tit_ap = aprop_ctbl_ap.num_id_movto_tit_ap
            no-error. 
           
        IF NOT AVAIL movto_tit_ap THEN NEXT.        

        FIND FIRST tit_ap OF movto_tit_ap NO-LOCK NO-ERROR.

        if  v_cod_estab_aux <> movto_tit_ap.cod_estab THEN
        DO:        
            assign v_cod_estab_aux = movto_tit_ap.cod_estab.
            run pi_retornar_finalid_econ_corren_estab (Input v_cod_estab_aux,
                                                       output v_cod_finalid_econ) /*pi_retornar_finalid_econ_corren_estab*/.
        END.

        find val_aprop_ctbl_ap no-lock
             where val_aprop_ctbl_ap.cod_estab = movto_tit_ap.cod_estab
               and val_aprop_ctbl_ap.num_id_aprop_ctbl_ap = aprop_ctbl_ap.num_id_aprop_ctbl_ap
               and val_aprop_ctbl_ap.cod_finalid_econ = v_cod_finalid_econ
               and val_aprop_ctbl_ap.val_aprop_ctbl > 0
        &if "{&emsfin_version}" /*l_{&emsfin_version}*/  >= "5.01" &then
             use-index vlprpctb_id
        &endif
              /* cl_relat_diario_aux_apb of val_aprop_ctbl_ap*/ no-error.

        IF NOT AVAIL val_aprop_ctbl_ap THEN NEXT.

        RUN pi-acomp(subst("Estabelecimento: &1 / Item: &2",movto_tit_ap.cod_estab,string(movto_tit_ap.dat_transacao))).

        i-regs = i-regs + 1.
        create tt-controle.
        assign tt-controle.dt-trans       = movto_tit_ap.dat_transacao
               tt-controle.origem         = "APP"
               tt-controle.conta-contabil = aprop_ctbl_ap.cod_cta_ctbl
               tt-controle.sc-codigo      = aprop_ctbl_ap.cod_ccusto
               tt-controle.nr-docto       = tit_ap.cod_tit_ap.

        if aprop_ctbl_ap.ind_natur_lancto_ctbl = "DB" THEN
           assign tt-controle.valor = val_aprop_ctbl_ap.val_aprop_ctbl.    
        ELSE
           assign tt-controle.valor = (val_aprop_ctbl_ap.val_aprop_ctbl) * - 1.    


        descricoes().

        for first ems5.fornecedor no-lock
            where ems5.fornecedor.cdn_fornecedor = tit_ap.cdn_fornecedor:
            assign tt-controle.emitente = subst("&1 - &2",string(tit_ap.cdn_fornecedor),ems5.fornecedor.nom_abrev).
        end.


        FIND FIRST histor_tit_movto_ap WHERE
                   histor_tit_movto_ap.cod_estab           = movto_tit_ap.cod_estab             AND 
                   histor_tit_movto_ap.num_id_tit_ap       = movto_tit_ap.num_id_tit_ap         AND 
                   histor_tit_movto_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap   NO-LOCK NO-ERROR.
        IF AVAIL histor_tit_movto_ap THEN
           ASSIGN tt-controle.narrativa =    "Est: " + tit_ap.cod_estab
                                        + " - Esp: " + tit_ap.cod_espec_docto
                                        + " - Ser: " + tit_ap.cod_ser_docto
                                        + " - Par: " + tit_ap.cod_parcela
                                        + " - Trans: " + string(movto_tit_ap.ind_trans_ap)
                                        + ( if histor_tit_movto_ap.des_text_histor = "" then "" else chr(10) + histor_tit_movto_ap.des_text_histor ).
    end.
end.

if  temp-table tt-controle:has-records then do:
    RUN pi-inicializar in h-acomp (subst("Aguarde, Exportando &1 registros para o Excel",TRIM(STRING(i-regs,"->>>,>>>,>>9")))).

    FILE-INFO:FILE-NAME = "esp/esmat003.xls".
    arquivo-at = FILE-INFO:FULL-PATHNAME.

    if  arquivo-at = ? then
        run utp/ut-msgs.p ("show",15825,"Planilha modelo n∆o foi encontrada!~~O arquivo modelo 'esp/esmat003.xls' n∆o foi encontrado, e por este motivo a planilha n∆o poder† ser gerada.").
    else do:
        CREATE "excel.application" chexcelapplication no-error.
        if  chExcelApplication = ? then
            run utp/ut-msgs.p ("show",15825,"Aplicativo MS EXCEL n∆o est† instalado !~~O Microsoft Excel n∆o est† instalado ou configurado adequadamente, e por este motivo a planilha n∆o poder† ser gerada.").
        else do:
            ASSIGN arquivo-cp = subst("&1esmat003_&2_&3.xls",
                                      session:temp-dir,
                                      string(year(today) * 10000 + month(today) * 100 + day(today),"99999999"),
                                      replace(string(time,"HH:MM:SS"),":","")).

            OS-DELETE VALUE(arquivo-cp).
            OS-COPY VALUE(arquivo-at) VALUE(CAPS(arquivo-cp)).

            chworkbook = chexcelapplication:workbooks:ADD(arquivo-cp).
            ASSIGN chexcelapplication:VISIBLE = FALSE.
            ASSIGN chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
            chworksheet = chexcelapplication:sheets:ITEM(1).   

            ASSIGN chworksheet:range("A2"):VALUE = subst("Est: &1 <> &2   -   Item: &3 <> &4   -   Data: &5 <> &6   -   Tipo Controle: &7",
                                                         fi-est-ini,fi-est-fim,fi-item-ini,fi-item-fim,string(dt-base-ini,"99/99/9999"),string(dt-base-fim,"99/99/9999"),
                                                         subst("F°sico=&1 / Total=&2 / Consignado=&3 / DÇbito Direto=&4",
                                                               string(l-fisico,"Sim/N∆o"),string(l-total,"Sim/N∆o"),string(l-consig,"Sim/N∆o"),string(l-ddireto,"Sim/N∆o"))).

            i-linha = 4.
            FOR EACH tt-controle:
                assign x-regs = x-regs + 1.
                run pi-acomp(subst("Data: &1 - &2%",string(tt-controle.dt-trans,"99/99/9999"),string(int( x-regs / i-regs * 100)))).

                ASSIGN i-linha                                         = i-linha + 1
                       chexcelapplication:range("A" + STRING(I-LINHA)) = tt-controle.dt-trans              
                       chexcelapplication:range("B" + STRING(I-LINHA)) = tt-controle.origem
                       chexcelapplication:range("C" + STRING(I-LINHA)) = tt-controle.conta-contabil
                       chexcelapplication:range("D" + STRING(I-LINHA)) = tt-controle.desc-conta    
                       chexcelapplication:range("E" + STRING(I-LINHA)) = tt-controle.sc-codigo     
                       chexcelapplication:range("F" + STRING(I-LINHA)) = tt-controle.desc-sc-codigo
                       chexcelapplication:range("G" + STRING(I-LINHA)) = tt-controle.nr-docto      
                       chexcelapplication:range("H" + STRING(I-LINHA)) = tt-controle.emitente 
                       chexcelapplication:range("I" + STRING(I-LINHA)) = tt-controle.valor 
                       chexcelapplication:range("J" + STRING(I-LINHA)) = tt-controle.narrativa.

                IF  i-linha = 65535 THEN DO:
                    run utp/ut-msgs.p ("show",15825,"ERRO na geraá∆o da planilha Excel~~Quantidade de registros processados Ç maior que a capacidade da planilha.~nDefina uma seleá∆o menor de filtros").
                    LEAVE.
                END.
            END.

            run utp/ut-msgs.p ("show",15825,"Processamento finalizado com sucesso!~~Foi gerado o arquivo:~n" + arquivo-cp).

            chworkbook:saved = TRUE.
            chexcelapplication:ActiveWorkbook:SaveAs(arquivo-cp,,,,,,).
            chexcelapplication:VISIBLE = TRUE.
            chExcelApplication:APPLICATION:DisplayAlerts = TRUE.
        end.

        IF VALID-HANDLE(chexcelapplication) THEN RELEASE OBJECT chexcelapplication.
        IF VALID-HANDLE(chworkbook)         THEN RELEASE OBJECT chworkbook.
        IF VALID-HANDLE(chworksheet)        THEN RELEASE OBJECT chworksheet.
    end.
end.
else 
    run utp/ut-msgs.p ("show",15825,"Nenhum registro foi processado!~~Verifique se os valores utilizados nos filtros est∆o corretos.").

RUN pi-finalizar IN h-acomp.

RETURN "OK":U.


PROCEDURE pi_retornar_finalid_econ_corren_estab:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_estab
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
        as character
        format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
        as Character
        format "x(5)"
    &ENDIF
        no-undo.
    def output param p_cod_finalid_econ
        as character
        format "x(10)"
        no-undo.


    /************************* Parameter Definition End *************************/

    find estabelecimento no-lock
         where estabelecimento.cod_estab = p_cod_estab
         use-index stblcmnt_id no-error.
    if  avail estabelecimento
    then do:
       find pais no-lock
            where pais.cod_pais = estabelecimento.cod_pais
             no-error.
       assign p_cod_finalid_econ = pais.cod_finalid_econ_pais.
    end.
END PROCEDURE. /*  */
