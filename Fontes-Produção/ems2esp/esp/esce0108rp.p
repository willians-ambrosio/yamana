/*
Programa    : ESCE0108RP.P 
Descricao   : Importaá∆o de Dados do CISP
Programador : Edilaine Cristine Barboza - Datasul SP (GWA)
Data        : 09/03/2006
*/

{include/i-prgvrs.i ESCE0108RP 2.06.00.000}

{cdp/cd1234.i}       /* Decimais - Chile */

def buffer bb-item              for item.
def temp-table tt-item-2     like item use-index codigo.


DEFINE TEMP-TABLE tt-er-item NO-UNDO
    FIELD it-codigo LIKE ITEM.it-codigo
    FIELD mens      AS CHAR
    INDEX xx-er-item it-codigo.
    
define temp-table tt-param no-undo
    field destino           as integer
    field arquivo           as char format "x(35)"
    field usuario           as char format "x(12)"
    field data-exec         as date
    field hora-exec         as integer
    field c-it-codigo-ini   LIKE item.it-codigo 
    field c-it-codigo-fim   LIKE item.it-codigo
    field tipo              as integer
    field medio-mat         as dec decimals 4 extent 3 format ">>>,>>>,>>9.9999"
    field medio-mob         as dec decimals 4 extent 3 format ">>>,>>>,>>9.9999"
    field medio-ggf         as dec decimals 4 extent 3 format ">>>,>>>,>>9.9999"
    field r-conta           as rowid
    field da-inipa-x        like movto-estoq.dt-trans
    field depos-pad         like movto-estoq.cod-depos
    field serie1            like movto-estoq.serie-docto
    field docto1            like movto-estoq.nro-docto
    field it-codigo         like item.it-codigo
    field parametro         as char format "x(30)"
    FIELD ct-conta     AS CHAR
    FIELD sc-conta     AS CHAR.

def temp-table tt-param1 no-undo
    field tipo          as integer 
    field medio-mat     as dec decimals 4 extent 3 format ">>>,>>>,>>9.9999"
    field medio-mob     as dec decimals 4 extent 3 format ">>>,>>>,>>9.9999"
    field medio-ggf     as dec decimals 4 extent 3 format ">>>,>>>,>>9.9999"
    field r-conta       as rowid 
    field da-inipa-x    like movto-estoq.dt-trans
    field depos-pad     like movto-estoq.cod-depos
    field serie1        like movto-estoq.serie-docto
    field docto1        like movto-estoq.nro-docto
    field it-codigo     like item.it-codigo
    field parametro     as char format "x(30)".

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita    AS raw.

FOR EACH tt-er-item.
    DELETE tt-er-item.
END.

DEFINE INPUT PARAMETER raw-param AS raw NO-UNDO.
DEFINE INPUT PARAMETER table FOR tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param NO-ERROR.

FOR EACH tt-raw-digita NO-LOCK:
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO  tt-digita.
END.

/*88888888888888888888888888888888888888888888888888888888888888888888888888888*/
def var i-docto                     AS INT INITIAL 0         NO-UNDO.
def var i-mo-fasb                   as integer               no-undo.
def var i-mo-cmi                    as integer               no-undo.
def var unid-monet                  as int extent 3          no-undo.


def var l-nota-ft                   as logical no-undo.
DEF VAR l-OK                        AS LOGICAL INITIAL YES  NO-UNDO.

def var i-empresa                   like param-global.empresa-prin  no-undo.
def var c-depos-pad                 like item.deposito-pad.
def var da-ult-fech-dia             like param-estoq.ult-fech-dia   no-undo.
DEF VAR n-preco-unit                LIKE ordem-compra.preco-fornec  NO-UNDO.
/*888888888888888888888888888888888888888888888888888888888888888888888888888888888*/

DEF VAR h-acomp            AS HANDLE        NO-UNDO.

{include/i-rpvar.i}

run utp/ut-acomp.p persistent set h-acomp.

{utp/ut-liter.i Imprimindo *}

run pi-inicializar in h-acomp (input RETURN-VALUE).

FIND FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = tt-param.c-it-codigo-ini NO-ERROR.
DISABLE TRIGGERS FOR LOAD OF ITEM.
RUN pi-alter-tipo.

{include/i-rpclo.i}

run pi-finalizar in h-acomp.

PROCEDURE pi-alter-tipo:

    find first param-global no-lock no-error.
    IF AVAIL param-global THEN
        assign i-empresa = param-global.empresa-prin.

    find first param-estoq  no-lock no-error. 

    assign unid-monet[1] = 0.
    if param-estoq.tem-moeda1 = yes then
       assign unid-monet[2] = param-estoq.moeda1.
    if param-estoq.tem-moeda2 = yes then
       assign unid-monet[3] = param-estoq.moeda2.

    find first tt-param no-lock no-error.

    FOR EACH tt-er-item. DELETE tt-er-item. END.

    FOR EACH bb-item NO-LOCK
        WHERE bb-item.it-codigo >= tt-param.c-it-codigo-ini
        AND   bb-item.it-codigo <= tt-param.c-it-codigo-fim
        AND   bb-item.tipo-contr = 1:

        RUN pi-acompanhar IN h-acomp (('Item...' + bb-item.it-codigo)).
        ASSIGN l-ok      = YES
               l-nota-ft = YES.

        RUN check-dados IN THIS-PROCEDURE.
    END.
    OUTPUT TO "D:\temp\edilson.mendes\Yamana\LOG\CE0108.TXT".
    FOR EACH tt-er-item NO-LOCK:
        PUT tt-er-item.it-codigo " "
            tt-er-item.mens FORMAT "x(300)" SKIP.
    END.
    OUTPUT CLOSE.
    MESSAGE "Arquivo de LOG esta no diretorio: " SKIP (2)
            "D:\temp\edilson.mendes\Yamana\LOG\CE0108.TXT"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

PROCEDURE check-dados:
    FIND FIRST ITEM EXCLUSIVE-LOCK WHERE ITEM.it-codigo = bb-item.it-codigo NO-ERROR.
    IF AVAIL ITEM THEN DO:
        ASSIGN n-preco-unit = 0.
        FIND LAST  movto-estoq NO-LOCK
             WHERE movto-estoq.esp-docto = 21
             AND   movto-estoq.it-codigo = item.it-codigo
             USE-INDEX esp-data NO-ERROR.
        IF AVAIL movto-estoq THEN DO:
            FIND FIRST item-doc-est NO-LOCK
                 WHERE item-doc-est.serie-docto  = movto-estoq.serie-docto 
                 AND   item-doc-est.nro-docto    = movto-estoq.nro-docto   
                 AND   item-doc-est.cod-emitente = movto-estoq.cod-emitente
                 AND   item-doc-est.nat-operacao = movto-estoq.nat-operacao
                 AND   item-doc-est.sequencia    = movto-estoq.seq NO-ERROR.   
            IF NOT AVAIL item-doc-est THEN DO:
                FIND LAST  ordem-compra NO-LOCK
                     WHERE ordem-compra.it-codigo = item.it-codigo
                     AND   (ordem-compra.situacao  = 2
                            OR ordem-compra.situacao  = 6) NO-ERROR.
                IF NOT AVAIL ordem-compra THEN DO:
                    FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                    CREATE tt-er-item.
                    ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                           tt-er-item.mens      = "Item sem preáo".
                    ASSIGN l-ok = NO.
                END.
                ELSE ASSIGN n-preco-unit = ordem-compra.preco-fornec.
            END.
            ELSE ASSIGN n-preco-unit = item-doc-est.preco-unit[1]. 
        END.
        ELSE DO: 
            FIND LAST  ordem-compra NO-LOCK
                 WHERE ordem-compra.it-codigo = item.it-codigo
                 AND   (ordem-compra.situacao  = 2
                        OR ordem-compra.situacao  = 6) NO-ERROR.
            IF NOT AVAIL ordem-compra THEN DO:
                FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                CREATE tt-er-item.
                ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                       tt-er-item.mens      = "Item sem preáo".
                ASSIGN l-ok = NO.
            END.
            ELSE ASSIGN n-preco-unit = ordem-compra.preco-fornec.
        END.

        IF l-ok = NO THEN DO:
            ASSIGN l-ok         = YES
                   n-preco-unit = 0.0001.
        END.
    
        IF l-ok THEN DO:
    
            ASSIGN i-docto = i-docto + 1.
    
            find first tt-param no-error.
            if not avail tt-param then
                create tt-param.
            assign l-rejeita-dec         = no.    
            assign tt-param.medio-mat[1] = fn_vld_ajust_dec(n-preco-unit,0).
            assign tt-param.medio-mat[2] = fn_vld_ajust_dec(n-preco-unit,param-estoq.moeda1).
            assign tt-param.medio-mat[3] = fn_vld_ajust_dec(n-preco-unit,param-estoq.moeda2).
            assign tt-param.depos-pad    = "ALM" 
                   tt-param.serie1       = ""
                   tt-param.docto1       = string(i-docto)
                   tt-param.parametro    = "2" . 

            if l-rejeita-dec then do:
                /*Foram informadas mais casas decimais em algum valor informado do que o parametrizado para a moeda.*/
                FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                CREATE tt-er-item.
                ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                       tt-er-item.mens      = "Foram informadas mais casas decimais em algum valor informado do que o parametrizado para a moeda".
                ASSIGN l-ok = NO.
            end.

            for each tt-item-2: delete tt-item-2. end.

            create tt-item-2.
            buffer-copy item to tt-item-2.

            /* Code placed here will execute PRIOR to standard behavior. */
            /*Cadastre os ParÉmetros Globais antes de executar este programa*/
            find first param-global no-lock no-error.
            if not avail param-global then do:
                FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                CREATE tt-er-item.
                ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                       tt-er-item.mens      = "Cadastre os ParÉmetros Globais antes de executar este programa".
                ASSIGN l-ok = NO.
            end.

             /*Os ParÉmetros de Estoque devem ser cadastrados.*/
            find first param-estoq no-lock no-error.
            if not avail param-estoq then do:
                FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                CREATE tt-er-item.
                ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                       tt-er-item.mens      = "Os ParÉmetros de Estoque devem ser cadastrados".
                ASSIGN l-ok = NO.
            end.

            if item.tipo-contr = 2 then do:
              /*O tipo de controle selecionado para a mudanáa Ç igual ao tipo de controle atual. Selecione outro tipo de controle para mudanáa.*/
                FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                CREATE tt-er-item.
                ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                       tt-er-item.mens      = "O tipo de controle selecionado para a mudanáa Ç igual ao tipo de controle atual. Selecione outro tipo de controle para mudanáa".
                ASSIGN l-ok = NO.
            end.
            
            find param-fasb where param-fasb.ep-codigo = i-empresa no-lock no-error .
            
            assign i-mo-fasb = if avail param-fasb and param-fasb.moeda-fasb <> 0 then 
                               if param-fasb.moeda-fasb = param-estoq.moeda1  then 2
                                 else if param-fasb.moeda-fasb = param-estoq.moeda2 then 3
                                      else 0
                               else 0
                   i-mo-cmi  = if avail param-fasb and param-fasb.moeda-cmi <> 0 then
                               if param-fasb.moeda-cmi = param-estoq.moeda1 then 2
                                 else if param-fasb.moeda-cmi = param-estoq.moeda2
                                        then 3
                                        else 0
                               else 0. 
            
            for each item-estab use-index item
                where item-estab.it-codigo = item.it-codigo no-lock:
                &if defined (bf_mat_fech_estab) &then
                    if param-estoq.tp-fech = 2 then do:
                        find estab-mat 
                            where estab-mat.cod-estabel = item-estab.cod-estabel
                            no-lock no-error.
                        if avail estab-mat then
                            assign da-ult-fech-dia = estab-mat.ult-fech-dia.
                        end.
                        else
                            assign da-ult-fech-dia = param-estoq.ult-fech-dia.
                &else
                    assign da-ult-fech-dia = param-estoq.ult-fech-dia.
                &endif

                find first movto-estoq use-index item-data 
                     where movto-estoq.it-codigo = item.it-codigo  
                       and movto-estoq.dt-trans  > da-ult-fech-dia 
                       and movto-estoq.esp-docto = 25  no-lock no-error.
                /*Item nao pode ser modificado porque possui transacao REF*/
                if avail movto-estoq then do:
                    FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                    CREATE tt-er-item.
                    ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                           tt-er-item.mens      = "Item nao pode ser modificado porque possui transacao REF".
                    ASSIGN l-ok = NO.
                end.

                if item-estab.sald-ini-mat-m[1] < 0 or
                   item-estab.sald-ini-mat-m[2] < 0 or
                   item-estab.sald-ini-mat-m[3] < 0 or
                   item-estab.sald-ini-mob-m[1] < 0 or
                   item-estab.sald-ini-mob-m[2] < 0 or
                   item-estab.sald-ini-mob-m[3] < 0 then do:
                    /*Item com saldo inicial negativo*/
                    FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                    CREATE tt-er-item.
                    ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                           tt-er-item.mens      = "Item com saldo inicial negativo".
                    ASSIGN l-ok = NO.
                end.
            end.

            for each saldo-estoq use-index item
                where saldo-estoq.it-codigo = item.it-codigo no-lock:
                if saldo-estoq.qtidade-ini < 0 then do:
                    /*Item com saldo inicial negativo*/
                    FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                    CREATE tt-er-item.
                    ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                           tt-er-item.mens      = "Item com saldo inicial negativo".
                    ASSIGN l-ok = NO.
                end.
            end.

            {utp/ut-liter.i Verificando_Notas_n∆o_Confirmadas_no_Faturamento... mce R}
            status input return-value.

            /* Verifica Notas nao Confirmadas no Faturamento */
            if param-global.modulo-ft then do:
                assign l-nota-ft = no.
                for each it-nota-fisc
                   where it-nota-fisc.it-codigo = item.it-codigo
                     and it-nota-fisc.dt-confirma = ? no-lock :
                    find nota-fiscal where nota-fiscal.cod-estabel = it-nota-fisc.cod-estabel and
                                           nota-fiscal.serie       = it-nota-fisc.serie       and
                                           nota-fiscal.nr-nota-fis = it-nota-fisc.nr-nota-fis no-lock no-error.
                    if avail nota-fiscal then 
                        if nota-fiscal.dt-cancela <> ? then next.
                    else do:
                        assign l-nota-ft = yes.
                        leave.
                    end.
                end.

                if l-nota-ft = no then 
                    /*Existem notas fiscais ou embarques pendentes no faturamento*/
                    for each pre-fatur where pre-fatur.cod-sit-pre <> 3 no-lock:
                        for each it-pre-fat where it-pre-fat.cdd-embarq = pre-fatur.cdd-embarq and
                                                  it-pre-fat.nome-abrev  = pre-fatur.nome-abrev  and
                                                  it-pre-fat.nr-pedcli   = pre-fatur.nr-pedcli   and
                                                  it-pre-fat.it-codigo   = item.it-codigo        no-lock:
                            assign l-nota-ft = yes.
                            leave.
                        end.
                    end.

                if l-nota-ft then do:
                    FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                    CREATE tt-er-item.
                    ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                           tt-er-item.mens      = "Existem notas fiscais ou embarques pendentes no faturamento".
                    ASSIGN l-ok = NO.
                end.
            end.
            
            /* Se tiver alguma transacao NFT com valores zerados */
            find first movto-estoq  where movto-estoq.it-codigo      = item.it-codigo and
                                          movto-estoq.dt-trans      >= da-inipa-x     and
                                          movto-estoq.esp-docto      = 23             and
                                          movto-estoq.valor-mat-m[1] = 0              and
                                          movto-estoq.valor-mob-m[1] = 0              and
                                          movto-estoq.valor-ggf-m[1] = 0 no-lock no-error.
            if avail movto-estoq then do:
                /*O item possui Notas Fiscais de Transferància a serem valorizadas*/
                FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                CREATE tt-er-item.
                ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                       tt-er-item.mens      = "O item possui Notas Fiscais de Transferància a serem valorizadas".
                ASSIGN l-ok = NO.
            end.  
            
            find cta_ctbl_integr where cta_ctbl_integr.cod_modul_dtsul = "CEP"
                                   and cta_ctbl_integr.cod_cta_ctbl = "17202014"
                                   AND cta_ctbl_integr.dat_inic_valid <= TODAY
                                   AND cta_ctbl_integr.dat_fim_valid  >= TODAY no-lock no-error.
            if not avail cta_ctbl_integr then do:
                /*A conta informada n∆o consta no plano de contas desta empresa.*/
                FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                CREATE tt-er-item.
                ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                       tt-er-item.mens      = "A conta informada n∆o consta no plano de contas desta empresa".
                ASSIGN l-ok = NO.
            end.    
            
            if cta_ctbl_integr.ind_finalid_ctbl <> "Consumo" then do:
                /*Conta n∆o pode ser do Tipo T°tulo.*/
                FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                CREATE tt-er-item.
                ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                       tt-er-item.mens      = "Conta n∆o pode ser do Tipo T°tulo".
                ASSIGN l-ok = NO.
            end.

            assign tt-param.r-conta     = rowid(cta_ctbl_integr)
                   item.conta-aplicacao = "17202014"
                   tt-param.it-codigo   = item.it-codigo.

            find deposito where deposito.cod-depos = "ALM" no-lock no-error.
            if not avail deposito then do:
                /*Deposito nao cadastrado ALM*/
                FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                CREATE tt-er-item.
                ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                       tt-er-item.mens      = "Deposito nao cadastrado ALM".
                ASSIGN l-ok = NO.
            end.

            if deposito.ind-dep-cq = yes and item.contr-qualid then do:
                /*Movimentaá∆o n∆o pode ser feita com o dep¢sito  CQ.*/
                FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                CREATE tt-er-item.
                ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                       tt-er-item.mens      = "Movimentaá∆o n∆o pode ser feita com o dep¢sito  CQ".
                ASSIGN l-ok = NO.
            end.   

            if deposito.ind-dep-rej = yes and item.contr-qualid then do:
                /*Movimentaá∆o n∆o pode ser feita com o dep¢sito de rejeiá∆o de CQ.*/
                FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                CREATE tt-er-item.
                ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                       tt-er-item.mens      = "Movimentaá∆o n∆o pode ser feita com o dep¢sito de rejeiá∆o de CQ".
                ASSIGN l-ok = NO.
            end.   

            for each item-estab where item-estab.it-codigo = item.it-codigo no-lock:
                find estabelec where estabelec.cod-estabel = item-estab.cod-estabel no-lock no-error.
                if not avail estabelec then 
                    next.
                /*Movimentaá∆o n∆o pode ser feita com o dep¢sito  CQ.*/
                if estabelec.deposito-cq = c-depos-pad and item.contr-qualid then do:
                    FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                    CREATE tt-er-item.
                    ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                           tt-er-item.mens      = "Movimentaá∆o n∆o pode ser feita com o dep¢sito  CQ".
                    ASSIGN l-ok = NO.
                end.    
                /*Movimentaá∆o n∆o pode ser feita com o dep¢sito de rejeiá∆o de CQ.*/
                if estabelec.dep-rej-cq = c-depos-pad and item.contr-qualid then do:
                    FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                    CREATE tt-er-item.
                    ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                           tt-er-item.mens      = "Movimentaá∆o n∆o pode ser feita com o dep¢sito de rejeiá∆o de CQ".
                    ASSIGN l-ok = NO.
                end.
            end. 

            if  item.tipo-contr = 1 or
                item.tipo-contr = 3 or
                item.tipo-contr = 4 then do:
                if item.tipo-contr = 3 /*and {ininc/i09in122.i 06 tt-param.parametro } = 2edix*/  then do:
                    {utp/ut-liter.i Assumir†_o_valor_informado_como_mÇdio,_caso_o_mÇdio_calculado_for_igual_a_0 mce R}
                    status input return-value.
                end.

            /************************************************/
                if tt-param.medio-mat[1] = 0 and
                   tt-param.medio-mob[1] = 0 and
                   tt-param.medio-ggf[1] = 0 then do:
                    /*valor deve ser diferente de zero medio-mat mob ggf*/
                    FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                    CREATE tt-er-item.
                    ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                           tt-er-item.mens      = "valor deve ser diferente de zero medio-mat mob ggf".
                    ASSIGN l-ok = NO.
                end.    
                if item.tipo-contr = 3 /*and {ininc/i09in122.i 06 tt-param.parametro} = 2 */  THEN
                 status input " ".
            end.

            if item.tipo-contr = 1 /*and {ininc/i09in122.i 06 tt-param.parametro} = 2 */  then 
                find first movto-estoq
                     where movto-estoq.it-codigo = item.it-codigo  and
                           movto-estoq.dt-trans  < da-ult-fech-dia and
                           movto-estoq.esp-docto = 29 /*"RFS"*/ no-lock no-error.

            if  item.tipo-contr = 4 /*and {ininc/i09in122.i 06 tt-param.parametro} = 2 or
                item.tipo-contr = 4 and {ininc/i09in122.i 06 tt-param.parametro} = 1 */ then 
                find first movto-estoq
                     where movto-estoq.it-codigo = item.it-codigo  and
                           movto-estoq.dt-trans  < da-ult-fech-dia and
                           movto-estoq.esp-docto = 27 /*"RDD"*/ no-lock no-error.

            if item.tipo-contr = 3 /*and {ininc/i09in122.i 06 tt-param.parametro} = 2 */ then 
                find first movto-estoq
                     where movto-estoq.it-codigo = item.it-codigo  and
                           movto-estoq.dt-trans  < da-ult-fech-dia and
                           movto-estoq.esp-docto = 26 /*"RCS"*/ no-lock no-error.
            if avail movto-estoq then do:
                run utp/ut-msgs.p (input "show":U, 
                                   input 3264, 
                                   input "").
                if return-value = "no" then
                  ASSIGN l-ok = NO. 
            end.

            FOR EACH tt-param1:
                DELETE tt-param1.
            END.
    
            CREATE  tt-param1.
            assign  tt-param1.medio-mat[1] = tt-param.medio-mat[1]
                    tt-param1.medio-mat[2] = tt-param.medio-mat[2]
                    tt-param1.medio-mat[3] = tt-param.medio-mat[3]
                    tt-param1.depos-pad    = tt-param.depos-pad   
                    tt-param1.serie1       = tt-param.serie1      
                    tt-param1.docto1       = tt-param.docto1      
                    tt-param1.parametro    = tt-param.parametro   
                    tt-param1.r-conta      = tt-param.r-conta     
                    tt-param1.it-codigo    = tt-param.it-codigo.   

            IF l-ok THEN DO:
                run esp/esce0108a.p (input table tt-param1).
                if return-value = "NOK" THEN DO:
                    FIND FIRST tt-er-item EXCLUSIVE-LOCK NO-ERROR.
                    CREATE tt-er-item.
                    ASSIGN tt-er-item.it-codigo = ITEM.it-codigo
                           tt-er-item.mens      = "Erro na API".
                    ASSIGN l-ok = NO.
                END.
                ELSE DO:
/*                     assign tt-item-2.tipo-contr = 2.  */
/*                     buffer-copy tt-item-2 using tipo-contr to item.  */
/*                     RELEASE ITEM.  */
                END.
            END.
    
            IF l-ok = NO THEN  ASSIGN i-docto = i-docto - 1.
    
            END.
        END.

END.

