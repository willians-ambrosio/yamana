/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i CC0323RP 2.00.00.013}  /*** 010013 ***/
/*****************************************************************************
**
**     Programa: CC0323RP.P
**
**     Data....: MAIO DE 1997
**
**     Autor...: DATASUL S.A.
**
**     Objetivo: EMISS√O DE FICHA PARA COTA«√O POR PROCESSO DE COMPRAS
**
**     Versao..: 1.00.000 - Sandra Stadelhofer
**
*****************************************************************************/
{cdp/cdcfgmat.i}
{cdp/cd0669.i} /*Definiá∆o da tt-erro*/

define temp-table tt-param
    field destino        as integer
    field arquivo        as char
    field usuario        as char format 'x(12)'
    field data-exec      as date
    field hora-exec      as integer
    field i-processo-i   as integer
    field i-processo-f   as integer
    field i-ordem-i      as integer
    field i-ordem-f      as integer
    field c-item-i       as char
    field c-item-f       as char
    field c-comp-i       as char
    field c-comp-f       as char
    field c-destino      as char format 'x(40)'
    field l-eprocurement as logical format "Sim/N∆o"
    field l-automatico   as logical
    field diretorio      as char
    field l-envio        as logical .

def temp-table tt-raw-digita
    field raw-digita as raw.

def temp-table tt-ficha
    field processo     like proc-compra.nr-processo
    field comprador    AS CHAR FORMAT "x(40)"
    FIELD tel-umater   AS CHAR FORMAT "x(15)"
    FIELD fax-umater   AS CHAR FORMAT "x(15)"
    FIELD e-mail       LIKE usuar-mater.e-mail
    FIELD parcela      LIKE prazo-compra.parcela
    FIELD un           LIKE prazo-compra.un
    field fornecedor   like cotacao-item.cod-emitente
    field ordem        like cotacao-item.numero-ordem
    field item         like cotacao-item.it-codigo
    FIELD descricao    LIKE ITEM.descricao-1
    field quantidade   as decimal format '>>,>>>,>>9.9999'
    field entrega      as char    format 'x(10)'
    FIELD narrativa    LIKE ITEM.narrativa
    FIELD narrat-proc  LIKE proc-compra.narrativa
    index id-ficha is primary processo   ascending
                              fornecedor ascending
                              ordem      ascending.

/*------------ Envio e-mail -------------*/
{utp/utapi009.i}
def var l-first-ordem as log init no.
def var c-arq            as char no-undo.
def var c-assunto-e-mail as char no-undo.
def var c-mensagem-e-mail as char format "x(38)" no-undo.
def var c-lb-email as char no-undo.
def var c-email    as char no-undo.

def var c-url-wcc0303a     as char no-undo.
def stream s-email .

{utp/ut-liter.i Emiss„o_Ficha_CotaÁ„o_Processo * L}
assign c-assunto-e-mail = return-value.
{utp/ut-liter.i Arquivo_Anexo_Para_CotaÁ„o_do_Processo * L}
assign c-mensagem-e-mail = trim(return-value) + ":".

/*---------------------------------------*/

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

def var de-qtde    as decimal format '>>,>>>,>>9.9999' init 0.
def var i-cont     as integer format 99.
def var c-entrega  as char    format 'x(10)'.
def var c-traco    as char    format 'x(132)'.
def var h-acomp    as handle  no-undo.
DEFINE VARIABLE i-conta          AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-conta-aux      AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-conta-aux2     AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-narrativa      LIKE item.narrativa NO-UNDO.
DEFINE VARIABLE c-nar-aux        LIKE item.narrativa NO-UNDO.


/*Variaveis para o bloqueio do fornecedor na 2.05*/
&IF defined(bf_mat_bloqueio_fornec) &THEN
    DEFINE VARIABLE h-api029 AS HANDLE       NO-UNDO.
    DEFINE VARIABLE i-situacao AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dt-vig-ini AS DATE       NO-UNDO.
    DEFINE VARIABLE dt-vig-fim AS DATE       NO-UNDO.
&ENDIF

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input 'Emiss∆o de Ficha para Cotaá∆o por Processo de Compra').

{ccp/cc0323.i}
{ccp/cc0327.i} /* eProcurement */

{include/i-rpvar.i}

assign c-programa = 'CC0323'
       c-versao   = '1.00'
       c-revisao  = '000'
       c-traco    = fill('-', 132).

{utp/ut-liter.i Emiss∆o_de_Ficha_para_Cotaá∆o_por_Processo * r}
assign c-titulo-relat = trim(return-value).
{utp/ut-liter.i COMPRAS * r}
assign c-sistema = trim(return-value).

create tt-param.
raw-transfer raw-param to tt-param.

/* e procurement*/ 
for each tt-transfer:
    delete tt-transfer.
end.
for each tt-raw-digita.
    create tt-transfer.
    raw-transfer tt-raw-digita.raw-digita to tt-transfer.
end.

find first param-global no-lock no-error.
find first param-compra no-lock no-error.
find first param-estoq  no-lock no-error.


{utp/ut-liter.i Email * L}
assign c-lb-email = return-value.
if tt-param.l-envio then do:
   {utp/ut-liter.i Sim * L}
   assign c-email = return-value.
end.
else do:
   {utp/ut-liter.i N∆o * L}
   assign c-email = return-value.
end.


{include/i-rpcab.i}

{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.

/* &IF defined(bf_mat_bloqueio_fornec) &THEN       */
/*     run cdp/cdapi029.p persistent set h-api029. */
/* &ENDIF                                          */

if  tt-param.l-eprocurement then
    run ccp/cc0302h.p(input table tt-transfer,true, tt-param.l-automatico).
else do:
    /* E-mail */
    if  tt-param.l-envio then
        run pi-email.
    else do:
        /* Nao depende mais das cotacoes pendentes.
           Passa a considerar apenas os fornecedores
           ligados ao processo */
        for each ordem-compra 
            where ordem-compra.numero-ordem >= tt-param.i-ordem-i
            and   ordem-compra.numero-ordem <= tt-param.i-ordem-f
            and   ordem-compra.it-codigo    >= tt-param.c-item-i
            and   ordem-compra.it-codigo    <= tt-param.c-item-f
            and   ordem-compra.cod-comprado >= tt-param.c-comp-i
            and   ordem-compra.cod-comprado <= tt-param.c-comp-f
            and   ordem-compra.nr-processo  >= tt-param.i-processo-i
            and   ordem-compra.nr-processo  <= tt-param.i-processo-f no-lock:

            if  ordem-compra.impr-ficha  = no 
            or  ordem-compra.nr-processo = 0 then next.
            
            /* nao confirmada e em cotacao */
            if  ordem-compra.situacao <> 1 and
                ordem-compra.situacao <> 5 then next.
            
            assign de-qtde = 0
                   i-cont  = 0.
            for each prazo-compra 
                where prazo-compra.numero-ordem = ordem-compra.numero-ordem no-lock:  
                assign i-cont = i-cont + 1.
                if  i-cont = 1 then
                    assign c-entrega = string(prazo-compra.data-entrega).
                assign de-qtde = de-qtde + prazo-compra.quantidade.   
            end.
            if  i-cont > 1 then do:
                {utp/ut-liter.i Parcelada * r}
                assign c-entrega = trim(return-value).
            end.
    
            find proc-compra where proc-compra.nr-processo = ordem-compra.nr-processo
                no-lock no-error.

            for each proc-fornec where
                proc-fornec.nr-processo  = ordem-compra.nr-processo
                no-lock:
                
                run pi-acompanhar in h-acomp (input string(ordem-compra.nr-processo)).
                
                run pi-altera-ordem (input rowid(ordem-compra)).

                &IF defined(bf_mat_bloqueio_fornec) &THEN
                    run cdp/cdapi029(input ordem-compra.cod-comprado,
                                     input 1,
                                     input today,
                                     input proc-fornec.cod-emitente,
                                     output i-situacao,
                                     output dt-vig-ini,
                                     output dt-vig-fim,
                                     output table tt-erro).
                    if return-value = "NOK":u then next.
                &ENDIF

                FIND FIRST usuar-mater NO-LOCK
                     WHERE usuar-mater.cod-usuario = proc-compra.cod-comprado
                     NO-ERROR.
                FIND FIRST prazo-compra NO-LOCK
                     WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem 
                     NO-ERROR.
                FIND FIRST ITEM NO-LOCK
                     WHERE ITEM.it-codigo = ordem-compra.it-codigo
                     NO-ERROR.

                create tt-ficha.
                assign tt-ficha.processo    = ordem-compra.nr-processo
                       tt-ficha.fornecedor  = proc-fornec.cod-emitente
                       tt-ficha.ordem       = ordem-compra.numero-ordem
                       tt-ficha.item        = ordem-compra.it-codigo
                       tt-ficha.descricao   = ITEM.descricao-1
                       tt-ficha.quantidade  = de-qtde
                       tt-ficha.entrega     = c-entrega
                       tt-ficha.parcela     = prazo-compra.parcela
                       tt-ficha.un          = prazo-compra.un
                       tt-ficha.narrat-proc = IF AVAIL proc-compra THEN proc-compra.narrativa ELSE "".

                IF ITEM.tipo-contr = 4 THEN /* Debito Direto */
                   ASSIGN tt-ficha.narrativa = ordem-compra.narrativa.
                ELSE
                   ASSIGN tt-ficha.narrativa = item.narrativa.

                IF AVAIL usuar-mater THEN
                   ASSIGN tt-ficha.comprador     = usuar-mater.nome-usuar
                          tt-ficha.tel-umater    = usuar-mater.telefone[1]
                          tt-ficha.fax-umater    = usuar-mater.telefax
                          tt-ficha.e-mail        = usuar-mater.e-mail.
            END.
        end.
            

        for each tt-ficha use-index id-ficha
            break by processo 
                  by fornecedor:
            
            if  first-of(processo) or first-of(fornecedor) then do:
                PAGE.
                assign i-cont = 0.
                disp tt-ficha.processo
                     tt-ficha.comprador 
                     tt-ficha.tel-umater   
                     tt-ficha.fax-umater
                     tt-ficha.e-mail     with frame f-processo.

                find first emitente
                    where emitente.cod-emitente = tt-ficha.fornecedor
                    no-lock no-error.

                disp tt-ficha.fornecedor
                     emitente.nome-abrev
                     emitente.telefone[1]
                     emitente.telefax    
                     c-lb-dt-cot
                     c-traco with frame f-fornec.
                
                disp c-lb-ordem
                     c-lb-item
                     c-lb-qtde
                     c-lb-preco
                     c-lb-per-ipi with frame f-cab1.
            END.

            IF LINE-COUNTER < 10 THEN 
               disp c-lb-ordem
                    c-lb-item
                    c-lb-qtde
                    c-lb-preco
                    c-lb-per-ipi with frame f-cab1.

            disp tt-ficha.ordem
                 tt-ficha.item
                 tt-ficha.quantidade
                 /*tt-ficha.parcela*/
                 tt-ficha.un
                 tt-ficha.descricao with frame f-ordem.
            
            down with frame f-ordem.
            assign i-cont = i-cont + 1.

            IF tt-ficha.narrativa <> "" THEN
               put unformatted
                   fill("-",29) " Narrativa " fill("-",29) SKIP.

            ASSIGN i-conta-aux = 0.
            DO i-conta = 1 TO 2000:
              IF SUBSTRING(tt-ficha.narrativa,i-conta,1) = CHR(10) THEN DO:
                 PUT SKIP.
                 ASSIGN i-conta-aux = 0.
                 NEXT.
              END.
              IF SUBSTRING(tt-ficha.narrativa,i-conta,4) = " " THEN
                 LEAVE.
              PUT SUBSTRING(tt-ficha.narrativa,i-conta,1) FORMAT "X(01)".

              ASSIGN i-conta-aux = i-conta-aux + 1.

              IF i-conta-aux = 69 THEN DO:
                 ASSIGN i-conta-aux = 0.
                 PUT SKIP.
              END.
            END.

            IF tt-ficha.narrativa <> "" THEN
               PUT SKIP(2).

            /*Fim impress∆o da descriá∆o do Item e Narrativa da Ordem*/
            if  last-of(tt-ficha.processo)
            or  last-of(tt-ficha.fornecedor) then do:

                IF LAST-OF(tt-ficha.fornecedor) THEN DO:
                   IF tt-ficha.narrat-proc <> " " THEN DO:
                      PUT UNFORMATTED FILL("-",132) SKIP
                          "Observacoes" AT 55 SKIP
                          FILL("-" ,132) SKIP. 
  
                      ASSIGN i-conta-aux = 0.
                      DO i-conta = 1 TO 2000:
                         IF SUBSTRING(tt-ficha.narrat-proc,i-conta,1) = CHR(10) THEN DO:
                            PUT SKIP.
                            ASSIGN i-conta-aux = 0.
                            NEXT.
                         END.
                         IF SUBSTRING(tt-ficha.narrat-proc,i-conta,4) = " " THEN
                            LEAVE.
                         PUT SUBSTRING(tt-ficha.narrat-proc,i-conta,1) FORMAT "X(01)".

                         ASSIGN i-conta-aux = i-conta-aux + 1.
                         IF i-conta-aux = 132 THEN DO:
                            ASSIGN i-conta-aux = 0.
                            PUT SKIP.
                         END.
                      END.
                      PUT SKIP(2).
                   END.


                   for each tt-editor:
                      if  tt-editor.linha = 1 then
                          put unformatted
                              fill("-",29) " " c-lb-ordem " " fill("-",29).
                   end.

                   disp c-lb-dados with frame f-cab2.
                   disp c-lb-ipi      
                        c-lb-inc      
                        c-lb-aliq-icms
                        c-lb-aliq-iss 
                        c-lb-valid    
                        /*c-lb-per-desc*/
                        c-lb-prazo    
                        c-lb-contato  
                        c-lb-cpg      
                        c-lb-ninc     
                        c-lb-emdia
                        c-traco with frame f-det-fornec.   
                END.
            end.
        end.
    end.
end.
PAGE.
put unformatted
    c-lb-param             skip(1)
    "Portal Datasul"       at 5 ': ' if tt-param.l-eprocurement then "Sim" else "N∆o" 
    c-lb-email             at 5 ': ' c-email skip(1)
    c-lb-tit-sel           skip(1)
    c-lb-processo          at 5 ':'
    tt-param.i-processo-i  at 16 "|<  >| " at 33 tt-param.i-processo-f
    c-lb-ordem             at 5  ':'
    tt-param.i-ordem-i     at 16 "|<  >| " at 33 tt-param.i-ordem-f
    c-lb-item              at 5  ':'
    tt-param.c-item-i      at 16 "|<  >| " at 33 tt-param.c-item-f
    c-lb-comprador         at 5  ':'
    tt-param.c-comp-i      at 16 "|<  >| " at 33 tt-param.c-comp-f skip(1)
    c-lb-tit-imp           skip(1)
    c-lb-destino           at 5  ': ' tt-param.c-destino ' - ' tt-param.arquivo
    c-lb-usuario           at 5  ': ' tt-param.usuario.

if  can-find (first tt-erros) then do:
    put skip (3) " Erros durante envio do e-mail ===================" .
    put skip "".
    for each tt-erros:

    end.
end.

{include/i-rpclo.i}

run pi-finalizar in h-acomp.

/* -------------------------------------------------------------- */

PROCEDURE pi-email:
    
    assign c-arq = tt-param.diretorio.
    
    for each tt-erros:
        delete tt-erros.
    end.
    
    for each tt-ficha:
        delete tt-ficha.
    end.
    
    /* Nao depende mais das cotacoes pendentes.
       Passa a considerar apenas os fornecedores
       ligados ao processo */
    for each ordem-compra 
        where ordem-compra.numero-ordem >= tt-param.i-ordem-i
        and   ordem-compra.numero-ordem <= tt-param.i-ordem-f
        and   ordem-compra.it-codigo    >= tt-param.c-item-i
        and   ordem-compra.it-codigo    <= tt-param.c-item-f
        and   ordem-compra.cod-comprado >= tt-param.c-comp-i
        and   ordem-compra.cod-comprado <= tt-param.c-comp-f
        and   ordem-compra.nr-processo  >= tt-param.i-processo-i
        and   ordem-compra.nr-processo  <= tt-param.i-processo-f no-lock:
        
        if  ordem-compra.impr-ficha  = no 
        or  ordem-compra.nr-processo = 0 then next.
        
        /* nao confirmada e em cotacao */
        if  ordem-compra.situacao <> 1 and
            ordem-compra.situacao <> 5 then next.
        
        assign de-qtde = 0
               i-cont  = 0.
        for each prazo-compra 
            where prazo-compra.numero-ordem = ordem-compra.numero-ordem no-lock:  
            assign i-cont = i-cont + 1.
            if  i-cont = 1 then
                assign c-entrega = string(prazo-compra.data-entrega).
            assign de-qtde = de-qtde + prazo-compra.quantidade.
        end.
        if  i-cont > 1 then do:
            {utp/ut-liter.i Parcelada * r}
            assign c-entrega = trim(return-value).
        end.

        find proc-compra where proc-compra.nr-processo = ordem-compra.nr-processo
            no-lock no-error.
        for each proc-fornec where
            proc-fornec.nr-processo  = ordem-compra.nr-processo
            no-lock:
            
            run pi-acompanhar in h-acomp (input string(ordem-compra.nr-processo)).
            
            run pi-altera-ordem (input rowid(ordem-compra)).

            &IF defined(bf_mat_bloqueio_fornec) &THEN
                run cdp/cdapi029(input ordem-compra.cod-comprado,
                                 input 1,
                                 input today,
                                 input proc-fornec.cod-emitente,
                                 output i-situacao,
                                 output dt-vig-ini,
                                 output dt-vig-fim,
                                 output table tt-erro).
                if return-value = "NOK":u then next.
            &ENDIF
            

            FIND FIRST usuar-mater NO-LOCK
                 WHERE usuar-mater.cod-usuario = ordem-compra.cod-comprado
                 NO-ERROR.
            FIND FIRST prazo-compra NO-LOCK
                 WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem 
                 NO-ERROR.
            FIND FIRST ITEM NO-LOCK
                 WHERE ITEM.it-codigo = ordem-compra.it-codigo
                 NO-ERROR.

            create tt-ficha.
            assign tt-ficha.processo    = ordem-compra.nr-processo
                   tt-ficha.fornecedor  = proc-fornec.cod-emitente
                   tt-ficha.ordem       = ordem-compra.numero-ordem
                   tt-ficha.item        = ordem-compra.it-codigo
                   tt-ficha.descricao   = ITEM.descricao-1
                   tt-ficha.quantidade  = de-qtde
                   tt-ficha.entrega     = c-entrega
                   tt-ficha.parcela     = prazo-compra.parcela
                   tt-ficha.un          = prazo-compra.un
                   tt-ficha.narrat-proc = IF AVAIL proc-compra THEN proc-compra.narrativa ELSE "".

            IF ITEM.tipo-contr = 4 THEN /* Debito Direto */
               ASSIGN tt-ficha.narrativa = ordem-compra.narrativa.
            ELSE
               ASSIGN tt-ficha.narrativa = item.narrativa.

            IF AVAIL usuar-mater THEN
               ASSIGN tt-ficha.comprador     = usuar-mater.nome-usuar
                      tt-ficha.tel-umater    = usuar-mater.telefone[1]
                      tt-ficha.fax-umater    = usuar-mater.telefax
                      tt-ficha.e-mail        = usuar-mater.e-mail.
        end.
    end.
    
    for each tt-ficha use-index id-ficha
        break by processo 
              by fornecedor:
        
        if  not avail ordem-compra 
        or (avail ordem-compra and ordem-compra.numero-ordem <> tt-ficha.ordem)
        then do:
            find ordem-compra where ordem-compra.numero-ordem = tt-ficha.ordem
                 no-lock no-error.
        end.
        
        IF  FIRST-OF(tt-ficha.processo) OR FIRST-OF (tt-ficha.fornecedor) THEN DO:
            output stream s-email to value(trim(c-arq) + "\" + 
                            STRING(tt-ficha.processo)   + "-" +
                            STRING(tt-ficha.fornecedor) + ".txt")
                   paged page-size 64 convert target 'iso8859-1'.
        end.
        
        if  first-of(tt-ficha.processo) or first-of(tt-ficha.fornecedor) then do:
            page. 
            assign i-cont = 0.
            disp stream s-email 
                 tt-ficha.processo  
                 tt-ficha.comprador 
                 tt-ficha.tel-umater
                 tt-ficha.fax-umater                
                 tt-ficha.e-mail     with frame f-processo.

            find first emitente
                where emitente.cod-emitente = tt-ficha.fornecedor
                no-lock no-error.

            disp stream s-email
                 tt-ficha.fornecedor
                 emitente.nome-abrev
                 emitente.telefone[1]
                 emitente.telefax
                 c-lb-dt-cot
                 c-traco with frame f-fornec.

            disp stream s-email
                 c-lb-ordem
                 c-lb-item
                 c-lb-qtde
                 c-lb-preco
                 c-lb-per-ipi with frame f-cab1.
        end.  

        /*
        IF LINE-COUNTER < 10 THEN 
           disp stream s-email 
                c-lb-ordem
                c-lb-item
                c-lb-qtde
                c-lb-preco
                c-lb-per-ipi with frame f-cab1.
        */

        disp stream s-email
             tt-ficha.ordem
             tt-ficha.item
             tt-ficha.quantidade
             /*tt-ficha.parcela*/
             tt-ficha.un
             tt-ficha.descricao with frame f-ordem.
        down stream s-email with frame f-ordem.

        assign i-cont = i-cont + 1.

        IF tt-ficha.narrativa <> "" THEN
           put stream s-email unformatted
               fill("-",29) " Narrativa " fill("-",29) SKIP.

        ASSIGN i-conta-aux = 0.
        DO i-conta = 1 TO 2000:
          IF SUBSTRING(tt-ficha.narrativa,i-conta,1) = CHR(10) THEN DO:
             PUT stream s-email SKIP.
             ASSIGN i-conta-aux = 0.
             NEXT.
          END.
          IF SUBSTRING(tt-ficha.narrativa,i-conta,4) = " " THEN
             LEAVE.
          PUT stream s-email SUBSTRING(tt-ficha.narrativa,i-conta,1) FORMAT "X(01)".

          ASSIGN i-conta-aux = i-conta-aux + 1.

          IF i-conta-aux = 69 THEN DO:
             ASSIGN i-conta-aux = 0.
             PUT stream s-email SKIP.
          END.
        END.

        IF tt-ficha.narrativa <> "" THEN
           PUT stream s-email SKIP(2).

        /*Fim impress∆o da descriá∆o do Item e Narrativa da Ordem*/
        if  last-of(tt-ficha.processo)
        or  last-of(tt-ficha.fornecedor) then do:

            IF LAST-OF(tt-ficha.fornecedor) THEN DO:
               IF tt-ficha.narrat-proc <> " " THEN DO:
                  PUT stream s-email UNFORMATTED FILL("-",132) SKIP
                      "Observacoes" AT 55 SKIP
                      FILL("-" ,132) SKIP. 

                  ASSIGN i-conta-aux = 0.
                  DO i-conta = 1 TO 2000:
                     IF SUBSTRING(tt-ficha.narrat-proc,i-conta,1) = CHR(10) THEN DO:
                        PUT stream s-email SKIP.
                        ASSIGN i-conta-aux = 0.
                        NEXT.
                     END.
                     IF SUBSTRING(tt-ficha.narrat-proc,i-conta,4) = " " THEN
                        LEAVE.
                     PUT stream s-email SUBSTRING(tt-ficha.narrat-proc,i-conta,1) FORMAT "X(01)".

                     ASSIGN i-conta-aux = i-conta-aux + 1.
                     IF i-conta-aux = 132 THEN DO:
                        ASSIGN i-conta-aux = 0.
                        PUT stream s-email SKIP.
                     END.
                  END.
                  PUT stream s-email SKIP(2).
               END.

               for each tt-editor:
                  if  tt-editor.linha = 1 then
                      put stream s-email unformatted
                          fill("-",29) " " c-lb-ordem " " fill("-",29).
               end.

               disp stream s-email 
                     c-lb-dados with frame f-cab2.
               disp stream s-email
                    c-lb-ipi      
                    c-lb-inc      
                    c-lb-aliq-icms
                    c-lb-aliq-iss 
                    c-lb-valid    
                    /*c-lb-per-desc*/
                    c-lb-prazo    
                    c-lb-contato  
                    c-lb-cpg      
                    c-lb-ninc     
                    c-lb-emdia
                    c-traco with frame f-det-fornec.   
            END.
        end.

        if  last-of(tt-ficha.fornecedor) then do:
            find emitente where emitente.cod-emitente = tt-ficha.fornecedor
                no-lock no-error.
            output stream s-email close.
        end.
        
        if  last-of(tt-ficha.fornecedor) and 
            emitente.e-mail <> "":U      and 
            param-compra.log-2           then do:

            find proc-compra where
                 proc-compra.nr-processo = tt-ficha.processo no-lock no-error.
            
            FIND proc-fornec WHERE proc-fornec.nr-processo = tt-ficha.processo
                               AND proc-fornec.cod-emitente = tt-ficha.fornecedor
                NO-LOCK NO-ERROR.
            if avail proc-compra then do:            
                find usuar-mater where 
                     usuar-mater.cod-usuario = proc-compra.cod-comprado no-lock no-error.
                
                for each tt-envio:
                    delete tt-envio.
                end.
                /* quando o dominio for diferente de branco informar o link para que fornecedor possa registrar a cotaá∆o diretamente */ 
                &if  "{&bf_mat_versao_ems}" >= "2.04" &then
                    IF param-global.des-app-url <> "" THEN
                    assign c-url-wcc0303a = "http://" + param-global.nome-dominio-ext + param-global.des-app-url + "/web/men/wrun.w?program=web/ccp/wcc0322.w&param=fin&chave=" + string(rowid(proc-fornec))
                           /* "http://acores/scripts/cgiip.exe/WService=ems204/web/men/wrun.w?program=web/ccp/wcc0322.w&param=fin&chave=" + string(rowid(proc-fornec))  */
                           c-mensagem-e-mail = "Preenchimento da Cotaá∆o Via Internet: " + chr(13) + c-url-wcc0303a + chr(13) 
                                             + "Arquivo Anexo Para Cotaá∆o do Processo.....:".
                &endif
                
                create tt-envio.
                assign tt-envio.versao-integracao = 1
                       tt-envio.exchange    = param-global.log-1
                       tt-envio.destino     = emitente.e-mail
                       tt-envio.assunto     = c-assunto-e-mail
                       tt-envio.mensagem    = c-mensagem-e-mail
                       tt-envio.importancia = 2
                       tt-envio.log-enviada = yes
                       tt-envio.log-lida    = yes
                       tt-envio.acomp       = yes
                       tt-envio.arq-anexo   = (trim(c-arq) + "\" + 
                                               string(tt-ficha.processo)   + "-" +
                                               string(tt-ficha.fornecedor) + ".txt")
                       tt-envio.remetente   = if avail usuar-mater then usuar-mater.e-mail else "".
                
                run utp/utapi009.p (input  table tt-envio,
                                    output table tt-erros).
            end.
        end.    
    end.

/* &IF defined(bf_mat_bloqueio_fornec) &THEN */
/*     delete procedure h-api029.            */
/* &ENDIF                                    */

END PROCEDURE.

PROCEDURE pi-altera-ordem:

def buffer b-ordem for ordem-compra.
def input param r-registro as rowid.

find b-ordem where rowid(b-ordem) = r-registro exclusive-lock.
assign b-ordem.impr-ficha = no.

release b-ordem.

END PROCEDURE.


{include/pi-edit.i}
