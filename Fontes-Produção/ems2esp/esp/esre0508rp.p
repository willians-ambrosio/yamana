/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESRE0508RP 12.1.13.000}  /*** 010033 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i esre0508rp MRE}
&ENDIF

/****************************************************************************
**
**       PROGRAMA: ESRE0508
**
**       OBJETIVO: Saldos de Materiais em Poder de Terceiros
**
**       VERSAO..: 2.06.000 - Sergio Luiz Neto da Silveira - DSC PRAXIS - 22/12/16
******************************************************************************/
{include/i-epc200.i "esre0508rp"}

{cdp/cdcfgmat.i} /* defini‡Æo de pr‚-processadores distribui‡Æo */
{cdp/cdcfgdis.i}
{esp/esre0508.i1} /* Define frames, temp-tables e vari veis */
{cdp/cd1234.i}  /* Valida‡Æo Decimais */
{include/i-rpvar.i}

DEFINE BUFFER empresa FOR ems2cadme.empresa.

DEFINE STREAM st-excel.
    
DEFINE VARIABLE c-form-logico AS CHARACTER   NO-UNDO.
                                  
DEFINE VARIABLE c-arquivo          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-responsavel      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-aprovador-nome   LIKE usuar-mater.nome-usuar NO-UNDO.
DEFINE VARIABLE c-responsavel-nome LIKE usuar-mater.nome-usuar NO-UNDO.
DEFINE VARIABLE c-aprovador        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-nr-dias          AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-situacao         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-cont             AS INTEGER    NO-UNDO.

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.    

def var h-acomp as handle no-undo.
run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input 'Saldo em Poder de Terceiros').

FUNCTION fcNarrativa RETURN CHARACTER (INPUT ip-narrativa AS CHARACTER):
   DEFINE VARIABLE c-narrativa AS CHARACTER NO-UNDO.

   ASSIGN c-narrativa = ip-narrativa.

   ASSIGN c-narrativa = REPLACE(c-narrativa,CHR(08),'')
          c-narrativa = REPLACE(c-narrativa,CHR(10),'')
          c-narrativa = REPLACE(c-narrativa,CHR(13),'')
          c-narrativa = REPLACE(c-narrativa,";",':').

   RETURN c-narrativa.

END FUNCTION.

find first param-global no-lock no-error.
if avail param-global then
   assign c-empresa = param-global.grupo.
find first param-estoq no-lock no-error.

FIND FIRST es-param-terceiros
    NO-LOCK NO-ERROR.

find first empresa where empresa.ep-codigo = param-global.empresa-prin
     no-lock no-error.

assign c-programa = 'ESRE0508'
       c-versao   = '2.06'
       c-revisao  = '000'. 

{utp/ut-liter.i Saldos_em_Poder_de_Terceiros * r}
assign c-titulo-relat = trim(return-value) + ' - ' 
                        + string(tt-param.da-ini-per) + ' a ' 
                        + string(tt-param.da-fim-per).

{utp/ut-liter.i ESTOQUE * r}
assign c-sistema = return-value.

{include/i-rpcab.i}
{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.                                  

DEFINE TEMP-TABLE tt-dig-imp NO-UNDO LIKE tt-digita.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.


FOR EACH tt-digita:
    CREATE tt-dig-imp.
    BUFFER-COPY tt-digita TO tt-dig-imp.
END.

FORM tt-dig-imp.nat-operacao AT 20 COLUMN-LABEL "Nat Oper"
     tt-dig-imp.denominacao        COLUMN-LABEL "Denomina‡Æo"
    WITH WIDTH 132 NO-BOX DOWN STREAM-IO FRAME f-digita.

FORM tt-dig-imp.cod-estabel AT 20 COLUMN-LABEL "Est"
     tt-dig-imp.nome              COLUMN-LABEL "Nome"
    WITH WIDTH 132 NO-BOX DOWN STREAM-IO FRAME f-digita2.

&IF DEFINED (bf_mat_versao_ems)  &THEN
    &IF {&bf_mat_versao_ems} >= 2.03 &THEN

        IF  NOT CAN-FIND(FIRST tt-digita
                         WHERE tt-digita.tipo-digita = 2) THEN DO:

            FOR LAST tt-digita:
                ASSIGN vi-seq = tt-digita.i-seq.
            END.

            FOR EACH estabelec FIELDS(cod-estabel nome) NO-LOCK
               WHERE estabelec.cod-estabel >= tt-param.c-ini-estabel
                 AND estabelec.cod-estabel <= tt-param.c-fim-estabel:

                CREATE tt-digita.
                ASSIGN tt-digita.i-seq       = vi-seq + 1
                       tt-digita.tipo-digita = 2
                       tt-digita.cod-estabel = estabelec.cod-estabel
                       tt-digita.nome        = estabelec.nome
                       vi-seq                = vi-seq + 1.
            END.
        END.
    &ENDIF
&ENDIF

FIND FIRST tt-digita NO-LOCK
     WHERE tt-digita.tipo-digita = 1 NO-ERROR.
IF  AVAIL tt-digita THEN
    ASSIGN l-existe = YES.
ELSE
    ASSIGN l-existe = NO.

RUN pi-abre-excel.

if tt-param.classifica = 1 then do:
   {esp/esre0508.i2}
end.   
else do:
   {esp/esre0508.i3}
end.

if tt-param.l-preco-medio then
   c-preco = c-lb-preco-medio-m.
else
   c-preco = c-lb-preco-medio-i.  

IF tt-param.tp-emis-relat = 1 THEN
    ASSIGN c-emis-relat = "Detalhado".
ELSE
    ASSIGN c-emis-relat = "Resumido".

page.

{utp/ut-liter.i Sim/NÆo * r}
    ASSIGN c-form-logico = TRIM(RETURN-VALUE).

IF tt-param.param-impr = YES  THEN DO:
    put skip(1)
        c-lb-tit-sel                at 20 
        c-lb-emitente               at 20  ":"
        tt-param.i-ini-cod-emitente at 45 format "999999999"  
        "|<"                        at 65 
        ">|"                        at 70
        tt-param.i-fim-cod-emitente at 75 format "999999999"
        c-lb-nro-docto              at 20  ":"
        tt-param.c-ini-nro-docto    at 45 
        "|<"                        at 65
        ">|"                        at 70
        tt-param.c-fim-nro-docto    at 75   
        c-lb-serie-docto            at 20  ":"
        tt-param.c-ini-serie-docto  at 45
        "|<"                        at 65
        ">|"                        at 70
        tt-param.c-fim-serie-docto  at 75
        c-lb-nat-operacao           at 20  ":"
        tt-param.c-ini-nat-operacao at 45 
        "|<"                        at 65
        ">|"                        at 70
        tt-param.c-fim-nat-operacao at 75
        c-lb-item                   at 20  ":"
        tt-param.c-ini-it-codigo    at 45 
        "|<"                        at 65
        ">|"                        at 70
        tt-param.c-fim-it-codigo    at 75
        c-lb-periodo                at 20  ":"
        tt-param.da-ini-per         at 45 format "99/99/9999"
        "|<"                        at 65
        ">|"                        at 70
        tt-param.da-fim-per         at 75 format "99/99/9999"
        &if defined (bf_mat_versao_ems)  &then
          &if "{&bf_mat_versao_ems}" >= "2.03" &then
            c-ato                       at 20  ":"
            tt-param.nr-ato-conce       at 45 
            "|<"                        at 65
            ">|"                        at 70
            tt-param.nr-ato-conce-fim   at 75
            c-estabel                   at 20  ":"
            tt-param.c-ini-estabel      at 45 
            "|<"                        at 65
            ">|"                        at 70
            tt-param.c-fim-estabel      at 75
          &endif
        &endif  
        skip(1)
        c-lb-tit-cla                at 20
        "......:"                   at 40
        tt-param.desc-classifica    at 48
        skip(1)
        c-lb-tit-par                at 20
        c-lb-ent-ben                at 20
        "......:"                   at 40
        tt-param.l-ent-ben          at 48   FORMAT c-form-logico 
        c-lb-sai-ben                at 20
        "......:"                   at 40
        tt-param.l-sai-ben          at 48    FORMAT c-form-logico
        c-lb-transf                 at 20
        "......:"                   at 40
        tt-param.l-transf           at 48    FORMAT c-form-logico
        c-lb-ent-cons               at 20
        "......:"                   at 40
        tt-param.l-ent-cons         at 48    FORMAT c-form-logico
        c-lb-sai-cons               at 20
        "......:"                   at 40
        tt-param.l-sai-cons         at 48    FORMAT c-form-logico
        c-lb-ent-fut                at 20
        "......:"                   at 40
        tt-param.l-ent-fut          at 48    FORMAT c-form-logico        
        c-lb-preco-medio            at 20
        "......:"                   at 40
        c-preco                     at 48
        c-lb-saldo-zerado           at 20
        "......:"                   at 40
        tt-param.l-saldo-zerado     at 48    FORMAT c-form-logico
        &if "{&bf_mat_versao_ems}" >= "2.062" &then
            c-lb-lista-narra            at 20
            "......:"                   at 40
            tt-param.l-lista-narra      at 48    FORMAT c-form-logico
        &endif 
        &if defined (bf_mat_versao_ems)  &then
          &if "{&bf_mat_versao_ems}" >= "2.03" &then
           c-lb-drawback               at 20
           "......:"                   at 40
           tt-param.l-drawback         at 48  FORMAT c-form-logico  
          &endif
        &endif  
        c-lb-emis-relat             at 20
        "......:"                   at 40
        c-emis-relat                at 48.

    IF  CAN-FIND(FIRST tt-dig-imp) THEN
        PUT SKIP(1) c-lb-tit-dig AT 20.

    IF  CAN-FIND(FIRST tt-dig-imp
                 WHERE tt-dig-imp.tipo-digita = 1) THEN DO:

        PUT SKIP(1).

        FOR EACH tt-dig-imp NO-LOCK
           WHERE tt-dig-imp.tipo-digita = 1:
            DISP tt-dig-imp.nat-operacao
                 tt-dig-imp.denominacao
                 WITH FRAME f-digita.
            DOWN WITH FRAME f-digita.
        END.
    END.

    IF  CAN-FIND(FIRST tt-dig-imp
                 WHERE tt-dig-imp.tipo-digita = 2) THEN DO:

        PUT SKIP(1).

        FOR EACH tt-dig-imp NO-LOCK
           WHERE tt-dig-imp.tipo-digita = 2:

            DISP tt-dig-imp.cod-estabel
                 tt-dig-imp.nome
                 WITH FRAME f-digita2.
            DOWN WITH FRAME f-digita2.
        END.
    END.
END.

{include/i-rpclo.i}
run pi-finalizar in h-acomp.

RUN pi-encerra-excel.


/******************************** PROCEDURES INTERNAS *******************************************/

/* Gera‡Æo de Dados para ImpressÆo */
PROCEDURE pi-imprime-terc:

    IF  tt-param.l-saldo-zerado = NO 
    AND saldo-terc.quantidade = 0 THEN 
        RETURN.

    IF l-existe THEN DO:
        FIND FIRST tt-digita
             WHERE tt-digita.nat-operacao = saldo-terc.nat-operacao
               AND tt-digita.tipo-digita  = 1 NO-ERROR.
        IF NOT AVAIL tt-digita THEN 
            RETURN.
    END.

    &IF DEFINED (bf_mat_versao_ems)  &THEN
        &IF "{&bf_mat_versao_ems}" >= "2.03" &THEN
    
            IF  NOT CAN-FIND(FIRST tt-digita
                             WHERE tt-digita.cod-estabel = saldo-terc.cod-estabel
                               AND tt-digita.tipo-digita = 2) THEN NEXT.
        &ENDIF
    &ENDIF           

    IF NOT saldo-terc.log-entreg-fut THEN
        IF ( NOT tt-param.l-sai-ben  AND saldo-terc.tipo-sal-terc = 1 ) 
        OR ( NOT tt-param.l-ent-ben  AND saldo-terc.tipo-sal-terc = 2 ) 
        OR ( NOT tt-param.l-transf   AND saldo-terc.tipo-sal-terc = 3 )
        OR ( NOT tt-param.l-sai-cons AND saldo-terc.tipo-sal-terc = 4 )
        OR ( NOT tt-param.l-ent-cons AND saldo-terc.tipo-sal-terc = 5 )    
        /************************************************************ 
        *  Pr‚-Processador para - 2.03
        ************************************************************/      
        &IF DEFINED (bf_mat_versao_ems) &THEN
            &IF "{&bf_mat_versao_ems}" >= "2.03" &THEN
                OR ( NOT tt-param.l-drawback AND saldo-terc.tipo-sal-terc = 6 ) 
            &ENDIF
        &ENDIF     
        THEN
            RETURN.

    IF NOT tt-param.l-ent-fut  AND saldo-terc.log-entreg-fut THEN RETURN.
    FIND componente OF saldo-terc NO-LOCK NO-ERROR.
    IF NOT AVAIL componente THEN 
        RETURN.

    RUN pi-verifica-movto (INPUT saldo-terc.serie-docto,
                           INPUT saldo-terc.nro-docto,
                           INPUT saldo-terc.nat-operacao,
                           INPUT saldo-terc.sequencia ).

    IF tt-param.l-preco-medio THEN DO:
        ASSIGN de-preco       =  (saldo-terc.valor-mat-m[1]
                              + saldo-terc.valor-mob-m[1]
                              + saldo-terc.valor-ggf-m[1]).
        ASSIGN de-saldo-valor = fn_ajust_dec((de-preco * componente.quantidade), 0).
    END.    
    ELSE
        ASSIGN de-preco       = componente.preco-total[1]
               de-saldo-valor = componente.preco-total[1].

    ASSIGN de-saldo    = componente.quantidade
           de-qtd-orig = componente.quantidade.


    &IF DEFINED (bf_mat_versao_ems)  &THEN
        &IF "{&bf_mat_versao_ems}" >= "2.03" &THEN
            DISP saldo-terc.cod-estabel
                WITH FRAME f-movtos.
        &ENDIF
    &ENDIF

    IF tt-param.tp-emis-relat = 1  THEN DO:
       ASSIGN c-narra = IF tt-param.l-lista-narra THEN componente.narrativa ELSE "".

       &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN 
          IF tt-param.l-transf AND saldo-terc.emite-comp <> 0 THEN
             DISPLAY saldo-terc.emite-comp @ saldo-terc.cod-emitente
                     saldo-terc.it-codigo
                     saldo-terc.dt-retorno
                     saldo-terc.sequencia
                     saldo-terc.serie-docto
                     saldo-terc.nro-docto
                     saldo-terc.nat-operacao
                     componente.quantidade
                     de-saldo
                     de-saldo-valor
                     c-mens
                     WITH FRAME f-movtos.
          ELSE
       &ENDIF
           DISPLAY saldo-terc.cod-emitente
               saldo-terc.it-codigo
               saldo-terc.dt-retorno
               saldo-terc.sequencia
               saldo-terc.serie-docto
               saldo-terc.nro-docto
               saldo-terc.nat-operacao
               componente.quantidade
               de-saldo
               de-saldo-valor
               c-mens
               WITH FRAME f-movtos.
       DOWN WITH FRAME f-movtos.

       /*INSERIR NARRATIVA */
       &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN 
         if  tt-param.l-lista-narra
         and c-narra <> "" then do:
             run pi-print-editor (c-narra, 60).
             for each tt-editor:
                 if  tt-editor.linha = 1 then do:
                     {utp/ut-liter.i Narrativa * r}
                     put unformatted return-value at 10 ": ".
                 end.
                 put unformatted tt-editor.conteudo SKIP.
             end.
             PUT UNFORMATTED SKIP(1).
         end.
       &ENDIF

    END.
    
    FOR EACH componente
        WHERE componente.cod-emitente = saldo-terc.cod-emitente
        AND   componente.serie-comp   = saldo-terc.serie-docto
        AND   componente.nro-comp     = saldo-terc.nro-docto
        AND   componente.nat-comp     = saldo-terc.nat-operacao
        AND   componente.it-codigo    = saldo-terc.it-codigo
        AND   componente.cod-refer    = saldo-terc.cod-refer 
        AND   componente.seq-comp     = saldo-terc.sequencia  
/************************************************************ 
 *  Pr‚-Processador para - 2.03
 ************************************************************/      
        &IF DEFINED (bf_mat_versao_ems)  &THEN
            &IF "{&bf_mat_versao_ems}" >= "2.03" &THEN
                AND   componente.nr-ato-concessorio >= tt-param.nr-ato-conce        
                AND   componente.nr-ato-concessorio <= tt-param.nr-ato-conce-fim
            &ENDIF
        &ENDIF 
        NO-LOCK 
        BY componente.dt-retorno:

        IF  CAN-FIND(FIRST param-of NO-LOCK
         WHERE param-of.cod-estabel = saldo-terc.cod-estabel
          &IF "{&mguni_version}" >= "2.08" &THEN
           AND param-of.log-devol-consig 
          &ELSE
           AND STRING(SUBSTR(param-of.char-2,57,1)) = "S"
          &ENDIF)
        AND componente.quantidade = 0 AND componente.preco-total[1] = 0
        AND &IF "{&mguni_version}" >= "2.08" &THEN  /* Simb¢lica */
             componente.idi-tip-devol-consig = 1
            &ELSE
             INT(SUBSTR(componente.char-1, 82,1)) = 1
            &ENDIF THEN
            NEXT.

        RUN pi-verifica-movto (INPUT componente.serie-docto,
                               INPUT componente.nro-docto,
                               INPUT componente.nat-operacao,
                               INPUT componente.sequencia ).

        ASSIGN de-saldo = de-saldo
                        + IF  componente.componente = 1 THEN 
                              componente.quantidade
                          ELSE 
                              (componente.quantidade * -1).

        IF tt-param.l-preco-medio THEN
            ASSIGN de-saldo-valor = de-saldo * de-preco.
        ELSE DO:
            ASSIGN de-saldo-valor = de-saldo-valor
                                  + IF componente.componente = 1 THEN 
                                       componente.preco-total[1]
                                    ELSE 
                                       (componente.preco-total[1] * -1).
        END.

        IF tt-param.tp-emis-relat = 1 THEN DO:
            ASSIGN c-narra = IF tt-param.l-lista-narra THEN componente.narrativa ELSE "".
            DISP ""                     @ saldo-terc.cod-emitente
                 ""                     @ saldo-terc.it-codigo
                 componente.dt-retorno  @ saldo-terc.dt-retorno
                 componente.sequencia   @ saldo-terc.sequencia
                 componente.serie-docto @ saldo-terc.serie-docto
                 componente.nro-docto   @ saldo-terc.nro-docto
                 componente.nat-operac  @ saldo-terc.nat-operacao
                 componente.quantidade
                 de-saldo
                 de-saldo-valor
                 c-mens
                 WITH FRAME f-movtos.
            DOWN WITH FRAME f-movtos.
            
           /*INSERIR NARRATIVA */
           &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN 
               if  tt-param.l-lista-narra
               and c-narra <> "" then do:
                   run pi-print-editor (c-narra, 60).
                   for each tt-editor:
                       if  tt-editor.linha = 1 then do:
                           {utp/ut-liter.i Narrativa * r}
                           put unformatted return-value at 10 ": ".
                       end.
                       put unformatted tt-editor.conteudo SKIP.
                   end.
                   PUT UNFORMATTED SKIP(1).
               end.
           &ENDIF

        END.
    END.     

    IF tt-param.tp-emis-relat = 2 THEN DO:
        {utp/ut-liter.i Saldo_Valor * L}                                      
        assign de-saldo-valor:label in frame f-movtos = "  " + return-value.
        ASSIGN componente.quantidade:LABEL = "     Qtde Orig":U.
        &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
            IF tt-param.l-transf AND saldo-terc.emite-comp <> 0 THEN
               DISPLAY saldo-terc.emite-comp @ saldo-terc.cod-emitente
                       saldo-terc.it-codigo
                       saldo-terc.dt-retorno
                       saldo-terc.sequencia
                       saldo-terc.serie-docto
                       saldo-terc.nro-docto
                       saldo-terc.nat-operacao
                       de-qtd-orig @ componente.quantidade
                       de-saldo                           
                       de-saldo-valor                      
                       c-mens
                       WITH FRAME f-movtos.
            ELSE
        &ENDIF
               DISPLAY saldo-terc.cod-emitente
                       saldo-terc.it-codigo
                       saldo-terc.dt-retorno
                       saldo-terc.sequencia
                       saldo-terc.serie-docto
                       saldo-terc.nro-docto
                       saldo-terc.nat-operacao
                       de-qtd-orig @ componente.quantidade
                       de-saldo                           
                       de-saldo-valor                      
                       c-mens
                       WITH FRAME f-movtos.
        DOWN WITH FRAME f-movtos.
    END. 

    IF tt-param.tp-emis-relat = 1 THEN DO:
        IF saldo-terc.quantidade <> de-saldo then do:
            ASSIGN c-mens = "****" 
                   de-saldo = saldo-terc.quantidade.
            DISP "" @ saldo-terc.cod-emitente
                 "" @ saldo-terc.serie-docto
                 "" @ saldo-terc.nro-docto
                 "" @ saldo-terc.nat-oper
                 "" @ saldo-terc.sequencia
                 "" @ saldo-terc.it-codigo
                 "" @ saldo-terc.dt-retorno
                 "Calculado....:" @ componente.quantidade
                  de-saldo
                  de-saldo-valor
                  WITH FRAME f-movtos.
                DOWN WITH FRAME f-movtos.
        END.
    END.
END.                       

procedure pi-print-editor:

    def input param c-editor    as char    no-undo.
    def input param i-len       as integer no-undo.

    def var i-linha  as integer no-undo.
    def var i-aux    as integer no-undo.
    def var c-aux    as char    no-undo.
    def var c-ret    as char    no-undo.

    for each tt-editor:
        delete tt-editor.
    end.

    assign c-ret = chr(255) + chr(255).

    do  while c-editor <> "":
        if  c-editor <> "" then do:
            assign i-aux = index(c-editor, chr(10)).
            if  i-aux > i-len or (i-aux = 0 and length(c-editor) > i-len) then
                assign i-aux = r-index(c-editor, " ", i-len + 1).
            if  i-aux = 0 then
                assign c-aux = substr(c-editor, 1, i-len)
                       c-editor = substr(c-editor, i-len + 1).
            else
                assign c-aux = substr(c-editor, 1, i-aux - 1)
                       c-editor = substr(c-editor, i-aux + 1).
            if  i-len = 0 then
                assign entry(1, c-ret, chr(255)) = c-aux.
            else do:
                assign i-linha = i-linha + 1.
                create tt-editor.
                assign tt-editor.linha    = i-linha
                       tt-editor.conteudo = c-aux.
            end.
        end.
        if  i-len = 0 then
            return c-ret.
    end.
    return c-ret.
end procedure.

procedure pi-verifica-movto:

    def input param pi-serie-docto    like saldo-terc.serie-docto   no-undo.
    def input param pi-nro-docto      like saldo-terc.nro-docto     no-undo.
    def input param pi-nat-operacao   like saldo-terc.nat-operacao  no-undo.
    def input param pi-sequen-nf      like saldo-terc.sequencia     no-undo.

    find first movto-estoq
         where movto-estoq.serie-docto  = pi-serie-docto
         and   movto-estoq.nro-docto    = pi-nro-docto
         and   movto-estoq.nat-operacao = pi-nat-operacao 
         and   movto-estoq.cod-emitente = saldo-terc.cod-emitente
         and   movto-estoq.it-codigo    = saldo-terc.it-codigo
         and   movto-estoq.sequen-nf    = pi-sequen-nf  
         no-lock no-error.
    
   
   IF tt-param.tp-emis-relat = 1 THEN 
        assign c-mens = c-rem-ret[componente.componente] + " " 
                  + ( if  not avail movto-estoq then "S/M":U
                      else " " ) .
   ELSE
        assign c-mens = "Rem" + " " 
                  + ( if  not avail movto-estoq then "S/M":U
                      else " " ) .
end.


PROCEDURE pi-abre-excel:
   DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
   DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv".
   
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   OUTPUT STREAM st-excel TO VALUE(c-arquivo) NO-CONVERT NO-MAP.


   {utp/ut-field.i ems2movemp saldo-terc cod-estabel      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems2movemp saldo-terc cod-emitente     1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems2cadme  emitente   nome-emit        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems2movemp saldo-terc it-codigo        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems2cademp ITEM       desc-item        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems2cademp ITEM       ge-codigo        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems2movemp saldo-terc dt-retorno       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems2movemp saldo-terc sequencia        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems2movemp saldo-terc serie-docto      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems2movemp saldo-terc nro-docto        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems2movemp saldo-terc nat-operacao     1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems2movemp componente quantidade       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".

   PUT STREAM st-excel 
       "Quant Saldo"     ";"
       "Valor Saldo"     ";"
       "Mensagem"        ";"
       "Respons vel"     ";"
       "Nome"            ";"
       "Aprovador"       ";"
       "Nome"            ";"
       "Quant Dias Terc" ";"
       "Situa‡Æo"        ";"
       "Narrativa"       ";" SKIP.

   ASSIGN i-cont = 1.
END PROCEDURE.

PROCEDURE pi-imprime-terc-excel:
    ASSIGN c-responsavel       = ""
           c-aprovador         = ""
           c-responsavel-nome  = ""
           c-aprovador-nome    = ""
           i-nr-dias           = 0
           c-situacao          = "Normal".

    IF  tt-param.l-saldo-zerado = NO 
    AND saldo-terc.quantidade = 0 THEN 
        RETURN.

    IF l-existe THEN DO:
        FIND FIRST tt-digita
             WHERE tt-digita.nat-operacao = saldo-terc.nat-operacao
               AND tt-digita.tipo-digita  = 1 NO-ERROR.
        IF NOT AVAIL tt-digita THEN 
            RETURN.
    END.

    &IF DEFINED (bf_mat_versao_ems)  &THEN
        &IF "{&bf_mat_versao_ems}" >= "2.03" &THEN
    
            IF  NOT CAN-FIND(FIRST tt-digita
                             WHERE tt-digita.cod-estabel = saldo-terc.cod-estabel
                               AND tt-digita.tipo-digita = 2) THEN NEXT.
        &ENDIF
    &ENDIF           

    IF NOT saldo-terc.log-entreg-fut THEN
        IF ( NOT tt-param.l-sai-ben  AND saldo-terc.tipo-sal-terc = 1 ) 
        OR ( NOT tt-param.l-ent-ben  AND saldo-terc.tipo-sal-terc = 2 ) 
        OR ( NOT tt-param.l-transf   AND saldo-terc.tipo-sal-terc = 3 )
        OR ( NOT tt-param.l-sai-cons AND saldo-terc.tipo-sal-terc = 4 )
        OR ( NOT tt-param.l-ent-cons AND saldo-terc.tipo-sal-terc = 5 )    
        /************************************************************ 
        *  Pr‚-Processador para - 2.03
        ************************************************************/      
        &IF DEFINED (bf_mat_versao_ems) &THEN
            &IF "{&bf_mat_versao_ems}" >= "2.03" &THEN
                OR ( NOT tt-param.l-drawback AND saldo-terc.tipo-sal-terc = 6 ) 
            &ENDIF
        &ENDIF     
        THEN
            RETURN.

    IF NOT tt-param.l-ent-fut  AND saldo-terc.log-entreg-fut THEN RETURN.
    FIND componente OF saldo-terc NO-LOCK NO-ERROR.
    IF NOT AVAIL componente THEN 
        RETURN.

    FIND FIRST ITEM
         WHERE ITEM.it-codigo = componente.it-codigo
         NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(ITEM) THEN
       RETURN.

    IF (ITEM.ge-codigo < tt-param.i-ini-ge-codigo) OR
       (ITEM.ge-codigo > tt-param.i-fim-ge-codigo) THEN
       RETURN.

    ASSIGN i-cont = i-cont + 1.

    RUN pi-acompanhar IN h-acomp ("Listando: " + STRING(i-cont)).

    FIND FIRST es-it-nota-fisc-terc 
         WHERE es-it-nota-fisc-terc.cod-estabel = saldo-terc.cod-estabel AND       
               es-it-nota-fisc-terc.serie       = componente.serie-docto AND       
               es-it-nota-fisc-terc.nr-nota-fis = componente.nro-docto   AND       
               es-it-nota-fisc-terc.nr-seq-fat  = componente.sequencia   AND       
               es-it-nota-fisc-terc.it-codigo   = componente.it-codigo 
         NO-LOCK NO-ERROR.
    IF AVAILABLE(es-it-nota-fisc-terc) THEN DO:
       FIND FIRST usuar-mater
            WHERE usuar-mater.cod-usuar = es-it-nota-fisc-terc.cod-usuario
            NO-LOCK NO-ERROR.
       IF AVAILABLE(usuar-mater) THEN DO:
          ASSIGN c-responsavel      = usuar-mater.cod-usuar 
                 c-responsavel-nome = usuar-mater.nome-usuar.
       END.

       FIND FIRST usuar-mater
            WHERE usuar-mater.cod-usuar = es-it-nota-fisc-terc.cod-usuario-aprov
            NO-LOCK NO-ERROR.
       IF AVAILABLE(usuar-mater) THEN DO:
          ASSIGN c-aprovador      = usuar-mater.cod-usuar
                 c-aprovador-nome = usuar-mater.nome-usuar.
       END.
    END.

    FIND FIRST nota-fiscal
         WHERE nota-fiscal.cod-estabel = saldo-terc.cod-estabel AND
               nota-fiscal.serie       = componente.serie-docto AND
               nota-fiscal.nr-nota-fis = componente.nro-docto  
         NO-LOCK NO-ERROR.
    IF AVAILABLE(nota-fiscal) THEN DO:
       ASSIGN i-nr-dias = TODAY - nota-fiscal.dt-emis-nota.

       IF i-nr-dias > es-param-terceiros.qt-limite-dias THEN
          ASSIGN c-situacao = "Cr¡tico".
    END.

    RUN pi-verifica-movto (INPUT saldo-terc.serie-docto,
                           INPUT saldo-terc.nro-docto,
                           INPUT saldo-terc.nat-operacao,
                           INPUT saldo-terc.sequencia ).

    IF tt-param.l-preco-medio THEN DO:
        ASSIGN de-preco       =  (saldo-terc.valor-mat-m[1]
                              + saldo-terc.valor-mob-m[1]
                              + saldo-terc.valor-ggf-m[1]).
        ASSIGN de-saldo-valor = fn_ajust_dec((de-preco * componente.quantidade), 0).
    END.    
    ELSE
        ASSIGN de-preco       = componente.preco-total[1]
               de-saldo-valor = componente.preco-total[1].

    ASSIGN de-saldo    = componente.quantidade
           de-qtd-orig = componente.quantidade.


    &IF DEFINED (bf_mat_versao_ems)  &THEN
        &IF "{&bf_mat_versao_ems}" >= "2.03" &THEN
            PUT STREAM st-excel saldo-terc.cod-estabel ";".
                
        &ENDIF
    &ENDIF

    IF tt-param.tp-emis-relat = 1  THEN DO:
       ASSIGN c-narra = IF tt-param.l-lista-narra THEN componente.narrativa ELSE "".

       ASSIGN c-narra = fcNarrativa(c-narra).

       &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN 
          IF tt-param.l-transf AND saldo-terc.emite-comp <> 0 THEN DO:
             FIND FIRST emitente
                  WHERE emitente.cod-emitente = saldo-terc.emite-comp
                  NO-LOCK NO-ERROR.
                 PUT STREAM st-excel 
                     saldo-terc.emite-comp   ";"
                     emitente.nome-emit      ";"
                     saldo-terc.it-codigo    ";"
                     ITEM.desc-item          ";"
                     ITEM.ge-codigo          ";"
                     saldo-terc.dt-retorno   ";"
                     saldo-terc.sequencia    ";"
                     saldo-terc.serie-docto  ";"
                     saldo-terc.nro-docto    ";"
                     saldo-terc.nat-operacao ";"
                     componente.quantidade   ";"
                     de-saldo                ";"
                     de-saldo-valor          ";"
                     c-mens                  ";"
                     c-responsavel           ";"
                     c-responsavel-nome      ";"
                     c-aprovador             ";"
                     c-aprovador-nome        ";"
                     i-nr-dias               ";"
                     c-situacao              ";"
                     c-narra                 ";".
          END.
          ELSE DO:
             FIND FIRST emitente
                  WHERE emitente.cod-emitente = saldo-terc.cod-emitente
                  NO-LOCK NO-ERROR.
       &ENDIF
           PUT STREAM st-excel
               saldo-terc.cod-emitente       ";"
               emitente.nome-emit            ";"
               saldo-terc.it-codigo          ";"
               ITEM.desc-item                ";" 
               ITEM.ge-codigo                ";" 
               saldo-terc.dt-retorno         ";"
               saldo-terc.sequencia          ";"
               saldo-terc.serie-docto        ";"
               saldo-terc.nro-docto          ";"
               saldo-terc.nat-operacao       ";"
               componente.quantidade         ";"
               de-saldo                      ";"
               de-saldo-valor                ";"
               c-mens                        ";"
               c-responsavel                 ";"
               c-responsavel-nome            ";"
               c-aprovador                   ";"
               c-aprovador-nome              ";"
               i-nr-dias                     ";"
               c-situacao                    ";"
               c-narra                       ";".
          END.

      PUT STREAM st-excel SKIP.

/*        /*INSERIR NARRATIVA */                                                 */
/*        &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN                            */
/*          if  tt-param.l-lista-narra                                           */
/*          and c-narra <> "" then do:                                           */
/*              run pi-print-editor (c-narra, 60).                               */
/*              for each tt-editor:                                              */
/*                  if  tt-editor.linha = 1 then do:                             */
/*                      {utp/ut-liter.i Narrativa * r}                           */
/*                      put unformatted return-value at 10 ": ".                 */
/*                  end.                                                         */
/*                  ASSIGN c-narrativa = c-narrativa + " " + tt-editor.conteudo. */
/* /*                  put unformatted tt-editor.conteudo SKIP. */               */
/*              end.                                                             */
/* /*              PUT UNFORMATTED SKIP(1). */                                   */
/*          end.                                                                 */
/*        &ENDIF                                                                 */

    END.
    
    FOR EACH componente
        WHERE componente.cod-emitente = saldo-terc.cod-emitente
        AND   componente.serie-comp   = saldo-terc.serie-docto
        AND   componente.nro-comp     = saldo-terc.nro-docto
        AND   componente.nat-comp     = saldo-terc.nat-operacao
        AND   componente.it-codigo    = saldo-terc.it-codigo
        AND   componente.cod-refer    = saldo-terc.cod-refer 
        AND   componente.seq-comp     = saldo-terc.sequencia  
/************************************************************ 
 *  Pr‚-Processador para - 2.03
 ************************************************************/      
        &IF DEFINED (bf_mat_versao_ems)  &THEN
            &IF "{&bf_mat_versao_ems}" >= "2.03" &THEN
                AND   componente.nr-ato-concessorio >= tt-param.nr-ato-conce        
                AND   componente.nr-ato-concessorio <= tt-param.nr-ato-conce-fim
            &ENDIF
        &ENDIF 
        NO-LOCK 
        BY componente.dt-retorno:

        IF  CAN-FIND(FIRST param-of NO-LOCK
         WHERE param-of.cod-estabel = saldo-terc.cod-estabel
          &IF "{&mguni_version}" >= "2.08" &THEN
           AND param-of.log-devol-consig 
          &ELSE
           AND STRING(SUBSTR(param-of.char-2,57,1)) = "S"
          &ENDIF)
        AND componente.quantidade = 0 AND componente.preco-total[1] = 0
        AND &IF "{&mguni_version}" >= "2.08" &THEN  /* Simb¢lica */
             componente.idi-tip-devol-consig = 1
            &ELSE
             INT(SUBSTR(componente.char-1, 82,1)) = 1
            &ENDIF THEN
            NEXT.

        RUN pi-verifica-movto (INPUT componente.serie-docto,
                               INPUT componente.nro-docto,
                               INPUT componente.nat-operacao,
                               INPUT componente.sequencia ).

        ASSIGN de-saldo = de-saldo
                        + IF  componente.componente = 1 THEN 
                              componente.quantidade
                          ELSE 
                              (componente.quantidade * -1).

        IF tt-param.l-preco-medio THEN
            ASSIGN de-saldo-valor = de-saldo * de-preco.
        ELSE DO:
            ASSIGN de-saldo-valor = de-saldo-valor
                                  + IF componente.componente = 1 THEN 
                                       componente.preco-total[1]
                                    ELSE 
                                       (componente.preco-total[1] * -1).
        END.

        IF tt-param.tp-emis-relat = 1 THEN DO:
            ASSIGN c-narra = IF tt-param.l-lista-narra THEN componente.narrativa ELSE "".

            ASSIGN c-narra = fcNarrativa(c-narra).

            FIND FIRST emitente
                  WHERE emitente.cod-emitente = saldo-terc.cod-emitente
                  NO-LOCK NO-ERROR.

            PUT STREAM st-excel
                 saldo-terc.cod-estabel ";"
                 emitente.cod-emitente  ";"
                 emitente.nome-emit     ";"
                 saldo-terc.it-codigo   ";"
                 ITEM.desc-item         ";"
                 ITEM.ge-codigo         ";"
                 componente.dt-retorno  ";"
                 componente.sequencia   ";"
                 componente.serie-docto ";"
                 componente.nro-docto   ";"
                 componente.nat-operac  ";"
                 componente.quantidade  ";"
                 de-saldo               ";"
                 de-saldo-valor         ";"
                 c-mens                 ";"
                 c-responsavel          ";"
                 c-responsavel-nome     ";"
                 c-aprovador            ";"
                 c-aprovador-nome       ";"
                 i-nr-dias              ";"
                 c-situacao             ";"
                 c-narra                ";".

            PUT STREAM st-excel SKIP.
            
/*            /*INSERIR NARRATIVA */                                   */
/*            &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN              */
/*                if  tt-param.l-lista-narra                           */
/*                and c-narra <> "" then do:                           */
/*                    run pi-print-editor (c-narra, 60).               */
/*                    for each tt-editor:                              */
/*                        if  tt-editor.linha = 1 then do:             */
/*                            {utp/ut-liter.i Narrativa * r}           */
/*                            put unformatted return-value at 10 ": ". */
/*                        end.                                         */
/*                        put unformatted tt-editor.conteudo SKIP.     */
/*                    end.                                             */
/*                    PUT UNFORMATTED SKIP(1).                         */
/*                end.                                                 */
/*            &ENDIF                                                   */

        END.
    END.     

    IF tt-param.tp-emis-relat = 2 THEN DO:
        {utp/ut-liter.i Saldo_Valor * L}                                      
        assign de-saldo-valor:label in frame f-movtos = "  " + return-value.
        ASSIGN componente.quantidade:LABEL = "     Qtde Orig":U.
        &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
            IF tt-param.l-transf AND saldo-terc.emite-comp <> 0 THEN DO:
               FIND FIRST emitente
                    WHERE emitente.cod-emitente = saldo-terc.emite-comp
                    NO-LOCK NO-ERROR.
                   PUT STREAM st-excel
                       saldo-terc.emite-comp   ";"
                       emitente.nome-emit      ";"
                       saldo-terc.it-codigo    ";"
                       ITEM.desc-item          ";" 
                       ITEM.ge-codigo          ";" 
                       saldo-terc.dt-retorno   ";"
                       saldo-terc.sequencia    ";"
                       saldo-terc.serie-docto  ";"
                       saldo-terc.nro-docto    ";"
                       saldo-terc.nat-operacao ";"
                       de-qtd-orig             ";"
                       de-saldo                ";"           
                       de-saldo-valor          ";"            
                       c-mens                  ";"
                       c-responsavel           ";"
                       c-responsavel-nome      ";"
                       c-aprovador             ";"
                       c-aprovador-nome        ";"
                       i-nr-dias               ";"
                       c-situacao              ";"
                       c-narra                 ";".
            END.
            ELSE DO:
               FIND FIRST emitente
                    WHERE emitente.cod-emitente = saldo-terc.cod-emitente
                    NO-LOCK NO-ERROR.
        &ENDIF
                   PUT STREAM st-excel
                       saldo-terc.cod-emitente ";"
                       emitente.nome-emit      ";"
                       saldo-terc.it-codigo    ";"
                       ITEM.desc-item          ";" 
                       ITEM.ge-codigo          ";" 
                       saldo-terc.dt-retorno   ";"
                       saldo-terc.sequencia    ";"
                       saldo-terc.serie-docto  ";"
                       saldo-terc.nro-docto    ";"
                       saldo-terc.nat-operacao ";"
                       de-qtd-orig             ";"
                       de-saldo                ";"           
                       de-saldo-valor          ";"            
                       c-mens                  ";"
                       c-responsavel           ";"
                       c-responsavel-nome      ";"
                       c-aprovador             ";"
                       c-aprovador-nome        ";"
                       i-nr-dias               ";"
                       c-situacao              ";"
                       c-narra                 ";".
            END.


        PUT STREAM st-excel SKIP.
    END. 

   IF tt-param.tp-emis-relat = 1 THEN DO:
        IF saldo-terc.quantidade <> de-saldo then do:
            ASSIGN c-mens = "****" 
                   de-saldo = saldo-terc.quantidade.

            PUT STREAM st-excel
                ""                ";"
                ""                ";"
                ""                ";"
                ""                ";"
                ""                ";"
                ""                ";"
                ""                ";"
                ""                ";" 
                ""                ";"
                ""                ";"
                "Calculado....:"  ";"
                 de-saldo         ";"
                 de-saldo-valor   ";".

            PUT STREAM st-excel SKIP.
        END.
   END.
END PROCEDURE.

PROCEDURE pi-encerra-excel:
   OUTPUT STREAM st-excel CLOSE.
   DOS SILENT START excel.exe VALUE(c-arquivo).
END.    

/*
procedure pi-impressao:
   DEFINE VARIABLE c-narrativa      AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-arquivo        AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
   DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.
   DEFINE VARIABLE c-tipo           AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-ordem          AS    CHARACTER             NO-UNDO FORMAT "x(300)".

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv".
   
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   OUTPUT STREAM st-excel TO VALUE(c-arquivo) NO-CONVERT NO-MAP.

   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado num-pedido         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado data               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado hora               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-usuario        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado num-ped-benef      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado natureza           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado data-pedido        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado situacao           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-emitente       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado end-entrega        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado end-cobranca       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado frete              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-transp         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado via-transp         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-cond-pag       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado responsavel        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-mensagem       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado impr-pedido        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado comentarios        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado mot-elimina        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nome-ass           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cargo-ass          1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado emergencial        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nr-prox-ped        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado contr-forn         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nr-processo        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado compl-entrega      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado l-tipo-ped         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado l-classificacao    1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado l-ind-prof         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-importador       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-situacao         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado c-cod-tabela       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-moeda            1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-cod-forma        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-cod-via          1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado c-prazo            1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado c-descr-merc       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-cod-porto        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado de-vl-fob          1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado c-embalagem        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado c-observacao       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-exportador       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado desc-forma         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado desc-via           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado de-vl-frete-i      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado ind-orig-entrada   1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado ind-via-envio      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nro-proc-entrada   1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nro-proc-saida     1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nro-proc-alteracao 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-maq-origem     1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado num-processo-mp    1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado num-id-documento   1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nr-contrato        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-estabel        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado check-sum          1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado gera-edi           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-estab-gestor   1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-emit-terc      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nr-ped-venda       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-entrega        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado endereco_text      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado endereco           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado bairro             1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cidade             1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado estado             1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado pais               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cep                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado jurisdicao         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado local-entrega      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   
   PUT STREAM st-excel SKIP.
       
   blk1:
   FOR EACH es-pedido-compr-eliminado
            WHERE es-pedido-compr-eliminado.num-pedido   >= tt-param.num-pedido-ini   AND
                  es-pedido-compr-eliminado.num-pedido   <= tt-param.num-pedido-fim   AND
                  es-pedido-compr-eliminado.cod-emitente >= tt-param.cod-emitente-ini AND
                  es-pedido-compr-eliminado.cod-emitente <= tt-param.cod-emitente-fim AND
                  es-pedido-compr-eliminado.data-pedido  >= tt-param.data-pedido-ini  AND
                  es-pedido-compr-eliminado.data-pedido  <= tt-param.data-pedido-fim  AND
                  es-pedido-compr-eliminado.data         >= tt-param.data-ini         AND
                  es-pedido-compr-eliminado.data         <= tt-param.data-fim         AND
                  es-pedido-compr-eliminado.cod-usuario  >= tt-param.cod-usuario-ini  AND
                  es-pedido-compr-eliminado.cod-usuario  <= tt-param.cod-usuario-fim  
            NO-LOCK:

      ASSIGN i-cont = i-cont + 1.

      RUN pi-acompanhar IN h-acomp ("Listando: " + STRING(i-cont)).

      PUT STREAM st-excel
          es-pedido-compr-eliminado.num-pedido            ";"
          es-pedido-compr-eliminado.data                  ";"
          es-pedido-compr-eliminado.hora                  ";"
          es-pedido-compr-eliminado.cod-usuario           ";"
          es-pedido-compr-eliminado.num-ped-benef         ";"
          es-pedido-compr-eliminado.natureza              ";"
          es-pedido-compr-eliminado.data-pedido           ";"
          es-pedido-compr-eliminado.situacao              ";"
          es-pedido-compr-eliminado.cod-emitente          ";"
          es-pedido-compr-eliminado.end-entrega           ";"
          es-pedido-compr-eliminado.end-cobranca          ";"
          es-pedido-compr-eliminado.frete                 ";"
          es-pedido-compr-eliminado.cod-transp            ";"
          es-pedido-compr-eliminado.via-transp            ";"
          es-pedido-compr-eliminado.cod-cond-pag          ";"
          es-pedido-compr-eliminado.responsavel           ";"
          es-pedido-compr-eliminado.cod-mensagem          ";"
          es-pedido-compr-eliminado.impr-pedido           ";"
          es-pedido-compr-eliminado.comentarios           ";"
          es-pedido-compr-eliminado.mot-elimina           ";"
          es-pedido-compr-eliminado.nome-ass              ";"
          es-pedido-compr-eliminado.cargo-ass             ";"
          es-pedido-compr-eliminado.emergencial           ";"
          es-pedido-compr-eliminado.nr-prox-ped           ";"
          es-pedido-compr-eliminado.contr-forn            ";"
          es-pedido-compr-eliminado.nr-processo           ";"
          es-pedido-compr-eliminado.compl-entrega         ";"
          es-pedido-compr-eliminado.l-tipo-ped            ";"
          es-pedido-compr-eliminado.l-classificacao       ";"
          es-pedido-compr-eliminado.l-ind-prof            ";"
          es-pedido-compr-eliminado.i-importador          ";"
          es-pedido-compr-eliminado.i-situacao            ";"
          es-pedido-compr-eliminado.c-cod-tabela          ";"
          es-pedido-compr-eliminado.i-moeda               ";"
          es-pedido-compr-eliminado.i-cod-forma           ";"
          es-pedido-compr-eliminado.i-cod-via             ";"
          es-pedido-compr-eliminado.c-prazo               ";"
          es-pedido-compr-eliminado.c-descr-merc          ";"
          es-pedido-compr-eliminado.i-cod-porto           ";"
          es-pedido-compr-eliminado.de-vl-fob             ";"
          es-pedido-compr-eliminado.c-embalagem           ";"
          es-pedido-compr-eliminado.c-observacao          ";"
          es-pedido-compr-eliminado.i-exportador          ";"
          es-pedido-compr-eliminado.desc-forma            ";"
          es-pedido-compr-eliminado.desc-via              ";"
          es-pedido-compr-eliminado.de-vl-frete-i         ";"
          es-pedido-compr-eliminado.ind-orig-entrada      ";"
          es-pedido-compr-eliminado.ind-via-envio         ";"
          es-pedido-compr-eliminado.nro-proc-entrada      ";"
          es-pedido-compr-eliminado.nro-proc-saida        ";"
          es-pedido-compr-eliminado.nro-proc-alteracao    ";"
          es-pedido-compr-eliminado.cod-maq-origem        ";"
          es-pedido-compr-eliminado.num-processo-mp       ";"
          es-pedido-compr-eliminado.num-id-documento      ";"
          es-pedido-compr-eliminado.nr-contrato           ";"
          es-pedido-compr-eliminado.cod-estabel           ";"
          es-pedido-compr-eliminado.check-sum             ";"
          es-pedido-compr-eliminado.gera-edi              ";"
          es-pedido-compr-eliminado.cod-estab-gestor      ";"
          es-pedido-compr-eliminado.cod-emit-terc         ";"
          es-pedido-compr-eliminado.nr-ped-venda          ";"
          es-pedido-compr-eliminado.cod-entrega           ";"
          es-pedido-compr-eliminado.endereco_text         ";"
          es-pedido-compr-eliminado.endereco              ";"
          es-pedido-compr-eliminado.bairro                ";"
          es-pedido-compr-eliminado.cidade                ";"
          es-pedido-compr-eliminado.estado                ";"
          es-pedido-compr-eliminado.pais                  ";"
          es-pedido-compr-eliminado.cep                   ";"
          es-pedido-compr-eliminado.jurisdicao            ";"
          es-pedido-compr-eliminado.local-entrega         ";".

      PUT STREAM st-excel SKIP.                                                                                       
   END.                                                                                                   

   OUTPUT STREAM st-excel CLOSE.
   DOS SILENT START excel.exe VALUE(c-arquivo).
END PROCEDURE.
*/

return "OK".



