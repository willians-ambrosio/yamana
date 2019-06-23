/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i AP0804RP 2.00.00.001}  /*** 010001 ***/
/******************************************************************************
***
***  app/ap0804rp.p - Listagem Clientes
***
*******************************************************************************/

{utp/ut-glob.i}
{app/ap0804d.i}

define temp-table tt-doc no-undo
       FIELD c-barra                      AS CHAR FORMAT "x(01)" INIT "/"
       FIELD c-barra2                     AS CHAR FORMAT "x(01)" INIT "/"
       FIELD cod-estabel                  LIKE titulo.cod-estabel
       FIELD cod-esp                      LIKE titulo.cod-esp
       FIELD serie                        LIKE titulo.serie
       FIELD nr-docto                     LIKE titulo.nr-docto
       FIELD parcela                      LIKE titulo.parcela
       FIELD portador                     LIKE tit-ap.portador
       FIELD modalidade                   LIKE tit-ap.modalidade
       FIELD vl-saldo                     LIKE titulo.vl-saldo
       FIELD vl-original                  LIKE titulo.vl-original
       FIELD dt-emissao                   LIKE tit-ap.dt-emissao
       FIELD dt-vencimen                  LIKE tit-ap.dt-vencimen
       FIELD cod-emitente                 LIKE titulo.cod-emitente
       FIELD nome-abrev                   LIKE titulo.nome-abrev
       FIELD dias-atraso                  LIKE tit-ap.dias-atraso.

DEFINE TEMP-TABLE tt-pag NO-UNDO
       FIELD c-barra        AS CHAR FORMAT "x(01)" INIT "/"
       FIELD c-barra2       AS CHAR FORMAT "x(01)" INIT "/"
       FIELD cod-emitente   LIKE emitente.cod-emitente
       FIELD nome-abrev     LIKE mov-ap.nome-abrev
       FIELD ep-codigo      LIKE mov-ap.ep-codigo
       FIELD cod-estabel    LIKE mov-ap.cod-estabel
       FIELD cod-esp        LIKE mov-ap.cod-esp
       FIELD nr-docto       LIKE mov-ap.nr-docto
       FIELD parcela        LIKE mov-ap.parcela
       FIELD portador       LIKE mov-ap.portador
       FIELD modalidade     LIKE mov-ap.modalidade 
       FIELD dt-vencimen    LIKE mov-ap.dt-vencimen
       FIELD dt-transacao   LIKE mov-ap.dt-transacao
       FIELD valor-baixa    LIKE mov-ap.valor-mov
       FIELD de-dias-atraso AS INT.

/*** Defini‡Æo do Parametr“s do Programa ***/

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.
def input param table for tt-doc.
def input param table for tt-pag.  
def input param table for tt-documento.        
def input param r-emitente       as rowid no-undo.
def input param fi-total-titulos as dec no-undo.
def input param de-tot-aberto    as dec format "->,>>>,>>>,>>>,>>9.99" no-undo.
def input param de-tot-anterior  as dec format "->,>>>,>>>,>>>,>>9.99" no-undo.
DEF VAR de-total-baixa           AS DEC FORMAT "->>>,>>>,>>>,>>>,>>9.99" no-undo.
DEF INPUT PARAM cod-escolha      AS INT NO-UNDO.
DEF VAR l-venct-matriz                   AS LOG NO-UNDO.
/*** Defini‡äes de Vari veis ***/


DEF VAR c-label-fornecedor               AS CHAR FORMAT "X(10)" NO-UNDO.
DEF VAR c-label-total-das-baixas         AS CHAR FORMAT "X(20)" no-undo.
DEF VAR c-label-total-saldo-aberto       AS CHAR FORMAT "x(19)" NO-UNDO.
DEF VAR c-label-sald-anterior-do-cliente AS CHAR FORMAT "x(26)" NO-UNDO.
DEF VAR i-codigo                         AS INT FORMAT ">>>>>>>>9" NO-UNDO.

/*--- Form's do Relat¢rio ---*/
{app/ap0804n.i3}

FORM
        tt-doc.cod-emitente COLUMN-LABEL "C¢digo" 
        tt-doc.nome-abrev   COLUMN-LABEL "Nome Abreviado"  
        WITH DOWN NO-ATTR-SPACE STREAM-IO WIDTH 118 FRAME f-forn-doc.

form
        tt-doc.cod-estabel    COLUMN-LABEL "Est"  
        tt-doc.cod-esp        COLUMN-LABEL "Esp"
        tt-doc.serie          COLUMN-LABEL "S‚rie"
        tt-doc.nr-docto       COLUMN-LABEL "Documento" SPACE(0)
        tt-doc.c-barra        COLUMN-LABEL "/"         SPACE(0)
        tt-doc.parcela        COLUMN-LABEL "P"         
        tt-doc.port           COLUMN-LABEL "Port"      SPACE(0)
        tt-doc.c-barra2       COLUMN-LABEL "/"         SPACE(0)
        tt-doc.modalidade     COLUMN-LABEL "M"
        tt-doc.vl-saldo       COLUMN-LABEL "Valor Saldo"
        tt-doc.dt-emissao     COLUMN-LABEL "EmissÆo"
        tt-doc.dias-atraso    COLUMN-LABEL "Dias" 
        tt-doc.vl-original    COLUMN-LABEL "Vl Original"
        tt-doc.dt-vencimen    COLUMN-LABEL "Vencto"
        WITH DOWN NO-ATTR-SPACE STREAM-IO WIDTH 139 FRAME f-doc.

FORM    
        c-label-total-saldo-aberto       NO-LABEL AT  59
        de-tot-aberto                    NO-LABEL
        c-label-sald-anterior-do-cliente NO-LABEL AT 52
        de-tot-anterior                NO-LABEL
        WITH SIDE-LABELS OVERLAY NO-ATTR-SPACE STREAM-IO WIDTH 118 ROW 2 FRAME f-forn-doc-total.

form
         c-label-fornecedor  NO-LABEL AT 1 
         tt-pag.cod-emitente NO-LABEL AT 14 "-"
         tt-pag.nome-abrev   NO-LABEL 
         WITH SIDE-LABELS OVERLAY NO-ATTR-SPACE STREAM-IO WIDTH 80 ROW 2 FRAME f-fornecedor.
FORM
        tt-pag.ep-codigo      COLUMN-LABEL "Emp" AT 9
        tt-pag.cod-estabel    COLUMN-LABEL "Est"  
        tt-pag.cod-esp        COLUMN-LABEL "Esp"
        tt-pag.nr-docto       COLUMN-LABEL "Documento" SPACE(0)
        tt-pag.c-barra        COLUMN-LABEL "/"         SPACE(0)
        tt-pag.parcela        COLUMN-LABEL "P"         
        tt-pag.portador       COLUMN-LABEL "Port"      SPACE(0)
        tt-pag.c-barra2       COLUMN-LABEL "/"         SPACE(0)
        tt-pag.modalidade     COLUMN-LABEL "M"
        tt-pag.dt-vencimen    COLUMN-LABEL "Data Venc."
        tt-pag.dt-transacao   COLUMN-LABEL "Data Tran."
        tt-pag.valor-baixa    COLUMN-LABEL "Valor Baixa"
        tt-pag.de-dias-atraso COLUMN-LABEL "Dias"
        WITH DOWN NO-ATTR-SPACE STREAM-IO WIDTH 118 FRAME f-pag TITLE " Baixas do Cliente ".

FORM 
        c-label-total-das-baixas NO-LABEL AT 53
        de-total-baixa NO-LABEL
        WITH SIDE-LABELS OVERLAY NO-ATTR-SPACE STREAM-IO WIDTH 118 ROW 2 FRAME f-pag-total.

{include/i-rpvar.i}

    find first empresa no-lock where empresa.ep-codigo = i-ep-codigo-usuario no-error.
    if  not avail empresa then
        return.

{utp/ut-liter.i CONTAS_A_PAGAR}
assign c-programa = "AP0804":U
       c-empresa  = empresa.nome  
       c-sistema  = trim(return-value).

create tt-param.
raw-transfer raw-param to tt-param. 

run utp/ut-trfrrp.p (input frame f-pag-total:handle).
run utp/ut-trfrrp.p (input frame f-fornecedor:handle).
run utp/ut-trfrrp.p (input frame f-forn-doc-total:handle).
run utp/ut-trfrrp.p (input frame f-doc:handle).
run utp/ut-trfrrp.p (input frame f-forn-doc:handle).
{include/i-rpcab.i}
{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.

ASSIGN l-venct-matriz = NO.
&IF DEFINED(BF_FIN_VENCT_MATRIZ) &THEN
    ASSIGN l-venct-matriz = YES.
&else
    IF CAN-FIND(funcao
                WHERE funcao.cd-funcao = 'spp-venct-matriz' 
                AND   funcao.ativo = yes) THEN
        ASSIGN l-venct-matriz = YES.
&ENDIF
IF cod-escolha = 1 THEN DO:
    {utp/ut-liter.i Listando_dados_do_Fornecedor}
    assign c-titulo-relat = trim(return-value).

    IF l-venct-matriz THEN DO:
        FOR EACH tt-doc NO-LOCK 
            BREAK BY tt-doc.cod-emitente
                  BY tt-doc.dt-vencimen:
            IF FIRST-OF(tt-doc.cod-emitente) THEN 
                DISP tt-doc.cod-emitente
                     tt-doc.nome-abrev
                     WITH FRAME f-forn-doc.

            DISP tt-doc.cod-estabel  
                 tt-doc.cod-esp         
                 tt-doc.serie           
                 tt-doc.nr-docto        
                 tt-doc.parcela
                 tt-doc.portador
                 tt-doc.modalidade
                 tt-doc.vl-saldo
                 tt-doc.dt-emissao 
                 tt-doc.dias-atraso
                 tt-doc.vl-original
                 tt-doc.dt-vencimen WITH FRAME f-doc.
                 DOWN WITH FRAME f-doc.
        END.
    END.
    ELSE DO:
        FOR EACH tt-doc NO-LOCK BREAK BY tt-doc.cod-emitente:
            IF FIRST-OF(tt-doc.cod-emitente) THEN 
                DISP tt-doc.cod-emitente
                     tt-doc.nome-abrev
                     WITH FRAME f-forn-doc.
            DISP tt-doc.cod-estabel  
                 tt-doc.cod-esp         
                 tt-doc.serie           
                 tt-doc.nr-docto        
                 tt-doc.parcela
                 tt-doc.portador
                 tt-doc.modalidade
                 tt-doc.vl-saldo
                 tt-doc.dt-emissao 
                 tt-doc.dias-atraso
                 tt-doc.vl-original
                 tt-doc.dt-vencimen WITH FRAME f-doc.
                 DOWN WITH FRAME f-doc.
        END.
    END.
    {utp/ut-liter.i Total_Saldo_Aberto:}
    ASSIGN c-label-total-saldo-aberto = TRIM (RETURN-VALUE).

    {utp/ut-liter.i Saldo_Anterior_do_Cliente:}
    ASSIGN c-label-sald-anterior-do-cliente = TRIM (RETURN-VALUE).

    DISP c-label-total-saldo-aberto
         de-tot-aberto format "->,>>>,>>>,>>>,>>9.99"
         c-label-sald-anterior-do-cliente
         de-tot-anterior format "->,>>>,>>>,>>>,>>9.99"
         WITH FRAME f-forn-doc-total.
END.
else do:
    IF cod-escolha = 2  THEN DO:
        {utp/ut-liter.i Consulta_das_Baixas}
        assign c-titulo-relat = trim(return-value).

        {utp/ut-liter.i Fornecedor:}
        ASSIGN c-label-fornecedor = trim(RETURN-VALUE).

        ASSIGN de-total-baixa = 0.

        FOR EACH tt-pag NO-LOCK BREAK BY tt-pag.cod-emitente:
            IF FIRST-OF(tt-pag.cod-emitente) THEN
                 DISP c-label-fornecedor
                      tt-pag.cod-emitente
                      tt-pag.nome-abrev
                      WITH FRAME f-fornecedor.
            DISP tt-pag.ep-codigo 
                 tt-pag.cod-estabel  
                 tt-pag.cod-esp      
                 tt-pag.nr-docto        
                 tt-pag.parcela  
                 tt-pag.portador
                 tt-pag.modalidade
                 tt-pag.dt-vencimen
                 tt-pag.dt-transacao
                 tt-pag.valor-baixa
                 tt-pag.de-dias-atraso
                 WITH FRAME f-pag.
            DOWN WITH FRAME f-pag.
            ASSIGN de-total-baixa = de-total-baixa + tt-pag.valor-baixa.
        END.
        {utp/ut-liter.i Total_das_Baixas:}
        ASSIGN c-label-total-das-baixas = TRIM (RETURN-VALUE).
        DISP c-label-total-das-baixas
             de-total-baixa
             WITH FRAME f-pag-total.
    END.
    ELSE DO: 
        IF cod-escolha = 3 THEN DO: 
            find first emitente 
                where rowid(emitente) = r-emitente NO-LOCK no-error.
            if  avail emitente then do:
                {utp/ut-liter.i Listando_dados_do_Cliente * C}
                assign c-titulo-relat = trim(return-value).

                disp emitente.cod-emitente
                     emitente.nome-abrev 
                     with frame f-emitente.
                down with frame f-emitente.

                if  tt-param.tipo-relat = 1 then
                    view frame f-cab-corpo.
                else    
                    view frame f-cab-corpo-resumido.    
                assign de-tot-original = 0
                       de-tot-saldo    = 0
                       de-tot-juros    = 0
                       de-total-saldo  = 0.
                for each tt-documento:
                    if  tt-param.tipo-relat = 1 then do:
                        disp tt-documento.cod-estabel    format "x(4)"            no-label
                             tt-documento.cod-esp                                 no-label
                             tt-documento.serie          format "x(6)"            no-label
                             tt-documento.nr-docto                                no-label
                             tt-documento.parcela        format "x(3)"            no-label
                             tt-documento.cod-port       format ">>>>9"           no-label
                             tt-documento.modalidade                              no-label
                             tt-documento.de-saldo       format "->>>,>>>,>>9.99" no-label
                             tt-documento.de-juros       format "->>>,>>>,>>9.99" no-label
                             tt-documento.de-total-juros format "->>>,>>>,>>9.99" no-label
                             tt-documento.vl-original    format "->>>,>>>,>>9.99" no-label
                             tt-documento.dt-emissao                              no-label
                             tt-documento.dt-vencimen                             no-label 
                             tt-documento.i-dias         format "->>>>9"          no-label 
                             with frame f-documento.
                        down with frame f-documento.
                    end.
                    else do:
                        disp tt-documento.cod-estabel    format "x(4)"            no-label
                             tt-documento.cod-esp                                 no-label
                             tt-documento.serie          format "x(6)"            no-label
                             tt-documento.nr-docto                                no-label
                             tt-documento.parcela        format "x(2)"            no-label
                             tt-documento.cod-port       format ">>>>9"           no-label
                             tt-documento.modalidade                              no-label
                             tt-documento.de-saldo                                no-label
                             tt-documento.de-juros       format "->>,>>9.99"      no-label
                             tt-documento.de-total-juros format "->>,>>>,>>9.99"  no-label
                             tt-documento.vl-original                             no-label
                             tt-documento.dt-emissao                              no-label
                             tt-documento.dt-vencimen                             no-label 
                             tt-documento.i-dias                                  no-label 
                             with frame f-documento-resumido.
                        down with frame f-documento-resumido.
                    end.        

                    find esp-doc where esp-doc.cod-esp = tt-documento.cod-esp no-lock no-error.       

                    if  avail esp-doc then do:
                        if (esp-doc.tipo = 2) then do:
                            assign de-tot-original = de-tot-original - tt-documento.vl-original
                                   de-tot-saldo    = de-tot-saldo    - tt-documento.de-saldo
                                   de-tot-juros    = de-tot-juros    - tt-documento.de-juros
                                   de-total-saldo  = de-total-saldo  - tt-documento.de-total-juros.
                        end.           
                        else do: 
                             assign de-tot-original = de-tot-original + tt-documento.vl-original
                                    de-tot-saldo    = de-tot-saldo    + tt-documento.de-saldo
                                    de-tot-juros    = de-tot-juros    + tt-documento.de-juros
                                    de-total-saldo  = de-total-saldo  + tt-documento.de-total-juros.
                        end.
                    end.        
                    if  line-counter >= 62 then do:
                        page.
                        if  tt-param.tipo-relat = 1 then
                            view frame f-cab-corpo.
                        else    
                            view frame f-cab-corpo-resumido.

                    end.    
                end.
                if tt-param.tipo-relat = 1 then do:
                    underline tt-documento.vl-original    
                              tt-documento.de-saldo       
                              tt-documento.de-juros       
                              tt-documento.de-total-juros with frame f-documento.    

                    disp de-tot-original @ tt-documento.vl-original     format "->>,>>>,>>>,>>9.99" no-label
                         de-tot-saldo    @ tt-documento.de-saldo        format "->>,>>>,>>>,>>9.99" no-label
                         de-tot-juros    @ tt-documento.de-juros        format "->>,>>>,>>>,>>9.99" no-label
                         de-total-saldo  @ tt-documento.de-total-juros  format "->>,>>>,>>>,>>9.99" no-label
                         with frame f-documento.
                end.
                else do:
                    underline tt-documento.vl-original    
                              tt-documento.de-saldo       
                              tt-documento.de-juros       
                              tt-documento.de-total-juros with frame f-documento-resumido.    

                    disp de-tot-original @ tt-documento.vl-original     format "->>,>>>,>>>,>>9.99" no-label
                         de-tot-saldo    @ tt-documento.de-saldo        format "->>,>>>,>>9.99"     no-label
                         de-tot-juros    @ tt-documento.de-juros        format "->>,>>9.99"         no-label
                         de-total-saldo  @ tt-documento.de-total-juros  format "->>,>>>,>>9.99"     no-label
                         with frame f-documento-resumido.    
                end.         

                disp de-tot-aberto
                     de-tot-anterior 
                     with frame f-total.
                down with frame f-total.     
            end.
        END.
    END.
END.

{include/i-rpclo.i} 

return "OK".





