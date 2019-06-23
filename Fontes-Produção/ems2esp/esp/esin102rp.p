/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa     for ems2cadme.empresa.

{include/i-prgvrs.i esin102rp 2.00.00.015}  /*** 010015 ***/
{include/i_fnctrad.i}
/******************************************************************************
**
**   Programa: esin102rp.p
**
**   Data....: Marco de 2008.
**
**   Autor...: DSC - Maria Aparecida L. Nogueira
**
**   Objetivo: Relat¢rio de Investimentos Sumarizado
**  
*******************************************************************************/
{include/i-epc200.i esin102rp}

{include/i-rpvar.i}

def var h-acomp as handle no-undo.
def var c-titulo-i       as char format "x(20)"  no-undo.
def var c-titulo-p       as char format "x(20)"  no-undo.
def var c-destino        as char format "x(7)"   no-undo.             
def var c-titulo-s       as char format "x(20)"  no-undo.   
def var c-titulo-c       as char format "x(20)"  no-undo.   
def var c-titulo-par     as char format "x(20)"  no-undo.
def var l-imprime        as log  initial yes     no-undo.
def var c-geral          as char format "x(20)"  no-undo.
def var c-verba          as char format "x(20)"  no-undo.
def var c-situacao       as char format "x(11)"  no-undo.
def var c-situacao-1     as char format "x(11)"  no-undo.
def var c-situacao-2     as char format "x(11)"  no-undo.
def var c-exib-moeda     as char format "x(20)"  no-undo.
def var c-lb-exib-moeda  as char format "x(25)"  no-undo.
def var c-emp-sel        as char format "x(10)"  no-undo.
def var c-est-sel        as char format "x(10)"  no-undo.
def var c-proj-sel       as char format "x(10)"  no-undo.
def var c-ord-sel        as char format "x(10)"  no-undo.
def var c-imprimindo     as char format "x(13)"  no-undo. 
def var c-processando    as char format "x(13)"  no-undo. 
def var c-moeda-controle as char format "x(15)"  no-undo.
def var c-moedas-inv     as char format "x(15)"  no-undo extent 3.
def var c-lb-proj        as char format "x(5)"   no-undo.
def var i-cod-moeda-par  as integer              no-undo.
def var c-lb-dest        as char                 no-undo.
def var c-lb-usuar       as char                 no-undo.
def var c-lb-atualiza    as char                 no-undo.
def var c-atualiza       as char                 no-undo.
def var c-lb-moeda-controle as char format "x(10)" no-undo.
def var c-lb-situacao    as char format "x(10)"  no-undo.
def var l-imprimiu       as logical              no-undo.

def var c-lb-total       as char format "x(10)" no-undo.
def var c-lb-dt-trans    as char format "x(10)" no-undo.
def var c-lb-tipo-doc    as char format "x(10)" no-undo.
def var c-lb-serie       as char format "x(05)" no-undo.
def var c-lb-docto       as char format "x(25)" no-undo.
def var c-lb-ent-comp    as char format "x(14)" no-undo.    
def var c-lb-sai-comp    as char format "x(14)" no-undo.
def var c-lb-ent-real    as char format "x(14)" no-undo.
def var c-lb-sai-real    as char format "x(14)" no-undo.
def var c-lb-acum-comp   as char format "x(16)" no-undo. 
def var c-lb-acum-real   as char format "x(16)" no-undo. 
def var c-lb-solicitacao as char                no-undo.
def var c-lb-ordem       as char                no-undo.
def var c-lb-pedido      as char                no-undo.
def var c-lb-especie     as char                no-undo.     
def var c-lb-item        as char                no-undo.
def var c-lb-num-ord-magnus as char             no-undo.

{cdp/cd9731.i}  /* l-integra-cn-in */
{cdp/cd9731.i1} /* l-integra-cn-in-medicao */

def buffer b-controle-verba for controle-verba.

def var de-acum-comp     as dec                 no-undo.
def var de-acum-real     as dec                 no-undo.
def var de-acum-tot      as dec                 no-undo.
def var de-total         as dec                 no-undo.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classif-1        as integer
    field desc-classifica  as char format "x(40)"
    field i-ep-ini         as char 
    field i-ep-fim         as char
    field c-est-ini        as char
    field c-est-fim        as char
    field i-proj-ini       as int 
    field i-proj-fim       as int
    field i-ord-ini        as int
    field i-ord-fim        as int
    field i-moeda-par      as int
    FIELD i-data-ini       AS DATE
    FIELD i-data-fim       AS DATE
    field arquivo-destino  as char format "x(35)"
    field atualiza         as logical.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem. 

def temp-table tt-raw-digita                   
    field raw-digita as raw.
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.


DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE iTotalNumberOfOrders    AS INTEGER.
DEFINE VARIABLE iMonth                  AS INTEGER.
DEFINE VARIABLE dAnnualQuota            AS DECIMAL.
DEFINE VARIABLE dTotalSalesAmount       AS DECIMAL.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 3.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.

DEF VAR COUNT   AS INT.
DEF VAR i-linha AS INT.
DEF VAR c-range AS CHAR.


/* Defini‡Æo das frames e tradu‡Æo*/
{esp/esin101.i2}

DEF VAR de-ent-real LIKE t-controle-3.ent-real.
DEF VAR de-vl-jan   LIKE t-controle-3.ent-real.
DEF VAR de-vl-fev   LIKE t-controle-3.ent-real.
DEF VAR de-vl-mar   LIKE t-controle-3.ent-real.
DEF VAR de-vl-abr   LIKE t-controle-3.ent-real.
DEF VAR de-vl-mai   LIKE t-controle-3.ent-real.
DEF VAR de-vl-jun   LIKE t-controle-3.ent-real.
DEF VAR de-vl-jul   LIKE t-controle-3.ent-real.
DEF VAR de-vl-ago   LIKE t-controle-3.ent-real.
DEF VAR de-vl-set   LIKE t-controle-3.ent-real.
DEF VAR de-vl-out   LIKE t-controle-3.ent-real.
DEF VAR de-vl-nov   LIKE t-controle-3.ent-real.
DEF VAR de-vl-dez   LIKE t-controle-3.ent-real.


DEF VAR tot-comprom             LIKE t-controle-1.ent-comp.
DEF VAR tot-vl-verba            LIKE  ordem-inv.vl-verba[1].
DEF VAR c-desc-ord              AS CHAR FORMAT "x(40)". 
DEF VAR tot-ger-vl-verba        LIKE  tot-vl-verba  .
DEF VAR tot-ger-ent-real        LIKE de-ent-real.  
DEF VAR tot-ger-comprom         LIKE tot-comprom.
DEF VAR tot-compr-ate           LIKE tot-comprom.
DEF VAR tot-de-ent-real-ate     LIKE  t-controle-3.ent-real.
DEF VAR tot-ger-compr-ate       LIKE  tot-compr-ate.        
DEF VAR tot-ger-de-ent-real-ate LIKE  tot-de-ent-real-ate. 

DEF VAR ttt-valor             LIKE t-controle-1.ent-comp.
DEF VAR tot-current           LIKE controle-verba.vl-real[1].
DEF VAR tot-commit            LIKE controle-verba.vl-real[1].


find first param-global no-lock no-error.

{utp/ut-liter.i Listagem_Verba_Ordem_de_Investimento_-_Detalhada * r }
assign c-titulo-relat = return-value.
{utp/ut-liter.i INVESTIMENTO  * r }
assign c-programa     = "esin102" 
       c-versao       = "I.00"
       c-revisao      = "000"
       c-sistema      = return-value.

&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
    DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
    ASSIGN cAuxTraducao001 = {varinc/var00002.i 04 tt-param.destino}.
    run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                        INPUT "",
                        INPUT "").
    ASSIGN  c-destino = RETURN-VALUE.
&else
    ASSIGN c-destino = {varinc/var00002.i 04 tt-param.destino}.
&endif
run INIC-FORMS.  /* gera cabecalho e rodape de 233 cols */

run utp/ut-acomp.p persistent set h-acomp.
                        
/* Monta valores na temp-table */
def var aux-i-data-ini like tt-param.i-data-ini.
def var aux-i-data-fim like tt-param.i-data-fim.


{esp/esin102.i1} /* teste */ /* era esin102.i1 */   


{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.


FOR EACH t-controle NO-LOCK:
    ASSIGN t-controle.valor = t-controle.ent-comp.
    IF t-controle.tipo-doc = "imd" OR
       t-controle.tipo-doc = "pef" THEN
        ASSIGN t-controle.valor = t-controle.ent-real
               t-controle.ent-comp = t-controle.ent-real.

    RELEASE contrato-for. 
    IF t-controle.nr-contrato <> 0 THEN DO:
        FIND FIRST contrato-for OF t-controle NO-LOCK NO-ERROR.
        IF AVAIL contrato-for THEN
            ASSIGN t-controle.dt-contrato = contrato-for.dt-ini-validade
                   t-controle.valor = contrato-for.dec-2
                   t-controle.valor-cont = contrato-for.dec-2.
    END.

    IF t-controle.num-pedido <> 0 THEN DO:
        FIND FIRST pedido-compr OF t-controle NO-LOCK NO-ERROR.
        IF AVAIL pedido-compr THEN
            ASSIGN t-controle.cod-emitente = pedido-compr.cod-emitente.
    END.


    IF t-controle.tipo-doc = "imd" OR
       t-controle.tipo-doc = "pef" THEN DO:
        ASSIGN t-controle.cod-emitente = int(ENTRY(4,t-controle.num-docto,"/")).
    END.


    FIND FIRST t-controle-1 OF t-controle NO-ERROR.
    IF NOT AVAIL t-controle-1 THEN DO:
        CREATE t-controle-1.
        BUFFER-COPY t-controle TO t-controle-1.
    END.
    ELSE DO:
        ASSIGN t-controle-1.ent-comp = t-controle-1.ent-comp + t-controle.ent-comp.
               
        IF NOT AVAIL contrato-for THEN
            ASSIGN t-controle-1.valor = t-controle-1.valor + t-controle.valor.

        IF t-controle-1.descricao = "" THEN 
            ASSIGN t-controle-1.descricao = t-controle.descricao.
        ELSE
            ASSIGN t-controle-1.descricao = t-controle-1.descricao + " / " + t-controle.descricao.
    END.

END.

/************
FOR EACH t-controle NO-LOCK:
    ASSIGN t-controle.valor = t-controle.ent-comp.
    IF t-controle.tipo-doc = "imd" OR
       t-controle.tipo-doc = "pef" THEN
        ASSIGN t-controle.valor = t-controle.ent-real
               t-controle.ent-comp = t-controle.ent-real.

    RELEASE contrato-for. 
    IF t-controle.nr-contrato <> 0 THEN DO:
        FIND FIRST contrato-for OF t-controle NO-LOCK NO-ERROR.
        IF AVAIL contrato-for THEN
            ASSIGN t-controle.dt-contrato = contrato-for.dt-ini-validade
                   t-controle.valor = contrato-for.dec-2
                   t-controle.valor-cont = contrato-for.dec-2.
    END.

    IF t-controle.num-pedido <> 0 THEN DO:
        FIND FIRST pedido-compr OF t-controle NO-LOCK NO-ERROR.
        IF AVAIL pedido-compr THEN
            ASSIGN t-controle.cod-emitente = pedido-compr.cod-emitente.
    END.


    IF t-controle.tipo-doc = "imd" OR
       t-controle.tipo-doc = "pef" THEN DO:
        ASSIGN t-controle.cod-emitente = int(ENTRY(4,t-controle.num-docto,"/")).
    END.


    FIND FIRST t-controle-1 OF t-controle NO-ERROR.
    IF NOT AVAIL t-controle-1 THEN DO:
        CREATE t-controle-1.
        BUFFER-COPY t-controle TO t-controle-1.
    END.
    ELSE DO:
        ASSIGN t-controle-1.ent-comp = t-controle-1.ent-comp + t-controle.ent-comp.
               
        IF NOT AVAIL contrato-for THEN
            ASSIGN t-controle-1.valor = t-controle-1.valor + t-controle.valor.

        IF t-controle-1.descricao = "" THEN 
            ASSIGN t-controle-1.descricao = t-controle.descricao.
        ELSE
            ASSIGN t-controle-1.descricao = t-controle-1.descricao + " / " + t-controle.descricao.
    END.

END.
******************/

/*************** esin0519 ***********/

for each controle-verba
    where controle-verba.ep-codigo    >= tt-param.i-ep-ini
      and controle-verba.ep-codigo    <= tt-param.i-ep-fim
      and controle-verba.cod-est-exec >= tt-param.c-est-ini
      and controle-verba.cod-est-exec <= tt-param.c-est-fim      
      and controle-verba.num-projeto  >= tt-param.i-proj-ini
      and controle-verba.num-projeto  <= tt-param.i-proj-fim
      and controle-verba.num-ordem    >= tt-param.i-ord-ini
      and controle-verba.num-ordem    <= tt-param.i-ord-fim no-lock
      break by controle-verba.ep-codigo
            by controle-verba.cod-est-exec
            by controle-verba.num-projeto
            by controle-verba.num-ordem:

      find param-inv 
           where param-inv.ep-codigo = controle-verba.ep-codigo no-lock no-error.     
      if first-of (controle-verba.ep-codigo) then do:
         if tt-param.i-moeda-par = 1 then
            assign i-cod-moeda-par = param-inv.moeda-inv.
         else if tt-param.i-moeda-par = 2 then
            assign i-cod-moeda-par = param-inv.moeda1.
         else
            assign i-cod-moeda-par = param-inv.moeda2.
      end.   

      run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                         string(controle-verba.num-projeto) + "  " + 
                                         c-lb-ordem + 
                                         string(controle-verba.num-ordem))).
     

      
      for each movto-nf
         where movto-nf.ep-codigo = controle-verba.ep-codigo
           and movto-nf.cod-est-exec = controle-verba.cod-est-exec
           and movto-nf.num-projeto = controle-verba.num-projeto
           and movto-nf.num-ordem = controle-verba.num-ordem
           /*and movto-nf.dt-trans >= tt-param.i-data-ini */
           and movto-nf.dt-trans <= tt-param.i-data-fim
           no-lock:

           find plano-aprov where
                plano-aprov.ep-codigo = movto-nf.ep-codigo and
                plano-aprov.cod-area = movto-nf.cod-area and
                plano-aprov.num-ord-comp = movto-nf.num-ord-comp and
                plano-aprov.seq-comp = movto-nf.seq-comp no-lock no-error.
           if not avail plano-aprov then next.     

           find ord-ped where 
                   ord-ped.ep-codigo = plano-aprov.ep-codigo and
                   ord-ped.cod-estabel = plano-aprov.cod-estabel and
                   ord-ped.cod-area = 0 and
                   ord-ped.num-ord-comp = plano-aprov.num-ord-comp and
                   ord-ped.seq-comp = plano-aprov.seq-comp no-lock no-error.
           if not avail ord-ped then next.  

           create t-controle-3.
           assign t-controle-3.ep-codigo    = movto-nf.ep-codigo
                  t-controle-3.cod-est-exec = movto-nf.cod-est-exec
                  t-controle-3.num-projeto  = movto-nf.num-projeto
                  t-controle-3.num-ordem    = movto-nf.num-ordem 
                  t-controle-3.it-codigo    = plano-aprov.it-codigo
                  t-controle-3.num-ord-magnus = plano-aprov.num-ord-magnus
                  t-controle-3.tipo-doc     = "Nota Fisc"
                  t-controle-3.dt-trans     = movto-nf.dt-trans
                  t-controle-3.num-ord-comp = movto-nf.num-ord-comp
                  t-controle-3.seq-comp     = ord-ped.seq-comp
                  t-controle-3.solicitacao  = plano-aprov.num-solicitacao
                  t-controle-3.cod-emitente = movto-nf.cod-emitente
                  t-controle-3.nr-contrato  = ord-ped.nr-contrato
                  t-controle-3.num-pedido   = movto-nf.num-pedido
                  t-controle-3.num-docto    = movto-nf.serie-docto + "/" +
                                            substring(movto-nf.nro-docto,1,10) + "/" +
                                            movto-nf.nat-operacao /*+ "/" +
                                            string(movto-nf.sequencia,">>>>9" )*/
                 /* t-controle.num-docto  = substring(movto-nf.nro-docto,1,10)*/
                  t-controle-3.ent-real     = movto-nf.vl-nota[1].
                  
          assign t-controle-3.preco-total  = movto-nf.vl-nota[1]. 

         
          find first item-doc-est of movto-nf no-lock no-error.    
          if avail item-doc-est then do:
              assign t-controle-3.ent-real = item-doc-est.preco-total[1] + item-doc-est.despesas[1] + item-doc-est.valor-ipi[1]
                     t-controle-3.l-nota = yes.
          end.      
          
           assign t-controle-3.imposto = t-controle-3.ent-real - movto-nf.vl-nota[1].
           

            run cdp/cd0812.p (input  param-inv.moeda-inv,
                              input  i-cod-moeda-par,
                              input  t-controle-3.ent-real,
                              input  movto-nf.dt-trans,
                              output t-controle-3.ent-real).  

           if ord-ped.cod-sit-comp <> "E" and ord-ped.cod-sit-ped <> "E" then do:                  
                assign t-controle-3.sai-comp = movto-nf.quant-doc-ent * 
                                             (ord-ped.vl-item[1] / ord-ped.quant-comp).
                if ord-ped.data-2 <> ? then                             
                   run cdp/cd0812.p (input  param-inv.moeda-inv,
                                     input  i-cod-moeda-par,
                                     input  t-controle-3.sai-comp,
                                     input  ord-ped.data-2,
                                     output t-controle-3.sai-comp).              
                else                                                 
                   run cdp/cd0812.p (input  param-inv.moeda-inv,
                                     input  i-cod-moeda-par,
                                     input  t-controle-3.sai-comp,
                                     input  ord-ped.data-1,
                                     output t-controle-3.sai-comp).

           end.

           /* Devolu‡Æo ao Fornecedor: Gera entrada no compromissado e sa¡da no realizado */
           &IF "{&bf_mat_versao_EMS}" >= "2.04" &THEN
               if  movto-nf.tipo-trans = 2 then
                   assign t-controle-3.imposto  = t-controle-3.imposto * -1
                          t-controle-3.pis  = t-controle-3.pis * -1
                          t-controle-3.cofins  = t-controle-3.cofins * -1
                          t-controle-3.icm  = t-controle-3.icm * -1
                          t-controle-3.ipi  = t-controle-3.ipi * -1
                          t-controle-3.preco-total  = t-controle-3.preco-total * -1
                          t-controle-3.sai-real = t-controle-3.ent-real
                          t-controle-3.ent-real = 0
                          t-controle-3.ent-comp = t-controle-3.sai-comp
                          t-controle-3.sai-comp = 0.
           &endif.

           if plano-aprov.num-solicitacao <> 0 then
               assign t-controle-3.seq = 10.
           else
               assign t-controle-3.seq = 20.

           /* {esp/esin0519.i3} */

               if t-controle-3.ent-comp = ? then assign t-controle-3.ent-comp = 0.
               if t-controle-3.sai-comp = ? then assign t-controle-3.sai-comp = 0.
               if t-controle-3.ent-real = ? then assign t-controle-3.ent-real = 0.
               if t-controle-3.sai-real = ? then assign t-controle-3.sai-real = 0.

      end.     

      

        /** CHAMADA UPC **/   

        for each tt-epc:
            delete tt-epc.
        end.    

        create tt-epc.
        assign tt-epc.cod-event     = "apos-leitura-movto-nf":U
               tt-epc.cod-parameter = "controle-verba-rowid"
               tt-epc.val-parameter = string(rowid(controle-verba)). 

        {include/i-epc201.i "apos-leitura-movto-nf"}        

        if return-value = 'NOK' then do:
            undo,leave.
        end.

        /********************/      
    
     for each movto-apr
         where movto-apr.ep-codigo = controle-verba.ep-codigo
           and movto-apr.cod-est-exec = controle-verba.cod-est-exec
           and movto-apr.num-projeto = controle-verba.num-projeto
           and movto-apr.num-ordem = controle-verba.num-ordem
           /* and movto-apr.dt-trans >= tt-param.i-data-ini */
           and movto-apr.dt-trans <= tt-param.i-data-fim
           no-lock:      
           if movto-apr.transacao = "IMD" and
              movto-apr.num-pedido <> 0 then next.             

           if movto-apr.transacao = "REQ" then next.                                 
           if movto-apr.transacao = "DIV" then next.                                 


           create t-controle-3.
           assign t-controle-3.ep-codigo    = movto-apr.ep-codigo
                  t-controle-3.cod-est-exec = movto-apr.cod-est-exec
                  t-controle-3.num-projeto  = movto-apr.num-projeto
                  t-controle-3.num-ordem    = movto-apr.num-ordem
                  t-controle-3.it-codigo    = movto-apr.it-codigo
                  t-controle-3.num-ord-magnus = movto-apr.num-ord-magnus
                  t-controle-3.cod-emitente = movto-apr.cod-emitente
                  t-controle-3.dt-trans     = movto-apr.dt-trans 
                  t-controle-3.esp-docto    = movto-apr.esp-docto
                  t-controle-3.num-docto    = movto-apr.nro-docto
                  t-controle-3.ent-real     = movto-apr.vl-mat[1] + 
                                            movto-apr.vl-mob[1].

           if movto-apr.transacao = "IMD" or 
              movto-apr.transacao = "PEF" then
              assign t-controle-3.num-docto = movto-apr.serie-docto    + "/" + 
                                              movto-apr.nro-docto      + "/" + 
                                              movto-apr.parcela        + "/" +
                                              trim(string(movto-apr.cod-emitente,">>>>>>>>9")). 


           assign t-controle-3.num-docto    = movto-apr.nro-docto.

           if movto-apr.tipo-trans = 2 then
              assign t-controle-3.ent-real = t-controle-3.ent-real * -1.    

           run cdp/cd0812.p(input  param-inv.moeda-inv,
                          input  i-cod-moeda-par,
                          input  t-controle-3.ent-real,
                          input  movto-apr.dt-trans,
                          output t-controle-3.ent-real).    

           if movto-apr.transacao = "REQ" then do:                                 
                if movto-apr.tipo-trans = 1 then                              
                   assign t-controle-3.tipo-doc = "Atend. REQ".
                else
                   assign t-controle-3.tipo-doc = "Devol. REQ".
           end.
           else do:
               assign t-controle-3.tipo-doc = movto-apr.transacao.
           end.        

           if movto-apr.transacao = "REQ" then do:
              assign t-controle-3.seq = 10.
              find plano-aprov where
                   plano-aprov.ep-codigo = movto-apr.ep-codigo and
                   plano-aprov.cod-estabel = movto-apr.cod-estabel and
                   plano-aprov.num-solicitacao = int(movto-apr.nro-docto) and
                   plano-aprov.seq-solic = int(movto-apr.parcela) and
                   plano-aprov.tp-solic = "1" 
                   no-lock no-error.
              if avail plano-aprov then     
                  assign t-controle-3.solicitacao = plano-aprov.num-solicitacao.   

              if movto-apr.log-1 then do: /* Executou o in2301 */                    
                if avail plano-aprov then do:                     
                     assign t-controle-3.sai-comp = dec(substring(plano-aprov.char-1,1,15)) +
                                                   (dec(substring(plano-aprov.char-1,16,2)) / 100).
                     assign t-controle-3.sai-comp = t-controle-3.sai-comp / plano-aprov.quant-solic.
                     assign t-controle-3.sai-comp = t-controle-3.sai-comp * movto-apr.quant-mov.                                 
                            t-controle-3.solicitacao  = plano-aprov.num-solicitacao.                        
                     run cdp/cd0812.p(input  0,
                                    input  i-cod-moeda-par,
                                    input  t-controle-3.sai-comp,
                                    input  plano-aprov.dt-emiss-solic,
                                    output t-controle-3.sai-comp).                  
                end.                
              end.

              if movto-apr.tipo-trans = 2 then do:                 
                 assign t-controle-3.ent-real = t-controle-3.ent-real * -1
                        t-controle-3.ent-comp = t-controle-3.sai-comp
                        t-controle-3.sai-comp = 0
                        t-controle-3.sai-real = t-controle-3.ent-real
                        t-controle-3.ent-real = 0.                          
              end.                             
           end.              
           else if movto-apr.transacao = "DIV" then 
              assign t-controle-3.seq = 40.
           else
              assign t-controle-3.seq = 50.   

           /* {esp/esin0519.i3} */
           if t-controle-3.ent-comp = ? then assign t-controle-3.ent-comp = 0.
           if t-controle-3.sai-comp = ? then assign t-controle-3.sai-comp = 0.
           if t-controle-3.ent-real = ? then assign t-controle-3.ent-real = 0.
           if t-controle-3.sai-real = ? then assign t-controle-3.sai-real = 0.

      end. 


END. /* controle verba */

/********* esin0519 **********/

DEF STREAM st.
DEF VAR c-nome LIKE emitente.nome-emit.
/*
OUTPUT STREAM st TO VALUE(tt-param.arquivo-destino) CONVERT TARGET "iso8859-1". /* D:/TEMP/20000047/ESIN0518.LST.*/
*/

FIND empresa WHERE 
     empresa.ep-codigo = i-ep-ini NO-LOCK NO-ERROR.

FIND proj-inv WHERE 
     proj-inv.ep-codigo = i-ep-ini     AND 
     proj-inv.cod-est-exec = c-est-ini AND
     proj-inv.num-projeto = i-proj-ini NO-LOCK NO-ERROR.



/************************************************************/
/*          Abre planilha Excel                             */
CREATE "Excel.Application" chExcelApplication.
chworkbook  = chexcelapplication:workbooks:add.
chworksheet = chexcelapplication:sheets:item(1).
chExcelApplication:Visible = true.
/************************************************************/

 
/*************** Imprime altera‡äes **************************/
    /*************** Cabe»alho ***********************************/ 

   chWorkSheet:Range("c1"):Value =    empresa.nome    
                                    +  "   -   " 
                                    + proj-inv.descricao.
                                    /*+  string(tt-param.i-data-ini) + "  a   "   */
    chWorkSheet:Range("c1"):Font:Size = 12.
    chWorkSheet:Range("c1"):Font:Bold = TRUE.

    chWorkSheet:Range("a2"):Value =    "Cost Summary Report                    "    
                                    +  "                                       " 
                                    +  "                                       "
                                    +  "                             Period:   " 
                                    +  string(tt-param.i-data-ini) + "  a   "       
                                    +  string(tt-param.i-data-fim).
    chWorkSheet:Range("a2"):Font:Size = 12.
    chWorkSheet:Range("a2"):Font:Bold = TRUE.

   /**********
    assign i-linha = i-linha + 1
           /*i-conta-linha = i-conta-linha + 1*/
           c-range = "A" + string(i-linha) + ":I" + string(i-linha + 1).
    chExcelApplication:Range(c-range):select.
    chExcelApplication:Range(c-range):Borders:LineStyle = 1.
    chExcelApplication:Range(c-range):Borders:Weight    = 4.
    chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
    chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.
    **********/

   
    chWorkSheet:Range("c4"):Value = "     Budget(R$)".
    chWorkSheet:Range("c4"):Font:Bold = TRUE.
    chWorkSheet:Range("f4"):Value = "     Commited (R$)".  
    chWorkSheet:Range("f4"):Font:Bold = TRUE.
    chWorkSheet:Range("h4"):Value = "         Payment (R$)".  
    chWorkSheet:Range("h4"):Font:Bold = TRUE.


    chWorkSheet:Range("a5"):Value = "Account ". 
    chWorkSheet:Range("b5"):Value = "   Original".
    chWorkSheet:Range("c5"):Value = "Change Order" /* Transfers"*/ .
    /* chWorkSheet:Range("d5"):Value = "Change Order".    */
    chWorkSheet:Range("d5"):Value = "   Current".
    chWorkSheet:Range("e5"):Value = "This Period ".  
    chWorkSheet:Range("f5"):Value = "Cummulative to date". 
    chWorkSheet:Range("g5"):Value = "To be Commited".    
    chWorkSheet:Range("h5"):Value = "This Period".  
    chWorkSheet:Range("i5"):Value = "Cummulative to date". 
   
   
    chWorkSheet:Range("a5:i5"):Font:Bold = TRUE.
   
    chWorkSheet:Columns("a"):ColumnWidth = 30.
    chWorkSheet:Columns("b"):ColumnWidth = 12.
    chWorkSheet:Columns("C"):ColumnWidth = 14.
    /* chWorkSheet:Columns("d"):ColumnWidth = 12.*/
    chWorkSheet:Columns("d"):ColumnWidth = 12.
    chWorkSheet:Columns("e"):ColumnWidth = 12.
    chWorkSheet:Columns("f"):ColumnWidth = 19.
    chWorkSheet:Columns("g"):ColumnWidth = 14.
    chWorkSheet:Columns("h"):ColumnWidth = 12.
    chWorkSheet:Columns("i"):ColumnWidth = 19.
  
   
    ASSIGN i-linha = 7.


    ASSIGN  de-ent-real = 0
            de-vl-jan   = 0
            de-vl-fev   = 0
            de-vl-mar   = 0
            de-vl-abr   = 0
            de-vl-mai   = 0
            de-vl-jun   = 0
            de-vl-jul   = 0
            de-vl-ago   = 0
            de-vl-set   = 0
            de-vl-out   = 0
            de-vl-nov   = 0
            de-vl-dez   = 0.


/****** cria registros da tabela excel - fixa - empresa 800 - Cida ***

IF  tt-param.i-ep-ini = 800 THEN do:
 
  def new shared temp-table tt-importa no-undo
      field num-pedido     AS   CHAR FORMAT "99999999999"
      field num-ordem      like controle-verba.num-ordem  
      field num-ord-magnus like plano-aprov.num-ord-magnus
      field nr-contrato    like ordem-compra.nr-contrato
      field num-pedido-2   like pedido-compr.num-pedido
      field cod-emitente   like pedido-compr.cod-emitente
      field nome-emit      like emitente.nome-emit
      field nome-equipto   as c
      field vl-comp        as   dec format "->>>,>>>,>>9.99"
      field dt-comp        as   date format "99/99/9999" .
                    
  DEF VAR c-linha    AS CHAR FORMAT "X(200)" NO-UNDO.
  DEF VAR c-nome-arq AS CHAR FORMAT "X(200)" INIT "d:\temp\20199917\doctos\ped-manu.csv" NO-UNDO.

  FOR EACH tt-importa:
      DELETE tt-importa.
  END.                              

  /*OUTPUT TO d:\temp\20199917\doctos\log-manu.txt NO-CONVERT.*/
  INPUT FROM VALUE (c-nome-arq) NO-CONVERT.
      /* CREATE tt-importa. */
      REPEAT:    
          IMPORT UNFORMATTED c-linha.

          IF c-linha = "" THEN NEXT.
          
             CREATE tt-importa.

             ASSIGN tt-importa.num-pedido       = (ENTRY(1,c-linha,";"))
                    tt-importa.num-ord-magnus   =  int(ENTRY(3,c-linha,";"))
                    tt-importa.cod-emitente     =  INT(ENTRY(6,c-linha,";"))
                    tt-importa.nome-emit        = (ENTRY(7,c-linha,";"))  
                    tt-importa.vl-comp          =  DEC(ENTRY(9,c-linha,";")).
          
          
      END.
  INPUT CLOSE.
  /*OUTPUT CLOSE.*/
  
END.                                

********************************/

/* cria controle-verba p/ ordens nÆo movimentadas - 24/03/08 */

for each controle-verba
    where controle-verba.ep-codigo    >= tt-param.i-ep-ini
      and controle-verba.ep-codigo    <= tt-param.i-ep-fim
      and controle-verba.cod-est-exec >= tt-param.c-est-ini
      and controle-verba.cod-est-exec <= tt-param.c-est-fim      
      and controle-verba.num-projeto  >= tt-param.i-proj-ini
      and controle-verba.num-projeto  <= tt-param.i-proj-fim
      and controle-verba.num-ordem    >= tt-param.i-ord-ini
      and controle-verba.num-ordem    <= tt-param.i-ord-fim no-lock
      break by controle-verba.ep-codigo
            by controle-verba.cod-est-exec
            by controle-verba.num-projeto
            by controle-verba.num-ordem:

    IF FIRST-OF(controle-verba.num-ordem) THEN DO:
       
       FIND FIRST t-controle-1 
            WHERE t-controle-1.ep-codigo      = controle-verba.ep-codigo    
              AND t-controle-1.cod-est-exec   = controle-verba.cod-est-exec 
              AND t-controle-1.num-projeto    = controle-verba.num-projeto  
              AND t-controle-1.num-ordem      = controle-verba.num-ordem NO-ERROR.

       IF NOT AVAIL t-controle-1  THEN DO:

          CREATE t-controle-1.
          ASSIGN t-controle-1.ep-codigo      = controle-verba.ep-codigo     
                 t-controle-1.cod-est-exec   = controle-verba.cod-est-exec  
                 t-controle-1.num-projeto    = controle-verba.num-projeto   
                 t-controle-1.num-ordem      = controle-verba.num-ordem
                 t-controle-1.num-ord-magnus = controle-verba.num-ordem.

          FIND FIRST mginv.sub-div-ordem no-lock
               where mginv.sub-div-ordem.ep-codigo    = controle-verba.ep-codigo 
                 AND mginv.sub-div-ordem.cod-est-exec = controle-verba.cod-est-exec 
                 AND mginv.sub-div-ordem.num-projeto  = controle-verba.num-projeto 
                 and mginv.sub-div-ordem.num-ordem    = controle-verba.num-ordem  NO-ERROR.

          IF AVAIL mginv.sub-div-ordem THEN
             ASSIGN t-controle-1.num-ord-magnus = sub-div-ordem.num-ord-magnus.


       END.
    END.
END.

/*
OUTPUT TO d:\temp\20199917\lista.txt.
FOR EACH t-controle-1:
    DISP t-controle-1.ep-codigo    
         t-controle-1.cod-est-exec 
         t-controle-1.num-projeto  
         t-controle-1.num-ordem    
         t-controle-1.num-ord-magnus.

END.

OUTPUT TO CLOSE.

  */

ASSIGN tot-ger-vl-verba = 0
       tot-ger-ent-real = 0
       tot-ger-comprom  = 0.


FOR EACH t-controle-1
    BREAK BY t-controle-1.ep-codigo       
          BY t-controle-1.cod-est-exec    
          BY t-controle-1.num-projeto     
          BY t-controle-1.num-ordem /* num-ord-magnus */ : 


    /******
    FIND FIRST emitente NO-LOCK
        WHERE emitente.cod-emitente = t-controle-1.cod-emitente NO-ERROR.
    IF AVAIL emitente THEN
        ASSIGN c-nome = emitente.nome-emit.
    ELSE
        ASSIGN c-nome = "".*******/

    ASSIGN t-controle-1.descricao = REPLACE(REPLACE(t-controle-1.descricao,CHR(10)," "),CHR(13)," ").
    DISP t-controle-1.ep-codigo
         t-controle-1.cod-est-exec
         t-controle-1.num-projeto
         t-controle-1.num-ord-magnus
         t-controle-1.num-pedido
         t-controle-1.nr-contrato 
         t-controle-1.cod-emitente
         t-controle-1.ent-comp  COLUMN-LABEL "Comprometido"
         t-controle-1.dt-trans
         t-controle-1.valor-cont
         t-controle-1.dt-contrato
         t-controle-1.tipo-doc
         /*t-controle-1.num-ord-comp*/
        WITH WIDTH 333 STREAM-IO.   

   
    ASSIGN tot-comprom = tot-comprom + t-controle-1.ent-comp .


    IF t-controle-1.dt-trans    >= i-data-ini AND
       t-controle-1.dt-trans    <= i-data-fim THEN DO:
       ASSIGN tot-compr-ate = tot-compr-ate + t-controle-1.ent-comp.
    END.
    

   IF last-of(t-controle-1.num-ordem  /*num-ord-magnus*/) THEN DO:

       FOR EACH  t-controle-3 
         WHERE t-controle-3.ep-codigo       = t-controle-1.ep-codigo 
         AND   t-controle-3.cod-est-exec    = t-controle-1.cod-est-exec
         AND   t-controle-3.num-projeto     = t-controle-1.num-projeto
         AND   t-controle-3.num-ordem       = t-controle-1.num-ordem /*
         AND   t-controle-3.it-codigo       = t-controle-1.it-codigo
         AND   t-controle-3.num-ord-magnus  = t-controle-1.num-ord-magnus 
         AND   t-controle-3.cod-emitente    = t-controle-1.cod-emitente
         AND   t-controle-3.num-pedido      = t-controle-1.num-pedido  */
         NO-LOCK:

    /*IF AVAIL t-controle-3 THEN DO: */
        ASSIGN  de-ent-real = de-ent-real + t-controle-3.ent-real .

        IF t-controle-3.dt-trans    >= i-data-ini AND
           t-controle-3.dt-trans    <= i-data-fim THEN DO:
           ASSIGN tot-de-ent-real-ate = tot-de-ent-real-ate + t-controle-3.ent-real.
        END.
        
     END.


        FIND mginv.ordem-inv no-lock
          where mginv.ordem-inv.ep-codigo    = t-controle-1.ep-codigo  
            AND mginv.ordem-inv.cod-est-exec = t-controle-1.cod-est-exec  
            AND mginv.ordem-inv.num-projeto  = t-controle-1.num-projeto    
            and mginv.ordem-inv.num-ordem    = t-controle-1.num-ordem NO-ERROR.

        IF AVAIL mginv.ordem-inv THEN 
           ASSIGN /* tot-vl-verba = ordem-inv.vl-verba[1]*/ 
                  c-desc-ord   = string(t-controle-1.num-ord-magnus)  +  " - " 
                                 + ordem-inv.descricao.
        ELSE 
           ASSIGN tot-vl-verba = 0
                  c-desc-ord = string(t-controle-1.num-ord-magnus).

          FIND controle-verba 
                WHERE controle-verba.ep-codigo    = t-controle-1.ep-codigo    
                  AND controle-verba.cod-est-exec = t-controle-1.cod-est-exec 
                  AND controle-verba.num-projeto  = t-controle-1.num-projeto  
                  AND controle-verba.num-ordem    = t-controle-1.num-ordem NO-LOCK NO-ERROR.

           ASSIGN tot-vl-verba = controle-verba.vl-verba-orig[1].
      

     /* totais gerais */
     ASSIGN tot-ger-vl-verba        = tot-ger-vl-verba        + tot-vl-verba
            tot-ger-ent-real        = tot-ger-ent-real        + de-ent-real
            tot-ger-comprom         = tot-ger-comprom         + tot-comprom  
            tot-ger-compr-ate       = tot-ger-compr-ate       + tot-compr-ate
            tot-ger-de-ent-real-ate = tot-ger-de-ent-real-ate + tot-de-ent-real-ate
            tot-commit              = tot-commit  + (controle-verba.vl-verba[1] - tot-comprom ).
      
         
     ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value =  c-desc-ord /* t-controle-1.num-ord-magnus   */
            chExcelApplication:range( "B" + STRING(i-linha) ):value =  tot-vl-verba                 
            chExcelApplication:Range( "b" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "C" + STRING(i-linha) ):value =  controle-verba.vl-verba[1] - tot-vl-verba      
            /* chExcelApplication:Range( "c" + STRING(i-linha)):NumberFormat="###.###.##0"*/
            /* chExcelApplication:range( "D" + STRING(i-linha) ):value =  0         
            chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="###.###.##0" */
            chExcelApplication:range( "d" + STRING(i-linha) ):value =  controle-verba.vl-verba[1]    
            chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="###.###.##0" 
            chExcelApplication:range( "e" + STRING(i-linha) ):value =  tot-compr-ate        
            chExcelApplication:Range( "e" + STRING(i-linha)):NumberFormat="###.###.##0" 
            chExcelApplication:range( "f" + STRING(i-linha) ):value =  tot-comprom     
            chExcelApplication:Range( "f" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "g" + STRING(i-linha) ):value =  controle-verba.vl-verba[1] - tot-comprom              
            /*chExcelApplication:Range( "h" + STRING(i-linha)):NumberFormat="###.###.##0"*/
            chExcelApplication:range( "h" + STRING(i-linha) ):value = tot-de-ent-real-ate                     
            chExcelApplication:Range( "h" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "i" + STRING(i-linha) ):value =  de-ent-real                      
            chExcelApplication:Range( "i" + STRING(i-linha)):NumberFormat="###.###.##0".
       

     ASSIGN 
            i-linha = i-linha + 1
            tot-comprom  = 0
            tot-vl-verba = 0
            de-ent-real  = 0
            c-desc-ord   = ""
            tot-compr-ate = 0
            tot-de-ent-real-ate = 0
            tot-current = tot-current + controle-verba.vl-verba[1].
            
   END.

END.
OUTPUT TO CLOSE.
ASSIGN i-linha = i-linha + 1.


/* imprime totais */
ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value =  "                 Total :"
       chExcelApplication:range( "B" + STRING(i-linha) ):value =   tot-ger-vl-verba                
       chExcelApplication:Range( "b" + STRING(i-linha)):NumberFormat="###.###.##0"
       chExcelApplication:range( "c" + STRING(i-linha) ):value =   tot-current - tot-ger-vl-verba              
       chExcelApplication:Range( "c" + STRING(i-linha)):NumberFormat="###.###.##0"
       chExcelApplication:range( "d" + STRING(i-linha) ):value =   tot-current               
       chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="###.###.##0"
       chExcelApplication:range( "e" + STRING(i-linha) ):value =   tot-ger-compr-ate               
       chExcelApplication:Range( "e" + STRING(i-linha)):NumberFormat="###.###.##0"
       chExcelApplication:range( "f" + STRING(i-linha) ):value =   tot-ger-comprom              
       chExcelApplication:Range( "f" + STRING(i-linha)):NumberFormat="###.###.##0"
       chExcelApplication:range( "g" + STRING(i-linha) ):value =   tot-commit              
       chExcelApplication:Range( "g" + STRING(i-linha)):NumberFormat="###.###.##0"
       chExcelApplication:range( "h" + STRING(i-linha) ):value =   tot-ger-de-ent-real-ate             
       chExcelApplication:Range( "h" + STRING(i-linha)):NumberFormat="###.###.##0"
       chExcelApplication:range( "i" + STRING(i-linha) ):value =   tot-ger-ent-real               
       chExcelApplication:Range( "i" + STRING(i-linha)):NumberFormat="###.###.##0".


/* imprime dados de pedidos manuais 

FOR EACH tt-importa:
   
     ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value =   tt-importa.num-ord-magnus    /* t-controle-1.cod-est-exec      */
            chExcelApplication:range( "B" + STRING(i-linha) ):value =   tt-importa.num-pedido       /* t-controle-1.num-projeto       */
            /* chExcelApplication:range( "C" + STRING(i-linha) ):value =  t-controle-1.descricao        /* t-controle-1.num-ord-magnus    */ */
            chExcelApplication:range( "D" + STRING(i-linha) ):value =  tt-importa.cod-emitente          /* t-controle-1.nr-contrato       */
            chExcelApplication:range( "e" + STRING(i-linha) ):value =   tt-importa.nome-emit                         /* t-controle-1.nr-contrato       */
            chExcelApplication:range( "f" + STRING(i-linha) ):value =   tt-importa.vl-comp         /* t-controle-1.num-pedido        */
            chExcelApplication:Range( "f" + STRING(i-linha)):NumberFormat="###.###.##0" .

     ASSIGN i-linha = i-linha + 1.

     DELETE tt-importa.
     
END.  */ 

/* imprime dados - tabela es-investimento - pagamentos manuais */
/* es-investimento.ep-codigo    
es-investimento.cod-est-exec 
es-investimento.num-projeto  
es-investimento.num-ordem    
es-investimento.num-pedido   
es-investimento.nr-contrato  
es-investimento.cod-emitente 
es-investimento.nome-emit    
es-investimento.it-codigo    
es-investimento.descricao    
es-investimento.nr-docto     
es-investimento.serie        
es-investimento.dt-emissao   
es-investimento.dt-pagto     
es-investimento.vl-docto     */


/**********
FOR EACH es-investimento 
   WHERE  int(es-investimento.ep-codigo) = tt-param.i-ep-ini  
     AND  es-investimento.cod-est-exec  >= tt-param.c-est-ini
     AND  es-investimento.cod-est-exec  <= tt-param.c-est-fim
     AND  es-investimento.num-projeto   >= tt-param.i-proj-ini   
     AND  es-investimento.num-projeto   <= tt-param.i-proj-fim 
     AND  es-investimento.num-ordem     >= tt-param.i-ord-ini   
     AND  es-investimento.num-ordem     <= tt-param.i-ord-fim 
     AND  es-investimento.dt-pagto      >= tt-param.i-data-ini   
     AND  es-investimento.dt-pagto      <= tt-param.i-data-fim  NO-LOCK:

   ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value =  es-investimento.num-ordem     
          chExcelApplication:range( "B" + STRING(i-linha) ):value =  es-investimento.num-pedido    
          chExcelApplication:range( "C" + STRING(i-linha) ):value =  es-investimento.descricao     
          chExcelApplication:range( "D" + STRING(i-linha) ):value =  es-investimento.cod-emitente  
          chExcelApplication:range( "e" + STRING(i-linha) ):value =  es-investimento.nome-emit     
          chExcelApplication:range( "f" + STRING(i-linha) ):value =  es-investimento.vl-docto      
          chExcelApplication:Range( "f" + STRING(i-linha)):NumberFormat="###.###.##0" .

     ASSIGN i-linha = i-linha + 1.

END. 
*******/                             

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 





{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return 'ok'.


{esp/esin101.i5}






