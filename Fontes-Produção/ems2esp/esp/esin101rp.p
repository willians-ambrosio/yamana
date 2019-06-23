/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa     for ems2cadme.empresa.

{include/i-prgvrs.i esin101rp 2.00.00.015}  /*** 010015 ***/
{include/i_fnctrad.i}
/******************************************************************************
**
**   Programa: esin101rp.p
**
**   Data....: Dezembro de 2007.
**
**   Autor...: DSC - Maria Aparecida L. Nogueira
**
**   Objetivo: Relat¢rio de Investimentos
**  
*******************************************************************************/
{include/i-epc200.i esin101rp}

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

/*define temp-table tt-param no-undo
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
    field atualiza         as logical.
*/
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

DEF VAR c-desc-ord  LIKE ordem-inv.descricao.
DEF VAR c-desc-forn AS CHAR FORMAT "x(50)".
DEF VAR c-range AS CHARACTER.

/* totais */

DEF VAR tot-ent-comp    LIKE t-controle-1.ent-comp.
DEF VAR tot-de-ent-real LIKE de-ent-real.             
DEF VAR tot-de-vl-jan   LIKE de-vl-jan.               
DEF VAR tot-de-vl-fev   LIKE de-vl-fev.               
DEF VAR tot-de-vl-mar   LIKE de-vl-mar.               
DEF VAR tot-de-vl-abr   LIKE de-vl-abr.               
DEF VAR tot-de-vl-mai   LIKE de-vl-mai.               
DEF VAR tot-de-vl-jun   LIKE de-vl-jun.               
DEF VAR tot-de-vl-jul   LIKE de-vl-jul.               
DEF VAR tot-de-vl-ago   LIKE de-vl-ago.               
DEF VAR tot-de-vl-set   LIKE de-vl-set.               
DEF VAR tot-de-vl-out   LIKE de-vl-out.               
DEF VAR tot-de-vl-nov   LIKE de-vl-nov.               
DEF VAR tot-de-vl-dez   LIKE de-vl-dez. 

DEF VAR tot-ger-ent-comp    LIKE t-controle-1.ent-comp.
DEF VAR tot-ger-de-ent-real LIKE de-ent-real.          
DEF VAR tot-ger-de-vl-jan   LIKE de-vl-jan.            
DEF VAR tot-ger-de-vl-fev   LIKE de-vl-fev.            
DEF VAR tot-ger-de-vl-mar   LIKE de-vl-mar.            
DEF VAR tot-ger-de-vl-abr   LIKE de-vl-abr.            
DEF VAR tot-ger-de-vl-mai   LIKE de-vl-mai.            
DEF VAR tot-ger-de-vl-jun   LIKE de-vl-jun.            
DEF VAR tot-ger-de-vl-jul   LIKE de-vl-jul.            
DEF VAR tot-ger-de-vl-ago   LIKE de-vl-ago.            
DEF VAR tot-ger-de-vl-set   LIKE de-vl-set.            
DEF VAR tot-ger-de-vl-out   LIKE de-vl-out.            
DEF VAR tot-ger-de-vl-nov   LIKE de-vl-nov.            
DEF VAR tot-ger-de-vl-dez   LIKE de-vl-dez.           


find first param-global no-lock no-error.

{utp/ut-liter.i Listagem_Verba_Ordem_de_Investimento_-_Detalhada * r }
assign c-titulo-relat = return-value.
{utp/ut-liter.i INVESTIMENTO  * r }
assign c-programa     = "esin101" 
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
{esp/esin101.i1}        

{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.

FOR EACH t-controle-1:
    DELETE t-controle-1.
END.



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
           and movto-nf.dt-trans >= tt-param.i-data-ini
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

          /* desc - t-controle-3 - 08/04/08 */
          FIND first ordem-compra no-lock
               where ordem-compra.numero-ordem = ord-ped.num-ord-comp
               and ordem-compra.num-pedido = ord-ped.num-pedido NO-ERROR.
          IF AVAIL  ordem-compra THEN
             assign t-controle-3.descricao = replace(ordem-compra.narrativa,";"," ").

         
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
           and movto-apr.dt-trans >= tt-param.i-data-ini
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

   END.

     
     


      DEF VAR tot-controle LIKE  t-controle-3.ent-real.


       
      FOR EACH  t-controle-3 NO-LOCK
            BREAK BY t-controle-3.ep-codigo  
                  BY t-controle-3.cod-est-exec  
                  BY t-controle-3.num-projeto   
                  BY t-controle-3.num-ordem     
                  BY t-controle-3.it-codigo     
                  BY t-controle-3.num-ord-magnus
                  BY t-controle-3.cod-emitente  
                  BY t-controle-3.num-pedido    
                  BY month(t-controle-3.dt-trans) 
                  BY YEAR(t-controle-3.dt-trans):
                
         

            ASSIGN tot-controle = tot-controle + t-controle-3.ent-real. 

            
            IF LAST-OF(month(t-controle-3.dt-trans)) AND
               LAST-OF(YEAR(t-controle-3.dt-trans)) THEN DO:
      
             
              if  month(t-controle-3.dt-trans) = 01 then
                  assign t-controle-3.vl-jan =  tot-controle.  /*movto-nf.vl-nota[1]*/.
        
              if  month(t-controle-3.dt-trans) = 02 then
                  assign t-controle-3.vl-fev =  tot-controle. /*movto-nf.vl-nota[1]*/.
           
              if  month(t-controle-3.dt-trans) = 03 then
                  assign t-controle-3.vl-mar =  tot-controle. /*movto-nf.vl-nota[1]*/.
           
              if  month(t-controle-3.dt-trans) = 04 then
                  assign t-controle-3.vl-abr =  tot-controle. /*movto-nf.vl-nota[1]*/.
         
              if  month(t-controle-3.dt-trans) = 05 then
                  assign t-controle-3.vl-mai =  tot-controle . /*movto-nf.vl-nota[1]*/.
         
              if  month(t-controle-3.dt-trans) = 06 then
                  assign t-controle-3.vl-jun =  tot-controle. /*movto-nf.vl-nota[1]*/.
         
              if  month(t-controle-3.dt-trans) = 07 then
                  assign t-controle-3.vl-jul =  tot-controle . /*movto-nf.vl-nota[1]*/.
       
              if  month(t-controle-3.dt-trans) = 08 then
                  assign t-controle-3.vl-ago =  tot-controle. /*movto-nf.vl-nota[1]*/.
        
              if  month(t-controle-3.dt-trans) = 09 then
                  assign t-controle-3.vl-set =  tot-controle. /*movto-nf.vl-nota[1]*/.
         
              if  month(t-controle-3.dt-trans) = 10 then
                  assign t-controle-3.vl-out =  tot-controle . /*movto-nf.vl-nota[1]*/.
          
              if  month(t-controle-3.dt-trans) = 11 then
                  assign t-controle-3.vl-nov = tot-controle. /*movto-nf.vl-nota[1]*/.
         
              if  month(t-controle-3.dt-trans) = 12 then
                  assign t-controle-3.vl-dez =  tot-controle. /*movto-nf.vl-nota[1]*/.

             
              ASSIGN tot-controle = 0.
           end. 
      
      END.
      /**** totais *********/
    

  /***  END. /* controle verba */ cidaaa*/





DEF STREAM st.
DEF VAR c-nome LIKE emitente.nome-emit.
/*
OUTPUT STREAM st TO VALUE(tt-param.arquivo-destino) CONVERT TARGET "iso8859-1". /* D:/TEMP/20000047/ESIN0518.LST.*/
*/
/************************************************************/
/*          Abre planilha Excel                             */
CREATE "Excel.Application" chExcelApplication.
chworkbook  = chexcelapplication:workbooks:add.
chworksheet = chexcelapplication:sheets:item(1).
chExcelApplication:Visible = true.
/************************************************************/

FIND empresa WHERE 
     empresa.ep-codigo = i-ep-ini NO-LOCK NO-ERROR.

FIND proj-inv WHERE 
     proj-inv.ep-codigo = i-ep-ini     AND 
     proj-inv.cod-est-exec = c-est-ini AND
     proj-inv.num-projeto = i-proj-ini NO-LOCK NO-ERROR.


/*************** Imprime altera‡äes **************************/
    /*************** Cabe»alho ***********************************/ 
    chWorkSheet:Range("f1"):Value =    empresa.nome    
                                    +  "   -   " 
                                    + proj-inv.descricao.
                                    /*+  string(tt-param.i-data-ini) + "  a   "   */
    chWorkSheet:Range("f1"):Font:Size = 12.
    chWorkSheet:Range("f1"):Font:Bold = TRUE.

         
                      
     chWorkSheet:Range("o2"):Value = " Period:   " 
                                    +  string(tt-param.i-data-ini) + "  a   "       
                                    +  string(tt-param.i-data-fim).
    chWorkSheet:Range("o2"):Font:Size = 12.
    chWorkSheet:Range("o2"):Font:Bold = TRUE.  


    chWorkSheet:Range("f3"):Value =    "Cost Detailed Report".
    chWorkSheet:Range("f3"):Font:Size = 12.
    chWorkSheet:Range("f3"):Font:Bold = TRUE. 

    
    
    chWorkSheet:Columns("a"):ColumnWidth = 10.
    chWorkSheet:Columns("b"):ColumnWidth = 15.
    chWorkSheet:Columns("C"):ColumnWidth = 50.
    chWorkSheet:Columns("d"):ColumnWidth = 12.
    chWorkSheet:Columns("e"):ColumnWidth = 6.
    chWorkSheet:Columns("f"):ColumnWidth = 12.
    chWorkSheet:Columns("g"):ColumnWidth = 12.
    chWorkSheet:Columns("h"):ColumnWidth = 12.
    chWorkSheet:Columns("i"):ColumnWidth = 12.
    chWorkSheet:Columns("j"):ColumnWidth = 10.
    chWorkSheet:Columns("k"):ColumnWidth = 10.
    chWorkSheet:Columns("l"):ColumnWidth = 10.
    chWorkSheet:Columns("m"):ColumnWidth = 10.
    chWorkSheet:Columns("n"):ColumnWidth = 10.
    chWorkSheet:Columns("o"):ColumnWidth = 10.
    chWorkSheet:Columns("p"):ColumnWidth = 10.
    chWorkSheet:Columns("q"):ColumnWidth = 10.
    chWorkSheet:Columns("r"):ColumnWidth = 10.
    chWorkSheet:Columns("s"):ColumnWidth = 10.
   
    ASSIGN i-linha = 5. 

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


/****** cria registros da tabela excel - fixa - empresa 800 - Cida **/

IF  tt-param.i-ep-ini     = "800"  AND
    tt-param.c-est-ini    = "801"  AND 
    tt-param.i-proj-ini   = 2      THEN do:
 
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
  DEF VAR c-nome-arq AS CHAR FORMAT "X(200)" INIT "ped-manu.csv" NO-UNDO.

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
                    tt-importa.num-ordem       =  int(ENTRY(3,c-linha,";"))
                    tt-importa.cod-emitente     =  INT(ENTRY(6,c-linha,";"))
                    tt-importa.nome-emit        = (ENTRY(7,c-linha,";"))  
                    tt-importa.vl-comp          =  DEC(ENTRY(9,c-linha,";")).


      END.
  INPUT CLOSE.
  /*OUTPUT CLOSE.*/
  
END.                                

/************************************/

FOR EACH tt-importa:           
   
    CREATE t-controle-1.
    ASSIGN t-controle-1.ep-codigo      = "800"
           t-controle-1.cod-est-exec   = "801"
           t-controle-1.num-projeto    = 2 
           t-controle-1.num-ordem      = int(tt-importa.num-ordem )
           t-controle-1.num-pedido     = INT(tt-importa.num-pedido)
           t-controle-1.nome-emit      = tt-importa.nome-emit
           t-controle-1.cod-emit       = tt-importa.cod-emit
           t-controle-1.ent-comp       = INT(tt-importa.vl-comp).

END.

/********* esin0519 **********/


/* cria t-controle-1 quando nÆo tiver no t-controle-3 - cida 08/04/08 */


FOR EACH t-controle-3:
    
    FIND first t-controle-1 
      WHERE  t-controle-1.ep-codigo        = t-controle-3.ep-codigo      
        AND  t-controle-1.cod-est-exec     = t-controle-3.cod-est-exec   
        AND  t-controle-1.num-projeto      = t-controle-3.num-projeto    
        AND  t-controle-1.num-ordem        = t-controle-3.num-ordem      
        AND  t-controle-1.it-codigo        = t-controle-3.it-codigo      
        AND  t-controle-1.num-ord-magnus   = t-controle-3.num-ord-magnus 
        AND  t-controle-1.cod-emitente     = t-controle-3.cod-emitente   
        AND  t-controle-1.num-pedido       = t-controle-3.num-pedido NO-ERROR.

  
   IF NOT AVAIL t-controle-1 THEN DO:
      CREATE t-controle-1.
      ASSIGN t-controle-1.ep-codigo        = t-controle-3.ep-codigo             
             t-controle-1.cod-est-exec     = t-controle-3.cod-est-exec          
             t-controle-1.num-projeto      = t-controle-3.num-projeto           
             t-controle-1.num-ordem        = t-controle-3.num-ordem             
             t-controle-1.it-codigo        = t-controle-3.it-codigo             
             t-controle-1.num-ord-magnus   = t-controle-3.num-ord-magnus        
             t-controle-1.cod-emitente     = t-controle-3.cod-emitente          
             t-controle-1.num-pedido       = t-controle-3.num-pedido
             t-controle-1.ent-real         = t-controle-3.ent-real
             t-controle-1.descricao        = t-controle-3.descricao.

   END.
/*    ELSE DO:                                                               */
/*        ASSIGN t-controle-1.ep-codigo        = t-controle-3.ep-codigo      */
/*               t-controle-1.cod-est-exec     = t-controle-3.cod-est-exec   */
/*               t-controle-1.num-projeto      = t-controle-3.num-projeto    */
/*               t-controle-1.num-ordem        = t-controle-3.num-ordem      */
/*               t-controle-1.it-codigo        = t-controle-3.it-codigo      */
/*               t-controle-1.num-ord-magnus   = t-controle-3.num-ord-magnus */
/*               t-controle-1.cod-emitente     = t-controle-3.cod-emitente   */
/*               t-controle-1.num-pedido       = t-controle-3.num-pedido     */
/*               t-controle-1.ent-real         = t-controle-3.ent-real       */
/*               t-controle-1.descricao        = t-controle-3.descricao.     */
/*    END.                                                                   */
END.
                                                                     


FOR EACH t-controle-1:
    FIND emitente WHERE emitente.cod-emit = t-controle-1.cod-emit NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN 
       ASSIGN t-controle-1.nome-emit = emitente.nome-emit.
END.


FOR EACH t-controle-1 NO-LOCK
    BREAK BY  t-controle-1.ep-codigo         
          BY  t-controle-1.cod-est-exec      
          BY  t-controle-1.num-projeto       
          BY  t-controle-1.num-ordem 
          /**
          BY  t-controle-1.it-codigo
          BY  t-controle-1.num-ord-magnus
          ***/
          /*BY  t-controle-1.num-pedido*/
          BY  t-controle-1.nome-emit:

    FIND FIRST emitente NO-LOCK
        WHERE emitente.cod-emitente = t-controle-1.cod-emitente NO-ERROR.
    IF AVAIL emitente THEN
        ASSIGN c-nome = emitente.nome-emit.
    ELSE
        ASSIGN c-nome = t-controle-1.nome-emit /* pedidos manuais */.

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

    
     FOR EACH  t-controle-3 
         WHERE t-controle-3.ep-codigo       = t-controle-1.ep-codigo 
         AND   t-controle-3.cod-est-exec    = t-controle-1.cod-est-exec
         AND   t-controle-3.num-projeto     = t-controle-1.num-projeto
         AND   t-controle-3.num-ordem       = t-controle-1.num-ordem 
         AND   t-controle-3.it-codigo       = t-controle-1.it-codigo
         AND   t-controle-3.num-ord-magnus  = t-controle-1.num-ord-magnus 
         AND   t-controle-3.cod-emitente    = t-controle-1.cod-emitente
         AND   t-controle-3.num-pedido      = t-controle-1.num-pedido 
         NO-LOCK:

    /*IF AVAIL t-controle-3 THEN DO: */
        ASSIGN  de-ent-real = de-ent-real + t-controle-3.ent-real 
                de-vl-jan   = de-vl-jan + t-controle-3.vl-jan
                de-vl-fev   = de-vl-fev + t-controle-3.vl-fev
                de-vl-mar   = de-vl-mar + t-controle-3.vl-mar 
                de-vl-abr   = de-vl-abr + t-controle-3.vl-abr
                de-vl-mai   = de-vl-mai + t-controle-3.vl-mai
                de-vl-jun   = de-vl-jun + t-controle-3.vl-jun
                de-vl-jul   = de-vl-jul + t-controle-3.vl-jul
                de-vl-ago   = de-vl-ago + t-controle-3.vl-ago
                de-vl-set   = de-vl-set + t-controle-3.vl-set
                de-vl-out   = de-vl-out + t-controle-3.vl-out
                de-vl-nov   = de-vl-nov + t-controle-3.vl-nov
                de-vl-dez   = de-vl-dez + t-controle-3.vl-dez.

     END.

                 
     IF FIRST-OF(t-controle-1.num-ordem) THEN DO:
        FIND mginv.ordem-inv no-lock
          where mginv.ordem-inv.ep-codigo    = t-controle-1.ep-codigo  
            AND mginv.ordem-inv.cod-est-exec = t-controle-1.cod-est-exec  
            AND mginv.ordem-inv.num-projeto  = t-controle-1.num-projeto    
            and mginv.ordem-inv.num-ordem    = t-controle-1.num-ordem NO-ERROR.
     

        IF AVAIL mginv.ordem-inv THEN 
           ASSIGN c-desc-ord   = string(t-controle-1.num-ordem)  +  " - " 
                                 + ordem-inv.descricao.
        ELSE
           ASSIGN c-desc-ord  = string(t-controle-1.num-ordem) .

        ASSIGN c-range = "a" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 10
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = "Ord.Invest.:"
               c-range = "b" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 10
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = c-desc-ord.
        ASSIGN i-linha = i-linha + 1.

        ASSIGN c-range = "c" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 10
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = "Ordem EMS: " + string(t-controle-1.num-ord-magnus)
               /*c-range = "d" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 10
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = t-controle-1.num-ord-magnus*/
               c-range = "k" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 10
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = "Payment on Period (R$)".
         ASSIGN i-linha = i-linha + 1.

        

       ASSIGN c-range = "a" + string(i-linha)
              chExcelApplication:Range(c-range):value = "N.Pedido"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "b" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Description"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "c" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Supplier"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "d" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Commited "
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "e" + string(i-linha)
              chExcelApplication:Range(c-range):value = "NC"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "f" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Cummulative Payment"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "g" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Jan"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "h" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Fev"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "i" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Mar"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "j" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Abr"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "k" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Mai"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "l" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Jun"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "m" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Jul"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "n" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Ago"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "o" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Set"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "p" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Out"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "q" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Nov"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "r" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Dez"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE.

        ASSIGN i-linha = i-linha + 1.
   


     END.

     ASSIGN c-desc-forn = string(t-controle-1.cod-emit) + " "  + c-nome.
    
     ASSIGN /* chExcelApplication:range( "A" + STRING(i-linha) ):value =  t-controle-1.num-ord-magnus    t-controle-1.cod-est-exec      */
            chExcelApplication:range( "a" + STRING(i-linha) ):value =  t-controle-1.num-pedido       /* t-controle-1.num-projeto       */
            chExcelApplication:range( "b" + STRING(i-linha) ):value =  t-controle-1.descricao        /* t-controle-1.num-ord-magnus    */
            chExcelApplication:range( "c" + STRING(i-linha) ):value =  c-desc-forn /* t-controle-1.cod-emit          */
            /*chExcelApplication:range( "d" + STRING(i-linha) ):value =  c-nome                        /* t-controle-1.nr-contrato       */*/
            chExcelApplication:range( "d" + STRING(i-linha) ):value =  t-controle-1.ent-comp         /* t-controle-1.num-pedido        */
            chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="###.###.##0" 
            chExcelApplication:range( "e" + STRING(i-linha) ):value =  t-controle-1.nr-contrato      /* t-controle-1.cod-emitente      */
            chExcelApplication:range( "f" + STRING(i-linha) ):value =  de-ent-real                   /* c-nome                         */
            chExcelApplication:Range( "f" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "g" + STRING(i-linha) ):value =  de-vl-jan                     /* t-controle-1.descricao   */ 
            chExcelApplication:Range( "g" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "h" + STRING(i-linha) ):value =  de-vl-fev    
            chExcelApplication:Range( "h" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "i" + STRING(i-linha) ):value =  de-vl-mar    
            chExcelApplication:Range( "i" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "j" + STRING(i-linha) ):value =  de-vl-abr    
            chExcelApplication:Range( "j" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "k" + STRING(i-linha) ):value =  de-vl-mai    
            chExcelApplication:Range( "k" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "l" + STRING(i-linha) ):value =  de-vl-jun    
            chExcelApplication:Range( "l" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "m" + STRING(i-linha) ):value =  de-vl-jul    
            chExcelApplication:Range( "m" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "n" + STRING(i-linha) ):value =  de-vl-ago    
            chExcelApplication:Range( "n" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "o" + STRING(i-linha) ):value =  de-vl-set    
            chExcelApplication:Range( "o" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "p" + STRING(i-linha) ):value =  de-vl-out    
            chExcelApplication:Range( "p" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "q" + STRING(i-linha) ):value =  de-vl-nov    
            chExcelApplication:Range( "q" + STRING(i-linha)):NumberFormat="###.###.##0"
            chExcelApplication:range( "r" + STRING(i-linha) ):value =  de-vl-dez
            chExcelApplication:Range( "r" + STRING(i-linha)):NumberFormat="###.###.##0".

  /* acumula totais */
  ASSIGN tot-ent-comp    = tot-ent-comp    + t-controle-1.ent-comp
         tot-de-ent-real = tot-de-ent-real + de-ent-real
         tot-de-vl-jan   = tot-de-vl-jan   + de-vl-jan
         tot-de-vl-fev   = tot-de-vl-fev   + de-vl-fev
         tot-de-vl-mar   = tot-de-vl-mar   + de-vl-mar
         tot-de-vl-abr   = tot-de-vl-abr   + de-vl-abr
         tot-de-vl-mai   = tot-de-vl-mai   + de-vl-mai
         tot-de-vl-jun   = tot-de-vl-jun   + de-vl-jun
         tot-de-vl-jul   = tot-de-vl-jul   + de-vl-jul
         tot-de-vl-ago   = tot-de-vl-ago   + de-vl-ago
         tot-de-vl-set   = tot-de-vl-set   + de-vl-set
         tot-de-vl-out   = tot-de-vl-out   + de-vl-out
         tot-de-vl-nov   = tot-de-vl-nov   + de-vl-nov
         tot-de-vl-dez   = tot-de-vl-dez   + de-vl-dez.


  /* total geral */
  ASSIGN tot-ger-ent-comp    = tot-ger-ent-comp    + t-controle-1.ent-comp
         tot-ger-de-ent-real = tot-ger-de-ent-real + de-ent-real
         tot-ger-de-vl-jan   = tot-ger-de-vl-jan   + de-vl-jan
         tot-ger-de-vl-fev   = tot-ger-de-vl-fev   + de-vl-fev
         tot-ger-de-vl-mar   = tot-ger-de-vl-mar   + de-vl-mar
         tot-ger-de-vl-abr   = tot-ger-de-vl-abr   + de-vl-abr
         tot-ger-de-vl-mai   = tot-ger-de-vl-mai   + de-vl-mai
         tot-ger-de-vl-jun   = tot-ger-de-vl-jun   + de-vl-jun
         tot-ger-de-vl-jul   = tot-ger-de-vl-jul   + de-vl-jul
         tot-ger-de-vl-ago   = tot-ger-de-vl-ago   + de-vl-ago
         tot-ger-de-vl-set   = tot-ger-de-vl-set   + de-vl-set
         tot-ger-de-vl-out   = tot-ger-de-vl-out   + de-vl-out
         tot-ger-de-vl-nov   = tot-ger-de-vl-nov   + de-vl-nov
         tot-ger-de-vl-dez   = tot-ger-de-vl-dez   + de-vl-dez.


  ASSIGN i-linha = i-linha + 1.

   IF LAST-OF(t-controle-1.num-ordem) THEN DO:
      
      ASSIGN c-range = "c" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = "Total Ordem EMS:  " + STRING(t-controle-1.num-ord-magnus)
             c-range = "d" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ent-comp
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "f" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-de-ent-real
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "g" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-de-vl-jan
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "h" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-de-vl-fev
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "i" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-de-vl-mar
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "j" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-de-vl-abr
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "k" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-de-vl-mai
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "l" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-de-vl-jun
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "m" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-de-vl-jul
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "n" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-de-vl-ago
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "o" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-de-vl-set
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "p" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-de-vl-out
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "q" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-de-vl-nov
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "r" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-de-vl-dez
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0".


      ASSIGN tot-ent-comp    = 0
             tot-de-ent-real = 0
             tot-de-vl-jan   = 0
             tot-de-vl-fev   = 0
             tot-de-vl-mar   = 0
             tot-de-vl-abr   = 0
             tot-de-vl-mai   = 0
             tot-de-vl-jun   = 0
             tot-de-vl-jul   = 0
             tot-de-vl-ago   = 0
             tot-de-vl-set   = 0
             tot-de-vl-out   = 0
             tot-de-vl-nov   = 0
             tot-de-vl-dez   = 0.

        ASSIGN i-linha = i-linha + 2.

     END.



END.



/* total geral */

      ASSIGN c-range = "c" + string(i-linha + 1)
             chWorkSheet:Range(c-range):font:size    = 12
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = "Total - "  
                                                     +  empresa.nome    
                                                     + "   -   " 
                                                     + proj-inv.descricao.
      ASSIGN i-linha = i-linha + 2.

      ASSIGN  c-range = "d" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Commited "
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "f" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Cummulative Payment"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "g" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Jan"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "h" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Fev"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "i" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Mar"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "j" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Abr"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "k" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Mai"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "l" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Jun"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "m" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Jul"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "n" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Ago"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "o" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Set"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "p" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Out"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "q" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Nov"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE
              c-range = "r" + string(i-linha)
              chExcelApplication:Range(c-range):value = "Dez"
              chWorkSheet:Range(c-range):font:size    = 10
              chWorkSheet:Range(c-range):font:bold    = TRUE.

        ASSIGN i-linha = i-linha + 1.
             
        ASSIGN c-range = "c" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = "Total Geral:"
             c-range = "d" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-ent-comp
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "f" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-de-ent-real
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "g" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-de-vl-jan
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "h" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-de-vl-fev
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "i" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-de-vl-mar
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "j" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-de-vl-abr
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "k" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-de-vl-mai
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "l" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-de-vl-jun
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "m" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-de-vl-jul
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "n" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-de-vl-ago
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "o" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-de-vl-set
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "p" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-de-vl-out
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "q" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-de-vl-nov
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0"
             c-range = "r" + string(i-linha)
             chWorkSheet:Range(c-range):font:size    = 10
             chWorkSheet:Range(c-range):font:bold    = true
             chExcelApplication:Range(c-range):value = tot-ger-de-vl-dez
             chExcelApplication:Range(c-range):Numberformat = "###.###.##0".

/***********

/* imprime dados de pedidos manuais */

FOR EACH tt-importa:
   
     ASSIGN /* chExcelApplication:range( "A" + STRING(i-linha) ):value =   tt-importa.num-ord-magnus    /* t-controle-1.cod-est-exec      */ */
            chExcelApplication:range( "a" + STRING(i-linha) ):value =   tt-importa.num-pedido       /* t-controle-1.num-projeto       */
            /* chExcelApplication:range( "C" + STRING(i-linha) ):value =  t-controle-1.descricao        /* t-controle-1.num-ord-magnus    */ */
            chExcelApplication:range( "c" + STRING(i-linha) ):value =  tt-importa.cod-emitente          /* t-controle-1.nr-contrato       */
            /*chExcelApplication:range( "d" + STRING(i-linha) ):value =   tt-importa.nome-emit */                        /* t-controle-1.nr-contrato       */
            chExcelApplication:range( "d" + STRING(i-linha) ):value =   tt-importa.vl-comp         /* t-controle-1.num-pedido        */
            chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="###.###.##0" .

     ASSIGN i-linha = i-linha + 1.

     DELETE tt-importa.
     
END.  
**************/



/*************/
/* imprime dados - tabela es-investimento - pagamentos manuais */

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






