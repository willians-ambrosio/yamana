/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESIN0518RP 2.00.00.030}  /*** 010030 ***/

{include/i_fnctrad.i}
/******************************************************************************
**
**   Programa: ESIN0518rp.p
**
**   Data....: Setembro de 2000.
**
**   Autor...: DATASUL S.A. Alan Koerbel
**
**   Objetivo: Listagem da Verba da Ordem Investimento - Detalhado.
**  
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.

{include/i-epc200.i in0518rp}

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
def var c-lbl-ep-codigo     as char format "x(08)" no-undo.
def var c-lbl-estab-exec    as char format "x(11)" no-undo.
def var c-lbl-num-projeto   as char format "x(12)" no-undo.
def var c-lbl-num-ordem     as char format "x(13)" no-undo.
def var c-lbl-situacao      as char format "x(09)" no-undo.
def var c-lbl-moeda         as char format "x(15)" no-undo.
def var c-lbl-vl-verba      as char format "x(12)" no-undo.
def var c-lbl-vl-verba-orig as char format "x(17)" no-undo.
def var c-lbl-vl-compromis  as char format "x(16)" no-undo.
def var c-lbl-vl-realizado  as char format "x(16)" no-undo.
def var c-lbl-total         as char format "x(06)" no-undo.

{cdp/cd9731.i}  /* l-integra-cn-in */
{cdp/cd9731.i1} /* l-integra-cn-in-medicao */
&if '{&bf_mat_versao_ems}' >= '2.062' &THEN
  {cdp/cd9590.i} /* Unidade de Negocio */
&endif

def buffer b-controle-verba for controle-verba.
def var de-acum-comp     as dec                 no-undo.
def var de-acum-real     as dec                 no-undo.
def var de-acum-tot      as dec                 no-undo.
def var de-total         as dec                 no-undo.
DEF VAR c-txt-unid-negoc AS CHAR FORMAT 'x(28)' EXTENT 33 NO-UNDO.
DEF VAR i-cnt-unid-negoc AS INT                 NO-UNDO.
DEF VAR c-lbl-unid-negoc AS CHAR FORMAT '(15)'  NO-UNDO.

def var c-data-sel       as char format "x(10)"  no-undo.
DEF VAR v_cod_lista_label AS CHAR NO-UNDO.
DEF VAR v_cod_cgc         AS CHAR NO-UNDO.
DEF STREAM s-planilha.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classif-1        as integer
    field desc-classifica  as char format "x(40)"
    field i-ep-ini         LIKE empresa.ep-codigo 
    field i-ep-fim         LIKE empresa.ep-codigo
    field c-est-ini        as char
    field c-est-fim        as char
    field i-proj-ini       as int 
    field i-proj-fim       as int
    field i-ord-ini        as int
    field i-ord-fim        as int
    FIELD d-data-ini       AS DATE
    FIELD d-data-fim       AS DATE
    field i-moeda-par      as int
    field atualiza         as logical
    field log-gera-excel   as logical
    field caminho-excel    as char format "x(100)"
    field delimitador      as char format "x(1)".

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem. 

define temp-table tt-impressao
    field linha-1 as char format "x(100)"
    field linha-2 as char format "x(100)"
    field linha-3 as char format "x(100)"
    field linha-4 as char format "x(100)"
    field linha-5 as char format "x(100)"
    field linha-6 as char format "x(100)"
    field linha-7 as char format "x(100)"
    field linha-8 as char format "x(100)"
    field linha-9 as char format "x(100)"
    field colunas as char format "x(100)".

def temp-table tt-raw-digita                   
    field raw-digita as raw.
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

{cdp/cdcfgmat.i}

/* Defini‡Æo das frames e tradu‡Æo*/
{inp/spp/esin0518.i2}

define buffer B-T-CONTROLE for T-CONTROLE.

find first param-global no-lock no-error.

{utp/ut-liter.i Listagem_Verba_Ordem_de_Investimento_-_Detalhada * r }
assign c-titulo-relat = return-value.
{utp/ut-liter.i INVESTIMENTO  * r }
assign c-programa     = "ESIN0518" 
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

run utp/ut-acomp.p persistent set h-acomp.

if not tt-param.log-gera-excel then do:
    run INIC-FORMS.  /* gera cabecalho e rodape de 233 cols */
    {include/i-rpout.i}

    view frame f-cabec.
    view frame f-rodape.
end.
/* Monta valores na temp-table */
/*******************************************************************************
* ESIN0518.I1 - Busca Valores
********************************************************************************/
{cdp/cdcfgmat.i}

run pi-inicializar in h-acomp(input c-processando).
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

      if first-of (controle-verba.ep-codigo) then do:
         find param-inv 
              where param-inv.ep-codigo = controle-verba.ep-codigo no-lock no-error.     
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
      for each plano-aprov
         where plano-aprov.ep-codigo = controle-verba.ep-codigo
           and plano-aprov.cod-est-exec = controle-verba.cod-est-exec
           and plano-aprov.num-projeto = controle-verba.num-projeto
           and plano-aprov.num-ordem = controle-verba.num-ordem
           and plano-aprov.cod-sit-solic <> " " 
           and plano-aprov.cod-sit-plano <> "E"
           and plano-aprov.num-solicitacao > 0 no-lock:

          if plano-aprov.num-ord-comp <> 0 then do:
              find ord-ped where 
                   ord-ped.ep-codigo = plano-aprov.ep-codigo and
                   ord-ped.cod-estabel = plano-aprov.cod-estabel and
                   ord-ped.cod-area = 0 and
                   ord-ped.num-ord-comp = plano-aprov.num-ord-comp and
                   ord-ped.seq-comp = plano-aprov.seq-comp no-lock no-error.
              if avail ord-ped then do:
                 if ord-ped.cod-sit-comp = "E" or
                    ord-ped.cod-sit-ped  = "E" then
                    next.
              end.     
           end.

           create t-controle.
           assign t-controle.seq          = 10
                  t-controle.ep-codigo    = plano-aprov.ep-codigo
                  t-controle.cod-est-exec = plano-aprov.cod-est-exec
                  t-controle.num-projeto  = plano-aprov.num-projeto
                  t-controle.num-ordem    = plano-aprov.num-ordem
                  t-controle.it-codigo    = plano-aprov.it-codigo
                  t-controle.num-ord-magnus = plano-aprov.num-ord-magnus
                  t-controle.dt-trans     = plano-aprov.dt-emiss-solic
                  t-controle.solicitacao  = plano-aprov.num-solicitacao
                  t-controle.ent-comp     = dec(substring(plano-aprov.char-1,1,15)) +
                                            (dec(substring(plano-aprov.char-1,16,2)) / 100).

                  run cdp/cd0812.p(input  0,
                                 input  i-cod-moeda-par,
                                 input  t-controle.ent-comp,
                                 input  t-controle.dt-trans,
                                 output t-controle.ent-comp).                          

                  if plano-aprov.tp-solic = "1" then
                     assign t-controle.tipo-doc = "Requisi‡Æo".
                  else
                     assign t-controle.tipo-doc = "Solicit".
        {inp/spp/esin0518.i3}
      end. 

      for each ord-ped
         where ord-ped.ep-codigo = controle-verba.ep-codigo
           and ord-ped.cod-est-exec = controle-verba.cod-est-exec
           and ord-ped.num-projeto = controle-verba.num-projeto
           and ord-ped.num-ordem = controle-verba.num-ordem
           and ord-ped.cod-sit-comp <> "E"
           and ord-ped.cod-sit-ped <> "E"
           no-lock:

           find plano-aprov where
                plano-aprov.ep-codigo = ord-ped.ep-codigo and
                plano-aprov.cod-area = ord-ped.cod-area and
                plano-aprov.num-ord-comp = ord-ped.num-ord-comp and
                plano-aprov.seq-comp = ord-ped.seq-comp no-lock no-error.
           if not avail plano-aprov then next.     

           create t-controle.
           assign t-controle.ep-codigo    = ord-ped.ep-codigo
                  t-controle.cod-est-exec = ord-ped.cod-est-exec
                  t-controle.num-projeto  = ord-ped.num-projeto
                  t-controle.num-ordem    = ord-ped.num-ordem
                  t-controle.it-codigo    = plano-aprov.it-codigo
                  t-controle.num-ord-magnus = plano-aprov.num-ord-magnus
                  t-controle.tipo-doc     = "Ordem"
                  t-controle.dt-trans     = ord-ped.data-1
                  t-controle.num-ord-comp = ord-ped.num-ord-comp
                  t-controle.seq-comp     = ord-ped.seq-comp
                  t-controle.solicitacao  = plano-aprov.num-solicitacao
                  t-controle.ent-comp     = ord-ped.dec-1 * ord-ped.quant-comp.

           &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
               IF ord-ped.nr-contrato <> 0 THEN DO:
                 if l-integra-cn-in or 
                    l-integra-cn-in-medicao then do:
    
                    assign T-CONTROLE.NUM-DOCTO = string(ord-ped.nr-contrato).
    
                    if l-integra-cn-in-medicao THEN DO:
                       IF  ord-ped.num-seq-item <> 0 THEN DO: 
    
                          assign T-CONTROLE.NUM-DOCTO = string(ord-ped.nr-contrato)  + "/" + 
                                                        string(ord-ped.num-seq-item) + "/" + 
                                                        string(ord-ped.num-seq-medicao) + "/" +
                                                        string(ord-ped.num-seq-event).
    
                       END.
                    END.
                 END.
               end.
           &else
                 IF SUBSTRING(ORD-PED.CHAR-2,1,9) <> "" THEN DO:
                   if l-integra-cn-in or 
                      l-integra-cn-in-medicao then do:
    
                      assign T-CONTROLE.NUM-DOCTO = substring(ord-ped.char-2,1,9).
    
                      if l-integra-cn-in-medicao THEN DO:
                         IF SUBSTRING(ord-ped.char-2,10,4) <> "" THEN DO: 
    
                            assign T-CONTROLE.NUM-DOCTO = trim(substring(ord-ped.char-2,1,9))  + "/" + 
                                                          trim(SUBSTRING(ord-ped.char-2,10,4)) + "/" + 
                                                          trim(SUBSTRING(ord-ped.char-2,18,4)) + "/" +
                                                          trim(substring(ord-ped.char-2,14,4)).
    
                         END.
                      END.
                   END.
                 end.
           &ENDIF
           if plano-aprov.num-solicitacao <> 0 then           
              assign t-controle.sai-comp = dec(substring(plano-aprov.char-1,1,15)) +
                                               (dec(substring(plano-aprov.char-1,16,2)) / 100).


           run cdp/cd0812.p(input  0,
                          input  i-cod-moeda-par,
                          input  t-controle.ent-comp,
                          input  t-controle.dt-trans,
                          output t-controle.ent-comp).

           if plano-aprov.num-solicitacao <> 0 then                
              run cdp/cd0812.p(input  0,
                             input  i-cod-moeda-par,
                             input  t-controle.sai-comp,
                             input  plano-aprov.dt-emiss-solic,
                             output t-controle.sai-comp).

           if plano-aprov.num-solicitacao <> 0 then             
              assign t-controle.seq = 10.
           else
              assign t-controle.seq = 20.                                 

         {inp/spp/esin0518.i3}
      end.       


      for each ord-ped
         where ord-ped.ep-codigo = controle-verba.ep-codigo
           and ord-ped.cod-est-exec = controle-verba.cod-est-exec
           and ord-ped.num-projeto = controle-verba.num-projeto
           and ord-ped.num-ordem = controle-verba.num-ordem
           and ord-ped.num-pedido <> 0 
           and (ord-ped.cod-sit-ped = "A"
                or ord-ped.cod-sit-ped = "F") 
           and ord-ped.cod-sit-comp <> "E"  no-lock:

           find plano-aprov where
                plano-aprov.ep-codigo = ord-ped.ep-codigo and
                plano-aprov.cod-area = ord-ped.cod-area and
                plano-aprov.num-ord-comp = ord-ped.num-ord-comp and
                plano-aprov.seq-comp = ord-ped.seq-comp no-lock no-error.
           if not avail plano-aprov then next.     

           create t-controle.
           assign t-controle.ep-codigo    = ord-ped.ep-codigo
                  t-controle.cod-est-exec = ord-ped.cod-est-exec
                  t-controle.num-projeto  = ord-ped.num-projeto
                  t-controle.num-ordem    = ord-ped.num-ordem
                  t-controle.it-codigo    = plano-aprov.it-codigo
                  t-controle.num-ord-magnus = plano-aprov.num-ord-magnus
                  t-controle.tipo-doc     = "Pedido"
                  t-controle.dt-trans     = ord-ped.dt-pedido
                  t-controle.num-ord-comp = ord-ped.num-ord-comp
                  t-controle.seq-comp     = ord-ped.seq-comp
                  t-controle.solicitacao  = plano-aprov.num-solicitacao
                  t-controle.num-pedido   = ord-ped.num-pedido
                  t-controle.ent-comp     = ord-ped.vl-item[1]
                  t-controle.sai-comp     = ord-ped.dec-1 * ord-ped.quant-comp.

           /*Log339640*/
           find first B-T-CONTROLE no-lock
                where B-t-controle.tipo-doc     = "ORDEM" 
                and   B-T-CONTROLE.NUM-ORD-COMP = T-CONTROLE.NUM-ORD-COMP no-error.

           if avail B-T-CONTROLE then do:
               run utp/ut-liter.p (input "Item_Contrato",
                                   input "",
                                   input "") no-error.

               if (can-find (first controle-inv-esp 
                            where controle-inv-esp.ep-codigo    = controle-verba.ep-codigo   
                            and   controle-inv-esp.cod-est-exec = controle-verba.cod-est-exec
                            and   controle-inv-esp.num-projeto  = controle-verba.num-projeto 
                            and   controle-inv-esp.tipo-doc     = "Contrato"
                            and   controle-inv-esp.NRO-DOCTO    = ENTRY(1, B-T-CONTROLE.NUM-DOCTO, "/") )
                   or
                   can-find (first controle-inv-esp 
                            where controle-inv-esp.ep-codigo    = controle-verba.ep-codigo   
                            and   controle-inv-esp.cod-est-exec = controle-verba.cod-est-exec
                            and   controle-inv-esp.num-projeto  = controle-verba.num-projeto 
                            and   controle-inv-esp.tipo-doc     = return-value
                            and   ENTRY(1, controle-inv-esp.NRO-DOCTO, "/") = ENTRY(1, B-T-CONTROLE.NUM-DOCTO, "/") 
                            AND   ENTRY(2, controle-inv-esp.NRO-DOCTO, "/") = ENTRY(2, B-T-CONTROLE.NUM-DOCTO, "/") ))
               then do:
                   assign t-controle.sai-comp = t-controle.ent-comp.
               end.
           end.
           /*Log339640*/
           
        run cdp/cd0812.p(input  param-inv.moeda-inv,
                          input  i-cod-moeda-par,
                          input  t-controle.ent-comp,
                          input  ord-ped.dt-pedido,
                          output t-controle.ent-comp).

           run cdp/cd0812.p(input  0,
                          input  i-cod-moeda-par,
                          input  t-controle.sai-comp,
                          input  ord-ped.data-1,
                          output t-controle.sai-comp).

           if plano-aprov.num-solicitacao <> 0 then             
              assign t-controle.seq = 10.
           else
              assign t-controle.seq = 20.                                 

           {inp/spp/esin0518.i3}
      end.       

      for each movto-nf
         where movto-nf.ep-codigo = controle-verba.ep-codigo
           and movto-nf.cod-est-exec = controle-verba.cod-est-exec
           and movto-nf.num-projeto = controle-verba.num-projeto
           and movto-nf.num-ordem = controle-verba.num-ordem
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

           create t-controle.
           assign t-controle.ep-codigo    = movto-nf.ep-codigo
                  t-controle.cod-est-exec = movto-nf.cod-est-exec
                  t-controle.num-projeto  = movto-nf.num-projeto
                  t-controle.num-ordem    = movto-nf.num-ordem
                  t-controle.it-codigo    = plano-aprov.it-codigo
                  t-controle.num-ord-magnus = plano-aprov.num-ord-magnus
                  t-controle.tipo-doc     = "Nota Fisc"
                  t-controle.dt-trans     = movto-nf.dt-trans
                  t-controle.num-ord-comp = movto-nf.num-ord-comp
                  t-controle.seq-comp     = ord-ped.seq-comp
                  t-controle.solicitacao  = plano-aprov.num-solicitacao
                  t-controle.num-pedido   = movto-nf.num-pedido
                  t-controle.num-docto    = movto-nf.serie-docto + "/" +
                                            substring(movto-nf.nro-docto,1,10) + "/" +
                                            movto-nf.nat-operacao + "/" +
                                            string(movto-nf.sequencia,">>>>9")
                  t-controle.ent-real     = movto-nf.vl-nota[1].

            run cdp/cd0812.p (input  param-inv.moeda-inv,
                              input  i-cod-moeda-par,
                              input  t-controle.ent-real,
                              input  movto-nf.dt-trans,
                              output t-controle.ent-real).  

           if ord-ped.cod-sit-comp <> "E" and ord-ped.cod-sit-ped <> "E" then do:                  
                assign t-controle.sai-comp = movto-nf.quant-doc-ent * 
                                             (ord-ped.vl-item[1] / ord-ped.quant-comp).
                                             
                &if '{&BF_MAT_VERSAO_EMS}' = '2.04':U &then
                     if  can-find (funcao where funcao.cd-funcao = "spp-atu-comp-receb" 
                                            and funcao.ativo) THEN DO:
                     
                         assign t-controle.sai-comp = dec(substr(movto-nf.char-1,39,18)).
                     END.
                &endif
                
                
                &if '{&BF_MAT_VERSAO_EMS}' > '2.04':U and '{&BF_MAT_VERSAO_EMS}' <= '2.062':U &then
                     assign t-controle.sai-comp = dec(substr(movto-nf.char-1,39,18)).
                &endif
                 
                
                &if '{&BF_MAT_VERSAO_EMS}' > '2.062':U &then
                     assign t-controle.sai-comp = movto-nf.val-cpromis.
                &endif

             IF ord-ped.data-2 <> ? THEN
                 RUN cdp/cd0812.p (INPUT  param-inv.moeda-inv,
                                   INPUT  i-cod-moeda-par,
                                   INPUT  t-controle.sai-comp,
                                   INPUT  ord-ped.data-2,
                                   OUTPUT t-controle.sai-comp).
             ELSE                                              
                 RUN cdp/cd0812.p (INPUT  param-inv.moeda-inv,
                                   INPUT  i-cod-moeda-par,
                                   INPUT  t-controle.sai-comp,
                                   INPUT  ord-ped.data-1,
                                   OUTPUT t-controle.sai-comp).
           end.

           /* Devolu‡Æo ao Fornecedor: Gera entrada no compromissado e sa¡da no realizado */
           &IF "{&bf_mat_versao_EMS}" >= "2.04" &THEN
               if  movto-nf.tipo-trans = 2 then
                   assign t-controle.sai-real = t-controle.ent-real
                          t-controle.ent-real = 0
                          t-controle.ent-comp = t-controle.sai-comp
                          t-controle.sai-comp = 0.
           &endif.

           if plano-aprov.num-solicitacao <> 0 then
               assign t-controle.seq = 10.
           else
               assign t-controle.seq = 20.

         {inp/spp/esin0518.i3}
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
           no-lock:      
           if movto-apr.transacao = "IMD" and
              movto-apr.num-pedido <> 0 then next.             

           create t-controle.
           assign t-controle.ep-codigo    = movto-apr.ep-codigo
                  t-controle.cod-est-exec = movto-apr.cod-est-exec
                  t-controle.num-projeto  = movto-apr.num-projeto
                  t-controle.num-ordem    = movto-apr.num-ordem
                  t-controle.it-codigo    = movto-apr.it-codigo
                  t-controle.num-ord-magnus = movto-apr.num-ord-magnus
                  t-controle.dt-trans     = movto-apr.dt-trans 
                  t-controle.esp-docto    = movto-apr.esp-docto
                  t-controle.num-docto    = movto-apr.nro-docto
                  t-controle.ent-real     = movto-apr.vl-mat[1] + 
                                            movto-apr.vl-mob[1].

           if movto-apr.transacao = "IMD" or 
              movto-apr.transacao = "PEF" then
              assign t-controle.num-docto = movto-apr.serie-docto    + "/" + 
                                            movto-apr.nro-docto      + "/" + 
                                            movto-apr.parcela        + "/" +
                                            trim(string(movto-apr.cod-emitente,">>>>>>>>9")). 



           if movto-apr.tipo-trans = 2 then
              assign t-controle.ent-real = t-controle.ent-real * -1.    

           run cdp/cd0812.p(input  param-inv.moeda-inv,
                          input  i-cod-moeda-par,
                          input  t-controle.ent-real,
                          input  movto-apr.dt-trans,
                          output t-controle.ent-real).    

           if movto-apr.transacao = "REQ" then do:                                 
                if movto-apr.tipo-trans = 1 then                              
                   assign t-controle.tipo-doc = "Atend. REQ".
                else
                   assign t-controle.tipo-doc = "Devol. REQ".
           end.
           else do:
               assign t-controle.tipo-doc = movto-apr.transacao.
           end.        

           if movto-apr.transacao = "REQ" or movto-apr.transacao = "ACE" then do:
              assign t-controle.seq = 10.
              find plano-aprov where
                   plano-aprov.ep-codigo = movto-apr.ep-codigo and
                   plano-aprov.cod-estabel = movto-apr.cod-estabel and
                   plano-aprov.num-solicitacao = int(movto-apr.nro-docto) and
                   plano-aprov.seq-solic = int(movto-apr.parcela) and
                   plano-aprov.tp-solic = "1" 
                   no-lock no-error.
              if avail plano-aprov then     
                  assign t-controle.solicitacao = plano-aprov.num-solicitacao.   

              if movto-apr.log-1 then do: /* Executou o in2301 */                    
                if avail plano-aprov then do:                     
                     assign t-controle.sai-comp = dec(substring(plano-aprov.char-1,1,15)) +
                                                   (dec(substring(plano-aprov.char-1,16,2)) / 100).
                     assign t-controle.sai-comp = t-controle.sai-comp / plano-aprov.quant-solic.
                     assign t-controle.sai-comp = t-controle.sai-comp * movto-apr.quant-mov.                                 
                            t-controle.solicitacao  = plano-aprov.num-solicitacao.                        
                     run cdp/cd0812.p(input  0,
                                    input  i-cod-moeda-par,
                                    input  t-controle.sai-comp,
                                    input  plano-aprov.dt-emiss-solic,
                                    output t-controle.sai-comp).                  
                end.                
              end.

              if movto-apr.tipo-trans = 2 then do:                 
                 assign t-controle.ent-real = t-controle.ent-real * -1
                        t-controle.ent-comp = t-controle.sai-comp
                        t-controle.sai-comp = 0
                        t-controle.sai-real = t-controle.ent-real
                        t-controle.ent-real = 0.                          
              end.                             
           end.              
           else if movto-apr.transacao = "DIV" or movto-apr.transacao = "ACE" then 
              assign t-controle.seq = 40.
           else
              assign t-controle.seq = 50.   

        {inp/spp/esin0518.i3}
      end.           


      for each ordem-man
         where ordem-man.ep-codigo = controle-verba.ep-codigo
           and ordem-man.cod-est-exec = controle-verba.cod-est-exec
           and ordem-man.num-projeto = controle-verba.num-projeto
           and ordem-man.num-ordem = controle-verba.num-ordem
           no-lock:                     

           create t-controle.
           assign t-controle.ep-codigo    = ordem-man.ep-codigo
                  t-controle.cod-est-exec = ordem-man.cod-est-exec
                  t-controle.num-projeto  = ordem-man.num-projeto
                  t-controle.num-ordem    = ordem-man.num-ordem
                  t-controle.it-codigo    = ordem-man.char-2
                  t-controle.num-ord-magnus = ordem-man.num-ord-magnus                  
                  t-controle.dt-trans     = ordem-man.dt-cadastro
                  t-controle.num-ord-comp = 0
                  t-controle.solicitacao  = 0
                  t-controle.num-pedido   = 0
                  t-controle.num-docto    = string(ordem-man.num-ord-man)
                  t-controle.ent-real     = ordem-man.vl-material[1] + ordem-man.vl-servico[1] + 
                                            ordem-man.vl-mob[1] + decimal(substring(ordem-man.char-1,1,18)).
           if ordem-man.log-2 then
              assign t-controle.tipo-doc = "Ord Manut".
           else   
              assign t-controle.tipo-doc = "Ord Prod".

            run cdp/cd0812.p (input  param-inv.moeda-inv,
                              input  i-cod-moeda-par,
                              input  t-controle.ent-real,
                              input  ordem-man.dt-cadastro,
                              output t-controle.ent-real).  



            assign t-controle.seq = 50.

        {inp/spp/esin0518.i3}
      end.              

    {utp/ut-liter.i Apontamento_MOB}
    FOR EACH  controle-inv-esp NO-LOCK
        WHERE controle-inv-esp.ep-codigo    = controle-verba.ep-codigo   
        AND   controle-inv-esp.cod-est-exec = controle-verba.cod-est-exec
        AND   controle-inv-esp.num-projeto  = controle-verba.num-projeto 
          and   controle-inv-esp.num-ordem    = controle-verba.num-ordem
          and   controle-inv-esp.tipo-doc     = return-value:

        CREATE t-controle.
        ASSIGN t-controle.dt-trans       = controle-inv-esp.dt-trans
               t-controle.it-codigo      = STRING(controle-inv-esp.item)
               t-controle.num-ord-magnus = controle-inv-esp.num-ord-inv
               t-controle.tipo-doc       = controle-inv-esp.tipo-doc
               t-controle.solicitacao    = 0
               t-controle.num-ord-comp   = controle-inv-esp.numero-ordem
               t-controle.num-pedido     = controle-inv-esp.num-pedido
               t-controle.ent-comp       = 0
               t-controle.sai-comp       = 0
               t-controle.ent-real       = controle-inv-esp.valor-origem
               t-controle.sai-real       = 0
               t-controle.ep-codigo      = controle-inv-esp.ep-codigo
               t-controle.cod-est-exec   = controle-inv-esp.cod-est-exec
               t-controle.num-projeto    = controle-inv-esp.num-projeto
               t-controle.num-ordem      = controle-inv-esp.num-ordem.

          IF  controle-inv-esp.nro-docto <> "" THEN
              ASSIGN t-controle.num-docto = controle-inv-esp.serie-docto  + "/" +
                                            controle-inv-esp.nro-docto    + "/" +
                                            controle-inv-esp.nat-operacao + "/" +
                                     STRING(controle-inv-esp.sequencia,">>>>9").

        RUN cdp/cd0812.p (INPUT  param-inv.moeda-inv,
                          INPUT  i-cod-moeda-par,
                          INPUT  t-controle.ent-real,
                          INPUT  t-controle.dt-trans,
                          OUTPUT t-controle.ent-real).  

        ASSIGN t-controle.seq = 60.

        {inp/spp/esin0518.i3}
    END.

    {utp/ut-liter.i Despesa_de_Importa‡Æo}
    FOR EACH  controle-inv-esp NO-LOCK
        WHERE controle-inv-esp.ep-codigo    = controle-verba.ep-codigo   
        AND   controle-inv-esp.cod-est-exec = controle-verba.cod-est-exec
        AND   controle-inv-esp.num-projeto  = controle-verba.num-projeto 
          and   controle-inv-esp.num-ordem    = controle-verba.num-ordem
          and   controle-inv-esp.tipo-doc     = return-value:

        CREATE t-controle.
        ASSIGN t-controle.dt-trans       = controle-inv-esp.dt-trans
               t-controle.it-codigo      = STRING(controle-inv-esp.item)
               t-controle.num-ord-magnus = controle-inv-esp.num-ord-inv
               t-controle.tipo-doc       = controle-inv-esp.tipo-doc
               t-controle.solicitacao    = 0
               t-controle.num-ord-comp   = controle-inv-esp.numero-ordem
               t-controle.num-pedido     = controle-inv-esp.num-pedido
               t-controle.ent-comp       = controle-inv-esp.ent-comp
               t-controle.sai-comp       = controle-inv-esp.sai-comp
               t-controle.ent-real       = controle-inv-esp.ent-real
               t-controle.sai-real       = controle-inv-esp.sai-real
               t-controle.ep-codigo      = controle-inv-esp.ep-codigo
               t-controle.cod-est-exec   = controle-inv-esp.cod-est-exec
               t-controle.num-projeto    = controle-inv-esp.num-projeto
               t-controle.num-ordem      = controle-inv-esp.num-ordem.
                           
          IF  controle-inv-esp.nro-docto <> "" THEN
              ASSIGN t-controle.num-docto = controle-inv-esp.serie-docto  + "/" +
                                            controle-inv-esp.nro-docto    + "/" +
                                            controle-inv-esp.nat-operacao + "/" +
                                     STRING(controle-inv-esp.sequencia,">>>>9").

        run cdp/cd0812.p (INPUT  param-inv.moeda-inv,
                          INPUT  i-cod-moeda-par,
                          INPUT  t-controle.ent-real,
                          INPUT  t-controle.dt-trans,
                          OUTPUT t-controle.ent-real).  

        ASSIGN t-controle.seq = 70.

        {inp/spp/esin0518.i3}
    END.
    
    {utp/ut-liter.i Contrato}
    FOR EACH  controle-inv-esp NO-LOCK
        WHERE controle-inv-esp.ep-codigo    = controle-verba.ep-codigo   
        AND   controle-inv-esp.cod-est-exec = controle-verba.cod-est-exec
        AND   controle-inv-esp.num-projeto  = controle-verba.num-projeto 
          and   controle-inv-esp.num-ordem    = controle-verba.num-ordem
          and   controle-inv-esp.tipo-doc     = return-value:

        CREATE t-controle.
        ASSIGN t-controle.dt-trans       = controle-inv-esp.dt-trans
               t-controle.it-codigo      = STRING(controle-inv-esp.item)
               t-controle.num-ord-magnus = controle-inv-esp.num-ord-inv
               t-controle.tipo-doc       = controle-inv-esp.tipo-doc
               t-controle.solicitacao    = 0
               t-controle.num-ord-comp   = controle-inv-esp.numero-ordem
               t-controle.num-pedido     = controle-inv-esp.num-pedido
               t-controle.ent-comp       = controle-inv-esp.ent-comp
               t-controle.sai-comp       = controle-inv-esp.sai-comp
               t-controle.ent-real       = controle-inv-esp.ent-real
               t-controle.sai-real       = controle-inv-esp.sai-real
               t-controle.ep-codigo      = controle-inv-esp.ep-codigo
               t-controle.cod-est-exec   = controle-inv-esp.cod-est-exec
               t-controle.num-projeto    = controle-inv-esp.num-projeto
               t-controle.num-ordem      = controle-inv-esp.num-ordem.
               
          IF  controle-inv-esp.nro-docto <> "" THEN
              ASSIGN t-controle.num-docto = controle-inv-esp.serie-docto  + "/" +
                                            controle-inv-esp.nro-docto    + "/" +
                                            controle-inv-esp.nat-operacao + "/" +
                                     STRING(controle-inv-esp.sequencia,">>>>9").

        RUN cdp/cd0812.p (INPUT  param-inv.moeda-inv,
                          INPUT  i-cod-moeda-par,
                          INPUT  t-controle.ent-real,
                          INPUT  t-controle.dt-trans,
                          OUTPUT t-controle.ent-real).  

        ASSIGN t-controle.seq = 80.

        {inp/spp/esin0518.i3}
    END.
    
    {utp/ut-liter.i Item_Contrato}
    FOR EACH  controle-inv-esp NO-LOCK
        WHERE controle-inv-esp.ep-codigo    = controle-verba.ep-codigo   
        AND   controle-inv-esp.cod-est-exec = controle-verba.cod-est-exec
        AND   controle-inv-esp.num-projeto  = controle-verba.num-projeto 
          and   controle-inv-esp.num-ordem    = controle-verba.num-ordem
          and   controle-inv-esp.tipo-doc     = return-value:

        CREATE t-controle.
        ASSIGN t-controle.dt-trans       = controle-inv-esp.dt-trans
               t-controle.it-codigo      = STRING(controle-inv-esp.item)
               t-controle.num-ord-magnus = controle-inv-esp.num-ord-inv
               t-controle.tipo-doc       = controle-inv-esp.tipo-doc
               t-controle.solicitacao    = 0
               t-controle.num-ord-comp   = controle-inv-esp.numero-ordem
               t-controle.num-pedido     = controle-inv-esp.num-pedido
               t-controle.ent-comp       = controle-inv-esp.ent-comp
               t-controle.sai-comp       = controle-inv-esp.sai-comp
               t-controle.ent-real       = controle-inv-esp.ent-real
               t-controle.sai-real       = controle-inv-esp.sai-real
               t-controle.ep-codigo      = controle-inv-esp.ep-codigo
               t-controle.cod-est-exec   = controle-inv-esp.cod-est-exec
               t-controle.num-projeto    = controle-inv-esp.num-projeto
               t-controle.num-ordem      = controle-inv-esp.num-ordem.

            IF  controle-inv-esp.nro-docto <> "" THEN
              ASSIGN t-controle.num-docto = controle-inv-esp.serie-docto  + "/" +
                                            controle-inv-esp.nro-docto    + "/" +
                                            controle-inv-esp.nat-operacao + "/" +
                                     STRING(controle-inv-esp.sequencia,">>>>9").
            
        run cdp/cd0812.p (INPUT  param-inv.moeda-inv,
                          INPUT  i-cod-moeda-par,
                          INPUT  t-controle.ent-real,
                          INPUT  t-controle.dt-trans,
                          OUTPUT t-controle.ent-real).  

        ASSIGN t-controle.seq = 80.

        {inp/spp/esin0518.i3}
    END.
end.

RUN pi-principal.
if not tt-param.log-gera-excel then do:
    RUN pi-parametros.

    {include/i-rpclo.i}
 
/**********************************************************************
*  in0518.i5
*  Cabe‡alho
***********************************************************************/

procedure INIC-FORMS.

  /****************************************************************************
   **  Identico: I-RPCAB.I - Form do Cabe‡alho PadrÆo e Rodap‚ (ex-CD9500.F)
   **              {&STREAM} - indica o nome da stream (opcional)
   ****************************************************************************/

  /* Rodape */
  c-rodape = "DATASUL - " + c-sistema + " - " + c-prg-obj + " - V:" + c-prg-vrs.
  c-rodape = fill("-", 233 - length(c-rodape)) + c-rodape.
  form header
       c-rodape format 'x(233)'
       with stream-io width 233 no-labels no-box page-bottom frame f-rodape.

  /* Cabecalho */
  &IF "{&LANGUAGE-CODE}" = "ING" &THEN 
     &IF  "{&STREAM}" = "" &THEN
        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Page:" at 224 page-number  at 229 format ">>>>9" skip
            fill("-", 213) format "x(211)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabec.

        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Page:" at 224 page-number  at 229 format ">>>>9" skip
            "Period:" i-numper-x at 09 "-"
            da-iniper-x at 14 "to" da-fimper-x
            fill("-", 175) format "x(173)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabper.
     &ELSE
        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Page:" at 224 page-number({&STREAM})  at 229 format ">>>>9" skip
            fill("-", 213) format "x(211)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabec.

        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Page:" at 224 page-number({&STREAM})  at 229 format ">>>>9" skip
            "Period:" i-numper-x at 10 "-"
            da-iniper-x at 14 "to" da-fimper-x
            fill("-", 175) format "x(173)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabper.
     &ENDIF

  &ELSEIF "{&LANGUAGE-CODE}" = "ESP" &THEN
     &IF  "{&STREAM}" = "" &THEN
        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "P gina:" at 222 page-number  at 227 format ">>>>9" skip
            fill("-", 213) format "x(211)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabec.

        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "P gina:" at 222 page-number  at 227 format ">>>>9" skip
            "Periodo:" i-numper-x at 10 "-"
            da-iniper-x at 15 "hasta" da-fimper-x
            fill("-", 167) format "x(165)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabper.
     &ELSE
        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "P gina:" at 222 page-number({&STREAM})  at 227 format ">>>>9" skip
            fill("-", 213) format "x(211)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabec.

        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "P gina:" at 222 page-number({&STREAM})  at 227 format ">>>>9" skip
            "Periodo:" i-numper-x at 10 "-"
            da-iniper-x at 15 "hasta" da-fimper-x
            fill("-", 167) format "x(165)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabper.
     &ENDIF

  &ELSE
     &IF "{&STREAM}" = "" &THEN
        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Folha:"  at 223 page-number  at 229 format ">>>>9" skip
            fill("-", 213) format "x(211)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabec.

        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Folha:" at 223 page-number  at 229 format ">>>>9" skip
            "Periodo:" i-numper-x at 10 "-"
            da-iniper-x at 15 "a" da-fimper-x
            fill("-", 163) format "x(161)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabper.
     &ELSE
        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Folha:" at 223 page-number({&STREAM})  at 229 format ">>>>9" skip
            fill("-", 213) format "x(211)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabec.

        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Folha:" at 223 page-number({&STREAM})  at 229 format ">>>>9" skip
            "Periodo:" i-numper-x at 10 "-"
            da-iniper-x at 15 "a" da-fimper-x
            fill("-", 163) format "x(161)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabper.
     &ENDIF
  &ENDIF

end procedure.

/* Fim esin0518.i5 *************************/
 
end.
ELSE DO:
    OUTPUT TO VALUE(SESSION:TEMP-DIR + "esin0518.tmp").
    os-command VALUE('start excel' + " " + CHR(34) + tt-param.caminho-excel + CHR(34)).
    put unformatted "Gera‡Æo de Arquivo em Planilha do EXCEL!"               skip
                    "O arquivo gerado foi salvo em: " tt-param.caminho-excel skip.
    OUTPUT CLOSE.
END.

run pi-finalizar in h-acomp.
return 'ok'.

PROCEDURE pi-principal:

    if  c-nom-prog-dpc-mg97  <> ""
    or  c-nom-prog-appc-mg97 <> ""
    or  c-nom-prog-upc-mg97  <> "" then do:
        /* Chamada EPC */
        for each tt-epc
            where tt-epc.cod-event = "modify-report":U:
            delete tt-epc.
        end.
        
        /* Include i-epc200.i2: Criação de registro para Temp-Table tt-epc */
        {include/i-epc200.i2 &CodEvent='"modify-report"'
                             &CodParameter='"handle-tt-param"'
                             &ValueParameter="string(temp-table tt-param:handle)"}
        
        {include/i-epc200.i2 &CodEvent='"modify-report"'
                             &CodParameter='"handle-t-controle"'
                             &ValueParameter="string(temp-table t-controle:handle)"}
        
        /* Include i-epc201.i: Chamada do programa de EPC */
        {include/i-epc201.i "modify-report"}
          
        if return-value = "NOK":U then
            undo, leave.
    end.

    /* GIL - Elimina as informa‡äes que estÆo fora da faixa de datas */
  /*  FOR EACH t-controle
       WHERE t-controle.dt-trans < tt-param.d-data-ini
          OR t-controle.dt-trans > tt-param.d-data-fim:
        DELETE t-controle.
    END.*/

    if not tt-param.log-gera-excel then do:
        if tt-param.classif-1 = 1 THEN DO:
           /*******************************************************************************
* ESIN0518.I - ImpressÆo
********************************************************************************/
run pi-inicializar in h-acomp(input c-imprimindo).
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
            
     
      run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                         string(controle-verba.num-projeto) + "  " + 
                                         c-lb-ordem + 
                                         string(controle-verba.num-ordem))).     
      if first-of (controle-verba.ep-codigo) or
         not avail empresa then do:
         find empresa
              where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
         find param-inv 
              where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
         run inp/in9997.p (input 1).
         assign c-moedas-inv [1] = Trim(return-value).
         run inp/in9997.p (input 2).
         assign c-moedas-inv [2] = Trim(return-value).
         run inp/in9997.p (input 3).
         assign c-moedas-inv [3] = Trim(return-value). 
      end.
      if first-of (controle-verba.cod-est-exec) or
         not avail estabelec then    
         find estabelec
              where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
      if first-of (controle-verba.num-projeto) or 
         not avail proj-inv then         
         find proj-inv
             where proj-inv.ep-codigo = controle-verba.ep-codigo
               and proj-inv.cod-est-exec = controle-verba.cod-est-exec
               and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
      if first-of (controle-verba.num-ordem) or
         not avail ordem-inv then do:
         find ordem-inv
              where ordem-inv.ep-codigo = controle-verba.ep-codigo
                and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                and ordem-inv.num-projeto = controle-verba.num-projeto
                and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
         
         &if '{&bf_mat_versao_ems}' >= '2.062' &then
         ASSIGN i-cnt-unid-negoc = 1.
          DO  WHILE i-cnt-unid-negoc < 34:
              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
          END.
          
          FOR EACH mat-rat-med-inv NO-LOCK
              WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                AND mat-rat-med-inv.numero-ordem = 0
                AND mat-rat-med-inv.nr-contrato  = 0:
              IF  c-txt-unid-negoc[1] = "" THEN DO:
                  {utp/ut-liter.i Matriz_Unidade_de_Neg¢cio}
                  ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                         i-cnt-unid-negoc = 1.
              END.
              
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
              IF  i-cnt-unid-negoc < 34 THEN DO:
                 {utp/ut-liter.i Matriz_Unidade_de_Neg¢cio}
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                 &if '{&bf_mat_versao_ems}' = '2.062' &then
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                 &else
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                 &endif
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
              END.
          END.
         &endif
      end.
     
      if controle-verba.situacao = 1 then 
         assign c-situacao = c-situacao-1.
      else 
         assign c-situacao = c-situacao-2.      

      assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
 
      assign de-acum-comp = 0
             de-acum-real = 0.

      if tt-param.atualiza then do:
          for each t-controle
             where t-controle.ep-codigo    = controle-verba.ep-codigo
               and t-controle.cod-est-exec = controle-verba.cod-est-exec
               and t-controle.num-projeto  = controle-verba.num-projeto
               and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
               assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                      de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
          end.

          if de-acum-comp  < 0 then assign de-acum-comp = 0.
          if de-acum-real < 0 then assign de-acum-real = 0.

          find first b-controle-verba exclusive-lock
              where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
          if avail b-controle-verba then
            assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                   b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
      end.

      {inp/spp/esin0518.i4}
           
      assign de-acum-comp = 0
             de-acum-real = 0
             l-imprimiu   = no.
                   
      for each t-controle
         where t-controle.ep-codigo    = controle-verba.ep-codigo
           and t-controle.cod-est-exec = controle-verba.cod-est-exec
           and t-controle.num-projeto  = controle-verba.num-projeto
           and t-controle.num-ordem    = controle-verba.num-ordem
           no-lock break by t-controle.it-codigo by t-controle.seq by t-controle.solicitacao by t-controle.num-ord-comp by t-controle.seq-comp by t-controle.num-pedido by t-controle.esp-docto by t-controle.num-docto:
           assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                  de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                  t-controle.acum-comp = de-acum-comp
                  t-controle.acum-real = de-acum-real
                  l-imprimiu           = yes.
           if line-counter < 4 then do:
              {inp/spp/esin0518.i4}
           end.
       
           IF t-controle.dt-trans > d-data-fim OR
              t-controle.dt-trans < d-data-ini THEN NEXT.
             
           disp t-controle.dt-trans   
                t-controle.it-codigo
                t-controle.num-ord-magnus
                t-controle.tipo-doc
                t-controle.solicitacao      
                t-controle.num-ord-comp
                t-controle.num-pedido
                t-controle.esp-docto
                t-controle.num-docto
                t-controle.ent-comp   
                t-controle.sai-comp   
                t-controle.ent-real   
                t-controle.sai-real 
                t-controle.acum-comp  
                t-controle.acum-real
                with frame f-controle.
                down with frame f-controle.


            /** CHAMADA UPC **/   
    
            for each tt-epc:
                delete tt-epc.
            end.    
             
            create tt-epc.
            assign tt-epc.cod-event     = "apos-display-frame-controle":U
                   tt-epc.cod-parameter = "controle-verba-rowid"
                   tt-epc.val-parameter = string(rowid(controle-verba)). 
                  
            {include/i-epc201.i "apos-display-frame-controle"}        
          
            if return-value = 'NOK' then do:
                undo,leave.
            end.
        
            /********************/    

      end.   
      if l-imprimiu = yes then do:
         if line-counter + 4 > page-size then do:
            page.
            {inp/spp/esin0518.i4}
         end.
         if de-acum-comp  < 0 then assign de-acum-comp = 0.
         if de-acum-real < 0 then assign de-acum-real = 0.
         assign de-acum-tot = de-acum-comp + de-acum-real.
         disp c-lb-total
              de-acum-tot
              with frame f-total. 
      end.        
      page. 
end.
 
        END.
        else if tt-param.classif-1 = 2 THEN DO:
           /*******************************************************************************
* ESIN0518.I - ImpressÆo
********************************************************************************/
run pi-inicializar in h-acomp(input c-imprimindo).
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
            
     
      run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                         string(controle-verba.num-projeto) + "  " + 
                                         c-lb-ordem + 
                                         string(controle-verba.num-ordem))).     
      if first-of (controle-verba.ep-codigo) or
         not avail empresa then do:
         find empresa
              where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
         find param-inv 
              where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
         run inp/in9997.p (input 1).
         assign c-moedas-inv [1] = Trim(return-value).
         run inp/in9997.p (input 2).
         assign c-moedas-inv [2] = Trim(return-value).
         run inp/in9997.p (input 3).
         assign c-moedas-inv [3] = Trim(return-value). 
      end.
      if first-of (controle-verba.cod-est-exec) or
         not avail estabelec then    
         find estabelec
              where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
      if first-of (controle-verba.num-projeto) or 
         not avail proj-inv then         
         find proj-inv
             where proj-inv.ep-codigo = controle-verba.ep-codigo
               and proj-inv.cod-est-exec = controle-verba.cod-est-exec
               and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
      if first-of (controle-verba.num-ordem) or
         not avail ordem-inv then do:
         find ordem-inv
              where ordem-inv.ep-codigo = controle-verba.ep-codigo
                and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                and ordem-inv.num-projeto = controle-verba.num-projeto
                and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
         
         &if '{&bf_mat_versao_ems}' >= '2.062' &then
         ASSIGN i-cnt-unid-negoc = 1.
          DO  WHILE i-cnt-unid-negoc < 34:
              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
          END.
          
          FOR EACH mat-rat-med-inv NO-LOCK
              WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                AND mat-rat-med-inv.numero-ordem = 0
                AND mat-rat-med-inv.nr-contrato  = 0:
              IF  c-txt-unid-negoc[1] = "" THEN DO:
                  {utp/ut-liter.i Matriz_Unidade_de_Neg¢cio}
                  ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                         i-cnt-unid-negoc = 1.
              END.
              
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
              IF  i-cnt-unid-negoc < 34 THEN DO:
                 {utp/ut-liter.i Matriz_Unidade_de_Neg¢cio}
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                 &if '{&bf_mat_versao_ems}' = '2.062' &then
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                 &else
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                 &endif
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
              END.
          END.
         &endif
      end.
     
      if controle-verba.situacao = 1 then 
         assign c-situacao = c-situacao-1.
      else 
         assign c-situacao = c-situacao-2.      

      assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
 
      assign de-acum-comp = 0
             de-acum-real = 0.

      if tt-param.atualiza then do:
          for each t-controle
             where t-controle.ep-codigo    = controle-verba.ep-codigo
               and t-controle.cod-est-exec = controle-verba.cod-est-exec
               and t-controle.num-projeto  = controle-verba.num-projeto
               and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
               assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                      de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
          end.

          if de-acum-comp  < 0 then assign de-acum-comp = 0.
          if de-acum-real < 0 then assign de-acum-real = 0.

          find first b-controle-verba exclusive-lock
              where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
          if avail b-controle-verba then
            assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                   b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
      end.

      {inp/spp/esin0518.i4}
           
      assign de-acum-comp = 0
             de-acum-real = 0
             l-imprimiu   = no.
                   
      for each t-controle
         where t-controle.ep-codigo    = controle-verba.ep-codigo
           and t-controle.cod-est-exec = controle-verba.cod-est-exec
           and t-controle.num-projeto  = controle-verba.num-projeto
           and t-controle.num-ordem    = controle-verba.num-ordem
           no-lock break by t-controle.dt-trans by t-controle.seq by t-controle.solicitacao by t-controle.num-ord-comp by t-controle.seq-comp by t-controle.num-pedido by t-controle.esp-docto by t-controle.num-docto:
           assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                  de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                  t-controle.acum-comp = de-acum-comp
                  t-controle.acum-real = de-acum-real
                  l-imprimiu           = yes.
           if line-counter < 4 then do:
              {inp/spp/esin0518.i4}
           end.
       
           IF t-controle.dt-trans > d-data-fim OR
              t-controle.dt-trans < d-data-ini THEN NEXT.
             
           disp t-controle.dt-trans   
                t-controle.it-codigo
                t-controle.num-ord-magnus
                t-controle.tipo-doc
                t-controle.solicitacao      
                t-controle.num-ord-comp
                t-controle.num-pedido
                t-controle.esp-docto
                t-controle.num-docto
                t-controle.ent-comp   
                t-controle.sai-comp   
                t-controle.ent-real   
                t-controle.sai-real 
                t-controle.acum-comp  
                t-controle.acum-real
                with frame f-controle.
                down with frame f-controle.


            /** CHAMADA UPC **/   
    
            for each tt-epc:
                delete tt-epc.
            end.    
             
            create tt-epc.
            assign tt-epc.cod-event     = "apos-display-frame-controle":U
                   tt-epc.cod-parameter = "controle-verba-rowid"
                   tt-epc.val-parameter = string(rowid(controle-verba)). 
                  
            {include/i-epc201.i "apos-display-frame-controle"}        
          
            if return-value = 'NOK' then do:
                undo,leave.
            end.
        
            /********************/    

      end.   
      if l-imprimiu = yes then do:
         if line-counter + 4 > page-size then do:
            page.
            {inp/spp/esin0518.i4}
         end.
         if de-acum-comp  < 0 then assign de-acum-comp = 0.
         if de-acum-real < 0 then assign de-acum-real = 0.
         assign de-acum-tot = de-acum-comp + de-acum-real.
         disp c-lb-total
              de-acum-tot
              with frame f-total. 
      end.        
      page. 
end.
 
        end.
        else do:
           /*******************************************************************************
* ESIN0518.I - ImpressÆo
********************************************************************************/
run pi-inicializar in h-acomp(input c-imprimindo).
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
            
     
      run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                         string(controle-verba.num-projeto) + "  " + 
                                         c-lb-ordem + 
                                         string(controle-verba.num-ordem))).     
      if first-of (controle-verba.ep-codigo) or
         not avail empresa then do:
         find empresa
              where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
         find param-inv 
              where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
         run inp/in9997.p (input 1).
         assign c-moedas-inv [1] = Trim(return-value).
         run inp/in9997.p (input 2).
         assign c-moedas-inv [2] = Trim(return-value).
         run inp/in9997.p (input 3).
         assign c-moedas-inv [3] = Trim(return-value). 
      end.
      if first-of (controle-verba.cod-est-exec) or
         not avail estabelec then    
         find estabelec
              where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
      if first-of (controle-verba.num-projeto) or 
         not avail proj-inv then         
         find proj-inv
             where proj-inv.ep-codigo = controle-verba.ep-codigo
               and proj-inv.cod-est-exec = controle-verba.cod-est-exec
               and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
      if first-of (controle-verba.num-ordem) or
         not avail ordem-inv then do:
         find ordem-inv
              where ordem-inv.ep-codigo = controle-verba.ep-codigo
                and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                and ordem-inv.num-projeto = controle-verba.num-projeto
                and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
         
         &if '{&bf_mat_versao_ems}' >= '2.062' &then
         ASSIGN i-cnt-unid-negoc = 1.
          DO  WHILE i-cnt-unid-negoc < 34:
              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
          END.
          
          FOR EACH mat-rat-med-inv NO-LOCK
              WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                AND mat-rat-med-inv.numero-ordem = 0
                AND mat-rat-med-inv.nr-contrato  = 0:
              IF  c-txt-unid-negoc[1] = "" THEN DO:
                  {utp/ut-liter.i Matriz_Unidade_de_Neg¢cio}
                  ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                         i-cnt-unid-negoc = 1.
              END.
              
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
              IF  i-cnt-unid-negoc < 34 THEN DO:
                 {utp/ut-liter.i Matriz_Unidade_de_Neg¢cio}
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                 &if '{&bf_mat_versao_ems}' = '2.062' &then
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                 &else
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                 &endif
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
              END.
          END.
         &endif
      end.
     
      if controle-verba.situacao = 1 then 
         assign c-situacao = c-situacao-1.
      else 
         assign c-situacao = c-situacao-2.      

      assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
 
      assign de-acum-comp = 0
             de-acum-real = 0.

      if tt-param.atualiza then do:
          for each t-controle
             where t-controle.ep-codigo    = controle-verba.ep-codigo
               and t-controle.cod-est-exec = controle-verba.cod-est-exec
               and t-controle.num-projeto  = controle-verba.num-projeto
               and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
               assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                      de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
          end.

          if de-acum-comp  < 0 then assign de-acum-comp = 0.
          if de-acum-real < 0 then assign de-acum-real = 0.

          find first b-controle-verba exclusive-lock
              where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
          if avail b-controle-verba then
            assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                   b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
      end.

      {inp/spp/esin0518.i4}
           
      assign de-acum-comp = 0
             de-acum-real = 0
             l-imprimiu   = no.
                   
      for each t-controle
         where t-controle.ep-codigo    = controle-verba.ep-codigo
           and t-controle.cod-est-exec = controle-verba.cod-est-exec
           and t-controle.num-projeto  = controle-verba.num-projeto
           and t-controle.num-ordem    = controle-verba.num-ordem
           no-lock break by t-controle.tipo-doc by t-controle.seq by t-controle.solicitacao by t-controle.num-ord-comp by t-controle.seq-comp by t-controle.num-pedido by t-controle.esp-docto by t-controle.num-docto:
           assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                  de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                  t-controle.acum-comp = de-acum-comp
                  t-controle.acum-real = de-acum-real
                  l-imprimiu           = yes.
           if line-counter < 4 then do:
              {inp/spp/esin0518.i4}
           end.
       
           IF t-controle.dt-trans > d-data-fim OR
              t-controle.dt-trans < d-data-ini THEN NEXT.
             
           disp t-controle.dt-trans   
                t-controle.it-codigo
                t-controle.num-ord-magnus
                t-controle.tipo-doc
                t-controle.solicitacao      
                t-controle.num-ord-comp
                t-controle.num-pedido
                t-controle.esp-docto
                t-controle.num-docto
                t-controle.ent-comp   
                t-controle.sai-comp   
                t-controle.ent-real   
                t-controle.sai-real 
                t-controle.acum-comp  
                t-controle.acum-real
                with frame f-controle.
                down with frame f-controle.


            /** CHAMADA UPC **/   
    
            for each tt-epc:
                delete tt-epc.
            end.    
             
            create tt-epc.
            assign tt-epc.cod-event     = "apos-display-frame-controle":U
                   tt-epc.cod-parameter = "controle-verba-rowid"
                   tt-epc.val-parameter = string(rowid(controle-verba)). 
                  
            {include/i-epc201.i "apos-display-frame-controle"}        
          
            if return-value = 'NOK' then do:
                undo,leave.
            end.
        
            /********************/    

      end.   
      if l-imprimiu = yes then do:
         if line-counter + 4 > page-size then do:
            page.
            {inp/spp/esin0518.i4}
         end.
         if de-acum-comp  < 0 then assign de-acum-comp = 0.
         if de-acum-real < 0 then assign de-acum-real = 0.
         assign de-acum-tot = de-acum-comp + de-acum-real.
         disp c-lb-total
              de-acum-tot
              with frame f-total. 
      end.        
      page. 
end.
 
        
        end.
    end.
    else do:
        if tt-param.classif-1 = 1 THEN DO:
           /*******************************************************************************
* ESIN0518.I - ImpressÆo
********************************************************************************/

OUTPUT STREAM s-planilha TO VALUE(tt-param.caminho-excel) CONVERT TARGET 'iso8859-1'.

run pi-inicializar in h-acomp(input c-imprimindo).
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
            
     
      run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                         string(controle-verba.num-projeto) + "  " + 
                                         c-lb-ordem + 
                                         string(controle-verba.num-ordem))).     
      if first-of (controle-verba.ep-codigo) or
         not avail empresa then do:
         find empresa
              where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
         find param-inv 
              where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
         run inp/in9997.p (input 1).
         assign c-moedas-inv [1] = Trim(return-value).
         run inp/in9997.p (input 2).
         assign c-moedas-inv [2] = Trim(return-value).
         run inp/in9997.p (input 3).
         assign c-moedas-inv [3] = Trim(return-value). 
      end.
      if first-of (controle-verba.cod-est-exec) or
         not avail estabelec then    
         find estabelec
              where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
      if first-of (controle-verba.num-projeto) or 
         not avail proj-inv then         
         find proj-inv
             where proj-inv.ep-codigo = controle-verba.ep-codigo
               and proj-inv.cod-est-exec = controle-verba.cod-est-exec
               and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
      if first-of (controle-verba.num-ordem) or
         not avail ordem-inv then do:
         find ordem-inv
              where ordem-inv.ep-codigo = controle-verba.ep-codigo
                and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                and ordem-inv.num-projeto = controle-verba.num-projeto
                and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
         
         &if '{&bf_mat_versao_ems}' >= '2.062' &then
         ASSIGN i-cnt-unid-negoc = 1.
          DO  WHILE i-cnt-unid-negoc < 34:
              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
          END.
          
          FOR EACH mat-rat-med-inv NO-LOCK
              WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                AND mat-rat-med-inv.numero-ordem = 0
                AND mat-rat-med-inv.nr-contrato  = 0:
              IF  c-txt-unid-negoc[1] = "" THEN DO:
                  {utp/ut-liter.i Matriz_Unidade_de_Neg¢cio}
                  ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                         i-cnt-unid-negoc = 1.
              END.
              
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
              IF  i-cnt-unid-negoc < 34 THEN DO:
                 {utp/ut-liter.i Matriz_Unidade_de_Neg¢cio}
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                 &if '{&bf_mat_versao_ems}' = '2.062' &then
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                 &else
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                 &endif
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
              END.
          END.
         &endif
      end.
     
      if controle-verba.situacao = 1 then 
         assign c-situacao = c-situacao-1.
      else 
         assign c-situacao = c-situacao-2.      

      assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
 
      assign de-acum-comp = 0
             de-acum-real = 0.

      if tt-param.atualiza then do:
          for each t-controle
             where t-controle.ep-codigo    = controle-verba.ep-codigo
               and t-controle.cod-est-exec = controle-verba.cod-est-exec
               and t-controle.num-projeto  = controle-verba.num-projeto
               and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
               assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                      de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
          end.

          if de-acum-comp  < 0 then assign de-acum-comp = 0.
          if de-acum-real < 0 then assign de-acum-real = 0.

          find first b-controle-verba exclusive-lock
              where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
          if avail b-controle-verba then
            assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                   b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
      end.

      for each tt-impressao:
          delete tt-impressao.
      end.
      /*******************************************************************************
* ESIN0518-EXCEL.I4 - ImpressÆo do cabe‡alho no EXCEL
********************************************************************************/
assign de-total = controle-verba.vl-comp[i-moeda-par] +
                  controle-verba.vl-real[i-moeda-par].


    if l-unidade-negocio and l-mat-unid-negoc then do:
        create tt-impressao.
        ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                    + tt-param.delimitador + string(controle-verba.ep-codigo)
                                    + tt-param.delimitador + if avail empresa then empresa.nome else ""
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[01]
               tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                    + tt-param.delimitador + controle-verba.cod-est-exec
                                    + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[02]
                                    + tt-param.delimitador + c-txt-unid-negoc[10]
                                    + tt-param.delimitador + c-txt-unid-negoc[18]
                                    + tt-param.delimitador + c-txt-unid-negoc[26]
               tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                    + tt-param.delimitador + string(controle-verba.num-projeto)
                                    + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[03]
                                    + tt-param.delimitador + c-txt-unid-negoc[11]
                                    + tt-param.delimitador + c-txt-unid-negoc[19]
                                    + tt-param.delimitador + c-txt-unid-negoc[27]
               tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                    + tt-param.delimitador + string(controle-verba.num-ordem)
                                    + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador 
                                    + tt-param.delimitador + c-txt-unid-negoc[04]
                                    + tt-param.delimitador + c-txt-unid-negoc[12]
                                    + tt-param.delimitador + c-txt-unid-negoc[20]
                                    + tt-param.delimitador + c-txt-unid-negoc[28]
               tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                    + tt-param.delimitador + c-situacao
                                    + tt-param.delimitador
                                    + tt-param.delimitador + c-lbl-moeda
                                    + tt-param.delimitador + c-moeda-controle
                                    + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[05]
                                    + tt-param.delimitador + c-txt-unid-negoc[13]
                                    + tt-param.delimitador + c-txt-unid-negoc[21]
                                    + tt-param.delimitador + c-txt-unid-negoc[29]
               tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                    + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                    + tt-param.delimitador + c-lbl-vl-verba-orig
                                    + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
                                    + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[06]
                                    + tt-param.delimitador + c-txt-unid-negoc[14]
                                    + tt-param.delimitador + c-txt-unid-negoc[22]
                                    + tt-param.delimitador + c-txt-unid-negoc[30]
               tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                    + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[07]
                                    + tt-param.delimitador + c-txt-unid-negoc[15]
                                    + tt-param.delimitador + c-txt-unid-negoc[23]
                                    + tt-param.delimitador + c-txt-unid-negoc[31]
               tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                    + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[08]
                                    + tt-param.delimitador + c-txt-unid-negoc[16]
                                    + tt-param.delimitador + c-txt-unid-negoc[24]
                                    + tt-param.delimitador + c-txt-unid-negoc[32]
               tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                    + tt-param.delimitador + string(de-total)
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[09]
                                    + tt-param.delimitador + c-txt-unid-negoc[17]
                                    + tt-param.delimitador + c-txt-unid-negoc[25]
                                    + tt-param.delimitador + c-txt-unid-negoc[33]
               tt-impressao.colunas = c-lb-dt-trans
                                    + tt-param.delimitador + c-lb-item
                                    + tt-param.delimitador + c-lb-num-ord-magnus
                                    + tt-param.delimitador + c-lb-tipo-doc
                                    + tt-param.delimitador + c-lb-solicitacao
                                    + tt-param.delimitador + c-lb-ordem
                                    + tt-param.delimitador + c-lb-pedido
                                    + tt-param.delimitador + c-lb-especie
                                    + tt-param.delimitador + c-lb-docto
                                    + tt-param.delimitador + c-lb-ent-comp
                                    + tt-param.delimitador + c-lb-sai-comp
                                    + tt-param.delimitador + c-lb-ent-real
                                    + tt-param.delimitador + c-lb-sai-real
                                    + tt-param.delimitador + c-lb-acum-comp
                                    + tt-param.delimitador + c-lb-acum-real.
    end.
    else do:
        create tt-impressao.
        ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                    + tt-param.delimitador + string(controle-verba.ep-codigo)
                                    + tt-param.delimitador + if avail empresa then empresa.nome else ""
               tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                    + tt-param.delimitador + controle-verba.cod-est-exec
                                    + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
               tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                    + tt-param.delimitador + string(controle-verba.num-projeto)
                                    + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
               tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                    + tt-param.delimitador + string(controle-verba.num-ordem)
                                    + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
               tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                    + tt-param.delimitador + c-situacao
                                    + tt-param.delimitador 
                                    + tt-param.delimitador + c-lbl-moeda
                                    + tt-param.delimitador + c-moeda-controle
               tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                    + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                    + tt-param.delimitador 
                                    + tt-param.delimitador + c-lbl-vl-verba-orig
                                    + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
               tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                    + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
               tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                    + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
               tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                    + tt-param.delimitador + string(de-total)
               tt-impressao.colunas = c-lb-dt-trans
                                    + tt-param.delimitador + c-lb-item
                                    + tt-param.delimitador + c-lb-num-ord-magnus
                                    + tt-param.delimitador + c-lb-tipo-doc
                                    + tt-param.delimitador + c-lb-solicitacao
                                    + tt-param.delimitador + c-lb-ordem
                                    + tt-param.delimitador + c-lb-pedido
                                    + tt-param.delimitador + c-lb-especie
                                    + tt-param.delimitador + c-lb-docto
                                    + tt-param.delimitador + c-lb-ent-comp
                                    + tt-param.delimitador + c-lb-sai-comp
                                    + tt-param.delimitador + c-lb-ent-real
                                    + tt-param.delimitador + c-lb-sai-real
                                    + tt-param.delimitador + c-lb-acum-comp
                                    + tt-param.delimitador + c-lb-acum-real.
    end.

 
      find first tt-impressao no-lock no-error.
      if avail tt-impressao then
          PUT STREAM s-planilha UNFORMATTED
               tt-impressao.linha-1 skip
               tt-impressao.linha-2 skip
               tt-impressao.linha-3 skip
               tt-impressao.linha-4 skip
               tt-impressao.linha-5 skip
               tt-impressao.linha-6 skip
               tt-impressao.linha-7 skip
               tt-impressao.linha-8 skip
               tt-impressao.linha-9 skip(2)
               tt-impressao.colunas SKIP.
           
      assign de-acum-comp = 0
             de-acum-real = 0
             l-imprimiu   = no.
                   
      for each t-controle
         where t-controle.ep-codigo    = controle-verba.ep-codigo
           and t-controle.cod-est-exec = controle-verba.cod-est-exec
           and t-controle.num-projeto  = controle-verba.num-projeto
           and t-controle.num-ordem    = controle-verba.num-ordem
           no-lock break by t-controle.it-codigo by t-controle.seq by t-controle.solicitacao by t-controle.num-ord-comp by t-controle.seq-comp by t-controle.num-pedido by t-controle.esp-docto by t-controle.num-docto:
           assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                  de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                  t-controle.acum-comp = de-acum-comp
                  t-controle.acum-real = de-acum-real
                  l-imprimiu           = yes.

           PUT STREAM s-planilha UNFORMATTED 
               t-controle.dt-trans       tt-param.delimitador
               t-controle.it-codigo      tt-param.delimitador
               t-controle.num-ord-magnus tt-param.delimitador
               t-controle.tipo-doc       tt-param.delimitador
               t-controle.solicitacao    tt-param.delimitador  
               t-controle.num-ord-comp   tt-param.delimitador
               t-controle.num-pedido     tt-param.delimitador
               t-controle.esp-docto      tt-param.delimitador
               t-controle.num-docto      tt-param.delimitador
               t-controle.ent-comp       tt-param.delimitador
               t-controle.sai-comp       tt-param.delimitador
               t-controle.ent-real       tt-param.delimitador
               t-controle.sai-real       tt-param.delimitador
               t-controle.acum-comp      tt-param.delimitador
               t-controle.acum-real      tt-param.delimitador 
               SKIP.
            /** CHAMADA UPC **/   
    
            for each tt-epc:
                delete tt-epc.
            end.    
             
            create tt-epc.
            assign tt-epc.cod-event     = "apos-display-frame-controle":U
                   tt-epc.cod-parameter = "controle-verba-rowid"
                   tt-epc.val-parameter = string(rowid(controle-verba)). 
                  
            {include/i-epc201.i "apos-display-frame-controle"}        
          
            if return-value = 'NOK' then do:
                undo,leave.
            end.
        
            /********************/    

      end.   
      if l-imprimiu = yes then do:
         if de-acum-comp  < 0 then assign de-acum-comp = 0.
         if de-acum-real < 0 then assign de-acum-real = 0.
         assign de-acum-tot = de-acum-comp + de-acum-real.
         PUT STREAM s-planilha UNFORMATTED 
              tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador 
              tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador 
              tt-param.delimitador tt-param.delimitador tt-param.delimitador c-lb-total 
              tt-param.delimitador de-acum-tot SKIP(2).
      end.        
      PUT STREAM s-planilha UNFORMATTED SKIP(10).
end.

OUTPUT STREAM s-planilha CLOSE.
 
        END.
        else if tt-param.classif-1 = 2 THEN DO:
           /*******************************************************************************
* ESIN0518.I - ImpressÆo
********************************************************************************/

OUTPUT STREAM s-planilha TO VALUE(tt-param.caminho-excel) CONVERT TARGET 'iso8859-1'.

run pi-inicializar in h-acomp(input c-imprimindo).
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
            
     
      run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                         string(controle-verba.num-projeto) + "  " + 
                                         c-lb-ordem + 
                                         string(controle-verba.num-ordem))).     
      if first-of (controle-verba.ep-codigo) or
         not avail empresa then do:
         find empresa
              where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
         find param-inv 
              where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
         run inp/in9997.p (input 1).
         assign c-moedas-inv [1] = Trim(return-value).
         run inp/in9997.p (input 2).
         assign c-moedas-inv [2] = Trim(return-value).
         run inp/in9997.p (input 3).
         assign c-moedas-inv [3] = Trim(return-value). 
      end.
      if first-of (controle-verba.cod-est-exec) or
         not avail estabelec then    
         find estabelec
              where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
      if first-of (controle-verba.num-projeto) or 
         not avail proj-inv then         
         find proj-inv
             where proj-inv.ep-codigo = controle-verba.ep-codigo
               and proj-inv.cod-est-exec = controle-verba.cod-est-exec
               and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
      if first-of (controle-verba.num-ordem) or
         not avail ordem-inv then do:
         find ordem-inv
              where ordem-inv.ep-codigo = controle-verba.ep-codigo
                and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                and ordem-inv.num-projeto = controle-verba.num-projeto
                and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
         
         &if '{&bf_mat_versao_ems}' >= '2.062' &then
         ASSIGN i-cnt-unid-negoc = 1.
          DO  WHILE i-cnt-unid-negoc < 34:
              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
          END.
          
          FOR EACH mat-rat-med-inv NO-LOCK
              WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                AND mat-rat-med-inv.numero-ordem = 0
                AND mat-rat-med-inv.nr-contrato  = 0:
              IF  c-txt-unid-negoc[1] = "" THEN DO:
                  {utp/ut-liter.i Matriz_Unidade_de_Neg¢cio}
                  ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                         i-cnt-unid-negoc = 1.
              END.
              
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
              IF  i-cnt-unid-negoc < 34 THEN DO:
                 {utp/ut-liter.i Matriz_Unidade_de_Neg¢cio}
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                 &if '{&bf_mat_versao_ems}' = '2.062' &then
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                 &else
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                 &endif
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
              END.
          END.
         &endif
      end.
     
      if controle-verba.situacao = 1 then 
         assign c-situacao = c-situacao-1.
      else 
         assign c-situacao = c-situacao-2.      

      assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
 
      assign de-acum-comp = 0
             de-acum-real = 0.

      if tt-param.atualiza then do:
          for each t-controle
             where t-controle.ep-codigo    = controle-verba.ep-codigo
               and t-controle.cod-est-exec = controle-verba.cod-est-exec
               and t-controle.num-projeto  = controle-verba.num-projeto
               and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
               assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                      de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
          end.

          if de-acum-comp  < 0 then assign de-acum-comp = 0.
          if de-acum-real < 0 then assign de-acum-real = 0.

          find first b-controle-verba exclusive-lock
              where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
          if avail b-controle-verba then
            assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                   b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
      end.

      for each tt-impressao:
          delete tt-impressao.
      end.
      /*******************************************************************************
* ESIN0518-EXCEL.I4 - ImpressÆo do cabe‡alho no EXCEL
********************************************************************************/
assign de-total = controle-verba.vl-comp[i-moeda-par] +
                  controle-verba.vl-real[i-moeda-par].


    if l-unidade-negocio and l-mat-unid-negoc then do:
        create tt-impressao.
        ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                    + tt-param.delimitador + string(controle-verba.ep-codigo)
                                    + tt-param.delimitador + if avail empresa then empresa.nome else ""
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[01]
               tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                    + tt-param.delimitador + controle-verba.cod-est-exec
                                    + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[02]
                                    + tt-param.delimitador + c-txt-unid-negoc[10]
                                    + tt-param.delimitador + c-txt-unid-negoc[18]
                                    + tt-param.delimitador + c-txt-unid-negoc[26]
               tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                    + tt-param.delimitador + string(controle-verba.num-projeto)
                                    + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[03]
                                    + tt-param.delimitador + c-txt-unid-negoc[11]
                                    + tt-param.delimitador + c-txt-unid-negoc[19]
                                    + tt-param.delimitador + c-txt-unid-negoc[27]
               tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                    + tt-param.delimitador + string(controle-verba.num-ordem)
                                    + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador 
                                    + tt-param.delimitador + c-txt-unid-negoc[04]
                                    + tt-param.delimitador + c-txt-unid-negoc[12]
                                    + tt-param.delimitador + c-txt-unid-negoc[20]
                                    + tt-param.delimitador + c-txt-unid-negoc[28]
               tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                    + tt-param.delimitador + c-situacao
                                    + tt-param.delimitador
                                    + tt-param.delimitador + c-lbl-moeda
                                    + tt-param.delimitador + c-moeda-controle
                                    + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[05]
                                    + tt-param.delimitador + c-txt-unid-negoc[13]
                                    + tt-param.delimitador + c-txt-unid-negoc[21]
                                    + tt-param.delimitador + c-txt-unid-negoc[29]
               tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                    + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                    + tt-param.delimitador + c-lbl-vl-verba-orig
                                    + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
                                    + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[06]
                                    + tt-param.delimitador + c-txt-unid-negoc[14]
                                    + tt-param.delimitador + c-txt-unid-negoc[22]
                                    + tt-param.delimitador + c-txt-unid-negoc[30]
               tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                    + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[07]
                                    + tt-param.delimitador + c-txt-unid-negoc[15]
                                    + tt-param.delimitador + c-txt-unid-negoc[23]
                                    + tt-param.delimitador + c-txt-unid-negoc[31]
               tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                    + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[08]
                                    + tt-param.delimitador + c-txt-unid-negoc[16]
                                    + tt-param.delimitador + c-txt-unid-negoc[24]
                                    + tt-param.delimitador + c-txt-unid-negoc[32]
               tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                    + tt-param.delimitador + string(de-total)
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[09]
                                    + tt-param.delimitador + c-txt-unid-negoc[17]
                                    + tt-param.delimitador + c-txt-unid-negoc[25]
                                    + tt-param.delimitador + c-txt-unid-negoc[33]
               tt-impressao.colunas = c-lb-dt-trans
                                    + tt-param.delimitador + c-lb-item
                                    + tt-param.delimitador + c-lb-num-ord-magnus
                                    + tt-param.delimitador + c-lb-tipo-doc
                                    + tt-param.delimitador + c-lb-solicitacao
                                    + tt-param.delimitador + c-lb-ordem
                                    + tt-param.delimitador + c-lb-pedido
                                    + tt-param.delimitador + c-lb-especie
                                    + tt-param.delimitador + c-lb-docto
                                    + tt-param.delimitador + c-lb-ent-comp
                                    + tt-param.delimitador + c-lb-sai-comp
                                    + tt-param.delimitador + c-lb-ent-real
                                    + tt-param.delimitador + c-lb-sai-real
                                    + tt-param.delimitador + c-lb-acum-comp
                                    + tt-param.delimitador + c-lb-acum-real.
    end.
    else do:
        create tt-impressao.
        ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                    + tt-param.delimitador + string(controle-verba.ep-codigo)
                                    + tt-param.delimitador + if avail empresa then empresa.nome else ""
               tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                    + tt-param.delimitador + controle-verba.cod-est-exec
                                    + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
               tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                    + tt-param.delimitador + string(controle-verba.num-projeto)
                                    + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
               tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                    + tt-param.delimitador + string(controle-verba.num-ordem)
                                    + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
               tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                    + tt-param.delimitador + c-situacao
                                    + tt-param.delimitador 
                                    + tt-param.delimitador + c-lbl-moeda
                                    + tt-param.delimitador + c-moeda-controle
               tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                    + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                    + tt-param.delimitador 
                                    + tt-param.delimitador + c-lbl-vl-verba-orig
                                    + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
               tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                    + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
               tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                    + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
               tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                    + tt-param.delimitador + string(de-total)
               tt-impressao.colunas = c-lb-dt-trans
                                    + tt-param.delimitador + c-lb-item
                                    + tt-param.delimitador + c-lb-num-ord-magnus
                                    + tt-param.delimitador + c-lb-tipo-doc
                                    + tt-param.delimitador + c-lb-solicitacao
                                    + tt-param.delimitador + c-lb-ordem
                                    + tt-param.delimitador + c-lb-pedido
                                    + tt-param.delimitador + c-lb-especie
                                    + tt-param.delimitador + c-lb-docto
                                    + tt-param.delimitador + c-lb-ent-comp
                                    + tt-param.delimitador + c-lb-sai-comp
                                    + tt-param.delimitador + c-lb-ent-real
                                    + tt-param.delimitador + c-lb-sai-real
                                    + tt-param.delimitador + c-lb-acum-comp
                                    + tt-param.delimitador + c-lb-acum-real.
    end.

 
      find first tt-impressao no-lock no-error.
      if avail tt-impressao then
          PUT STREAM s-planilha UNFORMATTED
               tt-impressao.linha-1 skip
               tt-impressao.linha-2 skip
               tt-impressao.linha-3 skip
               tt-impressao.linha-4 skip
               tt-impressao.linha-5 skip
               tt-impressao.linha-6 skip
               tt-impressao.linha-7 skip
               tt-impressao.linha-8 skip
               tt-impressao.linha-9 skip(2)
               tt-impressao.colunas SKIP.
           
      assign de-acum-comp = 0
             de-acum-real = 0
             l-imprimiu   = no.
                   
      for each t-controle
         where t-controle.ep-codigo    = controle-verba.ep-codigo
           and t-controle.cod-est-exec = controle-verba.cod-est-exec
           and t-controle.num-projeto  = controle-verba.num-projeto
           and t-controle.num-ordem    = controle-verba.num-ordem
           no-lock break by t-controle.dt-trans by t-controle.seq by t-controle.solicitacao by t-controle.num-ord-comp by t-controle.seq-comp by t-controle.num-pedido by t-controle.esp-docto by t-controle.num-docto:
           assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                  de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                  t-controle.acum-comp = de-acum-comp
                  t-controle.acum-real = de-acum-real
                  l-imprimiu           = yes.

           PUT STREAM s-planilha UNFORMATTED 
               t-controle.dt-trans       tt-param.delimitador
               t-controle.it-codigo      tt-param.delimitador
               t-controle.num-ord-magnus tt-param.delimitador
               t-controle.tipo-doc       tt-param.delimitador
               t-controle.solicitacao    tt-param.delimitador  
               t-controle.num-ord-comp   tt-param.delimitador
               t-controle.num-pedido     tt-param.delimitador
               t-controle.esp-docto      tt-param.delimitador
               t-controle.num-docto      tt-param.delimitador
               t-controle.ent-comp       tt-param.delimitador
               t-controle.sai-comp       tt-param.delimitador
               t-controle.ent-real       tt-param.delimitador
               t-controle.sai-real       tt-param.delimitador
               t-controle.acum-comp      tt-param.delimitador
               t-controle.acum-real      tt-param.delimitador 
               SKIP.
            /** CHAMADA UPC **/   
    
            for each tt-epc:
                delete tt-epc.
            end.    
             
            create tt-epc.
            assign tt-epc.cod-event     = "apos-display-frame-controle":U
                   tt-epc.cod-parameter = "controle-verba-rowid"
                   tt-epc.val-parameter = string(rowid(controle-verba)). 
                  
            {include/i-epc201.i "apos-display-frame-controle"}        
          
            if return-value = 'NOK' then do:
                undo,leave.
            end.
        
            /********************/    

      end.   
      if l-imprimiu = yes then do:
         if de-acum-comp  < 0 then assign de-acum-comp = 0.
         if de-acum-real < 0 then assign de-acum-real = 0.
         assign de-acum-tot = de-acum-comp + de-acum-real.
         PUT STREAM s-planilha UNFORMATTED 
              tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador 
              tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador 
              tt-param.delimitador tt-param.delimitador tt-param.delimitador c-lb-total 
              tt-param.delimitador de-acum-tot SKIP(2).
      end.        
      PUT STREAM s-planilha UNFORMATTED SKIP(10).
end.

OUTPUT STREAM s-planilha CLOSE.
 
        end.
        else do:
           /*******************************************************************************
* ESIN0518.I - ImpressÆo
********************************************************************************/

OUTPUT STREAM s-planilha TO VALUE(tt-param.caminho-excel) CONVERT TARGET 'iso8859-1'.

run pi-inicializar in h-acomp(input c-imprimindo).
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
            
     
      run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                         string(controle-verba.num-projeto) + "  " + 
                                         c-lb-ordem + 
                                         string(controle-verba.num-ordem))).     
      if first-of (controle-verba.ep-codigo) or
         not avail empresa then do:
         find empresa
              where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
         find param-inv 
              where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
         run inp/in9997.p (input 1).
         assign c-moedas-inv [1] = Trim(return-value).
         run inp/in9997.p (input 2).
         assign c-moedas-inv [2] = Trim(return-value).
         run inp/in9997.p (input 3).
         assign c-moedas-inv [3] = Trim(return-value). 
      end.
      if first-of (controle-verba.cod-est-exec) or
         not avail estabelec then    
         find estabelec
              where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
      if first-of (controle-verba.num-projeto) or 
         not avail proj-inv then         
         find proj-inv
             where proj-inv.ep-codigo = controle-verba.ep-codigo
               and proj-inv.cod-est-exec = controle-verba.cod-est-exec
               and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
      if first-of (controle-verba.num-ordem) or
         not avail ordem-inv then do:
         find ordem-inv
              where ordem-inv.ep-codigo = controle-verba.ep-codigo
                and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                and ordem-inv.num-projeto = controle-verba.num-projeto
                and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
         
         &if '{&bf_mat_versao_ems}' >= '2.062' &then
         ASSIGN i-cnt-unid-negoc = 1.
          DO  WHILE i-cnt-unid-negoc < 34:
              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
          END.
          
          FOR EACH mat-rat-med-inv NO-LOCK
              WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                AND mat-rat-med-inv.numero-ordem = 0
                AND mat-rat-med-inv.nr-contrato  = 0:
              IF  c-txt-unid-negoc[1] = "" THEN DO:
                  {utp/ut-liter.i Matriz_Unidade_de_Neg¢cio}
                  ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                         i-cnt-unid-negoc = 1.
              END.
              
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
              IF  i-cnt-unid-negoc < 34 THEN DO:
                 {utp/ut-liter.i Matriz_Unidade_de_Neg¢cio}
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                 &if '{&bf_mat_versao_ems}' = '2.062' &then
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                 &else
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                 &endif
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
              END.
          END.
         &endif
      end.
     
      if controle-verba.situacao = 1 then 
         assign c-situacao = c-situacao-1.
      else 
         assign c-situacao = c-situacao-2.      

      assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
 
      assign de-acum-comp = 0
             de-acum-real = 0.

      if tt-param.atualiza then do:
          for each t-controle
             where t-controle.ep-codigo    = controle-verba.ep-codigo
               and t-controle.cod-est-exec = controle-verba.cod-est-exec
               and t-controle.num-projeto  = controle-verba.num-projeto
               and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
               assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                      de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
          end.

          if de-acum-comp  < 0 then assign de-acum-comp = 0.
          if de-acum-real < 0 then assign de-acum-real = 0.

          find first b-controle-verba exclusive-lock
              where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
          if avail b-controle-verba then
            assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                   b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
      end.

      for each tt-impressao:
          delete tt-impressao.
      end.
      /*******************************************************************************
* ESIN0518-EXCEL.I4 - ImpressÆo do cabe‡alho no EXCEL
********************************************************************************/
assign de-total = controle-verba.vl-comp[i-moeda-par] +
                  controle-verba.vl-real[i-moeda-par].


    if l-unidade-negocio and l-mat-unid-negoc then do:
        create tt-impressao.
        ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                    + tt-param.delimitador + string(controle-verba.ep-codigo)
                                    + tt-param.delimitador + if avail empresa then empresa.nome else ""
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[01]
               tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                    + tt-param.delimitador + controle-verba.cod-est-exec
                                    + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[02]
                                    + tt-param.delimitador + c-txt-unid-negoc[10]
                                    + tt-param.delimitador + c-txt-unid-negoc[18]
                                    + tt-param.delimitador + c-txt-unid-negoc[26]
               tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                    + tt-param.delimitador + string(controle-verba.num-projeto)
                                    + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[03]
                                    + tt-param.delimitador + c-txt-unid-negoc[11]
                                    + tt-param.delimitador + c-txt-unid-negoc[19]
                                    + tt-param.delimitador + c-txt-unid-negoc[27]
               tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                    + tt-param.delimitador + string(controle-verba.num-ordem)
                                    + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador 
                                    + tt-param.delimitador + c-txt-unid-negoc[04]
                                    + tt-param.delimitador + c-txt-unid-negoc[12]
                                    + tt-param.delimitador + c-txt-unid-negoc[20]
                                    + tt-param.delimitador + c-txt-unid-negoc[28]
               tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                    + tt-param.delimitador + c-situacao
                                    + tt-param.delimitador
                                    + tt-param.delimitador + c-lbl-moeda
                                    + tt-param.delimitador + c-moeda-controle
                                    + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[05]
                                    + tt-param.delimitador + c-txt-unid-negoc[13]
                                    + tt-param.delimitador + c-txt-unid-negoc[21]
                                    + tt-param.delimitador + c-txt-unid-negoc[29]
               tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                    + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                    + tt-param.delimitador + c-lbl-vl-verba-orig
                                    + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
                                    + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[06]
                                    + tt-param.delimitador + c-txt-unid-negoc[14]
                                    + tt-param.delimitador + c-txt-unid-negoc[22]
                                    + tt-param.delimitador + c-txt-unid-negoc[30]
               tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                    + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[07]
                                    + tt-param.delimitador + c-txt-unid-negoc[15]
                                    + tt-param.delimitador + c-txt-unid-negoc[23]
                                    + tt-param.delimitador + c-txt-unid-negoc[31]
               tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                    + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[08]
                                    + tt-param.delimitador + c-txt-unid-negoc[16]
                                    + tt-param.delimitador + c-txt-unid-negoc[24]
                                    + tt-param.delimitador + c-txt-unid-negoc[32]
               tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                    + tt-param.delimitador + string(de-total)
                                    + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                    + tt-param.delimitador + c-txt-unid-negoc[09]
                                    + tt-param.delimitador + c-txt-unid-negoc[17]
                                    + tt-param.delimitador + c-txt-unid-negoc[25]
                                    + tt-param.delimitador + c-txt-unid-negoc[33]
               tt-impressao.colunas = c-lb-dt-trans
                                    + tt-param.delimitador + c-lb-item
                                    + tt-param.delimitador + c-lb-num-ord-magnus
                                    + tt-param.delimitador + c-lb-tipo-doc
                                    + tt-param.delimitador + c-lb-solicitacao
                                    + tt-param.delimitador + c-lb-ordem
                                    + tt-param.delimitador + c-lb-pedido
                                    + tt-param.delimitador + c-lb-especie
                                    + tt-param.delimitador + c-lb-docto
                                    + tt-param.delimitador + c-lb-ent-comp
                                    + tt-param.delimitador + c-lb-sai-comp
                                    + tt-param.delimitador + c-lb-ent-real
                                    + tt-param.delimitador + c-lb-sai-real
                                    + tt-param.delimitador + c-lb-acum-comp
                                    + tt-param.delimitador + c-lb-acum-real.
    end.
    else do:
        create tt-impressao.
        ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                    + tt-param.delimitador + string(controle-verba.ep-codigo)
                                    + tt-param.delimitador + if avail empresa then empresa.nome else ""
               tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                    + tt-param.delimitador + controle-verba.cod-est-exec
                                    + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
               tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                    + tt-param.delimitador + string(controle-verba.num-projeto)
                                    + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
               tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                    + tt-param.delimitador + string(controle-verba.num-ordem)
                                    + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
               tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                    + tt-param.delimitador + c-situacao
                                    + tt-param.delimitador 
                                    + tt-param.delimitador + c-lbl-moeda
                                    + tt-param.delimitador + c-moeda-controle
               tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                    + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                    + tt-param.delimitador 
                                    + tt-param.delimitador + c-lbl-vl-verba-orig
                                    + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
               tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                    + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
               tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                    + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
               tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                    + tt-param.delimitador + string(de-total)
               tt-impressao.colunas = c-lb-dt-trans
                                    + tt-param.delimitador + c-lb-item
                                    + tt-param.delimitador + c-lb-num-ord-magnus
                                    + tt-param.delimitador + c-lb-tipo-doc
                                    + tt-param.delimitador + c-lb-solicitacao
                                    + tt-param.delimitador + c-lb-ordem
                                    + tt-param.delimitador + c-lb-pedido
                                    + tt-param.delimitador + c-lb-especie
                                    + tt-param.delimitador + c-lb-docto
                                    + tt-param.delimitador + c-lb-ent-comp
                                    + tt-param.delimitador + c-lb-sai-comp
                                    + tt-param.delimitador + c-lb-ent-real
                                    + tt-param.delimitador + c-lb-sai-real
                                    + tt-param.delimitador + c-lb-acum-comp
                                    + tt-param.delimitador + c-lb-acum-real.
    end.

 
      find first tt-impressao no-lock no-error.
      if avail tt-impressao then
          PUT STREAM s-planilha UNFORMATTED
               tt-impressao.linha-1 skip
               tt-impressao.linha-2 skip
               tt-impressao.linha-3 skip
               tt-impressao.linha-4 skip
               tt-impressao.linha-5 skip
               tt-impressao.linha-6 skip
               tt-impressao.linha-7 skip
               tt-impressao.linha-8 skip
               tt-impressao.linha-9 skip(2)
               tt-impressao.colunas SKIP.
           
      assign de-acum-comp = 0
             de-acum-real = 0
             l-imprimiu   = no.
                   
      for each t-controle
         where t-controle.ep-codigo    = controle-verba.ep-codigo
           and t-controle.cod-est-exec = controle-verba.cod-est-exec
           and t-controle.num-projeto  = controle-verba.num-projeto
           and t-controle.num-ordem    = controle-verba.num-ordem
           no-lock break by t-controle.tipo-doc by t-controle.seq by t-controle.solicitacao by t-controle.num-ord-comp by t-controle.seq-comp by t-controle.num-pedido by t-controle.esp-docto by t-controle.num-docto:
           assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                  de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                  t-controle.acum-comp = de-acum-comp
                  t-controle.acum-real = de-acum-real
                  l-imprimiu           = yes.

           PUT STREAM s-planilha UNFORMATTED 
               t-controle.dt-trans       tt-param.delimitador
               t-controle.it-codigo      tt-param.delimitador
               t-controle.num-ord-magnus tt-param.delimitador
               t-controle.tipo-doc       tt-param.delimitador
               t-controle.solicitacao    tt-param.delimitador  
               t-controle.num-ord-comp   tt-param.delimitador
               t-controle.num-pedido     tt-param.delimitador
               t-controle.esp-docto      tt-param.delimitador
               t-controle.num-docto      tt-param.delimitador
               t-controle.ent-comp       tt-param.delimitador
               t-controle.sai-comp       tt-param.delimitador
               t-controle.ent-real       tt-param.delimitador
               t-controle.sai-real       tt-param.delimitador
               t-controle.acum-comp      tt-param.delimitador
               t-controle.acum-real      tt-param.delimitador 
               SKIP.
            /** CHAMADA UPC **/   
    
            for each tt-epc:
                delete tt-epc.
            end.    
             
            create tt-epc.
            assign tt-epc.cod-event     = "apos-display-frame-controle":U
                   tt-epc.cod-parameter = "controle-verba-rowid"
                   tt-epc.val-parameter = string(rowid(controle-verba)). 
                  
            {include/i-epc201.i "apos-display-frame-controle"}        
          
            if return-value = 'NOK' then do:
                undo,leave.
            end.
        
            /********************/    

      end.   
      if l-imprimiu = yes then do:
         if de-acum-comp  < 0 then assign de-acum-comp = 0.
         if de-acum-real < 0 then assign de-acum-real = 0.
         assign de-acum-tot = de-acum-comp + de-acum-real.
         PUT STREAM s-planilha UNFORMATTED 
              tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador 
              tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador 
              tt-param.delimitador tt-param.delimitador tt-param.delimitador c-lb-total 
              tt-param.delimitador de-acum-tot SKIP(2).
      end.        
      PUT STREAM s-planilha UNFORMATTED SKIP(10).
end.

OUTPUT STREAM s-planilha CLOSE.
 
        
        end.
    end.

    RETURN "OK":U.
END PROCEDURE.
    
PROCEDURE pi-parametros:
    put 
        c-titulo-s           skip(1)
        c-emp-sel            at 5  ": "
        tt-param.i-ep-ini    at 30 "|< >| " at 43 tt-param.i-ep-fim
        c-est-sel            at 5  ": "
        tt-param.c-est-ini   at 30 "|< >| " at 43 tt-param.c-est-fim
        c-proj-sel           at 5  ": "
        tt-param.i-proj-ini  at 30 "|< >| " at 43 tt-param.i-proj-fim
        c-ord-sel         at 5  ": "
        tt-param.i-ord-ini   at 30 "|< >| " at 43 tt-param.i-ord-fim 
        c-data-sel           at 5  ": "
        tt-param.d-data-ini FORMAT "99/99/9999"  at 30 "|< >| " at 43 tt-param.d-data-fim FORMAT "99/99/9999" skip(2)
        
        c-titulo-c           skip(1)
        c-geral              at 5  ": " tt-param.desc-classifica skip(2)
        
        c-titulo-p           skip(1)
        c-lb-exib-moeda      at 5  ": " c-exib-moeda
        c-lb-atualiza        at 5  ": " c-atualiza skip(2)
        
        c-titulo-i           skip(1)
        c-lb-dest            at 5  ": " c-destino + " - " + tt-param.arquivo
        c-lb-usuar           at 5  ": " tt-param.usuario skip(2).    
    RETURN "OK":U.
END.
