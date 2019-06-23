/*********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
********************************************************************************/
{include/i-prgvrs.i ESCE0108A 2.06.00.024}  /*** 010024 ***/
/******************************************************************************
**
**       Programa: CE0108 - Modifica Tipos de Controle de Itens.
**
*******************************************************************************/
{cep/ceapi001.i}    /* Definicao de temp-table do movto-estoq */

{cdp/cd0666.i}      /* Definicao da temp-table de erros */

{cdp/cd1234.i}       /* Decimais - Chile */

{cdp/cdcfgmat.i}

define variable h-ceapi001k as handle.

def buffer b-movto for movto-estoq.
def buffer b-saldo for saldo-estoq.
def buffer b-item for item.
def buffer b-movto-estoq for movto-estoq.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field c-it-codigo-ini  LIKE item.it-codigo 
    field c-it-codigo-fim  LIKE item.it-codigo
    field tipo       as integer 
    field medio-mat as dec decimals 4 extent 3 format ">>>,>>>,>>9.9999"
    field medio-mob as dec decimals 4 extent 3 format ">>>,>>>,>>9.9999"
    field medio-ggf as dec decimals 4 extent 3 format ">>>,>>>,>>9.9999"
    field r-conta      as rowid
    field da-inipa-x   like movto-estoq.dt-trans
    field depos-pad    like movto-estoq.cod-depos
    field serie1       like movto-estoq.serie-docto
    field docto1       like movto-estoq.nro-docto
    field it-codigo    like item.it-codigo
    field parametro    as char format "x(30)"
    FIELD ct-conta     AS CHAR
    FIELD sc-conta     AS CHAR.


define temp-table tt-param1 no-undo
    field tipo       as integer
    field medio-mat as dec decimals 4 extent 3 format ">>>,>>>,>>9.9999"
    field medio-mob as dec decimals 4 extent 3 format ">>>,>>>,>>9.9999"
    field medio-ggf as dec decimals 4 extent 3 format ">>>,>>>,>>9.9999"
    field r-conta      as rowid
    field da-inipa-x   like movto-estoq.dt-trans
    field depos-pad    like movto-estoq.cod-depos
    field serie1       like movto-estoq.serie-docto
    field docto1       like movto-estoq.nro-docto
    field it-codigo    like item.it-codigo
    field parametro    as char format "x(30)".

def var c-seg-usuario    like movto-estoq.usuario.
def var de-aux           like saldo-terc.valor extent 0 no-undo.
def var i-mo-fasb        as integer no-undo.
def var i-mo-cmi         as integer no-undo.
def var c-opcao          as character no-undo.
def var l-tipo           as logical extent 9.
def var l-resp           as l no-undo format "Sim/Nao".
def var l-param          as l no-undo.
def var de-quant-ini   like saldo-estoq.qtidade-ini init 0.
def var de-vl-mat-m    like movto-estoq.valor-mat-m.
def var de-vl-mat-p    like movto-estoq.valor-mat-p.
def var de-vl-mat-o    like movto-estoq.valor-mat-o.
def var de-vl-mob-m    like movto-estoq.valor-mob-m.
def var de-vl-mob-p    like movto-estoq.valor-mob-p.
def var de-vl-mob-o    like movto-estoq.valor-mob-o.
def var de-vl-ggf-m    like movto-estoq.valor-ggf-m.
def var de-vl-ggf-p    like movto-estoq.valor-ggf-p.
def var de-vl-ggf-o    like movto-estoq.valor-ggf-o.
def var da-data          as date.
def var i-cont           as integer.
def var i-y              as int.
def var i-x              as int.
def var i-quantidade     as int.
def var i-ano-corrente   as integer   no-undo.
def var i-per-corrente   as integer   no-undo.
def var c-depos-pad    like item.deposito-pad.
def var de-quant      as dec format "->>>,>>>,>>>,>>9.99" init 0.
def var de-mat-m     like item-estab.sald-ini-mat-m.
def var de-mat-p     like item-estab.sald-ini-mat-p.
def var de-mat-o     like item-estab.sald-ini-mat-o.
def var de-mob-m     like item-estab.sald-ini-mob-m.
def var de-mob-p     like item-estab.sald-ini-mob-p.
def var de-mob-o     like item-estab.sald-ini-mob-o.
def var de-ggf-m     like item-estab.sald-ini-ggf-m.
def var de-ggf-p     like item-estab.sald-ini-ggf-p.
def var de-ggf-o     like item-estab.sald-ini-ggf-o.
def var da-aux        as date                       no-undo.
def var i-moeda       as integer init 0 no-undo.
def var da-maior-dt   as date no-undo.
def var da-iniper-x   as date no-undo.
def var da-fimper-x   as date no-undo.
def var l-erro-x      as logical init no no-undo.
def var r-registro as recid.
def var c-cod-refer   like referencia.cod-refer.
def var unid-monet    as int extent 3 no-undo.
def var da-ult-per      like param-estoq.ult-per-fech no-undo.
def var da-ult-fech-dia like param-estoq.ult-fech-dia no-undo.

def var de-sald-ini-mat-m like item-estab.sald-ini-mat-m no-undo.
def var de-sald-ini-mob-m like item-estab.sald-ini-mob-m no-undo.
def var de-sald-ini-ggf-m like item-estab.sald-ini-ggf-m no-undo.
def var de-sald-ini-mat-o like item-estab.sald-ini-mat-o no-undo.
def var de-sald-ini-mob-o like item-estab.sald-ini-mob-o no-undo.
def var de-sald-ini-ggf-o like item-estab.sald-ini-ggf-o no-undo.
def var de-sald-ini-mat-p like item-estab.sald-ini-mat-p no-undo.
def var de-sald-ini-mob-p like item-estab.sald-ini-mob-p no-undo.
def var de-sald-ini-ggf-p like item-estab.sald-ini-ggf-p no-undo.
def var c-lote            like saldo-estoq.lote          no-undo.
{cdp/cd9999.i3}

&GLOBAL-DEFINE tabela item-uni-estab

def input parameter table for tt-param.

def input parameter table for tt-param1.

find first param-global no-lock no-error.
find first param-estoq  no-lock no-error. 

assign unid-monet[1] = 0.
if param-estoq.tem-moeda1 = yes then
   assign unid-monet[2] = param-estoq.moeda1.
if param-estoq.tem-moeda2 = yes then
   assign unid-monet[3] = param-estoq.moeda2.

find first tt-param1 no-lock no-error.
find item where 
     item.it-codigo = tt-param1.it-codigo no-error.

find first cta_ctbl_integr where
     rowid(cta_ctbl_integr) = tt-param1.r-conta 
     no-lock no-error.     
find first saldo-estoq where
     saldo-estoq.it-codigo = item.it-codigo no-lock no-error.
if avail saldo-estoq then
    assign c-cod-refer = saldo-estoq.cod-refer
           c-lote      = saldo-estoq.lote.  
else do:
    find first referencia no-lock no-error.
    if avail referencia then
       assign c-cod-refer = referencia.cod-refer.
end.

assign tt-param1.tipo = 2.

assign l-param = no.

        /* Elimina PR-IT-PER */
        if  item.tipo-contr = 2 and tt-param1.tipo = 1
        or  item.tipo-contr = 2 and tt-param1.tipo = 4
        or  item.tipo-contr = 2 and tt-param1.tipo = 3 then do:
            {cep/ce0108.i3}     /* Atualizacao de Saldo-Terc */
        end.

        if item.tipo-contr = 1 and  tt-param1.tipo = 2 
        or item.tipo-contr = 4 and  tt-param1.tipo = 2 
        or item.tipo-contr = 3 and  tt-param1.tipo = 2 then do:
            assign item.ct-codigo = "0"
                   item.sc-codigo = "0".
        end.

        run totalconsignado.

        /* T -> F  ou  T -> D */
        if  (item.tipo-contr = 2 and tt-param1.tipo = 1)
         or (item.tipo-contr = 2 and tt-param1.tipo = 4) then do:

            for each item-estab 
               where item-estab.it-codigo = item.it-codigo EXCLUSIVE-LOCK:


              &if defined (bf_mat_fech_estab) &then
                if param-estoq.tp-fech = 2 then do:
                   find estab-mat 
                        where estab-mat.cod-estabel = item-estab.cod-estabel
                        no-lock no-error.
                   if avail estab-mat then
                      assign da-ult-per = estab-mat.ult-per-fech.
                end.
                else
                   assign da-ult-per = param-estoq.ult-per-fech.
              &else
                  assign da-ult-per = param-estoq.ult-per-fech.
              &endif
               run cdp/cdapi005.p (input  da-ult-per    ,
                                   output da-iniper-x   ,
                                   output da-fimper-x   ,
                                   output i-per-corrente,
                                   output i-ano-corrente,
                                   output da-aux        ,
                                   output da-aux        ).
               assign tt-param1.da-inipa-x = da-iniper-x.

               find first estabelec where
                    estabelec.cod-estabel = item-estab.cod-estabel 
                    no-lock no-error.
               repeat preselect each saldo-estoq 
                  where saldo-estoq.cod-estabel = item-estab.cod-estabel
                    and saldo-estoq.it-codigo   = item-estab.it-codigo NO-LOCK:


                  find next saldo-estoq 
                      where saldo-estoq.cod-estabel = item-estab.cod-estabel
                       and saldo-estoq.it-codigo   = item-estab.it-codigo.

                  for each movto-estoq 
                     where movto-estoq.it-codigo   = item-estab.it-codigo
                       and movto-estoq.cod-estabel = item-estab.cod-estabel
                       and movto-estoq.cod-depos   = saldo-estoq.cod-depos
                       and movto-estoq.cod-localiz = saldo-estoq.cod-localiz
                       and movto-estoq.lote        = saldo-estoq.lote
                       and movto-estoq.cod-refer   = saldo-estoq.cod-refer
                       and movto-estoq.dt-trans   >=  tt-param1.da-inipa-x 
                       and movto-estoq.esp-docto   <> 1 /* "ACA" */
                       and  movto-estoq.esp-docto  <> 8 /* "EAC" */ no-lock
                       break by movto-estoq.it-codigo
                             by  year(movto-estoq.dt-trans)
                             by month(movto-estoq.dt-trans):

                       if  first-of(month(movto-estoq.dt-trans)) then
                            assign de-vl-mat-m = 0
                                   de-vl-mob-m = 0
                                   de-vl-ggf-m = 0
                                   de-vl-mat-p = 0
                                   de-vl-mob-p = 0
                                   de-vl-ggf-p = 0
                                   de-vl-mat-o = 0
                                   de-vl-mob-o = 0
                                   de-vl-ggf-o = 0
                                   de-quant    = 0.

                       if  movto-estoq.tipo-trans = 1 then do i-x = 1 to 3:
                            assign de-vl-mat-m[i-x] = de-vl-mat-m[i-x] 
                                                    + movto-estoq.valor-mat-m[i-x]
                                   de-vl-mob-m[i-x] = de-vl-mob-m[i-x] 
                                                    + movto-estoq.valor-mob-m[i-x]
                                   de-vl-ggf-m[i-x] = de-vl-ggf-m[i-x] 
                                                    + movto-estoq.valor-ggf-m[i-x]
                                   de-vl-mat-o[i-x] = de-vl-mat-o[i-x] 
                                                    + movto-estoq.valor-mat-o[i-x]
                                   de-vl-mob-o[i-x] = de-vl-mob-o[i-x] 
                                                    + movto-estoq.valor-mob-o[i-x]
                                   de-vl-ggf-o[i-x] = de-vl-ggf-o[i-x] 
                                                    + movto-estoq.valor-ggf-o[i-x]
                                   de-vl-mat-p[i-x] = de-vl-mat-p[i-x] 
                                                    + movto-estoq.valor-mat-p[i-x]
                                   de-vl-mob-p[i-x] = de-vl-mob-p[i-x] 
                                                    + movto-estoq.valor-mob-p[i-x]
                                   de-vl-ggf-p[i-x] = de-vl-ggf-p[i-x] 
                                                    + movto-estoq.valor-ggf-p[i-x].
                       end.
                       else do  i-x = 1 to 3:
                            assign de-vl-mat-m[i-x] = de-vl-mat-m[i-x] 
                                                    - movto-estoq.valor-mat-m[i-x]                               
                                   de-vl-mob-m[i-x] = de-vl-mob-m[i-x] 
                                                    - movto-estoq.valor-mob-m[i-x]
                                   de-vl-ggf-m[i-x] = de-vl-ggf-m[i-x] 
                                                    - movto-estoq.valor-ggf-m[i-x]
                                   de-vl-mat-o[i-x] = de-vl-mat-o[i-x] 
                                                    - movto-estoq.valor-mat-o[i-x]
                                   de-vl-mob-o[i-x] = de-vl-mob-o[i-x] 
                                                    - movto-estoq.valor-mob-o[i-x]
                                   de-vl-ggf-o[i-x] = de-vl-ggf-o[i-x] 
                                                    - movto-estoq.valor-ggf-o[i-x]
                                   de-vl-mat-p[i-x] = de-vl-mat-p[i-x] 
                                                    - movto-estoq.valor-mat-p[i-x]
                                   de-vl-mob-p[i-x] = de-vl-mob-p[i-x] 
                                                    - movto-estoq.valor-mob-p[i-x]
                                   de-vl-ggf-p[i-x] = de-vl-ggf-p[i-x] 
                                                    - movto-estoq.valor-ggf-p[i-x].
                       end.
                       if  item.tipo-contr = 2 and tt-param1.tipo = 4 then do:
                           assign de-quant = de-quant 
                                           + if movto-estoq.tipo-trans = 1 then 
                                                movto-estoq.quantidade
                                             else 
                                               (movto-estoq.quantidade * -1).
                       end.

                       if  last-of(month(movto-estoq.dt-trans)) 
                       and (de-vl-mat-m[1] <> 0
                       or   de-vl-mob-m[1] <> 0 
                       or   de-vl-ggf-m[1] <> 0 
                       or   de-vl-mat-p[1] <> 0
                       or   de-vl-mob-p[1] <> 0 
                       or   de-vl-ggf-p[1] <> 0 
                       or   de-vl-mat-o[1] <> 0
                       or   de-vl-mob-o[1] <> 0 
                       or   de-vl-ggf-o[1] <> 0 
                       or   de-quant     <> 0) then do:
                            if  month(movto-estoq.dt-trans) = 12 then
                                assign da-data =
                                 date(12,01,year(movto-estoq.dt-trans)).
                            else
                                assign da-data =
                                    date(month(movto-estoq.dt-trans),01,
                                    year(movto-estoq.dt-trans)).
                            assign l-tipo = no.

                            if estabelec.custo-contab = 1 then do:
                                if de-vl-mat-m[1] >= 0 then
                                    assign l-tipo[1] = yes.
                                if  de-vl-mat-m[1] <> 0  then do: 
                                      {cep/esce0108.i1 "valor-mat-m" "de-vl-mat-m" "1"}
                                end.

                                if de-vl-mob-m[1] >= 0 then  
                                    assign l-tipo[2] = yes. 
                                if  de-vl-mob-m[1] <> 0 then do: 
                                      {cep/esce0108.i1 "valor-mob-m" "de-vl-mob-m" "2"}
                                end.

                                if de-vl-ggf-m[1] >= 0 then  
                                    assign l-tipo[3] = yes. 
                                if  de-vl-ggf-m[1] <> 0 then do: 
                                      {cep/esce0108.i1 "valor-ggf-m" "de-vl-ggf-m" "3"}
                                end.
                            end.
                            if estabelec.custo-contab = 2 then do:
                                if de-vl-mat-p[1] >= 0 then
                                    assign l-tipo[4] = yes.
                                if de-vl-mat-p[1] <> 0 then do: 
                                      {cep/esce0108.i1 "valor-mat-p" "de-vl-mat-p" "4"}
                                end.

                                if de-vl-mob-p[1] >= 0 then  
                                    assign l-tipo[5] = yes. 
                                if de-vl-mob-p[1] <> 0 then do: 
                                      {cep/esce0108.i1 "valor-mob-p" "de-vl-mob-p" "5"}
                                end.

                                if de-vl-ggf-p[1] >= 0 then  
                                    assign l-tipo[6] = yes. 
                                if de-vl-ggf-p[1] <> 0 then do: 
                                     {cep/esce0108.i1 "valor-ggf-p" "de-vl-ggf-p" "6"}
                                end.
                            end.
                            if estabelec.custo-contab = 3 then do:     
                                if de-vl-mat-o[1] >= 0 then
                                    assign l-tipo[7] = yes.
                                if de-vl-mat-o[1] <> 0 then do: 
                                      {cep/esce0108.i1 "valor-mat-o" "de-vl-mat-o" "7"}
                                end.

                                if de-vl-mob-o[1] >= 0 then  
                                    assign l-tipo[8] = yes. 
                                if de-vl-mob-o[1] <> 0 then do: 
                                      {cep/esce0108.i1 "valor-mob-o" "de-vl-mob-o" "8"}
                                end.

                                if de-vl-ggf-o[1] >= 0 then  
                                    assign l-tipo[9] = yes. 
                                if de-vl-ggf-o[1] <> 0 then do: 
                                      {cep/esce0108.i1 "valor-ggf-o" "de-vl-ggf-o" "9"}
                                end.   
                            end.

                            if  item.tipo-contr = 2 and tt-param1.tipo = 4 
                            and de-quant <> 0 then do:
                                create tt-movto.
                                assign
                                  tt-movto.it-codigo   = item-estab.it-codigo
                                  tt-movto.cod-versao-integ = 1
                                  tt-movto.cod-estabel = item-estab.cod-estabel 
                                  tt-movto.dt-trans    = da-data
                                  tt-movto.tipo-trans  = if de-quant > 0 then 2
                                                               else 1
                                  tt-movto.esp-docto   = 6
                                  tt-movto.un          = item.un
                                  tt-movto.cod-depos   = saldo-estoq.cod-depos 
                                  tt-movto.cod-localiz = saldo-estoq.cod-localiz
                                  tt-movto.lote        = saldo-estoq.lote

                                  tt-movto.nro-docto   = tt-param1.docto1
                                  tt-movto.serie-docto = "TD"
                                  tt-movto.tipo-valor  = 1
                                  tt-movto.conta-contabil = cta_ctbl_integr.cod_cta_ctbl
                                  tt-movto.ct-codigo   = cta_ctbl_integr.cod_cta_ctbl
                                  tt-movto.sc-codigo   = ""
                                  tt-movto.cod-prog-orig = "ESCE0108"
                                  tt-movto.usuario       = c-seg-usuario.
                                if item.tipo-con-est = 4 then 
                                    assign tt-movto.cod-refer = c-cod-refer.
                                if de-quant >= 0 then
                                    assign tt-movto.quantidade = de-quant. 
                                else 
                                    assign tt-movto.quantidade = de-quant * (-1).

                                run cep/ceapi001.p (input-output table tt-movto,
                                                    input-output table tt-erro,
                                                    input yes).

                                find first tt-erro no-lock no-error.
                                if avail tt-erro then do: 
                                    run cdp/cd0666.w (input table tt-erro).
                                    undo,return "NOK".
                                end.   
                                find first tt-movto no-error.
                                if avail tt-movto then 
                                   delete tt-movto.
                            end.
                       end.
                  end.

                  for each movto-estoq 
                     where movto-estoq.it-codigo   = item-estab.it-codigo
                       and movto-estoq.cod-estabel = item-estab.cod-estabel
                       and movto-estoq.cod-depos   = saldo-estoq.cod-depos
                       and movto-estoq.cod-localiz = saldo-estoq.cod-localiz
                       and movto-estoq.lote        = saldo-estoq.lote
                       and movto-estoq.cod-refer   = saldo-estoq.cod-refer
                       and movto-estoq.dt-trans   >= tt-param1.da-inipa-x
                       and (movto-estoq.esp-docto   = 1 /*"ACA"*/
                        or  movto-estoq.esp-docto   = 8 /*"EAC"*/ ) no-lock:

                     if  month(movto-estoq.dt-trans) = 12 then
                         assign da-data =
                               date(12,01,year(movto-estoq.dt-trans)).
                     else
                         assign da-data =
                               date(month(movto-estoq.dt-trans),01,
                               year(movto-estoq.dt-trans)).
                     create tt-movto.
                     assign tt-movto.it-codigo    = movto-estoq.it-codigo
                            tt-movto.cod-versao-integ = 1
                            tt-movto.cod-refer    = movto-estoq.cod-refer
                            tt-movto.cod-estabel  = movto-estoq.cod-estabel
                            tt-movto.dt-trans     = da-data
                            tt-movto.cod-depos    = movto-estoq.cod-depos
                            tt-movto.cod-localiz  = movto-estoq.cod-localiz
                            tt-movto.lote         = movto-estoq.lote
                            tt-movto.tipo-trans   = 
                                                if movto-estoq.esp-docto = 1
                                                then 2
                                                else 1
                            tt-movto.nr-ord-produ = movto-estoq.nr-ord-produ
                            tt-movto.conta-contabil = cta_ctbl_integr.cod_cta_ctbl
                            tt-movto.ct-codigo    = cta_ctbl_integr.cod_cta_ctbl
                            tt-movto.sc-codigo    = ""
                            tt-movto.tipo-valor   = 1
                            tt-movto.serie-docto  = if item.tipo-contr = 2 and 
                                                       tt-param1.tipo = 1 then
                                                      "TF"
                                                   else "TD"
                            tt-movto.esp-docto    = if item.tipo-contr = 2 and 
                                                       tt-param1.tipo = 1 then                                                   29 /*"RFS"*/ 
                                                     else 27 /*"RDD"*/
                            tt-movto.un           = movto-estoq.un
                            tt-movto.nro-docto    = movto-estoq.nro-docto
                            tt-movto.num-sequen   = movto-estoq.num-sequen
                            tt-movto.cod-prog-orig = "ESCE0108"
                            tt-movto.usuario       = c-seg-usuario.
                     if item.tipo-con-est = 4 then 
                         assign tt-movto.cod-refer = c-cod-refer.
                     if item.tipo-contr = 2 and tt-param1.tipo = 4 then do:
                         assign tt-movto.quantidade = movto-estoq.quantidade.
                     end.    
                     if item.tipo-contr = 2 and tt-param1.tipo = 4 then do:
                         assign tt-movto.quantidade = movto-estoq.quantidade. 
                     end.

                     run cep/ceapi001.p (input-output table tt-movto,
                                         input-output table tt-erro,
                                         input yes).

                     find first tt-erro no-lock no-error.
                     if avail tt-erro then do: 
                         run cdp/cd0666.w (input table tt-erro).
                         undo,return "NOK".
                     end.
                     find first tt-movto no-error.
                     if avail tt-movto then 
                         delete tt-movto.
                  end.

                  if  saldo-estoq.qtidade-ini <> 0 
                  and item.tipo-contr = 2 and tt-param1.tipo = 4 then do:
                      create tt-movto.
                      assign tt-movto.it-codigo   = item.it-codigo
                             tt-movto.cod-versao-integ = 1
                             tt-movto.un          = item.un
                             tt-movto.cod-estabel = item-estab.cod-estabel
                             tt-movto.dt-trans    = tt-param1.da-inipa-x
                             tt-movto.tipo-trans  = if saldo-estoq.qtidade-ini < 0
                                                     then 1
                                                     else 2  
                             tt-movto.esp-docto   = 6
                             tt-movto.cod-depos   = saldo-estoq.cod-depos
                             tt-movto.cod-localiz = saldo-estoq.cod-localiz
                             tt-movto.lote        = saldo-estoq.lote
                             tt-movto.cod-refer   = saldo-estoq.cod-refer
                             tt-movto.nro-docto   = tt-param1.docto1
                             tt-movto.serie-docto = if item.tipo-contr = 2 and
                                                       tt-param1.tipo = 1  
                                                    then "TF"
                                                    else "TD"
                             tt-movto.tipo-valor  = 1
                             tt-movto.conta-contabil = cta_ctbl_integr.cod_cta_ctbl
                             tt-movto.ct-codigo   = cta_ctbl_integr.cod_cta_ctbl
                             tt-movto.sc-codigo   = ""
                             tt-movto.cod-prog-orig = "ESCE0108"
                             tt-movto.usuario       = c-seg-usuario.
                      if item.tipo-con-est = 4 then 
                         assign tt-movto.cod-refer = c-cod-refer.
                      if saldo-estoq.qtidade-ini >= 0 then
                         assign tt-movto.quantidade =
                                               saldo-estoq.qtidade-ini.
                      else
                         assign tt-movto.quantidade = 
                                               saldo-estoq.qtidade-ini * (-1).

                  end.
                  run cep/ceapi001.p (input-output table tt-movto,
                                      input-output table tt-erro,
                                      input yes).

                  find first tt-erro no-lock no-error.
                  if avail tt-erro then do: 
                     run cdp/cd0666.w (input table tt-erro).
                      undo,return "NOK".
                  end.       
                  find first tt-movto no-error.
                  if avail tt-movto then 
                       delete tt-movto.
               end.

               assign l-tipo = no.
               if item-estab.sald-ini-mat-m[1] >= 0 then
                  assign l-tipo[1] = yes.
               if item-estab.sald-ini-mob-m[1] >= 0 then
                  assign l-tipo[2] = yes.
               if item-estab.sald-ini-ggf-m[1] >= 0 then
                  assign l-tipo[3] = yes.
               if item-estab.sald-ini-mat-o[1] >= 0 then
                  assign l-tipo[4] = yes.
               if item-estab.sald-ini-mob-o[1] >= 0 then
                  assign l-tipo[5] = yes.
               if item-estab.sald-ini-ggf-o[1] >= 0 then
                  assign l-tipo[6] = yes.
               if item-estab.sald-ini-mat-p[1] >= 0 then
                  assign l-tipo[7] = yes.
               if item-estab.sald-ini-mob-p[1] >= 0 then
                  assign l-tipo[8] = yes.
               if item-estab.sald-ini-ggf-p[1] >= 0 then
                  assign l-tipo[9] = yes.

               do i-cont = 1 to 3:
                    assign item-estab.val-unit-mat-m[i-cont] = 0
                           item-estab.val-unit-mob-m[i-cont] = 0
                           item-estab.val-unit-ggf-m[i-cont] = 0
                           item-estab.val-unit-mat-p[i-cont] = 0
                           item-estab.val-unit-mob-p[i-cont] = 0
                           item-estab.val-unit-ggf-p[i-cont] = 0
                           item-estab.val-unit-mat-o[i-cont] = 0
                           item-estab.val-unit-mob-o[i-cont] = 0
                           item-estab.val-unit-ggf-o[i-cont] = 0.
               end.            

               FIND item-uni-estab WHERE item-uni-estab.it-codigo    = ITEM.it-codigo
                                     AND item-uni-estab.cod-estabel  = estabelec.cod-estabel NO-LOCK NO-ERROR.

               if estabelec.custo-contab = 1 then do:

                   if  item-estab.sald-ini-mat-m[1] <> 0 then do: 
                        {cep/esce0108.i2 "valor-mat-m" "item-estab.sald-ini-mat-m" "1"}
                   end.

                   if  item-estab.sald-ini-mob-m[1] <> 0 then do: 
                        {cep/esce0108.i2 "valor-mob-m" "item-estab.sald-ini-mob-m" "2"}
                   end.                                                

                   if  item-estab.sald-ini-ggf-m[1] <> 0 then do: 
                        {cep/esce0108.i2 "valor-ggf-m" "item-estab.sald-ini-ggf-m" "3"}
                   end.    
               end.

               if estabelec.custo-contab = 2 then do:    
                   if  item-estab.sald-ini-mat-p[1] <> 0 then do: 
                        {cep/esce0108.i2 "valor-mat-p" "item-estab.sald-ini-mat-p" "4"}
                   end.                                            

                   if  item-estab.sald-ini-mob-p[1] <> 0 then do: 
                        {cep/esce0108.i2 "valor-mob-p" "item-estab.sald-ini-mob-p" "5"}
                   end.

                   if  item-estab.sald-ini-ggf-p[1] <> 0 then do: 
                        {cep/esce0108.i2 "valor-ggf-p" "item-estab.sald-ini-ggf-p" "6"}
                   end.
               end.

               if estabelec.custo-contab = 3 then do:     
                   if  item-estab.sald-ini-mat-o[1] <> 0 then do: 
                        {cep/esce0108.i2 "valor-mat-o" "item-estab.sald-ini-mat-o" "7"}
                   end.                                            

                   if  item-estab.sald-ini-mob-o[1] <> 0 then do: 
                        {cep/esce0108.i2 "valor-mob-o" "item-estab.sald-ini-mob-o" "8"}
                   end.

                   if  item-estab.sald-ini-ggf-o[1] <> 0 then do: 
                        {cep/esce0108.i2 "valor-ggf-o" "item-estab.sald-ini-ggf-o" "9"}
                   end.
               end.
            end.

            if item.tipo-contr = 2 and tt-param1.tipo = 1 then
                assign item.tipo-contr = 1.
            else 
            if item.tipo-contr = 2 and tt-param1.tipo = 4 then 
                assign item.tipo-contr = 4.
        end.  

        /* DD -> F */
        if item.tipo-contr = 4 and tt-param1.tipo = 1 then do:
            for each item-estab 
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

                for each movto-estoq
                   where movto-estoq.it-codigo = item.it-codigo
                     and movto-estoq.dt-trans  > da-ult-fech-dia
                     and movto-estoq.esp-docto = 27 EXCLUSIVE-LOCK: /*"RDD"*/

                     assign movto-estoq.esp-docto   = 29 /*"RFS"*/
                            movto-estoq.serie-docto = "DF".
                end.
                assign item.tipo-contr = 1.
            end.
        end.


        /* F -> T , DD -> T  e  C -> T */
        if  item.tipo-contr = 1 and tt-param1.tipo = 2 
        or  item.tipo-contr = 4 and tt-param1.tipo = 2 
        or  item.tipo-contr = 3 and tt-param1.tipo = 2  then do:          
            assign de-quant = 0.

            for each item-estab 
                where item-estab.it-codigo = item.it-codigo EXCLUSIVE-LOCK:
            

               &if defined (bf_mat_fech_estab) &then
                 if param-estoq.tp-fech = 2 then do:
                    find estab-mat 
                         where estab-mat.cod-estabel = item-estab.cod-estabel
                         no-lock no-error.
                    if avail estab-mat then
                       assign da-ult-per      = estab-mat.ult-per-fech
                              da-ult-fech-dia = estab-mat.ult-fech-dia.
                 end.
                 else
                    assign da-ult-per      = param-estoq.ult-per-fech
                           da-ult-fech-dia = param-estoq.ult-fech-dia.
               &else
                   assign da-ult-per      = param-estoq.ult-per-fech
                          da-ult-fech-dia = param-estoq.ult-fech-dia.
               &endif

                if  item.tipo-contr = 1 and tt-param1.tipo = 2 then do:
                    for each movto-estoq 
                        where movto-estoq.it-codigo = item.it-codigo
                        and   movto-estoq.dt-trans >  da-ult-fech-dia
                        and   movto-estoq.esp-docto = 29 /*"RFS"*/ EXCLUSIVE-LOCK :
                       delete movto-estoq. 
                    end.
                end.
                if  item.tipo-contr = 4 and tt-param1.tipo = 2 then do:
                    for each movto-estoq
                        where movto-estoq.it-codigo = item.it-codigo
                        and   movto-estoq.dt-trans >  da-ult-fech-dia
                        and   movto-estoq.esp-docto = 27 /*"RDD"*/ EXCLUSIVE-LOCK:
                       find first saldo-estoq where
                            saldo-estoq.cod-estabel = movto-estoq.cod-estabel and
                            saldo-estoq.cod-depos   = movto-estoq.cod-depos   and
                            saldo-estoq.cod-localiz = movto-estoq.cod-localiz and 
                            saldo-estoq.lote        = movto-estoq.lote        and
                            saldo-estoq.it-codigo   = movto-estoq.it-codigo   and
                            saldo-estoq.cod-refer   = movto-estoq.cod-refer EXCLUSIVE-LOCK no-error.
                       if avail saldo-estoq and movto-estoq.tipo-trans = 2 then
                           assign saldo-estoq.qtidade-atu = saldo-estoq.qtidade-atu
                                                          + movto-estoq.quantidade.
                       else if avail saldo-estoq then
                           assign saldo-estoq.qtidade-atu = saldo-estoq.qtidade-atu
                                                          - movto-estoq.quantidade.
                       delete movto-estoq.
                    end.
                end.
                if  item.tipo-contr = 3 and tt-param1.tipo = 2 then do:
                    for each movto-estoq
                        where movto-estoq.it-codigo = item.it-codigo
                        and   movto-estoq.dt-trans >  da-ult-fech-dia
                        and   movto-estoq.esp-docto = 26 /*"RCS"*/ EXCLUSIVE-LOCK:
                       delete movto-estoq.
                    end.
                end.
                run cdp/cdapi005.p (input  da-ult-per    ,
                                    output da-iniper-x   ,
                                    output da-fimper-x   ,
                                    output i-per-corrente,
                                    output i-ano-corrente,
                                    output da-aux        ,
                                    output da-aux        ).
                assign tt-param1.da-inipa-x = da-iniper-x.

                assign de-quant = 0.

                find first estabelec where
                     estabelec.cod-estabel = item-estab.cod-estabel 
                     no-lock no-error.

                for each saldo-estoq 
                   where saldo-estoq.it-codigo   = item-estab.it-codigo
                     and saldo-estoq.cod-estabel = item-estab.cod-estabel
                   no-lock:
                     assign de-quant = de-quant + saldo-estoq.qtidade-atu.
                end.

                /* Consignado -> Total */
                if  item.tipo-contr = 3 and tt-param1.tipo = 2 then do :
                   do i-x = 1 to 3 :
                      if estabelec.custo-contab = 1 then do:
                            assign de-sald-ini-mat-m[i-x] = fn_ajust_dec((( tt-param1.medio-mat[i-x] 
                                                          * de-quant         ) -
                                                          item-estab.sald-ini-mat-m[i-x]),unid-monet[i-x]).
                            assign de-sald-ini-mob-m[i-x] = fn_ajust_dec((( tt-param1.medio-mob[i-x]
                                                          * de-quant         ) -
                                                          item-estab.sald-ini-mob-m[i-x]),unid-monet[i-x]).
                            assign de-sald-ini-ggf-m[i-x] = fn_ajust_dec((( tt-param1.medio-ggf[i-x]
                                                          * de-quant         ) -
                                                          item-estab.sald-ini-ggf-m[i-x]),unid-monet[i-x]).
                            assign item-estab.val-unit-mat-m[i-x] = tt-param1.medio-mat[i-x]
                                   item-estab.val-unit-mob-m[i-x] = tt-param1.medio-mob[i-x]
                                   item-estab.val-unit-ggf-m[i-x] = tt-param1.medio-ggf[i-x].    
                      end.             
                      if estabelec.custo-contab = 2 then do:
                            assign de-sald-ini-mat-o[i-x] = fn_ajust_dec((( tt-param1.medio-mat[i-x] 
                                                          * de-quant         ) -
                                                          item-estab.sald-ini-mat-o[i-x]),unid-monet[i-x]).
                            assign de-sald-ini-mob-o[i-x] = fn_ajust_dec((( tt-param1.medio-mob[i-x]
                                                          * de-quant         ) -
                                                          item-estab.sald-ini-mob-o[i-x]),unid-monet[i-x]).
                            assign de-sald-ini-ggf-o[i-x] = fn_ajust_dec((( tt-param1.medio-ggf[i-x]
                                                          * de-quant         ) -
                                                          item-estab.sald-ini-ggf-o[i-x]),unid-monet[i-x]).
                            assign item-estab.val-unit-mat-o[i-x] = tt-param1.medio-mat[i-x]
                                   item-estab.val-unit-mob-o[i-x] = tt-param1.medio-mob[i-x]
                                   item-estab.val-unit-ggf-o[i-x] = tt-param1.medio-ggf[i-x].
                            assign item-estab.sald-atu-mat-o[i-x] = fn_ajust_dec((tt-param1.medio-mat[i-x] * de-quant),unid-monet[i-x]).
                            assign item-estab.sald-atu-mob-o[i-x] = fn_ajust_dec((tt-param1.medio-mob[i-x] * de-quant),unid-monet[i-x]).
                            assign item-estab.sald-atu-ggf-o[i-x] = fn_ajust_dec((tt-param1.medio-ggf[i-x] * de-quant),unid-monet[i-x]).
                      end.             
                      if estabelec.custo-contab = 3 then do:
                            assign de-sald-ini-mat-p[i-x] = fn_ajust_dec((( tt-param1.medio-mat[i-x] 
                                                          * de-quant         ) -
                                                          item-estab.sald-ini-mat-p[i-x]),unid-monet[i-x]).
                            assign de-sald-ini-mob-p[i-x] = fn_ajust_dec((( tt-param1.medio-mob[i-x]
                                                          * de-quant         ) -
                                                          item-estab.sald-ini-mob-p[i-x]),unid-monet[i-x]).
                            assign de-sald-ini-ggf-p[i-x] = fn_ajust_dec((( tt-param1.medio-ggf[i-x]
                                                          * de-quant         ) -
                                                          item-estab.sald-ini-ggf-p[i-x]),unid-monet[i-x]).
                            assign item-estab.val-unit-mat-p[i-x] = tt-param1.medio-mat[i-x]
                                   item-estab.val-unit-mob-p[i-x] = tt-param1.medio-mob[i-x]
                                   item-estab.val-unit-ggf-p[i-x] = tt-param1.medio-ggf[i-x].
                            assign item-estab.sald-atu-mat-o[i-x] = fn_ajust_dec((tt-param1.medio-mat[i-x] * de-quant),unid-monet[i-x]).
                            assign item-estab.sald-atu-mob-o[i-x] = fn_ajust_dec((tt-param1.medio-mob[i-x] * de-quant),unid-monet[i-x]).
                            assign item-estab.sald-atu-ggf-o[i-x] = fn_ajust_dec((tt-param1.medio-ggf[i-x] * de-quant),unid-monet[i-x]).
                      end.       
                   end.

                   if(estabelec.custo-contab = 1 and
                      (de-sald-ini-mat-m[1] > 0 or
                       de-sald-ini-mat-m[2] > 0 or
                       de-sald-ini-mat-m[3] > 0 or
                       de-sald-ini-mob-m[1] > 0 or
                       de-sald-ini-mob-m[2] > 0 or
                       de-sald-ini-mob-m[3] > 0 or
                       de-sald-ini-ggf-m[1] > 0 or
                       de-sald-ini-ggf-m[2] > 0 or
                       de-sald-ini-ggf-m[3] > 0)) or
                     (estabelec.custo-contab = 2 and 
                      (de-sald-ini-mat-o[1] > 0 or
                       de-sald-ini-mat-o[2] > 0 or
                       de-sald-ini-mat-o[3] > 0 or
                       de-sald-ini-mob-o[1] > 0 or
                       de-sald-ini-mob-o[2] > 0 or
                       de-sald-ini-mob-o[3] > 0 or
                       de-sald-ini-ggf-o[1] > 0 or
                       de-sald-ini-ggf-o[2] > 0 or
                       de-sald-ini-ggf-o[3] > 0)) or 
                     (estabelec.custo-contab = 3 and  
                      (de-sald-ini-mat-p[1] > 0 or
                       de-sald-ini-mat-p[2] > 0 or
                       de-sald-ini-mat-p[3] > 0 or
                       de-sald-ini-mob-p[1] > 0 or
                       de-sald-ini-mob-p[2] > 0 or
                       de-sald-ini-mob-p[3] > 0 or
                       de-sald-ini-ggf-p[1] > 0 or
                       de-sald-ini-ggf-p[2] > 0 or
                       de-sald-ini-ggf-p[3] > 0)) then do :
                      /* Sera gerada uma movimentacao para se chegar ao saldo
                         inicial correspondente ao medio informado pelo usuario */

                      create tt-movto.
                      assign tt-movto.it-codigo    = item.it-codigo
                             tt-movto.cod-versao-integ = 1
                             tt-movto.un           = item.un
                             tt-movto.cod-estabel  = item-estab.cod-estabel
                             tt-movto.dt-trans     = tt-param1.da-inipa-x
                             tt-movto.tipo-trans   = 1
                             tt-movto.esp-docto    = 6 /*"DIV"*/
                             tt-movto.cod-depos    = 
                                                   if item.deposito-pad = ""
                                                     then  c-depos-pad                   
                                                      else item.deposito-pad
                             tt-movto.nro-docto    = tt-param1.docto1
                             tt-movto.serie-docto  = "CT"
                             tt-movto.tipo-valor   = 1
                             tt-movto.cod-localiz  = item.cod-localiz
                             tt-movto.conta-contabil = cta_ctbl_integr.cod_cta_ctbl
                             tt-movto.ct-codigo    = cta_ctbl_integr.cod_cta_ctbl
                             tt-movto.sc-codigo    = ""
                             tt-movto.conta-db     = ""
                             tt-movto.quantidade   = 0
                             tt-movto.lote          = c-lote.       
                      if item.tipo-con-est = 4 then 
                         assign tt-movto.cod-refer = c-cod-refer.

                      if estabelec.custo-contab = 1 then
                         assign tt-movto.valor-mat-m[1] = 
                                                      if de-sald-ini-mat-m[1] > 0
                                                         then de-sald-ini-mat-m[1]
                                                         else 0
                                tt-movto.valor-mat-m[2] = 
                                                      if de-sald-ini-mat-m[2] > 0
                                                         then de-sald-ini-mat-m[2]
                                                         else 0
                                tt-movto.valor-mat-m[3] = 
                                                      if de-sald-ini-mat-m[3] > 0
                                                         then de-sald-ini-mat-m[3]
                                                         else 0
                                tt-movto.valor-mob-m[1] =  
                                                      if de-sald-ini-mob-m[1] > 0
                                                         then de-sald-ini-mob-m[1]
                                                         else 0
                                tt-movto.valor-mob-m[2] = 
                                                      if de-sald-ini-mob-m[2] > 0
                                                         then de-sald-ini-mob-m[2]
                                                         else 0
                                tt-movto.valor-mob-m[3] = 
                                                      if de-sald-ini-mob-m[3] > 0
                                                         then de-sald-ini-mob-m[3]
                                                         else 0
                                tt-movto.valor-ggf-m[1] =  
                                                      if de-sald-ini-ggf-m[1] > 0
                                                         then de-sald-ini-ggf-m[1]
                                                         else 0
                                tt-movto.valor-ggf-m[2] = 
                                                      if de-sald-ini-ggf-m[2] > 0
                                                         then de-sald-ini-ggf-m[2]
                                                         else 0
                                tt-movto.valor-ggf-m[3] = 
                                                      if de-sald-ini-ggf-m[3] > 0
                                                         then de-sald-ini-ggf-m[3]
                                                         else 0.
                      if estabelec.custo-contab = 2 then
                         assign tt-movto.valor-mat-o[1] = 
                                                      if de-sald-ini-mat-o[1] > 0
                                                         then de-sald-ini-mat-o[1]
                                                         else 0
                                tt-movto.valor-mat-o[2] = 
                                                      if de-sald-ini-mat-o[2] > 0
                                                         then de-sald-ini-mat-o[2]
                                                         else 0
                                tt-movto.valor-mat-o[3] = 
                                                      if de-sald-ini-mat-o[3] > 0
                                                         then de-sald-ini-mat-o[3]
                                                         else 0
                                tt-movto.valor-mob-o[1] =  
                                                      if de-sald-ini-mob-o[1] > 0
                                                         then de-sald-ini-mob-o[1]
                                                         else 0
                                tt-movto.valor-mob-o[2] = 
                                                      if de-sald-ini-mob-o[2] > 0
                                                         then de-sald-ini-mob-o[2]
                                                         else 0
                                tt-movto.valor-mob-o[3] = 
                                                      if de-sald-ini-mob-o[3] > 0
                                                         then de-sald-ini-mob-o[3]
                                                         else 0
                                tt-movto.valor-ggf-o[1] =  
                                                      if de-sald-ini-ggf-o[1] > 0
                                                         then de-sald-ini-ggf-o[1]
                                                         else 0
                                tt-movto.valor-ggf-o[2] = 
                                                      if de-sald-ini-ggf-o[2] > 0
                                                         then de-sald-ini-ggf-o[2]
                                                         else 0
                                tt-movto.valor-ggf-o[3] = 
                                                      if de-sald-ini-ggf-o[3] > 0
                                                         then de-sald-ini-ggf-o[3]
                                                         else 0.
                      if estabelec.custo-contab = 3 then                                    
                         assign tt-movto.valor-mat-p[1] = 
                                                      if de-sald-ini-mat-p[1] > 0
                                                         then de-sald-ini-mat-p[1]
                                                         else 0
                                tt-movto.valor-mat-p[2] = 
                                                      if de-sald-ini-mat-p[2] > 0
                                                         then de-sald-ini-mat-p[2]
                                                         else 0
                                tt-movto.valor-mat-p[3] = 
                                                      if de-sald-ini-mat-p[3] > 0
                                                         then de-sald-ini-mat-p[3]
                                                         else 0
                                tt-movto.valor-mob-p[1] =  
                                                      if de-sald-ini-mob-p[1] > 0
                                                         then de-sald-ini-mob-p[1]
                                                         else 0
                                tt-movto.valor-mob-p[2] = 
                                                      if de-sald-ini-mob-p[2] > 0
                                                         then de-sald-ini-mob-p[2]
                                                         else 0
                                tt-movto.valor-mob-p[3] = 
                                                      if de-sald-ini-mob-p[3] > 0
                                                         then de-sald-ini-mob-p[3]
                                                         else 0
                                tt-movto.valor-ggf-p[1] =  
                                                      if de-sald-ini-ggf-p[1] > 0
                                                         then de-sald-ini-ggf-p[1]
                                                         else 0
                                tt-movto.valor-ggf-p[2] = 
                                                      if de-sald-ini-ggf-p[2] > 0
                                                         then de-sald-ini-ggf-p[2]
                                                         else 0
                                tt-movto.valor-ggf-p[3] = 
                                                      if de-sald-ini-ggf-p[3] > 0
                                                         then de-sald-ini-ggf-p[3]
                                                         else 0.
/*                       run cep/ceapi001.p (input-output table tt-movto, */
/*                                           input-output table tt-erro,  */
/*                                           input yes).                  */
/*                                                                        */
/*                       find first tt-erro no-lock no-error.             */
/*                       if avail tt-erro then do:                        */
/*                           run cdp/cd0666.w (input table tt-erro).      */
/*                           undo, return "NOK".                          */
/*                       end.                                             */
/*                       find first tt-movto no-error.                    */
/*                       if avail tt-movto then                           */
/*                            delete tt-movto.                            */
                   end.

                   if(estabelec.custo-contab = 1 and
                      (de-sald-ini-mat-m[1] < 0 or
                       de-sald-ini-mat-m[2] < 0 or
                       de-sald-ini-mat-m[3] < 0 or
                       de-sald-ini-mob-m[1] < 0 or
                       de-sald-ini-mob-m[2] < 0 or
                       de-sald-ini-mob-m[3] < 0 or
                       de-sald-ini-ggf-m[1] < 0 or
                       de-sald-ini-ggf-m[2] < 0 or
                       de-sald-ini-ggf-m[3] < 0)) or
                     (estabelec.custo-contab = 2 and 
                      (de-sald-ini-mat-o[1] < 0 or
                       de-sald-ini-mat-o[2] < 0 or
                       de-sald-ini-mat-o[3] < 0 or
                       de-sald-ini-mob-o[1] < 0 or
                       de-sald-ini-mob-o[2] < 0 or
                       de-sald-ini-mob-o[3] < 0 or
                       de-sald-ini-ggf-o[1] < 0 or
                       de-sald-ini-ggf-o[2] < 0 or
                       de-sald-ini-ggf-o[3] < 0)) or 
                     (estabelec.custo-contab = 3 and  
                      (de-sald-ini-mat-p[1] < 0 or
                       de-sald-ini-mat-p[2] < 0 or
                       de-sald-ini-mat-p[3] < 0 or
                       de-sald-ini-mob-p[1] < 0 or
                       de-sald-ini-mob-p[2] < 0 or
                       de-sald-ini-mob-p[3] < 0 or
                       de-sald-ini-ggf-p[1] < 0 or
                       de-sald-ini-ggf-p[2] < 0 or
                       de-sald-ini-ggf-p[3] < 0)) then do :
                      create tt-movto.
                      assign tt-movto.it-codigo    = item.it-codigo
                             tt-movto.un           = item.un
                             tt-movto.cod-versao-integ = 1
                             tt-movto.cod-estabel  = item-estab.cod-estabel
                             tt-movto.dt-trans     = tt-param1.da-inipa-x
                             tt-movto.tipo-trans   = 2
                             tt-movto.esp-docto    = 6 /*"DIV"*/
                             tt-movto.cod-depos    = 
                                                   if item.deposito-pad = ""
                                                     then  c-depos-pad                   
                                                      else item.deposito-pad
                             tt-movto.nro-docto    = tt-param1.docto1
                             tt-movto.serie-docto  = "CT"
                             tt-movto.tipo-valor   = 1
                             tt-movto.cod-localiz  = item.cod-localiz
                             tt-movto.conta-contabil = cta_ctbl_integr.cod_cta_ctbl
                             tt-movto.ct-codigo    = cta_ctbl_integr.cod_cta_ctbl
                             tt-movto.sc-codigo    = ""
                             tt-movto.conta-db     = ""
                             tt-movto.quantidade   = 0
                             tt-movto.lote         = c-lote.
                      if item.tipo-con-est = 4 then
                         assign tt-movto.cod-refer = c-cod-refer.
                      if estabelec.custo-contab = 1 then
                         assign tt-movto.valor-mat-m[1] =
                                                      if de-sald-ini-mat-m[1] < 0
                                                         then - de-sald-ini-mat-m[1]
                                                         else 0
                                tt-movto.valor-mat-m[2] =
                                                      if de-sald-ini-mat-m[2] < 0
                                                         then - de-sald-ini-mat-m[2]
                                                         else 0
                                tt-movto.valor-mat-m[3] =
                                                      if de-sald-ini-mat-m[3] < 0
                                                         then - de-sald-ini-mat-m[3]
                                                         else 0
                                tt-movto.valor-mob-m[1] =
                                                      if de-sald-ini-mob-m[1] < 0
                                                         then - de-sald-ini-mob-m[1]
                                                         else 0
                                tt-movto.valor-mob-m[2] =
                                                      if de-sald-ini-mob-m[2] < 0
                                                         then - de-sald-ini-mob-m[2]
                                                         else 0
                                tt-movto.valor-mob-m[3] =
                                                      if de-sald-ini-mob-m[3] < 0
                                                         then - de-sald-ini-mob-m[3]
                                                         else 0
                                tt-movto.valor-ggf-m[1] =
                                                      if de-sald-ini-ggf-m[1] < 0
                                                         then - de-sald-ini-ggf-m[1]
                                                         else 0
                                tt-movto.valor-ggf-m[2] =
                                                      if de-sald-ini-ggf-m[2] < 0
                                                         then - de-sald-ini-ggf-m[2]
                                                         else 0 
                                tt-movto.valor-ggf-m[3] =
                                                      if de-sald-ini-ggf-m[3] < 0
                                                         then - de-sald-ini-ggf-m[3]
                                                         else 0.
                      if estabelec.custo-contab = 2 then
                         assign tt-movto.valor-mat-o[1] =
                                                      if de-sald-ini-mat-o[1] < 0
                                                         then - de-sald-ini-mat-o[1]
                                                         else 0
                                tt-movto.valor-mat-o[2] =
                                                      if de-sald-ini-mat-o[2] < 0
                                                         then - de-sald-ini-mat-o[2]
                                                         else 0
                                tt-movto.valor-mat-o[3] =
                                                      if de-sald-ini-mat-o[3] < 0
                                                         then - de-sald-ini-mat-o[3]
                                                         else 0
                                tt-movto.valor-mob-o[1] =
                                                      if de-sald-ini-mob-o[1] < 0
                                                         then - de-sald-ini-mob-o[1]
                                                         else 0
                                tt-movto.valor-mob-o[2] =
                                                      if de-sald-ini-mob-o[2] < 0
                                                         then - de-sald-ini-mob-o[2]
                                                         else 0
                                tt-movto.valor-mob-o[3] =
                                                      if de-sald-ini-mob-o[3] < 0
                                                         then - de-sald-ini-mob-o[3]
                                                         else 0
                                tt-movto.valor-ggf-o[1] =
                                                      if de-sald-ini-ggf-o[1] < 0
                                                         then - de-sald-ini-ggf-o[1]
                                                         else 0
                                tt-movto.valor-ggf-o[2] =
                                                      if de-sald-ini-ggf-o[2] < 0
                                                         then - de-sald-ini-ggf-o[2]
                                                         else 0
                                tt-movto.valor-ggf-o[3] =
                                                      if de-sald-ini-ggf-o[3] < 0
                                                         then - de-sald-ini-ggf-o[3]
                                                         else 0.
                      if estabelec.custo-contab = 3 then
                         assign tt-movto.valor-mat-p[1] =
                                                      if de-sald-ini-mat-p[1] < 0
                                                         then - de-sald-ini-mat-p[1]
                                                         else 0
                                tt-movto.valor-mat-p[2] =
                                                      if de-sald-ini-mat-p[2] < 0
                                                         then - de-sald-ini-mat-p[2]
                                                         else 0
                                tt-movto.valor-mat-p[3] =
                                                      if de-sald-ini-mat-p[3] < 0
                                                         then - de-sald-ini-mat-p[3]
                                                         else 0
                                tt-movto.valor-mob-p[1] =
                                                      if de-sald-ini-mob-p[1] < 0
                                                         then - de-sald-ini-mob-p[1]
                                                         else 0
                                tt-movto.valor-mob-p[2] =
                                                      if de-sald-ini-mob-p[2] < 0
                                                         then - de-sald-ini-mob-p[2]
                                                         else 0
                                tt-movto.valor-mob-p[3] =
                                                      if de-sald-ini-mob-p[3] < 0
                                                         then - de-sald-ini-mob-p[3]
                                                         else 0
                                tt-movto.valor-ggf-p[1] =
                                                      if de-sald-ini-ggf-p[1] < 0
                                                         then - de-sald-ini-ggf-p[1]
                                                         else 0
                                tt-movto.valor-ggf-p[2] =
                                                      if de-sald-ini-ggf-p[2] < 0
                                                         then - de-sald-ini-ggf-p[2]
                                                         else 0
                                tt-movto.valor-ggf-p[3] =
                                                      if de-sald-ini-ggf-p[3] < 0
                                                         then - de-sald-ini-ggf-p[3]
                                                         else 0.
/*                       run cep/ceapi001.p (input-output table tt-movto, */
/*                                           input-output table tt-erro,  */
/*                                           input yes).                  */
/*                                                                        */
/*                       find first tt-erro no-lock no-error.             */
/*                       if avail tt-erro then do:                        */
/*                           run cdp/cd0666.w (input table tt-erro).      */
/*                           undo, return "NOK".                          */
/*                       end.                                             */
/*                       find first tt-movto no-error.                    */
/*                       if avail tt-movto then                           */
/*                            delete tt-movto.                            */
                   end.
                end.

                /* Fisico -> Total */
                if item.tipo-contr = 1 and tt-param1.tipo = 2  and
                   de-quant      > 0 then do :
                   create tt-movto.
                   assign tt-movto.it-codigo    = item.it-codigo
                          tt-movto.cod-versao-integ = 1
                          tt-movto.un           = item.un
                          tt-movto.cod-estabel  = item-estab.cod-estabel
                          tt-movto.dt-trans     = tt-param1.da-inipa-x
                          tt-movto.tipo-trans   = 1
                          tt-movto.esp-docto    = 6 /*"DIV"*/
                          tt-movto.cod-depos    = if item.deposito-pad = " "
                                                  then  c-depos-pad                   
                                                     else item.deposito-pad
                          tt-movto.nro-docto    = tt-param1.docto1
                          tt-movto.serie-docto  = "FT"
                          tt-movto.tipo-valor   = 1
                          tt-movto.cod-localiz  = item.cod-localiz
                          tt-movto.conta-contabil = cta_ctbl_integr.cod_cta_ctbl
                          tt-movto.ct-codigo    = cta_ctbl_integr.cod_cta_ctbl
                          tt-movto.sc-codigo    = ""
                          tt-movto.conta-db     = ""
                          tt-movto.lote         = c-lote.
                   if item.tipo-con-est = 4 then
                      assign tt-movto.cod-refer = c-cod-refer.
                   if estabelec.custo-contab = 1 then do:
                      assign tt-movto.valor-mat-m[1] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mat[1] ),unid-monet[1]).
                      assign tt-movto.valor-mat-m[2] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mat[2] ),unid-monet[2]).
                      assign tt-movto.valor-mat-m[3] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mat[3] ),unid-monet[3]).
                      assign tt-movto.valor-mob-m[1] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mob[1] ),unid-monet[1]).
                      assign tt-movto.valor-mob-m[2] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mob[2] ),unid-monet[2]).
                      assign tt-movto.valor-mob-m[3] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mob[3] ),unid-monet[3]).
                      assign tt-movto.valor-ggf-m[1] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-ggf[1] ),unid-monet[1]).
                      assign tt-movto.valor-ggf-m[2] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-ggf[2] ),unid-monet[2]).
                      assign tt-movto.valor-ggf-m[3] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-ggf[3] ),unid-monet[3]).
                   end.
                   if estabelec.custo-contab = 2 then do:
                      assign tt-movto.valor-mat-o[1] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mat[1] ),unid-monet[1]).
                      assign tt-movto.valor-mat-o[2] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mat[2] ),unid-monet[2]).
                      assign tt-movto.valor-mat-o[3] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mat[3] ),unid-monet[3]).
                      assign tt-movto.valor-mob-o[1] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mob[1] ),unid-monet[1]).
                      assign tt-movto.valor-mob-o[2] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mob[2] ),unid-monet[2]).
                      assign tt-movto.valor-mob-o[3] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mob[3] ),unid-monet[3]).
                      assign tt-movto.valor-ggf-o[1] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-ggf[1] ),unid-monet[1]).
                      assign tt-movto.valor-ggf-o[2] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-ggf[2] ),unid-monet[2]).
                      assign tt-movto.valor-ggf-o[3] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-ggf[3] ),unid-monet[3]).
                   end.
                   if estabelec.custo-contab = 3 then do:
                      assign tt-movto.valor-mat-p[1] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mat[1] ),unid-monet[1]).
                      assign tt-movto.valor-mat-p[2] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mat[2] ),unid-monet[2]).
                      assign tt-movto.valor-mat-p[3] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mat[3] ),unid-monet[3]).
                      assign tt-movto.valor-mob-p[1] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mob[1] ),unid-monet[1]).
                      assign tt-movto.valor-mob-p[2] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mob[2] ),unid-monet[2]).
                      assign tt-movto.valor-mob-p[3] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-mob[3] ),unid-monet[3]).
                      assign tt-movto.valor-ggf-p[1] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-ggf[1] ),unid-monet[1]).
                      assign tt-movto.valor-ggf-p[2] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-ggf[2] ),unid-monet[2]).
                      assign tt-movto.valor-ggf-p[3] = fn_ajust_dec(( de-quant *
                                                         tt-param1.medio-ggf[3] ),unid-monet[3]).
                   end.

                   for each tt-movto:
                     assign tt-movto.cod-prog-orig = "ESCE0108"
                            tt-movto.usuario       = c-seg-usuario.
                   end.

                end.

                do i-x = 1 to 3:
                   if estabelec.custo-contab = 1 then
                        assign item-estab.val-unit-mat-m[i-x] = tt-param1.medio-mat[i-x]
                               item-estab.val-unit-mob-m[i-x] = tt-param1.medio-mob[i-x]
                               item-estab.val-unit-ggf-m[i-x] = tt-param1.medio-ggf[i-x].
                   if estabelec.custo-contab = 2 then
                        assign item-estab.val-unit-mat-o[i-x] = tt-param1.medio-mat[i-x]
                               item-estab.val-unit-mob-o[i-x] = tt-param1.medio-mob[i-x]
                               item-estab.val-unit-ggf-o[i-x] = tt-param1.medio-ggf[i-x].
                   if estabelec.custo-contab = 3 then
                        assign item-estab.val-unit-mat-p[i-x] = tt-param1.medio-mat[i-x]
                               item-estab.val-unit-mob-p[i-x] = tt-param1.medio-mob[i-x]
                               item-estab.val-unit-ggf-p[i-x] = tt-param1.medio-ggf[i-x].
                end.
/*                 run cep/ceapi001.p (input-output table tt-movto, */
/*                                     input-output table tt-erro,  */
/*                                     input yes).                  */
/*                                                                  */
/*                 find first tt-erro no-lock no-error.             */
/*                 if avail tt-erro then do:                        */
/*                     run cdp/cd0666.w (input table tt-erro).      */
/*                     undo, return "NOK".                          */
/*                 end.                                             */
/*                 find first tt-movto no-error.                    */
/*                 if avail tt-movto then                           */
/*                      delete tt-movto.                            */
            end.
            assign item.tipo-contr = 2.
            find first tt-movto no-error.
            if avail tt-movto then do:  
                 run cep/ceapi001.p (input-output table tt-movto,
                                     input-output table tt-erro,
                                     input yes).
                
            end.
            find first tt-erro no-lock no-error.
            if avail tt-erro then do:
                run cdp/cd0666.w (input table tt-erro).
                assign item.tipo-contr = 1.
                undo, return "NOK".
            end.
            for each tt-movto:
                delete tt-movto.
            end.
        end.

return "OK".

/*********************** Procedure Internas *********************/

Procedure totalconsignado:
       /* T -> C */
        if  item.tipo-contr = 2 and  tt-param1.tipo = 3 then do:       
            for each item-estab where item-estab.it-codigo = item.it-codigo EXCLUSIVE-LOCK:
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

                assign de-mat-m = 0 
                       de-mob-m = 0
                       de-ggf-m = 0
                       de-mat-p = 0
                       de-mob-p = 0
                       de-ggf-p = 0
                       de-mat-o = 0
                       de-mob-o = 0
                       de-ggf-o = 0.

                find first estabelec where
                     estabelec.cod-estabel = item-estab.cod-estabel 
                     no-lock no-error.
                if estabelec.usa-mensal then do  i-moeda = 1 to 3:
                    assign de-mat-m[i-moeda] = item-estab.sald-ini-mat-m[i-moeda]
                           de-mob-m[i-moeda] = item-estab.sald-ini-mob-m[i-moeda]
                           de-ggf-m[i-moeda] = item-estab.sald-ini-ggf-m[i-moeda].
                end.
                if estabelec.usa-on-line then do  i-moeda = 1 to 3:
                    assign de-mat-o[i-moeda] = item-estab.sald-ini-mat-o[i-moeda]
                           de-mob-o[i-moeda] = item-estab.sald-ini-mob-o[i-moeda]
                           de-ggf-o[i-moeda] = item-estab.sald-ini-ggf-o[i-moeda].
                end.                                                    
                if estabelec.usa-padrao then do  i-moeda = 1 to 3:
                    assign de-mat-p[i-moeda] = item-estab.sald-ini-mat-p[i-moeda]
                           de-mob-p[i-moeda] = item-estab.sald-ini-mob-p[i-moeda]
                           de-ggf-p[i-moeda] = item-estab.sald-ini-ggf-p[i-moeda].
                end.                                                    

                assign da-maior-dt = da-ult-fech-dia + 1.
                for each movto-estoq use-index item-data 
                   where movto-estoq.cod-estabel = item-estab.cod-estabel
                    and  movto-estoq.it-codigo   = item.it-codigo
                    and  movto-estoq.dt-trans    > da-ult-fech-dia
                    no-lock:

                    if  can-find(first componente use-index documento where
                        componente.cod-emitente = movto-estoq.cod-emitente and
                        componente.serie-docto  = movto-estoq.serie-docto  and
                        componente.nro-docto    = movto-estoq.nro-docto    and
                        componente.nat-oper     = movto-estoq.nat-oper     and
                        componente.it-codigo    = movto-estoq.it-codigo    and
                        componente.cod-refer    = movto-estoq.cod-refer) then
                        next.

                    assign da-maior-dt = maximum(da-maior-dt,movto-estoq.dt-trans). 
                    if movto-estoq.tipo-trans = 1 then do:
                        if estabelec.usa-mensal then do  i-moeda = 1 to 3:
                            assign de-mat-m[i-moeda] = de-mat-m[i-moeda]
                                                     + movto-estoq.valor-mat-m[i-moeda]
                                   de-mob-m[i-moeda] = de-mob-m[i-moeda]
                                                     + movto-estoq.valor-mob-m[i-moeda]
                                   de-ggf-m[i-moeda] = de-ggf-m[i-moeda]
                                                     + movto-estoq.valor-ggf-m[i-moeda].
                        end.
                        if estabelec.usa-padrao then do  i-moeda = 1 to 3:
                             assign de-mat-p[i-moeda] = de-mat-p[i-moeda]
                                                      + movto-estoq.valor-mat-p[i-moeda]
                                    de-mob-p[i-moeda] = de-mob-p[i-moeda]
                                                      + movto-estoq.valor-mob-p[i-moeda]
                                    de-ggf-p[i-moeda] = de-ggf-p[i-moeda]
                                                      + movto-estoq.valor-ggf-p[i-moeda].
                        end.
                        if estabelec.usa-on-line then do  i-moeda = 1 to 3:
                             assign de-mat-o[i-moeda] = de-mat-o[i-moeda]
                                                      + movto-estoq.valor-mat-o[i-moeda]
                                    de-mob-o[i-moeda] = de-mob-o[i-moeda]
                                                     + movto-estoq.valor-mob-o[i-moeda]
                                    de-ggf-o[i-moeda] = de-ggf-o[i-moeda]
                                                      + movto-estoq.valor-ggf-o[i-moeda].
                        end.     
                     end.
                     else do:
                        if estabelec.usa-mensal then do  i-moeda = 1 to 3:
                            assign de-mat-m[i-moeda] = de-mat-m[i-moeda]
                                                     - movto-estoq.valor-mat-m[i-moeda]
                                   de-mob-m[i-moeda] = de-mob-m[i-moeda]
                                                     - movto-estoq.valor-mob-m[i-moeda]
                                   de-ggf-m[i-moeda] = de-ggf-m[i-moeda]
                                                     - movto-estoq.valor-ggf-m[i-moeda].
                        end.
                        if estabelec.usa-padrao then do  i-moeda = 1 to 3:
                             assign de-mat-p[i-moeda] = de-mat-p[i-moeda]
                                                      - movto-estoq.valor-mat-p[i-moeda]
                                    de-mob-p[i-moeda] = de-mob-p[i-moeda]
                                                      - movto-estoq.valor-mob-p[i-moeda]
                                    de-ggf-p[i-moeda] = de-ggf-p[i-moeda]
                                                      - movto-estoq.valor-ggf-p[i-moeda].
                        end.
                        if estabelec.usa-on-line then do  i-moeda = 1 to 3:
                             assign de-mat-o[i-moeda] = de-mat-o[i-moeda]
                                                      - movto-estoq.valor-mat-o[i-moeda]
                                    de-mob-o[i-moeda] = de-mob-o[i-moeda]
                                                      - movto-estoq.valor-mob-o[i-moeda]
                                    de-ggf-o[i-moeda] = de-ggf-o[i-moeda]
                                                      - movto-estoq.valor-ggf-o[i-moeda].
                        end.     
                     end.
                end.

                if  de-mat-m[1] > 0 or de-mat-m[2] > 0 or de-mat-m[3] > 0
                or  de-mob-m[1] > 0 or de-mob-m[2] > 0 or de-mob-m[3] > 0 
                or  de-ggf-m[1] > 0 or de-ggf-m[2] > 0 or de-ggf-m[3] > 0 
                or  de-mat-p[1] > 0 or de-mat-p[2] > 0 or de-mat-p[3] > 0
                or  de-mob-p[1] > 0 or de-mob-p[2] > 0 or de-mob-p[3] > 0 
                or  de-ggf-p[1] > 0 or de-ggf-p[2] > 0 or de-ggf-p[3] > 0         
                or  de-mat-o[1] > 0 or de-mat-o[2] > 0 or de-mat-o[3] > 0
                or  de-mob-o[1] > 0 or de-mob-o[2] > 0 or de-mob-o[3] > 0 
                or  de-ggf-o[1] > 0 or de-ggf-o[2] > 0 or de-ggf-o[3] > 0 
                then do:
                        create tt-movto.
                        assign tt-movto.it-codigo      = item.it-codigo
                               tt-movto.cod-estabel    = item-estab.cod-estabel
                               tt-movto.dt-trans       = da-maior-dt
                               tt-movto.cod-versao-integ = 1
                               tt-movto.tipo-trans     = 2
                               tt-movto.esp-docto      = 6 /*"DIV"*/
                               tt-movto.cod-depos      = item.deposito-pad
                               tt-movto.serie-docto  = tt-param1.serie1
                               tt-movto.nro-docto    = tt-param1.docto1
                               tt-movto.ct-codigo    = cta_ctbl_integr.cod_cta_ctbl
                               tt-movto.sc-codigo    = cta_ctbl_integr.cod_cta_ctbl
                               tt-movto.conta-contabil = ""
                               tt-movto.un             = item.un
                               tt-movto.cod-prog-orig = "ESCE0108"
                               tt-movto.usuario        = c-seg-usuario
                               tt-movto.lote           = c-lote.
                        if item.tipo-con-est = 4 then 
                            assign tt-movto.cod-refer = c-cod-refer.
                        do i-moeda = 1 to 3:                
                            if estabelec.usa-mensal then do:
                                assign tt-movto.valor-mat-m[i-moeda] = 
                                                   if de-mat-m[i-moeda] > 0
                                                      then de-mat-m[i-moeda]
                                                      else 0
                                       tt-movto.valor-mob-m[i-moeda] =
                                                   if de-mob-m[i-moeda] > 0
                                                      then de-mob-m[i-moeda]
                                                      else 0
                                       tt-movto.valor-ggf-m[i-moeda] = 
                                                   if de-ggf-m[i-moeda] > 0
                                                      then de-ggf-m[i-moeda]
                                                      else 0.   
                            end. 
                            if estabelec.usa-padrao then do:
                                    assign tt-movto.valor-mat-p[i-moeda] = 
                                                       if de-mat-p[i-moeda] > 0
                                                          then de-mat-p[i-moeda]
                                                          else 0.
                                    assign tt-movto.valor-mob-p[i-moeda] =
                                                   if de-mob-p[i-moeda] > 0
                                                      then de-mob-p[i-moeda]
                                                      else 0.
                                    assign tt-movto.valor-ggf-p[i-moeda] = 
                                                   if de-ggf-p[i-moeda] > 0
                                                      then de-ggf-p[i-moeda]
                                                      else 0 .  
                            end.                                   
                            if estabelec.usa-on-line then do:
                                assign tt-movto.valor-mat-o[i-moeda] = 
                                                   if de-mat-o[i-moeda] > 0
                                                      then de-mat-o[i-moeda]
                                                      else 0
                                       tt-movto.valor-mob-o[i-moeda] =
                                                   if de-mob-o[i-moeda] > 0
                                                      then de-mob-o[i-moeda]
                                                      else 0
                                       tt-movto.valor-ggf-o[i-moeda] = 
                                                   if de-ggf-o[i-moeda] > 0
                                                      then de-ggf-o[i-moeda]
                                                      else 0 .  
                            end. 
                        end.
                    run cep/ceapi001.p (input-output table tt-movto,
                                        input-output table tt-erro,
                                        input yes).
                    find first tt-erro no-lock no-error.
                    if avail tt-erro then do: 
                        run cdp/cd0666.w (input table tt-erro).
                        undo,return "NOK".
                    end.    
                    find first tt-movto no-error.
                    if avail tt-movto then 
                        delete tt-movto.

                end.

                if  de-mat-m[1] < 0 or de-mat-m[2] < 0 or de-mat-m[3] < 0
                or  de-mob-m[1] < 0 or de-mob-m[2] < 0 or de-mob-m[3] < 0 
                or  de-ggf-m[1] < 0 or de-ggf-m[2] < 0 or de-ggf-m[3] < 0 
                or  de-mat-p[1] < 0 or de-mat-p[2] < 0 or de-mat-p[3] < 0
                or  de-mob-p[1] < 0 or de-mob-p[2] < 0 or de-mob-p[3] < 0 
                or  de-ggf-p[1] < 0 or de-ggf-p[2] < 0 or de-ggf-p[3] < 0         
                or  de-mat-o[1] < 0 or de-mat-o[2] < 0 or de-mat-o[3] < 0
                or  de-mob-o[1] < 0 or de-mob-o[2] < 0 or de-mob-o[3] < 0 
                or  de-ggf-o[1] < 0 or de-ggf-o[2] < 0 or de-ggf-o[3] < 0 
                then do:
                    create tt-movto.
                    assign tt-movto.it-codigo    = item.it-codigo
                           tt-movto.cod-versao-integ = 1
                           tt-movto.cod-estabel  = item-estab.cod-estabel
                           tt-movto.dt-trans     = da-maior-dt
                           tt-movto.tipo-trans   = 1
                           tt-movto.esp-docto    = 6 /*"DIV"*/
                           tt-movto.cod-depos    = item.deposito-pad
                           tt-movto.serie-docto  = tt-param1.serie1
                           tt-movto.nro-docto    = tt-param1.docto1
                           tt-movto.conta-contabil = cta_ctbl_integr.cod_cta_ctbl
                           tt-movto.ct-codigo    = cta_ctbl_integr.cod_cta_ctbl
                           tt-movto.sc-codigo    = ""
                           tt-movto.un           = item.un
                           tt-movto.cod-prog-orig = "ESCE0108"
                           tt-movto.usuario       = c-seg-usuario
                           tt-movto.lote          = c-lote.
                    if item.tipo-con-est = 4 then 
                        assign tt-movto.cod-refer = c-cod-refer.
                    do i-moeda = 1 to 3:
                       if estabelec.usa-mensal then do:
                           assign tt-movto.valor-mat-m[i-moeda] = 
                                              if de-mat-m[i-moeda] < 0
                                                 then - (de-mat-m[i-moeda])
                                                 else 0
                                  tt-movto.valor-ggf-m[i-moeda] = 
                                              if de-ggf-m[i-moeda] < 0
                                                 then - (de-ggf-m[i-moeda])
                                                 else 0
                                  tt-movto.valor-mob-m[i-moeda] = 
                                              if de-mob-m[i-moeda] < 0
                                                 then - (de-mob-m[i-moeda])
                                                 else 0.      
                       end. 
                       if estabelec.usa-padrao then do:
                               assign tt-movto.valor-mat-p[i-moeda] = 
                                                  if de-mat-p[i-moeda] < 0
                                                     then - (de-mat-p[i-moeda])
                                                     else 0.             
                               assign tt-movto.valor-ggf-p[i-moeda] = 
                                              if de-ggf-p[i-moeda] < 0
                                                 then - (de-ggf-p[i-moeda])
                                                 else 0. 
                               assign tt-movto.valor-mob-p[i-moeda] = 
                                              if de-mob-p[i-moeda] < 0
                                                 then - (de-mob-p[i-moeda])
                                                 else 0.      
                       end. 
                       if estabelec.usa-on-line then do:
                           assign tt-movto.valor-mat-o[i-moeda] = 
                                              if de-mat-o[i-moeda] < 0
                                                 then - (de-mat-o[i-moeda])
                                                 else 0
                                  tt-movto.valor-ggf-o[i-moeda] = 
                                              if de-ggf-o[i-moeda] < 0
                                                 then - (de-ggf-o[i-moeda])
                                                 else 0
                                  tt-movto.valor-mob-o[i-moeda] = 
                                              if de-mob-o[i-moeda] < 0
                                                 then - (de-mob-o[i-moeda])
                                                 else 0.      
                       end. 
                    end.                    

                    run cep/ceapi001.p (input-output table tt-movto,
                                        input-output table tt-erro,
                                        input yes).

                     find first tt-erro no-lock no-error.
                     if avail tt-erro then do: 
                         run cdp/cd0666.w (input table tt-erro).
                         undo,return "NOK".
                     end.           
                     find first tt-movto no-error.
                     if avail tt-movto then 
                        delete tt-movto.

                end.
                do i-cont = 1 to 3:
                    assign item-estab.val-unit-mat-m[i-cont] = 0
                           item-estab.val-unit-mob-m[i-cont] = 0
                           item-estab.val-unit-ggf-m[i-cont] = 0
                           item-estab.val-unit-mat-p[i-cont] = 0
                           item-estab.val-unit-mob-p[i-cont] = 0
                           item-estab.val-unit-ggf-p[i-cont] = 0
                           item-estab.val-unit-mat-o[i-cont] = 0
                           item-estab.val-unit-mob-o[i-cont] = 0
                           item-estab.val-unit-ggf-o[i-cont] = 0.
                end.           
            end.

            assign item.tipo-contr = 3.

        end.   /* Fim T -> C */
end. /* procedure */
