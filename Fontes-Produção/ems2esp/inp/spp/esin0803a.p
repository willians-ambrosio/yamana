/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/************************************************************************
**
**  i-prgvrs.i - Programa para criacao do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

/*Alteraá∆o - 08/09/2006 - tech1007 - Alterado para possuir a definiá∆o dos prÇprocessadores logo no in°cio do programa*/
/**** Alteraá∆o efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
/*************************************************
* i_dbvers.i - Include de vers∆o de banco de dados   
**************************************************/

/* Preprocessadores que identificam os bancos do Produto EMS 5 */

/* Preprocessadores que identificam os bancos do Produto EMS 2 */
/*RAC Incorporado na 2.04*/

/* Preprocessadores que identificam os bancos do Produto HR */
/*Esta include est† sendo liberada vazia para o EMS 2
 para n∆o ocorrer erros de compilaá∆o*/
 


/* Fim */


/* Fim */
    

/* Fim */
    
.    
/* Fim */

 
/*Fim alteraá∆o 08/09/2006*/

def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.002[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.002"
       c-prg-obj = "ESIN0803A".

/* Alteracao - 02/10/2006 - Nakamura - Incluida chamada a include que verifica a integridade dos programas do registro de produto */
/*{include/i-ctrlrp.i {1}}*/

/*Alteraá∆o - 08/09/2006 - tech1007 - Alteraá∆o para exibir o nome do programa que executou o programa que ser† exibido no extrato de vers∆o
                                      Solicitaá∆o realizada na FO 1239827*/

/*Fim alteraá∆o 08/09/2006*/


if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "ESIN0803A"
        no-lock no-error.
        
   if not avail prog_dtsul then do:
          if  c-prg-obj begins "btb":U then
              assign c-prg-obj = "btb~/":U + c-prg-obj.
          else if c-prg-obj begins "men":U then
                  assign c-prg-obj = "men~/":U + c-prg-obj.
          else if c-prg-obj begins "sec":U then
                  assign c-prg-obj = "sec~/":U + c-prg-obj.
          else if c-prg-obj begins "utb":U then
                  assign c-prg-obj = "utb~/":U + c-prg-obj.
          find prog_dtsul where
               prog_dtsul.nom_prog_ext begins c-prg-obj no-lock no-error.
   end .            /*if*/
    
    output to value(c-arquivo-log) append.

    /*Alteraá∆o - 08/09/2006 - tech1007 - Alteraá∆o para exibir o nome do programa que executou o programa que ser† exibido no extrato de vers∆o
                                      Solicitaá∆o realizada na FO 1239827*/
    
        /*FO 1329.898 - tech1139 - 01/08/2006 */
        PUT "ESIN0803A" AT 1 "2.00.00.002" AT 69 STRING(TODAY,'99/99/99') AT 84 STRING(TIME,'HH:MM:SS':U) AT 94 SKIP.
        /*FO 1329.898 - tech1139 - 01/08/2006 */
    
    /*Fim alteraá∆o 08/09/2006*/
                                                  
    if  avail prog_dtsul then do:
        if  prog_dtsul.nom_prog_dpc <> "" then
            put "DPC : ":U at 5 prog_dtsul.nom_prog_dpc  at 12 skip.
        if  prog_dtsul.nom_prog_appc <> "" then
            put "APPC: ":U at 5 prog_dtsul.nom_prog_appc at 12 skip.
        if  prog_dtsul.nom_prog_upc <> "" then
            put "UPC : ":U at 5 prog_dtsul.nom_prog_upc  at 12 skip.
    end.
    output close.        
end.  
error-status:error = no.
/***************************************************
** i_dbtype.i - Tipo de Gerenciadores utilizados
***************************************************/


        
    /* Preprocessadores que identificam os bancos do Produto EMS 5 */
                    
    /* Preprocessadores que identificam os bancos do Produto EMS 2 */
                                                                        
    /* Preprocessadores que identificam os bancos do Produto HR 2 */
            

/* Fim */

 

/*alteracao Anderson(tech540) em 04/02/2003 Include com a definicao 
da temp table utilizada nas includes btb008za.i1 e btb008za.i2 para 
execucao de programas via rpc*/
def temp-table tt-control-prog NO-UNDO
    field cod-versao-integracao as integer       format '999'
    field cod-erro              as integer       format '99999'
    field desc-erro             as character     format 'x(60)'
    field wgh-servid-rpc        as widget-handle format '>>>>>>9'.
 
 
/*fim alteracao Anderson 04/02/2003*/

/* alteraá∆o feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */

DEFINE NEW GLOBAL SHARED VARIABLE hWenController AS HANDLE     NO-UNDO.
/*Constante utilizada apenas para verificar se a vari†vel acima foi definida */ 

/* fim da alateraá∆o */

/* Alteraá∆o realizada por tech38629 - 19/07/2006 - Definiá∆o do pre-processador para o facelift */
/****************************************************************/
/* i_fclpreproc.i                                               */
/* Criado por: tech38629                                        */
/* Data de criaá∆o: 19/07/2006                                  */
/* Descriá∆o: Define o prÇ-processador que indica a utilizaá∆o  */
/*            do facelift                                       */
/****************************************************************/

 
/* Fim da alteraá∆o */

   /*** 010002 ***/
/*****************************************************************************
**
**   ESIN0803a.p - Rotina de Gravacao temp-table t-real.
**
******************************************************************************/

def var i-emit-sal               like movto-apr.cod-emitente no-undo.

def var c-item-generico   like ord-ped.it-codigo no-undo.
def var de-tot-realizado  as dec format "->>>>>>>>>>>,>>9.99" no-undo.
def var de-tot-pago       as dec format "->>>>>>>>>>>,>>9.99" no-undo.
def var de-tot-adiant     as dec format "->>>>>>>>>>>,>>9.99" no-undo.
def var de-vl-compromis   as dec format "->>>>,>>>,>>>,>>9.99" no-undo.
def var de-vl-real-acum   as dec format "->>>>,>>>,>>>,>>9.99" no-undo.
def var de-vl-real-mes    as dec format "->>>>,>>>,>>>,>>9.99" no-undo.
def var de-vl-real-ano    as dec format "->>>>,>>>,>>>,>>9.99" no-undo.

/* Definicao temp-table tt-param */
/* Definicao temp-table tt-param */
define temp-table tt-param
   field destino          as integer
   field arquivo          as character format "x(50)"
   field usuario          as character format "x(12)"
   field data-exec        as date
   field hora-exec        as integer
   field classifica       as integer
   field desc-classifica  as character format "x(40)"
   field c-est-ini        as character 
   field c-est-fim        as character 
   field i-pro-ini        as integer   
   field i-pro-fim        as integer   
   field i-sig-ini        as character 
   field i-sig-fim        as character 
   field i-ord-ini        as integer   
   field i-ord-fim        as integer   
   field i-sec-ini        as integer   
   field i-sec-fim        as integer   
   field i-esp-ini        as integer   
   field i-esp-fim        as integer   
   field i-sub-ini        as integer   
   field i-sub-fim        as integer   
   field i-ori-ini        as integer 
   field i-ori-fim        as integer 
   field i-emp-ini        as CHAR 
   field i-emp-fim        as CHAR
   FIELD d-data-ini       AS DATE
   FIELD d-data-fim       AS DATE
   field i-nivel          as int 
   field i-imob           as int 
   field i-moeda          as int 
   field i-mes            as integer  
   field i-ano            as integer
   field da-data-base     as date   
   field l-divmil         as logical.  

def temp-table t-valor 
    field tipo-reg         like base-mensal.tipo-reg
    field tipo-trans       like movto-nf.tipo-trans
    field cod-emitente     like base-mensal.cod-emitente
    field mes              as integer format "99"
    field ano              like base-mensal.ano
    field valor            like base-mensal.vl-compromis extent 0.

def temp-table t-real
    field tipo             as int format "99"
    field detalhe          as int format "9"
    field it-codigo        like plano-aprov.it-codigo
    field cod-emitente     like base-mensal.cod-emitente
    field documento        as char format "x(16)"
    field desc-docto       as char format "x(40)"
    field vl-compromis     like base-mensal.vl-compromis extent 0
    field vl-real-mes      like base-mensal.vl-realizado extent 0
    field vl-real-ano      like base-mensal.vl-realizado extent 0
    field vl-real-acum     like base-mensal.vl-realizado extent 0.

def input parameter r-registro as rowid.
def input parameter table for tt-param.
def input-output parameter table for t-real .

def var c-lb-nota  as char format "x(15)" no-undo.
def var c-lb-pagto as char format "x(15)" no-undo.
def var c-lb-ant   as char format "x(15)" no-undo.

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "NOTA_FISCAL",
                    input "min",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-nota = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "PAGAMENTO",
                    input "min",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-pagto = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "ANTECIPAÄ«O",
                    input "min",
                    input "r") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-lb-ant = trim(return-value).


find first tt-param no-lock no-error.

find ord-ped 
    where rowid(ord-ped) = r-registro no-lock no-error.

    assign c-item-generico = ord-ped.char-1.

    if c-item-generico = "" then
        find first plano-aprov 
            where plano-aprov.ep-codigo      = ord-ped.ep-codigo 
            and   plano-aprov.num-ord-magnus = ord-ped.num-ord-magnus
            and   plano-aprov.it-codigo      = ord-ped.it-codigo
            and   plano-aprov.seq-planej     = ord-ped.seq-planej
            no-lock no-error.
    else
        find first plano-aprov 
            where plano-aprov.ep-codigo      = ord-ped.ep-codigo 
            and   plano-aprov.num-ord-magnus = ord-ped.num-ord-magnus
            and   plano-aprov.it-codigo      = c-item-generico
            and   plano-aprov.seq-planej     = ord-ped.seq-planej
            no-lock no-error.

    for each t-valor:
        delete t-valor.
    end.

    create t-valor.
    assign t-valor.tipo-reg     = "91"
           t-valor.tipo-trans   = 1
           t-valor.cod-emitente = ord-ped.cod-emitente
           t-valor.mes          = month(ord-ped.dt-pedido)
           t-valor.ano          = year(ord-ped.dt-pedido)
           t-valor.valor        = ord-ped.vl-item[tt-param.i-moeda].

    for each movto-nf 
        where movto-nf.ep-codigo      = ord-ped.ep-codigo 
        and   movto-nf.num-ord-magnus = ord-ped.num-ord-magnus
        and   movto-nf.num-pedido     = ord-ped.num-pedido
        and   movto-nf.seq-pedido     = ord-ped.seq-pedido
        and   movto-nf.cod-area       = ord-ped.cod-area
        and   movto-nf.num-ord-comp   = ord-ped.num-ord-comp
        and   movto-nf.seq-comp       = ord-ped.seq-comp
        no-lock:

        create t-valor.
        assign t-valor.tipo-reg     = "95"
               t-valor.tipo-trans   = movto-nf.tipo-trans
               t-valor.cod-emitente = movto-nf.cod-emitente
               t-valor.mes          = month(movto-nf.dt-trans)
               t-valor.ano          = year(movto-nf.dt-trans)
               t-valor.valor        = movto-nf.vl-nota[tt-param.i-moeda].
        if movto-nf.tipo-trans = 1 then
            assign de-tot-realizado = 
                   de-tot-realizado + t-valor.valor.
        else
            assign de-tot-realizado =
                   de-tot-realizado - t-valor.valor.

        if tt-param.i-nivel = 9 then do:
            create t-real.
            assign t-real.it-codigo    = plano-aprov.it-codigo when avail plano-aprov
                   t-real.cod-emitente = movto-nf.cod-emitente
                   t-real.documento    = "NF " +
                                       string(movto-nf.nro-docto, "x(7)")
                   t-real.desc-docto   = trim(c-lb-nota) + " " +
                                       string(movto-nf.dt-trans, "99/99/9999")
                   t-real.tipo         = 08
                   t-real.detalhe      = 1
                   t-real.vl-compromis = 0
                   t-real.vl-real-acum = t-valor.valor.
            if t-valor.mes = tt-param.i-mes and
               t-valor.ano = i-ano then
                assign t-real.vl-real-mes = t-valor.valor.
            else
                assign t-real.vl-real-mes = 0.
            if t-valor.ano = i-ano then
                assign t-real.vl-real-ano = t-valor.valor.
            else
                assign t-real.vl-real-ano = 0.
            if not movto-nf.tipo-trans = 1 then
                assign t-real.vl-real-acum = t-real.vl-real-acum * -1
                       t-real.vl-real-mes  = t-real.vl-real-mes  * -1
                       t-real.vl-real-ano  = t-real.vl-real-ano  * -1.                       
            
        end.

    end.

    for each movto-apr use-index emp-est-ordm
        where movto-apr.ep-codigo         = ord-ped.ep-codigo
        and   movto-apr.num-ord-magnus    = ord-ped.num-ord-magnus
        and   movto-apr.num-pedido        = ord-ped.num-pedido
        and   movto-apr.int-1             = ord-ped.num-ord-comp        
        no-lock:

        if movto-apr.num-pedido <> 0 
            and movto-apr.transacao = "IMD" then do:

            create t-valor.
            assign t-valor.tipo-reg     = "97"
                   t-valor.tipo-trans   = movto-apr.tipo-trans
                   t-valor.cod-emitente = movto-apr.cod-emitente
                   t-valor.mes          = month(movto-apr.dt-trans)
                   t-valor.ano          = year(movto-apr.dt-trans).
            assign t-valor.valor        =
                   movto-apr.vl-mat[tt-param.i-moeda] +
                   movto-apr.vl-mob[tt-param.i-moeda].
            if movto-apr.tipo-trans = 1 then 
                assign de-tot-adiant  = 
                       de-tot-adiant + 
                       t-valor.valor.
            else 
                assign de-tot-adiant  = 
                       de-tot-adiant  - 
                       t-valor.valor.

            if tt-param.i-nivel = 9 then do:                
                create t-real.
                assign t-real.it-codigo    = plano-aprov.it-codigo when avail plano-aprov
                       t-real.cod-emitente = movto-apr.cod-emitente
                       t-real.documento    = 
                       string(movto-apr.esp-docto, "xxx") + " " +
                       string(movto-apr.cod-estabel,"xxx") + " " +
                       string(movto-apr.nro-docto, "x(10)")
                       t-real.desc-docto   = trim(c-lb-ant)
                       t-real.tipo         = 08
                       t-real.detalhe      = 1
                       t-real.vl-compromis = 0
                       t-real.vl-real-acum = t-valor.valor.
                if t-valor.mes = i-mes and
                   t-valor.ano = i-ano then
                    assign t-real.vl-real-mes = t-valor.valor.
                else
                    assign t-real.vl-real-mes = 0.
                if t-valor.ano = i-ano then
                    assign t-real.vl-real-ano = t-valor.valor.
                else
                    assign t-real.vl-real-ano = 0.
                if not movto-apr.tipo-trans = 1 then
                    assign t-real.vl-real-acum = t-real.vl-real-acum * -1
                           t-real.vl-real-mes  = t-real.vl-real-mes  * -1
                           t-real.vl-real-ano  = t-real.vl-real-ano  * -1.                
            end.
        end.
        else do:

            create t-valor.
            assign t-valor.tipo-reg     = "98"
                   t-valor.tipo-trans   = movto-apr.tipo-trans
                   t-valor.cod-emitente = movto-apr.cod-emitente
                   t-valor.mes          = month(movto-apr.dt-trans)
                   t-valor.ano          = year(movto-apr.dt-trans).
            assign t-valor.valor        =
                   movto-apr.vl-mat[tt-param.i-moeda] +
                   movto-apr.vl-mob[tt-param.i-moeda].
            if movto-apr.tipo-trans = 1 then 
                assign de-tot-pago = de-tot-pago + t-valor.valor.
            else 
                assign de-tot-pago = de-tot-pago - t-valor.valor.
         
            if tt-param.i-nivel = 9 then do:
                create t-real.
                assign t-real.it-codigo    = plano-aprov.it-codigo when avail plano-aprov
                       t-real.cod-emitente = movto-apr.cod-emitente
                       t-real.documento    = 
                       string(movto-apr.esp-docto, "xxx") + " " +
                       string(movto-apr.cod-estabel,"xxx") + " " +
                       string(movto-apr.nro-docto, "x(10)")
                       t-real.desc-docto   = trim(c-lb-pagto)
                       t-real.tipo         = 08
                       t-real.detalhe      = 1
                       t-real.vl-compromis = 0
                       t-real.vl-real-acum = t-valor.valor.
                if t-valor.mes = i-mes and
                   t-valor.ano = i-ano then
                    assign t-real.vl-real-mes = t-valor.valor.
                else
                    assign t-real.vl-real-mes = 0.
                if t-valor.ano = i-ano then
                    assign t-real.vl-real-ano = t-valor.valor.
                else
                    assign t-real.vl-real-ano = 0.
                if not movto-apr.tipo-trans = 1 then
                    assign t-real.vl-real-acum = t-real.vl-real-acum * -1
                           t-real.vl-real-mes  = t-real.vl-real-mes  * -1
                           t-real.vl-real-ano  = t-real.vl-real-ano  * -1.                
            end.
        end.
    end.

    assign i-emit-sal = 0.

    assign de-vl-compromis = 0
           de-vl-real-mes  = 0
           de-vl-real-ano  = 0
           de-vl-real-acum = 0.

    for each t-valor
        break by t-valor.cod-emitente
              by t-valor.tipo-reg:
        if i-emit-sal <> 0 and
           i-emit-sal <> t-valor.cod-emitente
        then do:

            run grava-real.

        end.
            
        assign i-emit-sal = t-valor.cod-emitente.

        if t-valor.tipo-reg = "91" then do:

            if t-valor.tipo-trans = 1 then
                assign de-vl-compromis = de-vl-compromis + t-valor.valor.
            else
                assign de-vl-compromis = de-vl-compromis + t-valor.valor.
        end.

        if t-valor.tipo-reg = "95" then do:

            if ord-ped.cod-sit-comp = "P" or
               de-tot-realizado <= (de-tot-adiant + de-tot-pago)
            then             
                next.

            if t-valor.tipo-trans = 1 then do:
                if t-valor.valor <= de-tot-adiant then do:
                    assign de-tot-adiant = 
                           de-tot-adiant - t-valor.valor.
                    next.
                end.
                else do:
                    assign t-valor.valor = 
                           t-valor.valor - de-tot-adiant
                           de-tot-adiant = 0.
                end.

                if t-valor.valor <= de-tot-pago then do:
                    assign de-tot-pago = 
                           de-tot-pago - t-valor.valor.
                    next.
                end.
                else do:
                    assign t-valor.valor = 
                           t-valor.valor - de-tot-pago
                           de-tot-pago   = 0.
                end.

                assign de-vl-real-acum =
                       de-vl-real-acum + t-valor.valor.

                if t-valor.mes = i-mes and
                   t-valor.ano = i-ano then
                    assign de-vl-real-mes =
                           de-vl-real-mes + t-valor.valor.
                if t-valor.ano = i-ano then
                    assign de-vl-real-ano =
                           de-vl-real-ano + t-valor.valor.

            end.
            else do:
                    
                assign de-vl-real-acum =
                       de-vl-real-acum - t-valor.valor.

                if t-valor.mes = i-mes and
                   t-valor.ano = i-ano then
                    assign de-vl-real-mes =
                           de-vl-real-mes - t-valor.valor.
                if t-valor.ano = i-ano then
                    assign de-vl-real-ano =
                           de-vl-real-ano - t-valor.valor.
            end.

        end.

        if t-valor.tipo-reg = "97" or
           t-valor.tipo-reg = "98" then do:

            if t-valor.tipo-trans = 1 then do:
                assign de-vl-real-acum =
                       de-vl-real-acum + t-valor.valor.
                if t-valor.mes = i-mes and
                   t-valor.ano = i-ano then
                    assign de-vl-real-mes =
                           de-vl-real-mes + t-valor.valor.
                if t-valor.ano = i-ano then
                    assign de-vl-real-ano =
                           de-vl-real-ano + t-valor.valor.
            end.
            else do:
                assign de-vl-real-acum =
                       de-vl-real-acum - t-valor.valor.
                if t-valor.mes = i-mes and
                   t-valor.ano = i-ano then
                    assign de-vl-real-mes =
                           de-vl-real-mes - t-valor.valor.
                if t-valor.ano = i-ano then
                    assign de-vl-real-ano =
                           de-vl-real-ano - t-valor.valor.
            end.
        end.
    end.

    run grava-real.

procedure grava-real.

    create t-real.
    assign t-real.it-codigo    = plano-aprov.it-codigo when avail plano-aprov
           t-real.cod-emitente = i-emit-sal
           t-real.documento    = ""
           t-real.desc-docto   = ""
           t-real.tipo         = 08
           t-real.detalhe      = 0
           t-real.vl-compromis = de-vl-compromis
           t-real.vl-real-mes  = de-vl-real-mes
           t-real.vl-real-ano  = de-vl-real-ano
           t-real.vl-real-acum = de-vl-real-acum.

    assign de-vl-compromis = 0
           de-vl-real-mes  = 0
           de-vl-real-ano  = 0
           de-vl-real-acum = 0.

end procedure.


/* fim programa  */

