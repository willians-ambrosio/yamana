/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa         for ems2cadme.empresa.

{include/i-prgvrs.i IN2302A 2.00.00.009}  /*** 010009 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i IN2302A MIN}
&ENDIF


/*******************************************************************************
**
**   IN2302A.P - Gera arquivo movto-apr. Chamado nos programas:
**
**                                      - in2302.
** versao 2.00.00.009 - 11/03/09 - Samila e Tatiana
*******************************************************************************/

def shared var i-emp-inv like empresa.ep-codigo no-undo.
def shared var i-ord-inv as int format ">>>,>>9" no-undo.

{cep/ceapi001.i} /* Definicao da tabela tt-movto */
DEFINE INPUT PARAMETER TABLE FOR tt-movto.

def var de-valor-realiz as decimal no-undo.
def var de-valor-comp   as decimal no-undo.

/**** Multiplanta ****/
def var i-tipo-movto as integer no-undo.
{utp/ut-glob.i}
{cdp/cd7300.i1}
{inp/inapi228.i}
{mpp/mpapi011.i}
/******* Fim Multiplanta ******/
{cdp/cdcfgmat.i} /* include com definiá∆o das funá‰es de neg¢cio */
{cdp/cd0669.i} /* definiá∆o padr∆o tt-erro */ 

def var de-vl-mat-conv like movto-apr.vl-mat no-undo.
def var de-vl-mob-conv like movto-apr.vl-mob no-undo.
def var de-vl-mat-est  like movto-apr.vl-mat no-undo.
def var de-vl-mob-est  like movto-apr.vl-mob no-undo.
def var c-esp-docto like movto-apr.esp-docto no-undo.       
DEF VAR l-multiplanta  AS LOGICAL            NO-UNDO.

find first param-global no-lock no-error.
IF AVAIL param-global AND param-global.modulo-mp AND CONNECTED("mgmp") THEN
    ASSIGN l-multiplanta = YES.

find first param-estoq no-lock no-error.
find first param-inv where param-inv.ep-codigo = i-ep-codigo-usuario no-lock no-error.

find first tt-movto no-lock no-error.

 find first param-inv
      where param-inv.ep-codigo = i-emp-inv 
      no-lock no-error.
 
 find first sub-div-ordem use-index emp-ordm 
      where sub-div-ordem.ep-codigo      = i-emp-inv 
        and sub-div-ordem.num-ord-magnus = i-ord-inv 
        no-lock no-error.
        
 find first estabelec
      where estabelec.cod-estabel = tt-movto.cod-estabel
      no-lock no-error.
 
 assign de-valor-realiz = 0.    
 
 &if defined(bf_mat_controle_verba) &then

     if avail estabelec then do:
        if estabelec.custo-contab = 1 then do:
           assign de-valor-realiz = (tt-movto.valor-mat-m[1] +
                                     tt-movto.valor-mob-m[1] +
                                     tt-movto.valor-ggf-m[1]).
        end. 
        else do:
            if estabelec.custo-contab = 2 then
               assign de-valor-realiz = (tt-movto.valor-mat-o[1] +
                                         tt-movto.valor-mob-o[1] + 
                                         tt-movto.valor-ggf-o[1]).
            else
               assign de-valor-realiz = (tt-movto.valor-mat-p[1] +
                                         tt-movto.valor-mob-p[1] + 
                                         tt-movto.valor-ggf-p[1]).
         end.
     end.         
     
     if tt-movto.tipo-trans = 1 then do:
        assign de-valor-realiz = de-valor-realiz * -1. 
     end.
     else do: /* se o de-valor-realiz for negativo n∆o precisa validar */
          de-valor-comp = de-valor-comp * -1.
          if param-inv.contr-verba then do:
               /** valida a verba da ordem **/
/***/               run inp/inapi048.p(2,
                                  sub-div-ordem.ep-codigo,
                                  tt-movto.num-ord-inv,
                                  tt-movto.dt-trans,
                                  0,
                                  de-valor-comp,
                                  de-valor-realiz,
                                  output table tt-erro). /* controle-verba */
               find first tt-erro no-lock no-error.
               if avail tt-erro then do:
                  run cdp/cd0669.w(input table tt-erro).
                  return "NOK".
               end.
          end.   
     end.         
     /** se retorna OK ent∆o atualiza o valor compromissado **/
/***/     run inp/inapi048.p(2,
                        sub-div-ordem.ep-codigo,
                        tt-movto.num-ord-inv,
                        tt-movto.dt-trans,
                        0,
                        0,
                        de-valor-realiz,
                        output table tt-erro). /* controle-verba */
 &else    
     assign c-esp-docto = {ininc/i03in218.i 04 tt-movto.esp-docto}.          
               
    find first movto-apr where
         movto-apr.ep-codigo    = i-emp-inv             and
         movto-apr.cod-estabel  = tt-movto.cod-estabel  and
         movto-apr.esp-docto    = c-esp-docto           and
         movto-apr.cod-emitente = tt-movto.cod-emitente and
         movto-apr.serie-docto  = tt-movto.serie-docto  and
         movto-apr.nro-docto    = tt-movto.nro-docto no-lock no-error.
         
    if avail movto-apr then do:
       run utp/ut-msgs(input "show", input 19728, input "").
       return "NOK":U.
    end.                       
 &endif. 
    
create movto-apr.
assign movto-apr.ep-codigo = sub-div-ordem.ep-codigo
        movto-apr.cod-est-exec      = sub-div-ordem.cod-est-exec
        movto-apr.num-projeto       = sub-div-ordem.num-projeto
        movto-apr.num-ordem         = sub-div-ordem.num-ordem
        movto-apr.num-secao         = sub-div-ordem.num-secao
        movto-apr.cod-especialidade = sub-div-ordem.cod-especialidade
        movto-apr.cod-sub-espec     = sub-div-ordem.cod-sub-espec
        movto-apr.cod-origem        = sub-div-ordem.cod-origem
        movto-apr.num-ord-magnus    = sub-div-ordem.num-ord-magnus
        movto-apr.it-codigo         = tt-movto.it-codigo
        movto-apr.cod-estabel       = tt-movto.cod-estabel
        movto-apr.cod-emitente      = tt-movto.cod-emitente
        movto-apr.serie-docto       = tt-movto.serie-docto
        movto-apr.nro-docto         = tt-movto.nro-docto
/***/        movto-apr.esp-docto         = "ACE"
/***/        movto-apr.transacao         = "ACE"
/***/        movto-apr.tipo-trans        = if tt-movto.tipo-trans = 1 then 1 else 2
        movto-apr.dt-emiss          = TODAY
        movto-apr.dt-trans          = tt-movto.dt-trans
        movto-apr.quant-mov         = tt-movto.quantidade
        movto-apr.int-2             = tt-movto.nr-trans
        movto-apr.dt-vencto         = today
        movto-apr.dt-atualizacao    = today
        movto-apr.usuario-atu       = c-seg-usuario
        movto-apr.parcela           = string(tt-movto.num-sequen).
   
   
 if estabelec.custo-contab = 1 then
   assign de-vl-mat-est[1] = tt-movto.valor-mat-m[1] 
      de-vl-mat-est[2] = tt-movto.valor-mat-m[2] 
      de-vl-mat-est[3] = tt-movto.valor-mat-m[3]
      de-vl-mob-est[1] = tt-movto.valor-mob-m[1] + tt-movto.valor-ggf-m[1]
      de-vl-mob-est[2] = tt-movto.valor-mob-m[2] + tt-movto.valor-ggf-m[2]
      de-vl-mob-est[3] = tt-movto.valor-mob-m[3] + tt-movto.valor-ggf-m[3].
else do:
  if estabelec.custo-contab = 2  then
   assign de-vl-mat-est[1] = tt-movto.valor-mat-o[1] 
          de-vl-mat-est[2] = tt-movto.valor-mat-o[2] 
          de-vl-mat-est[3] = tt-movto.valor-mat-o[3]
          de-vl-mob-est[1] = tt-movto.valor-mob-o[1] + tt-movto.valor-ggf-o[1]
          de-vl-mob-est[2] = tt-movto.valor-mob-o[2] + tt-movto.valor-ggf-o[2]
          de-vl-mob-est[3] = tt-movto.valor-mob-o[3] + tt-movto.valor-ggf-o[3].

else       
    assign de-vl-mat-est[1] = tt-movto.valor-mat-p[1] 
           de-vl-mat-est[2] = tt-movto.valor-mat-p[2] 
           de-vl-mat-est[3] = tt-movto.valor-mat-p[3]
           de-vl-mob-est[1] = tt-movto.valor-mob-p[1] + tt-movto.valor-ggf-p[1]
           de-vl-mob-est[2] = tt-movto.valor-mob-p[2] + tt-movto.valor-ggf-p[2]
           de-vl-mob-est[3] = tt-movto.valor-mob-p[3] + tt-movto.valor-ggf-p[3].
end.          

/* Converte os valores para as moedas de investimento */
if param-inv.moeda-inv <> 0 then do:
   run cdp/cd0812.p (0,
             param-inv.moeda-inv,
             input de-vl-mat-est [1], 
             input tt-movto.dt-trans,
             output de-vl-mat-conv[1]).
   run cdp/cd0812.p (0,
              param-inv.moeda-inv,
              input de-vl-mob-est  [1], 
              input tt-movto.dt-trans,
              output de-vl-mob-conv[1]).                  
end.
else 
   assign de-vl-mat-conv[1] = de-vl-mat-est[1]
          de-vl-mob-conv[1] = de-vl-mob-est[1].

if param-inv.tem-moeda1 then do:
    run cdp/cd0812.p (0,
                  param-inv.moeda1,
                  input de-vl-mat-est [1], 
                  input tt-movto.dt-trans,
                  output de-vl-mat-conv[2]).
    run cdp/cd0812.p (0,
                  param-inv.moeda1,
                  input de-vl-mob-est [1], 
                  input tt-movto.dt-trans,
                  output de-vl-mob-conv[2]).
end.   
else
   assign de-vl-mat-conv[2] = 0
          de-vl-mob-conv[2] = 0.
  
if param-inv.tem-moeda2 then do:
    run cdp/cd0812.p (0,
                  param-inv.moeda2,
                  input de-vl-mat-est [1], 
                  input tt-movto.dt-trans,
                  output de-vl-mat-conv[3]).
    run cdp/cd0812.p (0,
                  param-inv.moeda2,
                  input de-vl-mob-est [1], 
                  input tt-movto.dt-trans,
                  output de-vl-mob-conv[3]).
    end.   
else
   assign de-vl-mat-conv[3] = 0
          de-vl-mob-conv[3] = 0.       
  
if de-vl-mat-conv[1] = ? then assign de-vl-mat-conv[1] = 0.              
if de-vl-mat-conv[2] = ? then assign de-vl-mat-conv[2] = 0.
if de-vl-mat-conv[3] = ? then assign de-vl-mat-conv[3] = 0.
if de-vl-mob-conv[1] = ? then assign de-vl-mob-conv[1] = 0.
if de-vl-mob-conv[2] = ? then assign de-vl-mob-conv[2] = 0.
if de-vl-mob-conv[3] = ? then assign de-vl-mob-conv[3] = 0.

assign movto-apr.vl-mat[1] = de-vl-mat-conv[1] 
       movto-apr.vl-mat[2] = de-vl-mat-conv[2] 
       movto-apr.vl-mat[3] = de-vl-mat-conv[3]
       movto-apr.vl-mob[1] = de-vl-mob-conv[1]
       movto-apr.vl-mob[2] = de-vl-mob-conv[2]
       movto-apr.vl-mob[3] = de-vl-mob-conv[3].               
   
if movto-apr.transacao <> "DIV" then
   run inp/inapi01c.p (rowid(movto-apr)).        
   
  
   if not avail param-global then
      find first param-global no-lock no-error.
      
   if param-global.modulo-mp then do:

     IF l-multiplanta THEN DO:
    
        /************multiplanta********************/          
         find first param-inv where param-inv.ep-codigo  = sub-div-ordem.ep-codigo NO-LOCK no-error.
         find first proj-inv where proj-inv.ep-codigo    = sub-div-ordem.ep-codigo
                           and proj-inv.cod-est-exec = sub-div-ordem.cod-est-exec
                           and proj-inv.num-projeto  = sub-div-ordem.num-projeto NO-LOCK no-error.
         assign c-transacao  = "MAT045"
            i-tipo-movto = 1.                      
        /******* Multiplanta - Gera log para o estabelecimento gestor ********/  
        assign c-estabel-dest = proj-inv.cod-est-gestor.
        run pi-cria-tt-maq-ep-est. 
        find first tt-maq-ep-est where tt-maq-ep-est.cod-estabel = c-estabel-dest no-lock no-error.
        for each tt-param-maq-ep-est:
        delete tt-param-maq-ep-est.
        end. 
        if avail tt-maq-ep-est and tt-maq-ep-est.tp-conexao <> 1 then do:
       {cdp/cd7300.i5 "001" c-transacao}
       run pi-cria-tt-dados-movto-apr(input i-tipo-movto).
       {cdp/cd7300.i6 "001" c-transacao}
        end.     
        for each tt-control-env:
       delete tt-control-env.
        end.
        /*********************************************************************/
       
        /****** Multiplanta - Gera log para o estabelecimento executor *******/ 
        if proj-inv.cod-est-exec <> c-estabel-dest then do:
        assign c-estabel-dest = proj-inv.cod-est-exec.
        run pi-cria-tt-maq-ep-est.
        if avail tt-maq-ep-est and tt-maq-ep-est.tp-conexao <> 1 then do:
             {cdp/cd7300.i5 "001" c-transacao}          
              run pi-cria-tt-dados-movto-apr(input i-tipo-movto).
             {cdp/cd7300.i6 "001" c-transacao}
        end. 
        for each tt-control-env:
            delete tt-control-env.
        end.      
         end.   
        /*********************************************************************/
      
        /******* Multiplanta - Gera log para o estabelecimento central *******/ 
        if param-inv.cod-est-central <> proj-inv.cod-est-gestor
          and param-inv.cod-est-central <> proj-inv.cod-est-exec then do:
            assign c-estabel-dest = param-inv.cod-est-central.
            run pi-cria-tt-maq-ep-est.
            if avail tt-maq-ep-est and tt-maq-ep-est.tp-conexao <> 1 then do:
                 {cdp/cd7300.i5 "001" c-transacao}          
                  run pi-cria-tt-dados-movto-apr(input i-tipo-movto).
                 {cdp/cd7300.i6 "001" c-transacao}
            end. 
            for each tt-control-env:
                delete tt-control-env.
            end.      
        end.    
        /*********************************************************************/          
        /**************Fim Multiplanta*******************/  
     END.
  
  end.
                        
                              
assign i-ord-inv = 0.               

PROCEDURE pi-cria-tt-dados-movto-apr:

define input parameter i-tipo-movto as integer no-undo.

            assign c-transacao = "MAT045".
            {cdp/cd7300.i5 "001" c-transacao}
            if param-global.modulo-mp and tt-replica-msg.log-replica-msg = yes then do:
                create tt-movto-apr-aux.
                buffer-copy movto-apr to tt-movto-apr-aux.
                create tt-dados-env.
                assign tt-dados-env.num-sequencia  = i-seq
                       tt-dados-env.cod-tipo-reg   = 122
                       tt-dados-env.ind-tipo-movto = i-tipo-movto
                       tt-dados-env.identif-msg    = string(movto-apr.ep-codigo)    + " , " +
                                                     string(movto-apr.cod-estabel)  + " , " +
                                                     string(movto-apr.esp-docto)    + " , " +
                                                     string(movto-apr.cod-emitente) + " , " +
                                                     string(movto-apr.serie-docto)  + " , " +
                                                     string(movto-apr.nro-docto).
                raw-transfer tt-movto-apr-aux to tt-dados-env.conteudo-msg.
                assign i-seq = i-seq + 1.
            end.
            
END PROCEDURE.


PROCEDURE pi-cria-tt-maq-ep-est:

find estabelec where estabelec.cod-estabel = c-estabel-dest no-lock no-error.
if avail estabelec and estabelec.cod-estabel <> " " then do: 

    create tt-param-maq-ep-est.
    assign tt-param-maq-ep-est.cod-versao-integracao = 1
           tt-param-maq-ep-est.cod-estabel = c-estabel-dest
           tt-param-maq-ep-est.opcao = 2.
           
    run mpp/mpapi011.p (input-output table tt-param-maq-ep-est,
                        input-output table tt-maq-ep-est).    
   
end.
find first tt-maq-ep-est where tt-maq-ep-est.cod-estabel = c-estabel-dest no-lock no-error.
for each tt-param-maq-ep-est:
    delete tt-param-maq-ep-est.
end. 

 
END PROCEDURE.
