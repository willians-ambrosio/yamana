/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i CR0706F 2.00.00.007}  /*** 010007 ***/
/***************************************************************************
***                                                                      
***    Programa: crp/cr0706f.p  -  Calcula saldo atual e anterior do cliente.
***                                                                      
***************************************************************************/

/* Definicao TTs e Variaveis - Validacao Decimais - Chile */
{cdp/cd1234.i}

{utp/ut-glob.i}

def input        param i-cod-emit           like titulo.cod-emitente   no-undo.
def input        param c-cod-esp-inicial    like titulo.cod-esp        no-undo.
def input        param c-cod-esp-final      like titulo.cod-esp        no-undo.
def input        param i-cod-rep-inicial    like titulo.cod-rep        no-undo.
def input        param i-cod-rep-final      like titulo.cod-rep        no-undo.
def input        param i-cod-por-inicial    like titulo.cod-port       no-undo.
def input        param i-cod-por-final      like titulo.cod-port       no-undo.
def input        param d-dt-emissao-inicial like titulo.dt-emissao     no-undo.
def input        param d-dt-emissao-final   like titulo.dt-emissao     no-undo.
def input        param da-data-base         like titulo.dt-emissao     no-undo.
def input        param l-juros              as   logical               no-undo.
def input        param de-juros             as   dec                   no-undo.
def input        param i-op-juros           as   integer               no-undo.
def input        param l-web                as   logical               no-undo.
def input        param l-cliente            as   logical               no-undo.
def input-output param de-saldo-anterior    like titulo.vl-saldo       no-undo.
def input-output param de-total-matriz      like titulo.vl-saldo       no-undo.
def input-output param de-juros-calc        like titulo.vl-saldo       no-undo.

def var de-calc-vl-antecip as dec                  no-undo.
def var de-calc-vl-baixa   as dec                  no-undo.
def var de-calc-vl-saldo   as dec                  no-undo.
def var i-atraso           as integer format 99999 no-undo.
def var h-acomp            as handle               no-undo.

&if "{&mgadm_version}" >= "2.02" &then
  {crp/cr0501d.i4}
  {crp/cr0501d.i3 "da-data-base"
                  "de-perc-juro"
                  "i-car-juro"
                  "de-valor-min"
                  "l-gera-ad"
                  "de-perc-multa"
                  "i-car-multa"
                  "i-tp-juros"
                  "i-mo-vl-min"}
&endif

find first param-cr    
     where param-cr.ep-codigo = i-ep-codigo-usuario no-lock no-error.

find emitente where emitente.cod-emitente = i-cod-emit 
     no-lock no-error.

do on stop undo, leave:
    if l-web = no then do:
        run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Calculando_Saldo_Atual_e_Anterior *}
        run pi-inicializar in h-acomp (input  Return-value ).
    end.

    if l-cliente = yes then do:
       {crp/cr0706f.i "use-index emitente" 
                      "titulo.cod-emitente = emitente.cod-emitente"}
    end.   
    else do:
       {crp/cr0706f.i "use-index matriz" 
                      "titulo.matriz       = emitente.cod-emitente"}
    end.

    if l-web = no then do:
        run pi-finalizar in h-acomp.
    end.    
end.

/* Fim de Programa */


procedure pi-saldo-anterior:

   assign de-calc-vl-saldo   = titulo.vl-saldo.

   /*---- Saldo  Anterior ----*/

   {crp/cr0706.i "d-dt-emissao-inicial"}

end procedure.
