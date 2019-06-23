/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESIN002RP 2.00.00.000}   /*** 010000 ***/

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field modelo           AS char format "x(35)":U
    field num-ordem-ini    like controle-inv-esp.num-ordem 
    field num-ordem-fim    like controle-inv-esp.num-ordem 
    field num-projeto-ini  like controle-inv-esp.num-projeto 
    field num-projeto-fim  like controle-inv-esp.num-projeto 
    field dt-trans-ini     like controle-inv-esp.dt-trans
    field dt-trans-fim     like controle-inv-esp.dt-trans
    /*Alterado 15/02/2005 - tech1007 - Criado campo l¢gico para verificar se o RTF foi habilitado*/
    field l-habilitaRtf    as LOG.
    /*Fim alteracao 15/02/2005*/

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id ordem.

def temp-table tt-raw-digita NO-UNDO
        field raw-digita        as raw.
        
/* recebimento de parƒmetros */
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def var c-destino as character format "x(16)":U label "Destino" no-undo.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */

/* defini‡Æo de frames do relat¢rio */

form
    controle-inv-esp.num-ord-inv  column-label "Num Ord EMS"
    controle-inv-esp.cod-est-exec column-label "Est"
    controle-inv-esp.num-projeto  column-label "Num Proj"
    controle-inv-esp.num-ordem    column-label "Ord"
    controle-inv-esp.dt-trans     column-label "Data Movto"
    controle-inv-esp.valor-origem column-label "Valor MOB"
    controle-inv-esp.narrativa    column-label "Narrativa"
with stream-io frame f-controles width 132 DOWN NO-BOX.

FIND FIRST param-global NO-LOCK NO-ERROR.
/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}
{include/i-rpcab.i}

{utp/ut-liter.i "Apontamento_MOB" "min" "C"}
 
assign c-programa = 'ESIN002'
       c-sistema  = return-value
       c-destino  = /******************************************************************
**
** var00002.i  Vari vel: Destino - Template de Relat¢rios
**
******************************************************************/

 






/***************************************************************************
**  ind01-10.i - define as funcoes de um indicador
**  Para indicadores de 1 a 10 items
**
**  Funcoes disponiveis
**  01: view-as Combo-box
**  02: view-as radio-set
**  03: lista com os itens separados por virgula
**  04 n: retorna o item n da lista
**  05: retorna o numero de items da lista
**  06: retorna a posicao do item (numero)
**  07: valores para a propriedade Radio-Buttons de um Radio-Set
***************************************************************************/

/* verifica parametros ****************************************************/




/* &if lookup("{1}", "01,02,03,04,05,06,07") = 0 &then
    &message *** ({&file-name}): Parametro incorreto: {1} !
    &message *** Deveria ser: 01, 02, 03, 04, 05, 06, 07 
&endif  */
    

  


/* monta lista de items para LISTA (03), NUM (04), ITEM(05), IND(06) ************************/

          
               
     
               
     
     
     
     
     
     
     


/* funcao Combo-box (01) *************************************************************/


/* funcao Radio-set (02) *************************************************************/


/* funcao Lista (03) **********************************************************/


/* funcao NUM (05) ************************************************************/


/* funcao Item n (04) *********************************************************/    


     entry(tt-param.destino, "Impressora,Arquivo,Terminal")



/* funcao IND string (06) ****************************************************/



/* valores para a propriedade Radio-Buttons de um Radio-Set *******************/




    

    

    

    








/* fim */
 

/* fim */
 .

view frame f-cabec.
view frame f-rodape.

/*****/

for each  controle-inv-esp no-lock
    where controle-inv-esp.num-ordem   >= tt-param.num-ordem-ini
    and   controle-inv-esp.num-ordem   <= tt-param.num-ordem-fim
    and   controle-inv-esp.num-projeto >= tt-param.num-projeto-ini
    and   controle-inv-esp.num-projeto <= tt-param.num-projeto-fim
    and   controle-inv-esp.dt-trans    >= tt-param.dt-trans-ini
    and   controle-inv-esp.dt-trans    <= tt-param.dt-trans-fim:

    disp controle-inv-esp.num-ord-inv
         controle-inv-esp.cod-est-exec
         controle-inv-esp.num-projeto
         controle-inv-esp.num-ordem
         controle-inv-esp.dt-trans
         controle-inv-esp.valor-origem
         controle-inv-esp.narrativa format "x(71)"
        with frame f-controles.
    down with frame f-controles.
end.

/****/


/* fechamento do output do relat¢rio */
{include/i-rpclo.i}
 

return "OK":U.
