/*****************************************************************************
*   Programa: es0007rp.p
*   Data....: Out/05
*   Autor...: Fl†vio Capitanio (11) 9756-8761 AFVIEW
*   Cliente : Fazenda Brasileiro
*   Objetivo: Tranfere eventos de insalubridade no per°odo
*   Vers∆o..: 2.08.000                            
*   OBS.....: 
*******************************************************************************/
/*---------------- Include de controle de Vers„o ------------------*/ 
{include/buffers_RH.i}

{include/i-prgvrs.i es0007rp 2.08.00.000}

/******** DefiniÁ„o Temp-table para Recebimento de Parametro **********************/
message "Executando o programa es0007rp.p : Aguarde" view-as alert-box.

def temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param no-undo
    field destino                  as integer
    field arquivo                  as char format "x(35)"
    field usuario                  as char format "x(12)"
    field v_cdn_empres_usuar       like param_empres_rh.cdn_empresa
    field v_num_tip_aces_usuar     as integer format "9" 
    field data-exec                as date
    field hora-exec                as integer
    field classific                as integer
    field i-empresa                like param_empres_rh.cdn_empresa 
    field i-est-ini                like funcionario.cdn_estab
    field i-est-fim                like funcionario.cdn_estab
    FIELD fi-mes-ref               AS i
    FIELD fi-ano-ref               AS i
    Field evto-orig-20             As Int
    Field evto-orig-40             As Int
    FIELD insal-20                 As Int   
    FIELD insal-40                 As Int
    Field tip-calc                 As Int 
    field formato                  as integer
    field parametro                as logical.


/****************** Definiáao de Vari†veis do Relat¢rio N∆o Pedidas em Tela ******************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.


/****************** DefiniÁao de Vari·veis do RelatÛrio N„o Pedidas em Tela ******************/ 

/****************** DefiniÁao de Vari·veis de Processamento do RelatÛrio *********************/

def var h-acomp              as handle no-undo.
Def Var i-cont               As Int   No-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.
Def Var evto-insal As Int.
Def Var evto-sind            As int.
Def Var c-mens              As Char Form "X(40)" No-undo.

/****************** DefiniÁao de  Frames e Forms do RelatÛrio 132 Colunas ***************************************/ 
/*-------- include padr„o para variaveis de relatorio--------- */
  {include/i-rpvar.i}
/*------------------------------------------------------------ */

find empresa no-lock where
     empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.

FIND param_empres_rh WHERE param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar NO-LOCK NO-ERROR.

assign c-programa     = "es0007RP"
       c-versao       = "2.08"
       c-revisao      = "000"
       c-titulo-relat = "Transfere evento de insalubridade no per°odo"
       c-sistema      = "Folha de Pagamento"
       c-empresa      = empresa.razao-social.

/*---------------- Abertura do arquivo de saida ----------------------*/

/*------------ DefiniÁ„o parametro &stream para os includes
               da frame de cabeÁalho e rodape para 132 colunas ---------*/
{include/i-rpcab.i}
/*--------------------------------------------------------------------*/

/*------------ Include Padrao para output de relatorios --------------*/
{include/i-rpout.i}
/*--------------------------------------------------------------------*/

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.

{utp/ut-liter.i Imprimindo * I}

run pi-inicializar in h-acomp (input "Tranferindo Eventos de Insalubriczcd").  

assign v-num-reg-lidos = 0.
message "Lendo o Movimento" view-as alert-box.

FOR EACH funcionario where funcionario.cdn_empresa = tt-param.v_cdn_empres_usuar And
                           funcionario.cdn_estab >= tt-param.i-est-ini And
                           funcionario.cdn_estab <= tt-param.i-est-fim And 
                           funcionario.log_recebe_insal No-lock:

/*  if funcionario.dat_desligto_func <> ? then
       if funcionario.dat_desligto_func < DATE(tt-param.fi-mes-ref,01,tt-param.fi-ano-ref) THEN NEXT. */

    Find sindicato Of funcionario No-lock.
    Assign evto-insal = If sindicato.val_perc_adc_insal_niv[funcionario.num_niv_insal] = 20.00 Then
                               tt-param.insal-20
                            Else
                              If sindicato.val_perc_adc_insal_niv[funcionario.num_niv_insal] = 40.00 Then
                                 tt-param.insal-40
                              Else 0
              evto-sind  = if sindicato.cdn_efp_adc_insal > "000" Then 
                              int(sindicato.cdn_efp_adc_insal) 
                           Else
                              If sindicato.val_perc_adc_insal_niv[funcionario.num_niv_insal] = 20.00 Then
                                 tt-param.evto-orig-20
                              Else
                                 If sindicato.val_perc_adc_insal_niv[funcionario.num_niv_insal] = 40.00 Then
                                    tt-param.evto-orig-40
                                 Else 71.

    run pi-acompanhar in h-acomp(input funcionario.nom_pessoa_fisic).
    If evto-insal = 0 Then Next.
    Assign c-mens = "".
    If tt-param.tip-calc = 1 Then
    Do:
       For Each movto_calcul_func of funcionario Exclusive-lock where
                 movto_calcul_func.num_ano_refer_fp         = tt-param.fi-ano-ref and
                 movto_calcul_func.num_mes_refer_fp         = tt-param.fi-mes-ref and
                 movto_calcul_func.idi_tip_fp               = 1         and
                 movto_calcul_func.qti_parc_habilit_calc_fp = 9 :
                                         
            Do  i-cont = 1 to qti_efp :  
                if movto_calcul_func.cdn_event_fp[i-cont] = string(evto-sind,"999") then
                   assign movto_calcul_func.cdn_event_fp[i-cont] = string(evto-insal,"999")
                          c-mens       = "Movimento Folha Alterado Com Sucesso, Evento " + String(evto-sind).
            End. 
        End.    
    End.
    Else 
       Do:
         For Each det_rescis Of funcionario Exclusive-lock Where
               det_rescis.num_ano_refer_fp = tt-param.fi-ano-ref and  
               det_rescis.num_mes_refer_fp = tt-param.fi-mes-ref .    
             Do i-cont = 1 to qti_efp :  
                if det_rescis.cdn_event_fp[i-cont] = string(evto-sind,"999") then
                   assign det_rescis.cdn_event_fp[i-cont] = string(evto-insal,"999")
                          c-mens       = "Movimento Rescis∆o Alterado Com Sucesso, Evento " + String(evto-sind).
            End. 
        End.    
     End.
     
    DISP funcionario.cdn_estab 
         funcionario.cdn_funcionario
         funcionario.nom_pessoa_fisic
         c-mens
         WITH WIDTH 132 FRAME f-rel-132 DOWN STREAM-IO.
End. 

/*----------------- Fechamento do output do relatorio -----------------*/
{include/i-rpclo.i}

run pi-finalizar in h-acomp.

return "OK":U.

/* fim do programa */


