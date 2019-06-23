/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_rh.i}

{include/i-prgvrs.i twpy151ymep 12.1.13.001}

/*****************************************************************************************
***
**       Programa: prghur/upc/twpy151ymep.p
**
**       Data....: Maio 2017
**
**       Cliente.: YAMANA
**
**       Objetivo: Criar relacionamentos da tabela habilit_rescis
******************************************************************************************/

def parameter buffer p-table     for habilit_rescis.
def parameter buffer p-old-table for habilit_rescis.

DEFINE BUFFER b1-funcionario FOR funcionario.

DEFINE VARIABLE c-cpf       LIKE rh_pessoa_fisic.cod_id_feder NO-UNDO.
DEFINE VARIABLE l-achou-cta AS LOG                            NO-UNDO.

def new global shared var v_cod_usuar_corren  like usuar_mestre.cod_usuario no-undo.

IF NEW(p-table) THEN DO:
   IF p-table.idi_tip_rescis_func = 1 /*efetivo*/ THEN DO:
      RUN pi-confirma.
   END.
END.
ELSE DO:
   IF (p-old-table.idi_tip_rescis_func <> p-table.idi_tip_rescis_func) AND p-table.idi_tip_rescis_func = 1 /*efetivo*/ THEN DO:
      RUN pi-confirma.
   END.
END.

PROCEDURE pi-confirma:
   RUN utp/ut-msgs (INPUT "show",
                    INPUT 27100,
                    INPUT "Confirma Desligamento do Funcion†rio?~~ATENÄ«O ! !!" + CHR(13) + CHR(13) + "Ser† encaminhado um e-mail para o setor de T.I eliminando todos os acessos do colaborador em quest∆o.").
   IF RETURN-VALUE = "YES" THEN
      RUN pi-cria-tabela.
   ELSE
      RETURN "NOK":u.
END PROCEDURE.

PROCEDURE pi-cria-tabela:
   blk:
   FOR EACH funcionario 
            WHERE funcionario.cdn_empresa     =  p-table.cdn_empresa     AND
                  funcionario.cdn_estab       =  p-table.cdn_estab       AND
                  funcionario.cdn_funcionario =  p-table.cdn_funcionario
            NO-LOCK:
      FIND FIRST func_desligto                                                                        
           WHERE func_desligto.cdn_empresa     = funcionario.cdn_empresa     AND                                            
                 func_desligto.cdn_estab       = funcionario.cdn_estab       AND                                           
                 func_desligto.cdn_funcionario = funcionario.cdn_funcionario
           EXCLUSIVE-LOCK NO-ERROR.                                                        
      IF NOT AVAILABLE(func_desligto) THEN DO:  
         CREATE func_desligto.                                           
         ASSIGN func_desligto.cdn_empresa     = funcionario.cdn_empresa    
                func_desligto.cdn_estab       = funcionario.cdn_estab      
                func_desligto.cdn_funcionario = funcionario.cdn_funcionario.
      END.

      RUN pi-mensagem-conta.

      ASSIGN func_desligto.dat_desligto = p-table.dat_desligto_func
             func_desligto.existe-cta   = l-achou-cta
             func_desligto.usuar_alter  = v_cod_usuar_corren.
      
      RUN prghur\upc\twpy151ymep-01.p (INPUT ROWID(funcionario),
                                       INPUT ROWID(func_desligto)).

      RETURN "OK":U.
   END.

   
END PROCEDURE.

PROCEDURE pi-mensagem-conta:
   ASSIGN c-cpf      = "" 
          l-achou-cta = NO.

   RUN pi-busca-cpf(OUTPUT c-cpf).  

   FIND FIRST b1-funcionario 
        WHERE b1-funcionario.cdn_cta_corren     =  funcionario.cdn_cta_corren     AND
              b1-funcionario.cdn_bco_liq        =  funcionario.cdn_bco_liq        AND
              b1-funcionario.cdn_agenc_bcia_liq =  funcionario.cdn_agenc_bcia_liq AND
              b1-funcionario.cod_id_feder       <> funcionario.cod_id_feder
        NO-LOCK NO-ERROR.
   IF AVAILABLE(b1-funcionario) THEN DO:
      /*-----------------------------------------------------------------------*/ 
      ASSIGN l-achou-cta = YES.
      /*-----------------------------------------------------------------------*/ 
      DEFINE BUTTON db-bt-cancel AUTO-END-KEY LABEL "&Fechar" SIZE 10 BY 1 BGCOLOR 8.

      DEFINE RECTANGLE db-rt-botoes EDGE-PIXELS 2 GRAPHIC-EDGE  SIZE 58 BY 1.42 BGCOLOR 7.  

      DEFINE VARIABLE c-mensagem AS CHARACTER FORMAT  "X(45)" NO-UNDO.

      DEFINE RECTANGLE db-rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 59 BY 4.30.

      DEFINE FRAME db-frame-1
             c-mensagem NO-LABEL VIEW-AS EDITOR  SIZE 55 BY 3
                at ROW 2.7 col 2 NO-TAB-STOP FONT 0
             db-rect-1 AT ROW 1.9 COL 01
             db-bt-cancel      AT ROW 7.3 COL 23             
             db-rt-botoes      AT ROW 7.0 COL 1
             SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE  THREE-D SCROLLABLE TITLE "* * * Conta corrente Duplicada! * * *" FONT 1 DEFAULT-BUTTON db-bt-cancel CANCEL-BUTTON db-bt-cancel.

      ASSIGN c-mensagem = "Matr: " + STRING(funcionario.cdn_funcionario) + " - " +
                          funcionario.nom_pessoa_fisic    + CHR(10) + 
                          "Ctps: "+ funcionario.cod_cart_trab + "  Serie: " + 
                          funcionario.cod_ser_cart_trab + CHR(10) + 
                          "Pis: " + funcionario.cod_pis + CHR(10) + 
                          "Banco: " + string(funcionario.cdn_bco_liq) + 
                          "  Agencia: " + STRING(funcionario.cdn_agenc_bcia_liq) + 
                          "  Conta: "+ string(funcionario.cdn_cta_corren) + "-" +
                          STRING(funcionario.cod_digito_cta_corren)  .

      DISPLAY c-mensagem WITH FRAME db-frame-1.

      ASSIGN c-mensagem:SENSITIVE = YES
             c-mensagem:read-only = yes
             c-mensagem:AUTO-RESIZE = yes.

      ENABLE db-bt-cancel  WITH FRAME db-frame-1. 

      WAIT-FOR "GO":U OF FRAME db-frame-1.
      /*-----------------------------------------------------------------------*/
    END.
END PROCEDURE.

PROCEDURE pi-busca-cpf:
   DEFINE OUTPUT PARAMETER p_cpf LIKE rh_pessoa_fisic.cod_id_feder NO-UNDO.
   /*------------------------------------------------------------------------*/
   IF AVAILABLE(funcionario) THEN DO:
      FIND FIRST rh_pessoa_fisic 
           WHERE rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic
           NO-LOCK NO-ERROR.
      IF AVAILABLE(rh_pessoa_fisic) THEN
         ASSIGN p_cpf = rh_pessoa_fisic.cod_id_feder.
   END.  
   /*------------------------------------------------------------------------*/
END PROCEDURE.

/*                                                                                                                                                                   */
/* find first func_desligto exclusive-lock                                                                                                                           */
/*     where func_desligto.cdn_empresa     =  p-table.cdn_empresa                                                                                           */
/*       and func_desligto.cdn_estab       =  p-table.cdn_estab                                                                                             */
/*       and func_desligto.cdn_funcionario =  p-table.cdn_funcionario                                                                                       */
/*       and func_desligto.dat_desligto    <> ? no-error.                                                                                                            */
/* IF NOT AVAIL func_desligto THEN DO:                                                                                                                               */
/*     run utp/ut-msgs.p (input "show",                                                                                                                              */
/*                        input 17006,                                                                                                                               */
/*                        input 'Falta Informar Data Desligto!~~ Favor informar a "Data Desligto" no programa de cadastro de funcionario(fp1500) aba "Cadastral".'). */
/*     RETURN "nok".                                                                                                                                                 */
/* END.                                                                                                                                                              */
