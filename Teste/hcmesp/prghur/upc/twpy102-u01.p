/*****************************************************************************************
***
**       Programa: prghur/upc/twpy102-u01.p
**
**       Data....: Out/2018
**
**       Cliente.: YAMANA
**
**       Autor...: Willians Ambrosio - Grupo DKP
**
**       Objetivo: Envio de Email x Altera‡Æo de Cargo
******************************************************************************************/
{include/buffers_rh.i}

{include/i-prgvrs.i twpy102-u01 12.1.21.001}


def parameter buffer p-table     for histor_sal_func.
def parameter buffer p-old-table for histor_sal_func.

def new global shared var v_cod_usuar_corren  like usuar_mestre.cod_usuario no-undo.

DEFINE VARIABLE c-cpf             AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE l-achou-cta       AS LOGICAL                   NO-UNDO.
DEFINE VARIABLE c-mensagem        AS CHARACTER FORMAT  "X(45)" NO-UNDO.
DEFINE VARIABLE c-descricao-cargo AS CHARACTER                 NO-UNDO.

IF (p-table.cdn_cargo_basic = p-old-table.cdn_cargo_basic) OR
    p-table.cdn_cargo_basic = 0 THEN
   RETURN "OK".

RUN pi-confirma.

RETURN "OK".

PROCEDURE pi-confirma:
   RUN utp/ut-msgs (INPUT "show",
                    INPUT 27100,
                    INPUT "Confirma Alterar o Cargo do Funcion rio?~~ATEN€ÇO!!!" + CHR(13) + CHR(13) + "Ser  encaminhado um e-mail para o setor de T.I.").
   IF RETURN-VALUE = "YES" THEN
      RUN pi-prepara-email.
/*    ELSE               */
/*       RETURN "NOK":u. */
END PROCEDURE.

PROCEDURE pi-prepara-email:
   blk:
   FOR FIRST funcionario WHERE
             funcionario.cdn_empresa     = p-table.cdn_empresa     AND
             funcionario.cdn_estab       = p-table.cdn_estab       AND
             funcionario.cdn_funcionario = p-table.cdn_funcionario NO-LOCK:
      /* --------------------------------------------------------- */
      RUN pi-mensagem-conta.
      /* --------------------------------------------------------- */
      RUN prghur\upc\twpy102-u02.p (INPUT ROWID(funcionario), INPUT c-mensagem).
      
      ASSIGN c-mensagem = "".
      /* --------------------------------------------------------- */
   END.
END PROCEDURE.

PROCEDURE pi-mensagem-conta:
   /*-----------------------------------------------------------------------*/ 
   ASSIGN c-cpf      = "" 
          l-achou-cta = NO.
   /*-----------------------------------------------------------------------*/ 
   RUN pi-busca-cpf(OUTPUT c-cpf).  
   /*-----------------------------------------------------------------------*/ 
   ASSIGN l-achou-cta = YES.
   /*-----------------------------------------------------------------------*/ 
   DEFINE BUTTON db-bt-cancel AUTO-END-KEY LABEL "&Fechar" SIZE 10 BY 1 BGCOLOR 8.

   DEFINE RECTANGLE db-rt-botoes EDGE-PIXELS 2 GRAPHIC-EDGE  SIZE 59 BY 1.42 BGCOLOR 7.   



   DEFINE RECTANGLE db-rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 59 BY 5.00.

   DEFINE FRAME db-frame-1
          c-mensagem NO-LABEL VIEW-AS EDITOR  SIZE 55 BY 4
                            AT ROW 1.7 col 2 NO-TAB-STOP FONT 0
          db-rect-1         AT ROW 1.3 COL 01
          db-bt-cancel      AT ROW 6.7 COL 23             
          db-rt-botoes      AT ROW 6.5 COL 1
          SPACE(0.28)
          WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE  THREE-D SCROLLABLE TITLE "* * * Altera‡Æo de Cargo! * * *" FONT 1 DEFAULT-BUTTON db-bt-cancel CANCEL-BUTTON db-bt-cancel.

   ASSIGN c-descricao-cargo = "".

   FIND FIRST cargo WHERE
              cargo.cdn_cargo_basic          = p-table.cdn_cargo_basic       and
              cargo.cdn_niv_cargo            = p-table.cdn_niv_cargo         and
              cargo.idi_tip_cargo_funcao     = p-table.idi_tip_cargo_funcao  NO-LOCK NO-ERROR.
   IF NOT AVAIL cargo THEN
      FIND FIRST cargo WHERE
                 cargo.cdn_cargo_basic      = p-table.cdn_cargo_basic       and
                 cargo.cdn_niv_cargo        = p-table.cdn_niv_cargo         and
                 cargo.idi_tip_cargo_funcao = 1                             NO-LOCK NO-ERROR.
   IF AVAIL cargo THEN
   DO:   
      ASSIGN c-descricao-cargo = cargo.des_cargo.
      RELEASE cargo.
   END.

   ASSIGN c-mensagem = "Empresa: "          + STRING(p-table.cdn_empresa )                                                + CHR(10) +  
                       "Estabelecimento: "  + STRING(p-table.cdn_estab   )                                                + CHR(10) + 
                       "Matr: "             + STRING(funcionario.cdn_funcionario) + " - " + funcionario.nom_pessoa_fisic  + CHR(10) + 
                       "CPF: "              + STRING(c-cpf)                                                               + CHR(10) +                         
                       "Novo Cargo:  "      + STRING(p-table.cdn_cargo_basic)     + " - " + STRING(p-table.cdn_niv_cargo)     + " / " + c-descricao-cargo.

   DISPLAY c-mensagem WITH FRAME db-frame-1.

   ASSIGN c-mensagem:SENSITIVE   = YES
          c-mensagem:READ-ONLY   = YES
          c-mensagem:AUTO-RESIZE = YES.

   ENABLE db-bt-cancel  WITH FRAME db-frame-1. 

   WAIT-FOR "GO":U OF FRAME db-frame-1.
   /*-----------------------------------------------------------------------*/
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
