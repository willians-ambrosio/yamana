
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESUT0017RP 2.06.00.001 } /*** 010114 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESUT0017rp MRE}
&ENDIF

{include/i_fnctrad.i}
/********************************************************************************
** Copyright DATASUL S.A. (1998)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*********************************************************************************/
/********************************************************************************
*              
*  ESUT0017RP.P - Remote Program da Importaá∆o de Documento     
*
*********************************************************************************/
{utp/ut-glob.i}
{include/tt-edit.i}


    

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arquivo          as char
    field arq-entrada1     as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    FIELD l-atualiza       AS LOGICAL.    

def temp-table tt-raw-digita
   field raw-digita      as raw.

DEFINE TEMP-TABLE tt-dirf_apb NO-UNDO LIKE dirf_apb
       FIELDS dat_refer_old                    LIKE dirf_apb.dat_refer                  
       FIELDS val_rendto_tribut_old            LIKE dirf_apb.val_rendto_tribut          
       FIELDS val_aliq_impto_old               LIKE dirf_apb.val_aliq_impto             
       FIELDS val_acum_impto_retid_period_old  LIKE dirf_apb.val_acum_impto_retid_period
       FIELDS r-rowid                          AS ROWID
       FIELDS acao                             AS CHARACTER FORMAT "x"
       FIELDS observacao                       AS CHARACTER FORMAT "x(60)".

{cdp/cd0666.i}
{utp/ut-glob.i}
{include/i-rpvar.i}

DEFINE STREAM st-excel.
    

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

def var h-acomp            as handle  no-undo.

def var c-serie-docto      like item-doc-est.serie-docto no-undo.
def var c-nro-docto        like item-doc-est.nro-docto no-undo.
def var c-cod-emitente     like item-doc-est.cod-emitente no-undo.
def var c-nat-operacao     like item-doc-est.nat-operacao no-undo.
def var c-cod-esp          like dupli-apagar.cod-esp no-undo.
def var c-parcela          like dupli-apagar.parcela no-undo.
def var i-seq              as integ no-undo initial 0.
def var i-seq-item         as integ no-undo initial 0.
def var c-char             as char  no-undo.


form tt-erro.cd-erro "-"   
     tt-erro.mensagem format "x(117)"
     with width 132 frame f-erros stream-io. 

{utp/ut-liter.i Erro *}
assign tt-erro.cd-erro:label in frame f-erros = trim(return-value).

{utp/ut-liter.i Descriá∆o *}
assign tt-erro.mensagem:label in frame f-erros = trim(return-value).     

def stream s-entrada.
def var c-linha          as character.
def var i-cont           as integer.
def var l-cabec          as logical.
def var l-num-automatica as log initial no.
def var l-erro           as log.
def var c-text-aux       as char no-undo.
def var i-seq-aux        as int no-undo init 0.
def var l-spp-nfe        as log no-undo.

/**** Inicio ****/   


/*********************  Chamada EPC TMS ***********************/ 
FIND ems2cadme.empresa 
     WHERE empresa.ep-codigo = i-ep-codigo-usuario
     NO-LOCK NO-ERROR.

/* bloco principal do programa */
ASSIGN c-programa 	    = "ESUT0017RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.001"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "UTP"
	   c-titulo-relat   = "Alteraá∆o de DIRF".

FORM HEADER 
     FILL("-", 212)        FORMAT "x(212)" SKIP 
     c-empresa 
     c-titulo-relat AT 090
     "Pagina:":U    AT 200 
     page-number    AT 208 FORMAT ">>>>9" SKIP 
     FILL("-", 192)        FORMAT "x(190)" 
     TODAY                 FORMAT "99/99/9999"
     "-" 
     STRING(TIME,"HH:MM:SS":U) SKIP 
     "Contrato     Seq Item             Descriá∆o                                                           Preáo Unit    Preáo Unit Final          Preáo Novo Observaá∆o" SKIP
     "--------- ------ ---------------- ------------------------------------------------------------ ----------------- ------------------- ------------------- -----------------------------------------------------------" SKIP
     
WITH  STREAM-IO NO-BOX NO-LABEL OVERLAY PAGE-TOP WIDTH 212 FRAME f-cabec.

ASSIGN c-rodape = "DATASUL - " + c-sistema + " - " + c-programa + " - V:" + c-versao + "." + c-revisao
       c-rodape = FILL("-", 212 - LENGTH(c-rodape)) + c-rodape.

FORM HEADER 
     c-rodape   FORMAT "x(212)"
  WITH STREAM-IO WIDTH 212 NO-LABELS NO-BOX PAGE-BOTTOM FRAME f-rodape.
/**************************************************************/

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Alteraá∆o_Preáos_Item_Contratos *}
run pi-inicializar in h-acomp (input  Return-value ).

/* include padr“o para output de relatÆrios */
{include/i-rpout.i}

RUN pi-importa-planilha.
RUN pi-processa-registros.
RUN pi-gera-log.

{include/i-rpclo.i}

run pi-finalizar in h-acomp.

return "OK".


PROCEDURE pi-importa-planilha:
   DEFINE VARIABLE ch-excel          AS COMPONENT-HANDLE  NO-UNDO.
   DEFINE VARIABLE i-cont-linha      AS INTEGER           NO-UNDO.
   DEFINE VARIABLE i-nr-contrato     AS INTEGER           NO-UNDO.
   DEFINE VARIABLE c-des-contrat     AS CHARACTER         NO-UNDO FORMAT "x(100)".
   DEFINE VARIABLE c-ind-sit         AS CHARACTER         NO-UNDO.
                                                      
   DEFINE VARIABLE v-cod_estab                        LIKE dirf_apb.cod_estab                   NO-UNDO.
   DEFINE VARIABLE v-num_pessoa                       LIKE dirf_apb.num_pessoa                  NO-UNDO.
   DEFINE VARIABLE v-dat_refer                        LIKE dirf_apb.dat_refer                   NO-UNDO.
   DEFINE VARIABLE v-cod_pais                         LIKE dirf_apb.cod_pais                    NO-UNDO.
   DEFINE VARIABLE v-cod_unid_federac                 LIKE dirf_apb.cod_unid_federac            NO-UNDO.
   DEFINE VARIABLE v-cod_imposto                      LIKE dirf_apb.cod_imposto                 NO-UNDO.
   DEFINE VARIABLE v-cod_classif_impto                LIKE dirf_apb.cod_classif_impto           NO-UNDO.
   DEFINE VARIABLE v-cod_espec_docto                  LIKE dirf_apb.cod_espec_docto             NO-UNDO.
   DEFINE VARIABLE v-cod_ser_docto                    LIKE dirf_apb.cod_ser_docto               NO-UNDO.
   DEFINE VARIABLE v-cod_tit_ap                       LIKE dirf_apb.cod_tit_ap                  NO-UNDO.
   DEFINE VARIABLE v-cod_parcela                      LIKE dirf_apb.cod_parcela                 NO-UNDO.   
   DEFINE VARIABLE v-val_rendto_tribut_new            LIKE dirf_apb.val_rendto_tribut           NO-UNDO.
   DEFINE VARIABLE v-val_aliq_impto_new               LIKE dirf_apb.val_aliq_impto              NO-UNDO.
   DEFINE VARIABLE v-val_acum_impto_retid_period_new  LIKE dirf_apb.val_acum_impto_retid_period NO-UNDO.
   DEFINE VARIABLE v-dat_refer_new                    LIKE dirf_apb.dat_refer                   NO-UNDO.
   DEFINE VARIABLE v-acao                             AS   CHARACTER                            NO-UNDO.
   DEFINE VARIABLE i-cod_tit_ap                       AS   INTEGER                              NO-UNDO.
   

   EMPTY TEMP-TABLE tt-dirf_apb.
   
   FILE-INFO:FILE-NAME = tt-param.arq-entrada1.

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:OPEN(FILE-INFO:FULL-PATHNAME).
   ch-Excel:Visible = FALSE.

   DEFINE VARIABLE l-sai AS LOGICAL INITIAL NO.

   blk:
   DO i-cont-linha = 1 TO 1048572:
      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "rs_sea_dirf_apb:" THEN DO:
         NEXT blk.
      END.

      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Estab" THEN DO:
         NEXT blk.
      END.

      ASSIGN v-cod_estab         = STRING(INTEGER(ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE))
             v-num_pessoa        =        INTEGER(ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE) 
             v-dat_refer         =         DATE  (ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE) 
             v-cod_pais          = STRING(        ch-excel:Range( "F" + STRING(i-cont-linha,'99999')):VALUE) 
             v-cod_unid_federac  =                ch-excel:Range( "G" + STRING(i-cont-linha,'99999')):VALUE 
             v-cod_imposto       = STRING(INTEGER(ch-excel:Range( "H" + STRING(i-cont-linha,'99999')):VALUE))
             v-cod_classif_impto = STRING(INTEGER(ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE))
             v-cod_espec_docto   = STRING(        ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE) 
             v-cod_ser_docto     =                ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE 
             v-cod_tit_ap        =                ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE
             v-cod_parcela       = STRING(        ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE) 
             v-acao              = STRING(        ch-excel:Range( "U" + STRING(i-cont-linha,'99999')):VALUE).

      
      CASE v-cod_estab:
           WHEN "1" THEN ASSIGN v-cod_estab = "01".
           WHEN "2" THEN ASSIGN v-cod_estab = "02".
           WHEN "2" THEN ASSIGN v-cod_estab = "03".
      END CASE.

      ASSIGN v-val_rendto_tribut_new           = ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE
             v-val_aliq_impto_new              = ch-excel:Range( "R" + STRING(i-cont-linha,'99999')):VALUE
             v-val_acum_impto_retid_period_new = ch-excel:Range( "S" + STRING(i-cont-linha,'99999')):VALUE
             v-dat_refer_new                   = ch-excel:Range( "T" + STRING(i-cont-linha,'99999')):VALUE.

      IF v-acao = " " OR v-acao = "" OR v-acao = ? THEN
         ASSIGN l-sai = YES.

      IF l-sai THEN 
         LEAVE blk.

      IF v-acao <> "" THEN DO:
         ASSIGN v-cod_estab         = IF v-cod_estab         = ? THEN  "" ELSE v-cod_estab        
                v-cod_pais          = IF v-cod_pais          = ? THEN  "" ELSE v-cod_pais         
                v-cod_unid_federac  = IF v-cod_unid_federac  = ? THEN  "" ELSE v-cod_unid_federac 
                v-cod_imposto       = IF v-cod_imposto       = ? THEN  "" ELSE v-cod_imposto      
                v-cod_classif_impto = IF v-cod_classif_impto = ? THEN  "" ELSE v-cod_classif_impto
                v-cod_espec_docto   = IF v-cod_espec_docto   = ? THEN  "" ELSE v-cod_espec_docto  
                v-cod_ser_docto     = IF v-cod_ser_docto     = ? THEN  "" ELSE v-cod_ser_docto    
                v-cod_tit_ap        = IF v-cod_tit_ap        = ? THEN  "" ELSE v-cod_tit_ap       
                v-cod_parcela       = IF v-cod_parcela       = ? THEN  "" ELSE v-cod_parcela     
                v-acao              = IF v-acao              = ? THEN  "" ELSE v-acao.            
                                      
         RUN pi-acompanhar IN h-acomp (INPUT "Importando : " +
                                       STRING(v-cod_estab)         + "-" +
                                       STRING(v-num_pessoa)        + "-" +      
                                       STRING(v-dat_refer)         + "-" +
                                       STRING(v-cod_pais)          + "-" +
                                       STRING(v-cod_unid_federac)  + "-" +
                                       STRING(v-cod_imposto)       + "-" +
                                       STRING(v-cod_classif_impto) + "-" +
                                       STRING(v-cod_espec_docto)   + "-" +
                                       STRING(v-cod_ser_docto)     + "-" +
                                       STRING(v-cod_tit_ap)        + "-" +
                                       STRING(v-cod_parcela)).

         ASSIGN i-cod_tit_ap  = INTEGER(v-cod_tit_ap) NO-ERROR.

         IF INDEX(v-cod_parcela,',') <> 0 THEN
            ASSIGN v-cod_parcela = ENTRY(1,v-cod_parcela,',').
         
         IF INDEX(v-cod_ser_docto,',') <> 0 THEN
            ASSIGN v-cod_ser_docto = ENTRY(1,v-cod_ser_docto,',').


         

         IF ERROR-STATUS:ERROR THEN DO:
            FIND FIRST dirf_apb
                 WHERE dirf_apb.cod_estab            = v-cod_estab         AND   
                       dirf_apb.num_pessoa           = v-num_pessoa        AND   
                       dirf_apb.dat_refer            = v-dat_refer         AND   
                       dirf_apb.cod_pais             = v-cod_pais          AND   
                       dirf_apb.cod_unid_federac     = v-cod_unid_federac  AND   
                       dirf_apb.cod_imposto          = v-cod_imposto       AND   
                       dirf_apb.cod_classif_impto    = v-cod_classif_impto AND   
                       dirf_apb.cod_espec_docto      = v-cod_espec_docto   AND   
                       dirf_apb.cod_ser_docto        = v-cod_ser_docto     AND   
                       dirf_apb.cod_tit_ap           = v-cod_tit_ap        AND   
                       dirf_apb.cod_parcela          = v-cod_parcela            
                 NO-LOCK NO-ERROR.

            



            IF NOT AVAILABLE(dirf_apb) THEN DO:
/*                 MESSAGE "nao achou " SKIP v-cod_parcela */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.  */
                
                /*Para verificar as parcelas 01*/
                ASSIGN v-cod_parcela = "0" + v-cod_parcela.

/*                 MESSAGE "tenta assim " SKIP v-cod_parcela */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.    */

                FIND FIRST dirf_apb
                 WHERE dirf_apb.cod_estab            = v-cod_estab         AND   
                       dirf_apb.num_pessoa           = v-num_pessoa        AND   
                       dirf_apb.dat_refer            = v-dat_refer         AND   
                       dirf_apb.cod_pais             = v-cod_pais          AND   
                       dirf_apb.cod_unid_federac     = v-cod_unid_federac  AND   
                       dirf_apb.cod_imposto          = v-cod_imposto       AND   
                       dirf_apb.cod_classif_impto    = v-cod_classif_impto AND   
                       dirf_apb.cod_espec_docto      = v-cod_espec_docto   AND   
                       dirf_apb.cod_ser_docto        = v-cod_ser_docto     AND   
                       dirf_apb.cod_tit_ap           = v-cod_tit_ap        AND   
                       dirf_apb.cod_parcela          = v-cod_parcela            
                 NO-LOCK NO-ERROR.



            END.
            IF NOT AVAILABLE(dirf_apb) THEN DO:
                

/*                 MESSAGE "ainda nao tem"                */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
               CREATE tt-dirf_apb.
               ASSIGN tt-dirf_apb.cod_estab         = v-cod_estab        
                      tt-dirf_apb.num_pessoa        = v-num_pessoa       
                      tt-dirf_apb.dat_refer         = v-dat_refer        
                      tt-dirf_apb.cod_pais          = v-cod_pais         
                      tt-dirf_apb.cod_unid_federac  = v-cod_unid_federac 
                      tt-dirf_apb.cod_imposto       = v-cod_imposto      
                      tt-dirf_apb.cod_classif_impto = v-cod_classif_impto
                      tt-dirf_apb.cod_espec_docto   = v-cod_espec_docto  
                      tt-dirf_apb.cod_ser_docto     = v-cod_ser_docto    
                      tt-dirf_apb.cod_tit_ap        = v-cod_tit_ap       
                      tt-dirf_apb.cod_parcela       = v-cod_parcela.

               IF v-acao <> "I" THEN
                  ASSIGN tt-dirf_apb.observacao        = "*Documento n∆o encontrado".
               ELSE 
                  ASSIGN tt-dirf_apb.val_rendto_tribut           = v-val_rendto_tribut_new           
                         tt-dirf_apb.val_aliq_impto              = v-val_aliq_impto_new              
                         tt-dirf_apb.val_acum_impto_retid_period = v-val_acum_impto_retid_period_new. 

               ASSIGN tt-dirf_apb.acao              = v-acao.
               
               NEXT blk.
            END.
         END.
         ELSE DO:
            ASSIGN v-cod_tit_ap = STRING(i-cod_tit_ap,'9999999').



            




            FIND FIRST dirf_apb
                 WHERE dirf_apb.cod_estab            = v-cod_estab         AND   
                       dirf_apb.num_pessoa           = v-num_pessoa        AND   
                       dirf_apb.dat_refer            = v-dat_refer         AND   
                       dirf_apb.cod_pais             = v-cod_pais          AND   
                       dirf_apb.cod_unid_federac     = v-cod_unid_federac  AND   
                       dirf_apb.cod_imposto          = v-cod_imposto       AND   
                       dirf_apb.cod_classif_impto    = v-cod_classif_impto AND   
                       dirf_apb.cod_espec_docto      = v-cod_espec_docto   AND   
                       dirf_apb.cod_ser_docto        = v-cod_ser_docto     AND   
                       dirf_apb.cod_tit_ap           = v-cod_tit_ap        AND   
                       dirf_apb.cod_parcela          = v-cod_parcela            
                 NO-LOCK NO-ERROR.

            IF NOT AVAILABLE(dirf_apb) THEN DO:
/*                  MESSAGE "nao achou  " SKIP v-cod_parcela */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.    */
                /*Para verificar as parcelas 01*/
                ASSIGN v-cod_parcela = "0" + v-cod_parcela.

/*                  MESSAGE "tenta assim " SKIP v-cod_parcela */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.     */

                FIND FIRST dirf_apb
                 WHERE dirf_apb.cod_estab            = v-cod_estab         AND   
                       dirf_apb.num_pessoa           = v-num_pessoa        AND   
                       dirf_apb.dat_refer            = v-dat_refer         AND   
                       dirf_apb.cod_pais             = v-cod_pais          AND   
                       dirf_apb.cod_unid_federac     = v-cod_unid_federac  AND   
                       dirf_apb.cod_imposto          = v-cod_imposto       AND   
                       dirf_apb.cod_classif_impto    = v-cod_classif_impto AND   
                       dirf_apb.cod_espec_docto      = v-cod_espec_docto   AND   
                       dirf_apb.cod_ser_docto        = v-cod_ser_docto     AND   
                       dirf_apb.cod_tit_ap           = v-cod_tit_ap        AND   
                       dirf_apb.cod_parcela          = v-cod_parcela            
                 NO-LOCK NO-ERROR.



            END.
            IF NOT AVAILABLE(dirf_apb) THEN DO:
/*                 MESSAGE "nao tem mesmo"                */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
               CREATE tt-dirf_apb.
               ASSIGN tt-dirf_apb.cod_estab         = v-cod_estab        
                      tt-dirf_apb.num_pessoa        = v-num_pessoa       
                      tt-dirf_apb.dat_refer         = v-dat_refer        
                      tt-dirf_apb.cod_pais          = v-cod_pais         
                      tt-dirf_apb.cod_unid_federac  = v-cod_unid_federac 
                      tt-dirf_apb.cod_imposto       = v-cod_imposto      
                      tt-dirf_apb.cod_classif_impto = v-cod_classif_impto
                      tt-dirf_apb.cod_espec_docto   = v-cod_espec_docto  
                      tt-dirf_apb.cod_ser_docto     = v-cod_ser_docto    
                      tt-dirf_apb.cod_tit_ap        = v-cod_tit_ap       
                      tt-dirf_apb.cod_parcela       = v-cod_parcela.

               ASSIGN tt-dirf_apb.acao              = v-acao.
            
               IF v-acao <> "I" THEN
                  ASSIGN tt-dirf_apb.observacao     = "Documento n∆o encontrado".
               ELSE
                  ASSIGN tt-dirf_apb.val_rendto_tribut           = v-val_rendto_tribut_new           
                         tt-dirf_apb.val_aliq_impto              = v-val_aliq_impto_new              
                         tt-dirf_apb.val_acum_impto_retid_period = v-val_acum_impto_retid_period_new. 
            
               NEXT blk.
            END.
         END.

         IF AVAILABLE(dirf_apb) THEN DO:
            CREATE tt-dirf_apb.
            BUFFER-COPY dirf_apb TO tt-dirf_apb.
            ASSIGN tt-dirf_apb.observacao = ""
                   tt-dirf_apb.acao       = v-acao
                   tt-dirf_apb.r-rowid    = ROWID(dirf_apb).

            IF tt-dirf_apb.acao = "A" THEN DO:
               
               /* Armazena os valores antigos */
               ASSIGN tt-dirf_apb.dat_refer_old                   = dirf_apb.dat_refer                  
                      tt-dirf_apb.val_rendto_tribut_old           = dirf_apb.val_rendto_tribut          
                      tt-dirf_apb.val_aliq_impto_old              = dirf_apb.val_aliq_impto             
                      tt-dirf_apb.val_acum_impto_retid_period_old = dirf_apb.val_acum_impto_retid_period.
               
               /* Grava os novos valores */
               IF v-dat_refer_new <> ? AND v-dat_refer_new <> tt-dirf_apb.dat_refer THEN
                  ASSIGN tt-dirf_apb.dat_refer                    = v-dat_refer_new.

               IF v-val_rendto_tribut_new <> ? AND v-val_rendto_tribut_new <> tt-dirf_apb.val_rendto_tribut THEN
                  ASSIGN tt-dirf_apb.val_rendto_tribut            = v-val_rendto_tribut_new.

               IF v-val_aliq_impto_new <> ? AND v-val_aliq_impto_new <> tt-dirf_apb.val_aliq_impto  THEN
                  ASSIGN tt-dirf_apb.val_aliq_impto               = v-val_aliq_impto_new.

               IF v-val_acum_impto_retid_period_new <> ? AND v-val_acum_impto_retid_period_new <> tt-dirf_apb.val_acum_impto_retid_period   THEN
                  ASSIGN tt-dirf_apb.val_acum_impto_retid_period  = v-val_acum_impto_retid_period_new.
            END.
         END.
      END.
      ELSE DO:
         ASSIGN i-cont-linha = 1048573.
      END.
   END.

   /* all prompts will be shut off/on */
   /* this prevents unsaved check     */
   ch-Excel:DisplayAlerts = false.
   /* Exit Excel */
   ch-Excel:QUIT().

   RELEASE OBJECT ch-excel.
END PROCEDURE.


PROCEDURE pi-processa-registros:
   DISABLE TRIGGERS FOR LOAD OF dirf_apb.

   FIND FIRST param-contrat 
        NO-LOCK NO-ERROR.

   FOR EACH tt-dirf_apb
            WHERE tt-dirf_apb.observacao = ""
            EXCLUSIVE-LOCK:

      RUN pi-acompanhar IN h-acomp (INPUT "Atualizando : " +
                                    STRING(tt-dirf_apb.cod_estab)         + "-" +
                                    STRING(tt-dirf_apb.num_pessoa)        + "-" +      
                                    STRING(tt-dirf_apb.dat_refer)         + "-" +
                                    STRING(tt-dirf_apb.cod_pais)          + "-" +
                                    STRING(tt-dirf_apb.cod_unid_federac)  + "-" +
                                    STRING(tt-dirf_apb.cod_imposto)       + "-" +
                                    STRING(tt-dirf_apb.cod_classif_impto) + "-" +
                                    STRING(tt-dirf_apb.cod_espec_docto)   + "-" +
                                    STRING(tt-dirf_apb.cod_ser_docto)     + "-" +
                                    STRING(tt-dirf_apb.cod_tit_ap)        + "-" +
                                    STRING(tt-dirf_apb.cod_parcela)             
                                    ).

      FIND FIRST dirf_apb
           WHERE ROWID(dirf_apb) = tt-dirf_apb.r-rowid
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE(dirf_apb) THEN DO:
         CASE tt-dirf_apb.acao:
              WHEN  "E" THEN DO:
                 DELETE dirf_apb.

                 ASSIGN tt-dirf_apb.observacao = "Registro eliminado com sucesso".
              END.
              WHEN "A" THEN DO:
                 ASSIGN dirf_apb.dat_refer                   = tt-dirf_apb.dat_refer                   
                        dirf_apb.val_rendto_tribut           = tt-dirf_apb.val_rendto_tribut           
                        dirf_apb.val_aliq_impto              = tt-dirf_apb.val_aliq_impto              
                        dirf_apb.val_acum_impto_retid_period = tt-dirf_apb.val_acum_impto_retid_period.

                 ASSIGN tt-dirf_apb.observacao = "Registro alterado com sucesso".
              END.
              WHEN "I" THEN DO:
                 ASSIGN tt-dirf_apb.observacao = "Registro j† existente".
              END.
         END CASE.
      END.
      ELSE DO:
         IF tt-dirf_apb.acao = "I" THEN DO:
            CREATE dirf_apb.
            BUFFER-COPY tt-dirf_apb TO dirf_apb.
            ASSIGN dirf_apb.log_gerac_autom = NO.
            ASSIGN tt-dirf_apb.observacao = "Registro criado com sucesso".
         END.

      END.
   END.
END PROCEDURE.


PROCEDURE pi-gera-log:
   DEFINE VARIABLE c-narrativa      AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-arquivo        AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
   DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.
   DEFINE VARIABLE c-tipo           AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-ordem          AS    CHARACTER             NO-UNDO FORMAT "x(300)".

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv".

   IF i-num-ped-exec-rpw <> 0 Then 
      Assign c-arquivo = c-dir-spool-servid-exec + "~/" + ENTRY(1,tt-param.arquivo,".") + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv".

   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   OUTPUT STREAM st-excel TO VALUE(c-arquivo) NO-CONVERT NO-MAP.

   {utp/ut-field.i ems5 dirf_apb cod_estab                          1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb num_pessoa                         1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb dat_refer                          1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb cod_pais                           1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb cod_unid_federac                   1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb cod_imposto                        1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb cod_classif_impto                  1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb cod_espec_docto                    1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb cod_ser_docto                      1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb cod_tit_ap                         1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb cod_parcela                        1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb dat_refer                          1} . PUT STREAM st-excel RETURN-VALUE + "(ANT)"       FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb val_rendto_tribut                  1} . PUT STREAM st-excel RETURN-VALUE + "(ANT)"       FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb val_aliq_impto                     1} . PUT STREAM st-excel RETURN-VALUE + "(ANT)"       FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb val_acum_impto_retid_period        1} . PUT STREAM st-excel RETURN-VALUE + "(ANT)"       FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb dat_refer                          1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb val_rendto_tribut                  1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb val_aliq_impto                     1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".
   {utp/ut-field.i ems5 dirf_apb val_acum_impto_retid_period        1} . PUT STREAM st-excel RETURN-VALUE                 FORMAT "X(30)" ";".

   PUT STREAM st-excel "Observaá‰es" SKIP.


   FOR EACH tt-dirf_apb
            NO-LOCK:

      RUN pi-acompanhar IN h-acomp (INPUT "Gerando LOG : " +
                                    STRING(tt-dirf_apb.cod_estab)         + "-" +
                                    STRING(tt-dirf_apb.num_pessoa)        + "-" +      
                                    STRING(tt-dirf_apb.dat_refer)         + "-" +
                                    STRING(tt-dirf_apb.cod_pais)          + "-" +
                                    STRING(tt-dirf_apb.cod_unid_federac)  + "-" +
                                    STRING(tt-dirf_apb.cod_imposto)       + "-" +
                                    STRING(tt-dirf_apb.cod_classif_impto) + "-" +
                                    STRING(tt-dirf_apb.cod_espec_docto)   + "-" +
                                    STRING(tt-dirf_apb.cod_ser_docto)     + "-" +
                                    STRING(tt-dirf_apb.cod_tit_ap)        + "-" +
                                    STRING(tt-dirf_apb.cod_parcela)             
                                    ).

      PUT STREAM st-excel
          tt-dirf_apb.cod_estab                        ";"
          tt-dirf_apb.num_pessoa                       ";"
          tt-dirf_apb.dat_refer                        ";"
          tt-dirf_apb.cod_pais                         ";"
          tt-dirf_apb.cod_unid_federac                 ";"
          tt-dirf_apb.cod_imposto                      ";"
          tt-dirf_apb.cod_classif_impto                ";"
          tt-dirf_apb.cod_espec_docto                  ";"
          tt-dirf_apb.cod_ser_docto                    ";"
          tt-dirf_apb.cod_tit_ap                       ";"
          tt-dirf_apb.cod_parcela                      ";"
          tt-dirf_apb.dat_refer_old                    ";"
          tt-dirf_apb.val_rendto_tribut_old            ";"
          tt-dirf_apb.val_aliq_impto_old               ";"
          tt-dirf_apb.val_acum_impto_retid_period_old  ";"
          tt-dirf_apb.dat_refer                        ";"
          tt-dirf_apb.val_rendto_tribut                ";"
          tt-dirf_apb.val_aliq_impto                   ";"
          tt-dirf_apb.val_acum_impto_retid_period      ";"
          tt-dirf_apb.observacao                       SKIP.
   END.

   PUT STREAM st-excel SKIP.
       
   OUTPUT STREAM st-excel CLOSE.

   IF i-num-ped-exec-rpw = 0 THEN
      DOS SILENT START excel.exe VALUE(c-arquivo).
END PROCEDURE.



