
/****************************************************************************************** 
** 	   Programa: add_cta_corren-u01
**   	  Autor: Felipe Vieira
** 	 Fornecedor: DKP
**         Data: 22/08/2018
** Change/Chamado: 
**      Objetivo: Criar campo "imposto" e solicitar o c¢digo do tributo qdo for verdadeiro - Usado apenas qdo o 
                  bordorì n∆o for de DARF
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: add_cta_corren,mod_cta_corren_basico   
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{utp\ut-glob.i}
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID NO-UNDO.


/* MESSAGE "p-ind-evento.....: " p-ind-event         SKIP */
/*         "p-ind-objeto.....: " p-ind-object        SKIP */
/*         "Handle do Objeto.: " p-wgh-object        SKIP */
/*         "Frame............: " p-wgh-frame         SKIP */
/*         "Nome da tabela...: " p-cod-table         SKIP */
/*         "Rowid da tabela..: " STRING(p-row-table) SKIP */
/*     VIEW-AS ALERT-BOX TITLE "bas-espec-docto".         */

DEFINE NEW GLOBAL SHARED VARIABLE fill-cont-cosmo     AS WIDGET-HANDLE NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE text-cont-cosmo     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE aux-contaCorrente   AS WIDGET-HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE h-bt_cx_bcos        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-bt_psdid           AS WIDGET-HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE c-cod_cta_corren     as char no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE c-tip_trans_cx       as char no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE c-cod_finalid_econ   as char no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE i-psdid              as int  no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE i-cdn_parcei_edi     as int  no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE l-aux-salvar         as INT  no-undo.

IF p-ind-event = "ENABLE" THEN DO:

    l-aux-salvar = 2.

    RUN piBuscaWidget (INPUT "cod_cta_corren",
                       INPUT p-wgh-frame,
                       OUTPUT aux-contaCorrente).

    RUN piBuscaWidget (INPUT  "bt_cx_bcos",
                       INPUT  p-wgh-frame,
                       OUTPUT h-bt_cx_bcos).
   

   CREATE TEXT text-cont-cosmo 
        assign frame        = p-wgh-frame
               format       = "x(14)"
               width        = 10
               screen-value = "Conta Cosmo:"
               row          = aux-contaCorrente:ROW + 2.6 
               col          = aux-contaCorrente:COL + 31
               visible      = YES.


   CREATE FILL-IN fill-cont-cosmo
      assign frame              = p-wgh-frame
             format             = "9999999999" 
             DATA-TYPE          = "INTEGER"
             side-label-handle  = text-cont-cosmo:handle
             width              = 11
             height             = 0.88
             row                = aux-contaCorrente:ROW + 2.5
             col                = aux-contaCorrente:COL + 41
             visible            = yes
             sensitive          = YES.

    CREATE BUTTON h-bt_psdid
       ASSIGN ROW                = h-bt_cx_bcos:ROW              
              COLUMN             = h-bt_cx_bcos:COLUMN + 15.5          
              WIDTH              = h-bt_cx_bcos:WIDTH            
              HEIGHT             = h-bt_cx_bcos:HEIGHT           
              LABEL              = "PSDID" 
              FRAME              = h-bt_cx_bcos:FRAME            
/*               FLAT-BUTTON        = h-aux-bt_era1:FLAT-BUTTON   */
/*               TOOLTIP            = "*" + h-aux-bt_era1:TOOLTIP */
/*               HELP               = h-aux-bt_era1:HELP          */
              NAME               = "PSDID"             
              SENSITIVE          = YES        
              VISIBLE            = YES          
              CONVERT-3D-COLOR   = h-bt_cx_bcos:CONVERT-3D-COLOR

              TRIGGERS:
                ON "CHOOSE" PERSISTENT RUN prgint\upc\add_cta_corren-u01.p (INPUT "psdid":U,
                                                                            INPUT p-ind-object,
                                                                            INPUT p-wgh-object,
                                                                            INPUT p-wgh-frame,
                                                                            INPUT p-cod-table,
                                                                            INPUT p-row-table).

              END TRIGGERS.

    IF VALID-HANDLE(aux-contaCorrente)
       AND VALID-HANDLE(fill-cont-cosmo) THEN DO:
    
       FIND FIRST ext-cta_corren EXCLUSIVE-LOCK
            WHERE cod_cta_corren = aux-contaCorrente:SCREEN-VALUE NO-ERROR.
         IF AVAIL(ext-cta_corren)THEN
           fill-cont-cosmo:SCREEN-VALUE = cod_cta_cosmo.
    
    END.  
END.

IF p-ind-event = "VALIDATE" THEN DO:

    FIND FIRST ext-cta_corren EXCLUSIVE-LOCK
         WHERE cod_cta_corren = aux-contaCorrente:SCREEN-VALUE NO-ERROR.
    
    IF AVAIL(ext-cta_corren) THEN
      ASSIGN ext-cta_corren.cod_cta_cosmo = fill-cont-cosmo:SCREEN-VALUE.
    ELSE DO:
     CREATE ext-cta_corren.
     ASSIGN ext-cta_corren.cod_cta_corren = aux-contaCorrente:SCREEN-VALUE
            ext-cta_corren.cod_cta_cosmo  = fill-cont-cosmo:SCREEN-VALUE.
    END.

    IF l-aux-salvar = 1 THEN DO:

       FIND FIRST es_param_cta_corren EXCLUSIVE-LOCK
            WHERE es_param_cta_corren.cod_cta_corren = c-cod_cta_corren NO-ERROR.

       IF NOT AVAIL es_param_cta_corren THEN DO:
           CREATE es_param_cta_corren.
           ASSIGN es_param_cta_corren.cod_cta_corren = c-cod_cta_corren. 
       END.
           ASSIGN es_param_cta_corren.tip_trans_cx        = c-tip_trans_cx     
                  es_param_cta_corren.cod_finalid_econ    = c-cod_finalid_econ 
                  es_param_cta_corren.psdid               = i-psdid            
                  es_param_cta_corren.cdn_parcei_edi      = i-cdn_parcei_edi.  



    END.
    ELSE IF l-aux-salvar = 3 THEN DO:

       FIND FIRST es_param_cta_corren EXCLUSIVE-LOCK
            WHERE es_param_cta_corren.cod_cta_corren = c-cod_cta_corren NO-ERROR.

       IF AVAIL es_param_cta_corren THEN DO:
           DELETE es_param_cta_corren.
       END.

    END.


END.


IF p-ind-event = "psdid" AND VALID-HANDLE(aux-contaCorrente)  THEN DO:
 
     c-cod_cta_corren = aux-contaCorrente:SCREEN-VALUE.
     l-aux-salvar = 2.

     FIND FIRST es_param_cta_corren NO-LOCK
          WHERE es_param_cta_corren.cod_cta_corren = c-cod_cta_corren NO-ERROR.
     IF AVAIL es_param_cta_corren THEN
         ASSIGN c-tip_trans_cx        = es_param_cta_corren.tip_trans_cx     
                c-cod_finalid_econ    = es_param_cta_corren.cod_finalid_econ 
                i-psdid               = es_param_cta_corren.psdid            
                i-cdn_parcei_edi      = es_param_cta_corren.cdn_parcei_edi. 

     RUN VAN\van002-w01.w.

END.




PROCEDURE piBuscaWidget:
    DEF INPUT  PARAM pNome     AS CHAR.
    DEF INPUT  PARAM pFrame    AS WIDGET-HANDLE.
    DEF OUTPUT PARAM pObject   AS WIDGET-HANDLE.

    DEF VAR hFrame             AS WIDGET-HANDLE.
    DEF VAR whObjeto           AS WIDGET-HANDLE.

    ASSIGN hFrame = pFrame:FIRST-CHILD.

    DO WHILE VALID-HANDLE(hFrame):

        IF hFrame:TYPE <> "field-group" THEN
        DO:

            IF hFrame:Type = "frame" THEN
            DO:
                RUN piBuscaWidget(INPUT  pNome,
                                  INPUT  hFrame,
                                  OUTPUT whObjeto).
                IF whObjeto <> ? THEN
                DO:
                    ASSIGN pObject = whObjeto.
                    LEAVE.
                END.
            END.
                                                        
/*             MESSAGE hFrame:NAME SKIP               */
/*                     hFrame:TOOLTIP                 */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */

            IF hFrame:NAME = pNome THEN
            DO:
                ASSIGN pObject = hFrame.
                LEAVE.
            END.

            ASSIGN hFrame = hFrame:NEXT-SIBLING.
        END.
        ELSE ASSIGN hFrame = hFrame:FIRST-CHILD.
    END.
END PROCEDURE.

