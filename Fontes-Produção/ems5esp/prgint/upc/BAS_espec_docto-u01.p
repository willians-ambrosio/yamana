
/****************************************************************************************** 
** 	   Programa: apl007aa-u01.p
**   	  Autor: Unknow
** 	 Fornecedor: DKP
**         Data: ?/2017
** Change/Chamado: 
**      Objetivo: Cria o bot∆o para vincular contrato ROF e CÉmbio na operaá∆o Financeira
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID NO-UNDO.

DEF new GLOBAL SHARED VAR h-fi-cod-espec          AS WIDGET-HANDLE NO-UNDO.
DEF new GLOBAL SHARED VAR h-bt-elimina            AS WIDGET-HANDLE NO-UNDO.
/* campos da tela  */
DEF NEW GLOBAL SHARED VAR h-bt-des_espec_docto    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-cod_espec_docto    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-ent                AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-tip_espec_docto    AS WIDGET-HANDLE NO-UNDO.
/* campos da tela  */
/* pega literal */
DEF NEW GLOBAL SHARED VAR h-bt-literal           AS WIDGET-HANDLE EXTENT 3 NO-UNDO.
/* pega literal */
DEF new GLOBAL SHARED VAR h-bt-salva              AS WIDGET-HANDLE NO-UNDO.
DEF new GLOBAL SHARED VAR h-bt-ok                 AS WIDGET-HANDLE NO-UNDO.


/* def new global shared var wh-fill    as widget-handle no-undo.             */
/* def new global shared var tx-label   as widget-handle no-undo.             */
/* def new global shared var wh-fill-2    as widget-handle no-undo.           */
/* def new global shared var tx-label-2   as widget-handle no-undo.           */
 DEF NEW GLOBAL SHARED VAR CHBOX-IMP    AS widget-handle no-undo. 
 DEF NEW GLOBAL SHARED VAR CHBOX-FGTS   AS widget-handle no-undo.
 DEF NEW GLOBAL SHARED VAR b-auxElimina AS widget-handle no-undo. 
 DEF NEW GLOBAL SHARED VAR b-auxSalva   AS widget-handle no-undo.
 DEF NEW GLOBAL SHARED VAR b-auxOk      AS widget-handle no-undo.
 DEF NEW GLOBAL SHARED VAR CHBOX-GPS    AS widget-handle no-undo.

/* MESSAGE "p-ind-event " p-ind-event        SKIP */
/*         "p-ind-object" p-ind-object       SKIP */
/*         "p-wgh-object" p-wgh-object:NAME  SKIP */
/*         "p-wgh-frame " p-wgh-frame        SKIP */
/*         "p-cod-table " p-cod-table        SKIP */
/* /*         "c-objeto    " c-objeto */          */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.    */


IF  p-ind-event = "ENABLE" 
AND p-ind-object = "VIEWER" 
AND VALID-HANDLE(h-bt-elimina) THEN DO:

    /* cria o botao para pagar o registro correspondente na tabela ext_espec_docto */
    CREATE BUTTON b-auxElimina
           ASSIGN ROW                = h-bt-elimina:ROW              
                  COLUMN             = h-bt-elimina:COLUMN           
                  WIDTH              = h-bt-elimina:WIDTH            
                  HEIGHT             = h-bt-elimina:HEIGHT           
/*                   LABEL              = h-bt-elimina:LABEL */
                  FRAME              = h-bt-elimina:FRAME            
                  FLAT-BUTTON        = h-bt-elimina:FLAT-BUTTON 
                  TOOLTIP            = "*" + h-bt-elimina:TOOLTIP
                  HELP               = h-bt-elimina:HELP             
                  NAME               = "elimina_esp"             
                  SENSITIVE          = h-bt-elimina:SENSITIVE        
                  VISIBLE            = h-bt-elimina:VISIBLE          
                  CONVERT-3D-COLOR   = h-bt-elimina:CONVERT-3D-COLOR
                  
                  TRIGGERS:
                    ON "CHOOSE" PERSISTENT RUN prgint\upc\BAS_espec_docto-u02.p (INPUT "Apagar":U,
                                                                                 INPUT p-ind-object, 
                                                                                 INPUT p-wgh-object, 
                                                                                 INPUT p-wgh-frame,  
                                                                                 INPUT p-cod-table,  
                                                                                 INPUT p-row-table,
                                                                                 INPUT h-bt-elimina).


                  END TRIGGERS.


           b-auxElimina:LOAD-IMAGE-UP(h-bt-elimina:IMAGE-UP).
           b-auxElimina:LOAD-IMAGE-INSENSITIVE(h-bt-elimina:IMAGE-INSENSITIVE).
           b-auxElimina:MOVE-TO-TOP().
           ASSIGN h-bt-elimina:TAB-STOP  = NO
                  h-bt-elimina:SENSITIVE = NO.


END.


IF  p-ind-event = "ENABLE" 
AND p-ind-object = "VIEWER" 
AND VALID-HANDLE(h-bt-salva)
AND VALID-HANDLE(h-bt-ok) THEN DO: 
    

/*      CHBOX-IMP:SENSITIVE  = h-bt-des_espec_docto:SENSITIVE. */
/*      CHBOX-FGTS:SENSITIVE = h-bt-des_espec_docto:SENSITIVE. */

  
   RUN reposiciona.

   FIND FIRST ext_espec_docto NO-LOCK 
       WHERE ext_espec_docto.cod_espec_docto = h-fi-cod-espec:SCREEN-VALUE NO-ERROR.

   IF AVAIL(ext_espec_docto) THEN DO:
     
    IF NOT ext_espec_docto.LOG_imposto AND NOT ext_espec_docto.LOG_fgts AND NOT ext_espec_docto.LOG_GPS THEN      
      ASSIGN CHBOX-IMP:SENSITIVE  = yes
             CHBOX-FGTS:SENSITIVE = yes
             CHBOX-GPS:SENSITIVE  = YES.
    ELSE 
       ASSIGN CHBOX-IMP:SENSITIVE  = ext_espec_docto.LOG_imposto 
              CHBOX-FGTS:SENSITIVE = ext_espec_docto.LOG_fgts
              CHBOX-GPS:SENSITIVE  = ext_espec_docto.LOG_GPS. 
   END.
   ELSE 
     ASSIGN CHBOX-IMP:SENSITIVE  = h-bt-des_espec_docto:SENSITIVE 
            CHBOX-FGTS:SENSITIVE = h-bt-des_espec_docto:SENSITIVE
            CHBOX-GPS:SENSITIVE  = h-bt-des_espec_docto:SENSITIVE. 



END. 


if  p-ind-event = "DISPLAY"
and p-ind-object = "VIEWER"
 then do: 
    

FIND FIRST ext_espec_docto NO-LOCK
     WHERE ext_espec_docto.cod_espec_docto = h-fi-cod-espec:SCREEN-VALUE NO-ERROR.


   RUN reposiciona.

   /* cria o campo de imposto */
   CREATE TOGGLE-BOX  CHBOX-IMP     
       ASSIGN FRAME        = p-wgh-frame
              HEIGHT-CHARS = 1
              WIDTH-CHARS  = 20
              ROW          = h-bt-des_espec_docto:ROW + 1
              SCREEN-VALUE = IF AVAIL ext_espec_docto THEN STRING(ext_espec_docto.LOG_imposto) ELSE "NO"
              COLUMN       = 42
              SENSITIVE    = NO
              VISIBLE      = YES
              LABEL        = "DARF Manual"
              TOOLTIP      = "Indica se o documento possui DARF sem imposto"
              TRIGGERS:
                    ON "VALUE-CHANGED" PERSISTENT RUN prgint\upc\BAS_espec_docto-u02.p (INPUT "desabilitaFgts":U,
                                                                                        INPUT p-ind-object,
                                                                                        INPUT p-wgh-object,
                                                                                        INPUT p-wgh-frame,
                                                                                        INPUT p-cod-table,
                                                                                        INPUT p-row-table,
                                                                                        INPUT h-bt-salva).
                                                                                       

              END TRIGGERS.

     /* cria o campo de Fgts */
   CREATE TOGGLE-BOX  CHBOX-FGTS     
       ASSIGN FRAME        = p-wgh-frame
              HEIGHT-CHARS = 1
              WIDTH-CHARS  = 10
              ROW          = h-bt-des_espec_docto:ROW + 1
              SCREEN-VALUE = IF AVAIL ext_espec_docto THEN STRING(ext_espec_docto.LOG_FGTS) ELSE "NO"
              COLUMN       = 55
              SENSITIVE    = NO
              VISIBLE      = YES
              LABEL        = "FGTS"
              TOOLTIP      = "Indica se o documento possui FGTS"
              TRIGGERS:
                    ON "VALUE-CHANGED" PERSISTENT RUN prgint\upc\BAS_espec_docto-u02.p (INPUT "desabilitaImposto":U,
                                                                                        INPUT p-ind-object,
                                                                                        INPUT p-wgh-object,
                                                                                        INPUT p-wgh-frame,
                                                                                        INPUT p-cod-table,
                                                                                        INPUT p-row-table,
                                                                                        INPUT h-bt-salva).
                                                                                       

              END TRIGGERS.

   /* cria o campo de GPS */
   CREATE TOGGLE-BOX  CHBOX-GPS     
       ASSIGN FRAME        = p-wgh-frame
              HEIGHT-CHARS = 1
              WIDTH-CHARS  = 12
              ROW          = h-bt-des_espec_docto:ROW + 1
              SCREEN-VALUE = IF AVAIL ext_espec_docto THEN STRING(ext_espec_docto.LOG_GPS) ELSE "NO" 
              COLUMN       = 63
              SENSITIVE    = NO
              VISIBLE      = YES
              LABEL        = "GPS Manual"
              TOOLTIP      = "Indica se o documento possui GPS Manual"
              TRIGGERS:
                    ON "VALUE-CHANGED" PERSISTENT RUN prgint\upc\BAS_espec_docto-u02.p (INPUT "desabilitaImpFgts":U,
                                                                                        INPUT p-ind-object,
                                                                                        INPUT p-wgh-object,
                                                                                        INPUT p-wgh-frame,
                                                                                        INPUT p-cod-table,
                                                                                        INPUT p-row-table,
                                                                                        INPUT h-bt-salva).
                                                                                       

              END TRIGGERS.



    
end.



PROCEDURE reposiciona:

    IF VALID-HANDLE(h-bt-literal[1])
       AND VALID-HANDLE(h-bt-literal[2])
       AND VALID-HANDLE(h-bt-literal[3])
       AND VALID-HANDLE(h-bt-des_espec_docto)
       AND VALID-HANDLE(h-bt-cod_espec_docto)
/*        AND VALID-HANDLE(h-bt-ent) */
/*        AND VALID-HANDLE(h-bt-tip_espec_docto) */
       THEN DO:

        ASSIGN
        h-bt-literal[1]:COLUMN      = 10
        h-bt-literal[2]:COLUMN      = 11
        h-bt-literal[3]:COLUMN      = 15
        h-bt-des_espec_docto:COLUMN = 25
        h-bt-cod_espec_docto:COLUMN = 25
        .

        IF VALID-HANDLE(h-bt-ent) THEN h-bt-ent:COLUMN  = 29.
        IF VALID-HANDLE(h-bt-tip_espec_docto) THEN h-bt-tip_espec_docto:COLUMN = 25.

     END.


END PROCEDURE.



