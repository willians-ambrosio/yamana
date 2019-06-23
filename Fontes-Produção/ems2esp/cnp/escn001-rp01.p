/****************************************************************************************** 
** 	     Programa: ESCN001-RP01.p
**          Autor: Felipe Vieria / Daniela Campos
**     Fornecedor: DKP
**       	 Data: 20/11/2018
** Change/Chamado: CHxxxxx
**       Objetivo: Relat¢rio de Mediá‰es de contrato
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** 
** Data         Autor   		Fornecedor    Change/Chamado      Descriá∆o da Alteraá∆o
** N/A          N/A         	 N/A               N/A	                   N/a
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
**     PAR∂METROS DE ENTRADA: tt-param
**       PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
**      CADASTRADO NA TABELA: N/A
******************************************************************************************/
{utp\ut-glob.i}
{cnp\cn001-rp4.i}
{cnp/cn001-rp1.i}

/* definiá∆o das temp-tables para recebimento de parametros */
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as INTEGER
    FIELD fornec-ini       AS INT
    FIELD fornec-fim       AS INT
    FIELD contrato-ini     AS INT
    FIELD contrato-fim     AS INT.

define temp-table tt-digita no-undo
    field codigo            as CHAR
    field descricao         as CHAR FORMAT "X(50)".

DEF TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita  AS RAW.

/*recebimento de parametros*/
DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

DEFINE VARIABLE c-esp1 AS CHARACTER   NO-UNDO INITIAL "DP;RT;GL".
DEFINE VARIABLE c-esp2 AS CHARACTER   NO-UNDO INITIAL "DP;RT;RC;GL".
DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.
DEF VAR h-acomp AS HANDLE NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.                                                                                         
{utp/ut-liter.i Imprimindo *}                                                                                                         
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    
FOR EACH contrato-for NO-LOCK WHERE
         contrato-for.nr-contrato >= tt-param.contrato-ini  AND 
         contrato-for.nr-contrato <= tt-param.contrato-fim:

    IF (contrato-for.cod-emitente < tt-param.fornec-ini OR 
        contrato-for.cod-emitente > tt-param.fornec-fim) THEN NEXT.

    FIND ext-contrato-for OF contrato-for NO-LOCK NO-ERROR.

    FOR EACH medicao-contrat OF contrato-for NO-LOCK:

        FIND es-medicao-contrat OF medicao-contrat NO-LOCK NO-ERROR.

        FOR EACH rat-ordem NO-LOCK OF medicao-contrat:

             FIND item-doc-est OF rat-ordem NO-LOCK NO-ERROR.
             FIND item-doc-est OF rat-ordem NO-LOCK NO-ERROR.
             FIND docum-est OF item-doc-est NO-LOCK NO-ERROR.
  
              /* Incluir for each tt-digita */
              /* Busca o t°tulo no contas a pagar */ 
             DO i-cont = 1 TO NUM-ENTRIES(c-esp1,";"):
             
                  FOR EACH tit_ap NO-LOCK 
                      WHERE tit_ap.cod_estab         = docum-est.cod-estab
                        AND tit_ap.cdn_fornecedor    = rat-ordem.cod-emitente
                        AND tit_ap.cod_espec_docto   = TRIM(ENTRY(i-cont,c-esp1,";")) /* "DP" RT */
                        AND tit_ap.cod_ser_docto     = rat-ordem.serie-docto 
                        AND tit_ap.cod_tit_ap        = rat-ordem.nro-docto
                        AND tit_ap.ind_origin_tit_ap = "REC":
    
                      RUN pi-acompanhar IN h-acomp (INPUT "Contrato:" + STRING(contrato-for.nr-contrato) + " Titulo: " + tit_ap.cod_tit_ap).

                      RUN pi_criar_tt_relacto_tit_ap.
    
                  END.
             END.
        END.
    END.
END.

RUN pi-abre-excel.

RUN pi-monta-excel.

RUN pi-encerra-excel.

RUN pi-finalizar IN h-acomp.
RETURN "OK":U.

PROCEDURE pi-monta-excel:

    DO i-cont = 1 TO NUM-ENTRIES(c-esp2,";"):
        FOR EACH tt_relacto_tit_ap WHERE 
                 tt_relacto_tit_ap.cod_espec_docto = ENTRY(i-cont,c-esp2,";"): /* Listar RC e RT */
            
            ASSIGN chexcel:Range("A" + STRING(iLinha)):value  = tt_relacto_tit_ap.nr_contrato                          
                   chexcel:Range("B" + STRING(iLinha)):value  = tt_relacto_tit_ap.tta_status                               
                   chexcel:Range("C" + STRING(iLinha)):value  = tt_relacto_tit_ap.dt-ini-validade                           
                   chexcel:Range("D" + string(iLinha)):value  = tt_relacto_tit_ap.dt-ter-validade                          
                   chexcel:Range("E" + string(iLinha)):value  = tt_relacto_tit_ap.dat_vencto_tit_ap                          
                   chexcel:Range("F" + string(iLinha)):VALUE  = STRING(tt_relacto_tit_ap.log_pagto_bloqdo,"Sim/N∆o")                    
                   chexcel:Range("G" + string(iLinha)):value  = tt_relacto_tit_ap.cod_estab                    
                   chexcel:Range("H" + string(iLinha)):value  = tt_relacto_tit_ap.cdn_fornec               
                   chexcel:Range("I" + string(iLinha)):value  = tt_relacto_tit_ap.tta_cnd_fornec_orig           
                   chexcel:Range("J" + string(iLinha)):value  = tt_relacto_tit_ap.cod_tit_ap               
                   chexcel:Range("K" + string(iLinha)):value  = tt_relacto_tit_ap.cod_ser_docto            
                   chexcel:Range("L" + string(iLinha)):value  = tt_relacto_tit_ap.cod_espec_docto          
                   chexcel:Range("M" + string(iLinha)):value  = tt_relacto_tit_ap.cod_parcela              
                   chexcel:Range("N" + string(iLinha)):value  = tt_relacto_tit_ap.nat-operacao             
                   chexcel:Range("O" + string(iLinha)):value  = tt_relacto_tit_ap.cod_portador             
                   chexcel:Range("P" + string(iLinha)):value  = tt_relacto_tit_ap.dat_emis_docto           
                   chexcel:Range("Q" + string(iLinha)):value  = tt_relacto_tit_ap.dat_prev_pagto           
                   chexcel:Range("R" + string(iLinha)):value  = tt_relacto_tit_ap.cod_indic_econ           
                   chexcel:Range("S" + string(iLinha)):value  = tt_relacto_tit_ap.val_origin_tit_ap        
                   chexcel:Range("T" + string(iLinha)):value  = tt_relacto_tit_ap.val_sdo_tit_ap           
                   chexcel:Range("U" + string(iLinha)):value  = tt_relacto_tit_ap.num-seq-medicao      
                   chexcel:Range("V" + string(iLinha)):value  = tt_relacto_tit_ap.num-seq-item         
                   chexcel:Range("W" + string(iLinha)):value  = tt_relacto_tit_ap.dat-medicao              
                   chexcel:Range("X" + string(iLinha)):value  = tt_relacto_tit_ap.val-medicao              
                   chexcel:Range("Y" + string(iLinha)):value  = tt_relacto_tit_ap.vl-glosa.
    
            chExcel:range("A" + string(iLinha) + ":" + "Y" + string(iLinha) ):borderaround(1,,1).
          
            iLinha = iLinha + 1.
                    
        END.
    END.
END.

PROCEDURE pi-abre-excel:
    DEFINE VARIABLE cRange AS CHARACTER   NO-UNDO.

    ASSIGN cNomeArqDestino = tt-param.arquivo
           iLinha = 1.

    /* cria a planilha */
    {cnp\cn001-RP2.i "Novo"}
    /* inserindo cabeáalho */
    ASSIGN chexcel:Range("A" + STRING(iLinha)):value  = "Contrato" 
           chexcel:Range("B" + STRING(iLinha)):value  = "Status" 
           chexcel:Range("C" + string(iLinha)):value  = "Data Ini Validade"  
           chexcel:Range("D" + string(iLinha)):value  = "Data Fim Validade" 
           chexcel:Range("E" + string(iLinha)):VALUE  = "Data Vencto"
           chexcel:Range("F" + string(iLinha)):VALUE  = "Pagto Bloq" 
           chexcel:Range("G" + string(iLinha)):VALUE  = "Est"
           chexcel:Range("H" + string(iLinha)):VALUE  = "Fornec. Titulo"
           chexcel:Range("I" + string(iLinha)):VALUE  = "Fornec. Contrato" 
           chexcel:Range("J" + string(iLinha)):value  = "Titulo/NF" 
           chexcel:Range("K" + string(iLinha)):value  = "Serie"
           chexcel:Range("L" + string(iLinha)):value  = "Esp."
           chexcel:Range("M" + string(iLinha)):value  = "Parcela"
           chexcel:Range("N" + string(iLinha)):value  = "Nat.Oper"
           chexcel:Range("O" + string(iLinha)):value  = "Portador"
           chexcel:Range("P" + string(iLinha)):value  = "Data Emiss∆o"
           chexcel:Range("Q" + string(iLinha)):value  = "Data Prev. Pagto"
           chexcel:Range("R" + string(iLinha)):value  = "Moeda"
           chexcel:Range("S" + string(iLinha)):value  = "Valor"
           chexcel:Range("T" + string(iLinha)):value  = "Saldo"
           chexcel:Range("U" + string(iLinha)):value  = "Seq. Mediá∆o"
           chexcel:Range("V" + string(iLinha)):value  = "Seq. Item"
           chexcel:Range("W" + string(iLinha)):value  = "Data Mediá∆o"
           chexcel:Range("X" + string(iLinha)):value  = "Valor Mediá∆o"
           chexcel:Range("Y" + string(iLinha)):value  = "Valor Glosa".


   ASSIGN chExcel:columns( "A"):NumberFormat = "@"                         
          chExcel:columns( "B"):NumberFormat = "@"                         
          chExcel:columns( "C"):NumberFormat = "dd/mm/aaaa;@"             
          chExcel:columns( "D"):NumberFormat = "dd/mm/aaaa;@"             
          chExcel:columns( "E"):NumberFormat = "dd/mm/aaaa;@"              
          chExcel:columns( "F"):NumberFormat = "@"                         
          chExcel:columns( "G"):NumberFormat = "@"                         
          chExcel:columns( "H"):NumberFormat = "@"                         
          chExcel:columns( "I"):NumberFormat = "@"                         
          chExcel:columns( "J"):NumberFormat = "@"                         
          chExcel:columns( "K"):NumberFormat = "@"                         
          chExcel:columns( "L"):NumberFormat = "@"                         
          chExcel:columns( "M"):NumberFormat = "@"                         
          chExcel:columns( "N"):NumberFormat = "@"                         
          chExcel:columns( "O"):NumberFormat = "@"                         
          chExcel:columns( "P"):NumberFormat = "dd/mm/aaaa;@"              
          chExcel:columns( "Q"):NumberFormat = "dd/mm/aaaa;@"              
          chExcel:columns( "R"):NumberFormat = "@"                         
          chExcel:columns( "S"):NumberFormat = "#.##0,00"                  
          chExcel:columns( "T"):NumberFormat = "#.##0,00"                  
          chExcel:columns( "U"):NumberFormat = "@"                         
          chExcel:columns( "V"):NumberFormat = "@"                         
          chExcel:columns( "W"):NumberFormat = "dd/mm/aaaa;@"              
          chExcel:columns( "X"):NumberFormat = "#.##0,00"                  
          chExcel:columns( "Y"):NumberFormat = "#.##0,00".                 
                                                                  
  cRange                          = "A1:Y1".                                        
  chExcel:Range(cRange):FONT:bold = YES.                                            
  chExcel:Range(cRange):HorizontalAlignment = 3.      

  ASSIGN iLinha = 2.
                                                                                    
END.                                                                                
                                                                                    
PROCEDURE pi-encerra-excel:                                                         
   /* Encerra o excel */
/*     chExcel:DisplayAlerts = FALSE.                                                   */
/*                                                                                      */
/*     /* formataá∆o  */                                                                */
/*     chExcel:range("A" + STRING(1) + ":" + "Y" + STRING(1)):interior:colorindex = 45. */
/*     chExcel:COLUMNS("A:AA"):SELECT.                                                  */
/*     chExcel:SELECTION:COLUMNS:AUTOFIT.                                               */

    /* salva a planilha */
    {cnp\cn001-RP2.i "Open"}  
  
END PROCEDURE.
