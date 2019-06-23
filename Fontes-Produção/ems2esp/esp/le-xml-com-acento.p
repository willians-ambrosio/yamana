DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
 DEFINE VARIABLE hRoot AS HANDLE NO-UNDO.
 DEFINE VARIABLE hTable AS HANDLE NO-UNDO.
 DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE hFieldq AS HANDLE NO-UNDO.
DEFINE VARIABLE hFieldq1 AS HANDLE NO-UNDO.
DEFINE VARIABLE hFieldq2 AS HANDLE NO-UNDO.
DEFINE VARIABLE hText AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuf1 AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBFld AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBFld1 AS HANDLE NO-UNDO.
DEFINE VARIABLE ix AS INTEGER NO-UNDO.
DEFINE VARIABLE jx AS INTEGER NO-UNDO.
DEFINE VARIABLE q AS INTEGER     NO-UNDO.
DEFINE VARIABLE q1 AS INTEGER     NO-UNDO.
DEFINE VARIABLE q2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-valor AS LONGCHAR  NO-UNDO.

DEFINE VARIABLE i-seq AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-idklassmatt        AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-empresasol         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-estabelecimentosol AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-pergunta AS CHARACTER FORMAT "x(500)" NO-UNDO.
DEFINE VARIABLE c-resposta AS CHARACTER FORMAT "x(500)" NO-UNDO.
DEFINE VARIABLE c-descricaoresposta AS CHARACTER FORMAT "x(200)" NO-UNDO.




 /* So we can create new recs */
DEFINE TEMP-TABLE ttship
     FIELD IdKlassmatt AS INT                              
     FIELD modo        AS INT                              
     FIELD IdSIN       AS INT                              
     FIELD Codigo      AS CHAR FORMAT "x(16)"              
     FIELD CodigoCompl AS CHAR FORMAT "x(20)"              
     FIELD Origem      AS INT FORMAT ">9"                  
     FIELD Tipo        AS CHAR FORMAT "99"                 
     FIELD UM          AS CHAR FORMAT "xx"                 
     FIELD NCM         AS CHAR FORMAT "9999.99.99"         
     FIELD NCMExc      AS DEC  FORMAT ">,>>9.99999999"     
     FIELD Subgrupo    AS CHAR FORMAT "x(8)"               
     FIELD DsRes       AS CHAR FORMAT "x(60)"              
     FIELD DsComp      AS CHAR FORMAT "x(2000)"            
     FIELD GrEstoque   AS INT  FORMAT ">9"                 
     FIELD Familia     AS CHAR                             
     FIELD EmpresaSol  AS CHAR FORMAT "x(5)"               
     FIELD EstabelecimentoSol AS CHAR FORMAT "x(5)"        
     FIELD QuestFiscalUsu AS CHAR FORMAT "x(12)"           
     FIELD QuestFiscalData AS CHAR                         
     FIELD QuestFiscalHora AS CHAR                         
     FIELD FormSupCtrleQtdeEstoque         AS CHAR         
     FIELD FormSupQtdeMinEstoque           AS CHAR         
     FIELD FormSupFamComEqpto              AS CHAR         
     FIELD FormSupDepart                   AS CHAR.        
    



DEF TEMP-TABLE QuestFiscal   
     FIELD seq          AS INT FORMAT "999"                
     FIELD IdKlassmatt  AS INT 
     FIELD estabelecimentosol AS CHAR
     FIELD EmpresaSol   AS CHAR                            
     FIELD pergunta     AS CHAR FORMAT "x(400)"            
     FIELD resposta     AS CHAR FORMAT "x(400)"                                   
     FIELD DescricaoResposta AS CHAR FORMAT "x(200)".      
    

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttship.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR QuestFiscal.
DEFINE INPUT PARAMETER c-xml  AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE c-Codigo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-Origem AS INTEGER     NO-UNDO.
     

 CREATE X-DOCUMENT hDoc.
 CREATE X-NODEREF hRoot.
 CREATE X-NODEREF hTable.
 CREATE X-NODEREF hField.
 CREATE X-NODEREF hFieldq.
 CREATE X-NODEREF hFieldq1.
 CREATE X-NODEREF hText.
 CREATE X-NODEREF hFieldq2.

 hBuf  = BUFFER ttship:HANDLE.
 hbuf1 = BUFFER QuestFiscal:HANDLE.

 /* Read in the file created in i-outcus.p */
 hDoc:LOAD("longchar",c-xml, FALSE).
 hDoc:GET-DOCUMENT-ELEMENT(hRoot).


  

 /* Read each Record from the root */
 REPEAT ix = 1 TO hRoot:NUM-CHILDREN:

     hRoot:GET-CHILD(hTable, ix).
     CREATE ttship.

     /* Get the fields given as attributes */
     /* Get the remaining fields given as elements with text */

     REPEAT jx = 1 TO hTable:NUM-CHILDREN: 
         
         hTable:GET-CHILD(hField,jx).
         IF hField:NUM-CHILDREN < 1 THEN NEXT.
         /* Skip any null value */

        
         hfield:GET-CHILD(hFieldq, 1).
         IF hFieldq:NUM-CHILDREN < 2 THEN .
         ELSE DO:
             REPEAT q2 = 1 TO hField:NUM-CHILDREN:
            
                 hField:GET-CHILD(hFieldq2,q2) NO-ERROR.
         
                 REPEAT q1 = 1 TO hFieldq:NUM-CHILDREN:
                     hFieldq2:GET-CHILD(hFieldq1,q1)NO-ERROR. 
                     hFieldq1:GET-CHILD(hText, 1) NO-ERROR.
                     c-valor = hTEXT:NODE-VALUE NO-ERROR.
                     IF hFieldq1:NAME = "seq" THEN i-seq = int(c-valor). 
                     IF hFieldq1:NAME = "idklassmatt" THEN i-idklassmatt = int(c-valor).
                     IF hFieldq1:NAME = "empresasol" THEN c-empresasol = c-valor.
                     IF hFieldq1:NAME = "estabelecimentosol" THEN c-estabelecimentosol = c-valor.
                     IF hFieldq1:NAME = "pergunta" THEN c-pergunta = c-valor.
                     IF hFieldq1:NAME = "resposta" THEN c-resposta = c-valor.
                     IF hFieldq1:NAME = "descricaoresposta" THEN c-descricaoresposta = c-valor.
                 END.
                 CREATE questfiscal.
                        questfiscal.seq = i-seq.
                        questfiscal.idklassmatt = i-idklassmatt.
                        questfiscal.empresasol  = c-empresasol.
                        questfiscal.estabelecimentosol = c-estabelecimentosol.
                        questfiscal.pergunta = c-pergunta.
                        questfiscal.resposta = c-resposta.
                        questfiscal.descricaoresposta = c-descricaoresposta.
             END.
         END.
     


         /* hDBFld = hBuf:BUFFER-FIELD(hField:NAME).      */
         hField:GET-CHILD(hText, 1).                   
         /* /* Get the text value of the field */         */
         /* hDBFld:BUFFER-VALUE = hTEXT:NODE-VALUE.       */
         c-valor = hTEXT:NODE-VALUE .
          IF hField:NAME = "IdKlassmatt"              THEN  ttship.IdKlassmatt            = int(c-valor).           /*  ASSIGN i-idklassmatt1 = int(c-valor).*/                           
          IF hField:NAME =  "modo                 "   THEN  ttship.modo                     = int(c-valor).                 
          IF hField:NAME =  "IdSIN                "   THEN  ttship.IdSIN                    = int(c-valor).             
          IF hField:NAME =  "Codigo               "   THEN  ttship.Codigo                   = c-valor.                   
          IF hField:NAME =  "CodigoCompl          "   THEN  ttship.CodigoCompl              = c-valor.                   
          IF hField:NAME =  "Origem               "   THEN  ttship.Origem                   = int(c-valor).              
          IF hField:NAME =  "Tipo                 "   THEN  ttship.Tipo                     = c-valor.                   
          IF hField:NAME =  "UM                   "   THEN  ttship.UM                       = c-valor.                   
          IF hField:NAME =  "NCM                  "   THEN  ttship.NCM                      = c-valor.                   
          IF hField:NAME =  "NCMExc               "   THEN  ttship.NCMExc                   = dec(c-valor).                   
          IF hField:NAME =  "Subgrupo             "   THEN  ttship.Subgrupo                 = c-valor.                   
          IF hField:NAME =  "DsRes                "   THEN  ttship.DsRes                    = c-valor.                   
          IF hField:NAME =  "DsComp               "   THEN  ttship.DsComp                   = c-valor.                   
          IF hField:NAME =  "GrEstoque            "   THEN  ttship.GrEstoque                = int(c-valor).                   
          IF hField:NAME =  "Familia              "   THEN  ttship.Familia                  = c-valor.                   
          IF hField:NAME =  "EmpresaSol           "   THEN  ttship.EmpresaSol               = c-valor.                   
          IF hField:NAME =  "EstabelecimentoSol   "   THEN  ttship.EstabelecimentoSol       = c-valor.                   
/*           IF hField:NAME =  "questfiscal          "   THEN  ttship.questfiscal              = c-valor. */
          IF hField:NAME =  "QuestFiscalUsu       "   THEN ASSIGN ttship.QuestFiscalUsu     = c-valor.                   
          IF hField:NAME =  "QuestFiscalData      "   THEN  ttship.QuestFiscalData          = c-valor .                    
          IF hField:NAME =  "QuestFiscalHora      "   THEN  ttship.QuestFiscalHora          = c-valor.                 
          IF hField:NAME =  "FormSupCtrleQtdeEstoque" THEN ttship.FormSupCtrleQtdeEstoque   = c-valor.               
          IF hField:NAME =  "FormSupQtdeMinEstoque "  THEN  ttship.FormSupQtdeMinEstoque    = c-valor.               
          IF hField:NAME =  "FormSupFamComEqpto  "    THEN  ttship.FormSupFamComEqpto       = c-valor.                
          IF hField:NAME =  "FormSupDepart    "       THEN  ttship.FormSupDepart            = c-valor.                 

     END.
 

 END.


 DELETE OBJECT hDoc.
 DELETE OBJECT hRoot.
 DELETE OBJECT hTable.
 DELETE OBJECT hField.
 DELETE OBJECT hText.
 


 /* show data made it by displaying temp-table */
 
