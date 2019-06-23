DEF TEMP-TABLE tt-beneficio NO-UNDO
    FIELD cod-beneficio AS INT
    FIELD rid-ben       AS ROWID
    FIELD TP-TAX        AS CHAR
    FIELD PRIOR         AS INT.

DEFINE INPUT PARAMETER p-estab        AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER p-it-codigo    AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER p-cod-emitente AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tt-beneficio.

DEFINE VARIABLE c-uf-estab    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-uf-emitente AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cfa         AS CHARACTER   NO-UNDO.

def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.


/* FIND  ext-item-cfa WHERE ext-item-cfa.it-codigo = p-it-codigo NO-LOCK NO-ERROR. */
/* IF NOT AVAIL ext-item-cfa THEN DO:                                              */
/*     MESSAGE "CFA NÇO CADASTRADA!!!"                                             */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                                      */
/*     RETURN "nok".                                                               */
/* END.                                                                            */


 FIND estabelec WHERE estabelec.cod-estabel = p-estab NO-LOCK NO-ERROR.
 IF AVAIL estabelec THEN DO:
     c-uf-estab = estabelec.estado.
 END.
 FIND emitente WHERE emitente.cod-emitente = p-cod-emitente NO-LOCK NO-ERROR.
 IF AVAIL emitente THEN DO:
     c-uf-emitente = emitente.estado.
 END.

es-movto-ext-item-cfa:
FOR EACH es-movto-ext-item-cfa  
    WHERE es-movto-ext-item-cfa.it-codigo   = p-it-codigo
      AND es-movto-ext-item-cfa.ep-codigo   = i-ep-codigo-usuario
    NO-LOCK
    BY es-movto-ext-item-cfa.identificador DESC  
    BY es-movto-ext-item-cfa.nr-seq :
      
    ASSIGN c-cfa = es-movto-ext-item-cfa.classe.  
    LEAVE es-movto-ext-item-cfa.
END.

IF c-cfa = "" THEN DO:
/*     MESSAGE "CFA NÇO CADASTRADA!!!"        */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    RETURN "CFA".
END.


FIND ITEM WHERE ITEM.it-codigo = p-it-codigo NO-LOCK NO-ERROR.

FOR EACH es-ben-estab WHERE es-ben-estab.cod-estabel  =  p-estab
                       AND  es-ben-estab.dt-ini-vald  <= TODAY
                       AND  es-ben-estab.dt-fim-vald  >= TODAY
                       AND (es-ben-estab.icms-ben-icms-est   = YES OR
                            es-ben-estab.icms-ben-icms-inter = YES OR
                            es-ben-estab.icms-ben-icms-imp   = YES)
                       BY   es-ben-estab.icms-prior :
 

     IF p-cod-emitente <> 0 THEN DO:
         /*FIND estabelec WHERE estabelec.cod-estabel = p-estab NO-LOCK NO-ERROR.
         IF AVAIL estabelec THEN DO:
             c-uf-estab = estabelec.estado.
         END.
         FIND emitente WHERE emitente.cod-emitente = p-cod-emitente NO-LOCK NO-ERROR.
         IF AVAIL emitente THEN DO:
             c-uf-emitente = emitente.estado.
         END.*/

         IF c-uf-estab = c-uf-emitente THEN DO:
             IF es-ben-estab.icms-ben-icms-inter = YES THEN.
             ELSE DO:
                IF es-ben-estab.icms-ben-icms-imp   = YES THEN DO:
                   
                    IF emitente.pais <> "brasil" THEN.
                    ELSE NEXT.

                END.
                ELSE NEXT.
             END.

             
         END.
         ELSE DO:
            IF es-ben-estab.icms-ben-icms-est   = YES THEN.
            ELSE DO:
                 IF es-ben-estab.icms-ben-icms-imp   = YES THEN DO:
                     IF emitente.pais <> "brasil" THEN.
                     ELSE NEXT.
                 END.
                 ELSE NEXT.
            END.
         END.
     END.


     FIND es-beneficio-cfa WHERE es-beneficio-cfa.cod-beneficio = es-ben-estab.cod-beneficio
                           AND   es-beneficio-cfa.classe        = c-cfa
                           NO-LOCK NO-ERROR.

     IF AVAIL es-beneficio-cfa  THEN DO:
         CREATE tt-beneficio.
         ASSIGN tt-beneficio.cod-beneficio = es-beneficio-cfa.cod-beneficio
                tt-beneficio.rid-ben = ROWID(es-ben-estab)
                tt-beneficio.tp-tax  = "ICMS" 
                tt-beneficio.prior   = es-ben-estab.icms-prior.
     END.


     FIND es-beneficio-ncm WHERE es-beneficio-ncm.cod-beneficio = es-ben-estab.cod-beneficio
                           AND es-beneficio-ncm.class-fisc-ini <= item.class-fiscal
                           AND es-beneficio-ncm.class-fisc-fim >= item.class-fiscal
                           NO-LOCK NO-ERROR.

     IF AVAIL es-beneficio-ncm  THEN DO:
         CREATE tt-beneficio.                                          
         ASSIGN tt-beneficio.cod-beneficio = es-beneficio-ncm.cod-beneficio
                tt-beneficio.rid-ben = ROWID(es-ben-estab)
                tt-beneficio.tp-tax  = "ICMS"
                tt-beneficio.prior   = es-ben-estab.icms-prior.
                
     END.

   
END.


FOR EACH es-ben-estab WHERE es-ben-estab.cod-estabel                       =  p-estab
                      AND   es-ben-estab.dt-ini-vald                      <= TODAY
                      AND   es-ben-estab.dt-fim-vald                      >= TODAY
                      AND   (es-ben-estab.pis-cofins-ben-pis-cof-compr-nac = YES OR
                             es-ben-estab.pis-cofins-ben-pis-cof-imp       = YES)
                      BY    es-ben-estab.pis-cofins-prioridade :


       IF AVAIL(emitente) THEN DO:
           
           IF emitente.pais <> "brasil" THEN DO:
               IF es-ben-estab.pis-cofins-ben-pis-cof-imp <> TRUE THEN NEXT.
           END.
           ELSE DO:
               IF es-ben-estab.pis-cofins-ben-pis-cof-compr-nac <> TRUE  THEN NEXT.
           END.

       END.

       FIND es-beneficio-cfa WHERE es-beneficio-cfa.cod-beneficio = es-ben-estab.cod-beneficio        
                             AND   es-beneficio-cfa.classe        = c-cfa               
                             NO-LOCK NO-ERROR.                                                        
                                                                                                      
       IF AVAIL es-beneficio-cfa  THEN DO: 

           CREATE tt-beneficio.                                                                       
           ASSIGN tt-beneficio.cod-beneficio = es-beneficio-cfa.cod-beneficio
                   tt-beneficio.rid-ben      = ROWID(es-ben-estab)
                   tt-beneficio.tp-tax       = "PIS"
                   tt-beneficio.prior        = es-ben-estab.pis-cofins-prioridade.
       END.                                                                                           
                                                                                                      
       FIND ITEM WHERE ITEM.it-codigo = p-it-codigo NO-LOCK NO-ERROR.  


       FIND es-beneficio-ncm WHERE es-beneficio-ncm.cod-beneficio = es-ben-estab.cod-beneficio        
                             AND es-beneficio-ncm.class-fisc-ini <= item.class-fiscal                 
                             AND es-beneficio-ncm.class-fisc-fim >= item.class-fiscal                 
                             NO-LOCK NO-ERROR.                                                        
                                                                                                      
       IF AVAIL es-beneficio-ncm  THEN DO: 

           CREATE tt-beneficio.                                                                       
           ASSIGN tt-beneficio.cod-beneficio = es-beneficio-ncm.cod-beneficio
                  tt-beneficio.rid-ben       = ROWID(es-ben-estab)
                  tt-beneficio.tp-tax        = "PIS"
                  tt-beneficio.prior         = es-ben-estab.pis-cofins-prioridade.
       END.
      
END.


