DEF VAR i-num-ord-inv LIKE ordem-compra.num-ord-inv.
DEF VAR d-vl-saldo    LIKE ordem-inv.vl-verba[1].

FOR EACH ordem-compra
         WHERE ordem-compra.num-ord-inv <> 0
         BY ordem-compra.numero-ordem DESCENDING:

   RUN pi-verba (INPUT  ordem-compra.num-ord-inv,
                 OUTPUT d-vl-saldo). 

   DISPLAY ordem-compra.numero-ordem
           ordem-compra.num-ord-inv
           d-vl-saldo WITH SCROLLABLE .

   LEAVE.
END.
                    



PROCEDURE pi-verba:
   DEFINE INPUT  PARAMETER ip-num-ord-inv LIKE ordem-compra.num-ord-inv NO-UNDO.
   DEFINE OUTPUT PARAMETER ip-saldo       LIKE ordem-inv.vl-verba[1]    NO-UNDO.

   DEFINE VARIABLE i-numero-ordem LIKE ordem-compra.numero-ordem.

   ASSIGN ip-saldo = 0.
   
   blk:
   FOR FIRST sub-div-ordem
             WHERE sub-div-ordem.num-ord-magnus = ordem-compra.num-ord-inv
             NO-LOCK,
       FIRST ordem-inv OF sub-div-ordem
             NO-LOCK:
      FOR EACH controle-verba  
               WHERE controle-verba.ep-codigo    = ordem-inv.ep-codigo    AND
                     controle-verba.cod-est-exec = ordem-inv.cod-est-exec AND
                     controle-verba.num-projeto  = ordem-inv.num-projeto  AND
                     controle-verba.num-ordem    = ordem-inv.num-ordem   
               USE-INDEX emp-ord 
               NO-LOCK:

          MESSAGE 
              ordem-inv.ep-codigo       skip
              ordem-inv.cod-est-exec    skip
              ordem-inv.num-projeto     skip
              ordem-inv.num-ordem       skip

              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      
         ASSIGN ip-saldo = ip-saldo + (ordem-inv.vl-verba[1] - (controle-verba.vl-comp[1] + controle-verba.vl-real[1])).
           
      END.
   END.
END PROCEDURE.
