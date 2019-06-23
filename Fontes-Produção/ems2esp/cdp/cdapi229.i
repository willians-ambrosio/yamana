/*************************************************************************
** 
**  Programa: CDAPI229.I
** 
**  Objetivo: Defini‡Æo das temp-tables usadas na API cdapi229.p
**
*************************************************************************/

def temp-table tt-emitente-aux like emitente
    INDEX ch-codigo IS PRIMARY cod-emitente. 
    
def temp-table tt-emitente like emitente
    field cod-maq-origem   as   integer format "9999"
    field num-processo     as   integer format ">>>>>>>>9" initial 0
    field num-sequencia    as   integer format ">>>>>9"    initial 0
    field ind-tipo-movto   as   integer format "99"        initial 1
    INDEX ch-codigo IS PRIMARY  cod-maq-origem
                                num-processo
                                num-sequencia.
                                
&IF DEFINED(BF_MAT_BLOQUEIO_FORNEC) &THEN  
    def temp-table tt-dist-emitente-aux like dist-emitente
        INDEX ch-codigo IS PRIMARY cod-emitente. 
    
    def temp-table tt-dist-emitente like dist-emitente
        field cod-maq-origem   as   integer format "9999"
        field num-processo     as   integer format ">>>>>>>>9" initial 0
        field num-sequencia    as   integer format ">>>>>9"    initial 0
        field ind-tipo-movto   as   integer format "99"        initial 1
        INDEX ch-codigo IS PRIMARY  cod-maq-origem
                                    num-processo
                                    num-sequencia.
&ENDIF

def temp-table tt-loc-entr-aux like loc-entr
    INDEX ch-codigo IS PRIMARY nome-abrev
                               cod-entrega. 
    
def temp-table tt-loc-entr like loc-entr
    field cod-maq-origem   as   integer format "9999"
    field num-processo     as   integer format ">>>>>>>>9" initial 0
    field num-sequencia    as   integer format ">>>>>9"    initial 0
    field ind-tipo-movto   as   integer format "99"        initial 1
    INDEX ch-codigo IS PRIMARY  cod-maq-origem
                                num-processo
                                num-sequencia.                                
                                    
                         


    
