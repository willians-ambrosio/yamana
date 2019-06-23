/****************************************************************************
**
**    MI0408.i - Include para verificar relacionamentos da Ord-Manut
**
****************************************************************************/


if  NOT can-find(first tt-tag-equipto
                 where tt-tag-equipto.cd-tag = ord-manut.cd-tag) then
    next.

&if defined (bf_mnt_ems203) &then
find mnt-planejador where mnt-planejador.cd-planej = ord-manut.cd-planejado
     no-lock no-error.
&else        
find planejad where planejad.cd-planej = ord-manut.cd-planejado
     no-lock no-error.
&endif    
find centro-custo
    where centro-custo.cc-codigo = equipto.cc-codigo
    no-lock no-error.
find manut
    where manut.cd-manut = ord-manut.cd-manut
    no-lock no-error.
find tag
    where tag.cd-tag = ord-manut.cd-tag
    no-lock no-error.
find equipe
    where equipe.cd-equipe = ord-manut.cd-equip-res 
    no-lock no-error.

/* Fim do Include */
