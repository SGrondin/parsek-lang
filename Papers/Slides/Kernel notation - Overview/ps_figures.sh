
for i in *.fig 
do 
b=`echo $i | sed s/"\(.*\).fig"/"\1"/`
fig2dev -L pstex < $i > $b.eps
fig2dev -L pstex_t -p $b.eps < $i > $b.pstex_t
done
