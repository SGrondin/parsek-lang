
for i in *.fig 
do 
b=`echo $i | sed s/"\(.*\).fig"/"\1"/`
fig2dev -L pdftex < $i > $b.pdf
fig2dev -L pdftex_t -p $b.pdf < $i > $b.pstex_t
done
