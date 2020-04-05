sleep 10


vhostname=$(hostname)

varch="$HOME/cloud/cloud1/exec/$vhostname.r"

echo $varch
if [  -f $varch ]; then
    cd
    mkdir tmp
    mkdir  $HOME/cloud/cloud1/work/$vhostname
    cat  $HOME/tmp/log.txt  >>  $HOME/cloud/cloud1/work/$vhostname/log.txt
    cd tmp
    rm *
    echo "Archivo $varch  Encontrado!"

    echo -n "Rscript --vanilla  " > "$HOME/tmp/run.sh"
    cat  $varch >>  "$HOME/tmp/run.sh"
    echo ""   >>  "$HOME/tmp/run.sh"
    echo "cat  $HOME/tmp/log.txt  >> $HOME/cloud/cloud1/work/$vhostname/log.txt"  >> "$HOME/tmp/run.sh"
    echo "rm $HOME/tmp/log.txt" >> "$HOME/tmp/run.sh"
    echo "sudo  poweroff"  >>  "$HOME/tmp/run.sh"

    chmod +x  $HOME/tmp/run.sh

    nohup  ./run.sh  > $HOME/tmp/log.txt 2>&1 &
else
    varch="$HOME/cloud/cloud1/exec/$vhostname.sh"
    if [  -f $varch ]; then
        cd
        mkdir tmp
        mkdir  $HOME/cloud/cloud1/work/$vhostname
        cat  $HOME/tmp/log.txt  >>  $HOME/cloud/cloud1/work/$vhostname/log.txt
        cd tmp
        rm *
        echo "Archivo $varch  Encontrado!"
       
        cat  $varch >>  "$HOME/tmp/run.sh"
    
        chmod +x  $HOME/tmp/run.sh

        nohup  ./run.sh  > $HOME/tmp/log.txt  2>&1 &
fi	

fi
