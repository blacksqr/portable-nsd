#Change the tag
rm -rf d:/temp/portable.nsd
mkdir d:/temp/portable.nsd
cd d:/temp
#cvs co -r test_tag portable.nsd
cvs co portable.nsd
cd d:/temp/portable.nsd
cvs co nstcl-1.0
cd d:/temp
find d:/temp/portable.nsd/ -name CVS | xargs rm -rf
find d:/temp/portable.nsd/ -name *.flc | xargs rm -rf
cd d:/temp
rm portable.nsd.tgz
tar -zvcf portable.nsd.tgz portable.nsd/*
#cd d:/projects/portable.nsd
#scp /cygdrive/d/temp/portable.nsd.tgz root@192.168.101.1:/home/e-smith/files/ibays/portable.nsd/html