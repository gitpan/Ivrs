#!/usr/bin/perl -w
#This will convert a wave file to rmd (Raw Modem data) file

if ($ARGV[0] eq "")
{
print "\nUsage convw2l filename\n\n";
exit;
}
if (-e $ARGV[0])
{

($finfile,$fileext)=split (/\./,$ARGV[0]);
system "wavtopvf $ARGV[0] /tmp/tmp1";
system "pvfspeed -s 7200 /tmp/tmp1 /tmp/tmp2";
system "pvftormd Rockwell 4 /tmp/tmp2 $finfile";
print "File $finfile."."$fileext converted to rmd file $finfile\n\n"; 
}
else
{
print "\nFile does not exist. Please check.\n\n";
#exit;
}

exit;