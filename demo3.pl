#!/usr/bin/perl
#This is simple demo script showing some basic operations. 
#It initializes the serial port,
#Waits for ring on the modem, when a call comes, sends a text file
#faxmsg.txt over fax,

require 5.000;
use Ivrs;

#you must specify your serial port where voice modem is connected
$portname =$ARGV[0];
die "\n usage: demo3.pl ttyS0 or ttyS1\n\n" if ($portname eq "");

#Set the voice file directory in Ivrs.pm. The absolute path is required if you want to
#run the IVRS from other directory or inittab.


#initialize the serial port
$iv = new Ivrs($portname);
print "Serial port and Modem initialized\n";

#set the serial port parameters if you are sure.
$iv->setport("38400","none","8","1","rts","8096")||die"Setting Failed\n";

#initialize the modem and put it in voice mode.
$iv->initmodem||die"Modem failed\n";

#put the modem in answer mode $cid will have the caller ID
$cid=$iv->waitring;
print "Call received from $cid \n";

#pick up the receiver when a call come and play the instruction to stat fax
#machine.
$iv->playfile("faxst")||&closeall;

#Put the modem in fax mode
$iv->faxmode;
sleep 1;

#send fax using efix and efax. The modem and IVRS (at present) can not
#recover from fax mode, so this must be last message and close down is required.
#uncomment following line if you do want messages from efax and efix.
#open (STDOUT,">>/dev/null");
#open (STDERR,">>/dev/null");

system "efix -p8x3 -n /tmp/faxmsg faxmsg.txt";
system "efax -d /dev/$portname -o1 -jX3 -t T /tmp/faxmsg";
unlink ("/tmp/faxmsg");


#close every thing
&closeall;
exit 1;

sub closeall
{
$iv->closep;
exit 1;
}

