#!/usr/bin/perl
#This is simple demo script showing some basic operations. 
#It initializes the serial port,
#Waits for ring on the modem, when a call comes, play the file greet,
#takes the dtmf code from the caller and finally says sorry and plays , 
#the dtmf code received in numeriacal format, date, and some text.

require 5.000;
use Ivrs;

#you must specify your serial port where voice modem is connected
$portname=$ARGV[0];
die "\n usage: demo1.pl ttyS0 or ttyS1\n\n" if ($portname eq "");

#Set the voice file directory. The absolute path is required if you want to
#run the IVRS from the inittab.

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

#pick up the receiver when a call come and play the greeting message.
#and collect 4 digits of dtmf code, returned in $accno
$accno=$iv->playfile("bgreet","4")||&closeall;
$iv->playfile||&closeall;

print "The caller has punched $accno dtmf codes \n";

#add file sory and waccno to say "Sorry" and "Wrong account Number"
#along with received number to a file (set in Ivrs.pm as $tmpmsg) 
$iv->addval($accno);
$iv->addmsg("sory");
$iv->addmsg("waccno");
$iv->addate("12122000");
$iv->addtxt("ABCDE");

#play the final file and accept one DTMF just to stop the playing of message. 
$iv->playfile("",1)||&closeall;

#close the port and say thank you.
$iv->addmsg("thank");
$iv->playfile||&closeall;
&closeall;
exit 1;

sub closeall
{
$iv->closep;
exit 1;
}

