#!/usr/bin/perl
#ver 0.01
#This is simple demo script showing some basic operations. 
#It initializes the serial port,
#Waits for ring on the modem, when a call comes, it records the caller message
#and plays the same message to caller

require 5.000;
use Ivrs;
$portname=$ARGV[0];
die "\n usage:demo2.pl ttyS0 or ttyS1\n\n" if ($portname eq "");


#initialize the serial port
$iv = new Ivrs($portname);
print "Serial port and Modem initialized\n";

#set the serial port parameters if you are sure.
$iv->setport("38400","none","8","1","rts","1024")||die"Setting Failed\n";

#initialize the modem and put it in voice mode.
$iv->initmodem||die"Modem failed";

#put the modem in answer mode $cid will have the caller ID
$cid=$iv->waitring;

#pick up the receiver when a call come and play the greeting message.
#and collect 4 digits of dtmf code, returned in $accno
$iv->playfile("recmail",0)||&closeall;


#Record the caller message for 10 seconds and play the same to caller. 
$iv->recfile("/tmp/inmsg",10)||&closeall;

#end of message recording.
$iv->playfile("beep500")||&closeall;

#play the recorded message.
$iv->playfile("/tmp/inmsg")||&closeall;

#close the port and say thank you.
sleep 2;
$iv->addmsg("thank");
$iv->playfile||&closeall;
&closeall;
exit;

sub closeall
{
$iv->closep;
exit 1;
}

