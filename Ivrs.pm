#Perl module to implement a full functional Inter Active Voice Response
#System using standard voice modem. I have *taken* some codes from 
#SerialPort.pm for serial port
#access, with due respect to Bill Birthisel.

package SerialJunk;
# this is the linux path. Need to determine location on other OSs
use vars qw($ioctl_ok);
eval { require 'asm/termios.ph'; };
if ($@) {
   $ioctl_ok = 0;
##   print "error message: $@\n"; ## DEBUG ##
}
else {
   $ioctl_ok = 1;
}

package Ivrs;
use POSIX qw(:termios_h);
use IO::Handle;

use vars qw($bitset $bitclear $rtsout $dtrout $getstat $incount $outcount
	    $txdone);
if ($SerialJunk::ioctl_ok) {
    $bitset = &SerialJunk::TIOCMBIS;
    $bitclear = &SerialJunk::TIOCMBIC;
    $getstat = &SerialJunk::TIOCMGET;
    $incount = &SerialJunk::TIOCINQ;
    $outcount = &SerialJunk::TIOCOUTQ;
    $txdone = &SerialJunk::TIOCSERGETLSR;
    $rtsout = pack('L', &SerialJunk::TIOCM_RTS);
    $dtrout = pack('L', &SerialJunk::TIOCM_DTR);
}
else {
    $bitset = 0;
    $bitclear = 0;
    $statget = 0;
    $incount = 0;
    $outcount = 0;
    $txdone = 0;
    $rtsout = pack('L', 0);
    $dtrout = pack('L', 0);
}


sub TIOCM_LE1 {
    if (defined &SerialJunk::TIOCSER_TEMT) { return &SerialJunk::TIOCSER_TEMT; }
    if (defined &SerialJunk::TIOCM_LE) { return &SerialJunk::TIOCM_LE; }
    0;
}

use Carp;
use strict;

use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
$VERSION = '0.03';

require Exporter;

@ISA = qw(Exporter);
@EXPORT= qw();
@EXPORT_OK= qw();
%EXPORT_TAGS = (STAT	=> [qw( MS_CTS_ON	MS_DSR_ON
				MS_RING_ON	MS_RLSD_ON
				ST_BLOCK	ST_INPUT
				ST_OUTPUT	ST_ERROR )],

		PARAM	=> [qw( LONGsize	SHORTsize	OS_Error
				nocarp		yes_true )]);

Exporter::export_ok_tags('STAT', 'PARAM');

$EXPORT_TAGS{ALL} = \@EXPORT_OK;

# Linux-specific constant for Hardware Handshaking
sub CRTSCTS { 020000000000 }

# Linux-specific Baud-Rates
sub B57600  { 0010001 }
sub B115200 { 0010002 }
sub B230400 { 0010003 }
sub B460800 { 0010004 }

my %bauds = (
             9600     => B9600,
             19200    => B19200,
             38400    => B38400,
             # These are Linux-specific
             57600    => B57600,
             115200   => B115200,
             230400   => B230400,
             460800   => B460800,
             );


my %c_cc_fields = (
		   VEOF     => &POSIX::VEOF,
		   VEOL     => &POSIX::VEOL,
		   VERASE   => &POSIX::VERASE,
		   VINTR    => &POSIX::VINTR,
		   VKILL    => &POSIX::VKILL,
		   VQUIT    => &POSIX::VQUIT,
		   VSUSP    => &POSIX::VSUSP,
		   VSTART   => &POSIX::VSTART,
		   VSTOP    => &POSIX::VSTOP,
		   VMIN     => &POSIX::VMIN,
		   VTIME    => &POSIX::VTIME,
		   );
# Set your directories, tmp files and log files here. You need absolute path 
#for all the directory and files if you are running IVRS from the different 
#directory or vis /etc/inittab
my $vdir="sfiles";
my $bindir="bin";
my $logfile="/var/log/ivrs";
my $tmpmsg="/tmp/ivrstmp.1";

my $Babble = 0;
my $testactive = 0;	# test mode active
my @Yes_resp = (
		"YES", "Y",
		"ON",
		"TRUE", "T",
		"1"
		);

my @binary_opt = ( 0, 1 );
my @byte_opt = (0, 255);

## my $null=[];
my $null=0;
my $zero=0;

sub nocarp { return $testactive }

sub yes_true {
    my $choice = uc shift;
    my $ans = 0;
    foreach (@Yes_resp) { $ans = 1 if ( $choice eq $_ ) }
    return $ans;
}

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self  = {};
    my $ok    = 0;		# API return value

    my $item = 0;
    $self->{NAME} = shift;
    open (LOG, ">>$logfile.$self->{NAME}")||die"Log file failed";
    print LOG "\n-----------Access Log file for IVRS-----------\n";
    print LOG `date`;
    $self->{NAME}="/dev/".$self->{NAME} ;
    my $tmpdir=shift;
    $vdir=$tmpdir if $tmpdir ne "";
    $tmpdir=shift;
    $bindir=$tmpdir if $tmpdir ne "";
    my $quiet = shift;
    unless ($quiet or ($bitset && $bitclear && $rtsout && $dtrout) ) {
       nocarp or warn "disabling ioctl methods - constants not found\n";
    }

    my $lockfile = shift;
    if ($lockfile) {
        $self->{LOCK} = $lockfile;
	my $lockf = POSIX::open($self->{LOCK}, 
				    &POSIX::O_WRONLY |
				    &POSIX::O_CREAT |
				    &POSIX::O_NOCTTY |
				    &POSIX::O_EXCL);
	unless (defined $lockf) {
            unless ($quiet) {
                nocarp or carp "can't open lockfile: $self->{LOCK}\n"; 
            }
            return 0 if ($quiet);
	    return;
	}
	my $pid = "$$\n";
	$ok = POSIX::write($lockf, $pid, length $pid);
	my $ok2 = POSIX::close($lockf);
	return unless ($ok && (defined $ok2));
	sleep 2;	# wild guess for Version 0.05
    }
    else {
        $self->{LOCK} = "";
    }

    $self->{FD}= POSIX::open($self->{NAME}, 
				    &POSIX::O_RDWR |
				    &POSIX::O_NOCTTY |
				    &POSIX::O_NONBLOCK);

    unless (defined $self->{FD}) { $self->{FD} = -1; }
    unless ($self->{FD} >= 1) {
        unless ($quiet) {
            nocarp or carp "can't open device: $self->{NAME}\n"; 
        }
        $self->{FD} = -1;
        if ($self->{LOCK}) {
	    $ok = unlink $self->{LOCK};
	    unless ($ok or $quiet) {
                nocarp or carp "can't remove lockfile: $self->{LOCK}\n"; 
    	    }
            $self->{LOCK} = "";
        }
        return 0 if ($quiet);
	return;
    }

    $self->{TERMIOS} = POSIX::Termios->new();

    # a handle object for ioctls: read-only ok
    $self->{HANDLE} = new_from_fd IO::Handle ($self->{FD}, "r");
    
    # get the current attributes
    $ok = $self->{TERMIOS}->getattr($self->{FD});

    unless ( $ok ) {
        carp "can't getattr";
        undef $self;
        return undef;
    }

    # save the original values
    $self->{"_CFLAG"} = $self->{TERMIOS}->getcflag();
    $self->{"_IFLAG"} = $self->{TERMIOS}->getiflag();
    $self->{"_ISPEED"} = $self->{TERMIOS}->getispeed();
    $self->{"_LFLAG"} = $self->{TERMIOS}->getlflag();
    $self->{"_OFLAG"} = $self->{TERMIOS}->getoflag();
    $self->{"_OSPEED"} = $self->{TERMIOS}->getospeed();

   foreach $item (keys %c_cc_fields) {
	$self->{"_$item"} = $self->{TERMIOS}->getcc($c_cc_fields{$item});
    }

    # copy the original values into "current" values
    foreach $item (keys %c_cc_fields) {
	$self->{"C_$item"} = $self->{"_$item"};
    }

    $self->{"C_CFLAG"} = $self->{"_CFLAG"};
    $self->{"C_IFLAG"} = $self->{"_IFLAG"};
    $self->{"C_ISPEED"} = $self->{"_ISPEED"};
    $self->{"C_LFLAG"} = $self->{"_LFLAG"};
    $self->{"C_OFLAG"} = $self->{"_OFLAG"};
    $self->{"C_OSPEED"} = $self->{"_OSPEED"};

    # Finally, default to "raw" mode for this package
    $self->{"C_IFLAG"} &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|IGNCR|ICRNL|IXON);
    $self->{"C_OFLAG"} &= ~OPOST;
    $self->{"C_LFLAG"} &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN);
    $self->{"C_CFLAG"} &= ~(CSIZE|PARENB);
    $self->{"C_CFLAG"} |= (CS8|CLOCAL);
    &write_settings($self);

    $self->{ALIAS} = $self->{NAME};	# so "\\.\+++" can be changed
##    print "opening $self->{NAME}\n"; ## DEBUG ##

    # "private" data
#    $self->{"_DEBUG"}    	= 0;
#    $self->{U_MSG}     		= 0;
#    $self->{E_MSG}     		= 0;
#    $self->{RCONST}   		= 0;
#    $self->{RTOT}   		= 0;
#    $self->{"_T_INPUT"}		= "";
#    $self->{"_LOOK"}		= "";
#    $self->{"_LASTLOOK"}	= "";
#    $self->{"_LASTLINE"}	= "";
#    $self->{"_CLASTLINE"}	= "";
#    $self->{"_SIZE"}		= 1;
#    $self->{OFS}		= "";
#    $self->{ORS}		= "";
#    $self->{"_LMATCH"}		= "";
#    $self->{"_LPATT"}		= "";
#    $self->{"_PROMPT"}		= "";
#    $self->{"_MATCH"}		= [];
#    $self->{"_CMATCH"}		= [];
#    @{ $self->{"_MATCH"} }	= "\n";
#    @{ $self->{"_CMATCH"} }	= "\n";
#    $self->{DVTYPE}		= "none";
#    $self->{HNAME}		= "localhost";
#    $self->{HADDR}		= 0;
#    $self->{DATYPE}		= "raw";
#    $self->{CFG_1}		= "none";
#    $self->{CFG_2}		= "none";
#    $self->{CFG_3}		= "none";
    print LOG "Port $self->{NAME} opened by IVRS\n";
    bless ($self, $class);
    return $self;
}

sub write_settings {
    my $self = shift;
    my $item;

    # put current values into Termios structure
    $self->{TERMIOS}->setcflag($self->{"C_CFLAG"});
    $self->{TERMIOS}->setiflag($self->{"C_IFLAG"});
    $self->{TERMIOS}->setispeed($self->{"C_ISPEED"});
    $self->{TERMIOS}->setlflag($self->{"C_LFLAG"});
    $self->{TERMIOS}->setoflag($self->{"C_OFLAG"});
    $self->{TERMIOS}->setospeed($self->{"C_OSPEED"});

    foreach $item (keys %c_cc_fields) {
	$self->{TERMIOS}->setcc($c_cc_fields{$item}, $self->{"C_$item"});
    }

    $self->{TERMIOS}->setattr($self->{FD}, &POSIX::TCSANOW);

    if ($Babble) {
        print "writing settings to $self->{ALIAS}\n";
    }
    1;
}

sub readport {
    return undef unless (@_ == 2);
    my $self = shift;
    my $wanted = shift;
    my $result = "";
    my $ok     = 0;
    return unless ($wanted > 0);

    my $done = 0;
    my $count_in = 0;
    my $string_in = "";
    my $in2 = "";
    my $bufsize = 255;	# VMIN max (declared as char)

    while ($done < $wanted) {
	my $size = $wanted - $done;
        if ($size > $bufsize) { $size = $bufsize; }
	($count_in, $string_in) = $self->read_vmin($size);
	if ($count_in) {
            $in2 .= $string_in;
	    $done += $count_in;
	}
	elsif ($done) {
	    last;
	}
        else {
            return if (!defined $count_in);
	    last;
        }
    }
    return ($done, $in2);
}

sub read_vmin {
    return undef unless (@_ == 2);
    my $self = shift;
    my $wanted = shift;
    my $result = "";
    my $ok     = 0;
    return unless ($wanted > 0);

    if ($self->{"C_VMIN"} != $wanted) {
	$self->{"C_VMIN"} = $wanted;
        write_settings($self);
    }
    my $rin = "";
    vec($rin, $self->{FD}, 1) = 1;
    my $ein = $rin;
    my $tin = $self->{RCONST} + ($wanted * $self->{RTOT});
    my $rout;
    my $wout;
    my $eout;
    my $tout;
    my $ready = select($rout=$rin, $wout=undef, $eout=$ein, $tout=$tin);

    my $got = POSIX::read ($self->{FD}, $result, $wanted);

    unless (defined $got) {
##	$got = -1;	## DEBUG
	return (0,"") if (&POSIX::EAGAIN == ($ok = POSIX::errno()));
	return (0,"") if (!$ready and (0 == $ok));
		# at least Solaris acts like eof() in this case
	carp "Error #$ok in Device::SerialPort::read";
	return;
    }

    print "read_vmin=$got, ready=$ready, result=..$result..\n" if ($Babble);
    return ($got, $result);
}

sub input {
    return undef unless (@_ == 1);
    my $self = shift;
    my $ok     = 0;
    my $result = "";
    my $wanted = 255;

    if (nocarp && $self->{"_T_INPUT"}) {
	$result = $self->{"_T_INPUT"};
	$self->{"_T_INPUT"} = "";
	return $result;
    }

    if ( $self->{"C_VMIN"} ) {
	$self->{"C_VMIN"} = 0;
	write_settings($self);
    }

    my $got = POSIX::read ($self->{FD}, $result, $wanted);

    unless (defined $got) { $got = -1; }
    if ($got == -1) {
	return "" if (&POSIX::EAGAIN == ($ok = POSIX::errno()));
	return "" if (0 == $ok);	# at least Solaris acts like eof()
	carp "Error #$ok in Device::SerialPort::input"
    }
    return $result;
}

sub write {
    return undef unless (@_ == 2);
    my $self = shift;
    my $wbuf = shift;
    my $ok;

    return 0 if ($wbuf eq "");
    my $lbuf = length ($wbuf);
    my $written = POSIX::write ($self->{FD}, $wbuf, $lbuf);

    return $written;
}

sub write_drain {
    my $self = shift;
    return if (@_);
    return 1 if (defined POSIX::tcdrain($self->{FD}));
    return;
}

sub purge_all {
    my $self = shift;
    return if (@_);
    return 1 if (defined POSIX::tcflush($self->{FD}, TCIOFLUSH));
    return;
}

sub dtr_active {
    return unless (@_ == 2);
    return unless ($bitset && $bitclear && $dtrout);
    my $self = shift;
    my $onoff = shift;
    # returns ioctl result
    if ($onoff) {
        ioctl($self->{HANDLE}, $bitset, $dtrout);
    }
    else {
        ioctl($self->{HANDLE}, $bitclear, $dtrout);
    }
}

sub rts_active {
    return unless (@_ == 2);
    return unless ($bitset && $bitclear && $rtsout);
    my $self = shift;
    my $onoff = shift;
    # returns ioctl result
    if ($onoff) {
        ioctl($self->{HANDLE}, $bitset, $rtsout);
    }
    else {
        ioctl($self->{HANDLE}, $bitclear, $rtsout);
    }
}

sub pulse_break_on {
    return unless (@_ == 2);
    my $self = shift;
    my $delay = (shift)/1000;
    my $length = 0;
    my $ok = POSIX::tcsendbreak($self->{FD}, $length);
    warn "could not pulse break on" unless ($ok);
    select (undef, undef, undef, $delay);
    return $ok;
}

sub pulse_rts_on {
    return unless (@_ == 2);
    return unless ($bitset && $bitclear && $rtsout);
    my $self = shift;
    my $delay = (shift)/1000;
    $self->rts_active(1) or warn "could not pulse rts on";
##    print "rts on\n"; ## DEBUG
    select (undef, undef, undef, $delay);
    $self->rts_active(0) or warn "could not restore from rts on";
##    print "rts_off\n"; ## DEBUG
    select (undef, undef, undef, $delay);
    1;
}

sub pulse_dtr_on {
    return unless (@_ == 2);
    return unless ($bitset && $bitclear && $dtrout);
    my $self = shift;
    my $delay = (shift)/1000;
    $self->dtr_active(1) or warn "could not pulse dtr on";
##    print "dtr on\n"; ## DEBUG
    select (undef, undef, undef, $delay);
    $self->dtr_active(0) or warn "could not restore from dtr on";
##    print "dtr_off\n"; ## DEBUG
    select (undef, undef, undef, $delay);
    1;
}

sub pulse_rts_off {
    return unless (@_ == 2);
    return unless ($bitset && $bitclear && $rtsout);
    my $self = shift;
    my $delay = (shift)/1000;
    $self->rts_active(0) or warn "could not pulse rts off";
##    print "rts off\n"; ## DEBUG
    select (undef, undef, undef, $delay);
    $self->rts_active(1) or warn "could not restore from rts off";
##    print "rts on\n"; ## DEBUG
    select (undef, undef, undef, $delay);
    1;
}

sub pulse_dtr_off {
    return unless (@_ == 2);
    return unless ($bitset && $bitclear && $dtrout);
    my $self = shift;
    my $delay = (shift)/1000;
    $self->dtr_active(0) or warn "could not pulse dtr off";
##    print "dtr off\n"; ## DEBUG
    select (undef, undef, undef, $delay);
    $self->dtr_active(1) or warn "could not restore from dtr off";
##    print "dtr on\n"; ## DEBUG
    select (undef, undef, undef, $delay);
    1;
}
sub baudrate {
    my $self = shift;
    my $item = 0;

    if (@_) {
        if (defined $bauds{$_[0]}) {
            $self->{"C_OSPEED"} = $bauds{$_[0]};
            $self->{"C_ISPEED"} = $bauds{$_[0]};
            write_settings($self);
        }
        else {
            if ($self->{U_MSG} or $Babble) {
                carp "Can't set baudrate on $self->{ALIAS}";
            }
            return undef;
        }
    }
    if (wantarray) { return (keys %bauds); }
    foreach $item (keys %bauds) {
        return $item if ($bauds{$item} == $self->{"C_OSPEED"});
    }
    return undef;
}
sub parity_enable {
    my $self = shift;
    if (@_) {
        if ( yes_true( shift ) ) {
            $self->{"C_IFLAG"} |= PARMRK;
            $self->{"C_CFLAG"} |= PARENB;
        } else {
            $self->{"C_IFLAG"} &= ~PARMRK;
            $self->{"C_CFLAG"} &= ~PARENB;
        }
        write_settings($self);
    }
    return wantarray ? @binary_opt : ($self->{"C_CFLAG"} & PARENB);
}

sub parity {
    my $self = shift;
    if (@_) {
	if ( $_[0] eq "none" ) {
	    $self->{"C_IFLAG"} &= ~INPCK;
	    $self->{"C_CFLAG"} &= ~PARENB;
	}
	elsif ( $_[0] eq "odd" ) {
	    $self->{"C_IFLAG"} |= INPCK;
	    $self->{"C_CFLAG"} |= (PARENB|PARODD);
	}
	elsif ( $_[0] eq "even" ) {
	    $self->{"C_IFLAG"} |= INPCK;
	    $self->{"C_CFLAG"} |= PARENB;
	    $self->{"C_CFLAG"} &= ~PARODD;
	}
        else {
            if ($self->{U_MSG} or $Babble) {
                carp "Can't set parity on $self->{ALIAS}";
            }
	    return;
        }
	write_settings($self);
    }
    if (wantarray) { return ("none", "odd", "even"); }
    return "none" unless ($self->{"C_IFLAG"} & INPCK);
    my $mask = (PARENB|PARODD);
    return "odd"  if ($mask == ($self->{"C_CFLAG"} & $mask));
    $mask = (PARENB);
    return "even" if ($mask == ($self->{"C_CFLAG"} & $mask));
    return "none";
}

sub databits {
    my $self = shift;
    if (@_) {
	if ( $_[0] == 8 ) {
	    $self->{"C_CFLAG"} &= ~CSIZE;
	    $self->{"C_CFLAG"} |= CS8;
	}
	elsif ( $_[0] == 7 ) {
	    $self->{"C_CFLAG"} &= ~CSIZE;
	    $self->{"C_CFLAG"} |= CS7;
	}
	elsif ( $_[0] == 6 ) {
	    $self->{"C_CFLAG"} &= ~CSIZE;
	    $self->{"C_CFLAG"} |= CS6;
	}
	elsif ( $_[0] == 5 ) {
	    $self->{"C_CFLAG"} &= ~CSIZE;
	    $self->{"C_CFLAG"} |= CS5;
	}
        else {
            if ($self->{U_MSG} or $Babble) {
                carp "Can't set databits on $self->{ALIAS}";
            }
	    return;
        }
	write_settings($self);
    }
    if (wantarray) { return (5, 6, 7, 8); }
    my $mask = ($self->{"C_CFLAG"} & CSIZE);
    return 8 if ($mask == CS8);
    return 7 if ($mask == CS7);
    return 6 if ($mask == CS6);
    return 5;
}

sub stopbits {
    my $self = shift;
    if (@_) {
	if ( $_[0] == 2 ) {
	    $self->{"C_CFLAG"} |= CSTOPB;
	}
	elsif ( $_[0] == 1 ) {
	    $self->{"C_CFLAG"} &= ~CSTOPB;
	}
        else {
            if ($self->{U_MSG} or $Babble) {
                carp "Can't set stopbits on $self->{ALIAS}";
            }
	    return; 
        }
	write_settings($self);
    }
    if (wantarray) { return (1, 2); }
    return 2 if ($self->{"C_CFLAG"} & CSTOPB);
    return 1;
}
  
sub handshake {
    my $self = shift;
    
    if (@_) {
	if ( $_[0] eq "none" ) {
	    $self->{"C_IFLAG"} &= ~(IXON | IXOFF);
	    $self->{"C_CFLAG"} &= ~CRTSCTS;
	}
	elsif ( $_[0] eq "xoff" ) {
	    $self->{"C_IFLAG"} |= (IXON | IXOFF);
	    $self->{"C_CFLAG"} &= ~CRTSCTS;
	}
	elsif ( $_[0] eq "rts" ) {
	    $self->{"C_IFLAG"} &= ~(IXON | IXOFF);
	    $self->{"C_CFLAG"} |= CRTSCTS;
	}
        else {
            if ($self->{U_MSG} or $Babble) {
                carp "Can't set handshake on $self->{ALIAS}";
            }
	    return;
        }
	write_settings($self);
    }
    if (wantarray) { return ("none", "xoff", "rts"); }
    my $mask = (IXON|IXOFF);
    return "xoff" if ($mask == ($self->{"C_IFLAG"} & $mask));
    return "rts" if ($self->{"C_CFLAG"} & CRTSCTS);
    return "none";
}

sub buffers {
    my $self = shift;
    if (@_) { return unless (@_ == 2); }
    return wantarray ?  (4096, 4096) : 1;
}

sub pclose {
    my $self = shift;
    my $ok = undef;
    my $item;

    return unless (defined $self->{NAME});

    if ($Babble) {
        carp "Closing $self " . $self->{ALIAS};
    }
    if ($self->{FD}) {
        purge_all ($self);

	# copy the original values into "current" values
	foreach $item (keys %c_cc_fields) {
	    $self->{"C_$item"} = $self->{"_$item"};
	}

	$self->{"C_CFLAG"} = $self->{"_CFLAG"};
	$self->{"C_IFLAG"} = $self->{"_IFLAG"};
	$self->{"C_ISPEED"} = $self->{"_ISPEED"};
	$self->{"C_LFLAG"} = $self->{"_LFLAG"};
	$self->{"C_OFLAG"} = $self->{"_OFLAG"};
	$self->{"C_OSPEED"} = $self->{"_OSPEED"};
	
	write_settings($self);

        $ok = POSIX::close($self->{FD});
	# also closes $self->{HANDLE}

	$self->{FD} = undef;
    }
    if ($self->{LOCK}) {
	unless ( unlink $self->{LOCK} ) {
            nocarp or carp "can't remove lockfile: $self->{LOCK}\n"; 
	}
        $self->{LOCK} = "";
    }
    $self->{NAME} = undef;
    $self->{ALIAS} = undef;
    close (LOG);
    return unless ($ok);
#    exit 0;
}

##My routines starts from here
#---------------------------------------------------------------------#
sub initmodem {
    my $self=shift;
    $self->pulse_dtr_on(500)||return undef;
    $self->pulse_dtr_off(500)||return undef;
    $self->atcomm("ATZ","OK") ||return undef; 
    $self->atcomm("AT&F1","OK") ||return undef;
    $self->atcomm("AT&C1&D2&K3M2L3","OK")||return undef; 
    $self->atcomm("AT#CLS=8","OK")||return undef;
    $self->atcomm("AT","OK")||return undef; 
    $self->atcomm("ATS91=50","OK")||return undef;
    $self->atcomm("AT#VBS=4","OK")||return undef;
    $self->atcomm("AT#VSP=2","OK")||return undef;
    $self->atcomm("AT#VTD=3F,3F,3F","OK")||return undef; 
    $self->atcomm("AT#VSR=7200","OK")||return undef;
    $self->atcomm("AT#VSD=1","OK")||return undef; 
    $self->atcomm("AT#BDR=16","OK")||return undef;
    $self->atcomm("AT#VLS=2","VCON")||return undef;
    #$self->atcomm("ATL3","OK")||return undef;
    unlink("$tmpmsg");
    print LOG "Port Configured for Voice \n";
}
sub setport {
    my $self=shift;
    my $baud=shift;
    my $parity=shift;
    my $data=shift;
    my $stop=shift;
    my $hand=shift;
    my $buff=shift;
    $self->baudrate($baud)||return undef;
    $self->parity($parity)||return undef;
    $self->databits($data)||return undef;
    $self->stopbits($stop)||return undef;
    $self->handshake($hand)||return undef;
    $self->buffers($buff,$buff)||return undef;
    $self->write_settings;
    print LOG "Port Configuration changed\n"||die "failed write";
    return 1;
}
		
sub waitring {
    my $self=shift;
    my $callid="";
    print LOG "Waiting for ring from ",`date`;
    $self->atcomm("AT#VLS=0","OK")||return undef;
    $self->atcomm("AT#CLS=8","OK")||return undef;
    while (!($self->input=~/[RING]/)){}
        $callid=$self->input;
        $self->atcomm("ATA","")||return undef;
        $self->atcomm("AT#VLS=0","VCON")||return undef;
        $self->atcomm("AT#VTX","CONNECT")||return undef;
        print LOG "Call from <$callid> received at ",`date`;
        return $callid;
}

sub atcomm {
    my $self=shift;
    my $atstr=shift;
    my $waitfor=shift;
    my $oltime=time;
    my $getstr="";
    #$atstr=$atstr."AT\r";
    $self->write("$atstr\r");
    while (!($getstr=~/$waitfor/)) {
        $getstr=$getstr.$self->input;
        if (((time - $oltime)>5)||($getstr=~/[b]/)) {
            print LOG "Modem failed to reply <$atstr> \n";
            #$self->pclose ;
            return undef;
        }
    }
    #return $getstr;
#    print "$atstr -> $getstr\n";
    return 1;
}

sub faxmode {
    my $self=shift;
    $self->atcomm("\020\003\020\003","VCON");
    $self->atcomm("AT#BDR=0","OK");
    $self->atcomm("AT#CLS=2","OK");
}

sub playfile {
    my $self=shift;
    my $playfile=shift;
    $playfile=$tmpmsg if $playfile eq "";
    if (substr($playfile,0,1) ne "/") {$playfile="$vdir/$playfile";}
    if (!(-e $playfile)) {print LOG "File $playfile not found\n";return undef;}
    my $ndtmf=shift;
    $ndtmf=0 if !($ndtmf);
    my $rdtmf="";
    my $dtmf="";
    my $tmp;
    my $dtcount =0;
    open (FH1,$playfile);
    read FH1,$tmp,4;
    if ($tmp ne "RMD1") {
        close FH1;
        system "$bindir/lintopvf -s 7200 $playfile $tmpmsg.1";
        system "$bindir/pvftormd Rockwell 4 $tmpmsg.1 $tmpmsg.2";
        open (FH1,"$tmpmsg.2");
    }
    while (!eof(FH1)) {
        read FH1,$tmp,1000;
        $tmp=$self->rmdle($tmp);
        $self->write($tmp);
        $self->write_drain;
        $dtmf=$self->input; 
        last if ($dtmf=~/[0-9]/) && ($ndtmf !=0);
        if ($dtmf=~/[b]/) {
            print LOG "User hanged up before call was finished\n";
            return undef;
        }
    }
    unlink("$tmpmsg");
    system "touch $tmpmsg";
    if ($ndtmf == 0) {
        return 1;
    }
    if ($ndtmf==1) {
        return join('', split(/\W/,$dtmf));
    } 
    $rdtmf=$dtmf;
    open (FH1,"$vdir/tsil15");
    while (!eof(FH1)) {
        read FH1,$tmp,1000;
        $tmp=$self->rmdle($tmp);
        $self->write($tmp);
        $self->write_drain;
        $dtmf=$self->input; 
        $rdtmf=$rdtmf.$dtmf if ($dtmf=~/[0-9]/);
        if ($dtmf=~/[b]/) {
            print LOG "User hanged up before call was finished\n";
            return undef;
        }
        last if (length($rdtmf)==$ndtmf*2);
    }
    return " " if !($rdtmf=~/[0-9]/);
    return join('', split(/\W/, $rdtmf));
}

sub recfile {
    my $self=shift;
    my $recfile = shift;
    my $ttime=shift;
    $self->atcomm("\020\003\020\003","VCON");
    $self->atcomm("AT#VRX","CONNECT")||return undef;
    open (FH1,">$recfile");
    print FH1 "RMD1Rockwell";
    my $rmdstr=pack("C20",0,0,0,0,0,0,0,0,0,4,28,32,4,0,0,0,0,0,0,0);
    print FH1 $rmdstr;
    my $otimer=time;
    while ((time-$otimer)<$ttime) {
        print FH1 $self->input;
    }
    close FH1;
    if ($self->input=~/[b]/) {
        print LOG "User hanged up before call was finished\n";
        return undef;
    }
    print LOG "Message file $recfile recorded\n";
    $self->atcomm("\020\030\020\003","VCON")||return undef;
    $self->atcomm("AT#VTX","CONNECT")||return undef;
    return 1;
}

sub addmsg {
    my $self=shift;
    my $playfile=shift;
    system "cat $vdir/$playfile >> $tmpmsg";
}

sub addval {
    my $self=shift;
    my $num1=shift;
    my $num2;
    my $num3;
    $num1="0".$num1 while (length($num1) ne 9);
    $num2=substr($num1,0,2);
    $self->addint1($num2,"crore");
    $num2=substr($num1,2,2);
    $self->addint1($num2,"lack");
    $num2=substr($num1,4,2);
    $self->addint1($num2,"thousand");
    $num2=substr($num1,6,1);
    system "cat $vdir/$num2 $vdir/hundred >> $tmpmsg" if ($num2 != 0);
    $num2=substr($num1,7,2);
    $self->addint1($num2,"sil0");
    return ;
}

sub addint1 {
    my $self=shift;
    my $num2=shift;
    my $unit=shift;
    my $num3;
    if (($num2<21)&&($num2>0)) {
        $num2=int($num2);
        system "cat $vdir/$num2 >> $tmpmsg";
    }
    if ($num2>20) {
        $num3=10*substr($num2,0,1);
        system "cat $vdir/$num3 >> $tmpmsg";
        $num3=substr($num2,1,1);
        system "cat $vdir/$num3 >> $tmpmsg";
    }
    system "cat $vdir/$unit >> $tmpmsg" if ($num2 != 0);
    return;
}

sub addmil {
    my $self=shift;
    my $num1=shift;
    my $num2;
    my $num3;
    $num1="0".$num1 while (length($num1) ne 9);
    $num2=substr($num1,0,3);
    $self->addint2($num2,"crore");
    $num2=substr($num1,3,3);
    $self->addint2($num2,"thousand");
    $num2=substr($num1,6,3);
    $self->addint2($num2,"sil0");
    return ;
}

sub addint2 {
    my $self=shift;
    my $num2=shift;
    my $unit=shift;
    my $num3=0;
    $num3=substr($num2,0,1);
    system "cat $vdir/$num3 $vdir/hundred >> $tmpmsg" if ($num3 != 0);
    $num2=substr($num2,1,2);
    if (($num2<21)&&($num2>0)) {
        $num2=int($num2);
        system "cat $vdir/$num2 >> $tmpmsg";
    }
    if ($num2>20) {
        $num3=10*substr($num2,0,1);
        system "cat $vdir/$num3 >> $tmpmsg";
        $num3=substr($num2,1,1);
        system "cat $vdir/$num3 >> $tmpmsg";
    }
    system "cat $vdir/$unit >> $tmpmsg" if ($num2 != 0);
    return;
}

sub addtxt {
    my $self = shift;
    my $pstr = shift;
    my $i=0;
    my $pchr="";
    while ($i!=length($pstr)) {
        $pchr=lc(substr($pstr,$i,1));
        system "cat $vdir/$pchr >> $tmpmsg"||return undef;
        $i++;
    }
}

sub addate {
    my $self=shift;
    my $num1=shift;
    my $num2="";
    $num2=substr($num1,0,2);
    $self->addval($num2);
    $num2=substr($num1,2,2);
    #$num2=abs($num2);
    $num2="m$num2";
    system "cat $vdir/$num2 >> $tmpmsg";
    $num2=substr($num1,4,4);
    #$num2=substr($num1,2,2) if (substr($num1,0,2) eq 19);
    $self->addval($num2);
    return;
}

sub rmdle {
    my $self=shift; 
    my $tmp=shift;
    my $tmp1="";
    my $i;
    for ($i=0;$i<length($tmp);$i++) {
        if (substr($tmp,$i,1)=~/[\020]/) {$tmp1=$tmp1."\020";}
        $tmp1=$tmp1.substr($tmp,$i,1);
    }
    return $tmp1;
}

sub closep {
    my $self=shift;
    unlink ("$tmpmsg");
    unlink ("$tmpmsg.1");
    unlink ("$tmpmsg.2");
    $self->atcomm("\020\030\020\003","VCON");
    $self->atcomm("ATH","OK");
    $self->atcomm("ATZ","OK");
    $self->pclose
}


1;
__END__

=head1 NAME

Ivrs - Perl extension for Interactive Voice Response System.

=head1 SYNOPSIS

$iv = new Ivrs($portname,$vdir,$bindir);

=head1 DESCRIPTION

This module provides the complete interface to voice modem for Interactive
Voice Response System (IVRS). The IVRS are widely used for telebanking,
product inforamtion, tele marketing, voice mail, fax servers, and many more.

All these can be implemented using this module and with very few lines of code.
This module takes care of all the low level function for serial port and
modem. 

=head1 EXAMPLE

The demo files are short and with full of explanation, which should serve to understanding of module.

demo1 - A simple voice interaction.

demo2 - Message recording and playback.

demo3 - Fax server.

=head1 METHODS

$iv = new Ivrs('ttyS1',$vdir,$bindir);

The first variable is the mdoem/serial port.

You must specify $vdir and $bindir if you want to use other than default
(namely sfiles/ or bin/). If you are running IVRS from /etc/inittab than
also absolute path will be rquired.

$iv->setport('38400','none','8','1','rts','8096');

The serial port parameters are set here. These parameters are carefully
worked after extensive trials. Change these only if you know what are you
doing.

$iv->initmodem;

This will put the modem in voice mode. Number of AT commands are required to
set this. Again do not change any thing here also, unless you are sure.

$cid=$iv->waitring;

This will put the modem in annswer mode and wait for the ring. When the ring
comes call will be received and Caller ID will be returned in $cid (not
tested so far). If you want to play a message through Modem speaker then
skip it, but then you will not be able to punch DTMF codes. 


$iv->playfile("$msgfile","$dtmf")

This requires a lot of explanation.

$msgfile can be rmd or lin sound file generated by pvftools (See Sound
Files later). You can specify the full path of the file with / or only file 
name in voice file directory specified in $vdir. If no file is specified, it 
will play special file contained in $tmpmsg. We will discuss this file in next
section.

Another variable required is $dtmf. 

If $dtmf=0 then playing will not be stopped even if caller presses any key.

If $dtmf=1, then the specified file will played and if caller presses any key
then playing will immidiately stop and $iv->playfile will return the digit
pressed.

If $dtmf=2 or more, then playing of specified file will stopped when caller
presses first digit and next a silence of 15 seconds will played for caller
to enter remaining digits. When caller has pressed required number of digits
($dtmf) then $iv->playfile will return with complete dtmf digits.

$iv->addmsg($msg)

$iv->addval($val)

$iv->addmil($val)

$iv->addtxt("ABCDEF")

$iv->addate("20001212")

Any IVRS requires many messages to generated on fly and then played to
caller, like number in numerical format. The linear type of sound files
(refer pvftools ) are header less files and you can do cut paste on these
file. The default voice directory sfiles/ has mostly linear files. The above 
mentioned routines does exactly this. All the linear files are dumped to a
file specified by $tmpmsg, and finally this file is converted to rmd (again
pvftools ) and then played to caller.

$iv->addmsg($msg)

Add a message from sfiles/

$iv->addval($val)

Add a numeric value in Indian format (using lacs and crore)

$iv->addmil($val)

Same as above but with Internation format (using milions and billions)

$iv->addtxt("ABCDEF")

Add alphabetical characters

$iv->addate("20001212")

Add date in yyyymmdd format.

$iv->recfile($filename,$duration)

This will record the file $filename in rmd file format with proper header
for a period of $duration seconds. This rmd file can be converted to any
format using pvftools.

$iv->closep

This will do some cleanup and hangup the line and reset the modem.

The demo files demo1 demo2 and demo3 are good examples for
implementation of IVRS.
I will add some more demo files in future release.

=head1 THANKS

I must thank Bill Birthisel, wcbirthisel@alum.mit.edu, for serial port code
taken from SerialPort.pm,

=head1 AUTHOR

Mukund Deshmukh <betacomp@nagpur.dot.net.in>

=head1 SEE ALSO

SerialPort.pm, pvftools

=cut
