#!/usr/bin/perl -wCSDA
# gcalc.pl - Google calculator example
#
# © Copyright, 2004-2005 By John Bokma, http://johnbokma.com/
#
# This script is for educational purposes only.
#
# $Id: gcalc.pl 1088 2008-09-30 19:11:55Z john $ 

use strict;
use warnings;

use URI::Escape qw(uri_escape_utf8);
use LWP::UserAgent;
use charnames ':full';

unless ( @ARGV ) {
    print "usage: gcalc.pl expression\n",
	"    example: gcalc.pl 75 kg in stones\n";
    exit( 1 ) ;
}

my $url = 'http://www.google.ru/search?num=1&q=' .
    uri_escape_utf8( join ' ' => @ARGV ) . "&ie=utf-8&oe=utf-8";

my $ua = LWP::UserAgent->new( agent => 'Mozilla/5.0' );
my $response = $ua->get( $url );

$response->is_success or
    die "$url: ", $response->status_line;

my $content = $response->decoded_content;

#print "$content\n";

#$content =~ s/\N{NO-BREAK SPACE}//g;

my ($result) = $content =~ m|<td nowrap.*><h2 class=r.*><b>(.*?)</b></h2>|;

#	print "$result\n";

if ($result) {
    $result =~ s/<sup>/^/g;
    $result =~ s/&times;/x/g;
    $result =~ s/<.+?>//g;
	$result =~ s/\N{NO-BREAK SPACE}/ /g;
	$result =~ s/\N{LATIN CAPITAL LETTER A WITH CIRCUMFLEX}//g;
	print "$result\n";
}
