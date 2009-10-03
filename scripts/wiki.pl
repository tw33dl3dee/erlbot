#!/usr/bin/perl -wCSDA
# query Wikipedia

use strict;
use warnings;

use URI::Escape qw(uri_escape_utf8);
use LWP::UserAgent;
use charnames ':full';
use Data::Dumper;
use XML::Simple ':strict';
#use HTML::Strip;
use HTML::TreeBuilder;

unless (@ARGV) {
    print "usage: wiki.pl topic\n",
    exit 1;
}

my $lang = shift @ARGV;

my $url = "http://$lang.wikipedia.org/w/api.php?action=query&prop=revisions&rvprop=content&".
	"rvlimit=1&rvsection=0&redirects=1&format=xml&titles=".
    uri_escape_utf8(join ' ' => @ARGV);

#print "URL: $url\n";

my $ua = LWP::UserAgent->new(agent => 'Yadbot/1.0');
my $response = $ua->get($url);

$response->is_success or
    die "$url: ", $response->status_line;

my $content = $response->decoded_content;
my $xs = XML::Simple->new(ForceArray => 1, KeyAttr => [], KeepRoot => 0);
my $page = $xs->xml_in($content)->{query}[0]{pages}[0]{page}[0];

#print "XML result: $content\n";

unless ($page->{revisions}) {
	print "Page not found\n";
	exit 1;
}

#my $hs = HTML::Strip->new(striptags => [qw(ref)], emit_spaces => 0, decode_entities => 0);
my $html_text = $page->{revisions}[0]{rev}[0]->{content};
#my $text = $hs->strip_html($page->{revisions}[0]{rev}[0]);

#$html_text =~ s/\n/<br \/>\n/g;

#print "Result: $html_text\n";

my $builder = HTML::TreeBuilder->new;
$builder->no_space_compacting(1);
my $tree = $builder->parse_content($html_text);

#print Dumper($html_text)."\n";

my $text = $tree->as_text();

#$text =~ s/\\<ref\\>.*?\\<\/ref\\>//gm;
$text =~ s/{{[^}]*}}//gm;
$text =~ s/\[\[[^\]:]+:.*\]\]//gm;
$text =~ s/^\s*//g;
$text =~ s/(\s)+/$1/g;

print "Wikipedia topic:\n".$text."\n";
