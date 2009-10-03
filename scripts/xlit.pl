#!/usr/bin/perl -w

use strict;
use Lingua::RU::Translit qw(translit2koi);

print translit2koi($ARGV[0])."\n";
