#!perl

# run the Test::Perinci::CmdLine test suite

use 5.010;
use strict;
use warnings;

use Test::More 0.98;
use Test::Perinci::CmdLine qw(pericmd_ok);

pericmd_ok(
    class => 'Perinci::CmdLine::Inline',
    exclude_tags => [
        'embedded-meta', # probably won't ever be supported

        'completion', # probably won't be unsupported

        'subcommand', # currently unsupported
        'cmdline_src', # currently unsupported
        'env', # currently unsupported
        'config-file', # currently unsupported
    ],
);
done_testing;
