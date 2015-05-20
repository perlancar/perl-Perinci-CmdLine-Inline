package Perinci::CmdLine::Inline;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;

our %SPEC;

$SPEC{gen_inline_pericmd_script} = {
    v => 1.1,
    summary => 'Generate inline Perinci::CmdLine CLI script',
    description => <<'_',

The goal of this module is to let you generate a CLI script from a Riap
function/metadata, like `Perinci::CmdLine::Lite` or `Perinci::CmdLine::Classic`
but the generated CLI script will have the functionalities inlined so it does
not need any of the `Perinci::CmdLine::*` module to run.

It's useful to create even more lightweight (in term of startup overhead or
dependencies) CLI script than `Perinci::CmdLine::Lite`.

Currently it only supports a subset of features compared to other
`Perinci::CmdLine::*` implementations:

* Only support local Riap URL (e.g. `/Foo/bar`, not
  `http://example.org/Foo/bar`);

* No tab completion;

* and so on.

_
    args => {
        url => {
            schema => 'str*',
            req => 1,
            pos => 0,
        },
    },
};
sub gen_inline_pericmd_script {
    my %args = @_;

    my $url = $args{url};
    $url =~ m!\A(?:pl:)?((?:/[^/]+)+)/([^/]+)\z!
        or return [412, "URL scheme not supported, only local Perl URL ".
                       "currently supported"];
    my ($mod_pm, $func_name) = @_;
    (my $mod = $mod_pm) =~ s!/!::!g;
    $mod_pm .= ".pm";

    require $mod_pm;

    my $func_ref;
    my $meta;
    {
        no strict 'refs';
        $meta = ${"$mod\::SPEC"}{$func_name}
            or return [412, "Can't find meta for URL '$url'"];
        defined &{"$mod\::$func_name"}
            or return [412, "Can't find function for URL '$url'"];
        $func_ref = \&{"$mod\::$func_name"};
    }

    [200];
}

1;
# ABSTRACT:

=head1 SYNOPSIS

 % gen-inline-pericmd-script /Perinci/Examples/gen_array -o gen-array

 % ./gen-array
 ERROR 400: Missing required argument(s): len

 % ./gen-array --help
 ... help message printed ...

 % ./gen-array 3
 2
 3
 1

 % ./gen-array 3 --json
 [200,"OK",[3,1,2],{}]


=head1 DESCRIPTION


=head1 SEE ALSO

L<Perinci::CmdLine>, L<Perinci::CmdLine::Any>, L<Perinci::CmdLine::Lite>,
L<Perinci::CmdLine::Classic>

L<App::GenPericmdScript>

=cut
