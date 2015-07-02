package Perinci::CmdLine::Inline;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;

use Data::Dmp;
use Module::Path::More qw(module_path);

use Exporter qw(import);
our @EXPORT_OK = qw(gen_inline_pericmd_script);

our %SPEC;

sub _deparse {
    require Data::Dumper;
    no warnings 'once';
    local $Data::Dumper::Deparse = 1;
    local $Data::Dumper::Terse   = 1;
    local $Data::Dumper::Indent  = 0;
    Data::Dumper::Dumper($_[0]);
}

sub _get_module {
    my $path = module_path(module => $_[0]) or die "Can't load module '$_[0]'";
    local $/;
    open my($fh), "<", $path or die "Can't read file '$path': $!";
    ~~<$fh>;
}

# keep synchronize with Perinci::CmdLine::Base
my %pericmd_attrs = (

    # the currently unsupported/unused/irrelevant
    (map {(
        $_ => {
            schema => 'any*',
        },
    )} qw/actions common_opts completion default_subcommand get_subcommand_from_arg
          description exit formats
          riap_client riap_version riap_client_args
          log
          subcommands
          tags
          read_env env_name
          read_config config_dirs config_filename
         /),

    summary => {
        summary => 'Program summary',
        schema  => 'str*',
    },
    pass_cmdline_object => {
        summary => 'Whether to pass Perinci::CmdLine::Inline object',
        schema  => 'bool*',
        default => 0,
    },
    program_name => {
        summary => 'Program name',
        schema => 'str*',
    },
    program_version => {
        summary => 'Program version (otherwise will use version from url metadata)',
        schema => 'str',
    },
    url => {
        summary => 'Program URL',
        schema => 'str*',
        pos => 0,
        'x.schema.entity' => 'riap_url',
    },
    extra_urls_for_version => {
        summary => 'More URLs to show version for --version',
        description => <<'_',

Currently not implemented in Perinci::CmdLine::Inline.

_
        schema => ['array*', of=>'str*'],
        'x.schema.element_entity' => 'riap_url',
    },
);

$SPEC{gen_inline_pericmd_script} = {
    v => 1.1,
    summary => 'Generate inline Perinci::CmdLine CLI script',
    description => <<'_',

The goal of this module is to let you create a CLI script from a Riap
function/metadata. This is like what `Perinci::CmdLine::Lite` or
`Perinci::CmdLine::Classic` does, except that the generated CLI script will have
the functionalities inlined so it only need core Perl modules and not any of the
`Perinci::CmdLine::*` or other modules to run (excluding what modules the Riap
function itself requires).

It's useful if you want a CLI script that is even more lightweight (in terms of
startup overhead or dependencies) than the one using `Perinci::CmdLine::Lite`.

So to reiterate, the goal of this module is to create a Perinci::CmdLine-based
script which only requires core modules, and has as little startup overhead as
possible.

Currently it only supports a subset of features compared to other
`Perinci::CmdLine::*` implementations:

* Only support local Riap URL (e.g. `/Foo/bar`, not
  `http://example.org/Foo/bar`);

* No tab completion;

* No subcommands support yet;

* No support for streaming input or output;

* No support for cmdline_src argument specification property;

* No support for per_arg_yaml (not used as often as per_arg_json, no core module
  for parsing YAML).

* and so on.

As an alternative to this module, if you are looking to reduce dependencies, you
might also want to try using `fatten` to fatpack your
`Perinci::CmdLine::Lite`-based script.

TODO:

* Option to validate argument (using periswrap).

* Support per_arg_json.

_
    args_rels => {
        'dep_any&' => [
            [meta_is_normalized => ['meta']],
        ],
        'req_one&' => [
            [qw/url meta/],
        ],
        'choose_all&' => [
            [qw/meta sub_name/],
        ],
   },
    args => {
        (map {
            $_ => {
                %{ $pericmd_attrs{$_} },
                summary => $pericmd_attrs{$_}{summary} // 'Currently does nothing, provided only for compatibility with Perinci::CmdLine::Base',
                tags => ['category:pericmd-attribute'],
            },
        } keys %pericmd_attrs),

        meta => {
            summary => 'An alternative to specifying `url`',
            schema => 'hash',
            tags => ['category:input'],
        },
        meta_is_normalized => {
            schema => 'bool',
            tags => ['category:input'],
        },
        sub_name => {
            schema => 'str*',
            tags => ['category:input'],
        },

        shebang => {
            summary => 'Set shebang line',
            schema  => 'str*',
        },
        validate_args => {
            summary => 'Whether to validate arguments using schemas',
            schema  => 'bool',
            default => 1,
        },
        #validate_result => {
        #    summary => 'Whether to validate result using schemas',
        #    schema  => 'bool',
        #    default => 1,
        #},

        with_debug => {
            summary => 'Generate script with debugging outputs',
            schema => 'bool',
            tags => ['category:debugging'],
        },
        include => {
            summary => 'Include extra modules',
            'summary.alt.plurality.singular' => 'Include an extra module',
            schema => ['array*', of=>'str*'],
            'x.schema.element_entity' => 'modulename',
        },

        code_before_parse_cmdline_options => {
            schema => 'str*',
            tags => ['category:extra-code'],
        },

        output_file => {
            summary => 'Set output file, defaults to stdout',
            schema => 'str*',
            'x.schema.entity' => 'filename',
            cmdline_aliases => {o=>{}},
            tags => ['category:output'],
        },
        overwrite => {
            schema => 'bool',
            tags => ['category:output'],
        },
    },
};
sub gen_inline_pericmd_script {
    my %args = @_;

    my $validate_args = $args{validate_args} // 1;
    #my $validate_result = $args{validate_result} // 1;
    my $program_name = $args{program_name};

    my $meta;
    my $mod;
    my $func_name;
  GET_META:
    {
        no strict 'refs';
        my $url = $args{url};
        if ($url) {
            $url =~ m!\A(?:pl:)?((?:/[^/]+)+)/([^/]+)\z!
                or return [412, "URL scheme not supported, only local Perl ".
                           "URL currently supported"];
            my ($mod_pm, $short_func_name) = ($1, $2);
            $mod_pm =~ s!\A/!!;
            ($mod = $mod_pm) =~ s!/!::!g;
            $mod_pm .= ".pm";
            require $mod_pm;
            $meta = ${"$mod\::SPEC"}{$short_func_name}
                or return [412, "Can't find meta for URL '$url'"];
            defined &{"$mod\::$short_func_name"}
                or return [412, "Can't find function for URL '$url'"];
            $program_name //= do {
                local $_ = $short_func_name;
                s/_/-/g;
                $_;
            };
            $func_name = "$mod\::$short_func_name";
        } else {
            $meta = $args{meta};
            $func_name = $args{sub_name};
            $program_name //= do {
                local $_ = $args{sub_name};
                s/_/-/g;
                $_;
            };
        }

        $program_name //= do {
            local $_ = $0;
            s!.+[/\\]!!;
            $_;
        };

        last if $args{meta_is_normalized};
        require Perinci::Sub::Normalize;
        $meta = Perinci::Sub::Normalize::normalize_function_metadata($meta);
    }

    my $cd = {
        modules => {},
        vars => {},
        subs => {},
        module_srcs => {
            map {$_ => _get_module($_)} (
                "Data::Check::Structure",
                "Text::Table::Tiny",
                "Getopt::Long::EvenLess",
                @{ $args{include} // [] },
            )
        },
    };

  GEN_SCRIPT:
    {
        no strict 'refs';

        my @l;

        my %copts;
        {
            require Perinci::CmdLine::Base;
            no warnings 'once';
            for (qw/help version json format/) {
                $copts{$_} = $Perinci::CmdLine::Base::copts{$_};
            }
            # "naked_res!" currently not supported by Getopt::Long::EvenLess, so
            # we split it. the downside is that we don't hide the default, by
            # default.
            $copts{naked_res} = {
                getopt  => "naked-res",
                summary => "When outputing as JSON, strip result envelope",
            };
            $copts{no_naked_res} = {
                getopt  => "no-naked-res|nonaked-res",
                summary => "When outputing as JSON, don't strip result envelope",
            };
        }

        my $shebang_line;
        {
            $shebang_line = $args{shebang} // $^X;
            $shebang_line = "#!$shebang_line" unless $shebang_line =~ /\A#!/;
            $shebang_line .= "\n" unless $shebang_line =~ /\R\z/;
        }

        $cd->{subs}{_pci_err} = <<'_';
    my $res = shift;
    print STDERR "ERROR $res->[0]: $res->[1]\n";
    exit $res->[0]-300;
_

        $cd->{subs}{_pci_debug} = <<'_' if $args{with_debug};
    no warnings "once";
    require Data::Dumper;
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    print "DEBUG: ";
    for (@_) {
        if (ref($_)) { print Data::Dumper::Dumper($_) } else { print $_ }
    }
    print "\n";
_

        # from List::MoreUtils 0.401
        $cd->{subs}{_pci_firstidx} = ['&@', <<'_'];
    my $f = shift;
    foreach my $i ( 0 .. $#_ )
    {
        local *_ = \$_[$i];
        return $i if $f->();
    }
    return -1;
_
        $cd->{subs}{_pci_json} = <<'_';
     state $json = do {
        # XXX try JSON::XS first, fallback to JSON::PP
        require JSON::PP;
        JSON::PP->new->canonical(1)->allow_nonref;
    };
    $json;
_

        {
            require Data::Clean::JSON;
            my $cleanser = Data::Clean::JSON->get_cleanser;
            my $src = $cleanser->{src};
            $cd->{module_srcs}{'Inlined::_pci_clean_json'} = "require Scalar::Util; use feature 'state'; sub _pci_clean_json { $src }\n1;\n";
        }

        # borrowed from Perinci::CmdLine::Lite 1.13, use our own firstidx
        $cd->{module_srcs}{'Inlined::_pci_gen_table'} = <<'_';
sub _pci_gen_table {
    my ($data, $header_row, $resmeta, $is_pretty) = @_;

    $resmeta //= {};

    my @columns;
    if ($header_row) {
        @columns = @{$data->[0]};
    } else {
        @columns = map {"col$_"} 0..@{$data->[0]}-1;
    }

    my $column_orders; # e.g. [col2, col1, col3, ...]
  SET_COLUMN_ORDERS: {

        # find column orders from 'table_column_orders' in result metadata (or
        # from env)
        my $tcos;
        if ($ENV{FORMAT_PRETTY_TABLE_COLUMN_ORDERS}) {
            $tcos = _pci_json()->decode($ENV{FORMAT_PRETTY_TABLE_COLUMN_ORDERS});
        } elsif (my $rfos = ($resmeta->{'cmdline.format_options'} //
                                 $resmeta->{format_options})) {
            my $rfo = $rfos->{'text-pretty'} // $rfos->{text} // $rfos->{any};
            if ($rfo) {
                $tcos = $rfo->{table_column_orders};
            }
        }
        if ($tcos) {
            # find an entry in tcos that @columns contains all the columns of
          COLS:
            for my $cols (@$tcos) {
                for my $col (@$cols) {
                    next COLS unless first {$_ eq $col} @columns;
                }
                $column_orders = $cols;
                last SET_COLUMN_ORDERS;
            }
        }

        # find column orders from table spec
        $column_orders = $resmeta->{'table.fields'};
    }

    # reorder each row according to requested column order
    if ($column_orders) {
        # 0->2, 1->0, ... (map column position from unordered to ordered)
        my @map0 = sort {
            my $idx_a = _pci_firstidx(sub {$_ eq $a->[1]},
                                                  @$column_orders) // 9999;
            my $idx_b = _pci_firstidx(sub {$_ eq $b->[1]},
                                                  @$column_orders) // 9999;
            $idx_a <=> $idx_b || $a->[1] cmp $b->[1];
        } map {[$_, $columns[$_]]} 0..@columns-1;
        #use DD; dd \@map0;
        my @map;
        for (0..@map0-1) {
            $map[$_] = $map0[$_][0];
        }
        #use DD; dd \@map;
        my $newdata = [];
        for my $row (@$data) {
            my @newrow;
            for (0..@map-1) { $newrow[$_] = $row->[$map[$_]] }
            push @$newdata, \@newrow;
        }
        $data = $newdata;
    }

    if ($is_pretty) {
        require Text::Table::Tiny;
        Text::Table::Tiny::table(rows=>$data, header_row=>$header_row) . "\n";
    } else {
        no warnings 'uninitialized';
        shift @$data if $header_row;
        join("", map {join("\t", @$_)."\n"} @$data);
    }
}
1;
_

$cd->{module_srcs}{'Inlined::_pci_format_result'} = <<'_';
sub _pci_format_result {
    require Data::Check::Structure;

    my $r = shift;

    my $res    = $r->{res};
    my $format = $r->{format} // 'text';

    if ($format =~ /\Atext(-simple|-pretty)?\z/) {
        my $is_pretty = $format eq 'text-pretty' ? 1 :
            $format eq 'text-simple' ? 0 : (-t STDOUT);
        no warnings 'uninitialized';
        if ($res->[0] !~ /^(2|304)/) {
            my $fres = "ERROR $res->[0]: $res->[1]";
            if (my $prev = $res->[3]{prev}) {
                $fres .= " ($prev->[0]: $prev->[1])";
            }
            return "$fres\n";
        } elsif ($res->[3] && $res->[3]{"x.hint.result_binary"}) {
            return $res->[2];
        } else {
            my $data = $res->[2];
            my $max = 5;
            if (!ref($data)) {
                $data //= "";
                $data .= "\n" unless !length($data) || $data =~ /\n\z/;
                return $data;
            } elsif (ref($data) eq 'ARRAY' && !@$data) {
                return "";
            } elsif (Data::Check::Structure::is_aos($data, {max=>$max})) {
                return join("", map {"$_\n"} @$data);
            } elsif (Data::Check::Structure::is_aoaos($data, {max=>$max})) {
                require Inlined::_pci_gen_table;
                return _pci_gen_table($data, 0, $res->[3], $is_pretty);
            } elsif (Data::Check::Structure::is_hos($data, {max=>$max})) {
                $data = [map {[$_, $data->{$_}]} sort keys %$data];
                unshift @$data, ["key", "value"];
                require Inlined::_pci_gen_table;
                return _pci_gen_table($data, 1, $res->[3], $is_pretty);
            } elsif (Data::Check::Structure::is_aohos($data, {max=>$max})) {
                # collect all mentioned fields
                my %fieldnames;
                for my $row (@$data) {
                    $fieldnames{$_}++ for keys %$row;
                }
                my @fieldnames = sort keys %fieldnames;
                my $newdata = [];
                for my $row (@$data) {
                    push @$newdata, [map {$row->{$_}} @fieldnames];
                }
                unshift @$newdata, \@fieldnames;
                require Inlined::_pci_gen_table;
                return _pci_gen_table($newdata, 1, $res->[3], $is_pretty);
            } else {
                $format = 'json-pretty';
            }
        }
    }

    $res = $res->[2] if $r->{naked_res};

    warn "Unknown format '$format', fallback to json-pretty"
        unless $format =~ /\Ajson(-pretty)?\z/;
    require Inlined::_pci_clean_json;
    _pci_clean_json($res);
    if ($format eq 'json') {
        return _pci_json()->encode($res) . "\n";
    } else {
        return _pci_json()->canonical(1)->pretty->encode($res);
    }
}
1;
_

        require Perinci::Sub::GetArgs::Argv;
        my $ggl_res = Perinci::Sub::GetArgs::Argv::gen_getopt_long_spec_from_meta(
            meta => $meta,
            meta_is_normalized => 1,
            common_opts => \%copts,
        );
        return [500, "Can't generate Getopt::Long spec from meta: ".
                    "$ggl_res->[0] - $ggl_res->[1]"]
            unless $ggl_res->[0] == 200;

        # gen function to check arguments
        {
            my @l2;
            push @l2, '    my $args = shift;', "\n";
            my $args_prop = $meta->{args} // {};

            push @l2, "  FILL_FROM_POS: {\n";
            push @l2, "        1;\n";
            for my $arg (sort {
                ($args_prop->{$b}{pos} // 9999) <=>
                    ($args_prop->{$a}{pos} // 9999)
                } keys %$args_prop) {
                my $arg_spec = $args_prop->{$arg};
                my $arg_opts = $ggl_res->[3]{'func.opts_by_arg'}{$arg};
                next unless defined $arg_spec->{pos};
                push @l2, '        if (@ARGV > '.$arg_spec->{pos}.') {';
                push @l2, ' if (exists $_pci_args{"'.$arg.'"}) {';
                push @l2, ' return [400, "You specified '.$arg_opts->[0].' but also argument #'.$arg_spec->{pos}.'"];';
                push @l2, " } else {";
                if ($arg_spec->{greedy}) {
                    push @l2, ' $_pci_args{"'.$arg.'"} = [splice(@ARGV, '.$arg_spec->{pos}.')];';
                } else {
                    push @l2, ' $_pci_args{"'.$arg.'"} = delete($ARGV['.$arg_spec->{pos}.']);';
                }
                push @l2, " }";
                push @l2, " }\n";
            }
            push @l2, "    }\n";

            push @l2, '    # fill defaults', "\n";
            for my $arg (sort keys %$args_prop) {
                my $arg_spec = $args_prop->{$arg};
                next unless exists($arg_spec->{default}) || exists($arg_spec->{schema}[1]{default});
                push @l2, '    unless (exists $_pci_args{"'.$arg.'"}) {';
                if (exists $arg_spec->{default}) {
                    push @l2, ' $_pci_args{"'.$arg.'"} //= '.dmp($arg_spec->{default}).';';
                }
                if (exists $arg_spec->{schema}[1]{default}) {
                    push @l2, ' $_pci_args{"'.$arg.'"} //= '.dmp($arg_spec->{schema}[1]{default}).';';
                }
                push @l2, " }\n";
            }
            push @l2, "\n";

            push @l2, '    # check required args', "\n";
            for my $arg (sort keys %$args_prop) {
                my $arg_spec = $args_prop->{$arg};
                if ($arg_spec->{req}) {
                    push @l2, '    return [400, "Missing required argument: '.$arg.'"] unless exists $_pci_args{"'.$arg.'"};', "\n";
                }
                if ($arg_spec->{schema}[1]{req}) {
                    push @l2, '    return [400, "Missing required value for argument: '.$arg.'"] if exists($_pci_args{"'.$arg.'"}) && !defined($_pci_args{"'.$arg.'"});', "\n";
                }
            }

            push @l2, '    [200];', "\n";
            $cd->{subs}{_pci_check_args} = join('', @l2);
        }

        $cd->{vars}{'$_pci_r'}++;
        push @l, '$_pci_r = { format=>"text", naked_res=>0, };', "\n\n";

        # gen code to parse cmdline options
        $cd->{vars}{'%_pci_args'}++;
        push @l, "# parse cmdline options\n\n";
        push @l, "{\n";
        push @l, "require Getopt::Long::EvenLess;\n";
        push @l, 'my %mentioned_args;', "\n";
        {
            push @l, 'my $go_spec = {', "\n";
            for my $go_spec (sort keys %{ $ggl_res->[2] }) {
                my $specmeta = $ggl_res->[3]{'func.specmeta'}{$go_spec};
                push @l, "    '$go_spec' => sub {\n";
                if ($specmeta->{common_opt}) {
                    if ($specmeta->{common_opt} eq 'help') {
                        require Perinci::CmdLine::Help;
                        my $res = Perinci::CmdLine::Help::gen_help(
                            meta => $meta,
                            common_opts => \%copts,
                            program_name => $program_name,
                        );
                        return [500, "Can't generate help: $res->[0] - $res->[1]"]
                            unless $res->[0] == 200;
                        push @l, '        print ', dmp($res->[2]), '; exit 0;', "\n";
                    } elsif ($specmeta->{common_opt} eq 'version') {
                        no strict 'refs';
                        push @l, '        print "', $program_name , ' version ',
                            (defined($args{program_version}) ? $args{program_version} :
                             $mod && ${"$mod\::VERSION"} ? ${"$mod\::VERSION"} : '?'),
                            ($mod && ${"$mod\::DATE"} ? " (".${"$mod\::DATE"}.")" : ''),
                            '\n";', "\n";
                        push @l, '        print "  Generated by ', __PACKAGE__ , ' version ',
                            (${__PACKAGE__."::VERSION"} // 'dev'),
                            (${__PACKAGE__."::DATE"} ? " (".${__PACKAGE__."::DATE"}.")" : ""),
                            '\n";', "\n";
                        push @l, '        exit 0;', "\n";
                    } elsif ($specmeta->{common_opt} eq 'format') {
                        push @l, '        $_pci_r->{format} = $_[1];', "\n";
                    } elsif ($specmeta->{common_opt} eq 'json') {
                        push @l, '        $_pci_r->{format} = (-t STDOUT) ? "json-pretty" : "json";', "\n";
                    } elsif ($specmeta->{common_opt} eq 'naked_res') {
                        push @l, '        $_pci_r->{naked_res} = 1;', "\n";
                    } elsif ($specmeta->{common_opt} eq 'no_naked_res') {
                        push @l, '        $_pci_r->{naked_res} = 0;', "\n";
                    } else {
                        die "BUG: Unrecognized common_opt '$specmeta->{common_opt}'";
                    }
                } else {
                    my $arg_spec = $meta->{args}{$specmeta->{arg}};
                    push @l, '        ';
                    if ($specmeta->{is_alias} && $specmeta->{is_code}) {
                        my $alias_spec = $arg_spec->{cmdline_aliases}{$specmeta->{alias}};
                        if ($specmeta->{is_code}) {
                            push @l, 'my $code = ', _deparse($alias_spec->{code}), '; ';
                            push @l, '$code->(\%_pci_args);';
                        } else {
                            push @l, '$_pci_args{', $specmeta->{arg}, '} = $_[1];';
                        }
                    } else {
                        if (($specmeta->{parsed}{type} // '') =~ /\@/) {
                            push @l, 'if ($mentioned_args{', $specmeta->{arg}, '}++) { push @{ $_pci_args{', $specmeta->{arg}, '} }, $_[1] } else { $_pci_args{', $specmeta->{arg}, '} = [$_[1]] }';
                        } else {
                            push @l, '$_pci_args{', $specmeta->{arg}, '} = $_[1];';
                        }
                    }
                    push @l, "\n";
                }
                push @l, "    },\n";
            }
            push @l, "};\n";
            push @l, 'my $res = Getopt::Long::EvenLess::GetOptions(%$go_spec);', "\n";
            push @l, '_pci_err([500, "GetOptions failed"]) unless $res;', "\n";
            push @l, '_pci_debug("args after GetOptions: ", \%_pci_args);', "\n" if $args{with_debug};
            push @l, '$res = _pci_check_args(\%_pci_args);', "\n";
            push @l, '_pci_err($res) if $res->[0] != 200;', "\n";
            push @l, '_pci_debug("args after _pci_check_args: ", \%_pci_args);', "\n" if $args{with_debug};
        }
        push @l, "}\n\n";

        # generate code to call function
        push @l, "# call function\n\n";
        push @l, "{\n";
        push @l, "require $mod;\n" if $mod;
        push @l, '$_pci_args{-cmdline} = Perinci::CmdLine::Inline::Object->new(@{', _deparse([%args]), '});', "\n"
            if $args{pass_cmdline_object};
        push @l, 'eval { $_pci_r->{res} = ', $func_name, '(%_pci_args) };', "\n";
        push @l, 'if ($@) { $_pci_r->{res} = [500, "Function died: $@"] }', "\n";
        push @l, "}\n\n";

        # generate code to display result
        push @l, "# display result\n\n";
        push @l, "{\n";
        push @l, 'my $fres;', "\n";
        push @l, 'if ($_pci_r->{res}[3]{"cmdline.skip_format"}) { $fres = $_pci_r->{res}[2] } else { require Inlined::_pci_format_result; $fres = _pci_format_result($_pci_r) }', "\n";
        push @l, 'print $fres;', "\n";
        push @l, "}\n\n";

        # generate code to exit with code
        push @l, "# exit\n\n";
        push @l, "{\n";
        push @l, 'my $status = $_pci_r->{res}[0];', "\n";
        push @l, 'exit($status =~ /200|304/ ? 0 : ($status-300));', "\n";
        push @l, "}\n\n";

        if ($args{pass_cmdline_object}) {
            require Class::GenSource;
            my $cl = 'Perinci::CmdLine::Inline::Object';
            $cd->{module_srcs}{$cl} =
                Class::GenSource::gen_class_source_code(
                    name => $cl,
                    attributes => {
                        map { $_ => {} } keys %pericmd_attrs,
                    },
                );
        }

        require Module::DataPack;
        my $dp_res = Module::DataPack::datapack_modules(
            module_srcs => $cd->{module_srcs},
            stripper    => 1,
        );
        return [500, "Can't datapack: $dp_res->[0] - $dp_res->[1]"]
            unless $dp_res->[0] == 200;
        my ($dp_code1, $dp_code2) = $dp_res->[2] =~ /(.+?)^(__DATA__\n.+)/sm;

        # generate final result
        $cd->{result} = join(
            "",
            $shebang_line, "\n",

            "# PERICMD_INLINE_SCRIPT: ", dmp(\%args), "\n\n",

            "# This script is generated by ", __PACKAGE__,
            " version ", (${__PACKAGE__."::VERSION"} // 'dev'), " on ",
            scalar(localtime), ".\n",
            "# You probably should not manually edit this file.\n\n",

            # for dzil
            "# DATE\n",
            "# VERSION\n",
            "\n",

            $dp_code1,

            "package main;\n",
            "use 5.010001;\n",
            "use strict;\n",
            "use warnings;\n",
            (map {"require $_;\n"} sort keys %{$cd->{modules}}),
            "\n",

            "# global variables\n\n",
            (map {"my $_;\n"} sort keys %{$cd->{vars}}),
            (keys(%{$cd->{vars}}) ? "\n" : ""),

            "# subroutines\n\n",
            (map {"sub $_" . (ref($cd->{subs}{$_}) eq 'ARRAY' ?
                "($cd->{subs}{$_}[0]) {\n$cd->{subs}{$_}[1]\n}\n\n" : " {\n$cd->{subs}{$_}\n}\n\n")}
                sort keys %{$cd->{subs}}),
            "\n",

            ("# code before parse cmdline options \n\n", $args{code_before_parse_cmdline_options}) x !!$args{code_before_parse_cmdline_options},

            @l,

            $dp_code2,
        );
    }

  WRITE_OUTPUT:
    {
        my ($fh, $output_is_stdout);
        if (!defined($args{output_file}) || $args{output_file} eq '-') {
            $output_is_stdout++;
        } else {
            if (-f $args{output_file}) {
                return [412, "Output file '$args{output_file}' exists, ".
                            "won't overwrite (see --overwrite)"]
                    unless $args{overwrite};
            }
                open $fh, ">", $args{output_file}
                    or return [500, "Can't open $args{output_file}: $!"];
        }

        if ($output_is_stdout) {
            return [200, "OK", $cd->{result}, {
                'cmdline.skip_format' => 1,
                'func.raw_result' => $cd,
            }];
        } else {
            print $fh $cd->{result};
            close $fh or return [500, "Can't write $args{output_file}: $!"];
            chmod 0755, $args{output_file} or do {
                warn "Can't chmod 755 $args{output_file}: $!";
            };
            return [200, "OK", undef, {
                'func.raw_result'=>$cd,
            }];
        }
    }
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

B<EARLY DEVELOPMENT.>

Sample startup overhead benchmark vs Perinci::CmdLine::Lite:

# COMMAND: perl devscripts/bench-startup


=head1 SEE ALSO

L<Perinci::CmdLine>, L<Perinci::CmdLine::Any>, L<Perinci::CmdLine::Lite>,
L<Perinci::CmdLine::Classic>

L<App::GenPericmdScript>

=cut
