package Scrappy::Console::Support;

use App::Rad;
use App::Rad::Shell;
use Module::Pluggable search_path => ['Scrappy::Console::Command'];

sub new {
    my $class = shift;
    return bless {}, $class;
}

# App::Rad 1.05 is still in beta and hasnt been officially released yet
# argggg, curses garu

{
    no strict;
    no warnings 'redefine';
    
    package
        App::Rad;
        use 5.006;
        use App::Rad::Command;
        use App::Rad::Help;
        use Carp ();
        use warnings;
        use strict;
        
        our $VERSION = '1.05'; # Experimental
        {
        
            no strict;
            no warnings 'redefine';
        
            my @OPTIONS = ();
        
            # - "I'm so excited! Feels like I'm 14 again" (edenc on Rad)
            sub _init {
                my $c = shift;
        
                # instantiate references for the first time
                $c->{'_ARGV'}    = [];
                $c->{'_options'} = {};
                $c->{'_stash'}   = {};
                $c->{'_config'}  = {};
                $c->{'_plugins'} = [];
        
                # this internal variable holds
                # references to all special
                # pre-defined control functions
                $c->{'_functions'} = {
                    'setup'        => \&setup,
                    'pre_process'  => \&pre_process,
                    'post_process' => \&post_process,
                    'default'      => \&default,
                    'invalid'      => \&invalid,
                    'teardown'     => \&teardown,
                };
        
                #load extensions
                App::Rad::Help->load($c);
                foreach (@OPTIONS) {
                    if ( $_ eq 'include' ) {
                        eval 'use App::Rad::Include; App::Rad::Include->load($c)';
                        Carp::croak 'error loading "include" extension.' if ($@);
                    }
                    elsif ( $_ eq 'exclude' ) {
                        eval 'use App::Rad::Exclude; App::Rad::Exclude->load($c)';
                        Carp::croak 'error loading "exclude" extension.' if ($@);
                    }
                    elsif ( $_ eq 'debug' ) {
                        $c->{'debug'} = 1;
                    }
                    else {
                        $c->load_plugin($_);
                    }
                }
        
                # tiny cheat to avoid doing a lot of processing
                # when not in debug mode. If needed, I'll create
                # an actual is_debugging() method or something
                if ( $c->{'debug'} ) {
                    $c->debug( 'initializing: default commands are: '
                          . join( ', ', $c->commands() ) );
                }
            }
        
            sub import {
                my $class = shift;
                @OPTIONS = @_;
            }
        
            sub load_plugin {
                my $c      = shift;
                my $plugin = shift;
                my $class  = ref $c;
        
                my $plugin_fullname = '';
                if ( $plugin =~ s{^\+}{} ) {
                    $plugin_fullname = $plugin;
                }
                else {
                    $plugin_fullname = "App::Rad::Plugin::$plugin";
                }
                eval "use $plugin_fullname ()";
                Carp::croak "error loading plugin '$plugin_fullname': $@\n"
                  if $@;
                my %methods = _get_subs_from($plugin_fullname);
        
                Carp::croak "No methods found for plugin '$plugin_fullname'\n"
                  unless keys %methods > 0;
        
                no strict 'refs';
                foreach my $method ( keys %methods ) {
        
                    # don't add plugin's internal methods
                    next if substr( $method, 0, 1 ) eq '_';
        
                    *{"$class\::$method"} = $methods{$method};
                    $c->debug("-- method '$method' added [$plugin_fullname]");
        
                    # fill $c->plugins()
                    push @{ $c->{'_plugins'} }, $plugin;
                }
            }
        
            # this function browses a file's
            # symbol table (usually 'main') and maps
            # each function to a hash
            #
            # FIXME: if I create a sub here (Rad.pm) and
            # there is a global variable with that same name
            # inside the user's program (e.g.: sub ARGV {}),
            # the name will appear here as a command. It really
            # shouldn't...
            sub _get_subs_from {
                my $package = shift || 'main';
                $package .= '::';
        
                my %subs = ();
        
                no strict 'refs';
                while ( my ( $key, $value ) = ( each %{ *{$package} } ) ) {
                    local (*SYMBOL) = $value;
                    if ( defined $value && defined *SYMBOL{CODE} ) {
                        $subs{$key} = *{$value}{CODE};
                    }
                }
                return %subs;
            }
        
            # overrides our pre-defined control
            # functions with any available
            # user-defined ones
            sub _register_functions {
                my $c    = shift;
                my %subs = _get_subs_from('main');
        
                # replaces only if the function is
                # in 'default', 'pre_process' or 'post_process'
                foreach ( keys %{ $c->{'_functions'} } ) {
                    if ( defined $subs{$_} ) {
                        $c->debug("overriding $_ with user-defined function.");
                        $c->{'_functions'}->{$_} = $subs{$_};
                    }
                }
            }
        
            # retrieves command line arguments
            # to be executed by the main program
            sub parse_input {
                my $c = shift;
        
                # parse global arguments out of ARGV
                if ( $c->{'_globals'} ) {
                    $c->_parse( \@ARGV, $c->{'_globals'} );
                }
        
                #TODO: this could use some major improvements
                # now the next item in ARGV is our command name.
                # If it doesn't exist, we make it blank so we
                # can call the 'default' command
                my $cmd = $c->{'cmd'} = '';
                if ( defined $ARGV[0] ) {
                    my $cmd_obj = undef;
        
                    # argument looks like command
                    if ( substr( $ARGV[0], 0, 1 ) ne '-' ) {
                        $cmd = shift @ARGV;
                        $c->{'cmd'} = $cmd;
        
                        # valid command
                        if ( $c->is_command($cmd) ) {
                            $cmd_obj = $c->{'_commands'}->{$cmd};
                        }
        
                        # invalid command
                        else {
                            $cmd = undef;
                        }
                    }
                    my @tARGV = @ARGV;
                    $c->_parse( \@tARGV, $cmd_obj );
                }
                return $cmd;    # default (''), invalid (undef), command ($cmd)
            }
        
            sub _parse {
                my ( $c, $arg_ref, $cmd_obj ) = (@_);
        
                # al newkirk: conflict support
                my @arg_names = ();
                my @conflicts_with = ();
        
                # reset any previous value
                %{ $c->options } = ();
                @{ $c->argv }    = ();
        
                while ( my $arg = shift @{$arg_ref} ) {
        
                    # single option (could be grouped)
                    if ( $arg =~ m/^\-([^\-\=]+)$/o ) {
                        my @args = split //, $1;
                        foreach (@args) {
        
                            # _parse_arg returns the options' name
                            # and its "to_stash" values as an arrayref,
                            # or undef and an error message.
                            # TODO: this is a horrible approach I took only
                            # because it's 4am and I'm in a rush to get it done.
                            # any attempts to rewrite the parser in order to
                            # improve it will be **much** appreciated. Thanks!
                            my ( $opt, $to_stash ) = ( $_, undef );
                            if ( defined $cmd_obj ) {
                                ( $opt, $to_stash ) = $cmd_obj->_parse_arg($opt);
                                unless ($opt) {
                                    Carp::croak "Error: $to_stash";
        
                                    # TODO x 2: this should be forwared to an
                                    # overridable help error handler or whatever
                                }
                            }
        
                            $c->options->{$opt} =
                              ( defined $c->options->{$opt} )
                              ? $c->options->{$opt} + 1
                              : 1;
        
                            foreach my $stash_key (@$to_stash) {
                                $c->stash->{$stash_key} =
                                  ( defined $c->stash->{$stash_key} )
                                  ? $c->stash->{$stash_key} + 1
                                  : 1;
                            }
                        }
                    }
        
                    # long option: --name or --name=value
                    elsif ( $arg =~ m/^\-\-([^\-\=]+)(?:\=(.+))?$/o ) {
                    
                        my ($key, $val) = ($1, (defined $2 ? $2 : ""));
        
                        # al newkirk: when defaulting to a value of one, the type
                        # if exists, must be changed to "num" avoid attempting to validate "1"
                        # as "any" or "str" and failing.
                        # see - App::Rad::Command::_parse_arg
        
                        my $to_stash = undef;
        
                        # TODO: see above TODO :)
                        if ( defined $cmd_obj ) {
        
                          # WARNING! al newkirk: I am adding an additional parameter
                          # to the cmd_obj which may break some other code.
                          # Hopefully not :)
                          # I am making App::Rad::Command aware of self ($c to be exact)
                            ( $key, $to_stash, $val ) = $cmd_obj->_parse_arg( $key, $val, $c );
                            if (!$key) {
                                Carp::croak "Error: $to_stash";
                            }
                        }
        
                        # original code
                        # my ($key, $val) = ($1, (defined $2 ? $2 : 1));
        
                        # al newkirk: my nasty little hacked in fail safe.
                        # added in default value checking before defaulting to ""
                        
                        unless ($to_stash) {
                            if (!$key || !$val) {
                                ( $key, $val ) = (
                                    $1,
                                    (
                                        defined $2 ? $2
                                        : (
                                            defined $cmd_obj->{args}->{$1}->{default}
                                            ? $cmd_obj->{args}->{$1}->{default}
                                            : ""
                                        )
                                    )
                                );
                            }
                        }
        
                        $c->options->{$key} = $val;
                        foreach my $stash_key (@$to_stash) {
                            $c->stash->{$stash_key} = $val;
                        }
                        # al newkirk: save key/name for conflict validation, etc
                        push ( @arg_names, $key ) if $key;
                        
                        # al newkirk: conflict support
                        push @conflicts_with, { arg => $key, conflict => $cmd_obj->{args}->{$key}->{conflicts_with} }
                          if defined $cmd_obj->{args}->{$key}->{conflicts_with};
                    }
                    else {
                        push @{ $c->argv }, $arg;
                    }
                }
                # al newkirk: conflict support
                # Note! conflict support currently only works against args using the long option
                if (@conflicts_with) {
                    foreach my $name (@arg_names) {
                        if ( grep { $name eq $_->{conflict} } @conflicts_with ) {
                            my @clist = map { $_->{arg} } @conflicts_with;
                            die "Error: $name conflicts with ". join(" and ", @clist ) ." and can not be use together.";
                        }
                    }
                }
            }
        
            sub _run_full_round {
                my $c   = shift;
                my $sub = shift;
        
                $c->debug('calling pre_process function...');
                $c->{'_functions'}->{'pre_process'}->($c);
        
                $c->debug('executing command...');
                $c->{'output'} = $sub->($c);
        
                $c->debug('calling post_process function...');
                $c->{'_functions'}->{'post_process'}->($c);
        
                $c->debug('reseting output');
                $c->{'output'} = undef;
            }
        
            #========================#
            #     PUBLIC METHODS     #
            #========================#
        
            sub load_config {
                require App::Rad::Config;
                App::Rad::Config::load_config(@_);
            }
        
            sub path {
                require FindBin;
                return $FindBin::Bin;
            }
        
            sub real_path {
                require FindBin;
                return $FindBin::RealBin;
            }
        
            # - "Wow! you guys rock!" (zoso on Rad)
            #TODO: this code probably could use some optimization
            sub register_commands {
                my $c            = shift;
                my %help_for_sub = ();
                my %rules        = ();
        
                # process parameters
                foreach my $item (@_) {
        
                    # if we receive a hash ref, it could be commands or
                    # rules for fetching commands.
                    if ( ref($item) ) {
                        Carp::croak
                          '"register_commands" may receive only HASH references'
                          unless ref $item eq 'HASH';
        
                        foreach my $params ( keys %{$item} ) {
                            Carp::croak
        'registered elements may only receive strings or hash references'
                              if ref $item->{$params}
                                  and ref $item->{$params} ne 'HASH';
        
                            # we got a rule - push it in.
                            if (   $params eq '-ignore_prefix'
                                or $params eq '-ignore_suffix'
                                or $params eq '-ignore_regexp' )
                            {
                                $rules{$params} = $item->{$params};
                            }
        
                            # not a rule, so it's either a command with
                            # help text or a command with an argument list.
                            # either way, we push it to our 'help' hash.
                            else {
                                $help_for_sub{$params} = $item->{$params};
                            }
                        }
                    }
                    else {
                        $help_for_sub{$item} = undef;    # no help text
                    }
                }
        
                # hack, prevents registering methods from App::Rad namespace when
                # using shell-mode - Al Newkirk (awnstudio)
                # my $caller = ( caller(2) or 'main' );
                my $caller =
                    (
                     caller(2) &&
                     caller(2) ne 'App::Rad' &&
                     caller(2) ne 'App::Rad::Shell'
                    ) ?
                    caller(2) : 'main';
                my %subs = _get_subs_from($caller);
        
                # handles explicit command calls first, as
                # they have priority over generic rules (below)
                foreach my $cmd ( keys %help_for_sub ) {
        
                    # we only add the sub to the commands
                    # list if it's *not* a control function
                    if ( not defined $c->{'_functions'}->{$cmd} ) {
        
                        if ( $cmd eq '-globals' ) {
        
                            # use may set it as a flag to enable global arguments
                            # or elaborate on each available argument
                            my %command_options = ( name => '', code => sub { } );
                            if ( ref $help_for_sub{$cmd} ) {
                                $command_options{args} = $help_for_sub{$cmd};
                            }
                            my $cmd_obj = App::Rad::Command->new( \%command_options );
                            $c->{'_globals'} = $cmd_obj;
        
                       #                $c->register(undef, undef, $help_for_sub{$cmd});
                        }
        
                        # user wants to register a valid (existant) sub
                        elsif ( exists $subs{$cmd} ) {
                            $c->register( $cmd, $subs{$cmd}, $help_for_sub{$cmd} );
                        }
                        else {
                            Carp::croak
        "'$cmd' does not appear to be a valid sub. Registering seems impossible.\n";
                        }
                    }
                }
        
                # no parameters, or params+rules: try to register everything
                if ( ( !%help_for_sub ) or %rules ) {
                    foreach my $subname ( keys %subs ) {
        
                        # we only add the sub to the commands
                        # list if it's *not* a control function
                        if ( not defined $c->{'_functions'}->{$subname} ) {
        
                            if ( $rules{'-ignore_prefix'} ) {
                                next
                                  if (
                                    substr(
                                        $subname, 0,
                                        length( $rules{'-ignore_prefix'} )
                                    ) eq $rules{'-ignore_prefix'}
                                  );
                            }
                            if ( $rules{'-ignore_suffix'} ) {
                                next
                                  if (
                                    substr(
                                        $subname,
                                        length($subname) -
                                          length( $rules{'-ignore_suffix'} ),
                                        length( $rules{'-ignore_suffix'} )
                                    ) eq $rules{'-ignore_suffix'}
                                  );
                            }
                            if ( $rules{'-ignore_regexp'} ) {
                                my $re = $rules{'-ignore_regexp'};
                                next if $subname =~ m/$re/o;
                            }
        
                            # avoid duplicate registration
                            if ( !exists $help_for_sub{$subname} ) {
                                $c->register( $subname, $subs{$subname} );
                            }
                        }
                    }
                }
            }
        
            sub register_command { return register(@_) }
        
            sub register {
                my ( $c, $command_name, $coderef, $extra ) = @_;
        
                # short circuit
                return unless ref $coderef eq 'CODE';
        
                my %command_options = (
                    name => $command_name,
                    code => $coderef,
                );
        
                # the extra parameter may be a help string
                # or an argument hashref
                if ($extra) {
                    if ( ref $extra ) {
                        $command_options{args} = $extra;
                    }
                    else {
                        $command_options{help} = $extra;
                    }
                }
        
                my $cmd_obj = App::Rad::Command->new( \%command_options );
                return unless $cmd_obj;
        
                #TODO: I don't think this message is ever being printed (wtf?)
                $c->debug("registering $command_name as a command.");
        
                $c->{'_commands'}->{$command_name} = $cmd_obj;
                return $command_name;
            }
        
            sub unregister_command { return unregister(@_) }
        
            sub unregister {
                my ( $c, $command_name ) = @_;
        
                if ( $c->{'_commands'}->{$command_name} ) {
                    delete $c->{'_commands'}->{$command_name};
                }
                else {
                    return undef;
                }
            }
        
            sub create_command_name {
                my $id = 0;
                foreach ( commands() ) {
                    if (m/^cmd(\d+)$/) {
                        $id = $1 if ( $1 > $id );
                    }
                }
                return 'cmd' . ( $id + 1 );
            }
        
            sub commands {
                return ( keys %{ $_[0]->{'_commands'} } );
            }
        
            sub is_command {
                my ( $c, $cmd ) = @_;
                return (
                    defined $c->{'_commands'}->{$cmd}
                    ? 1
                    : 0
                );
            }
        
            sub command : lvalue {
                cmd(@_);
            }
        
            sub cmd : lvalue {
                $_[0]->{'cmd'};
            }
        
            # - "I'm loving having something else write up the 80% drudge
            #   code for the small things." (benh on Rad)
            sub run {
                my $class = shift;
                my $c     = {};
                bless $c, $class;
                
                # set state
                $c->{state} = 'cli';
                
                $c->_init();
        
                # first we update the control functions
                # with any overriden value
                $c->_register_functions();
        
                # then we run the setup to register
                # some commands
                $c->{'_functions'}->{'setup'}->($c);
        
                # now we get the actual input from
                # the command line (someone using the app!)
                my $cmd = $c->parse_input();
        
                if ( not defined $cmd ) {
                    $c->debug( "'"
                          . $c->cmd
                          . "' is not a valid command. Falling to invalid." );
                    $cmd = $c->{'_functions'}->{'invalid'};
                }
                elsif ( $cmd eq '' ) {
                    $c->debug('no command detected. Falling to default');
                    $cmd = $c->{'_functions'}->{'default'};
                }
                else {
                    my $obj = $c->{'_commands'}->{$cmd};
        
                    # set default values for command (if available)
                    $obj->_set_default_values( $c->options, $c->stash );
        
                    $cmd = sub { $obj->run(@_) }
                }
        
                # run the specified command
                $c->_run_full_round($cmd);
        
                # that's it. Tear down everything and go home :)
                $c->{'_functions'}->{'teardown'}->($c);
        
                return 0;
            }
        
            # run operations
            # in a shell-like environment
            sub shell {
                my $class  = shift;
                my $params = shift;
                require App::Rad::Shell;
                return App::Rad::Shell::shell($class, $params);
            }
        
            sub execute {
                my ( $c, $cmd ) = @_;
        
                # given command has precedence
                if ($cmd) {
                    $c->{'cmd'} = $cmd;
                }
                else {
                    $cmd = $c->{'cmd'};    # now $cmd always has the called cmd
                }
        
                # valid command, run it and return the command name
                if ( $c->is_command($cmd) ) {
                    my $cmd_obj = $c->{'_commands'}->{$cmd};
        
                    # set default values for command (if available)
                    $cmd_obj->_set_default_values( $c->options, $c->stash );
        
                    $c->_run_full_round( sub { $cmd_obj->run(@_) } );
                    return $cmd;
                }
                else {
        
                    # if not a command, return undef
                    return;
                }
            }
        
            sub argv    { return $_[0]->{'_ARGV'} }
            sub options { return $_[0]->{'_options'} }
            sub stash   { return $_[0]->{'_stash'} }
            sub config  { return $_[0]->{'_config'} }
            
            # get user information via prompting - Al Newkirk (awnstudio)
            sub prompt { return App::Rad::Shell::prompt(@_); }
        
            # $c->plugins is sort of "read-only" externally
            sub plugins {
                my @plugins = @{ $_[0]->{'_plugins'} };
                return @plugins;
            }
        
            sub getopt {
                require Getopt::Long;
                Carp::croak "Getopt::Long needs to be version 2.36 or above"
                  unless $Getopt::Long::VERSION >= 2.36;
        
                my ( $c, @options ) = @_;
        
                # reset values from tinygetopt
                #$c->{'_options'} = {};
                %{ $c->options } = ();
        
                my $parser = new Getopt::Long::Parser;
                $parser->configure(qw(bundling));
        
                my @tARGV = @ARGV;    # we gotta stick to our API
                my $ret = $parser->getoptions( $c->{'_options'}, @options );
                @{ $c->argv } = @ARGV;
                @ARGV = @tARGV;
        
                return $ret;
            }
        
            sub debug {
                if ( shift->{'debug'} ) {
                    print "[debug]   @_\n";
                }
            }
        
            # gets/sets the output (returned value)
            # of a command, to be post processed
            sub output {
                my ( $c, @msg ) = @_;
                if (@msg) {
                    $c->{'output'} = join( ' ', @msg );
                }
                else {
                    return $c->{'output'};
                }
            }
        
            #=========================#
            #     CONTROL FUNCTIONS   #
            #=========================#
        
            sub setup { $_[0]->register_commands( { -ignore_prefix => '_' } ) }
        
            sub teardown { }
        
            sub pre_process { }
        
            sub post_process {
                my $c = shift;
        
                if ( $c->output() ) {
                    print $c->output() . $/;
                }
            }
        
            sub default {
                my $c = shift;
                return $c->{'_commands'}->{'help'}->run($c);
            }
        
            sub invalid {
                my $c = shift;
                return $c->{'_functions'}->{'default'}->($c);
            }
            
        }
}
{
    no strict;
    no warnings 'redefine';

    package
        App::Rad::Shell;
        use Carp 'croak';
        use Term::ReadLine;
        use strict;
        use warnings;
        
        our $VERSION = '0.01';
        
        {
            no strict;
            no warnings 'redefine';
        
            # get input from user via prompting - Al Newkirk (awnstudio)
            sub prompt {
                my $criteria = pop(@_);
                my $c 	 = pop(@_);
                
                croak 'input prompt criteria must be a hash reference'
                        unless (ref $criteria eq 'HASH');
                
                return $c->options->{$criteria->{opt}} if
                    defined $criteria->{opt} && $c->options->{$criteria->{opt}};
                
                if ($criteria->{set}) {
                    print $criteria->{ask}, ":> e.g. ", $criteria->{set}, " :> ";
                }
                else {
                    print $criteria->{ask}, ":> ";
                }
            
                $| = 1;          # force a flush after our print
                $_ = <STDIN>;    # get the input from STDIN (presumably the keyboard)
            
                chomp;
                if ( $_ eq "0" ) {
                    return "0";
                }
                else {
                    if ($criteria->{set}) {
                        return $_ ? $_ : $criteria->{set};    # return $_ if it has a value
                    }
                    else {
                        return $_;
                    }
                }
            }
            
            #    App::Rad->shell( {
            #        prompt     => 'cmd: ',
            #        autocomp   => 1,
            #        abbrev     => 1,
            #        ignorecase => 0,
            #        history    => 1, # or 'path/to/histfile.txt'
            #    });
            #
            sub shell {
                my $class = shift; # should be 'App::Rad'
                my $c = {};
                bless $c, $class;
            
                # set state
                $c->{state} = 'shell';
            
                my $prompt = $0;
                $prompt =~ s{\.pl$}{};
            
                my $params_ref = shift;
                if ($params_ref) {
                    croak 'argument to shell() method must be a hash reference'
                        unless (ref $params_ref eq 'HASH');
                    $prompt = $params_ref->{prompt} if defined $params_ref->{prompt};
                }
                # $c->{'_shell'} = %{ shift() };
            
                eval 'use Term::ReadKey';
                %{$c->{'_shell'}} = (
                        'has_readkey' => (defined $@) ? 0 : 1,
                        'prompt'      => "$prompt> ",
                        'autocomp'    => 1,
                                    );
                
                # process cli request instead of shell - Al Newkirk (awnstudio)
                if (@ARGV) {
                    return App::Rad->run;
                }
                
                $c->_init();
                $c->_register_functions();
            
                    # dirty hack to override 'default' function
                    $c->{'_functions'}->{'default'} = sub {};
                    $c->{'_functions'}->{'invalid'} = sub { return "Invalid command. Type help; for a list of commands." };
            
                # this is *before* setup() because the application
                # developer might want to modify the command.
                $c->register('quit', \&quit, 'exits the command shell.');
                $c->register('help', \&App::Rad::Help::helpstr, 'show syntax and available commands.');
                
                # EXPERIMENTAL - Al Newkirk (awnstudio)
                # clear shell buffer
                $c->register('clear', sub {
                    my $c = shift;
                    my $clr = $^O =~ /[Ww]in(32)?/ ? "cls" : "clear";
                    system($clr);
                }, 'clear text from the screen.');
                
                # then we run the setup to register
                # some commands
                $c->{'_functions'}->{'setup'}->($c);
                
                # print startup message - Al Newkirk (awnstudio)
                sub startup_message {
                    my $ref = shift;
                    my $startup_message = "";
                    if (defined $ref->{title}) {
                        if (ref($ref->{title}) eq "GLOB") {
                            my $fh = ${$ref->{title}};
                            while (<$fh>) {
                                $startup_message .= "$_";
                            }
                            # replace filehandle with content :)
                            $ref->{title} = $startup_message;
                        }
                        elsif (ref($ref->{title}) eq "ARRAY") {
                            $startup_message .=
                                join($/, map {chomp $_; $_} @{$ref->{title}});
                        }
                        else {
                            $startup_message = $ref->{title};
                        }
                        $startup_message .= "\n";
                    }
                    return ($startup_message || "") . 
                        "Type help; for a list of available commands, and quit; to quit.\n" .
                        "Execute commands [w/ or wo/ options] using a ; at the end of the line.\n\n";
                }
                print startup_message($params_ref);
                
                # EXPERIMENTAL - Al Newkirk (awnstudio)
                # create synonyms for quit :)
                #    $c->{'_commands'}->{'exit'} = $c->{'_commands'}->{'quit'} unless
                #	defined $c->{'_commands'}->{'q'};
                #    $c->{'_commands'}->{'q'} = $c->{'_commands'}->{'quit'} unless
                #	defined $c->{'_commands'}->{'q'};
                
                # Adding multi-line read support - Al Newkirk (awnstudio)
                my $multiline_read   = 0;
                my @multiline_buffer = ();
                
                do {
                    print $c->{'_shell'}->{'prompt'};
                    # get command and options - Al Newkirk (awnstudio)
                    my $cmd  = join " ", split /\s+/, <>;
                    my @opts = ();
                    if ($cmd) {
                        if ($cmd =~ /\;(\s+)?$/) {
                            
                            my $lp = $c->{'_shell'}->{'prompt'};
                            $lp  =~ s/\s+$//g;
                            $cmd =~ s/^$lp//;
                            
                            # what kind of multiline?
                            if ($multiline_read) {
                                # build command
                                push @multiline_buffer,
                                    split /\s+(?![\w]+["'])/, $cmd;
                                ($cmd, @opts) = @multiline_buffer;
                                # reset 
                                $c->{'_shell'}->{'prompt'} = "$prompt> ";
                                $multiline_read   = 0;
                                @multiline_buffer = ();
                            }
                            else {
                                # hack for one-liners, may break something :\
                                # Al Newkirk (awnstudio)
                                if ($cmd =~ /^\w+\s(\w+|-+\w+(=\w+)?).*;/) {
                                    ($cmd, @opts) = $cmd =~ /^(\w+)\s(.*)/;
                                }
                            }
                            
                            $cmd =~ s/\;$//;
                            $opts[0] =~ s/\;(\s+)?$// if @opts;
                            $opts[$#opts] =~ s/\;(\s+)?$// if @opts;
                            
                            # process commands
                            if (defined $c->{_commands}->{$cmd}) {
                                # hack for clear buffer function - Al Newkirk (awnstudio)
                                if (lc($cmd) eq "clear") {
                                    $c->{_commands}->{$cmd}->run($c);
                                    print startup_message($params_ref);
                                }
                                else {
                                    # seperate params by space
                                    my @psuedo_argvs = ();
                                    @opts = split /\s+(?![\w]+["'])/, join " ", @opts;
                                    foreach my $opt (@opts) {
                                        $opt =~ s/(^\s+|\s+$)//g;
                                        if ($opt =~ /^(\-+)(\w+)=(['"])(.+)(['"])/)
                                        {
                                            $opt = "$1$2=$4" if $1 && $2 && $4;
                                        }
                                        push @psuedo_argvs, $opt if $opt;
                                    }
                                    $c->_parse(\@psuedo_argvs, $c->{_commands}->{$cmd});
                                    my $output = $c->{_commands}->{$cmd}->run($c);
                                    print "$output\n" if $output;
                                    # reset parameters to avoid collision - Al Newkirk (awnstudio)
                                    # possbily not good :\
                                    # delete $c->{_commands}->{$cmd}->{args};
                                }
                            }
                            else {
                                print $c->{'_functions'}->{'invalid'}->(), "\n";
                            }
                            
                        }
                        else {
                            my $mlp =
                                $c->{'_shell'}->{'prompt'} =
                                    ("-" x length($prompt)) . "> ";
                            $mlp =~ s/\s+$//g;
                            $cmd =~ s/^$mlp//;
                            if ($cmd) {
                                push @multiline_buffer,
                                    split /\s+(?![\w]+["'])/, $cmd;
                            }
                            $multiline_read = 1;
                        }
                    }
                } 	while (1);
            }
            
            sub quit {
                    my $c = shift;
                    $c->{'_functions'}->{'teardown'}->($c);
                    exit;
            }
        }
}
   
1;
