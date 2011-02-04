# ABSTRACT: Scrappy Interactive Shell
package Scrappy::Console;

use 5.006;
use warnings;
use strict;
use Scrappy::Console::Util;
use Scrappy::Console::Support;
use Scrappy::Console::Command::HelpMenu;

BEGIN {
    use Exporter();
    use vars qw( @ISA @EXPORT @EXPORT_OK );
    @ISA    = qw( Exporter );
    @EXPORT = qw(shell);
}

=head1 SYNOPSIS

    ... from the command-line type
    
    scrappy
    
    -or-
    
    perl -MScrappy::Console -e shell


=cut

=method shell

An interactive console for Scrappy

=cut

sub shell {
    App::Rad->shell({
        title      => [<DATA>],
        prompt     => 'S',
        autocomp   => 1,
        abbrev     => 1,
        ignorecase => 0,
        history    => 1, # or 'path/to/histfile.txt'
    });
}

sub main::setup {
    
    my $c = shift;
       $c->stash->{commands} = $c->{_commands};

    # Load commands
    
    my $support = Scrappy::Console::Support->new;
    foreach my $plugin ( $support->plugins ) {
        foreach my $cmd ( @{$plugin->new($c)->{commands}} ) {
            my $n = $c->register($cmd->{name}, $cmd->{code}, $cmd->{help});
            $c->{_commands}->{$n}->{args} = $cmd->{args};
        }
    }
    
    # Error handling
    
    $c->{'_functions'}->{'invalid'} = sub {
        my $c = shift;
        my $u = Scrappy::Console::Util->new;            
        return $u->template('misc/error_string.tt', $c);
    };
    
}

1;

__DATA__

Welcome to the Scrappy interactive console application.
This application should be primarily used to load; HTML
and grab; HTML elements using CSS and XPATH selectors for
testing and debugging purposes.

