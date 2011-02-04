#ABSTRACT: Help Documentation For The Scrappy Console
package Scrappy::Console::Command::HelpMenu;

use warnings;
use strict;

use base 'Scrappy::Console::Command';

sub new {
    my $class = shift;
    my $self  = {};
    
    $self->{commands} = [
        {
            name => 'help',
            code => sub {
                my $c = shift;
                my $u = Scrappy::Console::Util->new;
                my $h = Scrappy::Console::Command::HelpMenu->new;
                
                # display help document for a specific function
                if (defined $c->argv->[0]) {
                    if (defined $c->{_commands}->{$c->argv->[0]}) {
                        return $h->display($c->argv->[0], $c);
                    }
                }
                return $u->template('menus/commands.tt', $c);
            },
            help => 'display available commands.'
        },
        {
            name => 'menu',
            code => sub {
                my $c = shift;
                my $u = Scrappy::Console::Util->new;            
                
                return $u->template('menus/master.tt', $c);
            },
            help => 'display main menu.'            
        },
    ];
    
    return bless $self, $class;
}

=method display

Show help screen for a given command or topic.

=cut

sub display {
    my $self = shift;
    my ($cmd, $c) = @_;
    
    if (defined $c->{_commands}->{$cmd}) {
        return $self->{util}->template("commands/help/$cmd"."_help.tt", $c);
    }
    else {
        return $self->{help}->error("Sorry, cannot find help for $cmd.")->report($c);
    }
}

1;