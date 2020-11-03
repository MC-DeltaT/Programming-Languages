# Import the file iteration function
use File::Find;

sub callback {
    # If filename matches regex .*\.conf$
    if ($_ =~ /.*\.conf$/) {
        # Print filename with newline
        print "$_\n";
    }
}

# Iterate over all files in system, call "callback" for each one
find(\&callback, "/");
