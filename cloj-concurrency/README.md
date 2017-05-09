# cloj-concurrency

FIXME: description

## Installation

Download from http://example.com/FIXME.

## Usage

FIXME: explanation

    $ java -jar cloj-concurrency-0.1.0-standalone.jar [args]

## Options

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

### Any Other Sections


This is all interesting and fun, but what happens if two separate threads call (swap! fred increase-cuddle-hunger-level 1)? Is it possible for one of the increments to get lost the way it did in the Ruby example at Listing 10-1?
The answer is no! swap! implements compare-and-set semantics, meaning it does the following internally:

It reads the current state of the atom.
It then applies the update function to that state.
Next, it checks whether the value it read in step 1 is identical to the atom’s current value.
If it is, then swap! updates the atom to refer to the result of step 2.
If it isn’t, then swap! retries, going through the process again with step 1.

### That You Think
### Might be Useful

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
